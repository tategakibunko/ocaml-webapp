(* this code is fetched from somewhere, but I can't remember it,
   so this code is not used in this library because of license.
*)
open Utils
open Https_client
open Http_client
open Http_client.Convenience

exception OAuthException of (int * string)

type oAuthVersion =
  | OAUTH_2_D10

type oAuthHTTPMethods =
  | GET
  | POST
  | PUT
  | DELETE

type oAuthUserStatus =
  | LoggedOut
  | Code of string (** Code to be exchanged for an Access Token *)
  | Token of (string * int) (** Access Token with an expiration timestamp set*)

type oAuthPermission = string

type oAuthEndpoint = { 
  api_login_url : string;
  api_token_url : string;
  api_base_url : string;
  auth_function : (string -> oAuthUserStatus);
  oauth_version : oAuthVersion;
  identifier : string;
  addon_login_param : (oAuthUser -> (string * string) list -> (string * string) list);
  addon_token_param : (oAuthUser -> (string * string) list -> (string * string) list);
  addon_apicall_param : (oAuthUser -> (string * string) list -> (string * string) list);
}

and oAuthUser = {
  status : oAuthUserStatus;
  permissions : oAuthPermission list;
  api_client : oAuthClient;
}

and oAuthClient = {
  id : string;
  secret : string;
  state : string;
  endpoint : oAuthEndpoint;
}


(** Initialize random stuff *)
let () = 
  ignore @@ Random.self_init ();
  ignore @@ Ssl.init ();
  configure_pipeline (fun p ->
    let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
    let tct = https_transport_channel_type ctx in
    p#configure_transport https_cb_id tct
  )

(**
   Find an element in a key * value list

   @param needle element to be found
   @param l (string * string) list l
   @raise Failure Fails when key not found
   @return string
*)
let rec find_param needle l = match List.hd l with
  | (name, value) when ((String.compare name needle) == 0) -> value
  | _ -> find_param needle (List.tl l)

let add_nothing user params = params

(** sample output like this.
{
  "access_token" : "????",
  "token_type" : "Bearer",
  "expires_in" : 3600,
  "id_token" : "???"
}
*)
let decode_json_token_access str =
  let json = Json_io.json_of_string str in
  let alist = match json with
    | Json_type.Object alist -> alist
    | _ -> failwith "decode_json_token_access:not object" in
  let access_token = match List.assoc "access_token" alist with Json_type.String token -> token | _ -> failwith "access_token not found" in
  let expires = match List.assoc "expires_in" alist with Json_type.Int expires -> expires | _ -> failwith "expires_in not found" in
  Token (access_token, expires)

let decode_url_encoded_access_token data = 
  print_endline data;
  let resp_list = Netencoding.Url.dest_url_encoded_parameters
    data in
  let token = find_param "access_token" resp_list in
  let expires =
    try
      int_of_string (find_param "expires" resp_list)
    with
      | _ -> 0 in
  Token (token, expires)

(*
  Register your Google app at
  https://code.google.com/apis/console/
*)
let google_oauth_endpoint = {
  api_login_url = "https://accounts.google.com/o/oauth2/auth";
  api_token_url = "https://accounts.google.com/o/oauth2/token";
  api_base_url = "https://www.googleapis.com/oauth2/v1/";
  auth_function = decode_json_token_access;
  oauth_version = OAUTH_2_D10;
  identifier = "google";
  addon_login_param = add_nothing;
  addon_token_param = add_nothing;
  addon_apicall_param = add_nothing;
}

(*
  Register your Github app at
  https://github.com/settings/applications/new
*)
let github_oauth_endpoint = {
  api_login_url = "https://github.com/login/oauth/authorize";
  api_token_url = "https://github.com/login/oauth/access_token";
  api_base_url = "https://api.github.com/";
  auth_function = decode_url_encoded_access_token;
  oauth_version = OAUTH_2_D10;
  identifier = "github";
  addon_login_param = add_nothing;
  addon_token_param = add_nothing;
  addon_apicall_param = add_nothing;
}

let random_token () =
  Digest.to_hex @@
    Digest.string @@
    Int32.to_string @@ 
    Random.int32 @@
    Int32.max_int

let set_user_status oauth_user new_status = {
  status = new_status;
  permissions = oauth_user.permissions;
  api_client = oauth_user.api_client;
}

let make_login_url api_user redirect_url = api_user.api_client.endpoint.api_login_url ^ "?" ^
  Netencoding.Url.mk_url_encoded_parameters [
    ("client_id",  api_user.api_client.id);
    ("redirect_uri",  redirect_url);
    ("scope", String.concat " " api_user.permissions);
    ("response_type", "code");
    ("state",  api_user.api_client.state);
  ]

let api_access_token api_client =
  api_client.id ^ "|" ^ api_client.secret

let access_token_params api_client redirect_url code = [
  ("client_id", api_client.id) ;
  ("client_secret", api_client.secret) ;
  ("redirect_uri",  redirect_url) ;
  ("grant_type", "authorization_code") ;
  ("code", code) ;
]

let exchange_code_for_access_token user redirect_url () =
  let response_parse = user.api_client.endpoint.auth_function in
  match user.status with
    | Code (code) ->
      let url = user.api_client.endpoint.api_token_url in
      let params =  user.api_client.endpoint.addon_token_param user (access_token_params user.api_client redirect_url code) in
      let response = http_post_message url params in
      let resp_body = response#get_resp_body () in
      (** Log.sys_notice resp_body *)
      (match response#response_status_code with
	| x when (x < 400) -> set_user_status user @@ response_parse @@ resp_body
	| _ -> raise @@ OAuthException (400, ("Failure on HTTP request:" ^ resp_body))
      )
    | _ -> raise @@ OAuthException (400, "User has no code")
	  
let get_access_token user =
  match user.status with
    | Token(token, expires) -> token
    | _ -> api_access_token user.api_client

let get_access_token_expires user =
  match user.status with
    | Token(token, expires) -> expires
    | _ -> failwith "not authorized yet"

let call_api user action ?(http_method = GET) ?(http_params = []) ?(http_content = "") () =
  let access_token = get_access_token user in
  let params_to_url url params =
    url ^ "?" ^ Netencoding.Url.mk_url_encoded_parameters params in
  let http_params = (* appends access token to parameters *)
    user.api_client.endpoint.addon_apicall_param user (("access_token", access_token) :: http_params) in
  let http_url =  (* check if a full URL is given *)
    if Str.string_match (Str.regexp "^https?://") action 0 then action
    else user.api_client.endpoint.api_base_url ^ action in
  let http_response = match http_method with
    | GET -> http_get_message @@ params_to_url http_url http_params
    | POST -> http_post_message http_url http_params
    | PUT -> http_put_message (params_to_url http_url http_params) http_content
    | DELETE -> http_delete_message @@ params_to_url http_url http_params in
  let resp_body = http_response#get_resp_body () in
  match http_response#response_status_code with
    | 200 -> resp_body
    | _ -> raise (OAuthException (400, "Failure on HTTP request"))

let make_api_client id secret endpoint ?(state = (random_token ())) () = {
  id = id;
  secret = secret;
  state = state;
  endpoint = endpoint;
}

let make_api_user api_client ?(status = LoggedOut) ?(permissions = []) () = {
  status = status;
  permissions = permissions;
  api_client = api_client;
}
