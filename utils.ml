(*
  utils.ml
*)
open Netcgi
open CalendarLib

let (@@) f g = f g
let ($) f g x = f (g x)
let (+>) f g = g f
let spf = Printf.sprintf

module UTF8 = struct
  let string_to_list s =
    Uutf.String.fold_utf_8 (fun acc _ c -> c :: acc) [] s
    +> List.rev

  let length s = Uutf.String.fold_utf_8 (fun acc _ _ -> acc + 1) 0 s

  let compare = String.compare

  let sub s off len =
    let buf = Buffer.create 0 in
    let encoder = Uutf.encoder `UTF_8 (`Buffer buf) in
    let uchar_array = string_to_list s +> Array.of_list in
    let sub_array = Array.sub uchar_array off len in
    Array.iter (function
        | `Malformed s -> Buffer.add_string buf s
        | `Uchar _ as u -> ignore @@ Uutf.encode encoder u
      ) sub_array;
    ignore @@ Uutf.encode encoder `End;
    Buffer.contents buf
end

(** init time environment *)
let () = Time_Zone.change Time_Zone.Local

(** init random environment *)
let () = Random.init @@ int_of_float @@ Unix.time ()

let mapi fn list =
  let rec iter ret i = function
    | h :: rest -> iter ((fn i h) :: ret) (i+1) rest
    | _ -> List.rev ret in
  iter [] 0 list

let strlen = UTF8.length
let strcmp = UTF8.compare

(** application friendly substring *)
let rec substring base count str =
  let len = UTF8.length str in
  if base >= len || count = 0 then
    ""
  else if base = 0 && count >= len then
    str
  else if base < 0 then
    substring (len + base) count str
  else if base + count >= len then
    UTF8.sub str base (len - base)
  else
    UTF8.sub str base count

let rec take ?(pad=None) n lst =
  match n, lst, pad with
    | n, _, _ when n <= 0 -> []
    | n, [], None -> []
    | n, [], Some value -> value :: (take (n-1) [] ~pad)
    | n, h :: rest, _ -> h :: (take (n-1) rest ~pad)

let search_rex rex src =
  try ignore @@ Pcre.exec ~rex:rex src; true
  with Not_found -> false

let ymd_of_day day = 
  let y = Date.year day in
  let m = Date.int_of_month @@ Date.month day in
  let d = Date.day_of_month day in
  (y, m, d)

let make_hash alist =
  let hash = Hashtbl.create @@ List.length alist in
  List.fold_left (fun h (n,v) -> Hashtbl.add h n v; h) hash alist

let get_ymd ?(yesterday=false) () = 
  if yesterday then
    ymd_of_day @@ Date.prev (Date.today ()) `Day
  else
    ymd_of_day @@ Date.today ()

let get_rex ?(unicode=false) pat =
  if unicode then Pcre.regexp ~flags:[`UTF8] pat
  else Pcre.regexp pat

let get_path_args (cgi:cgi) =
  try
    match Pcre.split ~rex:(Pcre.regexp "\\/" ~flags:[`UTF8]) @@ cgi # environment # cgi_path_info with
      | "" :: rest -> rest
      | other -> other
  with
      exn -> []

let get_raw_md5 str =
  Digest.string str

let get_hex_md5 str =
  Digest.to_hex @@ Digest.string str

let get_raw_sha1 str =
  Cryptokit.hash_string (Cryptokit.Hash.sha1()) str

let get_hex_sha1 str =
  Cryptokit.transform_string (Cryptokit.Hexa.encode()) @@
    Cryptokit.hash_string (Cryptokit.Hash.sha1()) str

let get_base64 str =
  let trans = Cryptokit.Base64.encode_compact () in
  Cryptokit.transform_string trans str

let urlsafe_base64_char = function
  | '=' -> 'x'
  | '/' -> 'Y'
  | '+' -> 'z'
  | c -> c

let get_base64_short_hash ?(len=20) str =
  let sha1 = get_raw_sha1 str in
  let base64 = get_base64 sha1 in
  let len' = min len (String.length base64) in
  let buff = String.sub base64 0 len' in
  for i = 0 to len' - 1 do
    buff.[i] <- urlsafe_base64_char buff.[i]
  done;
  buff

let get_short_hash ?(len=20) str =
  let sha1 = get_hex_sha1 str in
  let len' = min len (String.length sha1) in
  String.sub sha1 0 len'

let gen_random_int () =
  Random.int 100000

let gen_random_float () =
  Unix.time ()

let gen_random_salt () =
  let random_float = gen_random_float () in
  let random_int = gen_random_int () in
  get_hex_sha1 @@ spf "%f-random-salt-%d" random_float random_int

let gen_random_hash ?(len=20) salt =
  get_base64_short_hash ~len @@ spf "%s-%d" salt (gen_random_int())

let stretch_hash ?(hash_fun=get_hex_sha1) ~count hash =
  let rec repeat i str =
    if i >= count then str
    else repeat (i+1) (hash_fun str) in
  repeat 0 hash

(*
let http_post ?(params=[]) url =
  let req = new Http_client.post url params in
  let header = req # request_header `Effective in
  let () = header # set_fields [
    ("Content-Type", "application/x-www-form-urlencoded");
  ] in
  let () = req # set_request_header header in
  let pipeline = new Http_client.pipeline in
  pipeline # add req;
  pipeline # run ()
*)

let forever f () = while true do f () done

let get_ip (cgi:cgi) =
  cgi # environment # cgi_remote_addr

let is_local_ip (cgi:cgi) =
  let ip = get_ip cgi in
  if ip = "127.0.0.1" then true
  else try ignore @@ Pcre.exec ~pat:"192.168." ip; true with _ -> false

let get_remote_host ip =
  try Some ((Unix.gethostbyaddr @@ Unix.inet_addr_of_string ip).Unix.h_name) with _ -> None

let get_referer (cgi:cgi) =
  cgi#environment#input_header_field "REFERER" ~default:""

let is_ssl (cgi:cgi) =
  cgi#environment#cgi_https

let get_cookie (cgi:cgi) name =
  try Some (cgi#environment#cookie name) with Not_found -> None

let get_cookie_value (cgi:cgi) name =
  match get_cookie cgi name with
    | Some cookie ->
      (match Cookie.value cookie with
	| "" -> None
	| value -> Some value)
    | None -> None

let create_cookie ?(is_secure=false) ?(domain=None) ?(expire=None) ?(path=None) (cgi:cgi) name value =
  let cookie = Cookie.make name value ~secure:is_secure in
  Cookie.set_max_age cookie expire;
  Cookie.set_path cookie path;
  (** disabled for IE problem *)
  (* Cookie.set_domain cookie domain; *)
  cookie

let set_cookie (cgi:cgi) cookies =
  cgi#set_header ~set_cookies:cookies ()

let get_suffix filename =
  match List.rev @@ Pcre.split ~rex:(Pcre.regexp "\\.") filename with
    | ext :: rest -> String.lowercase ext
    | _ -> ""

(** output json to stdout with proper content-type *)
let output_json
    ?(nosniff=true)
    ?(content_type="application/json; charset=utf-8")
    (cgi:cgi) json =

  cgi#environment#set_output_header_field "Content-Type" content_type;
  if nosniff then
    cgi#environment#set_output_header_field "X-Content-Type-Options" "nosniff"
  ;
  cgi#output#output_string @@ Json_io.string_of_json json

(** send_mail by Config.app.mailer  *)
let send_mail
    ?(from_addr=("no title", "noreply@nohost"))
    ?(subject="no title")
    ?(mailer="sendmail.postfix")
    ~to_addrs (** (title, mail) list *)
    message =
  Netsendmail.sendmail ~mailer @@
    Netsendmail.compose
    ~in_charset:`Enc_utf8
    ~out_charset:`Enc_utf8
    ~from_addr:from_addr
    ~subject:subject
    ~to_addrs:to_addrs
    message
