open Netcgi
open Utils
open Json_type

type webapp_settings = {
  mutable debug: bool;
  mutable log_root: string option;
}

type session = {
  is_admin: bool;
  is_login: bool;
  is_local: bool;
  is_mobile: bool;
  user_id: string;
  email: string;
  session_token: string;
  csrf_token: string;
}

let settings : webapp_settings = {
  debug = false;
  log_root = None;
}

let debug_out str =
  if settings.debug then print_endline str else ()

let attach_pid_file ~pidfile ~daemon_pid =
  match pidfile with
    | None -> ()
    | Some filename ->
      debug_out @@ spf "attach pid(%d) as file[%s]" daemon_pid filename;
      let pidfile = open_out_gen [Open_creat; Open_trunc; Open_wronly] 0o644 filename in
      output_string pidfile @@ string_of_int daemon_pid;
      close_out pidfile

let start_with_daemon fn =
  debug_out @@ spf "start daemon...";
  match Unix.fork () with
    | pid when pid > 0 -> exit 0 (** parent *)
    | _ -> (** child *)
      let daemon_pid = Unix.setsid () in (** detouch from parent session group *)
      debug_out @@ spf "pid = %d" daemon_pid;
      fn daemon_pid

let get_sock_addr ~port ~sockfile =
  match port, sockfile with
    | _, Some filename ->
      debug_out @@ spf "get unix socket(file = %s)" filename;
      (None, Unix.ADDR_UNIX filename)
    | Some port_no, None ->
      debug_out @@ spf "get sock addr(port = %d)" port_no;
      (port, Unix.ADDR_INET(Unix.inet_addr_loopback, port_no))
    | _ -> failwith "invalid socket addr"

let run_fcgi ?(port=None) ?(content_types: string list = [
  "multipart/form-data";
  "application/x-www-form-urlencoded";
  "application/json";
]) sockaddr fn =
  debug_out @@ spf "run fcgi...";
  let buffered _ ch = new Netchannels.buffered_trans_channel ch in
  let output_type = `Transactional buffered in
  let config = {Netcgi.default_config with
    permitted_http_methods = [`HEAD; `GET; `POST; `DELETE; `PUT];
    permitted_input_content_types = content_types;
  } in
  match port with
    | Some port -> Netcgi_fcgi.run ~config ~output_type ~sockaddr ~port fn
    | None -> Netcgi_fcgi.run ~config ~output_type ~sockaddr fn

let start_session_server
    ?(debug=false)
    ?(pidfile=None)
    ?(sockfile=None)
    ?(log_root=None)
    ~port
    ~on_error
    ~session_loader
    ~path_router =

  (** initialize settings *)
  settings.debug <- debug;
  settings.log_root <- log_root;
  debug_out "start session server";

  (** start fcgi daemon *)
  start_with_daemon (fun daemon_pid ->
    let (port, sockaddr) = get_sock_addr ~port ~sockfile in
    attach_pid_file ~pidfile ~daemon_pid;
    run_fcgi ~port sockaddr (fun cgi ->
      let _ = cgi#set_header ~cache:`No_cache ~content_type:"text/html; charset=utf-8" () in
      let path_args = get_path_args (cgi:>cgi) in
      let session = session_loader (cgi:>cgi) in
      (try path_router (cgi:>cgi) session path_args with exn -> on_error (cgi:>cgi) session exn);
      cgi#out_channel#commit_work();
      cgi#finalize()
    ))
