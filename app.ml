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

let attach_pid_file ~file_path ~daemon_pid =
  debug_out @@ spf "attach pid(%d) as file[%s]" daemon_pid file_path;
  let ch = open_out_gen [Open_creat; Open_trunc; Open_wronly] 0o644 file_path in
  output_string ch @@ string_of_int daemon_pid;
  close_out ch

let start_with_daemon fn =
  debug_out @@ spf "start daemon...";
  match Unix.fork () with
    | pid when pid > 0 -> exit 0 (** parent *)
    | _ -> (** child *)
      let daemon_pid = Unix.setsid () in (** detouch from parent session group *)
      debug_out @@ spf "pid = %d" daemon_pid;
      fn daemon_pid

let get_sock_addr ?(addr=None) ~port ~sockfile =
  match addr, port, sockfile with
    | _, _, Some filename ->
      debug_out @@ spf "get unix socket(file = %s)" filename;
      (None, Unix.ADDR_UNIX filename)
    | Some(addr), Some port_no, None ->
      debug_out @@ spf "get sock addr(addr = %s, port = %d)" addr port_no;
      (port, Unix.ADDR_INET(Unix.inet_addr_of_string addr, port_no))
    | None, Some port_no, None ->
      debug_out @@ spf "get sock addr(addr = 127.0.0.1, port = %d)" port_no;
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
    ?(addr=None)
    ~port
    ~on_error
    ~session_loader
    ~path_router =

  (** initialize settings *)
  settings.debug <- debug;
  settings.log_root <- log_root;
  debug_out "start session server";

  let (port, sockaddr) = get_sock_addr ~addr ~port ~sockfile in
  let fcgi_main (cgi : Netcgi_fcgi.cgi) =
    let _ = cgi#set_header ~cache:`No_cache ~content_type:"text/html; charset=utf-8" () in
    let path_args = get_path_args (cgi:>cgi) in
    let session = session_loader (cgi:>cgi) in
    (try path_router (cgi:>cgi) session path_args with exn -> on_error (cgi:>cgi) session exn);
    cgi#out_channel#commit_work();
    cgi#finalize() in

  match pidfile with
  (* if pidfile is defined, start daemon with pidfile *)
  | Some(file_path) -> 
    start_with_daemon (fun daemon_pid ->
      attach_pid_file ~file_path ~daemon_pid;
      run_fcgi ~port sockaddr fcgi_main
    )
  (* if pidfile is not defined, just start fcgi  *)
  | None ->
    run_fcgi ~port sockaddr fcgi_main
