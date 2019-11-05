
(** Get file from a url. **)
let get_file url =
  let open Js_of_ocaml in
  let open Js_of_ocaml_lwt in
  let (res, w) = Lwt.task () in
  let request = XmlHttpRequest.create () in
  request##_open (Js.string "GET") (Js.string url) Js._true ;
  request##overrideMimeType (Js.string "text/plain") ;
  request##.onreadystatechange :=
    Js.wrap_callback (fun _ ->
      if request##.readyState = XmlHttpRequest.DONE then (
        if request##.status = 200 then
          Lwt.wakeup_later w (Js.to_string request##.responseText)
        else
          Lwt.wakeup_later_exn w (Invalid_argument
               ("Error when fetching " ^ url ^ ". "
                ^ string_of_int request##.status ^ " :"
                ^ Js.to_string request##.statusText)))) ;
  request##send Js.null ;
  res

(** Get parse a file. **)
let get_state url =
  let%lwt file = get_file url in
  let file =
    let lexbuf = Lexing.from_string file in
    lexbuf.Lexing.lex_curr_p <- {
        Lexing.pos_fname = url ;
        Lexing.pos_lnum = 1 ;
        Lexing.pos_bol = 0 ;
        Lexing.pos_cnum = 0
      } ;
    try Parser.main Lexer.read lexbuf with
    | Parser.Error ->
      failwith ("Error: Parser error " ^ Lexer.current_position lexbuf ^ ".")
    | Lexer.SyntaxError msg ->
      failwith ("Error: Lexer error " ^ Lexer.current_position lexbuf ^ ": " ^ msg)
    | e ->
      failwith ("Error during parsing " ^ Lexer.current_position lexbuf ^ ": "
                ^ Printexc.to_string e) in
  Lwt.return (Driver.parse_state file)

let _ =
  get_state "examples/picasso.kurso"

