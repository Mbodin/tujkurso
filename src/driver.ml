
module SMap = Map.Make(String)

type item =
  | String of string
  | Space
  | Command of string
  | Block of ast
and ast = item list (* TODO: Add positions to help creating nicer error messages. *)

type state = (* TODO *) list

type 'b command_arguments =
  | Arg_nil : 'b -> 'b command_arguments
  | Arg_cons : (string -> 'a option) -> ('a -> 'b) command_arguments -> 'b command_arguments
  | Arg_inner : 'a command_arguments -> ('a -> 'b) command_arguments -> 'b command_arguments
  | Arg_block : 'a commands -> (string -> 'a) -> ('a list -> 'b) command_arguments -> 'b command_arguments
and 'b commands = 'b command_arguments SMap.t


let rec get_block = function
  | Block b :: l -> Some (b, l)
  | Space :: l -> get_block l
  | _ -> None

let rec get_string = function
  | String str :: l -> Some (str, l)
  | Space :: l -> get_string l
  | _ -> None

let empty = List.for_all ((=) Space)

let rec interpret l = function
  | Arg_nil r -> Some (r, l)
  | Arg_cons (parse, ind) ->
    Option.bind (get_string l) (fun (str, l) ->
      Option.bind (parse str) (fun arg ->
        Option.bind (interpret l ind) (fun (f, l) ->
          (f arg, l))))
  | Arg_inner (parse, ind) ->
    Option.bind (get_block l) (fun (b, l) ->
      Option.bind (interpret b) (fun (arg, b) ->
        if empty b then
          Option.bind (interpret l ind) (fun (f, l) ->
            (f arg, l))
        else None))
  | Arg_block (commands, strings, ind) ->
    Option.bind (get_block l) (fun (b, l) ->
      Option.bind (parse commands strings b) (fun arg ->
        Option.bind (interpret l ind) (fun (f, l) ->
          (f arg, l))))
and parse commands strings = function
  | Space :: l -> parse commands strings l
  | String str :: l -> strings str :: parse commands strings l
  | Block b :: l ->
    parse commands strings b @ parse commands strings l
  | Command c :: l ->
    match SMap.find_opt c commands with
    | None -> failwith ("Invalid command " ^ c ^ " at this place.")
    | Some args ->
      match interpret l args with
      | None -> failwith ("Error while dealing with command " ^ c ^ ".")
      | Some (a, l) ->
        a :: parse commands strings l

let parse_state =
  parse (SMap.of_seq (List.to_seq [
      ("paragraph", ) ;
    ])) (fun _ -> failwith "No strings are allowed at top-level.")

let rec parse =
  | [] -> (* TODO *)
  | Command "paragraph" :: l ->
    unsome_or_error "Block expected after \\paragraph." (get_block l) (fun (b, l) ->
      (* TODO *))
  | Command "code" :: l -> (* TODO *)
    unsome_or_error "Block expected after \\code." (get_block l) (fun (b, l) ->
      (* TODO *))
  | Command "set" :: l
    unsome_or_error "Block expected after \\code." (get_block l) (fun (b1, l) ->
      unsome_or_error "Two blocks expected after \\code." (get_block l) (fun (b2, l) ->
        (* TODO *)))


