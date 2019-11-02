
type item =
  | String of string
  | Space
  | Command of string
  | Block of ast
and ast = item list

type state (* TODO *)

type 'b command_arguments =
  | Arg_nil : 'b -> 'b command_arguments
  | Arg_cons : (string -> 'a option) -> ('a -> 'b) command_arguments -> 'b command_arguments
  | Arg_block : 'a command_arguments -> ('a -> 'b) command_arguments -> 'b command_arguments

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
  | Arg_block (parse, ind) ->
    Option.bind (get_block l) (fun (b, l) ->
      Option.bind (interpret b) (fun (arg, b) ->
        if empty b then
          Option.bind (interpret l ind) (fun (f, l) ->
            (f arg, l))
        else None))

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


