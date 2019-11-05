
module SMap = Map.Make(String)

type item =
  | String of string
  | Space
  | Command of string
  | Block of ast
and ast = item list (* TODO: Add positions to help creating nicer error messages. *)

(* TODO: Instead of [None], use an error monad. *)

type _ command_arguments =
  | Arg_nil : 'b -> 'b command_arguments
  | Arg_list : 'a command_arguments * ('a list -> 'b) -> 'b command_arguments
  | Arg_cons : (string -> 'a option) * ('a -> 'b) command_arguments -> 'b command_arguments
  | Arg_inner : 'a command_arguments * ('a -> 'b) command_arguments -> 'b command_arguments
  | Arg_block : 'a commands * (string -> 'a option) * ('a list -> 'b) command_arguments -> 'b command_arguments
and 'b commands = 'b command_arguments SMap.t

let rec apply (args : 'a command_arguments) (f : 'a -> 'b) : 'b command_arguments =
  match args with
  | Arg_nil r ->
    Arg_nil (f r)
  | Arg_list (parse, merge) ->
    Arg_list (parse, (fun l -> f (merge l)))
  | Arg_cons (parse, ind) ->
    Arg_cons (parse, apply ind (fun f' x -> f (f' x)))
  | Arg_inner (parse, ind) ->
    Arg_inner (parse, apply ind (fun f' x -> f (f' x)))
  | Arg_block (commands, strings, ind) ->
    Arg_block (commands, strings, apply ind (fun f' x -> f (f' x)))

let rec get_block = function
  | Block b :: l -> Some (b, l)
  | Space :: l -> get_block l
  | _ -> None

let rec to_string =
  let rec aux acc = function
    | [] -> Some (String.concat "" (List.rev acc))
    | Block b :: l ->
      Option.bind (to_string b) (fun b ->
        aux (b :: acc) l)
    | Command _ :: _ -> None
    | String str :: l -> aux (str :: acc) l
    | Space :: l ->
      match acc with
      | " " :: _ -> aux acc l
      | _ -> aux (" " :: acc) l in
  aux []

let empty = List.for_all ((=) Space)

let rec interpret l = function
  | Arg_nil r -> Some (r, l)
  | Arg_list (parse, merge) ->
    let rec aux acc = function
      | Space :: l -> aux acc l
      | Block b :: l ->
        Option.bind (interpret b parse) (fun (r, l) ->
          aux (r :: acc) l)
      | l -> Some (merge (List.rev acc), l) in
    aux [] l
  | Arg_cons (parse, ind) ->
    Option.bind (get_block l) (fun (str, l) ->
      Option.bind (to_string str) (fun str ->
        Option.bind (parse str) (fun arg ->
          Option.bind (interpret l ind) (fun (f, l) ->
            (f arg, l)))))
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
  | String str :: l ->
    Option.bind (strings str) (fun a ->
      Option.bind (parse commands strings l) (fun l ->
        Some (a :: l)))
  | Block b :: l ->
    Option.bind (parse commands strings b) (fun b ->
      Option.bind (parse commands strings l) (fun l ->
        Some (b @ l)))
  | Command c :: l ->
    Option.bind (SMap.find_opt c commands) (fun args ->
      Option.bind (interpret l args) (fun (a, l) ->
        Option.bind (parse commands strings l) (fun l ->
          Some (a :: l))))

let todo _ = failwith "TBI" (* TODO *)

let code =
  Arg_block (Arg_nil (todo ()), (fun _ -> None),
    Arg_nil (fun commands -> todo ()))

let picture =
  Arg_cons ((fun link -> todo ()), Arg_nil (fun link -> todo ()))

let pattern ind =
  Arg_cons ((fun name -> todo ()),
    Arg_list
      (Arg_cons ((fun case_name -> todo ()), apply ind (fun block case_name -> todo ())),
       (fun cases -> todo ())))

let paragraph style =
  let rec ind =
    Arg_block (SMap.of_seq (List.to_seq [
        ("match", pattern ind) ;
        ("alternative", Arg_list ind) ;
        ("code", code) ;
        ("picture", picture) ;
      ]), (fun str -> Some (State.String str)), Arg_nil (fun items -> items)) in
  apply ind (fun items -> (style, items))

let parse_state =
  parse (SMap.of_seq (List.to_seq [
      ("paragraph", paragraph State.Paragraph) ;
      ("example", paragraph State.Example) ;
      ("information", paragraph State.Information) ;
      ("code", code) ;
      ("picture", picture) ;
    ])) (fun _ -> None)

