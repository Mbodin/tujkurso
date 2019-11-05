
type item =
  | Space
  | String of string
  | Command of string
  | Block of ast
and ast = item list

type state

val parse_state : ast -> State.t

