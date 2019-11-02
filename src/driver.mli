
type item =
  | String of string
  | Command of string
  | Block of ast
and ast = item list

type state

val parse : ast -> state

