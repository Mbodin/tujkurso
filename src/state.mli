
type item =
  | String of string

type style =
  | Paragraph
  | Example
  | Information

type paragraph =
  style * item list

type t =
  paragraph list

