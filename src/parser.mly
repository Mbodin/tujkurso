%{

%}

%token         EOF
%token<string> STRING
%token<string> COMMAND
%token         SPACE
%token         LBRACE RBRACE

%start<Driver.ast> main

%%

main:
  | c = content; EOF    { c }

content:
  | l = list (item)     { l }

item:
  | str = string                { Driver.String str }
  | c = COMMAND                 { Driver.Command c }
  | LBRACE; b = content; RBRACE { Driver.Block b }

string:
  | l = nonempty_list (string_item) { String.concat "" l }

string_item:
  | SPACE        { " " }
  | str = STRING { str }

