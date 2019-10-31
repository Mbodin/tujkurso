%{

%}

%token         EOF
%token<string> STRING
%token<string> COMMAND
%token         SPACE
%token         LBRACE RBRACE

%start<??> main

%%

main:
  | c = content; EOF    { c }

content:
  | l = list (item)     { l }

item:
  | list (SPACE)                { String " " }
  | str = STRING                { String c }
  | c = COMMAND                 { Command c }
  | LBRACE; b = content; RBRACE { Block b }

