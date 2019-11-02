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
  | SPACE                       { Driver.Space }
  | str = STRING                { Driver.String str }
  | c = COMMAND                 { Driver.Command c }
  | LBRACE; b = content; RBRACE { Driver.Block b }

