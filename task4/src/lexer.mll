{
  open Parser
}

let whitespace = [' ' '\t' '\r']
let variable   = ['a'-'z']['0'-'9']*
let predicate  = ['A'-'Z']['0'-'9']*

rule token = parse
             | whitespace       { token lexbuf }
             | variable as var  { TVar var }
             | predicate as p   { TPredicate p }
             | '\n'             { TEoln }
             | '('              { TOpenPar }
             | ')'              { TClosePar }
             | '!'              { TNot }
             | '&'              { TAnd }
             | '|'              { TOr }
             | "->"             { TImpl }
             | "@"              { TForall }
             | "?"              { TExists }
             | "0"              { TZero }
             | "'"              { TQuote }
             | "+"              { TPlus }
             | "="              { TEqual }
             | "*"              { TMul }
             | ','              { TComma }
             | "|-"             { TTourniquet }
             | eof              { raise End_of_file }
