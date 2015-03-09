{
  open Parser
}

let whitespace = [' ' '\t' '\r']
let variable   = ['A'-'Z']['A'-'Z' '0'-'9']*

rule token = parse
             | whitespace       { token lexbuf }
             | variable as var  { TVar var }
             | '\n'             { TEoln }
             | '('              { TOpenPar }
             | ')'              { TClosePar }
             | '!'              { TNot }
             | '&'              { TAnd }
             | ','              { TComma }
             | "|-"             { TTourniquet }
             | '|'              { TOr }
             | "->"             { TImpl }
             | eof              { raise End_of_file }
