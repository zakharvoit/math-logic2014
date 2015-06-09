{
  open Parser
}

let whitespace = [' ' '\t' '\r' '\n']
let number     = ['0'-'9']+

rule token = parse
             | whitespace       { token lexbuf }
             | number as num    { TNum num }
             | '('              { TOpenPar }
             | ')'              { TClosePar }
             | '+'              { TAdd }
             | '-'              { TSub }
             | '*'              { TMul }
             | '^'              { TExp }
             | 'w'              { TOmega }
             | '='              { TEq }
             | eof              { TEof }
