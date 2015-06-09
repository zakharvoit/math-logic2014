%{
module O = Ordinal
%}

%token <string> TNum
%token TOpenPar TClosePar TEq TAdd TSub TMul TExp TOmega TEof
%left TEq
%left TAdd TSub
%left TMul
%right TExp
%start comparison
%type <bool> comparison
%%

comparison:
        ordinal TEq ordinal TEof { (* print_endline (O.to_str $1); print_endline (O.to_str $3); *) $1 = $3 }
    ;

ordinal: TNum                    { O.Nat (int_of_string $1) }
    | TOpenPar ordinal TClosePar { $2 }
    | ordinal TAdd ordinal       { O.add $1 $3 }
    | ordinal TSub ordinal       { O.sub $1 $3 }
    | ordinal TMul ordinal       { O.mul $1 $3 }
    | ordinal TExp ordinal       { O.exp $1 $3 }
    | TOmega                     { O.omega }
    ;
