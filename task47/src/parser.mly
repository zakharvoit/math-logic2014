%{
    module E = Arithmetic
%}

%token <string> TVar
%token <string> TPredicate
%token TNot TAnd TOr TImpl
%token TForall TExists
%token TComma TTourniquet
%token TPlus TMul TQuote
%token TZero TEqual
%token TOpenPar TClosePar TEoln
%left TTourniquet
%left TComma
%right TImpl
%left TOr
%left TAnd
%left TPlus
%left TMul
%nonassoc TNot
%start expr_line
%start assumptions_line
%type <Arithmetic.expression list * Arithmetic.expression> assumptions_line
%type <Arithmetic.expression> expr_line
%%

assumptions_line: TTourniquet expr TEoln { ([], $2) }
   | csexprs TTourniquet expr TEoln { ($1, $3) }
   ;

csexprs: expr                { [$1] }
       | csexprs TComma expr { $3 :: $1 }
       ;

expr_line: expr TEoln { $1 }
    ;
  
term: TOpenPar term TClosePar { $2 }
    | TVar                    { E.Var $1 }
    | TZero                   { E.Zero }
    | term TQuote             { E.Succ $1 }
    | term TPlus term         { E.Plus ($1, $3) }
    | term TMul term          { E.Mul ($1, $3) }
    | TVar TOpenPar csterms TClosePar { E.Function ($1, List.rev $3) } 
    ;

expr: expr TAnd expr              { E.And ($1, $3) }
    | expr TOr expr               { E.Or ($1, $3) }
    | expr TImpl expr             { E.Impl ($1, $3) }
    | unary                       { $1 }
    ;

unary: TOpenPar expr TClosePar              { $2 }
    | TNot unary                            { E.Not $2 }
    | TPredicate TOpenPar csterms TClosePar { E.Predicate ($1, List.rev $3) }
    | TPredicate                            { E.PVar $1 }
    | TForall TVar unary                    { E.Forall ($2, $3) }
    | term TEqual term                      { E.Predicate ("=", [$1 ; $3])}
    | TExists TVar unary                    { E.Exists ($2, $3) }

csterms: term             { [$1] }
    | csterms TComma term { $3 :: $1 }
    ;