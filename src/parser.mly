%{
    module E = Expression
%}

%token <string> TVar
%token TNot TAnd TOr TImpl
%token TComma TTourniquet
%token TOpenPar TClosePar TEoln
%left TTourniquet
%left TComma
%right TImpl
%left TOr
%left TAnd
%nonassoc TNot
%start expr_line
%start assumptions_line
%type <Expression.expression list * Expression.expression> assumptions_line
%type <Expression.expression> expr_line
%%

assumptions_line: csexprs TTourniquet expr TEoln { ($1, $3) }
    ;

csexprs: expr                { [ $1 ] }
       | csexprs TComma expr { $3 :: $1 }
       ;

expr_line: expr TEoln { $1 }
    ;
  
expr: TVar                    { E.Var $1 }
    | TOpenPar expr TClosePar { $2 }
    | TNot expr               { E.Not $2 }
    | expr TAnd expr          { E.And ($1, $3) }
    | expr TOr expr           { E.Or ($1, $3) }
    | expr TImpl expr         { E.Impl ($1, $3) }
    ;
