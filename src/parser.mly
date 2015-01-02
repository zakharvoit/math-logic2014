%{
  module E = Expression
%}

%token <string> TVar
%token TNot TAnd TOr TImpl
%token TOpenPar TClosePar TEoln
%right TImpl
%left TOr
%left TAnd
%nonassoc TNot
%start expr_line
%type <Expression.expression> expr_line
%%

expr_line: expr TEoln { $1 };
  
expr: TVar                    { E.Var $1 }
    | TOpenPar expr TClosePar { $2 }
    | TNot expr               { E.Not $2 }
    | expr TAnd expr          { E.And ($1, $3) }
    | expr TOr expr           { E.Or ($1, $3) }
    | expr TImpl expr         { E.Impl ($1, $3) }
    ;
