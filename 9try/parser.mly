%{
(* 補助的な変数、関数、型などの定義 *)
%}

/* 以降、どういうわけかコメントが C 式になることに注意 */
/* トークンの定義 */
%token LPAREN RPAREN LLIST RLIST NIL CONS SEMI
%token TRUE FALSE
%token PLUS MINUS TIMES DIVIDE EQUAL NOTEQUAL LESS ORLESS MORE ORMORE
%token IF THEN ELSE
%token LET IN
%token FUN TO REC
%token MATCH WITH PIPE
%token RAISE ERROR TRY
%token <int> NUMBER
/* これは、数字には int 型の値が伴うことを示している */
%token <string> VARIABLE
/* 変数は、string 型の値が伴う */
%token EOF
/* End of File: 入力の終わりを示す */

/* 非終端記号の型をここで宣言する */
%type <Syntax.t> expr

/* 開始記号の定義 */
%start expr

/* 演算子の優先順位を指定する */
/* 下に行くほど強く結合する */
%nonassoc ELSE IN
%nonassoc TO
%left EQUAL NOTEQUAL LESS ORLESS MORE ORMORE
%right CONS
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UNARY
/* nonassoc は結合なし（毎回、かっこを書かなくてはならない）、
   left は左結合、right は右結合 */

/* 以下の %% は省略不可。それ以降に文法規則を書く */
%%

simple_expr:
| NUMBER
	{ Syntax.Number ($1) }
| VARIABLE
	{ Syntax.Variable ($1) }
| TRUE
	{ Syntax.Bool (true) }
| FALSE
	{ Syntax.Bool (false) }
| LPAREN expr RPAREN
	{ $2 }
| NIL
	{ Syntax.Nil }
| LLIST expr_list RLIST
	{ $2 }

expr_list:
| expr
	{ Syntax.Cons($1, Syntax.Nil) }
| expr SEMI expr_list
	{ Syntax.Cons($1, $3) }

expr_app:
| simple_expr simple_expr
	{ Syntax.App ($1, $2) }
| expr_app simple_expr
	{ Syntax.App ($1, $2) }

expr_fun:
| VARIABLE EQUAL expr
	{ Syntax.Fun($1, $3) }
| VARIABLE expr_fun
	{ Syntax.Fun($1, $2) }

expr:
| simple_expr
	{ $1 }
| expr CONS expr
	{ Syntax.Cons ($1, $3) }
| expr PLUS expr
	{ Syntax.Op ($1, Syntax.Plus, $3) }
| MINUS expr %prec UNARY
	{ Syntax.Op (Syntax.Number (0), Syntax.Minus, $2) }
| expr MINUS expr
	{ Syntax.Op ($1, Syntax.Minus, $3) }
| expr TIMES expr
	{ Syntax.Op ($1, Syntax.Times, $3) }
| expr DIVIDE expr
        { Syntax.Op ($1, Syntax.Devide, $3) }
| expr EQUAL expr
	{ Syntax.Op ($1, Syntax.Equal, $3) }
| expr NOTEQUAL expr
        { Syntax.Op ($1, Syntax.NotEqual, $3) }
| expr LESS expr
	{ Syntax.Op ($1, Syntax.Less, $3) }
| expr ORLESS expr
	{ Syntax.Op ($1, Syntax.OrLess, $3) }
| expr MORE expr
	{ Syntax.Op ($3, Syntax.Less, $1) }
| expr ORMORE expr
	{ Syntax.Op ($3, Syntax.OrLess, $1) }
| IF expr THEN expr ELSE expr
	{ Syntax.If ($2, $4, $6) }
| LET VARIABLE EQUAL expr IN expr
	{ Syntax.Let ($2, $4, $6) }
| LET VARIABLE expr_fun IN expr
	{ Syntax.Let ($2, $3, $5) }
| LET REC VARIABLE VARIABLE EQUAL expr IN expr
	{ Syntax.LetRec ($3, $4, $6, $8) }
| LET REC VARIABLE VARIABLE expr_fun IN expr
	{ Syntax.LetRec ($3, $4, $5, $7) }
| FUN VARIABLE TO expr
	{ Syntax.Fun ($2, $4) }
| expr_app
	{ $1 }
| MATCH expr WITH NIL TO expr PIPE VARIABLE CONS VARIABLE TO expr
	{ Syntax.Match ($2, $6, $8, $10, $12) }
| RAISE LPAREN ERROR expr RPAREN
        { Syntax.Raise ($4) }
| TRY expr WITH ERROR VARIABLE TO expr
        { Syntax.Try ($2, $5, $7) }
