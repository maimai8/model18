%{
(* 補助的な変数、関数、型などの定義 *)
%}

/* 以降、どういうわけかコメントが C 式になることに注意 */
/* トークンの定義 */
%token LPAREN RPAREN
%token PLUS MINUS TIMES EQUAL NOTEQUAL LESS ORLESS MORE ORMORE
%token IF THEN ELSE
%token <int> NUMBER
/* これは、数字には int 型の値が伴うことを示している */
%token TRUE FALSE
%token EOF
/* End of File: 入力の終わりを示す */

/* 非終端記号の型をここで宣言する */
%type <Syntax.t> expr

/* 開始記号の定義 */
%start expr

/* 演算子の優先順位を指定する */
/* 下に行くほど強く結合する */
%nonassoc ELSE
%left EQUAL NOTEQUAL LESS ORLESS MORE ORMORE
%left PLUS MINUS
%left TIMES
%nonassoc UNARY
/* nonassoc は結合なし（毎回、かっこを書かなくてはならない）、
   left は左結合、right は右結合 */

/* 以下の %% は省略不可。それ以降に文法規則を書く */
%%

simple_expr:
| NUMBER
	{ Syntax.Number ($1) }
| TRUE
	{ Syntax.Bool (true) }
| FALSE
	{ Syntax.Bool (false) }
| LPAREN expr RPAREN
	{ $2 }

expr:
| simple_expr
	{ $1 }
| expr PLUS expr
	{ Syntax.Op ($1, Syntax.Plus, $3) }
| MINUS expr %prec UNARY
	{ Syntax.Op (Syntax.Number (0), Syntax.Minus, $2) }
| expr MINUS expr
	{ Syntax.Op ($1, Syntax.Minus, $3) }
| expr TIMES expr
	{ Syntax.Op ($1, Syntax.Times, $3) }
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
