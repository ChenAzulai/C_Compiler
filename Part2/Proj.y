%{
	#include <stdio.h>
	#include <stdlib.h>
	#include <string.h>
	#include "AST.h"
	#include "Scope.h"
	
	int start=1;
	char st[100][10];
	int top=0,condition=0,or_c=-1;
	char i_l[2]="0";
	char i_l1[2]="0";
	char temp[2] = "t";
	int label[20];
	int lnum=0,ltop=0;
	int tempTOP;
	int pos;
	int yylex();
	int yyerror(char *);

%}

%union
{
    struct node *node;
    char *string;
}

%token <string> COMMENT WHILE IF ELSE 
%token <string> RETURN
%token <string> BOOL STRING CHAR_P CHAR INT INT_P PROC
%token <string> AND ADDS IS_EQ EQUAL OR SIZE BIG_EQ BIGGER SMALL_EQ SMALLER DIFF EX_MARK
%token <string> DIV PLUS MINUS MUL VAR
%token <string> CONST_STRING REAL_NUM CONST_CHAR NULL1
%token <string> MAIN IDEN SEMICOL COMMA OPEN_ROUND CLOSE_ROUND OPEN_SQUARE CLOSE_SQUARE OPEN_CURLY CLOSE_CURLY
%token <string> NUM HEX_NUM TRUE FALSE  REAL REAL_P FUNC COL  POINTER 
%token <string> QUOTE DOUBLE_QUOTES BEGIN_COMMENT END_COMMENT 

%left DIFF SMALLER SMALL_EQ BIG_EQ BIGGER OR AND IS_EQ
%left MINUS PLUS RETURN
%left MUL DIV
%left SEMICOL  
%right EX_MARK CLOSE_CURLY EQUAL

%nonassoc IDEN 
%nonassoc OPEN_ROUND
%nonassoc IF
%nonassoc ELSE 


%type <node> addsExp nestedStmt body_stmt pointerExp nestedExp function 
%type <node> elem values mathExp condition expr lhs nestedAssign body else_expr if_expr and_or_expr
%type <node> statement typeOfVar typeStr typeSt nestedIden declare expBody
%type <node> procBody paramList paramProc procOrFunc nestedProc
%type <node>  code s nestedDec 

%%
s: code {analayzeSyntax($1,globalScope);}; 

code: nestedProc {$$=mkNode("CODE",$1,NULL);};

nestedProc: nestedProc procOrFunc {$$=mkNode("",$1,$2);}
	| {$$=NULL;};

procOrFunc: FUNC IDEN{printf("%s:\n  BeginFunc\n",yytext=yylval.string);} OPEN_ROUND paramProc CLOSE_ROUND RETURN typeStr  OPEN_CURLY  procBody CLOSE_CURLY
{
lab1_Return();
printf("  EndFunc\n\n");}
{$$=mkNode("FUNC",mkNode($2,mkNode("(",NULL,NULL),mkNode("arguments",$5,mkNode("RETURN",$8,NULL))),mkNode("",$10,NULL));}
| PROC IDEN{printf("%s:\n  BeginFunc\n",yytext=yylval.string);} OPEN_ROUND paramProc CLOSE_ROUND  OPEN_CURLY  procBody CLOSE_CURLY
{$$=mkNode("PROC",mkNode($2,mkNode("(",NULL,NULL),NULL),mkNode("arguments",$5,$8));

if(!(strcmp($2,"Main")))
printf("L%d: EndFunc\n\n",lnum);
if(strcmp($2,"Main"))
printf("  EndFunc\n\n");
};

paramProc: paramList {$$=$1;}
	| {$$=NULL;};

function: IDEN expBody {$$=mkNode("callFunction",mkNode($1,NULL,NULL),mkNode("arguments",$2,NULL));codegen_func();printf("  %s = LCall %s\n",temp,$1);};

paramList: nestedIden COL typeSt {$$=mkNode("(",$3,mkNode("",$1,mkNode(")",NULL,NULL)));}
	|  paramList SEMICOL paramList {$$=mkNode("",$1,mkNode("",$3,NULL));}	;

procBody: nestedProc nestedDec nestedStmt {$$=mkNode("BODY", mkNode(" ",$1,NULL),mkNode(" ",$2,mkNode(" ",$3,mkNode(" ",NULL,NULL))));};

declare: VAR nestedIden COL typeSt SEMICOL {$$=mkNode("var", $4,$2);};

nestedDec: nestedDec declare  {$$=mkNode("",$1,$2);} 
	| {$$=NULL;}  ;
 

nestedIden: IDEN COMMA nestedIden {$$=mkNode($1, mkNode(" ", $3, NULL),NULL);}
	| IDEN {$$=mkNode($1, NULL, NULL);} ;
 
typeOfVar:BOOL {$$=mkNode("bool", NULL, NULL);}
	| INT {$$=mkNode("int", NULL, NULL);}
	| REAL {$$=mkNode("real", NULL, NULL);}
	| CHAR {$$=mkNode("char", NULL, NULL);}
 	| INT_P {$$=mkNode("int*", NULL, NULL);}
	| REAL_P {$$=mkNode("real*", NULL, NULL);}
	| CHAR_P {$$=mkNode("char*", NULL, NULL);};

typeSt: typeOfVar {$$=$1;}
	| STRING OPEN_SQUARE NUM CLOSE_SQUARE {$$=mkNode("string", mkNode("[",mkNode("$3",NULL,NULL),NULL), NULL);};

typeStr: typeOfVar {$$=$1;}
	| STRING {$$=mkNode("string", NULL, NULL);};

nestedStmt: nestedStmt statement {$$=mkNode("",$1,$2);} 
	| {$$=NULL;};

body_stmt: statement {$$=$1;}
	|declare {$$=$1;}
	|procOrFunc {$$=$1;} 
	|SEMICOL  {$$=mkNode("",NULL,NULL);};

body: OPEN_CURLY nestedProc nestedDec nestedStmt CLOSE_CURLY {$$=mkNode("{",$2,mkNode("", $3,mkNode("", $4,("}",NULL,NULL))));};


statement: IF OPEN_ROUND expr CLOSE_ROUND {lab1_IF();lab2_or();} if_expr {$$=mkNode("if",mkNode("(", $3,mkNode(")",NULL,NULL)),$6);}%prec IF;
	| WHILE {lab1_WHILE();} OPEN_ROUND expr CLOSE_ROUND {lab2_WHILE();} body_stmt {$$=mkNode("while",mkNode("(", $4,mkNode(")",NULL,NULL)),$7);lab3_WHILE();}
	| nestedAssign SEMICOL {$$=mkNode("",$1,NULL);}
	| expr SEMICOL {$$=$1;}
	| RETURN expr SEMICOL {$$=mkNode("return",$2,NULL);}
	| body {$$=$1;};

if_expr: body_stmt {lab2_IF();} ELSE else_expr {$$=mkNode("",$1,$4);}
	| body_stmt {$$=mkNode("",$1,NULL);lab3_IF();};

else_expr: body_stmt {$$=mkNode("",$1,NULL);lab3_IF();};

nestedAssign: lhs {pos=top;} EQUAL {yytext="=";push();} expr {$$=mkNode("=",$1,$5);

	int tokenFlag=!strcmp($5->token,"callFunction");
	if(tokenFlag)
		printf("  %s = %s\n",st[pos],st[top]);
	else	 
		codegen_assign();};

lhs: IDEN OPEN_SQUARE expr CLOSE_SQUARE {$$=mkNode($1, mkNode("[",$3,mkNode("]",NULL,NULL)), NULL);} 
	| IDEN {$$=mkNode($1,NULL,NULL);yytext=yylval.string;push();}
	| addsExp {$$=$1;}
	| pointerExp{$$=$1;} ;

condition:expr IS_EQ {yytext="==";push();} expr {$$=mkNode("==",$1,$4);codegen();}
	| expr DIFF{yytext="!=";push();}  expr {$$=mkNode("!=",$1,$4);codegen();}
	| expr BIG_EQ {yytext=">=";push();} expr {$$=mkNode(">=",$1,$4);codegen();}
	| expr BIGGER {yytext=">";push();} expr {$$=mkNode(">",$1,$4); codegen();}
	| expr SMALL_EQ {yytext="<=";push();}  expr {$$=mkNode("<=",$1,$4);codegen();}
	| expr SMALLER {yytext="<";push();}  expr {$$=mkNode("<",$1,$4);codegen();}
	| and_or_expr {$$=$1;}
	| mathExp {$$=$1;};

and_or_expr:expr AND {lab1_IF();} expr  {$$=mkNode("&&",$1,$4);}
	| expr OR {lab1_or();} expr {$$=mkNode("||",$1,$4);};

mathExp: expr PLUS {yytext="+";push();} expr {$$=mkNode("+",$1,$4);codegen();}
	| expr MINUS {yytext="-";push();} expr {$$=mkNode("-",$1,$4);codegen();}
	| expr MUL {yytext="*";push();} expr {$$=mkNode("*",$1,$4);codegen();}
	| expr DIV {yytext="/";push();} expr {$$=mkNode("/",$1,$4);codegen();};


values: NUM {$$=mkNode($1,mkNode("INT_NUM",NULL,NULL),NULL);push();}
	| HEX_NUM {$$=mkNode($1,mkNode("HEX_NUM", NULL, NULL),NULL);}
	| CONST_CHAR {$$=mkNode($1,mkNode("CONST_CHAR", NULL, NULL),NULL);}
	| REAL_NUM {$$=mkNode($1,mkNode("REAL_NUM", NULL, NULL),NULL);}
	| CONST_STRING {$$=mkNode($1,mkNode("CONST_STRING", NULL, NULL),NULL);};

elem: FALSE {$$=mkNode($1,mkNode("T_F_BOOLEAN", NULL, NULL),NULL);push();}
	| TRUE {$$=mkNode($1,mkNode("T_F_BOOLEAN", NULL, NULL),NULL);push();}
	| NULL1 {$$=mkNode("null",NULL,NULL);}
	| SIZE IDEN SIZE {$$=mkNode("|",mkNode($2,NULL,NULL),mkNode("|",NULL,NULL));}
	| IDEN OPEN_SQUARE expr CLOSE_SQUARE {$$=mkNode("SingleVariable",mkNode($1,mkNode("[",$3,mkNode("]",NULL,NULL)),NULL),NULL);}
	| IDEN {$$=mkNode("SingleVariable",mkNode($1,NULL,NULL),NULL);yytext=yylval.string;push();};

expr:  OPEN_ROUND expr CLOSE_ROUND {$$=mkNode("(",$2,mkNode(")",NULL,NULL));}
	| EX_MARK expr {$$=mkNode("!",$2,NULL);}
     | condition {$$=$1;}
	| values {$$=$1;}
	| addsExp {$$=$1;}
	| pointerExp {$$=$1;}
	| function {$$=$1;}
	| elem {$$=$1;};

addsExp: ADDS IDEN {$$=mkNode("&",mkNode($2,NULL,NULL),NULL);}
	| ADDS OPEN_ROUND IDEN CLOSE_ROUND {$$=mkNode("&",mkNode("(",mkNode($3,NULL,NULL),NULL),mkNode(")",NULL,NULL));}
	| ADDS IDEN OPEN_SQUARE expr CLOSE_SQUARE {$$=mkNode("&", mkNode($2,mkNode("[",$4,mkNode("]",NULL,NULL)),NULL),NULL);}
	| ADDS OPEN_ROUND IDEN OPEN_SQUARE expr CLOSE_SQUARE CLOSE_ROUND {$$=mkNode("&",mkNode("(",mkNode($3,mkNode("[",$5,mkNode("]",NULL,NULL)),NULL),mkNode(")",NULL,NULL)),NULL);};

pointerExp: POINTER IDEN {$$=mkNode("^",mkNode($2,NULL,NULL),NULL);}
	| POINTER OPEN_ROUND mathExp CLOSE_ROUND {$$=mkNode("^",mkNode("(",$3,NULL),mkNode(")",NULL,NULL));};

nestedExp:expr COMMA nestedExp {$$=mkNode("NotEmpty",$1,mkNode(",",$3,NULL));
printf("  PushParam %s\n",st[top--]);} 
	| expr {$$=mkNode("NotEmpty",$1,NULL);printf("  PushParam %s\n",st[top--]);}
	|{$$=NULL;};

expBody:OPEN_ROUND nestedExp CLOSE_ROUND {$$=$2;}; 

%%
#include "lex.yy.c"

void main()
{
	int flag;
	flag=yyparse();
	Check(flag);
}

int yyerror(char *error)
{
	int yydebug=1;
	fflush(stdout);
	fprintf(stderr,"%s: Not Accapted: '%s' in line %d!\n" ,error,yytext,yylineno);
	
	return 0;
}

lab1_or()
{

	 //lnum++;
	 //strcpy(temp,"_t");
	 //strcat(temp,i_l);
	 //printf("%s = %s\n",temp,st[top]);
	 if (or_c == -1)
		or_c = lnum;
	 printf("  if %s goTo L%d\n",temp,lnum);
	 //i_l[0]++;
	 label[++ltop]=lnum;
	 condition++;
}

lab2_or()
{
	if (or_c>-1 && condition >= -1){
	 	printf("L%d: \n",or_c);
		if (condition==1)
			condition=0;
		else
			condition-=2;
	}
	//or_c=-2;
	
}

push()
{

	strcpy(st[++top], yytext);
}



codegen()
{	
	 strcpy(temp,"_t");
	 strcat(temp,i_l1);
	 strcat(temp,i_l);
	 printf("  %s = %s %s %s \n",temp,st[top-2],st[top-1],st[top]);
	 top-=2;
	 strcpy(st[top],temp);
	 increase_i_l();
}

codegen_assign()
{
	 printf("  %s = %s\n",st[top-2],st[top]);
	 top-=2;
	 
}

lab1_IF()
{
	 lnum++;
	 strcpy(temp,"_t");
	 strcat(temp,i_l1);
	 strcat(temp,i_l);
	 printf("  %s = not %s\n",temp,st[top]);
	 printf("  if%s goTo L%d\n",temp,lnum);
	 increase_i_l();
	 label[++ltop]=lnum;
	 condition++;
}



lab2_IF()
{
	int x;
	lnum++;
	x=label[ltop--];
	printf("GoTo L%d\n",lnum);
	printf("L%d: \n",x);
	label[++ltop]=lnum;
}

lab3_IF()
{
	int y,x;
	y=label[ltop--];
	printf("L%d: \n",y);
	condition--;

	while (condition>0 ){
		if (or_c!=y-1)
			printf("L%d: \n",--y);
		condition--;
		}

	or_c=-1;
}



lab1_WHILE()
{
	start=++lnum;
	printf("L%d: \n",lnum++);
}


lab2_WHILE()
{

	 strcpy(temp,"_t");
	 strcat(temp,i_l1);
	 strcat(temp,i_l);
	 printf("  %s = not %s\n",temp,st[top]);
	 printf("  if %s goto L%d\n",temp,lnum);
	 increase_i_l();
 }

lab3_WHILE()
{
printf("goto L%d \n",start);
printf("L%d: \n",lnum);
}

increase_i_l()
{
	if(!(strcmp(i_l,"9"))){
		strcpy(i_l,"0");
		i_l1[0]++;
	}
	else
		i_l[0]++;
}

lab1_Return()
{
printf("  Return %s\n",st[top]);
}


codegen_func(){

	 strcpy(temp,"_t");
	 strcat(temp,i_l1);
	 strcat(temp,i_l);
	 //printf("%s = %s %s %s \n",temp,st[top-2],st[top-1],st[top]);
	 //top-=2;
	 strcpy(st[top],temp);
	 increase_i_l();
}
