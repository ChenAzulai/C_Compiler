%{
	#include <stdio.h>
	#include <stdlib.h>
	#include <string.h>
	#include "AST.h"
	#include "Scope.h"


	int yylex();
	int yyerror(char *);
	void Check(int);

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
%left SEMICOL EQUAL 
%right EX_MARK CLOSE_CURLY

%nonassoc IDEN 
%nonassoc OPEN_ROUND
%nonassoc IF
%nonassoc ELSE 


%type <node> addsExp nestedStmt body_stmt pointerExp nestedExp function 
%type <node> elem values mathExp condition expr lhs nestedAssign body 
%type <node> statement typeOfVar typeStr typeSt nestedIden declare expBody
%type <node> procBody paramList paramProc procOrFunc nestedProc
%type <node>  code s nestedDec 

%%
s: cmt code { analayzeSyntax($2,globalScope);}; 

code: nestedProc {$$=mkNode("CODE",$1,NULL);};

nestedProc: nestedProc procOrFunc {$$=mkNode("",$1,$2);}
	| {$$=NULL;};

procOrFunc: FUNC IDEN OPEN_ROUND paramProc CLOSE_ROUND cmt RETURN typeStr  OPEN_CURLY  procBody CLOSE_CURLY 
{$$=mkNode("FUNC",mkNode($2,mkNode("(",NULL,NULL),mkNode("arguments",$4,mkNode("RETURN",$8,NULL))),mkNode("",$10,NULL));}
	| PROC IDEN OPEN_ROUND paramProc CLOSE_ROUND  OPEN_CURLY  procBody CLOSE_CURLY 
{$$=mkNode("PROC",mkNode($2,mkNode("(",NULL,NULL),NULL),mkNode("arguments",$4,$7));};

paramProc: paramList {$$=$1;}
	| {$$=NULL;};

function: IDEN expBody {$$=mkNode("callFunction",mkNode($1,NULL,NULL),mkNode("arguments",$2,NULL));} ;

paramList: nestedIden COL typeSt {$$=mkNode("(",$3,mkNode("",$1,mkNode(")",NULL,NULL)));}
	|  paramList SEMICOL cmt  paramList {$$=mkNode("",$1,mkNode("",$4,NULL));}	;

procBody: cmt nestedProc nestedDec nestedStmt {$$=mkNode("BODY", mkNode(" ",$2,NULL),mkNode(" ",$3,mkNode(" ",$4,mkNode(" ",NULL,NULL))));};

declare: VAR nestedIden COL typeSt cmt SEMICOL cmt {$$=mkNode("var", $4,$2);};

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

body: OPEN_CURLY nestedProc cmt nestedDec nestedStmt CLOSE_CURLY cmt {$$=mkNode("{",$2,mkNode("", $4,mkNode("", $5,("}",NULL,NULL))));};


statement: IF OPEN_ROUND expr CLOSE_ROUND  body_stmt {$$=mkNode("if",mkNode("(", $3,mkNode(")",NULL,NULL)),$5);}%prec IF
	| IF OPEN_ROUND expr CLOSE_ROUND body_stmt ELSE body_stmt {$$=mkNode("if-else",mkNode("", $3, mkNode("",NULL,NULL)),mkNode("",$5,mkNode("",$7,NULL)));}
	| WHILE cmt OPEN_ROUND expr CLOSE_ROUND  body_stmt {$$=mkNode("while",mkNode("(", $4,mkNode(")",NULL,NULL)),$6);}
	| nestedAssign SEMICOL cmt {$$=mkNode("",$1,NULL);}
	| expr SEMICOL cmt {$$=$1;}
	| RETURN expr SEMICOL cmt {$$=mkNode("return",$2,NULL);}
	| body {$$=$1;};


nestedAssign: lhs EQUAL expr {$$=mkNode("=",$1,$3);};


lhs: IDEN OPEN_SQUARE expr CLOSE_SQUARE {$$=mkNode($1, mkNode("[",$3,mkNode("]",NULL,NULL)), NULL);} 
	| IDEN {$$=mkNode($1,NULL,NULL);}
	| addsExp {$$=$1;}
	| pointerExp{$$=$1;} ;
condition:expr IS_EQ expr {$$=mkNode("==",$1,$3);}
	| expr DIFF expr {$$=mkNode("!=",$1,$3);}
	| expr BIG_EQ expr {$$=mkNode(">=",$1,$3);}
	| expr BIGGER expr {$$=mkNode(">",$1,$3);}
	| expr SMALL_EQ expr {$$=mkNode("<=",$1,$3);}
	| expr SMALLER expr {$$=mkNode("<",$1,$3);}
	| expr AND expr {$$=mkNode("&&",$1,$3);}
	| expr OR expr {$$=mkNode("||",$1,$3);}
	| mathExp {$$=$1;};

mathExp: expr PLUS expr {$$=mkNode("+",$1,$3);}
	| expr MINUS expr {$$=mkNode("-",$1,$3);}
	| expr MUL expr {$$=mkNode("*",$1,$3);}
	| expr DIV expr {$$=mkNode("/",$1,$3);};

values: NUM {$$=mkNode($1,mkNode("INT",NULL,NULL),NULL);}
	| HEX_NUM {$$=mkNode($1,mkNode("HEX", NULL, NULL),NULL);}
	| CONST_CHAR {$$=mkNode($1,mkNode("CHAR", NULL, NULL),NULL);}
	| REAL_NUM {$$=mkNode($1,mkNode("REAL", NULL, NULL),NULL);}
	| CONST_STRING {$$=mkNode($1,mkNode("STRING", NULL, NULL),NULL);};

elem: FALSE {$$=mkNode($1,mkNode("BOOLEAN", NULL, NULL),NULL);}
	| TRUE {$$=mkNode($1,mkNode("BOOLEAN", NULL, NULL),NULL);}
	| NULL1 {$$=mkNode("null",NULL,NULL);}
	| SIZE IDEN SIZE {$$=mkNode("|",mkNode($2,NULL,NULL),mkNode("|",NULL,NULL));}
	| IDEN OPEN_SQUARE expr CLOSE_SQUARE {$$=mkNode("solovar",mkNode($1,mkNode("[",$3,mkNode("]",NULL,NULL)),NULL),NULL);}
	| IDEN {$$=mkNode("solovar",mkNode($1,NULL,NULL),NULL);};

expr:  OPEN_ROUND expr CLOSE_ROUND {$$=mkNode("(",$2,mkNode(")",NULL,NULL));}
	| EX_MARK expr {$$=mkNode("!",$2,NULL);}
        | condition {$$=$1;}
	| values {$$=$1;}
	| addsExp {$$=$1;}
	| pointerExp {$$=$1;}
	| function cmt {$$=$1;}
	| elem {$$=$1;};

addsExp: ADDS IDEN {$$=mkNode("&",mkNode($2,NULL,NULL),NULL);}
	| ADDS OPEN_ROUND IDEN CLOSE_ROUND {$$=mkNode("&",mkNode("(",mkNode($3,NULL,NULL),NULL),mkNode(")",NULL,NULL));}
	| ADDS IDEN OPEN_SQUARE expr CLOSE_SQUARE {$$=mkNode("&", mkNode($2,mkNode("[",$4,mkNode("]",NULL,NULL)),NULL),NULL);}
	| ADDS OPEN_ROUND IDEN OPEN_SQUARE expr CLOSE_SQUARE CLOSE_ROUND {$$=mkNode("&",mkNode("(",mkNode($3,mkNode("[",$5,mkNode("]",NULL,NULL)),NULL),mkNode(")",NULL,NULL)),NULL);};

pointerExp: POINTER IDEN {$$=mkNode("^",mkNode($2,NULL,NULL),NULL);}
	| POINTER OPEN_ROUND mathExp CLOSE_ROUND {$$=mkNode("^",mkNode("(",$3,NULL),mkNode(")",NULL,NULL));};

nestedExp: expr COMMA nestedExp {$$=mkNode("",$1,mkNode(",",$3,NULL));} 
	| expr {$$=mkNode("",$1,NULL);}
	| {$$=NULL;};

expBody:OPEN_ROUND nestedExp CLOSE_ROUND {$$=$2;}; 

cmt: COMMENT cmt {;}
	| {;};

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
	fprintf(stderr,"%s: Not Accapted: '%s' at line %d! \n" ,error,yytext,yylineno);
	
	return 0;
}

void Check(int flag){
	if(flag==1)
		exit(1);
	else if(flag==0 &&AdditionalMain==1)
		printf("Syntax & Semantic Checked-OK!\n"); 
	else if(AdditionalMain==0){
		printf("Syntax Error: proc Main() was not declared in the Code! \n");
		exit(1);}
	else if(AdditionalMain==2){
		printf("Syntax Error: Allowed only One and only proc Main() in the Code! \n");
		exit(1);}
}
