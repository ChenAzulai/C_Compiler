%{
	#include <stdio.h>
	#include <stdlib.h>
	#include <string.h>
	#include "AST.h"
	#include "Scope.h"


	int yylex();
	int yyerror(char *e);

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
%token <string>	QUOTE DOUBLE_QUOTES BEGIN_COMMENT END_COMMENT 

%left DIFF SMALLER SMALL_EQ BIG_EQ BIGGER OR AND IS_EQ
%left PLUS MINUS RETURN
%left MUL DIV
%left SEMICOL EQUAL 
%right EX_MARK CLOSE_CURLY

%nonassoc IDEN 
%nonassoc OPEN_ROUND
%nonassoc IF
%nonassoc ELSE 


%type <node> address_expr stmnts stmnt_block derefrence_expr expr_list call_func 
%type <node> expr lhs assmnt_stmnt new_block 
%type <node> stmnt type_pro type_id var_id declear paren_expr
%type <node> pro_body para_list para_pro procedure procedures
%type <node>  program project declares 
%%

project: cmmnt program { syntaxAnalyzer($2,globalScope);}; 

program: procedures {$$=mkNode("CODE",$1,NULL);};
cmmnt: COMMENT cmmnt {;}| ;

procedures: procedures  procedure {$$=mkNode("",$1,$2);}
	| {$$=NULL;};

procedure: FUNC IDEN OPEN_ROUND para_pro CLOSE_ROUND cmmnt RETURN type_pro  OPEN_CURLY  pro_body CLOSE_CURLY
{$$=mkNode("FUNC",mkNode($2,mkNode(" ",NULL,NULL),mkNode("ARGS",$4,mkNode("Return",$8,NULL))),mkNode("",$10,NULL));}
| PROC IDEN OPEN_ROUND para_pro CLOSE_ROUND  OPEN_CURLY  pro_body CLOSE_CURLY
{$$=mkNode("PROC",mkNode($2,mkNode("",NULL,NULL),NULL),mkNode("ARGS",$4,$7));};

para_pro: para_list {$$=$1;}
	| {$$=NULL;};


para_list: var_id COL type_id {$$=mkNode("(",$3,mkNode("",$1,mkNode(")",NULL,NULL)));}
	|  para_list SEMICOL cmmnt  para_list {$$=mkNode("",$1,mkNode("",$4,NULL));}	;

pro_body: cmmnt  procedures declares stmnts 
{
	$$=mkNode("BODY", mkNode(" ",$2,NULL),mkNode(" ",$3,mkNode(" ",$4,mkNode(" ",NULL,NULL))));
};


declares: declares declear  {$$=mkNode("",$1,$2);} | {$$=NULL;}  ;
 
declear: VAR var_id COL type_id cmmnt SEMICOL cmmnt
{
	$$=mkNode("var", $4,$2);
};

var_id: IDEN COMMA var_id {$$=mkNode($1, mkNode(" ", $3, NULL),NULL);}
	| IDEN {$$=mkNode($1, NULL, NULL);} ;

type_id: BOOL {$$=mkNode("boolean", NULL, NULL);}
	| STRING OPEN_SQUARE NUM CLOSE_SQUARE {$$=mkNode("string", mkNode("[",mkNode("$3",NULL,NULL),NULL), NULL);}
	| CHAR {$$=mkNode("char", NULL, NULL);}
	| INT {$$=mkNode("int", NULL, NULL);}
	| REAL {$$=mkNode("real", NULL, NULL);}
	| INT_P {$$=mkNode("int*", NULL, NULL);}
	| CHAR_P {$$=mkNode("char*", NULL, NULL);}
	| REAL_P {$$=mkNode("real*", NULL, NULL);};



type_pro: BOOL {$$=mkNode("boolean", NULL, NULL);}
 	| STRING {$$=mkNode("string", NULL, NULL);}
	| CHAR {$$=mkNode("char", NULL, NULL);}
	| INT {$$=mkNode("int", NULL, NULL);}
	| REAL {$$=mkNode("real", NULL, NULL);}
	| INT_P {$$=mkNode("int*", NULL, NULL);}
	| CHAR_P {$$=mkNode("char*", NULL, NULL);}
	| REAL_P {$$=mkNode("real*", NULL, NULL);};
	

stmnts: stmnts stmnt {$$=mkNode("",$1,$2);} 
	| {$$=NULL;};

stmnt_block: stmnt {$$=$1;}|declear {$$=$1;}|procedure {$$=$1;} 
	|SEMICOL  {$$=mkNode("",NULL,NULL);};

new_block: OPEN_CURLY procedures cmmnt declares stmnts CLOSE_CURLY cmmnt
{
	$$=mkNode("{",$2,mkNode("", $4,mkNode("", $5,("}",NULL,NULL))));
};


stmnt: IF OPEN_ROUND expr CLOSE_ROUND  stmnt_block 
{
	$$=mkNode("if",
	mkNode("(", $3, 
	mkNode(")",NULL,NULL)),$5);
}%prec IF
| IF OPEN_ROUND expr CLOSE_ROUND   stmnt_block    ELSE  stmnt_block  
{
	$$=mkNode("if-else",
	mkNode("", $3, 
	mkNode("",NULL,NULL)),
	mkNode("",$5,
	mkNode("",$7,NULL)));
}
| WHILE cmmnt OPEN_ROUND expr CLOSE_ROUND  stmnt_block  
{
	$$=mkNode("while",
	mkNode("(", $4, 
	mkNode(")",NULL,NULL)),$6);
}
| assmnt_stmnt SEMICOL cmmnt {$$=mkNode("",$1,NULL);}
| expr SEMICOL cmmnt {$$=$1;}
| RETURN expr SEMICOL cmmnt {$$=mkNode("return",$2,NULL);}
| new_block {$$=$1;};


assmnt_stmnt: lhs EQUAL expr 
{
	$$=mkNode("=",$1,$3);
};


lhs: IDEN OPEN_SQUARE expr CLOSE_SQUARE 
{
	$$=mkNode($1, mkNode("[",$3,mkNode("]",NULL,NULL)), NULL);
} 
| IDEN {$$=mkNode($1,NULL,NULL);}
| address_expr {$$=$1;}
| derefrence_expr{$$=$1;} ;

expr:  OPEN_ROUND expr CLOSE_ROUND {$$=mkNode("(",$2,mkNode(")",NULL,NULL));}
         |expr IS_EQ expr {$$=mkNode("==",$1,$3);}
	| expr DIFF expr {$$=mkNode("!=",$1,$3);}
	| expr BIG_EQ expr {$$=mkNode(">=",$1,$3);}
	| expr BIGGER expr {$$=mkNode(">",$1,$3);}
	| expr SMALL_EQ expr {$$=mkNode("<=",$1,$3);}
	| expr SMALLER expr {$$=mkNode("<",$1,$3);}
	| expr AND expr {$$=mkNode("&&",$1,$3);}
	| expr OR expr {$$=mkNode("||",$1,$3);}
	| expr PLUS expr {$$=mkNode("+",$1,$3);}
	| expr MINUS expr {$$=mkNode("-",$1,$3);}
	| expr MUL expr {$$=mkNode("*",$1,$3);}
	| expr DIV expr {$$=mkNode("/",$1,$3);}
	| EX_MARK expr {$$=mkNode("!",$2,NULL);}
	| address_expr {$$=$1;}
	| derefrence_expr {$$=$1;}
	| call_func cmmnt {$$=$1;}
	| NUM {$$=mkNode($1,mkNode("INT",NULL,NULL),NULL);}
	| HEX_NUM {$$=mkNode($1,mkNode("HEX", NULL, NULL),NULL);}
	| CONST_CHAR {$$=mkNode($1,mkNode("CHAR", NULL, NULL),NULL);}
	| REAL_NUM {$$=mkNode($1,mkNode("REAL", NULL, NULL),NULL);}
	| CONST_STRING {$$=mkNode($1,mkNode("STRING", NULL, NULL),NULL);}
	| FALSE {$$=mkNode($1,mkNode("BOOLEAN", NULL, NULL),NULL);}
	| TRUE {$$=mkNode($1,mkNode("BOOLEAN", NULL, NULL),NULL);}
	| SIZE IDEN SIZE 
	{
		$$=mkNode("|",
		mkNode($2,NULL,NULL),
		mkNode("|",NULL,NULL));
	}
	| IDEN OPEN_SQUARE expr CLOSE_SQUARE 
	{$$=mkNode("solovar",mkNode($1,mkNode("[",$3,mkNode("]",NULL,NULL)),NULL),NULL);}
	| IDEN {$$=mkNode("solovar",mkNode($1,NULL,NULL),NULL);}
	| NULL1 {$$=mkNode("null",NULL,NULL);};
address_expr: ADDS IDEN {$$=mkNode("&",mkNode($2,NULL,NULL),NULL);}
	| ADDS OPEN_ROUND IDEN CLOSE_ROUND {$$=mkNode("&",mkNode("(",mkNode($3,NULL,NULL),NULL),mkNode(")",NULL,NULL));}
	| ADDS IDEN OPEN_SQUARE expr CLOSE_SQUARE 
	{$$=mkNode("&", mkNode($2,mkNode("[",$4,mkNode("]",NULL,NULL)),NULL),NULL);}
	| ADDS OPEN_ROUND IDEN OPEN_SQUARE expr CLOSE_SQUARE CLOSE_ROUND 
	{
		$$=mkNode("&",
		mkNode("(", 
		mkNode($3,mkNode("[",$5,mkNode("]",NULL,NULL)),NULL)
		,mkNode(")",NULL,NULL)),NULL);
	};


derefrence_expr: POINTER IDEN {$$=mkNode("^",mkNode($2,NULL,NULL),NULL);};

expr_list: expr COMMA expr_list {$$=mkNode("",$1,mkNode(",",$3,NULL));} 
	| expr {$$=mkNode("",$1,NULL);}
	| {$$=NULL;};

paren_expr:OPEN_ROUND expr_list CLOSE_ROUND {$$=$2;}; 
call_func: IDEN paren_expr {$$=mkNode("Call func",mkNode($1,NULL,NULL),mkNode("ARGS",$2,NULL));} ;
%%
#include "lex.yy.c"

int main()
{
	int res = yyparse();
	//printf("Res:%d \n", res);
	//printf("Main Check: %d \n",AdditionalMain);
	if(res==0&&AdditionalMain==1)
	{
	printf("Syntax & Semantic Checked-OK!\nProgram Works!\n"); 
	}
	else if(AdditionalMain==0)
	{
		printf("Syntax Error: proc Main() was not declared in the Code! \n");
		exit(1);
	}
	else if(AdditionalMain==2)
	{
		printf("Syntax Error: Allowed only One and only proc Main() in the Code! \n");
		exit(1);
	}
	return res;	
}


int yyerror(char *e)
{
	int yydebug=1;
	fflush(stdout);
	fprintf(stderr,"%s: Not Accapted: '%s' at line %d! \n" ,e,yytext,yylineno);
	
	return 0;
}







