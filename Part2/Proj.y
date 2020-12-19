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
%token <string> BOOL STRING CHARPTR CHAR INT INTPTR PROCEDUR
%token <string> AND ADDRESS EQL ASSINGMENT OR LENGTH GREATEREQL GREATER LESSEQL LESS NOTEQL NOT
%token <string> DIVISION PLUS MINUS MULTI VARIABLE
%token <string> STRING_LTL REAL_LTL CHAR_LTL NULLL
%token <string> MAIN IDENTIFIER SEMICOLON COMMA OPENPAREN CLOSEPAREN OPENBRACKET CLOSEBRACKET OPENBRACE CLOSEBRACE
%token <string> DECIMAL_LTL HEX_LTL BOOLTRUE BOOLFALSE  REAL REALPTR FUNCTION COLON  DEREFRENCE 
%token <string>	QUOTE DOUBLE_QUOTES BEGIN_COMMENT END_COMMENT 

%left NOTEQL LESS LESSEQL GREATEREQL GREATER OR AND EQL
%left PLUS MINUS RETURN
%left MULTI DIVISION
%left SEMICOLON ASSINGMENT 
%right NOT CLOSEBRACE

%nonassoc IDENTIFIER 
%nonassoc OPENPAREN
%nonassoc IF
%nonassoc ELSE 


%type <node> address_expr stmnts stmnt_block derefrence_expr expr_list call_func 
%type <node> expr lhs assmnt_stmnt new_block 
%type <node> stmnt type_pro type_id var_id declear paren_expr
%type <node> pro_body para_list para_pro procedure procedures
%type <node>  program project declares 
%%

project: cmmnt program { syntaxAnalyzer($2,globalScope);}; 

program: procedures {$$=mkNode("CODE",$1,NULL);}

cmmnt: COMMENT cmmnt {;}| ;

procedures: procedures  procedure {$$=mkNode("",$1,$2);}
	| {$$=NULL;};

procedure: FUNCTION IDENTIFIER OPENPAREN para_pro CLOSEPAREN cmmnt RETURN type_pro  OPENBRACE  pro_body CLOSEBRACE
{ 
		$$=mkNode("FUNC",mkNode($2,mkNode(" ",NULL,NULL),mkNode("ARGS",$4,mkNode("Return",$8,NULL))),mkNode("",$10,NULL));

}
| PROCEDUR IDENTIFIER OPENPAREN para_pro CLOSEPAREN  OPENBRACE  pro_body CLOSEBRACE
{
	$$=mkNode("PROC",mkNode($2,mkNode("",NULL,NULL),NULL),mkNode("ARGS",$4,$7));
};

para_pro: para_list {$$=$1;}

| {$$=NULL;};


para_list: var_id COLON type_id {$$=mkNode("(",$3,mkNode("",$1,mkNode(")",NULL,NULL)));}
	|  para_list SEMICOLON cmmnt  para_list {$$=mkNode("",$1,mkNode("",$4,NULL));}	;

pro_body: cmmnt  procedures declares stmnts 
{
	$$=mkNode("BODY", mkNode(" ",$2,NULL),mkNode(" ",$3,mkNode(" ",$4,mkNode(" ",NULL,NULL))));
};


declares: declares declear  {$$=mkNode("",$1,$2);} | {$$=NULL;}  ;
 
declear: VARIABLE var_id COLON type_id cmmnt SEMICOLON cmmnt
{
	$$=mkNode("var", $4,$2);
};

var_id: IDENTIFIER COMMA var_id {$$=mkNode($1, mkNode(" ", $3, NULL),NULL);}
	| IDENTIFIER {$$=mkNode($1, NULL, NULL);} ;

type_id: BOOL {$$=mkNode("boolean", NULL, NULL);}
	| STRING OPENBRACKET DECIMAL_LTL CLOSEBRACKET {$$=mkNode("string", mkNode("[",mkNode("$3",NULL,NULL),NULL), NULL);}
	| CHAR {$$=mkNode("char", NULL, NULL);}
	| INT {$$=mkNode("int", NULL, NULL);}
	| REAL {$$=mkNode("real", NULL, NULL);}
	| INTPTR {$$=mkNode("int*", NULL, NULL);}
	| CHARPTR {$$=mkNode("char*", NULL, NULL);}
	| REALPTR {$$=mkNode("real*", NULL, NULL);};



type_pro: BOOL {$$=mkNode("boolean", NULL, NULL);}
 	| STRING {$$=mkNode("string", NULL, NULL);}
	| CHAR {$$=mkNode("char", NULL, NULL);}
	| INT {$$=mkNode("int", NULL, NULL);}
	| REAL {$$=mkNode("real", NULL, NULL);}
	| INTPTR {$$=mkNode("int*", NULL, NULL);}
	| CHARPTR {$$=mkNode("char*", NULL, NULL);}
	| REALPTR {$$=mkNode("real*", NULL, NULL);};
	

stmnts: stmnts stmnt {$$=mkNode("",$1,$2);} | {$$=NULL;};

stmnt_block: stmnt {$$=$1;}|declear {$$=$1;}|procedure {$$=$1;} |SEMICOLON  {$$=mkNode("",NULL,NULL);};

new_block: OPENBRACE procedures cmmnt declares stmnts CLOSEBRACE cmmnt
{
	$$=mkNode("{",$2,mkNode("", $4,mkNode("", $5,("}",NULL,NULL))));
};


stmnt: IF OPENPAREN expr CLOSEPAREN  stmnt_block 
{
	$$=mkNode("if",
	mkNode("(", $3, 
	mkNode(")",NULL,NULL)),$5);
}%prec IF
| IF OPENPAREN expr CLOSEPAREN   stmnt_block    ELSE  stmnt_block  
{
	$$=mkNode("if-else",
	mkNode("", $3, 
	mkNode("",NULL,NULL)),
	mkNode("",$5,
	mkNode("",$7,NULL)));
}
| WHILE cmmnt OPENPAREN expr CLOSEPAREN  stmnt_block  
{
	$$=mkNode("while",
	mkNode("(", $4, 
	mkNode(")",NULL,NULL)),$6);
}
| assmnt_stmnt SEMICOLON cmmnt {$$=mkNode("",$1,NULL);}
| expr SEMICOLON cmmnt {$$=$1;}
| RETURN expr SEMICOLON cmmnt {$$=mkNode("return",$2,NULL);}
| new_block {$$=$1;};


assmnt_stmnt: lhs ASSINGMENT expr 
{
	$$=mkNode("=",$1,$3);
};


lhs: IDENTIFIER OPENBRACKET expr CLOSEBRACKET 
{
	$$=mkNode($1, mkNode("[",$3,mkNode("]",NULL,NULL)), NULL);
} 
| IDENTIFIER {$$=mkNode($1,NULL,NULL);}
| address_expr {$$=$1;}
| derefrence_expr{$$=$1;} ;

expr:  OPENPAREN expr CLOSEPAREN {$$=mkNode("(",$2,mkNode(")",NULL,NULL));}
         |expr EQL expr {$$=mkNode("==",$1,$3);}
	| expr NOTEQL expr {$$=mkNode("!=",$1,$3);}
	| expr GREATEREQL expr {$$=mkNode(">=",$1,$3);}
	| expr GREATER expr {$$=mkNode(">",$1,$3);}
	| expr LESSEQL expr {$$=mkNode("<=",$1,$3);}
	| expr LESS expr {$$=mkNode("<",$1,$3);}
	| expr AND expr {$$=mkNode("&&",$1,$3);}
	| expr OR expr {$$=mkNode("||",$1,$3);}
	| expr PLUS expr {$$=mkNode("+",$1,$3);}
	| expr MINUS expr {$$=mkNode("-",$1,$3);}
	| expr MULTI expr {$$=mkNode("*",$1,$3);}
	| expr DIVISION expr {$$=mkNode("/",$1,$3);}
	| NOT expr {$$=mkNode("!",$2,NULL);}
	| address_expr {$$=$1;}
	| derefrence_expr {$$=$1;}
	| call_func cmmnt {$$=$1;}
	| DECIMAL_LTL {$$=mkNode($1,mkNode("INT",NULL,NULL),NULL);}
	| HEX_LTL {$$=mkNode($1,mkNode("HEX", NULL, NULL),NULL);}
	| CHAR_LTL {$$=mkNode($1,mkNode("CHAR", NULL, NULL),NULL);}
	| REAL_LTL {$$=mkNode($1,mkNode("REAL", NULL, NULL),NULL);}
	| STRING_LTL {$$=mkNode($1,mkNode("STRING", NULL, NULL),NULL);}
	| BOOLFALSE {$$=mkNode($1,mkNode("BOOLEAN", NULL, NULL),NULL);}
	| BOOLTRUE {$$=mkNode($1,mkNode("BOOLEAN", NULL, NULL),NULL);}
	| LENGTH IDENTIFIER LENGTH 
	{
		$$=mkNode("|",
		mkNode($2,NULL,NULL),
		mkNode("|",NULL,NULL));
	}
	| IDENTIFIER OPENBRACKET expr CLOSEBRACKET 
	{$$=mkNode("solovar",mkNode($1,mkNode("[",$3,mkNode("]",NULL,NULL)),NULL),NULL);}
	| IDENTIFIER {$$=mkNode("solovar",mkNode($1,NULL,NULL),NULL);}
	| NULLL {$$=mkNode("null",NULL,NULL);};
address_expr: ADDRESS IDENTIFIER {$$=mkNode("&",mkNode($2,NULL,NULL),NULL);}
	| ADDRESS OPENPAREN IDENTIFIER CLOSEPAREN {$$=mkNode("&",mkNode("(",mkNode($3,NULL,NULL),NULL),mkNode(")",NULL,NULL));}
	| ADDRESS IDENTIFIER OPENBRACKET expr CLOSEBRACKET 
	{$$=mkNode("&", mkNode($2,mkNode("[",$4,mkNode("]",NULL,NULL)),NULL),NULL);}
	| ADDRESS OPENPAREN IDENTIFIER OPENBRACKET expr CLOSEBRACKET CLOSEPAREN 
	{
		$$=mkNode("&",
		mkNode("(", 
		mkNode($3,mkNode("[",$5,mkNode("]",NULL,NULL)),NULL)
		,mkNode(")",NULL,NULL)),NULL);
	};


	derefrence_expr: DEREFRENCE IDENTIFIER {$$=mkNode("^",mkNode($2,NULL,NULL),NULL);}

expr_list: expr COMMA expr_list {$$=mkNode("",$1,mkNode(",",$3,NULL));} 
	| expr {$$=mkNode("",$1,NULL);}
	| {$$=NULL;};

paren_expr:OPENPAREN expr_list CLOSEPAREN {$$=$2;}; 
call_func: IDENTIFIER paren_expr {$$=mkNode("Call func",mkNode($1,NULL,NULL),mkNode("ARGS",$2,NULL));} ;
%%
#include "lex.yy.c"

int main()
{
	int res = yyparse();
	printf("Res:%d \n", res);
	printf("CheckMain:%d \n",checkmain);
	if(res==0&&checkmain==1)
	{
	printf("syntax accept\n"); 
	printf("Semantic accept\n");
	}
	else if(checkmain==0)
	{
		printf("ERROR Main not defined\n");
		exit(1);
	}
	else if(checkmain==2)
	{
		printf("ERROR you need to declare only one Main proc and not proc main \n");
		exit(1);
	}
	return res;	
}


int yyerror(char *e)
{
	int yydebug=1;
	fflush(stdout);
	fprintf(stderr,"Error %s at line %d\n" ,e,yylineno);
	fprintf(stderr, "Does not accept '%s'\n",yytext);
	
	return 0;
}
