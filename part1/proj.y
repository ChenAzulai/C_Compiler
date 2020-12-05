%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define YYSTYPE struct node *
typedef struct node{
    char* token;
    struct node* left;
    struct node* right;
} node;


node *mknode (char *token, node *left, node *right);
void printtree (node *tree);
/*int yyerror(char* error); -- add this after adding counter*/

%}

%token CHAR INT REAL BOOL INT_P CHAR_P REAL_P STRING VAR FUNC PROC RETURN IF ELSE WHILE MAIN
%token AND DIV EQUAL IS_EQ BIGGER BIG_EQ SMALLER SMALL_EQ MINUS EX_MARK DIFF OR PLUS MUL ADDS POINTER NULL1
%token TRUE FALSE 
%token NUM HEX_NUM REAL_NUM CONST_CHAR CONST_STRING IDEN
%token EQUAL IS_EQ BIGGER BIG_EQ SMALLER SMALL_EQ DIFF
%left PLUS MINUS AND OR
%left MUL DIV

%%

s: code {printf("OK!\n");};

code: code_c proc_main
	|proc_main;

code_c: code_c function
	|code_c proc
	|proc
	|function;

proc_main: PROC MAIN body_proc;

function: FUNC IDEN "(" paramList ")" RETURN typeOfVar body;

proc: PROC IDEN "(" paramList ")" body_proc;

paramList: paramList ";" id ":" typeOfVar 
	|id ":" typeOfVar 
	|; 

values: NUM|REAL_NUM|CONST_CHAR|CONST_STRING|HEX_NUM ;

declare: VAR id ":" typeOfVar ";" ;

typeOfVar: CHAR {$$=mknode("int",NULL,NULL);}
	|INT {$$=mknode("int",NULL,NULL);}
	|REAL {$$=mknode("real",NULL,NULL);}
	|BOOL {$$=mknode("bool",NULL,NULL);}
	|INT_P {$$=mknode("int*",NULL,NULL);}
	|CHAR_P {$$=mknode("char*",NULL,NULL);}
	|REAL_P {$$=mknode("real*",NULL,NULL);}
	|STRING {$$=mknode("string",NULL,NULL);}
	|STRING "["NUM"]" {$$=mknode("string",$2,NULL);};

id: id "," IDEN 
	| IDEN {$$=mknode(yytext,NULL,NULL);};

if: 	IF "(" condition ")" body_proc {$$=mknode("IF",$2,$3);}
	|IF "(" condition ")" assign {$$=mknode("IF",$2,$3);}
	|IF "(" condition ")" body_proc ELSE body_proc {$$=mknode("IF",$2,mknode("ELSE",$3,$5));}  /*not sure */
	|IF "(" condition ")" assign ELSE assign {$$=mknode("IF",$2,mknode("ELSE",$3,$5));}; /*not sure */
 
	
condition: 	values IS_EQ values {$$=mknode("==",$1,$3);}
		|values BIGGER values {$$=mknode(">",$1,$3);}
		|values BIG_EQ values {$$=mknode(">=",$1,$3);}
		|values SMALLER values {$$=mknode("<",$1,$3);}
		|values SMALL_EQ values {$$=mknode("<=",$1,$3);}
		|values DIFF values {$$=mknode("!=",$1,$3);}
		|elem ;

while: WHILE "("condition")" body 
	|WHILE "("condition")" assign;

assign: IDEN EQUAL mathExp ";"
	|IDEN EQUAL values ";"
	|IDEN EQUAL IDEN ";"
	|IDEN EQUAL ADDS IDEN ";"
	|IDEN EQUAL POINTER IDEN ";";

mathExp:  mathExp PLUS mathExp {$$=mknode("+",$1,$3);}
	| mathExp MINUS mathExp {$$=mknode("-",$1,$3);}
	| mathExp MUL mathExp {$$=mknode("*",$1,$3);}
	| mathExp DIV mathExp {$$=mknode("/",$1,$3);}
	| mathExp OR mathExp
	| mathExp AND mathExp
	| elem;

elem: values
	|TRUE {$$ = mknode("true", NULL, NULL);}
	|FALSE {$$ = mknode("false", NULL, NULL);}
	|NULL1 {$$ = mknode(NULL, NULL, NULL);}
	|IDEN;

body: "{" nestedStmt return"}"
	|"{"return"}";

body_proc:"{"nestedStmt"}"
	|"{""}";

/*code_block:
	"{" nestedStmt "}"
	|"{""}";*/


nestedStmt: statement
	|body_proc
	|nestedStmt statement
	|nestedStmt body_proc;

/*nestedStmt_proc: statement
	|nestedStmt_proc statement;
	|code_block
	|nestedStmt_proc code_block;*/

statement:declare
	|assign
	|if
	|while
	|function
	|proc;

return:  RETURN mathExp ";"
	|RETURN ";";
	

%%
#include "lex.yy.c"
main()
{
	return yyparse();
}

node *mknode(char *token, node *left, node *right)
{
	node* newnode=(node*)malloc(sizeof(node));
	char* newstr=(char*)malloc(sizeof(token)+1);
	strcpy(newstr,token);
	newnode->left=left;
	newnode->right=right;
	newnode->token=token;

	return newnode;
}

int yyerror()
{
	/*printf ("%s: at line %d found token [%s]\n",  error,counter, yytext); -- add a counter*/ 
	printf("Doesnt Work.........!!!!\n");
	return 0;
}
