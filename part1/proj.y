%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct node{
    char *token;
    struct node *left;
    struct node *right;
} node;


node *mknode (char *token, node *left, node *right);
void printtree (node *tree);
/*int yyerror(char* error); -- add this after adding counter*/

%}

%token CHAR INT REAL BOOL INT_P CHAR_P REAL_P STRING VAR FUNC PROC RETURN IF ELSE WHILE NULL1
%token AND DIV EQUAL IS_EQ BIGGER BIG_EQ SMALLER SMALL_EQ MINUS EX_MARK DIFF OR PLUS MUL ADDS POINTER
%token TRUE FALSE 
%token NUM HEX_NUM REAL_NUM CONST_CHAR CONST_STRING IDEN
%token EQUAL IS_EQ BIGGER BIG_EQ SMALLER SMALL_EQ DIFF
%left PLUS MINUS AND OR
%left MUL DIV

%%

s: code {printf("OK!\n");};

code: var
	|function
	|function code
	|assign;

function: FUNC IDEN"("paramList")" RETURN typeOfVar body;
proc: FUNC IDEN"("paramList")" body;
paramList: paramList ";" id ":" typeOfVar |id ":" typeOfVar | ; 

value: NUM|REAL_NUM|CONST_CHAR|CONST_STRING|HEX_NUM|TRUE|FALSE ;

var: var declare | declare ;

declare: VAR id ":" typeOfVar ";" ;

typeOfVar: CHAR|INT|REAL|BOOL|INT_P|CHAR_P|REAL_P|STRING|STRING "[" NUM "]" ;

id: id","IDEN | IDEN ;

if: 	IF"("condition")" body 
	|IF"("condition")" assign
	|IF"("condition")" body ELSE body  
	|IF"("condition")" assign ELSE assign ; 
	
condition: 	value IS_EQ value
		|value BIGGER value
		|value BIG_EQ value
		|value SMALLER value
		|value DIFF value
		|value ;
		
while: WHILE "("condition")" body
	|WHILE "("condition")" assign;

assign: IDEN EQUAL value ";"
	| IDEN EQUAL IDEN ";"
	|IDEN EQUAL mathExp;

mathExp: elem
	| mathExp PLUS mathExp {&&=mknode=("+",$1,$3);}
	| mathExp MINUS mathExp {&&=mknode=("-",$1,$3);}
	| mathExp MUL mathExp {&&=mknode=("*",$1,$3);}
	| mathExp DIV mathExp  {&&=mknode=("/",$1,$3);}

elem: 
	|TRUE {$$ = mknode ("true", NULL, NULL);}
	|FALSE {$$ = mknode ("false", NULL, NULL);}
	|NULL1 {$$ = mknode (NULL, NULL, NULL);}
	|IDEN;

body: "{" nestedStmt return"}"
	|"{"return"}"
	|"{""}";
	
nestedStmt: statement
	|body
	|nestedStmt body
	|nestedStmt statement;

statement: assign
	|if
	|while
	|function
	|declare
	|proc;

return: RETURN mathExp ";"
	|RETURN ";";

%%
#include "lex.yy.c"
main()
{
	return yyparse();
}
node *mknode (char *token, node *left, node *right)
{
node* newnode=(node*)malloc(sizeof(node));
char* newstr=(char*)malloc(sizeof(token)+1);
strcpy(newstr,token);
newnode->left=left;
newnode->right=right;
newnode->token=token;

return newnode;
}

int yyerror(/*char* error*/)
{
	/*printf ("%s: at line %d found token [%s]\n",  error,counter, yytext); -- add a counter*/ 
	printf("Doesnt Work.........!!!!");
	return 0;
}
