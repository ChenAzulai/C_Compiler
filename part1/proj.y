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

proc: PROC IDEN"("paramList")" body_proc;

paramList: paramList ";" id ":" typeOfVar |id ":" typeOfVar | ; 

value: NUM|REAL_NUM|CONST_CHAR|CONST_STRING|HEX_NUM|TRUE|FALSE ;

var: var declare | declare ;

declare: VAR id ":" typeOfVar ";" ;

typeOfVar: 
        CHAR {$$=mknode(yytext,NULL,NULL);}
	|INT {$$=mknode(yytext,NULL,NULL);}
	|REAL {$$=mknode(yytext,NULL,NULL);}
	|BOOL {$$=mknode(yytext,NULL,NULL);}
	|INT_P {$$=mknode(yytext,NULL,NULL);}
	|CHAR_P {$$=mknode(yytext,NULL,NULL);}
	|REAL_P {$$=mknode(yytext,NULL,NULL);}
	|STRING {$$=mknode(yytext,NULL,NULL);}
	|STRING "[" NUM "]" {$$=mknode(yytext,yytext,NULL);}; /*not sure if it is right */

id: id","IDEN  /*not sure*/
	|IDEN {$$=mknode(yytext,NULL,NULL);};

if: 	IF"("condition")" body {$$=mknode("IF",$2,$3);}
	|IF"("condition")" assign {$$=mknode("IF",$2,$3);}
	|IF"("condition")" body ELSE body {$$=mknode("IF",$2,mknode("ELSE",$3,$5));}  /*not sure */
	|IF"("condition")" assign ELSE assign {$$=mknode("IF",$2,mknode("ELSE",$3,$5));}; /*not sure */
	
condition: 	value IS_EQ value {$$=mknode("==",$1,$3);}
		|value BIGGER value {$$=mknode(">",$1,$3);}
		|value BIG_EQ value {$$=mknode(">=",$1,$3);}
		|value SMALLER value {$$=mknode("<",$1,$3);}
		|value SMALL_EQ value {$$=mknode("<=",$1,$3);}
		|value DIFF value {$$=mknode("!=",$1,$3);}
		/*| value AND value -- we need to implement if((1>2)and x=3) for example
		| value OR value*/
		|value ;
		
while: WHILE "("condition")" body
	|WHILE "("condition")" assign;

assign: IDEN EQUAL value ";"
	| IDEN EQUAL IDEN ";"
	|IDEN EQUAL mathExp;

mathExp: elem
	| mathExp PLUS mathExp {&&=mknode("+",$1,$3);}
	| mathExp MINUS mathExp {&&=mknode("-",$1,$3);}
	| mathExp MUL mathExp {&&=mknode("*",$1,$3);}
	| mathExp DIV mathExp  {&&=mknode("/",$1,$3);};

elem: 
	|TRUE {$$ = mknode ("true", NULL, NULL);}
	|FALSE {$$ = mknode ("false", NULL, NULL);}
	|NULL1 {$$ = mknode (NULL, NULL, NULL);}
	|IDEN;

body: "{" nestedStmt return"}"
	|"{"return"}"
	|"{""}";
	
body_proc: "{"nestedStmt_proc"}"
	|"{""}";

nestedStmt: statement
	|body
	|nestedStmt body
	|nestedStmt statement;
	
nestedStmt_proc: statement
	|body_proc
	|nestedStmt_proc body_proc
	|nestedStmt_proc statement;
	
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
