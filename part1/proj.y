%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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
	| mathExp PLUS mathExp
	| mathExp MINUS mathExp
	| mathExp MUL mathExp
	| mathExp DIV mathExp ;

elem: 
	|TRUE
	|FALSE
	|NULL1
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
	|declare;

return:  RETURN mathExp ";"
	|RETURN ";";

%%
#include "lex.yy.c"
main()
{
	return yyparse();
}

int yyerror()
{
	printf("Suck IT\n");
	return 0;
}


