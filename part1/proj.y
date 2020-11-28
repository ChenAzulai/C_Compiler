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

code: var|function|if|while;

function: FUNC IDEN"("paramList")" RETURN value";";

paramList: paramList ";" id ":" typeOfVar |id ":" typeOfVar | ; 

value: NUM|REAL_NUM|CONST_CHAR|CONST_STRING ;

var: var declare | declare ;

declare: VAR id ":" typeOfVar ";" ;

typeOfVar: CHAR|INT|REAL|BOOL|INT_P|CHAR_P|REAL_P|STRING|STRING "[" NUM "]" ;

id: id","IDEN | IDEN ;

if: 	IF"("condition")" "{"body"}" 
	|IF"("condition")" statment
	|IF"("condition")" "{"body"}"  ELSE "{"body"}"  
	|IF"("condition")" statment ELSE statment ; 
	
condition: 	value IS_EQ value
		|value BIGGER value
		|value BIG_EQ value
		|value SMALLER value
		|value DIFF value
		|value ;
while: "while" ("expression")" "{"body"}"  | ("expression")" statment;

assign: IDEN EQUAL values ";"
	| IDEN EQUAL IDEN ";"
	|IDEN EQUAL mathExp;

mathExp: elem
	| mathExp PLUS mathExp
	| mathExp MINUS mathExp
	| mathExp MUL mathExp
	| mathExp DIV mathExp ;

elem: values
	|TRUE
	|FALSE
	|NULL1
	|IDEN
	|HEX_NUM;

expression: ;
statment: ;
body: ;


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


