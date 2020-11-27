%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
%}

%token CHAR INT REAL BOOL INT_P CHAR_P REAL_P STRING VAR FUNC PROC RETURN IF ELSE WHILE NADA
%token AND DIV EQUAL IS_EQ BIGGER BIG_EQ SMALLER SMALL_EQ MINUS EX_MARK DIFF OR PLUS MUL ADDS POINTER
%token TRUE FALSE 
%token NUM HEX_NUM REAL_NUM CONST_CHAR CONST_STRING IDEN
%token EQUAL IS_EQ BIGGER BIG_EQ SMALLER SMALL_EQ DIFF
%left PLUS MINUS AND OR
%left MUL DIV

%%

s: code {printf("OK!\n");};

code: var|function;

function: FUNC IDEN"("paramList")" RETURN values";";

paramList: paramList ";" id ":" typeOfVar |id ":" typeOfVar | ; 

values: NUM|REAL_NUM|CONST_CHAR|CONST_STRING ;

var: var declare | declare ;

declare: VAR id ":" typeOfVar ";" ;

typeOfVar: CHAR|INT|REAL|BOOL|INT_P|CHAR_P|REAL_P|STRING|STRING "[" NUM "]" ;

id: id","IDEN | IDEN ;

if: 	IF"("condition")" body 
	|IF"("condition")" statment
	|IF"("condition")" body ELSE body 
	|IF"("condition")" statment ELSE statment ; 
condition: value IS_EQ value
	|value BIGGER value
	|value BIG_EQ value
	|value SMALLER value
	|value DIFF value
	|value ;
	
statment: ;
else: statment | body ;
for: ;
while: ;

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


