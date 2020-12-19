%{
#include<stdio.h>
#include "y.tab.h"

%}
%x C_COMMENT
%%
"bool"	{return BOOL;}
"char"	{return CHAR;}
"int"	{return INT;}
"real"	{return REAL;}
"string" {return STRING;}
"int*"	{return INT_P;}
"char*"	{return CHAR_P;}
"real*"	{return REAL_P;}
"if"	{return IF;}
"else"	{return ELSE;}
"while"	{return WHILE;}
"var"	{return VAR;}
"func"	{return FUNC;}
"proc"	{return PROC;}
"return" {return RETURN;}
"null"	{return NULL1;}
"&&"	{return AND;}
"/"	{return DIV;}
"="	{return EQUAL;}
"=="	{return IS_EQ;}
">"	{return BIGGER;}
">="	{return BIG_EQ;}
"<"	{return SMALLER;}
"<="	{return SMALL_EQ;}
"-"	{return MINUS;}
"!"	{return EX_MARK;}
"!="	{return DIFF;}
"||"	{return OR;}
"+"	{return PLUS;}
"*"	{return MUL;}
"&"	{return ADDS;}
"^"	{return POINTER;}
"true"	{return TRUE;}
"false"	{return FALSE;}
"Main()" {return MAIN;}

"/%" {BEGIN(C_COMMENT);}
<C_COMMENT>"%/" {BEGIN(INITIAL);}
<C_COMMENT>. { }
<C_COMMENT>\n {yylineno++; }

[+-]?0|[1-9][0-9]*	{yylval.string= strdup(yytext);return NUM;}
0[xX][0-9A-F]+	{yylval.string= strdup(yytext);return HEX_NUM;}
[+-]?[0-9]*\.?[0-9]+([eE][+-]?[0-9]+)?	{yylval.string= strdup(yytext);return REAL_NUM;}
[\'].[\']	{yylval.string= strdup(yytext);return CONST_CHAR;}
[\"][^\"]*[\"]	{yylval.string= strdup(yytext);return CONST_STRING;}
[a-zA-Z][a-zA-Z0-9_]* 	{yylval.string= strdup(yytext);return IDEN;}
[\n]	{yylineno++;};
[ \t]	;
.	{return yytext[0];}
%%
