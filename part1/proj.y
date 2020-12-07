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
void printShift(int noShift);
void printtree (node *tree,int count);
int yyerror(char* error);

%}

%token CHAR INT REAL BOOL INT_P CHAR_P REAL_P STRING VAR FUNC PROC RETURN IF ELSE WHILE MAIN
%token AND DIV EQUAL IS_EQ BIGGER BIG_EQ SMALLER SMALL_EQ MINUS EX_MARK DIFF OR PLUS MUL ADDS POINTER NULL1
%token TRUE FALSE 
%token NUM HEX_NUM REAL_NUM CONST_CHAR CONST_STRING IDEN
%token EQUAL IS_EQ BIGGER BIG_EQ SMALLER SMALL_EQ DIFF
%left PLUS MINUS AND OR
%left MUL DIV

%%

s: code {printtree($1,0);};

code: code_c proc_main {$$=mknode("CODE",$1,$2);}
	|proc_main {$$=mknode("CODE",$1,NULL);};
	
code_c: code_c function {$$=mknode("",$1,$2);}
	|code_c proc {$$=mknode("",$1,$2);}
	|proc 
	|function;

proc_main: PROC MAIN body_proc {$$=mknode("PROC Main()",$3,NULL);};

function: FUNC IDEN "(" paramList ")" RETURN typeOfVar body {$$=mknode("func",$4,mknode("ret",$7,$8));};

proc: PROC IDEN "(" paramList ")" body_proc {$$=mknode("proc",$4,$6);};

paramList: paramList ";" id ":" typeOfVar {$$=mknode("args",$1,mknode("",$3,$5));}
	|id ":" typeOfVar {$$=mknode("args",$1,$3);}
	| {$$=mknode("",NULL,NULL);};

values: NUM {$$=mknode(yytext,NULL,NULL);}
	|REAL_NUM {$$=mknode(yytext,NULL,NULL);}
	|CONST_CHAR {$$=mknode(yytext,NULL,NULL);}
	|CONST_STRING {$$=mknode(yytext,NULL,NULL);}
	|HEX_NUM {$$=mknode(yytext,NULL,NULL);};

declare: VAR id ":" typeOfVar ";" {$$=mknode("var",$2,$4);};

typeOfVar: CHAR {$$=mknode("int",NULL,NULL);}
	|INT {$$=mknode("int",NULL,NULL);}
	|REAL {$$=mknode("real",NULL,NULL);}
	|BOOL {$$=mknode("bool",NULL,NULL);}
	|INT_P {$$=mknode("int*",NULL,NULL);}
	|CHAR_P {$$=mknode("char*",NULL,NULL);}
	|REAL_P {$$=mknode("real*",NULL,NULL);}
	|STRING {$$=mknode("string",NULL,NULL);}
	|STRING "["NUM"]" {$$=mknode("string[_]",NULL,NULL);};

id:	name 
	|id "," name {$$=mknode(yytext,$1,$3);};
	
name: IDEN {$$=mknode(yytext,NULL,NULL);};

if: 	IF "(" condition ")" body_proc {$$=mknode("IF",$3,$5);}
	|IF "(" condition ")" assign {$$=mknode("IF",$3,$5);}
	|IF "(" condition ")" body_proc ELSE body_proc {$$=mknode("IF-ELSE",$3,mknode("",$5,$7));}
	|IF "(" condition ")" assign ELSE assign {$$=mknode("IF",$2,mknode("ELSE",$3,$5));};
 
	
condition: 	values IS_EQ values {$$=mknode("==",$1,$3);}
		|values BIGGER values {$$=mknode(">",$1,$3);}
		|values BIG_EQ values {$$=mknode(">=",$1,$3);}
		|values SMALLER values {$$=mknode("<",$1,$3);}
		|values SMALL_EQ values {$$=mknode("<=",$1,$3);}
		|values DIFF values {$$=mknode("!=",$1,$3);}
		|elem ;

while: WHILE "("condition")" body  {$$=mknode("while",$3,$5);}
	|WHILE "("condition")" assign {$$=mknode("while",$3,$5);};

assign: IDEN EQUAL mathExp ";" {$$=mknode("=",$1,$3);}
	|IDEN EQUAL values ";" {$$=mknode("=",$1,$3);}
	|IDEN EQUAL IDEN ";" {$$=mknode("=",$1,$3);}
	|IDEN EQUAL ADDS IDEN ";" {$$=mknode("*=",$1,$4);}
	|IDEN EQUAL POINTER IDEN ";" {$$=mknode("*=",$1,$4);};

mathExp:  mathExp PLUS mathExp {$$=mknode("+",$1,$3);}
	| mathExp MINUS mathExp {$$=mknode("-",$1,$3);}
	| mathExp MUL mathExp {$$=mknode("*",$1,$3);}
	| mathExp DIV mathExp {$$=mknode("\\",$1,$3);}
	| mathExp OR mathExp {$$=mknode("OR",$1,$3);}
	| mathExp AND mathExp {$$=mknode("AND",$1,$3);}
	| elem;

elem: values
	|TRUE {$$ = mknode("true", NULL, NULL);}
	|FALSE {$$ = mknode("false", NULL, NULL);}
	|NULL1 {$$ = mknode(NULL, NULL, NULL);}
	|IDEN;

body: "{" nestedStmt return "}" {$$=mknode("",$2,$3);}
	| "{" return "}" {$$=mknode("",$2,NULL);};

body_proc: "{" nestedStmt "}" {$$=mknode("",$2,NULL);}
	| "{" "}" {$$=mknode("",NULL,NULL);};

nestedStmt: statement
	|body_proc
	|nestedStmt statement {$$=mknode("",$1,$2);}
	|nestedStmt body_proc {$$=mknode("",$1,$2);};

statement:declare
	|assign
	|if
	|while
	|function
	|proc;

return:  RETURN mathExp ";" {$$=mknode("return!!!!",$2,NULL);}
	|RETURN ";" {$$=mknode("return@@@",NULL,NULL);};
	

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

void printtree (node *tree, int count)
{
    int shift = count;
    if (strlen(tree->token) > 0) {
        printShift(count);
        printf ("(%s", tree->token);
        if (tree->left != NULL) {
            printf("\n");
        }
    }
    if (tree->left) {
        if (strlen(tree->token) == 0) {
            shift = count - 1;
        }
        printtree(tree->left, shift + 1);
        if (strlen(tree->token) > 0) {
            printShift(count);
        }
    }
    if (strlen(tree->token) > 0) {
        printf (")\n");
    }
    if (tree->right) {
        printtree (tree->right, count);
    }
}

void printShift(int noShifts) {
    int i;
    for (i = 0; i < noShifts; i++) {
        printf ("\t");
    }
}

int yyerror(char* error)
{
	printf ("%s: at line %d found token [%s]\n",  error,counter, yytext);
	return 0;
}
