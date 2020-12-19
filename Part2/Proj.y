%{
	#include <stdio.h>
	#include <stdlib.h>
	#include <string.h>
	#include "lex.yy.c"

typedef enum {false,true} bool;
	typedef struct node
	{
		char *token;
		struct node *left;
		struct node *right;
	} node;


	typedef struct Varaiables
	{	int isArg;
		char *name;
		char *value;
		char *type;
		char * length;
	}Varaiable;

    typedef struct Function 
	{
        char * name;
		struct Varaiables * arguments;
        char *returnType; 
		int argNum;
		bool findreturn;
    } Function;

		typedef struct SCOPE
	{	
		char *name;
		Varaiable * var;
		int VarCount;
		int Fcount;
		Function ** func;
		struct SCOPE * nextScope;
		struct SCOPE * preScope;
	}SCOPE;
	int yylex();
	int yyerror(char *e);
	static int scope=0;
	SCOPE* mkScope(char *);
	SCOPE* finScope(SCOPE * scopes);
	SCOPE* globalScope=NULL;
	void addFunc(char * name,Varaiable * arguments,node *returnType,int argNum,SCOPE*);
	void addVar(Varaiable * arguments,int,int,SCOPE * MYscope);
	void syntaxAnalyzer(node *tree,SCOPE * scope);
	void pushScopes(SCOPE* from,char*);
	char* getExpType(node *,SCOPE*);
	char* findFuncInScopes(node * tree,SCOPE * MYscope);
	char* findVar(node * tree,SCOPE * MYscope);
	node* mkNode(char* token, node *left, node *right);
	Varaiable* mkArguments(node *,int *);
	Varaiable* callFuncArguments(SCOPE *,node *tree,int * count);
	
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
int checkmain=0;
int main()
{
	int res = yyparse();
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

void addVar(Varaiable * arguments,int countvars,int isArg,SCOPE * MYscope){
	int i=0, j=0;
	if(countvars==0)
	return;
	Varaiable* tmp;
	SCOPE * scopes=MYscope;

	for(i=0;i<countvars;i++)
		for(j=0;j<countvars;j++)
	if(i!=j && strcmp(arguments[j].name,arguments[i].name)==0 )
	{
		printf("ERROR: same var name %s in one declare",arguments[i].name);
		SCOPE * t=scopes->preScope;
		while(t->preScope!=NULL && t->preScope->Fcount==0)
			t=t->preScope;
		if(t->func!=NULL)
		printf(",in func %s\n",t->func[t->Fcount-1]->name);
			else
		printf("\n");
		exit(1);
	}
	if(scopes->var==NULL)
	{ 
		scopes->var=(Varaiable*) malloc(sizeof(Varaiable)*countvars);
	}
	else
	{
		tmp=scopes->var;
		scopes->var=(Varaiable*) malloc(sizeof(Varaiable)*(scopes->VarCount+countvars));
		for(i=0;i<scopes->VarCount;i++)
		{
			for(j=0;j<countvars;j++)
			{
				if(strcmp(tmp[i].name,arguments[j].name)==0 )
				{
					printf("ERROR:same var name %s in same scope",tmp[i].name);
					SCOPE * t=scopes->preScope;
					while(t->preScope!=NULL && t->preScope->Fcount==0)
						t=t->preScope;
					if(t->func!=NULL)
					printf(",in func %s\n",t->func[t->Fcount-1]->name);
					else
					printf("\n");
					exit(1);
				}
			}
			scopes->var[i]=tmp[i];	
		}
	}
	for(j=0;j<countvars;j++)
	{

		scopes->var[scopes->VarCount].name=arguments[j].name;
		scopes->var[scopes->VarCount].value=NULL;
		scopes->var[scopes->VarCount].isArg=isArg;
		scopes->var[scopes->VarCount].length=arguments[j].length;
		scopes->var[(scopes->VarCount)++].type=arguments[j].type;
	}

}


char * getExpType(node * tree,SCOPE* MYscope){
	char* msg=(char*)malloc(sizeof(char)*7);
	msg="";
	if(strcmp(tree->token,"null")==0)
		msg="NULL";
	else
	if(tree->left!=NULL){
		if(strcmp(tree->left->token,"INT")==0)
			msg= "int";
		if(strcmp(tree->left->token,"HEX")==0)
			msg= "hex";
		if(strcmp(tree->left->token,"CHAR")==0)
			msg= "char";
		if(strcmp(tree->left->token,"REAL")==0)
			msg= "real";
		if(strcmp(tree->left->token,"STRING")==0)
			msg= "string";
		if(strcmp(tree->left->token,"BOOLEAN")==0)
			msg= "boolean";
		if(strcmp(tree->token,"!")==0)
		if(strcmp(getExpType(tree->left,MYscope),"boolean")==0)
			msg="boolean";
		else{
			printf("ERROR, operator !  used only on boolean type");
			exit(1);
		
		}
		if(strcmp(tree->token,"|")==0)
		if(strcmp(getExpType(tree->left,MYscope),"string")==0)
		msg="int";
		else{
			printf("ERROR,operator | only used on string type in func/proc %s",globalScope->func[globalScope->Fcount-1]->name);
			exit(1);
			
		}
		if(strcmp(tree->token,"==")==0||strcmp(tree->token,"!=")==0)
		{
			if(strcmp(getExpType(tree->left,MYscope),getExpType(tree->right,MYscope))==0&&strcmp(getExpType(tree->right,MYscope),"string")!=0)
			msg="boolean";
			else{
				printf("ERROR: invalid use operator %s between %s and %s in func/proc %s\n",tree->token,getExpType(tree->left,MYscope),getExpType(tree->right,MYscope),globalScope->func[globalScope->Fcount-1]->name);
			exit(1);	
			}
		}

		if(strcmp(tree->token,">=")==0||strcmp(tree->token,">")==0||strcmp(tree->token,"<=")==0||strcmp(tree->token,"<")==0)
		{
			if((strcmp(getExpType(tree->left,MYscope),"int")==0||strcmp(getExpType(tree->left,MYscope),"real")==0)&&(strcmp(getExpType(tree->right,MYscope),"int")==0||strcmp(getExpType(tree->right,MYscope),"real")==0))
			msg="boolean";
			else{
				printf("ERROR: invalid use operator %s between %s and %s in func/proc %s\n",tree->token,getExpType(tree->left,MYscope),getExpType(tree->right,MYscope),globalScope->func[globalScope->Fcount-1]->name);
				exit(1);
	
			}
		}

		if(strcmp(tree->token,"&&")==0||strcmp(tree->token,"||")==0)
		{

			if(strcmp(getExpType(tree->left,MYscope),getExpType(tree->right,MYscope))==0&&strcmp(getExpType(tree->right,MYscope),"boolean")==0)
			msg="boolean";
			else{
				printf("ERROR: invalid use operator %s between %s and %s in func/proc %s\n",tree->token,getExpType(tree->left,MYscope),getExpType(tree->right,MYscope),globalScope->func[globalScope->Fcount-1]->name);
				exit(1);
				
			}
			

		}
		if(strcmp(tree->token,"-")==0||strcmp(tree->token,"+")==0)
		{
			if((strcmp(getExpType(tree->left,MYscope),"int")==0||strcmp(getExpType(tree->left,MYscope),"real")==0)&&(strcmp(getExpType(tree->right,MYscope),"int")==0||strcmp(getExpType(tree->right,MYscope),"real")==0))
			{
			if(strcmp(getExpType(tree->left,MYscope),getExpType(tree->right,MYscope))==0&&strcmp(getExpType(tree->left,MYscope),"int")==0)
			msg="int";
			else
			msg="real";
			}

			if(strcmp(getExpType(tree->right,MYscope),"int")==0&&(strcmp(getExpType(tree->left,MYscope),"char*")==0||strcmp(getExpType(tree->right,MYscope),"int*")==0||strcmp(getExpType(tree->right,MYscope),"real*")==0)){
				msg=getExpType(tree->left,MYscope);
			}
			else if(strcmp(msg,"")==0)
			{
				printf("ERROR: invalid use operator %s between %s and %s in func/proc %s\n",tree->token,getExpType(tree->left,MYscope),getExpType(tree->right,MYscope),globalScope->func[globalScope->Fcount-1]->name);
				exit(1);
				
			}

		}
		if(strcmp(tree->token,"*")==0||strcmp(tree->token,"/")==0)
		{
			if((strcmp(getExpType(tree->left,MYscope),"int")==0||strcmp(getExpType(tree->left,MYscope),"real")==0)&&(strcmp(getExpType(tree->right,MYscope),"int")==0||strcmp(getExpType(tree->right,MYscope),"real")==0))
			{
			if(strcmp(getExpType(tree->left,MYscope),getExpType(tree->right,MYscope))==0&&strcmp(getExpType(tree->left,MYscope),"int")==0)
			msg="int";
			else
			msg="real";
			}
			else
			{
				printf("ERROR: invalid use operator %s between %s and %s in func/proc %s\n\n",tree->token,getExpType(tree->left,MYscope),getExpType(tree->right,MYscope),globalScope->func[globalScope->Fcount-1]->name);
				exit(1);
				
			}
		}
		if(strcmp(tree->token,"&")==0)
		{
			if(strcmp(tree->left->token,"(")==0)
				msg=getExpType(tree->left->left,MYscope);
			else{
				msg=getExpType(tree->left,MYscope);
				
				}
			if(strcmp(msg,"char")==0)
			msg="char*";
			else
			if(strcmp(msg,"int")==0)
			msg="int*";
			else
			if(strcmp(msg,"real")==0)
			msg="real*";
			else
			{
				printf("ERROR: invalid use %s on %s \n",tree->token,msg);
				exit(1);
			}
		}
		if(strcmp(tree->token,"^")==0)
		{
			if(strcmp(tree->left->token,"(")==0)
				msg=getExpType(tree->left->left,MYscope);
			else
				msg=getExpType(tree->left,MYscope);
			
			if(strcmp(msg,"char*")==0)
			msg="char";
			else
			if(strcmp(msg,"int*")==0)
			msg="int";
			else
			if(strcmp(msg,"real*")==0)
			msg="real";
			else
			{
				printf("ERROR: invalid use %s on %s \n",tree->token,msg);
			exit(1);	
			}

		}
		if(strcmp(tree->token,"(")==0)
			msg=getExpType(tree->left,MYscope);
		if(strcmp(tree->token,"Call func")==0)
			msg=findFuncInScopes(tree,MYscope);
		
	}
	if(strcmp(msg,"")==0)
		msg=findVar(tree,MYscope);

	
	

	return msg;
}

SCOPE* mkScope(char* name)
{	
	SCOPE *newScope = (SCOPE*)malloc(sizeof(SCOPE));
	newScope->name=name;
	newScope->var=NULL;
	newScope->VarCount=0;
	newScope->func=NULL;
	newScope->Fcount=0;
	newScope->nextScope=NULL;
	newScope->preScope=NULL;
	return newScope;
}


void addFunc(char * name,Varaiable * arguments,node *returnType,int argNum,SCOPE * MYscope){
	int i=0, j=0;
	Function** tmp;
	SCOPE * scopes = MYscope;
	for(i=0;i<argNum;i++)
		for(j=0;j<argNum;j++)
	if(i!=j && strcmp(arguments[j].name,arguments[i].name)==0 )
	{
		printf("ERROR, there are arguments  with same name %s in func %s\n",arguments[i].name,name);
	exit(1);	
	}
	if(scopes->func==NULL)
	{ 
		scopes->func=(Function**) malloc(sizeof(Function*));
	}
	else
	{
		tmp=scopes->func;
		scopes->func=(Function**) malloc(sizeof(Function*)*(scopes->Fcount+1));
		for(i=0;i<scopes->Fcount;i++)
		{		

				if(strcmp(tmp[i]->name,name)==0 )
				{
					printf("ERROR, there's already func/proc %s with the same name in the same scope \n",tmp[i]->name);
	exit(1);				
				}	
				scopes->func[i]=tmp[i];
		}
	}
		scopes->func[scopes->Fcount]=(Function*) malloc(sizeof(Function));
		scopes->func[scopes->Fcount]->name=name;
		scopes->func[scopes->Fcount]->arguments=arguments;
		
		if(returnType==NULL)
		scopes->func[scopes->Fcount]->returnType=NULL;
		else{
		if(strcmp(returnType->token,"string")==0)
			{
				printf("ERORR, the return type  of func %s can't be string\n",name);
			exit(1);	
			}
		scopes->func[scopes->Fcount]->returnType=returnType->token;
		}
		scopes->func[scopes->Fcount]->argNum=argNum;
		scopes->func[scopes->Fcount]->findreturn=false;
		++(scopes->Fcount); 

}

node* mkNode (char *token, node *left, node *right)
{
	node *newnode = (node*)malloc(sizeof(node));
	newnode->left=left;
	newnode->right=right;
	newnode->token=token;
	return newnode;
}

int yyerror(char *e)
{
	int yydebug=1;
	fflush(stdout);
	fprintf(stderr,"Error %s at line %d\n" ,e,yylineno);
	fprintf(stderr, "Does not accept '%s'\n",yytext);
	
	return 0;
}

SCOPE* finScope(SCOPE * scopes)
{
	SCOPE * MYscope=scopes;
	if(MYscope!=NULL)
	while(MYscope->nextScope!=NULL)
		MYscope=MYscope->nextScope;
	return MYscope;
}

void syntaxAnalyzer(node *tree,SCOPE * MYscope){	
	if(strcmp(tree->token, "=") == 0 )
	{
		if(!(strcmp(getExpType(tree->right,MYscope),"NULL")==0&& (strcmp(getExpType(tree->left,MYscope),"real*")==0||strcmp(getExpType(tree->left,MYscope),"int*")==0||strcmp(getExpType(tree->left,MYscope),"char*")==0)))
		if(strcmp(getExpType(tree->left,MYscope),getExpType(tree->right,MYscope))!=0)
		{
			printf("ERORR,can't use  operator '=' between %s and %s in scope %s in func/proc %s\n",getExpType(tree->left,MYscope),getExpType(tree->right,MYscope),MYscope->name,globalScope->func[globalScope->Fcount-1]->name);
		exit(1);	
		}
	}
	else if(strcmp(tree->token, "var") == 0)
	{
		int VarCount=0;
		Varaiable * var=mkArguments(tree,&VarCount);
		addVar(var,VarCount,0,MYscope);
		
		
	}
	else if(strcmp(tree->token, "if") == 0)
	{
		if(strcmp(getExpType(tree->left->left,MYscope),"boolean")!=0)
		{
			printf("ERORR:'if' condition must be type boolean\n");
	exit(1);		
		}

		if(strcmp(tree->right->token,"{")!=0)
		{
			pushScopes(MYscope,tree->token);
			if (tree->left) 
				syntaxAnalyzer(tree->left,finScope( MYscope->nextScope));
	
			if (tree->right)
				syntaxAnalyzer(tree->right,finScope( MYscope->nextScope));
        	scope--;
			return;
		}
		
		
		
	}
		else if(strcmp(tree->token, "while") == 0)
	{
		if(strcmp(getExpType(tree->left->left,MYscope),"boolean")!=0)
		{
			printf("ERORR:'while' condition must be type boolean\n");
		exit(1);	
		}

		if(strcmp(tree->right->token,"{")!=0)
		{
			pushScopes(MYscope,tree->token);
			if (tree->left) 
				syntaxAnalyzer(tree->left,finScope( MYscope->nextScope));
	
			if (tree->right)
				syntaxAnalyzer(tree->right,finScope( MYscope->nextScope));
        	scope--;
			return;
		}
		
		
		
	}
	else if(strcmp(tree->token, "FUNC") == 0 )
	{
        int count=0;
		Varaiable * arg=mkArguments(tree->left->right->left,&count);
		addFunc(tree->left->token,arg,tree->left->right->right->left,count,MYscope);
		pushScopes(MYscope,tree->token);
		addVar(arg,count,1,finScope(MYscope));
	if (tree->left) 
		syntaxAnalyzer(tree->left,finScope( MYscope->nextScope));
	
	if (tree->right)
		syntaxAnalyzer(tree->right,finScope( MYscope->nextScope));
		if(MYscope->func[MYscope->Fcount-1]->findreturn==false)
		{
			printf("ERORR in func %s must be return\n",tree->left->token);
		exit(1);	
		}
        scope--;		
		return;
	}
    else if(strcmp(tree->token, "PROC") == 0)
	{
        int count=0;
		Varaiable * arg=mkArguments(tree->right->left,&count);
	if(strcmp(tree->left->token,"Main")==0)
			{
				checkmain=1;
			}
	if(strcmp(tree->left->token,"Main")==0)
			{
				checkmain=2;
			}

	if(strcmp(tree->left->token,"Main")==0 && arg!=NULL)
        {
        	printf("ERORR %s dont accept variable\n",tree->left->token);
		exit(1);
        }
		addFunc(tree->left->token,arg,NULL,count,MYscope);
		pushScopes(MYscope,tree->token);
		addVar(arg,count,1,finScope(MYscope));
	if (tree->left) 
		syntaxAnalyzer(tree->left,finScope( MYscope->nextScope));
	
	if (tree->right)
		syntaxAnalyzer(tree->right,finScope( MYscope->nextScope));
		scope--;	
		return;
    }

	else if(strcmp(tree->token, "Call func") == 0)
	{
		findFuncInScopes(tree,MYscope);
		
	}
	else if(strcmp(tree->token, "CODE") == 0)
	{
		pushScopes(NULL,tree->token);
	if (tree->left) 
		syntaxAnalyzer(tree->left,globalScope);
	
	if (tree->right)
		syntaxAnalyzer(tree->right,globalScope);
		scope--;
		return;
	}

    else if(strcmp(tree->token, "Main") == 0)
	{
		addFunc(tree->token,NULL,NULL,0,MYscope);
		pushScopes(MYscope,tree->token);
	if (tree->left) 
		syntaxAnalyzer(tree->left,finScope( MYscope->nextScope));
	
	if (tree->right)
		syntaxAnalyzer(tree->right,finScope( MYscope->nextScope));
        scope--;
		return;
               
    }       
	else if(strcmp(tree->token, "if-else") == 0)
	{
		if(strcmp(getExpType(tree->left->left,MYscope),"boolean")!=0)
		{
			printf("ERORR:'if' condition must be type boolean\n");
		exit(1);	
		}

		if(strcmp(tree->right->left->token,"{")!=0)
		{
			pushScopes(MYscope,tree->token);
			syntaxAnalyzer(tree->right->left,finScope( MYscope->nextScope));
			scope--;
			pushScopes(MYscope,tree->token);
			syntaxAnalyzer(tree->right->right->left,finScope( MYscope->nextScope));
        	scope--;
			return;
		}
	}

	else if(strcmp(tree->token, "return") == 0)
	{
		SCOPE * tmp= MYscope;
		int flag=true;
		while(strcmp(tmp->name,"FUNC")!=0&&strcmp(tmp->name,"PROC")!=0&&strcmp(tmp->name,"CODE")!=0)
		{
			tmp=tmp->preScope;
			flag=false;
		}
		if(flag==false)
		{
			if(strcmp(getExpType(tree->left,MYscope),tmp->preScope->func[tmp->preScope->Fcount-1]->returnType))
			{
			printf("ERORR,the return type doesn't match the type of func %s \n",tmp->preScope->func[tmp->preScope->Fcount-1]->name);
			printf("%s ,%s %s\n",getExpType(tree->left,MYscope),tmp->preScope->func[tmp->preScope->Fcount-1]->returnType,tmp->preScope->func[tmp->preScope->Fcount-1]->name);
			exit(1);
			}
		}
		else
		{
			if(tmp->preScope->func[tmp->preScope->Fcount-1]->returnType!=NULL)
			{
				if(0==strcmp(getExpType(tree->left,MYscope),tmp->preScope->func[tmp->preScope->Fcount-1]->returnType))
				{
					tmp->preScope->func[tmp->preScope->Fcount-1]->findreturn=true;
				}
				else
				{
					printf("ERORR,the return type doesn't match the type of func %s \n",tmp->preScope->func[tmp->preScope->Fcount-1]->name);
					printf("%s ,%s %s\n",getExpType(tree->left,MYscope),tmp->preScope->func[tmp->preScope->Fcount-1]->returnType,tmp->preScope->func[tmp->preScope->Fcount-1]->name);
					exit(1);
				}
			}
			else
			{
				printf("ERORR, invalid return value in procedure %s\n",tmp->preScope->func[tmp->preScope->Fcount-1]->name);
			exit(1);	
			}  
		}  
	}
	else if(strcmp(tree->token, "{") == 0)
	{
    pushScopes(MYscope,tree->token);
	if (tree->left) 
		syntaxAnalyzer(tree->left,finScope( MYscope->nextScope));
	
	if (tree->right)
		syntaxAnalyzer(tree->right,finScope( MYscope->nextScope));
        scope--;
		return;			
	}
	else if(strcmp(tree->token, "solovar") == 0 )
	{
		findVar(tree->left,MYscope);
	}
	if (tree->left) 
		syntaxAnalyzer(tree->left,MYscope);
	
	if (tree->right)
		syntaxAnalyzer(tree->right,MYscope);
}


void pushScopes(SCOPE* from,char* name)
{
	SCOPE * point;
	if(globalScope==NULL)
		globalScope=mkScope(name);
	else{
	point=globalScope;
	while(point->nextScope!=NULL)
		point=point->nextScope;
	point->nextScope=mkScope(name);
	point->nextScope->preScope=from;
	}
}
char* findFuncInScopes(node * tree,SCOPE * MYscope)
{
	int i=0, j=0,t;
	SCOPE* tmp=MYscope;
	Varaiable* arguments;
	bool find = false, flag = true;
	while(tmp!=NULL)
	{
		for(i=0;i<tmp->Fcount;i++)
		if(strcmp(tree->left->token,tmp->func[i]->name)==0)
		{
			find=true;
			flag=true;
			int count=0;
			arguments=callFuncArguments(MYscope,tree->right->left,&count);
			if(count==tmp->func[i]->argNum)
			{
				for(j=0, t=count-1; j<count; j++,t--)
				{
					if(strcmp(arguments[j].type,tmp->func[i]->arguments[t].type)!=0)
						flag=false;
				}
				if(flag==true)
					return tmp->func[i]->returnType;
			}
		}
		tmp=tmp->preScope;
	}
	printf("ERROR,func %s not find call in scope %s in func/proc %s\n",tree->left->token,MYscope->name,globalScope->func[globalScope->Fcount-1]->name);
	if(find==true)
	{
		printf("ERROR, There is a func with the same name that accepts different arguments\n");
		exit(1);
	}	
}

char *findVar(node * tree,SCOPE * MYscope)
{
	int j=0, i=0;
	SCOPE* tmp = MYscope;
	if(strcmp(tree->token,"solovar")==0)
		tree=tree->left;
	while(tmp!=NULL)
	{
		for(i=0;i<tmp->VarCount;i++)
		if(strcmp(tree->token,tmp->var[i].name)==0)
		{
			
			if(tree->left!=NULL && strcmp(tree->left->token,"[")==0)
			{
				if(strcmp(tmp->var[i].type,"string")==0)
					if(strcmp(getExpType(tree->left->left,MYscope),"int")==0)
					{
						return "char";
					}
					else
					{
						printf("ERORR type for index of string must be int (<string>[<int>])in scope %s in func/proc %s\n",MYscope->name,globalScope->func[globalScope->Fcount-1]->name);
exit(1);						
					}
				else
				{
					printf("ERORR using index only on string type (<string>[<int>]) in scope %s in func/proc %s\n",MYscope->name,globalScope->func[globalScope->Fcount-1]->name);
exit(1);					
				}

			}
			else
			return tmp->var[i].type;

		}
		tmp=tmp->preScope;
	}
	printf("ERORR the variable %s not found in scope %s in func/proc %s\n ",tree->token,MYscope->name,globalScope->func[globalScope->Fcount-1]->name);
		exit(1);
}

Varaiable * mkArguments(node *tree,int *count){
	int i=0, j=0;
	Varaiable  *arr=NULL,arr2[50];
	char* type,*length;
	if(tree!=NULL)
	{
		node * temp1=tree,*tmp=tree;
		do{
		if(strcmp(temp1->token, "")==0)
		{
			tmp=temp1->right->left;
			temp1=temp1->left;
			
			
			if(strcmp(tmp->token, "(")==0||strcmp(tmp->token, "var")==0)
			{
				type=tmp->left->token;
				if(tmp->left->left!=NULL)
					length=tmp->left->left->left->token;
				node * tmptree;
				tmptree=tmp->right->left;
				do{
				arr2[*count].name=tmptree->token;
				arr2[*count].type=type;
				arr2[*count].length=length;
				(*count)++;
				if(tmptree->left==NULL)
					tmptree=NULL;
				else
					tmptree=tmptree->left->left;
				}while(tmptree!=NULL);
			}
		}
		}while(strcmp(temp1->token, "(")!=0&&strcmp(tmp->token, "var")!=0);
		tmp=temp1;
		if(strcmp(tmp->token, "(")==0||strcmp(tmp->token, "var")==0)
		{
			type=tmp->left->token;
			node * tmptree;
			if(strcmp(tmp->token, "var")==0)
			tmptree=tmp->right;
			else
			tmptree=tmp->right->left;
			if(tmp->left->left!=NULL)
			length=tmp->left->left->left->token;
			do{
			arr2[*count].name=tmptree->token;
			arr2[*count].type=type;
			arr2[*count].length=length;
			(*count)++;
			if(tmptree->left==NULL)
				tmptree=NULL;
			else
				tmptree=tmptree->left->left;
			}while(tmptree!=NULL);
		}
		arr=(Varaiable*)malloc(sizeof(Varaiable)*(*count));
		for(i=0;i<*count;i++)
		{
			for(j=0;j<*count;j++){
			}
			arr[i].name=arr2[i].name;
			arr[i].type=arr2[i].type;
		}
	}
	return arr;
}


Varaiable* callFuncArguments(SCOPE * MYscope,node *tree,int * count)
{
	int i=0, j=0;
	Varaiable  *arr=NULL,arr2[50];
	char* type,*length;
	while(tree!=NULL)
	{
		arr2[(*count)++].type=getExpType(tree->left,MYscope);
		if(tree->right!=NULL)
			tree=tree->right->left;
		else
			tree=NULL;

	}
	arr=(Varaiable*)malloc(sizeof(Varaiable)*(*count));
	for(i = 0; i<*count; i++)
		arr[i].type=arr2[i].type;
	return arr;
}
