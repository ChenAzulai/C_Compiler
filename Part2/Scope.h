#ifndef Scope
#define Scope
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include "AST.h"

typedef struct Varaiables
	{int isArg;
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
}Function;


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

extern SCOPE* globalScope;
extern int AdditionalMain;

SCOPE* mkScope(char *);
SCOPE* finScope(SCOPE *);
void addFunc(char * , Varaiable * , node *, int , SCOPE*);
void addVar(Varaiable * , int , int , SCOPE *);
void syntaxAnalyzer(node *, SCOPE *);
void pushScopes(SCOPE* , char*);
char* getExpType(node * , SCOPE*);
char* findFuncInScopes(node * , SCOPE *);
char* findVar(node *, SCOPE *);
Varaiable* mkArguments(node * , int *);
Varaiable* callFuncArguments(SCOPE *, node *,int * );

#endif
