#ifndef Scope
#define Scope
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include "AST.h"
extern int checkmain;

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

SCOPE* mkScope(char *);
SCOPE* finScope(SCOPE * scopes);
void addFunc(char * name,Varaiable * arguments,node *returnType,int argNum,SCOPE*);
void addVar(Varaiable * arguments,int,int,SCOPE * MYscope);
void syntaxAnalyzer(node *tree,SCOPE * scope);
void pushScopes(SCOPE* from,char*);
char* getExpType(node *,SCOPE*);
char* findFuncInScopes(node * tree,SCOPE * MYscope);
char* findVar(node * tree,SCOPE * MYscope);
Varaiable* mkArguments(node *,int *);
Varaiable* callFuncArguments(SCOPE *,node *tree,int * count);

#endif
