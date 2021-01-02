#ifndef Scope
#define Scope
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include "AST.h"

typedef struct Var
{
	char *name;
	char *value;
	char *type;
	char * len;
	int isArg;
}Var;

typedef struct FuncOrProc 
{
	char * name;
	char *returnType; 
	int argNum;
	bool hasReturn;
	Var * args;
}FuncOrProc;


typedef struct SCOPE
{	
	char *name;
	Var * var;
	int VarCount;
	int FuncCount;
	FuncOrProc ** func;
	struct SCOPE * innerScope;
	struct SCOPE * upperScope;
}SCOPE;

extern SCOPE* globalScope;
extern int AdditionalMain;
SCOPE* mkSCOPE(char *);
SCOPE* finalScope(SCOPE *);
void addFunction(char * , Var * , node *, int , SCOPE*);
void addVar(Var * , int , int , SCOPE *);
void analayzeSyntax(node *, SCOPE *);
void pushScopes(SCOPE* , char*);
char* getExprType(node * , SCOPE*);
char* findFuncInScopes(node * , SCOPE *);
char* findVar(node *, SCOPE *);
Var* mkArgs(node * , int *);
Var* callFuncArguments(SCOPE *, node *,int * );

#endif
