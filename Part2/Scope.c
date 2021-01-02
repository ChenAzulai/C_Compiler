#include "Scope.h"

static int scopeAmount=0;
int AdditionalMain=0;
SCOPE* globalScope=NULL;


void addVar(Var * args,int numOfVars,int isArg,SCOPE * CurrScope){
	int i=0, j=0;
	if(numOfVars==0)
		return;
	Var* Temp;
	SCOPE * thisScope=CurrScope;
	for(i=0;i<numOfVars;i++)
		for(j=0;j<numOfVars;j++)
	if(i!=j && strcmp(args[j].name,args[i].name)==0)
	{
		printf("Syntax Error: Not allowed to declare the same Var '%s' more then once in the same scope",args[i].name);
		SCOPE * t=thisScope->upperScope;
		while(t->upperScope!=NULL && t->upperScope->FuncCount==0)
			t=t->upperScope;
		if(t->func!=NULL)
		printf("( in %s() )\n",t->func[t->FuncCount-1]->name);
			else
		printf("\n");
		exit(1);
	}
	if(thisScope->var==NULL)
	{ 
		thisScope->var=(Var*)malloc(sizeof(Var)*numOfVars);
	}
	else
	{
		Temp=thisScope->var;
		thisScope->var=(Var*) malloc(sizeof(Var)*(thisScope->VarCount+numOfVars));
		for(i=0;i<thisScope->VarCount;i++)
		{
			for(j=0;j<numOfVars;j++)
			{
				if(strcmp(Temp[i].name,args[j].name)==0 )
				{
					printf("Syntax Error: %s Var Was declared in the same scope",Temp[i].name);
					SCOPE * t=thisScope->upperScope;
					while(t->upperScope!=NULL && t->upperScope->FuncCount==0)
						t=t->upperScope;
					if(t->func!=NULL)
					printf(",inside Func %s() !\n",t->func[t->FuncCount-1]->name);
					else
					printf("\n");
					exit(1);
				}
			}
			thisScope->var[i]=Temp[i];	
		}
	}
	j=0;
	while(j<numOfVars)
	{
		thisScope->var[thisScope->VarCount].name=args[j].name;
		thisScope->var[thisScope->VarCount].value=NULL;
		thisScope->var[thisScope->VarCount].isArg=isArg;
		thisScope->var[thisScope->VarCount].len=args[j].len;
		thisScope->var[(thisScope->VarCount)++].type=args[j].type;
		j++;
	}

}


char * getExprType(node * tree,SCOPE* CurrScope){
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
		if(strcmp(getExprType(tree->left,CurrScope),"boolean")==0)
			msg="boolean";
		else
		{
			printf("Syntax Error: The ! Operator can be used only for boolean types!\n");
			exit(1);
		
		}
		if(strcmp(tree->token,"|")==0)
		if(strcmp(getExprType(tree->left,CurrScope),"string")==0)
		msg="int";
		else{
			printf("Syntax Error: in '%s' - The | Operator can be used only on String type! \n",globalScope->func[globalScope->FuncCount-1]->name);
			exit(1);
			
		}
		if(strcmp(tree->token,"==")==0||strcmp(tree->token,"!=")==0)
		{
			if(strcmp(getExprType(tree->left,CurrScope),getExprType(tree->right,CurrScope))==0&&strcmp(getExprType(tree->right,CurrScope),"string")!=0)
			msg="boolean";
			else{
				printf("Syntax Error: in '%s' invalid use of '%s' Operator between '%s' and '%s'\n",globalScope->func[globalScope->FuncCount-1]->name,tree->token,getExprType(tree->left,CurrScope),getExprType(tree->right,CurrScope));
			exit(1);	
			}
		}

		if(strcmp(tree->token,">=")==0||strcmp(tree->token,">")==0||strcmp(tree->token,"<=")==0||strcmp(tree->token,"<")==0)
		{
			if((strcmp(getExprType(tree->left,CurrScope),"int")==0||strcmp(getExprType(tree->left,CurrScope),"real")==0)&&(strcmp(getExprType(tree->right,CurrScope),"int")==0||strcmp(getExprType(tree->right,CurrScope),"real")==0))
			msg="boolean";
			else{
				printf("Syntax Error: in '%s' invalid use of %s Operator between '%s' and '%s' \n",globalScope->func[globalScope->FuncCount-1]->name,tree->token,getExprType(tree->left,CurrScope),getExprType(tree->right,CurrScope));
				exit(1);
	
			}
		}

		if(strcmp(tree->token,"&&")==0||strcmp(tree->token,"||")==0)
		{

			if(strcmp(getExprType(tree->left,CurrScope),getExprType(tree->right,CurrScope))==0&&strcmp(getExprType(tree->right,CurrScope),"boolean")==0)
			msg="boolean";
			else{
				printf("Syntax Error: in '%s' invalid use of %s Operator  between %s and %s\n",globalScope->func[globalScope->FuncCount-1]->name,tree->token,getExprType(tree->left,CurrScope),getExprType(tree->right,CurrScope));
				exit(1);
				
			}
			

		}
		if(strcmp(tree->token,"-")==0||strcmp(tree->token,"+")==0)
		{
			if((strcmp(getExprType(tree->left,CurrScope),"int")==0||strcmp(getExprType(tree->left,CurrScope),"real")==0)&&(strcmp(getExprType(tree->right,CurrScope),"int")==0||strcmp(getExprType(tree->right,CurrScope),"real")==0))
			{
			if(strcmp(getExprType(tree->left,CurrScope),getExprType(tree->right,CurrScope))==0&&strcmp(getExprType(tree->left,CurrScope),"int")==0)
			msg="int";
			else
			msg="real";
			}

			if(strcmp(getExprType(tree->right,CurrScope),"int")==0&&(strcmp(getExprType(tree->left,CurrScope),"char*")==0||strcmp(getExprType(tree->right,CurrScope),"int*")==0||strcmp(getExprType(tree->right,CurrScope),"real*")==0)){
				msg=getExprType(tree->left,CurrScope);
			}
			else if(strcmp(msg,"")==0)
			{
				printf("Syntax Error: in '%s' invalid use of %s Operator  between %s and %s= in Func\\Proc %s\n",globalScope->func[globalScope->FuncCount-1]->name,tree->token,getExprType(tree->left,CurrScope),getExprType(tree->right,CurrScope));
				exit(1);
				
			}

		}
		if(strcmp(tree->token,"*")==0||strcmp(tree->token,"/")==0)
		{
			if((strcmp(getExprType(tree->left,CurrScope),"int")==0||strcmp(getExprType(tree->left,CurrScope),"real")==0)&&(strcmp(getExprType(tree->right,CurrScope),"int")==0||strcmp(getExprType(tree->right,CurrScope),"real")==0))
			{
			if(strcmp(getExprType(tree->left,CurrScope),getExprType(tree->right,CurrScope))==0&&strcmp(getExprType(tree->left,CurrScope),"int")==0)
			msg="int";
			else
			msg="real";
			}
			else
			{
				printf("Syntax Error: in '%s' invalid use of '%s' Operator  between '%s' and '%s' in Func\\Proc '%s'\n",globalScope->func[globalScope->FuncCount-1]->name,tree->token,getExprType(tree->left,CurrScope),getExprType(tree->right,CurrScope));
				exit(1);
				
			}
		}
		if(strcmp(tree->token,"&")==0)
		{
			if(strcmp(tree->left->token,"(")==0)
				msg=getExprType(tree->left->left,CurrScope);
			else{
				msg=getExprType(tree->left,CurrScope);
				
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
				printf("Syntax Error: Incorrect use of %s on %s \n",tree->token,msg);
				exit(1);
			}
		}
		if(strcmp(tree->token,"^")==0)
		{
			if(strcmp(tree->left->token,"(")==0)
				msg=getExprType(tree->left->left,CurrScope);
			else
				msg=getExprType(tree->left,CurrScope);
			
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
				printf("Syntax Error: Incorrect use of %s on %s \n",tree->token,msg);
			exit(1);	
			}

		}
		if(strcmp(tree->token,"(")==0)
			msg=getExprType(tree->left,CurrScope);
		if(strcmp(tree->token,"Call func")==0)
			msg=findFuncInScopes(tree,CurrScope);
		
	}
	if(strcmp(msg,"")==0)
		msg=findVar(tree,CurrScope);

	
	

	return msg;
}
//--------------------Make New Scope---------------------------------
SCOPE* mkSCOPE(char* name)
{	
	SCOPE *newScope = (SCOPE*)malloc(sizeof(SCOPE));
	newScope->name=name;
	newScope->var=NULL;
	newScope->VarCount=0;
	newScope->func=NULL;
	newScope->FuncCount=0;
	newScope->innerScope=NULL;
	newScope->upperScope=NULL;
	return newScope;
}//--------------------------End Make New Sceope


void addFunction(char * name,Var * args,node *returnType,int argNum,SCOPE * CurrScope){
	int i=0, j=0;
	FuncOrProc** Temp;
	SCOPE * scopes = CurrScope;
	for(i=0;i<argNum;i++)
		for(j=0;j<argNum;j++)
	if(i!=j && strcmp(args[j].name,args[i].name)==0 )
	{
		printf("Syntax Error: Several arguments named: '%s' in Func %s() !\n",args[i].name,name);
	exit(1);	
	}
	if(scopes->func==NULL)
	{ 
		scopes->func=(FuncOrProc**) malloc(sizeof(FuncOrProc*));
	}
	else
	{
		Temp=scopes->func;
		scopes->func=(FuncOrProc**) malloc(sizeof(FuncOrProc*)*(scopes->FuncCount+1));
		for(i=0;i<scopes->FuncCount;i++)
		{		

				if(strcmp(Temp[i]->name,name)==0)
				{
					if(strcmp(Temp[i]->name,"Main")==0)
						{printf("Syntax Error: Allowed only One and only proc Main() in the Code!\n",Temp[i]->name);
						exit(1);}				
					if(strcmp(Temp[i]->name,name)==0)
						{printf("Syntax Error: The name: '%s' being used in the same scope by a different Func Or Proc! \n",Temp[i]->name);
						exit(1);}					
				}	
				scopes->func[i]=Temp[i];
		}
	}
		scopes->func[scopes->FuncCount]=(FuncOrProc*) malloc(sizeof(FuncOrProc));
		scopes->func[scopes->FuncCount]->name=name;
		scopes->func[scopes->FuncCount]->args=args;
		
		if(returnType==NULL)
		scopes->func[scopes->FuncCount]->returnType=NULL;
		else{
		if(strcmp(returnType->token,"string")==0)
			{
				printf("Syntax Error: in Func %s() Not Allowed string as a Return type! \n",name);
			exit(1);	
			}
		scopes->func[scopes->FuncCount]->returnType=returnType->token;
		}
		scopes->func[scopes->FuncCount]->argNum=argNum;
		scopes->func[scopes->FuncCount]->hasReturn=false;
		++(scopes->FuncCount); 

}


SCOPE* finalScope(SCOPE * scopes)
{
	SCOPE * CurrScope=scopes;
	if(CurrScope!=NULL)
	while(CurrScope->innerScope!=NULL)
		CurrScope=CurrScope->innerScope;
	return CurrScope;
}

void analayzeSyntax(node *tree,SCOPE * CurrScope){	
	if(strcmp(tree->token, "=") == 0 )
	{
		if(!(strcmp(getExprType(tree->right,CurrScope),"NULL")==0&& (strcmp(getExprType(tree->left,CurrScope),"real*")==0||strcmp(getExprType(tree->left,CurrScope),"int*")==0||strcmp(getExprType(tree->left,CurrScope),"char*")==0)))
		if(strcmp(getExprType(tree->left,CurrScope),getExprType(tree->right,CurrScope))!=0)
		{
			printf("Syntax Error: in '%s' Not Allowed '=' Operator to use between '%s' and '%s', not the same type!\n",globalScope->func[globalScope->FuncCount-1]->name,getExprType(tree->left,CurrScope),getExprType(tree->right,CurrScope),CurrScope->name);
		exit(1);	
		}
	}
	else if(strcmp(tree->token, "var") == 0)
	{
		int VarCount=0;
		Var * var=mkArgs(tree,&VarCount);
		addVar(var,VarCount,0,CurrScope);
		
		
	}
	else if(strcmp(tree->token, "if") == 0)
	{
		if(strcmp(getExprType(tree->left->left,CurrScope),"boolean")!=0)
		{
			printf("Syntax Error: Inside 'if' condition has to be Boolean Type!\n");
	exit(1);		
		}

		if(strcmp(tree->right->token,"{")!=0)
		{
			pushScopes(CurrScope,tree->token);
			if (tree->left) 
				analayzeSyntax(tree->left,finalScope( CurrScope->innerScope));
	
			if (tree->right)
				analayzeSyntax(tree->right,finalScope( CurrScope->innerScope));
        	scopeAmount--;
			return;
		}
		
		
		
	}
		else if(strcmp(tree->token, "while") == 0)
	{
		if(strcmp(getExprType(tree->left->left,CurrScope),"boolean")!=0)
		{
			printf("Syntax Error: Inside 'while' condition has to be Boolean Type!\n");
		exit(1);	
		}

		if(strcmp(tree->right->token,"{")!=0)
		{
			pushScopes(CurrScope,tree->token);
			if (tree->left) 
				analayzeSyntax(tree->left,finalScope(CurrScope->innerScope));
	
			if (tree->right)
				analayzeSyntax(tree->right,finalScope(CurrScope->innerScope));
        	scopeAmount--;
			return;
		}
				
		
	}
	else if(strcmp(tree->token, "FUNC") == 0 )
	{
        int count=0;
		Var * arg=mkArgs(tree->left->right->left,&count);
		addFunction(tree->left->token,arg,tree->left->right->right->left,count,CurrScope);
		pushScopes(CurrScope,tree->token);
		addVar(arg,count,1,finalScope(CurrScope));
	if (tree->left) 
		analayzeSyntax(tree->left,finalScope( CurrScope->innerScope));
	
	if (tree->right)
		analayzeSyntax(tree->right,finalScope( CurrScope->innerScope));
		if(CurrScope->func[CurrScope->FuncCount-1]->hasReturn==false)
		{
			printf("Syntax Error: Func %s() must have Return statment!\n",tree->left->token);
		exit(1);	
		}
        scopeAmount--;		
		return;
	}
    else if(strcmp(tree->token, "PROC") == 0)
	{
        int count=0;
		Var * arg=mkArgs(tree->right->left,&count);
	if(strcmp(tree->left->token,"Main")==0)
			{
				AdditionalMain=AdditionalMain+1;
			}
	if(strcmp(tree->right->token,"Main")==0)
			{
				AdditionalMain=2;
			}

	if(strcmp(tree->left->token,"Main")==0 && arg!=NULL)
        {
        	printf("Syntax Error: %s does not accept Arguments!\n",tree->left->token);
		exit(1);
        }
		addFunction(tree->left->token,arg,NULL,count,CurrScope);
		pushScopes(CurrScope,tree->token);
		addVar(arg,count,1,finalScope(CurrScope));
	if (tree->left) 
		analayzeSyntax(tree->left,finalScope( CurrScope->innerScope));
	
	if (tree->right)
		analayzeSyntax(tree->right,finalScope( CurrScope->innerScope));
		scopeAmount--;	
		return;
    }

	else if(strcmp(tree->token, "Call func") == 0)
	{
		findFuncInScopes(tree,CurrScope);
		
	}
	else if(strcmp(tree->token, "CODE") == 0)
	{
		pushScopes(NULL,tree->token);
	if (tree->left) 
		analayzeSyntax(tree->left,globalScope);
	
	if (tree->right)
		analayzeSyntax(tree->right,globalScope);
		scopeAmount--;
		return;
	}

    else if(strcmp(tree->token, "Main") == 0)
	{
		addFunction(tree->token,NULL,NULL,0,CurrScope);
		pushScopes(CurrScope,tree->token);
	if (tree->left) 
		analayzeSyntax(tree->left,finalScope( CurrScope->innerScope));
	
	if (tree->right)
		analayzeSyntax(tree->right,finalScope( CurrScope->innerScope));
        scopeAmount--;
		return;
               
    }       
	else if(strcmp(tree->token, "if-else") == 0)
	{
		if(strcmp(getExprType(tree->left->left,CurrScope),"boolean")!=0)
		{
			printf("Syntax Error: Inside 'if' condition has to be Boolean Type!\n");
		exit(1);	
		}

		if(strcmp(tree->right->left->token,"{")!=0)
		{
			pushScopes(CurrScope,tree->token);
			analayzeSyntax(tree->right->left,finalScope( CurrScope->innerScope));
			scopeAmount--;
			pushScopes(CurrScope,tree->token);
			analayzeSyntax(tree->right->right->left,finalScope( CurrScope->innerScope));
        	scopeAmount--;
			return;
		}
	}

	else if(strcmp(tree->token, "return") == 0)
	{
		SCOPE * Temp= CurrScope;
		int flag=true;
		while(strcmp(Temp->name,"FUNC")!=0&&strcmp(Temp->name,"PROC")!=0&&strcmp(Temp->name,"CODE")!=0)
		{
			Temp=Temp->upperScope;
			flag=false;
		}
		if(flag==false)
		{
			if(strcmp(getExprType(tree->left,CurrScope),Temp->upperScope->func[Temp->upperScope->FuncCount-1]->returnType))
			{
			printf("Syntax Error: Return Type doesn't match the Func %s() Type! \n",Temp->upperScope->func[Temp->upperScope->FuncCount-1]->name);
			printf("%s ,%s %s\n",getExprType(tree->left,CurrScope),Temp->upperScope->func[Temp->upperScope->FuncCount-1]->returnType,Temp->upperScope->func[Temp->upperScope->FuncCount-1]->name);
			exit(1);
			}
		}
		else
		{
			if(Temp->upperScope->func[Temp->upperScope->FuncCount-1]->returnType!=NULL)
			{
				if(0==strcmp(getExprType(tree->left,CurrScope),Temp->upperScope->func[Temp->upperScope->FuncCount-1]->returnType))
				{
					Temp->upperScope->func[Temp->upperScope->FuncCount-1]->hasReturn=true;
				}
				else
				{
					printf("Syntax Error: Return Type doesn't match the Func %s() Type! \n",Temp->upperScope->func[Temp->upperScope->FuncCount-1]->name);
					printf("%s ,%s %s\n",getExprType(tree->left,CurrScope),Temp->upperScope->func[Temp->upperScope->FuncCount-1]->returnType,Temp->upperScope->func[Temp->upperScope->FuncCount-1]->name);
					exit(1);
				}
			}
			else
			{
				printf("Syntax Error: invalid return value in procedure '%s'\n",Temp->upperScope->func[Temp->upperScope->FuncCount-1]->name);//Whats this printf mean?
			exit(1);	
			}  
		}  
	}
	else if(strcmp(tree->token, "{") == 0)
	{
    pushScopes(CurrScope,tree->token);
	if (tree->left) 
		analayzeSyntax(tree->left,finalScope( CurrScope->innerScope));
	
	if (tree->right)
		analayzeSyntax(tree->right,finalScope( CurrScope->innerScope));
        scopeAmount--;
		return;			
	}
	else if(strcmp(tree->token, "solovar") == 0 )
	{
		findVar(tree->left,CurrScope);
	}
	if (tree->left) 
		analayzeSyntax(tree->left,CurrScope);
	
	if (tree->right)
		analayzeSyntax(tree->right,CurrScope);
}


void pushScopes(SCOPE* from,char* name)
{
	SCOPE * point;
	if(globalScope==NULL)
		globalScope=mkSCOPE(name);
	else{
	point=globalScope;
	while(point->innerScope!=NULL)
		point=point->innerScope;
	point->innerScope=mkSCOPE(name);
	point->innerScope->upperScope=from;
	}
}
char* findFuncInScopes(node * tree,SCOPE * CurrScope)
{
	int i=0, j=0,t;
	SCOPE* Temp=CurrScope;
	Var* args;
	bool find = false, flag = true;
	while(Temp!=NULL)
	{
		for(i=0;i<Temp->FuncCount;i++)
		if(strcmp(tree->left->token,Temp->func[i]->name)==0)
		{
			find=true;
			flag=true;
			int count=0;
			args=callFuncArguments(CurrScope,tree->right->left,&count);
			if(count==Temp->func[i]->argNum)
			{
				for(j=0, t=count-1; j<count; j++,t--)
				{
					if(strcmp(args[j].type,Temp->func[i]->args[t].type)!=0)
						flag=false;
				}
				if(flag==true)
					return Temp->func[i]->returnType;
			}
		}
		Temp=Temp->upperScope;
	}
	printf("Syntax Error: Func %s() can not be called in the scope or Above because it was not declared\n",tree->left->token,CurrScope->name,globalScope->func[globalScope->FuncCount-1]->name);
	if(find==true)
	{
		printf("Syntax Error: A Function Has already been Declared with the same name but different arguments!\n");
		exit(1);
	}	
}

char *findVar(node * tree,SCOPE * CurrScope)
{
	int j=0, i=0;
	SCOPE* Temp = CurrScope;
	if(strcmp(tree->token,"solovar")==0)
		tree=tree->left;
	while(Temp!=NULL)
	{
		for(i=0;i<Temp->VarCount;i++)
		if(strcmp(tree->token,Temp->var[i].name)==0)
		{
			
			if(tree->left!=NULL && strcmp(tree->left->token,"[")==0)
			{
				if(strcmp(Temp->var[i].type,"string")==0)
					if(strcmp(getExprType(tree->left->left,CurrScope),"int")==0)
					{
						return "char";
					}
					else
					{
						printf("Syntax Error: The index of string must be int: <string>[int]\n",CurrScope->name,globalScope->func[globalScope->FuncCount-1]->name);
exit(1);						
					}
				else
				{
					printf("Syntax Error: using index only on string type <string>[int] in scope %s in Func\\Proc! %s\n",CurrScope->name,globalScope->func[globalScope->FuncCount-1]->name);
exit(1);					
				}

			}
			else
			return Temp->var[i].type;

		}
		Temp=Temp->upperScope;
	}
	printf("Syntax Error: the var '%s' was not declared at all\n" ,tree->token,CurrScope->name,globalScope->func[globalScope->FuncCount-1]->name);
		exit(1);
}

Var * mkArgs(node *tree,int *count){
	int i=0, j=0;
	Var  *arr=NULL,arr2[50];
	char* type,*len;
	if(tree!=NULL)
	{
		node * temp1=tree,*Temp=tree;
		do{
		if(strcmp(temp1->token, "")==0)
		{
			Temp=temp1->right->left;
			temp1=temp1->left;
			
			
			if(strcmp(Temp->token, "(")==0||strcmp(Temp->token, "var")==0)
			{
				type=Temp->left->token;
				if(Temp->left->left!=NULL)
					len=Temp->left->left->left->token;
				node * tmptree;
				tmptree=Temp->right->left;
				do{
				arr2[*count].name=tmptree->token;
				arr2[*count].type=type;
				arr2[*count].len=len;
				(*count)++;
				if(tmptree->left==NULL)
					tmptree=NULL;
				else
					tmptree=tmptree->left->left;
				}while(tmptree!=NULL);
			}
		}
		}while(strcmp(temp1->token, "(")!=0&&strcmp(Temp->token, "var")!=0);
		Temp=temp1;
		if(strcmp(Temp->token, "(")==0||strcmp(Temp->token, "var")==0)
		{
			type=Temp->left->token;
			node * tmptree;
			if(strcmp(Temp->token, "var")==0)
			tmptree=Temp->right;
			else
			tmptree=Temp->right->left;
			if(Temp->left->left!=NULL)
			len=Temp->left->left->left->token;
			do{
			arr2[*count].name=tmptree->token;
			arr2[*count].type=type;
			arr2[*count].len=len;
			(*count)++;
			if(tmptree->left==NULL)
				tmptree=NULL;
			else
				tmptree=tmptree->left->left;
			}while(tmptree!=NULL);
		}
		arr=(Var*)malloc(sizeof(Var)*(*count));
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


Var* callFuncArguments(SCOPE * CurrScope,node *tree,int * count)
{
	int i=0, j=0;
	Var  *arr=NULL,arr2[50];
	char* type,*len;
	while(tree!=NULL)
	{
		arr2[(*count)++].type=getExprType(tree->left,CurrScope);
		if(tree->right!=NULL)
			tree=tree->right->left;
		else
			tree=NULL;

	}
	arr=(Var*)malloc(sizeof(Var)*(*count));
	for(i = 0; i<*count; i++)
		arr[i].type=arr2[i].type;
	return arr;
}
