#include "Scope.h"

static int scope=0;
int AdditionalMain=0;
SCOPE* globalScope=NULL;


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
		printf("Syntax Error: Not allowed to declare the same Var '%s' more then once in the same scope",arguments[i].name);
		SCOPE * t=scopes->preScope;
		while(t->preScope!=NULL && t->preScope->Fcount==0)
			t=t->preScope;
		if(t->func!=NULL)
		printf("( in %s() )\n",t->func[t->Fcount-1]->name);
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
					printf("Syntax Error: %s Var Was declared in the same scope",tmp[i].name);
					SCOPE * t=scopes->preScope;
					while(t->preScope!=NULL && t->preScope->Fcount==0)
						t=t->preScope;
					if(t->func!=NULL)
					printf(",inside Func %s() !\n",t->func[t->Fcount-1]->name);
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
			printf("Syntax Error: The ! Operator can be used only for boolean types!\n");
			exit(1);
		
		}
		if(strcmp(tree->token,"|")==0)
		if(strcmp(getExpType(tree->left,MYscope),"string")==0)
		msg="int";
		else{
			printf("Syntax Error: The | (OR) Operator can be used only on string type in Func\\Proc %s! \n",globalScope->func[globalScope->Fcount-1]->name);
			exit(1);
			
		}
		if(strcmp(tree->token,"==")==0||strcmp(tree->token,"!=")==0)
		{
			if(strcmp(getExpType(tree->left,MYscope),getExpType(tree->right,MYscope))==0&&strcmp(getExpType(tree->right,MYscope),"string")!=0)
			msg="boolean";
			else{
				printf("Syntax Error: invalid use of '%s' Operator between '%s' and '%s'\n",tree->token,getExpType(tree->left,MYscope),getExpType(tree->right,MYscope),globalScope->func[globalScope->Fcount-1]->name);
			exit(1);	
			}
		}

		if(strcmp(tree->token,">=")==0||strcmp(tree->token,">")==0||strcmp(tree->token,"<=")==0||strcmp(tree->token,"<")==0)
		{
			if((strcmp(getExpType(tree->left,MYscope),"int")==0||strcmp(getExpType(tree->left,MYscope),"real")==0)&&(strcmp(getExpType(tree->right,MYscope),"int")==0||strcmp(getExpType(tree->right,MYscope),"real")==0))
			msg="boolean";
			else{
				printf("Syntax Error: invalid use of %s Operator between '%s' and '%s' \n",tree->token,getExpType(tree->left,MYscope),getExpType(tree->right,MYscope),globalScope->func[globalScope->Fcount-1]->name);
				exit(1);
	
			}
		}

		if(strcmp(tree->token,"&&")==0||strcmp(tree->token,"||")==0)
		{

			if(strcmp(getExpType(tree->left,MYscope),getExpType(tree->right,MYscope))==0&&strcmp(getExpType(tree->right,MYscope),"boolean")==0)
			msg="boolean";
			else{
				printf("Syntax Error: invalid use of %s Operator  between %s and %s in Func\\Proc %s\n",tree->token,getExpType(tree->left,MYscope),getExpType(tree->right,MYscope),globalScope->func[globalScope->Fcount-1]->name);
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
				printf("Syntax Error: invalid use of %s Operator  between %s and %s= in Func\\Proc %s\n",tree->token,getExpType(tree->left,MYscope),getExpType(tree->right,MYscope),globalScope->func[globalScope->Fcount-1]->name);
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
				printf("Syntax Error: invalid use of %s Operator  between %s and %s in Func\\Proc %s\n",tree->token,getExpType(tree->left,MYscope),getExpType(tree->right,MYscope),globalScope->func[globalScope->Fcount-1]->name);
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
				printf("Syntax Error: Incorrect use of %s on %s \n",tree->token,msg);
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
				printf("Syntax Error: Incorrect use of %s on %s \n",tree->token,msg);
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
		printf("Syntax Error: Several arguments named: %s in Func %s() !\n",arguments[i].name,name);
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

				if(strcmp(tmp[i]->name,name)==0)
				{
					if(strcmp(tmp[i]->name,"Main")==0)
						{printf("Syntax Error: Allowed only One and only proc Main() in the Code!\n",tmp[i]->name);
						exit(1);}				
					if(strcmp(tmp[i]->name,name)==0)
						{printf("Syntax Error: The name: '%s' being used in the same scope by a different Func Or Proc! \n",tmp[i]->name);
						exit(1);}					
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
				printf("Syntax Error: in Func %s() Not Allowed string as a Return type! \n",name);
			exit(1);	
			}
		scopes->func[scopes->Fcount]->returnType=returnType->token;
		}
		scopes->func[scopes->Fcount]->argNum=argNum;
		scopes->func[scopes->Fcount]->findreturn=false;
		++(scopes->Fcount); 

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
			printf("Syntax Error: Not Allowed '=' Operator to use between '%s' and '%s' not the same type!\n",getExpType(tree->left,MYscope),getExpType(tree->right,MYscope),MYscope->name,globalScope->func[globalScope->Fcount-1]->name);
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
			printf("Syntax Error: Inside 'if' condition has to be Boolean Type!\n");
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
			printf("Syntax Error: Inside 'while' condition has to be Boolean Type!\n");
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
			printf("Syntax Error: Func %s() must have Return statment!\n",tree->left->token);
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
			printf("Syntax Error: Inside 'if' condition has to be Boolean Type!\n");
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
			printf("Syntax Error: Return Type doesn't match the Func %s() Type! \n",tmp->preScope->func[tmp->preScope->Fcount-1]->name);
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
					printf("Syntax Error: Return Type doesn't match the Func %s() Type! \n",tmp->preScope->func[tmp->preScope->Fcount-1]->name);
					printf("%s ,%s %s\n",getExpType(tree->left,MYscope),tmp->preScope->func[tmp->preScope->Fcount-1]->returnType,tmp->preScope->func[tmp->preScope->Fcount-1]->name);
					exit(1);
				}
			}
			else
			{
				printf("Syntax Error: invalid return value in procedure %s\n",tmp->preScope->func[tmp->preScope->Fcount-1]->name);//Whats this printf mean?
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
	printf("Syntax Error: Func %s() can not be called in the scope or Above because it was not declared\n",tree->left->token,MYscope->name,globalScope->func[globalScope->Fcount-1]->name);
	if(find==true)
	{
		printf("Syntax Error: A Function Has already been Declared with the same name but different arguments!\n");
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
						printf("Syntax Error: The index of string must be int: <string>[int]\n",MYscope->name,globalScope->func[globalScope->Fcount-1]->name);
exit(1);						
					}
				else
				{
					printf("Syntax Error: using index only on string type <string>[int] in scope %s in Func\\Proc! %s\n",MYscope->name,globalScope->func[globalScope->Fcount-1]->name);
exit(1);					
				}

			}
			else
			return tmp->var[i].type;

		}
		tmp=tmp->preScope;
	}
	printf("Syntax Error: the var '%s' was not declared at all\n" ,tree->token,MYscope->name,globalScope->func[globalScope->Fcount-1]->name);
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
