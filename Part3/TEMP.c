%{
/* includes, C defs */

#if YYBISON
int yylex(void);
int yyerror();
int printf(const char *format, ...);
char* yytext;
int debug;
struct node *tree;
#endif

#include "symtab.c"
#include "tests.c"

#define YYSTYPE struct node*
%}

// Types
%token BOOL CHARPTR INTPTR REALPTR CHAR INT REAL TSTRING TVOID
// Keywords
%token IF ELSE WHILE FOR DO
%token VAR FUNCTION RET MAIN
// Values
%token IDENTIFIER DOUBLE INTEGER VNULL CHARACTER STRING BOOLT BOOLF
// Binary Operators
%token GT GTE LT LTE EQ NE ASSIGN PLUS MINUS MUL DIV AND OR
// Unary Operators
%token NOT ADDR LEN
// Other
%token '(' ')'  '{' '}' ','
%token '[' ']'

%left ',' '}' ')' ']'
%left GT GTE LT LTE EQ NE
%left PLUS MINUS
%left MUL DIV
%left AND OR
%left ASSIGN
%right ADDR NOT '{' '(' '['
%nonassoc IF ELSE WHILE FOR DO FUNCTION IDENTIFIER MAIN

%start program
%%
program:  code {
        takeSons(tree, $1);
        // free($1);
        dPrint("YACC - program 1\n"); 
    }

code: code func { 
    $$ = $1;
    addSon($$, $2);
    dPrint("YACC - code 2\n");
    }
    | {
        $$ = mknode("CODE");
        dPrint("YACC - code 5\n");
    }


func: FUNCTION type id '(' parameter_list INITPARAM ')' block_func  { 
        $$ = mknode("FUNCTION");
        addValue($$, $3->value);
        // free($3);
        addSon($$, $2);
        addSon($$, $5);
        $8->token = strdup("BODY");
        addSon($$, $8);
        $$->type = strdup($2->type);
        functionTests($$, $5);
        checkReturnType($$);
        dPrint("YACC - func 1\n");
    }
    | FUNCTION void id '(' parameter_list INITPARAM ')' block  {
        $$ = mknode("FUNCTION");
        addValue($$, $3->value);
        // free($3);
        addSon($$, $2);
        addSon($$, $5);
        $8->token = strdup("BODY");
        addSon($$, $8);
        $$->type = strdup($2->type);
        functionTests($$, $5);
        dPrint("YACC - procedure 1\n");
    }

SSPUSH: {ss_push(new_scope());}
INITPARAM: {init_function_args($0);}
SSPOP: {delete_scope();}

block: SSPUSH '{' declares stmts '}' SSPOP { 
        $$ = mknode("BLOCK"); 
        addSon($$, $3); 
        addSon($$, $4); 
        dPrint("YACC - block 1\n"); 
    }
    | SSPUSH '{' declares '}' SSPOP { 
        $$ = mknode("BLOCK"); 
        addSon($$, $3); 
        dPrint("YACC - block 1\n"); 
    }
    | SSPUSH '{' stmts '}' SSPOP { 
        $$ = mknode("BLOCK"); 
        addSon($$, $3); 
        dPrint("YACC - block 1\n"); 
    }
    | SSPUSH '{' '}' SSPOP { $$ = mknode("BLOCK"); }

block_func: SSPUSH '{' declares stmts return_stmt '}' SSPOP {
        $$ = mknode("BLOCK");
        addSon($$, $3);
        addSon($$, $4);
        addSon($$, $5);
        dPrint("YACC - block_func 1\n");
    }
    | SSPUSH '{' stmts return_stmt '}' SSPOP {
        $$ = mknode("BLOCK");
        addSon($$, $3);
        addSon($$, $4);
        dPrint("YACC - block_func 2\n");
    }
    | SSPUSH '{' declares return_stmt '}' SSPOP {
        $$ = mknode("BLOCK");
        addSon($$, $3);
        addSon($$, $4);
        dPrint("YACC - block_func 3\n");
    }
    | SSPUSH '{' return_stmt '}' SSPOP {
        $$ = mknode("BLOCK");
        addSon($$, $3);
        dPrint("YACC - block_func 4\n");
    }

parameter_list:  parameter_list ';' type args {
                    $$ = mknode("ARGS");
                    takeSons($$, $1);
                    // free($1);
                    takeSons($3, $4);
                    // free($4);
                    addSon($$, $3);
                    $3->type = strdup($3->value);
                    dPrint("YACC - parameter_list 1 ','\n");
                }
                |  type args {
                    $$ = mknode("ARGS");
                    takeSons($1, $2);
                    // free($2);
                    addSon($$, $1);
                    $1->type = strdup($1->value);
                    dPrint("YACC - parameter list 2\n");
                    }
                | { $$ = mknode("ARGS");
                    addValue($$, "NONE");
                    dPrint("YACC - parameter list 3\n");
                }

args: id { 
    $$ = mknode("");
    addSon($$,$1);
    dPrint("YACC - args 1\n");
    }
    | args ',' id { 
        $$ = $1;
        addSon($$, $3);
        dPrint("YACC - args 2\n");
    }

for_stmt: FOR '(' init ';' expression ';' update ')' stmt {
        $$ = mknode("FOR");
        addSon($$, $3);
        addSon($$, $5);
        addSon($$, $7);
        addSon($$, $9);
        conditionTests($5);
        dPrint("YACC - for_stmt");
    }

init:  assign_stmt {
        $$ = $1;
        assignOpTests($1);
    }

update: assign_stmt {
        $$ = mknode("FOR UPDATE");
        addSon($$, $1);
        assignOpTests($1);
        dPrint("YACC - update");
    }

if_stmt: IF '(' expression ')' stmt {
        $$ = mknode("IF");
        addSon($$, $3);
        addSon($$, $5);
        conditionTests($3);
        dPrint("YACC - if_stmt 1\n");
    }
    | IF '(' expression ')' stmt ELSE stmt {
        $$ = mknode("IF-ELSE");
        addSon($$, $3);
        addSon($$, $5);
        addSon($$, $7);
        conditionTests($3);
        dPrint("YACC - if_stmt 2\n");
    }

while_stmt: WHILE '(' expression ')' stmt {
        $$ = mknode("WHILE");
        addSon($$, $3);
        addSon($$, $5);
        conditionTests($3);
        dPrint("YACC - while_stmt 1\n");
    }
    | DO SSPUSH block SSPOP WHILE '(' expression ')' ';' {
        $$ = mknode("DO-WHILE");
        addSon($$, $7);
        addSon($$, $3);
        conditionTests($7);
        dPrint("YACC - while_stmt 3\n");
    }

stmts: stmts stmt { 
        $$ = mknode("STATEMENTS");
        takeSons($$, $1);
        addSon($$, $2);
        dPrint("YACC - stmts 1\n");
    }
    | stmt { $$ = mknode("STATEMENTS"); addSon($$, $1); dPrint("YACC - stmts 2\n"); }

stmt: if_stmt { $$ = $1;  dPrint("YACC - stmt 1\n");}
    | while_stmt { $$ = $1;  dPrint("YACC - stmt 2\n");}
    | for_stmt { $$ = $1;  dPrint("YACC - stmt 6\n");}
    | assign_stmt ';' { 
        $$ = $1; 
        assignOpTests($1); 
        dPrint("YACC - atomic_stmt 1\n");
    }
    | func_call ';' { $$ = $1;  dPrint("YACC - atomic_stmt 2\n");}
    | block { $$ = $1; }
    | return_stmt { $$ = $1; }
    
assign_stmt: lhs ASSIGN expression {
        
        $$ = mknode("=");
        addSon($$, $1);
        addSon($$, $3);
        dPrint("YACC - assign_stmt 1\n");
    }

declares: declares declare_opts {
        $$ = mknode("DECLARES");
        takeSons($$, $1);
        // free($1);
        addSon($$, $2);
        dPrint("YACC - declares 1\n");
    }
    | declare_opts {
        $$ = mknode("DECLARES"); 
        addSon($$, $1); 
        dPrint("YACC - declares 3\n");
    }

declare_opts: var_declare { $$ = $1; dPrint("YACC - declare_opts 1\n");}
    | str_declare { $$ = $1; $$->type = strdup("STRING"); dPrint("YACC - declare_opts 2\n");}
    | func { $$ = $1; dPrint("YACC - declare_opts 3\n");}

var_declare: VAR type var_dec_args ';' {
        $$ = mknode("VAR");
        takeSons($2, $3);
        addSon($$, $2);
        $$->type = strdup($2->value);
        for (int i = 0; i < $3->children_len; i++)  {
            node *var = $3->children[i];
            if (strcmp(var->token, "IDENTIFIER") != 0) 
                var = var->children[0];
            struct idInfo *id = ht_search(var->value);
            id->decl->type = strdup($$->type);
        }
        dPrint("YACC - var_declare\n");
    }

var_dec_args: id { 
        $$ = mknode("");
        addSon($$, $1);
        varDecTests($1);
        dPrint("YACC - var_dec_args 1\n");
    }
    | assign_stmt {
        $$ = mknode("");
        addSon($$, $1);
        varDecTests($1->children[0]);
        dPrint("YACC - var_dec_args 2\n");
    }
    | var_dec_args ',' id { 
        $$ = mknode("VAR");
        takeSons($$, $1);
        // free($1);
        addSon($$, $3);
        varDecTests($3);
        dPrint("YACC - var_dec_args 3\n");
    }
    | var_dec_args ',' assign_stmt {
        $$ = mknode("VAR");
        takeSons($$, $1);
        // free($1);
        addSon($$, $3);
        varDecTests($3->children[0]);
        dPrint("YACC - var_dec_args 4\n");
    }

str_declare: TSTRING str_declare_args ';' {
        $$ = mknode("VAR");
        node *t = mknode("TYPE");
        addValue(t, "STRING");
        takeSons(t, $2);
        $$->type = strdup(t->value);
        for (int i = 0; i < $2->children_len; i++)  {
            node *var = $2->children[i];
            if (strcmp(var->token, "=") == 0)  {
                struct idInfo *id = ht_search(var->children[0]->value);
                id->decl->type = strdup($$->type);
                assignOpTests(var);
            } else {
                struct idInfo *id = ht_search(var->value);
                id->decl->type = strdup($$->type);
                checkStringElement(var);
            }
        }
        addSon($$, t);
        dPrint("YACC - str_declare\n");
    }

str_declare_args: string_element {
        $$ = mknode("");
        addSon($$, $1);
        $1->type = strdup("STRING");
        varDecTests($1);
        dPrint("YACC - str_declare_args 2\n");
    }
    | assign_stmt {
        $$ = mknode("");
        addSon($$, $1);
        $1->type = strdup("STRING");
        varDecTests($1->children[0]);
        dPrint("YACC - str_declare_args 1\n");
    }
    | assign_stmt ',' str_declare_args {
        $$ = mknode("VAR");
        addSon($$, $1);
        takeSons($$, $3);
        // free($3);
        $1->type = strdup("STRING");
        varDecTests($1->children[0]);
        dPrint("YACC - str_declare_args 3\n");
    }
    | string_element ',' str_declare_args {
        $$ = mknode("VAR");
        addSon($$, $1);
        takeSons($$, $3);
        // free($3);
        $1->type = strdup("STRING");
        varDecTests($1);
        dPrint("YACC - str_declare_args 4\n");
    }

func_call: id '(' expression_list ')' {
        $$ = mknode("CALL");
        addValue($$, $1->value);
        // free($1);
        addSon($$, $3);
        funcCallTests($$, $3);
        dPrint("YACC - func_call 1\n");
    }
    | id '(' ')' {
        $$ = mknode("CALL");
        addValue($$, $1->value);
        // free($1);
        addSon($$, mknode("ARGS NONE"));
        funcCallTests($$, $$->children[0]);
        dPrint("YACC - func_call 2\n");
    }

expression_list: expression {
        $$ = mknode("ARGS");
        addSon($$, $1);
        dPrint("YACC - expression_list 1\n");
    }
    | expression_list ',' expression {
        $$ = $1;
        addSon($$, $3);
        dPrint("YACC - expression_list 2\n");
    }

return_stmt: RET expression ';' {
        $$ = mknode("RETURN");
        addSon($$, $2);
        $$->type = decideType($2);
        dPrint("YACC - return_stmt\n");
    }

expression: expression GT expression {
        $$ = mknode(">");
        addSon($$, $1);
        addSon($$, $3);
        comparisonAndArithmeticOpTests($1, $3, $$->token);
        $$->type = strdup("BOOL");
        dPrint("YACC - expression 1\n");
    }
    | expression GTE expression {
        $$ = mknode(">=");
        addSon($$, $1);
        addSon($$, $3);
        comparisonAndArithmeticOpTests($1, $3, $$->token);
        $$->type = strdup("BOOL");
        dPrint("YACC - expression 2\n");
    }
    | expression LT expression {
        $$ = mknode("<");
        addSon($$, $1);
        addSon($$, $3);
        comparisonAndArithmeticOpTests($1, $3, $$->token);
        $$->type = strdup("BOOL");
        dPrint("YACC - expression 3\n");
    }
    | expression LTE expression {
        $$ = mknode("<=");
        addSon($$, $1);
        addSon($$, $3);
        comparisonAndArithmeticOpTests($1, $3, $$->token);
        $$->type = strdup("BOOL");
        dPrint("YACC - expression 4\n");
    }
    | expression EQ expression {
        $$ = mknode("==");
        addSon($$, $1);
        addSon($$, $3);
        equalityOpTests($1, $3, $$->token);
        $$->type = strdup("BOOL");
        dPrint("YACC - expression 5\n");
    }
    | expression NE expression {
        $$ = mknode("!=");
        addSon($$, $1);
        addSon($$, $3);
        equalityOpTests($1, $3, $$->token);
        $$->type = strdup("BOOL");
        dPrint("YACC - expression 6\n");
    }
    | expression PLUS expression {
        $$ = mknode("+");
        addSon($$, $1);
        addSon($$, $3);
        comparisonAndArithmeticOpTests($1, $3, $$->token);
        $$->type = strdup(decideType($1));
        dPrint("YACC - expression 7\n");
    }
    | expression MINUS expression {
        $$ = mknode("-");
        addSon($$, $1);
        addSon($$, $3);
        comparisonAndArithmeticOpTests($1, $3, $$->token);
        $$->type = strdup(decideType($1));
        dPrint("YACC - expression 8\n");
    }
    | expression MUL expression {
        $$ = mknode("*");
        addSon($$, $1);
        addSon($$, $3);
        comparisonAndArithmeticOpTests($1, $3, $$->token);
        $$->type = strdup(decideType($1));
        dPrint("YACC - expression 9\n");
    }
    | expression DIV expression {
        $$ = mknode("/");
        addSon($$, $1);
        addSon($$, $3);
        comparisonAndArithmeticOpTests($1, $3, $$->token);
        $$->type = strdup(decideType($1));
        dPrint("YACC - expression 10\n");
    }
    | expression AND expression {
        $$ = mknode("&&");
        addSon($$, $1);
        addSon($$, $3);
        logicalOpTests($1, $3, $$->token);
        $$->type = strdup(decideType($1));
        dPrint("YACC - expression 11\n");
    }
    | expression OR expression {
        $$ = mknode("||");
        addSon($$, $1);
        addSon($$, $3);
        logicalOpTests($1, $3, $$->token);
        $$->type = strdup(decideType($1));
        dPrint("YACC - expression 12\n");
    }
    | NOT expression {
        $$ = mknode("!");
        addSon ($$, $2);
        notOpTest($2);
        $$->type = strdup(decideType($2));
        dPrint("YACC - expression 13\n");
    }
    | '(' expression ')' {
        $$ = $2;
        dPrint("YACC - expression 14\n");
    }
    | val { 
        $$ = $1;
        dPrint("YACC - expression 15\n");
    }
    | ADDR expression {
        $$ = mknode("&");
        addSon($$, $2);
        addrOpTest($2);
        $$->type = strdup(decideType($2));
        dPrint("YACC - expression 17\n");
    }
    | deref {
        $$ = $1;
        dPrint("YACC - expression 18\n");
    }

deref: MUL expression {
        $$ = mknode("DEREF");
        addSon($$, $2);
        derefOpTest($2);
        $$->type = strdup(decideType($2));
        dPrint("YACC - deref 1\n");
    }

type: BOOL      {
    $$ = mknode("TYPE"); 
    addValue($$, "BOOL");
    $$->type = strdup($$->value);
    dPrint("YACC - type 1\n");
    }
    | CHARPTR   {
    $$ = mknode("TYPE");
    addValue($$, "CHARPTR");
    $$->type = strdup($$->value);
    dPrint("YACC - type 2\n");
    }
    | INTPTR    {
    $$ = mknode("TYPE");
    addValue($$, "INTPTR"); 
    $$->type = strdup($$->value);
    dPrint("YACC - type 3\n");
    }
    | REALPTR   {
    $$ = mknode("TYPE");
    addValue($$, "REALPTR"); 
    $$->type = strdup($$->value);
    dPrint("YACC - type 4\n");
    }
    | CHAR      {
    $$ = mknode("TYPE");
    addValue($$, "CHAR"); 
    $$->type = strdup($$->value);
    dPrint("YACC - type 5\n");
    }
    | INT       {
    $$ = mknode("TYPE");
    addValue($$, "INT"); 
    $$->type = strdup($$->value);
    dPrint("YACC - type 6\n");
    }
    | REAL      {
    $$ = mknode("TYPE");
    addValue($$, "REAL"); 
    $$->type = strdup($$->value);
    dPrint("YACC - type 7\n");
    }

void: TVOID { 
    $$ = mknode("TYPE"); 
    addValue($$, "VOID");
    $$->type = strdup($$->value);
    dPrint("YACC - void 1\n");
    }

string_element: id '[' expression ']' {
    $$ = mknode("STRING ELEMENT");
    addValue($$, $1->value);
    // free($1);
    addSon($$, $3);
    $$->type = strdup("STRING");
    dPrint("YACC - string_element 1\n");
}

lhs: id { $$ = $1; dPrint("YACC - lhs 1\n");}
    | string_element { 
        $$ = $1;
        dPrint("YACC - lhs 2\n");
    }
    | deref { $$ = $1; dPrint("YACC - lhs 3\n");}


val: id { $$ = $1; dPrint("YACC - val 1\n"); }
    | INTEGER { $$ = mknode("INTEGER"); addValue($$, yytext); $$->type = strdup("INT"); dPrint("YACC - val 2\n"); }
    | CHARACTER { $$ = mknode("CHARACTER"); addValue($$, yytext); $$->type = strdup("CHAR"); dPrint("YACC - val 3\n"); }
    | DOUBLE { $$ = mknode("DOUBLE"); addValue($$, yytext); $$->type = strdup("REAL"); dPrint("YACC - val 4\n"); }
    | string_literal { $$ = $1; dPrint("YACC - val 5\n"); }
    | boolean { $$ = $1; dPrint("YACC - val 6\n"); }
    | length { $$ = $1; dPrint("YACC - val 7\n"); }
    | VNULL { $$ = mknode("NULL"); $$->type = strdup("NULL"); dPrint("YACC - val 8\n"); }
    | string_element { 
        $$ = $1;
        dPrint("YACC - val 9\n");
    }
    | func_call { $$ = $1; }

id: IDENTIFIER { 
    $$ = mknode("IDENTIFIER"); 
    addValue($$, yytext);
    dPrint("YACC - id 1\n"); 
}

string_literal: STRING { $$ = mknode("STRING LITERAL"); addValue($$, yytext); $$->type = strdup("STRING"); dPrint("YACC - string_literal\n"); }

length: LEN id LEN {
        $$ = mknode("LEN");
        addSon($$, $2);
        if(!isString($2)) {
            printf("| | operand should be of type string.\n");
            exit(1);
        }
        $$->type = strdup(ht_search($2->value)->decl->type);
        dPrint("YACC - length 1\n");
    }
    | LEN string_literal LEN {
        $$ = mknode("STRING LEN");
        addSon($$, $2);
        $$->type = strdup("INT");
        dPrint("YACC - length 2\n");
    }

boolean: BOOLT { $$ = mknode("BOOLEAN"); addValue($$, yytext); $$->type = strdup("BOOL"); dPrint("YACC - boolean 1\n"); }
    | BOOLF { $$ = mknode("BOOLEAN"); addValue($$, yytext); $$->type = strdup("BOOL"); dPrint("YACC - boolean 2\n"); }

%%
#include "lex.yy.c"

int debug = 0;

int main()
{
    tree = mknode("CODE"); // declared in "ast.c", which is included in "symtab.c" which is included here
    ht = ht_init();
    stack = ss_init(new_scope());
    int ast_result = yyparse(); // 0 - success, 1 - fail by invalid input, 2 - fail by memory exhaustion
    if (ast_result == 0) {
        printNode(tree, 0);
        printf("End of parsing - Success!\n");
    }
    else {
        printf("End of parsing - Fail!\n");
    }
    // preOrderChecks(tree); // tree root as current node
    freemem(tree);
    return ast_result;
}

int yyerror(){
    extern int yylineno;
 	fflush(stdout);
	printf("Error: at line %d\n", yylineno);
	printf("Parser does not expect '%s'\n",yytext);
	return 0;
}
