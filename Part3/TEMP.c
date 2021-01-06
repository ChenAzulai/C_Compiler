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
#include "3ac.c"

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
%left AND OR
%left GT GTE LT LTE EQ NE
%left MUL DIV
%left PLUS MINUS
%left ASSIGN
%right ADDR NOT '{' '(' '['
%nonassoc IF ELSE WHILE FOR DO FUNCTION IDENTIFIER MAIN

%start program
%%
program:  code {
        takeSons(tree, $1);
        tree->code = gen(2, $1->code, "\n");
        free($1);
    }

code: code func { 
    $$ = $1;
    addSon($$, $2);
    $$->code = gen(3, $1->code, "\n", $2->code);
    dPrint("YACC - code 1\n");
    }
    | {
        $$ = mknode("CODE");
        $$->code = strdup("");
    dPrint("YACC - code 2\n");
    }


func: FUNCTION type id '(' parameter_list SSPUSH INITPARAM ')' block_func SSPOP { 
        $$ = mknode("FUNCTION");
        addValue($$, $3->value);
        free($3);
        addSon($$, $2);
        addSon($$, $5);
        $9->token = strdup("BODY");
        addSon($$, $9);
        $$->type = strdup($2->type);
        functionTests($$, $5);
        checkReturnType($$);
        $$->code = gen(4, $3->value, ":\n\tBeginFunc", $9->code, "\n\tEndFunc");
        dPrint("YACC - func 1\n");
    }
    | FUNCTION void id '(' parameter_list SSPUSH INITPARAM ')' block SSPOP  {
        $$ = mknode("FUNCTION");
        addValue($$, $3->value);
        free($3);
        addSon($$, $2);
        addSon($$, $5);
        $9->token = strdup("BODY");
        addSon($$, $9);
        $$->type = strdup($2->type);
        functionTests($$, $5);
        $$->code = gen(4, $3->value, ":\n\tBeginFunc\n", $9->code, "\n\tEndFunc");
        dPrint("YACC - func 2\n");
    }

SSPUSH: {ss_push(new_scope());}
INITPARAM: {init_function_args($-1);}
SSPOP: {delete_scope();}

block: '{' declares stmts '}' { 
        $$ = mknode("BLOCK"); 
        addSon($$, $2); 
        addSon($$, $3); 
        $$->code = strdup($3->code);
        dPrint("YACC - block 1\n");
    }
    | '{' declares '}' { 
        $$ = mknode("BLOCK"); 
        addSon($$, $2); 
        $$->code = strdup("");
        dPrint("YACC - block 2\n");
    }
    | '{' stmts '}' { 
        $$ = mknode("BLOCK"); 
        addSon($$, $2); 
        $$->code = strdup($2->code);
        dPrint("YACC - block 3\n");
    }
    | '{' '}' { 
        $$ = mknode("BLOCK");
        $$->code = strdup("");
        dPrint("YACC - block 4\n");
    }

block_func: '{' declares stmts return_stmt '}' {
        $$ = mknode("BLOCK");
        addSon($$, $2);
        addSon($$, $3);
        addSon($$, $4);
        $$->code = gen(3, $3->code, "\n", $4->code);
        dPrint("YACC - block 5\n");
    }
    | '{' stmts return_stmt '}' {
        $$ = mknode("BLOCK");
        addSon($$, $2);
        addSon($$, $3);
        $$->code = gen(3, $2->code, "\n", $3->code);
        dPrint("YACC - block 6\n");
    }
    | '{' declares return_stmt '}' {
        $$ = mknode("BLOCK");
        addSon($$, $2);
        addSon($$, $3);
        $$->code = strdup($3->code);
        dPrint("YACC - block 7\n");
    }
    | '{' return_stmt '}' {
        $$ = mknode("BLOCK");
        addSon($$, $2);
        $$->code = strdup($2->code);
        dPrint("YACC - block 8\n");
    }

parameter_list:  parameter_list ';' type args {
                    $$ = mknode("ARGS");
                    takeSons($$, $1);
                    free($1);
                    takeSons($3, $4);
                    free($4);
                    addSon($$, $3);
                    $3->type = strdup($3->value);
        dPrint("YACC - parameter_list 1\n");
                }
                |  type args {
                    $$ = mknode("ARGS");
                    takeSons($1, $2);
                    free($2);
                    addSon($$, $1);
                    $1->type = strdup($1->value);
        dPrint("YACC - parameter_list 2\n");
                    }
                | { 
                    $$ = mknode("ARGS");
                    addValue($$, "NONE");
        dPrint("YACC - parameter_list 3\n");
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
        $$->begin = freshLabel();
        $$->nextLabel = freshLabel();
        $$->code = gen(17, "\t", $$->begin, ":\n", $5->code, "\n\tifz ", $5->var, " GoTo ", $$->nextLabel, "\n", $9->code, "\n", $7->code, "\n\tGoTo ", $$->begin, "\n\t", $$->nextLabel, ":");
        dPrint("YACC - for_stmt 1\n");
    }

init:  assign_stmt {
        $$ = $1;
        assignOpTests($1);
        dPrint("YACC - for_init 1\n");
    }

update: assign_stmt {
        $$ = $1;
        assignOpTests($$);
        dPrint("YACC - for_update 1\n");
    }

if_stmt: IF '(' expression ')' stmt {
        $$ = mknode("IF");
        addSon($$, $3);
        addSon($$, $5);
        conditionTests($3);
        $$->begin = freshLabel();
        $$->nextLabel = freshLabel();
        $$->code = gen(9, $3->code, "\n\tifz ", $3->var, " GoTo ", $$->begin, $5->code, "\n\t", $$->begin, ":");
        dPrint("YACC - if_stmt 1\n");
    }
    | IF '(' expression ')' stmt ELSE stmt {
        $$ = mknode("IF-ELSE");
        addSon($$, $3);
        addSon($$, $5);
        addSon($$, $7);
        conditionTests($3);
        $$->begin = freshLabel();
        $$->nextLabel = freshLabel();
        $$->code = gen(15, $3->code, "\n\tifz ", $3->var, " GoTo ", $$->begin, $5->code, "\n\tGoTo ", $$->nextLabel, "\n\t", $$->begin, ":", $7->code, "\n\t", $$->nextLabel, ":");
        dPrint("YACC - if_stmt 2\n");
    }

while_stmt: WHILE '(' expression ')' stmt {
        $$ = mknode("WHILE");
        addSon($$, $3);
        addSon($$, $5);
        conditionTests($3);

        $$->begin = freshLabel();
        $$->nextLabel = freshLabel();
        $$->code = gen(14, "\n\t", $$->begin, ":", $3->code, "\n\tifz ", $3->var, " GoTo ", $$->nextLabel, $5->code, "\n\tGoTo ", $$->begin, "\n\t", $$->nextLabel, ":");
        dPrint("YACC - while_stmt 1\n");
    }
    | DO SSPUSH block SSPOP WHILE '(' expression ')' ';' {
        $$ = mknode("DO-WHILE");
        addSon($$, $7);
        addSon($$, $3);
        conditionTests($7);

        $$->begin = freshLabel();
        $$->nextLabel = freshLabel();

        $$->code = gen(15, "\n\t", $$->begin, ":\n", $3->code, "\n", $7->code, "\n\tifz ", $7->var, " GoTo ", $$->nextLabel, "\n\tGoTo ", $$->begin, "\n\t", $$->nextLabel, "\n");
        dPrint("YACC - while_stmt 2\n");
    }

stmts: stmts stmt { 
        $$ = mknode("STATEMENTS");
        takeSons($$, $1);
        free($1);
        addSon($$, $2);
        $$->code = gen(2, $1->code, $2->code);
        dPrint("YACC - stmts 1\n");
    }
    | stmt { 
        $$ = mknode("STATEMENTS"); 
        addSon($$, $1); 
        $$->code = strdup($1->code);
        dPrint("YACC - stmts 2\n");
    }

stmt: if_stmt { $$ = $1; dPrint("YACC - stmt 1\n"); }
    | while_stmt { $$ = $1; dPrint("YACC - stmt 2\n"); }
    | for_stmt { $$ = $1; dPrint("YACC - stmt 3\n"); }
    | assign_stmt ';' { 
        $$ = $1; 
        assignOpTests($1); 
        dPrint("YACC - stmt 4\n"); }
    | func_call ';' { $$ = $1; dPrint("YACC - stmt 5\n"); }
    | SSPUSH block SSPOP { $$ = $2; dPrint("YACC - stmt 6\n"); }
    | return_stmt { $$ = $1; dPrint("YACC - stmt 7\n"); }
    
assign_stmt: lhs ASSIGN expression {
        $$ = mknode("=");
        addSon($$, $1);
        addSon($$, $3);
        $$->code = gen(6, $1->code, $3->code, "\n\t", $1->var, " = ", $3->var);
        dPrint("YACC - assign_stmt 1\n");
    }

declares: declares declare_opts {
        $$ = mknode("DECLARES");
        takeSons($$, $1);
        free($1);
        addSon($$, $2);
        dPrint("YACC - declares 1\n");
    }
    | declare_opts {
        $$ = mknode("DECLARES"); 
        addSon($$, $1); 
        dPrint("YACC - declares 2\n");
    }

declare_opts: var_declare { $$ = $1; }
    | str_declare { 
        $$ = $1; 
        $$->type = strdup("STRING"); 
        dPrint("YACC - declare_opts 1\n");
    }
    | func { 
        $$ = $1; 
        dPrint("YACC - declare_opts 2\n");
    }

var_declare: VAR type var_dec_args ';' {
        $$ = mknode("VAR");
        takeSons($2, $3);
        addSon($$, $2);
        $$->type = strdup($2->value);
        for (int i = 0; i < $3->children_len; i++)  {
            node *var = $3->children[i];
            if (strcmp(var->token, "IDENTIFIER") != 0) 
                var = var->children[0];
            else
                var->type = strdup($$->type);

            struct idInfo *id = ht_search(ht, var->value);
            id->decl->type = strdup($$->type);
        }
        dPrint("YACC - var_declare 1\n");
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
        free($1);
        addSon($$, $3);
        varDecTests($3);
        dPrint("YACC - var_dec_args 3\n");
    }
    | var_dec_args ',' assign_stmt {
        $$ = mknode("VAR");
        takeSons($$, $1);
        free($1);
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
                struct idInfo *id = ht_search(ht, var->children[0]->value);
                id->decl->type = strdup($$->type);
                assignOpTests(var);
            } else {
                struct idInfo *id = ht_search(ht, var->value);
                id->decl->type = strdup($$->type);
                checkStringElement(var);
            }
        }
        addSon($$, t);
        dPrint("YACC - str_declare 1\n");
    }

str_declare_args: string_element {
        $$ = mknode("");
        addSon($$, $1);
        $1->type = strdup("STRING");
        varDecTests($1);
        dPrint("YACC - str_declare_args 1\n");
    }
    | assign_stmt {
        $$ = mknode("");
        addSon($$, $1);
        $1->type = strdup("STRING");
        varDecTests($1->children[0]);
        dPrint("YACC - str_declare_args 2\n");
    }
    | assign_stmt ',' str_declare_args {
        $$ = mknode("VAR");
        addSon($$, $1);
        takeSons($$, $3);
        free($3);
        $1->type = strdup("STRING");
        varDecTests($1->children[0]);
        dPrint("YACC - str_declare_args 3\n");
    }
    | string_element ',' str_declare_args {
        $$ = mknode("VAR");
        addSon($$, $1);
        takeSons($$, $3);
        free($3);
        $1->type = strdup("STRING");
        varDecTests($1);
        dPrint("YACC - str_declare_args 4\n");
    }

func_call: id '(' expression_list ')' {
        $$ = mknode("CALL");
        addValue($$, $1->value);
        free($1);
        addSon($$, $3);
        funcCallTests($$, $3);
        $$->var = freshVar();
        int memoryToPop = countParamMemory($3);
        $$->code = gen(7, $3->code, "\n\t", $$->var, " = LCall ", $1->value, "_\n\tPopParams ", MY_itoa(memoryToPop));
        dPrint("YACC - func_call 1\n");
    }
    | id '(' ')' {
        $$ = mknode("CALL");
        addValue($$, $1->value);
        free($1);
        addSon($$, mknode("ARGS NONE"));
        funcCallTests($$, $$->children[0]);
        $$->var = freshVar();
        $$->code = gen(5, "\t", $$->var, " = LCall ", $$->value, "_");
        dPrint("YACC - func_call 2\n");
    }

expression_list: expression {
        $$ = mknode("ARGS");
        addSon($$, $1);
        $$->var = freshVar();
        $$->code = gen(3, $1->code, "\n\tPushParam ", $1->var);
        dPrint("YACC - expression_list 1\n");
    }
    | expression_list ',' expression {
        $$ = $1;
        addSon($$, $3);
        $$->var = freshVar();
        $$->code = gen(4, $1->code, $3->code, "\n\tPushParam ", $3->var);
        dPrint("YACC - expression_list 2\n");
    }

return_stmt: RET expression ';' {
        $$ = mknode("RETURN");
        addSon($$, $2);
        $$->type = decideType($2);
        $$->code = gen(5, $2->code, "\n\t", $$->token, " ", $2->var);
        dPrint("YACC - return_stmt 1\n");
    }

expression: expression GT expression {
        $$ = mknode(">");
        addSon($$, $1);
        addSon($$, $3);
        comparisonAndArithmeticOpTests($1, $3, $$->token);
        $$->type = strdup("BOOL");
        $$->var = freshVar();
        $$->code = strdup(gen(8, $1->code, $3->code, "\n\t", $$->var, " = ", $1->var, $$->token, $3->var));
        dPrint("YACC - expression 1\n");
    }
    | expression GTE expression {
        $$ = mknode(">=");
        addSon($$, $1);
        addSon($$, $3);
        comparisonAndArithmeticOpTests($1, $3, $$->token);
        $$->type = strdup("BOOL");
        $$->var = freshVar();
        $$->code = strdup(gen(8, $1->code, $3->code, "\n\t", $$->var, " = ", $1->var, $$->token, $3->var));
        dPrint("YACC - expression 2\n");
    }
    | expression LT expression {
        $$ = mknode("<");
        addSon($$, $1);
        addSon($$, $3);
        comparisonAndArithmeticOpTests($1, $3, $$->token);
        $$->type = strdup("BOOL");
        $$->var = freshVar();
        $$->code = strdup(gen(8, $1->code, $3->code, "\n\t", $$->var, " = ", $1->var, $$->token, $3->var));
        dPrint("YACC - expression 3\n");
    }
    | expression LTE expression {
        $$ = mknode("<=");
        addSon($$, $1);
        addSon($$, $3);
        comparisonAndArithmeticOpTests($1, $3, $$->token);
        $$->type = strdup("BOOL");
        $$->var = freshVar();
        $$->code = strdup(gen(8, $1->code, $3->code, "\n\t", $$->var, " = ", $1->var, $$->token, $3->var));
        dPrint("YACC - expression 4\n");
    }
    | expression EQ expression {
        $$ = mknode("==");
        addSon($$, $1);
        addSon($$, $3);
        equalityOpTests($1, $3, $$->token);
        $$->type = strdup("BOOL");
        $$->var = freshVar();
        $$->code = strdup(gen(8, $1->code, $3->code, "\n\t", $$->var, " = ", $1->var, $$->token, $3->var));
        dPrint("YACC - expression 5\n");
    }
    | expression NE expression {
        $$ = mknode("!=");
        addSon($$, $1);
        addSon($$, $3);
        equalityOpTests($1, $3, $$->token);
        $$->type = strdup("BOOL");
        $$->var = freshVar();
        $$->code = strdup(gen(8, $1->code, $3->code, "\n\t", $$->var, " = ", $1->var, $$->token, $3->var));
        dPrint("YACC - expression 6\n");
    }
    | expression PLUS expression {
        $$ = mknode("+");
        addSon($$, $1);
        addSon($$, $3);
        comparisonAndArithmeticOpTests($1, $3, $$->token);
        $$->type = strdup(decideType($1));
        $$->var = freshVar();
        $$->code = strdup(gen(8, $1->code, $3->code, "\n\t", $$->var, " = ", $1->var, $$->token, $3->var));
        dPrint("YACC - expression 7\n");
    }
    | expression MINUS expression {
        $$ = mknode("-");
        addSon($$, $1);
        addSon($$, $3);
        comparisonAndArithmeticOpTests($1, $3, $$->token);
        $$->type = strdup(decideType($1));
        $$->var = freshVar();
        $$->code = strdup(gen(8, $1->code, $3->code, "\n\t", $$->var, " = ", $1->var, $$->token, $3->var));
        dPrint("YACC - expression 8\n");
    }
    | expression MUL expression {
        $$ = mknode("*");
        addSon($$, $1);
        addSon($$, $3);
        comparisonAndArithmeticOpTests($1, $3, $$->token);
        $$->type = strdup(decideType($1));
        $$->var = freshVar();
        $$->code = strdup(gen(8, $1->code, $3->code, "\n\t", $$->var, " = ", $1->var, $$->token, $3->var));
        dPrint("YACC - expression 9\n");
    }
    | expression DIV expression {
        $$ = mknode("/");
        addSon($$, $1);
        addSon($$, $3);
        comparisonAndArithmeticOpTests($1, $3, $$->token);
        $$->type = strdup(decideType($1));
        $$->var = freshVar();
        $$->code = strdup(gen(8, $1->code, $3->code, "\n\t", $$->var, " = ", $1->var, $$->token, $3->var));
        dPrint("YACC - expression 10\n");
    }
    | expression AND expression {
        $$ = mknode("&&");
        addSon($$, $1);
        addSon($$, $3);
        logicalOpTests($1, $3, $$->token);
        $$->type = strdup(decideType($1));
        $$->var = freshVar();
        $$->code = strdup(gen(8, $1->code, $3->code, "\n\t", $$->var, " = ", $1->var, $$->token, $3->var));
        dPrint("YACC - expression 11\n");
    }
    | expression OR expression {
        $$ = mknode("||");
        addSon($$, $1);
        addSon($$, $3);
        logicalOpTests($1, $3, $$->token);
        $$->type = strdup(decideType($1));
        $$->var = freshVar();
        $$->code = strdup(gen(8, $1->code, $3->code, "\n\t", $$->var, " = ", $1->var, $$->token, $3->var));
        dPrint("YACC - expression 12\n");
    }
    | NOT expression {
        $$ = mknode("!");
        addSon ($$, $2);
        notOpTest($2);
        $$->type = strdup(decideType($2));
        $$->var = freshVar();
        $$->code = gen(6, $2->code, "\n\t", $$->var, " = ", $$->token, $2->var);
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
        char *type = strdup(decideType($2));
        int size = strlen(type) + strlen("PTR") + 1;
        char *temp = realloc(type, size);
        if (!temp) {
            printf("addr in parser reallocation fail.\n");
            exit(1);
        }
        type = temp;
        strcat(type, "PTR");
        $$->type = strdup(type);
        $$->var = freshVar();
        $$->code = gen(6, $2->code, "\n\t", $$->var, " = ", $$->token, $2->var);
        dPrint("YACC - expression 16\n");
    }
    | deref {
        $$ = $1;
        dPrint("YACC - expression 17\n");
    }

deref: MUL expression {
        $$ = mknode("DEREF");
        addSon($$, $2);
        derefOpTest($2);
        char *type = strdup(decideType($2));
        if (strcmp(type, "INTPTR") == 0) $$->type = strdup("INT");
        if (strcmp(type, "CHARPTR") == 0) $$->type = strdup("CHAR");
        if (strcmp(type, "REALPTR") == 0) $$->type = strdup("REAL");
        $$->var = freshVar();
        $$->code = gen(6, $2->code, "\n\t", $$->var, " = ", $$->token, $2->var);
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
    free($1);
    addSon($$, $3);
    $$->type = strdup("CHAR");
    int size = strlen($$->value) + strlen($3->var) + 3;
    $$->var = malloc(size);
    *$$->var = '\0';
    strcat($$->var, $$->value);
    strcat($$->var, "[");
    strcat($$->var, $3->var);
    strcat($$->var, "]");
    $$->code = strdup("");
    dPrint("YACC - string_element 1\n");
}

lhs: id { $$ = $1; }
    | string_element { $$ = $1; dPrint("YACC - lhs 1\n"); }
    | deref { $$ = $1; dPrint("YACC - lhs 2\n"); }


val: id { $$ = $1; dPrint("YACC - val 1\n"); }
    | INTEGER { 
        $$ = mknode("INTEGER"); 
        addValue($$, yytext); 
        $$->type = strdup("INT"); 
        $$->var = strdup($$->value);
        $$->code = strdup("");
    dPrint("YACC - val 2\n");
    }
    | CHARACTER { 
        $$ = mknode("CHARACTER"); 
        addValue($$, yytext); 
        $$->type = strdup("CHAR"); 
        $$->var = strdup($$->value);
        $$->code = strdup("");
    dPrint("YACC - val 3\n");
    }
    | DOUBLE { 
        $$ = mknode("DOUBLE"); 
        addValue($$, yytext); 
        $$->type = strdup("REAL"); 
        $$->var = strdup($$->value);
        $$->code = strdup("");
    dPrint("YACC - val 4\n");
    }
    | string_literal { 
        $$ = $1; 
        $$->var = strdup($$->value);
        $$->code = strdup("");
        dPrint("YACC - val 5\n"); 
    }
    | boolean { 
        $$ = $1; 
        $$->var = strdup($$->value);
        $$->code = strdup("");
        dPrint("YACC - val 6\n"); 
    }
    | length { $$ = $1; dPrint("YACC - val 7\n"); }
    | VNULL { 
        $$ = mknode("NULL"); 
        $$->value = strdup("NULL"); 
        $$->type = strdup("NULL");
        $$->var = strdup($$->value);
        $$->code = strdup("");
    dPrint("YACC - val 8\n");
    }
    | string_element { $$ = $1; dPrint("YACC - val 9\n"); }
    | func_call { $$ = $1; dPrint("YACC - val 10\n"); }

id: IDENTIFIER { 
    $$ = mknode("IDENTIFIER"); 
    addValue($$, yytext);
    $$->var = strdup($$->value);
        $$->code = strdup("\0");
    dPrint("YACC - id 1\n");
}

string_literal: STRING { 
    $$ = mknode("STRING LITERAL"); 
    addValue($$, yytext); 
    $$->type = strdup("STRING"); 
    $$->var = strdup($$->value);
        $$->code = strdup("\0");
    dPrint("YACC - string_literal 1\n");
}

length: LEN id LEN {
        $$ = mknode("LEN");
        addSon($$, $2);
        if(!isString($2)) {
            printf("| | operand should be of type string.\n");
            exit(1);
        }
        $$->type = strdup("INT");
        $$->var = freshVar();
        $$->code = gen(5, $$->code, $$->var, " = |", $1->var, "|");
    dPrint("YACC - length 2\n");
    dPrint("YACC - length 1\n");
    }
    | LEN string_literal LEN {
        $$ = mknode("STRING LEN");
        addSon($$, $2);
        $$->type = strdup("INT");
        $$->var = freshVar();
        $$->code = gen(4, $$->var, " = |", $1->var, "|");
    dPrint("YACC - length 2\n");
    }

boolean: BOOLT { 
        $$ = mknode("BOOLEAN"); 
        addValue($$, yytext); 
        $$->type = strdup("BOOL");
        $$->var = strdup($$->value);
        $$->code = strdup("");
    dPrint("YACC - boolean 1\n");
    }
    | BOOLF { 
        $$ = mknode("BOOLEAN"); 
        addValue($$, yytext); 
        $$->type = strdup("BOOL"); 
        $$->var = strdup($$->value);
        $$->code = strdup("");
    dPrint("YACC - boolean 2\n");
    }

%%
#include "lex.yy.c"

int debug = 0;

int main()
{
    tree = mknode("CODE"); // declared in "ast.c", which is included in "symtab.c" which is included here
    ht = ht_init();
    ht_tac = ht_init();
    stack = ss_init(new_scope());
    int ast_result = yyparse(); // 0 - success, 1 - fail by invalid input, 2 - fail by memory exhaustion
    if (ast_result == 0) {
        // printNode(tree, 0);
        printf("End of parsing - Success!\n");
    }
    else {
        printf("End of parsing - Fail!\n");
    }
    FILE *fp;

    fp = fopen("3ac_representation_result.txt", "w+");
    fputs(tree->code, fp);
    fclose(fp);
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
