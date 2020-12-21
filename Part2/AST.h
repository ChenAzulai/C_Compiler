#ifndef AST_H
#define AST_H
#include<stdio.h>
#include<stdlib.h>
#include<string.h>

typedef struct node
{
char *token;
struct node *left;
struct node *right;
} node;

typedef enum {false,true} bool;
node* mkNode(char*, node *, node *);

#endif
