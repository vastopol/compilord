    /*
    ----------------------------------------
    Syntax Analyzer/Parser for MINI-L (v2)
    Sean Richardson
    ----------------------------------------
    */


    /*
    ----------------------------------------
    Declarations
    ----------------------------------------
    */


%{

#include "heading.h"

/* externals in flex file */
extern int curline;
extern int curpos;

/* externals for fixing g++ errors */
extern FILE* yyin; /* multiple declarations of yyin */
extern int yylex(); /* ‘yylex’ was not declared in this scope */

/* user subroutines */
void yyerror(const char *msg);

%}

%union{
    int     ival; /* currently where numbers are written to in flex file*/
    char*   cval; /* currently where identifiers are written to in flex file*/
    string* sval;
}

%error-verbose

%start program

%token FUNCTION BEGIN_PARAMS END_PARAMS BEGIN_LOCALS END_LOCALS BEGIN_BODY END_BODY INTEGER ARRAY OF
%token IF THEN ENDIF ELSE WHILE DO FOREACH IN BEGINLOOP ENDLOOP CONTINUE READ WRITE AND OR NOT TRUE FALSE RETURN

%token ADD SUB MULT DIV MOD

%token EQ NEQ LT GT LTE GTE

%token SEMICOLON COLON COMMA L_PAREN R_PAREN L_SQUARE_BRACKET R_SQUARE_BRACKET ASSIGN

%token IDENT

%token NUMBER

    /*

%token <chval> IDENT
%token <ival> NUMBER
%type  <ival> expression

%left PLUS MINUS
%left MULT DIV MOD
%left EQ NEQ LT GT LTE GTE
%left AND OR
%right NOT
%right ASSIGN

    */

%%


program
    : functions
        { cout << "program -> functions" << endl; }
    ;

functions
    : /* epsilon */
        { cout << "functions -> epsilon" << endl; }
    | function functions
        { cout << "functions -> function functions" << endl; }
    ;

function
    : FUNCTION identifiers SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY
        { cout << "function -> FUNCTION IDENT SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY" << endl; }
    ;

declarations
    : /* epsilon */
        { cout << "declarations -> epsilon" << endl;}
    | declaration SEMICOLON declarations
        { cout << "declarations -> declaration SEMICOLON declarations" << endl; }
    ;

declaration
    : identifiers COLON INTEGER
        { cout << "declaration -> identifiers COLON INTEGER" << endl; }
    | identifiers COLON ARRAY OF L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER
        { cout << "declaration -> identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER" << endl; }
    ;

identifiers
    : IDENT
    | IDENT COMMA identifiers
    ;

statements
    : /* epsilon */
        { cout << "statements -> epsilon" << endl;}
    | statement SEMICOLON statements
        { cout << "statements -> statement SEMICOLON statements" << endl; }
    ;

statement
    : var ASSIGN expression
    | IF bool-expr THEN statements ENDIF
    | IF bool-expr THEN statements ELSE statements ENDIF
    | WHILE bool-expr BEGINLOOP statements ENDLOOP
    | DO BEGINLOOP statements ENDLOOP WHILE bool-expr
    | READ vars
    | WRITE vars
    | CONTINUE
    | RETURN expression
    ;

bool-expr
    : relation-and-expr
    | relation-and-expr OR relation-and-expr
    ;

relation-and-expr
    : relation-expr
    | relation-expr AND relation-expr
    ;

relation-expr
    : expression comp expression
    | TRUE
    | FALSE
    | L_PAREN bool-expr R_PAREN
    | NOT expression comp expression
    | NOT TRUE
    | NOT FALSE
    | NOT L_PAREN bool-expr R_PAREN
    ;

comp
    : EQ
    | NEQ
    | GT
    | LT
    | GTE
    | LTE
    ;

expressions
    : expression
    | expression COMMA expressions
    ;

expression
    : mult-expr ADD mult-expr
    | mult-expr SUB mult-expr
    ;

mult-expr
    : term mult-exprs
    ;

mult-exprs
    :
    | MULT term mult-exprs
    | DIV  term mult-exprs
    | MOD  term mult-exprs
    ;

term
    : var
    | NUMBER
    | L_PAREN expression R_PAREN
    | identifiers L_PAREN expressions R_PAREN
    | SUB var
    | SUB NUMBER
    | SUB L_PAREN expression R_PAREN
    ;

vars
    : var
    | var COMMA vars
    ;

var
    : identifiers
    | identifiers L_SQUARE_BRACKET expression R_SQUARE_BRACKET
    ;


    /*
    ----------------------------------------
    Rules
    ----------------------------------------
    */



%%


    /*
    ----------------------------------------
    User subroutines
    ----------------------------------------
    */


int main(int argc, char** argv)
{
    /* try to read from a input file */
    if(argc >= 2)
    {
        yyin = fopen(argv[1], "r");
        if(yyin == NULL)
        {
           yyin = stdin;
        }
    }
    else
    {
        yyin = stdin;
    }

    yyparse(); /* calls yylex() */
}

void yyerror(const char *msg)
{
   printf("parser yyerror at Line %d, position %d: %s\n", curline, curpos, msg);
}


