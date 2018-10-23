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

void yyerror(string);

%}

%union{
    int     ival;
    string* sval;
    /* char*   cval; */ /* for raw char array yytext */
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

    */

%right ASSIGN
%left OR
%left AND
%right NOT
%left NEQ
%left EQ
%left GTE
%left GT
%left LTE
%left LT
%left MINUS
%left PLUS
%left MOD
%left DIV
%left MULT
%right UMINUS
%left R_SQUARE_BRACKET
%left L_SQUARE_BRACKET
%left R_PAREN
%left L_PAREN

%%

    /*
    ----------------------------------------
    Grammar Rules
    ----------------------------------------
    */


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
        /* { cout << "ident -> IDENT " << string(yylval.cval) << endl; } */
        { cout << "ident -> IDENT " << *yylval.sval << endl; }
    | IDENT COMMA identifiers
        { cout << "identifiers -> ident COMMA identifiers" << endl; }
    ;

statements
    : /* epsilon */
        { cout << "statements -> epsilon" << endl;}
    | statement SEMICOLON statements
        { cout << "statements -> statement SEMICOLON statements" << endl; }
    ;

statement
    : var ASSIGN expression
        { cout << "statement -> var ASSIGN expression" << endl; }
    | IF bool-expr THEN statements ENDIF
        { cout << "statement -> IF bool_exp THEN statements ENDIF" << endl; }
    | IF bool-expr THEN statements ELSE statements ENDIF
        { cout << "statement -> IF bool_exp THEN statements ELSE statements ENDIF" << endl; }
    | WHILE bool-expr BEGINLOOP statements ENDLOOP
        { cout << "statement -> WHILE bool_exp BEGINLOOP statements ENDLOOP" << endl; }
    | DO BEGINLOOP statements ENDLOOP WHILE bool-expr
        { cout << "statement -> DO BEGINLOOP statements ENDLOOP WHILE bool_exp" << endl; }
    | READ vars
        { cout << "statement -> READ vars" << endl; }
    | WRITE vars
        { cout << "statement -> WRITE vars" << endl; }
    | CONTINUE
        { cout << "statement -> CONTINUE" << endl; }
    | RETURN expression
        { cout << "statement -> RETURN expression" << endl; }
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
        { cout << "vars -> var" << endl; }
    | var COMMA vars
        { cout << "vars -> var COMMA vars" << endl; }
    ;

var
    : identifiers
        { cout << "var -> ident" << endl; }
    | identifiers L_SQUARE_BRACKET expression R_SQUARE_BRACKET
        { cout << "var -> ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET" << endl; }
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
   yyerror(string(msg));
}

void yyerror(string s)
{
   cout << "parser yyerror at Line " << curline << ", position " << curpos << ": " << s << endl;
}


