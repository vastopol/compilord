    /*
    ----------------------------------------
    Lexical Analyzer/Scanner for MINI-L
    Sean Richardson
    ----------------------------------------
    */


    /*
    ----------------------------------------
    Declarations
    ----------------------------------------
    */


%{

#include <iostream>

#include "y.tab.h"

using namespace std;

int curline = 1;
int curpos = 1;

%}


    /*
    ----------------------------------------
    Definitions
    ----------------------------------------
    */


    /* ----- Regexes ----- */

DIGIT       [0-9]
NUMBER      {DIGIT}+
LETTER      [a-zA-Z]
IDENTIFIER  {LETTER}({LETTER}|{DIGIT}|([_])({LETTER}|{DIGIT}))*
COMMENT     [#][#].*\n
IDERR1      {NUMBER}{LETTER}
IDERR2      {IDENTIFIER}[_]+

    /*----- Reserved Words ----- */

FUNCTION      "function"
BEGIN_PARAMS  "beginparams"
END_PARAMS    "endparams"
BEGIN_LOCALS  "beginlocals"
END_LOCALS    "endlocals"
BEGIN_BODY    "beginbody"
END_BODY      "endbody"
INTEGER       "integer"
ARRAY         "array"
OF            "of"
IF            "if"
THEN          "then"
ENDIF         "endif"
ELSE          "else"
WHILE         "while"
DO            "do"
FOREACH       "foreach"
IN            "in"
BEGINLOOP     "beginloop"
ENDLOOP       "endloop"
CONTINUE      "continue"
READ          "read"
WRITE         "write"
AND           "and"
OR            "or"
NOT           "not"
TRUE          "true"
FALSE         "false"
RETURN        "return"

    /* ----- Arithmetic Operators ----- */

ADD   "+"
SUB   "-"
MULT  "*"
DIV   "/"
MOD   "%"

    /* ----- Comparison Operators ----- */

EQ   "=="
NEQ  "<>"
LT   "<"
GT   ">"
LTE  "<="
GTE  ">="

    /* ----- Special Symbols ----- */

SEMICOLON         ";"
COLON             ":"
COMMA             ","
L_PAREN           "("
R_PAREN           ")"
L_SQUARE_BRACKET  "["
R_SQUARE_BRACKET  "]"
ASSIGN            ":="


%%


    /*
    ----------------------------------------
    Rules
    ----------------------------------------
    */


    /*----- Reserved Words ----- */

{FUNCTION} {
    curpos += yyleng;
    return FUNCTION;
}

{BEGIN_PARAMS} {
    curpos += yyleng;
    return BEGIN_PARAMS;
}

{END_PARAMS} {
    curpos += yyleng;
    return END_PARAMS;
}

{BEGIN_LOCALS} {
    curpos += yyleng;
    return BEGIN_LOCALS;
}

{END_LOCALS} {
    curpos += yyleng;
    return END_LOCALS;
}

{BEGIN_BODY} {
    curpos += yyleng;
    return BEGIN_BODY;
}

{END_BODY} {
    curpos += yyleng;
    return END_BODY;
}

{INTEGER} {
    curpos += yyleng;
}

{ARRAY} {
    curpos += yyleng;
}

{OF} {
    curpos += yyleng;
}

{IF} {
    curpos += yyleng;
}

{THEN} {
    curpos += yyleng;
}

{ENDIF} {
    curpos += yyleng;
}

{ELSE} {
    curpos += yyleng;
}

{WHILE} {
    curpos += yyleng;
}

{DO} {
    curpos += yyleng;
}

{FOREACH} {
    curpos += yyleng;
}

{IN} {
    curpos += yyleng;
}

{BEGINLOOP} {
    curpos += yyleng;
}

{ENDLOOP} {
    curpos += yyleng;
}

{CONTINUE} {
    curpos += yyleng;
}

{READ} {
    curpos += yyleng;
}

{WRITE} {
    curpos += yyleng;
}

{AND} {
    curpos += yyleng;
}

{OR} {
    curpos += yyleng;
}

{NOT} {
    curpos += yyleng;
}

{TRUE} {
    curpos += yyleng;
}

{FALSE} {
    curpos += yyleng;
}

{RETURN} {
    curpos += yyleng;
}

    /* ----- Arithmetic Operators ----- */

{ADD} {
    curpos += yyleng;
}

{SUB} {
    curpos += yyleng;
}

{MULT} {
    curpos += yyleng;
}

{DIV} {
    curpos += yyleng;
}

{MOD} {
    curpos += yyleng;
}

    /* ----- Comparison Operators ----- */

{EQ} {
    curpos += yyleng;
}

{NEQ} {
    curpos += yyleng;
}

{LT} {
    curpos += yyleng;
}

{GT} {
    curpos += yyleng;
}

{LTE} {
    curpos += yyleng;
}

{GTE} {
    curpos += yyleng;
}

    /* ----- Special Symbols ----- */

{SEMICOLON} {
    curpos += yyleng;
}

{COLON} {
    curpos += yyleng;
}

{COMMA} {
    curpos += yyleng;
}

{L_PAREN} {
    curpos += yyleng;
}

{R_PAREN} {
    curpos += yyleng;
}

{L_SQUARE_BRACKET} {
    curpos += yyleng;
}

{R_SQUARE_BRACKET} {
    curpos += yyleng;
}

{ASSIGN} {
    curpos += yyleng;
}

    /* ----- Identifiers && Numbers ----- */

{NUMBER} {
    curpos += yyleng;
}

{IDENTIFIER} {
    curpos += yyleng;
    return ID; /* Not actually sure if this needs to be here or not... */
}

    /* ----- Whitespace ----- */

[ \t]+ {
    /* ignore spaces && tabs */
    curpos += yyleng;
}

"\n" {
    curline++;
    curpos = 1;
    return END; /* Not actually sure if this needs to be here or not... */
}

{COMMENT} {
    curline++;
    curpos = 1;
}

    /* ----- Error Catching ----- */

{IDERR1} {
    /* Error: Identifier must begin with letter */
    printf("Error at line %d, column %d: identifier \"%s\" must begin with a letter\n", curline, curpos, yytext);
    exit(0);
}

{IDERR2} {
    /* Error: Identifier cannot end with an underscore */
    printf("Error at line %d, column %d: identifier \"%s\" cannot end with an underscore\n", curline, curpos, yytext);
    exit(0);
}

. {
    /* Error: Unrecognized Symbol */
    printf("Error at line %d, column %d: unrecognized symbol \"%s\"\n", curline, curpos, yytext);
    exit(0);
}


%%


/* ----- main() is in mini_l.y now ----- */

