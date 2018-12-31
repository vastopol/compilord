    /*
    ----------------------------------------
    Lexical Analyzer/Scanner for MINI-L (v3)
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

/* Bison includes */
#include "y.tab.h"

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
ERR1        {NUMBER}{LETTER}
ERR2        {IDENTIFIER}[_]+
ERR3        [_]+{IDENTIFIER}
ERR4        [_]+{NUMBER}
ERR5        {NUMBER}[_]+

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
    return INTEGER;
}

{ARRAY} {
    curpos += yyleng;
    return ARRAY;
}

{OF} {
    curpos += yyleng;
    return OF;
}

{IF} {
    curpos += yyleng;
    return IF;
}

{THEN} {
    curpos += yyleng;
    return THEN;
}

{ENDIF} {
    curpos += yyleng;
    return ENDIF;
}

{ELSE} {
    curpos += yyleng;
    return ELSE;
}

{WHILE} {
    curpos += yyleng;
    return WHILE;
}

{DO} {
    curpos += yyleng;
    return DO;
}

{BEGINLOOP} {
    curpos += yyleng;
    return BEGINLOOP;
}

{ENDLOOP} {
    curpos += yyleng;
    return ENDLOOP;
}

{CONTINUE} {
    curpos += yyleng;
    return CONTINUE;
}

{READ} {
    curpos += yyleng;
    return READ;
}

{WRITE} {
    curpos += yyleng;
    return WRITE;
}

{AND} {
    curpos += yyleng;
    return AND;
}

{OR} {
    curpos += yyleng;
    return OR;
}

{NOT} {
    curpos += yyleng;
    return NOT;
}

{TRUE} {
    curpos += yyleng;
    return TRUE;
}

{FALSE} {
    curpos += yyleng;
    return FALSE;
}

{RETURN} {
    curpos += yyleng;
    return RETURN;
}

    /* ----- Arithmetic Operators ----- */

{ADD} {
    curpos += yyleng;
    return ADD;
}

{SUB} {
    curpos += yyleng;
    return SUB;
}

{MULT} {
    curpos += yyleng;
    return MULT;
}

{DIV} {
    curpos += yyleng;
    return DIV;
}

{MOD} {
    curpos += yyleng;
    return MOD;
}

    /* ----- Comparison Operators ----- */

{EQ} {
    curpos += yyleng;
    return EQ;
}

{NEQ} {
    curpos += yyleng;
    return NEQ;
}

{LT} {
    curpos += yyleng;
    return LT;
}

{GT} {
    curpos += yyleng;
    return GT;
}

{LTE} {
    curpos += yyleng;
    return LTE;
}

{GTE} {
    curpos += yyleng;
    return GTE;
}

    /* ----- Special Symbols ----- */

{SEMICOLON} {
    curpos += yyleng;
    return SEMICOLON;
}

{COLON} {
    curpos += yyleng;
    return COLON;
}

{COMMA} {
    curpos += yyleng;
    return COMMA;
}

{L_PAREN} {
    curpos += yyleng;
    return L_PAREN;
}

{R_PAREN} {
    curpos += yyleng;
    return R_PAREN;
}

{L_SQUARE_BRACKET} {
    curpos += yyleng;
    return L_SQUARE_BRACKET;
}

{R_SQUARE_BRACKET} {
    curpos += yyleng;
    return R_SQUARE_BRACKET;
}

{ASSIGN} {
    curpos += yyleng;
    return ASSIGN;
}

    /* ----- Identifiers && Numbers ----- */

{NUMBER} {
    yylval.ival = atoi(yytext);
    curpos += yyleng;
    return NUMBER;
}

{IDENTIFIER} {
    yylval.sval = new string(yytext);
    curpos += yyleng;
    return IDENT;
}

    /* ----- Whitespace ----- */

[ \t]+ {
    /* ignore spaces && tabs */
    curpos += yyleng;
}

"\n" {
    curline++;
    curpos = 1;
}

{COMMENT} {
    curline++;
    curpos = 1;
}

    /* ----- Error Catching ----- */

{ERR1} {
    /* Error: Identifier must begin with letter */
    printf("Lexer error at line %d, column %d: identifier \"%s\" must begin with a letter\n", curline, curpos, yytext);
    exit(0);
}

{ERR2} {
    /* Error: Identifier cannot end with an underscore */
    printf("Lexer error at line %d, column %d: identifier \"%s\" cannot end with an underscore\n", curline, curpos, yytext);
    exit(0);
}

{ERR3} {
    /* Error: Identifier cannot begin with an underscore */
    printf("Lexer error at line %d, column %d: identifier \"%s\" cannot begin with an underscore\n", curline, curpos, yytext);
    exit(0);
}

{ERR4} {
    /* Error: Number cannot begin with an underscore */
    printf("Lexer error at line %d, column %d: number \"%s\" cannot begin with an underscore\n", curline, curpos, yytext);
    exit(0);
}

{ERR5} {
    /* Error: Number cannot end with an underscore */
    printf("Lexer error at line %d, column %d: number \"%s\" cannot end with an underscore\n", curline, curpos, yytext);
    exit(0);
}

. {
    /* Error: Unrecognized Symbol */
    printf("Lexer error at line %d, column %d: unrecognized symbol \"%s\"\n", curline, curpos, yytext);
    exit(0);
}


%%


/* ----- main() is in mini_l.y now ----- */

