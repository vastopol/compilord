    // Mini-L Lexer File

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
BEGINPARAMS  "beginparams"
ENDPARAMS    "endparams"
BEGINLOCALS  "beginlocals"
ENDLOCALS    "endlocals"
BEGINBODY    "beginbody"
ENDBODY      "endbody"
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
NE   "<>"
LT   "<"
GT   ">"
LE  "<="
GE  ">="

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

{BEGINPARAMS} {
    curpos += yyleng;
    return BEGINPARAMS;
}

{ENDPARAMS} {
    curpos += yyleng;
    return ENDPARAMS;
}

{BEGINLOCALS} {
    curpos += yyleng;
    return BEGINLOCALS;
}

{ENDLOCALS} {
    curpos += yyleng;
    return ENDLOCALS;
}

{BEGINBODY} {
    curpos += yyleng;
    return BEGINBODY;
}

{ENDBODY} {
    curpos += yyleng;
    return ENDBODY;
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
    //return ADD;
    return '+';
}

{SUB} {
    curpos += yyleng;
    //return SUB;
    return '-';

}

{MULT} {
    curpos += yyleng;
    //return MULT;
    return '*';
}

{DIV} {
    curpos += yyleng;
    //return DIV;
    return '/';
}

{MOD} {
    curpos += yyleng;
    //return MOD;
    return '%';
}

    /* ----- Comparison Operators ----- */

{EQ} {
    curpos += yyleng;
    return EQ;
}

{NE} {
    curpos += yyleng;
    return NE;
    //return "<>";
}

{LT} {
    curpos += yyleng;
    //return LT;
    return '<';
}

{GT} {
    curpos += yyleng;
    //return GT;
    return '>';
}

{LE} {
    curpos += yyleng;
    return LE;
}

{GE} {
    curpos += yyleng;
    return GE;
}

    /* ----- Special Symbols ----- */

{SEMICOLON} {
    curpos += yyleng;
    //return SEMICOLON;
    return ';';
}

{COLON} {
    curpos += yyleng;
    //return COLON;
    return ':';
}

{COMMA} {
    curpos += yyleng;
    //return COMMA;
    return ',';
}

{L_PAREN} {
    curpos += yyleng;
    //return L_PAREN;
    return '(';
}

{R_PAREN} {
    curpos += yyleng;
    //return R_PAREN;
    return ')';
}

{L_SQUARE_BRACKET} {
    curpos += yyleng;
    //return L_SQUARE_BRACKET;
    return '[';
}

{R_SQUARE_BRACKET} {
    curpos += yyleng;
    //return R_SQUARE_BRACKET;
    return ']';
}

{ASSIGN} {
    curpos += yyleng;
    return ASSIGN;
}

    /* ----- Identifiers && Numbers ----- */

{NUMBER} {
    yylval.int_val = atoi(yytext);
    curpos += yyleng;
    return NUMBER;
}

{IDENTIFIER} {
    yylval.ident = new string(yytext);
    curpos += yyleng;
    return ID;
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

