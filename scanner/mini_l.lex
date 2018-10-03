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
    /* IDENTIFIER is based on t-payne's notes, probably modify later to prevent "__" in middle */

DIGIT       [0-9]
NUMBER      {DIGIT}+
LETTER      [a-zA-Z]
IDENTIFIER  {LETTER}({LETTER}|{DIGIT}|([_])({LETTER}|{DIGIT}))*

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

SUB   "-"
ADD   "+"
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
    cout << "FUNCTION" << endl;
    curpos += yyleng;
}

{BEGIN_PARAMS} {
    cout << "BEGIN_PARAMS" << endl;
    curpos += yyleng;
}

{END_PARAMS} {
    cout << "END_PARAMS" << endl;
    curpos += yyleng;
}

{BEGIN_LOCALS} {
    cout << "BEGIN_LOCALS" << endl;
    curpos += yyleng;
}

{END_LOCALS} {
    cout << "END_LOCALS" << endl;
    curpos += yyleng;
}

{BEGIN_BODY} {
    cout << "BEGIN_BODY" << endl;
    curpos += yyleng;
}

{END_BODY} {
    cout << "END_BODY" << endl;
    curpos += yyleng;
}

{INTEGER} {
    cout << "INTEGER" << endl;
    curpos += yyleng;
}

{ARRAY} {
    cout << "ARRAY" << endl;
    curpos += yyleng;
}

{OF} {
    cout << "OF" << endl;
    curpos += yyleng;
}

{IF} {
    cout << "IF" << endl;
    curpos += yyleng;
}

{THEN} {
    cout << "THEN" << endl;
    curpos += yyleng;
}

{ENDIF} {
    cout << "ENDIF" << endl;
    curpos += yyleng;
}

{ELSE} {
    cout << "ELSE" << endl;
    curpos += yyleng;
}

{WHILE} {
    cout << "WHILE" << endl;
    curpos += yyleng;
}

{DO} {
    cout << "DO" << endl;
    curpos += yyleng;
}

{FOREACH} {
    cout << "FOREACH" << endl;
    curpos += yyleng;
}

{IN} {
    cout << "IN" << endl;
    curpos += yyleng;
}

{BEGINLOOP} {
    cout << "BEGINLOOP" << endl;
    curpos += yyleng;
}

{ENDLOOP} {
    cout << "ENDLOOP" << endl;
    curpos += yyleng;
}

{CONTINUE} {
    cout << "CONTINUE" << endl;
    curpos += yyleng;
}

{READ} {
    cout << "READ" << endl;
    curpos += yyleng;
}

{WRITE} {
    cout << "WRITE" << endl;
    curpos += yyleng;
}

{AND} {
    cout << "AND" << endl;
    curpos += yyleng;
}

{OR} {
    cout << "OR" << endl;
    curpos += yyleng;
}

{NOT} {
    cout << "NOT" << endl;
    curpos += yyleng;
}

{TRUE} {
    cout << "TRUE" << endl;
    curpos += yyleng;
}

{FALSE} {
    cout << "FALSE" << endl;
    curpos += yyleng;
}

{RETURN} {
    cout << "RETURN" << endl;
    curpos += yyleng;
}

    /* ----- Arithmetic Operators ----- */

{SUB} {
    cout << "SUB" << endl;
    curpos += yyleng;
}

{ADD} {
    cout << "ADD" << endl;
    curpos += yyleng;
}

{MULT} {
    cout << "MULT" << endl;
    curpos += yyleng;
}

{DIV} {
    cout << "DIV" << endl;
    curpos += yyleng;
}

{MOD} {
    cout << "MOD" << endl;
    curpos += yyleng;
}

    /* ----- Comparison Operators ----- */

{EQ} {
    cout << "EQUAL" << endl;
    curpos += yyleng;
}

{NEQ} {
    cout << "NEQ" << endl;
    curpos += yyleng;
}

{LT} {
    cout << "LT" << endl;
    curpos += yyleng;
}

{GT} {
    cout << "GT" << endl;
    curpos += yyleng;
}

{LTE} {
    cout << "LTE" << endl;
    curpos += yyleng;
}

{GTE} {
    cout << "GTE" << endl;
    curpos += yyleng;
}

    /* ----- Special Symbols ----- */

{SEMICOLON} {
    cout << "SEMICOLON" << endl;
    curpos += yyleng;
}

{COLON} {
    cout << "COLON" << endl;
    curpos += yyleng;
}

{COMMA} {
    cout << "COMMA" << endl;
    curpos += yyleng;
}

{L_PAREN} {
    cout << "L_PAREN" << endl;
    curpos += yyleng;
}

{R_PAREN} {
    cout << "R_PAREN" << endl;
    curpos += yyleng;
}

{L_SQUARE_BRACKET} {
    cout << "L_SQUARE_BRACKET" << endl;
    curpos += yyleng;
}

{R_SQUARE_BRACKET} {
    cout << "R_SQUARE_BRACKET" << endl;
    curpos += yyleng;
}

{ASSIGN} {
    cout << "ASSIGN" << endl;
    curpos += yyleng;
}

    /* ----- Identifiers && Numbers ----- */

{NUMBER} {
    cout << "NUMBER" << " " << yytext << endl;
    curpos += yyleng;
}

{IDENTIFIER} {
    cout << "IDENT" << " " << yytext << endl;
    curpos += yyleng;
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

    /* ----- Error Catching ----- */
    /* need to add error catch for bad variable names with specific error messages for each kind of error */

. {
    /* Error: Unrecognized Symbol */
    printf("Error at line %d, column %d: unrecognized symbol \"%s\"\n", curline, curpos, yytext);
    exit(0);
}


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

    yylex();
}

