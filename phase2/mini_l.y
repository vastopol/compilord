    /*
    ----------------------------------------
    Syntax Analyzer/Parser for MINI-L
    Sean Richardson
    ----------------------------------------
    */


    /*
    ----------------------------------------
    Declarations
    ----------------------------------------
    */


%{

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
    int ival;
}

%error-verbose

%start input

%token FUNCTION BEGIN_PARAMS END_PARAMS BEGIN_LOCALS END_LOCALS BEGIN_BODY END_BODY INTEGER ARRAY OF IF THEN ENDIF ELSE WHILE DO FOREACH IN BEGINLOOP ENDLOOP CONTINUE READ WRITE AND OR NOT TRUE FALSE RETURN
%token ADD SUB MULT DIV MOD
%token EQ NEQ LT GT LTE GTE
%token SEMICOLON COLON COMMA L_PAREN R_PAREN L_SQUARE_BRACKET R_SQUARE_BRACKET ASSIGN

%token END
%token ID

%token <ival> NUMBER
%type  <ival> exp

%left PLUS MINUS
%left MULT DIV MOD
%left EQ NEQ LT GT LTE GTE

%left AND OR
%right NOT
%right ASSIGN
%right UMINUS


%%


    /*
    ----------------------------------------
    Rules
    ----------------------------------------
    */

input:
		| input line
		;

line:	exp END { printf("\t%s\n", $1); }
		;

exp:	NUMBER { $$ = $1; }
        ;


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


