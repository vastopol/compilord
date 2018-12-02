/* -*- c++ -*- */
/* source.y for Mini-L to MIL Fall 2018*/


// Everything from here to "%}" is copied verbatim to the top of source.tab.c
%{

#include "heading.h"

int yyerror( char* s );
int yylex( void );

int vectorSize;               // holds the size of vectors being declared
int reductionCt;
ostringstream rules;   // holds grammar rules printed out by actions
ostringstream decs;    // holds gen()-emitted target-code declarations
ostringstream code;    // holds gen()-emitted target-code instructions
ostringstream init;    // holds gen()-emitted target-code instructions

extern FILE* yyin;     // declare extern to remove compiler error

extern int curline;
extern int curpos;

void print_funs();
void print_symtabs();

string funs;              // used to write out a whole function to a string from milvec
vector<string> funslst;   // hold all the compiled function strings

map<string,string> symtab;              // symbol table for current function
vector< map<string,string> > symtablst; // all symbol tables for every function

%}


%union{

  // Here we define the types and names of the components of YYSTYPE, which
  // is the type of the semantic portion of parse-stack entries.

  // declarations of union members for lexical values of tokens
  int                 junk;         // for one-of-a-kind lexical values
  int	              int_val;      // values of integer literals, i.e., NUMBER
  string*             ident;        // points to identifers' actual lexemes

  // declarations of union members lexical values of non-terminals,
  // which are pointers to translation records
  string*             code;         // for all nonterminals.

}

// Here is the start symbol of the grammar.
%start	                    Program

// Here we specify the non-terminals symbols and which components of
// the union YYSTYPE their translation records will occupy.

  %type   <code>     Program
  %type   <code>     Decl
  %type   <code>     StmtList
  %type   <code>     ExpList
  %type   <code>     FunctionDecl
  %type   <code>     BoolExp
  %type   <ident>    Exp
  %type   <code>     Stmt

// Here, in order of increasing precedence, are the names of the
// tokens, their associativity, and which components of the union
// YYSTYPE their lexical values will occupy.
%nonassoc  <junk>       LO
%left      <junk>       FUNCTION
%token     <Junk>       BEGINPARAMS
%token     <junk>       ENDPARAMS
%token     <junk>       BEGINLOCALS
%token     <junk>       ENDLOCALS
%token     <junk>       BEGINBODY
%token     <junk>       ENDBODY
%token     <junk>       ARRAY
%token     <junk>       OF
%token     <junk>       IF
%token     <junk>       THEN
%token     <junk>       ENDIF
%token     <junk>       ELSE
%token     <junk>       WHILE
%token     <junk>       BEGINLOOP
%token     <junk>       ENDLOOP
%token     <junk>       DO
%token     <junk>       READ
%token     <junk>       WRITE
%token     <junk>       CONTINUE
%token     <junk>       RETURN
%token     <junk>       INTEGER
%token     <int_val>    NUMBER
%token     <ident>      ID
%token     <junk>       TRUE
%token     <junk>       FALSE
%token     <junk>       ASSIGN
%left      <junk>       OR
%left      <junk>       AND
%right     <junk>       NOT
%nonassoc  <junk>       NE EQ LE GE '>' '<'
%left      <junk>       '+' '-'
%left      <junk>       '*' '/' '%'
%left      <junk>       ';'
%left      <junk>       ':'
%left      <junk>       ','
%left      <junk>       ')'
%left      <junk>       '('
%left      <junk>       ']'
%left      <junk>       '['
%token     <junk>       HI


// Here are the grammar rules; their semantic actions will be added later

%%

Program
    : /* EMPTY */    // Possibly empty list of FunctionDecls
        {
            rules << "Program -> /* EMPTY */ \n";
        }
    | Program FunctionDecl       //
        {
            rules << "Program -> ProgramFunctionDecl \n";
        }
    ;

StmtList
    : Stmt ';'          // nonempty, semicolon terminated. *
        {
            rules << "StmtList -> Stmt \n";
        }
    | StmtList Stmt ';'
        {
            rules << "StmtList -> StmtList Stmt ';' \n";
        }
    ;

ExpList
    : /* EMPTY */      // possibly empty, comma separated. *
        {
            rules << "ExpList -> /* EMPTY */ \n";
        }
    | ExpList ',' Exp
        {
            rules << "ExpList -> Explist ',' Exp \n";
        }
    ;

FunctionDecl
    : FUNCTION ID ';'
      BEGINPARAMS DeclList  ENDPARAMS
      BEGINLOCALS DeclList  ENDLOCALS
      BEGINBODY   StmtList  ENDBODY
        {
            rules << "FunctionDecl -> FUNCTION ID ';' \n";
            rules << "   BEGINPARAMS DeclList  ENDPARAMS \n"; // scalars
            rules << "   BEGINLOCALS DeclList  ENDLOCALS \n";
            rules << "   BEGINBODY   StmtList  ENDBODY \n";
                // for code, must expand lists

            funs += "func ";
            funs += *$2;
            funs += "\n";
            funs += code.str();
            funs += "endfunc\n";

            funslst.push_back(funs);
            funs = "";

        }
    ;

Decl
    : ID ':' INTEGER                      // A scalar variable *
        {
            rules << "Decl -> ID ':' INTEGER \n";
            code << ". " << *($1) << "\n";
        }
    | ID ':' ARRAY '[' NUMBER ']' OF INTEGER   // A vector var *
        {
            rules << "Decl -> ID ':' ARRAY '[' NUMBER ']' OF INTEGER \n";
            vectorSize = $5;
            code  << ".[] " << *($1) << ", " << vectorSize << "\n";
        }
    | ID ',' Decl                           // right recursion *
        {
            rules << "VectorDec. ->  ID ',' VectorDecl \n";
            //code  << ".[] " << *($1) << ", " << vectorSize << "\n"; // Broken: this will declare scalars in a list as arrays
            code << ". " << *$1 << "\n";
        }
    ; // vectorSize is global declared at the top of main.cc

DeclList
    : /* EMPTY */   // possibly empty, semicolon terminated.
        {
            rules << "DeclList -> EMPTY\n";
        }
    | DeclList Decl ';'                      // left recursion *
        {
            rules << "DeclList -> DeclList Decl ';' \n";
        }
    ;

BoolExp
    : TRUE
        {
            rules << " TRUE \n";
            $$ = new string("_T" + to_string(reductionCt++));
            code << ". " << *$$ << "\n";                        // declare temp
            code << "= " << *$$ << ", " << 1 << "\n";
        }
    | FALSE
        {
            rules << " FALSE \n";
            $$ = new string("_T" + to_string(reductionCt++));
            code << ". " << *$$ << "\n";                        // declare temp
            code << "= " << *$$ << ", " << 0 << "\n";
        }
    | '(' BoolExp ')'
        {
            rules << " '(' BoolExp ')' \n";
            $$ = new string("_T" + to_string(reductionCt++));
            code << ". " << *$$ << "\n";                        // declare temp
            code << "= " << *$$ << ", " << *$2 << "\n";
        }
    | NOT BoolExp
        {
            rules << " NOT BoolExp \n";
            $$ = new string("_T" + to_string(reductionCt++));
            code << ". " << *$$ << "\n";                        // declare temp
            code << "! " << *$$ << ", " << *$2 << "\n";
        }
    | BoolExp AND BoolExp
        {
            rules << " BoolExp AND BoolExp \n";
            $$ = new string("_T" + to_string(reductionCt++));
            code << ". " << *$$ << "\n";                        // declare temp
            code << "&& " << *$$ << ", " << *$1 << ", " << *$3 << "\n";
        }
    | BoolExp OR BoolExp
        {
            rules << " BoolExp OR BoolExp \n";
            $$ = new string("_T" + to_string(reductionCt++));
            code << ". " << *$$ << "\n";                        // declare temp
            code << "|| " << *$$ << ", " << *$1 << ", " << *$3 << "\n";
        }
    | Exp EQ Exp
        {
            rules << " Exp EQ Exp \n";
            $$ = new string("_T" + to_string(reductionCt++));
            code << ". " << *$$ << "\n";                        // declare temp
            code << "== " << *$$ << ", " << *$1 << ", " << *$3 << "\n";
        }
    | Exp NE Exp
        {
            rules << " Exp NE Exp \n";
            $$ = new string("_T" + to_string(reductionCt++));
            code << ". " << *$$ << "\n";                        // declare temp
            code << "!= " << *$$ << ", " << *$1 << ", " << *$3 << "\n";
        }
    | Exp GE Exp
        {
            rules << " Exp GE Exp \n";
            $$ = new string("_T" + to_string(reductionCt++));
            code << ". " << *$$ << "\n";                        // declare temp
            code << ">= " << *$$ << ", " << *$1 << ", " << *$3 << "\n";
        }
    | Exp LE Exp
        {
            rules << " Exp LE Exp \n";
            $$ = new string("_T" + to_string(reductionCt++));
            code << ". " << *$$ << "\n";                        // declare temp
            code << "<= " << *$$ << ", " << *$1 << ", " << *$3 << "\n";
        }
    | Exp '>' Exp
        {
            rules << " Exp '>' Exp \n";
            $$ = new string("_T" + to_string(reductionCt++));
            code << ". " << *$$ << "\n";                        // declare temp
            code << "> " << *$$ << ", " << *$1 << ", " << *$3 << "\n";
        }
    | Exp '<' Exp
        {
            rules << " Exp '<' Exp \n";
            $$ = new string("_T" + to_string(reductionCt++));
            code << ". " << *$$ << "\n";                        // declare temp
            code << "< " << *$$ << ", " << *$1 << ", " << *$3 << "\n";
        }
    ;

Exp
    : ID                                 // scalar variable *
        {
            rules << "Exp -> ID\n";
            $$ = new string("_T" + to_string(reductionCt++));
            code << ". " << *$$ << "\n";                        // declare temp
            code << "= " << *$$ << ", " << *$1 << "\n";
        }
    | ID '[' Exp ']'       //  vector/subscripted variable *
        {
            rules << "Exp -> ID '[' Exp ']' \n";
            $$ = new string("_T" + to_string(reductionCt++));
            code << ". " << *$$ << "\n";                        // declare temp
            code << "=[] " << *$$ << ", " << *$1 << ", " << *$3 << "\n";
        }
    | Exp '+' Exp
        {
            rules << "Exp -> Exp '+' Exp\n";
            $$ = new string("_T" + to_string(reductionCt++));
            code << ". " << *$$ << "\n";                        // declare temp
            code << "+ " << *$$ << ", " << *$1 << ", " << *$3 << "\n";
        }
    | Exp '-' Exp
        {
            rules << "Exp -> Exp '-' Exp\n";
            $$ = new string("_T" + to_string(reductionCt++));
            code << ". " << *$$ << "\n";                        // declare temp
            code << "- " << *$$ << ", " << *$1 << ", " << *$3 << "\n";
        }
    | Exp '*' Exp
        {
            rules << "Exp -> Exp '*' Exp\n";
            $$ = new string("_T" + to_string(reductionCt++));
            code << ". " << *$$ << "\n";                        // declare temp
            code << "* " << *$$ << ", " << *$1 << ", " << *$3 << "\n";
        }
    |  Exp '/' Exp
        {
            rules << "Exp -> Exp '/' Exp\n";
            $$ = new string("_T" + to_string(reductionCt++));
            code << ". " << *$$ << "\n";                        // declare temp
            code << "/ " << *$$ << ", " << *$1 << ", " << *$3 << "\n";
        }
    | Exp '%' Exp
        {
            rules << "Exp -> Exp '%' Exp\n";
            $$ = new string("_T" + to_string(reductionCt++));
            code << ". " << *$$ << "\n";                        // declare temp
            code << "% " << *$$ << ", " << *$1 << ", " << *$3 << "\n";
    }
    | '-' Exp  %prec '('
        {
            rules << "Exp -> '-' Exp\n";
            $$ = new string("_T" + to_string(reductionCt++));
            code << ". " << *$$ << "\n";                        // declare temp
            code << "- " << *$$ << ", " << 0 << ", " << *$2 << "\n";
        }
    | NUMBER
        {
            $$ = new string("_T" + to_string(reductionCt++));
            code << ". " << *$$ << "\n";                        // declare temp
            rules << "Exp -> NUMBER\n";
            code << "= " << *$$ << ", " << $1 << "\n";
        }
    | '(' Exp ')'
        {
            rules << "Exp -> '(' Exp ')' \n";
            $$ = new string("_T" + to_string(reductionCt++));
            code << ". " << *$$ << "\n";                        // declare temp
            code << "= " << *$$ << ", " << *$2 << "\n";
        }
    | ID '(' ExpList ')'      // function call         // ???
        {
            rules << "Exp -> ID '(' Exp ')' \n";
            $$ = new string("_T" + to_string(reductionCt++)); // dest
            // parameters
            code << ". " << *$$ << "\n";                        // declare temp dest
            code << "call " << *$1 << ", " << *$$ << "\n";      // call func with dest
        }
    ;

ReadStmt
    : READ ID                                            // *
        {
            rules << "ReadStmt -> Read ID \n";
            code  << ".< " << *$2 << "\n";
        }
    | READ ID '[' Exp ']'
        {
            rules << "ReadStmt -> READ ID '[' Exp ']' \n";
            code  << ".[]< " << *$2 << ", " << *$4 << "\n";
        }
    | ReadStmt ',' ID                     // left recursion *
        {
            rules << "ReadStmt -> ReadStmt ',' ID \n";
            code  << ".< " << *$3 << "\n";
        }
    | ReadStmt ',' ID '[' Exp ']'         // left recursion *
        {
            rules << "ReadStmt -> ReadStmt ',' ID '[' Exp ']'\n";
            code  << ".[]< " << *$3 << ", " << $5 << "\n";
        }
    ;

WriteStmt
    : WRITE Exp                                          // *
        {
            rules << "WriteStmt -> WRITE Exp \n";
            code << ".> " << *$2 << "\n";
        }
    | WriteStmt ',' Exp                   // left recursion *
        {
            rules << "WriteStmt -> WriteStmt ',' Exp \n";
            code << ".> " << *$3 << "\n";
        }

Stmt
    : ID ASSIGN Exp    // The desination can be either scalar *
        {
            rules << "Stmt -> ID ASSIGN Exp\n";
            code << "= " << *$1 << ", " << *$3 << "\n";
        }
    | ID '[' Exp ']' ASSIGN Exp             // or subscripted *
        {
            rules << "Stmt -> ID '[' Exp ']' ASSIGN Exp\n";
            code << "[]= " << *$1 << ", " << *$3 << ", " << *$6 << "\n";
        }
    | ReadStmt                                 // See above *
        {
            rules << "Stmt -> ReadStmt" ;
        }                  // *
    | WriteStmt                                // see above *
        {
            rules << "Stmt -> WriteStmt" ;
        }                 // *
    | IF BoolExp THEN StmtList ELSE StmtList ENDIF    // outline
        {
            rules << "Stmt -> IF BoolExp THEN StmtList ELSE StmtList ENDIF\n";
            code << "?:= THEN, BoolExp \n"        // if BoolExp goto THEN
            << ":= ELSE \n"                  // goto ELSE
            << ":THEN \n"                    // THEN:
            << "..."                         // dump StmtList
            << ":= ENDIF \n"                 // goto ENDIF
            << ":ELSE \n"                    // ELSE:
            << "..."                         // StmtList
            << ":ENDIF \n";                   // ENDIF:
        }
    | IF BoolExp THEN StmtList ENDIF                  // outline
        {
            rules << "Stmt -> IF BoolExp THEN StmtList ENDIF\n";
            code << "?:= THEN, BoolExp \n"        // if BoolExp goto THEN
            << ":= ELSE \n"                  // goto ELSE
            << ":THEN \n"                    // THEN:
            << "..."                         // StmtList
            << ":ELSE \n";                    // ELSE:
        }
    | WHILE BoolExp BEGINLOOP StmtList ENDLOOP        // outline
        {
            rules << "Stmt -> WHILE BoolExp BEGINLOOP StmtList ENDLOOP\n";
            code << ": WHILE \n"                  // WHILE:
            << "?:= BEGINLOOP, BoolExp \n"
            // if boolExp goto BEGINLOOP
            << ":= EXIT \n"        // otherwise, goto EXIT
            << ": BEGINLOOP \n"              // BEGINLOOP:
            << "..."                         // StmtList
            << ":= WHILE \n"                 // goto WHILE
            << ": EXIT \n";                   // EXIT:
        }
    | DO BEGINLOOP StmtList ENDLOOP WHILE BoolExp // outline
        {
            rules << "Stmt -> DO BEGINLOOP StmtList ENDLOOP "
            << "WHILE BoolExp\n";
            code  << ": DO BEGINLOOP \n"          // BEGINLOOP:
            << "..."                        // StmtList
            << "?:=  << BEGINLOOP <<,  << BoolExp \n";
            // if BoolExp goto BEGINLOOP
        }
    | CONTINUE                                // outline
        {
            rules << "Stmt -> CONTINUE\n";
            code  << ":= BEGINLOOP\n";
        }
    | RETURN Exp                                        // ???
        {
            rules << "Stmt -> RETURN Exp\n";
        }
    ;

%%

// Here is the function that reports lexical and parse errors.  It is
// overloaded so that error messages can be either C strings or C++
// strings.

int yyerror( string s ) {  // error handler routine
  extern int  yylineno;    // defined and maintained in lex.c
  extern char* yytext;     // defined and maintained in lex.c
  cerr << "ERROR: " << s << " at symbol " << yytext;
  cerr << " on line " << curline << " position " << curpos << endl;
  exit( 1 );
}

int yyerror( char* s ) { return yyerror( string(s) ); }


int main(int argc, char** argv)
{
    // try to read from a input file
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

    yyparse(); // calls yylex()

    print_funs();
    //print_symtabs();
}

void print_funs()
{
    // print out mil code for functions
    for(auto i : funslst)
    {
        output(i);
    }
}

void print_symtabs()
{
    // print out symbol tables
    for(auto i : symtablst)
    {
        for( auto j : i )
        {
            string t = j.first;
            t+=" ";
            t+=j.second;
            t+=" ";
            output(t);
        }
        output("");
    }
}

/*  Commentary

There is a global symbol table for functions that specifies the number
of parameters for each.

A function's symbol table includes its parameters and locals without
distinction between them.  Each variable has a non-negative size
attribute, which is zero iff the variable is a scalar.  That table is
a strings-to-integers map that exists during and only during the
compilation of that function.

Even though a function's symbol table doesn't need to be accessed
beyond the end of the function's declaration, it would be a good idea
to keep it around for debugging and dynamic bounds-checking.
*/



