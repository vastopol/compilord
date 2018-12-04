/* -*- c++ -*- */
/* source.y for Mini-L to MIL Fall 2018*/


// Everything from here to "%}" is copied verbatim to the top of source.tab.c
%{

#include "heading.h"

int yyerror( char* s );
int yyerror( string s );
int yylex( void );

extern FILE* yyin;     // declare extern to remove compiler error

extern int curline;
extern int curpos;

void print_funs();
void print_symtabs();
void semantic_error(string s);

string fname;
int loop_deep = 0;
bool is_main = false;     // was main declared
map<string,int> decfunctions; // names of declared functions

ostringstream rules;   // holds grammar rules printed out by actions
ostringstream decs;    // holds gen()-emitted target-code declarations
ostringstream code;    // holds gen()-emitted target-code instructions
ostringstream bcode;    // holds gen()-emitted target-code instructions
stack<string> bstack;  // hold code strings of bools

int vectorSize;               // holds the size of vectors being declared
int reductionCt;

bool pdec_flag = true;  // parameter declarations, output $0, $1, etc...
int pnum_cnt = 0;       // count the # of params

bool continue_loop = false;
string continue_lbl_name;

string funs;              // used to write out a whole function to a string from milvec
vector<string> funslst;   // hold all the compiled function strings

map<string,int>::iterator iter;
map<string,int> symtab;              // symbol table for current function
vector< map<string,int> > symtablst; // all symbol tables for every function

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
  %type   <code>     DeclList
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

            if(is_main == false)
            {
                string s = "No main function declared";
                semantic_error(s);
            }
        }
    ;

StmtList
    : Stmt ';'          // nonempty, semicolon terminated. *
        {
            rules << "StmtList -> Stmt \n";

            $$ = new string(*$1);
            code.str("");
            code.clear();
        }
    | StmtList Stmt ';'
        {
            rules << "StmtList -> StmtList Stmt ';' \n";

            $$ = new string(*$1 + *$2);
            code.str("");
            code.clear();
        }
    ;

ExpList
    : /* EMPTY */      // possibly empty, comma separated. *
        {
            rules << "ExpList -> /* EMPTY */ \n";
        }
    | Exp
        {
            rules << "ExpList -> Exp \n";

            $$ = new string(*$1);
            code << "param " << *$$ << "\n";

        }
    | ExpList ',' Exp
        {
            rules << "ExpList -> Explist ',' Exp \n";

            $$ = new string(*$3);
            code << "param " << *$$ << "\n";
        }
    ;

beginparams
    : BEGINPARAMS
        {
            pdec_flag = true;
        }
      ;

endparams
    : ENDPARAMS
        {
            pdec_flag = false; // set flag to done with params
        }
    ;

FunctionDecl
    : FUNCTION ID ';'
      beginparams DeclList  endparams
      BEGINLOCALS DeclList  ENDLOCALS
      BEGINBODY   StmtList  ENDBODY
        {
            rules << "FunctionDecl -> FUNCTION ID ';' \n";
            rules << "   BEGINPARAMS DeclList  ENDPARAMS \n"; // scalars
            rules << "   BEGINLOCALS DeclList  ENDLOCALS \n";
            rules << "   BEGINBODY   StmtList  ENDBODY \n";
                // for code, must expand lists

            if(*$2 == "main")
            {
                is_main = true;
            }

            decfunctions[*$2] = 0;

            stringstream func_dec;
            func_dec << "func " << *$2 << "\n";
            func_dec << *$5;
            func_dec << *$8;
            func_dec << *$11;
            func_dec << "endfunc" << "\n";

            funs = func_dec.str();
            func_dec.str("");

            // save function
            funslst.push_back(funs);

            // save symtab
            symtablst.push_back(symtab);

            // reset variables
            code.str("");
            code.clear();
            decs.str("");
            decs.clear();
            bcode.str("");
            bcode.clear();
            symtab.clear();
            funs = "";
            continue_loop = false;
            pdec_flag = true;
            pnum_cnt = 0;

        }
    ;

Decl
    : ID ':' INTEGER                      // A scalar variable *
        {
            rules << "Decl -> ID ':' INTEGER \n";

            iter = symtab.find(*$1);
            if(iter != symtab.end())
            {
                string s = "Identifier " + *$1 + " multiple declares";
                semantic_error(s);
            }
            else if(iter->first == fname)
            {
                string s = "Identifier " + *$1 + " same name as file";
                semantic_error(s);
            }

            symtab[*$1] = 0; // add to symbol table

            decs << ". " << *($1) << "\n";
            if(pdec_flag == true)
            {
                decs << "= " << *$1 << ", $" << pnum_cnt << "\n";
                pnum_cnt++;
            }
            $$ = new string(decs.str());
            decs.str("");
            decs.clear();
        }
    | ID ':' ARRAY '[' NUMBER ']' OF INTEGER   // A vector var *
        {
            rules << "Decl -> ID ':' ARRAY '[' NUMBER ']' OF INTEGER \n";

            iter = symtab.find(*$1);
            if(iter != symtab.end())
            {
                string s = "Identifier " + *$1 + " multiple declares";
                semantic_error(s);
            }
            else if(iter->first == fname)
            {
                string s = "Identifier " + *$1 + " same name as file";
                semantic_error(s);
            }

            if($5 <= 0)
            {
                string s = "Zero/Negative array size";
                semantic_error(s);
            }

            symtab[*$1] = $5; // add to symbol table

            vectorSize = $5;
            decs  << ".[] " << *($1) << ", " << vectorSize << "\n";
            $$ = new string(decs.str());
            decs.str("");
            decs.clear();
        }
    | ID ',' Decl                           // right recursion *
        {
            rules << "VectorDec. ->  ID ',' VectorDecl \n";

            iter = symtab.find(*$1);
            if(iter != symtab.end())
            {
                string s = "Identifier " + *$1 + " multiple declares";
                semantic_error(s);
            }
            else if(iter->first == fname)
            {
                string s = "Identifier " + *$1 + " same name as file";
                semantic_error(s);
            }

            symtab[*$1] = 0; // add to symbol table

            decs << ". " << *$1 << "\n";
            if(pdec_flag == true)
            {
                decs << "= " << *$1 << ", $" << pnum_cnt << "\n";
                pnum_cnt++;
            }
            decs << *$3;
            $$ = new string(decs.str());
            decs.str("");
            decs.clear();
        }
    ; // vectorSize is global declared at the top of main.cc

DeclList
    : /* EMPTY */   // possibly empty, semicolon terminated.
        {
            rules << "DeclList -> EMPTY\n";

            //decls << *$1;
            $$ = new string(decs.str());
            decs.str("");
            decs.clear();
        }
    | DeclList Decl ';'                      // left recursion *
        {
            rules << "DeclList -> DeclList Decl ';' \n";

            decs << *$1 << *$2;
            $$ = new string(decs.str());
            decs.str("");
            decs.clear();
        }
    ;

BoolExp
    : TRUE
        {
            rules << " TRUE \n";

            $$ = new string("_T" + to_string(reductionCt++));

            bcode << code.str();
            bcode << ". " << *$$ << "\n";                        // declare temp
            bcode << "= " << *$$ << ", " << 1 << "\n";

            bstack.push(bcode.str());
            code.str("");
        }
    | FALSE
        {
            rules << " FALSE \n";

            $$ = new string("_T" + to_string(reductionCt++));

            bcode << code.str();
            bcode << ". " << *$$ << "\n";                        // declare temp
            bcode << "= " << *$$ << ", " << 0 << "\n";

            bstack.push(bcode.str());
            code.str("");

        }
    | '(' BoolExp ')'
        {
            rules << " '(' BoolExp ')' \n";

            $$ = new string("_T" + to_string(reductionCt++));

            bcode << code.str();
            bcode << ". " << *$$ << "\n";                        // declare temp
            bcode << "= " << *$$ << ", " << *$2 << "\n";

            bstack.push(bcode.str());
            code.str("");
        }
    | NOT BoolExp
        {
            rules << " NOT BoolExp \n";

            $$ = new string("_T" + to_string(reductionCt++));

            bcode << code.str();
            bcode << ". " << *$$ << "\n";                        // declare temp
            bcode << "! " << *$$ << ", " << *$2 << "\n";

            bstack.push(bcode.str());
            code.str("");
        }
    | BoolExp AND BoolExp
        {
            rules << " BoolExp AND BoolExp \n";

            $$ = new string("_T" + to_string(reductionCt++));

            bcode << code.str();
            bcode << ". " << *$$ << "\n";                        // declare temp
            bcode << "&& " << *$$ << ", " << *$1 << ", " << *$3 << "\n";

            bstack.push(bcode.str());
            code.str("");
        }
    | BoolExp OR BoolExp
        {
            rules << " BoolExp OR BoolExp \n";

            $$ = new string("_T" + to_string(reductionCt++));

            bcode << code.str();
            bcode << ". " << *$$ << "\n";                        // declare temp
            bcode << "|| " << *$$ << ", " << *$1 << ", " << *$3 << "\n";

            bstack.push(bcode.str());
            code.str("");
        }
    | Exp EQ Exp
        {
            rules << " Exp EQ Exp \n";

            $$ = new string("_T" + to_string(reductionCt++));

            bcode << code.str();
            bcode << ". " << *$$ << "\n";                        // declare temp
            bcode << "== " << *$$ << ", " << *$1 << ", " << *$3 << "\n";

            bstack.push(bcode.str());
            code.str("");
        }
    | Exp NE Exp
        {
            rules << " Exp NE Exp \n";

            $$ = new string("_T" + to_string(reductionCt++));

            bcode << code.str();
            bcode << ". " << *$$ << "\n";                        // declare temp
            bcode << "!= " << *$$ << ", " << *$1 << ", " << *$3 << "\n";

            bstack.push(bcode.str());
            code.str("");
        }
    | Exp GE Exp
        {
            rules << " Exp GE Exp \n";

            $$ = new string("_T" + to_string(reductionCt++));

            bcode << code.str();
            bcode << ". " << *$$ << "\n";                        // declare temp
            bcode << ">= " << *$$ << ", " << *$1 << ", " << *$3 << "\n";

            bstack.push(bcode.str());
            code.str("");
        }
    | Exp LE Exp
        {
            rules << " Exp LE Exp \n";

            $$ = new string("_T" + to_string(reductionCt++));

            bcode << code.str();
            bcode << ". " << *$$ << "\n";                        // declare temp
            bcode << "<= " << *$$ << ", " << *$1 << ", " << *$3 << "\n";

            bstack.push(bcode.str());
            code.str("");
        }
    | Exp '>' Exp
        {
            rules << " Exp '>' Exp \n";

            $$ = new string("_T" + to_string(reductionCt++));

            bcode << code.str();
            bcode << ". " << *$$ << "\n";                        // declare temp
            bcode << "> " << *$$ << ", " << *$1 << ", " << *$3 << "\n";

            bstack.push(bcode.str());
            code.str("");
        }
    | Exp '<' Exp
        {
            rules << " Exp '<' Exp \n";

            $$ = new string("_T" + to_string(reductionCt++));

            bcode << code.str();
            bcode << ". " << *$$ << "\n";                        // declare temp
            bcode << "< " << *$$ << ", " << *$1 << ", " << *$3 << "\n";

            bstack.push(bcode.str());
            code.str("");
        }
    ;

Exp
    : ID                                 // scalar variable *
        {
            rules << "Exp -> ID\n";

            iter = symtab.find(*$1);
            if(iter == symtab.end())
            {
                string s = "Identifier " + *$1 + " used, but not declared";
                semantic_error(s);
            }
            else if(iter->second > 0)
            {
                string s = "Using an array " + *$1 + " as an integer";
                semantic_error(s);
            }

            $$ = new string("_T" + to_string(reductionCt++));

            code << ". " << *$$ << "\n";                        // declare temp
            code << "= " << *$$ << ", " << *$1 << "\n";
        }
    | ID '[' Exp ']'       //  vector/subscripted variable *
        {
            rules << "Exp -> ID '[' Exp ']' \n";

            iter = symtab.find(*$1);
            if(iter == symtab.end())
            {
                string s = "Identifier " + *$1 + " used, but not declared";
                semantic_error(s);
            }
            else if(iter->second > 0)
            {
                string s = "Using an integer " + *$1 + " as an array";
                semantic_error(s);
            }

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
            rules << "Exp -> NUMBER\n";

            $$ = new string("_T" + to_string(reductionCt++));

            code << ". " << *$$ << "\n";                        // declare temp
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

            iter = decfunctions.find(*$1);
            if(iter == decfunctions.end())
            {
                string s = "Using undeclared function " + *$1;
                semantic_error(s);
            }

            $$ = new string("_T" + to_string(reductionCt++));   // dest

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
            $$ = new string(code.str());
        }
    | ID '[' Exp ']' ASSIGN Exp             // or subscripted *
        {
            rules << "Stmt -> ID '[' Exp ']' ASSIGN Exp\n";

            code << "[]= " << *$1 << ", " << *$3 << ", " << *$6 << "\n";
            $$ = new string(code.str());
        }
    | ReadStmt                                 // See above *
        {
            rules << "Stmt -> ReadStmt" ;

            $$ = new string(code.str());
        }                  // *
    | WriteStmt                                // see above *
        {
            rules << "Stmt -> WriteStmt" ;

            $$ = new string(code.str());
        }                 // *
    | IF BoolExp THEN StmtList ELSE StmtList ENDIF    // outline
        {
            rules << "Stmt -> IF BoolExp THEN StmtList ELSE StmtList ENDIF\n";

            string* then_lbl = new string("_L" + to_string(reductionCt++));
            string* else_lbl = new string("_L" + to_string(reductionCt++));
            string* endif_lbl = new string("_L" + to_string(reductionCt++));

            string bool_string = bstack.top(); bstack.pop();

            code << bool_string;
            code << "?:= " << *then_lbl << ", " << *$2 << "\n";        // if BoolExp goto THEN
            code << ":= " << *else_lbl << "\n";                  // goto ELSE
            code << ": " << *then_lbl << "\n";                    // THEN:
            //code << "...\n";                         // StmtList
            code << *$4;
            code << ":= " << *endif_lbl << "\n";                 // goto ENDIF
            code << ": " << *else_lbl << "\n";                    // ELSE:
            //code << "...\n";                         // StmtList
            code << *$6;
            code << ": " << *endif_lbl << "\n";                   // ENDIF:
            $$ = new string(code.str());
        }
    | IF BoolExp THEN StmtList ENDIF                  // outline
        {
            rules << "Stmt -> IF BoolExp THEN StmtList ENDIF\n";

            string* then_lbl = new string("_L" + to_string(reductionCt++));
            string* else_lbl = new string("_L" + to_string(reductionCt++));

            string bool_string = bstack.top(); bstack.pop();

            code << bool_string;
            code << "?:= " << *then_lbl << ", " << *$2 << "\n";        // if BoolExp goto THEN
            code << ":= " << *else_lbl << "\n";                  // goto ELSE
            code << ": " << *then_lbl << "\n";                    // THEN:
            //code << "...\n";                         // StmtList
            code << *$4;
            code << ": " << *else_lbl << "\n";                    // ELSE:
            $$ = new string(code.str());
        }
    | WHILE BoolExp BEGINLOOP {++loop_deep;} StmtList ENDLOOP        // outline
        {
            rules << "Stmt -> WHILE BoolExp BEGINLOOP StmtList ENDLOOP\n";

            loop_deep--;

            string* while_lbl = new string("_L" + to_string(reductionCt++));
            string* begin_lbl = new string("_L" + to_string(reductionCt++));
            string* exit_lbl = new string("_L" + to_string(reductionCt++));

            string bool_string = bstack.top(); bstack.pop();

            if(continue_loop == true)
            {
                code << ": " << continue_lbl_name << "\n";
                continue_loop = false;
            }
            code << ": " << *while_lbl << "\n";                  // WHILE:
            code << bool_string;
            code << "?:= " << *begin_lbl << ", " << *$2 << "\n";
            // if boolExp goto BEGINLOOP
            code << ":= " << *exit_lbl << "\n";        // otherwise, goto EXIT
            code << ": " << *begin_lbl <<  "\n";              // BEGINLOOP:
            //code << "...\n";                         // StmtList
            code << *$5;
            code << ":= " << *while_lbl <<  "\n";                 // goto WHILE
            code << ": " << *exit_lbl << "\n";                   // EXIT:
            $$ = new string(code.str());
        }
    | DO BEGINLOOP {++loop_deep;} StmtList ENDLOOP WHILE BoolExp // outline
        {
            rules << "Stmt -> DO BEGINLOOP StmtList ENDLOOP WHILE BoolExp\n";

            --loop_deep;

            string* begin_lbl = new string("_L" + to_string(reductionCt++));

            string bool_string = bstack.top(); bstack.pop();

            code  << ": " << *begin_lbl << "\n" ;         // BEGINLOOP:
            //code << "...\n";                        // StmtList
            code << *$4;
            if(continue_loop == true)
            {
                code << ": " << continue_lbl_name << "\n";
                continue_loop = false;
            }
            code << bool_string;
            code << "?:= " << *begin_lbl << ", " << *$7 << "\n";
            // if BoolExp goto BEGINLOOP
            $$ = new string(code.str());
        }
    | CONTINUE                                // outline
        {
            rules << "Stmt -> CONTINUE\n";

            if(loop_deep == 0)
            {
                string s = "Continue not in a loop";
                semantic_error(s);
            }

            string* cont_lbl = new string("_L" + to_string(reductionCt++));

            continue_loop = true;
            continue_lbl_name = *cont_lbl;

            code  << ":= " << *cont_lbl << "\n";
            $$ = new string(code.str());
        }
    | RETURN Exp                                        // ???
        {
            rules << "Stmt -> RETURN Exp\n";

            $$ = new string("_T" + to_string(reductionCt++));
            code << ". " << *$$ << "\n";                        // declare temp
            code << "= " << *$$ << ", " << *$2 << "\n";
            code << "ret " << *$$ << "\n";
            $$ = new string(code.str());
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
        //yyin = stdin;
        cout << "Error: needs input file" << endl;
        exit(1);
    }

    fname = string(argv[1]);
    fname.pop_back(); fname.pop_back(); fname.pop_back(); fname.pop_back(); // remove .min

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
            t+=to_string(j.second);
            t+=" ";
            output(t);
        }
        output("");
    }
}

void semantic_error(string s)
{
    string err = "Semantic error: ";
    err += s;
    output(err);
}



