    /*
    ----------------------------------------
    Syntax Analyzer/Parser for MINI-L (v3)
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

// externals in flex file
extern int curline;
extern int curpos;

// externals for fixing g++ errors
extern FILE* yyin;  // multiple declarations of yyin
extern int yylex(); // ‘yylex’ was not declared in this scope

// user subroutines
void print_funs();
void print_symtabs();
void yyerror(string);
void yyerror(const char *msg);

// symbol table. str stream, etc...
string buf;
stringstream ss;

string funs;
vector<string> funslst;

int vtag; // for assign choosing: 0 =  num, 1 = str
vector<string> rwvarslst; // var names in read,write,assign
vector<string> varidxlst; // array index

vector<string> idslst; // idents for symbol table

map<string,string> symtab;
vector< map<string,string> > symtablst;

%}

%union{
    int     ival;
    string* sval; // for raw char array yytext
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
        {
            //cout << "prog_start -> functions" << endl;
        }
    ;

functions
    : /* epsilon */
        {
            //cout << "functions -> epsilon" << endl;
        }
    | function functions
        {
            //cout << "functions -> function functions" << endl;
        }
    ;

function
    : FUNCTION identifierF SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY
        {
            //cout << "function -> FUNCTION IDENT SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY" << endl;

            // write mil code
            funs += "func ";
            while(getline(ss,buf))
            {
                funs += buf;
                funs += "\n";
            }
            funs += "endfunc\n";

            // save mil code function str
            funslst.push_back(funs);

            // save symbol table
            symtablst.push_back(symtab);

            // clear
            funs = "";
            buf = "";
            ss.str("");
            ss.clear();
            idslst.clear();
            rwvarslst.clear();
            symtab.clear();
        }
    ;

declarations
    : /* epsilon */
        {
            //cout << "declarations -> epsilon" << endl;
        }
    | declaration SEMICOLON declarations
        {
            //cout << "declarations -> declaration SEMICOLON declarations" << endl;
        }
    ;

declaration                          // add identifiers to symbol table here
    : identifiers COLON INTEGER
        {
            //cout << "declaration -> identifiers COLON INTEGER" << endl;

            for (auto id : idslst)
            {
                symtab[id] = "0";
                ss << ". " << id << "\n";
            }
            idslst.clear();
        }
    | identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER
        {
            //cout << "declaration -> identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER" << endl;

            for (auto id : idslst)
            {
                symtab[id] = to_string(yylval.ival);
                ss << ".[] " << id << ", " << yylval.ival << "\n";
            }
            idslst.clear();
        }
    ;

identifiers
    : identifier
        {
            //cout << "ident -> IDENT " << *yylval.sval << endl;

            //output("_1_"+*yylval.sval)
        }
    | identifier COMMA identifiers
        {
            //cout << "identifiers -> ident COMMA identifiers" << endl;

            //output("_2_"+*yylval.sval)
        }
    ;

identifier
    : IDENT
        {
            //cout << "ident -> IDENT " << *yylval.sval << endl;

            //output("_~_"+*yylval.sval);

            //ss << *yylval.sval << "\n";   // Might comment out because double print of ids for some rules

            // capture name to store in symtab map
            idslst.push_back(*yylval.sval);
        }
    ;

identifierF
    : IDENT
        {
            //cout << "ident -> IDENT " << *yylval.sval << endl;

            //output("_F_"+*yylval.sval);

            ss << *yylval.sval << "\n";  // need function name output for the call instruction

            /*
            // capture name to store in symtab map
            idslst.push_back(*yylval.sval);
            */
        }
    ;

statements
    : /* epsilon */
        {
            //cout << "statements -> epsilon" << endl;
        }
    | statement SEMICOLON statements
        {
            //cout << "statements -> statement SEMICOLON statements" << endl;
        }
    ;

statement
    : var ASSIGN expression
        {
            //cout << "statement -> var ASSIGN expression" << endl;

            /*output("id");
            outarr(idslst);
            output("");
            output("var");
            outarr(rwvarslst);
            output("");
            output("index");
            outarr(varidxlst);
            output("");*/

            map<string,string>::iterator it = symtab.find(rwvarslst.at(0));
            if(it == symtab.end()){ yyerror("error: in assign null var encountered"); }

            if(it->second == "0") // scalar
            {
                ss << "= " << rwvarslst.at(0) << ", ";

                if(vtag == 0) // was a number
                {
                    ss << yylval.ival << "\n";  // int = num
                }
                else // need to check yylval.sval is int or arr
                {
                    ss << *yylval.sval << "\n";  // int = int

                    // int = arr
                    // fixme
                }
            }
            else // array
            {
                ss << "[]= " << rwvarslst.at(0) << ", " << varidxlst.at(0) << ", ";

                if(vtag == 0) // was a number
                {
                    ss << yylval.ival << "\n";  // arr = num
                }
                else // need to check yylval.sval is int or arr
                {
                    ss << *yylval.sval << "\n";  // arr = int

                    // arr = arr
                    // fixme
                }
                varidxlst.erase(varidxlst.begin());
            }
            vtag = -1;         // reset var tag
            rwvarslst.clear(); // remove var from list
        }
    | IF bool-expr THEN statements ENDIF
        {
            //cout << "statement -> IF bool_exp THEN statements ENDIF" << endl;

            ss << "?:= " << "\n";
        }
    | IF bool-expr THEN statements ELSE statements ENDIF
        {
            //cout << "statement -> IF bool_exp THEN statements ELSE statements ENDIF" << endl;

            ss << "?:= " << "\n";
        }
    | WHILE bool-expr BEGINLOOP statements ENDLOOP
        {
            //cout << "statement -> WHILE bool_exp BEGINLOOP statements ENDLOOP" << endl;
        }
    | DO BEGINLOOP statements ENDLOOP WHILE bool-expr
        {
            //cout << "statement -> DO BEGINLOOP statements ENDLOOP WHILE bool_exp" << endl;
        }
    | READ vars
        {
            //cout << "statement -> READ vars" << endl;

            map<string,string>::iterator it;
            for(auto v : rwvarslst)
            {
                it = symtab.find(v);
                if(it == symtab.end()){ yyerror("read error with id"); }
                if(it->second == "0")
                {
                    ss << ".< " << v << "\n";
                }
                else
                {
                    //ss << ".[]< " << v << ", " << yylval.ival << "\n";

                    ss << ".[]< " << v << ", " << varidxlst.at(0) << "\n";
                    varidxlst.erase(varidxlst.begin());
                }
            }
            rwvarslst.clear();
        }
    | WRITE vars
        {
            //cout << "statement -> WRITE vars" << endl;

            map<string,string>::iterator it;
            for(auto v : rwvarslst)
            {
                it = symtab.find(v);
                if(it == symtab.end()){ yyerror("write error with id"); }
                if(it->second == "0")
                {
                    ss << ".> " << v << "\n";
                }
                else
                {
                    //ss << ".[]> " << v << ", " << yylval.ival << "\n";

                    ss << ".[]> " << v << ", " << varidxlst.at(0) << "\n";
                    varidxlst.erase(varidxlst.begin());
                }
            }
            rwvarslst.clear();
        }
    | CONTINUE
        {
            //cout << "statement -> CONTINUE" << endl;
        }
    | RETURN expression
        {
            //cout << "statement -> RETURN expression" << endl;

            //output(yylval.ival); // might be return value?

            ss << "ret " << "\n";
        }
    ;

bool-expr
    : relation-and-expr
        {
            //cout << "bool_exp -> relation_and_exp" << endl;
        }
    | relation-and-expr OR relation-and-expr
        {
            //cout << "bool_exp -> relation_and_exp OR relation_and_exp" << endl;

            ss << "||" << "\n";
        }
    ;

relation-and-expr
    : relation-expr
        {
            //cout << "relation_and_exp -> relation_exp" << endl;
        }
    | relation-expr AND relation-and-expr
        {
            //cout << "relation_and_exp -> relation_exp AND relation_exp" << endl;

            ss << "&&" << "\n";
        }
    ;

relation-expr
    : expression comp expression
        {
            //cout << "relation_exp -> expression comp expression" << endl;
        }
    | TRUE
        {
            //cout << "relation_exp -> TRUE" << endl;
        }
    | FALSE
        {
            //cout << "relation_exp -> FALSE" << endl;
        }
    | L_PAREN bool-expr R_PAREN
        {
            //cout << "relation_exp -> L_PAREN bool-exp R_PAREN" << endl;
        }
    | NOT expression comp expression
        {
            //cout << "relation_exp -> NOT expression comp expression" << endl;
        }
    | NOT TRUE
        {
            //cout << "relation_exp -> NOT TRUE" << endl;
        }
    | NOT FALSE
        {
            //cout << "relation_exp -> NOT FALSE" << endl;
        }
    | NOT L_PAREN bool-expr R_PAREN
        {
            //cout << "relation_exp -> NOT L_PAREN bool-exp R_PAREN" << endl;
        }
    ;

comp
    : EQ
        {
            //cout << "comp -> EQ" << endl;

            ss << "==" << "\n";
        }
    | NEQ
        {
            //cout << "comp -> NEQ" << endl;

            ss << "!=" << "\n";
        }
    | GT
        {
            //cout << "comp -> GT" << endl;

            ss << ">" << "\n";
        }
    | LT
        {
            //cout << "comp -> LT" << endl;

            ss << "<" << "\n";
        }
    | GTE
        {
            //cout << "comp -> GTE" << endl;

            ss << ">=" << "\n";
        }
    | LTE
        {
            //cout << "comp -> LTE" << endl;

            ss << "<=" << "\n";
        }
    ;

expressions
    :   /* empty */
        {
            //cout << "expression -> epsilon" << endl;
        }
    | expression
        {
            //cout << "expressions -> expression" << endl;
        }
    | expression COMMA expressions
        {
            //cout << "expressions -> expression COMMA expressions" << endl;
        }
    ;

expression
    : mult-expr
        {
            //cout << "expression -> multiplicative_expression" << endl;
        }
    | mult-expr ADD expression
        {
            //cout << "expression -> multiplicative_expression ADD multiplicative_expression" << endl;

            ss << "+" << "\n";
        }
    | mult-expr SUB expression
        {
            //cout << "expression -> multiplicative_expression SUB multiplicative_expression" << endl;

            ss << "-" << "\n";
        }
    ;

mult-expr
    : term
        {
            //cout << "multiplicative_expression -> term" << endl;
        }
    | term MULT term
        {
            //cout << "multiplicative_expression -> term MULT term" << endl;

            ss << "*" << "\n";
        }
    | term DIV term
        {
            //cout << "multiplicative_expression -> term DIV term" << endl;

            ss << "/" << "\n";
        }
    | term MOD term
        {
            //cout << "multiplicative_expression -> term MOD term" << endl;

            ss << "%" << "\n";
        }
    ;

term
    : var
        {
            //cout << "term -> var" << endl;

            //rwvarslst.clear(); // comment out so assign works

            vtag = 1; // for assign src is ident
        }
    | NUMBER
        {
            //cout << "term -> NUMBER" << " " << yylval.ival << endl;

            //ss << yylval.ival << "\n";  // probably comment out eventually

            vtag = 0; // for the assign src is num
        }
    | L_PAREN expression R_PAREN
        {
            //cout << "term -> L_PAREN expression R_PAREN" << endl;
        }
    | identifierF L_PAREN expressions R_PAREN
        {
            //cout << "term -> identifiers L_PAREN expressions R_PAREN" << endl;

            ss << "param" << "\n";
            // expressions stored as params here
            ss << "call" << "\n";
            // identifier, stored expressions from param
        }
    | SUB var
        {
            //cout << "term -> SUB var" << endl;
        }
    | SUB NUMBER
        {
            //cout << "term -> SUB NUMBER" << " " << yylval.ival << endl;

            ss << (-1 * yylval.ival) << "\n";  // not sure...
        }
    | SUB L_PAREN expression R_PAREN
        {
            //cout << "term -> SUB L_PAREN expression R_PAREN" << endl;
        }
    | SUB identifierF L_PAREN expressions R_PAREN
        {
            //cout << "term -> SUB identifiers L_PAREN expressions R_PAREN" << endl;
        }
    ;

vars
    : var
        {
            //cout << "vars -> var" << endl;

            //output(*yylval.sval);
        }
    | var COMMA vars
        {
            //cout << "vars -> var COMMA vars" << endl;

            //output(*yylval.sval);
        }
    ;

var
    : identifier
        {
            //cout << "var -> ident" << endl;

            //output(*yylval.sval);

            rwvarslst.push_back(*yylval.sval);
        }
    | identifier L_SQUARE_BRACKET expression R_SQUARE_BRACKET
        {
            //cout << "var -> ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET" << endl;

            //output(idslst.at(idslst.size()-1)); // name
            //if(yylval.sval){output(*yylval.sval);}else{output(yylval.ival);} // index

            string a_id = idslst.at(idslst.size()-1);
            rwvarslst.push_back(a_id);

            if(vtag == 1) // id
            {
                varidxlst.push_back(*yylval.sval);
            }
            else // number
            {
                varidxlst.push_back(to_string(yylval.ival));
            }
        }
    ;


%%


    /*
    ----------------------------------------
    User subroutines
    ----------------------------------------
    */


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

void yyerror(const char *msg)
{
   yyerror(string(msg));
}

void yyerror(string s)
{
   cout << "parser yyerror at Line " << curline << ", position " << curpos << ": " << s << endl;
}


