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

// data structures grouped by use

int lblnum = 0;           // # of labels counter
int tmpnum = 0;           // # of tmp vars
string lastlbl;
string lasttmp;
string lblmkr();          // produce new label name
string tmpmkr();          // produce new tmp var name

stringstream ss;          // generate code to string stream as parses
string buf;               // buffer to read from string stream
string funs;              // write out function from string stream
vector<string> funslst;   // all the compiled functions code

int vtag;                 // in assign flag for choosing: 0 =  num, 1 = str
int neg;                  // in assign to tell if negative number
vector<string> rwvarslst; // var names in read,write,assign
vector<string> varidxlst; // array index

int fcnt = 0;             // function counter for string stream
string fid;               // function name for call

int pcnt = 0;             // param counter for ss
int pnum = 0;             // number of params

vector<string> idslst;    // idents for symbol table

int maths;              // is in math expression
vector<string> expvec;  // expressions

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
    : FUNCTION identifierF SEMICOLON BEGIN_PARAMS declarationsP END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY
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
            vtag = -1;
            fcnt = 0;
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

declarationsP
    : /* epsilon */
        {
            //cout << "declarations -> epsilon" << endl;

            pcnt = 0; // done in declarationsP
            pnum = 0; // reset counter
        }
    | declaration SEMICOLON declarationsP
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

                if(pcnt == 1) // output the assignment of positional arguments to parameters
                {
                    ss << "= " << id << ", $" << pnum << "\n";
                    pnum++;
                }
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

            fid = *yylval.sval; // save function name for the call instruction

            if(fcnt == 0) // output for the first line of code func name
            {
                ss << *yylval.sval << "\n";
                fcnt++;

                pcnt = 1; // set for declarationsP
            }
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

            // useful later probably
            /*output("id");
            outarr(idslst);
            output("");
            output("var");
            outarr(rwvarslst);
            output("");
            output("index");
            outarr(varidxlst);
            output("");

            outarr(expvec);
            outarr(rwvarslst);
            output(lasttmp);*/

            map<string,string>::iterator it = symtab.find(rwvarslst.at(0));
            if(it == symtab.end()){ yyerror("error: in assign null var encountered"); }

            if(it->second == "0") // scalar
            {
                ss << "= " << rwvarslst.at(0) << ", ";

                if(vtag == 0) // was a number
                {
                    if(maths == 1 || neg == 1) // math expressions
                    {
                        ss << lasttmp << "\n"; // int = expr
                    }
                    else
                    {
                        ss << yylval.ival << "\n";  // int = num
                    }
                }
                else // need to check yylval.sval is int or arr
                {
                    if(maths == 1) // math expressions
                    {
                        ss << lasttmp << "\n";
                    }
                    else
                    {
                        ss << *yylval.sval << "\n";  // int = int
                    }

                    // int = arr
                    // fixme
                }
            }
            else // array
            {
                ss << "[]= " << rwvarslst.at(0) << ", " << varidxlst.at(0) << ", ";

                if(vtag == 0) // was a number
                {
                    if(maths == 1 || neg == 1) // math expressions
                    {
                        ss << lasttmp << "\n"; // arr = expr
                    }
                    else
                    {
                        ss << yylval.ival << "\n";  // arr = num
                    }
                }
                else // need to check yylval.sval is int or arr
                {
                    if(maths == 1) // math expressions
                    {
                        ss << lasttmp << "\n";
                    }
                    else
                    {
                        ss << *yylval.sval << "\n";  // arr = int
                    }

                    // arr = arr
                    // fixme
                }
                varidxlst.erase(varidxlst.begin());
            }

            maths = 0;         // done
            neg = 0;

            vtag = -1;         // reset var tag
            rwvarslst.clear(); // remove var from list
        }
    | IF bool-expr THEN statements ENDIF
        {
            //cout << "statement -> IF bool_exp THEN statements ENDIF" << endl;

            string lbl_false = lblmkr();
            string lbl_true = lblmkr();
            string lbl_end = lblmkr();

            ss << "?:= " << lbl_true << ", " << lasttmp << "\n";
            ss << ": " << lbl_false << "\n";
                ss << ":= " << lbl_end << "\n";
            ss << ": " << lbl_true << "\n";
                //ss << /* statements */ << "\n";
            ss << ": " << lbl_end << "\n";
        }
    | IF bool-expr THEN statements ELSE statements ENDIF
        {
            //cout << "statement -> IF bool_exp THEN statements ELSE statements ENDIF" << endl;

            string lbl_false = lblmkr();
            string lbl_true = lblmkr();
            string lbl_end = lblmkr();

            ss << "?:= " << lbl_true << ", " << lasttmp << "\n";
            ss << ": " << lbl_false << "\n";
                //ss << /* statements */ << "\n";
                ss << ":= " << lbl_end << "\n";
            ss << ": " << lbl_true << "\n";
                //ss << /* statements */ << "\n";
            ss << ": " << lbl_end << "\n";
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
    : TRUE
        {
            //cout << "relation_exp -> TRUE" << endl;

            vtag = 0; // for the assign src is num

            expvec.push_back(to_string(1));
        }
    | FALSE
        {
            //cout << "relation_exp -> FALSE" << endl;

            vtag = 0; // for the assign src is num

            expvec.push_back(to_string(0));
        }
    | L_PAREN bool-expr R_PAREN
        {
            //cout << "relation_exp -> L_PAREN bool-exp R_PAREN" << endl;
        }
    | NOT bool-expr
        {
            //cout << "relation_exp -> NOT expression comp expression" << endl;
        }
    | bool-expr AND bool-expr
        {
            //cout << "relation_and_exp -> relation_exp AND relation_exp" << endl;

            ss << "&&" << "\n";
        }
    | bool-expr OR bool-expr
        {
            //cout << "bool_exp -> relation_and_exp OR relation_and_exp" << endl;

            ss << "||" << "\n";
        }
    | expression EQ expression
        {
            //cout << "comp -> EQ" << endl;

            ss << "==" << "\n";
        }
    | expression NEQ expression
        {
            //cout << "comp -> NEQ" << endl;

            ss << "!=" << "\n";
        }
    | expression GT expression
        {
            //cout << "comp -> GT" << endl;

            ss << ">" << "\n";
        }
    | expression LT expression
        {
            //cout << "comp -> LT" << endl;

            ss << "<" << "\n";
        }
    | expression GTE expression
        {
            //cout << "comp -> GTE" << endl;

            ss << ">=" << "\n";
        }
    | expression LTE expression
        {
            //cout << "comp -> LTE" << endl;

            //ss << "<=" << "\n";

            //outarr(expvec);

            maths = 1;

            string rhs = tmpmkr(); // result
            ss << ". " << rhs << "\n";
            ss << "= " << rhs << ", " << expvec.at(expvec.size()-1) << "\n";
            expvec.pop_back();

            string lhs = tmpmkr(); // result
            ss << ". " << lhs << "\n";
            ss << "= " << lhs << ", " << expvec.at(expvec.size()-1) << "\n";
            expvec.pop_back();

            string res = tmpmkr(); // result
            ss << ". " << res << "\n";
            ss << "<= " << res << ", " << lhs << ", " << rhs << "\n";

            expvec.push_back(res);

            //outarr(expvec);

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
    : term
        {
            //cout << "multiplicative_expression -> term" << endl;
        }
    | term MULT expression
        {
            //cout << "multiplicative_expression -> term MULT term" << endl;

            //outarr(expvec);

            maths = 1;

            string rhs = tmpmkr(); // result
            ss << ". " << rhs << "\n";
            ss << "= " << rhs << ", " << expvec.at(expvec.size()-1) << "\n";
            expvec.pop_back();

            string lhs = tmpmkr(); // result
            ss << ". " << lhs << "\n";
            ss << "= " << lhs << ", " << expvec.at(expvec.size()-1) << "\n";
            expvec.pop_back();

            string res = tmpmkr(); // result
            ss << ". " << res << "\n";
            ss << "* " << res << ", " << lhs << ", " << rhs << "\n";

            expvec.push_back(res);

            //outarr(expvec);
        }
    | term DIV expression
        {
            //cout << "multiplicative_expression -> term DIV term" << endl;

            //outarr(expvec);

            maths = 1;

            string rhs = tmpmkr(); // result
            ss << ". " << rhs << "\n";
            ss << "= " << rhs << ", " << expvec.at(expvec.size()-1) << "\n";
            expvec.pop_back();

            string lhs = tmpmkr(); // result
            ss << ". " << lhs << "\n";
            ss << "= " << lhs << ", " << expvec.at(expvec.size()-1) << "\n";
            expvec.pop_back();

            string res = tmpmkr(); // result
            ss << ". " << res << "\n";
            ss << "/ " << res << ", " << lhs << ", " << rhs << "\n";

            expvec.push_back(res);

            //outarr(expvec);
        }
    | term MOD expression
        {
            //cout << "multiplicative_expression -> term MOD term" << endl;

            //outarr(expvec);

            maths = 1;

            string rhs = tmpmkr(); // result
            ss << ". " << rhs << "\n";
            ss << "= " << rhs << ", " << expvec.at(expvec.size()-1) << "\n";
            expvec.pop_back();

            string lhs = tmpmkr(); // result
            ss << ". " << lhs << "\n";
            ss << "= " << lhs << ", " << expvec.at(expvec.size()-1) << "\n";
            expvec.pop_back();

            string res = tmpmkr(); // result
            ss << ". " << res << "\n";
            ss << "% " << res << ", " << lhs << ", " << rhs << "\n";

            expvec.push_back(res);

            //outarr(expvec);
        }
    | term ADD expression
        {
            //cout << "expression -> multiplicative_expression ADD multiplicative_expression" << endl;

            //outarr(expvec);

            maths = 1;

            string rhs = tmpmkr(); // result
            ss << ". " << rhs << "\n";
            ss << "= " << rhs << ", " << expvec.at(expvec.size()-1) << "\n";
            expvec.pop_back();

            string lhs = tmpmkr(); // result
            ss << ". " << lhs << "\n";
            ss << "= " << lhs << ", " << expvec.at(expvec.size()-1) << "\n";
            expvec.pop_back();

            string res = tmpmkr(); // result
            ss << ". " << res << "\n";
            ss << "+ " << res << ", " << lhs << ", " << rhs << "\n";

            expvec.push_back(res);

            //outarr(expvec);
        }
    | term SUB expression
        {
            //cout << "expression -> multiplicative_expression SUB multiplicative_expression" << endl;

            //outarr(expvec);

            maths = 1;

            string rhs = tmpmkr(); // result
            ss << ". " << rhs << "\n";
            ss << "= " << rhs << ", " << expvec.at(expvec.size()-1) << "\n";
            expvec.pop_back();

            string lhs = tmpmkr(); // result
            ss << ". " << lhs << "\n";
            ss << "= " << lhs << ", " << expvec.at(expvec.size()-1) << "\n";
            expvec.pop_back();

            string res = tmpmkr(); // result
            ss << ". " << res << "\n";
            ss << "- " << res << ", " << lhs << ", " << rhs << "\n";

            expvec.push_back(res);

            //outarr(expvec);
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

            expvec.push_back(to_string(yylval.ival));
        }
    | L_PAREN expression R_PAREN
        {
            //cout << "term -> L_PAREN expression R_PAREN" << endl;
        }
    | identifierF L_PAREN expressions R_PAREN
        {
            //cout << "term -> identifiers L_PAREN expressions R_PAREN" << endl;

            ss << "param" << "\n";
            // output params probably in a loop

            string t_dst = tmpmkr();    // output a temp variable for the destination
            ss << ". " << t_dst << "\n";
            ss << "call " << fid << ", " << t_dst << "\n";
        }
    | SUB var
        {
            //cout << "term -> SUB var" << endl;
        }
    | SUB NUMBER
        {
            //cout << "term -> SUB NUMBER" << " " << yylval.ival << endl;

            // ss << (-1 * yylval.ival) << "\n";  // not sure...

            vtag = 0; // for the assign src is num

            neg = 1; // for assign to know if neg

            string neg_s = tmpmkr();

            ss << ". " << neg_s << "\n";
            ss << "- " << neg_s << ", 0, " << yylval.ival << "\n";

            expvec.push_back(neg_s);
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

            expvec.push_back(*yylval.sval);
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

string lblmkr()
{
    string s = "_label";
    s += to_string(lblnum);
    lblnum++;
    lastlbl = s;
    return s;
}

string tmpmkr()
{
    string s = "_temp";
    s += to_string(tmpnum);
    tmpnum++;
    lasttmp = s;
    return s;
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


