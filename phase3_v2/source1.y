/* -*- c++ -*- */
/* source.y for Mini-L to MIL Fall 2018*/


// Everything from here to "%}" is copied verbatim to the top of source.tab.c
%{
#include "heading.H"
int yyerror( char* s );
int yylex( void );
int vectorSize;               // holds the size of vectors being declared
extern ostringstream rules;   // holds grammar rules printed out by actions
extern ostringstream decs;    // holds gen()-emitted target-code declarations
extern ostringstream code;    // holds gen()-emitted target-code instructions
extern ostringstream init;    // holds gen()-emitted target-code instructions
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
  %type   <code>     ScalarDecl
  %type   <code>     VectorDecl
  %type   <code>     StmtList
  %type   <code>     ExpList
  %type   <code>     FunctionDecl
  %type   <code>     BoolExp
  %type   <code>     Exp
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
%token     <junk>       ASSIGN
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
%token     <junk>       ASMT
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

Program            : /* EMPTY */    // Possibly empty list of FunctionDecls
                     { rules << "Program -> /* EMPTY */ \n"; }
                   | Program FunctionDecl       //
		     { rules << "Program -> ProgramFunctionDecl \n"; }
                   ;   

sVar               : ID                               // scalar variable. *
                     { rules << "sVar -> ID \n"; }
                   ;

vVar               : ID '[' Exp ']'       // vector/subscribted variable. *
                     { rules << "vVar -> ID '[' Exp ']' \n"; }
                   ;

StmtList           : Stmt ';'          // nonempty, semicolon terminated. *
                     { rules << "StmtList -> Stmt \n"; }
                   | StmtList Stmt ';'
                     { rules << "StmtList -> StmtList Stmt ';' \n"; }  
                   ;

ExpList            : /* EMPTY */      // possibly empty, comma separated. *
                     { rules << "ExpList -> /* EMPTY */ \n"; }                       
                   | ExpList ',' Exp        
                     { rules << "ExpList -> Explist ',' Exp \n"; }     
                   ;

FunctionDecl       : FUNCTION ID ';' BEGINPARAMS DeclList  ENDPARAMS
                                     BEGINLOCALS DeclList  ENDLOCALS
		                     BEGINBODY   StmtList  ENDBODY
                     { rules << "FunctionDecl -> FUNCTION ID ';' \n";
                       rules << "   BEGINPARAMS DeclList  ENDPARAMS \n"; // scalars
                       rules << "   BEGINLOCALS DeclList  ENDLOCALS \n";
                       rules << "   BEGINBODY   StmtList  ENDBODY \n";
                     }

ScalarDecl         : ID ':' INTEGER             // record in symbol table *
                     { rules << "ScalarDecl -> ID ':' INTEGER \n"; 
                       code << ". " << *($1) << "\n";
                     }
                   | ID ',' ScalarDecl                 // right recursion *
		     { rules << "ScalarDecl -> ID ',' ScalarDecl \n";
		       code << ". " <<  *($1) << "\n";
		     }  
                   ;

VectorDecl         : ID ':' ARRAY '[' NUMBER ']' OF INTEGER            // *
                     { rules << "VectorDecl -> ID ':' ARRAY '[' NUMBER ']' OF INTEGER \n"; 
                       vectorSize = $5; code  << ".[] "
                       << *($1) << ", " << vectorSize << "\n";
		     }
                   | ID ',' VectorDecl                 // right recursion *
    		     { rules << "VectorDec. ->  ID ',' VectorDecl \n"; 
                       code  << ".[] " << *($1) << ", " << vectorSize << "\n";
		     }
                   ; // vectorSize is global declared at the top of main.cc

DeclList           : /* EMPTY */   // possibly empty, semicolon terminated.
                     { rules << "DeclList -> EMPTY\n"; } 
                   | DeclList ScalarDecl ';'            // left recursion *
                     { rules << "DeclList -> DeclList ScalarDecl ';' \n"; }
                   | DeclList VectorDecl ';'            // left recursion *
                     { rules << "DeclList -> DeclList VectorDecl ';' \n"; }
                   ;

BoolExp            : TRUE                   
                     { rules << " TRUE \n"; }
                   | FALSE                  
                     { rules << " FALSE \n"; }                    
                   | '(' BoolExp ')'
		     { rules << " '(' BoolExp ')' \n"; }     
                   | NOT BoolExp
		     { rules << " NOT BoolExp \n"; }
                   | BoolExp AND BoolExp    
                     { rules << " BoolExp AND BoolExp \n"; }
                   | BoolExp OR BoolExp     
                     { rules << " BoolExp OR BoolExp \n"; }
                   | Exp EQ Exp           
                     { rules << " Exp EQ Exp \n"; }
                   | Exp NE Exp           
                     { rules << " Exp NE Exp \n"; }
                   | Exp GE Exp           
                     { rules << " Exp GE Exp \n"; }
                   | Exp LE Exp           
                     { rules << " Exp LE Exp \n"; }
                   | Exp '>' Exp           
                     { rules << " Exp '>' Exp \n"; }
                   | Exp '<' Exp           
                     { rules << " Exp '<' Exp \n"; }
                   ;

Exp                : sVar                              // scalar variable *
                     { rules << "Exp -> sVar\n"; }
                   | vVar                 //  vector/subscripted variable *
                     { rules << "Exp -> vVar\n"; }
                   | Exp '+' Exp	    
                     { rules << "Exp -> Exp '+' Exp\n"; }
                   | Exp '-' Exp	    
                     { rules << "Exp -> Exp '-' Exp\n"; }
                   | Exp '*' Exp	    
                     { rules << "Exp -> Exp '*' Exp\n"; }
                   |  Exp '/' Exp	    
                     { rules << "Exp -> Exp '/' Exp\n"; }
                   | Exp '%' Exp	    
  		     { rules << "Exp -> Exp '%' Exp\n"; }
                   | '-' Exp  %prec '('     
                     { rules << "Exp -> '-' Exp\n"; }
                   | NUMBER                 
                     { rules << "Exp -> NUMBER\n"; }
                   | '(' Exp ')'             
                     { rules << "Exp -> '(' Exp ')' \n"; }
                   | ID '(' ExpList ')'      // function call
                     { rules << "Exp -> ID '(' Exp ')' \n"; }
	           ;

ReadStmt           : READ sVar                                          // *
                   | READ vVar                                          // *
                   | ReadStmt ',' sVar                   // left recursion *
                   | ReadStmt ',' vVar                   // left recursion *
                   ;

WriteStmt          : WRITE Exp                                          // *
                   | WriteStmt ',' Exp                   // left recursion *
                   ;
			  
Stmt               : sVar ASMT Exp  // The desination can be either scalar *
                     { rules << "Stmt -> sVar ASMT Exp\n"; }
                   | vVar ASMT Exp                       // or subscripted *
		     { rules << "Stmt -> vVar ASMT Exp\n"; }
                   | ReadStmt                                 // See above *
		     { rules << "Stmt -> ReadStmt" ; }                  // *
                   | WriteStmt                                // see above *
		     { rules << "Stmt -> WriteStmt" ; }                 // *
                   | IF BoolExp THEN StmtList ENDIF
		     { rules << "Stmt -> IF BoolExp THEN StmtList ENDIF\n"; }  
                   | IF BoolExp THEN StmtList ELSE StmtList ENDIF
		     { rules << "Stmt -> IF BoolExp THEN StmtList ELSE StmtList ENDIF\n";
		     }
                   | WHILE BoolExp BEGINLOOP StmtList ENDLOOP
		     { rules << "Stmt -> WHILE BoolExp BEGINLOOP StmtList ENDLOOP\n";
		     }
                   | DO BEGINLOOP StmtList ENDLOOP WHILE BoolExp
		     { rules << "Stmt -> DO BEGINLOOP StmtList ENDLOOP ";
		       rules << "WHILE BoolExp\n";
		     }
                   | CONTINUE
		     { rules << "Stmt -> CONTINUE\n"; }
                   | RETURN Exp
		     { rules << "Stmt -> RETURN Exp\n"; }
                   ;
%%


// Here is the function that reports lexical and parse errors.  It is
// overloaded so that error messages can be either C strings or C++
// strings.

int yyerror( string s ) {  // error handler routine
  extern int  yylineno;    // defined and maintained in lex.c
  extern char* yytext;     // defined and maintained in lex.c
  cerr << "ERROR: " << s << " at symbol " << yytext;
  cerr << " on line " << yylineno << endl;
  exit( 1 );
}

int yyerror( char* s ) { return yyerror( string(s) ); }




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

