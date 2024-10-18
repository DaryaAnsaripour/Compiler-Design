grammar FunctionCraft;

program: fc;

// -----------------------------Program-----------------------------
fc:
    NEWLINE* (function_decleration | pattern_decleration)* main;


// -----------------------------Main function-----------------------------
main:
    DEF MAIN {System.out.println("MAIN");}
     LPAR RPAR NEWLINE  (statement|NEWLINE)* END  NEWLINE*;


// -----------------------------Statements-----------------------------
statement:
    conditional_statement |   loop_statement   | function_call_statement eol | assign_statement eol      |
    puts         eol         |        push   eol     |           len     eol      |          chop     eol     |
    chomp          eol       |  append_statement eol |      return_statement eol  | pattern_call_statement eol |
    lambda_function_statement eol;


// -----------------------------Return statement-----------------------------
return_statement:
    RETURN {System.out.println("RETURN");}
     ( (IDENTIFIER | value | list | expression | predefined_statement | func_ptr))?;


// -----------------------------Function decleration-----------------------------
function_decleration :
    DEF
    name = IDENTIFIER {System.out.println("FuncDec: " + $name.text);}
     LPAR parameters RPAR NEWLINE function_body END  NEWLINE*;

parameters:
     (nodefault | defaultonly | both | ) ;

nodefault:
    (IDENTIFIER  COMMA NEWLINE? )* (IDENTIFIER) ;

defaultonly:
    (LBRACK  (IDENTIFIER  ASSIGN value  COMMA NEWLINE? )*
     IDENTIFIER  ASSIGN value  RBRACK);

both:
    nodefault  COMMA NEWLINE?  defaultonly;

function_body:
    (statement | NEWLINE)*;

// -----------------------------Pattern matching-----------------------------
pattern_decleration:
    PATTERN
    name = IDENTIFIER {System.out.println("PatternDec: " + $name.text);}
    LPAR parameters RPAR NEWLINE (CASE  condition  ASSIGN (expression|list) NEWLINE)* eol NEWLINE*;


pattern_call_statement:
    IDENTIFIER DOT MATCH (LPAR RPAR | LPAR (expression | list) (COMMA (expression | list))* RPAR ) {System.out.println("Built-In: MATCH");} ;


// -----------------------------Lambda function-----------------------------

lambda_function_statement:
    ARROW {System.out.println("Structure: LAMBDA");}
      LPAR parameters RPAR  LBRACE  return_statement  SEMICOLON  RBRACE  LPAR (expression | list) (COMMA NEWLINE? (expression | list))* RPAR;

lambda_function_expression:
    ARROW {System.out.println("Structure: LAMBDA");}
      LPAR parameters RPAR  LBRACE  return_statement  SEMICOLON  RBRACE;



// -----------------------------Base expressions-----------------------------
expression:
    LPAR expression RPAR | list_index | expression name = (PLUSPLUS | MINUSMINUS) {System.out.println("Operator: "+ $name.text);}
    | name =  (NOT | MINUS) expression {System.out.println("Operator: "+ $name.text);}
    | expression name = (DIV | MULT | MOD) expression {System.out.println("Operator: "+ $name.text);}
    | expression name = (PLUS | MINUS) expression {System.out.println("Operator: "+ $name.text);}
    | expression op_name = bool_op expression {System.out.println("Operator: "+ $op_name.text);}
    | expression op_name2 = eq_noteq expression {System.out.println("Operator: "+ $op_name2.text);}
    | expression AND expression {System.out.println("Operator: &&");}
    | expression OR expression {System.out.println("Operator: ||");}
    | IDENTIFIER | value | function_call_statement | pattern_call_statement
    | lambda_function_expression | lambda_function_statement;



// -----------------------------Conditional statements-----------------------------
arithmetic:
      LPAR arithmetic RPAR | list_index | arithmetic name = (PLUSPLUS | MINUSMINUS) {System.out.println("Operator: "+ $name.text);}
    | MINUS arithmetic {System.out.println("Operator: -");}
    | arithmetic name = (DIV | MULT | MOD) arithmetic {System.out.println("Operator: "+ $name.text);}
    | arithmetic name = (PLUS | MINUS) arithmetic {System.out.println("Operator: "+ $name.text);}
    | arithmetic op_name = bool_op arithmetic {System.out.println("Operator: "+ $op_name.text);}
    | arithmetic op_name2 = eq_noteq arithmetic {System.out.println("Operator: "+ $op_name2.text);}
    | IDENTIFIER | value | function_call_statement | pattern_call_statement
    | lambda_function_expression | lambda_function_statement;

condition : LPAR condition op_name = bool_op condition RPAR {System.out.println("Operator: "+ $op_name.text);}
            | LPAR condition op_name2 = eq_noteq condition RPAR {System.out.println("Operator: "+ $op_name2.text);} |
            | LPAR condition AND condition RPAR {System.out.println("Operator: &&");}
            | LPAR condition OR condition RPAR {System.out.println("Operator: ||");}
            | LPAR NOT LPAR condition RPAR RPAR {System.out.println("Operator: !");}
            | LPAR condition RPAR | NOT arithmetic {System.out.println("Operator: !");}
            | arithmetic;


conditional_statement:
     IF {System.out.println("Decision: IF");}
     condition NEWLINE+ (statement | NEWLINE)* elseif_statement* else_statement? END NEWLINE;

elseif_statement:
    ELSEIF {System.out.println("Decision: ELSE IF");}
      condition NEWLINE+ (statement)+;

else_statement:
    ELSE {System.out.println("Decision: ELSE");}
    NEWLINE+ (statement)+;

// -----------------------------Loop statements-----------------------------
next_statement:
    NEXT {System.out.println("Control: NEXT");}
    ;

nextif_statement:
    NEXTIF {System.out.println("Control: NEXT");}
     condition;

break_statement:
    BREAK {System.out.println("Control: BREAK");}
   ;

breakif_statement:
    BREAKIF {System.out.println("Control: BREAK");}
    condition;

loop_statement:
    loopdo_statement | for_statement;

loop_body_statement:
    statement | next_statement eol | nextif_statement eol | break_statement eol | breakif_statement eol;

loopdo_statement:
    LOOP  DO {System.out.println("Loop: DO");}
    NEWLINE+ ( loop_body_statement)* END;

for_statement:
    FOR {System.out.println("Loop: FOR");}
     IDENTIFIER  IN (range | IDENTIFIER | list) NEWLINE (loop_body_statement)* END;


// -----------------------------Function call statements-----------------------------
function_call_statement:
      IDENTIFIER {System.out.println("FunctionCall");}
    LPAR RPAR
    | IDENTIFIER {System.out.println("FunctionCall");}
    LPAR (expression | list | append_statement) (COMMA NEWLINE? (expression | list | append_statement))* RPAR;

func_ptr:
    METHOD LPAR COLON IDENTIFIER RPAR;


// -----------------------------Variable declaration-----------------------------
assign_statement:
    name = IDENTIFIER
    ((assign_op  (expression| list | predefined_statement | func_ptr))
    | MINUSMINUS | PLUSPLUS) {System.out.println("Assignment: " + $name.text);};


// -----------------------------Predefined functions-----------------------------
predefined_statement:
    push | len | chop | chomp | append_statement;

puts:
    PUTS {System.out.println("Built-In: PUTS");}
     LPAR (expression | list |append_statement | chomp | chop | len) RPAR;

push:
    PUSH {System.out.println("Built-In: PUSH");}
    LPAR (expression | chop | chomp | list) COMMA expression RPAR;

len:
    LEN {System.out.println("Built-In: LEN");}
    LPAR (expression | chop | chomp | list) RPAR;

chop:
    CHOP {System.out.println("Built-In: CHOP");}
    LPAR (expression | chop | chomp) RPAR;

chomp:
    CHOMP {System.out.println("Built-In: CHOMP");}
    LPAR (expression| chop | chomp) RPAR;

append_statement:
    append_list | append_string;
append_list:
    (list | IDENTIFIER | list_index)  ( APPEND (list | expression) {System.out.println("Operator:<<");} )+ ;
append_string:
    (STRING_LITERAL | list_index)  (APPEND {System.out.println("Operator: <<");} (STRING_LITERAL | function_call_statement | chop | chomp | IDENTIFIER)  )+;



eol:
    SEMICOLON NEWLINE;

list:       // Currently, the type of the elements stored in the list is not our problem ...
    LBRACK RBRACK | LBRACK (list | expression) (COMMA NEWLINE? (list | expression))* RBRACK;
list_index:
    (list | IDENTIFIER) (LBRACK expression RBRACK)+;
range:
    list | LPAR (expression)  DOT DOT  (expression) RPAR;









/*---------------------------Tokens---------------------------*/

// Keywords
DEF: 'def';
END: 'end';
RETURN: 'return';


// Conditionals
IF: 'if';
ELSE: 'else';
ELSEIF: 'elseif';
METHOD: 'method';


// Loops
DO: 'do';
NEXT: 'next';
NEXTIF: 'next if';
BREAK: 'break';
BREAKIF: 'break if';
LOOP: 'loop';
FOR: 'for';
IN: 'in';


// Predefined functions
CHOP: 'chop';
CHOMP: 'chomp';
PUSH: 'push';
PUTS: 'puts';
LEN: 'len';
MAIN: 'main';
PATTERN: 'pattern';
MATCH: 'match';


// Initial types
INT: 'int';
FLOAT: 'float';
STRING: 'string';
BOOL: 'bool';
LIST: 'list';
//FPTR: 'fptr';


// Symbols
LPAR: '(';
RPAR: ')';
LBRACK: '[';
RBRACK: ']';
LBRACE: '{';
RBRACE: '}';
SHARP: '#';
COMMA: ',';
DOT: '.';
SEMICOLON: ';';
COLON: ':';
ARROW: '->';
DOUBLEQUOTE: '"';
APPEND: '<<';
CASE: '    |';

// Operators (Arithmetical and Logical)
PLUS: '+';
MINUS: '-';
MULT: '*';
DIV: '/';
MOD: '%';
MINUSMINUS: '--';
PLUSPLUS: '++';
EQUAL: '==';
NOTEQ: '!=';
GREATER_THAN: '>';
GREATER_THAN_EQUAL: '>=';
LESS_THAN: '<';
LESS_THAN_EQUAL: '<=';
bool_op: GREATER_THAN | GREATER_THAN_EQUAL | LESS_THAN | LESS_THAN_EQUAL ;
eq_noteq: EQUAL | NOTEQ;

//   name = IDENTIFIER {System.out.println("Assignment: " + $name.text);}


AND: '&&';
OR: '||';
NOT: '!';
ASSIGN: '=';
PLUSEQ: '+=';
MINUSEQ: '-=';
MULEQ: '*=';
DIVEQ: '/=';
MODEQ: '%=';
assign_op: ASSIGN | PLUSEQ | MINUSEQ | MULEQ | DIVEQ | MODEQ;


// Typedefs
BOOL_VALUE: 'true' | 'false';
INTEGER: (NONZERODIGIT DIGIT*) | [0];
FLOAT_VALUE: INTEGER DOT INTEGER;
STRING_LITERAL: '"' ( ~["\\\r\n] | '\\' . )* '"';
// FUNCPTR: IDENTIFIER  ASSIGN   METHOD LPAR COLON IDENTIFIER RPAR SEMICOLON;
IDENTIFIER: [a-zA-Z_][a-zA-Z_0-9]*;
//(LETTER | UNDERSCORE) (LETTER | UNDERSCORE | DIGIT)*;
value: (INTEGER | STRING_LITERAL | BOOL_VALUE | FLOAT_VALUE);


// Helper
DIGIT: [0-9];
NONZERODIGIT: [1-9];
LETTER: [a-zA-Z];
UNDERSCORE: '_';

// Whitespace and comment
COMMENT: (('=begin' .*? '=end') NEWLINE* | ('#' (~[\r\n]*) NEWLINE)) -> skip;
WS: (' ' | '\t') -> skip;
NEWLINE: ('\n' | '\r' | '\r\n');