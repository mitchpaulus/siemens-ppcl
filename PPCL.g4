grammar PPCL;

LPAREN : '(';
RPAREN : ')';
COMMA : ',';
EQUAL_SIGN : '=' ;

MINUS : '-';
PLUS : '+';

program : line* EOF;

line : POS_INT (sample? statement NL | COMMENT);

// I have seen a lowercase 'c' in the wild. Could not find official documentation about case sensitivity in PPCL.
// I have also seen the case where a line starts with C$LOC... and apparently parses?
COMMENT : ('C' | 'c') (' ' | '\t' | '$') .*? '\r'? '\n' | ('C' | 'c') '\r'? '\n' ;

NL : '\r'?'\n' ;

fragment UPPERALPHANUM : [A-Z0-9] ;

ACT    : 'ACT'    ;
DBSWIT : 'DBSWIT' ;
DEACT  : 'DEACT'  ;
DISALM : 'DISALM' ;
ENALM  : 'ENALM'  ;
HLIMIT : 'HLIMIT' ;
LLIMIT : 'LLIMIT' ;
LOCAL  : 'LOCAL'  ;
GOSUB  : 'GOSUB'  ;

SAMPLE : 'SAMPLE' ;
sample : SAMPLE LPAREN (POS_INT | POINT | LOCALVAR) RPAREN ;

TIMAVG : 'TIMAVG' ;
WAIT : 'WAIT' ;

TABLE : 'TABLE' ;
IF    : 'IF'    ;
THEN  : 'THEN'  ;
ELSE  : 'ELSE'  ;

POS_INT : [0-9]+ ;
NEG_INT : '-' [0-9]+ ;
integer : NEG_INT | POS_INT ;
MILITARY_TIME : [0-9][0-9]?':'[0-9][0-9];

LOCALVAR : '$LOC' ('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '10' | '11' | '12' | '13' | '14' | '15') ;

DECIMAL : '-'? [0-9]+ '.' [0-9]+ ;

LOOP : 'LOOP' ;
GOTO : 'GOTO' ;
RETURN : 'RETURN' ;

SET : 'SET' ;

// Resident Points
ALMCNT          : 'ALMCNT'           ;
ALMCT2          : 'ALMCT2'           ;
BATT            : '$BATT'            ;
CRTIME          : 'CRTIME'           ;
DAY             : 'DAY'              ;
DAYOFM          : 'DAYOFM'           ;
LINK            : 'LINK'             ;
MONTH           : 'MONTH'            ;
NODENUM         : 'NODE' [0-9][0-9]? ;
PDL_MONITOR     : '$PDL'             ;
SECNDS          : 'SECNDS'           ;
SECONDS_COUNTER : 'SECOND' [1-7]     ;
TIME            : 'TIME'             ;

residentPoint : ALMCNT | ALMCT2 | BATT | CRTIME | DAY | DAYOFM | LINK | MONTH | NODENUM | PDL_MONITOR | SECNDS | SECONDS_COUNTER | TIME ;

// At (@) Priority Status Indicators
EMER  : '@EMER'  ;
NONE  : '@NONE'  ;
OPER  : '@OPER'  ;
PDL   : '@PDL'   ;
SMOKE : '@SMOKE' ;

atPriorityStatusIndicator : EMER | NONE | OPER | PDL | SMOKE ;

// Point Status Indicators
ALARM  : 'ALARM'  ;
ALMACK : 'ALMACK' ;
AUTO   : 'AUTO'   ;
DEAD   : 'DEAD'   ;
LOW    : 'LOW'    ;
OK     : 'OK'     ;
DAYMOD : 'DAYMOD' ;
FAILED : 'FAILED' ;
FAST   : 'FAST'   ;
HAND   : 'HAND'   ;
NGTMOD : 'NGTMOD' ;
OFF    : 'OFF'    ;
ON     : 'ON'     ;
PRFON  : 'PRFON'  ;
SLOW   : 'SLOW'   ;

pointStatusIndicator : ALARM | ALMACK | AUTO | DEAD | LOW | OK | DAYMOD | FAILED | FAST | HAND | NGTMOD | OFF | ON | PRFON | SLOW ;


// See pg. 1-45 (p. 32) Table 1-5 for order of precedence.
ALARMPRI : 'ALARMPRI' ;

ATN   : 'ATN'   ;
COM   : 'COM'   ;
COS   : 'COS'   ;
EXP   : 'EXP'   ;
LOG   : 'LOG'   ;
SIN   : 'SIN'   ;
SQRT  : 'SQRT'  ;
TAN   : 'TAN'   ;
TOTAL : 'TOTAL' ;


ROOT : '.ROOT.' ;

EQUAL_TO                 : '.EQ.' ;
NOT_EQUAL_TO             : '.NE.' ;
LESS_THAN                : '.LT.' ;
LESS_THAN_OR_EQUAL_TO    : '.LE.' ;
GREATER_THAN             : '.GT.' ;
GREATER_THAN_OR_EQUAL_TO : '.GE.' ;

AND  : '.AND.'  ;
NAND : '.NAND.' ;

OR  : '.OR.'  ;
XOR : '.XOR.' ;

ONPWRT : 'ONPWRT' ;
RELEAS : 'RELEAS' ;
DEFINE : 'DEFINE' ;
INITTO : 'INITTO' ;

MIN : 'MIN' ;
MAX : 'MAX' ;

// Notes from manual:
// Defining Points in a PPCL Program
// • All physical and virtual points used in the program must be defined in the point database.
// • Point names that begin with numbers must be prefixed with the at (@) character.
// • Point names that are greater than 6 characters must be enclosed in double quotes.
// • Point names that use characters other than A-Z or 0-9 must be enclosed in double quotes.

// Local points defined with LOCAL can be referenced without quotes if desired.
POINT : '"' .*? '"' |
        '$'? [A-Z] |
        '$'? [A-Z] UPPERALPHANUM |
        '$'? [A-Z] UPPERALPHANUM UPPERALPHANUM |
        '$'? [A-Z] UPPERALPHANUM UPPERALPHANUM UPPERALPHANUM |
        '$'? [A-Z] UPPERALPHANUM UPPERALPHANUM UPPERALPHANUM UPPERALPHANUM |
        '$'? [A-Z] UPPERALPHANUM UPPERALPHANUM UPPERALPHANUM UPPERALPHANUM UPPERALPHANUM ;

WS : [ \t]+ -> skip ;

arithmetic_function : ATN | COM | COS | EXP | LOG | SIN | SQRT | TAN ;

statement
    : assignmentStatement
    | actStatement
    | dbswitStatement
    | deactStatement
    | defineStatement
    | disalmStatement
    | enalmStatement
    | gosubStatement
    | gotoStatement
    | hlimitStatement
    | ifStatement
    | inittoStatement
    | llimitStatement
    | localStatement
    | loopStatement
    | maxStatement
    | minStatement
    | offStatement
    | onStatement
    | onpwrtStatement
    | releasStatement
    | returnStatement
    | setStatement
    | tableStatement
    | timAvgStatement
    | waitStatement
    ;

actStatement : ACT LPAREN POS_INT (COMMA POS_INT)* RPAREN ;

loopStatement : LOOP LPAREN
                POS_INT   COMMA // Type
                (POINT | LOCALVAR) COMMA // Process Variable
                (POINT | LOCALVAR) COMMA // Control Variable
                (POINT | DECIMAL | integer | LOCALVAR) COMMA // Set Point
                (POINT | DECIMAL | integer | LOCALVAR) COMMA // Proportional Gain
                (POINT | DECIMAL | integer | LOCALVAR) COMMA // Integral Gain
                (POINT | DECIMAL | integer | LOCALVAR) COMMA // Derivative Gain
                (POINT | DECIMAL | integer | LOCALVAR) COMMA // Sample Time
                (POINT | DECIMAL | integer | LOCALVAR) COMMA // Bias
                (POINT | DECIMAL | integer | LOCALVAR) COMMA // Low Limit
                (POINT | DECIMAL | integer | LOCALVAR) COMMA // High Limit
                POS_INT RPAREN ; // Should be 0.

gosubStatement : GOSUB POS_INT LPAREN? POINT (COMMA POINT)* RPAREN? ;

gotoStatement : GOTO POS_INT ;

hlimitStatement : HLIMIT LPAREN expression COMMA (POINT | LOCALVAR)+ RPAREN ;

validSetPoint : (POINT | LOCALVAR | SECNDS | SECONDS_COUNTER) ;

assignmentStatement : validSetPoint EQUAL_SIGN expression ;

//                              type (0 or 1)     input                   low              high                output points
dbswitStatement : DBSWIT LPAREN expression COMMA (POINT | LOCALVAR) COMMA expression COMMA expression (COMMA  (POINT | LOCALVAR))+ RPAREN ;

deactStatement : DEACT LPAREN POS_INT (COMMA POS_INT)* RPAREN ;

tableStatement : TABLE LPAREN
        (POINT | LOCALVAR) COMMA // Input
        (POINT | LOCALVAR)       // Output
        (COMMA (integer | DECIMAL | POINT | LOCALVAR)   // X1
         COMMA (integer | DECIMAL | POINT | LOCALVAR))+ // Y1
        RPAREN ;

ifStatement : IF LPAREN expression RPAREN THEN statement (ELSE statement)? ;

inittoStatement : INITTO LPAREN expression COMMA POINT (COMMA POINT)* RPAREN ;

llimitStatement : LLIMIT LPAREN expression COMMA (POINT | LOCALVAR)+ RPAREN ;

offStatement : OFF LPAREN (POINT | LOCALVAR | atPriorityStatusIndicator) (COMMA (POINT | LOCALVAR))* RPAREN ;
onStatement  : ON  LPAREN (POINT | LOCALVAR | atPriorityStatusIndicator) (COMMA (POINT | LOCALVAR))* RPAREN ;

// The manual says that the value parameter can't be an integer, but I've seen it in the wild.
setStatement : SET LPAREN expression (COMMA validSetPoint)+ RPAREN |
               SET LPAREN atPriorityStatusIndicator COMMA expression (COMMA POINT)+ RPAREN ;

onpwrtStatement : ONPWRT LPAREN integer RPAREN ;

releasStatement : RELEAS LPAREN (atPriorityStatusIndicator | POINT) (COMMA POINT)* RPAREN ;

returnStatement : RETURN ;

defineStatement : DEFINE LPAREN POINT COMMA POINT RPAREN ;

disalmStatement : DISALM LPAREN (POINT | LOCALVAR) (COMMA (POINT | LOCALVAR))* RPAREN ;
enalmStatement :  ENALM  LPAREN (POINT | LOCALVAR) (COMMA (POINT | LOCALVAR))* RPAREN ;

localStatement : LOCAL LPAREN POINT (COMMA POINT)* RPAREN ;

minStatement : MIN LPAREN (POINT | DECIMAL | integer | LOCALVAR) (COMMA (POINT | DECIMAL | integer | LOCALVAR))* RPAREN ;
maxStatement : MAX LPAREN (POINT | DECIMAL | integer | LOCALVAR) (COMMA (POINT | DECIMAL | integer | LOCALVAR))* RPAREN ;

timAvgStatement : TIMAVG LPAREN (POINT | LOCALVAR) COMMA (POS_INT | POINT | LOCALVAR) COMMA POS_INT COMMA (POINT | LOCALVAR) RPAREN ;

waitStatement : WAIT LPAREN POS_INT COMMA (POINT | LOCALVAR) COMMA (POINT | LOCALVAR) COMMA POS_INT RPAREN ;

comparers : EQUAL_TO | NOT_EQUAL_TO | LESS_THAN | LESS_THAN_OR_EQUAL_TO | GREATER_THAN | GREATER_THAN_OR_EQUAL_TO ;

expression
    : POINT
    | integer
    | DECIMAL
    | LOCALVAR
    | atPriorityStatusIndicator
    | pointStatusIndicator
    | residentPoint
    | MILITARY_TIME
    | MINUS expression
    | PLUS expression // I have seen the use of unary plus in the wild
    | arithmetic_function LPAREN expression RPAREN
    | TOTAL LPAREN POINT RPAREN
    | expression ROOT expression
    | expression op=('*'|'/') expression
    | expression op=('+'|'-') expression
    | expression comparers expression
    | expression (AND | NAND) expression
    | expression (OR | XOR) expression
    | LPAREN expression RPAREN
    ;

