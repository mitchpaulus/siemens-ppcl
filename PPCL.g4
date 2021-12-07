grammar PPCL;

LPAREN : '(';
RPAREN : ')';
COMMA : ',';
EQUAL_SIGN : '=' ;

MINUS : '-';

program : line* EOF;

line : POS_INT (sample? statement NL | COMMENT);

COMMENT : 'C' (' '|'\t') .*? '\n' | 'C\n'  ;

NL : '\n' ;

fragment UPPERALPHANUM : [A-Z0-9] ;

SAMPLE : 'SAMPLE' ;
sample : SAMPLE LPAREN POS_INT RPAREN ;

TIMAVG : 'TIMAVG' ;
WAIT : 'WAIT' ;

TABLE : 'TABLE' ;
IF : 'IF' ;
THEN : 'THEN' ;
ELSE : 'ELSE' ;

POS_INT : [0-9]+ ;
NEG_INT : '-' [0-9]+ ;
integer : NEG_INT | POS_INT ;
MILITARY_TIME : [0-9][0-9]?':'[0-9][0-9];

LOCALVAR : '$LOC' ('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '10' | '11' | '12' | '13' | '14' | '15') ;

DECIMAL : '-'? [0-9]+ '.' [0-9]+ ;

LOOP : 'LOOP' ;
GOTO : 'GOTO' ;

SET : 'SET' ;

// Resident Points
ALMCNT : 'ALMCNT' ;
ALMCT2 : 'ALMCT2' ;
BATT : '$BATT' ;
CRTIME : 'CRTIME' ;
DAY : 'DAY' ;
DAYOFM : 'DAYOFM' ;
LINK : 'LINK' ;
MONTH : 'MONTH' ;
NODENUM : 'NODE' [0-9][0-9]? ;
PDL_MONITOR : '$PDL' ;
SECNDS : 'SECNDS' ;
SECONDS_COUNTER : 'SECOND' [1-7] ;
TIME : 'TIME' ;

residentPoint : ALMCNT | ALMCT2 | BATT | CRTIME | DAY | DAYOFM | LINK | MONTH | NODENUM | PDL_MONITOR | SECNDS | SECONDS_COUNTER | TIME ;

// At (@) Priority Status Indicators
EMER : '@EMER' ;
NONE : '@NONE' ;
OPER : '@OPER' ;
PDL : '@PDL' ;
SMOKE : '@SMOKE' ;

atPriorityStatusIndicator : EMER | NONE | OPER | PDL | SMOKE ;

// Point Status Indicators
ALARM : 'ALARM' ;
ALMACK : 'ALMACK' ;
AUTO : 'AUTO' ;
DEAD : 'DEAD' ;
LOW : 'LOW' ;
OK : 'OK' ;
DAYMOD : 'DAYMOD' ;
FAILED : 'FAILED' ;
FAST : 'FAST' ;
HAND : 'HAND' ;
NGTMOD : 'NGTMOD' ;
OFF : 'OFF' ;
ON : 'ON' ;
PRFON : 'PRFON' ;
SLOW : 'SLOW' ;

pointStatusIndicator : ALARM | ALMACK | AUTO | DEAD | LOW | OK | DAYMOD | FAILED | FAST | HAND | NGTMOD | OFF | ON | PRFON | SLOW ;


// See pg. 1-45 (p. 32) Table 1-5 for order of precedence.
ALARMPRI : 'ALARMPRI' ;
ATN : 'ATN' ;
COM: 'COM' ;
COS : 'COS' ;
EXP : 'EXP' ;
LOG : 'LOG' ;
SIN : 'SIN' ;
SQRT : 'SQRT' ;
TAN : 'TAN' ;
TOTAL : 'TOTAL' ;

ROOT : '.ROOT.' ;

EQUAL_TO : '.EQ.' ;
NOT_EQUAL_TO : '.NE.' ;
LESS_THAN : '.LT.' ;
LESS_THAN_OR_EQUAL_TO : '.LE.' ;
GREATER_THAN : '.GT.' ;
GREATER_THAN_OR_EQUAL_TO : '.GE.' ;

AND : '.AND.' ;
NAND : '.NAND.' ;

OR: '.OR.' ;
XOR : '.XOR.' ;

ONPWRT : 'ONPWRT' ;
RELEAS : 'RELEAS' ;
DEFINE : 'DEFINE' ;

MIN : 'MIN' ;
MAX : 'MAX' ;

// Notes from manual:
// Defining Points in a PPCL Program
// • All physical and virtual points used in the program must be defined in the point database.
// • Point names that begin with numbers must be prefixed with the at (@) character.
// • Point names that are greater than 6 characters must be enclosed in double quotes.
// • Point names that use characters other than A-Z or 0-9 must be enclosed in double quotes.
POINT : '"' .*? '"' |
        [A-Z] |
        [A-Z] UPPERALPHANUM |
        [A-Z] UPPERALPHANUM UPPERALPHANUM |
        [A-Z] UPPERALPHANUM UPPERALPHANUM UPPERALPHANUM |
        [A-Z] UPPERALPHANUM UPPERALPHANUM UPPERALPHANUM UPPERALPHANUM |
        [A-Z] UPPERALPHANUM UPPERALPHANUM UPPERALPHANUM UPPERALPHANUM UPPERALPHANUM ;

WS : [ \t]+ -> skip ;

statement : loopStatement |
            gotoStatement |
            assignmentStatement |
            tableStatement |
            ifStatement |
            onStatement |
            offStatement |
            setStatement |
            onpwrtStatement |
            releasStatement |
            defineStatement |
            minStatement |
            maxStatement |
            timAvgStatement |
            waitStatement
            ;

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


gotoStatement : GOTO POS_INT ;

assignmentStatement : (POINT | LOCALVAR) EQUAL_SIGN expression ;

tableStatement : TABLE LPAREN
        (POINT | LOCALVAR) COMMA // Input
        (POINT | LOCALVAR)       // Output
        (COMMA (integer | DECIMAL | POINT | LOCALVAR)   // X1
         COMMA (integer | DECIMAL | POINT | LOCALVAR))+ // Y1
        RPAREN ;

ifStatement : IF LPAREN expression RPAREN THEN statement (ELSE statement)? ;

onStatement : ON LPAREN (POINT | LOCALVAR) (COMMA (POINT | LOCALVAR))* RPAREN ;
offStatement : OFF LPAREN (POINT | LOCALVAR) (COMMA (POINT | LOCALVAR))* RPAREN ;

// The manual says that the value parameter can't be an integer, but I've seen it in the wild.
setStatement : SET LPAREN (DECIMAL | POINT | LOCALVAR | integer) COMMA POINT (COMMA POINT)* RPAREN |
               SET LPAREN atPriorityStatusIndicator COMMA (DECIMAL | POINT | LOCALVAR | integer) (COMMA POINT)* RPAREN ;

onpwrtStatement : ONPWRT LPAREN integer RPAREN ;

releasStatement : RELEAS LPAREN (OPER | POINT) (COMMA POINT)* RPAREN ;

defineStatement : DEFINE LPAREN POINT COMMA POINT RPAREN ;

minStatement : MIN LPAREN (POINT | DECIMAL | integer | LOCALVAR) (COMMA (POINT | DECIMAL | integer | LOCALVAR))* RPAREN ;
maxStatement : MAX LPAREN (POINT | DECIMAL | integer | LOCALVAR) (COMMA (POINT | DECIMAL | integer | LOCALVAR))* RPAREN ;

timAvgStatement : TIMAVG LPAREN (POINT | LOCALVAR) COMMA (POS_INT | POINT | LOCALVAR) COMMA POS_INT COMMA (POINT | LOCALVAR) RPAREN ;

waitStatement : WAIT LPAREN POS_INT COMMA (POINT | LOCALVAR) COMMA (POINT | LOCALVAR) COMMA POS_INT RPAREN ;

comparers : EQUAL_TO | NOT_EQUAL_TO | LESS_THAN | LESS_THAN_OR_EQUAL_TO | GREATER_THAN | GREATER_THAN_OR_EQUAL_TO ;

expression :POINT |
            integer |
            DECIMAL |
            LOCALVAR |
            atPriorityStatusIndicator |
            pointStatusIndicator |
            residentPoint |
            MILITARY_TIME |
            LPAREN expression RPAREN |
            MINUS expression |
            expression ROOT expression |
            expression op=('*'|'/') expression |
            expression op=('+'|'-') expression |
            expression comparers expression |
            expression (AND | NAND) expression |
            expression (OR | XOR) expression ;



