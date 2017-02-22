%{
	#include <string.h>	
	#include <stdlib.h>	
	#include "AnaliSintactico.tab.h"
	
	int lines=1, chars=0, errors=0;

	void count_lb(char *str, int leng, int *c, int *l);
%}


attribution	("=")
horizontal	[" "\t]
linebreak	\n|\v\r
comment		"/*"({linebreak}*{horizontal}*)?[^}\n]*({linebreak}*{horizontal}*)?"*/"|"//"[^}\n]*
%option noyywrap
%option noinput
%option nounput


%%

"main"	{ chars += yyleng; return (MAIN); }
"if"	{ chars += yyleng; return (IF); }
"else"	{ chars += yyleng; return (ELSE); }
"do"	{ chars += yyleng; return (DO); }
"while"	{ chars += yyleng; return (WHILE); }
"for"	{ chars += yyleng; return (FOR); }
"return"	{ chars += yyleng; return (RETURN); }

{comment} { lines++; }

int {chars+=yyleng;return (INT_TYPE);}
float {chars+=yyleng;return (FLOAT_TYPE);}
char {chars+=yyleng;return (LETRA_TYPE);}
string {chars+=yyleng;return (STRING_TYPE);}
bool  {chars+=yyleng;return (BOOL_TYPE);}

[-]?[0-9]+  {yylval.integer=(int)atoi(yytext);chars += yyleng;return (INTEGER);}
[-]?[0-9]*"."[0-9]*([eE][+-]?[0-9]+)? {yylval.flotante=(float)atof(yytext);chars += yyleng;return (FLOTANTE);}

"true"|"false"	{yylval.booleano=(float)atoi(yytext);chars += yyleng;return (BOOLEANO);}

[a-z][a-zA-Z0-9]*   {yylval.lexeme=(char *)strdup(yytext);chars+=yyleng;return (ID);}
('.')   {yylval.letra=(char *)strdup(yytext);chars+=yyleng;return CHAR;}
(\".*\")  {yylval.string=(char *)strdup(yytext);chars+=yyleng;return STRING;}

"+"|"-"|"*"|"/" 	{ chars++; return yytext[0]; }

"++" {chars+=yyleng; return MATH_INC; }
"--" { chars+=yyleng; return MATH_DEC; }
"<"  { chars+=yyleng; return MENOR_QUE; } 
">"  { chars+=yyleng; return MAYOR_QUE; } 
"==" { chars+=yyleng; return EQL; } 
"&&" { chars+=yyleng; return AND; } 
"||" { chars+=yyleng; return OR; } 
"!"  { chars+=yyleng; return NOT; } 


{attribution} { chars++; return (yytext[0]); }

"="                 {chars+=yyleng;return IGUAL;}
";"                 {chars+=yyleng;return FinCommand;}
","                       {chars+=yyleng;return COMA;}
":"                       {chars+=yyleng;return ASIGNACION;}
"{"                      {chars+=yyleng;return KEYOP;}
"}"                      {chars+=yyleng;return KEYCL;}
"("                        {chars+=yyleng;return ParetOP;}
")"                       {chars+=yyleng;return ParetCL;}
"["                        {chars+=yyleng;return BracketOP;}
"]"                      {chars+=yyleng;return BracketCL;}



{horizontal} { chars++; }

{linebreak} {
	chars = 0;
	lines++;
}
. {	printf("( %3d : %-3d ) Token no reconocido\n", lines, chars);
	chars++;
	errors++;}

%%

void count_lb(char *str, int leng, int *c, int *l) {
	int i;

	for(i=0; i<leng; i++) {
		(*c)++;
		if(str[i] == '\n') {
			(*l)++;
			*c = 0;
		}
	}
}
