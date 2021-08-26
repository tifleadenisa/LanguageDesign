
%{
#include <stdio.h>
#include "y.tab.h"
%}
%option noyywrap
%%
"+" {return PLUS;}
"-" {return MINUS;}
"*" {return TIMES;}
"/" {return DIVIDE;}
"%" {return MODULO;}
"=="    {return EQ;}
"!="    {return DIF;}
"<"     {return LT;}
">"     {return GT;}
"&&"    {return AND;}
"||"    {return OR;}
"not"   {return NOT;}
"["		{return lpar;}
"]"		{return rpar;}
"strlen" {return STRLEN;}
"Eval" {return EVAL;}
"int"|"float"|"char"|"bool"|"void"|"constInt"|"constChar"|"constFloat"|"constBool" { yylval=strdup(yytext);return TIP;}
"LET'S_GO" {return BGIN;}
"LET'S_STOP" {return END;}
"struct" {return STRUCT;}
"true"|"false" {yylval=strdup(yytext); return BOOL;}
"if" {return IF;}
"then" {return THEN;}
"else" {return ELSE;}
"for" {return FOR;}
"<-strcpy<-" {yylval=strdup(yytext); return STRCPY;}
"->strcat<-" {yylval=strdup(yytext); return STRCAT;}
[a-zA-Z]+ {yylval=strdup(yytext); return CHR;}
&[a-zA-Z][_a-zA-Z0-9]* {yylval=strdup(yytext);return STRUCTID;}
@[a-zA-Z][_a-zA-Z0-9]* {yylval=strdup(yytext);return ID;}
":=" {return ASSIGN;}
[0-9]+ {yylval=strdup(yytext); return NR;}
[0-9]+,[0-9]+ {yylval=strdup(yytext); return NRR;}
[ \t] ;
\n {yylineno++;}
. {return yytext[0];}
