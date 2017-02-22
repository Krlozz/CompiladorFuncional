/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Skeleton interface for Bison GLR parsers in C

   Copyright (C) 2002-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

#ifndef YY_YY_ANALISINTACTICO_TAB_H_INCLUDED
# define YY_YY_ANALISINTACTICO_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    MAIN = 258,
    IF = 259,
    ELSE = 260,
    DO = 261,
    WHILE = 262,
    FOR = 263,
    BREAK = 264,
    PRINT = 265,
    RETURN = 266,
    INT_TYPE = 267,
    FLOAT_TYPE = 268,
    LETRA_TYPE = 269,
    STRING_TYPE = 270,
    BOOL_TYPE = 271,
    CHAR = 272,
    STRING = 273,
    INTEGER = 274,
    FLOTANTE = 275,
    BOOLEANO = 276,
    ID = 277,
    MATH_INC = 278,
    MATH_DEC = 279,
    EQL = 280,
    MENOR_QUE = 281,
    MAYOR_QUE = 282,
    AND = 283,
    OR = 284,
    NOT = 285,
    KEYOP = 286,
    KEYCL = 287,
    ParetOP = 288,
    ParetCL = 289,
    BracketOP = 290,
    BracketCL = 291,
    IGUAL = 292,
    ASIGNACION = 293,
    COMA = 294,
    FinCommand = 295
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 35 "AnaliSintactico.y" /* glr.c:197  */

	char *lexeme;
	char *string;
	char *letra;
	int integer;
	float flotante;
	float booleano;

#line 104 "AnaliSintactico.tab.h" /* glr.c:197  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_ANALISINTACTICO_TAB_H_INCLUDED  */
