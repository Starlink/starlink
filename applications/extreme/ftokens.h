/* Define token numbers for yylex return values.  Since we're not using yacc
   we have to do this by hand.  The values of these are not significant,
   but they must be ints out of the range of ASCII characters, and all
   different. */

#define LINE_START 257
#define LINE_END 258
#define BLANK_LINE 259
#define COMMENT_LINE 260
#define SUBROUTINE 261
#define ENTRY 262
#define BLOCKDATA 263
#define PROGRAM 264
#define FUNCTION 265
#define INTEGER 266
#define REAL 267
#define DOUBLEPRECISION 268
#define COMPLEX 269
#define LOGICAL 270
#define CHARACTER 271
#define BYTE 272
#define UBYTE 273
#define WORD 274
#define UWORD 275
#define GENERIC_TYPE 276
#define DIMENSION 277
#define INCLUDE 278
#define IF 279
#define ELSEIF 280
#define THEN 281
#define CALL 282
#define INTEGER_CONSTANT 283
#define STRING_CONSTANT 284
#define ILLEGAL_CHAR 285
#define IDENTIFIER 286
#define VAL 287
#define END 288
#define CNF_PVAL 289
#define EQUIVALENCE 300
#define IOSTAT 301
#define IMPLICITNONE 302
#define IMPLICIT 303

#define IABS 400
#define IDIM 401
#define ISIGN 402
#define MAX0 403
#define AMAX0 404
#define MIN0 405
#define AMIN0 406

/* $Id$ */
