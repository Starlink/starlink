
/* Define token numbers for yylex return values.  Since we're not using yacc
   we have to do this by hand.  The values of these are not significant,
   but they must be ints out of the range of ASCII characters, and all
   different. */

#define AUTO 256
#define BREAK 257
#define CASE 258
#define CHAR 259
#define CONST 260
#define CONTINUE 261
#define DEFAULT 262
#define DO 263
#define DOUBLE 264
#define ELSE 265
#define ENUM 266
#define EXTERN 267
#define FLOAT 268
#define FOR 269
#define GOTO 270
#define IF 271
#define INT 272
#define LONG 273
#define REGISTER 274
#define RETURN 275
#define SHORT 276
#define SIGNED 277
#define SIZEOF 278
#define STATIC 279
#define STRUCT 280
#define SWITCH 281
#define TYPEDEF 282
#define UNION 283
#define UNSIGNED 284
#define VOID 285
#define VOLATILE 286
#define WHILE 287

#define F77_SUBROUTINE 288
#define F77_FUNCTION 289
#define F77_EXTERNAL 290
#define F77_CALL 291

#define IDENTIFIER 292
#define CONSTANT 293
#define STRING_LITERAL 294
#define CPP_INCLUDE 295
#define CPP_DEFINE 296
#define CPP_UNDEF 297
#define CPP_IF 298
#define CPP_IFDEF 299
#define CPP_IFNDEF 300
#define CPP_ELIF 301
#define CPP_ELSE 302
#define CPP_ENDIF 303
#define CPP_LINE 304
#define CPP_ERROR 305
#define CPP_PRAGMA 306
#define CPP_START 307

/* $Id$ */
