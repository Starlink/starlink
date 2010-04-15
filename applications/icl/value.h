/******************************************************************************
 *
 *		V A L U E . H
 *
 *	Created		S.K. Robinson 6/5/92
 *	Tidied		B.K. McIlwrath 12/11/93
 *
 * This file is an interface to value.c - The value structure is used
 * extensively within ICL.
 *
 * This file is included by icl.h
 *
 *****************************************************************************
 */
typedef struct _value
 {
 	int type;
 	union _val {
		int integer;		     /* TYPE_INTEGER, TYPE_LOGICAL */
		double real;		     /* TYPE_REAL */
		char *string;		     /* TYPE_STRING, TYPE_EXCEPTION */
		struct _symtab *symbols;     /* TYPE_SYMTAB */
		struct _value (*function)(); /* TYPE_FUNCTION */
		double (*dfunction)();	     /* TYPE_FUNCTION
					      *  - distinguished by context */
		int (*ifunction)();	     /* TYPE_FUNCTION
					      *  - distinguished by context */
		PORTPTR fp;		     /* TYPE_FILE */
	} u;
 } value;

/******************************************************************************
 *
 * ICL types that can be held in value structures
 *
 ******************************************************************************
 */
#define TYPE_UNDEFINED	0
#define TYPE_REAL	1
#define TYPE_INTEGER	2
#define TYPE_LOGICAL	3
#define TYPE_STRING	4

/******************************************************************************
 * The following are present so that other things can be stored in nodes in
 * symbol table entries
 ******************************************************************************
 */
#define TYPE_FUNCTION	5
#define TYPE_EXCEPTION	6
#define TYPE_FILE	7
#define TYPE_SYMTAB	8
#define TYPE_UNKNOWN	9
/*
 * Null values relating to ICL value types
 */
extern value *VALUENIL;
extern value (*INTERPRETNIL)();
#define VALUENULL ((value *)0)
/******************************************************************************
 *
 * macros for testing ICL values
 *
 ******************************************************************************
 */
#define isintegertype(x)	(x.type == TYPE_INTEGER)
#define isrealtype(x)		(x.type == TYPE_REAL)
#define islogicaltype(x)	(x.type == TYPE_LOGICAL)
#define isstringtype(x)		(x.type == TYPE_STRING)
#define isunknowntype(x) 	(x.type == TYPE_UNKNOWN)
#define thetypeof(x)		(x.type)

/******************************************************************************
 *
 * Global ICL values defined in init_values() (value.c)
 *
 ******************************************************************************
 */
extern value noval, falseval, trueval, intzeroval, realzeroval, nullstringval;

/******************************************************************************
 *
 * The public interface for value.c (ANSI C)
 *
 ******************************************************************************
 */
extern void init_values(void);
extern char *maptypetostring( value v  );
extern int  value_print( value v );
extern int  isexception( value v );
/*
 * Constructors
 */
extern value value_real   ( double f ),
       value_integer( int i ),
       value_logical( int i ),
       value_string ( char *s ),
       value_sstring ( char *s );
extern value value_function ( value (*fn)() ),
       value_real_function ( double (*fn)() ),
       value_integer_function ( int (*fn)() );
extern value exception ( char *exceptionstring ),
	     exception1(char *format, char *qualifier ),
	     exception2(char *format, char *qualifier, char *qualifier2 ),
             adam_exception (char *exname, int status);
extern value value_symtab( struct _symtab *symtable ),
	     value_file  ( PORTPTR fp );

extern void reset_integer_part( value *val, int i  );
/*
 * Accessors
 */
extern double real_part ( value val );
extern int integer_part ( value val  );
extern int logical_part ( value val  );
extern char *string_part( value val  );
extern PORTPTR file_part( value val  );
extern value  ((*(function_part  ( value val  )))());
extern double ((*(dfunction_part ( value val  )))());
extern int    ((*(ifunction_part ( value val  )))());
extern struct _symtab *symboltable_part(value val);
/*
 * Convertors
 */
extern value real_val    (  value val  );
extern value integer_val (  value val  );
extern value string_val  (  value val  );
extern value as_nonstring(  value val );
extern int as_logical    (  value val );
extern value numeric_as_real(  value numericval  );
