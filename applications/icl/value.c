/******************************************************************************
 *
 *	V A L U E . C
 *
 *	History
 *	Created :	S.K.Robinson	11/11/91
 *      Tidied, reformatted.
 *			B.K.McIlwrath	21/07/93 + 12/11/93
 *
 * ICL uses the concept of "values" extensively. This module provides an object
 * oriented interface to manipulate these quantities.
 *
 * The value definition (from value.h) is:-
 *
 * typedef struct _value
 * {
 * 	int type;
 * 	union _val {
 *		int integer;		      * TYPE_INTEGER, TYPE_LOGICAL
 *		double real;		      * TYPE_REAL
 *		char *string;		      * TYPE_STRING, TYPE_EXCEPTION
 *		struct _symtab *symbols;      * TYPE_SYMTAB
 *		struct _value (*function)();  * TYPE_FUNCTION
 *		double (*dfunction)();	      * TYPE_FUNCTION
 *		int (*ifunction)();	      * TYPE_FUNCTION
 *		PORTPTR fp;		      * TYPE_FILE
 *	} u;
 * } value;
 *
 *
 ******************************************************************************
 */
#include <stdio.h>
#include "icl.h"		/* includes value.h */
#include "parse.h"
#include "ems.h"

/******************************************************************************
 *
 * An array of strings used to convert the ICL-value type code
 * to a human readable string.
 *
 ******************************************************************************
 */
static char *(typetostringmap[]) =
{
    "UNDEFINED", "REAL", "INTEGER", "LOGICAL", "STRING",
    "FUNCTION", "EXCEPTION", "FILE", "SYMTAB", "UNKNOWN"
};

/******************************************************************************
 *
 * ICL_value nulls
 *
 ******************************************************************************
 */
value *VALUENIL = (value *) 0;

value (*INTERPRETNIL)() = (value(*) ()) 0;


/******************************************************************************
 *
 * ICL-value global constants - initialised in init_values()
 *
 ******************************************************************************
 */
value noval, trueval, falseval, intzeroval, realzeroval, nullstringval;

/******************************************************************************
 *
 *	I N I T _ V A L U E S (void)
 *
 * Initialise local variables and global values - called from main() during
 * ICL initialisation.
 *
 ******************************************************************************
 */
void
init_values(void)
{
    trueval     =   value_logical(1);
    falseval    =   value_logical(0);
    intzeroval  =   value_integer(0);
    realzeroval =   value_real(((double) 0.0));
    nullstringval = value_string(CHARNIL);
    noval.type  =   TYPE_UNKNOWN;
    noval.u.integer = 0;
    return;
}

/******************************************************************************
 *
 *		M A P T Y P E T O S T R I N G (value v)
 *
 * Return a string describing the current setting of the val.type component
 *
 ******************************************************************************
 */
char *
maptypetostring(value v)
{
    int t = v.type;

    if (t < 0 || t > 8)
	return (typetostringmap[9]);
    else
	return (typetostringmap[t]);
}

/******************************************************************************
 *
 *	V A L U E _ P R I N T (value v)
 *
 * Print an internal value for the user with default formatting and global
 * precision.
 *
 ******************************************************************************
 */
int
value_print(value v)
{
    extern int outfpstring(char *message);	/* output.c */
    extern int precision;			/* procs.c  */

    int istat;
    static char buf[1024];

    switch (v.type) {
      case TYPE_UNDEFINED:
        istat = outfpstring("<UNDEFINED>");
        return istat;
      case TYPE_INTEGER:
	sprintf(buf, "%10d", v.u.integer);
	break;
      case TYPE_REAL:
	sprintf(buf, "%#.*g", precision, v.u.real);
	break;
      case TYPE_LOGICAL:
	if (v.u.integer)
	    istat = outfpstring("TRUE");
	else
	    istat = outfpstring("FALSE");
	return istat;
      case TYPE_STRING:
	istat = outfpstring(v.u.string);
	return istat;
      case TYPE_EXCEPTION:
	sprintf(buf, "<unnoticed exception: \"%s\">", v.u.string);
	break;
      case TYPE_FUNCTION:
	sprintf(buf, "<function value %d>", v.u.integer);
	break;
      default:
	istat = outfpstring("<value of unknown type>");
	return istat;
    }
    istat = outfpstring(buf);
    return istat;
}

/******************************************************************************
 *
 *	I S E X C E P T I O N (value val)
 *
 * This routine is called by the macro isexc() on compilers which cannot
 * select off a function resulting in a structure.
 *
 * This is a compiler bug work-around.
 *
 ******************************************************************************
 */
int
isexception(value val)
{
    return (val.type == TYPE_EXCEPTION);
}

/******************************************************************************
 *
 *		CONSTRUCTORS
 *
 *      These create the value node in a local auto and return a copy
 *      of it.  This means that, to persist, the copy must then be
 *      assigned to a variable of type value.
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 *	V A L U E _ R E A L (double f)
 *
 ******************************************************************************
 */
value
value_real(double f)
{
    value new;

    new.type = TYPE_REAL;
    new.u.real = f;
    return new;
}

/******************************************************************************
 *
 *	V A L U E _ I N T E G E R (int i)
 *
 ******************************************************************************
 */
value
value_integer(int i)
{
    value new;

    new.type = TYPE_INTEGER;
    new.u.integer = i;
    return new;
}

/******************************************************************************
 *
 *	V A L U E _ L O G I C A L (int i)
 *
 ******************************************************************************
 */
value
value_logical(int i)
{
    value new;

    new.type = TYPE_LOGICAL;
    new.u.integer = i;
    return new;
}

/******************************************************************************
 *
 *	V A L U E _ S T R I N G (char *s)
 *
 ******************************************************************************
 */
value
value_string(char *s)
{
    value new;
    char *ls;

    if ((ls = s) != CHARNIL)
	if ((ls = strcopy(s)) == CHARNIL)
	    return (exception(
			"SYSERR  memory exhausted in routine value_string()"));
    new.type = TYPE_STRING;
    new.u.string = ls;
    return new;
}

/******************************************************************************
 *
 *	V A L U E _ F U N C T I O N (value (*fn)())
 *
 ******************************************************************************
 */
value
value_function(value(*fn) ())
{
    value new;

    new.type = TYPE_FUNCTION;
    new.u.function = fn;
    return new;
}

/******************************************************************************
 *
 *	V A L U E _ R E A L _ F U N C T I O N (double (*fn)())
 *
 ******************************************************************************
 */
value
value_real_function(double (*fn) ())
{
    value new;

    new.type = TYPE_FUNCTION;
    new.u.dfunction = fn;
    return new;
}

/******************************************************************************
 *
 *	V A L U E _ I N T E G E R _ F U N C T I O N (int (*fn)())
 *
 ******************************************************************************
 */
value
value_integer_function(int (*fn) ())
{
    value new;

    new.type = TYPE_FUNCTION;
    new.u.ifunction = fn;
    return new;
}

/******************************************************************************
 *
 *	E X C E P T I O N (char * name)
 *
 * Return an ICL exception value type.
 *
 * When ICL runs out of memory it generates an exception of the type SYSERR.
 * In this case there is little point in using strcopy() to create the
 * exception value so we set the exception value pointing to the original
 * exception message (as given to exception()).  This is usually a static
 * string so this is safe.
 *
 * Also note that the interpretation of BREAK generates an exception with a
 * CHARNIL message and this must be handled specially.
 *
 ******************************************************************************
 */
value
exception(char *name)
{
    value new;

    new.type = TYPE_EXCEPTION;
    if (name == CHARNIL || (strncmp(name, "SYSERR", 6) == 0))
	new.u.string = name;
    else if ((new.u.string = strcopy(name)) == CHARNIL)
	new.u.string = "SYSERR  memory exhausted in routine exception()";
    return new;
}

/******************************************************************************
 *
 *	E X C E P T I O N 1 (char *format, char *qual)
 *
 ******************************************************************************
 */
value
exception1(char *format, char *qual)
{
    value new;
    char *buf;

    if ((buf = (char *) malloc(((unsigned int)
				(strlen(format) + strlen(qual) + 1))))
				 == CHARNIL)
	return exception("SYSERR memory exhausted in routine exception1()");
    sprintf(buf, format, qual);
    new = exception(buf);
    (void) free(buf);
    return new;
}

/******************************************************************************
 *
 *	E X C E P T I O N 2 (char *format, char *qual, char *qual1)
 *
 ******************************************************************************
 */
value
exception2(char *format, char *qual, char *qual1)
{
    value new;
    char *buf;

    if ((buf = (char *) malloc(((unsigned int)
			(strlen(format) + strlen(qual) + strlen(qual1) + 1))))
			== CHARNIL)
	return exception("SYSERR memory exhausted in routine exception2()");
    sprintf(buf, format, qual, qual1);
    new = exception(buf);
    (void) free(buf);
    return new;
}

/******************************************************************************
 *
 *	A D A M _ E X C E P T I O N (char *exname, int status)
 *
 ******************************************************************************
 */
value
adam_exception(char *exname, int status)
{
    char buff[132];
    char *facility, *ident, *text;
    value val;

    ems1_get_facility_error(status, &facility, &ident, &text);
    sprintf(buff, "%s  %%%s, %s", exname, facility, text);

    val = exception(buff);
    return val;
}

/******************************************************************************
 *
 *	V A L U E _ S Y M T A B (struct _symtab *sym)
 *
 ******************************************************************************
 */
value
value_symtab(struct _symtab *sym)
{
    value new;

    new.type = TYPE_SYMTAB;
    new.u.symbols = sym;
    return new;
}

/******************************************************************************
 *
 *	V A L U E _ F I L E (PORTPTR fp)
 *
 ******************************************************************************
 */
value
value_file(PORTPTR fp)
{
    value new;

    new.type = TYPE_FILE;
    new.u.fp = fp;
    return new;
}

/******************************************************************************
 *
 *	R E S E T _ I N T E G E R _ P A R T (value *val, int i)
 *
 ******************************************************************************
 */
void
reset_integer_part(value *val, int i)
{
    (*val).u.integer = i;
    return;
}

/******************************************************************************
 *
 *                     A C C E S S O R S
 *
 *  These access the value's C variable component parts
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 *	R E A L _ P A R T (value val)
 *
 ******************************************************************************
 */
double
real_part(value val)
{
    return val.u.real;
}

/******************************************************************************
 *
 *	I N T E G E R _ P A R T (value val)
 *
 ******************************************************************************
 */
int
integer_part(value val)
{
    return val.u.integer;
}

/******************************************************************************
 *
 *	L O G I C A L _ P A R T (value val)
 *
 ******************************************************************************
 */
int
logical_part(value val)
{
    return val.u.integer;
}

/******************************************************************************
 *
 *	S T R I N G _ P A R T (value val)
 *
 ******************************************************************************
 */
char *
string_part(value val)
{
    return val.u.string;
}

/******************************************************************************
 *
 *	F I L E _ P A R T (value val)
 *
 ******************************************************************************
 */
PORTPTR
file_part(value val)
{
    return val.u.fp;
}

/******************************************************************************
 *
 *	F U N C T I O N _ P A R T (value val)
 *
 ******************************************************************************
 */
value((*(
function_part(value val))) ())
{
    return val.u.function;
}

/******************************************************************************
 *
 *	D F U N C T I O N _ P A R T (value val)
 *
 ******************************************************************************
 */
double ((*(
dfunction_part(value val))) ())
{
    return val.u.dfunction;
}

/******************************************************************************
 *
 *	I F U N C T I O N _ P A R T (value val)
 *
 ******************************************************************************
 */
int ((*(
ifunction_part(value val))) ())
{
    return val.u.ifunction;
}

/******************************************************************************
 *
 *	S Y M B O L T A B L E _ P A R T (value val)
 *
 * symboltable_part() is exported by value.c but should only be used in
 * symtab.c as it returns a pointer to a symboltable and this module is the
 * only one that knows about such things!
 *
 ******************************************************************************
 */
struct _symtab *
symboltable_part(value val)
{
    return val.u.symbols;
}

/******************************************************************************
 *
 *		TYPE CONVERSIONS
 *
 * The following routines are used for type conversion.
 *
 * ICL is permissive about conversions and will allow any to any conversion
 * in most contexts.
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 *	R E A L _ V A L (value aval)
 *
 * This function is used to ensure that a value is  a real.
 *
 * If given an exception value it is returned.
 * If given a string it is interpreted by as_nonstring() to a non-string. Then,
 * depending on the resultant value type, it is converted to a real value
 *
 ******************************************************************************
 */
value
real_val(value aval)
{
    value val;

    if (isexc(aval))
	return aval;
    if (isexc(val = as_nonstring(aval)))
	return val;
    switch (val.type) {
      case TYPE_INTEGER:
	return (value_real((double) val.u.integer));
      case TYPE_REAL:
	return val;
      case TYPE_LOGICAL:
	return exception("INVARGMAT Use of Logical in numeric context");
      case TYPE_UNDEFINED:
	return exception(
		"INVARGMAT Use of <UNDEFINED> variable in numeric context");
      default:
	systemfail("real_val(): conversion of weird type\n");
	return realzeroval; /* for lint */
    }
}

/******************************************************************************
 *
 *	I N T E G E R _ V A L (value aval)
 *
 * This function is used to ensure that a value is  a integer.
 *
 * If given an exception value it is returned.
 * If given a string it is interpreted by as_nonstring() to a non-string. Then,
 * depending on the resultant value type,  it is converted to a integer value
 *
 ******************************************************************************
 */
value
integer_val(value aval)
{
    value val;
    extern value func_nint(value vals);		/* carith.c */

    if (isexc(aval))
	return aval;
    if (isexc(val = as_nonstring(aval)))
	return val;
    switch (val.type) {
      case TYPE_INTEGER:
	return val;
      case TYPE_LOGICAL:
	return (exception("INVARGMAT  Use of Logical in numeric context"));
      case TYPE_REAL:
	return (func_nint(val));
      case TYPE_STRING:
	return (exception("INVARGMAT  Use of string in integer context"));
      case TYPE_UNDEFINED:
	return exception(
		"INVARGMAT Use of <UNDEFINED> variable in numeric context");
      default:
	return (exception("INVARGMAT Integer expected"));
    }
}

/******************************************************************************
 *
 *	S T R I N G _ V A L (value val)
 *
 *
 * This function returns the contents of the given value 'val' as a
 * packed string.
 *
 ******************************************************************************
 */
value
string_val(value val)
{
    extern int precision;					/* procs.c */
    static char buffer[20];

    if (isexc(val))
	return val;
    else {
	switch (val.type) {
	  case TYPE_STRING:
	    return val;
	  case TYPE_INTEGER:
	    sprintf(buffer, "%d", val.u.integer);
	    return (value_string(buffer));
	  case TYPE_LOGICAL:
	    if (val.u.integer)
		return value_string("TRUE");
	    else
		return value_string("FALSE");
	  case TYPE_REAL:
	    sprintf(buffer, "%#.*g", precision, val.u.real);
	    return (value_string(buffer));
	  case TYPE_UNDEFINED:
	    return value_string("<UNDEFINED>");
	  default:
	    systemfail("string_val(): conversion of weird type");
	    return nullstringval; /* for lint */
	}
    }
}

/******************************************************************************
 *
 *	S S T R I N G _ V A L (value val)
 *
 *
 * This function returns the contents of the given value 'val' as a string.
 * If the value structure requires converison to string form the string is
 * padded with leading blanks.
 *
 ******************************************************************************
 */
value
sstring_val(value val)
{
    static char buffer[20];

    switch (val.type) {
      case TYPE_INTEGER:
	sprintf(buffer, "%10d", val.u.integer);
	return (value_string(buffer));
    case TYPE_LOGICAL:
	if (val.u.integer)
	    return value_string(" TRUE");
	else
	    return value_string(" FALSE");
    default:
	return string_val(val);
    }
}

/******************************************************************************
 *
 *	A S _ N O N S T R I N G (value val)
 *
 * This function is used to ensure that a value is not a string.
 *
 * It does this by executing the string as an expression until it is no longer
 * a string, or an exception is raised (including a syntax error). This is
 * done so that such errors can be trapped earlier.
 *
 * The other conversion routines will only fail if there is something wrong
 * with ICL and are often used in expressions, so we want to know in advance
 * if there is some possibility of them failing.
 *
 * May be passed an exception which it returns.
 *
 ******************************************************************************
 */
value
as_nonstring(value val)
{
    extern node *parse_expression(char *s, int lenofs);	/* parse.y */
    extern value interpret(node *n);			/* interpret.c */
    int n;

    n = 0;
    while ((val.type == TYPE_STRING) && (n++ < 100))
	if (isexc(val = interpret(parse_expression(val.u.string,
						  (int) strlen(val.u.string)))))
	    return (val);
    if (val.type == TYPE_STRING)
	return (exception(
		"STKOVFLOW Stack overflow in routine as_nonstring()"));
    else
	return val;
}

/******************************************************************************
 *
 *	A S _ L O G I C A L (value val)
 *
 * This function is used to ensure that a value is a logical.
 *
 * Depending on the given value type it is converted to a logical value
 * If an exception value is given non-zero is returned
 *
 ******************************************************************************
 */
int
as_logical(value val)
{
    if (isexc(val))
	return (-1);
    else {
	if (isexc(val = as_nonstring(val)))
	    return (-1);
	switch (val.type) {
	  case TYPE_LOGICAL:
	    return val.u.integer;
	  default:
	    systemfail(	"as_logical(): conversion of weird type");
	    return 0; /* for lint */
	}
    }
}

/******************************************************************************
 *
 *	N U M E R I C _ A S _ R E A L (value val)
 *
 * Convert a numeric ICL value val to real.  Cannot exception!.
 *
 ******************************************************************************
 */
value
numeric_as_real(value val)
{
    switch (val.type) {
      case TYPE_INTEGER:
	return (value_real((double) val.u.integer));
      case TYPE_REAL:
	return val;
      default:
	systemfail("numeric_as_real: conversion of weird type\n");
	return realzeroval; /* for lint */
    }
}
