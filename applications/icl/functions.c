/******************************************************************************
 *
 *	F U N C T I O N S . C
 *
 * This module implements the user functions provided by the ICL language.
 *
 * N.B. These are all declared static (except init_functions() which sets up
 * the symbol table) as they are all called indirectly from the interpreter
 * parsing the node structure.
 *
 *	History
 *	Created :	S.K.Robinson	7/11/91
 *	Edited :	I.R.Jenkins	2/6/92
 *			Prototyped & restrucured
 *	Reformatted code and comments :
 *			B.K.McIlwrath	27/7/93 + 15/11/93
 *      Terminate returned SUBSTR:
 *                      A.J.Chipperfield  19/10/94
 *      Fix core dump in DEC2S function when angle > 100
 *                      T.Jenness 21 Feb 2005
 *
 ******************************************************************************
 */
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "icl.h"
#include "parse.h"
#include "carith.h"
#include "expr.h"
#include "interp.h"
#include "symtab.h"
#include "ems.h"

/* node.h used in init_functions() only */

#define FUNC_MAX	1
#define FUNC_MIN	2
#define FUNC_OCT	8
#define FUNC_DEC	10
#define FUNC_HEX	16

extern node *arglist[];					/* interp.c */
extern char *version;					/* main.c   */

/******************************************************************************
 *
 *	F U N C _ S U B S T R (void)
 *
 * The ICL function SUBSTR(S,n,m) returns the substring of S beginning at n
 * of length m.
 *
 * Note we adjust for the fact that the first character is in the 0th position.
 *
 ******************************************************************************
 */
static value
func_substr(void)
{
    int from, width, length;
    value val;
    char *substr, *s;
    node *arg0, *arg1, *arg2;

    arg0 = arglist[0];
    arg1 = arglist[1];
    arg2 = arglist[2];
    if (isexc(val = nargs_should_be(3)))
	return val;
    if (isexc(val = interpret_to_string(arg0)))
	return val;
    s = string_part(val);
    length = strlen(s);
    if (isexc(val = interpret_to_integer(arg1)))
	return val;
    from = integer_part(val);
    --from;
    if (isexc(val = interpret_to_integer(arg2)))
	return val;
    width = integer_part(val);
    if (width < 0 || from < 0 || from >= length)
	return exception("SUBSTRERR  Substring Selection Error");
    if (from + width > length)
	width = length - from;
    if ((substr = (char *) malloc((unsigned) width + 1)) == CHARNIL)
	return exception(
	    "SYSERR  memory exhausted in substr()");
    else {
        *(substr + width) = '\0';
	return value_string(strncpy(substr, s + from, width));
    }
}

/******************************************************************************
 *
 *	F U N C _ S N A M E (void)
 *
 * SNAME (S,n,m) delivers the name derived by concatenating the string
 * S with integer n formatted into m characters including leading zeros
 * m is optional, if not present is taken to be the number required.
 *
 ******************************************************************************
 */
static value
func_sname(void)
{
    char buf[ICL_BUFSIZE], *w;
    int num, width;
    node *arg0, *arg1, *arg2 = NULL;
    value val, varg, varg1, varg2;

    arg0 = arglist[0];
    arg1 = arglist[1];
    if (isexc(val = nargs_in_range(2, 3)))
	return val;
    if (nargs > 2)
	arg2 = arglist[2];
    if (isexc(varg = interpret_to_string(arg0)))
	return varg;
    if (isexc(varg1 = interpret_to_integer(arg1)))
	return varg1;
    num = integer_part(varg1);
    if (nargs > 2) {
	if (isexc(varg2 = interpret_to_integer(arg2)))
	    return varg2;
	width = integer_part(varg2);
    } else
	width = ICL_BUFSIZE - 1;
    if (width < 0)
	width = 0;
    else if (width > ICL_BUFSIZE - 1)
	width = ICL_BUFSIZE - 1;
    sprintf(buf, "%0*d", width, num);
    if ((w = strconcat(string_part(varg), buf)) == CHARNIL)
	return exception("SYSERR  memory exhausted during SNAME()");
    else
	return value_string(w);
}

/******************************************************************************
 *
 *	F U N C _ I N D E X (char *s1, char *pattern)
 *
 * INDEX(s1,s2) returns the position of the first occurence of pattern
 * s2 in string s1.
 *
 ******************************************************************************
 */
static value
func_index(char *s1, char *pattern)
{
    char *where, startpattern;
    int patlen;

    where = s1;
    startpattern = *pattern;
    patlen = (int) strlen(pattern);
    while (((where = strchr(where, startpattern)) != CHARNIL) &&
	   (strncmp(where, pattern, patlen) != 0))
	where = where + 1;
    return value_integer(where != CHARNIL ? where - s1 + 1 : 0);
}

/******************************************************************************
 *
 *	F U N C _ E L E M E N T (void)
 *
 * ELEMENT(I,delim,s) returns the Ith element of a delimited string.
 *
 * So, if the string looks like ....(....(....(pppp( ....(....(..., then
 * element (3,"(",s) gives us pppp
 *
 ******************************************************************************
 */
static value
func_element(void)
{
    value val;
    int which, i;
    char *s, *delim, *where, *end, adelim, *w;
    node *arg0, *arg1, *arg2;

    arg0 = arglist[0];
    arg1 = arglist[1];
    arg2 = arglist[2];
    if (isexc(val = nargs_should_be(3)))
	return val;
    if (isexc(val = interpret_to_integer(arg0)))
	return val;
    which = integer_part(val);
    if (which < 0)
	return value_string("");
    if (isexc(val = interpret_to_string(arg1)))
	return val;
    delim = string_part(val);
    if (strlen(delim) != 1)
	return exception(
	    "FUNCERR  ELEMENT() - delimiter must be single character");
    else
	adelim = *delim;
    if (isexc(val = interpret_to_string(arg2)))
	return val;
    s = string_part(val);
    where = s;
    for (i = 0; i < which; i++)
	if ((where = strchr(where, adelim)) != CHARNIL)
	    ++where;
	else
	    return value_string("");
    if ((end = strchr(where, adelim)) == CHARNIL)
	end = where + strlen(where);
    if ((w = (char *) malloc((unsigned) (end - where + 1))) == CHARNIL)
	return exception("SYSERR  memory exhausted during ELEMENT()");
    else
	return value_string(strncpy(w, where, end - where));
}


/******************************************************************************
 *
 *	F U N C _ S Y S E R R (node *n)
 *
 * SYSERR <VALUE|IDENT|IDENTIFICATION|TEXT|FULL> <ERROR CODE>
 *
 * This function returns the translation of the facility (ADAM) error code
 * in the format requested.
 * If the 'error code' parameter is missing returns the translation of the last
 * global status value stored by ICL.
 *
 ******************************************************************************
 */
static value
func_syserr(node *n)
{
    value val;
    node *arg1, *arg2;
    int status = 0; /* SAI__OK */
    char *facility, *ident, *text, *format;
    char buff[132];

    format = NULL;
    arg1 = arglist[0];
    arg2 = arglist[1];
    if (isexc(val = nargs_in_range(0, 2)))
	return val;
    if (nargs == 2) {
	if (isexc(val = interpret_to_integer(arg2)))
	    return val;
	else
	    status = integer_part(val);
    }
    if (nargs != 0) {
	if (isexc(val = interpret_to_string(arg1)))
	    return val;
	format = uppercase(string_part(val));
	if (*format == '\0' || strcmp(format, "DEFAULT") == 0)
	    format = "DEFAULT";
	else if (strcmp(format, "FACILITY") == 0)
	    format = "FACILITY";
	else if (strncmp(format, "IDENT", 5) == 0)
	    format = "IDENT";
	else if (strcmp(format, "TEXT") == 0)
	    format = "TEXT";
	else if (strcmp(format, "VALUE") == 0)
	    format = "VALUE";
	else
	    return exception1(
		"FUNCERR parameter \"%s\" not recognised", format);
    }
    if (strcmp(format, "VALUE") == 0) {
	sprintf(buff, "%d", status);
	val = value_string(buff);
    } else {
	ems1_get_facility_error(status, &facility, &ident, &text);
	sprintf(buff, "%s__%s, %s", facility, ident, text);
	if (strcmp(facility, "FACERR") == 0)
	    return exception(buff);

        if (strcmp(format, "DEFAULT") == 0)
	    val = value_string(buff);
	else if (strcmp(format, "FACILITY") == 0)
	    val = value_string(facility);
	else if (strcmp(format, "IDENT") == 0)
	    val = value_string(ident);
	else /* TEXT */
	    val = value_string(text);
    }
    return val;
}


/******************************************************************************
 *
 *	F U N C _ R A (char *s)
 *
 * RA(s) returns the right ascension in radians from a string in hrs, mins
 * and seconds.
 *
 ******************************************************************************
 */
static value
func_ra(char *s)
{
    int hour, min;
    float sec;

    if (sscanf(s, "%d%*c%d%*c%f", &hour, &min, &sec) != 3)
	return exception("CONVERR  Error converting Right Ascension");
    return value_real(2 * M_PI * (hour * 3600 + min * 60 + sec) / 86400);
}

/******************************************************************************
 *
 *	F U N C _ R A 2 S (void)
 *
 * RA2S(r,ndp,sep) produces a string in HMS format.
 *
 * 'ra' is in radians, 'ndp' decimal places required on seconds. Character
 * 'sep' separates fields.
 *
 ******************************************************************************
 */
static value
func_ra2s(void)
{
    double secs, tol=0.5;
    int ndp, i;
    char sep;
    char *res;
    int hour, min;
    value val;
    node *arg1, *arg2, *arg3;

    arg1 = arglist[0];
    arg2 = arglist[1];
    arg3 = arglist[2];
    if (isexc(val = nargs_in_range(2, 3)))
	return val;
    if (isexc(val = interpret_to_real(arg1)))
	return val;
    secs = 86400 * real_part(val) / (2 * M_PI);
    if (isexc(val = interpret_to_integer(arg2)))
	return val;
    ndp = integer_part(val);
    if (nargs == 3) {
	if (isexc(val = interpret_to_string(arg3)))
	    return val;
	sep = *(string_part(val));
    } else
	sep = ' ';
    if (ndp < 0)
	ndp = 0;
    if ((res = (char *) malloc((unsigned) (11 + ndp))) == CHARNIL)
	return exception("SYSERR  memory exhausted during RA2S()");
    secs = fmod(secs, 86400.0);
    if (secs < 0)
	secs += 86400.0;
    hour = secs / 3600;
    secs -= hour * 3600;
    min = secs / 60;
    secs -= min * 60;
    if ((int) secs == 59 && ndp != 0) {
	i = ndp;
	tol = 1.0;
	do
	    tol /= 10.0;
	while (--i != 0);
    }
    if (60.0-secs < tol) {
	secs = 0.0;
	if (++min == 60) {
	    min = 0;
	    if (++hour == 24)
		hour = 0;
        }
    }
    sprintf(res, "%02d%c%02d%c%0*.*f", hour, sep, min, sep,
	ndp ? ndp + 3 : 2, ndp, secs);
    return value_string(res);
}

/******************************************************************************
 *
 *	F U N C _ D E C L (char *s)
 *
 * DECL(I,m,n) - calculates a delination in radians from a string in degrees,
 * minutes & seconds.
 *
 ******************************************************************************
 */
static value
func_decl(char *s)
{
    int deg, min;
    float sec;

    if (sscanf(s, "%d%*c%d%*c%f", &deg, &min, &sec) != 3)
	return exception("CONVERR  Error converting Declination");
    return value_real(2 * M_PI * (deg * 3600 + min * 60 + sec) / (360 * 3600));
}

/******************************************************************************
 *
 *	F U N C _ D E C 2 S (void)
 *
 * DEC2S(r,ndp,sep) returns a string in DMS format.
 *
 * 'decl' is in radians, 'ndp' decimal places required in seconds,
 * character 'sep' separates fields
 *
 ******************************************************************************
 */
static value
func_dec2s(void)
{
    double secs, tol=0.5;
    int ndp, i;
    char sep;
    char *res;
    int deg, min;
    value val;
    node *arg1, *arg2, *arg3;
    char sign;

    arg1 = arglist[0];
    arg2 = arglist[1];
    arg3 = arglist[2];
    if (isexc(val = nargs_in_range(2, 3)))
	return val;
    if (isexc(val = interpret_to_real(arg1)))
	return val;
    secs = 360 * 3600 * real_part(val) / (2 * M_PI);
    sign = ( real_part(val) < 0 ? '-' : '+' );
    if (isexc(val = interpret_to_integer(arg2)))
	return val;
    ndp = integer_part(val);
    if (nargs == 3) {
	if (isexc(val = interpret_to_string(arg3)))
	    return val;
	sep = *(string_part(val));
    } else
	sep = ' ';
    if (ndp < 0)
	ndp = 0;
    if ((res = (char *) malloc((unsigned) (11 + ndp))) == CHARNIL)
	return exception("SYSERR memory exhausted during DEC2S()");
    deg = secs/3600.0;
    secs -= deg*3600;
    if (secs < 0)
	secs = -secs;
    min = secs/60.0;
    secs -= min * 60;
    if ((int) secs == 59 && ndp != 0) {
	i = ndp;
	tol = 1.0;
	do
	    tol /= 10.0;
	while (--i != 0);
    }
    if (60.0-secs < tol) {
	secs = 0.0;
	if (++min == 60) {
	    min = 0;
  	    ++deg;
        }
    }
    if (deg > 99)
	sprintf(res, "%c%03d%c%02d%c%0*.*f",
		sign,abs(deg), sep, min, sep,
		(ndp ? ndp + 3 : 2), ndp, secs);
    else
	sprintf(res, "%c%02d%c%02d%c%0*.*f",
		sign, abs(deg), sep, min, sep, (ndp ? ndp + 3 : 2), ndp, secs);
    return value_string(res);
}

/******************************************************************************
 *
 * Bitwise integer functions AND, OR, XOR and NOT
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 *	F U N C _ I A N D (int left, int right)
 *
 ******************************************************************************
 */
static int
func_iand(int left, int right)
{
    return left & right;
}

/******************************************************************************
 *
 *	F U N C _ I O R (int left, int right)
 *
 ******************************************************************************
 */
static int
func_ior(int left, int right)
{
    return left | right;
}

/******************************************************************************
 *
 *	F U N C _ I E O R (int left, int right)
 *
 ******************************************************************************
 */
static int
func_ieor(int left, int right)
{
    return left ^ right;
}

/******************************************************************************
 *
 *	F U N C _ I N O T (int arg)
 *
 ******************************************************************************
 */
static int
func_inot(int arg)
{
    return ~arg;
}

/******************************************************************************
 *
 * Compare two strings returning logical LGE, LGT, LLE, LLT, LEQ, LNE
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 *	F U N C _ L G E (char *lf, char *rt)
 *
 ******************************************************************************
 */
static value
func_lge(char *lf, char *rt)
{
    return value_logical(strcmp(lf, rt) >= 0);
}

/******************************************************************************
 *
 *	F U N C _ L G T (char *lf, char *rt)
 *
 ******************************************************************************
 */
static value
func_lgt(char *lf, char *rt)
{
    return value_logical(strcmp(lf, rt) > 0);
}

/******************************************************************************
 *
 *	F U N C _ L L E (char *lf, char *rt)
 *
 ******************************************************************************
 */
static value
func_lle(char *lf, char *rt)
{
    return value_logical(strcmp(lf, rt) <= 0);
}

/******************************************************************************
 *
 *	F U N C _ L L T (char *lf, char *rt)
 *
 ******************************************************************************
 */
static value
func_llt(char *lf, char *rt)
{
    return value_logical(strcmp(lf, rt) < 0);
}

/******************************************************************************
 *
 *	F U N C _ L E Q (char *lf, char *rt)
 *
 ******************************************************************************
 */
static value
func_leq(char *lf, char *rt)
{
    return value_logical(strcmp(lf, rt) == 0);
}

/******************************************************************************
 *
 *	F U N C _ L N E (char *lf, char *rt)
 *
 ******************************************************************************
 */
static value
func_lne(char *lf, char *rt)
{
    return value_logical(strcmp(lf, rt) != 0);
}

/******************************************************************************
 *
 *	F U N C _ L E N ( char *s)
 *
 * Evaluates LEN(s) - the length of the string s.
 *
 ******************************************************************************
 */
static value
func_len(char *s)
{
    return value_integer((int) strlen(s));
}

/******************************************************************************
 *
 *	F U N C _ K E Y V A L (char *s)
 *
 * KEYVAL(s) - value of the key with name 's'. Not yet implemented!
 *
 ******************************************************************************
 */
static value
func_keyval(char *s)
{
    return exception("UNIMPL  KEYVAL() function not yet implemented");
}

/******************************************************************************
 *
 *	F U N C _ I N K E Y (void)
 *
 * INKEY() - value of the last key pressed (only works in screen mode)
 * 	     (Not yet implemented)
 *
 ******************************************************************************
 */
static value
func_inkey(void)
{
    value val;

    if (isexc(val = nargs_should_be(0)))
	return val;
    return exception("UNIMPL  INKEY() function not yet implemented");
}

/******************************************************************************
 *
 *	F U N C _ I C H A R (char *s)
 *
 * ICHAR(s)  - ASCII value of first character of string.
 *
 ******************************************************************************
 */
static value
func_ichar(char *s)
{
    return value_integer(*s);
}

/******************************************************************************
 *
 *	F U N C _ U P C A S E (char *s)
 *
 * UPCASE(s) string 's' is converted to upper case.
 * Uses uppercase() in utils.c
 *
 ******************************************************************************
 */
static value
func_upcase(char *s)
{
    char *t;

    if ((t = strcopy(s)) == CHARNIL)
	return exception(
	    "SYSERR  memory exhausted during UPCASE()");
    else
	return value_string(uppercase(t));
}

/******************************************************************************
 *
 *	F U N C _ L O G I C A L (void)
 *
 * LOGICAL(x) - the value of x converted to logical.
 *
 ******************************************************************************
 */
static value
func_logical(void)
{
    value val;
    node *arg0;

    arg0 = arglist[0];
    if (isexc(val = nargs_should_be(1)))
	return val;
    if (isexc(val = interpret(arg0)))
	return val;
    else if (isexc(val = as_nonstring(val)))
	return val;
    else if (islogicaltype(val))
	return val;
    else
	return exception("INVARGLOG  Argument to LOGICAL() invalid");
}

/******************************************************************************
 *
 *	F U N C _ S T R I N G (void)
 *
 * STRING(x) - the value of x converted to a string.
 *
 ******************************************************************************
 */
static value
func_string(void)
{
    value val;
    node *arg0;

    if (isexc(val = nargs_should_be(1)))
	return val;
    arg0 = arglist[0];
    if (isexc(val = interpret_to_string(arg0)))
	return val;
    return val;	 /* NOT NECESSARY OR MISTAKE */
}

/******************************************************************************
 *
 *	F U N C _ V E R S I O N (void)
 *
 * VERSION() - returns the current version of ICL as a string.
 *
 ******************************************************************************
 */
static value
func_version(void)
{
    value val;

    if (isexc(val = nargs_should_be(0)))
	return val;
    return value_string(version);
}

/******************************************************************************
 *
 *	F U N C _ O S (void)
 *
 * OS() - returns the host operating system name as a string.
 *
 ******************************************************************************
 */
static value
func_os(void)
{
    extern char *os;
    value val;

    if (isexc(val = nargs_should_be(0)))
	return val;
    return value_string(os);
}

/******************************************************************************
 *
 *	F U N C _ C H A R (void)
 *
 * CHAR(i) - returns the character whose ASCII value is i.
 *
 ******************************************************************************
 */
static value
func_char(void)
{
    static char s[2] = {'\0', '\0'};
    value val;

    if (isexc(val = nargs_should_be(1)))
	return val;
    if (isexc(val = interpret_to_integer(arglist[0])))
	return val;
    s[0] = (integer_part(val) & 0xff);
    return value_string(s);
}

/******************************************************************************
 *
 *	F U N C _ T Y P E (void)
 *
 * TYPE(x) returns the type of x as one of "REAL", "INTEGER", "LOGICAL",
 *      "STRING" or "UNDEFINED".
 *
 * x is an identifer (should be a node of type name_interpret() with the
 * string_part() member equal to the identifer).  We find x in arglist[0] (set
 * up there by get_args() in interp.c).
 * We use lookup_variable_value() to get the variable's value if it exists
 * and use maptypetostring() to produce the type of the value as a string.
 *
 ******************************************************************************
 */
static value
func_type(void)
{
    value val;
    node *arg;

    if (isexc(val = nargs_should_be(1)))
	return val;
    arg = arglist[0];
    while (arg->interpret == paren_interpret)
	arg = arg->sub[0];
    if (arg->interpret != name_interpret)
	return value_string("UNDEFINED");
    if (isexc(val = lookup_variable_value(string_part(arg->val))))
	return value_string("UNDEFINED");
    return value_string(maptypetostring(val));
}

/******************************************************************************
 *
 *	F U N C _ U N D E F I N E D (void)
 *
 * UNDEFINED(x) -true if x is undefined.
 *
 ******************************************************************************
 */
static value
func_undefined(void)
{
    value val;
    node *arg0;

    if (isexc(val = nargs_should_be(1)))
	return val;
    arg0 = arglist[0];
    while (arg0->interpret == paren_interpret)
	arg0 = arg0->sub[0];
    if (arg0->interpret != name_interpret)
	return value_logical(TRUE);
    if ( isexc(val = lookup_variable_value(string_part(arg0->val))) ||
	 val.type == TYPE_UNDEFINED)
	return value_logical(TRUE);

    return value_logical(FALSE);
}

/******************************************************************************
 *
 *	F U N C _ V A R I A B L E (void)
 *
 * VARIABLE(proc,x) a functions which returns the value of variable 'x' of
 * procedure 'proc' or, with only one parameter (VARIABLE(x)) returns the
 * variable value from the global symbol table.
 *
 * The parameters (accessed via arglist[]) are name_interpret() nodes whose
 * string_part() contain the relevant parameter.
 *
 * A procedure entry (in symtab world) has a value member that points to a
 * procedure_interpret() node whose type is TYPE_SYMBOL and whose
 * symbol_part() points to that procedure's symbol table.
 * The sub[0] member points to the formal parameter list, sub[1] to the
 * procedure body, etc. lookup_symbol() will return the procedure_interpret()
 * node if it can find the named procedure.
 * If the procedure has been executed the local variables and parameters
 * will be in the symbol_part() table. The routine
 * lookup_procvariable_value(procnode, varname) will look up the variable name
 * and return its corresponding value (including the case where the variable
 * is a parameter)
 * OR
 * return an exception if the variable is not defined, or is defined as a
 * parameter passed as a variable that is not defined.
 *
 ******************************************************************************
 */
static value
func_variable(void)
{
    value val;
    node *proc, *arg0, *arg1;

    if (isexc(val = nargs_in_range(1,2)))
	return val;
    if (nargs == 2) {		/* Get procedure name */
	arg0 = arglist[0];
	arg1 = arglist[1];
	while (arg0->interpret == paren_interpret)
	    arg0 = arg0->sub[0];
	if (arg0->interpret != name_interpret ||
	    !(proc = lookup_symbol(string_part(arg0->val), SYM_PROC)))
	    return exception("PROCERR  Unknown procedure name in VARIABLE()");
    } else {
	proc = NODENULL;
	arg1 = arglist[0];
    }
/* Variable name */
    while (arg1->interpret == paren_interpret)
	arg1 = arg1->sub[0];
    if (arg1->interpret != name_interpret)
	return exception(
		"ASSNOTVAR  2nd parameter to VARIABLE() is not a name");
    return (lookup_procvariable_value(proc, string_part(arg1->val)));
}

/******************************************************************************
 *
 *	F U N C _ R A N D O M (void)
 *
 * RANDOM(I) - returns a random number between 0 and 1 from seed I.
 *
 * I must be a variable, ie a name_interpret() node with string_part()
 * set to the variables identity.
 * lookup_variable_value() will return the variables value or an exception if
 * not defined.
 * assign_helper(pointer to name_interpret node, value) (interp.c) will perform
 * the assignment of the random number.
 *
 ******************************************************************************
 */
static value
func_random(void)
{
    unsigned seed;
    value val, val1;
    extern value assign_helper(node *, value);
    node *arg0;

    if (isexc(val = nargs_should_be(1)))
	return val;
    arg0 = arglist[0];
    while (arg0->interpret == paren_interpret)
	arg0 = arg0->sub[0];
    if (arg0->interpret != name_interpret)
	return exception(
	   "ASSNOTVAR  RANDOM() - assignment to non-variable parameter");
    if (isexc(val = lookup_variable_value(string_part(arg0->val))))
	seed = 2078934344;
    else {
	if (isexc(val1 = integer_val(val)))
	    return val1;
	seed = (unsigned) (integer_part(val1));
    }
    seed = (seed >> 8) & 0xffff;
    seed *= seed;
    seed += INT_MAX;
    if (isexc(val = assign_helper(arg0, value_integer((int) seed))))
	return val;
    return value_real(fabs(((double) (seed) / (double) ((unsigned) UINT_MAX))));
}

/******************************************************************************
 *
 *	M I N _ M A X (int func)
 *
 * A general function returning either the minimun or maximum (func = FUNC_MIN
 * or FUNC_MAX) of a series of arguments.
 *
 * The arguments are in arglist[i], i = 0,1,2..., a count is in nargs (set up
 * by get_args()).  All arguments can be integer or real.
 *
 * The smart bit in this is the use of
 *   (func == FUNC_MAX) == (result of currentmin/max < next arg)
 *   If func is FUNC_MAX then currentmax is kept in val and
 *                            changes if max IS < next arg
 *   If func is FUNC_MIN then currentmin is kept in val and
 *                            changes if min ISNOT < next arg
 *
 ******************************************************************************
 */
static value
min_max(int func)
{
    int i;
    value val;
    node **args;

    if (nargs < 2)
	return exception("TOOFEWPARS  Not enough parameters to MIN()/MAX()");
    if ((args = (node **) (malloc(sizeof(node *) * nargs))) == ((node **) 0))
	return exception(
	   "SYSERR  memory exhausted during MIN()/MAX()");
    for (i = 0; i < nargs; ++i)
	args[i] = arglist[i];
    if (isexc(val = interpret_to_numeric(args[0])))
	return exception("INVARGMAT  Invalid argument to MIN()/MAX()");
    for (i = 1; i < nargs; i++) {
	value val2;

	if (isexc(val2 = interpret_to_numeric(args[i])))
	    return exception("INVARGMAT  Invalid argument to MIN()/MAX()");
	if ((func == FUNC_MAX) ==
	    ((!isrealtype(val) && !isrealtype(val2)) ?
	     integer_part(val) < integer_part(val2) :
	     (isintegertype(val) ? integer_part(val) : real_part(val)) <
	     (isintegertype(val2) ? integer_part(val2) : real_part(val2))))
	    val = val2;
    }
    (void) free(args);
    return val;
}

/******************************************************************************
 *
 *	F U N C _ M I N (void)
 *
 * MIN(x1,x2...) the minimum of at least two arguments.
 *
 ******************************************************************************
 */
static value
func_min(void)
{
    return min_max(FUNC_MIN);
}

/******************************************************************************
 *
 *	F U N C _ M A X (void)
 *
 * MAX(x1,x2...) the maximum of at least two arguments.
 *
 ******************************************************************************
 */
static value
func_max(void)
{
    return min_max(FUNC_MAX);
}

/******************************************************************************
 *
 *	F U N C _ B I N (void)
 *
 * BIN(I,n,m) converts the integer I into binary  with an 'n' char string
 * result with 'm' digits.
 *
 ******************************************************************************
 */
static value
func_bin(void)
{
    value val, arg, arg1, arg2;
    int num, width, sig, m, maxbuf;
    char *w;
    node *aarg1, *aarg2, *aarg3;

    aarg1 = arglist[0];
    aarg2 = arglist[1];
    aarg3 = arglist[2];
    if (isexc(val = nargs_in_range(1, 3)))
	return val;
    if (isexc(arg = interpret_to_integer(aarg1)))
	return arg;
    num = integer_part(arg);
    m = sizeof(int) * 8;
    if (m > ICL_BUFSIZE)
	maxbuf = m;
    else
	maxbuf = ICL_BUFSIZE;
    if (nargs >= 2) {
	if (isexc(arg1 = interpret_to_integer(aarg2)))
	    return arg1;
	width = integer_part(arg1);
	if (width > maxbuf)
	    width = maxbuf;
	if (nargs == 3) {
	    if (isexc(arg2 = interpret_to_integer(aarg3)))
		return arg2;
	    sig = integer_part(arg1);
	    if (sig > width)
		sig = width;
	} else
	    sig = width;
    } else {
	width = m;
	sig = m;
    }
    if ((w = bin(num, width, sig)) == CHARNIL)
	return exception(
	    "SYSERR  memory exhausted during BIN()");
    else
	return value_string(w);
}

/******************************************************************************
 *
 *	H E X D E C O C T (int func)
 *
 * HEX/DEC/OCT(I,n,m) converts the integer I into hex, decimal or octal with
 * an 'n' char string result with 'm' significant digits.
 *
 * The func argument specifies which format (FUNC_HEX, FUNC_DEC, FUNC_OCT).
 *
 ******************************************************************************
 */
static value
hexdecoct(int func)
{
    value val, arg, arg1, arg2;
    int num, width, sig, m, maxbuf, j;
    char *buf, *buf1;
    node *aarg1, *aarg2, *aarg3;
    width = 0;
    sig = 0;

    aarg1 = arglist[0];
    aarg2 = arglist[1];
    aarg3 = arglist[2];
    if (isexc(val = nargs_in_range(1, 3)))
	return val;
    if (isexc(arg = interpret_to_integer(aarg1)))
	return arg;
    num = integer_part(arg);
    m = sizeof(int) * 8;
    if (m > ICL_BUFSIZE)
	maxbuf = m;
    else
	maxbuf = ICL_BUFSIZE;
    if (nargs >= 2) {
	if (isexc(arg1 = interpret_to_integer(aarg2)))
	    return arg1;
	width = integer_part(arg1);
	if (width > maxbuf)
	    width = maxbuf;
	if (nargs == 3) {
	    if (isexc(arg2 = interpret_to_integer(aarg3)))
		return arg2;
	    sig = integer_part(arg2);
	    if (sig > width)
		sig = width;
	} else
	    sig = width;
    } else {	/* Set width and sig to the maximum required on this host  */
	switch (func) {
	  case FUNC_HEX:
	    width = m / 4;
	    if (m % 4 != 0)
		width++;
	    sig = width;
	    break;
	  case FUNC_DEC:
	    {
		char *buf2;

		if ((buf2 = (char *) malloc((unsigned) maxbuf + 1)) == CHARNIL)
		    return exception(
		       "SYSERR  memory exhausted during HEX(), DEC() or OCT()");
		sprintf(buf2, "%d", INT_MIN);
		width = strlen(buf2);
		sig = width;
		free(buf2);
		break;
	    }
	  case FUNC_OCT:
	    width = m / 3;
	    if (m % 3 != 0)
		width++;
	    sig = width;
	    break;
	/* default: */
	}
    }
/*
 * Convert the number into a buffer with leading zeros
 */
    if ((buf = (char *) malloc((unsigned) maxbuf + 1)) == CHARNIL)
	return exception(
	    "SYSERR  memory exhausted during HEX(), DEC() or OCT()");
    switch (func) {
      case FUNC_HEX:
	sprintf(buf, "%0*X", width, num);
	break;
      case FUNC_DEC:
	sprintf(buf, "%0*d", width, num);
	break;
      case FUNC_OCT:
	sprintf(buf, "%0*o", width, num);
	break;
    }
/*
 * We have a string with leading zeros. First of all make it width chars long
 * (adding leading zeros if need-be) otherwise left truncating
 */
    if ((buf1 = make_width(buf, strlen(buf), width)) == CHARNIL) {
	(void) free(buf);
	return exception(
	    "SYSERR  memory exhausted during HEX(), DEC() or OCT()");
    }
    (void) free(buf);

/* Count number of digits after leading non-zero */

    j = non_lead_zero_count(buf1);

/*
 * if the user has asked to see sig digits and we have j and sig > j then
 * replace (width-sig) leading zeros with spaces otherwise we can strip all
 * leading zeros causing the user to see all digits other than leading zeros
 */

    if (sig > j)
	return value_string(strip_zeros(buf1, width - sig));
    else
	return value_string(strip_all_zeros(buf1));
}

/******************************************************************************
 *
 *	F U N C _ H E X (void)
 *
 ******************************************************************************
 */
static value
func_hex(void)
{
    return hexdecoct(FUNC_HEX);
}

/******************************************************************************
 *
 *	F U N C _ D E C (void)
 *
 ******************************************************************************
 */
static value
func_dec(void)
{
    return hexdecoct(FUNC_DEC);
}

/******************************************************************************
 *
 *	F U N C _ O C T (void)
 *
 ******************************************************************************
 */
static value
func_oct(void)
{
    return hexdecoct(FUNC_OCT);
}

/******************************************************************************
 *
 *	I N I T _ F U N C T I O N S (void)
 *
 * Establish entries for all the ICL function routines in the symbol table.
 * Called from main() during ICL initialisation.
 *
 ******************************************************************************
 */
value
init_functions(void)
{
#include "node.h"
    value val;

    if ((isexc(val = store_symbol("REAL", SYM_FUNCTION,
				node_unary_numeric_func(func_real))))	||
	(isexc(val = store_symbol("SIN", SYM_FUNCTION,
				node_unary_numeric_func(func_sin))))	||
	(isexc(val = store_symbol("COS", SYM_FUNCTION,
				node_unary_numeric_func(func_cos))))	||
	(isexc(val = store_symbol("TAN", SYM_FUNCTION,
				node_unary_numeric_func(func_tan))))	||
	(isexc(val = store_symbol("ATAN", SYM_FUNCTION,
				node_unary_numeric_func(func_atan))))	||
	(isexc(val = store_symbol("EXP", SYM_FUNCTION,
				node_unary_numeric_func(func_exp))))	||
	(isexc(val = store_symbol("SINH", SYM_FUNCTION,
				node_unary_numeric_func(func_sinh))))	||
	(isexc(val = store_symbol("COSH", SYM_FUNCTION,
				node_unary_numeric_func(func_cosh))))	||
	(isexc(val = store_symbol("TANH", SYM_FUNCTION,
				node_unary_numeric_func(func_tanh))))	||
	(isexc(val = store_symbol("SIND", SYM_FUNCTION,
				node_unary_numeric_func(func_sind))))	||
	(isexc(val = store_symbol("COSD", SYM_FUNCTION,
				node_unary_numeric_func(func_cosd))))	||
	(isexc(val = store_symbol("TAND", SYM_FUNCTION,
				node_unary_numeric_func(func_tand))))	||
	(isexc(val = store_symbol("ATAND", SYM_FUNCTION,
				node_unary_numeric_func(func_atand))))	||
	(isexc(val = store_symbol("SQRT", SYM_FUNCTION,
				node_unary_numeric_func(func_sqrt))))	||
	(isexc(val = store_symbol("ABS", SYM_FUNCTION,
				node_unary_numeric_func(func_abs))))	||
	(isexc(val = store_symbol("LOG", SYM_FUNCTION,
				node_unary_numeric_func(func_log))))	||
	(isexc(val = store_symbol("LOG10", SYM_FUNCTION,
				node_unary_numeric_func(func_log10))))	||
	(isexc(val = store_symbol("NINT", SYM_FUNCTION,
				node_unary_numeric_func(func_nint))))	||
	(isexc(val = store_symbol("INTEGER", SYM_FUNCTION,
				node_unary_numeric_func(func_nint))))	||
	(isexc(val = store_symbol("INT", SYM_FUNCTION,
				node_unary_numeric_func(func_int))))	||
	(isexc(val = store_symbol("IFIX", SYM_FUNCTION,
				node_unary_numeric_func(func_int))))	||
	(isexc(val = store_symbol("ASIN", SYM_FUNCTION,
				node_unary_numeric_func(func_asin))))	||
	(isexc(val = store_symbol("ACOS", SYM_FUNCTION,
				node_unary_numeric_func(func_acos))))	||
	(isexc(val = store_symbol("ASIND", SYM_FUNCTION,
				node_unary_numeric_func(func_asind))))	||
	(isexc(val = store_symbol("ACOSD", SYM_FUNCTION,
				node_unary_numeric_func(func_acosd))))	||
	(isexc(val = store_symbol("FLOAT", SYM_FUNCTION,
				node_unary_numeric_func(func_float))))	||
	(isexc(val = store_symbol("ATAN2", SYM_FUNCTION,
				node_binary_numeric_func(func_atan2))))	||
	(isexc(val = store_symbol("ATAN2D", SYM_FUNCTION,
				node_binary_numeric_func(func_atan2d)))) ||
	(isexc(val = store_symbol("DIM", SYM_FUNCTION,
				node_binary_numeric_func(func_dim))))	||
	(isexc(val = store_symbol("SIGN", SYM_FUNCTION,
				node_binary_numeric_func(func_sign))))	||
	(isexc(val = store_symbol("MOD", SYM_FUNCTION,
				node_binary_numeric_func(func_mod))))	||
	(isexc(val = store_symbol("IAND", SYM_FUNCTION,
				node_binary_integer_func(func_iand))))	||
	(isexc(val = store_symbol("IOR", SYM_FUNCTION,
				node_binary_integer_func(func_ior))))	||
	(isexc(val = store_symbol("IEOR", SYM_FUNCTION,
				node_binary_integer_func(func_ieor))))	||
	(isexc(val = store_symbol("INOT", SYM_FUNCTION,
				node_unary_integer_func(func_inot))))	||
	(isexc(val = store_symbol("UPCASE", SYM_FUNCTION,
				node_unary_string_func(func_upcase))))	||
	(isexc(val = store_symbol("ICHAR", SYM_FUNCTION,
				node_unary_string_func(func_ichar))))	||
	(isexc(val = store_symbol("LEN", SYM_FUNCTION,
				node_unary_string_func(func_len))))	||
	(isexc(val = store_symbol("DECL", SYM_FUNCTION,
				node_unary_string_func(func_decl))))	||
	(isexc(val = store_symbol("RA", SYM_FUNCTION,
				node_unary_string_func(func_ra))))	||
	(isexc(val = store_symbol("KEYVAL", SYM_FUNCTION,
				node_unary_string_func(func_keyval))))	||
	(isexc(val = store_symbol("LGE", SYM_FUNCTION,
				node_binary_string_func(func_lge))))	||
	(isexc(val = store_symbol("LGT", SYM_FUNCTION,
				node_binary_string_func(func_lgt))))	||
	(isexc(val = store_symbol("LLE", SYM_FUNCTION,
				node_binary_string_func(func_lle))))	||
	(isexc(val = store_symbol("LLT", SYM_FUNCTION,
				node_binary_string_func(func_llt)))) 	||
	(isexc(val = store_symbol("LEQ", SYM_FUNCTION,
				node_binary_string_func(func_leq))))	||
	(isexc(val = store_symbol("LNE", SYM_FUNCTION,
				node_binary_string_func(func_lne))))	||
	(isexc(val = store_symbol("INDEX", SYM_FUNCTION,
				node_binary_string_func(func_index))))	||
	(isexc(val = store_symbol("LOGICAL", SYM_FUNCTION,
				node_func(func_logical))))		||
	(isexc(val = store_symbol("BIN", SYM_FUNCTION,
				node_func(func_bin))))			||
	(isexc(val = store_symbol("DEC", SYM_FUNCTION,
				node_func(func_dec))))			||
	(isexc(val = store_symbol("OCT", SYM_FUNCTION,
				node_func(func_oct))))			||
	(isexc(val = store_symbol("HEX", SYM_FUNCTION,
				node_func(func_hex))))			||
	(isexc(val = store_symbol("VARIABLE", SYM_FUNCTION,
				node_func(func_variable))))		||
	(isexc(val = store_symbol("SUBSTR", SYM_FUNCTION,
				node_func(func_substr))))		||
	(isexc(val = store_symbol("SNAME", SYM_FUNCTION,
				node_func(func_sname))))		||
	(isexc(val = store_symbol("CHAR", SYM_FUNCTION,
				node_func(func_char))))			||
	(isexc(val = store_symbol("TYPE", SYM_FUNCTION,
				node_func(func_type))))			||
	(isexc(val = store_symbol("ELEMENT", SYM_FUNCTION,
				node_func(func_element))))		||
	(isexc(val = store_symbol("UNDEFINED", SYM_FUNCTION,
				node_func(func_undefined))))		||
	(isexc(val = store_symbol("STRING", SYM_FUNCTION,
				node_func(func_string))))		||
	(isexc(val = store_symbol("RANDOM", SYM_FUNCTION,
				node_func(func_random))))		||
	(isexc(val = store_symbol("MAX", SYM_FUNCTION,
				node_func(func_max))))			||
	(isexc(val = store_symbol("MIN", SYM_FUNCTION,
				node_func(func_min))))			||
	(isexc(val = store_symbol("VERSION", SYM_FUNCTION,
				node_func(func_version))))		||
	(isexc(val = store_symbol("OS", SYM_FUNCTION,
				node_func(func_os))))			||
	(isexc(val = store_symbol("INKEY", SYM_FUNCTION,
				node_func(func_inkey))))		||
	(isexc(val = store_symbol("DEC2S", SYM_FUNCTION,
				node_func(func_dec2s))))		||
	(isexc(val = store_symbol("RA2S", SYM_FUNCTION,
				node_func(func_ra2s))))			||
	(isexc(val = store_symbol("SYSERR", SYM_FUNCTION,
				node_func(func_syserr)))))
	return (val);
    else
	return (trueval);
}
