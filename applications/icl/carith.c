/******************************************************************************
 *
 *	CARITH.C - ICL arithmetic routines.
 *
 * This file contains routines for performing integer and real arithmetic
 * in the C world and returning a value in the ICL world.
 *
 *	History
 *	Created :	S.K.Robinson	04/12/91
 *	Edited  :	S.K.Robinson    28/4/92
 *			To correct error in integer_power()
 *			and to add init_arith() to calculate ICL_MIN_INT
 *			I.R.Jenkins	02/06/92
 *			Added conditional prototyping headers to functions
 *			Added Sccsid for use in version management
 *	Tidied and reformatted :
 *			B.K.McIlwrath	21/07/93 + 16/11/93
 *      Cater for relop on strings: A.J.Chipperfield  19/1/94
 *
 ******************************************************************************
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#include <string.h>
#include <limits.h>
#include <setjmp.h>
#include "icl.h"
#include "carith.h"
#include "parse.h"

extern void systemfail(char *mess);				/* output.c */
#define same_sign(a,b) (((a) < 0) == ((b) < 0))

static int ICL_MIN_INT;

value radianstodegreesval, degreestoradiansval;

/******************************************************************************
 *
 *	I N T E G E R _ M U L T I P L Y (int a, int b)
 *
 * Integer Multiply of a and b;  Checks for overflow by checking that the
 * product divided by either a or b gives b or a respectively; By checking
 * for zeros divide explicitly this cannot overflow.
 *
 ******************************************************************************
 */
static value
integer_multiply(int a, int b)
{
    int k;

    if (a == 0 || b == 0)
	return (value_integer(0));
    else {
	k = a * b;
	if (k / b != a || k / a != b)
	    return (exception("INTOVF  Integer overflow in *"));
	else
	    return (value_integer(k));
    }
}

/******************************************************************************
 *
 *	I N T E G E R _ D I V I D E (int a, int b)
 *
 * Integer Divide of a by b ;  Checks for division by zero.
 *
 * The only other case of worry is when b is -1 and a is ICL_MIN_INT
 * (calculated in init_arith()) at initialisation time) which would give us
 * INT_MAXT+1 as a result.
 *
 ******************************************************************************
 */
static value
integer_divide(int a, int b)
{
    if (b == 0)
	return (exception("INTDIV  Integer division by zero"));
    else if (b == (-1))
	if (a == ICL_MIN_INT)
	    return (exception("INTOVF  Integer overflow in /"));
	else
	    return (value_integer(-a));	/* cannot overflow */
    else {
	if( a%b == 0)
	    return (value_integer(a / b) );
	else
	    return (value_real( (float) a / (float) b ) );
    }
}

/******************************************************************************
 *
 *	I N T E G E R _ A D D (int a, int b)
 *
 * Integer add of a and b.
 *
 * With add when the operands are of differing signs no overflow can occur.
 * With same signs
 *   +ve + +ve  Crucial case is 01111111111 + 0000000001 => 100000000000
 *              ie a change of sign occurs when it overflows
 *		Worst case is 01111111 + 011111111 => 111111110
 *              ie a change of sign occurs when it overflows
 *		Anything in between therefore changes sign.
 *   -ve + -ve  Crucial case is 100000000 + 11111111111 => 011111111111
 *              ie a change of sign occurs when it overflows
 *		Worst case is 100000 + 1000000 => 00000000
 *              ie a change of sign occurs on overflow
 *		Anything in between therefore changes sign.
 *
 ******************************************************************************
 */
static value
integer_add(int a, int b)
{
    int k;

    if (same_sign(a, b)) {
	k = a + b;
    /* Note the use of 'a' below.  It could equally well be 'b' */
	if (!(same_sign(k, a)))
	    return (exception("INTOVF  Integer overflow in +"));
	else
	    return (value_integer(k));
    } else
	return (value_integer(a + b));
}

/******************************************************************************
 *
 *	I N T E G E R _ S U B T R A C T (int a, int b)
 *
 * Integer subtract of b from a.
 *
 * With subtract when the operands are of the same sign no overflow can occur
 * as we will be in effect adding a positive to a negative (-ve - -ve)
 * or subtracting a positive from a positive (+ve - +ve)
 * With differing signs
 *   +ve - -ve  Crucial case is 0111111 - 1111111
 *			   is 0111111 + 000000000 + 1  (2's complement)
 *			   is 1000000
 *		Worst case is 011111 - 100000
 *                         is 011111 + 011111 + 1  (2's compliment)
 *                         is 111111
 *              ie a change of sign with first operand  occurs when it overflows
 *   -ve - +ve  Crucial case is 1000000 - 000000001
 *			   is 1000000 + 1111111110 +1  (2's complement)
 *			   is 0111111111
 *		Worst case is 100000 - 011111
 *                         is 100000 + 100000 + 1
 *                         is 000001
 *              ie a change of sign occurs with first operand on overflow
 *
 ******************************************************************************
 */
static value
integer_subtract(int a, int b)
{
    int k;

    if (same_sign(a, b))
	return (value_integer(a - b));
    else {
	k = a - b;
    /* Note the use of 'a' in this test as the first operand of subtract */
	if (!same_sign(k, a))
	    return (exception("INTOVF  Integer overflow in -"));
	else
	    return (value_integer(k));
    }
}

/******************************************************************************
 *
 *	I N T E G E R _ N E G A T E (int a)
 *
 * When negating an integer the host can only overflow if we start with
 * ICL_MIN_INT as -(ICL_MIN_INT) = - ( - MAX_INT -1) = MAX_INT + 1.
 *
 ******************************************************************************
 */
static value
integer_negate(int a)
{
    if (a == ICL_MIN_INT)
	return (exception("INTOVF Integer Overflow in negate"));
    else
	return (value_integer(-a));
}

/******************************************************************************
 *
 *	I N T E G E R _ P O W E R (int a, int b)
 *
 * Calculates a to the power b.
 *
 * Integer arithmetic has to be checked for overflow and so we use
 * integer_multiply() to multiply a by itself b times.
 *
 * Special Cases
 * ____________
 *
 * If b < 0 then we return zero (as x^-m = 1/x^m which is not an int ) unless
 * a is zero as well which is a singularity.
 * Otherwise if a is zero, then if b is zero we have a domain error and if
 * b is non zero we return zero.
 * Otherwise, anything else to power zero is one.
 *
 * Once all these special cases are dealt with we can use interger_multiply()
 * to deliver the result taking care of overflow.
 *
 ******************************************************************************
 */
static value
integer_power(int a, int b)
{
    value val;
    int p = a;

    if (b < 0)
	if (a == 0)
	    return exception("UNDEXP Singularity in Integer Exponentiation");
	else
	    return value_integer(0);
    else if (a == 0)
	if (b == 0)
	    return exception("UNDEXP  Undefined Integer Exponentiation");
	else
	    return (value_integer(0));
    else if (b == 0)
	return (value_integer(1));
    else {
	while ((--b) != 0) {
	    if (isexc(val = integer_multiply(p, a)))
		return (exception("INTOVF  Integer overflow in **"));
	    p = integer_part(val);
	}
	return value_integer(p);
    }
}

/******************************************************************************
 *
 *	D O R E A L O P (int op, double l, double r)
 *
 * Perform a double precision binary operation 'op' on the ICL values l and r
 * (left operand and right operand).
 *
 * 'op' can be POWER (l**r), MULTIPLY (l*r), ADD(l+r), SUBTRACT (l-r)
 * or DIVIDE (l/r).  The function does not address overflow as this is
 * handled by its caller do_operator().  It returns the ICL value result or
 * an exception (0**0 or 0**(-n)) or l/0).
 * This function aborts ICL if op is not one of the expected values.
 *
 * pow(double x, double r) returns x**y.  pow(x,0.0) is 1 for all x. The
 * function handles exceptional arguments in the spirit of ANSI/IEEE Std
 * 754-1985.  In addition it may also set errno and call matherr().
 *
 * The SVID (System V Interface Definition) specifies that certain libm
 * functions call matherr() when Unix exceptions are detected.  Users may
 * define their own mechanisms for handling Unix exceptions, by including a
 * function named matherr() in their programs.  When an Unix exception occurs,
 * a pointer to the structure exception will be passed to the user supplied
 * matherr() function.  This structure, defined in math.h, is as follows:
 *
 *  struct exception {
 *                   int type; char *name; double arg1, arg2, retval;
 *                   }
 *
 * The element type is an integer describing the type of the exception and can
 * be DOMAIN, SING, OVERFLOW, UNDERFLOW.  The element name will point to a
 * string naming the function that incurred the error. The elements arg1 and
 * arg2 are the calling values of that function and retval is set to the
 * default value that will be returned by the function unless matherr() sets it
 * otherwise. If the user supplied matherr() returns non-zero, no Unix
 * exception messages will be printed and the global system owned variable
 * errno will not be set.  If no user supplied function is available the
 * system supplied function will be invoked which will:
 *
 * DOMAIN	NaN returned; errno set to EDOM and message printed on stderr.
 *		Pow(0.0,0.0) and atan(0.0,0.0) return default results but set
 *		errno and print the message.
 * SING		An IEEE infinity returned; errno set to EDOM and message
 *		printed on stderr.
 * OVERFLOW	In default rounding mode (As used in ICL), an IEEE infinity
 *		of appropriate sign is returned; errno set to ERANGE.
 * UNDERFLOW	An appropriately signed zero, subnormal number or smallest
*		normalised number is returned. errno is set to ERANGE.
 *
 * Specifically (entry is M: message printed, NaN: IEEE NaN result and
 *		invalid operation exception; INF : IEEE infinity result and
 *		division by zero exception; IEEE OVF : IEEE Overflow result
 *		and exception; IEEE UND : IEEE Underflow result and exception;
 *		PI : Closest machine representation to pi.
 *
 *		DOMAIN		SING		OVERFLOW	UNDERFLOW
 *
 * errno set to	EDOM		EDOM		ERANGE		ERANGE
 * Normal op	Invalid op	Div by zero	Overflow	Underflow
 * ACOS,ASIN	M,NaN
 * ATAN2(0,0)	M,+-0.0 or +-PI
 * COSH,SINH					IEEE OVF
 * EXP						IEEE OVF	IEEE UND
 * LOG, LOG10(x<0) M,NaN
 * LOG, LOG10(x=0)  		M,-INF
 * POW
 *   Usual Cases    				IEEE OVF	IEEE UND
 *   (x<0)**(y not int)
 *		M,NaN
 *   0**0	M,1.0
 *   0 ** (y < 0)		M,+-INF
 * SQRT(X<0)	M,NaN
 *
 * As we are calling pow() here and acos(), asin(), atan2() etc. elsewhere
 * we wish to avoid the M effect above (ie prevent the message being
 * printed).  Hence we should (but do not yet!) provide our own matherr()
 * function.
 *
 ******************************************************************************
 */
static value
dorealop(int op, double l, double r)
{
    value val;

    switch (op) {
      case POWER:
	if (l == 0.0 && r <= 0.0)
	    return (exception("UNDEXP  Undefined Exponentiation"));
	else
	    val = (value_real(pow(l, r)));
	break;
      case MULTIPLY:
	val = value_real(l * r);
	break;
      case ADD:
	val = value_real(l + r);
	break;
      case SUBTRACT:
	val = value_real(l - r);
	break;
      case DIVIDE:
	if (r == 0.0)
	    return (exception("FLTDIV  Floating point division by zero"));
	else
	    val = (value_real(l / r));
	break;
      default:
	systemfail("Unknown operator in dorealop()");
	val = realzeroval; /* never reached! */
	break;
    }
    return val;
}

/******************************************************************************
 *
 *		BINARY ARITHMETIC
 *
 * Perform a binary arithmetic opereration in the ICL world.
 *
 * Given an operator code in op, and the operands in left and right (in the
 * form of ICL values) perform the arithmetic operation, handling overflow.
 *
 * If BOTH operands are integer then an integer arithmetic operation is
 * performed. Otherwise a double precision operation is performed.
 *
 * If the host can signal float overflow then the main() function at load time
 * will have told the host that when a floating exception occurs that it
 * should call the routine float_exception() (in main.c) using
 * signal(SIGFPE, float_exception). This routine, if jmp_float_setup is set,
 * will do a longjmp() using the jump buffer jmp_float.
 *
 * To cause it to come back into this do_operator() we call setjmp(jmp_float)
 * (having saved the current jmp location in jmp_save) and set the
 * jmp_float_setup flag just prior to attempting the floating
 * point operation. On this direct call, setjmp() always returns zero (false)
 * and if the floating overflow signal subsequently occurs it will
 * be as if we had just called setjmp() which returns non-zero this time.
 * Hence we have the code
 *	if(setjmp(...))  Handle overflow by generating ICL exception value
 *      else perform the operation delivering ICL value
 * Both jmp_float and jmp_float_setup are reset once the operation is complete
 *
 * Functions called from value.c
 *
 * integer_part(val) will return the C integer value of an integer ICL value.
 * real_part(val) will return the C real value of an real ICL value.
 * isintegertype(val) returns true if the ICL value val is an integer.
 * isrealtype(val) returns true if the ICL value val is a real.
 * real_val(val) will try to deliver an ICL value of type real from any given
 *	ICL value val or return an exception in the attempt.
 *
 * Other functions called
 *
 * finite(x) returns 1 if the double x is zero, normal or subnormal
 * ANSI/IEEE Std 754-1985 floating point number.  !finite(x)
 * indicates an exceptional floating point value (such as NaN or infinity)
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 * 	D O _ O P E R A T O R (left operand, operator code, right operand )
 *
 ******************************************************************************
 */
value
do_operator(value left, int op, value right)
{

    if (isintegertype(left) && isintegertype(right)) {
	int l = integer_part(left), r = integer_part(right);

	switch (op) {
	  case POWER:
	    return integer_power(l, r);
	  case MULTIPLY:
	    return integer_multiply(l, r);
	  case ADD:
	    return integer_add(l, r);
	  case SUBTRACT:
	    return integer_subtract(l, r);
	  case DIVIDE:
	    return integer_divide(l, r);
	  default:
	    systemfail("Unknown operator in do_operator()");
	    return intzeroval;	/* for lint */
	}
    } else {	/* we must do a real operation */
	value val, vall, valr;
	double l, r;

	if (isexc(vall = real_val(left)))
	    return vall;
	if (isexc(valr = real_val(right)))
	    return valr;
	l = real_part(vall);
	r = real_part(valr);
	if (float_can_signal) {
	    jmp_buf jmp_save;
	    extern jmp_buf jmp_float;
	    extern int jmp_float_setup;

	    jmp_float_setup = 1;
	    memcpy(jmp_float, jmp_save, sizeof(jmp_buf));
	    if (setjmp(jmp_float))
		val = exception("FLTOVF  Floating point overflow");
	    else
		val = dorealop(op, l, r);
	    memcpy(jmp_save, jmp_float, sizeof(jmp_buf));
	    jmp_float_setup = 0;
	} else
	    val = dorealop(op, l, r);
	if (isrealtype(val) && !isfinite(real_part(val)))
	    return exception("FLTOVF  Floating point overflow");
	else
	    return val;
    }
}

/******************************************************************************
 *
 *	D O _ R E L O P E R A T O R (value val1, int op, value val2)
 *
 * Calculate expression R E L A T I O N A L expression.
 *
 * 'val1' and 'val2' contain the left and right operands respectively while 'op'
 * is the yacc operation code for the relational operation.
 *
 * Note that one or both of val1 and val2 may be of TYPE_STRING. If BOTH
 * are of type string then the ICL syntax defines that we do string comparison
 * but only for a limited number of valid relational operations.
 *
 * If either (or both) val1 or val2 are not of TYPE_STRING the other is
 * interpreted as using as_nonstring() and arithmetic relational operations
 * are used.
 *
 * Cannot cause overflow
 *
 ******************************************************************************
 */
value
do_reloperator(value val1, int op, value val2)
{
    if (isstringtype(val1) && isstringtype(val2)) {
        char *s1 = string_part(val1), *s2 = string_part(val2);
	strtrim(s1, strlen(s1));
	strtrim(s2, strlen(s2));
	switch (op) {
	  case EQUAL:
	    return
              value_logical(!strcmp(s1, s2));
	  case NOT_EQUAL:
            return
              value_logical(strcmp(s1, s2));
          default:
	    return exception("RELOP Invalid string relational operation");
	}
    } else {
	if(isstringtype(val1))
	    val1 = as_nonstring(val1);
	if(isstringtype(val2))
	    val1 = as_nonstring(val2);
    }

    if ((isintegertype(val1) && isintegertype(val2)) ||
	(islogicaltype(val1) && islogicaltype(val2))) {
	int l = integer_part(val1), r = integer_part(val2);

	switch (op) {
	  case EQUAL:
	    return value_logical(l == r);
	  case LESS_THAN:
	    return value_logical(l < r);
	  case LESS_EQUAL:
	    return value_logical(l <= r);
	  case GREATER_THAN:
	    return value_logical(l > r);
	  case GREATER_EQUAL:
	    return value_logical(l >= r);
	  case NOT_EQUAL:
	    return value_logical(l != r);
	}
    } else if (isrealtype(val1) || isrealtype(val2)) {
	double l = real_part(real_val(val1)), r = real_part(real_val(val2));
	switch (op) {
	  case EQUAL:
	    return value_logical(l == r);
	  case LESS_THAN:
	    return value_logical(l < r);
	  case LESS_EQUAL:
	    return value_logical(l <= r);
	  case GREATER_THAN:
	    return value_logical(l > r);
	  case GREATER_EQUAL:
	    return value_logical(l >= r);
	  case NOT_EQUAL:
	    return value_logical(l != r);
	}
    }
/* what return value should be here ?? */
    return exception("RELOP Illegal relational expression");
}

/******************************************************************************
 *
 *	C A L L _ U N A R Y _ I N T E G E R _ F U N C (value val, int (*fn)())
 *
 * Interpret a built-in function with one integer argument and an integer
 * result which cannot generate an exception.
 *
 * Prior to the call to this function interpret_to_integer() guarantees us an
 * integer value 'val'.
 *
 ******************************************************************************
 */
value
call_unary_integer_func(value val, int (*fn) ())
{
    return (value_integer((*fn) (integer_part(val))));
}

/******************************************************************************
 *
 *	C A L L _ B I N A R Y _ I N T E G E R _ F U N C (value val1,
 *					value val2, int (*fn)( int, int ))
 *
 *
 * Interpret a built-in function of two integer arguments and an integer result
 * which cannot generate an exception.
 *
 * Prior to the call to this function interpret_to_integer() guarantees us
 * integer values for 'val1' and 'val2'.
 *
 ******************************************************************************
 */
value
call_binary_integer_func(value val1, value val2, int (*fn) (int, int))
{
    return (value_integer((*fn) (integer_part(val1), integer_part(val2))));
}

/******************************************************************************
 *
 *	S T R I N G T O I N T V A L U E (char *thestring)
 *
 * Given a string we wish to convert the string in decimal format into an
 * integer ICL value.
 *
 * Care must be taken to ensure that if there is an overflow in the evaluating
 * value that we return an exception
 *
 ******************************************************************************
 */
value
stringtointvalue(char *thestring)
{
    value val;
    char *scan = thestring;
    int ch;

    val = intzeroval;
    while ((ch = (*(scan++))))
	if (ch >= '0' && ch <= '9') {
	    if (isexc(val = integer_multiply(integer_part(val), 10)))
		return (exception("INTOVF Integer Overflow"));
	    if (isexc(val = integer_add(integer_part(val), (ch - '0'))))
		return (exception("INTOVF Integer Overflow"));
	} else
	    return exception("INVLDINT  Invalid Integer ");
    return (val);
}

/******************************************************************************
 *
 *	B A S E D S T R I N G T O I N T V A L U E (char *thestring)
 *
 * Given a string we wish to convert the string in based format into an
 * integer ICL value.
 *
 * The bases are indicated by a prefix on the string of digits
 *        %x or %X for hexadecimal
 *        %b or %B for binary
 *        %o or %O for octal
 *        NONE     for decimal
 *
 * Care must be taken to ensure that if there is an overflow in the evaluating
 * value that we return an exception
 *
 ******************************************************************************
 */
value
basedstringtointvalue(char *thestring)
{
    char *scan;
    int ch;
    value val;

    val = intzeroval;
    scan = thestring;
    if ((*(scan++)) != '%')
	return (stringtointvalue(thestring));
    switch ((*(scan++))) {
      case 'X':
      case 'x':
	while ((ch = (*(scan++))))
	    if (ch >= '0' && ch <= '9') {
		if (isexc(val = integer_multiply(integer_part(val), 16)))
		    return (exception("INTOVF Integer Overflow"));
		if (isexc(val = integer_add(integer_part(val), (ch - '0'))))
		    return (exception("INTOVF Integer Overflow"));
	    } else if (ch >= 'A' && ch <= 'F') {
		if (isexc(val = integer_multiply(integer_part(val), 16)))
		    return (exception("INTOVF Integer Overflow"));
		if (isexc(val = integer_add(integer_part(val),(ch - 'A' + 10))))
		    return (exception("INTOVF Integer Overflow"));
	    } else if (ch >= 'a' && ch <= 'f') {
		if (isexc(val = integer_multiply(integer_part(val), 16)))
		    return (exception("INTOVF Integer Overflow"));
		if (isexc(val = integer_add(integer_part(val), (ch - 'a' + 10))))
		    return (exception("INTOVF Integer Overflow"));
	    } else
		return exception("INVLDINT  Invalid Integer ");
	break;
      case 'B':
      case 'b':
	while ((ch = (*(scan++))))
	    if (ch >= '0' && ch <= '1') {
		if (isexc(val = integer_multiply(integer_part(val), 2)))
		    return (exception("INTOVF Integer Overflow"));
		if (isexc(val = integer_add(integer_part(val), (ch - '0'))))
		    return (exception("INTOVF Integer Overflow"));
	    } else
		return exception("INVLDINT  Invalid Integer ");
	break;
      case 'O':
      case 'o':
	while ((ch = (*(scan++))))
	    if (ch >= '0' && ch <= '7') {
		if (isexc(val = integer_multiply(integer_part(val), 8)))
		    return (exception("INTOVF Integer Overflow"));
		if (isexc(val = integer_add(integer_part(val), (ch - '0'))))
		    return (exception("INTOVF Integer Overflow"));
	    } else
		return exception("INVLDINT  Invalid Integer ");
	break;
      default:
	return exception("INVLDINT  Invalid Integer ");
    }
    return val;
}

/******************************************************************************
 *
 *	F U N C _ A B S (value arg)
 *
 * Provide abs() but detecting overflow due to taking abs() of largest
 * negative integer value; The standard abs cannot overflow (fabs())
 *
 ******************************************************************************
 */
value
func_abs(value arg)
{
    int iarg;

    if (isintegertype(arg)) {
	iarg = integer_part(arg);
	if (iarg == ICL_MIN_INT)
	    return (exception("INTOVFL  Integer Overflow "));
	else
	    return (value_integer(abs(iarg)));
    } else
	return (value_real(fabs(real_part(arg))));
}

/******************************************************************************
 *
 *	F U N C _ N I N T (value arg)
 *
 * Provide fortran type nint() detecting overflow due to taking nint() of
 * a real that is out-of-range for an integer.
 *
 * If given an integer numeric we return it.
 *
 ******************************************************************************
 */
value
func_nint(value arg)
{
    int ival;
    double rval;

    if (isintegertype(arg))
	return arg;
    rval = real_part(arg);

/* Ultrix and Linux do not have a nint() function, so we use (int)rint() */

#if HAVE_NINT
    ival = nint(rval);
#elif HAVE_RINT
    ival = (int) rint(rval);
#else
# error "Can not take nearest int"
#endif

    if (fabs((double) ival - rval) < 1.0)
	return (value_integer(ival));
    else
	return (exception(
		"INTOVFL Integer Overflow whilst converting from real"));
}

/******************************************************************************
 *
 * Provide functions that perform numeric to integer conversion.
 * In the case of real to int handle overflow exception.
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 *	F U N C _ I N T (value arg)
 *
 ******************************************************************************
 */
value
func_int(value arg)
{
    int ival;
    double rval;

    if (isintegertype(arg))
	return arg;
    rval = real_part(arg);
    ival = (int) (rval);
    if (fabs((double) ival - rval) < 1.0)
	return (value_integer(ival));
    else
	return (exception(
		"INTOVFL Integer Overflow whilst converting from real"));
}

/******************************************************************************
 *
 *	F U N C _ I F I X (value arg)
 *
 ******************************************************************************
 */
value
func_ifix(value arg)
{
    int ival;

    if (isintegertype(arg))
	return arg;
    ival = (int) (real_part(arg));
    if (fabs((double) ival - real_part(arg)) < 1.0)
	return (value_integer(ival));
    else
	return (exception(
		"INTOVFL Integer Overflow whilst converting from real"));
}

/******************************************************************************
 *
 *	F U N C _ A S I N (value arg)
 *
 * Provide the function asin() in the ICL world detecting the exception when
 * the argument is not between -1 and 1.
 *
 ******************************************************************************
 */
value
func_asin(value arg)
{
    int ival;
    double dval;

    if (isintegertype(arg)) {
	ival = integer_part(arg);
	if (ival < -1 || ival > 1)
	    return (exception("INVARGMAT |Argument| to asin  greater than 1"));
	dval = (double) ival;
    } else {
	dval = real_part(arg);
	if (dval < -1.0 || dval > 1.0)
	    return (exception("INVARGMAT |Argument| to asin  greater than 1"));
    }
    return (value_real(asin(dval)));
}

/******************************************************************************
 *
 *	F U N C _ A C O S (value arg)
 *
 * Provide the function acos() in the ICL world detecting the exception when
 * the argument is not between -1 and 1.
 *
 ******************************************************************************
 */
value
func_acos(value arg)
{
    int ival;
    double dval;

    if (isintegertype(arg)) {
	ival = integer_part(arg);
	if (ival < -1 || ival > 1)
	    return (exception("INVARGMAT |Argument| to acos  greater than 1"));
	dval = (double) ival;
    } else {
	dval = real_part(arg);
	if (dval < -1.0 || dval > 1.0)
	    return (exception("INVARGMAT |Argument| to acos  greater than 1"));
    }
    return (value_real(acos(dval)));
}

/******************************************************************************
 *
 *	F U N C _ A S I N D (value arg)
 *
 * Provide the function asind() in the ICL world detecting the exception when
 * the argument is not between -1 and 1.
 *
 ******************************************************************************
 */
value
func_asind(value arg)
{
    value val;

    if (isexc(val = func_asin(arg)))
	return (exception("INVARGMAT |Argument| to asind  greater than 1"));
    else
	return (do_operator(val, MULTIPLY, radianstodegreesval));
}

/******************************************************************************
 *
 *	F U N C _ A C O S D (value arg)
 *
 * Provide the function asin() in the ICL world detecting the exception when
 * the argument is not between -1 and 1.
 *
 ******************************************************************************
 */
value
func_acosd(value arg)
{
    value val;

    if (isexc(val = func_acos(arg)))
	return (exception("INVARGMAT |Argument| to acosd  greater than 1"));
    return (do_operator(val, MULTIPLY, radianstodegreesval));
}

/******************************************************************************
 *
 *	F U N C _ A T A N 2 (value y, value x)
 *
 * Provide the function atan2() in the ICL world detecting the exception when
 * the arguments are both zero.
 *
 ******************************************************************************
 */
value
func_atan2(value y, value x)
{
    double dval, dval2;

    if (isintegertype(y))
	dval = (double) (integer_part(y));
    else
	dval = real_part(y);
    if (isintegertype(x))
	dval2 = (double) (integer_part(x));
    else
	dval2 = real_part(x);
    if (dval == 0.0 && dval2 == 0.0)
	return (exception("INVARGMAT Domain error in atan2"));
    else
	return (value_real(atan2(dval, dval2)));
}

/******************************************************************************
 *
 *	F U N C _ A T A N 2 D (value y, value x)
 *
 * Provide the function atan2d() in the ICL world detecting the exception when
 * the arguments are both zero.
 *
 ******************************************************************************
 */
value
func_atan2d(value y, value x)
{
    value val;

    if (isexc(val = func_atan2(y, x)))
	return (exception("INVARGMAT Domain error in atan2d"));
    else
	return (do_operator(val, MULTIPLY, radianstodegreesval));
}

/******************************************************************************
 *
 *	F U N C _ L O G (value arg)
 *
 * Provide the function log(x) trapping the exception of log(x)<0.
 *
 * The argument x by this time is guaranteed to be real or integer by the use
 * of interpret_to_numeric in unary_numeric_func_interpret().
 *
 ******************************************************************************
 */
value
func_log(value arg)
{
    double darg;

    darg = real_part(numeric_as_real(arg));
    if (darg <= 0.0)
	return exception("LOGZERNEG  Logarithm of zero or negative number");
    else
	return value_real(log(darg));
}

/******************************************************************************
 *
 *	F U N C _ L O G 1 0 (value arg)
 *
 * Provide the function log10(x) trapping the exception of log10<0.
 *
 * The argument x by this time is guaranteed to be real or integer by the use
 * of interpret_to_numeric in unary_numeric_func_interpret().
 *
 ******************************************************************************
 */
value
func_log10(value arg)
{
    double darg;

    darg = real_part(numeric_as_real(arg));
    if (darg <= 0.0)
	return exception("LOGZERNEG  Logarithm of zero or negative number");
    else
	return value_real(log10(darg));
}

/******************************************************************************
 *
 *	F U N C _ S Q R T (value arg)
 *
 * Provide sqrt(x) taking care of the exception of sqare root of a negative.
 *
 * The argument x by this time is guaranteed to be real or integer by the use
 * of interpret_to_numeric in unary_numeric_func_interpret().
 *
 ******************************************************************************
 */
value
func_sqrt(value arg)
{
    double darg;

    darg = real_part(numeric_as_real(arg));
    return (darg < 0.0 ?
	    exception("SQUROONEG  Square root of negative number") :
	    value_real(sqrt(darg)));
}

/******************************************************************************
 *
 *	F U N C _ R E A L (value arg)
 *
 * Real(x) - Returns the value of x converted to real.
 *
 ******************************************************************************
 */
value
func_real (value arg)
{
    double res;
    res = real_part(numeric_as_real(arg));
    if (!isfinite (res))
	return exception ("FLTOVFL  Floating point overflow");
    else
	if (isnan (res))
	    return exception ("INVARGMAT Invalid argument to exp()");
	else
	    return value_real (res);
}

/******************************************************************************
 *
 *	F U N C _ S I N (value arg)
 *
 * Provide sind(x) - x in in radians.
 *
 * The argument x by this time is guarenteed to be real or integer by
 * the use of interpret_to_numeric in unary_numeric_func_interpret()
 *
 * finite(x) returns 1 if the double x is zero, normal or subnormal
 * ANSI/IEEE Std 754-1985 floating point number.  !finite(x)
 * indicates an exceptional floating point value (such as NaN or infinity)
 *
 * isnan(x) returns 1 if the double x is NaN (not-a-number)
 *
 ******************************************************************************
 */
value
func_sin(value arg)
{
    double res;

    res = sin(real_part(numeric_as_real(arg)));
    if (!isfinite(res))
	return exception("FLTOVFL  Floating point overflow");
    else if (isnan(res))
	return exception("INVARGMAT Invalid argument to sin()");
    else
	return value_real(res);
}

/******************************************************************************
 *
 *	F U N C _ C O S (value arg)
 *
 * Provide cosd(x) - x in in radians.
 *
 ******************************************************************************
 */
value
func_cos(value arg)
{
    double res;

    res = cos(real_part(numeric_as_real(arg)));
    if (!isfinite(res))
	return exception("FLTOVFL  Floating point overflow");
    else if (isnan(res))
	return exception("INVARGMAT Invalid argument to cos()");
    else
	return value_real(res);
}

/******************************************************************************
 *
 *	F U N C _ T A N (value arg)
 *
 * Provide tand(x) - x in in radians.
 *
 ******************************************************************************
 */
value
func_tan(value arg)
{
    double res;

    res = tan(real_part(numeric_as_real(arg)));
    if (!isfinite(res))
	return exception("FLTOVFL  Floating point overflow");
    else if (isnan(res))
	return exception("INVARGMAT Invalid argument to tan()");
    else
	return value_real(res);
}

/******************************************************************************
 *
 *	F U N C _ A T A N (value arg)
 *
 * Provide atan(x). Result is in radians.
 *
 ******************************************************************************
 */
value
func_atan(value arg)
{
    double res;

    res = atan(real_part(numeric_as_real(arg)));
    if (!isfinite(res))
	return exception("FLTOVFL  Floating point overflow");
    else if (isnan(res))
	return exception("INVARGMAT Invalid argument to atan()");
    else
	return value_real(res);
}

/******************************************************************************
 *
 *	F U N C _ E X P (value arg)
 *
 * Provide exp(x).
 *
 ******************************************************************************
 */
value
func_exp(value arg)
{
    double res;

    res = exp(real_part(numeric_as_real(arg)));
    if (!isfinite(res))
	return exception("FLTOVFL  Floating point overflow");
    else if (isnan(res))
	return exception("INVARGMAT Invalid argument to exp()");
    else
	return value_real(res);
}

/******************************************************************************
 *
 *	F U N C _ S I N H (value arg)
 *
 * Provide sinh(x). x is in radians.
 *
 ******************************************************************************
 */
value
func_sinh(value arg)
{
    double res;

    res = sinh(real_part(numeric_as_real(arg)));
    if (!isfinite(res))
	return exception("FLTOVFL  Floating point overflow");
    else if (isnan(res))
	return exception("INVARGMAT Invalid argument to sinh()");
    else
	return value_real(res);
}

/******************************************************************************
 *
 *	F U N C _ C O S H (value arg)
 *
 * Provide cosh(x). x is in radians.
 *
 ******************************************************************************
 */
value
func_cosh(value arg)
{
    double res;

    res = cosh(real_part(numeric_as_real(arg)));
    if (!isfinite(res))
	return exception("FLTOVFL  Floating point overflow");
    else if (isnan(res))
	return exception("INVARGMAT Invalid argument to cosh()");
    else
	return value_real(res);
}

/******************************************************************************
 *
 *	F U N C _ T A N H (value arg)
 *
 * Provide tanh(x).
 *
 ******************************************************************************
 */
value
func_tanh(value arg)
{
    double res;

    res = tanh(real_part(numeric_as_real(arg)));
    if (!isfinite(res))
	return exception("FLTOVFL  Floating point overflow");
    else if (isnan(res))
	return exception("INVARGMAT Invalid argument to tanh()");
    else
	return value_real(res);
}

/******************************************************************************
 *
 *	F U N C _ S I N D (value arg)
 *
 * Provide sind(x). x is in degrees.
 *
 ******************************************************************************
 */
value
func_sind(value arg)
{
    value val;
    double res;

    if (isexc(val = do_operator((numeric_as_real(arg)), MULTIPLY,
				degreestoradiansval)))
	return val;
    else
	res = sin(real_part(val));
    if (!isfinite(res))
	return exception("FLTOVFL  Floating point overflow");
    else if (isnan(res))
	return exception("INVARGMAT Invalid argument to sind()");
    else
	return value_real(res);
}

/******************************************************************************
 *
 *	F U N C _ C O S D (value arg)
 *
 * Provide cosd(x). x is in degrees.
 *
 ******************************************************************************
 */
value
func_cosd(value arg)
{
    value val;
    double res;

    if (isexc(val = do_operator((numeric_as_real(arg)), MULTIPLY,
				degreestoradiansval)))
	return val;
    else
	res = cos(real_part(val));
    if (!isfinite(res))
	return exception("FLTOVFL  Floating point overflow");
    else if (isnan(res))
	return exception("INVARGMAT Invalid argument to cosd()");
    else
	return value_real(res);
}

/******************************************************************************
 *
 *	F U N C _ T A N D (value arg)
 *
 * Provide tand(x). x is in degrees.
 *
 ******************************************************************************
 */
value
func_tand(value arg)
{
    value val;
    double res;

    if (isexc(val = do_operator((numeric_as_real(arg)), MULTIPLY,
				degreestoradiansval)))
	return val;
    else
	res = tan(real_part(val));
    if (!isfinite(res))
	return exception("FLTOVFL  Floating point overflow");
    else if (isnan(res))
	return exception("INVARGMAT Invalid argument to tand()");
    else
	return value_real(res);
}

/******************************************************************************
 *
 *	F U N C _ A T A N D (value arg)
 *
 * Provide atand(x). Result is in degrees.
 *
 ******************************************************************************
 */
value
func_atand(value arg)
{

/*
 * return (do_operator(value_real(atan (numeric_as_real(arg))),
 * MULTIPLY,radianstodegreesval));
 */
/* Changed to 'real_part()' by irj, 2/3/93 */
    return (do_operator(value_real(atan(real_part(numeric_as_real(arg)))),
		MULTIPLY, radianstodegreesval));
}

/******************************************************************************
 *
 *	F U N C _ M O D (value left, value right)
 *
 * Provide mod(X1,X2) = x1-int(x1/x2) * x2;  Integer or real args
 *
 ******************************************************************************
 */
value
func_mod(value left, value right)
{
    value val;

    if ((isintegertype(right) && integer_part(right) == 0) ||
		(real_part(right) == 0.0))
	return (exception("INVARGMAT Zero second argument to mod"));
    if (isexc(val = do_operator(left, DIVIDE, right)))
	return val;
    if (isexc(val = func_int(val)))
	return val;
    if (isexc(val = do_operator(val, MULTIPLY, right)))
	return val;
    return (do_operator(left, SUBTRACT, val));
}

/******************************************************************************
 *
 *	F U N C _ D I M (value left, value right)
 *
 * Provide DIM(X1,x2) positive difference - x1-min(x1,x2)
 *
 ******************************************************************************
 */
value
func_dim(value left, value right)
{
    if (isintegertype(left) && isintegertype(right)) {
	int lint = integer_part(left), rint = integer_part(right);

	if (lint <= rint)
	    return intzeroval;
	else
	    return (do_operator(left, SUBTRACT, right));
    } else {
	double lreal, rreal;

	if (isrealtype(left))
	    lreal = real_part(left);
	else
	    lreal = real_part(numeric_as_real(left));
	if (isrealtype(right))
	    rreal = real_part(right);
	else
	    rreal = real_part(numeric_as_real(right));
	if (lreal <= rreal)
	    return (realzeroval);
	else
	    return (do_operator(value_real(lreal), SUBTRACT, value_real(rreal)));
    }
}

/******************************************************************************
 *
 *	F U N C _ S I G N (value left, value right)
 *
 * Provide sign(x1,x2) gives |x1| if x2 >= 0 else -|x1| if x2 < 0.
 *
 ******************************************************************************
 */
value
func_sign(value left, value right)
{
    if (isintegertype(left))
	if ((isintegertype(right) && integer_part(right) >= 0) ||
		(isrealtype(right) && real_part(right) >= 0.0))
	    return (func_abs(left));
	else if (integer_part(left) <= 0)
	    return left;	/* -|x1| = x1 if x1 <= 0 */
	else
	    return (do_operator(intzeroval, SUBTRACT, left));
    else
     /* x1 is real */
	if ((isintegertype(right) && integer_part(right) >= 0) ||
		(isrealtype(right) && real_part(right) >= 0.0))
	return (func_abs(left));
    else if (real_part(left) <= 0.0)
	return left;		/* -|x1| = x1 if x1 <= 0 */
    else
	return (do_operator(realzeroval, SUBTRACT, left));
}

/******************************************************************************
 *
 *	F U N C _ F L O A T (value arg)
 *
 * Provide float(x) - the value of integer x converted to real.
 *
 * interpret_to_numeric() has been used in unary_numeric_func_interpret()
 * to force the argument to a numeric type.  We exception if the argument
 * did not deliver an integer value.
 *
 ******************************************************************************
 */
value
func_float(value arg)
{
    if (isintegertype(arg))
	return (value_real((double) integer_part(arg)));
    else
	return (exception("INVARGMAT Argument to float() not integer"));
}

/******************************************************************************
 *
 *	S T R I N G T O I N T (char **thestringptr)
 *
 * Given a pointer to a pointer to a string we convert the string in decimal
 * format into an integer icl value returning an exception if the integer
 * overflows in evaluation or if the input is in an invalid format.  We return
 * with the string pointer pointing to the character that terminated conversion.
 * The format accepted is {+|-}dd+
 ******************************************************************************
 */
static value
stringtoint(char **thestringptr)
{
    value val;
    char *scan = *thestringptr;
    int ch, sign;

    val = intzeroval;
    sign = 0;
    if ((ch = *scan) == '-') {
	sign = 1;
	scan = scan + 1;
    } else if (ch == '+')
	scan = scan + 1;
    if ((ch = *scan) >= '0' && ch <= '9') {	/* check for first digit */
	val = value_integer(ch - '0');
	scan = scan + 1;
    } else {
	*thestringptr = scan;
	return (exception("INPUTERR : Integer invalid format on input"));
    }
    while ((ch = (*scan)) != '\0')
	if (ch >= '0' && ch <= '9') {
	    scan = scan + 1;
	    if (isexc(val = integer_multiply(integer_part(val), 10)) ||
		isexc(val = integer_add(integer_part(val), (ch - '0')))) {
		while ((ch = (*scan)) != '\0')	/* Overflow so skip remaining
						 * digits */
		    if (ch >= '0' && ch <= '9')
			scan = scan + 1;
		    else
			break;
		*thestringptr = scan;
		return (exception(
			"INPUTERR : INTOVF Integer Overflow during input"));
	    }
	} else
	    break;
    *thestringptr = scan;
    if (sign)			/* we had leading '-'  */
	if (isexc(val = integer_negate(integer_part(val))))
	    return (exception(
			"INPUTERR : INTOVF Integer Overflow during input"));
    return (val);
}

/******************************************************************************
 *
 *	H E X D I G I T ( char ch )
 *
 * A small utility that, given a character ch, checks to see if it is a
 * hexadecimal digit and if so returns its binary value, otherwise
 * returns (-1)
 *
 ******************************************************************************
 */
static int
hexdigit(char ch)
{
    if (ch >= '0' && ch <= '9')
	return (ch - '0');
    else if (ch >= 'A' && ch <= 'F')
	return (ch - 'A' + 10);
    else if (ch >= 'a' && ch <= 'f')
	return (ch - 'a' + 10);
    else
	return (-1);
}

/******************************************************************************
 *
 *     	S T R I N G T O I C L I N T V A L U E (char **thestringptr)
 *
 * Given a pointer to a pointer to a string we convert the string in decimal
 * format into an integer ICL value returning an exception if the integer
 * overflows in evaluation or if the input is in an invalid format.
 *
 * We return with the string pointer pointing to the character that
 * terminated conversion.
 * The bases are indicated by a prefix on the string of digits
 *        %x or %X                    for hexadecimal
 *        %b or %B                    for binary
 *        %o or %O                    for octal
 *        + or - or decimal digit     for decimal
 * Care must be taken to ensure that if there is an overflow in evaluating the
 * value that we return an exception
 *
 ******************************************************************************
 */
value
stringtoiclintvalue(char **thestringptr)
{
    char *scan;
    int ch;
    value val;

    val = intzeroval;
    scan = *thestringptr;
    while ((ch = (*scan)) == ' ' || ch == '\t')	/* skip noise */
	scan = scan + 1;
    *thestringptr = scan;
    if ((*(scan++)) != '%')
	return (stringtoint(thestringptr));
    switch ((*(scan++))) {
      case 'X':
      case 'x':
	ch = *scan;
	if ((ch = hexdigit(ch)) != (-1)) {
	    scan = scan + 1;
	    val = value_integer(ch);
	} else {
	    *thestringptr = scan;
	    return (exception("INPUTERR : Invalid integer input format"));
	}
	while ((ch = *scan) != '\0')
	    if ((ch = hexdigit(ch)) != (-1)) {
		scan = scan + 1;
		if (isexc(val = integer_multiply(integer_part(val), 16)) ||
		    isexc(val = integer_add(integer_part(val), ch))) {
		    while ((ch = *scan) != '\0')  /* overflow so skip
						   * remaining hexdigits */
			if (hexdigit(ch) != (-1))
			    scan = scan + 1;
			else
			    break;
		    *thestringptr = scan;
		    return (exception(
			   "INPUTERR : INTOVF Integer Overflow during input"));
		}
	    } else
		break;
	break;
      case 'B':
      case 'b':
	ch = *scan;
	if (ch >= '0' && ch <= '1') {
	    scan = scan + 1;
	    val = value_integer(ch - '0');
	} else {
	    *thestringptr = scan;
	    return (exception("INPUTERR : Invalid integer input format"));
	}
	while ((ch = *scan) != '\0')
	    if (ch >= '0' && ch <= '1') {
		scan = scan + 1;
		if (isexc(val = integer_multiply(integer_part(val), 2)) ||
		  isexc(val = integer_add(integer_part(val), (ch - '0')))) {
		    while ((ch = *scan) != '\0')	/* overflow so skip
							 * remaining bin
						         * digits */
			if (ch >= '0' && ch <= '1')
			    scan = scan + 1;
			else
			    break;
		    *thestringptr = scan;
		    return (exception(
			"INPUTERR : INTOVF Integer Overflow during input"));
		}
	    } else
		break;
	break;
      case 'O':
      case 'o':
	ch = *scan;
	if (ch >= '0' && ch <= '7') {
	    scan = scan + 1;
	    val = value_integer(ch - '0');
	} else {
	    *thestringptr = scan;
	    return (exception("INPUTERR : Invalid integer input format"));
	}
	while ((ch = *scan) != '\0')
	    if (ch >= '0' && ch <= '7') {
		scan = scan + 1;
		if (isexc(val = integer_multiply(integer_part(val), 8)) ||
		  isexc(val = integer_add(integer_part(val), (ch - '0')))) {
		    while ((ch = *scan) != '\0')	/* overflow so skip
							 * remaining octdigits
							 */
			if (ch >= '0' && ch <= '7')
			    scan = scan + 1;
			else
			    break;
		    *thestringptr = scan;
		    return (exception(
			"INPUTERR : INTOVF Integer Overflow during input"));
		}
	    } else
		break;
	break;
      default:
	*thestringptr = scan;
	return (exception("INPUTERR : Invalid integer input format"));
    }
    *thestringptr = scan;
    return val;
}

/******************************************************************************
 *
 *     	S T R I N G T O I C L L O G V A L U E (char **thestringptr)
 *
 * Given a pointer to a pointer to a string we convert the string into an
 * ICL logical value returning an exception if the input is not one of
 * TRUE, FALSE, true, false, YES, NO, yes, no.
 *
 * We return with the string pointer pointing to the character that
 * terminated conversion.
 *
 ******************************************************************************
 */
value
stringtoicllogvalue(char **thestringptr)
{
    char *scan;
    int ch;
    value val;

    val = trueval;
    scan = *thestringptr;
    while ((ch = (*scan)) == ' ' || ch == '\t')	/* skip noise */
	scan = scan + 1;
    *thestringptr = scan;
    ch = *scan;
    switch (ch) {
      case 'T':
	scan = scan + 1;
	if ((ch = *scan) == 'R') {
	    scan = scan + 1;
	    if ((ch = *scan) == 'U') {
		scan = scan + 1;
		if ((ch = *scan) == 'E')
		    scan = scan + 1;
	    }
	}
	break;
      case 't':
	scan = scan + 1;
	if ((ch = *scan) == 'r') {
	    scan = scan + 1;
	    if ((ch = *scan) == 'u') {
		scan = scan + 1;
		if ((ch = *scan) == 'e')
		    scan = scan + 1;
	    }
	}
	break;
      case 'F':
	val = falseval;
	scan = scan + 1;
	if ((ch = *scan) == 'A') {
	    scan = scan + 1;
	    if ((ch = *scan) == 'L') {
		scan = scan + 1;
		if ((ch = *scan) == 'S') {
		    scan = scan + 1;
		    if ((ch = *scan) == 'E')
			scan = scan + 1;
		}
	    }
	}
	break;
      case 'f':
	val = falseval;
	scan = scan + 1;
	if ((ch = *scan) == 'a') {
	    scan = scan + 1;
	    if ((ch = *scan) == 'l') {
		scan = scan + 1;
		if ((ch = *scan) == 's') {
		    scan = scan + 1;
		    if ((ch = *scan) == 'e')
			scan = scan + 1;
		}
	    }
	}
	break;
      case 'Y':
	scan = scan + 1;
	if ((ch = *scan) == 'E') {
	    scan = scan + 1;
	    if ((ch = *scan) == 'S')
		scan = scan + 1;
	}
	break;
      case 'y':
	scan = scan + 1;
	if ((ch = *scan) == 'e') {
	    scan = scan + 1;
	    if ((ch = *scan) == 's')
		scan = scan + 1;
	}
	break;
      case 'N':
	val = falseval;
	scan = scan + 1;
	if ((ch = *scan) == 'O')
	    scan = scan + 1;
	break;
      case 'n':
	val = falseval;
	scan = scan + 1;
	if ((ch = *scan) == 'o')
	    scan = scan + 1;
	break;
      default:
	*thestringptr = scan;
	return (exception("INPUTERR : Invalid logical input format"));
    }
    *thestringptr = scan;
    return val;
}

/******************************************************************************
 *
 * Tables of floating point constants used in the conversion of strings to
 * floating point form.
 *
 ******************************************************************************
 */

static double tend[] =
{
    1.0, 1.0e1, 1.0e2, 1.0e3, 1.0e4, 1.0e5,
    1.0e6, 1.0e7, 1.0e8, 1.0e9
};

static double tendd[] =
{
    1.0, 1.0e10, 1.0e20, 1.0e30, 1.0e40, 1.0e50,
    1.0e60, 1.0e70, 1.0e80, 1.0e90
};

/******************************************************************************
 *
 *	F T O D (char **as, int *afc)
 *
 * Given a pointer to a pointer to a string this routine converts a fixed point
 * fraction string:
 *
 * ie  d+ or .d+ or d+.  or d+.d+
 *
 * to a real integer valued icl value (ie ignores the decimal point) returning
 * with the pointer to the string pointing to the character that terminated the
 * conversion or an exception if an ovreflow of format error is detected.
 *
 * Further, on return, afc is set to a count of the number of fractional
 * digits encoutered.
 *
 * For example 36.4 is evaluated as 364.0 with afc set to 1
 *
 ******************************************************************************
 */
static value
ftod(char **as, int *afc)
{
    int dc, fcon, fc, ov;
    char ch, *s;
    value vald, vali, valk;

    s = (*as);
    ov = 0;
    vald = realzeroval;
    valk = vali = intzeroval;
    fcon = fc = dc = 0;
    ch = *s;
    if (ch == '.') {
	fcon = 1;
	s = s + 1;
	ch = *s;
    }
    while (ch >= '0' && ch <= '9') {
	s = s + 1;
	if (isexc(vali = integer_multiply(integer_part(vali), 10)) ||
	    isexc(vali = integer_add(integer_part(vali), (ch - '0'))) ||
		dc >= 99) {
	    if (ov) {		/* Have we overflowed the integer value
				 * before? */
		if (dc % 10 != 0) {
		/* d = d * tend[dc%10];  */
		    if (isexc(vald = do_operator(vald, MULTIPLY,
				value_real(tend[dc % 10])))) {
		    /*
		     * we have overflowed so skip remaining digits and any
		     * decimal point
		     */
			while (((ch = *s) >= '0' && ch <= '9') ||
				(fcon == 0 && ch == '.')) {
			    if (ch == '.')
				fcon = 1;
			    s = s + 1;
			}
			*as = s;
			return exception(
				"FLTOVF : floating overflow during input");
		    }
		}
		if (dc / 10 != 0) {	/* d = d * tendd[dc/10]; */
		    if (isexc(vald = do_operator(vald, MULTIPLY,
				value_real(tendd[dc / 10])))) {
		    /*
		     * we have overflowed so skip remaining digits and any
		     * decimal point
		     */
			while (((ch = *s) >= '0' && ch <= '9') ||
				(fcon == 0 && ch == '.')) {
			    if (ch == '.')
				fcon = 1;
			    s = s + 1;
			}
			*as = s;
			return exception(
			    "FLTOVF : floating overflow during input");
		    }
		}
/*
 * d = d + (double)k;
 */
		if (isexc(vald = do_operator(vald, ADD,
			value_real((double) (integer_part(valk)))))) {
		/*
		 * we have overflowed so skip remaining digits and any
		 * decimal point
		 */
		    while (((ch = *s) >= '0' && ch <= '9') || (fcon == 0 &&
				ch == '.')) {
			if (ch == '.')
			    fcon = 1;
			s = s + 1;
		    }
		    *as = s;
		    return exception("FLTOVF : floating overflow during input");
		}
	    } /* end of integer - overflow already happened once */
	    else { /* first time the integer vali has overflowed */
		vald = value_real((double) (integer_part(valk)));
		ov = 1;
	    }
	    vali = value_integer(ch - '0');
	    dc = 0;
	} /* of int value overflowing */
	dc = dc + 1;
	valk = vali;
	if (fcon)
	    fc = fc + 1;
	ch = (*s);
	if (fcon == 0 && ch == '.') {
	    fcon = 1;
	    s = s + 1;
	    ch = *s;
	}
    }
    if (ov) {	/* have we once integer overflowed vali? */
	if (dc % 10 != 0) {
	/* d = d * tend[dc%10];  */
	    if (isexc(vald = do_operator(vald, MULTIPLY,
			value_real(tend[dc % 10])))) {
		*as = s;
		return exception("FLTOVF : floating overflow during input");
	    }
	}
	if (dc / 10 != 0) {	/* d = d * tendd[dc/10]; */
	    if (isexc(vald = do_operator(vald, MULTIPLY,
			value_real(tendd[dc / 10])))) {
		*as = s;
		return exception("FLTOVF : floating overflow during input");
	    }
	}
	if (integer_part(valk) != 0) {	/* d = d + (double)k; */
	    if (isexc(vald = do_operator(vald, ADD,
			value_real((double) (integer_part(valk)))))) {
		*as = s;
		return exception("FLTOVF : floating overflow during input");
	    }
	}
    } else
	vald = value_real((double) (integer_part(valk))); /* d = (double) k; */
    *as = s;
    *(afc) = fc;
    return vald;
}

/******************************************************************************
 *
 *	S T R I N G T O I C L R E A L V A L U E (char **thestringptr)
 *
 * Given a pointer to a pointer to a string we convert the string into a ICL
 * real value returning an exception if the input causes overflow or is an
 * invalid format.
 *
 * We return with the string pointer pointing to the character that terminated
 * conversion.
 *
 ******************************************************************************
 */
value
stringtoiclrealvalue(char **thestringptr)
{
    int fc;
    value vald, vale;
    int ch, sign, eval;
    char *scan;

    vald = realzeroval;
    scan = *thestringptr;
    while ((ch = (*scan)) == ' ' || ch == '\t')	/* skip noise */
	scan = scan + 1;
    *thestringptr = scan;
    sign = 0;
    if ((ch = *scan) == '-') {
	sign = 1;
	scan = scan + 1;
    } else if (ch == '+')
	scan = scan + 1;
    vald = ftod(&scan, &fc);	/* Convert fixed point fraction */
/* We do not check for exception yet so we can skip the exponent */
    vale = intzeroval;
    if ((ch = *scan) == 'e' || ch == 'E') {
	scan = scan + 1;
	vale = stringtoint(&scan);
    }
    *thestringptr = scan;
    if (isexc(vald) || isexc(vale) ||
	isexc(vale = integer_subtract(integer_part(vale), fc))) {
	if (isexc(vald))
	    return vald;
	else
	    return exception("FLTOVL : Floating overflow during input");
    }
    eval = integer_part(vale);
    if (eval != 0)
	if (eval > 0) {
	    int eeval;

	    while (eval != 0) {
		if (eval > 99) {
		    eeval = 99;
		    eval = eval - 99;
		} else {
		    eeval = eval;
		    eval = 0;
		}
		if ((eeval % 10 != 0 && isexc(vald = do_operator(vald,
			MULTIPLY, value_real(tend[eeval % 10])))) ||
		    (eeval / 10 != 0 && isexc(vald = do_operator(vald,
			MULTIPLY, value_real(tendd[eeval / 10])))))
		    return exception(
				"FLTOVL : Floating overflow during input");
	    }
	} else {
	    int eeval;

	    if (isexc(vale = integer_negate(eval)))
		return exception("FLTOVL : Floating overflow during input");
	    eval = integer_part(vale);
	    while (eval != 0) {
		if (eval > 99) {
		    eeval = 99;
		    eval = eval - 99;
		} else {
		    eeval = eval;
		    eval = 0;
		}
		if ((eeval % 10 != 0 && isexc(vald = do_operator(vald, DIVIDE,
			value_real(tend[eeval % 10])))) ||
		    (eeval / 10 != 0 && isexc(vald = do_operator(vald, DIVIDE,
			value_real(tendd[eeval / 10])))))
		    return exception(
				"FLTOVL : Floating overflow during input");
	    }
	}
    if (sign)
	if (isexc(vald = do_operator(realzeroval, SUBTRACT, vald)))
	    return exception("FLTOVL : Floating overflow during input");
    return vald;
}

/******************************************************************************
 *
 *	I N I T _ A R I T H (void)
 *
 * Initialise arithmetic constants.
 *
 * All this does at the moment is initialise the static variable ICL_MIN_INT
 * and calculate ICL values for constants 180/PI and PI/180 for degrees and
 * radians conversion.
 *
 * ICL_MIN_INT is calculated by host independent calculation. It does this by
 * placing 1 in an integer and repeatedly shifting it left (guaranteed to be
 * logical shift by C language) until it becomes negative.  The resulting
 * pattern 100000000...00 will be the minimum integer value in 2's complement
 * form.
 * This avoids dependence on limits.h containing sensible value for INT_MIN
 *
 ******************************************************************************
 */
void
init_arith(void)
{
    int i;

    i = 1;
    while (i > 0)
	i <<= 1;
    ICL_MIN_INT = i;
    radianstodegreesval = do_operator(value_real((double) 180.0), DIVIDE,
				value_real((double) M_PI));
    degreestoradiansval = do_operator(value_real((double) M_PI), DIVIDE,
				value_real((double) 180.0));
    return;
}
