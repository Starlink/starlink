/*
 * Explore IEEE single- and double-precision floats.
 *
 * Invoked with an argument, it reprints that number as a double, hex
 * and binary.  Without argument, it reads numbers from stdin.  The
 * argument is either a double or an integer (hex, with leading '0x',
 * eg 0x3f800000) as argument. 
 *
 * $Id$
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include "bytesex.h"

#ifndef DOUBLEPRECISION
#define DOUBLEPRECISION 1	/* double-precision by default */
#endif

#if DOUBLEPRECISION
#define EXPONENTWIDTH 11
#define MANTISSAWIDTH 20	/* actually just the part of the
				   mantissa in the MSB */
#define ZEROMANTISSA(F) ((F).ieee.m == 0 && (F).ieee.m2 == 0)
#define NINTS 2			/* number of ints in a double */
typedef double Float;
#else
#define EXPONENTWIDTH 8
#define MANTISSAWIDTH 23
#define ZEROMANTISSA(F) ((F).ieee.m == 0)
#define NINTS 1			/* number of ints in a float */
typedef float Float;
#endif
/* Create an integer with EXPONENTWIDTH set bits.  Relies on
   zero-filling when left-shifting. */
#define FULLEXPONENT ~(~0 << EXPONENTWIDTH)

/* Define the structure we'll use to do the manipulations */
typedef union {
    Float f;
    unsigned int i[NINTS];
    struct {
#if DOUBLEPRECISION
#if BIGENDIAN
	unsigned int s  : 1;
	unsigned int e  : 11;
	unsigned int m  : 20;
	unsigned int m2 : 32;
#else
	unsigned int m2 : 32;
	unsigned int m  : 20;
	unsigned int e  : 11;
	unsigned int s  : 1;
#endif
#else  /* DOUBLEPRECISION */
#if BIGENDIAN
	unsigned int s : 1;
	unsigned int e : 8;
	unsigned int m : 23;
#else
	unsigned int m : 23;
	unsigned int e : 8;
	unsigned int s : 1;
#endif
#endif
    } ieee;
} ieeefloat;

/* Function prototypes */
/* Utility functions */
int pisnan (const Float darg);		/* true if argument is a NaN */
int pisinf (const Float darg);		/* true if argument is infinite */
int pisfinite (const Float darg);	/* true if argument is finite */
int pisnormal (const Float darg);	/* true if argument is normal */
Float pnan (const char *tagp);		/* return a NaN */
Float pinfinity (Float darg);		/* return +ve or -ve infinity */

/* Functions which do the work of this demo program */
char *pbits (unsigned int i, unsigned int n);
int parse_int (const char *s, unsigned int i[NINTS]);
Float reprint_number (char *s, Float farg);



/* Following are utility functions, which are portable */

/* Returns non-zero if argument is a NaN -- all bits in exponent set,
   and mantissa non-zero. */
int pisnan (const Float darg)
{
    ieeefloat d;
    d.f = darg;
#if DOUBLEPRECISION
    return (d.ieee.e == FULLEXPONENT && !(d.ieee.m == 0 && d.ieee.m2 == 0));
#else
    return (d.ieee.e == FULLEXPONENT && d.ieee.m != 0);
#endif
}

/* returns non-zero if argument is infinity -- all bits in exponent
   set, and mantissa zero. */
int pisinf (const Float darg)
{
    ieeefloat d;
    d.f = darg;
#if DOUBLEPRECISION
    return (d.ieee.e == FULLEXPONENT && d.ieee.m == 0 && d.ieee.m2 == 0);
#else
    return (d.ieee.e == FULLEXPONENT && d.ieee.m == 0);
#endif
}

/* returns non-zero if argument is finite (normal, subnormal or zero)
   -- not all bits in exponent set. */
int pisfinite (const Float darg)
{
    ieeefloat d;
    d.f = darg;
    return (d.ieee.e != FULLEXPONENT);
}

/* Returns non-zero if argument is normal -- exponent is not all-ones
 * nor all-zeros.
 *
 * Note that this refers to the argument as a _double_, and might not
 * give the expected results if it's applied to a single which is 
 * promoted to a double when given as a parameter.
 */
int pisnormal (const Float darg)
{
    ieeefloat d;
    d.f = darg;
    return (d.ieee.e != FULLEXPONENT && d.ieee.e != 0);
}

/* returns a NaN.  Argument is ignored */
Float pnan (const char *tagp)
{
    ieeefloat d;
    d.ieee.s  = 0;
    d.ieee.e  = FULLEXPONENT;
    /* Set leading bit in mantissa -- quiet NaN. */
    d.ieee.m = (1<<(MANTISSAWIDTH-1));
#if DOUBLEPRECISION
    d.ieee.m2 = 0;
#endif
    return d.f;
}

/* returns positive or negative infinity, depending on the sign of darg */
Float pinfinity (Float darg)
{
    ieeefloat d;
    d.f = darg;			/* copies sign */
    d.ieee.e = FULLEXPONENT;
    d.ieee.m = 0;
#if DOUBLEPRECISION
    d.ieee.m2 = 0;
#endif
    return d.f;
}

/*
 * pbits: given an integer i (assumed 32 bits),
 * return the rightmost n (<=32) bits of the integer expressed in binary.
 */
char *pbits (unsigned int i, unsigned int n)
{
    static char s[33];
    char *p;
    /* printf ("pbits(%x/%d)", i, n); */
    s[n] = '\0';                        /* terminate the string */
    for (p=s+n-1; p>=s; p--, i>>=1)
        *p = ((i&1) ? '1' : '0');
    return s;
}

#if DOUBLEPRECISION
/* Take a string of (up to) 16 hex digits, remove leading '0x', and
 * any spaces, and convert the first 8 into i[0], and the second 8 into
 * i[1].  If the string is shorter than 16 digits, pad the remainder
 * with '0'.
 *
 * Only need this for converting 16-digit strings to d-p floats.
 *
 * Return non-zero on error.
 */
int parse_int (const char *s, unsigned int i[2])
{
    char digits0[9], digits1[9], *p;
    int ndigits = 0;

    if (!(s[0] == '0' && s[1] == 'x'))
	return 1;
    p = digits0;
    s += 2;
    while (ndigits <= 16)
    {
	while (isspace(*s))	/* skip blanks */
	    s++;
	if (*s == '\0')		/* pad remainder */
	    *p++ = '0';
	else
	    *p++ = *s++;
	ndigits++;
	if (ndigits == 8)	/* done 8 digits - switch to second 8*/
	    p = digits1;
    }
    digits0[8] = digits1[8] = '\0'; /* terminate the strings */

    i[0] = strtoul (digits0, (char**)NULL, 16);	/* ...and convert them */
    i[1] = strtoul (digits1, (char**)NULL, 16);

    /* printf ("parse_int: digits=%s %s --> %08x %08x\n",
       digits0, digits1, i[0], i[1]); */
    return 0;
}
#else
/* same, but simpler for single precision */
int parse_int (const char *s, unsigned int i[1])
{
    char digits0[9], *p;
    int ndigits;

    if (!(s[0] == '0' && s[1] == 'x'))
	return 1;
    p = digits0;
    s += 2;
    for (ndigits=0; ndigits<=8; ndigits++)
    {
	while (isspace(*s))	/* skip blanks */
	    s++;
	if (*s == '\0')		/* pad remainder */
	    *p++ = '0';
	else
	    *p++ = *s++;
    }
    digits0[8] = '\0'; /* terminate the strings */

    *i = strtoul (digits0, (char**)NULL, 16);	/* ...and convert them */

    /* printf ("parse_int: digits=%s --> %08x\n", digits0, *i); */
    return 0;
}
#endif
    

/*
 * Convert string to number and display in various formats.
 * If s is NULL, then use the number farg instead.
 * Return number.
 */
Float reprint_number (char *s, Float farg)
{
    ieeefloat F;
    if (s == NULL)
	F.f = farg;
    else
    {
	if (s[0]=='0' && s[1]=='x')         /* it's an integer */
	{
	    if (parse_int (s, F.i))
	    {
		printf ("can't parse integer <%s>\n", s);
		return;
	    }
	}
	else
	    F.f = strtod (s, (char**)NULL);
    }

#if DOUBLEPRECISION
    /* 49 bits of precision is 49*log_10(2)=14.75 dec.digits of precision */
	printf ("double  %24.16e\n   hex  %08x %08x\n",
	    F.f, F.i[0], F.i[1]);
#else
	printf (" float  %12.7e\n   hex  %08x\n", F.f, F.i[0]);
#endif
    printf ("binary  %s ", pbits (F.ieee.s, 1));
    printf ("%s ", pbits (F.ieee.e, EXPONENTWIDTH));
    printf ("%s ", pbits (F.ieee.m, MANTISSAWIDTH));
#if DOUBLEPRECISION
    printf ("%s", pbits (F.ieee.m2, 32));
#endif
    printf ("\n  type  ");

    /* now print out what type of number it is */
    if (F.ieee.e == 0)
        if (ZEROMANTISSA(F))
            printf ("%s zero",
                    (F.ieee.s ? "Negative" : "Positive"));
        else
            printf ("Subnormal");
    else if (F.ieee.e == FULLEXPONENT)
        if (ZEROMANTISSA(F))
            printf ("%s infinity",
                    (F.ieee.s ? "Negative" : "Positive"));
        else
            printf ("%s NaN",
                    (F.ieee.m & (1<<(MANTISSAWIDTH-1))
		     ? "Quiet"
		     : "Signalling"));
    else
        printf ("Normal");

    /* test the pis??? routines */
    printf (" (%c%c%c%c)\n",
	    (pisnan    (F.f) ? 'N' : 'n'),
	    (pisinf    (F.f) ? 'I' : 'i'),
	    (pisfinite (F.f) ? 'F' : 'f'),
	    (pisnormal (F.f) ? 'L' : 'l'));

    return;
}

int main (int argc, char **argv)
{
    Float f = 0;

    printf ("%s-precision %s endian...\n",
	    (DOUBLEPRECISION ? "Double" : "Single"),
	    (BIGENDIAN ? "big" : "little"));

    if (argc > 1)
        (void) reprint_number (argv[1], 0);
    else
    {
        char line[80];
        while (fgets (line, sizeof(line), stdin) != NULL)
	    if (line[0] == '=')
		switch (line[1])
		{
		  case 'n':
		    f = reprint_number (NULL, pnan(""));
		    break;
		  case 'i':
		    f = reprint_number (NULL, pinfinity(+1.0));
		    break;
		  case 'z':
		    f = reprint_number (NULL, 0);
		    break;
		  default:
		    printf ("eh?\n");
		    break;
		}
	    else
		f = reprint_number (line, 0);
    }
    exit (0);
}
