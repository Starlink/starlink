/*
 *+
 *  Name:
 *     make-prm-par.c

 *  Type of module:
 *     C source

 *  Purpose:
 *     Generate PRM header file(s).

 *  Description:
 *     Creates constants for the Primdat/PRM system.  We define limits
 *     for the seven HDS types:
 *
 *        <T>           HDS Type                Fortran Type
 *        UB    _UBYTE 	    (unsigned byte)     BYTE
 *        B     _BYTE 	    (byte) 	        BYTE
 *        UW    _UWORD 	    (unsigned word)     INTEGER*2
 *        W     _WORD 	    (word)              INTEGER*2
 *        I     _INTEGER    (integer)           INTEGER
 *        R     _REAL 	    (real)              REAL
 *        D     _DOUBLE     (double precision)  DOUBLE PRECISION
 *
 *     To avoid doubt: We take bytes to be 8 bits long (there is old
 *     hardware which had different byte lengths, but the chance of
 *     new hardware making the same choice is surely sufficiently
 *     remote that we can ignore it).  Oh, and a radix-2 machine.
 *
 *     Wherever possible, below, we try to avoid making any
 *     assumptions about the arithmetic on the host machine.  However
 *     there are a couple of places where this is unavoidable.
 *     We depend on having 4-byte integers, because HDS seems to
 *     depend on that.  We try not to depend on 4-byte floats and
 *     8-byte doubles, but there quite probably are such dependencies
 *     implicit and uncaught below.
 *
 *     The actual values of the bad values are based on the extreme
 *     values of the integer and float types, and matches those in HDS,
 *     as defined in dat1_init_ndr.c.


 *  Authors:
 *     NXG: Norman Gray (Starlink, Glasgow)
 *     {enter_new_authors_here_

 *  History:
 *     18-Mar-2004 (NXG):
 *        Original version

 *-
 */


#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* Make sure that FC_HAVE_HEX_NUMBERS has a value */
#ifndef FC_HAVE_HEX_NUMBERS
#define FC_HAVE_HEX_NUMBERS 0
#endif

#if HAVE_LIMITS_H
#  include <limits.h>
#else
   /* perhaps insert standard 4-byte 2s-complement values */
#  error "make-prm-par.c: No limits.h -- we're in trouble"
#endif

#if HAVE_FLOAT_H
#  include <float.h>
#else
   /* perhaps insert standard IEEE values */
#  error "make-prm-par.c: No float.h -- we're in trouble"
#endif

#if FLT_RADIX != 2
#  error "make-prm-par.c: No, let's NOT worry about non-binary processors right now"
#endif


/* 
 * Does this look like IEEE?  IEEE has 23 binary digits in the
 * significand, not counting the suppressed leading 1, and a bias of
 * 127, and thus a FLT_MAX_EXP of 128.  VAX floats have a bias one
 * larger than the IEEE one and they don't have INF, and so they have
 * a FLT_MAX_EXP of 130 (I think).
 */
#if FLT_MANT_DIG == 24 && FLT_MAX_EXP == 128
#  define FLOAT_IS_IEEE 1
#else
#  define FLOAT_IS_IEEE 1
#endif

#if HAVE_TIME_H
#  include <time.h>
#endif

#if HAVE_STDINT_H
#  include <stdint.h>
   /*
    * Assume that if stdint.h is defined, then all the required constants
    * are defined also 
    */
#else
#  define INT8_MAX  SCHAR_MAX
#  define INT8_MIN  SCHAR_MIN
#  define UINT8_MAX UCHAR_MAX
#  if SIZEOF_SHORT_INT == 2
#    define INT16_MAX  SHRT_MAX
#    define INT16_MIN  SHRT_MIN
#    define UINT16_MAX USHRT_MAX
#  else
     /*
      * That's a surprise -- this machine's short integers are something
      * other than two bytes long.  So use specific values (these
      * particlar ones are taken from gcc limits.h).
      */
#    define INT16_MAX  0x7fff
#    define INT16_MIN  (-32767-1)
#    define UINT16_MAX 0xffff
#  endif
   /*
    * The tests below are somewhat artificial, since correct output
    * depends on ints being 4 bytes.  But keep them in anyway, so there's
    * less to do if we do have to change this in future.
    */
#  if SIZEOF_INT == 4
#    define INT32_MAX  INT_MAX
#    define INT32_MIN  INT_MIN
#    define UINT32_MAX UINT_MAX
#  elif SIZEOF_LONG_INT == 4
#    define INT32_MAX  LONG_MAX
#    define INT32_MIN  LONG_MIN
#    define UINT32_MAX ULONG_MAX
#  else
     /* What sort of machine is this?! */
#    define INT32_MAX  0x7fffffff
#    define INT32_MIN  (-2147483647-1)
#    define UINT32_MAX 0xffffffff
#  endif
#  if !HAVE_INT32_T
/*
 * If we don't have int32/64_t, assume we don't have uint32/64_t
 * either.  I'm not sure about these definitions: they don't work with
 * gcc when I artificially exercise this branch, but everything works
 * OK when I properly include stdint.h.
 */
#    if SIZEOF_SHORT_INT == 4
typedef   signed short int int32_t;
typedef unsigned short int int32_t;
#    elif SIZEOF_INT == 4
typedef   signed int int32_t;
typedef unsigned int uint32_t;
#    else
#      error "Can't find 32-bit integer type"
#    endif
#  endif
#  if !HAVE_INT64_T
#    if SIZEOF_INT == 8
typedef   signed int int64_t;
typedef unsigned int uint64_t;
#    elif SIZEOF_LONG_INT == 8
typedef   signed long int int64_t;
typedef unsigned long int uint64_t;
#    elif SIZEOF_LONG_LONG_INT == 8
typedef   signed long long int int64_t;
typedef unsigned long long int uint64_t;
#    else
#      error "Can't find a 64-bit integer type"
#    endif
#  endif
#endif



/* Declarations. */
char *progname;                 /* program name */

typedef union {
    double d;
    float f;
    int64_t i;
} Number;


/* Global values */
int float_precision, double_precision;
FILE* FortranOutput;
FILE* COutput;


void comment(const char* comment);
void par_i(const int size, const char* name, int val);
void par_f(const int size, const char* name, double val);

/* Helper functions */
const char* todaysdate(void);
const char* tohex(int size, Number *p);
void assumption(const int, const char*);
void Usage(void);

/* Conditional functions, to replace possibly missing library functions */
#if !HAVE_NEXTAFTER
float  nextafterf(float, float);
double nextafter(double, double);
#endif
#if FLOAT_IS_IEEE
float min_denorm_f(void);
double min_denorm_d(void);
#endif


int main (int argc, char **argv)
{
    progname = argv[0];

    for (argc--, argv++; argc>0; argc--, argv++) {
        if (**argv != '-')
            Usage();
        switch (*++*argv) {
          case 'f':
            ++*argv;
            if (**argv == '\0')
                Usage();
            if ((FortranOutput = fopen(*argv, "w")) == 0) {
                fprintf(stderr, "%s: can't open file %s for output\n",
                        progname, *argv);
                exit(1);
            }
            break;

          case 'c':
            ++*argv;
            if (**argv == '\0')
                Usage();
            if ((COutput = fopen(*argv, "w")) == 0) {
                fprintf(stderr, "%s: can't open file %s for output\n",
                        progname, *argv);
                exit(1);
            }
            break;

          default:
            Usage();
        }
    }

    if (FortranOutput == 0 && COutput == 0)
        Usage();
    

    /*
     * Set the precisions (globlly, so they're visible in par_f).
     *
     * There are p+1 digits in the floating point mantissa, including
     * the leading 1 which is suppressed for normalised numbers
     * (p+1=FLT_MANT_DIG or DBL_MANT_DIG).  Let d=ceil(p log10(2))
     * = ceil(-log10(2^{-p})) (2^{-p} is epsilon, the smallest number
     * such that 1.0+epsilon != 1.0).  Then two successive floats are
     * not less than 10^{-d} apart, and so any correctly-rounded decimal
     * number with d decimal places represents one and only one binary
     * floating point number.  That is, d is the output precision
     * necessary to uniquely identify a float.
     */
    float_precision =  (int)ceil((FLT_MANT_DIG-1) * log10(2.0));
    double_precision = (int)ceil((DBL_MANT_DIG-1) * log10(2.0));

    if (FortranOutput) {
        fprintf(FortranOutput,
"*+\n"
"*  Name:\n"
"*     PRM_PAR\n"
"\n"
"*  Type of Module:\n"
"*     Fortran include file.\n"
"\n"
"*  Purpose:\n"
"*     Define public constants for the PRIMDAT system.\n"
"\n"
"*  Description:\n"
"*     This file defines machine-dependent public constants for the\n"
"*     PRIMDAT system.\n"
"\n"
"*  Machine-specific features used:\n"
"*     Fortran compiler %s support the \"'hex'X\" notation.\n"
"*     The machine %s appear to have IEEE floats.\n"
"\n"
"*  Authors:\n"
"*     RFWS: R.F. Warren-Smith (STARLINK, RAL)\n"
"*     %s program\n"
"\n"
"*  History:\n"
"*     9-AUG-1988 (RFWS):\n"
"*        Original version.\n"
"*     25-OCT-1991 (RFWS):\n"
"*        Adapted for SUN4 systems from the original VMS file.\n"
"*     %s (%s):\n"
"*        Generated\n"
"*     No further changes -- do not edit this file\n"
"\n"
"*-\n"
"\n",
                (FC_HAVE_HEX_NUMBERS ? "DOES" : "DOES NOT"),
                (FLOAT_IS_IEEE        ? "DOES" : "DOES NOT"),
                progname, todaysdate(), progname);
    }

    if (COutput) {
        fprintf(COutput,
"/*\n"
"*+\n"
"*  Name:\n"
"*     prm_par.h\n"
"\n"
"*  Type of Module:\n"
"*     C include file.\n"
"\n"
"*  Purpose:\n"
"*     Define public constants for the PRIMDAT system.\n"
"\n"
"*  Description:\n"
"*     This file defines machine-dependent public constants for the\n"
"*     PRIMDAT system.\n"
"\n"
"*  Machine-specific features used:\n"
"*     The machine %s appear to have IEEE floats.\n"
"\n"
"*  Authors:\n"
"*     RFWS: R.F. Warren-Smith (STARLINK, RAL)\n"
"*     %s program\n"
"\n"
"*  History:\n"
"*     9-AUG-1988 (RFWS):\n"
"*        Original version.\n"
"*     25-OCT-1991 (RFWS):\n"
"*        Adapted for SUN4 systems from the original VMS file.\n"
"*     %s (%s):\n"
"*        Generated\n"
"*     No further changes -- do not edit this file\n"
"\n"
"*-\n"
"*/\n"
"\n",
                (FLOAT_IS_IEEE        ? "DOES" : "DOES NOT"),
                progname, todaysdate(), progname);
    }
    
    comment("Bad values, used for flagging undefined data.");
    par_i(1, "VAL__BADUB",  UINT8_MAX);
    par_i(1, "VAL__BADB",    INT8_MIN);
    par_i(2, "VAL__BADUW", UINT16_MAX);
    par_i(2, "VAL__BADW",   INT16_MIN);
    par_i(4, "VAL__BADI",   INT32_MIN);
    par_f(1, "VAL__BADR", -FLT_MAX);
    par_f(2, "VAL__BADD", -DBL_MAX);

    comment("Machine precision.");
    par_i(1, "VAL__EPSUB", 1);
    par_i(1, "VAL__EPSB", 1);
    par_i(1, "VAL__EPSUW", 1);
    par_i(1, "VAL__EPSW", 1);
    par_i(1, "VAL__EPSI", 1);
    par_f(1, "VAL__EPSR", FLT_EPSILON);
    par_f(2, "VAL__EPSD", DBL_EPSILON);

    comment("Maximum (most positive) non-bad value.");
    par_i(1, "VAL__MAXUB",  UINT8_MAX-1);
    par_i(1, "VAL__MAXB",    INT8_MAX);
    par_i(2, "VAL__MAXUW", UINT16_MAX-1);
    par_i(2, "VAL__MAXW",   INT16_MAX);
    par_i(4, "VAL__MAXI",   INT32_MAX);
    par_f(1, "VAL__MAXR",     FLT_MAX);
    par_f(2, "VAL__MAXD",     DBL_MAX);

    comment("Maximum (most positive) number.");
    par_i(1, "NUM__MAXUB",  UINT8_MAX);
    par_i(1, "NUM__MAXB",    INT8_MAX);
    par_i(2, "NUM__MAXUW", UINT16_MAX);
    par_i(2, "NUM__MAXW",   INT16_MAX);
    par_i(4, "NUM__MAXI",   INT32_MAX);
    par_f(1, "NUM__MAXR",     FLT_MAX);
    par_f(2, "NUM__MAXD",     DBL_MAX);

    comment("Minimum (most negative) non-bad value.");
    par_i(1, "VAL__MINUB", 0);
    par_i(1, "VAL__MINB",  INT8_MIN+1);
    par_i(2, "VAL__MINUW", 0);
    par_i(2, "VAL__MINW",  INT16_MIN+1);
    par_i(4, "VAL__MINI",  INT32_MIN+1);
    par_f(1, "VAL__MINR",  nextafterf(-FLT_MAX, 0));
    par_f(2, "VAL__MIND",  nextafter(-DBL_MAX, 0));

    comment("Minimum (most negative) number.");
    par_i(1, "NUM__MINUB", 0);
    par_i(1, "NUM__MINB",  INT8_MIN);
    par_i(2, "NUM__MINUW", 0);
    par_i(2, "NUM__MINW",  INT16_MIN);
    par_i(4, "NUM__MINI",  INT32_MIN);
    par_f(1, "NUM__MINR",  -FLT_MAX);
    par_f(2, "NUM__MIND",  -DBL_MAX);

    comment("Number of basic machine units (bytes) used by a value.");
    par_i(-4, "VAL__NBUB", 1);
    par_i(-4, "VAL__NBB",  1);
    par_i(-4, "VAL__NBUW", 2);
    par_i(-4, "VAL__NBW",  2);
    par_i(-4, "VAL__NBI",  4);
    par_i(-4, "VAL__NBR",  sizeof(float));
    par_i(-4, "VAL__NBD",  sizeof(double));

    comment("Smallest positive value.");
    par_i(-1, "VAL__SMLUB", 1);
    par_i(-1, "VAL__SMLB", 1);
    par_i(-2, "VAL__SMLUW", 1);
    par_i(-2, "VAL__SMLW", 1);
    par_i(-4, "VAL__SMLI", 1);
#if FLOAT_IS_IEEE
    /* Note that the following are denormalised numbers: this is not epsilon */
    par_f(1, "VAL__SMLR", min_denorm_f());
    par_f(2, "VAL__SMLD", min_denorm_d());
#else
#  error "What's the minimum number in this float model?"
#endif

    comment("Number of characters required to format value as decimal string.");
    par_i(-4, "VAL__SZUB", 3);
    par_i(-4, "VAL__SZB",  4);
    par_i(-4, "VAL__SZUW", 5);
    par_i(-4, "VAL__SZW",  6);
    par_i(-4, "VAL__SZI",  11);
    /* -x.<float_precision>e+xx */
    par_i(-4, "VAL__SZR",  float_precision+7);
    /* -x.<double_precision>e+xxx */
    par_i(-4, "VAL__SZD",  double_precision+8);

    exit(0);
    
}


void comment(const char* comment)
{
    int i;
    if (FortranOutput) {
        fprintf(FortranOutput, "\n*  %s\n*  ", comment);
        for (i=strlen(comment); i>0; i--)
            fputc('=', FortranOutput);
        fputs("\n\n", FortranOutput);
    }
    if (COutput) {
        fprintf(COutput, "\n/* %s */\n", comment);
    }
}

/*
 * Print out an integer.  The `size' parameter is 1, 2 or 4; any
 * other value causes a message to be printed out and we exit with a
 * non-zero status.
 *
 * The size parameter is the number of 8-bit bytes in the required
 * type.  If this is negative, then its absolute value is the required
 * size, but avoid writing the number out as hex.
 */
void par_i(const int size, const char* name, int val)
{
    const char *type;
    switch (size) {
      case 1:
      case -1:
        type = "BYTE";
        break;
      case 2:
      case -2:
        type = "INTEGER*2";
        break;
      case 4:
      case -4: 
        type = "INTEGER";
        break;
      default:
        {
            char msg[80];
            sprintf(msg, "can handle 1-, 2-, or 4-byte integers, but not %d-byte ones\n",
                    size);
            assumption(0, msg);
        }
        break;
    }

    if (FortranOutput) {
        if (FC_HAVE_HEX_NUMBERS && size > 0) {
            /* Write out the val as a hex pattern.  It would seem good to
               use printf's %X format here, but that sometimes seems to
               misbehave, and act unpredictably with negative numbers, no
               matter how cunningly you cast the values.  At least with
               the tohex() function, we know _exactly_ what we're getting. */
            Number N;
            N.i = val;
            fprintf(FortranOutput,
                    "      %s %s\n      PARAMETER ( %s = '%s'X )\n*     [In decimal: %d]\n\n",
                    type, name, name, tohex(size, &N), val);
        } else {
            fprintf(FortranOutput,
                    "      %s %s\n      PARAMETER ( %s = %d )\n\n",
                    type, name, name, val);
        }
    }
    if (COutput) {
        if (size > 0) {
            Number N;
            N.i = val;
            fprintf(COutput, "#define %s 0x%s\n", name, tohex(size, &N));
        } else {
            fprintf(COutput, "#define %s %d\n", name, val);
        }
    }
}

/*
 * Print out a float.  The size parameter is 1 or 2, for single or
 * double precision; any other value causes a message to be printed
 * out and we exit with a non-zero status.  We are assuming that these
 * are different sizes.
 */
void par_f(const int size, const char* name, double val)
{
    const char *type;
    switch (size) {
      case 1:
        type = "REAL";
        break;
      case 2:
        type = "DOUBLE PRECISION";
        break;
      default:
        {
            char msg[80];
            sprintf(msg, "Can handle single- or double-precision, but not %d-precision",
                    size);
            assumption(0, msg);
        }
        break;
    }

    if (FortranOutput) {
#if FC_HAVE_HEX_NUMBERS
        /* We're assuming that the int within the Number type is wide enough */
        assumption(sizeof(double) <= sizeof(int64_t),
                   "Number type assumes double <= 64 bits");
        {
            Number N;
            int nbytes;
            if (size == 1) {
                N.f = val;
                nbytes = sizeof(float);
            } else {
                N.d = val;
                nbytes = sizeof(double);
            }
            fprintf(FortranOutput,
                    "      %s %s\n      PARAMETER ( %s = '%s'X )\n\n",
                    type, name, name, tohex(nbytes, &N));
        }
#else
        if (size == 1)
            fprintf(FortranOutput,
                    "      %s %s\n      PARAMETER ( %s = %.*E )\n\n",
                    type, name, name, float_precision, val);
        else
            fprintf(FortranOutput,
                    "      %s %s\n      PARAMETER ( %s = %.*lE )\n\n",
                    type, name, name, double_precision, val);
#endif
    }
    if (COutput) {
        if (size == 1)
            fprintf(COutput, "#define %s %.*e\n", name, float_precision, val);
        else
            fprintf(COutput, "#define %s %.*le\n",
                    name, double_precision, val);
    }
}

/*
 * Check that an assumption is true.  If the TEST argument is true
 * (non-zero), then return immediately.  Otherwise, print the message
 * to stderr and exit with a non-zero (failure) status.
 */
void assumption(const int test, const char*msg)
{
    if (test)
        return;
    fprintf(stderr, "%s: %s\n", progname, msg);
    exit(1);
}

const char *todaysdate(void)
{
#if HAVE_TIME_H
    static char s[12];
    time_t t = time(0);
    
    strftime(s, sizeof(s), "%d-%b-%Y", localtime(&t));
    return s;
#else
    return "TODAY";
#endif
}


const char* tohex(int size, Number* p)
{
    static char c[17];
    int64_t pi = p->i;
    int i;

    memset((void*)c, 0, sizeof(c));
    for (i=2*size-1; i>=0; i--, pi>>=4) {
        int64_t t = pi & 0xf;
        if (t < 10)
            c[i] = '0' + t;
        else
            c[i] = 'A' - 10 + t;
        /* printf("pi=%llx  t=%llx  c[%d]=%x\n", pi, t, i, c[i]); */
    }
    return c;
}

#if 0
int testhex(void)
{
    Number N;
    printf("int=%d  long int=%d  long long int=%d  int64_t=%d\n",
           sizeof(int), sizeof(long int), sizeof(long long int),
           sizeof(int64_t));
    
#define TI(size, n) N.i=n; printf("%lld(%d) -> %s\n", N.i, size, tohex(size,&N))
    TI(1,0);
    TI(1,10);
    TI(1,16);
    TI(1,255);
    TI(1,65535);
    TI(2,65535);
    TI(4,65535);

#define TF(size, n) N.f=n;  printf("%g(%d) -> %s\n", N.f, size, tohex(size,&N))
    TF(4,1.0);
    TF(4,-1.0);
    TF(4,1.7E38);
    
#define TD(size, n) N.d=n;  printf("%g(%d) -> %s\n", N.d, size, tohex(size,&N))
    TD(8,1.0);
    TD(8,-1.0);
    TD(8,1.7E38);
    TD(8,1E50);
}

#endif

#if !HAVE_NEXTAFTER
/* 
 * Replacement nextafter functions.  These are NOT general
 * implementations of these functions: they work only for negative
 * IEEE floats (of either endianness), and always take the next after
 * in the direction of positive numbers.  The won't work for VAX
 * floats, for example (because while VAX integers are little-endian,
 * VAX floats are PDP-endian, with the sign bit in the middle, so
 * simply adjusting the aliased integer's LSB will make bigger changes
 * in the aliased float).  They work only as long as the sizes of the
 * aliasing types are indeed the same, so we check this.
 */
float nextafterf(float x, float y)
{
    union {
        float f;
        uint32_t i;
    } pair;
    assumption(sizeof(float) == sizeof(uint32_t),
               "nextafterf replacement function depends on float size");
    pair.f = x;
    pair.i--;
    return pair.f;
}

double nextafter(double x, double y)
{
    union {
        double d;
        uint64_t i;
    } pair;
    assumption(sizeof(double) == sizeof(uint64_t),
               "nextafter replacement function depends on double size");
    pair.d = x;
    pair.i--;
    return pair.d;
}
#endif

/* Following are IEEE-specific */
#if FLOAT_IS_IEEE
float min_denorm_f(void)
{
    union {
        float f;
        uint32_t u;
    } n;
    n.u = 1;
    return n.f;
}

double min_denorm_d(void)
{
    union {
        double d;
        uint64_t u;
    } n;
    n.u = 1;
    return n.d;
}
#endif

void Usage(void)
{
    fprintf(stderr, "Usage: %s [-fFortranIncludeFile] [-cCIncludeFile]\n",
            progname);
    exit(1);
}
