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
 *
 *  Usage:
 *
 *       ./make-prm-par [-fFortranInclude] [-cCInclude] [-tTestProgram]
 *
 *     -f Write a Fortran include file
 *     -c Write a C include file
 *     -t Write a test file


*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

 *  Authors:
 *     BC: Brad Cavanagh (JAC, Hawaii)
 *     NXG: Norman Gray (Starlink, Glasgow)
 *     PWD: Peter Draper (University of Durham)
 *     TIMJ: Tim Jenness (JAC, Hawaii)
 *     DSB: David Berry (JAC, Hawaii)
 *     {enter_new_authors_here}

 *  History:
 *     18-Mar-2004 (NXG):
 *        Original version, regenerating files with longer histories.
 *     11-Aug-2005 (TIMJ):
 *        Fix compiler warnings.
 *     21-Aug-2006 (BC):
 *        Replace C++-style comments with C-style.
 *     24-Feb-2009 (TIMJ):
 *        Support F95 BOZ "Z" prefix.
 *     26-FEB-2009 (PWD):
 *        After discussion back out of supporting Z prefix. Using a BOZ
 *        descriptor doesn't in itself give the necessary support to
 *        assign the appropriate values for unsigned integers. That either
 *        requires a typeless BOZ, which is usually the 'X descriptor, or
 *        support from the compiler so that integer overflows are permitted.
 *        When compiler support is needed the various values may as well be
 *        written as plain integers (and floating point).
 *     12-OCT-2010 (DSB):
 *        Added VAL__NAM<X> constants which hold character strings "_DOUBLE",
 *        "_REAL" etc.

 *  Copyright:
 *     Copyright (C) 2009-2010 Science and Technology Facilities Council.
 *     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
 *     Copyright (C) 2004-2005 Council for the Central Laboratory of the Research Councils

 *-
 */

char copyright_string[] = "Copyright 1988, 1991, 1992, 1995, 2004, 2005, Council for the Central Laboratory of the Research Councils";


#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#if STDC_HEADERS
#  include <signal.h>
#endif

#if HAVE_ASSERT_H
#  include <assert.h>
#else
#  if NDEBUG
#    define assert(x)
#  else
#    define assert(x) if (!(x)) {                       \
        fprintf(stderr, "assertion \"" #x               \
                "\" failed: file \"%s\", line %d\n",    \
                __FILE__, __LINE__);                    \
        exit(1);                                        \
    }
#  endif
#endif

/* Make sure that HAVE_OLD_TYPELESS_BOZ and HAVE_TYPELESS_BOZ have
   values */
#ifndef HAVE_OLD_TYPELESS_BOZ
#define HAVE_OLD_TYPELESS_BOZ 0
#endif
#ifndef HAVE_TYPELESS_BOZ
#define HAVE_TYPELESS_BOZ 0
#endif

#if HAVE_OLD_TYPELESS_BOZ
#  define BOZCODE "X"
#elif HAVE_TYPELESS_BOZ
#  define BOZCODE "X"
#else
#  define BOZCODE "UNSUPPORTED"
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
 * a FLT_MAX_EXP of 130 (I think).  We nowhere include code for non-IEEE
 * cases, so these tests instead function both as checks and as
 * documentation.
 */
#if FLT_MANT_DIG == 24 && FLT_MAX_EXP == 128
#  define FLOAT_IS_IEEE 1
#else
#  define FLOAT_IS_IEEE 0
#endif

#if HAVE_TIME_H
#  include <time.h>
#endif

#if HAVE_INTTYPES_H
#  include <inttypes.h>
#endif
#if HAVE_STDINT_H
#  include <stdint.h>
   /*
    * Assume that if stdint.h is defined, then all the required constants
    * are defined also
    */
#else
   /* Avoid redefining any values */
#  ifndef INT8_MAX
#    define INT8_MAX  SCHAR_MAX
#  endif
#  ifndef INT8_MIN
#    define INT8_MIN  SCHAR_MIN
#  endif
#  ifndef UINT8_MAX
#    define UINT8_MAX UCHAR_MAX
#  endif
#    if SIZEOF_SHORT_INT == 2
#  ifndef INT16_MAX
#      define INT16_MAX  SHRT_MAX
#  endif
#  ifndef INT16_MIN
#      define INT16_MIN  SHRT_MIN
#  endif
#  ifndef UINT16_MAX
#      define UINT16_MAX USHRT_MAX
#  endif
#  else
     /*
      * That's a surprise -- this machine's short integers are something
      * other than two bytes long.  So use specific values (these
      * particlar ones are taken from gcc limits.h).
      */
#    ifndef INT16_MAX
#      define INT16_MAX  0x7fff
#    endif
#    ifndef INT16_MIN
#      define INT16_MIN  (-32767-1)
#    endif
#    ifndef UINT16_MAX
#      define UINT16_MAX 0xffff
#    endif
#  endif
   /*
    * The tests below are somewhat artificial, since correct output
    * depends on ints being 4 bytes.  But keep them in anyway, so there's
    * less to do if we do have to change this in future.
    */
#  if SIZEOF_INT == 4
#    ifndef INT32_MAX
#      define INT32_MAX  INT_MAX
#    endif
#    ifndef INT32_MIN
#      define INT32_MIN  INT_MIN
#    endif
#    ifndef UINT32_MAX
#      define UINT32_MAX UINT_MAX
#    endif
#  elif SIZEOF_LONG_INT == 4
#    ifndef INT32_MAX
#      define INT32_MAX  LONG_MAX
#    endif
#    ifndef INT32_MIN
#      define INT32_MIN  LONG_MIN
#    endif
#    ifndef UINT32_MAX
#      define UINT32_MAX ULONG_MAX
#    endif
#  else
     /* What sort of machine is this?! */
#    ifndef INT32_MAX
#      define INT32_MAX  0x7fffffff
#    endif
#    ifndef INT32_MIN
#      define INT32_MIN  (-2147483647-1)
#    endif
#    ifndef UINT32_MAX
#      define UINT32_MAX 0xffffffff
#    endif
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

/* Convenience -- get the number of leading spaces correct */
#define FLINE(s) "      " s "\n"
#define FLIN2(s) "     :" s "\n"



/* Declarations. */
char *progname;                 /* program name */

typedef struct {
    union {
	double d;
	float f;
	int i;
	int64_t i8;
    } v;
    int nbytes;			/* number of 8-bit bytes in the number */
} Number;


/* Global values */
int float_precision, double_precision;
FILE* FortranOutput;
FILE* COutput;
FILE* TestOutput;


void comment(const char* comment);
void par_i(const int size, const char* name, int val);
void par_fp(const int size, const char* name, void* val);
void par_f(const char* name, float val);
void par_d(const char* name, double val);
void par_c(const char* name, const char* val);

/* Helper functions */
const char* todaysdate(void);
const char* tohex(Number *p);
void assumption(const int, const char*);
int sigfpe_on_denormalized(void);
void sigfpe_on_denormalized_test(void);
static int sigfpe_on_denormalized_result = -1; /* -1 means not initialised */
void sigfpe_on_denormalized_handler(int i);
void Usage(void);

/* Conditional functions, to replace possibly missing library functions */
#if !HAVE_NEXTAFTER
double nextafter(double, double);
#endif
#if !HAVE_NEXTAFTERF
float  nextafterf(float, float);
#endif
#if FLOAT_IS_IEEE
float min_denorm_f(void);
double min_denorm_d(void);
float min_norm_f(void);
double min_norm_d(void);
#endif

/* config.h defines WORDS_BIGENDIAN to be 1 if we're on a big-endian
   machine, and leaves it undefined otherwise.  Force it to be defined
   one way or the other, for the benefit of tests below. */
#ifndef WORDS_BIGENDIAN
#  define WORDS_BIGENDIAN 0
#endif

/*
 * We can generate testing code.  If macro TEST_CODE is defined
 * non-zero, then we support an option -t which names a file to
 * receive a Fortran program, which can be compiled and run to make
 * some checks on the values we generate.  The program should exit
 * with zero status if everything's OK.
 *
 * I have gone to some lengths to try to write this out in a
 * form where they can be compared for equality, but have failed,
 * because there are just too many subtleties in integer and
 * floating-point conversions (this is essentially why we're doing all
 * this convert-to-hex nonsense, after all).  The tests aren't
 * completely reliable, therefore, and if they seem to indicate a
 * failure, then you should be at least as suspicious of the test as
 * of the failing behaviour.
 *
 * Grr.  Anyway, it really isn't worth wasting any more time on, as
 * the generated numbers look right on both the byte-orderings we care
 * about.  So yes, we're going to be in trouble if we ever try to do
 * this on some funky hardware, but in that case we're probably in
 * trouble anyway.
 *
 * The test code in question is surrounded by "#if
 * TEST_CODE...#endif".
 */
#ifndef TEST_CODE
#define TEST_CODE 0
#endif


int main (int argc, char **argv)
{
    progname = argv[0];

    for (argc--, argv++; argc>0; argc--, argv++) {
        if (**argv != '-')
            Usage();
        switch (*++*argv) {
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

#if TEST_CODE
          case 't':
            ++*argv;
            if (**argv == '\0')
                Usage();
            if ((TestOutput = fopen(*argv, "w")) == 0) {
                fprintf(stderr, "%s: can't open file %s for output\n",
                        progname, *argv);
                exit(1);
            }
            break;
#endif /* TEST_CODE */

          default:
            Usage();
        }
    }

    /* Perform the SIGFPE-on-denormalized test; we examine the results later */
    sigfpe_on_denormalized_test();

    /*
     * Set the precisions (globally, so they're visible in par_f).
     *
     * IEEE single- and double-precision floating point require 6-9
     * and 15-17 significant digits, respectively, in order that any
     * decimal number represent one and only one binary number.  The
     * argument is not trivial -- it's not just ceil(-log10(epsilon)),
     * for example -- and is given in, for example, David Goldberg,
     * `What every computer scientist should know about floating-point
     * arithmetic' (section `Binary to decimal conversion'), Computing
     * Surveys, March 1991, reprinted in Sun's `Numerical Computation
     * Guide' (see pp18-22 and pp214-6; on the web at
     * <http://docs.sun.com/app/docs/doc/806-7996>).  Note that these
     * numbers include the leading digit, so that the number of
     * decimal places which should be printed is xxx_precision-1.
     *
     * These numbers apply only to IEEE floating point, so we should
     * object if we detect we're on a processor with a different
     * arithmetic.
     */
#if FLOAT_IS_IEEE
    float_precision = 9;
    double_precision = 17;
#else
#  error "Don't know the number of decimals required for output"
#endif /* FLOAT_IS_IEEE */

    if (FortranOutput == 0 && COutput == 0 && TestOutput == 0)
        Usage();


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
"*     Define public constants for the PRM system.\n"
"\n"
"*  Description:\n"
"*     This file defines machine-dependent public constants for the\n"
"*     PRM system (used to be called PRIMDAT).\n"
"\n"
"*  Machine-specific features used:\n"
"*     (platform %s, Fortran compiler %s)\n"
"*     The Fortran compiler %s support the \"'hex'X\" notation.\n"
"*     The Fortran compiler %s support the \"X'hex'\" notation.\n"
"*     The machine %s appear to have IEEE floats.\n"
"*     The machine is %s-endian\n"
"*     The CPU %s throw SIGFPE on denormalized numbers.\n"
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
"* Copyright:\n"
"*     %s\n"
"\n"
"*-\n"
"\n",
                INFO_BUILD, INFO_FC,
                (HAVE_OLD_TYPELESS_BOZ ? "DOES" : "DOES NOT"),
                (HAVE_TYPELESS_BOZ     ? "DOES" : "DOES NOT"),
                (FLOAT_IS_IEEE            ? "DOES" : "DOES NOT"),
		(WORDS_BIGENDIAN          ? "BIG"  : "LITTLE"),
		(sigfpe_on_denormalized() ? "DOES" : "DOES NOT"),
                progname, todaysdate(), progname,
                copyright_string);
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
"*     Define public constants for the PRM system.\n"
"\n"
"*  Description:\n"
"*     This file defines machine-dependent public constants for the\n"
"*     PRM system (used to be called PRIMDAT).\n"
"\n"
"*  Machine-specific features used:\n"
"*     (platform %s, Fortran compiler %s)\n"
"*     The machine %s appear to have IEEE floats.\n"
"*     The machine is %s-endian\n"
"*     The CPU %s throw SIGFPE on denormalized numbers.\n"
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
"*  Copyright:\n"
"*     %s\n"
"\n"
"*-\n"
"*/\n"
"\n",
                INFO_BUILD, INFO_FC,
                (FLOAT_IS_IEEE            ? "DOES" : "DOES NOT"),
		(WORDS_BIGENDIAN          ? "BIG"  : "LITTLE"),
		(sigfpe_on_denormalized() ? "DOES" : "DOES NOT"),
                progname, todaysdate(), progname,
                copyright_string);
    }

#if TEST_CODE
    if (TestOutput) {
            fprintf(TestOutput,
"*  Test program.  Should exit 0 on success\n"
"*\n"
"*  Compiler characteristics:\n"
"*    The Fortran compiler %s support the \"'hex'X\" notation\n"
"*    The Fortran compiler %s support the \"X'hex'\" notation\n"
"*    The machine %s appear to have IEEE floats.\n"
"*    The machine is %s-endian\n"
"*    The CPU %s throw SIGFPE on denormalized numbers.\n"
"*\n"
"*  Test program generated by %s, %s\n"
"*\n",
                    (HAVE_OLD_TYPELESS_BOZ ? "DOES" : "DOES NOT"),
                    (HAVE_TYPELESS_BOZ     ? "DOES" : "DOES NOT"),
                    (FLOAT_IS_IEEE            ? "DOES" : "DOES NOT"),
		    (WORDS_BIGENDIAN          ? "BIG"  : "LITTLE"),
		    (sigfpe_on_denormalized() ? "DOES" : "DOES NOT"),
                    progname, todaysdate());

            fprintf(TestOutput, FLINE("PROGRAM prmtest"));

            fprintf(TestOutput,
                    "* int=%d  long int=%d  long long int=%d  int64_t=%d\n",
                    sizeof(int), sizeof(long int), sizeof(long long int),
                    sizeof(int64_t));
            fprintf(TestOutput,
                    FLINE("IMPLICIT NONE")
                    FLINE("INCLUDE 'PRM_PAR'")
                    FLINE("INTEGER NFAILS")
                    FLINE("INTEGER*4 XI")
                    FLINE("INTEGER*8 XII")
                    FLINE("REAL*4 X1")
                    FLINE("REAL*8 X2")
#if !FC_HAVE_TYPE_KIND
                    FLINE("INTEGER*8 L1")
#endif
                    FLINE("INTEGER*4 X1I")
                    FLINE("INTEGER*8 X2I")
                    FLINE("EQUIVALENCE (X1, X1I)")
                    FLINE("EQUIVALENCE (X2, X2I)")
                    "\n\n");

            fprintf(TestOutput,
                    FLINE("NFAILS=0")
#if !FC_HAVE_TYPE_KIND
                    FLINE("L1=1")
#endif
                    "\n\n");
    }
#endif /* TEST_CODE */

    comment("HDS data type codes.");
    par_c( "VAL__NAMB",  "_BYTE" );
    par_c( "VAL__NAMUB",  "_UBYTE" );
    par_c( "VAL__NAMW",  "_WORD" );
    par_c( "VAL__NAMUW",  "_UWORD" );
    par_c( "VAL__NAMI",  "_INTEGER" );
    par_c( "VAL__NAMR",  "_REAL" );
    par_c( "VAL__NAMD",  "_DOUBLE" );

    comment("Bad values, used for flagging undefined data.");
    par_i(+1, "VAL__BADUB",  UINT8_MAX);
    par_i(-1, "VAL__BADB",    INT8_MIN);
    par_i(+2, "VAL__BADUW", UINT16_MAX);
    par_i(-2, "VAL__BADW",   INT16_MIN);
    par_i(-4, "VAL__BADI",   INT32_MIN);
    par_f("VAL__BADR", -FLT_MAX);
    par_d("VAL__BADD", -DBL_MAX);

    comment("Machine precision.");
    par_i(1, "VAL__EPSUB", 1);
    par_i(1, "VAL__EPSB", 1);
    par_i(2, "VAL__EPSUW", 1);
    par_i(2, "VAL__EPSW", 1);
    par_i(4, "VAL__EPSI", 1);
    par_f("VAL__EPSR", FLT_EPSILON);
    par_d("VAL__EPSD", DBL_EPSILON);

    comment("Maximum (most positive) non-bad value.");
    par_i(1, "VAL__MAXUB",  UINT8_MAX-1);
    par_i(1, "VAL__MAXB",    INT8_MAX);
    par_i(2, "VAL__MAXUW", UINT16_MAX-1);
    par_i(2, "VAL__MAXW",   INT16_MAX);
    par_i(4, "VAL__MAXI",   INT32_MAX);
    par_f("VAL__MAXR",     FLT_MAX);
    par_d("VAL__MAXD",     DBL_MAX);

    comment("Maximum (most positive) number.");
    par_i(1, "NUM__MAXUB",  UINT8_MAX);
    par_i(1, "NUM__MAXB",    INT8_MAX);
    par_i(2, "NUM__MAXUW", UINT16_MAX);
    par_i(2, "NUM__MAXW",   INT16_MAX);
    par_i(4, "NUM__MAXI",   INT32_MAX);
    par_f("NUM__MAXR",     FLT_MAX);
    par_d("NUM__MAXD",     DBL_MAX);

    comment("Minimum (most negative) non-bad value.");
    par_i(1, "VAL__MINUB", 0);
    par_i(1, "VAL__MINB",  INT8_MIN+1);
    par_i(2, "VAL__MINUW", 0);
    par_i(2, "VAL__MINW",  INT16_MIN+1);
    par_i(4, "VAL__MINI",  INT32_MIN+1);
    par_f("VAL__MINR",  nextafterf(-FLT_MAX, 0));
    par_d("VAL__MIND",  nextafter(-DBL_MAX, 0));

    comment("Minimum (most negative) number.");
    par_i(1, "NUM__MINUB", 0);
    par_i(1, "NUM__MINB",  INT8_MIN);
    par_i(2, "NUM__MINUW", 0);
    par_i(2, "NUM__MINW",  INT16_MIN);
    par_i(4, "NUM__MINI",  INT32_MIN);
    par_f("NUM__MINR",  -FLT_MAX);
    par_d("NUM__MIND",  -DBL_MAX);

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
    /* Note that the following are potentially denormalised numbers:
       this is not epsilon */
    {
	float f;
	double d;
	if (sigfpe_on_denormalized()) {
	    f = min_norm_f();
	    d = min_norm_d();
	} else {
	    f = min_denorm_f();
	    d = min_denorm_d();
	}
	par_fp(1, "VAL__SMLR", &f);
	par_fp(2, "VAL__SMLD", &d);
    }
#else
#  error "What's the minimum number in this float model?"
#endif /* FLOAT_IS_IEEE */

    comment("Number of characters required to format value as decimal string.");
    par_i(-4, "VAL__SZUB", 3);
    par_i(-4, "VAL__SZB",  4);
    par_i(-4, "VAL__SZUW", 5);
    par_i(-4, "VAL__SZW",  6);
    par_i(-4, "VAL__SZI",  11);
    /* -x.<float_precision-1>e+xx */
    par_i(-4, "VAL__SZR",  float_precision-1+7);
    /* -x.<double_precision-1>e+xxx */
    par_i(-4, "VAL__SZD",  double_precision-1+8);

    if (TestOutput) {
        fprintf(TestOutput, "*  EXIT intrinsic is non-standard but common\n");
        fprintf(TestOutput, FLINE("call exit(nfails)"));
        fprintf(TestOutput, FLINE("end"));
    }

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
    if (TestOutput)
        fprintf(TestOutput, "\n*  %s\n", comment);
    if (COutput) {
        fprintf(COutput, "\n/* %s */\n", comment);
    }
}

void par_c(const char* name, const char* val)
{
    if (FortranOutput) {
       fprintf(FortranOutput,
               FLINE("CHARACTER*%d %s")
               FLINE("PARAMETER ( %s = '%s' )"),
                    strlen(val), name, name, val);
    }

    if (COutput) {
        fprintf(COutput, "#define %s \"%s\"\n", name, val);
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
    const char *type = NULL;
    int min = 0;

    switch (size) {
      case 1:
      case -1:
        type = "BYTE";
        min = INT8_MIN;
        break;
      case 2:
      case -2:
        type = "INTEGER*2";
        min = INT16_MIN;
        break;
      case 4:
      case -4:
        type = "INTEGER";
        min = INT32_MIN;
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
        if (HAVE_OLD_TYPELESS_BOZ && size > 0) {
            /* Write out the val as a hex pattern.  It would seem good to
               use printf's %X format here, but that sometimes seems to
               misbehave, and act unpredictably with negative numbers, no
               matter how cunningly you cast the values.  At least with
               the tohex() function, we know _exactly_ what we're getting. */
            Number N;
            N.v.i = val;
	    N.nbytes = size;
            fprintf(FortranOutput,
                    FLINE("%s %s")
                    FLINE("PARAMETER ( %s = '%s'%s )")
                    "*     [In decimal: %d]\n\n",
                    type, name, name, tohex(&N), BOZCODE, val);
        }
        else if ( HAVE_TYPELESS_BOZ && size > 0) {
            Number N;
            N.v.i = val;
	    N.nbytes = size;
            fprintf(FortranOutput,
                    FLINE("%s %s")
                    FLINE("PARAMETER ( %s = %s'%s' )")
                    "*     [In decimal: %d]\n\n",
                    type, name, name, BOZCODE, tohex(&N), val);
        } else {
            fprintf(FortranOutput,
                    FLINE("%s %s")
                    FLINE("PARAMETER ( %s = %d )") "\n",
                    type, name, name, val);
        }
    }

#if TEST_CODE
    if (TestOutput && size > 0) {
        /* Write out a test only if size is positive (indicating that
           we did write out a value for this number in hex).
	   These aren't good tests for the unsigned numbers: when
	   constants such as VAL__BADUB are read in, they're read in
	   as signed, but the val here is an unsigned value, so the
	   test fails spuriously.  The answer can still be correct, though. */
        fprintf(TestOutput,
                FLINE("if (%d .ne. %s) then")
                FLINE("    write(*,'(\"Fail: %d (%s) != \",I10)')")
                FLIN2("      %s")
                FLINE("    nfails = nfails+1")
                FLINE("endif"),
                val, name, val, name, name);
    }
#endif /* TEST_CODE */

    if (COutput) {
        if (val >= 0)
            fprintf(COutput, "#define %s %d\n", name, val);
        else {
            /* Output negative numbers in brackets, to guarantee no
               parsing surprises.  Also, write numbers like INT_MIN as
               (INT_MIN+1)-1.  This is how these numbers are defined
               in limits.h, for example: I think this is because
               |INT_MIN| > INT_MAX, and it might be that this is
               (sometimes?) parsed as a positive number plus a
               negation.  I admit there's a hint of voodoo here. */
            if (val == min)
                fprintf(COutput, "#define %s (%d - 1)\n", name, val+1);
            else
                fprintf(COutput, "#define %s (%d)\n", name, val);
        }
    }
}

#if TEST_CODE
const char *tolongint(Number *n)
{
    static char b[100];
    char sign;
    int64_t i = (n->nbytes == 4 ? n->v.i : n->v.i8);
    int parts[3];
    int mult;
    int64_t mask;
    const char *type_kind = "x";

    if (i > 0) {
        sign = '+';
    } else {
        sign = '-';
        i = -i;
    }

    mask = 0x00ffffffL;
    mult = mask+1;

    parts[2] = (i & mask);
    parts[1] = ((i >> 24) & mask);
    parts[0] = (i >> 48);
    /* use the type-kind notation to indicate that the integers are
       to be treated as long integers (how many Fortrans support this? --
       ought I to test for this?) */
#if FC_HAVE_TYPE_KIND
    type_kind = "_8";
#else
    type_kind = "*l1";
#endif
    printf("sign=%c, mult=%d, parts[0,1,2]=%d,%d,%d, type_kind=%s\n",
           sign, mult, parts[0], parts[1], parts[2], type_kind);
    if (parts[0] == 0 && parts[1] == 0) {
        sprintf(b, "%c%d", sign, parts[2]);
    } else if (parts[0] == 0) {
        sprintf(b, "%c(%d * (%d%s) + %d)",
                sign, parts[1], mult, type_kind, parts[2]);
    } else {
        sprintf(b, "%c(((%d * (%d%s) + %d) * (%d%s)) + %d)",
                sign, parts[0], mult, type_kind,
                parts[1], mult, type_kind,
                parts[2]);
    }
    {
        /* Check decomposition -- multiply parts[0,1,2] carefully */
        int64_t x = parts[0];
        x *= mult;
        x += parts[1];
        x *= mult;
        x += parts[2];

        if (i != x)
	    fprintf(stderr, "FAIL: tolongint: i=%lld = %s = %lld\n", i, b, x);
    }

    return b;
}
#endif /* TEST_CODE */

/*
 * Print out a float.  The size parameter is 1 or 2, for single or
 * double precision; any other value causes a message to be printed
 * out and we exit with a non-zero status.  We are assuming that these
 * are different sizes.
 */
void par_fp(const int size, const char* name, void* valp)
{
    const char *type = NULL;
    int issingle = 0;
    Number N;

    switch (size) {
      case 1:
        type = "REAL";
	issingle = 1;
        break;
      case 2:
        type = "DOUBLE PRECISION";
	issingle = 0;
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

    if (issingle) {
	N.v.f = (float)*((float*)valp);
	N.nbytes = sizeof(float);
    } else {
	N.v.d = (double)*((double*)valp);
	N.nbytes = sizeof(double);
    }

    if (FortranOutput) {
#if HAVE_OLD_TYPELESS_BOZ
        /* We're assuming that the int within the Number type is wide enough */
        assumption(sizeof(double) <= sizeof(int64_t),
                   "Number type assumes double <= 64 bits");
	fprintf(FortranOutput,
		"      %s %s\n      PARAMETER ( %s = '%s'%s )\n\n",
          type, name, name, tohex(&N), BOZCODE);
#elif HAVE_TYPELESS_BOZ
        assumption(sizeof(double) <= sizeof(int64_t),
                   "Number type assumes double <= 64 bits");
	fprintf(FortranOutput,
		"      %s %s\n      PARAMETER ( %s = %s'%s' )\n\n",
          type, name, name, BOZCODE, tohex(&N));
#else /* !HAVE_OLD_TYPELESS_BOZ && !HAVE_TYPELESS_BOZ */
	if (issingle) {
	    fprintf(FortranOutput,
		    "      %s %s\n      PARAMETER ( %s = %.*E )\n\n",
		    type, name, name, float_precision-1, N.v.f);
        } else {
            /* FORTRAN double precision constants use "D" not "E" */
            char dbleconst[2*(double_precision-1+8)];
            char *p;
	    sprintf(dbleconst, "%.*lE", double_precision-1, N.v.d);
            for (p = dbleconst; *p != '\0'; p++) {
                if (*p == 'E') {
                    *p = 'D';
                    break;
                }
            }
	    fprintf(FortranOutput,
		    "      %s %s\n      PARAMETER ( %s = %s )\n\n",
		    type, name, name, dbleconst);
        }
#endif /* HAVE_OLD_TYPELESS_BOZ */
    }

#if TEST_CODE
    if (TestOutput) {
        /*
         * Write out a few lines of Fortran to the test file, which
         * check that the value in varname, when parsed by the
         * configured Fortran, do in fact turn into the correct bit
         * pattern.  To do this, use tolongint to generate a Fortran
         * expression which should produce this bit pattern by
         * integer arithmetic.  This test doesn't work with the
         * VAL__BADU<X> values, since we can't get Fortran to parse
         * unsigned values usefully.
         */
        int precision = (issingle ? float_precision : double_precision);
        const char *varname;
        const char *testint;
        const char *t;

        if (issingle) {
            varname = "x1";
            testint = "xi";
        } else {
            varname = "x2";
            testint = "xii";
        }
        t = tolongint(&N);
        fprintf(TestOutput,
                FLINE("%s = %s")
                FLINE("%s =")
                FLIN2("%s"),
                varname, name,
                testint, t);
        fprintf(TestOutput,
                FLINE("print*, \"%s: \", %s, %s,")
                FLIN2("%s"),
                name, varname, testint, t);
        fprintf(TestOutput,
                FLINE("if (%si .ne. %s) then")
                FLINE("    write(*,'(\"Fail: %.*lE\",")
                FLIN2("      \" (%s) != \",F30.20,I20)') %s, %si")
                FLINE("    nfails = nfails+1")
                FLINE("endif"),
                varname, testint,
                precision, (issingle ? N.v.f : N.v.d),
                name, varname, varname);
    }
#endif /* TEST_CODE */

    if (COutput) {
        /*
         * The values {float,double}_precision indicate the maximum
         * number of significant figures required to represent a
         * float uniquely as a decimal.  The number of decimal places
         * required (this is what `precision' specifies for the %e
         * format) is therefore one less than this.
         */
        if (issingle)
            /* Include trailing F to indicate a single-precision constant */
            fprintf(COutput, "#define %s %.*eF\n",
                    name, float_precision-1, N.v.f);
        else
            fprintf(COutput, "#define %s %.*le\n",
                    name, double_precision-1, N.v.d);
    }
}

void par_f(const char* name, float val)
{
    par_fp(1, name, (void*)&val);
}

void par_d(const char* name, double val)
{
    par_fp(2, name, (void*)&val);
}


/*
 * Check that an assumption is true.  If the TEST argument is true
 * (non-zero), then return immediately.  Otherwise, print the message
 * to stderr and exit with a non-zero (failure) status.  That is, this
 * is effectively an assert with better manners, and documentation of
 * where our possibly dubious processor assumptions are located.
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


/*
 * Write out a number in hex.  `p' points to a Number which has a
 * `p->nbytes'-byte value in it.
 */
const char* tohex(Number* p)
{
    static char c[17];
    int i;
    int size = p->nbytes;
    static char *hexdigits = "0123456789ABCDEF";
    unsigned char *bp;

    assumption(sizeof(int64_t) <= 17-1,
               "tohex assumes that 16 bytes is big enough for int64_t");

    bp = (unsigned char*)&p->v.i8;
#if WORDS_BIGENDIAN
    /* In being assigned to p->i or p->i8, a 1 or 2-byte integer will
     * start after the MSB. */
    switch (size) {
      case 1:
        bp += 3;
        break;
      case 2:
        bp += 2;
        break;
      case 4:
      case 8:
        break;
      default:
        assert(0);
    }
    for (i=0; i<2*size; bp++) {
        c[i++] = hexdigits[(*bp & 0xf0) >> 4];
        c[i++] = hexdigits[*bp & 0x0f];
    }
    c[i] = '\0';
#else /* WORDS_BIGENDIAN */
    c[2*size] = '\0';
    for (i=2*size-1; i>=0; bp++) {
        c[i--] = hexdigits[*bp & 0x0f];
        c[i--] = hexdigits[(*bp & 0xf0) >> 4];
    }
#endif /* WORDS_BIGENDIAN */

    return c;
}



#if !HAVE_NEXTAFTERF
/*
 * Replacement nextafter functions.  These are NOT general
 * implementations of these functions: they work only for negative
 * IEEE floats (of either endianness), and always take the next after
 * in the direction of positive numbers.  They won't work for VAX
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
#endif

#if !HAVE_NEXTAFTER
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

float min_norm_f(void)
{
    union {
	float f;
	struct {
#if WORDS_BIGENDIAN
	    unsigned int s : 1;
	    unsigned int e : 8;
	    unsigned int m : 23;
#else
	    unsigned int m : 23;
	    unsigned int e : 8;
	    unsigned int s : 1;
#endif
	} ieee;
    } f;
    f.ieee.s = 0;
    f.ieee.e = 1;
    f.ieee.m = 0;
    return f.f;
}

double min_norm_d(void)
{
    union {
	double f;
	struct {
#if WORDS_BIGENDIAN
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
	} ieee;
    } f;

    f.ieee.s = 0;
    f.ieee.e = 1;
    f.ieee.m = 0;
    f.ieee.m2 = 0;
    return f.f;
}
#endif /* FLOAT_IS_IEEE */

/* Return true/non-zero if using denormalized numbers throws a SIGFPE
   (Alphas do this). */
int sigfpe_on_denormalized(void)
{
    /* first, ensure that sigfpe_on_denormalized_test() has in fact
       been called */
    assert(sigfpe_on_denormalized_result >= 0);
    return sigfpe_on_denormalized_result;
}

/*
 * Perform the test of whether denormalized numbers throw SIGFPE.  It
 * would be nice to fold this into the sigfpe_on_denormalized test,
 * but that's slightly tricky if we want to get the result of the test
 * immediately after performing it.  Since the FPE, if it happens, is
 * handled asynchronously, if sigfpe_on_denormalized_result is zero
 * after the denormalized arithmetic, we can't tell if this is because
 * there was no FPE, or because the handler hasn't kicked in yet.
 * There are various ways to handle this, but we don't need that much
 * dangerous cunning, since we can conveniently perform this test at
 * the beginning of the program, and examing its result separately.
 *
 * Function signal(2) is deprecated for its unreliable semantics.
 * We don't care, however, since we are extremely unlikely to get any
 * multiple FPEs between the two calls to signal(2).
 */
void sigfpe_on_denormalized_test(void)
{
    float testfloat;

    sigfpe_on_denormalized_result = 0; /* initialise to false */

    signal(SIGFPE, &sigfpe_on_denormalized_handler);

    testfloat = min_denorm_f();
    testfloat += 1;

    if (testfloat == 3.141592657) {
	/* Meaningless test, to ensure that the value of testfloat
	   is used, thus ensuring that the calculation is not
	   skipped by the compiler. */
	printf("Impossible test passed!\n");
	exit(1);
    }

    signal(SIGFPE, SIG_DFL);	/* reset signal handling */
}

/* Helper for sigfpe_on_denormalized_test */
void sigfpe_on_denormalized_handler(int i)
{
    sigfpe_on_denormalized_result = 1; /* true */

    return;
}



void Usage(void)
{
    fprintf(stderr, "Usage: %s [-fFortranIncludeFile] [-cCIncludeFile]"
#if TEST_CODE
	    " [-tTestProgram.f]"
#endif /* TEST_CODE */
	    "\n",
            progname);
    exit(1);
}
