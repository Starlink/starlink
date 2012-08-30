/*
*class++
*  Name:
*     MathMap

*  Purpose:
*     Transform coordinates using mathematical expressions.

*  Constructor Function:
c     astMathMap
f     AST_MATHMAP

*  Description:
c     A MathMap is a Mapping which allows you to specify a set of forward
c     and/or inverse transformation functions using arithmetic operations
c     and mathematical functions similar to those available in C. The
c     MathMap interprets these functions at run-time, whenever its forward
c     or inverse transformation is required. Because the functions are not
c     compiled in the normal sense (unlike an IntraMap), they may be used to
c     describe coordinate transformations in a transportable manner. A
c     MathMap therefore provides a flexible way of defining new types of
c     Mapping whose descriptions may be stored as part of a dataset and
c     interpreted by other programs.
f     A MathMap is a Mapping which allows you to specify a set of forward
f     and/or inverse transformation functions using arithmetic operations
f     and mathematical functions similar to those available in Fortran. The
f     MathMap interprets these functions at run-time, whenever its forward
f     or inverse transformation is required. Because the functions are not
f     compiled in the normal sense (unlike an IntraMap), they may be used to
f     describe coordinate transformations in a transportable manner. A
f     MathMap therefore provides a flexible way of defining new types of
f     Mapping whose descriptions may be stored as part of a dataset and
f     interpreted by other programs.

*  Inheritance:
*     The MathMap class inherits from the Mapping class.

*  Attributes:
*     In addition to those attributes common to all Mappings, every
*     MathMap also has the following attributes:
*     - Seed: Random number seed
*     - SimpFI: Forward-inverse MathMap pairs simplify?
*     - SimpIF: Inverse-forward MathMap pairs simplify?

*  Functions:
c     The MathMap class does not define any new functions beyond those
f     The MathMap class does not define any new routines beyond those
*     which are applicable to all Mappings.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public Licence as
*     published by the Free Software Foundation; either version 2 of
*     the Licence, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public Licence for more details.
*
*     You should have received a copy of the GNU General Public Licence
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     3-SEP-1999 (RFWS):
*        Original version.
*     8-JAN-2003 (DSB):
*        Changed private InitVtab method to protected astInitMathMapVtab
*        method.
*     14-FEB-2006 (DSB):
*        Override astGetObjSize.
*     14-MAR-2006 (DSB):
*        - Add QIF function.
*        - Override astEqual method.
*     20-NOV-2006 (DSB):
*        Re-implement the Equal method to avoid use of astSimplify.
*     30-AUG-2012 (DSB):
*        Fix bug in undocumented Gaussian noise function.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS MathMap

/* Allocate pointer array. */
/* ----------------------- */
/* This macro allocates an array of pointers. If successful, each element
   of the array is initialised to NULL. */
#define MALLOC_POINTER_ARRAY(array_name,array_type,array_size) \
\
/* Allocate the array. */ \
   (array_name) = astMalloc( sizeof(array_type) * (size_t) (array_size) ); \
   if ( astOK ) { \
\
/* If successful, loop to initialise each element. */ \
      int array_index_; \
      for ( array_index_ = 0; array_index_ < (array_size); array_index_++ ) { \
         (array_name)[ array_index_ ] = NULL; \
      } \
   }

/* Free pointer array. */
/* ------------------- */
/* This macro frees a dynamically allocated array of pointers, each of
   whose elements may point at a further dynamically allocated array
   (which is also to be freed). It also allows for the possibility of any
   of the pointers being NULL. */
#define FREE_POINTER_ARRAY(array_name,array_size) \
\
/* Check that the main array pointer is not NULL. */ \
   if ( (array_name) ) { \
\
/* If OK, loop to free each of the sub-arrays. */ \
      int array_index_; \
      for ( array_index_ = 0; array_index_ < (array_size); array_index_++ ) { \
\
/* Check that each sub-array pointer is not NULL before freeing it. */ \
         if ( (array_name)[ array_index_ ] ) { \
            (array_name)[ array_index_ ] = \
               astFree( (array_name)[ array_index_ ] ); \
         } \
      } \
\
/* Free the main pointer array. */ \
      (array_name) = astFree( (array_name) ); \
   }

/* SizeOf pointer array. */
/* --------------------- */
/* This macro increments "result" by the number of bytes allocated for an
   array of pointers, each of whose elements may point at a further
   dynamically allocated array (which is also to be included). It also
   allows for the possibility of any of the pointers being NULL. */
#define SIZEOF_POINTER_ARRAY(array_name,array_size) \
\
/* Check that the main array pointer is not NULL. */ \
   if ( (array_name) ) { \
\
/* If OK, loop to measure each of the sub-arrays. */ \
      int array_index_; \
      for ( array_index_ = 0; array_index_ < (array_size); array_index_++ ) { \
\
/* Check that each sub-array pointer is not NULL before measuring it. */ \
         if ( (array_name)[ array_index_ ] ) { \
            result += astTSizeOf( (array_name)[ array_index_ ] ); \
         } \
      } \
\
/* Include the main pointer array. */ \
      result += astTSizeOf( (array_name) ); \
   }

/* Header files. */
/* ============= */
/* Interface definitions. */
/* ---------------------- */
#include "channel.h"             /* I/O channels */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "mapping.h"             /* Coordinate mappings (parent class) */
#include "cmpmap.h"              /* Compound Mappings */
#include "mathmap.h"             /* Interface definition for this class */
#include "memory.h"              /* Memory allocation facilities */
#include "globals.h"             /* Thread-safe global data access */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* Sets of points */
#include "unitmap.h"             /* Unit Mapping */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* Module Variables. */
/* ================= */
/* This type is made obscure since it is publicly accessible (but not
   useful). Provide shorthand for use within this module. */
typedef AstMathMapRandContext_ Rcontext;



/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* Pointers to parent class methods which are extended by this class. */
static int (* parent_getobjsize)( AstObject *, int * );
static AstPointSet *(* parent_transform)( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static const char *(* parent_getattrib)( AstObject *, const char *, int * );
static int (* parent_testattrib)( AstObject *, const char *, int * );
static void (* parent_clearattrib)( AstObject *, const char *, int * );
static void (* parent_setattrib)( AstObject *, const char *, int * );

/* This declaration enumerates the operation codes recognised by the
   EvaluateFunction function which evaluates arithmetic expressions. */
typedef enum {

/* User-supplied constants and variables. */
   OP_LDCON,                     /* Load constant */
   OP_LDVAR,                     /* Load variable */

/* System constants. */
   OP_LDBAD,                     /* Load bad value (AST__BAD) */
   OP_LDDIG,                     /* Load # decimal digits (DBL_DIG) */
   OP_LDEPS,                     /* Load relative precision (DBL_EPSILON) */
   OP_LDMAX,                     /* Load largest value (DBL_MAX) */
   OP_LDMAX10E,                  /* Max. decimal exponent (DBL_MAX_10_EXP) */
   OP_LDMAXE,                    /* Load maximum exponent (DBL_MAX_EXP) */
   OP_LDMDIG,                    /* Load # mantissa digits (DBL_MANT_DIG) */
   OP_LDMIN,                     /* Load smallest value (DBL_MIN) */
   OP_LDMIN10E,                  /* Min. decimal exponent (DBL_MIN_10_EXP) */
   OP_LDMINE,                    /* Load minimum exponent (DBL_MIN_EXP) */
   OP_LDRAD,                     /* Load floating radix (FLT_RADIX) */
   OP_LDRND,                     /* Load rounding mode (FLT_ROUNDS) */

/* Mathematical constants. */
   OP_LDE,                       /* Load e (base of natural logarithms) */
   OP_LDPI,                      /* Load pi */

/* Functions with one argument. */
   OP_ABS,                       /* Absolute value (sign removal) */
   OP_ACOS,                      /* Inverse cosine (radians) */
   OP_ACOSD,                     /* Inverse cosine (degrees) */
   OP_ACOSH,                     /* Inverse hyperbolic cosine */
   OP_ACOTH,                     /* Inverse hyperbolic cotangent */
   OP_ACSCH,                     /* Inverse hyperbolic cosecant */
   OP_ASECH,                     /* Inverse hyperbolic secant */
   OP_ASIN,                      /* Inverse sine (radians) */
   OP_ASIND,                     /* Inverse sine (degrees) */
   OP_ASINH,                     /* Inverse hyperbolic sine */
   OP_ATAN,                      /* Inverse tangent (radians) */
   OP_ATAND,                     /* Inverse tangent (degrees) */
   OP_ATANH,                     /* Inverse hyperbolic tangent */
   OP_CEIL,                      /* C ceil function (round up) */
   OP_COS,                       /* Cosine (radians) */
   OP_COSD,                      /* Cosine (degrees) */
   OP_COSH,                      /* Hyperbolic cosine */
   OP_COTH,                      /* Hyperbolic cotangent */
   OP_CSCH,                      /* Hyperbolic cosecant */
   OP_EXP,                       /* Exponential function */
   OP_FLOOR,                     /* C floor function (round down) */
   OP_INT,                       /* Integer value (round towards zero) */
   OP_ISBAD,                     /* Test for bad value */
   OP_LOG,                       /* Natural logarithm */
   OP_LOG10,                     /* Base 10 logarithm */
   OP_NINT,                      /* Fortran NINT function (round to nearest) */
   OP_POISS,                     /* Poisson random number */
   OP_SECH,                      /* Hyperbolic secant */
   OP_SIN,                       /* Sine (radians) */
   OP_SINC,                      /* Sinc function [= sin(x)/x] */
   OP_SIND,                      /* Sine (degrees) */
   OP_SINH,                      /* Hyperbolic sine */
   OP_SQR,                       /* Square */
   OP_SQRT,                      /* Square root */
   OP_TAN,                       /* Tangent (radians) */
   OP_TAND,                      /* Tangent (degrees) */
   OP_TANH,                      /* Hyperbolic tangent */

/* Functions with two arguments. */
   OP_ATAN2,                     /* Inverse tangent (2 arguments, radians) */
   OP_ATAN2D,                    /* Inverse tangent (2 arguments, degrees) */
   OP_DIM,                       /* Fortran DIM (positive difference) fn. */
   OP_GAUSS,                     /* Gaussian random number */
   OP_MOD,                       /* Modulus function */
   OP_POW,                       /* Raise to power */
   OP_RAND,                      /* Uniformly distributed random number */
   OP_SIGN,                      /* Transfer of sign function */

/* Functions with three arguments. */
   OP_QIF,                       /* C "question mark" operator "a?b:c" */

/* Functions with variable numbers of arguments. */
   OP_MAX,                       /* Maximum of 2 or more values */
   OP_MIN,                       /* Minimum of 2 or more values */

/* Unary arithmetic operators. */
   OP_NEG,                       /* Negate (change sign) */

/* Unary boolean operators. */
   OP_NOT,                       /* Boolean NOT */

/* Binary arithmetic operators. */
   OP_ADD,                       /* Add */
   OP_DIV,                       /* Divide */
   OP_MUL,                       /* Multiply */
   OP_SUB,                       /* Subtract */

/* Bit-shift operators. */
   OP_SHFTL,                     /* Shift bits left */
   OP_SHFTR,                     /* Shift bits right */

/* Relational operators. */
   OP_EQ,                        /* Relational equal */
   OP_GE,                        /* Greater than or equal */
   OP_GT,                        /* Greater than */
   OP_LE,                        /* Less than or equal */
   OP_LT,                        /* Less than */
   OP_NE,                        /* Not equal */

/* Bit-wise operators. */
   OP_BITAND,                    /* Bit-wise AND */
   OP_BITOR,                     /* Bit-wise OR */
   OP_BITXOR,                    /* Bit-wise exclusive OR */

/* Binary boolean operators. */
   OP_AND,                       /* Boolean AND */
   OP_EQV,                       /* Fortran logical .EQV. operation */
   OP_OR,                        /* Boolean OR */
   OP_XOR,                       /* Boolean exclusive OR */

/* Null operation. */
   OP_NULL                       /* Null operation */
} Oper;

/* This structure holds a description of each symbol which may appear
   in an expression. */
typedef struct {
   const char *text;             /* Symbol text as it appears in expressions */
   const int size;               /* Size of symbol text */
   const int operleft;           /* An operator when seen from the left? */
   const int operright;          /* An operator when seen from the right? */
   const int unarynext;          /* May be followed by a unary +/- ? */
   const int unaryoper;          /* Is a unary +/- ? */
   const int leftpriority;       /* Priority when seen from the left */
   const int rightpriority;      /* Priority when seen from the right */
   const int parincrement;       /* Change in parenthesis level */
   const int stackincrement;     /* Change in evaluation stack size */
   const int nargs;              /* Number of function arguments */
   const Oper opcode;            /* Resulting operation code */
} Symbol;

/* This initialises an array of Symbol structures to hold data on all
   the supported symbols. The order is not important, but symbols are
   arranged here in approximate order of descending evaluation
   priority. The end of the array is indicated by an element with a NULL
   "text" component. */
static const Symbol symbol[] = {

/* User-supplied constants and variables. */
   { ""            ,  0,  0,  0,  0,  0, 19, 19,  0,  1,  0,  OP_LDCON    },
   { ""            ,  0,  0,  0,  0,  0, 19, 19,  0,  1,  0,  OP_LDVAR    },

/* System constants. */
   { "<bad>"       ,  5,  0,  0,  0,  0, 19, 19,  0,  1,  0,  OP_LDBAD    },
   { "<dig>"       ,  5,  0,  0,  0,  0, 19, 19,  0,  1,  0,  OP_LDDIG    },
   { "<epsilon>"   ,  9,  0,  0,  0,  0, 19, 19,  0,  1,  0,  OP_LDEPS    },
   { "<mant_dig>"  , 10,  0,  0,  0,  0, 19, 19,  0,  1,  0,  OP_LDMDIG   },
   { "<max>"       ,  5,  0,  0,  0,  0, 19, 19,  0,  1,  0,  OP_LDMAX    },
   { "<max_10_exp>", 12,  0,  0,  0,  0, 19, 19,  0,  1,  0,  OP_LDMAX10E },
   { "<max_exp>"   ,  9,  0,  0,  0,  0, 19, 19,  0,  1,  0,  OP_LDMAXE   },
   { "<min>"       ,  5,  0,  0,  0,  0, 19, 19,  0,  1,  0,  OP_LDMIN    },
   { "<min_10_exp>", 12,  0,  0,  0,  0, 19, 19,  0,  1,  0,  OP_LDMIN10E },
   { "<min_exp>"   ,  9,  0,  0,  0,  0, 19, 19,  0,  1,  0,  OP_LDMINE   },
   { "<radix>"     ,  7,  0,  0,  0,  0, 19, 19,  0,  1,  0,  OP_LDRAD    },
   { "<rounds>"    ,  8,  0,  0,  0,  0, 19, 19,  0,  1,  0,  OP_LDRND    },

/* Mathematical constants. */
   { "<e>"         ,  3,  0,  0,  0,  0, 19, 19,  0,  1,  0,  OP_LDE      },
   { "<pi>"        ,  4,  0,  0,  0,  0, 19, 19,  0,  1,  0,  OP_LDPI     },

/* Functions with one argument. */
   { "abs("        ,  4,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_ABS      },
   { "acos("       ,  5,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_ACOS     },
   { "acosd("      ,  6,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_ACOSD    },
   { "acosh("      ,  6,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_ACOSH    },
   { "acoth("      ,  6,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_ACOTH    },
   { "acsch("      ,  6,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_ACSCH    },
   { "aint("       ,  5,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_INT      },
   { "asech("      ,  6,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_ASECH    },
   { "asin("       ,  5,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_ASIN     },
   { "asind("      ,  6,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_ASIND    },
   { "asinh("      ,  6,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_ASINH    },
   { "atan("       ,  5,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_ATAN     },
   { "atand("      ,  6,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_ATAND    },
   { "atanh("      ,  6,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_ATANH    },
   { "ceil("       ,  5,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_CEIL     },
   { "cos("        ,  4,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_COS      },
   { "cosd("       ,  5,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_COSD     },
   { "cosh("       ,  5,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_COSH     },
   { "coth("       ,  5,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_COTH     },
   { "csch("       ,  5,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_CSCH     },
   { "exp("        ,  4,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_EXP      },
   { "fabs("       ,  5,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_ABS      },
   { "floor("      ,  6,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_FLOOR    },
   { "int("        ,  4,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_INT      },
   { "isbad("      ,  6,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_ISBAD    },
   { "log("        ,  4,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_LOG      },
   { "log10("      ,  6,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_LOG10    },
   { "nint("       ,  5,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_NINT     },
   { "poisson("    ,  8,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_POISS    },
   { "sech("       ,  5,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_SECH     },
   { "sin("        ,  4,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_SIN      },
   { "sinc("       ,  5,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_SINC     },
   { "sind("       ,  5,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_SIND     },
   { "sinh("       ,  5,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_SINH     },
   { "sqr("        ,  4,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_SQR      },
   { "sqrt("       ,  5,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_SQRT     },
   { "tan("        ,  4,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_TAN      },
   { "tand("       ,  5,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_TAND     },
   { "tanh("       ,  5,  0,  1,  1,  0, 19,  1,  1,  0,  1,  OP_TANH     },

/* Functions with two arguments. */
   { "atan2("      ,  6,  0,  1,  1,  0, 19,  1,  1, -1,  2,  OP_ATAN2    },
   { "atan2d("     ,  7,  0,  1,  1,  0, 19,  1,  1, -1,  2,  OP_ATAN2D   },
   { "dim("        ,  4,  0,  1,  1,  0, 19,  1,  1, -1,  2,  OP_DIM      },
   { "fmod("       ,  5,  0,  1,  1,  0, 19,  1,  1, -1,  2,  OP_MOD      },
   { "gauss("      ,  6,  0,  1,  1,  0, 19,  1,  1, -1,  2,  OP_GAUSS    },
   { "mod("        ,  4,  0,  1,  1,  0, 19,  1,  1, -1,  2,  OP_MOD      },
   { "pow("        ,  4,  0,  1,  1,  0, 19,  1,  1, -1,  2,  OP_POW      },
   { "rand("       ,  5,  0,  1,  1,  0, 19,  1,  1, -1,  2,  OP_RAND     },
   { "sign("       ,  5,  0,  1,  1,  0, 19,  1,  1, -1,  2,  OP_SIGN     },

/* Functions with two arguments. */
   { "qif("        ,  4,  0,  1,  1,  0, 19,  1,  1, -2,  3,  OP_QIF      },

/* Functions with variable numbers of arguments. */
   { "max("        ,  4,  0,  1,  1,  0, 19,  1,  1, -1, -2,  OP_MAX      },
   { "min("        ,  4,  0,  1,  1,  0, 19,  1,  1, -1, -2,  OP_MIN      },

/* Parenthesised expressions. */
   { ")"           ,  1,  1,  0,  0,  0,  2, 19, -1,  0,  0,  OP_NULL     },
   { "("           ,  1,  0,  1,  1,  0, 19,  1,  1,  0,  0,  OP_NULL     },

/* Unary arithmetic operators. */
   { "+"           ,  1,  0,  1,  1,  1, 17, 16,  0,  0,  0,  OP_NULL     },
   { "-"           ,  1,  0,  1,  1,  1, 17, 16,  0,  0,  0,  OP_NEG      },

/* Unary boolean operators. */
   { "!"           ,  1,  0,  1,  1,  0, 17, 16,  0,  0,  0,  OP_NOT      },
   { ".not."       ,  5,  0,  1,  1,  0, 17, 16,  0,  0,  0,  OP_NOT      },

/* Binary arithmetic operators. */
   { "**"          ,  2,  1,  1,  1,  0, 18, 15,  0, -1,  0,  OP_POW      },
   { "*"           ,  1,  1,  1,  1,  0, 14, 14,  0, -1,  0,  OP_MUL      },
   { "/"           ,  1,  1,  1,  1,  0, 14, 14,  0, -1,  0,  OP_DIV      },
   { "+"           ,  1,  1,  1,  1,  0, 13, 13,  0, -1,  0,  OP_ADD      },
   { "-"           ,  1,  1,  1,  1,  0, 13, 13,  0, -1,  0,  OP_SUB      },

/* Bit-shift operators. */
   { "<<"          ,  2,  1,  1,  1,  0, 12, 12,  0, -1,  0,  OP_SHFTL    },
   { ">>"          ,  2,  1,  1,  1,  0, 12, 12,  0, -1,  0,  OP_SHFTR    },

/* Relational operators. */
   { "<"           ,  1,  1,  1,  1,  0, 11, 11,  0, -1,  0,  OP_LT       },
   { ".lt."        ,  4,  1,  1,  1,  0, 11, 11,  0, -1,  0,  OP_LT       },
   { "<="          ,  2,  1,  1,  1,  0, 11, 11,  0, -1,  0,  OP_LE       },
   { ".le."        ,  4,  1,  1,  1,  0, 11, 11,  0, -1,  0,  OP_LE       },
   { ">"           ,  1,  1,  1,  1,  0, 11, 11,  0, -1,  0,  OP_GT       },
   { ".gt."        ,  4,  1,  1,  1,  0, 11, 11,  0, -1,  0,  OP_GT       },
   { ">="          ,  2,  1,  1,  1,  0, 11, 11,  0, -1,  0,  OP_GE       },
   { ".ge."        ,  4,  1,  1,  1,  0, 11, 11,  0, -1,  0,  OP_GE       },
   { "=="          ,  2,  1,  1,  1,  0, 10, 10,  0, -1,  0,  OP_EQ       },
   { ".eq."        ,  4,  1,  1,  1,  0, 10, 10,  0, -1,  0,  OP_EQ       },
   { "!="          ,  2,  1,  1,  1,  0, 10, 10,  0, -1,  0,  OP_NE       },
   { ".ne."        ,  4,  1,  1,  1,  0, 10, 10,  0, -1,  0,  OP_NE       },

/* Bit-wise operators. */
   { "&"           ,  1,  1,  1,  1,  0,  9,  9,  0, -1,  0,  OP_BITAND   },
   { "^"           ,  1,  1,  1,  1,  0,  8,  8,  0, -1,  0,  OP_BITXOR   },
   { "|"           ,  1,  1,  1,  1,  0,  7,  7,  0, -1,  0,  OP_BITOR    },

/* Binary boolean operators. */
   { "&&"          ,  2,  1,  1,  1,  0,  6,  6,  0, -1,  0,  OP_AND      },
   { ".and."       ,  5,  1,  1,  1,  0,  6,  6,  0, -1,  0,  OP_AND      },
   { "^^"          ,  2,  1,  1,  1,  0,  5,  5,  0, -1,  0,  OP_XOR      },
   { "||"          ,  2,  1,  1,  1,  0,  4,  4,  0, -1,  0,  OP_OR       },
   { ".or."        ,  4,  1,  1,  1,  0,  4,  4,  0, -1,  0,  OP_OR       },
   { ".eqv."       ,  5,  1,  1,  1,  0,  3,  3,  0, -1,  0,  OP_EQV      },
   { ".neqv."      ,  6,  1,  1,  1,  0,  3,  3,  0, -1,  0,  OP_XOR      },
   { ".xor."       ,  5,  1,  1,  1,  0,  3,  3,  0, -1,  0,  OP_XOR      },

/* Separators. */
   { ","           ,  1,  1,  1,  1,  0,  2,  2,  0,  0,  0,  OP_NULL     },

/* End of symbol data. */
   { NULL          ,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  OP_NULL     }
};

/* These variables identify indices in the above array which hold
   special symbols used explicitly in the code. */
static const int symbol_ldcon = 0; /* Load a constant */
static const int symbol_ldvar = 1; /* Load a variable */

/* Define macros for accessing each item of thread specific global data. */
#ifdef THREAD_SAFE

/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0; \
   globals->GetAttrib_Buff[ 0 ] = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(MathMap)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(MathMap,Class_Init)
#define class_vtab astGLOBAL(MathMap,Class_Vtab)
#define getattrib_buff astGLOBAL(MathMap,GetAttrib_Buff)



static pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;
#define LOCK_MUTEX2 pthread_mutex_lock( &mutex2 );
#define UNLOCK_MUTEX2 pthread_mutex_unlock( &mutex2 );

static pthread_mutex_t mutex3 = PTHREAD_MUTEX_INITIALIZER;
#define LOCK_MUTEX3 pthread_mutex_lock( &mutex3 );
#define UNLOCK_MUTEX3 pthread_mutex_unlock( &mutex3 );

static pthread_mutex_t mutex4 = PTHREAD_MUTEX_INITIALIZER;
#define LOCK_MUTEX4 pthread_mutex_lock( &mutex4 );
#define UNLOCK_MUTEX4 pthread_mutex_unlock( &mutex4 );

static pthread_mutex_t mutex5 = PTHREAD_MUTEX_INITIALIZER;
#define LOCK_MUTEX5 pthread_mutex_lock( &mutex5 );
#define UNLOCK_MUTEX5 pthread_mutex_unlock( &mutex5 );

static pthread_mutex_t mutex6 = PTHREAD_MUTEX_INITIALIZER;
#define LOCK_MUTEX6 pthread_mutex_lock( &mutex6 );
#define UNLOCK_MUTEX6 pthread_mutex_unlock( &mutex6 );

static pthread_mutex_t mutex7 = PTHREAD_MUTEX_INITIALIZER;
#define LOCK_MUTEX7 pthread_mutex_lock( &mutex7 );
#define UNLOCK_MUTEX7 pthread_mutex_unlock( &mutex7 );

/* If thread safety is not needed, declare and initialise globals at static
   variables. */
#else

static char getattrib_buff[ 51 ];


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstMathMapVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#define LOCK_MUTEX2
#define UNLOCK_MUTEX2

#define LOCK_MUTEX3
#define UNLOCK_MUTEX3

#define LOCK_MUTEX4
#define UNLOCK_MUTEX4

#define LOCK_MUTEX5
#define UNLOCK_MUTEX5

#define LOCK_MUTEX6
#define UNLOCK_MUTEX6

#define LOCK_MUTEX7
#define UNLOCK_MUTEX7

#endif


/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstMathMap *astMathMapId_( int, int, int, const char *[], int, const char *[], const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static int GetObjSize( AstObject *, int * );
static const char *GetAttrib( AstObject *, const char *, int * );
static double Gauss( Rcontext *, int * );
static double LogGamma( double, int * );
static double Poisson( Rcontext *, double, int * );
static double Rand( Rcontext *, int * );
static int DefaultSeed( const Rcontext *, int * );
static int Equal( AstObject *, AstObject *, int * );
static int GetSeed( AstMathMap *, int * );
static int GetSimpFI( AstMathMap *, int * );
static int GetSimpIF( AstMathMap *, int * );
static int MapMerge( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
static int TestAttrib( AstObject *, const char *, int * );
static int TestSeed( AstMathMap *, int * );
static int TestSimpFI( AstMathMap *, int * );
static int TestSimpIF( AstMathMap *, int * );
static void CleanFunctions( int, const char *[], char ***, int * );
static void ClearAttrib( AstObject *, const char *, int * );
static void ClearSeed( AstMathMap *, int * );
static void ClearSimpFI( AstMathMap *, int * );
static void ClearSimpIF( AstMathMap *, int * );
static void CompileExpression( const char *, const char *, const char *, int, const char *[], int **, double **, int *, int * );
static void CompileMapping( const char *, const char *, int, int, int, const char *[], int, const char *[], int ***, int ***, double ***, double ***, int *, int *, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void EvaluateFunction( Rcontext *, int, const double **, const int *, const double *, int, double *, int * );
static void EvaluationSort( const double [], int, int [], int **, int *, int * );
static void ExtractExpressions( const char *, const char *, int, const char *[], int, char ***, int * );
static void ExtractVariables( const char *, const char *, int, const char *[], int, int, int, int, int, char ***, int * );
static void ParseConstant( const char *, const char *, const char *, int, int *, double *, int * );
static void ParseName( const char *, int, int *, int * );
static void ParseVariable( const char *, const char *, const char *, int, int, const char *[], int *, int *, int * );
static void SetAttrib( AstObject *, const char *, int * );
static void SetSeed( AstMathMap *, int, int * );
static void SetSimpFI( AstMathMap *, int, int * );
static void SetSimpIF( AstMathMap *, int, int * );
static void ValidateSymbol( const char *, const char *, const char *, int, int, int *, int **, int **, int *, double **, int * );

/* Member functions. */
/* ================= */
static void CleanFunctions( int nfun, const char *fun[], char ***clean, int *status ) {
/*
*  Name:
*     CleanFunctions

*  Purpose:
*     Make a clean copy of a set of functions.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mathmap.h"
*     void CleanFunctions( int nfun, const char *fun[], char ***clean, int *status )

*  Class Membership:
*     MathMap member function.

*  Description:
*     This function copies an array of strings, eliminating any white space
*     characters and converting to lower case. It is intended for cleaning
*     up arrays of function definitions prior to compilation. The returned
*     copy is stored in dynamically allocated memory.

*  Parameters:
*     nfun
*        The number of functions to be cleaned.
*     fun
*        Pointer to an array, with "nfun" elements, of pointers to null
*        terminated strings which contain each of the functions.
*     clean
*        Address in which to return a pointer to an array (with "nfun"
*        elements) of pointers to null terminated strings containing the
*        cleaned functions (i.e. this returns an array of strings).
*
*        Both the returned array of pointers, and the strings to which they
*        point, will be dynamically allocated and should be freed by the
*        caller (using astFree) when no longer required.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*        - A NULL value will be returned for "*clean" if this function is
*        invoked with the global error status set, or if it should fail for
*        any reason.
*/

/* Local Variables: */
   char c;                       /* Character from function string */
   int i;                        /* Loop counter for characters */
   int ifun;                     /* Loop counter for functions */
   int nc;                       /* Count of non-blank characters */

/* Initialise. */
   *clean = NULL;

/* Check the global error status. */
   if ( !astOK ) return;

/* Allocate and initialise an array to hold the returned pointers. */
   MALLOC_POINTER_ARRAY( *clean, char *, nfun )

/* Loop through all the input functions. */
   if ( astOK ) {
      for ( ifun = 0; ifun < nfun; ifun++ ) {

/* Count the number of non-blank characters in each function string. */
         nc = 0;
         for ( i = 0; ( c = fun[ ifun ][ i ] ); i++ ) nc += !isspace( c );

/* Allocate a string long enough to hold the function with all the
   white space removed, storing its pointer in the array allocated
   earlier. Check for errors. */
         ( *clean )[ ifun ] = astMalloc( sizeof( char ) *
                                         (size_t) ( nc + 1 ) );
         if ( !astOK ) break;

/* Loop to copy the non-blank function characters into the new
   string. */
         nc = 0;
         for ( i = 0; ( c = fun[ ifun ][ i ] ); i++ ) {
            if ( !isspace( c ) ) ( *clean )[ ifun ][ nc++ ] = tolower( c );
         }

/* Null-terminate the result. */
         ( *clean )[ ifun ][ nc ] = '\0';
      }

/* If an error occurred, then free the main pointer array together
   with any strings that have been allocated, resetting the output
   value. */
      if ( !astOK ) {
         FREE_POINTER_ARRAY( *clean, nfun )
      }
   }
}

static void ClearAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for a MathMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mathmap.h"
*     void ClearAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     MathMap member function (over-rides the astClearAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function clears the value of a specified attribute for a
*     MathMap, so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the MathMap.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstMathMap *this;             /* Pointer to the MathMap structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the MathMap structure. */
   this = (AstMathMap *) this_object;

/* Check the attribute name and clear the appropriate attribute. */

/* Seed. */
/* ----- */
   if ( !strcmp( attrib, "seed" ) ) {
      astClearSeed( this );

/* SimpFI. */
/* ------- */
   } else if ( !strcmp( attrib, "simpfi" ) ) {
      astClearSimpFI( this );

/* SimpIF. */
/* ------- */
   } else if ( !strcmp( attrib, "simpif" ) ) {
      astClearSimpIF( this );

/* If the attribute is not recognised, pass it on to the parent method
   for further interpretation. */
   } else {
      (*parent_clearattrib)( this_object, attrib, status );
   }
}

static void CompileExpression( const char *method, const char *class,
                               const char *exprs, int nvar, const char *var[],
                               int **code, double **con, int *stacksize, int *status ) {
/*
*  Name:
*     CompileExpression

*  Purpose:
*     Compile a mathematical expression.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mathmap.h"
*     void CompileExpression( const char *method, const char *class,
*                             const char *exprs, int nvar, const char *var[],
*                             int **code, double **con, int *stacksize )

*  Class Membership:
*     MathMap member function.

*  Description:
*     This function checks and compiles a mathematical expression. It
*     produces a sequence of operation codes (opcodes) and a set of
*     numerical constants which may subsequently be used to evaluate the
*     expression on a push-down stack.

*  Parameters:
*     method
*        Pointer to a constant null-terminated character string
*        containing the name of the method that invoked this function.
*        This method name is used solely for constructing error messages.
*     class
*        Pointer to a constant null-terminated character string containing the
*        class name of the Object being processed. This name is used solely
*        for constructing error messages.
*     exprs
*        Pointer to a null-terminated string containing the expression
*        to be compiled. This is case sensitive and should contain no white
*        space.
*     nvar
*        The number of variable names defined for use in the expression.
*     var
*        An array of pointers (with "nvar" elements) to null-terminated
*        strings. Each of these should contain a variable name which may
*        appear in the expression. These strings are case sensitive and
*        should contain no white space.
*     code
*        Address of a pointer which will be set to point at a dynamically
*        allocated array of int containing the set of opcodes (cast to int)
*        produced by this function. The first element of this array will
*        contain a count of the number of opcodes which follow.
*
*        The allocated space must be freed by the caller (using astFree) when
*        no longer required.
*     con
*        Address of a pointer which will be set to point at a dynamically
*        allocated array of double containing the set of constants
*        produced by this function (this may be NULL if no constants are
*        produced).
*
*        The allocated space must be freed by the caller (using astFree) when
*        no longer required.
*     stacksize
*        Pointer to an int in which to return the size of the push-down stack
*        required to evaluate the expression using the returned opcodes and
*        constants.

*  Algorithm:
*     The function passes through the input expression searching for
*     symbols. It looks for standard symbols (arithmetic operators,
*     parentheses, function calls and delimiters) in the next part of the
*     expression to be parsed, using identification information stored in
*     the static "symbol" array. It ignores certain symbols, according to
*     whether they appear to be operators or operands. The choice depends on
*     what the previous symbol was; for instance, two operators may not
*     occur in succession. Unary +/- operators are also ignored in
*     situations where they are not permitted.
*
*     If a standard symbol is found, it is passed to the ValidateSymbol
*     function, which keeps track of the current level of parenthesis in the
*     expression and of the number of arguments supplied to any (possibly
*     nested) function calls. This function then accepts or rejects the
*     symbol according to whether it is valid within the current context. An
*     error is reported if it is rejected.
*
*     If the part of the expression currently being parsed did not contain a
*     standard symbol, an attempt is made to parse it first as a constant,
*     then as a variable name. If either of these succeeds, an appropriate
*     symbol number is added to the list of symbols identified so far, and a
*     value is added to the list of constants - this is either the value of
*     the constant itself, or the identification number of the variable. If
*     the expression cannot be parsed, an error is reported.
*
*     When the entire expression has been analysed as a sequence of symbols
*     (and associated constants), the EvaluationSort function is
*     invoked. This sorts the symbols into evaluation order, which is the
*     order in which the associated operations must be performed on a
*     push-down arithmetic stack to evaluate the expression. This routine
*     also substitutes operation codes (defined in the "Oper" enum) for the
*     symbol numbers and calculates the size of evaluation stack which will
*     be required.

*  Notes:
*     - A value of NULL will be returned for the "*code" and "*con" pointers
*     and a value of zero will be returned for the "*stacksize" value if this
*     function is invoked with the global error status set, or if it should
*     fail for any reason.
*/

/* Local Variables: */
   double c;                     /* Value of parsed constant */
   int *argcount;                /* Array of argument count information */
   int *opensym;                 /* Array of opening parenthesis information */
   int *symlist;                 /* Array of symbol indices */
   int found;                    /* Standard symbol identified? */
   int iend;                     /* Ending index in the expression string */
   int istart;                   /* Staring index in the expression string */
   int isym;                     /* Loop counter for symbols */
   int ivar;                     /* Index of variable name */
   int lpar;                     /* Parenthesis level */
   int ncon;                     /* Number of constants generated */
   int nsym;                     /* Number of symbols identified */
   int opernext;                 /* Next symbol an operator (from left)? */
   int size;                     /* Size of symbol matched */
   int sym;                      /* Index of symbol in static "symbol" array */
   int unarynext;                /* Next symbol may be unary +/- ? */

/* Initialise. */
   *code = NULL;
   *con = NULL;
   *stacksize = 0;

/* Check the global error status. */
   if ( !astOK ) return;

/* Further initialisation. */
   argcount = NULL;
   lpar = 0;
   ncon = 0;
   nsym = 0;
   opensym = NULL;
   symlist = NULL;
   sym = 0;
   ivar = 0;

/* The first symbol to be encountered must not look like an operator
   from the left. It may be a unary + or - operator. */
   opernext = 0;
   unarynext = 1;

/* Search through the expression to classify each symbol which appears
   in it. Stop when there are no more input characters or an error is
   detected. */
   istart = 0;
   for ( istart = 0; astOK && exprs[ istart ]; istart = iend + 1 ) {

/* Compare each of the symbols in the symbol data with the next
   section of the expression, looking for the longest symbol text which
   will match. Stop if a NULL "text" value is found, which acts as the
   end flag. */
      found = 0;
      size = 0;
      for ( isym = 0; symbol[ isym ].text; isym++ ) {

/* Only consider symbols which have text associated with them and
   which look like operators or operands from the left, according to the
   setting of the "opernext" flag. Thus, if an operator or operand is
   missing from the input expression, the next symbol will not be
   identified, because it will be of the wrong type. Also exclude unary
   +/- operators if they are out of context. */
         if ( symbol[ isym ].size &&
              ( symbol[ isym ].operleft == opernext ) &&
              ( !symbol[ isym ].unaryoper || unarynext ) ) {

/* Test if the text of the symbol matches the expression at the
   current position. If so, note that a match has been found. */
            if ( !strncmp( exprs + istart, symbol[ isym ].text,
                           (size_t) symbol[ isym ].size ) ) {
               found = 1;

/* If this symbol matches more characters than any previous symbol,
   then store the symbol's index and note its size. */
               if ( symbol[ isym ].size > size ) {
                  sym = isym;
                  size = symbol[ isym ].size;

/* Calculate the index of the last symbol character in the expression
   string. */
                  iend = istart + size - 1;
               }
            }
         }
      }

/* If the symbol was identified as one of the standard symbols, then
   validate it, updating the parenthesis level and argument count
   information at the same time. */
      if ( found ) {
         ValidateSymbol( method, class, exprs, iend, sym, &lpar, &argcount,
                         &opensym, &ncon, con, status );

/* If it was not one of the standard symbols, then check if the next
   symbol was expected to be an operator. If so, then there is a missing
   operator, so report an error. */
      } else {
         if ( opernext ) {
            astError( AST__MIOPR,
                      "%s(%s): Missing or invalid operator in the expression "
                      "\"%.*s\".", status,
                      method, class, istart + 1, exprs );

/* If the next symbol was expected to be an operand, then it may be a
   constant, so try to parse it as one. */
         } else {
            ParseConstant( method, class, exprs, istart, &iend, &c, status );
            if ( astOK ) {

/* If successful, set the symbol number to "symbol_ldcon" (load
   constant) and extend the "*con" array to accommodate a new
   constant. Check for errors. */
               if ( iend >= istart ) {
                  sym = symbol_ldcon;
                  *con = astGrow( *con, ncon + 1, sizeof( double ) );
                  if ( astOK ) {

/* Append the constant to the "*con" array. */
                     ( *con )[ ncon++ ] = c;
                  }

/* If the symbol did not parse as a constant, then it may be a
   variable name, so try to parse it as one. */
               } else {
                  ParseVariable( method, class, exprs, istart, nvar, var,
                                 &ivar, &iend, status );
                  if ( astOK ) {

/* If successful, set the symbol to "symbol_ldvar" (load variable) and
   extend the "*con" array to accommodate a new constant. Check for
   errors. */
                     if ( ivar != -1 ) {
                        sym = symbol_ldvar;
                        *con = astGrow( *con, ncon + 1, sizeof( double ) );
                        if ( astOK ) {

/* Append the variable identification number as a constant to the
   "*con" array. */
                           ( *con )[ ncon++ ] = (double) ivar;
                        }

/* If the expression did not parse as a variable name, then there is a
   missing operand in the expression, so report an error. */
                     } else {
                        astError( AST__MIOPA,
                                  "%s(%s): Missing or invalid operand in the "
                                  "expression \"%.*s\".", status,
                                  method, class, istart + 1, exprs );
                     }
                  }
               }
            }
         }
      }

/* If there has been no error, then the next symbol in the input
   expression has been identified and is valid. */
      if ( astOK ) {

/* Decide whether the next symbol should look like an operator or an
   operand from the left. This is determined by the nature of the symbol
   just identified (seen from the right) - two operands or two operators
   cannot be adjacent. */
         opernext = !symbol[ sym ].operright;

/* Also decide whether the next symbol may be a unary +/- operator,
   according to the "unarynext" symbol data entry for the symbol just
   identified. */
         unarynext = symbol[ sym ].unarynext;

/* Extend the "symlist" array to accommodate the symbol just
   identified. Check for errors. */
         symlist = astGrow( symlist, nsym + 1, sizeof( int ) );
         if ( astOK ) {

/* Append the symbol's index to the end of this list. */
            symlist[ nsym++ ] = sym;
         }
      }
   }

/* If there has been no error, check the final context after
   identifying all the symbols... */
   if ( astOK ) {

/* If an operand is still expected, then there is an unsatisfied
   operator on the end of the expression, so report an error. */
      if ( !opernext ) {
         astError( AST__MIOPA,
                   "%s(%s): Missing or invalid operand in the expression "
                   "\"%s\".", status,
                   method, class, exprs );

/* If the final parenthesis level is positive, then there is a missing
   right parenthesis, so report an error. */
      } else if ( lpar > 0 ) {
         astError( AST__MRPAR,
                   "%s(%s): Missing right parenthesis in the expression "
                   "\"%s\".", status,
                   method, class, exprs );
      }
   }

/* Sort the symbols into evaluation order to produce output opcodes. */
   EvaluationSort( *con, nsym, symlist, code, stacksize, status );

/* Free any memory used as workspace. */
   if ( argcount ) argcount = astFree( argcount );
   if ( opensym ) opensym = astFree( opensym );
   if ( symlist ) symlist = astFree( symlist );

/* If OK, re-allocate the "*con" array to have the correct size (since
   astGrow may have over-allocated space). */
   if ( astOK && *con ) {
      *con = astRealloc( *con, sizeof( double ) * (size_t) ncon );
   }

/* If an error occurred, free any allocated memory and reset the
   output values. */
   if ( !astOK ) {
      *code = astFree( *code );
      *con = astFree( *con );
      *stacksize = 0;
   }
}

static void CompileMapping( const char *method, const char *class,
                            int nin, int nout,
                            int nfwd, const char *fwdfun[],
                            int ninv, const char *invfun[],
                            int ***fwdcode, int ***invcode,
                            double ***fwdcon, double ***invcon,
                            int *fwdstack, int *invstack, int *status ) {
/*
*  Name:
*     CompileMapping

*  Purpose:
*     Compile the transformation functions for a MathMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mathmap.h"
*     void CompileMapping( const char *method, const char *class,
*                          int nin, int nout,
*                          int nfwd, const char *fwdfun[],
*                          int ninv, const char *invfun[],
*                          int ***fwdcode, int ***invcode,
*                          double ***fwdcon, double ***invcon,
*                          int *fwdstack, int *invstack, int *status )

*  Class Membership:
*     MathMap member function.

*  Description:
*     This function checks and compiles the transformation functions required
*     to create a MathMap. It produces sequences of operation codes (opcodes)
*     and numerical constants which may subsequently be used to evaluate the
*     functions on a push-down stack.

*  Parameters:
*     method
*        Pointer to a constant null-terminated character string
*        containing the name of the method that invoked this function.
*        This method name is used solely for constructing error messages.
*     class
*        Pointer to a constant null-terminated character string containing the
*        class name of the Object being processed. This name is used solely
*        for constructing error messages.
*     nin
*        Number of input variables for the MathMap.
*     nout
*        Number of output variables for the MathMap.
*     nfwd
*        The number of forward transformation functions being supplied.
*        This must be at least equal to "nout".
*     fwdfun
*        Pointer to an array, with "nfwd" elements, of pointers to null
*        terminated strings which contain each of the forward transformation
*        functions. These must be in lower case and should contain no white
*        space.
*     ninv
*        The number of inverse transformation functions being supplied.
*        This must be at least equal to "nin".
*     invfun
*        Pointer to an array, with "ninv" elements, of pointers to null
*        terminated strings which contain each of the inverse transformation
*        functions. These must be in lower case and should contain no white
*        space.
*     fwdcode
*        Address in which to return a pointer to an array (with "nfwd"
*        elements) of pointers to arrays of int containing the set of opcodes
*        (cast to int) for each forward transformation function. The number
*        of opcodes produced for each function is given by the first element
*        of the opcode array.
*
*        Both the returned array of pointers, and the arrays of int to which
*        they point, will be stored in dynamically allocated memory and should
*        be freed by the caller (using astFree) when no longer required.
*
*        If the right hand sides (including the "=" sign) of all the supplied
*        functions are absent, then this indicates an undefined transformation
*        and the returned pointer value will be NULL. An error results if
*        an "=" sign is present but no expression follows it.
*     invcode
*        Address in which to return a pointer to an array (with "ninv"
*        elements) of pointers to arrays of int containing the set of opcodes
*        (cast to int) for each inverse transformation function. The number
*        of opcodes produced for each function is given by the first element
*        of the opcode array.
*
*        Both the returned array of pointers, and the arrays of int to which
*        they point, will be stored in dynamically allocated memory and should
*        be freed by the caller (using astFree) when no longer required.
*
*        If the right hand sides (including the "=" sign) of all the supplied
*        functions are absent, then this indicates an undefined transformation
*        and the returned pointer value will be NULL. An error results if
*        an "=" sign is present but no expression follows it.
*     fwdcon
*        Address in which to return a pointer to an array (with "nfwd"
*        elements) of pointers to arrays of double containing the set of
*        constants for each forward transformation function.
*
*        Both the returned array of pointers, and the arrays of double to which
*        they point, will be stored in dynamically allocated memory and should
*        be freed by the caller (using astFree) when no longer required. Note
*        that any of the pointers to the arrays of double may be NULL if no
*        constants are associated with a particular function.
*
*        If the forward transformation is undefined, then the returned pointer
*        value will be NULL.
*     invcon
*        Address in which to return a pointer to an array (with "ninv"
*        elements) of pointers to arrays of double containing the set of
*        constants for each inverse transformation function.
*
*        Both the returned array of pointers, and the arrays of double to which
*        they point, will be stored in dynamically allocated memory and should
*        be freed by the caller (using astFree) when no longer required. Note
*        that any of the pointers to the arrays of double may be NULL if no
*        constants are associated with a particular function.
*
*        If the inverse transformation is undefined, then the returned pointer
*        value will be NULL.
*     fwdstack
*        Pointer to an int in which to return the size of the push-down stack
*        required to evaluate the forward transformation functions.
*     invstack
*        Pointer to an int in which to return the size of the push-down stack
*        required to evaluate the inverse transformation functions.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - A value of NULL will be returned for the "*fwdcode", "*invcode",
*     "*fwdcon" and "*invcon" pointers and a value of zero will be returned
*     for the "*fwdstack" and "*invstack" values if this function is invoked
*     with the global error status set, or if it should fail for any reason.
*/

/* Local Variables: */
   char **exprs;                 /* Pointer to array of expressions */
   char **var;                   /* Pointer to array of variable names */
   const char **strings;         /* Pointer to temporary array of strings */
   int ifun;                     /* Loop counter for functions */
   int nvar;                     /* Number of variables to extract */
   int stacksize;                /* Required stack size */

/* Initialise. */
   *fwdcode = NULL;
   *invcode = NULL;
   *fwdcon = NULL;
   *invcon = NULL;
   *fwdstack = 0;
   *invstack = 0;
   nvar = 0;

/* Check the global error status. */
   if ( !astOK ) return;

/* Further initialisation. */
   exprs = NULL;
   var = NULL;

/* Compile the forward transformation. */
/* ----------------------------------- */
/* Allocate space for an array of pointers to the functions from which
   we will extract variable names. */
   strings = astMalloc( sizeof( char * ) * (size_t) ( nin + nfwd ) );

/* Fill the first elements of this array with pointers to the inverse
   transformation functions ("nin" in number) which yield the final input
   values. These will have the names of the input variables on their left
   hand sides. */
   if ( astOK ) {
      nvar = 0;
      for ( ifun = ninv - nin; ifun < ninv; ifun++ ) {
         strings[ nvar++ ] = invfun[ ifun ];
      }

/* Fill the remaining elements of the array with pointers to the
   forward transformation functions. These will have the names of any
   intermediate variables plus the final output variables on their left
   hand sides. */
      for ( ifun = 0; ifun < nfwd; ifun++ ) strings[ nvar++ ] = fwdfun[ ifun ];

/* Extract the variable names from the left hand sides of these
   functions and check them for validity and absence of duplication. */
      ExtractVariables( method, class, nvar, strings, nin, nout, nfwd, ninv, 1,
                        &var, status );
   }

/* Free the temporary array of string pointers. */
   strings = astFree( strings );

/* Extract the expressions from the right hand sides of the forward
   transformation functions. */
   ExtractExpressions( method, class, nfwd, fwdfun, 1, &exprs, status );

/* If OK, and the forward transformation is defined, then allocate and
   initialise space for an array of pointers to the opcodes for each
   expression and, similarly, for the constants for each expression. */
   if ( astOK && exprs ) {
      MALLOC_POINTER_ARRAY( *fwdcode, int *, nfwd )
      MALLOC_POINTER_ARRAY( *fwdcon, double *, nfwd )

/* If OK, loop to compile each of the expressions, storing pointers to
   the resulting opcodes and constants in the arrays allocated above. On
   each loop, we make progressively more of the variable names in "var"
   visible to the compilation function. This ensures that each expression
   can only use variables which have been defined earlier. */
      if ( astOK ) {
         for ( ifun = 0; ifun < nfwd; ifun++ ) {
            CompileExpression( method, class, exprs[ ifun ],
                               nin + ifun, (const char **) var,
                               &( *fwdcode )[ ifun ], &( *fwdcon )[ ifun ],
                               &stacksize, status );

/* If an error occurs, then report contextual information and quit. */
            if ( !astOK ) {
               astError( astStatus,
                         "Error in forward transformation function %d.", status,
                         ifun + 1 );
               break;
            }

/* If OK, calculate the maximum evaluation stack size required by any
   of the expressions. */
            *fwdstack = ( *fwdstack > stacksize ) ? *fwdstack : stacksize;
         }
      }
   }

/* Free the memory containing the extracted expressions and variables. */
   FREE_POINTER_ARRAY( exprs, nfwd )
   FREE_POINTER_ARRAY( var, nvar )

/* Compile the inverse transformation. */
/* ----------------------------------- */
/* Allocate space for an array of pointers to the functions from which
   we will extract variable names. */
   strings = astMalloc( sizeof( char * ) * (size_t) ( nout + ninv ) );

/* Fill the first elements of this array with pointers to the forward
   transformation functions ("nout" in number) which yield the final
   output values. These will have the names of the output variables on
   their left hand sides. */
   if ( astOK ) {
      nvar = 0;
      for ( ifun = nfwd - nout; ifun < nfwd; ifun++ ) {
         strings[ nvar++ ] = fwdfun[ ifun ];
      }

/* Fill the remaining elements of the array with pointers to the
   inverse transformation functions. These will have the names of any
   intermediate variables plus the final input variables on their left
   hand sides. */
      for ( ifun = 0; ifun < ninv; ifun++ ) strings[ nvar++ ] = invfun[ ifun ];

/* Extract the variable names from the left hand sides of these
   functions and check them for validity and absence of duplication. */
      ExtractVariables( method, class, nvar, strings, nin, nout, nfwd, ninv, 0,
                        &var, status );
   }

/* Free the temporary array of string pointers. */
   strings = astFree( strings );

/* Extract the expressions from the right hand sides of the inverse
   transformation functions. */
   ExtractExpressions( method, class, ninv, invfun, 0, &exprs, status );

/* If OK, and the forward transformation is defined, then allocate and
   initialise space for an array of pointers to the opcodes for each
   expression and, similarly, for the constants for each expression. */
   if ( astOK && exprs ) {
      MALLOC_POINTER_ARRAY( *invcode, int *, ninv )
      MALLOC_POINTER_ARRAY( *invcon, double *, ninv )

/* If OK, loop to compile each of the expressions, storing pointers to
   the resulting opcodes and constants in the arrays allocated above. On
   each loop, we make progressively more of the variable names in "var"
   visible to the compilation function. This ensures that each expression
   can only use variables which have been defined earlier. */
      if ( astOK ) {
         for ( ifun = 0; ifun < ninv; ifun++ ) {
            CompileExpression( method, class, exprs[ ifun ],
                               nout + ifun, (const char **) var,
                               &( *invcode )[ ifun ], &( *invcon )[ ifun ],
                               &stacksize, status );

/* If an error occurs, then report contextual information and quit. */
            if ( !astOK ) {
               astError( astStatus,
                         "Error in inverse transformation function %d.", status,
                         ifun + 1 );
               break;
            }

/* If OK, calculate the maximum evaluation stack size required by any
   of the expressions. */
            *invstack = ( *invstack > stacksize ) ? *invstack : stacksize;
         }
      }
   }

/* Free the memory containing the extracted expressions and variables. */
   FREE_POINTER_ARRAY( exprs, ninv )
   FREE_POINTER_ARRAY( var, nvar )

/* If an error occurred, then free all remaining allocated memory and
   reset the output values. */
   if ( !astOK ) {
      FREE_POINTER_ARRAY( *fwdcode, nfwd )
      FREE_POINTER_ARRAY( *invcode, ninv )
      FREE_POINTER_ARRAY( *fwdcon, nfwd )
      FREE_POINTER_ARRAY( *invcon, ninv )
      *fwdstack = 0;
      *invstack = 0;
   }
}

static int DefaultSeed( const Rcontext *context, int *status ) {
/*
*  Name:
*     DefaultSeed

*  Purpose:
*     Generate an unpredictable seed for a random number generator.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mathmap.h"
*     int DefaultSeed( Rcontext *context, int *status )

*  Class Membership:
*     MathMap member function.

*  Description:
*     On each invocation this function returns an integer value which is
*     highly unpredictable. This value may be used as a default seed for the
*     random number generator associated with a MathMap, so that it
*     generates a different sequence on each occasion.

*  Parameters:
*     context
*        Pointer to the random number generator context associated with
*        the MathMap.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The unpredictable integer.

*  Notes:
*     - This function does not perform error checking and will execute even
*     if the global error status is set.
*/

/* Local Constants: */
   const int nwarm = 5;          /* Number of warm-up iterations */
   const long int a = 8121L;     /* Constants for random number generator... */
   const long int c = 28411L;
   const long int m = 134456L;

/* Local Variables; */
   int iwarm;                    /* Loop counter for warm-up iterations */
   static long init = 0;         /* Local initialisation performed? */
   static long int rand;         /* Local random integer */
   unsigned long int bits;       /* Bit pattern for producing result */

/* On the first invocation, initialise a local random number generator
   to a value derived by combining bit patterns obtained from the system
   clock and the processor time used. The result needs to be positive and
   lie in the range 0 to "m-1". */
   LOCK_MUTEX5
   if ( !init ) {
      rand = (long int) ( ( (unsigned long int) time( NULL ) ^
                            (unsigned long int) clock() ) %
                          (unsigned long int) m );

/* These values will typically only change in their least significant
   bits between programs run successively, but by using the bit pattern
   as a seed, we ensure that these differences are rapidly propagated to
   other bits. To hasten this process, we "warm up" the local generator
   with a few iterations. This is a quick and dirty generator using
   constants from Press et al. (Numerical recipes). */
      for ( iwarm = 0; iwarm < nwarm; iwarm++ ) {
         rand = ( rand * a + c ) % m;
      }

/* Note that this initialisation has been performed. */
      init = 1;
   }
   UNLOCK_MUTEX5

/* Generate a new bit pattern from the system time. Apart from the
   first invocation, this will be a different time to that used above. */
   bits = (unsigned long int) time( NULL );

/* Mask in a pattern derived from the CPU time used. */
   bits ^= (unsigned long int) clock();

/* The system time may change quite slowly (e.g. every second), so
   also mask in the address of the random number generator context
   supplied. This makes the seed depend on which MathMap is in use. */
   bits ^= (unsigned long int) context;

/* Now mask in the last random integer produced by the random number
   generator whose context has been supplied. This makes the seed depend
   on the MathMap's past use of random numbers. */
   bits ^= (unsigned long int) context->random_int;

/* Finally, in order to produce different seeds when this function is
   invoked twice in rapid succession on the same object (with no
   intermediate processing), we also mask in a pseudo-random value
   generated here. Generate the next local random integer. */
   rand = ( rand * a + c ) % m;

/* We then scale this value to give an integer in the range 0 to
   ULONG_MAX and mask the corresponding bit pattern into our seed. */
   bits ^= (unsigned long int) ( ( (double) rand / (double) ( m - 1UL ) ) *
                                 ( ( (double) ULONG_MAX + 1.0 ) *
                                   ( 1.0 - DBL_EPSILON ) ) );

/* Return the integer value of the seed (which may involve discarding
   some unwanted bits). */
   return (int) bits;
}

static int Equal( AstObject *this_object, AstObject *that_object, int *status ) {
/*
*  Name:
*     Equal

*  Purpose:
*     Test if two MathMaps are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     int Equal( AstObject *this, AstObject *that, int *status )

*  Class Membership:
*     MathMap member function (over-rides the astEqual protected
*     method inherited from the Object class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two MathMaps are equivalent.

*  Parameters:
*     this
*        Pointer to the first Object (a MathMap).
*     that
*        Pointer to the second Object.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the MathMaps are equivalent, zero otherwise.

*  Notes:
*     - The two MathMaps are considered equivalent if the combination of
*     the first in series with the inverse of the second simplifies to a
*     UnitMap.
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstMathMap *that;          /* Pointer to the second MathMap structure */
   AstMathMap *this;          /* Pointer to the first MathMap structure */
   double **that_con;         /* Lists of constants from "that" */
   double **this_con;         /* Lists of constants from "this" */
   int **that_code;           /* Lists of opcodes from "that" */
   int **this_code;           /* Lists of opcodes from "this" */
   int code;                  /* Opcode value */
   int icode;                 /* Opcode index */
   int icon;                  /* Constant index */
   int ifun;                  /* Function index */
   int ncode;                 /* No. of opcodes for current "this" function */
   int ncode_that;            /* No. of opcodes for current "that" function */
   int nin;                   /* Number of inputs */
   int nout;                  /* Number of outputs */
   int pass;                  /* Check fwd or inv */
   int result;                /* Result value to return */
   int that_nfun;             /* Number of functions from "that" */
   int this_nfun;             /* Number of functions from "this" */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain pointers to the two MathMap structures. */
   this = (AstMathMap *) this_object;
   that = (AstMathMap *) that_object;

/* Check the second object is a MathMap. We know the first is a
   MathMap since we have arrived at this implementation of the virtual
   function. */
   if( astIsAMathMap( that ) ) {

/* Check they have the same number of inputs and outputs */
      nin = astGetNin( this );
      nout = astGetNout( this );
      if( astGetNout( that ) == nout && astGetNin( that ) == nin ) {

/* Assume equality. */
         result = 1;

/* The first pass through this next loop compares forward functions, and
   the second pass compares inverse functions. */
         for( pass = 0; pass < 2 && result; pass++ ) {

/* On the first pass, get pointers to the lists of opcodes and constants for
   the effective forward transformations (taking into account the value
   of the Invert attribute), together with the number of such functions. */
            if( pass == 0 ) {
               if( !astGetInvert( this ) ) {
                  this_code = this->fwdcode;
                  this_con = this->fwdcon;
                  this_nfun = this->nfwd;
               } else {
                  this_code = this->invcode;
                  this_con = this->invcon;
                  this_nfun = this->ninv;
               }

               if( !astGetInvert( that ) ) {
                  that_code = that->fwdcode;
                  that_con = that->fwdcon;
                  that_nfun = that->nfwd;
               } else {
                  that_code = that->invcode;
                  that_con = that->invcon;
                  that_nfun = that->ninv;
               }

/* On the second pass, get pointers to the lists of opcodes and constants for
   the effective inverse transformations, together with the number of such
   functions. */
            } else {

               if( astGetInvert( this ) ) {
                  this_code = this->fwdcode;
                  this_con = this->fwdcon;
                  this_nfun = this->nfwd;
               } else {
                  this_code = this->invcode;
                  this_con = this->invcon;
                  this_nfun = this->ninv;
               }

               if( astGetInvert( that ) ) {
                  that_code = that->fwdcode;
                  that_con = that->fwdcon;
                  that_nfun = that->nfwd;
               } else {
                  that_code = that->invcode;
                  that_con = that->invcon;
                  that_nfun = that->ninv;
               }
            }

/* Check that "this" and "that" have the same number of functions */
            if( that_nfun != this_nfun ) result = 0;

/* Loop round each function. */
            for( ifun = 0; ifun < this_nfun && result; ifun++ ) {

/* The first element in the opcode array is the number of subsequent
   opcodes. Obtain and compare these counts. */
               ncode = this_code ? this_code[ ifun ][ 0 ] : 0;
               ncode_that = that_code ? that_code[ ifun ][ 0 ] : 0;
               if( ncode != ncode_that ) result = 0;

/* Compare the following opcodes. Some opcodes consume constants from the
   list of constants associated with the MathMap. Compare the constants
   for such opcodes. */
               icon = 0;
               for( icode = 0; icode < ncode && result; icode++ ){
                  code = this_code[ ifun ][ icode ];
                  if( that_code[ ifun ][ icode ] != code ) {
                     result = 0;

                  } else if( code == OP_LDCON ||
                             code == OP_LDVAR ||
                             code == OP_MAX ||
                             code == OP_MIN ) {

                     if( this_con[ ifun ][ icon ] !=
                         that_con[ ifun ][ icon ] ) {
                        result = 0;
                     } else {
                        icon++;
                     }
                  }
               }
            }
         }
      }
   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static void EvaluateFunction( Rcontext *rcontext, int npoint,
                              const double **ptr_in, const int *code,
                              const double *con, int stacksize, double *out, int *status ) {
/*
*  Name:
*     EvaluateFunction

*  Purpose:
*     Evaluate a compiled function.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mathmap.h"
*     void EvaluateFunction( Rcontext *rcontext, int npoint,
*                            const double **ptr_in, const int *code,
*                            const double *con, int stacksize, double *out, int *status )

*  Class Membership:
*     MathMap member function.

*  Description:
*     This function implements a "virtual machine" which executes operations
*     on an arithmetic stack in order to evaluate transformation functions.
*     Each operation is specified by an input operation code (opcode) and
*     results in the execution of a vector operation on a stack. The final
*     result, after executing all the supplied opcodes, is returned as a
*     vector.
*
*     This function detects arithmetic errors (such as overflow and division
*     by zero) and propagates any "bad" coordinate values, including those
*     present in the input, to the output.

*  Parameters:
*     npoint
*        The number of points to be transformd (i.e. the size of the vector
*        of values on which operations are to be performed).
*     ptr_in
*        Pointer to an array of pointers to arrays of double (with "npoint"
*        elements). These arrays should contain the input coordinate values,
*        such that coordinate number "coord" for point number "point" can be
*        found in "ptr_in[coord][point]".
*     code
*        Pointer to an array of int containing the set of opcodes (cast to int)
*        for the operations to be performed. The first element of this array
*        should contain a count of the number of opcodes which follow.
*     con
*        Pointer to an array of double containing the set of constants required
*        to evaluate the function (this may be NULL if no constants are
*        required).
*     stacksize
*        The size of the stack required to evaluate the expression using the
*        opcodes and constants supplied. This value should be calculated during
*        expression compilation.
*     out
*        Pointer to an array of double (with "npoint" elements) in which to
*        return the vector of result values.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Constants: */
   const int bits =              /* Number of bits in an unsigned long */
      sizeof( unsigned long ) * CHAR_BIT;
   const double eps =            /* Smallest number subtractable from 2.0 */
      2.0 * DBL_EPSILON;
   const double scale =          /* 2.0 raised to the power "bits" */
      ldexp( 1.0, bits );
   const double scale1 =         /* 2.0 raised to the power "bits-1" */
      scale * 0.5;
   const double rscale =         /* Reciprocal scale factor */
      1.0 / scale;
   const double rscale1 =        /* Reciprocal initial scale factor */
      1.0 / scale1;
   const int nblock =            /* Number of blocks of bits to process */
      ( sizeof( double ) + sizeof( unsigned long ) - 1 ) /
      sizeof( unsigned long );
   const unsigned long signbit = /* Mask for extracting sign bit */
      1UL << ( bits - 1 );

/* Local Variables: */
   double **stack;               /* Array of pointers to stack elements */
   double *work;                 /* Pointer to stack workspace */
   double *xv1;                  /* Pointer to first argument vector */
   double *xv2;                  /* Pointer to second argument vector */
   double *xv3;                  /* Pointer to third argument vector */
   double *xv;                   /* Pointer to sole argument vector */
   double *y;                    /* Pointer to result */
   double *yv;                   /* Pointer to result vector */
   double abs1;                  /* Absolute value (temporary variable) */
   double abs2;                  /* Absolute value (temporary variable) */
   double frac1;                 /* First (maybe normalised) fraction */
   double frac2;                 /* Second (maybe normalised) fraction */
   double frac;                  /* Sole normalised fraction */
   double newexp;                /* New power of 2 exponent value */
   double ran;                   /* Random number */
   double result;                /* Function result value */
   double unscale;               /* Factor for removing scaling */
   double value;                 /* Value to be assigned to stack vector */
   double x1;                    /* First argument value */
   double x2;                    /* Second argument value */
   double x3;                    /* Third argument value */
   double x;                     /* Sole argument value */
   int expon1;                   /* First power of 2 exponent */
   int expon2;                   /* Second power of 2 exponent */
   int expon;                    /* Sole power of 2 exponent */
   int iarg;                     /* Loop counter for arguments */
   int iblock;                   /* Loop counter for blocks of bits */
   int icode;                    /* Opcode value */
   int icon;                     /* Counter for number of constants used */
   int istk;                     /* Loop counter for stack elements */
   int ivar;                     /* Input variable number */
   int narg;                     /* Number of function arguments */
   int ncode;                    /* Number of opcodes to process */
   int point;                    /* Loop counter for stack vector elements */
   int sign;                     /* Argument is non-negative? */
   int tos;                      /* Top of stack index */
   static double d2r;            /* Degrees to radians conversion factor */
   static double log2;           /* Natural logarithm of 2.0 */
   static double pi;             /* Value of PI */
   static double r2d;            /* Radians to degrees conversion factor */
   static double rsafe_sq;       /* Reciprocal of "safe_sq" */
   static double safe_sq;        /* Huge value that can safely be squared */
   static int init = 0;          /* Initialisation performed? */
   unsigned long b1;             /* Block of bits from first argument */
   unsigned long b2;             /* Block of bits from second argument */
   unsigned long b;              /* Block of bits for result */
   unsigned long neg;            /* Result is negative? (sign bit) */

/* Check the global error status. */
   if ( !astOK ) return;

/* If this is the first invocation of this function, then initialise
   constant values. */
   LOCK_MUTEX2
   if ( !init ) {

/* Trigonometrical conversion factors. */
      pi = acos( -1.0 );
      r2d = 180.0 / pi;
      d2r = pi / 180.0;

/* Natural logarithm of 2.0. */
      log2 = log( 2.0 );

/* This value must be safe to square without producing overflow, yet
   large enough that adding or subtracting 1.0 from the square makes no
   difference. We also need its reciprocal. */
      safe_sq = 0.9 * sqrt( DBL_MAX );
      rsafe_sq = 1.0 / safe_sq;

/* Note that initialisation has been performed. */
      init = 1;
   }
   UNLOCK_MUTEX2

/* Allocate space for an array of pointers to elements of the
   workspace stack (each stack element being an array of double). */
   stack = astMalloc( sizeof( double * ) * (size_t) stacksize );

/* Allocate space for the stack itself. */
   work = astMalloc( sizeof( double ) *
                     (size_t) ( npoint * ( stacksize - 1 ) ) );

/* If OK, then initialise the stack pointer array to identify the
   start of each vector on the stack. The first element points at the
   output array (in which the result will be accumulated), while other
   elements point at successive vectors within the workspace allocated
   above. */
   if ( astOK ) {
      stack[ 0 ] = out;
      for ( istk = 1; istk < stacksize; istk++ ) {
         stack[ istk ] = work + ( istk - 1 ) * npoint;
      }

/* Define stack operations. */
/* ======================== */
/* We now define a set of macros for performing vector operations on
   elements of the stack. Each is in the form of a "case" block for
   execution in response to the appropriate operation code (opcode). */

/* Zero-argument operation. */
/* ------------------------ */
/* This macro performs a zero-argument operation, which results in the
   insertion of a new vector on to the stack. */
#define ARG_0(oper,setup,function) \
\
/* Test for the required opcode value. */ \
   case oper: \
\
/* Perform any required initialisation. */ \
      {setup;} \
\
/* Increment the top of stack index and obtain a pointer to the new stack \
   element (vector). */ \
      yv = stack[ ++tos ]; \
\
/* Loop to access each vector element, obtaining a pointer to it. */ \
      for ( point = 0; point < npoint; point++ ) { \
         y = yv + point; \
\
/* Perform the processing, which results in assignment to this element. */ \
         {function;} \
      } \
\
/* Break out of the "case" block. */ \
      break;

/* One-argument operation. */
/* ----------------------- */
/* This macro performs a one-argument operation, which processes the
   top stack element without changing the stack size. */
#define ARG_1(oper,function) \
\
/* Test for the required opcode value. */ \
   case oper: \
\
/* Obtain a pointer to the top stack element (vector). */ \
      xv = stack[ tos ]; \
\
/* Loop to access each vector element, obtaining its value and \
   checking that it is not bad. */ \
      for ( point = 0; point < npoint; point++ ) { \
         if ( ( x = xv[ point ] ) != AST__BAD ) { \
\
/* Also obtain a pointer to the element. */ \
            y = xv + point; \
\
/* Perform the processing, which uses the element's value and then \
   assigns the result to this element. */ \
            {function;} \
         } \
      } \
\
/* Break out of the "case" block. */ \
      break;

/* One-argument boolean operation. */
/* ------------------------------- */
/* This macro is similar in function to ARG_1 above, except that no
   checks are made for bad argument values. It is intended for use with
   boolean functions where bad values are handled explicitly. */
#define ARG_1B(oper,function) \
\
/* Test for the required opcode value. */ \
   case oper: \
\
/* Obtain a pointer to the top stack element (vector). */ \
      xv = stack[ tos ]; \
\
/* Loop to access each vector element, obtaining the argument value \
   and a pointer to the element. */ \
      for ( point = 0; point < npoint; point++ ) { \
         x = xv[ point ]; \
         y = xv + point; \
\
/* Perform the processing, which uses the element's value and then \
   assigns the result to this element. */ \
         {function;} \
      } \
\
/* Break out of the "case" block. */ \
      break;

/* Two-argument operation. */
/* ----------------------- */
/* This macro performs a two-argument operation, which processes the
   top two stack elements and produces a single result, resulting in the
   stack size decreasing by one. In this case, we first define a macro
   without the "case" block statements present. */
#define DO_ARG_2(function) \
\
/* Obtain pointers to the top two stack elements (vectors), decreasing \
   the top of stack index by one. */ \
      xv2 = stack[ tos-- ]; \
      xv1 = stack[ tos ]; \
\
/* Loop to access each vector element, obtaining the value of the \
   first argument and checking that it is not bad. */ \
      for ( point = 0; point < npoint; point++ ) { \
         if ( ( x1 = xv1[ point ] ) != AST__BAD ) { \
\
/* Also obtain a pointer to the element which is to receive the \
   result. */ \
            y = xv1 + point; \
\
/* Obtain the value of the second argument, again checking that it is \
   not bad. */ \
            if ( ( x2 = xv2[ point ] ) != AST__BAD ) { \
\
/* Perform the processing, which uses the two argument values and then \
   assigns the result to the appropriate top of stack element. */ \
               {function;} \
\
/* If the second argument was bad, so is the result. */ \
            } else { \
               *y = AST__BAD; \
            } \
         } \
      }

/* This macro simply wraps the one above up in a "case" block. */
#define ARG_2(oper,function) \
   case oper: \
      DO_ARG_2(function) \
      break;

/* Two-argument boolean operation. */
/* ------------------------------- */
/* This macro is similar in function to ARG_2 above, except that no
   checks are made for bad argument values. It is intended for use with
   boolean functions where bad values are handled explicitly. */
#define ARG_2B(oper,function) \
\
/* Test for the required opcode value. */ \
   case oper: \
\
/* Obtain pointers to the top two stack elements (vectors), decreasing \
   the top of stack index by one. */ \
      xv2 = stack[ tos-- ]; \
      xv1 = stack[ tos ]; \
\
/* Loop to access each vector element, obtaining the value of both \
   arguments and a pointer to the element which is to receive the \
   result. */ \
      for ( point = 0; point < npoint; point++ ) { \
         x1 = xv1[ point ]; \
         x2 = xv2[ point ]; \
         y = xv1 + point; \
\
/* Perform the processing, which uses the two argument values and then \
   assigns the result to the appropriate top of stack element. */ \
         {function;} \
      } \
\
/* Break out of the "case" block. */ \
      break;

/* Three-argument boolean operation. */
/* --------------------------------- */
/* This macro is similar in function to ARG_2B above, except that it
   takes three values of the stack and puts one back. It performs no
   checks for bad values. */
#define ARG_3B(oper,function) \
\
/* Test for the required opcode value. */ \
   case oper: \
\
/* Obtain pointers to the top three stack elements (vectors), decreasing \
   the top of stack index by two. */ \
      xv3 = stack[ tos-- ]; \
      xv2 = stack[ tos-- ]; \
      xv1 = stack[ tos ]; \
\
/* Loop to access each vector element, obtaining the value of all 3 \
   arguments and a pointer to the element which is to receive the \
   result. */ \
      for ( point = 0; point < npoint; point++ ) { \
         x1 = xv1[ point ]; \
         x2 = xv2[ point ]; \
         x3 = xv3[ point ]; \
         y = xv1 + point; \
\
/* Perform the processing, which uses the three argument values and then \
   assigns the result to the appropriate top of stack element. */ \
         {function;} \
      } \
\
/* Break out of the "case" block. */ \
      break;

/* Define arithmetic operations. */
/* ============================= */
/* We now define macros for performing some of the arithmetic
   operations we will require in a "safe" way - i.e. trapping numerical
   problems such as overflow and invalid arguments and translating them
   into the AST__BAD value. */

/* Absolute value. */
/* --------------- */
/* This is just shorthand. */
#define ABS(x) ( ( (x) >= 0.0 ) ? (x) : -(x) )

/* Integer part. */
/* ------------- */
/* This implements rounding towards zero without involving conversion
   to an integer (which could overflow). */
#define INT(x) ( ( (x) >= 0.0 ) ? floor( (x) ) : ceil( (x) ) )

/* Trap maths overflow. */
/* -------------------- */
/* This macro calls a C maths library function and checks for overflow
   in the result. */
#define CATCH_MATHS_OVERFLOW(function) \
   ( \
\
/* Clear the "errno" value. */ \
      errno = 0, \
\
/* Evaluate the function. */ \
      result = (function), \
\
/* Check if "errno" and the returned result indicate overflow and \
   return the appropriate result. */ \
      ( ( errno == ERANGE ) && ( ABS( result ) == HUGE_VAL ) ) ? AST__BAD : \
                                                                 result \
   )

/* Trap maths errors. */
/* ------------------ */
/* This macro is similar to the one above, except that it also checks
   for domain errors (i.e. invalid argument values). */
#define CATCH_MATHS_ERROR(function) \
   ( \
\
/* Clear the "errno" value. */ \
      errno = 0, \
\
/* Evaluate the function. */ \
      result = (function), \
\
/* Check if "errno" and the returned result indicate a domain error or \
   overflow and return the appropriate result. */ \
      ( ( errno == EDOM ) || \
        ( ( errno == ERANGE ) && ( ABS( result ) == HUGE_VAL ) ) ) ? \
                                 AST__BAD : result \
   )

/* Tri-state boolean OR. */
/* --------------------- */
/* This evaluates a boolean OR using tri-state logic. For example,
   "a||b" may evaluate to 1 if "a" is bad but "b" is non-zero, so that
   the normal rules of bad value propagation do not apply. */
#define TRISTATE_OR(x1,x2) \
\
/* Test if the first argument is bad. */ \
   ( (x1) == AST__BAD ) ? ( \
\
/* If so, test the second argument. */ \
      ( ( (x2) == 0.0 ) || ( (x2) == AST__BAD ) ) ? AST__BAD : 1.0 \
   ) : ( \
\
/* Test if the second argument is bad. */ \
      ( (x2) == AST__BAD ) ? ( \
\
/* If so, test the first argument. */ \
         ( (x1) == 0.0 ) ? AST__BAD : 1.0 \
\
/* If neither argument is bad, use the normal OR operator. */ \
      ) : ( \
         ( (x1) != 0.0 ) || ( (x2) != 0.0 ) \
      ) \
   )

/* Tri-state boolean AND. */
/* ---------------------- */
/* This evaluates a boolean AND using tri-state logic. */
#define TRISTATE_AND(x1,x2) \
\
/* Test if the first argument is bad. */ \
   ( (x1) == AST__BAD ) ? ( \
\
/* If so, test the second argument. */ \
      ( (x2) != 0.0 ) ? AST__BAD : 0.0 \
   ) : ( \
\
/* Test if the second argument is bad. */ \
      ( (x2) == AST__BAD ) ? ( \
\
/* If so, test the first argument. */ \
         ( (x1) != 0.0 ) ? AST__BAD : 0.0 \
\
/* If neither argument is bad, use the normal AND operator. */ \
      ) : ( \
         ( (x1) != 0.0 ) && ( (x2) != 0.0 ) \
      ) \
   )

/* Safe addition. */
/* -------------- */
/* This macro performs addition while avoiding possible overflow. */
#define SAFE_ADD(x1,x2) ( \
\
/* Test if the first argument is non-negative. */ \
   ( (x1) >= 0.0 ) ? ( \
\
/* If so, then we can perform addition if the second argument is \
   non-positive. Otherwise, we must calculate the most positive safe \
   second argument value that can be added and test for this (the test \
   itself is safe against overflow). */ \
      ( ( (x2) <= 0.0 ) || ( ( (DBL_MAX) - (x1) ) >= (x2) ) ) ? ( \
\
/* Perform addition if it is safe, otherwise return AST__BAD. */ \
         (x1) + (x2) \
      ) : ( \
         AST__BAD \
      ) \
\
/* If the first argument is negative, then we can perform addition if \
   the second argument is non-negative. Otherwise, we must calculate the \
   most negative second argument value that can be added and test for \
   this (the test itself is safe against overflow). */ \
   ) : ( \
      ( ( (x2) >= 0.0 ) || ( ( (DBL_MAX) + (x1) ) >= -(x2) ) ) ? ( \
\
/* Perform addition if it is safe, otherwise return AST__BAD. */ \
         (x1) + (x2) \
      ) : ( \
         AST__BAD \
      ) \
   ) \
)

/* Safe subtraction. */
/* ----------------- */
/* This macro performs subtraction while avoiding possible overflow. */
#define SAFE_SUB(x1,x2) ( \
\
/* Test if the first argument is non-negative. */ \
   ( (x1) >= 0.0 ) ? ( \
\
/* If so, then we can perform subtraction if the second argument is \
   also non-negative. Otherwise, we must calculate the most negative safe \
   second argument value that can be subtracted and test for this (the \
   test itself is safe against overflow). */ \
      ( ( (x2) >= 0.0 ) || ( ( (DBL_MAX) - (x1) ) >= -(x2) ) ) ? ( \
\
/* Perform subtraction if it is safe, otherwise return AST__BAD. */ \
         (x1) - (x2) \
      ) : ( \
         AST__BAD \
      ) \
\
/* If the first argument is negative, then we can perform subtraction \
   if the second argument is non-positive. Otherwise, we must calculate \
   the most positive second argument value that can be subtracted and \
   test for this (the test itself is safe against overflow). */ \
   ) : ( \
      ( ( (x2) <= 0.0 ) || ( ( (DBL_MAX) + (x1) ) >= (x2) ) ) ? ( \
\
/* Perform subtraction if it is safe, otherwise return AST__BAD. */ \
         (x1) - (x2) \
      ) : ( \
         AST__BAD \
      ) \
   ) \
)

/* Safe multiplication. */
/* -------------------- */
/* This macro performs multiplication while avoiding possible overflow. */
#define SAFE_MUL(x1,x2) ( \
\
/* Multiplication is safe if the absolute value of either argument is \
   unity or less. Otherwise, we must use the first argument to calculate \
   the maximum absolute value that the second argument may have and test \
   for this (the test itself is safe against overflow). */ \
   ( ( ( abs1 = ABS( (x1) ) ) <= 1.0 ) || \
     ( ( abs2 = ABS( (x2) ) ) <= 1.0 ) || \
     ( ( (DBL_MAX) / abs1 ) >= abs2 ) ) ? ( \
\
/* Perform multiplication if it is safe, otherwise return AST__BAD. */ \
      (x1) * (x2) \
   ) : ( \
      AST__BAD \
   ) \
)

/* Safe division. */
/* -------------- */
/* This macro performs division while avoiding possible overflow. */
#define SAFE_DIV(x1,x2) ( \
\
/* Division is unsafe if the second argument is zero. Otherwise, it is \
   safe if the abolute value of the second argument is unity or \
   more. Otherwise, we must use the second argument to calculate the \
   maximum absolute value that the first argument may have and test for \
   this (the test itself is safe against overflow). */ \
   ( ( (x2) != 0.0 ) && \
     ( ( ( abs2 = ABS( (x2) ) ) >= 1.0 ) || \
       ( ( (DBL_MAX) * abs2 ) >= ABS( (x1) ) ) ) ) ? ( \
\
/* Perform division if it is safe, otherwise return AST__BAD. */ \
      (x1) / (x2) \
   ) : ( \
      AST__BAD \
   ) \
)

/* Bit-shift operation. */
/* -------------------- */
/* This macro shifts the bits in a double value a specified number of
   places to the left, which simply corresponds to multiplying by the
   appropriate power of two. */
#define SHIFT_BITS(x1,x2) ( \
\
/* Decompose the value into a normalised fraction and a power of 2. */ \
   frac = frexp( (x1), &expon ), \
\
/* Calculate the new power of 2 which should apply after the shift, \
   rounding towards zero to give an integer value. */ \
   newexp = INT( (x2) ) + (double) expon, \
\
/* If the new exponent is too negative to convert to an integer, then \
   the result must underflow to zero. */ \
   ( newexp < (double) -INT_MAX ) ? ( \
      0.0 \
\
/* Otherwise, if it is too positive to convert to an integer, then the \
   result must overflow, unless the normalised fraction is zero. */ \
   ) : ( ( newexp > (double) INT_MAX ) ? ( \
      ( frac == 0.0 ) ? 0.0 : AST__BAD \
\
/* Otherwise, convert the new exponent to an integer and apply \
   it. Trap any overflow which may still occur. */ \
   ) : ( \
      CATCH_MATHS_OVERFLOW( ldexp( frac, (int) newexp ) ) \
   ) ) \
)

/* Two-argument bit-wise boolean operation. */
/* ---------------------------------------- */
/* This macro expands to code which performs a bit-wise boolean
   operation on a pair of arguments and assigns the result to the
   variable "result". It operates on floating point (double) values,
   which are regarded as if they are fixed-point binary numbers with
   negative values expressed in twos-complement notation. This means that
   it delivers the same results for integer values as the normal
   (integer) C bit-wise operations. However, it will also operate on the
   fraction bits of floating point numbers. It also offers greater
   precision (the first 53 or so significant bits of the result being
   preserved for typical IEEE floating point implementations). */
#define BIT_OPER(oper,x1,x2) \
\
/* Convert each argument to a normalised fraction in the range \
   [0.5,1.0) and a power of two exponent, removing any sign \
   information. */ \
   frac1 = frexp( ABS( (x1) ), &expon1 ); \
   frac2 = frexp( ABS( (x2) ), &expon2 ); \
\
/* Set "expon" to be the larger of the two exponents. If the two \
   exponents are not equal, divide the fraction with the smaller exponent \
   by 2 to the power of the exponent difference. This gives both \
   fractions the same effective exponent (although one of them may no \
   longer be normalised). Note that overflow is avoided because all \
   numbers remain less than 1.0, but underflow may occur. */ \
   expon = expon1; \
   if ( expon2 > expon1 ) { \
      expon = expon2; \
      frac1 = ldexp( frac1, expon1 - expon ); \
   } else if ( expon1 > expon2 ) { \
      frac2 = ldexp( frac2, expon2 - expon ); \
   } \
\
/* If either of the original arguments is negative, we now subtract \
   the corresponding fraction from 2.0. If we think of the fraction as \
   represented in fixed-point binary notation, this corresponds to \
   converting negative numbers into the twos-complement form normally used \
   for integers (the sign bit being the bit with value 1) instead \
   of having a separate sign bit as for floating point numbers. \
\
   Note that one of the fractions may have underflowed during the \
   scaling above. In that case (if the original argument was negative), \
   we must subtract the value "eps" (= 2.0 * DBL_EPSILON) from 2.0 \
   instead, so that we produce the largest number less than 2.0. In \
   twos-complement notation this represents the smallest possible \
   negative number and corresponds to extending the sign bit of the \
   original number up into more significant bits. This causes all bits to \
   be set as we require (rather than all being clear if the underflow \
   is simply ignored). */ \
   if ( (x1) < 0.0 ) frac1 = 2.0 - ( ( frac1 > eps ) ? frac1 : eps ); \
   if ( (x2) < 0.0 ) frac2 = 2.0 - ( ( frac2 > eps ) ? frac2 : eps ); \
\
/* We now extract the bits from the fraction values into integer \
   variables so that we may perform bit-wise operations on them. However, \
   since a double may be longer than any available integer, we may \
   have to handle several successive blocks of bits individually. */ \
\
/* Extract the first block of bits by scaling by the required power of \
   2 to shift the required bits to the left of the binary point. Then \
   extract the integer part. Note that this initial shift is one bit less \
   than the number of bits in an unsigned long, because we have \
   introduced an extra sign bit. */ \
   frac1 *= scale1; \
   frac2 *= scale1; \
   b1 = (unsigned long) frac1; \
   b2 = (unsigned long) frac2; \
\
/* Perform the required bit-wise operation on the extracted blocks of \
   bits. */ \
   b = b1 oper b2; \
\
/* Extract the sign bit from this initial result. This determines \
   whether the final result bit pattern should represent a negative \
   floating point number. */ \
   neg = b & signbit; \
\
/* Initialise the floating point result by setting it to the integer \
   result multipled by the reciprocal of the scale factor used to shift \
   the bits above. This returns the result bits to their correct \
   significance. */ \
   unscale = rscale1; \
   result = (double) b * unscale; \
\
/* We now loop to extract and process further blocks of bits (if \
   present). The number of blocks is determined by the relative lengths \
   of a double and an unsigned long. In practice, some bits of the double \
   will be used by its exponent, so the last block may be incomplete and \
   will simply be padded with zeros. */ \
   for ( iblock = 1; iblock < nblock; iblock++ ) { \
\
/* Subtract the integer part (which has already been processed) from \
   each fraction, to leave the bits which remain to be processed. Then \
   multiply by a scale factor to shift the next set of bits to the left \
   of the binary point. This time, we use as many bits as will fit into \
   an unsigned long. */ \
      frac1 = ( frac1 - (double) b1 ) * scale; \
      frac2 = ( frac2 - (double) b2 ) * scale; \
\
/* Extract the integer part, which contains the required bits. */ \
      b1 = (unsigned long) frac1; \
      b2 = (unsigned long) frac2; \
\
/* Perform the required bit-wise operation on the extracted blocks of \
   bits. */ \
      b = b1 oper b2; \
\
/* Update the result floating point value by adding the new integer \
   result multiplied by a scale factor to return the bits to their \
   original significance. */ \
      unscale *= rscale; \
      result += (double) b * unscale; \
   } \
\
/* If the (normalised fraction) result represents a negative number, \
   then subtract 2.0 from it (equivalent to subtracting it from 2 and \
   negating the result). This converts back to using a separate sign bit \
   instead of twos-complement notation. */ \
   if ( neg ) result -= 2.0; \
\
/* Scale by the required power of 2 to remove the initial \
   normalisation applied and assign the result to the "result" \
   variable. */ \
   result = ldexp( result, expon )

/* Gaussian random number. */
/* ----------------------- */
/* This macro expands to code which assigns a pseudo-random value to
   the "result" variable. The value is drawn from a Gaussian distribution
   with mean "x1" and standard deviation "ABS(x2)". */
#define GAUSS(x1,x2) \
\
/* Loop until a satisfactory result is obtained. */ \
   do { \
\
/* Obtain a value drawn from a standard Gaussian distribution. */ \
      ran = Gauss( rcontext, status ); \
\
/* Multiply by "ABS(x2)", trapping possible overflow. */ \
      result = ABS( (x2) ); \
      result = SAFE_MUL( ran, result ); \
\
/* If OK, add "x1", again trapping possible overflow. */ \
      if ( result != AST__BAD ) result = SAFE_ADD( result, (x1) ); \
\
/* Continue generating values until one is found which does not cause \
   overflow. */ \
   } while ( result == AST__BAD );

/* Implement the stack-based arithmetic. */
/* ===================================== */
/* Initialise the top of stack index and constant counter. */
      tos = -1;
      icon = 0;

/* Determine the number of opcodes to be processed and loop to process
   them, executing the appropriate "case" block for each one. */
      ncode = code[ 0 ];
      for ( icode = 1; icode <= ncode; icode++ ) {
         switch ( (Oper) code[ icode ] ) {

/* Ignore any null opcodes (which shouldn't occur). */
            case OP_NULL: break;

/* Otherwise, perform the required vector operation on the stack... */

/* User-supplied constants and variables. */
/* -------------------------------------- */
/* Loading a constant involves incrementing the constant count and
   assigning the next constant's value to the top of stack element. */
            ARG_0( OP_LDCON,    value = con[ icon++ ], *y = value )

/* Loading a variable involves obtaining the variable's index by
   consuming a constant (as above), and then copying the variable's
   values into the top of stack element. */
            ARG_0( OP_LDVAR,    ivar = (int) ( con[ icon++ ] + 0.5 ),
                                *y = ptr_in[ ivar ][ point ] )

/* System constants. */
/* ----------------- */
/* Loading a "bad" value simply means assigning AST__BAD to the top of
   stack element. */
            ARG_0( OP_LDBAD,    ;, *y = AST__BAD )

/* The following load constants associated with the (double) floating
   point representation into the top of stack element. */
            ARG_0( OP_LDDIG,    ;, *y = (double) DBL_DIG )
            ARG_0( OP_LDEPS,    ;, *y = DBL_EPSILON )
            ARG_0( OP_LDMAX,    ;, *y = DBL_MAX )
            ARG_0( OP_LDMAX10E, ;, *y = (double) DBL_MAX_10_EXP )
            ARG_0( OP_LDMAXE,   ;, *y = (double) DBL_MAX_EXP )
            ARG_0( OP_LDMDIG,   ;, *y = (double) DBL_MANT_DIG )
            ARG_0( OP_LDMIN,    ;, *y = DBL_MIN )
            ARG_0( OP_LDMIN10E, ;, *y = (double) DBL_MIN_10_EXP )
            ARG_0( OP_LDMINE,   ;, *y = (double) DBL_MIN_EXP )
            ARG_0( OP_LDRAD,    ;, *y = (double) FLT_RADIX )
            ARG_0( OP_LDRND,    ;, *y = (double) FLT_ROUNDS )

/* Mathematical constants. */
/* ----------------------- */
/* The following load mathematical constants into the top of stack
   element. */
            ARG_0( OP_LDE,      value = exp( 1.0 ), *y = value )
            ARG_0( OP_LDPI,     ;, *y = pi )

/* Functions with one argument. */
/* ---------------------------- */
/* The following simply evaluate a function of the top of stack
   element and assign the result to the same element. */
            ARG_1( OP_ABS,      *y = ABS( x ) )
            ARG_1( OP_ACOS,     *y = ( ABS( x ) <= 1.0 ) ?
                                     acos( x ) : AST__BAD )
            ARG_1( OP_ACOSD,    *y = ( ABS( x ) <= 1.0 ) ?
                                     acos( x ) * r2d : AST__BAD )
            ARG_1( OP_ACOSH,    *y = ( x < 1.0 ) ? AST__BAD :
                                     ( ( x > safe_sq ) ? log( x ) + log2 :
                                       log( x + sqrt( x * x - 1.0 ) ) ) )
            ARG_1( OP_ACOTH,    *y = ( ABS( x ) <= 1.0 ) ? AST__BAD :
                                     0.5 * ( log( ( x + 1.0 ) /
                                                  ( x - 1.0 ) ) ) )
            ARG_1( OP_ACSCH,    *y = ( ( x == 0.0 ) ? AST__BAD :
                                       ( sign = ( x >= 0.0 ), x = ABS( x ),
                                       ( sign ? 1.0 : -1.0 ) *
                                       ( ( x < rsafe_sq ) ? log2 - log( x ) :
                                         ( x = 1.0 / x,
                                       log( x + sqrt( x * x + 1.0 ) ) ) ) ) ) )
            ARG_1( OP_ASECH,    *y = ( ( x <= 0 ) || ( x > 1.0 ) ) ? AST__BAD :
                                       ( ( x < rsafe_sq ) ? log2 - log( x ) :
                                         ( x = 1.0 / x,
                                           log( x + sqrt( x * x - 1.0 ) ) ) ) )
            ARG_1( OP_ASIN,     *y = ( ABS( x ) <= 1.0 ) ?
                                     asin( x ) : AST__BAD )
            ARG_1( OP_ASIND,    *y = ( ABS( x ) <= 1.0 ) ?
                                     asin( x ) * r2d : AST__BAD )
            ARG_1( OP_ASINH,    *y = ( sign = ( x >= 0.0 ), x = ABS( x ),
                                       ( sign ? 1.0 : -1.0 ) *
                                       ( ( x > safe_sq ) ? log( x ) + log2 :
                                         log( x + sqrt( x * x + 1.0 ) ) ) ) )
            ARG_1( OP_ATAN,     *y = atan( x ) )
            ARG_1( OP_ATAND,    *y = atan( x ) * r2d )
            ARG_1( OP_ATANH,    *y = ( ABS( x ) >= 1.0 ) ? AST__BAD :
                                     0.5 * ( log( ( 1.0 + x ) /
                                                  ( 1.0 - x ) ) ) )
            ARG_1( OP_CEIL,     *y = ceil( x ) )
            ARG_1( OP_COS,      *y = cos( x ) )
            ARG_1( OP_COSD,     *y = cos( x * d2r ) )
            ARG_1( OP_COSH,     *y = CATCH_MATHS_OVERFLOW( cosh( x ) ) )
            ARG_1( OP_COTH,     *y = ( x = tanh( x ), SAFE_DIV( 1.0, x ) ) )
            ARG_1( OP_CSCH,     *y = ( x = CATCH_MATHS_OVERFLOW( sinh( x ) ),
                                       ( x == AST__BAD ) ?
                                       0.0 : SAFE_DIV( 1.0, x ) ) )
            ARG_1( OP_EXP,      *y = CATCH_MATHS_OVERFLOW( exp( x ) ) )
            ARG_1( OP_FLOOR,    *y = floor( x ) )
            ARG_1( OP_INT,      *y = INT( x ) )
            ARG_1B( OP_ISBAD,   *y = ( x == AST__BAD ) )
            ARG_1( OP_LOG,      *y = ( x > 0.0 ) ? log( x ) : AST__BAD )
            ARG_1( OP_LOG10,    *y = ( x > 0.0 ) ? log10( x ) : AST__BAD )
            ARG_1( OP_NINT,     *y = ( x >= 0 ) ?
                                     floor( x + 0.5 ) : ceil( x - 0.5 ) )
            ARG_1( OP_POISS,    *y = Poisson( rcontext, x, status ) )
            ARG_1( OP_SECH,     *y = ( x = CATCH_MATHS_OVERFLOW( cosh( x ) ),
                                       ( x == AST__BAD ) ? 0.0 : 1.0 / x ) )
            ARG_1( OP_SIN,      *y = sin( x ) )
            ARG_1( OP_SINC,     *y = ( x == 0.0 ) ? 1.0 : sin( x ) / x )
            ARG_1( OP_SIND,     *y = sin( x * d2r ) )
            ARG_1( OP_SINH,     *y = CATCH_MATHS_OVERFLOW( sinh( x ) ) )
            ARG_1( OP_SQR,      *y = SAFE_MUL( x, x ) )
            ARG_1( OP_SQRT,     *y = ( x >= 0.0 ) ? sqrt( x ) : AST__BAD )
            ARG_1( OP_TAN,      *y = CATCH_MATHS_OVERFLOW( tan( x ) ) )
            ARG_1( OP_TAND,     *y = tan( x * d2r ) )
            ARG_1( OP_TANH,     *y = tanh( x ) )

/* Functions with two arguments. */
/* ----------------------------- */
/* These evaluate a function of the top two entries on the stack. */
            ARG_2( OP_ATAN2,    *y = atan2( x1, x2 ) )
            ARG_2( OP_ATAN2D,   *y = atan2( x1, x2 ) * r2d )
            ARG_2( OP_DIM,      *y = ( x1 > x2 ) ? x1 - x2 : 0.0 )
            ARG_2( OP_GAUSS,    GAUSS( x1, x2 ); *y = result )
            ARG_2( OP_MOD,      *y = ( x2 != 0.0 ) ?
                                     fmod( x1, x2 ) : AST__BAD )
            ARG_2( OP_POW,      *y = CATCH_MATHS_ERROR( pow( x1, x2 ) ) )
            ARG_2( OP_RAND,     ran = Rand( rcontext, status );
                                *y = x1 * ran + x2 * ( 1.0 - ran ); )
            ARG_2( OP_SIGN,     *y = ( ( x1 >= 0.0 ) == ( x2 >= 0.0 ) ) ?
                                     x1 : -x1 )

/* Functions with three arguments. */
/* ------------------------------- */
/* These evaluate a function of the top three entries on the stack. */
            ARG_3B( OP_QIF,     *y = ( ( x1 ) ? ( x2 ) : ( x3 ) ) )


/* Functions with variable numbers of arguments. */
/* --------------------------------------------- */
/* These operations take a variable number of arguments, the actual
   number being determined by consuming a constant. We then loop to
   perform a 2-argument operation on the stack (as above) the required
   number of times. */
            case OP_MAX:
               narg = (int) ( con[ icon++ ] + 0.5 );
               for ( iarg = 0; iarg < ( narg - 1 ); iarg++ ) {
                  DO_ARG_2( *y = ( x1 >= x2 ) ? x1 : x2 )
               }
               break;
            case OP_MIN:
               narg = (int) ( con[ icon++ ] + 0.5 );
               for ( iarg = 0; iarg < ( narg - 1 ); iarg++ ) {
                  DO_ARG_2( *y = ( x1 <= x2 ) ? x1 : x2 )
               }
               break;

/* Unary arithmetic operators. */
/* --------------------------- */
            ARG_1( OP_NEG,      *y = -x )

/* Unary boolean operators. */
/* ------------------------ */
            ARG_1( OP_NOT,      *y = ( x == 0.0 ) )

/* Binary arithmetic operators. */
/* ---------------------------- */
            ARG_2( OP_ADD,      *y = SAFE_ADD( x1, x2 ) )
            ARG_2( OP_SUB,      *y = SAFE_SUB( x1, x2 ) )
            ARG_2( OP_MUL,      *y = SAFE_MUL( x1, x2 ) )
            ARG_2( OP_DIV ,     *y = SAFE_DIV( x1, x2 ) )

/* Bit-shift operators. */
/* -------------------- */
            ARG_2( OP_SHFTL,    *y = SHIFT_BITS( x1, x2 ) )
            ARG_2( OP_SHFTR,    *y = SHIFT_BITS( x1, -x2 ) )

/* Relational operators. */
/* --------------------- */
            ARG_2( OP_EQ,       *y = ( x1 == x2 ) )
            ARG_2( OP_GE,       *y = ( x1 >= x2 ) )
            ARG_2( OP_GT,       *y = ( x1 > x2 ) )
            ARG_2( OP_LE,       *y = ( x1 <= x2 ) )
            ARG_2( OP_LT,       *y = ( x1 < x2 ) )
            ARG_2( OP_NE,       *y = ( x1 != x2 ) )

/* Bit-wise operators. */
/* ------------------- */
            ARG_2( OP_BITOR,    BIT_OPER( |, x1, x2 ); *y = result )
            ARG_2( OP_BITXOR,   BIT_OPER( ^, x1, x2 ); *y = result )
            ARG_2( OP_BITAND,   BIT_OPER( &, x1, x2 ); *y = result )

/* Binary boolean operators. */
/* ------------------------- */
            ARG_2B( OP_AND,     *y = TRISTATE_AND( x1, x2 ) )
            ARG_2( OP_EQV,      *y = ( ( x1 != 0.0 ) == ( x2 != 0.0 ) ) )
            ARG_2B( OP_OR,      *y = TRISTATE_OR( x1, x2 ) )
            ARG_2( OP_XOR,      *y = ( ( x1 != 0.0 ) != ( x2 != 0.0 ) ) )
         }
      }
   }

/* When all opcodes have been processed, the result of the function
   evaluation will reside in the lowest stack entry - i.e. the output
   array. */

/* Free the workspace arrays. */
   work = astFree( work );
   stack = astFree( stack );

/* Undefine macros local to this function. */
#undef ARG_0
#undef ARG_1
#undef ARG_1B
#undef DO_ARG_2
#undef ARG_2
#undef ARG_2B
#undef ABS
#undef INT
#undef CATCH_MATHS_OVERFLOW
#undef CATCH_MATHS_ERROR
#undef TRISTATE_OR
#undef TRISTATE_AND
#undef SAFE_ADD
#undef SAFE_SUB
#undef SAFE_MUL
#undef SAFE_DIV
#undef SHIFT_BITS
#undef BIT_OPER
#undef GAUSS
}

static void EvaluationSort( const double con[], int nsym, int symlist[],
                            int **code, int *stacksize, int *status ) {
/*
*  Name:
*     EvaluationSort

*  Purpose:
*     Perform an evaluation-order sort on parsed expression symbols.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mathmap.h"
*     void EvaluationSort( const double con[], int nsym, int symlist[],
*                          int **code, int *stacksize, int *status )

*  Class Membership:
*     MathMap member function.

*  Description:
*     This function sorts a sequence of numbers representing symbols
*     identified in an expression. The symbols (i.e. the expression syntax)
*     must have been fully validated beforehand, as no validation is
*     performed here.
*
*     The symbols are sorted into the order in which corresponding
*     operations must be performed on a push-down arithmetic stack in order
*     to evaluate the expression. Operation codes (opcodes), as defined in
*     the "Oper" enum, are then substituted for the symbol numbers.

*  Parameters:
*     con
*        Pointer to an array of double containing the set of constants
*        generated while parsing the expression (these are required in order
*        to determine the number of arguments associated with functions which
*        take a variable number of arguments).
*     nsym
*        The number of symbols identified while parsing the expression.
*     symlist
*        Pointer to an array of int, with "nsym" elements. On entry, this
*        should contain the indices in the static "symbol" array of the
*        symbols identified while parsing the expression. On exit, the
*        contents are undefined.
*     code
*        Address of a pointer which will be set to point at a dynamically
*        allocated array of int containing the set of opcodes (cast to int)
*        produced by this function. The first element of this array will
*        contain a count of the number of opcodes which follow.
*
*        The allocated space must be freed by the caller (using astFree) when
*        no longer required.
*     stacksize
*        Pointer to an int in which to return the size of the push-down stack
*        required to evaluate the expression using the returned opcodes.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - A value of NULL will be returned for the "*code" pointer and a value
*     of zero will be returned for the "*stacksize" value if this function is
*     invoked with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   int flush;                    /* Flush parenthesised symbol sequence? */
   int icon;                     /* Input constant counter */
   int isym;                     /* Input symbol counter */
   int ncode;                    /* Number of opcodes generated */
   int nstack;                   /* Evaluation stack size */
   int push;                     /* Push a new symbol on to stack? */
   int sym;                      /* Variable for symbol number */
   int tos;                      /* Top of sort stack index */

/* Initialise */
   *code = NULL;
   *stacksize = 0;

/* Check the global error status. */
   if ( !astOK ) return;

/* Further initialisation. */
   flush = 0;
   icon = 0;
   isym = 0;
   ncode = 0;
   nstack = 0;
   tos = -1;

/* Loop to generate output opcodes until the sort stack is empty and
   there are no further symbols to process, or an error is detected.  */
   while ( astOK && ( ( tos > -1 ) || ( isym < nsym ) ) ) {

/* Decide whether to push a symbol on to the sort stack (which
   "diverts" it so that higher-priority symbols can be output), or to pop
   the top symbol off the sort stack and send it to the output
   stream... */

/* We must push a symbol on to the sort stack if the stack is
   currently empty. */
      if ( tos == -1 ) {
         push = 1;

/* We must pop the top symbol off the sort stack if there are no more
   input symbols to process. */
      } else if ( isym >= nsym ) {
         push = 0;

/* If the sort stack is being flushed to complete the evaluation of a
   parenthesised expression, then the top symbol (which will be the
   opening parenthesis or function call) must be popped. This is only
   done once, so reset the "flush" flag before the next loop. */
      } else if ( flush ) {
         push = 0;
         flush = 0;

/* In all other circumstances, we must push a symbol on to the sort
   stack if its evaluation priority (seen from the left) is higher than
   that of the current top of stack symbol (seen from the right). This
   means it will eventually be sent to the output stream ahead of the
   current top of stack symbol. */
      } else {
         push = ( symbol[ symlist[ isym ] ].leftpriority >
                  symbol[ symlist[ tos ] ].rightpriority );
      }

/* If a symbol is being pushed on to the sort stack, then get the next
   input symbol which is to be used. */
      if ( push ) {
         sym = symlist[ isym++ ];

/* If the symbol decreases the parenthesis level (a closing
   parenthesis), then all the sort stack entries down to the symbol which
   opened the current level of parenthesis (the matching opening
   parenthesis or function call) will already have been sent to the
   output stream as a consequence of the evaluation priority defined for
   a closing parenthesis in the symbol data. The opening parenthesis (or
   function call) must next be flushed from the sort stack, so set the
   "flush" flag which is interpreted on the next loop. Ignore the current
   symbol, which cancels with the opening parenthesis on the stack. */
         if ( symbol[ sym ].parincrement < 0 ) {
            flush = 1;

/* All other symbols are pushed on to the sort stack. The stack
   occupies that region of the "symlist" array from which the input
   symbol numbers have already been extracted. */
         } else {
            symlist[ ++tos ] = sym;
         }

/* If a symbol is being popped from the top of the sort stack, then
   the top of stack entry is transferred to the output stream. Obtain the
   symbol number from the stack. Increment the local constant counter if
   the associated operation will use a constant. */
      } else {
         sym = symlist[ tos-- ];
         icon += ( ( sym == symbol_ldvar ) || ( sym == symbol_ldcon ) );

/* If the output symbol does not represent a "null" operation,
   increase the size of the output opcode array to accommodate it,
   checking for errors. Note that we allocate one extra array element
   (the first) which will eventually hold a count of all the opcodes
   generated. */
         if ( symbol[ sym ].opcode != OP_NULL ) {
            *code = astGrow( *code, ncode + 2, sizeof( int ) );
            if ( astOK ) {

/* Append the new opcode to the end of this array. */
               ( *code )[ ++ncode ] = (int) symbol[ sym ].opcode;

/* Increment/decrement the counter representing the stack size
   required for evaluation of the expression.  If the symbol is a
   function with a variable number of arguments (indicated by a negative
   "nargs" entry in the symbol data table), then the change in stack size
   must be determined from the argument number stored in the constant
   table. */
               if ( symbol[ sym ].nargs >= 0 ) {
                  nstack += symbol[ sym ].stackincrement;
               } else {
                  nstack -= (int) ( con[ icon++ ] + 0.5 ) - 1;
               }

/* Note the maximum size of the stack. */
               *stacksize = ( nstack > *stacksize ) ? nstack : *stacksize;
            }
         }
      }
   }

/* If no "*code" array has been allocated, then allocate one simply to
   store the number of opcodes generated, i.e. zero (this shouldn't
   normally happen as this represents an invalid expression). */
   if ( !*code ) *code = astMalloc( sizeof( int ) );

/* If no error has occurred, store the count of opcodes generated in
   the first element of the "*code" array and re-allocate the array to
   its final size (since astGrow may have over-allocated space). */
   if ( astOK ) {
      ( *code )[ 0 ] = ncode;
      *code = astRealloc( *code, sizeof( int ) * (size_t) ( ncode + 1 ) );
   }

/* If an error occurred, free any memory that was allocated and reset
   the output values. */
   if ( !astOK ) {
      *code = astFree( *code );
      *stacksize = 0;
   }
}

static void ExtractExpressions( const char *method, const char *class,
                                int nfun, const char *fun[], int forward,
                                char ***exprs, int *status ) {
/*
*  Name:
*     ExtractExpressions

*  Purpose:
*     Extract and validate expressions.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mathmap.h"
*     void ExtractExpressions( const char *method, const char *class,
*                              int nfun, const char *fun[], int forward,
*                              char ***exprs, int *status )

*  Class Membership:
*     MathMap member function.

*  Description:
*     This function extracts expressions from the right hand sides of a set
*     of functions. These expressions are then validated to check that they
*     are either all present, or all absent (absence indicating an undefined
*     transformation). An error is reported if anything is found to be
*     wrong.
*
*     Note that the syntax of the expressions is not checked by this function
*     (i.e. they are not compiled).

*  Parameters:
*     method
*        Pointer to a constant null-terminated character string
*        containing the name of the method that invoked this function.
*        This method name is used solely for constructing error messages.
*     class
*        Pointer to a constant null-terminated character string containing the
*        class name of the Object being processed. This name is used solely
*        for constructing error messages.
*     nfun
*        The number of functions to be analysed.
*     fun
*        Pointer to an array, with "nfun" elements, of pointers to null
*        terminated strings which contain each of the functions. These
*        strings should contain no white space.
*     forward
*        A non-zero value indicates the the MathMap's forward transformation
*        functions are being processed, while a zero value indicates processing
*        of the inverse transformation functions. This value is used solely for
*        constructing error messages.
*     exprs
*        Address in which to return a pointer to an array (with "nfun"
*        elements) of pointers to null terminated strings containing the
*        extracted expressions (i.e. this returns an array of strings).
*
*        Both the returned array of pointers, and the strings to which they
*        point, will be stored in dynamically allocated memory and should
*        be freed by the caller (using astFree) when no longer required.
*
*        If the right hand sides (including the "=" sign) of all the supplied
*        functions are absent, then this indicates an undefined transformation
*        and the returned pointer value will be NULL. An error results if
*        an "=" sign is present but no expression follows it.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*        - A NULL value will be returned for "*exprs" if this function is
*        invoked with the global error status set, or if it should fail for
*        any reason.
*/

/* Local Variables: */
   char *ex;                     /* Pointer to start of expression string */
   int ifun;                     /* Loop counter for functions */
   int iud;                      /* Index of first undefined function */
   int nud;                      /* Number of undefined expressions */

/* Initialise. */
   *exprs = NULL;

/* Check the global error status. */
   if ( !astOK ) return;

/* Further initialisation. */
   nud = 0;
   iud = 0;

/* Allocate and initialise memory for the returned array of pointers. */
   MALLOC_POINTER_ARRAY( *exprs, char *, nfun )

/* Loop to inspect each function in turn. */
   if ( astOK ) {
      for ( ifun = 0; ifun < nfun; ifun++ ) {

/* Search for the first "=" sign. */
         if ( ( ex = strchr( fun[ ifun ], '=' ) ) ) {

/* If found, and there are more characters after the "=" sign, then
   find the length of the expression which follows. Allocate a string to
   hold this expression, storing its pointer in the array allocated
   above. Check for errors. */
            if ( *++ex ) {
               ( *exprs )[ ifun ] = astMalloc( strlen( ex ) + (size_t) 1 );
               if ( !astOK ) break;

/* If OK, extract the expression string. */
               (void) strcpy( ( *exprs )[ ifun ], ex );

/* If an "=" sign was found but there are no characters following it,
   then there is a missing right hand side to a function, so report an
   error and quit. */
            } else {
               astError( AST__NORHS,
                         "%s(%s): Missing right hand side in expression: "
                         "\"%s\".", status,
                         method, class, fun[ ifun ] );
               astError( astStatus,
                         "Error in %s transformation function %d.", status,
                         forward ? "forward" : "inverse", ifun + 1 );
               break;
            }

/* If no "=" sign was found, then the transformation may be undefined,
   in which case each function should only contain a variable name. Count
   the number of times this happens and record the index of the first
   instance. */
         } else {
            nud++;
            if ( nud == 1 ) iud = ifun;
         }
      }
   }

/* Either all functions should have an "=" sign (in which case the
   transformation is defined), or none of them should have (in which case
   it is undefined). If some do and some don't, then report an error,
   citing the first instance of a missing "=" sign. */
   if ( astOK && ( nud != 0 ) && ( nud != nfun ) ) {
      astError( AST__NORHS,
                "%s(%s): Missing right hand side in function: \"%s\".", status,
                method, class, fun[ iud ] );
      astError( astStatus,
                "Error in %s transformation function %d.", status,
                forward ? "forward" : "inverse", iud + 1 );
   }

/* If an error occurred, or all the expressions were absent, then free any
   allocated memory and reset the output value. */
   if ( !astOK || nud ) {
      FREE_POINTER_ARRAY( *exprs, nfun )
   }
}

static void ExtractVariables( const char *method, const char *class,
                              int nfun, const char *fun[],
                              int nin, int nout, int nfwd, int ninv,
                              int forward, char ***var, int *status ) {
/*
*  Name:
*     ExtractVariables

*  Purpose:
*     Extract and validate variable names.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mathmap.h"
*     void ExtractVariables( const char *method, const char *class,
*                            int nfun, const char *fun[],
*                            int nin, int nout, int nfwd, int ninv,
*                            int forward, char ***var, int *status )

*  Class Membership:
*     MathMap member function.

*  Description:
*     This function extracts variable names from the left hand sides of a
*     set of transformation functions belonging to a MathMap. These variable
*     names are then validated to check for correct syntax and no
*     duplication. An error is reported if anything is wrong with the
*     variable names obtained.

*  Parameters:
*     method
*        Pointer to a constant null-terminated character string
*        containing the name of the method that invoked this function.
*        This method name is used solely for constructing error messages.
*     class
*        Pointer to a constant null-terminated character string containing the
*        class name of the Object being processed. This name is used solely
*        for constructing error messages.
*     nfun
*        The number of functions to be analysed.
*     fun
*        Pointer to an array, with "nfun" elements, of pointers to null
*        terminated strings which contain each of the functions. These strings
*        are case sensitive and should contain no white space.
*
*        The first elements of this array should point to the functions that
*        define the primary input/output variables (depending on direction).
*        These should be followed by any functions which define intermediate
*        variables (taken from the set of functions which transform in the
*        opposite direction to the first ones).
*     nin
*        Number of input variables for the MathMap.
*     nout
*        Number of output variables for the MathMap.
*     nfwd
*        Number of forward transformation functions for the MathMap.
*     ninv
*        Number of inverse transformation functions for the MathMap.
*     forward
*        A non-zero value indicates the the MathMap's forward transformation
*        functions are being processed, while a zero value indicates processing
*        of the inverse transformation functions. This value, together with
*        "nin", "nout", "nfwd" and "ninv" are used solely for constructing
*        error messages.
*     var
*        Address in which to return a pointer to an array (with "nfun"
*        elements) of pointers to null terminated strings containing the
*        extracted variable names (i.e. this returns an array of strings).
*
*        Both the returned array of pointers, and the strings to which they
*        point, will be stored in dynamically allocated memory and should
*        be freed by the caller (using astFree) when no longer required.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*        - A NULL value will be returned for "*var" if this function is
*        invoked with the global error status set, or if it should fail for
*        any reason.
*/

/* Local Variables: */
   char *duser1;                 /* Transformation direction for function */
   char *duser2;                 /* Transformation direction for function */
   char c;                       /* Extracted character */
   int i1;                       /* Loop counter for detecting duplicates */
   int i2;                       /* Loop counter for detecting duplicates */
   int i;                        /* Loop counter for characters */
   int iend;                     /* Last character index in parsed name */
   int ifun;                     /* Loop counter for functions */
   int iuser1;                   /* Function number as known to the user */
   int iuser2;                   /* Function number as known to the user */
   int nc;                       /* Character count */
   int nextra;                   /* Number of intermediate functions */
   int nprimary;                 /* Number of primary input/output variables */

/* Initialise. */
   *var = NULL;

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain the number of primary input/output variables, depending on
   the direction of the coordinate transformation. */
   nprimary = ( forward ? nin : nout );

/* Deterine the number of extra (intermediate) functions that come
   before these primary ones. These affect the numbering of
   transformation functions as known to the user, and must be accounted
   for when reporting error messages. */
   nextra = ( forward ? ninv - nin : nfwd - nout );

/* Allocate and initialise memory for the returned array of pointers. */
   MALLOC_POINTER_ARRAY( *var, char *, nfun )

/* Loop to process each function in turn. */
   if ( astOK ) {
      for ( ifun = 0; ifun < nfun; ifun++ ) {

/* Count the number of characters appearing before the "=" sign (or in
   the entire string if the "=" is absent). */
         for ( nc = 0; ( c = fun[ ifun ][ nc ] ); nc++ ) if ( c == '=' ) break;

/* If no characters were counted, then report an appropriate error
   message, depending on whether the function string was entirely
   blank. */
         if ( !nc ) {
            if ( c ) {
               astError( AST__MISVN,
                         "%s(%s): No left hand side in expression: \"%s\".", status,
                         method, class, fun[ ifun ] );
            } else {
               astError( AST__MISVN,
                         "%s: Transformation function contains no variable "
                         "name.", status,
                         method );
            }
            break;
         }

/* If OK, allocate memory to hold the output string and check for
   errors. */
         ( *var )[ ifun ] = astMalloc( sizeof( char ) * (size_t) ( nc + 1 ) ) ;
         if ( !astOK ) break;

/* If OK, copy the characters before the "=" sign to the new
   string. */
         nc = 0;
         for ( i = 0; ( c = fun[ ifun ][ i ] ); i++ ) {
            if ( c == '=' ) break;
            ( *var )[ ifun ][ nc++] = c;
         }

/* Null terminate the result. */
         ( *var )[ ifun ][ nc ] = '\0';

/* Try to parse the contents of the extracted string as a name. */
         ParseName( ( *var )[ ifun ], 0, &iend, status );

/* If unsuccessful, or if all the characters were not parsed, then we
   have an invalid variable name, so report an error and quit. */
         if ( ( iend < 0 ) || ( *var )[ ifun ][ iend + 1 ] ) {
            astError( AST__VARIN,
                      "%s(%s): Variable name is invalid: \"%s\".", status,
                      method, class, ( *var )[ ifun ] );
            break;
         }
      }

/* If an error occurred above, then determine the function number, and
   the direction of the transformation of which it forms part, as known
   to the user. */
      if ( !astOK ) {
         if ( ifun < nprimary ) {
            iuser1 = ifun + 1 + nextra;
            duser1 = ( forward ? "inverse" : "forward" );
         } else {
            iuser1 = ifun + 1 - nprimary;
            duser1 = ( forward ? "forward" : "inverse" );
         }

/* Report a contextual error message. */
         astError( astStatus,
                   "Error in %s transformation function %d.", status,
                   duser1, iuser1 );
      }
   }

/* If there has been no error, loop to compare all the variable names
   with each other to detect duplication. */
   if ( astOK ) {
      for ( i1 = 1; i1 < nfun; i1++ ) {
         for ( i2 = 0; i2 < i1; i2++ ) {

/* If a duplicate variable name is found, report an error. */
            if ( !strcmp( ( *var )[ i1 ], ( *var )[ i2 ] ) ) {
               astError( AST__DUVAR,
                         "%s(%s): Duplicate definition of variable name: "
                         "\"%s\".", status,
                         method, class, ( *var )[ i1 ] );

/* For each transformation function involved, determine the function
   number and the direction of the transformation of which it forms part,
   as known to the user. */
               if ( i1 < nprimary ) {
                  iuser1 = i1 + 1 + nextra;
                  duser1 = ( forward ? "inverse" : "forward" );
               } else {
                  iuser1 = i1 + 1 - nprimary;
                  duser1 = ( forward ? "forward" : "inverse" );
               }
               if ( i2 < nprimary ) {
                  iuser2 = i2 + 1 + nextra;
                  duser2 = ( forward ? "inverse" : "forward" );
               } else {
                  iuser2 = i2 + 1 - nprimary;
                  duser2 = ( forward ? "forward" : "inverse" );
               }

/* Report a contextual error message. */
               astError( astStatus,
                         "Conflict between %s function %d and %s function %d.", status,
                         duser1, iuser1, duser2, iuser2 );
               break;
            }
         }
         if ( !astOK ) break;
      }
   }

/* If an error occurred, free any allocated memory and reset the
   output value. */
   if ( !astOK ) {
      FREE_POINTER_ARRAY( *var, nfun )
   }
}

static double Gauss( Rcontext *context, int *status ) {
/*
*  Name:
*     Gauss

*  Purpose:
*     Produce a pseudo-random sample from a standard Gaussian distribution.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mathmap.h"
*     double Gauss( Rcontext *context, int *status )

*  Class Membership:
*     MathMap member function.

*  Description:
*     On each invocation, this function returns a pseudo-random sample drawn
*     from a standard Gaussian distribution with mean zero and standard
*     deviation unity. The Box-Muller transformation method is used.

*  Parameters:
*     context
*        Pointer to an Rcontext structure which holds the random number
*        generator's context between invocations.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A sample from a standard Gaussian distribution.

*  Notes:
*     - The sequence of numbers returned is determined by the "seed"
*     value in the Rcontext structure supplied.
*     - If the seed value is changed, the "active" flag must also be cleared
*     so that this function can re-initiallise the Rcontext structure before
*     generating the next pseudo-random number. The "active" flag should
*     also be clear to force initialisation the first time an Rcontext
*     structure is used.
*     - This function does not perform error checking and does not generate
*     errors. It will execute even if the global error status is set.
*/

/* Local Variables: */
   double rsq;                   /* Squared radius */
   double s;                     /* Scale factor */
   double x;                     /* First result value */
   static double y;              /* Second result value */
   static int ysaved = 0;        /* Previously-saved value available? */

   LOCK_MUTEX7

/* If the random number generator context is not active, then it will
   be (re)initialised on the first invocation of Rand (below). Ensure
   that any previously-saved value within this function is first
   discarded. */
   if ( !context->active ) ysaved = 0;

/* If there is a previously-saved value available, then use it and
   mark it as no longer available. */
   if ( ysaved ) {
      x = y;
      ysaved = 0;

/* Otherwise, loop until a suitable new pair of values has been
   obtained. */
   } else {
      while ( 1 ) {

/* Loop to obtain two random values uniformly distributed inside the
   unit circle, while avoiding the origin (which maps to an infinite
   result). */
         do {
            x = 2.0 * Rand( context, status ) - 1.0;
            y = 2.0 * Rand( context, status ) - 1.0;
            rsq = x * x + y * y;
         } while ( ( rsq >= 1.0 ) || ( rsq == 0.0 ) );

/* Perform the Box-Muller transformation, checking that this will not
   produce overflow (which is extremely unlikely). If overflow would
   occur, we simply repeat the above steps with a new pair of random
   numbers. */
         s = -2.0 * log( rsq );
         if ( ( DBL_MAX * rsq ) >= s ) {
            s = sqrt( s / rsq );

/* Scale the original random values to give a pair of results. One will be
   returned and the second kept until next time. */
            x *= s;
            y *= s;
            break;
         }
      }

/* Note that a saved value is available. */
      ysaved = 1;
   }

   UNLOCK_MUTEX7

/* Return the current result. */
   return x;
}

static int GetObjSize( AstObject *this_object, int *status ) {
/*
*  Name:
*     GetObjSize

*  Purpose:
*     Return the in-memory size of an Object.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mathmap.h"
*     int GetObjSize( AstObject *this, int *status )

*  Class Membership:
*     MathMap member function (over-rides the astGetObjSize protected
*     method inherited from the parent class).

*  Description:
*     This function returns the in-memory size of the supplied MathMap,
*     in bytes.

*  Parameters:
*     this
*        Pointer to the MathMap.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The Object size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstMathMap *this;         /* Pointer to MathMap structure */
   int result;                /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointers to the MathMap structure. */
   this = (AstMathMap *) this_object;

/* Invoke the GetObjSize method inherited from the parent class, and then
   add on any components of the class structure defined by thsi class
   which are stored in dynamically allocated memory. */
   result = (*parent_getobjsize)( this_object, status );

   SIZEOF_POINTER_ARRAY( this->fwdfun, this->nfwd )
   SIZEOF_POINTER_ARRAY( this->invfun, this->ninv )
   SIZEOF_POINTER_ARRAY( this->fwdcode, this->nfwd )
   SIZEOF_POINTER_ARRAY( this->invcode, this->ninv )
   SIZEOF_POINTER_ARRAY( this->fwdcon, this->nfwd )
   SIZEOF_POINTER_ARRAY( this->invcon, this->ninv )

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static const char *GetAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     GetAttrib

*  Purpose:
*     Get the value of a specified attribute for a MathMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mathmap.h"
*     const char *GetAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     MathMap member function (over-rides the protected astGetAttrib
*     method inherited from the Mapping class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for a MathMap, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the MathMap.
*     attrib
*        Pointer to a null-terminated string containing the name of
*        the attribute whose value is required. This name should be in
*        lower case, with all white space removed.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     - Pointer to a null-terminated string containing the attribute
*     value.

*  Notes:
*     - The returned string pointer may point at memory allocated
*     within the MathMap, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the MathMap. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstMathMap *this;             /* Pointer to the MathMap structure */
   const char *result;           /* Pointer value to return */
   int ival;                     /* Integer attribute value */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(this_object);

/* Obtain a pointer to the MathMap structure. */
   this = (AstMathMap *) this_object;

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "getattrib_buff" as a null-terminated string in an appropriate
   format.  Set "result" to point at the result string. */

/* Seed. */
/* ----- */
   if ( !strcmp( attrib, "seed" ) ) {
      ival = astGetSeed( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", ival );
         result = getattrib_buff;
      }

/* SimpFI. */
/* ------- */
   } else if ( !strcmp( attrib, "simpfi" ) ) {
      ival = astGetSimpFI( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", ival );
         result = getattrib_buff;
      }

/* SimpIF. */
/* ------- */
   } else if ( !strcmp( attrib, "simpif" ) ) {
      ival = astGetSimpIF( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", ival );
         result = getattrib_buff;
      }

/* If the attribute name was not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_getattrib)( this_object, attrib, status );
   }

/* Return the result. */
   return result;

}

void astInitMathMapVtab_(  AstMathMapVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitMathMapVtab

*  Purpose:
*     Initialise a virtual function table for a MathMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "mathmap.h"
*     void astInitMathMapVtab( AstMathMapVtab *vtab, const char *name )

*  Class Membership:
*     MathMap vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the MathMap class.

*  Parameters:
*     vtab
*        Pointer to the virtual function table. The components used by
*        all ancestral classes will be initialised if they have not already
*        been initialised.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the virtual function table belongs (it
*        is this pointer value that will subsequently be returned by the Object
*        astClass function).
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstMappingVtab *mapping;      /* Pointer to Mapping component of Vtab */
   AstObjectVtab *object;        /* Pointer to Object component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitMappingVtab( (AstMappingVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsAMathMap) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstMappingVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that
   provide virtual methods for this class. */
   vtab->ClearSeed = ClearSeed;
   vtab->ClearSimpFI = ClearSimpFI;
   vtab->ClearSimpIF = ClearSimpIF;
   vtab->GetSeed = GetSeed;
   vtab->GetSimpFI = GetSimpFI;
   vtab->GetSimpIF = GetSimpIF;
   vtab->SetSeed = SetSeed;
   vtab->SetSimpFI = SetSimpFI;
   vtab->SetSimpIF = SetSimpIF;
   vtab->TestSeed = TestSeed;
   vtab->TestSimpFI = TestSimpFI;
   vtab->TestSimpIF = TestSimpIF;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   mapping = (AstMappingVtab *) vtab;
   parent_getobjsize = object->GetObjSize;
   object->GetObjSize = GetObjSize;

   parent_clearattrib = object->ClearAttrib;
   object->ClearAttrib = ClearAttrib;
   parent_getattrib = object->GetAttrib;
   object->GetAttrib = GetAttrib;
   parent_setattrib = object->SetAttrib;
   object->SetAttrib = SetAttrib;
   parent_testattrib = object->TestAttrib;
   object->TestAttrib = TestAttrib;

   parent_transform = mapping->Transform;
   mapping->Transform = Transform;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   object->Equal = Equal;
   mapping->MapMerge = MapMerge;

/* Declare the copy constructor, destructor and class dump function. */
   astSetCopy( vtab, Copy );
   astSetDelete( vtab, Delete );
   astSetDump( vtab, Dump, "MathMap",
               "Transformation using mathematical functions" );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
}

static double LogGamma( double x, int *status ) {
/*
*  Name:
*     LogGamma

*  Purpose:
*     Calculate the logarithm of the gamma function.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mathmap.h"
*     double LogGamma( double x, int *status )

*  Class Membership:
*     MathMap member function.

*  Description:
*     This function returns the natural logarithm of the gamma function
*     for real arguments x>0. It uses the approximation of Lanczos, with
*     constants from Press et al. (Numerical Recipes), giving a maximum
*     fractional error (on the gamma function) of less than 2e-10.

*  Parameters:
*     x
*        The function argument, which must be greater than zero.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The natural logarithm of the gamma function with "x" as argument,
*     or AST__BAD if "x" is not greater than zero.

*  Notes:
*     - This function does not generate errors and does not perform error
*     reporting. It will execute even if the global error status is set.
*/

/* Local Constants: */
   const double c0 = 1.000000000190015; /* Coefficients for series sum... */
   const double c1 = 76.18009172947146;
   const double c2 = -86.50532032941677;
   const double c3 = 24.01409824083091;
   const double c4 = -1.231739572450155;
   const double c5 = 0.1208650973866179e-2;
   const double c6 = -0.5395239384953e-5;
   const double g = 5.0;

/* Local Variables: */
   double result;                /* Result value to return */
   double sum;                   /* Series sum */
   double xx;                    /* Denominator for summing series */
   static double root_twopi;     /* sqrt( 2.0 * pi ) */
   static int init = 0;          /* Initialisation performed? */

/* If initialisation has not yet been performed, calculate the
   constant required below. */
   LOCK_MUTEX3
   if ( !init ) {
      root_twopi = sqrt( 2.0 * acos( -1.0 ) );

/* Note that initialisation has been performed. */
      init = 1;
   }
   UNLOCK_MUTEX3

/* Return a bad value if "x" is not greater than zero. */
   if ( x <= 0.0 ) {
      result = AST__BAD;

/* Otherwise, form the series sum. Since we only use 6 terms, the loop
   that would normally be used has been completely unrolled here. */
   } else {
      xx = x;
      sum = c0;
      sum += c1 / ++xx;
      sum += c2 / ++xx;
      sum += c3 / ++xx;
      sum += c4 / ++xx;
      sum += c5 / ++xx;
      sum += c6 / ++xx;

/* Calculate the result. */
      result = x + g + 0.5;
      result -= ( x + 0.5 ) * log( result );
      result = log( root_twopi * sum / x ) - result;
   }

/* Return the result. */
   return result;
}

static int MapMerge( AstMapping *this, int where, int series, int *nmap,
                     AstMapping ***map_list, int **invert_list, int *status ) {
/*
*  Name:
*     MapMerge

*  Purpose:
*     Simplify a sequence of Mappings containing a MathMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     int MapMerge( AstMapping *this, int where, int series, int *nmap,
*                   AstMapping ***map_list, int **invert_list, int *status )

*  Class Membership:
*     MathMap method (over-rides the protected astMapMerge method
*     inherited from the Mapping class).

*  Description:
*     This function attempts to simplify a sequence of Mappings by
*     merging a nominated MathMap in the sequence with its neighbours,
*     so as to shorten the sequence if possible.
*
*     In many cases, simplification will not be possible and the
*     function will return -1 to indicate this, without further
*     action.
*
*     In most cases of interest, however, this function will either
*     attempt to replace the nominated MathMap with one which it
*     considers simpler, or to merge it with the Mappings which
*     immediately precede it or follow it in the sequence (both will
*     normally be considered). This is sufficient to ensure the
*     eventual simplification of most Mapping sequences by repeated
*     application of this function.
*
*     In some cases, the function may attempt more elaborate
*     simplification, involving any number of other Mappings in the
*     sequence. It is not restricted in the type or scope of
*     simplification it may perform, but will normally only attempt
*     elaborate simplification in cases where a more straightforward
*     approach is not adequate.

*  Parameters:
*     this
*        Pointer to the nominated MathMap which is to be merged with
*        its neighbours. This should be a cloned copy of the MathMap
*        pointer contained in the array element "(*map_list)[where]"
*        (see below). This pointer will not be annulled, and the
*        MathMap it identifies will not be modified by this function.
*     where
*        Index in the "*map_list" array (below) at which the pointer
*        to the nominated MathMap resides.
*     series
*        A non-zero value indicates that the sequence of Mappings to
*        be simplified will be applied in series (i.e. one after the
*        other), whereas a zero value indicates that they will be
*        applied in parallel (i.e. on successive sub-sets of the
*        input/output coordinates).
*     nmap
*        Address of an int which counts the number of Mappings in the
*        sequence. On entry this should be set to the initial number
*        of Mappings. On exit it will be updated to record the number
*        of Mappings remaining after simplification.
*     map_list
*        Address of a pointer to a dynamically allocated array of
*        Mapping pointers (produced, for example, by the astMapList
*        method) which identifies the sequence of Mappings. On entry,
*        the initial sequence of Mappings to be simplified should be
*        supplied.
*
*        On exit, the contents of this array will be modified to
*        reflect any simplification carried out. Any form of
*        simplification may be performed. This may involve any of: (a)
*        removing Mappings by annulling any of the pointers supplied,
*        (b) replacing them with pointers to new Mappings, (c)
*        inserting additional Mappings and (d) changing their order.
*
*        The intention is to reduce the number of Mappings in the
*        sequence, if possible, and any reduction will be reflected in
*        the value of "*nmap" returned. However, simplifications which
*        do not reduce the length of the sequence (but improve its
*        execution time, for example) may also be performed, and the
*        sequence might conceivably increase in length (but normally
*        only in order to split up a Mapping into pieces that can be
*        more easily merged with their neighbours on subsequent
*        invocations of this function).
*
*        If Mappings are removed from the sequence, any gaps that
*        remain will be closed up, by moving subsequent Mapping
*        pointers along in the array, so that vacated elements occur
*        at the end. If the sequence increases in length, the array
*        will be extended (and its pointer updated) if necessary to
*        accommodate any new elements.
*
*        Note that any (or all) of the Mapping pointers supplied in
*        this array may be annulled by this function, but the Mappings
*        to which they refer are not modified in any way (although
*        they may, of course, be deleted if the annulled pointer is
*        the final one).
*     invert_list
*        Address of a pointer to a dynamically allocated array which,
*        on entry, should contain values to be assigned to the Invert
*        attributes of the Mappings identified in the "*map_list"
*        array before they are applied (this array might have been
*        produced, for example, by the astMapList method). These
*        values will be used by this function instead of the actual
*        Invert attributes of the Mappings supplied, which are
*        ignored.
*
*        On exit, the contents of this array will be updated to
*        correspond with the possibly modified contents of the
*        "*map_list" array.  If the Mapping sequence increases in
*        length, the "*invert_list" array will be extended (and its
*        pointer updated) if necessary to accommodate any new
*        elements.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     If simplification was possible, the function returns the index
*     in the "map_list" array of the first element which was
*     modified. Otherwise, it returns -1 (and makes no changes to the
*     arrays supplied).

*  Notes:
*     - A value of -1 will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstMapping *new;              /* Pointer to replacement Mapping */
   AstMathMap *mathmap1;         /* Pointer to first MathMap */
   AstMathMap *mathmap2;         /* Pointer to second MathMap */
   char **fwd1;                  /* Pointer to first forward function array */
   char **fwd2;                  /* Pointer to second forward function array */
   char **inv1;                  /* Pointer to first inverse function array */
   char **inv2;                  /* Pointer to second inverse function array */
   int ifun;                     /* Loop counter for functions */
   int imap1;                    /* Index of first Mapping */
   int imap2;                    /* Index of second Mapping */
   int imap;                     /* Loop counter for Mappings */
   int invert1;                  /* Invert flag for first MathMap */
   int invert2;                  /* Invert flag for second MathMap */
   int nfwd1;                    /* No. forward functions for first MathMap */
   int nfwd2;                    /* No. forward functions for second MathMap */
   int nin1;                     /* Number input coords for first MathMap */
   int ninv1;                    /* No. inverse functions for first MathMap */
   int ninv2;                    /* No. inverse functions for second MathMap */
   int nout2;                    /* Number output coords for second MathMap */
   int result;                   /* Result value to return */
   int simplify;                 /* Mappings may simplify? */

/* Initialise the returned result. */
   result = -1;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Initialise variables to avoid "used of uninitialised variable"
   messages from dumb compilers. */
   mathmap1 = NULL;
   mathmap2 = NULL;
   imap1 = 0;
   imap2 = 0;
   invert1 = 0;
   invert2 = 0;
   nfwd1 = 0;
   nin1 = 0;
   ninv1 = 0;

/* MathMaps are only worth simplifying if they occur in series. */
   simplify = series;

/* If simplification appears possible, then obtain the indices of the
   nominated mapping and of the one which follows it. Check that a
   mapping exists for the second index. */
   if ( simplify ) {
      imap1 = where;
      imap2 = imap1 + 1;
      simplify = ( imap2 < *nmap );
   }

/* If OK, check whether the class of both Mappings is "MathMap" (a
   MathMap can only combine with another MathMap). */
   if ( simplify ) {
      simplify = !strcmp( astGetClass( ( *map_list )[ imap1 ] ), "MathMap" );
   }
   if ( astOK && simplify ) {
      simplify = !strcmp( astGetClass( ( *map_list )[ imap2 ] ), "MathMap" );
   }

/* If still OK, obtain pointers to the two MathMaps and the associated
   invert flag values. */
   if ( astOK && simplify ) {
      mathmap1 = (AstMathMap *) ( *map_list )[ imap1 ];
      mathmap2 = (AstMathMap *) ( *map_list )[ imap2 ];
      invert1 = ( *invert_list )[ imap1 ];
      invert2 = ( *invert_list )[ imap2 ];

/* Depending on the invert flag values, obtain the SimpFI or SimpIF
   attribute value from each MathMap and check whether they are set so as
   to permit simplification. */
      simplify = ( ( invert1 ? astGetSimpIF( mathmap1 ) :
                               astGetSimpFI( mathmap1 ) ) &&
                   ( invert2 ? astGetSimpFI( mathmap2 ) :
                               astGetSimpIF( mathmap2 ) ) );
   }

/* If still OK, obtain the effective numbers of input coordinates for
   the first MathMap and output coordinates for the second. Take account
   of the associated invert flags and the way the Invert attribute of
   each MathMap is currently set. */
   if ( astOK && simplify ) {
      nin1 = ( invert1 == astGetInvert( mathmap1 ) ) ?
             astGetNin( mathmap1 ) : astGetNout( mathmap1 );
      nout2 = ( invert2 == astGetInvert( mathmap2 ) ) ?
              astGetNout( mathmap2 ) : astGetNin( mathmap2 );

/* Simplification is only possible if these two numbers are equal
   (otherwise the the two MathMaps cannot be identical). */
      simplify = ( nin1 == nout2 );
   }

/* If still OK, obtain the effective number of forward transformation
   functions for the first MathMap (allowing for the associated invert
   flag). Similarly, obtain the effective number of inverse
   transformation functions for the second MathMap. */
   if ( astOK && simplify ) {
      nfwd1 = !invert1 ? mathmap1->nfwd : mathmap1->ninv;
      ninv2 = !invert2 ? mathmap2->ninv : mathmap2->nfwd;

/* Check whether these values are equal. The MathMaps cannot be
   identical if they are not. */
      simplify = ( nfwd1 == ninv2 );
   }

/* As above, obtain pointers to the array of effective forward
   transformation functions for the first MathMap, and the effective
   inverse transformation functions for the second MathMap. */
   if ( astOK && simplify ) {
      fwd1 = !invert1 ? mathmap1->fwdfun : mathmap1->invfun;
      inv2 = !invert2 ? mathmap2->invfun : mathmap2->fwdfun;

/* Loop to check whether these two sets of functions are
   identical. The MathMaps cannot be merged unless they are. */
      for ( ifun = 0; ifun < nfwd1; ifun++ ) {
         simplify = !strcmp( fwd1[ ifun ], inv2[ ifun ] );
         if ( !simplify ) break;
      }
   }

/* If OK, repeat the above process to compare the effective inverse
   transformation functions of the first MathMap with the forward
   functions of the second one. */
   if ( astOK && simplify ) {
      ninv1 = !invert1 ? mathmap1->ninv : mathmap1->nfwd;
      nfwd2 = !invert2 ? mathmap2->nfwd : mathmap2->ninv;
      simplify = ( ninv1 == nfwd2 );
   }
   if ( astOK && simplify ) {
      inv1 = !invert1 ? mathmap1->invfun : mathmap1->fwdfun;
      fwd2 = !invert2 ? mathmap2->fwdfun : mathmap2->invfun;
      for ( ifun = 0; ifun < ninv1; ifun++ ) {
         simplify = !strcmp( inv1[ ifun ], fwd2[ ifun ] );
         if ( !simplify ) break;
      }
   }

/* If the two MathMaps can be merged, create a UnitMap as a
   replacement. */
   if ( astOK && simplify ) {
      new = (AstMapping *) astUnitMap( nin1, "", status );

/* If OK, annul the pointers to the original MathMaps. */
      if ( astOK ) {
         ( *map_list )[ imap1 ] = astAnnul( ( *map_list )[ imap1 ] );
         ( *map_list )[ imap2 ] = astAnnul( ( *map_list )[ imap2 ] );

/* Insert the pointer to the replacement UnitMap and store the
   associated invert flag. */
         ( *map_list )[ imap1 ] = new;
         ( *invert_list )[ imap1 ] = 0;

/* Loop to move the following Mapping pointers and invert flags down
   in their arrays to close the gap. */
         for ( imap = imap2 + 1; imap < *nmap; imap++ ) {
            ( *map_list )[ imap - 1 ] = ( *map_list )[ imap ];
            ( *invert_list )[ imap - 1 ] = ( *invert_list )[ imap ];
         }

/* Clear the final entry in each array. */
         ( *map_list )[ *nmap - 1 ] = NULL;
         ( *invert_list )[ *nmap - 1 ] = 0;

/* Decrement the Mapping count and return the index of the first
   modified element. */
         ( *nmap )--;
         result = imap1;
      }
   }

/* If an error occurred, clear the returned value. */
   if ( !astOK ) result = -1;

/* Return the result. */
   return result;
}

static void ParseConstant( const char *method, const char *class,
                           const char *exprs, int istart, int *iend,
                           double *con, int *status ) {
/*
*  Name:
*     ParseConstant

*  Purpose:
*     Parse a constant.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mathmap.h"
*     void ParseConstant( const char *method, const char *class,
*                         const char *exprs, int istart, int *iend,
*                         double *con, int *status )

*  Class Membership:
*     MathMap member function.

*  Description:
*     This routine parses an expression, looking for a constant starting at
*     the character with index "istart" in the string "exprs". If it
*     identifies the constant successfully, "*con" it will return its value
*     and "*iend" will be set to the index of the final constant character
*     in "exprs".
*
*     If the characters encountered are clearly not part of a constant (it
*     does not begin with a numeral or decimal point) the function returns
*     with "*con" set to zero and "*iend" set to -1, but without reporting
*     an error. However, if the first character appears to be a constant but
*     its syntax proves to be invalid, then an error is reported.
*
*     The expression must be in lower case with no embedded white space.
*     The constant must not have a sign (+ or -) in front of it.

*  Parameters:
*     method
*        Pointer to a constant null-terminated character string
*        containing the name of the method that invoked this function.
*        This method name is used solely for constructing error messages.
*     class
*        Pointer to a constant null-terminated character string containing the
*        class name of the Object being processed. This name is used solely
*        for constructing error messages.
*     exprs
*        Pointer to a null-terminated string containing the expression
*        to be parsed.
*     istart
*        Index of the first character in "exprs" to be considered by this
*        function.
*     iend
*        Pointer to an int in which to return the index in "exprs" of the
*        final character which forms part of the constant. If no constant is
*        found, a value of -1 is returned.
*     con
*        Pointer to a double, in which the value of the constant, if found,
*        will be returned.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   char *str;                    /* Pointer to temporary string */
   char c;                       /* Single character from the expression */
   int dpoint;                   /* Decimal point encountered? */
   int expon;                    /* Exponent character encountered? */
   int i;                        /* Loop counter for characters */
   int iscon;		         /* Character is part of the constant? */
   int n;                        /* Number of values read by astSscanf */
   int nc;                       /* Number of characters read by astSscanf */
   int numer;                    /* Numeral encountered in current field? */
   int sign;                     /* Sign encountered? */
   int valid;		         /* Constant syntax valid? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Initialise. */
   *con = 0.0;
   *iend = -1;

/* Check if the expression starts with a numeral or a decimal point. */
   c = exprs[ istart ];
   numer = isdigit( c );
   dpoint = ( c == '.' );

/* If it begins with any of these, the expression is clearly intended
   to be a constant, so any failure beyond this point will result in an
   error. Otherwise, failure to find a constant is not an error. */
   if ( numer || dpoint ) {

/* Initialise remaining variables specifying the parser context. */
      expon = 0;
      sign = 0;
      valid = 1;

/* Loop to increment the last constant character position until the
   following character in the expression does not look like part of the
   constant. */
      *iend = istart;
      iscon = 1;
      while ( ( c = exprs[ *iend + 1 ] ) && iscon ) {
         iscon = 0;

/* It may be part of a numerical constant if it is a numeral, wherever
   it occurs. */
         if ( isdigit( c ) ) {
            numer = 1;
            iscon = 1;

/* Or a decimal point, so long as it is the first one and is not in
   the exponent field. Otherwise it is invalid. */
         } else if ( c == '.' ) {
            if ( !( dpoint || expon ) ) {
               dpoint = 1;
               iscon = 1;
            } else {
               valid = 0;
            }

/* Or if it is a 'd' or 'e' exponent character, so long as it is the
   first one and at least one numeral has been encountered first.
   Otherwise it is invalid. */
          } else if ( ( c == 'd' ) || ( c == 'e' ) ) {
             if ( !expon && numer ) {
                expon = 1;
                numer = 0;
                iscon = 1;
             } else {
                valid = 0;
             }

/* Or if it is a sign, so long as it is in the exponent field and is
   the first sign with no previous numerals in the same field. Otherwise
   it is invalid (unless numerals have been encountered, in which case it
   marks the end of the constant). */
          } else if ( ( c == '+' ) || ( c == '-' ) ) {
             if ( expon && !sign && !numer ) {
                sign = 1;
                iscon = 1;
             } else if ( !numer ) {
                valid = 0;
             }
          }

/* Increment the character count if the next character may be part of
   the constant, or if it was invalid (it will then form part of the
   error message). */
          if ( iscon || !valid ) ( *iend )++;
      }

/* Finally, check that the last field contained a numeral. */
      valid = ( valid && numer );

/* If the constant appears valid, allocate a temporary string to hold
   it. */
      if ( valid ) {
         str = astMalloc( (size_t) ( *iend - istart + 2 ) );
         if ( astOK ) {

/* Copy the constant's characters, changing 'd' to 'e' so that
   "astSscanf" will recognise it as an exponent character. */
            for ( i = istart; i <= *iend; i++ ) {
               str[ i - istart ] = ( exprs[ i ] == 'd' ) ? 'e' : exprs[ i ];
            }
            str[ *iend - istart + 1 ] = '\0';

/* Attempt to read the constant as a double, noting how many values
   are read and how many characters consumed. */
            n = astSscanf( str, "%lf%n", con, &nc );

/* Check that one value was read and all the characters consumed. If
   not, then the constant's syntax is invalid. */
            if ( ( n != 1 ) || ( nc < ( *iend - istart + 1 ) ) ) valid = 0;
         }

/* Free the temporary string. */
         str = astFree( str );
      }

/* If the constant syntax is invalid, and no other error has occurred,
   then report an error. */
      if ( astOK && !valid ) {
         astError( AST__CONIN,
                   "%s(%s): Invalid constant syntax in the expression "
                   "\"%.*s\".", status,
                   method, class, *iend + 1, exprs );
      }

/* If an error occurred, reset the output values. */
      if ( !astOK ) {
         *iend = -1;
         *con = 0.0;
      }
   }
}

static void ParseName( const char *exprs, int istart, int *iend, int *status ) {
/*
*  Name:
*     ParseName

*  Purpose:
*     Parse a name.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mathmap.h"
*     void ParseName( const char *exprs, int istart, int *iend, int *status )

*  Class Membership:
*     MathMap member function.

*  Description:
*     This routine parses an expression, looking for a name starting at the
*     character with index "istart" in the string "exprs". If it identifies
*     a name successfully, "*iend" will return the index of the final name
*     character in "exprs". A name must begin with an alphabetic character
*     and subsequently contain only alphanumeric characters or underscores.
*
*     If the expression does not contain a name at the specified location,
*     "*iend" is set to -1. No error results.
*
*     The expression should not contain embedded white space.

*  Parameters:
*     exprs
*        Pointer to a null-terminated string containing the expression
*        to be parsed.
*     istart
*        Index of the first character in "exprs" to be considered by this
*        function.
*     iend
*        Pointer to an int in which to return the index in "exprs" of the
*        final character which forms part of the name. If no name is
*        found, a value of -1 is returned.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   char c;                       /* Single character from expression */

/* Check the global error status. */
   if ( !astOK ) return;

/* Initialise. */
   *iend = -1;

/* Check the first character is valid for a name (alphabetic). */
   if ( isalpha( exprs[ istart ] ) ) {

/* If so, loop to inspect each subsequent character until one is found
   which is not part of a name (not alphanumeric or underscore). */
      for ( *iend = istart; ( c = exprs[ *iend + 1 ] ); ( *iend )++ ) {
         if ( !( isalnum( c ) || ( c == '_' ) ) ) break;
      }
   }
}

static void ParseVariable( const char *method, const char *class,
                           const char *exprs, int istart, int nvar,
                           const char *var[], int *ivar, int *iend, int *status ) {
/*
*  Name:
*     ParseVariable

*  Purpose:
*     Parse a variable name.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mathmap.h"
*     void ParseVariable( const char *method, const char *class,
*                         const char *exprs, int istart, int nvar,
*                         const char *var[], int *ivar, int *iend, int *status )

*  Class Membership:
*     MathMap member function.

*  Description:
*     This routine parses an expression, looking for a recognised variable
*     name starting at the character with index "istart" in the string
*     "exprs". If it identifies a variable name successfully, "*ivar" will
*     return a value identifying it and "*iend" will return the index of the
*     final variable name character in "exprs". To be recognised, a name
*     must begin with an alphabetic character and subsequently contain only
*     alphanumeric characters or underscores. It must also appear in the
*     list of defined variable names supplied to this function.
*
*     If the expression does not contain a name at the specified location,
*     "*ivar" and "*iend" are set to -1 and no error results. However, if
*     the expression contains a name but it is not in the list of defined
*     variable names supplied, then an error is reported.
*
*     This function is case sensitive. The expression should not contain
*     embedded white space.

*  Parameters:
*     method
*        Pointer to a constant null-terminated character string
*        containing the name of the method that invoked this function.
*        This method name is used solely for constructing error messages.
*     class
*        Pointer to a constant null-terminated character string containing the
*        class name of the Object being processed. This name is used solely
*        for constructing error messages.
*     exprs
*        Pointer to a null-terminated string containing the expression
*        to be parsed.
*     istart
*        Index of the first character in "exprs" to be considered by this
*        function.
*     nvar
*        The number of defined variable names.
*     var
*        An array of pointers (with "nvar" elements) to null-terminated
*        strings. Each of these should contain a variable name to be
*        recognised. These strings are case sensitive and should contain
*        no white space.
*     ivar
*        Pointer to an int in which to return the index in "vars" of the
*        variable name found. If no variable name is found, a value of -1
*        is returned.
*     iend
*        Pointer to an int in which to return the index in "exprs" of the
*        final character which forms part of the variable name. If no variable
*        name is found, a value of -1 is returned.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   int found;                    /* Variable name recognised? */
   int nc;                       /* Number of characters in variable name */

/* Check the global error status. */
   if ( !astOK ) return;

/* Initialise. */
   *ivar = -1;
   *iend = -1;

/* Determine if the characters in the expression starting at index
   "istart" constitute a valid name. */
   ParseName( exprs, istart, iend, status );

/* If so, calculate the length of the name. */
   if ( *iend >= istart ) {
      nc = *iend - istart + 1;

/* Loop to compare the name with the list of variable names
   supplied. */
      found = 0;
      for ( *ivar = 0; *ivar < nvar; ( *ivar )++ ) {
         found = ( nc == (int) strlen( var[ *ivar ] ) ) &&
                 !strncmp( exprs + istart, var[ *ivar ], (size_t) nc );

/* Break if the name is recognised. */
         if ( found ) break;
      }

/* If it was not recognised, then report an error and reset the output
   values. */
      if ( !found ) {
         astError( AST__UDVOF,
                   "%s(%s): Undefined variable or function in the expression "
                   "\"%.*s\".", status,
                   method, class, *iend + 1, exprs );
         *ivar = -1;
         *iend = -1;
      }
   }
}

static double Poisson( Rcontext *context, double mean, int *status ) {
/*
*  Name:
*     Poisson

*  Purpose:
*     Produce a pseudo-random sample from a Poisson distribution.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mathmap.h"
*     double Poisson( Rcontext *context, double mean, int *status )

*  Class Membership:
*     MathMap member function.

*  Description:
*     On each invocation, this function returns a pseudo-random sample drawn
*     from a Poisson distribution with a specified mean. A combination of
*     methods is used, depending on the value of the mean. The algorithm is
*     based on that given by Press et al. (Numerical Recipes), but
*     re-implemented and extended.

*  Parameters:
*     context
*        Pointer to an Rcontext structure which holds the random number
*        generator's context between invocations.
*     mean
*        The mean of the Poisson distribution, which should not be
*        negative.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A sample (which will only take integer values) from the Poisson
*     distribution, or AST__BAD if the mean supplied is negative.

*  Notes:
*     - The sequence of numbers returned is determined by the "seed"
*     value in the Rcontext structure supplied.
*     - If the seed value is changed, the "active" flag must also be cleared
*     so that this function can re-initiallise the Rcontext structure before
*     generating the next pseudo-random number. The "active" flag should
*     also be clear to force initialisation the first time an Rcontext
*     structure is used.
*     - This function does not perform error checking and does not generate
*     errors. It will execute even if the global error status is set.
*/

/* Local Constants: */
   const double small = 9.3;     /* "Small" distribution mean value */

/* Local Variables: */
   double pfract;                /* Probability of accepting sample */
   double product;               /* Product of random samples */
   double ran;                   /* Sample from Lorentzian distribution */
   double result;                /* Result value to return */
   static double beta;           /* Constant for forming acceptance ratio */
   static double huge;           /* Large mean where std. dev. is negligible */
   static double last_mean;      /* Value of "mean" on last invocation */
   static double log_mean;       /* Logarithm of "mean" */
   static double pi;             /* Value of pi */
   static double ranmax;         /* Maximum safe value of "ran" */
   static double root_2mean;     /* sqrt( 2.0 * mean ) */
   static double sqrt_point9;    /* Square root of 0.9 */
   static double thresh;         /* Threshold for product of samples */
   static int init = 0;          /* Local initialisation performed? */

   LOCK_MUTEX6

/* If initialisation has not yet been performed, then perform it
   now. */
   if ( !init ) {

/* Initialise the mean value from the previous invocation. */
      last_mean = -1.0;

/* Calculate simple constants. */
      pi = acos( -1.0 );
      sqrt_point9 = sqrt( 0.9 );

/* Calculate the value of the distribution mean for which the smallest
   representable deviation from the mean permitted by the machine
   precision is one thousand standard deviations. */
      huge = pow( 1.0e3 / DBL_EPSILON, 2.0 );

/* Calculate the largest value such that
   (0.9+(sqrt_point9*ranmax)*(sqrt_point9*ranmax)) doesn't overflow,
   allowing a small margin for rounding error. */
      ranmax = ( sqrt( DBL_MAX - 0.9 ) / sqrt( 0.9 ) ) *
               ( 1.0 - 4.0 * DBL_EPSILON );

/* Note that initialisation has been performed. */
      init = 1;
   }

/* If the distribution mean is less than zero, then return a bad
   result. */
   if ( mean < 0.0 ) {
      result = AST__BAD;

/* If the mean is zero, then the result can only be zero. */
   } else if ( mean == 0.0 ) {
      result = 0.0;

/* Otherwise, if the mean is sufficiently small, we can use the direct
   method of summing a series of exponentially distributed random samples
   and counting the number which occur before the mean is exceeded. This
   is equivalent to multiplying a series of uniformly distributed
   samples and counting the number which occur before the product
   becomes less then an equivalent threshold. */
   } else if ( mean <= small ) {

/* If the mean has changed since the last invocation, store the new
   mean and calculate a new threshold. */
      if ( mean != last_mean ) {
         last_mean = mean;
         thresh = exp( -mean );
      }

/* Initialise the product and the result. */
      product = 1.0;
      result = -1.0;

/* Multiply the random samples, counting the number needed to reach
   the threshold. */
      do {
         product *= Rand( context, status );
         result += 1.0;
      } while ( product > thresh );

/* Otherwise, if the distribution mean is large (but not huge), we
   must use an indirect rejection method. */
   } else if ( mean <= huge ) {

/* If the mean has changed since the last invocation, then
   re-calculate the constants required below. Note that because of the
   restrictions we have placed on "mean", these calculations are safe
   against overflow. */
      if ( mean != last_mean ) {
         last_mean = mean;
         log_mean = log( mean );
         root_2mean = sqrt( 2.0 * mean );
         beta = mean * log_mean - LogGamma( mean + 1.0, status );
      }

/* Loop until a suitable random sample has been generated. */
      do {
         do {

/* First transform a sample from a uniform distribution to obtain a
   sample from a Lorentzian distribution. Check that the result is not so
   large as to cause overflow later. Also check for overflow in the maths
   library. If necessary, obtain a new sample. */
            do {
               errno = 0;
               ran = tan( pi * Rand( context, status ) );
            } while ( ( ran > ranmax ) ||
                      ( ( errno == ERANGE ) &&
                        ( ( ( ran >= 0.0 ) ? ran : -ran ) == HUGE_VAL ) ) );

/* If OK, scale the sample and add a constant so that the sample's
   distribution approximates the Poisson distribution we
   require. Overflow is prevented by the check on "ran" above, together
   with the restricted value of "mean". */
            result = ran * root_2mean + mean;

/* If the result is less than zero (where the Poisson distribution has
   value zero), then obtain a new sample. */
         } while ( result < 0.0 );

/* Round down to an integer, so that the sample is valid for a Poisson
   distribution. */
         result = floor( result );

/* Calculate the ratio between the required Poisson distribution and
   the Lorentzian from which we have sampled (the factor of 0.9 prevents
   this exceeding 1.0, and overflow is again prevented by the checks
   performed above). */
         ran *= sqrt_point9;
         pfract = ( 0.9 + ran * ran ) *
                  exp( result * log_mean - LogGamma( result + 1.0, status ) - beta );

/* Accept the sample with this fractional probability, otherwise
   obtain a new sample. */
      } while ( Rand( context, status ) > pfract );

/* If the mean is huge, the relative standard deviation will be
   negligible compared to the machine precision. In such cases, the
   probability of getting a result that differs from the mean is
   effectively zero, so we can simply return the mean. */
   } else {
      result = mean;
   }

   UNLOCK_MUTEX6

/* Return the result. */
   return result;
}

static double Rand( Rcontext *context, int *status ) {
/*
*  Name:
*     Rand

*  Purpose:
*     Produce a uniformly distributed pseudo-random number.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mathmap.h"
*     double Rand( Rcontext *context, int *status )

*  Class Membership:
*     MathMap member function.

*  Description:
*     On each invocation, this function returns a pseudo-random number
*     uniformly distributed in the range 0.0 to 1.0 (inclusive). The
*     underlying algorithm is that used by the "ran2" function of Press et
*     al. (Numerical Recipes), which has a long period and good statistical
*     properties. This independent implementation returns double precision
*     values.

*  Parameters:
*     context
*        Pointer to an Rcontext structure which holds the random number
*        generator's context between invocations.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - The sequence of numbers returned is determined by the "seed"
*     value in the Rcontext structure supplied.
*     - If the seed value is changed, the "active" flag must also be cleared
*     so that this function can re-initiallise the Rcontext structure before
*     generating the next pseudo-random number. The "active" flag should
*     also be clear to force initialisation the first time an Rcontext
*     structure is used.
*     - This function does not perform error checking and does not generate
*     errors. It will execute even if the global error status is set.
*/

/* Local Constants: */
   const long int a1 = 40014L;   /* Random number generator constants... */
   const long int a2 = 40692L;
   const long int m1 = 2147483563L;
   const long int m2 = 2147483399L;
   const long int q1 = 53668L;
   const long int q2 = 52774L;
   const long int r1 = 12211L;
   const long int r2 = 3791L;
   const int ntab =              /* Size of shuffle table */
      AST_MATHMAP_RAND_CONTEXT_NTAB_;
   const int nwarm = 8;          /* Number of warm-up iterations */

/* Local Variables: */
   double result;                /* Result value to return */
   double scale;                 /* Scale factor for random integers */
   double sum;                   /* Sum for forming normalisation constant */
   int dbits;                    /* Approximate bits in double mantissa */
   int irand;                    /* Loop counter for random integers */
   int itab;                     /* Loop counter for shuffle table */
   int lbits;                    /* Approximate bits used by generators */
   long int seed;                /* Random number seed */
   long int tmp;                 /* Temporary variable */
   static double norm;           /* Normalisation constant */
   static double scale0;         /* Scale decrement for successive integers */
   static int init = 0;          /* Local initialisation performed? */
   static int nrand;             /* Number of random integers to use */

/* If the random number generator context is not active, then
   initialise it. */
   if ( !context->active ) {

/* First, perform local initialisation for this function, if not
   already done. */
      LOCK_MUTEX4
      if ( !init ) {

/* Obtain the approximate number of bits used by the random integer
   generator from the value "m1". */
         (void) frexp( (double) m1, &lbits );

/* Obtain the approximate number of bits used by the mantissa of the
   double value we want to produce, allowing for the (unlikely)
   possibility that the mantissa's radix isn't 2. */
         dbits = (int) ceil( (double) DBL_MANT_DIG *
                             log( (double) FLT_RADIX ) / log( 2.0 ) );

/* Hence determine how many random integers we need to combine to
   produce each double value, so that all the mantissa's bits will be
   used. */
         nrand = ( dbits + lbits - 1 ) / lbits;

/* Calculate the scale factor by which each successive random
   integer's contribution to the result is reduced so as to generate
   progressively less significant bits. */
         scale0 = 1.0 / (double) ( m1 - 1L );

/* Loop to sum the maximum contributions from each random integer
   (assuming that each takes the largest possible value, of "m1-1",
   from which we will later subtract 1). This produces the normalisation
   factor by which the result must be scaled so as to lie between 0.0 and
   1.0 (inclusive). */
         sum = 0.0;
         scale = 1.0;
         for ( irand = 0; irand < nrand; irand++ ) {
            scale *= scale0;
            sum += scale;
         }
         norm = 1.0 / ( sum * (double) ( m1 - 2L ) );

/* Note that local initialisation has been done. */
         init = 1;
      }
      UNLOCK_MUTEX4

/* Obtain the seed value, enforcing positivity. */
      seed = (long int) context->seed;
      if ( seed < 1 ) seed = seed + LONG_MAX;
      if ( seed < 1 ) seed = LONG_MAX;

/* Initialise the random number generators with this seed. */
      context->rand1 = context->rand2 = seed;

/* Now loop to initialise the shuffle table with an initial set of
   random values. We generate more values than required in order to "warm
   up" the generator before recording values in the table. */
      for ( itab = ntab + nwarm - 1; itab >= 0; itab-- ) {

/* Repeatedly update "rand1" from the expression "(rand1*a1)%m1" while
   avoiding overflow. */
         tmp = context->rand1 / q1;
         context->rand1 = a1 * ( context->rand1 - tmp * q1 ) - tmp * r1;
         if ( context->rand1 < 0L ) context->rand1 += m1;

/* After warming up, start recording values in the table. */
         if ( itab < ntab ) context->table[ itab ] = context->rand1;
      }

/* Record the last entry in the table as the "previous" random
   integer. */
      context->random_int = context->table[ 0 ];

/* Note the random number generator context is active. */
      context->active = 1;
   }

/* Generate a random value. */
/* ------------------------ */
/* Initialise. */
   result = 0.0;

/* Loop to generate sufficient random integers to combine into a
   double value. */
   scale = norm;
   for ( irand = 0; irand < nrand; irand++ ) {

/* Update the first generator "rand1" from the expression
   "(a1*rand1)%m1" while avoiding overflow. */
      tmp = context->rand1 / q1;
      context->rand1 = a1 * ( context->rand1 - tmp * q1 ) - tmp * r1;
      if ( context->rand1 < 0L ) context->rand1 += m1;

/* Similarly, update the second generator "rand2" from the expression
   "(a2*rand2)%m2". */
      tmp = context->rand2 / q2;
      context->rand2 = a2 * ( context->rand2 - tmp * q2 ) - tmp * r2;
      if ( context->rand2 < 0L ) context->rand2 += m2;

/* Use the previous random integer to generate an index into the
   shuffle table. */
      itab = (int) ( context->random_int /
                     ( 1L + ( m1 - 1L ) / (long int) ntab ) );

/* The algorithm left by RFWS seems to have a bug that "itab" can
   sometimes be outside the range of [0.,ntab-1] causing the context->table
   array to be addressed out of bounds. To avoid this, use the
   following sticking plaster, since I'm not sure what the correct fix is. */
      if( itab < 0 ) itab = -itab;
      itab = itab % ntab;

/* Extract the table entry and replace it with a new random value from
   the first generator "rand1". This is the Bays-Durham shuffle. */
      context->random_int = context->table[ itab ];
      context->table[ itab ] = context->rand1;

/* Combine the extracted value with the latest value from the second
   generator "rand2". */
      context->random_int -= context->rand2;
      if ( context->random_int < 1L ) context->random_int += m1 - 1L;

/* Update the scale factor to apply to the resulting random integer
   and accumulate its contribution to the result. */
      scale *= scale0;
      result += scale * (double) ( context->random_int - 1L );
   }

/* Return the result. */
   return result;
}

static void SetAttrib( AstObject *this_object, const char *setting, int *status ) {
/*
*  Name:
*     SetAttrib

*  Purpose:
*     Set an attribute value for a MathMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mathmap.h"
*     void SetAttrib( AstObject *this, const char *setting, int *status )

*  Class Membership:
*     MathMap member function (extends the astSetAttrib method inherited from
*     the Mapping class).

*  Description:
*     This function assigns an attribute value for a MathMap, the attribute
*     and its value being specified by means of a string of the form:
*
*        "attribute= value "
*
*     Here, "attribute" specifies the attribute name and should be in lower
*     case with no white space present. The value to the right of the "="
*     should be a suitable textual representation of the value to be assigned
*     and this will be interpreted according to the attribute's data type.
*     White space surrounding the value is only significant for string
*     attributes.

*  Parameters:
*     this
*        Pointer to the MathMap.
*     setting
*        Pointer to a null terminated string specifying the new attribute
*        value.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     void
*/

/* Local Vaiables: */
   AstMathMap *this;             /* Pointer to the MathMap structure */
   int ival;                     /* Integer attribute value */
   int len;                      /* Length of setting string */
   int nc;                       /* Number of characters read by astSscanf */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the MathMap structure. */
   this = (AstMathMap *) this_object;

/* Obtain the length of the setting string. */
   len = strlen( setting );

/* Test for each recognised attribute in turn, using "astSscanf" to parse the
   setting string and extract the attribute value (or an offset to it in the
   case of string values). In each case, use the value set in "nc" to check
   that the entire string was matched. Once a value has been obtained, use the
   appropriate method to set it. */

/* Seed. */
/* ----- */
   if ( nc = 0,
        ( 1 == astSscanf( setting, "seed= %d %n", &ival, &nc ) )
        && ( nc >= len ) ) {
      astSetSeed( this, ival );

/* SimpFI. */
/* ------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( setting, "simpfi= %d %n", &ival, &nc ) )
               && ( nc >= len ) ) {
      astSetSimpFI( this, ival );

/* SimpIF. */
/* ------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( setting, "simpif= %d %n", &ival, &nc ) )
               && ( nc >= len ) ) {
      astSetSimpIF( this, ival );

/* Pass any unrecognised setting to the parent method for further
   interpretation. */
   } else {
      (*parent_setattrib)( this_object, setting, status );
   }
}

static int TestAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for a MathMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mathmap.h"
*     int TestAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     MathMap member function (over-rides the astTestAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     a value has been set for one of a MathMap's attributes.

*  Parameters:
*     this
*        Pointer to the MathMap.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if a value has been set, otherwise zero.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstMathMap *this;             /* Pointer to the MathMap structure */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the MathMap structure. */
   this = (AstMathMap *) this_object;

/* Check the attribute name and test the appropriate attribute. */

/* Seed. */
/* ----- */
   if ( !strcmp( attrib, "seed" ) ) {
      result = astTestSeed( this );

/* SimpFI. */
/* ------- */
   } else if ( !strcmp( attrib, "simpfi" ) ) {
      result = astTestSimpFI( this );

/* SimpIF. */
/* ------- */
   } else if ( !strcmp( attrib, "simpif" ) ) {
      result = astTestSimpIF( this );

/* If the attribute is not recognised, pass it on to the parent method
   for further interpretation. */
   } else {
      result = (*parent_testattrib)( this_object, attrib, status );
   }

/* Return the result, */
   return result;
}

static AstPointSet *Transform( AstMapping *map, AstPointSet *in,
                               int forward, AstPointSet *out, int *status ) {
/*
*  Name:
*     Transform

*  Purpose:
*     Apply a MathMap to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mathmap.h"
*     AstPointSet *Transform( AstMapping *map, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     MathMap member function (over-rides the astTransform method inherited
*     from the Mapping class).

*  Description:
*     This function takes a MathMap and a set of points encapsulated in a
*     PointSet and transforms the points so as to apply the required coordinate
*     transformation.

*  Parameters:
*     map
*        Pointer to the MathMap.
*     in
*        Pointer to the PointSet holding the input coordinate data.
*     forward
*        A non-zero value indicates that the forward coordinate transformation
*        should be applied, while a zero value requests the inverse
*        transformation.
*     out
*        Pointer to a PointSet which will hold the transformed (output)
*        coordinate values. A NULL value may also be given, in which case a
*        new PointSet will be created by this function.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the output (possibly new) PointSet.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*     -  The number of coordinate values per point in the input PointSet must
*     match the number of coordinates for the MathMap being applied.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstMathMap *this;             /* Pointer to MathMap to be applied */
   AstPointSet *result;          /* Pointer to output PointSet */
   double **data_ptr;            /* Array of pointers to coordinate data */
   double **ptr_in;              /* Pointer to input coordinate data */
   double **ptr_out;             /* Pointer to output coordinate data */
   double *work;                 /* Workspace for intermediate results */
   int idata;                    /* Loop counter for data pointer elements */
   int ifun;                     /* Loop counter for functions */
   int ncoord_in;                /* Number of coordinates per input point */
   int ncoord_out;               /* Number of coordinates per output point */
   int ndata;                    /* Number of data pointer elements filled */
   int nfun;                     /* Number of functions to evaluate */
   int npoint;                   /* Number of points */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise variables to avoid "used of uninitialised variable"
   messages from dumb compilers. */
   work = NULL;

/* Obtain a pointer to the MathMap. */
   this = (AstMathMap *) map;

/* Apply the parent mapping using the stored pointer to the Transform member
   function inherited from the parent Mapping class. This function validates
   all arguments and generates an output PointSet if necessary, but does not
   actually transform any coordinate values. */
   result = (*parent_transform)( map, in, forward, out, status );

/* We will now extend the parent astTransform method by performing the
   transformation needed to generate the output coordinate values. */

/* Determine the numbers of points and coordinates per point from the input
   and output PointSets and obtain pointers for accessing the input and output
   coordinate values. */
   ncoord_in = astGetNcoord( in );
   ncoord_out = astGetNcoord( result );
   npoint = astGetNpoint( in );
   ptr_in = astGetPoints( in );
   ptr_out = astGetPoints( result );

/* Determine whether to apply the forward or inverse transformation, according
   to the direction specified and whether the mapping has been inverted. */
   if ( astGetInvert( this ) ) forward = !forward;

/* Obtain the number of transformation functions that must be
   evaluated to perform the transformation. This will include any that
   produce intermediate results from which the final results are
   calculated. */
   nfun = forward ? this->nfwd : this->ninv;

/* If intermediate results are to be calculated, then allocate
   workspace to hold them (each intermediate result being a vector of
   "npoint" double values). */
   if ( nfun > ncoord_out ) {
      work = astMalloc( sizeof( double) *
                        (size_t) ( npoint * ( nfun - ncoord_out ) ) );
   }

/* Also allocate space for an array to hold pointers to the input
   data, intermediate results and output data. */
   data_ptr = astMalloc( sizeof( double * ) * (size_t) ( ncoord_in + nfun ) );

/* We now set up the "data_ptr" array to locate the data to be
   processed. */
   if ( astOK ) {

/* The first elements of this array point at the input data
   vectors. */
      ndata = 0;
      for ( idata = 0; idata < ncoord_in; idata++ ) {
         data_ptr[ ndata++ ] = ptr_in[ idata ];
      }

/* The following elements point at successive vectors within the
   workspace array (if allocated). These vectors will act first as output
   arrays for intermediate results, and then as input arrays for
   subsequent calculations which use these results. */
      for ( idata = 0; idata < ( nfun - ncoord_out ); idata++ ) {
         data_ptr[ ndata++ ] = work + ( idata * npoint );
      }

/* The final elements point at the output coordinate data arrays into
   which the final results will be written. */
      for ( idata = 0; idata < ncoord_out; idata++ ) {
         data_ptr[ ndata++ ] = ptr_out[ idata ];
      }

/* Perform coordinate transformation. */
/* ---------------------------------- */
/* Loop to evaluate each transformation function in turn. */
      for ( ifun = 0; ifun < nfun; ifun++ ) {

/* Invoke the function that evaluates compiled expressions. Pass the
   appropriate code and constants arrays, depending on the direction of
   coordinate transformation, together with the required stack size. The
   output array is the vector located by successive elements of the
   "data_ptr" array (skipping the input data elements), while the
   function has access to all previous elements of the "data_ptr" array
   to locate the required input data. */
         EvaluateFunction( &this->rcontext, npoint, (const double **) data_ptr,
                           forward ? this->fwdcode[ ifun ] :
                                     this->invcode[ ifun ],
                           forward ? this->fwdcon[ ifun ] :
                                     this->invcon[ ifun ],
                           forward ? this->fwdstack : this->invstack,
                           data_ptr[ ifun + ncoord_in ], status );
      }
   }

/* Free the array of data pointers and any workspace allocated for
   intermediate results. */
   data_ptr = astFree( data_ptr );
   if ( nfun > ncoord_out ) work = astFree( work );

/* If an error occurred, then return a NULL pointer. If no output
   PointSet was supplied, also delete any new one that may have been
   created. */
   if ( !astOK ) {
      result = ( result == out ) ? NULL : astDelete( result );
   }

/* Return a pointer to the output PointSet. */
   return result;
}

static void ValidateSymbol( const char *method, const char *class,
                            const char *exprs, int iend, int sym,
                            int *lpar, int **argcount, int **opensym,
                            int *ncon, double **con, int *status ) {
/*
*  Name:
*     ValidateSymbol

*  Purpose:
*     Validate a symbol in an expression.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mathmap.h"
*     void ValidateSymbol( const char *method, const char *class,
*                          const char *exprs, int iend, int sym, int *lpar,
*                          int **argcount, int **opensym, int *ncon,
*                          double **con, int *status )

*  Class Membership:
*     MathMap member function.

*  Description:
*     This function validates an identified standard symbol during
*     compilation of an expression. Its main task is to keep track of the
*     level of parenthesis in the expression and to count the number of
*     arguments supplied to functions at each level of parenthesis (for
*     nested function calls). On this basis it is able to interpret and
*     accept or reject symbols which represent function calls, parentheses
*     and delimiters. Other symbols are accepted automatically.

*  Parameters:
*     method
*        Pointer to a constant null-terminated character string
*        containing the name of the method that invoked this function.
*        This method name is used solely for constructing error messages.
*     class
*        Pointer to a constant null-terminated character string containing the
*        class name of the Object being processed. This name is used solely
*        for constructing error messages.
*     exprs
*        Pointer to a null-terminated string containing the expression
*        being parsed. This is only used for constructing error messages.
*     iend
*        Index in "exprs" of the last character belonging to the most
*        recently identified symbol. This is only used for constructing error
*        messages.
*     sym
*        Index in the static "symbol" array of the most recently identified
*        symbol in the expression. This is the symbol to be verified.
*     lpar
*        Pointer to an int which holds the current level of parenthesis. On
*        the first invocation, this should be zero. The returned value should
*        be passed to subsequent invocations.
*     argcount
*        Address of a pointer to a dynamically allocated array of int in
*        which argument count information is maintained for each level of
*        parenthesis (e.g. for nested function calls). On the first invocation,
*        "*argcount" should be NULL. This function will allocate the required
*        space as needed and update this pointer. The returned pointer value
*        should be passed to subsequent invocations.
*
*        The allocated space must be freed by the caller (using astFree) when
*        no longer required.
*     opensym
*        Address of a pointer to a dynamically allocated array of int, in which
*        information is maintained about the functions associated with each
*        level of parenthesis (e.g. for nested function calls). On the first
*        invocation, "*opensym" should be NULL. This function will allocate the
*        required space as needed and update this pointer. The returned pointer
*        value should be passed to subsequent invocations.
*
*        The allocated space must be freed by the caller (using astFree) when
*        no longer required.
*     ncon
*        Pointer to an int which holds a count of the constants associated
*        with the expression (and determines the size of the "*con" array).
*        This function will update the count to reflect any new constants
*        appended to the "*con" array and the returned value should be passed
*        to subsequent invocations.
*     con
*        Address of a pointer to a dynamically allocated array of double, in
*        which the constants associated with the expression being parsed are
*        accumulated. On entry, "*con" should point at a dynamic array with
*        at least "*ncon" elements containing existing constants (or may be
*        NULL if no constants have yet been stored). This function will
*        allocate the required space as needed and update this pointer (and
*        "*ncon") appropriately. The returned pointer value should be passed
*        to subsequent invocations.
*
*        The allocated space must be freed by the caller (using astFree) when
*        no longer required.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - The dynamically allocated arrays normally returned by this function
*     will be freed and NULL pointers will be returned if this function is
*     invoked with the global error status set, or if it should fail for any
*     reason.
*/

/* Check the global error status, but do not return at this point
   because dynamic arrays may require freeing. */
   if ( astOK ) {

/* Check if the symbol is a comma. */
      if ( ( symbol[ sym ].text[ 0 ] == ',' ) &&
           ( symbol[ sym ].text[ 1 ] == '\0' ) ) {

/* A comma is only used to delimit function arguments. If the current
   level of parenthesis is zero, or the symbol which opened the current
   level of parenthesis was not a function call (indicated by an argument
   count of zero at the current level of parenthesis), then report an
   error. */
         if ( ( *lpar <= 0 ) || ( ( *argcount )[ *lpar - 1 ] == 0 ) ) {
            astError( AST__COMIN,
                      "%s(%s): Spurious comma encountered in the expression "
                      "\"%.*s\".", status,
                      method, class, iend + 1, exprs );

/* If a comma is valid, then increment the argument count at the
   current level of parenthesis. */
         } else {
            ( *argcount )[ *lpar - 1 ]++;
         }

/* If the symbol is not a comma, check if it increases the current
   level of parenthesis. */
      } else if ( symbol[ sym ].parincrement > 0 ) {

/* Increase the size of the arrays which hold parenthesis level
   information and check for errors. */
         *argcount = astGrow( *argcount, *lpar + 1, sizeof( int ) );
         *opensym = astGrow( *opensym, *lpar + 1, sizeof( int ) );
         if ( astOK ) {

/* Increment the level of parenthesis and initialise the argument
   count at the new level. This count is set to zero if the symbol which
   opens the parenthesis level is not a function call (indicated by a
   zero "nargs" entry in the symbol data), and it subsequently remains at
   zero. If the symbol is a function call, the argument count is
   initially set to 1 and increments whenever a comma is encountered at
   this parenthesis level. */
            ( *argcount )[ ++( *lpar ) - 1 ] = ( symbol[ sym ].nargs != 0 );

/* Remember the symbol which opened this parenthesis level. */
            ( *opensym )[ *lpar - 1 ] = sym;
         }

/* Check if the symbol decreases the current parenthesis level. */
      } else if ( symbol[ sym ].parincrement < 0 ) {

/* Ensure that the parenthesis level is not already at zero. If it is,
   then there is a missing left parenthesis in the expression being
   compiled, so report an error. */
         if ( *lpar == 0 ) {
            astError( AST__MLPAR,
                      "%s(%s): Missing left parenthesis in the expression "
                      "\"%.*s\".", status,
                      method, class, iend + 1, exprs );

/* If the parenthesis level is valid and the symbol which opened this
   level of parenthesis was a function call with a fixed number of
   arguments (indicated by a positive "nargs" entry in the symbol data),
   then we must check the number of function arguments which have been
   encountered. */
         } else if ( symbol[ ( *opensym )[ *lpar - 1 ] ].nargs > 0 ) {

/* Report an error if the number of arguments is wrong. */
            if ( ( *argcount )[ *lpar - 1 ] !=
                 symbol[ ( *opensym )[ *lpar - 1 ] ].nargs ) {
               astError( AST__WRNFA,
                         "%s(%s): Wrong number of function arguments in the "
                         "expression \"%.*s\".", status,
                         method, class, iend + 1, exprs );

/* If the number of arguments is valid, decrement the parenthesis
   level. */
            } else {
               ( *lpar )--;
            }

/* If the symbol which opened this level of parenthesis was a function
   call with a variable number of arguments (indicated by a negative
   "nargs" entry in the symbol data), then we must check and process the
   number of function arguments. */
         } else if ( symbol[ ( *opensym )[ *lpar - 1 ] ].nargs < 0 ) {

/* Check that the minimum required number of arguments have been
   supplied. Report an error if they have not. */
            if ( ( *argcount )[ *lpar - 1 ] <
                 ( -symbol[ ( *opensym )[ *lpar - 1 ] ].nargs ) ) {
               astError( AST__WRNFA,
                         "%s(%s): Insufficient function arguments in the "
                         "expression \"%.*s\".", status,
                         method, class, iend + 1, exprs );

/* If the number of arguments is valid, increase the size of the
   constants array and check for errors. */
            } else {
               *con = astGrow( *con, *ncon + 1, sizeof( double ) );
               if ( astOK ) {

/* Append the argument count to the end of the array of constants and
   decrement the parenthesis level. */
                  ( *con )[ ( *ncon )++ ] =
                     (double) ( *argcount )[ --( *lpar ) ];
               }
            }

/* Finally, if the symbol which opened this level of parenthesis was
   not a function call ("nargs" entry in the symbol data is zero), then
   decrement the parenthesis level. In this case there is no need to
   check the argument count, because it will not have been
   incremented. */
         } else {
            ( *lpar )--;
         }
      }
   }

/* If an error occurred (or the global error status was set on entry),
   then reset the parenthesis level and free any memory which may have
   been allocated. */
   if ( !astOK ) {
      *lpar = 0;
      if ( *argcount ) *argcount = astFree( *argcount );
      if ( *opensym ) *opensym = astFree( *opensym );
      if ( *con ) *con = astFree( *con );
   }
}

/* Functions which access class attributes. */
/* ---------------------------------------- */
/* Implement member functions to access the attributes associated with
   this class using the macros defined for this purpose in the
   "object.h" file. For a description of each attribute, see the class
   interface (in the associated .h file). */

/*
*att++
*  Name:
*     Seed

*  Purpose:
*     Random number seed for a MathMap.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer.

*  Description:
*     This attribute, which may take any integer value, determines the
*     sequence of random numbers produced by the random number functions in
*     MathMap expressions. It is set to an unpredictable default value when
*     a MathMap is created, so that by default each MathMap uses a different
*     set of random numbers.
*
*     If required, you may set this Seed attribute to a value of your
*     choosing in order to produce repeatable behaviour from the random
*     number functions. You may also enquire the Seed value (e.g. if an
*     initially unpredictable value has been used) and then use it to
*     reproduce the resulting sequence of random numbers, either from the
*     same MathMap or from another one.
*
*     Clearing the Seed attribute gives it a new unpredictable default
*     value.

*  Applicability:
*     MathMap
*        All MathMaps have this attribute.
*att--
*/
/* Clear the Seed value by setting it to a new unpredictable value
   produced by DefaultSeed and clearing the "seed_set" flag in the
   MathMap's random number generator context. Also clear the "active"
   flag, so that the generator will be re-initialised to use this seed
   when it is next invoked. */
astMAKE_CLEAR(MathMap,Seed,rcontext.seed,( this->rcontext.seed_set = 0,
                                           this->rcontext.active = 0,
                                           DefaultSeed( &this->rcontext, status ) ))

/* Return the "seed" value from the random number generator
   context. */
astMAKE_GET(MathMap,Seed,int,0,this->rcontext.seed)

/* Store the new seed value in the MathMap's random number generator
   context and set the context's "seed_set" flag. Also clear the "active"
   flag, so that the generator will be re-initialised to use this seed
   when it is next invoked. */
astMAKE_SET(MathMap,Seed,int,rcontext.seed,( this->rcontext.seed_set = 1,
                                             this->rcontext.active = 0,
                                             value ))

/* Test the "seed_set" flag in the random number generator context. */
astMAKE_TEST(MathMap,Seed,( this->rcontext.seed_set ))

/*
*att++
*  Name:
*     SimpFI

*  Purpose:
*     Forward-inverse MathMap pairs simplify?

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer (boolean).

*  Description:
c     This attribute should be set to a non-zero value if applying a
c     MathMap's forward transformation, followed immediately by the matching
c     inverse transformation will always restore the original set of
c     coordinates. It indicates that AST may replace such a sequence of
c     operations by an identity Mapping (a UnitMap) if it is encountered
c     while simplifying a compound Mapping (e.g. using astSimplify).
f     This attribute should be set to a non-zero value if applying a
f     MathMap's forward transformation, followed immediately by the matching
f     inverse transformation will always restore the original set of
f     coordinates. It indicates that AST may replace such a sequence of
f     operations by an identity Mapping (a UnitMap) if it is encountered
f     while simplifying a compound Mapping (e.g. using AST_SIMPLIFY).
*
*     By default, the SimpFI attribute is zero, so that AST will not perform
*     this simplification unless you have set SimpFI to indicate that it is
*     safe to do so.

*  Applicability:
*     MathMap
*        All MathMaps have this attribute.

*  Notes:
*     - For simplification to occur, the two MathMaps must be in series and
*     be identical (with textually identical transformation
*     functions). Functional equivalence is not sufficient.
*     - The consent of both MathMaps is required before simplification can
*     take place. If either has a SimpFI value of zero, then simplification
*     will not occur.
*     - The SimpFI attribute controls simplification only in the case where
*     a MathMap's forward transformation is followed by the matching inverse
*     transformation. It does not apply if an inverse transformation is
*     followed by a forward transformation. This latter case is controlled
*     by the SimpIF attribute.
c     - The "forward" and "inverse" transformations referred to are those
c     defined when the MathMap is created (corresponding to the "fwd" and
c     "inv" parameters of its constructor function). If the MathMap is
c     inverted (i.e. its Invert attribute is non-zero), then the role of the
c     SimpFI and SimpIF attributes will be interchanged.
f     - The "forward" and "inverse" transformations referred to are those
f     defined when the MathMap is created (corresponding to the FWD and
f     INV arguments of its constructor function). If the MathMap is
f     inverted (i.e. its Invert attribute is non-zero), then the role of the
f     SimpFI and SimpIF attributes will be interchanged.
*att--
*/
/* Clear the SimpFI value by setting it to -INT_MAX. */
astMAKE_CLEAR(MathMap,SimpFI,simp_fi,-INT_MAX)

/* Supply a default of 0 if no SimpFI value has been set. */
astMAKE_GET(MathMap,SimpFI,int,0,( ( this->simp_fi != -INT_MAX ) ?
                                   this->simp_fi : 0 ))

/* Set a SimpFI value of 1 if any non-zero value is supplied. */
astMAKE_SET(MathMap,SimpFI,int,simp_fi,( value != 0 ))

/* The SimpFI value is set if it is not -INT_MAX. */
astMAKE_TEST(MathMap,SimpFI,( this->simp_fi != -INT_MAX ))

/*
*att++
*  Name:
*     SimpIF

*  Purpose:
*     Inverse-forward MathMap pairs simplify?

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer (boolean).

*  Description:
c     This attribute should be set to a non-zero value if applying a
c     MathMap's inverse transformation, followed immediately by the matching
c     forward transformation will always restore the original set of
c     coordinates. It indicates that AST may replace such a sequence of
c     operations by an identity Mapping (a UnitMap) if it is encountered
c     while simplifying a compound Mapping (e.g. using astSimplify).
f     This attribute should be set to a non-zero value if applying a
f     MathMap's inverse transformation, followed immediately by the matching
f     forward transformation will always restore the original set of
f     coordinates. It indicates that AST may replace such a sequence of
f     operations by an identity Mapping (a UnitMap) if it is encountered
f     while simplifying a compound Mapping (e.g. using AST_SIMPLIFY).
*
*     By default, the SimpIF attribute is zero, so that AST will not perform
*     this simplification unless you have set SimpIF to indicate that it is
*     safe to do so.

*  Applicability:
*     MathMap
*        All MathMaps have this attribute.

*  Notes:
*     - For simplification to occur, the two MathMaps must be in series and
*     be identical (with textually identical transformation
*     functions). Functional equivalence is not sufficient.
*     - The consent of both MathMaps is required before simplification can
*     take place. If either has a SimpIF value of zero, then simplification
*     will not occur.
*     - The SimpIF attribute controls simplification only in the case where
*     a MathMap's inverse transformation is followed by the matching forward
*     transformation. It does not apply if a forward transformation is
*     followed by an inverse transformation. This latter case is controlled
*     by the SimpFI attribute.
c     - The "forward" and "inverse" transformations referred to are those
c     defined when the MathMap is created (corresponding to the "fwd" and
c     "inv" parameters of its constructor function). If the MathMap is
c     inverted (i.e. its Invert attribute is non-zero), then the role of the
c     SimpFI and SimpIF attributes will be interchanged.
f     - The "forward" and "inverse" transformations referred to are those
f     defined when the MathMap is created (corresponding to the FWD and
f     INV arguments of its constructor function). If the MathMap is
f     inverted (i.e. its Invert attribute is non-zero), then the role of the
f     SimpFI and SimpIF attributes will be interchanged.
*att--
*/
/* Clear the SimpIF value by setting it to -INT_MAX. */
astMAKE_CLEAR(MathMap,SimpIF,simp_if,-INT_MAX)

/* Supply a default of 0 if no SimpIF value has been set. */
astMAKE_GET(MathMap,SimpIF,int,0,( ( this->simp_if != -INT_MAX ) ?
                                   this->simp_if : 0 ))

/* Set a SimpIF value of 1 if any non-zero value is supplied. */
astMAKE_SET(MathMap,SimpIF,int,simp_if,( value != 0 ))

/* The SimpIF value is set if it is not -INT_MAX. */
astMAKE_TEST(MathMap,SimpIF,( this->simp_if != -INT_MAX ))

/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout, int *status ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for MathMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for MathMap objects.

*  Parameters:
*     objin
*        Pointer to the object to be copied.
*     objout
*        Pointer to the object being constructed.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     void

*  Notes:
*     -  This constructor makes a deep copy.
*/

/* Local Variables: */
   AstMathMap *in;               /* Pointer to input MathMap */
   AstMathMap *out;              /* Pointer to output MathMap */
   int ifun;                     /* Loop counter for functions */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output MathMaps. */
   in = (AstMathMap *) objin;
   out = (AstMathMap *) objout;

/* For safety, first clear any references to the input memory from
   the output MathMap. */
   out->fwdfun = NULL;
   out->invfun = NULL;
   out->fwdcode = NULL;
   out->invcode = NULL;
   out->fwdcon = NULL;
   out->invcon = NULL;

/* Now allocate and initialise each of the output pointer arrays
   required. */
   if ( in->fwdfun ) {
      MALLOC_POINTER_ARRAY( out->fwdfun, char *, out->nfwd )
   }
   if ( in->invfun ) {
      MALLOC_POINTER_ARRAY( out->invfun, char *, out->ninv )
   }
   if ( in->fwdcode ) {
      MALLOC_POINTER_ARRAY( out->fwdcode, int *, out->nfwd )
   }
   if ( in->invcode ) {
      MALLOC_POINTER_ARRAY( out->invcode, int *, out->ninv )
   }
   if ( in->fwdcon ) {
      MALLOC_POINTER_ARRAY( out->fwdcon, double *, out->nfwd )
   }
   if ( in->invcon ) {
      MALLOC_POINTER_ARRAY( out->invcon, double *, out->ninv )
   }

/* If OK, loop to make copies of the data (where available) associated
   with each forward transformation function, storing pointers to the
   copy in the output pointer arrays allocated above. */
   if ( astOK ) {
      for ( ifun = 0; ifun < out->nfwd; ifun++ ) {
         if ( in->fwdfun && in->fwdfun[ ifun ] ) {
            out->fwdfun[ ifun ] = astStore( NULL, in->fwdfun[ ifun ],
                                            astSizeOf( in->fwdfun[ ifun ] ) );
         }
         if ( in->fwdcode && in->fwdcode[ ifun ] ) {
            out->fwdcode[ ifun ] = astStore( NULL, in->fwdcode[ ifun ],
                                            astSizeOf( in->fwdcode[ ifun ] ) );
         }
         if ( in->fwdcon && in->fwdcon[ ifun ] ) {
            out->fwdcon[ ifun ] = astStore( NULL, in->fwdcon[ ifun ],
                                            astSizeOf( in->fwdcon[ ifun ] ) );
         }
         if ( !astOK ) break;
      }
   }

/* Repeat this process for the inverse transformation functions. */
   if ( astOK ) {
      for ( ifun = 0; ifun < out->ninv; ifun++ ) {
         if ( in->invfun && in->invfun[ ifun ] ) {
            out->invfun[ ifun ] = astStore( NULL, in->invfun[ ifun ],
                                            astSizeOf( in->invfun[ ifun ] ) );
         }
         if ( in->invcode && in->invcode[ ifun ] ) {
            out->invcode[ ifun ] = astStore( NULL, in->invcode[ ifun ],
                                            astSizeOf( in->invcode[ ifun ] ) );
         }
         if ( in->invcon && in->invcon[ ifun ] ) {
            out->invcon[ ifun ] = astStore( NULL, in->invcon[ ifun ],
                                            astSizeOf( in->invcon[ ifun ] ) );
         }
         if ( !astOK ) break;
      }
   }

/* If an error occurred, clean up by freeing all output memory
   allocated above. */
   if ( !astOK ) {
      FREE_POINTER_ARRAY( out->fwdfun, out->nfwd )
      FREE_POINTER_ARRAY( out->invfun, out->ninv )
      FREE_POINTER_ARRAY( out->fwdcode, out->nfwd )
      FREE_POINTER_ARRAY( out->invcode, out->ninv )
      FREE_POINTER_ARRAY( out->fwdcon, out->nfwd )
      FREE_POINTER_ARRAY( out->invcon, out->ninv )
   }
}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for MathMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for MathMap objects.

*  Parameters:
*     obj
*        Pointer to the object to be deleted.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     void

*  Notes:
*     This function attempts to execute even if the global error status is
*     set.
*/

/* Local Variables: */
   AstMathMap *this;             /* Pointer to MathMap */

/* Obtain a pointer to the MathMap structure. */
   this = (AstMathMap *) obj;

/* Free all memory allocated by the MathMap. */
   FREE_POINTER_ARRAY( this->fwdfun, this->nfwd )
   FREE_POINTER_ARRAY( this->invfun, this->ninv )
   FREE_POINTER_ARRAY( this->fwdcode, this->nfwd )
   FREE_POINTER_ARRAY( this->invcode, this->ninv )
   FREE_POINTER_ARRAY( this->fwdcon, this->nfwd )
   FREE_POINTER_ARRAY( this->invcon, this->ninv )
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for MathMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the MathMap class to an output Channel.

*  Parameters:
*     this
*        Pointer to the MathMap whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Constants: */
#define COMMENT_LEN 150          /* Maximum length of a comment string */
#define KEY_LEN 50               /* Maximum length of a keyword */

/* Local Variables: */
   AstMathMap *this;             /* Pointer to the MathMap structure */
   char comment[ COMMENT_LEN + 1 ]; /* Buffer for comment strings */
   char key[ KEY_LEN + 1 ];      /* Buffer for keyword strings */
   int ifun;                     /* Loop counter for functions */
   int invert;                   /* MathMap inverted? */
   int ival;                     /* Integer attribute value */
   int nin;                      /* True number of input coordinates */
   int nout;                     /* True number of output coordinates */
   int set;                      /* Attribute value set? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the MathMap structure. */
   this = (AstMathMap *) this_object;

/* Determine if the MathMap is inverted and obtain the "true" number
   of input and output coordinates by un-doing the effects of any
   inversion. */
   invert = astGetInvert( this );
   nin = !invert ? astGetNin( this ) : astGetNout( this );
   nout = !invert ? astGetNout( this ) : astGetNin( this );

/* Write out values representing the instance variables for the
   MathMap class.  Accompany these with appropriate comment strings,
   possibly depending on the values being written.*/

/* In the case of attributes, we first use the appropriate (private)
   Test...  member function to see if they are set. If so, we then use
   the (private) Get... function to obtain the value to be written
   out.

   For attributes which are not set, we use the astGet... method to
   obtain the value instead. This will supply a default value
   (possibly provided by a derived class which over-rides this method)
   which is more useful to a human reader as it corresponds to the
   actual default attribute value.  Since "set" will be zero, these
   values are for information only and will not be read back. */

/* Number of forward transformation functions. */
/* ------------------------------------------- */
/* We regard this value as set if it differs from the number of output
   coordinates for the MathMap. */
   set = ( this->nfwd != nout );
   astWriteInt( channel, "Nfwd", set, 0, this->nfwd,
                "Number of forward transformation functions" );

/* Forward transformation functions. */
/* --------------------------------- */
/* Loop to write out each forward transformation function, generating
   a suitable keyword and comment for each one. */
   for ( ifun = 0; ifun < this->nfwd; ifun++ ) {
      (void) sprintf( key, "Fwd%d", ifun + 1 );
      (void) sprintf( comment, "Forward function %d", ifun + 1 );
      astWriteString( channel, key, 1, 1, this->fwdfun[ ifun ], comment );
   }

/* Number of inverse transformation functions. */
/* ------------------------------------------- */
/* We regard this value as set if it differs from the number of input
   coordinates for the MathMap. */
   set = ( this->ninv != nin );
   astWriteInt( channel, "Ninv", set, 0, this->ninv,
                "Number of inverse transformation functions" );

/* Inverse transformation functions. */
/* --------------------------------- */
/* Similarly, loop to write out each inverse transformation
   function. */
   for ( ifun = 0; ifun < this->ninv; ifun++ ) {
      (void) sprintf( key, "Inv%d", ifun + 1 );
      (void) sprintf( comment, "Inverse function %d", ifun + 1 );
      astWriteString( channel, key, 1, 1, this->invfun[ ifun ], comment );
   }

/* SimpFI. */
/* ------- */
/* Write out the forward-inverse simplification flag. */
   set = TestSimpFI( this, status );
   ival = set ? GetSimpFI( this, status ) : astGetSimpFI( this );
   astWriteInt( channel, "SimpFI", set, 0, ival,
                ival ? "Forward-inverse pairs may simplify" :
                       "Forward-inverse pairs do not simplify" );

/* SimpIF. */
/* ------- */
/* Write out the inverse-forward simplification flag. */
   set = TestSimpIF( this, status );
   ival = set ? GetSimpIF( this, status ) : astGetSimpIF( this );
   astWriteInt( channel, "SimpIF", set, 0, ival,
                ival ? "Inverse-forward pairs may simplify" :
                       "Inverse-forward pairs do not simplify" );

/* Seed. */
/* ----- */
/* Write out any random number seed value which is set. Prefix this with
   a separate flag which indicates if the seed has been set. */
   set = TestSeed( this, status );
   ival = set ? GetSeed( this, status ) : astGetSeed( this );
   astWriteInt( channel, "Seeded", set, 0, set,
                set? "Explicit random number seed set" :
                     "No random number seed set" );
   astWriteInt( channel, "Seed", set, 0, ival,
                set ? "Random number seed value" :
                      "Default random number seed used" );

/* Undefine macros local to this function. */
#undef COMMENT_LEN
#undef KEY_LEN
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAMathMap and astCheckMathMap functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(MathMap,Mapping)
astMAKE_CHECK(MathMap)

AstMathMap *astMathMap_( int nin, int nout,
                         int nfwd, const char *fwd[],
                         int ninv, const char *inv[],
                         const char *options, int *status, ...) {
/*
*+
*  Name:
*     astMathMap

*  Purpose:
*     Create a MathMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "mathmap.h"
*     AstMathMap *astMathMap( int nin, int nout,
*                             int nfwd, const char *fwd[],
*                             int ninv, const char *inv[],
*                             const char *options, ..., int *status )

*  Class Membership:
*     MathMap constructor.

*  Description:
*     This function creates a new MathMap and optionally initialises its
*     attributes.

*  Parameters:
*     nin
*        Number of input variables for the MathMap.
*     nout
*        Number of output variables for the MathMap.
*     nfwd
*        The number of forward transformation functions being supplied.
*        This must be at least equal to "nout".
*     fwd
*        Pointer to an array, with "nfwd" elements, of pointers to null
*        terminated strings which contain each of the forward transformation
*        functions.
*     ninv
*        The number of inverse transformation functions being supplied.
*        This must be at least equal to "nin".
*     inv
*        Pointer to an array, with "ninv" elements, of pointers to null
*        terminated strings which contain each of the inverse transformation
*        functions.
*     options
*        Pointer to a null terminated string containing an optional
*        comma-separated list of attribute assignments to be used for
*        initialising the new MathMap. The syntax used is the same as
*        for the astSet method and may include "printf" format
*        specifiers identified by "%" symbols in the normal way.
*     status
*        Pointer to the inherited status variable.
*     ...
*        If the "options" string contains "%" format specifiers, then
*        an optional list of arguments may follow it in order to
*        supply values to be substituted for these specifiers. The
*        rules for supplying these are identical to those for the
*        astSet method (and for the C "printf" function).

*  Returned Value:
*     A pointer to the new MathMap.

*  Notes:
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-

*  Implementation Notes:
*     - This function implements the basic MathMap constructor which is
*     available via the protected interface to the MathMap class.  A
*     public interface is provided by the astMathMapId_ function.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstMathMap *new;              /* Pointer to new MathMap */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the MathMap, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitMathMap( NULL, sizeof( AstMathMap ), !class_init, &class_vtab,
                         "MathMap", nin, nout, nfwd, fwd, ninv, inv );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new MathMap's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new MathMap. */
   return new;
}

AstMathMap *astMathMapId_( int nin, int nout,
                           int nfwd, const char *fwd[],
                           int ninv, const char *inv[],
                           const char *options, ... ) {
/*
*++
*  Name:
c     astMathMap
f     AST_MATHMAP

*  Purpose:
*     Create a MathMap.

*  Type:
*     Public function.

*  Synopsis:
c     #include "mathmap.h"
c     AstMathMap *astMathMap( int nin, int nout,
c                             int nfwd, const char *fwd[],
c                             int ninv, const char *inv[],
c                             const char *options, ... )
f     RESULT = AST_MATHMAP( NIN, NOUT, NFWD, FWD, NINV, INV, OPTIONS, STATUS )

*  Class Membership:
*     MathMap constructor.

*  Description:
*     This function creates a new MathMap and optionally initialises its
*     attributes.
*
c     A MathMap is a Mapping which allows you to specify a set of forward
c     and/or inverse transformation functions using arithmetic operations
c     and mathematical functions similar to those available in C. The
c     MathMap interprets these functions at run-time, whenever its forward
c     or inverse transformation is required. Because the functions are not
c     compiled in the normal sense (unlike an IntraMap), they may be used to
c     describe coordinate transformations in a transportable manner. A
c     MathMap therefore provides a flexible way of defining new types of
c     Mapping whose descriptions may be stored as part of a dataset and
c     interpreted by other programs.
f     A MathMap is a Mapping which allows you to specify a set of forward
f     and/or inverse transformation functions using arithmetic operations
f     and mathematical functions similar to those available in Fortran. The
f     MathMap interprets these functions at run-time, whenever its forward
f     or inverse transformation is required. Because the functions are not
f     compiled in the normal sense (unlike an IntraMap), they may be used to
f     describe coordinate transformations in a transportable manner. A
f     MathMap therefore provides a flexible way of defining new types of
f     Mapping whose descriptions may be stored as part of a dataset and
f     interpreted by other programs.

*  Parameters:
c     nin
f     NIN = INTEGER
*        Number of input variables for the MathMap. This determines the
*        value of its Nin attribute.
c     nout
f     NOUT = INTEGER
*        Number of output variables for the MathMap. This determines the
*        value of its Nout attribute.
c     nfwd
f     NFWD = INTEGER
*        The number of forward transformation functions being supplied.
c        This must be at least equal to "nout", but may be increased to
f        This must be at least equal to NOUT, but may be increased to
*        accommodate any additional expressions which define intermediate
*        variables for the forward transformation (see the "Calculating
*        Intermediate Values" section below).
c     fwd
f     FWD = CHARACTER * ( * )( NFWD )
c        An array (with "nfwd" elements) of pointers to null terminated strings
c        which contain the expressions defining the forward transformation.
f        An array which contains the expressions defining the forward
f        transformation.
*        The syntax of these expressions is described below.
c     ninv
f     NINV = INTEGER
*        The number of inverse transformation functions being supplied.
c        This must be at least equal to "nin", but may be increased to
f        This must be at least equal to NIN, but may be increased to
*        accommodate any additional expressions which define intermediate
*        variables for the inverse transformation (see the "Calculating
*        Intermediate Values" section below).
c     inv
f     INV = CHARACTER * ( * )( NINV )
c        An array (with "ninv" elements) of pointers to null terminated strings
c        which contain the expressions defining the inverse transformation.
f        An array which contains the expressions defining the inverse
f        transformation.
*        The syntax of these expressions is described below.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new MathMap. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
c        If no initialisation is required, a zero-length string may be
c        supplied.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new MathMap. The syntax used is identical to that for the
f        AST_SET routine. If no initialisation is required, a blank
f        value may be supplied.
c     ...
c        If the "options" string contains "%" format specifiers, then
c        an optional list of additional arguments may follow it in
c        order to supply values to be substituted for these
c        specifiers. The rules for supplying these are identical to
c        those for the astSet function (and for the C "printf"
c        function).
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astMathMap()
f     AST_MATHMAP = INTEGER
*        A pointer to the new MathMap.

*  Defining Transformation Functions:
c     A MathMap's transformation functions are supplied as a set of
c     expressions in an array of character strings. Normally you would
c     supply the same number of expressions for the forward transformation,
c     via the "fwd" parameter, as there are output variables (given by the
c     MathMap's Nout attribute). For instance, if Nout is 2 you might use:
c     - "r = sqrt( x * x + y * y )"
c     - "theta = atan2( y, x )"
c
c     which defines a transformation from Cartesian to polar
c     coordinates. Here, the variables that appear on the left of each
c     expression ("r" and "theta") provide names for the output variables
c     and those that appear on the right ("x" and "y") are references to
c     input variables.
f     A MathMap's transformation functions are supplied as a set of
f     expressions in an array of character strings. Normally you would
f     supply the same number of expressions for the forward transformation,
f     via the FWD argument, as there are output variables (given by the
f     MathMap's Nout attribute). For instance, if Nout is 2 you might use:
f     - 'R = SQRT( X * X + Y * Y )'
f     - 'THETA = ATAN2( Y, X )'
f
f     which defines a transformation from Cartesian to polar
f     coordinates. Here, the variables that appear on the left of each
f     expression (R and THETA) provide names for the output variables and
f     those that appear on the right (X and Y) are references to input
f     variables.
*
c     To complement this, you must also supply expressions for the inverse
c     transformation via the "inv" parameter.  In this case, the number of
c     expressions given would normally match the number of MathMap input
c     coordinates (given by the Nin attribute).  If Nin is 2, you might use:
c     - "x = r * cos( theta )"
c     - "y = r * sin( theta )"
c
c     which expresses the transformation from polar to Cartesian
c     coordinates. Note that here the input variables ("x" and "y") are
c     named on the left of each expression, and the output variables ("r"
c     and "theta") are referenced on the right.
f     To complement this, you must also supply expressions for the inverse
f     transformation via the INV argument.  In this case, the number of
f     expressions given would normally match the number of MathMap input
f     coordinates (given by the Nin attribute).  If Nin is 2, you might use:
f     - 'X = R * COS( THETA )'
f     - 'Y = R * SIN( THETA )'
f
f     which expresses the transformation from polar to Cartesian
f     coordinates. Note that here the input variables (X and Y) are named on
f     the left of each expression, and the output variables (R and THETA)
f     are referenced on the right.
*
*     Normally, you cannot refer to a variable on the right of an expression
*     unless it is named on the left of an expression in the complementary
*     set of functions. Therefore both sets of functions (forward and
*     inverse) must be formulated using the same consistent set of variable
*     names. This means that if you wish to leave one of the transformations
*     undefined, you must supply dummy expressions which simply name each of
*     the output (or input) variables.  For example, you might use:
c     - "x"
c     - "y"
f     - 'X'
f     - 'Y'
*
*     for the inverse transformation above, which serves to name the input
*     variables but without defining an inverse transformation.

*  Calculating Intermediate Values:
c     It is sometimes useful to calculate intermediate values and then to
c     use these in the final expressions for the output (or input)
c     variables. This may be done by supplying additional expressions for
c     the forward (or inverse) transformation functions. For instance, the
c     following array of five expressions describes 2-dimensional pin-cushion
c     distortion:
c     - "r = sqrt( xin * xin + yin * yin )"
c     - "rout = r * ( 1 + 0.1 * r * r )"
c     - "theta = atan2( yin, xin )"
c     - "xout = rout * cos( theta )"
c     - "yout = rout * sin( theta )"
f     It is sometimes useful to calculate intermediate values and then to
f     use these in the final expressions for the output (or input)
f     variables. This may be done by supplying additional expressions for
f     the forward (or inverse) transformation functions. For instance, the
f     following array of five expressions describes 2-dimensional pin-cushion
f     distortion:
f     - 'R = SQRT( XIN * XIN + YIN * YIN )'
f     - 'ROUT = R * ( 1 + 0.1 * R * R )'
f     - 'THETA = ATAN2( YIN, XIN )',
f     - 'XOUT = ROUT * COS( THETA )'
f     - 'YOUT = ROUT * SIN( THETA )'
*
c     Here, we first calculate three intermediate results ("r", "rout"
c     and "theta") and then use these to calculate the final results ("xout"
c     and "yout"). The MathMap knows that only the final two results
c     constitute values for the output variables because its Nout attribute
c     is set to 2. You may define as many intermediate variables in this
c     way as you choose. Having defined a variable, you may then refer to it
c     on the right of any subsequent expressions.
f     Here, we first calculate three intermediate results (R, ROUT
f     and THETA) and then use these to calculate the final results (XOUT
f     and YOUT). The MathMap knows that only the final two results
f     constitute values for the output variables because its Nout attribute
f     is set to 2. You may define as many intermediate variables in this
f     way as you choose. Having defined a variable, you may then refer to it
f     on the right of any subsequent expressions.
*
c     Note that when defining the inverse transformation you may only refer
c     to the output variables "xout" and "yout".  The intermediate variables
c     "r", "rout" and "theta" (above) are private to the forward
c     transformation and may not be referenced by the inverse
c     transformation. The inverse transformation may, however, define its
c     own private intermediate variables.
f     Note that when defining the inverse transformation you may only refer
f     to the output variables XOUT and YOUT. The intermediate variables R,
f     ROUT and THETA (above) are private to the forward transformation and
f     may not be referenced by the inverse transformation. The inverse
f     transformation may, however, define its own private intermediate
f     variables.

*  Expression Syntax:
c     The expressions given for the forward and inverse transformations
c     closely follow the syntax of the C programming language (with some
c     extensions for compatibility with Fortran). They may contain
c     references to variables and literal constants, together with
c     arithmetic, boolean, relational and bitwise operators, and function
c     invocations. A set of symbolic constants is also available. Each of
c     these is described in detail below. Parentheses may be used to
c     over-ride the normal order of evaluation. There is no built-in limit
c     to the length of expressions and they are insensitive to case or the
c     presence of additional white space.
f     The expressions given for the forward and inverse transformations
f     closely follow the syntax of Fortran (with some extensions for
f     compatibility with the C language). They may contain references to
f     variables and literal constants, together with arithmetic, logical,
f     relational and bitwise operators, and function invocations. A set of
f     symbolic constants is also available. Each of these is described in
f     detail below. Parentheses may be used to over-ride the normal order of
f     evaluation. There is no built-in limit to the length of expressions
f     and they are insensitive to case or the presence of additional white
f     space.

*  Variables:
*     Variable names must begin with an alphabetic character and may contain
*     only alphabetic characters, digits, and the underscore character
*     "_". There is no built-in limit to the length of variable names.

*  Literal Constants:
c     Literal constants, such as "0", "1", "0.007" or "2.505e-16" may appear
c     in expressions, with the decimal point and exponent being optional (a
c     "D" may also be used as an exponent character for compatibility with
c     Fortran). A unary minus "-" may be used as a prefix.
f     Literal constants, such as "0", "1", "0.007" or "2.505E-16" may appear
f     in expressions, with the decimal point and exponent being optional (a
f     "D" may also be used as an exponent character). A unary minus "-" may
f     be used as a prefix.

*  Arithmetic Precision:
*     All arithmetic is floating point, performed in double precision.

*  Propagation of Missing Data:
*     Unless indicated otherwise, if any argument of a function or operator
*     has the value AST__BAD (indicating missing data), then the result of
*     that function or operation is also AST__BAD, so that such values are
*     propagated automatically through all operations performed by MathMap
*     transformations.  The special value AST__BAD can be represented in
*     expressions by the symbolic constant "<bad>".
*
*     A <bad> result (i.e. equal to AST__BAD) is also produced in response
*     to any numerical error (such as division by zero or numerical
*     overflow), or if an invalid argument value is provided to a function
*     or operator.

*  Arithmetic Operators:
*     The following arithmetic operators are available:
c     - x1 + x2: Sum of "x1" and "x2".
f     - X1 + X2: Sum of X1 and X2.
c     - x1 - x2: Difference of "x1" and "x2".
f     - X1 - X2: Difference of X1 and X2.
c     - x1 * x2: Product of "x1" and "x1".
f     - X1 * X2: Product of X1 and X2.
c     - x1 / x2: Ratio of "x1" and "x2".
f     - X1 / X2: Ratio of X1 and X2.
c     - x1 ** x2: "x1" raised to the power of "x2".
f     - X1 ** X2: X1 raised to the power of X2.
c     - + x: Unary plus, has no effect on its argument.
f     - + X: Unary plus, has no effect on its argument.
c     - - x: Unary minus, negates its argument.
f     - - X: Unary minus, negates its argument.

c  Boolean Operators:
f  Logical Operators:
c     Boolean values are represented using zero to indicate false and
c     non-zero to indicate true. In addition, the value AST__BAD is taken to
c     mean "unknown". The values returned by boolean operators may therefore
c     be 0, 1 or AST__BAD. Where appropriate, "tri-state" logic is
c     implemented. For example, "a||b" may evaluate to 1 if "a" is non-zero,
c     even if "b" has the value AST__BAD. This is because the result of the
c     operation would not be affected by the value of "b", so long as "a" is
c     non-zero.
f     Logical values are represented using zero to indicate .FALSE. and
f     non-zero to indicate .TRUE.. In addition, the value AST__BAD is taken to
f     mean "unknown". The values returned by logical operators may therefore
f     be 0, 1 or AST__BAD. Where appropriate, "tri-state" logic is
f     implemented. For example, A.OR.B may evaluate to 1 if A is non-zero,
f     even if B has the value AST__BAD. This is because the result of the
f     operation would not be affected by the value of B, so long as A is
f     non-zero.
*
c     The following boolean operators are available:
f     The following logical operators are available:
c     - x1 && x2: Boolean AND between "x1" and "x2", returning 1 if both "x1"
c     and "x2" are non-zero, and 0 otherwise. This operator implements
c     tri-state logic. (The synonym ".and." is also provided for compatibility
c     with Fortran.)
f     - X1 .AND. X2: Logical AND between X1 and X2, returning 1 if both X1
f     and X2 are non-zero, and 0 otherwise. This operator implements
f     tri-state logic. (The synonym "&&" is also provided for compatibility
f     with C.)
c     - x1 || x2: Boolean OR between "x1" and "x2", returning 1 if either "x1"
c     or "x2" are non-zero, and 0 otherwise. This operator implements
c     tri-state logic. (The synonym ".or." is also provided for compatibility
c     with Fortran.)
f     - X1 .OR. X2: Logical OR between X1 and X2, returning 1 if either X1
f     or X2 are non-zero, and 0 otherwise. This operator implements
f     tri-state logic. (The synonym "||" is also provided for compatibility
f     with C.)
c     - x1 ^^ x2: Boolean exclusive OR (XOR) between "x1" and "x2", returning
c     1 if exactly one of "x1" and "x2" is non-zero, and 0 otherwise. Tri-state
c     logic is not used with this operator. (The synonyms ".neqv." and ".xor."
c     are also provided for compatibility with Fortran, although the second
c     of these is not standard.)
f     - X1 .NEQV. X2: Logical exclusive OR (XOR) between X1 and X2,
f     returning 1 if exactly one of X1 and X2 is non-zero, and 0
f     otherwise. Tri-state logic is not used with this operator. (The
f     synonym ".XOR." is also provided, although this is not standard
f     Fortran. In addition, the C-like synonym "^^" may be used, although
f     this is also not standard.)
c     - x1 .eqv. x2: This is provided only for compatibility with Fortran
c     and tests whether the boolean states of "x1" and "x2" (i.e. true/false)
c     are equal. It is the negative of the exclusive OR (XOR) function.
c     Tri-state logic is not used with this operator.
f     - X1 .EQV. X2: Tests whether the logical states of X1 and X2
f     (i.e. .TRUE./.FALSE.) are equal. It is the negative of the exclusive OR
f     (XOR) function.  Tri-state logic is not used with this operator.
c     - ! x: Boolean unary NOT operation, returning 1 if "x" is zero, and
c     0 otherwise. (The synonym ".not." is also provided for compatibility
c     with Fortran.)
f     - .NOT. X: Logical unary NOT operation, returning 1 if X is zero, and
f     0 otherwise. (The synonym "!" is also provided for compatibility with
f     C.)

*  Relational Operators:
c     Relational operators return the boolean result (0 or 1) of comparing
c     the values of two floating point values for equality or inequality. The
c     value AST__BAD may also be returned if either argument is <bad>.
f     Relational operators return the logical result (0 or 1) of comparing
f     the values of two floating point values for equality or inequality. The
f     value AST__BAD may also be returned if either argument is <bad>.
*
*     The following relational operators are available:
c     - x1 == x2: Tests whether "x1" equals "x1". (The synonym ".eq." is
c     also provided for compatibility with Fortran.)
f     - X1 .EQ. X2: Tests whether X1 equals X2. (The synonym "==" is also
f     provided for compatibility with C.)
c     - x1 != x2: Tests whether "x1" is unequal to "x2". (The synonym ".ne."
c     is also provided for compatibility with Fortran.)
f     - X1 .NE. X2: Tests whether X1 is unequal to X2. (The synonym "!=" is
f     also provided for compatibility with C.)
c     - x1 > x2: Tests whether "x1" is greater than "x2". (The synonym
c     ".gt." is also provided for compatibility with Fortran.)
f     - X1 .GT. X2: Tests whether X1 is greater than X2. (The synonym ">" is
f     also provided for compatibility with C.)
c     - x1 >= x2: Tests whether "x1" is greater than or equal to "x2". (The
c     synonym ".ge."  is also provided for compatibility with Fortran.)
f     - X1 .GE. X2: Tests whether X1 is greater than or equal to X2. (The
f     synonym ">=" is also provided for compatibility with C.)
c     - x1 < x2: Tests whether "x1" is less than "x2". (The synonym ".lt."
c     is also provided for compatibility with Fortran.)
f     - X1 .LT. X2: Tests whether X1 is less than X2. (The synonym "<" is also
f     provided for compatibility with C.)
c     - x1 <= x2: Tests whether "x1" is less than or equal to "x2". (The
c     synonym ".le." is also provided for compatibility with Fortran.)
f     - X1 .LE. X2: Tests whether X1 is less than or equal to X2. (The synonym
f     "<=" is also provided for compatibility with C.)
*
c     Note that relational operators cannot usefully be used to compare
c     values with the <bad> value (representing missing data), because the
c     result is always <bad>. The isbad() function should be used instead.
f     Note that relational operators cannot usefully be used to compare
f     values with the <bad> value (representing missing data), because the
f     result is always <bad>. The ISBAD() function should be used instead.
f
f     Note, also, that because logical operators can operate on floating
f     point values, care must be taken to use parentheses in some cases
f     where they would not normally be required in Fortran. For example,
f     the expresssion:
f     - .NOT. A .EQ. B
f
f     must be written:
f     - .NOT. ( A .EQ. B )
f
f     to prevent the .NOT. operator from associating with the variable A.

*  Bitwise Operators:
c     The bitwise operators provided by C are often useful when operating on
c     raw data (e.g. from instruments), so they are also provided for use in
c     MathMap expressions. In this case, however, the values on which they
c     operate are floating point values rather than pure integers. In order
c     to produce results which match the pure integer case, the operands are
c     regarded as fixed point binary numbers (i.e. with the binary
c     equivalent of a decimal point) with negative numbers represented using
c     twos-complement notation. For integer values, the resulting bit
c     pattern corresponds to that of the equivalent signed integer (digits
c     to the right of the point being zero). Operations on the bits
c     representing the fractional part are also possible, however.
f     Bitwise operators are often useful when operating on raw data
f     (e.g. from instruments), so they are provided for use in MathMap
f     expressions. In this case, however, the values on which they operate
f     are floating point values rather than the more usual pure integers. In
f     order to produce results which match the pure integer case, the
f     operands are regarded as fixed point binary numbers (i.e. with the
f     binary equivalent of a decimal point) with negative numbers
f     represented using twos-complement notation. For integer values, the
f     resulting bit pattern corresponds to that of the equivalent signed
f     integer (digits to the right of the point being zero). Operations on
f     the bits representing the fractional part are also possible, however.
*
*     The following bitwise operators are available:
c     - x1 >> x2: Rightward bit shift. The integer value of "x2" is taken
c     (rounding towards zero) and the bits representing "x1" are then
c     shifted this number of places to the right (or to the left if the
c     number of places is negative). This is equivalent to dividing "x1" by
c     the corresponding power of 2.
f     - X1 >> X2: Rightward bit shift. The integer value of X2 is taken
f     (rounding towards zero) and the bits representing X1 are then
f     shifted this number of places to the right (or to the left if the
f     number of places is negative). This is equivalent to dividing X1 by
f     the corresponding power of 2.
c     - x1 << x2: Leftward bit shift. The integer value of "x2" is taken
c     (rounding towards zero), and the bits representing "x1" are then
c     shifted this number of places to the left (or to the right if the
c     number of places is negative). This is equivalent to multiplying "x1"
c     by the corresponding power of 2.
f     - X1 << X2: Leftward bit shift. The integer value of X2 is taken
f     (rounding towards zero), and the bits representing X1 are then
f     shifted this number of places to the left (or to the right if the
f     number of places is negative). This is equivalent to multiplying X1
f     by the corresponding power of 2.
c     - x1 & x2: Bitwise AND between the bits of "x1" and those of "x2"
c     (equivalent to a boolean AND applied at each bit position in turn).
f     - X1 & X2: Bitwise AND between the bits of X1 and those of X2
f     (equivalent to a logical AND applied at each bit position in turn).
c     - x1 | x2: Bitwise OR between the bits of "x1" and those of "x2"
c     (equivalent to a boolean OR applied at each bit position in turn).
f     - X1 | X2: Bitwise OR between the bits of X1 and those of X2
f     (equivalent to a logical OR applied at each bit position in turn).
c     - x1 ^ x2: Bitwise exclusive OR (XOR) between the bits of "x1" and
c     those of "x2" (equivalent to a boolean XOR applied at each bit
c     position in turn).
f     - X1 ^ X2: Bitwise exclusive OR (XOR) between the bits of X1 and
f     those of X2 (equivalent to a logical XOR applied at each bit
f     position in turn).
*
c     Note that no bit inversion operator ("~" in C) is provided. This is
c     because inverting the bits of a twos-complement fixed point binary
c     number is equivalent to simply negating it. This differs from the
c     pure integer case because bits to the right of the binary point are
c     also inverted. To invert only those bits to the left of the binary
c     point, use a bitwise exclusive OR with the value -1 (i.e. "x^-1").
f     Note that no bit inversion operator is provided. This is
f     because inverting the bits of a twos-complement fixed point binary
f     number is equivalent to simply negating it. This differs from the
f     pure integer case because bits to the right of the binary point are
f     also inverted. To invert only those bits to the left of the binary
f     point, use a bitwise exclusive OR with the value -1 (i.e. X^-1).

*  Functions:
*     The following functions are available:
c     - abs(x): Absolute value of "x" (sign removal), same as fabs(x).
f     - ABS(X): Absolute value of X (sign removal), same as FABS(X).
c     - acos(x): Inverse cosine of "x", in radians.
f     - ACOS(X): Inverse cosine of X, in radians.
c     - acosd(x): Inverse cosine of "x", in degrees.
f     - ACOSD(X): Inverse cosine of X, in degrees.
c     - acosh(x): Inverse hyperbolic cosine of "x".
f     - ACOSH(X): Inverse hyperbolic cosine of X.
c     - acoth(x): Inverse hyperbolic cotangent of "x".
f     - ACOTH(X): Inverse hyperbolic cotangent of X.
c     - acsch(x): Inverse hyperbolic cosecant of "x".
f     - ACSCH(X): Inverse hyperbolic cosecant of X.
c     - aint(x): Integer part of "x" (round towards zero), same as int(x).
f     - AINT(X): Integer part of X (round towards zero), same as INT(X).
c     - asech(x): Inverse hyperbolic secant of "x".
f     - ASECH(X): Inverse hyperbolic secant of X.
c     - asin(x): Inverse sine of "x", in radians.
f     - ASIN(X): Inverse sine of X, in radians.
c     - asind(x): Inverse sine of "x", in degrees.
f     - ASIND(X): Inverse sine of X, in degrees.
c     - asinh(x): Inverse hyperbolic sine of "x".
f     - ASINH(X): Inverse hyperbolic sine of X.
c     - atan(x): Inverse tangent of "x", in radians.
f     - ATAN(X): Inverse tangent of X, in radians.
c     - atand(x): Inverse tangent of "x", in degrees.
f     - ATAND(X): Inverse tangent of X, in degrees.
c     - atanh(x): Inverse hyperbolic tangent of "x".
f     - ATANH(X): Inverse hyperbolic tangent of X.
c     - atan2(x1, x2): Inverse tangent of "x1/x2", in radians.
f     - ATAN2(X1, X2): Inverse tangent of X1/X2, in radians.
c     - atan2d(x1, x2): Inverse tangent of "x1/x2", in degrees.
f     - ATAN2D(X1, X2): Inverse tangent of X1/X2, in degrees.
c     - ceil(x): Smallest integer value not less then "x" (round towards
c       plus infinity).
f     - CEIL(X): Smallest integer value not less then X (round towards
f       plus infinity).
c     - cos(x): Cosine of "x" in radians.
f     - COS(X): Cosine of X in radians.
c     - cosd(x): Cosine of "x" in degrees.
f     - COSD(X): Cosine of X in degrees.
c     - cosh(x): Hyperbolic cosine of "x".
f     - COSH(X): Hyperbolic cosine of X.
c     - coth(x): Hyperbolic cotangent of "x".
f     - COTH(X): Hyperbolic cotangent of X.
c     - csch(x): Hyperbolic cosecant of "x".
f     - CSCH(X): Hyperbolic cosecant of X.
c     - dim(x1, x2): Returns "x1-x2" if "x1" is greater than "x2", otherwise 0.
f     - DIM(X1, X2): Returns X1-X2 if X1 is greater than X2, otherwise 0.
c     - exp(x): Exponential function of "x".
f     - EXP(X): Exponential function of X.
c     - fabs(x): Absolute value of "x" (sign removal), same as abs(x).
f     - FABS(X): Absolute value of X (sign removal), same as ABS(X).
c     - floor(x): Largest integer not greater than "x" (round towards
c       minus infinity).
f     - FLOOR(X): Largest integer not greater than X (round towards
f       minus infinity).
c     - fmod(x1, x2): Remainder when "x1" is divided by "x2", same as
c       mod(x1, x2).
f     - FMOD(X1, X2): Remainder when X1 is divided by X2, same as
f       MOD(X1, X2).
c     - gauss(x1, x2): Random sample from a Gaussian distribution with mean
c       "x1" and standard deviation "x2".
f     - GAUSS(X1, X2): Random sample from a Gaussian distribution with mean
f       X1 and standard deviation X2.
c     - int(x): Integer part of "x" (round towards zero), same as aint(x).
f     - INT(X): Integer part of X (round towards zero), same as AINT(X).
c     - isbad(x): Returns 1 if "x" has the <bad> value (AST__BAD), otherwise 0.
f     - ISBAD(X): Returns 1 if X has the <bad> value (AST__BAD), otherwise 0.
c     - log(x): Natural logarithm of "x".
f     - LOG(X): Natural logarithm of X.
c     - log10(x): Logarithm of "x" to base 10.
f     - LOG10(X): Logarithm of X to base 10.
c     - max(x1, x2, ...): Maximum of two or more values.
f     - MAX(X1, X2, ...): Maximum of two or more values.
c     - min(x1, x2, ...): Minimum of two or more values.
f     - MIN(X1, X2, ...): Minimum of two or more values.
c     - mod(x1, x2): Remainder when "x1" is divided by "x2", same as
c       fmod(x1, x2).
f     - MOD(X1, X2): Remainder when X1 is divided by X2, same as
f       FMOD(X1, X2).
c     - nint(x): Nearest integer to "x" (round to nearest).
f     - NINT(X): Nearest integer to X (round to nearest).
c     - poisson(x): Random integer-valued sample from a Poisson
c       distribution with mean "x".
f     - POISSON(X): Random integer-valued sample from a Poisson
f       distribution with mean X.
c     - pow(x1, x2): "x1" raised to the power of "x2".
f     - POW(X1, X2): X1 raised to the power of X2.
c     - qif(x1, x2, x3): Returns "x2" if "x1" is true, and "x3" otherwise.
f     - QIF(x1, x2, x3): Returns X2 if X1 is true, and X3 otherwise.
c     - rand(x1, x2): Random sample from a uniform distribution in the
c       range "x1" to "x2" inclusive.
f     - RAND(X1, X2): Random sample from a uniform distribution in the
f       range X1 to X2 inclusive.
c     - sech(x): Hyperbolic secant of "x".
f     - SECH(X): Hyperbolic secant of X.
c     - sign(x1, x2): Absolute value of "x1" with the sign of "x2"
c       (transfer of sign).
f     - SIGN(X1, X2): Absolute value of X1 with the sign of X2
f       (transfer of sign).
c     - sin(x): Sine of "x" in radians.
f     - SIN(X): Sine of X in radians.
c     - sinc(x): Sinc function of "x" [= "sin(x)/x"].
f     - SINC(X): Sinc function of X [= SIN(X)/X].
c     - sind(x): Sine of "x" in degrees.
f     - SIND(X): Sine of X in degrees.
c     - sinh(x): Hyperbolic sine of "x".
f     - SINH(X): Hyperbolic sine of X.
c     - sqr(x): Square of "x" (= "x*x").
f     - SQR(X): Square of X (= X*X).
c     - sqrt(x): Square root of "x".
f     - SQRT(X): Square root of X.
c     - tan(x): Tangent of "x" in radians.
f     - TAN(X): Tangent of X in radians.
c     - tand(x): Tangent of "x" in degrees.
f     - TAND(X): Tangent of X in degrees.
c     - tanh(x): Hyperbolic tangent of "x".
f     - TANH(X): Hyperbolic tangent of X.

*  Symbolic Constants:
*     The following symbolic constants are available (the enclosing "<>"
*     brackets must be included):
c     - <bad>: The "bad" value (AST__BAD) used to flag missing data. Note
c     that you cannot usefully compare values with this constant because the
c     result is always <bad>. The isbad() function should be used instead.
f     - <bad>: The "bad" value (AST__BAD) used to flag missing data. Note
f     that you cannot usefully compare values with this constant because the
f     result is always <bad>. The ISBAD() function should be used instead.
c     - <dig>: Number of decimal digits of precision available in a
c     floating point (double) value.
f     - <dig>: Number of decimal digits of precision available in a
f     floating point (double precision) value.
*     - <e>: Base of natural logarithms.
*     - <epsilon>: Smallest positive number such that 1.0+<epsilon> is
*     distinguishable from unity.
c     - <mant_dig>: The number of base <radix> digits stored in the
c     mantissa of a floating point (double) value.
f     - <mant_dig>: The number of base <radix> digits stored in the
f     mantissa of a floating point (double precision) value.
c     - <max>: Maximum representable floating point (double) value.
f     - <max>: Maximum representable floating point (double precision) value.
c     - <max_10_exp>: Maximum integer such that 10 raised to that power
c     can be represented as a floating point (double) value.
f     - <max_10_exp>: Maximum integer such that 10 raised to that power
f     can be represented as a floating point (double precision) value.
c     - <max_exp>: Maximum integer such that <radix> raised to that
c     power minus 1 can be represented as a floating point (double) value.
f     - <max_exp>: Maximum integer such that <radix> raised to that
f     power minus 1 can be represented as a floating point (double precision)
f     value.
c     - <min>: Smallest positive number which can be represented as a
c     normalised floating point (double) value.
f     - <min>: Smallest positive number which can be represented as a
f     normalised floating point (double precision) value.
c     - <min_10_exp>: Minimum negative integer such that 10 raised to that
c     power can be represented as a normalised floating point (double) value.
f     - <min_10_exp>: Minimum negative integer such that 10 raised to that
f     power can be represented as a normalised floating point (double
f     precision) value.
c     - <min_exp>: Minimum negative integer such that <radix> raised to
c     that power minus 1 can be represented as a normalised floating point
c     (double) value.
f     - <min_exp>: Minimum negative integer such that <radix> raised to
f     that power minus 1 can be represented as a normalised floating point
f     (double precision) value.
*     - <pi>: Ratio of the circumference of a circle to its diameter.
c     - <radix>: The radix (number base) used to represent the mantissa of
c     floating point (double) values.
f     - <radix>: The radix (number base) used to represent the mantissa of
f     floating point (double precision) values.
*     - <rounds>: The mode used for rounding floating point results after
*     addition. Possible values include: -1 (indeterminate), 0 (toward
*     zero), 1 (to nearest), 2 (toward plus infinity) and 3 (toward minus
*     infinity). Other values indicate machine-dependent behaviour.

*  Evaluation Precedence and Associativity:
*     Items appearing in expressions are evaluated in the following order
*     (highest precedence first):
*     - Constants and variables
*     - Function arguments and parenthesised expressions
*     - Function invocations
*     - Unary + - ! .not.
*     - **
*     - * /
*     - + -
*     - << >>
*     - < .lt. <= .le. > .gt. >= .ge.
*     - == .eq. != .ne.
*     - &
*     - ^
*     - |
*     - && .and.
*     - ^^
*     - || .or
*     - .eqv. .neqv. .xor.
*
*     All operators associate from left-to-right, except for unary +,
*     unary -, !, .not. and ** which associate from right-to-left.

*  Notes:
*     - The sequence of numbers produced by the random number functions
*     available within a MathMap is normally unpredictable and different for
*     each MathMap. However, this behaviour may be controlled by means of
*     the MathMap's Seed attribute.
c     - Normally, compound Mappings (CmpMaps) which involve MathMaps will
c     not be subject to simplification (e.g. using astSimplify) because AST
c     cannot know how different MathMaps will interact. However, in the
c     special case where a MathMap occurs in series with its own inverse,
c     then simplification may be possible. Whether simplification does, in
c     fact, occur under these circumstances is controlled by the MathMap's
c     SimpFI and SimpIF attributes.
f     - Normally, compound Mappings (CmpMaps) which involve MathMaps will
f     not be subject to simplification (e.g. using AST_SIMPLIFY) because AST
f     cannot know how different MathMaps will interact. However, in the
f     special case where a MathMap occurs in series with its own inverse,
f     then simplification may be possible. Whether simplification does, in
f     fact, occur under these circumstances is controlled by the MathMap's
f     SimpFI and SimpIF attributes.
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--

*  Implementation Notes:
*     - This function implements the external (public) interface to
*     the astMathMap constructor function. It returns an ID value
*     (instead of a true C pointer) to external users, and must be
*     provided because astMathMap_ has a variable argument list which
*     cannot be encapsulated in a macro (where this conversion would
*     otherwise occur).
*     - The variable argument list also prevents this function from
*     invoking astMathMap_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstMathMap *new;              /* Pointer to new MathMap */
   va_list args;                 /* Variable argument list */

   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise the MathMap, allocating memory and initialising the virtual
   function table as well if necessary. */
   new = astInitMathMap( NULL, sizeof( AstMathMap ), !class_init, &class_vtab,
                         "MathMap", nin, nout, nfwd, fwd, ninv, inv );

/* If successful, note that the virtual function table has been initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new MathMap's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new MathMap. */
   return astMakeId( new );
}

AstMathMap *astInitMathMap_( void *mem, size_t size, int init,
                             AstMathMapVtab *vtab, const char *name,
                             int nin, int nout,
                             int nfwd, const char *fwd[],
                             int ninv, const char *inv[], int *status ) {
/*
*+
*  Name:
*     astInitMathMap

*  Purpose:
*     Initialise a MathMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "mathmap.h"
*     AstMathMap *astInitMathMap_( void *mem, size_t size, int init,
*                                  AstMathMapVtab *vtab, const char *name,
*                                  int nin, int nout,
*                                  int nfwd, const char *fwd[],
*                                  int ninv, const char *inv[] )

*  Class Membership:
*     MathMap initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new MathMap object. It allocates memory (if necessary) to accommodate
*     the MathMap plus any additional data associated with the derived class.
*     It then initialises a MathMap structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a MathMap at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the MathMap is to be initialised.
*        This must be of sufficient size to accommodate the MathMap data
*        (sizeof(MathMap)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the MathMap (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the MathMap
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the MathMap's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new MathMap.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the Object
*        astClass function).
*     nin
*        Number of input variables for the MathMap.
*     nout
*        Number of output variables for the MathMap.
*     nfwd
*        The number of forward transformation functions being supplied.
*        This must be at least equal to "nout".
*     fwd
*        Pointer to an array, with "nfwd" elements, of pointers to null
*        terminated strings which contain each of the forward transformation
*        functions.
*     ninv
*        The number of inverse transformation functions being supplied.
*        This must be at least equal to "nin".
*     inv
*        Pointer to an array, with "ninv" elements, of pointers to null
*        terminated strings which contain each of the inverse transformation
*        functions.

*  Returned Value:
*     A pointer to the new MathMap.

*  Notes:
*     -  This function does not attempt to ensure that the forward and inverse
*     transformations performed by the resulting MathMap are consistent in any
*     way.
*     -  This function makes a copy of the contents of the strings supplied.
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstMathMap *new;              /* Pointer to new MathMap */
   char **fwdfun;                /* Array of cleaned forward functions */
   char **invfun;                /* Array of cleaned inverse functions */
   double **fwdcon;              /* Constants for forward functions */
   double **invcon;              /* Constants for inverse functions */
   int **fwdcode;                /* Code for forward functions */
   int **invcode;                /* Code for inverse functions */
   int fwdstack;                 /* Stack size for forward functions */
   int invstack;                 /* Stack size for inverse functions */

/* Initialise. */
   new = NULL;

/* Check the global status. */
   if ( !astOK ) return new;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitMathMapVtab( vtab, name );

/* Check the numbers of input and output variables for validity,
   reporting an error if necessary. */
   if ( nin < 1 ) {
      astError( AST__BADNI,
                "astInitMathMap(%s): Bad number of input coordinates (%d).", status,
                name, nin );
      astError( AST__BADNI,
                "This number should be one or more." , status);
   } else if ( nout < 1 ) {
      astError( AST__BADNO,
                "astInitMathMap(%s): Bad number of output coordinates (%d).", status,
                name, nout );
      astError( AST__BADNI,
                "This number should be one or more." , status);

/* Check that sufficient number of forward and inverse transformation
   functions have been supplied and report an error if necessary. */
   } else if ( nfwd < nout ) {
      astError( AST__INNTF,
                "astInitMathMap(%s): Too few forward transformation functions "
                "given (%d).", status,
                name, nfwd );
      astError( astStatus,
                "At least %d forward transformation functions must be "
                "supplied. ", status,
                nout );
   } else if ( ninv < nin ) {
      astError( AST__INNTF,
                "astInitMathMap(%s): Too few inverse transformation functions "
                "given (%d).", status,
                name, ninv );
      astError( astStatus,
                "At least %d inverse transformation functions must be "
                "supplied. ", status,
                nin );

/* Of OK, clean the forward and inverse functions provided. This makes
   a lower-case copy with white space removed. */
   } else {
      CleanFunctions( nfwd, fwd, &fwdfun, status );
      CleanFunctions( ninv, inv, &invfun, status );

/* Compile the cleaned functions. From the returned pointers (if
   successful), we can now tell which transformations (forward and/or
   inverse) are defined. */
      CompileMapping( "astInitMathMap", name, nin, nout,
                      nfwd, (const char **) fwdfun,
                      ninv, (const char **) invfun,
                      &fwdcode, &invcode, &fwdcon, &invcon,
                      &fwdstack, &invstack, status );

/* Initialise a Mapping structure (the parent class) as the first
   component within the MathMap structure, allocating memory if
   necessary. Specify that the Mapping should be defined in the required
   directions. */
      new = (AstMathMap *) astInitMapping( mem, size, 0,
                                           (AstMappingVtab *) vtab, name,
                                           nin, nout,
                                           ( fwdcode != NULL ),
                                           ( invcode != NULL ) );


/* If an error has occurred, free all the memory which may have been
   allocated by the cleaning and compilation steps above. */
      if ( !astOK ) {
         FREE_POINTER_ARRAY( fwdfun, nfwd )
         FREE_POINTER_ARRAY( invfun, ninv )
         FREE_POINTER_ARRAY( fwdcode, nfwd )
         FREE_POINTER_ARRAY( invcode, ninv )
         FREE_POINTER_ARRAY( fwdcon, nfwd )
         FREE_POINTER_ARRAY( invcon, ninv )
      }

/* Initialise the MathMap data. */
/* ---------------------------- */
/* Store pointers to the compiled function information, together with
   other MathMap data. */
      if ( new ) {
         new->fwdfun = fwdfun;
         new->invfun = invfun;
         new->fwdcode = fwdcode;
         new->invcode = invcode;
         new->fwdcon = fwdcon;
         new->invcon = invcon;
         new->fwdstack = fwdstack;
         new->invstack = invstack;
         new->nfwd = nfwd;
         new->ninv = ninv;
         new->simp_fi = -INT_MAX;
         new->simp_if = -INT_MAX;

/* Initialise the random number generator context associated with the
   MathMap, using an unpredictable default seed value. */
         new->rcontext.active = 0;
         new->rcontext.random_int = 0;
         new->rcontext.seed_set = 0;
         new->rcontext.seed = DefaultSeed( &new->rcontext, status );

/* If an error occurred, clean up by deleting the new object. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Return a pointer to the new object. */
   return new;
}

AstMathMap *astLoadMathMap_( void *mem, size_t size,
                             AstMathMapVtab *vtab, const char *name,
                             AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadMathMap

*  Purpose:
*     Load a MathMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "mathmap.h"
*     AstMathMap *astLoadMathMap( void *mem, size_t size,
*                                 AstMathMapVtab *vtab, const char *name,
*                                 AstChannel *channel )

*  Class Membership:
*     MathMap loader.

*  Description:
*     This function is provided to load a new MathMap using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     MathMap structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a MathMap at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the MathMap is to be
*        loaded.  This must be of sufficient size to accommodate the
*        MathMap data (sizeof(MathMap)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the MathMap (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the MathMap structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstMathMap) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new MathMap. If this is NULL, a pointer
*        to the (static) virtual function table for the MathMap class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "MathMap" is used instead.

*  Returned Value:
*     A pointer to the new MathMap.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Constants: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
#define KEY_LEN 50               /* Maximum length of a keyword */

/* Local Variables: */
   AstMathMap *new;              /* Pointer to the new MathMap */
   char key[ KEY_LEN + 1 ];      /* Buffer for keyword strings */
   int ifun;                     /* Loop counter for functions */
   int invert;                   /* Invert attribute value */
   int nin;                      /* True number of input coordinates */
   int nout;                     /* True number of output coordinates */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this MathMap. In this case the
   MathMap belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstMathMap );
      vtab = &class_vtab;
      name = "MathMap";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitMathMapVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built MathMap. */
   new = astLoadMapping( mem, size, (AstMappingVtab *) vtab, name,
                         channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "MathMap" );

/* Determine if the MathMap is inverted and obtain the "true" number
   of input and output coordinates by un-doing the effects of any
   inversion. */
      invert = astGetInvert( new );
      nin = invert ? astGetNout( new ) : astGetNin( new );
      nout = invert ? astGetNin( new ) : astGetNout( new );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* Numbers of transformation functions. */
/* ------------------------------------ */
/* Read the numbers of forward and inverse transformation functions,
   supplying appropriate defaults. */
      new->nfwd = astReadInt( channel, "nfwd", nout );
      new->ninv = astReadInt( channel, "ninv", nin );
      if ( astOK ) {

/* Allocate memory for the MathMap's transformation function arrays. */
         MALLOC_POINTER_ARRAY( new->fwdfun, char *, new->nfwd )
         MALLOC_POINTER_ARRAY( new->invfun, char *, new->ninv )
         if ( astOK ) {

/* Forward transformation functions. */
/* --------------------------------- */
/* Create a keyword for each forward transformation function and read
   the function's value as a string. */
            for ( ifun = 0; ifun < new->nfwd; ifun++ ) {
               (void) sprintf( key, "fwd%d", ifun + 1 );
               new->fwdfun[ ifun ] = astReadString( channel, key, "" );
            }

/* Inverse transformation functions. */
/* --------------------------------- */
/* Repeat this process for the inverse transformation functions. */
            for ( ifun = 0; ifun < new->ninv; ifun++ ) {
               (void) sprintf( key, "inv%d", ifun + 1 );
               new->invfun[ ifun ] = astReadString( channel, key, "" );
            }

/* Forward-inverse simplification flag. */
/* ------------------------------------ */
            new->simp_fi = astReadInt( channel, "simpfi", -INT_MAX );
            if ( TestSimpFI( new, status ) ) SetSimpFI( new, new->simp_fi, status );

/* Inverse-forward simplification flag. */
/* ------------------------------------ */
            new->simp_if = astReadInt( channel, "simpif", -INT_MAX );
            if ( TestSimpIF( new, status ) ) SetSimpIF( new, new->simp_if, status );

/* Random number context. */
/* ---------------------- */
/* Initialise the random number generator context. */
            new->rcontext.active = 0;
            new->rcontext.random_int = 0;

/* Read the flag that determines if the Seed value is set, and the
   Seed value itself. */
            new->rcontext.seed_set = astReadInt( channel, "seeded", 0 );
            if ( TestSeed( new, status ) ) {
               new->rcontext.seed = astReadInt( channel, "seed", 0 );
               SetSeed( new, new->rcontext.seed, status );

/* Supply an unpredictable default Seed value if necessary. */
            } else {
               new->rcontext.seed = DefaultSeed( &new->rcontext, status );
            }

/* Compile the MathMap's transformation functions. */
            CompileMapping( "astLoadMathMap", name, nin, nout,
                            new->nfwd, (const char **) new->fwdfun,
                            new->ninv, (const char **) new->invfun,
                            &new->fwdcode, &new->invcode,
                            &new->fwdcon, &new->invcon,
                            &new->fwdstack, &new->invstack, status );
         }

/* If an error occurred, clean up by deleting the new MathMap. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Return the new MathMap pointer. */
   return new;

/* Undefine macros local to this function. */
#undef KEY_LEN
}





