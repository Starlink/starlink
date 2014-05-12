/*
*  Name:
*     unit.c

*  Purpose:
*     Implement unit conversion functions.

*  Description:
*     This file implements the Unit module which is used for identifying
*     units and transforming between them. It follows the recommendations
*     for unit handling contained within FITS WCS paper I (Greisen &
*     Calabretta). All methods have protected access.

*  Methods:
*     astUnitMapper: Create a Mapping between two systems of units.
*     astUnitLabel: Returns a label for a given unit symbol.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*     
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*     
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Authors:
*     DSB: D.S. Berry (Starlink)

*  History:
*     10-DEC-2002 (DSB):
*        Original version.
*     10-FEB-2004 (DSB):
*        Added debug conditional code to keep track of memory leaks.
*     15-JUL-2004 (DSB):
*        In astUnitMapper: if no Mapping can be found from input to
*        output units (e.g. because fo the less than perfect simplication
*        algorithm in SimplifyTree), try finding a Mapping from output to
*        input units and inverting the result.
*     14-DEC-2004 (DSB):
*        In CreateTree, move the invocation of FixConstants from after
*        InvertConstants to before InvertConstants. This is because
*        InvertConstants ignores nodes which contain all constant
*        arguments. This results in constants not being inverted in
*        expressions such as "1/60 deg" (because all arguments are
*        constant in the the "1/60" node).
*     18-JAN-2005 (DSB):
*        Fix memory leaks.
*     2-FEB-2005 (DSB):
*        - Avoid using astStore to allocate more storage than is supplied
*        in the "data" pointer. This can cause access violations since
*        astStore will then read beyond the end of the "data" area.
*     15-FEB-2005 (DSB):
*        - Modified CleanExp to fix up some common units mistakes.
*     21-FEB-2005 (DSB):
*        - Modified CleanExp to accept <word><digit> as equivalent to
*        <word>^<digit>.
*        - Modified MakeTree to do case insensitive checking if case
*        sensitive checking failsto produce a match to a multiplier/unit
*        symbol combination. If this still produces no match, do a case
*        insensitive match for multiplier/unit label.
*     1-MAR-2006 (DSB):
*        Replace astSetPermMap within DEBUG blocks by astBeginPM/astEndPM.
*     6-APR-2006 (DSB):
*        Modify CleanExp to convert "MJY/STER" to standard form ("MJy/sr").
*     7-JUL-2006 (DSB):
*        Correct initialisation of "word" flag in CleanExp.
*     17-MAY-2007 (DSB):
*        Simplify the units string returned by astUnitNormaliser.
*        - Fix indexing bug in CombineFactors.
*     26-MAY-2007 (DSB):
*        - Correct error reporting in astUNitNormaliser.
*        - Fix bug in CleanExp that caused (for example) "-2" to be
*        converted to "^-2".
*     2-DEC-2008 (DSB):
*        Correct memory allocation bug in CleanExp.
*     6-MAY-2011 (DSB):
*        Include "adu" as basic unit.
*     9-MAY-2011 (DSB):
*        Change "A" to be Ampere (as defined by FITS-WCS paper 1) rather
*        than "Angstrom".
*/

/* Module Macros. */
/* ============== */
/* Define the astCLASS macro (even although this is not a class
   implementation) to obtain access to the protected error and memory
   handling functions. */
#define astCLASS
#define PI 3.141592653589793
#define E 2.718281828459045

/* Macro which returns the nearest integer to a given floating point
   value. */
#define NINT(x) (int)((x)+(((x)>0.0)?0.5:-0.5))

/* Macros which return the maximum and minimum of two values. */
#define MAX(aa,bb) ((aa)>(bb)?(aa):(bb))
#define MIN(aa,bb) ((aa)<(bb)?(aa):(bb))

/* Macro to check for equality of floating point values. We cannot
   compare bad values directory because of the danger of floating point
   exceptions, so bad values are dealt with explicitly. */
#define EQUAL(aa,bb) (((aa)==AST__BAD)?(((bb)==AST__BAD)?1:0):(((bb)==AST__BAD)?0:(fabs((aa)-(bb))<=1.0E5*MAX((fabs(aa)+fabs(bb))*DBL_EPSILON,DBL_MIN))))

/* Macro identifying a character as lower or upper case letter, digit or
+ or -. */
#define ISWORD(c) (isalnum(c)||((c)=='+')||((c)=='-'))

/* The number of basic dimension quantities used for dimensional analysis.
   In addition to the usual M, L and T, this includes pseudo-dimensions
   describing strictly dimensionless quantities such as plane angle,
   magnitude, etc. */
#define NQUANT 10

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "error.h"
#include "memory.h"
#include "pointset.h"
#include "mapping.h"
#include "unitmap.h"
#include "zoommap.h"
#include "mathmap.h"
#include "unit.h"

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>

#ifdef THREAD_SAFE
#include <pthread.h>
#endif

/* Module Type Definitions. */
/* ======================== */

/* This declaration enumerates the codes for the operations which may
   legally be included in a units expression. */
typedef enum {
   OP_LDCON,                     /* Load constant */
   OP_LDVAR,                     /* Load variable */
   OP_LOG,                       /* Common logarithm */
   OP_LN,                        /* Natural logarithm */
   OP_EXP,                       /* Natural exponential */
   OP_SQRT,                      /* Square root */
   OP_POW,                       /* Exponentiate */
   OP_DIV,                       /* Division */
   OP_MULT,                      /* Multiplication */
   OP_LDPI,                      /* Load constant PI */
   OP_LDE,                       /* Load constant E */
   OP_NULL                       /* Null operation */
} Oper;

/* A structure describing a standard multiplier prefix. */
typedef struct Multiplier {
   const char *label;       /* Multipler label string (null terminated) */
   const char *sym;         /* Multipler symbol string (null terminated) */
   int symlen;              /* Length of symbol (without trailing null ) */
   int lablen;              /* Length of label (without trailing null ) */
   double scale;            /* The scale factor associated with the prefix */
   struct Multiplier *next; /* Next Multiplier in linked list */
} Multiplier;

/* A structure describing a single node in a tree representation of a
   single units expression. */
typedef struct UnitNode {
   Oper opcode;             /* Code for operation performed by this node */
   int narg;                /* No. of arguments used by the operation */
   struct UnitNode **arg;   /* Array of pointers to argument nodes */
   double con;              /* Constant to be loaded by OP_LDCON operations */
   struct KnownUnit *unit;  /* Known unit referred to by OP_LDVAR nodes */
   Multiplier *mult;        /* Multiplier used by OP_LDVAR nodes */
   const char *name;        /* User-defined unit referred to by OP_LDVAR
                               nodes (no multiplier prefix included) */
} UnitNode;

/* A structure describing a known unit. */
typedef struct KnownUnit {
   const char *sym;         /* Unit symbol string (null terminated) */
   const char *label;       /* Unit label string (null terminated) */
   int symlen;              /* Length of symbol (without trailing null ) */
   int lablen;              /* Length of label (without trailing null ) */
   struct UnitNode *head;   /* Head of definition tree (NULL for basic units) */
   struct KnownUnit *next;  /* Next KnownUnit in linked list */
   struct KnownUnit *use;   /* KnownUnit to be used in place of this one */
} KnownUnit;

/* Module Variables. */
/* ================= */

/* A pointer to the KnownUnit structure at the head of a linked list of
   such structures containing definitions of all known units. */
static KnownUnit *known_units = NULL;

/* An array of pointers to KnownUnits which list the basic quantities
used in dimensional analysis. */
static KnownUnit *quant_units[ NQUANT ];

/* A pointer to the Multiplier structure at the head of a linked list of
   such structures containing definitions of all known multipliers. */
static Multiplier *multipliers = NULL;

/* Set up mutexes */
#ifdef THREAD_SAFE

static pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
#define LOCK_MUTEX1 pthread_mutex_lock( &mutex1 );
#define UNLOCK_MUTEX1 pthread_mutex_unlock( &mutex1 );

static pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;
#define LOCK_MUTEX2 pthread_mutex_lock( &mutex2 );
#define UNLOCK_MUTEX2 pthread_mutex_unlock( &mutex2 );

#else

#define LOCK_MUTEX1
#define UNLOCK_MUTEX1

#define LOCK_MUTEX2
#define UNLOCK_MUTEX2

#endif

/* Prototypes for Private Functions. */
/* ================================= */
static AstMapping *MakeMapping( UnitNode *, int * );
static KnownUnit *GetKnownUnits( int, int * );
static Multiplier *GetMultipliers( int * );
static UnitNode *ConcatTree( UnitNode *, UnitNode *, int * );
static UnitNode *CopyTree( UnitNode *, int * );
static UnitNode *CreateTree( const char *, int, int, int * );
static UnitNode *FixUnits( UnitNode *, UnitNode *, int * );
static UnitNode *FreeTree( UnitNode *, int * );
static UnitNode *MakeTree( const char *, int, int, int * );
static UnitNode *MakeLabelTree( const char *, int, int * );
static UnitNode *NewNode( UnitNode *, Oper, int * );
static UnitNode *CombineFactors( UnitNode **, double *, int, double, int * );
static const char *CleanExp( const char *, int * );
static int EndsWith( const char *, int, const char *, int * );
static int CmpTree( UnitNode *, UnitNode *, int, int * );
static void FixConstants( UnitNode **, int, int * );
static void InvertConstants( UnitNode **, int * );
static UnitNode *InvertTree( UnitNode *, UnitNode *, int * );
static void LocateUnits( UnitNode *, UnitNode ***, int *, int * );
static void MakeKnownUnit( const char *, const char *, const char *, int * );
static void MakeUnitAlias( const char *, const char *, int * );
static void RemakeTree( UnitNode **, int * );
static int SimplifyTree( UnitNode **, int, int * );
static int ComplicateTree( UnitNode **, int * );
static int ReplaceNode( UnitNode *, UnitNode *, UnitNode *, int * );
static void FindFactors( UnitNode *, UnitNode ***, double **, int *, double *, int * );
static const char *MakeExp( UnitNode *, int, int, int * );
static int DimAnal( UnitNode *, double[NQUANT], double *, int * );
static int Ustrcmp( const char *, const char *, int * );
static int Ustrncmp( const char *, const char *, size_t, int * );
static int SplitUnit( const char *, int, const char *, int, Multiplier **, int *, int * );
static UnitNode *ModifyPrefix( UnitNode *, int * );
static int ConStart( const char *, double *, int *, int * );

/*  Debug functions...
static const char *DisplayTree( UnitNode *, int );
static void OpSym( UnitNode *, char * );
static const char *OpName( Oper );
static const char *TreeExp( UnitNode * );
*/

/* Function implementations. */
/* ========================= */
static const char *CleanExp( const char *exp, int *status ) {
/*
*  Name:
*     CleanExp

*  Purpose:
*     Produce a clean copy of a units expression.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     const char *CleanExp( const char *exp )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function returns a pointer to dynamic memory containing a
*     cleaned copy of the supplied units expression. Cleaning consists of
*     the following operations:
*        - removal of leading and trailing white space.
*        - replacement of multiple adjacent spaces by a single space
*        - removal of spaces adjacent to a parenthesis
*        - removal of spaces adjacent to a binary operator
*        - translates various common non-standard units into equivalent
*          standard units.
*
*     Such carefull handling of spaces is necessary since a space is
*     recognised by the MakeTree function as a multiplication operator.

*  Parameters:
*     exp
*        A pointer to the expression to be cleaned.

*  Returned Value:
*     A pointer to the cleaned expression, which should be freed using
*     astFree when no longer needed.

*  Notes:
*     - This function returns NULL if it is invoked with the global error
*     status set, or if it should fail for any reason.
*/

/* Local Variables: */
   char **tok;
   char *p;
   char *r;
   char *result;
   char *s;
   char *t;
   char *w;
   const char *start;
   int i;
   int l;
   int len;
   char *tt;
   int ntok;
   int po;
   int ps;
   int word;

/* Initialise */
   result = NULL;

/* Check inherited status */
   if( !astOK ) return result;

/* Split the supplied string up into tokens. Each block of contiguous
   alphanumeric characters is a token. Each contiguous block of
   non-alphanumerical characters is also a token. The + and - signs are
   counted as alphanumeric. */
   start = exp;
   p = (char *) exp - 1;
   word = ISWORD( *( p + 1 ) );
   ntok = 0;
   tok = NULL;
   while( *(++p) ){
      if( word ) {
         if( !ISWORD( *p ) ) {
            l = p - start;
            t = astStore( NULL, start, l + 1 );
            if( t ) t[ l ] = 0;
            tok = astGrow( tok, ntok + 1, sizeof( char * ) );
            if( tok ) tok[ ntok++ ] = t;
            start = p;
            word = 0;
         }
      } else {
         if( ISWORD( *p ) ) {
            l = p - start;
            t = astStore( NULL, start, l + 1 );
            if( t ) t[ l ] = 0;
            tok = astGrow( tok, ntok + 1, sizeof( char * ) );
            if( tok ) tok[ ntok++ ] = t;
            start = p;
            word = 1;
         }
      }
   }

   l = p - start;
   t = astStore( NULL, start, l + 1 );
   if( t ) t[ l ] = 0;
   tok = astGrow( tok, ntok + 1, sizeof( char * ) );
   if( tok ) tok[ ntok++ ] = t;

/* Check the tokens for known non-standard unit syntax, and replace with the
   equivalent standard syntax. Starlink SPLAT has a class called UnitUtilities
   which has more of these common units mistakes. AST has to be a bit
   more conservative than SPLAT though because of its wider remit. */
   len = 0;
   tt = NULL;
   for( i = 0; i < ntok; i++ ) {
      t = tok[ i ];
      l = strlen( t );
      tt = astStore( tt, t, l + 1 );

/* Any alphabetical word followed by a digit is taken as <word>^<digit>.
   Any alphabetical word followed by a sign and a digit is taken as
   <word>^<sign><digit>. */
      if( l > 1 && *t != '-' && *t != '+' &&
          strcspn( t, "0123456789" ) == l - 1 ) {
         tok[ i ] = astMalloc( l + 2 );
         if( tok[ i ] ) {
            strcpy( tok[ i ], t );
            w = t + l - 2;
            if( *w != '+' && *w != '-' ) {
               tok[ i ][ l - 1 ] = '^';
               strcpy( tok[ i ] + l, t + l - 1 );
            } else {
               tok[ i ][ l - 2 ] = '^';
               strcpy( tok[ i ] + l - 1, t + l - 2 );
            }
            t = astFree( t );
         }
         l++;

/* If the word ends with "micron" change to "(<start>m*1.0E-6)". Should be OK
   for things like "Kmicron". */
      } else if( ( s = strstr( t, "micron" ) ) ) {
         tok[ i ] = astMalloc( s - t + 11 );
         if( tok[ i ] ) {
            w = tok[ i ];
            *(w++) = '(';
            if( s > t ) {
               strncpy( w, t, s - t );
               w += s - t;
            }
            strcpy( w, "m*1.0E-6)" );
            l = s - t + 11;
            t = astFree( t );
         }

/* Convert "STER" to "sr". */
      } else if( !Ustrcmp( t, "STER", status ) ) {
         tok[ i ] = astStore( NULL, "sr", 3 );
         l = 2;
         t = astFree( t );

/* If the word ends with "JY" and is preceded by a single character, change
   to "<start>Jy". Should be OK for things like "MJY". */
      } else if( l == 3 && !strcmp( t + 1, "JY" ) ) {
         tok[ i ][ 2 ] = 'y';

/* If the word begins with "nano" (case-insensitive) change "nano" to
   "n". Such changes are usually handled by SplitUnit, but we need to
   handle this as a special case here since scanf seems to read "nan" as
   a string representation of NaN. */
      } else if( !Ustrncmp( t, "nano", 4, status ) ) {
         tok[ i ] = astStore( NULL, t + 3, l - 2 );
         if( tok[ i ] ) {
            *(tok[ i ]) = 'n';
            t = astFree( t );
         }
         l -= 3;
      }

/* Update the total length of the string. */
      len += l;
   }
   tt = astFree( tt );

/* Concatentate the tokens into a single string, freeing the individual
   strings. */
   result = astMalloc( len + 1 );
   if( result ) {
      p = result;
      for( i = 0; i < ntok; i++ ) {
         len = strlen( tok[ i ] );
         memcpy( p, tok[ i ], len );
         p += len;
         tok[ i ] = astFree( tok[ i ] );
      }
      *p = 0;
      tok = astFree( tok );

/* Now do other cleaning.
   ---------------------- */

/* Initialise a pointer to the previous character read from the string. */
      r = result - 1;

/* Initialise a pointer to the next character to be written to the string. */
      w = result;

/* Pretend the previous character written to the string was a space. */
      ps = 1;

/* Read all the supplied string, copying it to earlier parts of the
   string discarding leading spaces and multiple adjacent embedded spaces in
   the process. */
      while( *(++r) ) {

/* If the character read is a space, only write it to the string if the
   previous character written was not a space (in which case set a flag
   to indicate that the previous character written to the string is now a
   space). */
         if( isspace( *r ) ) {
            if( !ps ) {
               *(w++) = *r;
               ps = 1;
            }

/* Write all non-space characters to the string, and clear the flag which
   indicates if the previous character written to the string was a space. */
         } else {
            *(w++) = *r;
            ps = 0;
         }
      }

/* If the last character written to the string was a space, reduce the
   length of the string by one since we do not want any trailing spaces. */
      if( ps ) w--;

/* Terminate the string. */
      *w = 0;

/* We now need to pass through the string again, this time removing any
   spaces which are adjacent to a binary operator or a parenthesis. */
      r = result - 1;
      w = result;
      ps = 0;
      po = 0;
      while( *(++r) ) {

/* If the current character is a space, only write it if the previous
   written character was not an operator or parenthesis. */
         if( isspace( *r ) ) {
            if( !po ) {
               *(w++) = *r;
               po = 1;
               ps = 1;
            }

/* If the current character is an operator or parenthesis, back up one
   character before writing it out if the previous written character was
   a space. */
         } else if( *r == '*' || *r == '/' || *r == '^' || *r == '.' ||
                    *r == ')' || *r == '(' ) {
            if( ps ) w--;
            *(w++) = *r;
            po = 1;
            ps = 0;

/* If the current character is not a space and not an operator symbol,
   just write it out. */
         } else {
            *(w++) = *r;
            po = 0;
            ps = 0;
         }
      }

/* Terminate the string. */
      if( ps ) w--;
      *w = 0;

   }

/* Return the result. */
   return (const char *) result;
}

static int CmpTree( UnitNode *tree1, UnitNode *tree2, int exact, int *status ) {
/*
*  Name:
*     CmpTree

*  Purpose:
*     Compares two trees of UnitNodes.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     int CmpTree( UnitNode *tree1, UnitNode *tree2, int exact, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function returns a zero value if the two trees are
*     equivalent. This requires the trees to have identical structure
*     except that, if "exact" is zero, arguments for OP_MULT nodes can
*     be swapped.
*
*     If the trees are not equivalent then a value of +1 or -1 is returned
*     depending on whether tree1 should be placed before or after tree2
*     in a sorted list of trees.

*  Parameters:
*     tree1
*        A pointer to the UnitNode at the head of the first tree.
*     tree2
*        A pointer to the UnitNode at the head of the second tree.
*     exact
*        If non-zero, then OP_MULT nodes must have their arguments the
*        same way round in order for the OP_MULT nodes to match. Otherwise,
*        OP_MULT nodes with equivalent arguments match even if the
*        arguments are swapped.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Zero if the two trees are equal. +1 if tree1 should be placed before
*     tree2 in a sorted list of trees. -1 if tree1 should be placed after
*     tree2 in a sorted list of trees.

* Notes:
*     - Zero is returned if an error has already occurred, or
*     if this function fails for any reason.

*/

/* Local Variables: */
   int result;
   int i;
   Oper op;

/* Initialise. */
   result = 0;

/* Check inherited status. */
   if( !astOK ) return result;

/* If the op codes differ, compare them as integers. */
   op = tree1->opcode;
   if( op != tree2->opcode ) {
      result = ( op > tree2->opcode ) ? 1: -1;

/* If both supplied nodes are OP_LDVAR nodes, compare the associated names. */
   } else if( op == OP_LDVAR ){
      result = strcmp( tree1->name, tree2->name );

/* If both supplied nodes are constant nodes, compare the constant values. */
   } else if( tree1->con != AST__BAD ){
      result = EQUAL( tree1->con, tree2->con ) ? 0 : (
                 ( tree1->con > tree2->con ) ? 1 : -1 );

/* Otherwise, compare the arguments for the node. */
   } else {
      for( i = 0; i < tree1->narg; i++ ) {
         result = CmpTree( tree1->arg[ i ], tree2->arg[ i ], exact, status );
         if( result ) break;
      }

/* If the head nodes of the two trees are OP_MULT nodes, and the above
   check determined they are different, this may be just because they
   have their operands swapped. If "exact" si zero, this is considered an
   insignificant difference between the two trees which we should ignore.
   To check for this try comparing the arguments again, this time swapping
   the arguments of tree2. */
      if( result && op == OP_MULT && !exact ) {
         for( i = 0; i < tree1->narg; i++ ) {
            result = CmpTree( tree1->arg[ i ], tree2->arg[ 1 - i ], 0, status );
            if( result ) break;
         }
      }
   }

/* If an error has occurred, return zero. */
   if( !astOK ) result = 0;

/* Return the answer. */
   return result;
}

static UnitNode *CombineFactors( UnitNode **factors, double *powers,
                                 int nfactor, double coeff, int *status ) {
/*
*  Name:
*     CombineFactors

*  Purpose:
*     Create a tree which represents the product of the supplied factors.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     UnitNode *CombineFactors( UnitNode **factors, double *powers,
*                               int nfactor, double coeff, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function createa a tree of UnitNodes which represents the
*     product of the supplied factors, and the supplied coefficient.
*     The factors are sorted before being combined, using the sort order
*     implemented by the CmpTree function.

*  Parameters:
*     factors
*        A pointer to an array with "nfactor" elements, each element being
*        a pointer to a UnitNode which is a factor of the required tree.
*        On exit, the array is sorted.
*     powers
*        A pointer to an array with "nfactor" elements, each element being a
*        double holding the power of the associated factor in "factors".
*        On exit, the array reflects the sorting applied to "factors".
*     nfactor
*        The number of elements in the "factors" and "powers" arrays.
*     coeff
*        The overall coefficient to be applied to the product of the factors.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a UnitNode which is at the head of the new tree.

* Notes:
*     - A NULL pointer is returned if an error has already occurred, or
*     if this function fails for any reason.

*/

/* Local Variables: */
   UnitNode *result;
   int i;
   int j;
   int jp;
   int done;
   UnitNode *ftmp;
   UnitNode *node1;
   UnitNode *node2;
   UnitNode *pnode;
   double ptmp;

/* Initialise. */
   result = NULL;

/* Check inherited status. */
   if( !astOK ) return result;

/* Sort the supplied list of factors, modifying the powers array
   correspondingly. A simple bubblesort algorithm is used since there
   will only be a handfull of factors. */
   for( i = nfactor - 1; i > 0; i-- ) {
      done = 1;
      for( j = 0, jp = 1; j < i; j++, jp++ ) {
         if( CmpTree( factors[ j ], factors[ jp ], 0, status ) > 0 ) {
            ftmp = factors[ j ];
            factors[ j ] = factors[ jp ];
            factors[ jp ] = ftmp;

            ptmp = powers[ j ];
            powers[ j ] = powers[ jp ];
            powers[ jp ] = ptmp;

            done = 0;
         }
      }
      if( done ) break;
   }

/* The first root term of the returned tree is the coefficient, unless the
   coefficient is 1.0, in which case it will be the first factor. */
   if( coeff != 1.0 ) {
      node1 = NewNode( NULL, OP_LDCON, status );
      if( astOK ) node1->con = coeff;
   } else {
      node1 = NULL;
   }

/* Loop through the factors. */
   for( i = 0; i < nfactor; i++ ) {

/* If the power of this factor is zero, we ignore the factor. */
      if( powers[ i ] != 0.0 ) {

/* If the power of this factor is one, we use the factor directly. */
         if( EQUAL( powers[ i ], 1.0 ) ) {
            node2 = CopyTree( factors[ i ], status );

/* Otherwise, for non-zero, non-unity powers, we create a POW node for
   the factor. */
         } else {
            node2 = NewNode( NULL, OP_POW, status );
            pnode = NewNode( NULL, OP_LDCON, status );
            if( astOK ) {
               pnode->con = powers[ i ];
               node2->arg[ 0 ] = CopyTree( factors[ i ], status );
               node2->arg[ 1 ] = pnode;
            }
         }

/* We now combine node1 and node2 using an OP_MULT node, which becomes
   the "node1" for the next pass. On the first pass we may have no node1 (if
   the supplied coefficient was 1.0), in which case we reserve the current
   node2 as the node1 for the next pass. */
         if( node1 ) {
            result = NewNode( NULL, OP_MULT, status );
            if( astOK ) {
               result->arg[ 0 ] = node1;
               result->arg[ 1 ] = node2;
               node1 = result;
            }
         } else {
            node1 = node2;
         }
      }
   }

/* Ensure we have a node to return. */
   if( astOK ) {
      if( !result ) result = node1;
      if( !result ) {
         result = NewNode( NULL, OP_LDCON, status );
         if( astOK ) result->con = 1.0;
      }
   }

/* If an error has occurred, free any new tree. */
   if( !astOK ) result = FreeTree( result, status );

/* Return the answer. */
   return result;
}

static int ComplicateTree( UnitNode **node, int *status ) {
/*
*  Name:
*     ComplicateTree

*  Purpose:
*     Removes standardisations introduced by SimplifyTree.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     int ComplicateTree( UnitNode **node )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function modifies a tree of UnitNodes by removing standardisations
*     introduced by SimplifyTree. The standardisations removed are ones
*     which would make the corresponding algebraic expression (as produced
*     by MakeExp) unnatural to a human reader.

*  Parameters:
*     node
*        The address of a pointer to the UnitNode at the head of the tree
*        which is to be complicated. On exit the supplied tree is freed and
*        a pointer to a new tree is placed at the given address.

*  Returned Value:
*     Non-zero if any change was introduced into the tree.

*/

/* Local Variables: */
   int i;
   UnitNode *newnode;
   UnitNode *node1;
   UnitNode *node2;
   UnitNode *node3;
   Oper op;
   double con;
   double fk;
   int k;
   int result;
   double kcon;

/* Initialise */
   result = 0;

/* Check inherited status. */
   if( !astOK ) return result;

/* Initiallially, we have no replacement node. */
   newnode = NULL;
   node1 = NULL;
   node3 = NULL;

/* Complicate the sub-trees corresponding to the arguments of the node at
   the head of the supplied tree. */
   for( i = 0; i < (*node)->narg; i++ ) {
      if( ComplicateTree( &( (*node)->arg[ i ] ), status ) ) result = 1;
   }

/* Now undo specific simplifications appropriate to the nature of the node at
   the head of the tree. */
   op = (*node)->opcode;

/* If the head is an OP_MULT node with a constant first argument and
   a "LN" second argument, rearrange the nodes to represent ln(x**k) instead
   of k*ln(x). If k is an integer multiple of "0.1/ln(10)" convert the "ln"
   function into a "log" (base 10) function. Check for "k==1" in which
   case we do not need a POW node. */
   if( (*node)->opcode == OP_MULT ) {

      con = (*node)->arg[ 0 ]->con;
      if( con != AST__BAD && (*node)->arg[ 1 ]->opcode == OP_LN ) {
         fk = 10.0*con*log( 10.0 );
         k = NINT(fk);
         if( EQUAL(fk,k) ) {
            newnode = NewNode( NULL, OP_LOG, status );
            con = k/10.0;
         } else {
            newnode = NewNode( NULL, OP_LN, status );
         }

         node2 = CopyTree( (*node)->arg[ 1 ]->arg[ 0 ], status );
         if( !EQUAL( con, 1.0 ) ){
            node1 = CopyTree( (*node)->arg[ 0 ], status );
            node3 = NewNode( NULL, OP_POW, status );
         }

         if( astOK ) {
            if( !EQUAL( con, 1.0 ) ){
               node1->con = con;
               node3->arg[ 0 ] = node2;
               node3->arg[ 1 ] = node1;
               newnode->arg[ 0 ] = node3;
            } else {
               newnode->arg[ 0 ] = node2;
            }
         }

/* Replace "(A**-1)*B" with "B/A" */
      } else if( (*node)->arg[ 0 ]->opcode == OP_POW &&
                 EQUAL( (*node)->arg[ 0 ]->arg[ 1 ]->con, -1.0 )) {
         newnode = NewNode( NULL, OP_DIV, status );
         if( astOK ) {
            newnode->arg[ 0 ] = CopyTree( (*node)->arg[ 1 ], status );
            newnode->arg[ 1 ] = CopyTree( (*node)->arg[ 0 ]->arg[ 0 ], status );
         }

/* Replace "B*(A**-1)" with "B/A" */
      } else if( (*node)->arg[ 1 ]->opcode == OP_POW &&
                 EQUAL( (*node)->arg[ 1 ]->arg[ 1 ]->con, -1.0 )) {
         newnode = NewNode( NULL, OP_DIV, status );
         if( astOK ) {
            newnode->arg[ 0 ] = CopyTree( (*node)->arg[ 0 ], status );
            newnode->arg[ 1 ] = CopyTree( (*node)->arg[ 1 ]->arg[ 0 ], status );
         }

/* Convert (x**k)*(y**k) to (x*y)**k. */
      } else if( (*node)->arg[ 0 ]->opcode == OP_POW &&
                 (*node)->arg[ 1 ]->opcode == OP_POW &&
                 EQUAL( (*node)->arg[ 0 ]->arg[ 1 ]->con,
                        (*node)->arg[ 1 ]->arg[ 1 ]->con )) {
         newnode = NewNode( NULL, OP_POW, status );
         node1 = NewNode( NULL, OP_MULT, status );
         if( astOK ) {
            node1->arg[ 0 ] = CopyTree( (*node)->arg[ 0 ]->arg[ 0 ], status );
            node1->arg[ 1 ] = CopyTree( (*node)->arg[ 1 ]->arg[ 0 ], status );
            newnode->arg[ 0 ] = node1;
            newnode->arg[ 1 ] = CopyTree( (*node)->arg[ 0 ]->arg[ 1 ], status );
         }

/* Convert c*sqrt(x) to sqrt((c**2)*x) (if c > 0). */
      } else if( (kcon=(*node)->arg[ 0 ]->con) != AST__BAD &&
                 kcon > 0.0 && (*node)->arg[ 1 ]->opcode == OP_SQRT ) {
         newnode = NewNode( NULL, OP_SQRT, status );
         node1 = NewNode( NULL, OP_MULT, status );
         node2 = NewNode( NULL, OP_LDCON, status );
         if( astOK ) {
            node2->con = kcon*kcon;
            node1->arg[ 0 ] = node2;
            node1->arg[ 1 ] = CopyTree( (*node)->arg[ 1 ]->arg[ 0 ], status );
            newnode->arg[ 0 ] = node1;
         }
      }

/* If the head node is a POW node, replace "x**0.5" by sqrt(x) */
   } else if( (*node)->opcode == OP_POW ) {
      if( EQUAL( (*node)->arg[ 1 ]->con, 0.5 ) ) {
         newnode = NewNode( NULL, OP_SQRT, status );
         if( astOK ) {
            newnode->arg[ 0 ] = CopyTree( (*node)->arg[ 0 ], status );
         }
      }
   }

/* If we have produced a new node which is identical to the old node,
   free it. Otherwise, indicate we have made some changes. */
   if( newnode ) {
      if( !CmpTree( newnode, *node, 1, status ) ) {
         newnode = FreeTree( newnode, status );
      } else {
         result = 1;
      }
   }

/* If an error has occurred, free any new node. */
   if( !astOK ) {
      newnode = FreeTree( newnode, status );
      result = 0;
   }

/* If we have a replacement node, free the supplied tree and return a
   pointer to the new tree. */
   if( newnode ) {
      FreeTree( *node, status );
      *node = newnode;
   }

/* If the above produced some change, try simplifying (without
   re-introducing the standardisation we have just got rid of!) and
   then re-complicating the tree. */
   if( result ) {
      SimplifyTree( node, 0, status );
      ComplicateTree( node, status );
   }

/* Return the result. */
   return result;
}

static UnitNode *ConcatTree( UnitNode *tree1, UnitNode *tree2, int *status ) {
/*
*  Name:
*     ConcatTree

*  Purpose:
*     Concatenate two trees together.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     UnitNode *ConcatTree( UnitNode *tree1, UnitNode *tree2, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function a pointer to the head of a new tree of UnitNodes which
*     is formed by feeding the output of "tree1" (i.e. the quantity
*     represented by the node at the head of tree1) into the (single)
*     input of "tree2" (i.e. the single OP_LDVAR Node containined within
*     tree2).

*  Parameters:
*     tree1
*        A pointer to the first tree.
*     tree2
*        A pointer to the second tree. This should have no more than one
*        OP_LDVAR node.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a UnitNode which is at the head of the new tree.

* Notes:
*     - If "tree2" contains zero units, a NULL pointer is returned but no
*     error is reported.
*     - If "tree2" contains more than one unit, an error is reported
*     error is reported.
*     - A NULL pointer is returned if an error has already occurred, or
*     if this function fails for any reason.

*/

/* Local Variables: */
   UnitNode *result;
   UnitNode **units;
   int nunits;

/* Initialise. */
   result = NULL;

/* Check inherited status. */
   if( !astOK ) return result;

/* Produce a copy of tree2. */
   result = CopyTree( tree2, status );

/* Locate the OP_LDVAR node in the copy of tree2. */
   units = NULL;
   nunits = 0;
   LocateUnits( result, &units, &nunits, status );

/* If no OP_LDVAR nodes were found in tree2, we cannot concatenate the
   trees. */
   if( nunits > 0 ) {

/* Report an error if the number of pointers returned is larger than 1. */
      if( nunits > 1 && astOK ) {
         astError( AST__INTER, "ConcatTree(unit): tree2 uses %d units - "
                   "should be 1 (internal AST programming error).", status, nunits );
      }

/* Replace the OP_LDVAR node in the copy of tree2 with a copy of tree1. */
      if( astOK ) {

/* If the node at the head of the supplied tree2 is the node to be
   replaced, just free the tree created earlier and return a copy of
   tree1. */
         if( units[ 0 ] == result ) {
            FreeTree( result, status );
            result = CopyTree( tree1, status );

/* Otherwise, search for the node to be replaced and do the substitution
   within the tree created earlier. */
         } else {
            ReplaceNode( result, units[ 0 ], CopyTree( tree1, status ), status );
         }
      }
   }

/* Free resources. */
   units = astFree( units );

/* If an error has occurred, free any new tree. */
   if( !astOK ) result = FreeTree( result, status );

/* Return the answer. */
   return result;
}

static int ConStart( const char *text, double *val, int *nc, int *status ) {
/*
*  Name:
*     ConStart

*  Purpose:
*     See if the supplied string starts with a literal numeric constant.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     int ConStart( const char *text, double *val, int *nc, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function checks if the supplied string starts with a literal
*     numeric constant and returns it if it does. It is a wrap-up for scanf
*     since scanf has non-standard behaviour on some platforms (e.g. Cygwin
*     scanf interprets the character "n" as a floating point number!).

*  Parameters:
*     text
*        The text to check.
*     val
*        Address of a double to receive any numerical constant read
*        from the start of the string. Unity is returned if the string
*        does not start with a numerical constant.
*     nc
*        Address of an int to receive the number of characters used to
*        create the value returned in "val". Zero is returned if the
*        string does not start with a numerical constant.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Non-zero if the text started with a numerical constant.

*/

/* Local Variables: */
   int result;
   const char *c;

/* Initialise */
   *nc = 0;
   *val = 1.0;

/* Return zero if no text was supplied */
   if( !text ) return 0;

/* Use sscanf to see if the string begin with a numerical constant */
   result = astSscanf( text, "%lf%n", val, nc );

/* If so, check that the first non-blank character in the string
   is not "N" (interpreted by Cygwin as numerical zero!). */
   if( result ) {
      c = text;
      while( isspace( *c ) ) c++;
      if( *c == 'n' || *c == 'N' ) {
         result = 0;
         *nc = 0;
         *val = 1.0;
      }
   }

/* Return the result. */
   return result;
}

static UnitNode *CopyTree( UnitNode *tree, int *status ) {
/*
*  Name:
*     CopyTree

*  Purpose:
*     Create a new tree of UnitNodes containing a copy of a given tree.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     UnitNode *CopyTree( UnitNode *tree, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function creates a copy of the supplied tree of UnitNodes.

*  Parameters:
*     tree
*        The UnitNode at the head of the tree to be copied.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the UnitNode at the head of the new tree.

*  Notes:
*     - A value of NULL will be returned if this function is invoked with
*     the global error status set, or if it should fail for any reason.
*/

/* Local Variables: */
   UnitNode **args;
   UnitNode *result;
   int i;
   int narg;

/* Initialise. */
   result = NULL;

/* Check the inherited status. */
   if( !astOK || !tree ) return result;

/* Create a new node to represent the head of the supplied tree. */
   result = astMalloc( sizeof( UnitNode ) );

/* Check pointers can be used safely. */
   if( astOK ) {

/* Copy the fields of the supplied node. */
      narg = tree->narg;

      result->arg = NULL;
      result->unit = tree->unit;
      result->mult = tree->mult;
      result->opcode = tree->opcode;
      result->narg = narg;
      result->con = tree->con;
      result->name = tree->name ? astStore( NULL, tree->name,
                                            strlen( tree->name ) + 1 ) : NULL;

/* Create an array of UnitNode pointers for the arguments. */
      args = astMalloc( narg*sizeof( UnitNode * ) );
      if( astOK ) {
         result->arg = args;

/* Copy the sub-trees headed by the argument nodes. */
         for( i = 0; i < narg; i++ ) {
            args[ i ] = CopyTree( tree->arg[ i ], status );
         }
      }
   }

/* Free any result if an error occurred. */
   if( !astOK ) result = FreeTree( result, status );

/* Return the answer. */
   return result;
}

static UnitNode *CreateTree( const char *exp, int basic, int lock, int *status ){
/*
*  Name:
*     CreateTree

*  Purpose:
*     Convert an algebraic units expression into a tree of UnitNodes.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     UnitNode *CreateTree( const char *exp, int basic, int lock, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function converts the supplied algebraic units expression into
*     a tree of UnitNodes. The result tree can optionally be expanded to
*     create a tree in which the "roots" (LDVAR nodes) all refer to
*     basic units.

*  Parameters:
*     exp
*        The units expression. This should not include any leading or
*        trailing spaces.
*     basic
*        Should the tree created from parsing "exp" be expanded so that
*        the leaf nodes of the tree are all basic units?
*     lock
*        Use a mutex to guard access to the KnownUnits list?
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a UnitNode which forms the head of a tree of UnitNodes
*     representing the supplied unit expression.

*  Notes:
*     -  A NULL value is returned if this function is invoked with the
*     global error status set or if it should fail for any reason.
*/

/* Local Variables: */
   UnitNode *result;
   const char *cleanex;

/* Initialise */
   result = NULL;

/* Check the global error status, and that we have a string. */
   if ( !astOK ) return result;

/* Produce a clean copy of the supplied string. This has no leading
   or trailing white space, and any spaces adjacent to operators within
   the string are removed (this is needed because spaces are treated as
   multiplication symbols). */
   cleanex = CleanExp( exp, status );

/* If the string is blank, return the NULL pointer. Otherwise, create a
   tree of UnitNodes describing the units. The returned tree has LDVAR
   nodes which refer to the unit symbols contained in the supplied string. */
   if( cleanex && (*cleanex) ) {
      result = MakeTree( cleanex, strlen( cleanex ), lock, status );

/* Replace each subtree which simply combines constants (i.e. which has no
   OP_LDVAR nodes) with a single OP_LDCON node. */
      FixConstants( &result, 0, status );

/* Invert literal constant unit multipliers. */
      InvertConstants( &result, status );

/* Now replace each LDVAR node which refers to a known derived unit with
   a sub-tree which defines the derived unit in terms of known basic units.
   The LDVAR nodes in the resulting tree all refer to basic units. */
      if( basic ) RemakeTree( &result, status );
   }

/* Free resources. */
   cleanex = astFree( (void *) cleanex );

/* Free any returned tree if an error has occurred. */
   if( !astOK ) result = FreeTree( result, status );

/* Return the result. */
   return result;
}

static int DimAnal( UnitNode *node, double powers[NQUANT], double *scale, int *status ) {
/*
*  Name:
*     DimAnal

*  Purpose:
*     Perform a dimensional analysis of a unit tree.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "unit.h"
*     int DimAnal( UnitNode *node, double powers[NQUANT], double *scale, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function returns a set of powers and a scaling factor which
*     represent the units tree.

*  Parameters:
*     node
*        Pointer to the UnitNode at the head of the unit tree.
*     powers
*        An array in which are returned the powers for each of the following
*        basic units (in the order shown): kilogramme, metre, second, radian,
*        Kelvin, count, adu, photon, magnitude, pixel. If the supplied unit
*        does not depend on a given basic unit a value of 0.0 will be returned
*        in the array. The returns values represent a system of units which is
*        a scaled form of the supplied units, expressed in the basic units of
*        m, kg, s, rad, K, count, adu, photon, mag and pixel. For instance, a
*        returned array of [1,0,-2,0,0,0,0,0,0,0] would represent "m/s**2".
*     scale
*        Pointer to a location at which to return a scaling factor for the
*        supplied units. The is the value, in the units represented by the
*        returned powers, which corresponds to a value of 1.0 in the supplied
*        units.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Non-zero if the tree was analysed succesfully. Zero otherwise.

*  Notes:
*     -  Zero is returned if this function is invoked with the
*     global error status set or if it should fail for any reason.
*/

/* Local Variables; */
   Oper oper;
   int result;
   int i;
   double p0[ NQUANT ];
   double p1[ NQUANT ];
   double s0;
   double s1;

/* Check inherited status */
   if( !astOK ) return 0;

/* Initialise the powers of all dimensions to zero, and set the scaling
   factor to unity. */
   result = 1;
   *scale = 1.0;
   for( i = 0; i < NQUANT; i++ ) powers[ i ] = 0.0;

/* Load constant: constant is dimensionaless so leave powers unchanged,
   and set the scaling factor. */
   oper = node->opcode;
   if( oper == OP_LDCON ) {
      *scale = 1.0/node->con;

/* Load variable: check it is one of the basic known dimensional
   quantities. If so, set the power of the quantity to unity and store
   the scale factor. If the unit is "g" modify the scale factor so that
   the analysis quantity is "kg". */
   } else if( oper == OP_LDVAR ) {
      result = 0;
      for( i = 0; i < NQUANT; i++ ) {
         if( node->unit == quant_units[ i ] ) {
            powers[ i ] = 1.0;
            *scale = node->mult ? 1.0/node->mult->scale : 1.0;
            if( !strcmp( node->unit->sym, "g" ) ) *scale *= 0.001;
            result = 1;
            break;
         }
      }

/* How does dimensional analysis handle log or exp units?*/
   } else if( oper == OP_LOG ) {
      result= 0;

   } else if( oper == OP_LN ) {
      result= 0;

   } else if( oper == OP_EXP ) {
      result= 0;

/* Get the powers for the child unit and then multiply each by 0.5 and
   take the square root of the scale factor. */
   } else if( oper == OP_SQRT ) {
      result = DimAnal( node->arg[0], powers, scale, status );
      if( result ) {
         for( i = 0; i < NQUANT; i++ ) powers[ i ]*= 0.5;
         *scale = sqrt( *scale );
      }

/* Similarly for pow nodes. */
   } else if( oper == OP_POW ) {
      result = DimAnal( node->arg[0], powers, scale, status );
      if( result ) {
         double power = node->arg[1]->con;
         for( i = 0; i < NQUANT; i++ ) powers[ i ]*= power;
         *scale = pow( *scale, power );
      }

/* Binary operators. Analyses the operands dimensions and combine. */
   } else if( oper == OP_DIV ) {
      if( DimAnal( node->arg[0], p0, &s0, status ) &&
          DimAnal( node->arg[1], p1, &s1, status ) ) {
         for( i = 0; i < NQUANT; i++ ) powers[ i ] = p0[ i ] - p1[ i ];
         *scale = s0/s1;
      } else {
         result = 0;
      }

   } else if( oper == OP_MULT ) {
      if( DimAnal( node->arg[0], p0, &s0, status ) &&
          DimAnal( node->arg[1], p1, &s1, status ) ) {
         for( i = 0; i < NQUANT; i++ ) powers[ i ] = p0[ i ] + p1[ i ];
         *scale = s0*s1;
      } else {
         result = 0;
      }

/* Named constants are dimensionless */
   } else if( oper == OP_LDPI ) {
      *scale = 1.0/PI;

   } else if( oper == OP_LDE ) {
      *scale = 1.0/E;

   }

   return result;

}

static int EndsWith( const char *c, int nc, const char *test, int *status ){
/*
*  Name:
*     EndsWith

*  Purpose:
*     See if a string ends with another string

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     int EndsWith( const char *c, int nc, const char *test, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function sees if the string given by "c" ends with the string
*     given by "test". The comparison is case-insensitive.

*  Parameters:
*     c
*        A pointer to the last character in the string to be tested.
*     nc
*        The number of characters in the string to be tested.
*     test
*        A pointer to the string to be tested for.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Non-zero if the string "c" ends with the string "test".

*/

/* Local Variables: */
   const char *start;
   int i;
   int result;
   int tlen;

/* initialise. */
   result = 0;

/* Check inherited status. */
   if( !astOK ) return result;

/* Check the string being tested for is not longer than the string being
   tested. */
   tlen = strlen( test );
   if( tlen <= nc ){

/* Get a pointer to where the matching string would start if the string "c"
   ends with the required string "test". */
      start = c - tlen + 1;

/* Do the comparison. */
      result = 1;
      for( i = 0; i < tlen; i++ ) {
         if( tolower( start[ i ] ) != tolower( test[ i ] ) ) {
            result = 0;
            break;
         }
      }
   }

/* Return the result. */
   return result;

}

static void FindFactors( UnitNode *node, UnitNode ***factors, double **powers,
                         int *nfactor, double *coeff, int *status ){
/*
*  Name:
*     FindFactors

*  Purpose:
*     Find the factors within an expression given by a tree of UnitNodes.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     void FindFactors( UnitNode *node, UnitNode ***factors, double **powers,
*                       int *nfactor, double *coeff, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function analyses the supplied tree of UnitNoes and returns
*     an array of pointers to nodes within the supplied tree which form
*     factors of the tree. The power associated with each factor is also
*     returned, together with an overall coefficient for the tree. The
*     expression represented by the tree is thus the product of the
*     coefficient with each of the factors, each raised to the associated
*     power.

*  Parameters:
*     node
*        A pointer to the UnitNode at the head of the tree which is to be
*        analysed.
*     factors
*        The address at which to return a pointer to an array with "*nfactor"
*        elements, each element being a pointer to a UnitNode within the
*        supplied tree which is a factor of the supplied tree.
*     powers
*        The address at which to return a pointer to an array with "*nfactor"
*        elements, each element being a double holding the power of the
*        associated factor in "*factors".
*     nfactor
*        The address of an int containing the number of elements in the
*        returned "*factors" and "*powers" arrays.
*     coeff
*        The address of a double containing the overall coefficient to be
*        applied to the product of the factors.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - If the supplied node is a constant node, then "*coeff" is
*     returned holding the value of the constant and "*nfactor" is returned
*     equal to zero ("*factors" and "*powers" are returned holding NULL).
*     - If an error has already occurred, or if this function fails, then
*     "*factors" and "*powers" are returned holding NULL, "*nfactor" is
*     returned holding zero and "*coeff" is returned holding 1.0.

*/

/* Local Variables: */
   int i;
   int j;
   int found;
   UnitNode **fact1;
   double *pow1;
   double coeff1;
   int nfac1;
   double con;

/* Initialise */
   *factors = NULL;
   *powers = NULL;
   *nfactor = 0;
   *coeff = 1.0;

/* Check inherited status. */
   if( !astOK ) return;

/* If the node at the head of the supplied tree is an OP_MULT node... */
   if( node->opcode == OP_MULT ) {

/* Find the factors of the two arguments of the OP_MULT node. */
      FindFactors( node->arg[ 0 ], factors, powers, nfactor, coeff, status );
      FindFactors( node->arg[ 1 ], &fact1, &pow1, &nfac1, &coeff1, status );

/* Combine the two lists. Loop round the factors of the seocnd argument. */
      for( i = 0; i < nfac1; i++ ) {

/* See if there is already an equivalent factor in the returned list of
   factors. */
         found = 0;
         for( j = 0; j < *nfactor; j++ ) {
            if( !CmpTree( (*factors)[ j ], fact1[ i ], 0, status ) ){
               found = 1;
               break;
            }
         }

/* If so, increment the power of the factor. */
         if( found ) {
            (*powers)[ j ] += pow1[ i ];

/* Otherwise, add the factor to the end of the returned list. */
         } else {
            *factors = astGrow( *factors, *nfactor + 1, sizeof( UnitNode *) );
            *powers = astGrow( *powers, *nfactor + 1, sizeof( double ) );
            if( astOK ) {
               (*factors)[ *nfactor ] = fact1[ i ];
               (*powers)[ (*nfactor)++ ] = pow1[ i ];
            }
         }
      }

/* Modify the overall coefficient. */
      *coeff *= coeff1;

/* Free resources */
      fact1 = astFree( fact1 );
      pow1 = astFree( pow1 );

/* If the node at the head of the supplied tree is an OP_POW node, */
   } else if( node->opcode == OP_POW ) {

/* Find the factors of the first argument. */
      FindFactors( node->arg[ 0 ], factors, powers, nfactor, coeff, status );

/* Multiply all the factor powers by the constant exponent of the POW
   node. */
      con = node->arg[ 1 ]->con;
      for( j = 0; j < *nfactor; j++ ) {
         (*powers)[ j ] *= con;
      }

/* Exponentiate the coefficient. */
      if( *coeff >= 0.0 || (int) con == con ) {
         *coeff = pow( *coeff, con );
      } else {
         astError( AST__BADUN, "Simplifying a units expression requires a "
                   "negative value to be raised to a non-intergal power." , status);
      }

/* If the node at the head of the supplied tree is an OP_DIV node, */
   } else if( node->opcode == OP_DIV ) {

/* Find the factors of the two arguments of the OP_DIV node. */
      FindFactors( node->arg[ 0 ], factors, powers, nfactor, coeff, status );
      FindFactors( node->arg[ 1 ], &fact1, &pow1, &nfac1, &coeff1, status );

/* Combine the two lists. Loop round the factors of the second argument
   (the denominator). */
      for( i = 0; i < nfac1; i++ ) {

/* See if there is already an equivalent factor in the returned list of
   factors. */
         found = 0;
         for( j = 0; j < *nfactor; j++ ) {
            if( !CmpTree( (*factors)[ j ], fact1[ i ], 0, status ) ){
               found = 1;
               break;
            }
         }

/* If so, decrement the power of the factor. */
         if( found ) {
            (*powers)[ j ] -= pow1[ i ];

/* Otherwise, add the factor to the end of the returned list, with a
   negated power. */
         } else {
            *factors = astGrow( *factors, *nfactor + 1, sizeof( UnitNode *) );
            *powers = astGrow( *powers, *nfactor + 1, sizeof( double ) );
            if( astOK ) {
               (*factors)[ *nfactor ] = fact1[ i ];
               (*powers)[ (*nfactor)++ ] = -pow1[ i ];
            }
         }
      }

/* Modify the overall coefficient. */
      if( coeff1 != 0.0 ) {
         *coeff /= coeff1;
      } else {
         astError( AST__BADUN, "Simplifying a units expression"
                   "requires a division by zero." , status);
      }

/* Free resources */
      fact1 = astFree( fact1 );
      pow1 = astFree( pow1 );

/* If the node at the head of the supplied tree is an OP_SQRT node, */
   } else if( node->opcode == OP_SQRT ) {

/* Find the factors of the argument. */
      FindFactors( node->arg[ 0 ], factors, powers, nfactor, coeff, status );

/* Multiply all the factor powers by 0.5. */
      for( j = 0; j < *nfactor; j++ ) {
         (*powers)[ j ] *= 0.5;
      }

/* Square root the coefficient. */
      if( *coeff >= 0.0 ) {
         *coeff = sqrt( *coeff );
      } else {
         astError( AST__BADUN, "Simplifying a units expression requires "
                   "the square root of a negative value to be taken." , status);
      }

/* If the node at the head of the supplied tree is constant we have no
   factors but we have a coeffcient. */
   } else if( node->con != AST__BAD ) {
      *coeff = node->con;

/* Other nodes have no factors other than themselves, so just return a
   pointer to the supplied node. */
   } else {
      *factors = astMalloc( sizeof( UnitNode *) );
      *powers = astMalloc( sizeof( double ) );
      if( astOK ) {
         *nfactor = 1;
         (*factors)[ 0 ] = node;
         (*powers)[ 0 ] = 1.0;
         *coeff = 1.0;
      }
   }

/* If an error has occurred, free any returned resources. */
   if( !astOK ) {
      *factors = astFree( *factors );
      *powers = astFree( *powers );
      *nfactor = 0;
      *coeff = 1.0;
   }
}

static void FixConstants( UnitNode **node, int unity, int *status ) {
/*
*  Name:
*     FixConstants

*  Purpose:
*     Take the reciprocal of all constants in a tree of UnitNodes.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     void FixConstants( UnitNode **node, int unity, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function replaces sub-trees which have a constant value by
*     a single OP_LDCON node which loads the appropriate constant.

*  Parameters:
*     node
*        The address of a pointer to the UnitNode at the head of the tree
*        which is to be fixed. On exit the supplied tree is freed and a
*        pointer to a new tree is palced at he given address.
*     unity
*        If non-zero, then all multiplicative constants are set to 1.0, and
*        their original values are forgotten, but only if the other
*        argument of the OP_MULT node is an OP_LDVAR, OP_POW or OP_SQRT Node.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   int i;
   UnitNode *newnode;
   int allcon;
   Oper op;
   double newcon;

/* Check inherited status and pointer. */
   if( !astOK || !node || !(*node) ) return;

/* Initiallially, we have no replacement node */
   newnode = NULL;
   newcon = AST__BAD;

/* There is nothing to fix if the node has no arguments. */
   if( (*node)->narg > 0 ) {

/* Note the op code for the node. */
      op = (*node)->opcode;

/* Fix up the argument nodes. Also note if all the arguments are
   constants. */
      allcon = 1;
      for( i = 0; i < (*node)->narg; i++ ) {
         FixConstants( &( (*node)->arg[ i ] ), unity, status );
         if( (*node)->arg[ i ]->con == AST__BAD ) allcon = 0;
      }

/* If an OP_MULT nodes within a simplified tree has a constant argument,
   it will always be argument zero.  If this is an OP_MULT node and arg[0]
   is constant and "unity" is non-zero and arg[1] is an OP_LDVAR, OP_POW
   or OP_SQRT node, replace the constant value by 1.0. */
      if( unity && op == OP_MULT &&
          (*node)->arg[ 0 ]->con != AST__BAD &&
          ( (*node)->arg[ 1 ]->opcode == OP_LDVAR ||
            (*node)->arg[ 1 ]->opcode == OP_SQRT ||
            (*node)->arg[ 1 ]->opcode == OP_POW ) ) {
         (*node)->arg[ 0 ]->con = 1.0;
      }

/* If the arguments of this node are all constants, replace the node by
   an OP_LDCON node which loads the resulting constant value. */
      if( allcon ) {
         if( (*node)->narg > 0 ) {
            newnode = NewNode( NULL, OP_LDCON, status );
            if( astOK ) {
               if( op == OP_LOG ) {
                  if( (*node)->arg[ 0 ]->con > 0.0 ) {
                     newcon = log10( (*node)->arg[ 0 ]->con );
                  } else {
                     astError( AST__BADUN, "Illegal negative or zero constant "
                               "value '%g' encountered.", status,
                               (*node)->arg[ 0 ]->con );
                  }
               } else if( op == OP_LN ){
                  if( (*node)->arg[ 0 ]->con > 0.0 ) {
                     newcon = log( (*node)->arg[ 0 ]->con );
                  } else {
                     astError( AST__BADUN, "Illegal negative or zero constant value "
                               "'%g' encountered.", status, (*node)->arg[ 0 ]->con );
                  }
               } else if( op == OP_EXP ){
                  newcon = exp( (*node)->arg[ 0 ]->con );

               } else if( op == OP_SQRT ){
                  if( (*node)->arg[ 0 ]->con >= 0.0 ) {
                     newcon = sqrt( (*node)->arg[ 0 ]->con );
                  } else {
                     astError( AST__BADUN, "Illegal negative constant value "
                               "'%g' encountered.", status, (*node)->arg[ 0 ]->con );
                  }

               } else if( op == OP_POW ){
                  if( (*node)->arg[ 0 ]->con >= 0.0 ||
                      (int) (*node)->arg[ 1 ]->con == (*node)->arg[ 1 ]->con ) {
                     newcon = pow( (*node)->arg[ 0 ]->con,
                                   (*node)->arg[ 1 ]->con );
                  } else {
                     astError( AST__BADUN, "Illegal negative constant value "
                               "'%g' encountered.", status, (*node)->arg[ 0 ]->con );
                  }

               } else if( op == OP_DIV ){
                  if( (*node)->arg[ 1 ]->con != 0.0 ) {
                     newcon = (*node)->arg[ 0 ]->con / (*node)->arg[ 1 ]->con;
                  } else {
                     astError( AST__BADUN, "Illegal zero constant value encountered." , status);
                  }

               } else if( op == OP_MULT ){
                  newcon = (*node)->arg[ 0 ]->con * (*node)->arg[ 1 ]->con;

               }


               if( astOK ) newnode->con = newcon;
            }
         }
      }
   }

/* If an error has occurred, free any new node. */
   if( !astOK ) newnode = FreeTree( newnode, status );

/* If we have a replacement node, free the supplied tree and return a
   pointer to the new tree. */
   if( newnode ) {
      FreeTree( *node, status );
      *node = newnode;
   }

}

static UnitNode *FixUnits( UnitNode *node, UnitNode *test, int *status ) {
/*
*  Name:
*     FixUnits

*  Purpose:
*     Assign a constant value to all units except for one.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     UnitNode *FixUnits( UnitNode *node, UnitNode *test, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function returns a copy of the supplied tree of UnitNodes. All
*     OP_LDVAR nodes within the copy which refer to units which differ
*     from those referred to by the supplied test node are replaced by
*     OP_LDCON nodes which load the constant value 1.0.

*  Parameters:
*     node
*        A pointer to the UnitNode at the head of the tree to be used.
*     test
*        A pointer to an OP_LDVAR node which defines the units which are
*        *not* to be replaced by a constant value of 1.0.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a UnitNode which is at the head of a tree of UnitNodes
*     which forms the required copy of th einput tree.

* Notes:
*     - A NULL pointer is returned if an error has already occurred, or
*     if this function fails for any reason.

*/

/* Local Variables: */
   int i;
   UnitNode *result;

/* Initialise. */
   result = NULL;

/* Check inherited status. */
   if( !astOK ) return result;

/* Create a complete copy of the supplied tree. */
   result = CopyTree( node, status );

/* Is the node at the head of the supplied tree an OP_LDVAR node? */
   if( node->opcode == OP_LDVAR ) {

/* Does it refer to a unit which differs from that of the test node? If so
   annul the copy created above and return a new OP_LDCON node which loads
   the constant value 1.0. */
      if( strcmp( test->name, node->name ) ) {
         FreeTree( result, status );
         result = NewNode( NULL, OP_LDCON, status );
         if( astOK ) result->con = 1.0;
      }

/* If the supplied node is not an OP_LDVAR node, check each argument of
   the head node. */
   } else {
      for( i = 0; i < node->narg; i++ ) {

/* Free the resources used to hold this argument in the tree copy created
   above. */
         FreeTree( result->arg[ i ], status );

/* Create a new argument tree by calling this function recursively to
   fix units in the argument sub-trees. */
         result->arg[ i ] = FixUnits( node->arg[ i ], test, status );
      }
   }

/* If an error has occurred, free any new tree. */
   if( !astOK ) result = FreeTree( result, status );

/* Return the answer. */
   return result;
}

static UnitNode *FreeTree( UnitNode *node, int *status ) {
/*
*  Name:
*     FreeTree

*  Purpose:
*     Free resources used by a tree of UnitNodes.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     UnitNode *FreeTree( UnitNode *node, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function frees the memory used to store a tree of UnitNodes.

*  Parameters:
*     node
*        A pointer to the UnitNode at the head of the tree which is to be
*        freed.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A NULL pointer is returned.

*  Notes:
*     - This function attempts to execute even if it is invoked with
*     the global error status set.
*/

/* Local Variables: */
   int i;

/* Check a node was supplied. */
   if( node ) {

/* Recursively free any argument nodes. */
      if( node->arg ) {
         for( i = 0; i < node->narg; i++ ) {
            (node->arg)[ i ] = FreeTree( (node->arg)[ i ], status );
         }
         node->arg = astFree( node->arg );
      }

/* Nullify other pointers for safety. */
      node->unit = NULL;
      node->mult = NULL;

/* Free the copy of the symbol string (if any). */
      node->name = astFree( (char *) node->name );

/* Free the memory holding the node. */
      node = astFree( node );
   }

/* Return a null pointer. */
   return NULL;
}

static KnownUnit *GetKnownUnits( int lock, int *status ) {
/*
*  Name:
*     GetKnownUnits

*  Purpose:
*     Get a pointer to the head of a linked list of known unit definitions.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     KnownUnit *GetKnownUnits( int lock, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function returns a pointer to the head of a linked list of known
*     unit definitions. The unit definitions are created as static module
*     variables if they have not previously been created.

*  Parameters:
*     lock
*        If non-zero, then lock a mutex prior to accessing the list of
*        known units.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the first known unit definition.

*  Notes:
*     - A NULL pointer is returned if it is invoked with the global error
*     status set, or if an error occurs.
*/

/* Local Variables: */
   int iq;
   KnownUnit *result;

/* Initialise. */
   result = NULL;

/* Check inherited status. */
   if( !astOK ) return result;

/* Ensure the known units list is only initialised once. */
   if( lock ) {
      LOCK_MUTEX1
   }

/* If the linked list of KnownUnit structures describing the known units
   has not yet been created, create it now. A pointer to the head of the
   linked list is put into the static variable "known_units". */
   if( !known_units ) {

/* At the same time we store pointers to the units describing the basic
   quantities used in dimensional analysis. Initialise th index of the
   next such unit. */
      iq = 0;

/* Create definitions for the known units. First do all IAU basic units.
   We include "g" instead of "kg" because otherwise we would have to
   refer to a gramme as a milli-kilogramme. */
      MakeKnownUnit( "g", "gram", NULL, status );
      quant_units[ iq++ ] = known_units;
      MakeKnownUnit( "m", "metre", NULL, status );
      quant_units[ iq++ ] = known_units;
      MakeKnownUnit( "s", "second", NULL, status );
      quant_units[ iq++ ] = known_units;
      MakeKnownUnit( "rad", "radian", NULL, status );
      quant_units[ iq++ ] = known_units;
      MakeKnownUnit( "K", "Kelvin", NULL, status );
      quant_units[ iq++ ] = known_units;
      MakeKnownUnit( "A", "Ampere", NULL, status );
      MakeKnownUnit( "mol", "mole", NULL, status );
      MakeKnownUnit( "cd", "candela", NULL, status );

/* Now do all IAU derived units. Unit definitions may only refer to units
   which have already been defined. */
      MakeKnownUnit( "sr", "steradian", "rad rad", status );
      MakeKnownUnit( "Hz", "Hertz", "1/s", status );
      MakeKnownUnit( "N", "Newton", "kg m/s**2", status );
      MakeKnownUnit( "J", "Joule", "N m", status );
      MakeKnownUnit( "W", "Watt", "J/s", status );
      MakeKnownUnit( "C", "Coulomb", "A s", status );
      MakeKnownUnit( "V", "Volt", "J/C", status );
      MakeKnownUnit( "Pa", "Pascal", "N/m**2", status );
      MakeKnownUnit( "Ohm", "Ohm", "V/A", status );
      MakeKnownUnit( "S", "Siemens", "A/V", status );
      MakeKnownUnit( "F", "Farad", "C/V", status );
      MakeKnownUnit( "Wb", "Weber", "V s", status );
      MakeKnownUnit( "T", "Tesla", "Wb/m**2", status );
      MakeKnownUnit( "H", "Henry", "Wb/A", status );
      MakeKnownUnit( "lm", "lumen", "cd sr", status );
      MakeKnownUnit( "lx", "lux", "lm/m**2", status );

/* Now do additional derived and basic units listed in the FITS-WCS paper. */
      MakeKnownUnit( "deg", "degree", "pi/180 rad", status );
      MakeKnownUnit( "arcmin", "arc-minute", "1/60 deg", status );
      MakeKnownUnit( "arcsec", "arc-second", "1/3600 deg", status );
      MakeKnownUnit( "mas", "milli-arcsecond", "1/3600000 deg", status );
      MakeKnownUnit( "min", "minute", "60 s", status );
      MakeKnownUnit( "h", "hour", "3600 s", status );
      MakeKnownUnit( "d", "day", "86400 s", status );
      MakeKnownUnit( "yr", "year", "31557600 s", status );
      MakeKnownUnit( "a", "year", "31557600 s", status );
      MakeKnownUnit( "eV", "electron-Volt", "1.60217733E-19 J", status );
      MakeKnownUnit( "erg", "erg", "1.0E-7 J", status );
      MakeKnownUnit( "Ry", "Rydberg", "13.605692 eV", status );
      MakeKnownUnit( "solMass", "solar mass", "1.9891E30 kg", status );
      MakeKnownUnit( "u", "unified atomic mass unit", "1.6605387E-27 kg", status );
      MakeKnownUnit( "solLum", "solar luminosity", "3.8268E26 W", status );
      MakeKnownUnit( "Angstrom", "Angstrom", "1.0E-10 m", status );
      MakeKnownUnit( "micron", "micron", "1.0E-6 m", status );
      MakeKnownUnit( "solRad", "solar radius", "6.9599E8 m", status );
      MakeKnownUnit( "AU", "astronomical unit", "1.49598E11 m", status );
      MakeKnownUnit( "lyr", "light year", "9.460730E15 m", status );
      MakeKnownUnit( "pc", "parsec", "3.0867E16 m", status );
      MakeKnownUnit( "count", "count", NULL, status );
      quant_units[ iq++ ] = known_units;
      MakeKnownUnit( "adu", "analogue-to-digital unit", NULL, status );
      quant_units[ iq++ ] = known_units;
      MakeKnownUnit( "photon", "photon", NULL, status );
      quant_units[ iq++ ] = known_units;
      MakeKnownUnit( "Jy", "Jansky", "1.0E-26 W /m**2 /Hz", status );
      MakeKnownUnit( "mag", "magnitude", NULL, status );
      quant_units[ iq++ ] = known_units;
      MakeKnownUnit( "G", "Gauss", "1.0E-4 T", status );
      MakeKnownUnit( "pixel", "pixel", NULL, status );
      quant_units[ iq++ ] = known_units;
      MakeKnownUnit( "barn", "barn", "1.0E-28 m**2", status );
      MakeKnownUnit( "D", "Debye", "(1.0E-29/3) C.m", status );

      if( iq != NQUANT && astOK ) {
         astError( AST__INTER, "unit(GetKnownUnits): %d basic quantities "
                   "noted but this should be %d (internal AST programming "
                   "error).", status, iq, NQUANT );
      }

/* Unit aliases... */
      MakeUnitAlias( "Angstrom", "Ang", status );
      MakeUnitAlias( "count", "ct", status );
      MakeUnitAlias( "photon", "ph", status );
      MakeUnitAlias( "Jy", "Jan", status );
      MakeUnitAlias( "pixel", "pix", status );
      MakeUnitAlias( "s", "sec", status );
      MakeUnitAlias( "m", "meter", status );
   }

/* If succesful, return the pointer to the head of the list. */
   if( astOK ) result = known_units;

/* Allow the next thread to proceed. */
   if( lock ) {
      UNLOCK_MUTEX1
   }

/* Return the result. */
   return result;
}

static Multiplier *GetMultipliers( int *status ) {
/*
*  Name:
*     GetMultiplier

*  Purpose:
*     Get a pointer to the head of a linked list of multiplier definitions.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     Multiplier *Multipliers( void )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function returns a pointer to the head of a linked list of known
*     multiplier definitions. The multiplier definitions are created as
*     static module variables if they have not previously been created.

*  Returned Value:
*     A pointer to the first known multiplier definition.

*  Notes:
*     - A NULL pointer is returned if it is invoked with the global error
*     status set, or if an error occurs.
*/

/* Local Variables: */
   Multiplier *result;
   Multiplier *mult;

/* Initialise. */
   result = NULL;

/* Check inherited status. */
   if( !astOK ) return result;

/* Ensure the list is only initialised by one thread. */
   LOCK_MUTEX2

/* If the linked list of Multiplier structures describing the known
   multipliers has not yet been created, create it now. A pointer to the
   head of the linked list is put into the static variable "multipliers". */
   if( !multipliers ) {

/* Define a macro to create a multiplier struncture and add it to the
   linked list of multiplier structures. */
#define MAKEMULT(s,sl,sc,lab,ll) \
      mult = astMalloc( sizeof( Multiplier ) ); \
      if( astOK ) { \
         mult->sym = s; \
         mult->symlen = sl; \
         mult->lablen = ll; \
         mult->scale = sc; \
         mult->label = lab; \
         mult->next = multipliers; \
         multipliers = mult; \
      }

/* Use the above macro to create all the standard multipliers listed in the
   FITS WCS paper I. */
      MAKEMULT("d",1,1.0E-1,"deci",4)
      MAKEMULT("c",1,1.0E-2,"centi",5)
      MAKEMULT("m",1,1.0E-3,"milli",5)
      MAKEMULT("u",1,1.0E-6,"micro",5)
      MAKEMULT("n",1,1.0E-9,"nano",4)
      MAKEMULT("p",1,1.0E-12,"pico",4)
      MAKEMULT("f",1,1.0E-15,"femto",5)
      MAKEMULT("a",1,1.0E-18,"atto",4)
      MAKEMULT("z",1,1.0E-21,"zepto",5)
      MAKEMULT("y",1,1.0E-24,"yocto",5)
      MAKEMULT("da",2,1.0E1,"deca",4)
      MAKEMULT("h",1,1.0E2,"hecto",5)
      MAKEMULT("k",1,1.0E3,"kilo",4)
      MAKEMULT("M",1,1.0E6,"mega",4)
      MAKEMULT("G",1,1.0E9,"giga",4)
      MAKEMULT("T",1,1.0E12,"tera",4)
      MAKEMULT("P",1,1.0E15,"peta",4)
      MAKEMULT("E",1,1.0E18,"exa",3)
      MAKEMULT("Z",1,1.0E21,"zetta",5)
      MAKEMULT("Y",1,1.0E24,"yotta",5)

/* Undefine the macro. */
#undef MAKEMULT

   }

/* If succesful, return the pointer to the head of the list. */
   if( astOK ) result = multipliers;

/* Allow the next thread to proceed. */
   UNLOCK_MUTEX2

/* Return the result. */
   return result;
}

static void InvertConstants( UnitNode **node, int *status ) {
/*
*  Name:
*     InvertConstants

*  Purpose:
*     Take the reciprocal of all constants in a tree of UnitNodes.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     void InvertConstants( UnitNode **node, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function replaces constant unit coefficients by their reciprocal.
*     This is because a string such as "0.01 m" will be interpreted as
*     meaning "multiply a value in metres by 0.01 to get the value in the
*     required units", whereas what is actually meant is "use units of
*     0.01 of a metre" which requires us to divide the value in metres by
*     0.01, not multiply it.

*  Parameters:
*     node
*        The address of a pointer to the UnitNode at the head of the tree.
*        On exit the supplied tree is freed and a pointer to a new tree is
*        placed at the given address.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   int i;
   UnitNode *newnode;
   int allcon;
   Oper op;

/* Check inherited status and pointer. */
   if( !astOK || !node || !(*node) ) return;

/* Initiallially, we have no replacement node */
   newnode = NULL;

/* There is nothing to fix if the node has no arguments. */
   if( (*node)->narg > 0 ) {

/* Note the op code for the node. */
      op = (*node)->opcode;

/* Fix up the argument nodes. Also note if all the arguments are
   constants. */
      allcon = 1;
      for( i = 0; i < (*node)->narg; i++ ) {
         InvertConstants( &( (*node)->arg[ i ] ), status );
         if( (*node)->arg[ i ]->con == AST__BAD ) allcon = 0;
      }

/* If all nodes are constant, there are no co-efficients to invert. */
      if( !allcon ) {

/* Iif this is a multiplication node, see if either of its arguments
   is a constant. If so, invert the constant. This is because a string like
   "0.01 m" means "each unit is 0.01 of a metre". Therefore, to transform
   a value in metres into required units means multiplying the metres
   value by 100.0 (i.e the reciprocal of 0.01), not 0.01. */
         if( op == OP_MULT ) {
            for( i = 0; i < 2; i++ ) {
               if( (*node)->arg[ i ]->con != AST__BAD ) {
                  if( (*node)->arg[ i ]->con != 0.0 ) {

                     (*node)->arg[ i ]->con = 1.0/(*node)->arg[ i ]->con;
                  } else {
                     astError( AST__BADUN, "Illegal zero constant encountered." , status);
                  }
               }
            }

/* Likewise, check for division nodes in which the denominator is
   constant. */
         } else if( op == OP_DIV ) {
            if( (*node)->arg[ 1 ]->con != AST__BAD ) {
               if( (*node)->arg[ 1 ]->con != 0.0 ) {
                  (*node)->arg[ 1 ]->con = 1.0/(*node)->arg[ 1 ]->con;
               } else {
                  astError( AST__BADUN, "Illegal zero constant encountered." , status);
               }
            }

/* If this is a "pow" node check that the second argument is constant
   (required by FITS WCS paper I). */
         } else if( op == OP_POW ) {
            if( (*node)->arg[ 1 ]->con == AST__BAD ) {
               astError( AST__BADUN, "Illegal variable exponent." , status);
            }
         }
      }
   }

/* If an error has occurred, free any new node. */
   if( !astOK ) newnode = FreeTree( newnode, status );

/* If we have a replacement node, free the supplied tree and return a
   pointer to the new tree. */
   if( newnode ) {
      FreeTree( *node, status );
      *node = newnode;
   }
}

static UnitNode *InvertTree( UnitNode *fwdnode, UnitNode *src, int *status ) {
/*
*  Name:
*     InvertTree

*  Purpose:
*     Invert a tree of UnitNodes.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     UnitNode *InvertTree( UnitNode *fwdnode, UnitNode *src )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function inverts a tree of UnitNodes. The supplied tree should
*     have exactly one OP_LDVAR node. This will be the quantity represented
*     by the node at the head of the returned tree.

*  Parameters:
*     fwdnode
*        A pointer to the UnitNode at the head of the tree which is to be
*        inverted.
*     src
*        A pointer to a UnitNode which is to be used as the root of the
*        inverted tree. That is, the output from this node should form
*        the (one and only) varying input to the inverted tree. If the
*        supplied tree is succesfulyl inverted, the tree of which "src"
*        is the head will be contained within the returned inverted tree.
*        Therefore "src" only needs to be freed explicitly if this
*        function fails to invert the supplied tree for any reason. If
*        this function succeeds, then "src" will be freed as part of
*        freeing the returned inverted tree.

*  Returned Value:
*     A pointer to a UnitNode which forms the head of the inverted tree.

*  Algorithm:
*     The algorithm works through the supplied forward tree, from the head
*     to the roots. First, the supplied node at the head of the forward
*     tree is inverted. To be invertable, the supplied head node must have
*     exactly one varying argument (any other arguments must be fixed,
*     i.e. not vary). This varying argument becomes the output of the
*     inverted node. The other (fixed) arguments to the forward node are
*     also used as arguments to the inverted node. The supplied "src" node
*     is used as the single varying input to the inverted node. Having
*     inverted the supplied forward head node, this function is called
*     recursively to invert the lower parts of the forward tree (i.e. the
*     part of the forward tree which provided the varying input to node
*     which has just been inverted).

*  Notes:
*     - It is assumed that he supplied forward tree has been simplified
*     using SimplifyTree. This means that the tree contains no nodes with
*     the following op codes: OP_LOG, OP_SQRT. OP_DIV (SimplifyTree
*     converts these nodes into OP_LN, OP_POW and OP_MULT nodes).
*     - A value of NULL will be returned if this function is invoked with
*     the global error status set, or if it should fail for any reason.

*/

/* Local Variables: */
   UnitNode *newnode;
   UnitNode *nextnode;
   UnitNode *result;
   UnitNode *node1;
   Oper fop;
   int varg;

/* Initialise */
   result = NULL;

/* Check inherited status. */
   if( !astOK ) return result;

/* Initiallially, we have no replacement node */
   newnode = NULL;
   nextnode = NULL;

/* Save the op code at the head of the forward tree. */
   fop = fwdnode->opcode;

/* If the head of the forward tree is a OP_EXP node. Inverse of
   "exp(x)" is "ln(x)". */
   if( fop == OP_EXP ) {
      newnode = NewNode( NULL, OP_LN, status );
      if( astOK ) {
         newnode->arg[ 0 ] = src;
         nextnode = fwdnode->arg[ 0 ];
      }

/* If the head of the forward tree is a OP_LN node. Inverse of
   "ln(x)" is "exp(x)". */
   } else if( fop == OP_LN ) {
      newnode = NewNode( NULL, OP_EXP, status );
      if( astOK ) {
         newnode->arg[ 0 ] = src;
         nextnode = fwdnode->arg[ 0 ];
      }

/* If the head of the forward tree is a OP_POW node. Inverse of
   "x**k" is "x**(1/k)" */
   } else if( fop == OP_POW ) {
      newnode = NewNode( NULL, OP_POW, status );
      node1 = NewNode( NULL, OP_LDCON, status );
      if( astOK ) {
         node1->con = 1.0/fwdnode->arg[ 1 ]->con;
         newnode->arg[ 0 ] = src;
         newnode->arg[ 1 ] = node1;
         nextnode = fwdnode->arg[ 0 ];
      }

/* If the head of the forward tree is a OP_MULT node... */
   } else if( fop == OP_MULT ) {

/* The node is only invertable if it has one constant node and one
   non-constant node. Get the index of the varying argument. */
      if( fwdnode->arg[ 0 ]->con != AST__BAD &&
          fwdnode->arg[ 1 ]->con == AST__BAD ) {
         varg = 1;
      } else if( fwdnode->arg[ 0 ]->con == AST__BAD &&
                 fwdnode->arg[ 1 ]->con != AST__BAD ) {
         varg = 0;
      } else {
         varg = -1;
      }
      if( varg != -1 ) {

/* The inverse of "k*x" is "(1/k)*x" (we use MULT nodes instead of DIV
   nodes to maintain the standardisation implemented by SimplifyTree). */
         newnode = NewNode( NULL, OP_MULT, status );
         node1 = NewNode( NULL, OP_LDCON, status );
         if( astOK ) {
            node1->con = 1.0/fwdnode->arg[ 1 - varg ]->con;
            newnode->arg[ 0 ] = node1;
            newnode->arg[ 1 ] = src;
            nextnode = fwdnode->arg[ varg ];
         }
      }

/* If the head of the forward tree is a OP_LDVAR node, there is nothing
   left to invert. SO return a pointer to the suppleid source node. */
   } else if( fop == OP_LDVAR ) {
      result = src;
      nextnode = NULL;

/* If the head of the forward tree is any other node (e.g. a OP_LDCON node),
   the tree cannot be inverted. */
   } else {
      nextnode = NULL;
   }

/* If we managed to invert the node at the head of the supplied tree,
   continue to invert its varying argument node (if any). */
   if( nextnode && newnode ) result = InvertTree( nextnode, newnode, status );

/* If the tree could not be inverted, free the newnode. */
   if( !result ) newnode = FreeTree( newnode, status );

/* If an error has occurred, free any new node. */
   if( !astOK ) result = FreeTree( result, status );

/* Return the result. */
   return result;

}

static void LocateUnits( UnitNode *node, UnitNode ***units, int *nunits, int *status ){
/*
*  Name:
*     LocateUnits

*  Purpose:
*     Locate the units used by a supplied tree of UnitNodes.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     void LocateUnits( UnitNode *node, UnitNode ***units, int *nunits, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function locates the units used by a supplied tree of
*     UnitNodes.

*  Parameters:
*     node
*        A pointer to the UnitNode at the head of the tree to be searched.
*     units
*        The address at which is stored a pointer to an array of "*nunits"
*        elements. Each element of the array holds a pointer to a UnitNode.
*        The array is extended on exit to hold pointers to the OP_LDVAR nodes
*        within the supplied tree (i.e. nodes which represent named units,
*        either known or unknown). A node is only included in the returned
*        array if no other node for the same unit is already included in the
*        array. A NULL pointer should be supplied on the first invocation of
*        this function.
*     nunits
*        The address of an integer which holds the number of elements in
*        the array given by "*units". Updated on exit to included any
*        elements added to the array. Zero should be supplied on the first
*        invocation of this function.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   int i;
   int found;

/* Check the global error status. */
   if( !astOK ) return;

/* Is the node at the head of the supplied tree an OP_LDVAR node? */
   if( node->opcode == OP_LDVAR ) {

/* If an array was supplied, see if it already contains a pointer to a node
   which refers to the same units. */
      found = 0;
      if( *units ) {
         for( i = 0; i < *nunits; i++ ) {
            if( !strcmp( (*units)[ i ]->name, node->name ) ) {
               found = 1;
               break;
            }
         }
      }

/* If not, ensure the array is big enough and add a pointer to the
   supplied node to the array. */
      if( !found ) {
         *units = astGrow( *units, *nunits + 1, sizeof( UnitNode * ) );
         if( astOK ) (*units)[ (*nunits)++ ] = node;
      }

/* If the supplied node is not an OP_LDVAR node, call this function
   recursively to search the argument sub-trees. */
   } else {
      for( i = 0; i < node->narg; i++ ) {
         LocateUnits( node->arg[ i ], units, nunits, status );
      }
   }
}

static const char *MakeExp( UnitNode *tree, int mathmap, int top, int *status ) {
/*
*  Name:
*     MakeExp

*  Purpose:
*     Make an algebraic expression from a supplied tree of UnitNodes.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     const char *MakeExp( UnitNode *tree, int mathmap, int top, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function produces a string holding an algebraic expression
*     corresponding to a supplied tree of UnitNodes.

*  Parameters:
*     tree
*        A pointer to the UnitNode at the head of the tree to be converted
*        into an algebraic expression.
*     mathmap
*        If zero, format as an axis label expression. If 1, format as a
*        MathMap expression. If 2, format as a FITS unit string.
*     top
*        Should be non-zero for a top-level entry to this function, and
*        zero for a recursive entry.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the cleaned expression, which should be freed using
*     astFree when no longer needed.

*  Notes:
*     - This function returns NULL if it is invoked with the global error
*     status set, or if it should fail for any reason.
*/

/* Local Variables: */
   UnitNode *newtree;
   UnitNode *sunit;
   char *a;
   char *result;
   char buff[200];
   const char *arg0;
   const char *arg1;
   const char *mtxt;
   int larg0;
   int larg1;
   int lbuff;
   int mlen;
   int par;
   int tlen;

/* Check inherited status. */
   result = NULL;
   if( !astOK ) return result;

/* Modify the tree to make the resulting transformation functions more
   natural to human readers. */
   newtree = CopyTree( tree, status );
   ComplicateTree( &newtree, status );

/* If we are producing an axis label... */
   if( !mathmap ) {

/* Fix all multiplicative constants to 1.0 if they multiply an OP_LDVAR
   OP_SQRT or OP_POW node. This is on the assumption that the returned label
   should not include any simple unit scaling (e.g. if the output label would
   be "2.345*wavelength", we prefer simply to use "wavelength" since a scaled
   wavelength is still a wavelength - i.e. simple scaling does not change
   the dimensions of a quantity). */
      FixConstants( &newtree, 1, status );

/* Simplify the tree again to get rid of the 1.0 terms which may have
   been introduced by the previous line (but do not re-introduce any
   standardisations - removing them was the reason for calling ComplicateTree).
   If this simplication introduces any changes, try fixing multiplicative
   constants again, and so on, until no more changes occur. */
      while( SimplifyTree( &newtree, 0, status ) ) {
         FixConstants( &newtree, 1, status );
      }

   }

/* Produce a string describing the action performed by the UnitNode at
   the head of the supplied tree, and then invoke this function recursively
   to format any arguments of the head node. */

/* Constant valued nodes... just format the constant in a local buffer and
   then copy the buffer. */
   if( newtree->con != AST__BAD ) {
      lbuff = sprintf( buff, "%.*g", DBL_DIG, newtree->con );
      result = astStore( NULL, buff, lbuff + 1 );

/* "Load Variable Value" nodes - return the variable name. If this is a
   recursive call to this function, and we are producing a label, append a
   single space before and after the name. */
   } else if( newtree->opcode ==  OP_LDVAR ) {
      tlen = strlen( newtree->name );

      if( !mathmap && !top ){
         result = astMalloc( tlen + 3 );
         if( result ) {
            result[ 0 ] = ' ';
            memcpy( result + 1, newtree->name, tlen );
            memcpy( result + tlen + 1, " ", 2 );
         }

      } else if( mathmap == 2 ) {

         if( newtree->mult ) {
           mlen = newtree->mult->symlen;
           mtxt = newtree->mult->sym;
         } else {
           mlen = 0;
           mtxt = NULL;
         }

         result = astMalloc( tlen + 1 + mlen );
         if( result ) {
            if( mtxt ) memcpy( result, mtxt, mlen );
            memcpy( result + mlen, newtree->name, tlen + 1 );
         }

      } else {
         result = astStore( NULL, newtree->name, tlen + 1 );
      }

/* Single argument functions... place the argument in parentheses after
   the function name. */
   } else if( newtree->opcode ==  OP_LOG ) {
      arg0 = MakeExp( newtree->arg[ 0 ], mathmap, 0, status );
      larg0 = strlen( arg0 );
      if( mathmap == 1 ) {
         result = astMalloc( larg0 + 8 );
         if( result ) memcpy( result, "log10(", 7 );
         a = result + 6;
      } else {
         result = astMalloc( larg0 + 6 );
         if( result ) memcpy( result, "log(", 5 );
         a = result + 4;
      }
      if( result ){
         memcpy( a, arg0, larg0 + 1 );
         memcpy( a + larg0, ")", 2 );
      }
      arg0 = astFree( (void *) arg0 );

   } else if( newtree->opcode ==  OP_LN ) {
      arg0 = MakeExp( newtree->arg[ 0 ], mathmap, 0, status );
      larg0 = strlen( arg0 );
      if( mathmap == 1 ) {
         result = astMalloc( larg0 + 6 );
         if( result ) memcpy( result, "log(", 5 );
         a = result + 4;
      } else {
         result = astMalloc( larg0 + 5 );
         if( result ) memcpy( result, "ln(", 4 );
         a = result + 3;
      }
      if( astOK ){
         memcpy( a, arg0, larg0 );
         memcpy( a + larg0, ")", 2 );
      }
      arg0 = astFree( (void *) arg0 );

   } else if( newtree->opcode ==  OP_EXP ) {
      arg0 = MakeExp( newtree->arg[ 0 ], mathmap, 0, status );
      larg0 = strlen( arg0 );
      result = astMalloc( larg0 + 6 );
      if( result ){
         memcpy( result, "exp(", 5 );
         memcpy( result + 4, arg0, larg0 );
         memcpy( result + 4 + larg0, ")", 2 );
      }
      arg0 = astFree( (void *) arg0 );

   } else if( newtree->opcode ==  OP_SQRT ) {
      arg0 = MakeExp( newtree->arg[ 0 ], mathmap, 0, status );
      larg0 = strlen( arg0 );
      result = astMalloc( larg0 + 7 );
      if( result ){
         memcpy( result, "sqrt(", 6 );
         memcpy( result + 5, arg0, larg0 );
         memcpy( result + 5 + larg0, ")", 2 );
      }
      arg0 = astFree( (void *) arg0 );

/* POW... the exponent (arg[1]) is always a constant and so does not need
   to be placed in parentheses. The first argument only needs to be
   placed in parentheses if it is a two arg node (except we also put it
   in parentheses if it is an OP_LDVAR node and "mathmap" is zero - this is
   because such OP_LDVAR nodes will correspond to axis labels which will
   have spaces before and after them which would look odd if not encloses
   in parentheses). */
   } else if( newtree->opcode ==  OP_POW ) {

      arg0 = MakeExp( newtree->arg[ 0 ], mathmap, 0, status );
      larg0 = strlen( arg0 );

      arg1 = MakeExp( newtree->arg[ 1 ], mathmap, 0, status );
      larg1 = strlen( arg1 );

      if( newtree->arg[ 0 ]->narg == 2 ||
          (newtree->arg[ 0 ]->opcode == OP_LDVAR && !mathmap) ) {
         par = 1;
         result = astMalloc( larg0 + larg1 + 7 );
         if( result ) memcpy( result, "(", 2 );
         a = result + 1;
      } else {
         par = 0;
         result = astMalloc( larg0 + larg1 + 5 );
         a = result;
      }

      if( result ) {
         memcpy( a, arg0, larg0 );
         a += larg0;
         if( par ) *(a++) = ')';
         memcpy( a, "**", 3 );
         a += 2;
         memcpy( a, arg1, larg1 );
         a += larg1;
         *a = 0;
      }

      arg0 = astFree( (void *) arg0 );
      arg1 = astFree( (void *) arg1 );

/* DIV... the first argument (numerator) never needs to be in parentheses.
   The second argument (denominator) only needs to be placed in parentheses
   if it is a MULT node. */
   } else if( newtree->opcode ==  OP_DIV ) {

      if( mathmap == 2 && ( sunit = ModifyPrefix( newtree, status ) ) ) {
         result = (char *) MakeExp( sunit, mathmap, 0, status );
         sunit = FreeTree( sunit, status );

      } else {
         arg0 = MakeExp( newtree->arg[ 0 ], mathmap, 0, status );
         larg0 = strlen( arg0 );

         arg1 = MakeExp( newtree->arg[ 1 ], mathmap, 0, status );
         larg1 = strlen( arg1 );

         if( newtree->arg[ 1 ]->opcode == OP_MULT &&
             strchr( arg1, '*' ) ) {
            par = 1;
            result = astMalloc( larg0 + larg1 + 4 );
         } else {
            par = 0;
            result = astMalloc( larg0 + larg1 + 2 );
         }

         if( result ) {
            memcpy( result, arg0, larg0 );
            a = result + larg0;
            *(a++) = '/';
            if( par ) *(a++) = '(';
            memcpy( a, arg1, larg1 );
            a += larg1;
            if( par ) *(a++) = ')';
            *a = 0;
         }

         arg0 = astFree( (void *) arg0 );
         arg1 = astFree( (void *) arg1 );
      }

/* MULT... the second argument never needs to be in parentheses. The first
   argument only needs to be placed in parentheses if it is a DIV or POW
   node. */
   } else if( newtree->opcode ==  OP_MULT ) {
      if( mathmap == 2 && ( sunit = ModifyPrefix( newtree, status ) ) ) {
         result = (char *) MakeExp( sunit, mathmap, 0, status );
         sunit = FreeTree( sunit, status );

      } else {
         arg0 = MakeExp( newtree->arg[ 0 ], mathmap, 0, status );
         larg0 = strlen( arg0 );

         arg1 = MakeExp( newtree->arg[ 1 ], mathmap, 0, status );
         larg1 = strlen( arg1 );

/* If this is a top-level entry and we are producing an axis label, do
   not include any constant multiplicative terms. */
         if( top && !mathmap ) {
            if( newtree->arg[ 0 ]->con != AST__BAD ) arg0 = astFree( (void *) arg0 );
            if( newtree->arg[ 1 ]->con != AST__BAD ) arg1 = astFree( (void *) arg1 );
         }

/* If we have two arguments, concatentate them, placing the operands in
   parentheses if necessary. */
         if( arg0 && arg1 ) {

            if( ( newtree->arg[ 0 ]->opcode == OP_DIV &&
                  strchr( arg0, '/' ) ) ||
                ( newtree->arg[ 0 ]->opcode == OP_POW &&
                  strstr( arg0, "**" ) ) ) {
               par = 1;
               result = astMalloc( larg0 + larg1 + 4 );
               if( result ) result[ 0 ] = '(';
               a = result + 1;
            } else {
               par = 0;
               result = astMalloc( larg0 + larg1 + 2 );
               a = result;
            }

            if( result ) {
               memcpy( a, arg0, larg0 );
               a += larg0;
               if( par ) *(a++) = ')';
               *(a++) = '*';
               memcpy( a, arg1, larg1 );
               a += larg1;
               *a = 0;
            }

            arg0 = astFree( (void *) arg0 );
            arg1 = astFree( (void *) arg1 );

/* If we do not have two arguments, just return the one we do have. */
         } else if( arg0 ){
            result = (char *) arg0;

         } else {
            result = (char *) arg1;
         }
      }
   }

/* Free the complicated tree. */
   newtree = FreeTree( newtree, status );

/* Free the returned string if an error has occurred. */
   if( !astOK ) result = astFree( result );

/* Return the result. */
   return (const char *) result;
}

static void MakeKnownUnit( const char *sym, const char *label, const char *exp, int *status ){
/*
*  Name:
*     MakeKnownUnit

*  Purpose:
*     Create a KnownUnit structure describing a known unit.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     void MakeKnownUnit( const char *sym, const char *label, const char *exp, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function creates a KnownUnit structure decribing a known unit,
*     and adds it to the head of the linked list of known units stored in
*     a module variable.

*  Parameters:
*     sym
*        A pointer to a string which can be used as a symbol to represent
*        the new named unit. Once defined, this symbol can be included within
*        the definition of other derived units. The string should contain
*        only alphabetical characters (no digits, spaces, punctuation,
*        etc). Symbols are case sensitive (e.g. "s" is second, but "S" is
*        Siemens). The string should not include any multiplier prefix.
*     label
*        Pointer to a null terminated string containing the label for
*        the required units. No restriction on content.
*     exp
*        This should be a pointer to a null terminated string containing
*        a definition of the required unit. See the description of the
*        "in" and "out" parameters for the astUnitMapper function.
*
*        A NULL pointer or a blank string may supplied for "exp", which
*        is interpreted as a request for a new basic unit to be created with
*        the symbol and label given by the other parameters.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     -  The supplied symbol and label strings are not copied. The
*     supplied pointers are simply stored in the returned structure.
*     Therefore the strings to which the pointers point should not be
*     modified after this function returned (in fact this function is
*     always called with literal strings for these arguments).
*/

/* Local Variables: */
   KnownUnit *result;

/* Check the global error status. */
   if( !astOK ) return;

/* Indicate that subsequent memory allocations may never be freed (other
   than by any AST exit handler). */
   astBeginPM;

/* Allocate memory for the structure, and check the returned pointer can
   be used safely. */
   result = astMalloc( sizeof( KnownUnit ) );
   if( astOK ) {

/* In case of errors, first nullify the pointer to the next KnownUnit. */
      result->next = NULL;

/* Store the supplied label and symbol pointers. */
      result->sym = sym;
      result->label = label;

/* Store the length of the symbol (without the trailing null character). */
      result->symlen = strlen( sym );

/* Store the length of the label (without the trailing null character). */
      result->lablen = strlen( label );

/* Create a tree of UnitNodes describing the unit if an expression was
   supplied. */
      result->head = exp ? CreateTree( exp, 1, 0, status ) : NULL;

/* Unit aliases are replaced in use by the KnownUnit pointed to by the
   "use" component of the structure. Indicate this KnownUnitis not an
    alias by setting its "use" component NULL. */
      result->use = NULL;
   }

/* Mark the end of the section in which memory allocations may never be
   freed (other than by any AST exit handler). */
   astEndPM;

/* If an error has occurred, free any returned structure. */
   if( !astOK ) {
      result->head = FreeTree( result->head, status );
      result = astFree( result ) ;

/* Otherwise, add the new KnownUnit to the head of the linked list of
   known units. */
   } else {
      result->next = known_units;
      known_units = result;
   }

}

static AstMapping *MakeMapping( UnitNode *tree, int *status ) {
/*
*  Name:
*     MakeMapping

*  Purpose:
*     Create a new Mapping from a given tree of UnitNodes.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     AstMapping *MakeMapping( UnitNode *tree )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function creates a Mapping with a forward transformation equal
*     to the transformation described by the tree of UnitNodes. The head
*     node of the tree corresponds to the output of the Mapping.

*  Parameters:
*     tree
*        The UnitNode at the head of the tree to be used. It should have
*        exactly one OP_LDVAR node, and should have been simplified using
*        the SimplifyTree function.

*  Returned Value:
*     A pointer to the Mapping. Its Nin and Nout attributes will both be 1.

*  Notes:
*     - A value of NULL will be returned if this function is invoked with
*     the global error status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstMapping *result;
   UnitNode *inv;
   UnitNode *src;
   const char *fwdexp;
   char *fwdfun;
   const char *invexp;
   char *invfun;
   int lfwd;
   int linv;

/* Initialise. */
   result = NULL;

/* Check the inherited status. */
   if( !astOK ) return result;

/* First see if a UnitMap can be used to represent the Mapping from
   input units to output units. This will be the case if the supplied tree
   consists of a aingle OP_LDVAR node (corresponding to the input units). */
   if( tree->opcode == OP_LDVAR ) {
      result = (AstMapping *) astUnitMap( 1, "", status );

/* Now see if a UnitMap or ZoomMap can be used to represent the Mapping from
   input units to output units. This will be the case if the supplied tree
   consists of a OP_MULT node with one constant argument and on OP_LDVAR
   argument (corresponding to the input units). The standardisation done by
   SimplifyTree will have ensured that the constant will be argument 0
   (and will also have converted "x/k" trees into "(1/k)*x" trees). */
   } else if( tree->opcode == OP_MULT &&
              tree->arg[ 0 ]->con != AST__BAD &&
              tree->arg[ 1 ]->opcode == OP_LDVAR ) {

      if( tree->arg[ 0 ]->con == 1.0 ) {
         result = (AstMapping *) astUnitMap( 1, "", status );
      } else {
         result = (AstMapping *) astZoomMap( 1, tree->arg[ 0 ]->con, "", status );
      }

/* For other trees we need to create a MathMap. */
   } else {

/* Format the supplied tree as an algebraic expression, and get its length. */
      fwdexp = MakeExp( tree, 1, 1, status );
      lfwd = strlen( fwdexp );

/* The MathMap constructor requires the forward and inverse
   transformation functions to be specified as equations (i.e. including an
   equals sign). We use the output variable name "output_units" (the
   astUnitMapper function creates the supplied tree usign the variable
   name "input_units" ). */
      lfwd += 13;

/* Invert the supplied tree and create an algebraic expression from it. */
      src = NewNode( NULL, OP_LDVAR, status );
      if( astOK ) src->name = astStore( NULL, "output_units", 13 );
      inv = InvertTree( tree, src, status );
      if( !inv ) {
         src = FreeTree( src, status );
         astError( AST__BADUN, "MakeMapping(Unit): Failed to invert "
                   "supplied tree '%s' (internal AST programming error).", status,
                   fwdexp );

/* If inverted succesfully (which it should be since astUnitMapper should
   have checked this)... */
      } else {

/* Format the inverted tree as an algebraic expression, and get its
   length, adding on extra characters for the variable name ("input_units")
   and equals sign. */
         invexp = MakeExp( inv, 1, 1, status );
         linv = strlen( invexp );
         linv += 12;

/* Allocate memory for the transformation functions, plus an extra
   character for the trailing null. */
         fwdfun = astMalloc( lfwd + 1 );
         invfun = astMalloc( linv + 1 );
         if( invfun ) {
            memcpy( fwdfun, "output_units=", 14 );
            memcpy( invfun, "input_units=", 13 );

/* Append the expressions following the equals signs. */
            strcpy( fwdfun + 13, fwdexp );
            strcpy( invfun + 12, invexp );

/* Create the MathMap. */
            result = (AstMapping *) astMathMap( 1, 1, 1,
                                                (const char **) &fwdfun, 1,
                                                (const char **) &invfun,
                                                "SimpFI=1,SimpIF=1", status );
         }

/* Free resources. */
         inv = FreeTree( inv, status );
         fwdfun = astFree( fwdfun );
         invfun = astFree( invfun );
         invexp = astFree( (void *) invexp );
      }
      fwdexp = astFree( (void *) fwdexp );
   }

/* Free any result if an error occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return the answer. */
   return result;
}

static UnitNode *MakeLabelTree( const char *lab, int nc, int *status ){
/*
*  Name:
*     MakeLabelTree

*  Purpose:
*     Convert an axis label into a tree of UnitNodes.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     UnitNode *MakeLabelTree( const char *lab, int nc, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function converts an axis label into a tree of UnitNodes.
*     It is assumed the supplied label represents some "basic" label
*     modified by the application of one or more single function arguments
*     and/or exponentiation operators. The (single) OP_LDVAR node in the
*     returned tree refers to the basic label (it is stored as the "name"
*     component of UnitNode structure).

*  Parameters:
*     lab
*        The label expression.
*     nc
*        The number of characters from "lab" to use.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a UnitNode which forms the head of a tree of UnitNodes
*     representing the supplied label expression.

*  Notes:
*     -  A NULL value is returned if this function is invoked with the
*     global error status set or if it should fail for any reason.
*/

/* Local Variables: */
   Oper op;
   UnitNode *result;
   char buff[ 10 ];
   const char *c;
   const char *exp;
   int depth;
   int i;
   int oplen;
   int n;
   double con;

/* Initialise */
   result = NULL;
   oplen = 0;

/* Check the global error status, and that we have a string. */
   if ( !astOK || !lab || !nc ) return result;

/* Get a pointer to the first non-blank character, and store the number of
   characters to examine (this excludes any trailing white space). */
   exp = lab;
   while( isspace( *exp ) ) exp++;
   c = lab + nc - 1;
   while( c >= exp && isspace( *c ) ) c--;
   nc = c - exp + 1;

/* Scan through the supplied string looking for the first pow operator at
   zero depth of nesting within parentheses. */
   depth = 0;
   c = exp;
   i = 0;
   op = OP_NULL;
   while( i < nc && *c ){

/* If this character is an opening parenthesis, increment the depth of
   nesting. */
      if( *c == '(' ) {
         depth++;

/* If this character is an closing parenthesis, decrement the depth of
   nesting. Report an error if it ever goes negative. */
      } else if( *c == ')' ) {
         depth--;
         if( depth < 0 && astOK ) {
            astError( AST__BADUN, "Missing opening parenthesis." , status);
            break;
         }

/* Ignore all other characters unless they are at zero depth of nesting.
   Also ignore spaces. */
      } else if( depth == 0 && !isspace( *c ) ) {

/* Compare the next part of the string with each of the "pow" operators. */
         if( !strncmp( c, "**", 2 ) ) {
            op = OP_POW;
            oplen = 2;
         } else if( *c == '^' ) {
            op = OP_POW;
            oplen = 1;
         }

/* If an operator was found, break out of the loop. */
         if( op != OP_NULL ) break;
      }

/* Pass on to check the next character. */
      i++;
      c++;
   }

/* If a "pow" operator was found, the strings on either side of it should be
   valid unit expressions, in which case we use this routine recursively to
   create corresponding trees of UnitNodes. */
   if( op != OP_NULL ) {

/* Create a UnitNode for the operator. */
      result = NewNode( NULL, op, status );
      if( astOK ) {

/* Create a tree of unit nodes from the string which precedes the binary
   operator. Report an error if it cannot be done. */
        result->arg[ 0 ] = MakeLabelTree( exp, i, status );
        if( !result->arg[ 0 ] && astOK ) {
           for( i = 0; i < oplen; i++ ) buff[ i ] = c[ i ];
           buff[ oplen ] = 0;
           astError( AST__BADUN, "Missing operand before '%s'.", status, buff );
        }

/* Create a tree of unit nodes from the string which follows the binary
   operator. Report an error if it cannot be done. */
         result->arg[ 1 ] = MakeLabelTree( c + oplen, nc - i - oplen, status );
         if( !result->arg[ 1 ] && astOK ) {
             for( i = 0; i < oplen; i++ ) buff[ i ] = c[ i ];
            buff[ oplen ] = 0;
            astError( AST__BADUN, "Missing operand after '%s'.", status, buff );
         }
      }

/* If no binary operator was found at depth zero, see if the supplied string
   starts with a function name (the only legal place for a function name
   given that the string has no binary operators at depth zero). */
   } else {
      if( !strncmp( exp, "sqrt(", 5 ) || !strncmp( exp, "SQRT(", 5 ) ) {
         op = OP_SQRT;
         oplen = 4;
      } else if( !strncmp( exp, "exp(", 4 ) || !strncmp( exp, "EXP(", 4 ) ) {
         op = OP_EXP;
         oplen = 3;
      } else if( !strncmp( exp, "ln(", 3 ) || !strncmp( exp, "LN(", 3 ) ) {
         op = OP_LN;
         oplen = 2;
      } else if( !strncmp( exp, "log(", 4 ) || !strncmp( exp, "LOG(", 4 ) ) {
         op = OP_LOG;
         oplen = 3;
      }

/* If a function was found, the string following the function name
   (including the opening parenthesis) should form a legal units
   expresssion (all the supported functions take a single argument and
   so we do not need to worry about comma-separated lists of function
   arguments). Use this routine recursively to create a tree of UnitNodes
   from the string which forms the function argument. */
      if( op != OP_NULL ) {

/* Create a UnitNode for the function. */
         result = NewNode( NULL, op, status );
         if( astOK ) {

/* Create a tree of unit nodes from the string which follows the function
   name. Report an error if it cannot be done. */
            result->arg[ 0 ] = MakeLabelTree( exp + oplen, nc - oplen, status );
            if( !result->arg[ 0 ] && astOK ) {
               for( i = 0; i < oplen; i++ ) buff[ i ] = c[ i ];
               buff[ oplen ] = 0;
               astError( AST__BADUN, "Missing argument for '%s'.", status, buff );
            }
         }

/* Arrive here if the supplied string does not contain a POW operator
   or function at depth zero. Check to see if the whole string is contained
   within parentheses, In which we interpret the contents of the
   parentheses as a units expression. It is safe simply to check the
   first and last characters (a string like "(fred)(Harry)" is not a
   legal possibility since there should be an operator in the middle).*/
      } else if( nc > 0 && ( exp[ 0 ] == '(' && exp[ nc - 1 ] == ')' ) ) {
         result = MakeLabelTree( exp + 1, nc - 2, status );

/* Does the string begin with a numerical constant? */
      } else if( ConStart( exp, &con, &n, status ) == 1 ) {

/* If the entire string was a numerical constant, represent it by a LDCON
   node. */
         if( n == nc ) {
            result = NewNode( NULL, OP_LDCON, status );
            if( astOK ) result->con = con;

/* If there was anything following the numerical constant, report an
   error. */
         } else if( astOK ){
            astError( AST__BADUN, "Missing operator after "
                      "numerical string '%.*s'.", status, n, exp );
         }

/* The only legal possibility left is that the string represents the basic
   label. Create an OP_LDVAR node for it and store the basic label as
   the node name, omitting any enclosing white space. */
      } else {
         result = NewNode( NULL, OP_LDVAR, status );
         if( astOK ) {
            result->name = astStore( NULL, exp, nc + 1 );
            if( astOK ) ( (char *) result->name)[ nc ] = 0;
         }
      }
   }

/* Free any returned tree if an error has occurred. */
   if( !astOK ) result = FreeTree( result, status );

/* Return the result. */
   return result;
}

static UnitNode *MakeTree( const char *exp, int nc, int lock, int *status ){
/*
*  Name:
*     MakeTree

*  Purpose:
*     Convert an algebraic units expression into a tree of UnitNodes.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     UnitNode *MakeTree( const char *exp, int nc, int lock, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function converts an algebraic units expression into a tree of
*     UnitNodes. It is a service routine for CreateTree. The roots of the
*     returned tree (i.e. the LDVAR nodes) refer to the unit symbols
*     contained within the supplied expression (i.e. definitions of these
*     units are not grafted onto the tree in place of the original nodes,
*     as is done by CreateTree).

*  Parameters:
*     exp
*        The units expression. This should not include any leading or
*        trailing spaces.
*     nc
*        The number of characters from "exp" to use.
*     lock
*        Use a mutex to guard access to the KnownUnits list?
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a UnitNode which forms the head of a tree of UnitNodes
*     representing the supplied unit expression.

*  Notes:
*     -  A NULL value is returned if this function is invoked with the
*     global error status set or if it should fail for any reason.
*/

/* Local Variables: */
   KnownUnit *munit;
   KnownUnit *unit;
   Multiplier *mmult;
   Multiplier *mult;
   Oper op;
   UnitNode *result;
   char buff[ 10 ];
   char d;
   const char *c;
   double con;
   int depth;
   int i;
   int l;
   int maxlen;
   int n;
   int oplen;
   int plural;

/* Initialise */
   result = NULL;

/* Check the global error status, and that we have a string. */
   if ( !astOK || !exp || nc <= 0 ) return result;

/* Scan through the supplied string from the end to the start looking for
   the last multiplication or division operator at zero depth of nesting
   within parentheses. We go backwards through the string in order to
   give the correct priority to multiple division operators (i.e. "a/b/c"
   needs to be interpreted as "(a/b)/c", not "a/(b/c)"). */
   op = OP_NULL;
   oplen = 1;
   depth = 0;
   c = exp + nc - 1;
   i = nc - 1;
   while( i >= 0 ){

/* If this character is an opening parenthesis, decrement the depth of
   nesting. Report an error if it ever goes negative. */
      if( *c == '(' ) {
         depth--;
         if( depth < 0 && astOK ) {
            astError( AST__BADUN, "Missing closing parenthesis." , status);
            break;
         }

/* An opening parenthesis at level zero must always be either the first
   character in the string, or be preceded by the name of a function, or
   be preceded by an operator. If none of these are true, assume there is
   an implicit multiplication operator before the parenthesis. */
         if( depth == 0 && i > 0 ) {
            d = *( c - 1 );
            if( d != '*' && d != '/' && d != '^' && d != '.' && d != ' ' &&
                !EndsWith( c, i + 1, "sqrt(", status ) && !EndsWith( c, i + 1, "exp(", status ) &&
                !EndsWith( c, i + 1, "ln(", status ) && !EndsWith( c, i + 1, "log(", status ) ) {
               op = OP_MULT;
               oplen = 0;
               break;
            }
         }

/* If this character is an closing parenthesis, increment the depth of
   nesting. */
      } else if( *c == ')' ) {
         depth++;

/* A closing parenthesis at level zero must always be either the last
   character in the string, or be followed by an operator. If neither of
   these are true, assume there is an implicit multiplication operator. */
         if( depth == 1 && i < nc - 1 ) {
            d = *(c+1);
            if( d != '*' && d != '/' && d != '^' && d != '.' && d != ' ') {
               op = OP_MULT;
               oplen = 0;

/* Correct "i" so that it gives the length of the left hand operand of
   the implicit MULT operator, correct "c" so that it points to the first
   character in the right hand operand, and leave the loop. */
               i++;
               c++;
               break;
            }
         }

/* Ignore all other characters unless they are at zero depth of nesting. */
      } else if( depth == 0 ) {

/* Compare the next part of the string with each of the multiplication
   and division operators. */
         if( *c == '/' ) {
            op = OP_DIV;

         } else if( *c == ' ' ) {
            op = OP_MULT;

/* An asterisk is only treated as a multiplication symbol if it does not occur
   before or after another asterisk. */
         } else if( *c == '*' ) {
            if(  c == exp ) {
               if( *(c+1) != '*' ) op = OP_MULT;
            } else if( i == nc - 1 ) {
               if( *(c-1) != '*' ) op = OP_MULT;
            } else {
               if( *(c+1) != '*' && *(c-1) != '*' ) op = OP_MULT;
            }

/* A dot is only treated as a multiplication symbol if it does not occur
   between two digits. */
         } else if( *c == '.' ) {
            if( ( c == exp || !isdigit( *(c-1) ) ) &&
                ( i == nc - 1 || !isdigit( *(c+1) ) ) ) {
               op = OP_MULT;
            }
         }
      }

/* If an operator was found, break out of the loop. */
      if( op != OP_NULL ) break;

/* Pass on to check the next character. */
      i--;
      c--;
   }

/* If a multiplication or division operator was found, the strings on either
   side of it should be valid unit expressions, in which case we use this
   routine recursively to create corresponding trees of UnitNodes. */
   if( op != OP_NULL ) {

/* Create a UnitNode for the binary operator. */
      result = NewNode( NULL, op, status );
      if( astOK ) {

/* Create a tree of unit nodes from the string which precedes the binary
   operator. Report an error if it cannot be done. */
         result->arg[ 0 ] = MakeTree( exp, i, lock, status );
         if( !result->arg[ 0 ] && astOK ) {
            for( i = 0; i < oplen; i++ ) buff[ i ] = c[ i ];
            buff[ oplen ] = 0;
            astError( AST__BADUN, "Missing operand before '%s'.", status, buff );
         }

/* Create a tree of unit nodes from the string which follows the binary
   operator. Report an error if it cannot be done. */
         result->arg[ 1 ] = MakeTree( c + oplen, nc - i - oplen, lock, status );
         if( !result->arg[ 1 ] && astOK ) {
            for( i = 0; i < oplen; i++ ) buff[ i ] = c[ i ];
            buff[ oplen ] = 0;
            astError( AST__BADUN, "Missing operand after '%s'.", status, buff );
         }
      }

/* If no multiplication or division operator was found at depth zero, check
   that the final depth of nesting was zero. Report an error if not. */
   } else if( depth > 0 && astOK ) {
      astError( AST__BADUN, "Missing opening parenthesis." , status);

/* Otherwise check for a "Pow" operator at depth zero. */
   } else {

/* Scan through the supplied string looking for the first pow operator at
   zero depth of nesting within parentheses. */
      depth = 0;
      c = exp;
      i = 0;
      while( i < nc && *c ){

/* If this character is an opening parenthesis, increment the depth of
   nesting. */
         if( *c == '(' ) {
            depth++;

/* If this character is an closing parenthesis, decrement the depth of
   nesting. Report an error if it ever goes negative. */
         } else if( *c == ')' ) {
            depth--;
            if( depth < 0 && astOK ) {
               astError( AST__BADUN, "Missing opening parenthesis." , status);
               break;
            }

/* Ignore all other characters unless they are at zero depth of nesting. */
         } else if( depth == 0 ) {

/* Compare the next part of the string with each of the "pow" operators. */
            if( !strncmp( c, "**", 2 ) ) {
               op = OP_POW;
               oplen = 2;
            } else if( *c == '^' ) {
               op = OP_POW;
               oplen = 1;
            }

/* If an operator was found, break out of the loop. */
            if( op != OP_NULL ) break;
         }

/* Pass on to check the next character. */
         i++;
         c++;
      }

/* If a "pow" operator was found, the strings on either side of it should be
   valid unit expressions, in which case we use this routine recursively to
   create corresponding trees of UnitNodes. */
      if( op != OP_NULL ) {

/* Create a UnitNode for the operator. */
         result = NewNode( NULL, op, status );
         if( astOK ) {

/* Create a tree of unit nodes from the string which precedes the binary
   operator. Report an error if it cannot be done. */
            result->arg[ 0 ] = MakeTree( exp, i, lock, status );
            if( !result->arg[ 0 ] && astOK ) {
               for( i = 0; i < oplen; i++ ) buff[ i ] = c[ i ];
               buff[ oplen ] = 0;
               astError( AST__BADUN, "Missing operand before '%s'.", status, buff );
            }

/* Create a tree of unit nodes from the string which follows the binary
   operator. Report an error if it cannot be done. */
            result->arg[ 1 ] = MakeTree( c + oplen, nc - i - oplen, lock, status );
            if( !result->arg[ 1 ] && astOK ) {
                for( i = 0; i < oplen; i++ ) buff[ i ] = c[ i ];
               buff[ oplen ] = 0;
               astError( AST__BADUN, "Missing operand after '%s'.", status, buff );
            }
         }

/* If no binary operator was found at depth zero, see if the supplied string
   starts with a function name (the only legal place for a function name
   given that the string has no binary operators at depth zero). */
      } else {
         if( !strncmp( exp, "sqrt(", 5 ) || !strncmp( exp, "SQRT(", 5 ) ) {
            op = OP_SQRT;
            oplen = 4;
         } else if( !strncmp( exp, "exp(", 4 ) || !strncmp( exp, "EXP(", 4 ) ) {
            op = OP_EXP;
            oplen = 3;
         } else if( !strncmp( exp, "ln(", 3 ) || !strncmp( exp, "LN(", 3 ) ) {
            op = OP_LN;
            oplen = 2;
         } else if( !strncmp( exp, "log(", 4 ) || !strncmp( exp, "LOG(", 4 ) ) {
            op = OP_LOG;
            oplen = 3;
         }

/* If a function was found, the string following the function name
   (including the opening parenthesis) should form a legal units
   expresssion (all the supported functions take a single argument and
   so we do not need to worry about comma-separated lists of function
   arguments). Use this routine recursively to create a tree of UnitNodes
   from the string which forms the function argument. */
         if( op != OP_NULL ) {

/* Create a UnitNode for the function. */
            result = NewNode( NULL, op, status );
            if( astOK ) {

/* Create a tree of unit nodes from the string which follows the function
   name. Report an error if it cannot be done. */
               result->arg[ 0 ] = MakeTree( exp + oplen, nc - oplen, lock, status );
               if( !result->arg[ 0 ] && astOK ) {
                  for( i = 0; i < oplen; i++ ) buff[ i ] = c[ i ];
                  buff[ oplen ] = 0;
                  astError( AST__BADUN, "Missing argument for '%s'.", status, buff );
               }
            }

/* Arrive here if the supplied string does not contain a binary operator
   or function at depth zero. Check to see if the whole string is contained
   within parentheses, In which we interpret the contents of the
   parentheses as a units expression. It is safe simply to check the
   first and last characters (a string like "(fred)(Harry)" is not a
   legal possibility since there should be an operator in the middle).*/
         } else if( exp[ 0 ] == '(' && exp[ nc - 1 ] == ')' ) {
            result = MakeTree( exp + 1, nc - 2, lock, status );

/* Does the string begin with a numerical constant? */
         } else if( ConStart( exp, &con, &n, status ) == 1 ) {

/* If the entire string was a numerical constant, represent it by a LDCON
   node. */
            if( n == nc ) {
               result = NewNode( NULL, OP_LDCON, status );
               if( astOK ) result->con = con;

/* If there was anything following the numerical constant, report an
   error. */
            } else if( astOK ){
               astError( AST__BADUN, "Missing operator after "
                         "numerical string '%.*s'.", status, n, exp );
            }

/* Does the string represent one of the named constants? If so represent it
   by a an appropriate operator. */
         } else if( nc == 2 && ( !strncmp( exp, "pi", 2 ) ||
                                 !strncmp( exp, "PI", 2 ) ) ) {
            result = NewNode( NULL, OP_LDPI, status );

         } else if( nc == 1 && ( !strncmp( exp, "e", 1 ) ||
                                 !strncmp( exp, "E", 1 ) ) ) {
            result = NewNode( NULL, OP_LDE, status );

/* The only legal possibility left is that the string represents the name
   of a basic unit, possibly prefixed by a multiplier character. */
         } else {

/* See if the string ends with the symbol for any of the known basic
   units. If it matches more than one basic unit, choose the longest.
   First ensure descriptions of the known units are  available. */
            mmult = NULL;
            plural = 0;
            while( 1 ) {
               unit = GetKnownUnits( lock, status );

               maxlen = -1;
               munit = NULL;
               while( unit ) {
                  if( SplitUnit( exp, nc, unit->sym, 1, &mult, &l, status ) ) {
                     if( l > maxlen ) {
                        maxlen = l;
                        munit = unit;
                        mmult = mult;
                     }
                  }
                  unit = unit->next;
               }

/* If the above did not produce a match, try matching the unit symbol
   case insensitive. */
               if( !munit ) {
                  unit = GetKnownUnits( lock, status );
                  while( unit ) {
                     if( SplitUnit( exp, nc, unit->sym, 0, &mult, &l, status ) ) {
                        if( l > maxlen ) {
                           maxlen = l;
                           munit = unit;
                           mmult = mult;
                        }
                     }
                     unit = unit->next;
                  }
               }

/* If the above did not produce a match, try matching the unit label
   case insensitive. */
               if( !munit ) {
                  unit = GetKnownUnits( lock, status );
                  while( unit ) {
                     if( SplitUnit( exp, nc, unit->label, 0, &mult, &l, status ) ) {
                        if( l > maxlen ) {
                           maxlen = l;
                           munit = unit;
                           mmult = mult;
                        }
                     }
                     unit = unit->next;
                  }
               }

/* If we still do not have a match, and if the string ends with "s", try
   removing the "s" (which could be a plural as in "Angstroms") and
   trying again. */
               if( !munit && nc > 1 && !plural &&
                   ( exp[ nc - 1 ] == 's' || exp[ nc - 1 ] == 'S' ) ) {
                  plural = 1;
                  nc--;
               } else {
                  break;
               }
            }
            if( plural ) nc++;

/* If a known unit and multiplier combination was found, create an
   OP_LDVAR node from it. */
            unit = munit;
            mult = mmult;
            if( unit ) {

/* If the unit is an alias for another unit, it will have a non-NULL
   value for its "use" component.In this case, use the unit for which the
   identified unit is an alias. */
               result = NewNode( NULL, OP_LDVAR, status );
               if( astOK ) {
                  result->unit = unit->use ? unit->use : unit;
                  result->mult = mult;
                  result->name = astStore( NULL, result->unit->sym, result->unit->symlen + 1 );
               }

/* If no known unit and multiplier combination was found, we assume the
   string represents a new user-defined basic unit, possibly preceded by a
   standard multiplier prefix. */
            } else {

/* Check the string to see if starts with a known multiplier prefix (but
   do not allow the multiplier to account for the entire string). */
               mult = GetMultipliers( status );
               c = exp;
               while( mult ) {
                  n = nc - mult->symlen;
                  if( n > 0 && !strncmp( exp, mult->sym, mult->symlen ) ) {
                     c += mult->symlen;
                     break;
                  }
                  mult = mult->next;
               }
               if( !mult ) n = nc;

/* Check there are no illegal characters in the following string. */
               for( i = 0; i < n && astOK; i++ ) {
                  if( !isalpha( c[ i ] ) ) {
                     astError( AST__BADUN, "Illegal character '%c' found.", status, c[ i ] );
                     break;
                  }
               }

/* If succesfull, create an OP_LDVAR node for th user-defined basic unit. */
               if( astOK ) {
                  result = NewNode( NULL, OP_LDVAR, status );
                  if( astOK ) {
                     result->mult = mult;
                     result->name = astStore( NULL, c, n + 1 );
                     if( astOK ) ( (char *) result->name)[ n ] = 0;
                  }
               }
            }
         }
      }
   }

/* Free any returned tree if an error has occurred. */
   if( !astOK ) result = FreeTree( result, status );

/* Return the result. */
   return result;
}

static void MakeUnitAlias( const char *sym, const char *alias, int *status ){
/*
*  Name:
*     MakeUnitAlias

*  Purpose:
*     Create a KnownUnit structure describing an alias for a known unit.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     void MakeUnitAlias( const char *sym, const char *alias, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function creates a KnownUnit structure decribing an alias for a
*     known unit, and adds it to the head of the linked list of known units
*     stored in a module variable. An alias is a KnownUnit which is
*     identical to an existing known but which has a different symbol.

*  Parameters:
*     sym
*        A pointer to the symbol string of an existing KnwonUnit. The string
*        should not include any multiplier prefix.
*     alias
*        A pointer to the symbol string to use as the alasi for the existing
*        KnownUnit. The string should not include any multiplier prefix.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     -  The supplied symbol and label strings are not copied. The
*     supplied pointers are simply stored in the returned structure.
*     Therefore the strings to which the pointers point should not be
*     modified after this function returned (in fact this function is
*     always called with literal strings for these arguments).
*/

/* Local Variables: */
   KnownUnit *unit;

/* Check the global error status. */
   if( !astOK ) return;

/* Search the existing list of KnownUnits for the specified symbol. */
   unit = known_units;
   while( unit ) {
      if( !strcmp( sym, unit->sym ) ) {

/* Create a new KnownUnit for the alias. It will becomes the head of the
   known units chain. */
         MakeKnownUnit( alias, unit->label, NULL, status );

/* Store a pointer to the KnownUnit which is to be used in place of the
   alias. */
         known_units->use = unit;

/* Leave the loop. */
         break;
      }

/* Move on to check the next existing KnownUnit. */
      unit = unit->next;
   }

/* Report an error if the supplied unit was not found. */
   if( !unit ) {
      astError( AST__INTER, "MakeUnitAlias(Unit): Cannot find existing "
                "units \"%s\" to associate with the alias \"%s\" (AST "
                "internal programming error).", status, sym, alias );
   }
}

static UnitNode *ModifyPrefix( UnitNode *old, int *status ) {
/*
*  Name:
*     ModifyPrefix

*  Purpose:
*     Replace a MULT or DIV node with a LDVAR and suitable multiplier.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     UnitNode *ModifyPrefix( UnitNode *old, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function checks the supplied node. If it is a DIV or MULT node
*     in which one argument is an LDVAR and the other is a constant, then
*     its checks to see if the constant can be absorbed into the LDVAR by
*     changing the multiplier in the LDVAR node. If so, it returns a new
*     node which is an LDVAR with the modified multiplier. Otherwise it
*     returns NULL.

*  Parameters:
*     old
*        Pointer to an existing UnitNode to be checked.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new UnitNode.

*  Notes:
*     - A value of NULL will be returned if this function is invoked with
*     the global error status set, or if it should fail for any reason.
*/

/* Local Variables: */
   Multiplier *mult;
   Multiplier *mmult;
   UnitNode *ldcon;
   UnitNode *ldvar;
   UnitNode *newtree;
   UnitNode *result;
   double con;
   double cmult;
   double r;
   double rmin;
   int recip;
   int changed;

/* Initialise. */
   result = NULL;

/* Check the inherited status. */
   if( !astOK ) return result;

/* Indicate that we have not yet found any reason to return a changed
   node. */
   changed = 0;

/* Check the supplied node is a DIV or MULT node. */
   if( old->opcode == OP_DIV || old->opcode == OP_MULT ) {

/* Get a copy of the supplied tree which we can modify safely. */
      newtree = CopyTree( old, status );

/* Identify the LDVAR argument (if any). */
      if( newtree->arg[ 0 ]->opcode == OP_LDVAR ) {
         ldvar = newtree->arg[ 0 ];

      } else if( newtree->arg[ 1 ]->opcode == OP_LDVAR ) {
         ldvar = newtree->arg[ 1 ];

      } else {
         ldvar = NULL;
      }

/* Identify the LDCON argument (if any). */
      if( newtree->arg[ 0 ]->opcode == OP_LDCON ) {
         ldcon = newtree->arg[ 0 ];

      } else if( newtree->arg[ 1 ]->opcode == OP_LDCON ) {
         ldcon = newtree->arg[ 1 ];

      } else {
         ldcon = NULL;
      }

/* If either was not found, return NULL. */
      if( !ldvar || !ldcon ) {
         newtree = FreeTree( newtree, status );

/* Otherwise, extract the multiplier constant. If there is no multiplier, the
   constant is 1.0. */
      } else {
         cmult = ldvar->mult ? ldvar->mult->scale: 1.0;

/* Extract the constant. */
         con = ldcon->con;

/* Combine the multiplier and the constant. The resulting constant is a
   factor which is used to multiply the LDVAR quantity. If the original
   node is a DIV node in which the LDVAR is in the denominator, then
   flag that we need to reciprocate the new MULT node which represents
   "constant*LDVAR" before returning. */
         if( newtree->opcode == OP_MULT ) {
            con = con*cmult;
            recip = 0;
         } else {
            con = cmult/con;
            recip = ( ldvar == newtree->arg[ 1 ] );
         }

/* Find the closest known multiplier to the new constant. */
         rmin = ( con > 1 ) ? con : 1.0/con;
         mmult = NULL;
         mult = GetMultipliers( status );
         while( mult ) {
            r = ( con > mult->scale) ? con/mult->scale : mult->scale/con;
            if( r < rmin ) {
               mmult = mult;
               rmin = r;
            }
            mult = mult->next;
         }

/* Modify the constant to take account of the new multiplier chosen
   above. "mmult" will be NULL if the best multiplier is unity. */
         if( mmult ) con = con/mmult->scale;

/* If they have changed, associate the chosen multiplier with the LDVAR node,
   and the constant with the LDCON node. */
         if( ldvar->mult != mmult ) {
            ldvar->mult = mmult;
            changed = 1;
         }

         if( ldcon->con != con ) {
            ldcon->con = con;
            changed = 1;
         }

/* Unless the node is proportional to the reciprocal of the variable, the
   new node should be a MULT node (it may originally have been a DIV). */
         if( !recip ) {
            if( newtree->opcode != OP_MULT ){
               newtree->opcode = OP_MULT;
               changed = 1;
            }

/* If the constant is 1.0 we can just return the LDVAR node by itself. */
            if( fabs( con - 1.0 ) < 1.0E-6 ) {
               result = CopyTree( ldvar, status );
               newtree = FreeTree( newtree, status );
               changed = 1;

/* Otherwise return the modified tree containing both LDVAR and LDCON nodes. */
            } else {
               result = newtree;
            }

/* If the node is proportional to the reciprocal of the variable, the
   new node will already be a DIV node and will have an LDCON as the first
   argument (numerator) and an LDVAR as the second argument (denominator). */
         } else {

/* The first argument (the numerator) should be the reciprocal of the constant
   found above. */
            ldcon->con = 1.0/ldcon->con;
            if( !EQUAL( ldcon->con, old->arg[0]->con ) ) changed = 1;

/* Return the modified tree containing both LDVAR and LDCON nodes. */
            result = newtree;
         }
      }
   }

/* If the new and old trees are equivalent, then we do not need to return
   it. */
   if( !changed && result ) result = FreeTree( result, status );

/* Return the answer. */
   return result;
}


static UnitNode *NewNode( UnitNode *old, Oper code, int *status ) {
/*
*  Name:
*     NewNode

*  Purpose:
*     Create and initialise a new UnitNode.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     UnitNode *NewNode( UnitNode *old, Oper code, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function creates and initialises a new UnitNode, or
*     re-initialises an existing UnitNode to use a different op code.

*  Parameters:
*     old
*        Pointer to an existing UnitNode to be modified, or NULL to create
*        a new UnitNode.
*     code
*        The op code for the new UnitNode.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new UnitNode.

*  Notes:
*     - A value of NULL will be returned if this function is invoked with
*     the global error status set, or if it should fail for any reason.
*/

/* Local Variables: */
   UnitNode **args;
   UnitNode *result;
   int i;

/* Initialise. */
   result = NULL;
   args = NULL;

/* Check the inherited status. */
   if( !astOK ) return result;

/* If an existig UnitNode was supplied, free any memory used to hold
   pointers to its arguments. */
   if( old ) {
      old->arg = astFree( old->arg );
      result = old;

/* Otherwise, allocate memory for a new structure. */
   } else {
      result = astMalloc( sizeof( UnitNode ) );
   }

/* Check the pointer can be used safely. */
   if( astOK ) {

/* Initialise the members of the UnitNode structure. */
      result->opcode = code;
      result->arg = NULL;
      result->con = AST__BAD;
      result->name = NULL;
      result->unit = NULL;
      result->mult = NULL;
      result->narg = 0;

      switch( code ){
         case OP_LDPI:
            result->con = PI;
            break;

         case OP_LDE:
            result->con = E;
            break;

         case OP_LOG:
         case OP_LN:
         case OP_EXP:
         case OP_SQRT:
            result->narg = 1;
            break;

         case OP_POW:
         case OP_DIV:
         case OP_MULT:
            result->narg = 2;
            break;

         default:
            ;
      }

/* Allocate memory for the UnitNode pointers which will locate the
   nodes forming the arguments to the new node. */
      args = astMalloc( (result->narg)*sizeof( UnitNode * ) );
      if( astOK ) {
         result->arg = args;

/* Initialise the argument pointers to NULL. */
         for( i = 0; i < result->narg; i++ ) args[ i ] = NULL;
      }
   }

/* Free any result if an error occurred. */
   if( !astOK ) {
      args = astFree( args );
      result = astFree( result );
   }

/* Return the answer. */
   return result;
}

static void RemakeTree( UnitNode **node, int *status ) {
/*
*  Name:
*     RemakeTree

*  Purpose:
*     Replace derived units within a tree of UnitNodes by basic units.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     void RemakeTree( UnitNode **node, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function searches for LDVAR nodes (i.e. references to unit
*     symbols) within the given tree, and replaces each such node which
*     refers to known derived unit with a sub-tree of nodes which
*     define the derived unit in terms of known basic units.

*  Parameters:
*     node
*        The address of a pointer to the UnitNode at the head of the tree
*        which is to be simplified. On exit the supplied tree is freed and a
*        pointer to a new tree is placed at the given address.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   KnownUnit *unit;
   int i;
   UnitNode *newnode;

/* Check inherited status. */
   if( !astOK ) return;

/* Initially, we have no replacement node */
   newnode = NULL;

/* If this is an LDVAR node... */
   if( (*node)->opcode == OP_LDVAR ) {

/* If the LDVAR node has a multiplier associated with it, we need to
   introduce a OP_MULT node to perform the scaling. */
      if( (*node)->mult ) {
         newnode = NewNode( NULL, OP_MULT, status );
         if( astOK ) {
            newnode->arg[0] = NewNode( NULL, OP_LDCON, status );
            if( astOK ) {
               newnode->arg[0]->con = 1.0/(*node)->mult->scale;

/* See if the node refers to a known unit. If not, or if the known unit
   is a basic unit (i.e. not a derived unit) use the supplied node for
   the second argument of the OP_MULT node (without the multiplier).
   Otherwise, use a copy of the tree which defines the derived unit. */
               unit = (*node)->unit;
               if( unit && unit->head ) {
                  newnode->arg[1] = CopyTree( unit->head, status );
               } else {
                  newnode->arg[1] = CopyTree( *node, status );
                  if( astOK ) newnode->arg[1]->mult = NULL;
               }
            }
         }

/* If no multiplier is supplied, the replacement node is simply the tree
   which defines the unscaled unit (if known), or the original node (if
   unknown). */
      } else {
         unit = (*node)->unit;
         if( unit && unit->head ) newnode = CopyTree( unit->head, status );
      }

/* If this is not an LDVAR Node, remake the sub-trees which form the
   arguments of this node. */
   } else {
      for( i = 0; i < (*node)->narg; i++ ) {
         RemakeTree( &((*node)->arg[ i ]), status );
      }
   }

/* If an error has occurred, free any new node. */
   if( !astOK ) newnode = FreeTree( newnode, status );

/* If we have a replacement node, free the supplied tree and return a
   pointer to the new tree. */
   if( newnode ) {
      FreeTree( *node, status );
      *node = newnode;
   }
}

static int ReplaceNode( UnitNode *target, UnitNode *old, UnitNode *new, int *status ) {
/*
*  Name:
*     ReplaceNode

*  Purpose:
*     Replace a node within a tree of UnitNodes.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     int ReplaceNode( UnitNode *target, UnitNode *old, UnitNode *new, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function replaces a specified node within a tree of UnitNodes
*     with another given node. The original node is freed if found.

*  Parameters:
*     target
*        A pointer to the UnitNode at the head of the tree containing the
*        node to be replaced.
*     old
*        A pointer to the UnitNode to be replaced.
*     new
*        A pointer to the UnitNode to replace "old".
*     status
*        Pointer to the inherited status variable.

*  Return Value:
*     Non-zero if the "old" node was found and replaced (in which case
*     the "old" node will have been freed).

*  Notes:
*     - It is assumed that the "old" node occurs at most once within the
*     target tree.
*     - The node at the head of the target tree is not compared with the
*     "old" node. It is assumed the called will already have done this.
*     - A value of zero is returned if an error has already occurred, or
*     if this function fails for any reason.

*/

/* Local Variables: */
   int i;
   int result;

/* Initialise */
   result = 0;

/* Check inherited status. */
   if( !astOK ) return result;

/* Loop round the arguments of the node at the head of the target tree.
   Break out of the loop as soone as the old node is found. */
   for( i = 0; i < target->narg; i++ ) {

/* If this argument is the node to be replaced, free the old one and store
   the new one, and then leave the loop. */
      if( target->arg[ i ] == old ) {
         FreeTree( old, status );
         target->arg[ i ] = new;
         result = 1;
         break;

/* Otherwise use this function recursively to search for the old node
   within the current argument. */
      } else {
         if( ReplaceNode( target->arg[ i ], old, new, status ) ) break;
      }
   }

/* If an error has occurred, return zero. */
   if( !astOK ) result = 0;

/* Return the answer. */
   return result;
}

static int SimplifyTree( UnitNode **node, int std, int *status ) {
/*
*  Name:
*     SimplifyTree

*  Purpose:
*     Simplify a tree of UnitNodes.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     int SimplifyTree( UnitNode **node, int std, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function simplifies a tree of UnitNodes. It is assumed that
*     all the OP_LDVAR nodes in the tree refer to the same basic unit.
*     A primary purpose of this function is to standardise the tree so
*     that trees which implement equivalent transformations but which
*     have different structures can be compared (for instance, so that
*     "2*x" and "x*2" are treated as equal trees). If "std" is non-zero,
*     reducing the complexity of the tree is only of secondary importance.
*     This explains why some "simplifications" actually produced trees which
*     are more complicated.

*  Parameters:
*     node
*        The address of a pointer to the UnitNode at the head of the tree
*        which is to be simplified. On exit the supplied tree is freed and a
*        pointer to a new tree is placed at the given address.
*     std
*        If non-zero, perform standardisations. Otherwise only perform
*        genuine simplifications.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Non-zero if some change was made to the tree.

*/

/* Local Variables: */
   int i;
   UnitNode *newnode;
   UnitNode *node1;
   UnitNode *node2;
   Oper op;
   UnitNode **factors;
   double *powers;
   int nfactor;
   double coeff;
   int result;

/* Initialise */
   result = 0;

/* Check inherited status. */
   if( !astOK ) return result;

/* Initiallially, we have no replacement node. */
   newnode = NULL;

/* First replace any complex constant expressions any corresponding
   OP_LDCON nodes. */
   FixConstants( node, 0, status );

/* Simplify the sub-trees corresponding to the arguments of the node at
   the head of the supplied tree. */
   for( i = 0; i < (*node)->narg; i++ ) {
      if( SimplifyTree( &( (*node)->arg[ i ] ), std, status ) ) result = 1;
   }

/* Now do specific simplifications appropriate to the nature of the node at
   the head of the tree. */
   op = (*node)->opcode;

/* Natural log */
/* =========== */
/* We standardise argument powers into coefficients of the LN value. */
   if( op == OP_LN ) {

/* If the argument is a OP_EXP node, they cancel out. Return a copy of the
   argument of OP_EXP node. */
      if( (*node)->arg[ 0 ]->opcode == OP_EXP ) {
         newnode = CopyTree( (*node)->arg[ 0 ]->arg[ 0 ], status );

/* If the argument is an OP_POW node, rearrange the nodes to represent
   k*ln(x) instead of ln(x**k) (note pow nodes always have a constant
   exponent - this is checked in InvertConstants). SQRT arguments will
   not occur because they will have been changed into POW nodes when the
   arguments of the supplied head node were simplified above. */
      } else if( std && (*node)->arg[ 0 ]->opcode == OP_POW ) {
         newnode = NewNode( NULL, OP_MULT, status );
         node1 = CopyTree( (*node)->arg[ 0 ]->arg[ 1 ], status );
         node2 = NewNode( NULL, OP_LN, status );
         if( astOK ) {
            node2->arg[ 0 ] = CopyTree( (*node)->arg[ 0 ]->arg[ 0 ], status );
            newnode->arg[ 0 ] = node1;
            newnode->arg[ 1 ] = node2;
         }
      }

/* Common log */
/* ========== */
/* We standardise natural logs into common logs. */
   } else if( op == OP_LOG ) {
      if( std ) {
         newnode = NewNode( NULL, OP_DIV, status );
         node1 = NewNode( NULL, OP_LN, status );
         node2 = NewNode( NULL, OP_LDCON, status );
         if( astOK ) {
            node1->arg[ 0 ] = CopyTree( (*node)->arg[ 0 ], status );
            node2->con = log( 10.0 );
            newnode->arg[ 0 ] = node1;
            newnode->arg[ 1 ] = node2;
         }
      }

/* Exponential */
/* =========== */
/* We prefer to minimise the number of EXP nodes, so, for instance, we do not
   change "exp(x*y)" to "exp(x)+exp(y)" (and the code for ADD nodes does
   the inverse conversion). */
   } else if( op == OP_EXP ) {

/* If the argument is an OP_LN node, they cancel out. Return a copy of the
   argument of the OP_LN node. Common log arguments will not occur because
   they will have been changed into natural logs when the arguments of
   the supplied head node were simplified above. */
      if( (*node)->arg[ 0 ]->opcode == OP_LN ) {
         newnode = CopyTree( (*node)->arg[ 0 ]->arg[ 0 ], status );
      }

/* Square root */
/* =========== */
/* We standardise sqrt nodes into pow nodes. */
   } else if( op == OP_SQRT ) {
      if( std ) {
         newnode = NewNode( NULL, OP_POW, status );
         node1 = CopyTree( (*node)->arg[ 0 ], status );
         node2 = NewNode( NULL, OP_LDCON, status );
         if( astOK ) {
            node2->con = 0.5;
            newnode->arg[ 0 ] = node1;
            newnode->arg[ 1 ] = node2;
         }
      }

/* Exponentiation */
/* ============== */
/* We want to simplfy factors. So, for instance, (x*y)**k is converted to
   (x**k)*(y**k). */
   } else if( op == OP_POW ) {

/* If the first argument is an OP_EXP node, then change "(e**x)**k" into
   "e**(k*x)" */
      if( (*node)->arg[ 0 ]->opcode == OP_EXP ) {
         newnode = NewNode( NULL, OP_EXP, status );
         node1 = NewNode( NULL, OP_MULT, status );
         if( astOK ) {
            node1->arg[ 0 ] = CopyTree( (*node)->arg[ 1 ], status );
            node1->arg[ 1 ] = CopyTree( (*node)->arg[ 0 ]->arg[ 0 ], status );
            newnode->arg[ 0 ] = node1;
         }

/* "x**0" can be replaced by 1.0 */
      } else if( (*node)->arg[ 1 ]->con == 0.0 ) {
         newnode = NewNode( NULL, OP_LDCON, status );
         if( astOK ) newnode->con = 1.0;

/* "x**1" can be replaced by x */
      } else if( EQUAL( (*node)->arg[ 1 ]->con, 1.0 ) ) {
         newnode = CopyTree( (*node)->arg[ 0 ], status );

/* If the first argument is an OP_POW node, then change "(x**k1)**k2" into
   "x**(k1*k2)" */
      } else if( (*node)->arg[ 0 ]->opcode == OP_POW ) {
         newnode = NewNode( NULL, OP_POW, status );
         node1 = NewNode( NULL, OP_LDCON, status );
         if( astOK ) {
            node1->con = ( (*node)->arg[ 0 ]->arg[ 1 ]->con )*
                         ( (*node)->arg[ 1 ]->con );
            newnode->arg[ 0 ] = CopyTree( (*node)->arg[ 0 ]->arg[ 0 ], status );
            newnode->arg[ 1 ] = node1;
         }

/* If the first argument is an OP_MULT node, then change "(x*y)**k" into
   "(x**(k))*(y**(k))" */
      } else if( std && (*node)->arg[ 0 ]->opcode == OP_MULT ) {
         newnode = NewNode( NULL, OP_MULT, status );
         node1 = NewNode( NULL, OP_POW, status );
         if( astOK ) {
            node1->arg[ 1 ] = CopyTree( (*node)->arg[ 1 ], status );
            node2 = CopyTree( node1, status );
            node1->arg[ 0 ] = CopyTree( (*node)->arg[ 0 ]->arg[ 0 ], status );
            node2->arg[ 0 ] = CopyTree( (*node)->arg[ 0 ]->arg[ 1 ], status );
            newnode->arg[ 0 ] = node1;
            newnode->arg[ 1 ] = node2;
         }
      }

/* Division. */
/* ========= */
/* We standardise divisions into corresponding multiplications. */
   } else if( op == OP_DIV ) {

/* Division by 1 is removed. */
      if( EQUAL( (*node)->arg[ 1 ]->con, 1.0 ) ){
         newnode = CopyTree( (*node)->arg[ 0 ], status );

/* Division by any other constant (except zero) is turned into a
   multiplication by the reciprocal constant. */
      } else if( (*node)->arg[ 1 ]->con != AST__BAD ) {
         if( (*node)->arg[ 1 ]->con != 0.0 ) {
            newnode = NewNode( NULL, OP_MULT, status );
            node1 = NewNode( NULL, OP_LDCON, status );
            if( astOK ) {
               node1->con = 1.0/(*node)->arg[ 1 ]->con;
               newnode->arg[ 0 ] = node1;
               newnode->arg[ 1 ] = CopyTree( (*node)->arg[ 0 ], status );
            }
         } else {
            astError( AST__BADUN, "Simplifying a units expression"
                      "requires a division by zero." , status);
         }

/* Other divisions "x/y" are turned into "x*(y**(-1))" */
      } else if( std ) {
         newnode = NewNode( NULL, OP_MULT, status );
         node1 = NewNode( NULL, OP_POW, status );
         node2 = NewNode( NULL, OP_LDCON, status );
         if( astOK ) {
            node2->con = -1.0;
            node1->arg[ 0 ] = CopyTree( (*node)->arg[ 1 ], status );
            node1->arg[ 1 ] = node2;
            newnode->arg[ 0 ] = CopyTree( (*node)->arg[ 0 ], status );
            newnode->arg[ 1 ] = node1;
         }
      }

/* Multiplication */
/* ============== */
   } else if( op == OP_MULT ) {

/* If the right hand argument is constant, swap the arguments. */
      if( (*node)->arg[ 1 ]->con != AST__BAD ) {
         newnode = NewNode( NULL, OP_MULT, status );
         if( astOK ) {
            newnode->arg[ 0 ] = CopyTree( (*node)->arg[ 1 ], status );
            newnode->arg[ 1 ] = CopyTree( (*node)->arg[ 0 ], status );
         }

/* Multiplication by zero produces a constant zero. */
      } else if( (*node)->arg[ 0 ]->con == 0.0 ){
         newnode = NewNode( NULL, OP_LDCON, status );
         if( astOK ) newnode->con = 0.0;

/* Multiplication by 1 is removed. */
      } else if( EQUAL( (*node)->arg[ 0 ]->con, 1.0 ) ){
         newnode = CopyTree( (*node)->arg[ 1 ], status );

/* For other MULT nodes, analyse the tree to find a list of all its
   factors with an associated power for each one, and an overall constant
   coefficient. */
      } else if( std ) {
         FindFactors( (*node), &factors, &powers, &nfactor, &coeff, status );

/* Produce a new tree from these factors. The factors are standardised by
   ordering them alphabetically (after conversion to a character string). */
         newnode = CombineFactors( factors, powers, nfactor, coeff, status );

/* Free resources */
         factors = astFree( factors );
         powers = astFree( powers );

      }
   }

/* If we have produced a new node which is identical to the old node,
   free it. Otherwise, indicate we have made some changes. */
   if( newnode ) {
      if( !CmpTree( newnode, *node, 1, status ) ) {
         newnode = FreeTree( newnode, status );
      } else {
         result = 1;
      }
   }

/* If an error has occurred, free any new node. */
   if( !astOK ) newnode = FreeTree( newnode, status );

/* If we have a replacement node, free the supplied tree and return a
   pointer to the new tree. */
   if( newnode ) {
      FreeTree( *node, status );
      *node = newnode;
   }

/* If the above produced some change, try re-simplifying the tree. */
   if( result ) SimplifyTree( node, std, status );

/* Return the result. */
   return result;

}

static int SplitUnit( const char *str, int ls, const char *u, int cs,
                      Multiplier **mult, int *l, int *status ) {
/*
*  Name:
*     SplitUnit

*  Purpose:
*     Split a given string into unit name and multiplier.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     int SplitUnit( const char *str, int ls, const char *u, int cs,
*                    Multiplier **mult, int *l, int *status  )

*  Class Membership:
*     Unit member function.

*  Description:
*     Returns non-zer0 if the supplied string ends with the supplied unit
*     name or label, and any leading string is a known multiplier.

*  Parameters:
*     str
*        The string to test, typically containing a multiplier and a unit
*        symbol or label.
*     ls
*        Number of characters to use from "str" (not including trailing null)
*     u
*        Pointer to the unit label or symbol string to be searched for.
*     cs
*        If non-zero, the test for "u" is case insensitive.
*     mult
*        Address of a location at which to return the multiplier at the
*        start of the supplied string. NULL is returned if the supplied
*        string does not match the supplied unit, or if the string
*        includes no multiplier.
*     l
*        Address of an int in which to return the length of "u".
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Non-zero if "str" ends with "u" and starts with a null string or a
*     known multiplier string.

*/

/* Local Variables: */
   int ret;
   int lm;
   int lu;

/* Initialise */
   ret = 0;
   *mult = NULL;
   *l = 0;

/* Check inherited status. */
   if( !astOK ) return ret;

/* Find the number of characters in the supplied unit label or symbol. The
   difference between lu and ls must be the length of the multiplier. */
   lu = strlen( u );
   lm = ls - lu;

/* Make sure "str" is not shorter than "u" */
   if( lm >= 0 ) {

/* Compare the end of "str" against "u" */
      if( cs ? !strncmp( str + lm, u, lu ) :
               !Ustrncmp( str + lm, u, lu, status ) ) {
         ret = 1;

/* If "str" ends with "u", see if it starts with a known multiplier */
         if( lm > 0 ) {
            ret = 0;
            *mult = GetMultipliers( status );
            while( *mult ) {
               if( (*mult)->symlen == lm && !strncmp( str, (*mult)->sym, lm ) ) {
                  ret = 1;
                  break;
               }
               *mult = (*mult)->next;
            }

/* If not, try again using case-insensitive matching. */
            if( !ret ) {
               *mult = GetMultipliers( status );
               while( *mult ) {
                  if( (*mult)->symlen == lm && !Ustrncmp( str, (*mult)->sym, lm, status ) ) {
                     ret = 1;
                     break;
                  }
                  *mult = (*mult)->next;
               }
            }

/* If not, try again using case-insensitive matching against the
   multiplier label. */
            if( !ret ) {
               *mult = GetMultipliers( status );
               while( *mult ) {
                  if( (*mult)->lablen == lm && !Ustrncmp( str, (*mult)->label, lm, status ) ) {
                     ret = 1;
                     break;
                  }
                  *mult = (*mult)->next;
               }
            }

         }
      }
   }

   *l = lu;
   return ret;
}

double astUnitAnalyser_( const char *in, double powers[9], int *status ){
/*
*+
*  Name:
*     astUnitAnalyser

*  Purpose:
*     Perform a dimensional analysis of a unti string.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "unit.h"
*     double astUnitAnalyser_( const char *in, double powers[9] )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function parses the supplied units string if possible, and
*     returns a set of pwoers and a scaling factor which represent the
*     units string.

*  Parameters:
*     in
*        A string representation of the units, for instance "km/h".
*     powers
*        An array in which are returned the powers for each of the following
*        basic units (in the order shown): kilogramme, metre, second, radian,
*        Kelvin, count, adu, photon, magnitude, pixel. If the supplied unit
*        does not depend on a given basic unit a value of 0.0 will be returned
*        in the array. The returns values represent a system of units which is
*        a scaled form of the supplied units, expressed in the basic units of
*        m, kg, s, rad, K, count, adu, photon, mag and pixel. For instance, a
*        returned array of [1,0,-2,0,0,0,0,0,0] would represent "m/s**2".

*  Returned Value:
*     A scaling factor for the supplied units. The is the value, in the
*     units represented by the returned powers, which corresponds to a
*     value of 1.0 in the supplied units.

*  Notes:
*     -  An error will be reported if the units string cannot be parsed
*     or refers to units or functions which cannot be analysed in this way.
*     -  AST__BAD is returned if this function is invoked with the
*     global error status set or if it should fail for any reason.
*-
*/

/* Local Variables: */
   UnitNode *in_tree;
   double result;

/* Initialise */
   result = AST__BAD;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Parse the input units string, producing a tree of UnitNodes which
   represents the input units. A pointer to the UnitNode at the head of
   the tree is returned if succesfull. Report a context message if this
   fails. */
   in_tree = CreateTree( in, 1, 1, status );
   if( in_tree ) {

/* Analyse the tree */
      if( !DimAnal( in_tree, powers, &result, status ) && astOK ) {
         result = AST__BAD;
         astError( AST__BADUN, "astUnitAnalyser: Error analysing input "
                   "units string '%s' (it may contain unsupported "
                   "functions or dimensionless units).", status, in );
      }

/* Free the tree. */
      in_tree = FreeTree( in_tree, status );

   } else if( astOK ) {
      astError( AST__BADUN, "astUnitAnalyser: Error parsing input "
                "units string '%s'.", status, in );
   }

/* Return the result */
   return result;
}

const char *astUnitLabel_( const char *sym, int *status ){
/*
*+
*  Name:
*     astUnitLabel

*  Purpose:
*     Return a string label for a given unit symbol.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "unit.h"
*     const char *astUnitLabel( const char *sym )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function returns a pointer to a constant string containing a
*     descriptive label for the unit specified by the given unit symbol.

*  Parameters:
*     sym
*        A string holing a known unit symbol.

*  Returned Value:
*     A pointer to constant string holding a descriptive label for the
*     supplied unit. A NULL pointer is returned (without error) if the
*     supplied unit is unknown.

*  Notes:
*     -  A NULL pointer is returned if this function is invoked with the
*     global error status set or if it should fail for any reason.
*-
*/

/* Local Variables: */
   const char *result;
   KnownUnit *unit;

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Ensure descriptions of the known units are available. */
   unit = GetKnownUnits( 1, status );

/* Loop through the chain of known units looking for a unit with a symbol
   equal to the supplied string. If found, store a pointer to its label
   and break out of the loop. */
   while( unit ) {
      if( !strcmp( sym, unit->sym ) ) {
         result = unit->label;
         break;
      }

      unit = unit->next;
   }

/* Return the answer. */
   return result;
}

AstMapping *astUnitMapper_( const char *in, const char *out,
                            const char *in_lab, char **out_lab, int *status ){
/*
*+
*  Name:
*     astUnitMapper

*  Purpose:
*     Create a Mapping between two system of units.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "unit.h"
*     AstMapping *astUnitMapper( const char *in, const char *out,
*                                const char *in_lab, char **out_lab )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function creates a Mapping between two specified system of
*     units. It also modifes a supplied label (which is typically
*     the axis label associated with the input units) so that it includes
*     any functional change implied by the supplied "in" and "out" units.

*  Parameters:
*     in
*        A string representation of the input units, for instance "km/h".
*        See "Unit Representations:" below.
*     out
*        A string representation of the output units, for instance "m/s".
*        See "Unit Representations:" below.
*     in_lab
*        A label describing the quantity associated with the input units.
*        If the "in" string is the Units attribute of an Axis, then
*        "in_lab" should be the Label of the same Axis. May be supplied
*        NULL in which case "out_lab" is ignored.
*     out_lab
*        The address at which to return a pointer to a label describing the
*        quantity associated with the output units. For instance, if the
*        input and output units are "Hz" and "sqrt(Hz)", and the input
*        label is "Frequency", then the returned output label will be
*        "sqrt( Frequency )". The returned label is stored in dynamically
*        allocated memory which should be freed (using astFree) when no longer
*        needed.

*  Returned Value:
*     A pointer to a Mapping which can be used to transform values in the
*     "in" system of units into the "out" system of units. The Mapping
*     will have 1 input and 1 output.

*  Unit Representations:
*     The string supplied for "in" and "out" should represent a system of
*     units following the recommendations of the FITS WCS paper I
*     "Representation of World Coordinates in FITS" (Greisen & Calabretta).
*     Various commonly used variants are also allowed.
*
*     To summarise, a string describing a system of units should be an
*     algebraic expression which combines one or more named units. The
*     following functions and operators may be used within these algebraic
*     expressions:
*
*     - "*": multiplication. A period "." or space " " may also be used
*       to represent multiplication (a period is only interpreted as a
*       multiplication operator if it is not positioned between two digits,
*       and a space is only interpreted as a multiplication operator if it
*       occurs between two operands).
*     - "/": division.
*     - "**": exponentiation. The exponent (i.e. the operand following the
*       exponentiation operator) must be a constant. The symbol "^" is also
*       interpreted as an exponentiation operator. Exponentiation is also
*       implied by an integer following a unit name without any separator
*       (e.g. "cm2" is "cm^2").
*     - log(): Common logarithm.
*     - ln(): Natural logarithm.
*     - sqrt(): Square root.
*     - exp(): Exponential.
*
*     Function names are case insensitive. White space may be included
*     within an expression (note that white space between two operands
*     will be interpreted as a muiltiplication operator as described
*     above). Parentheses may be used to indicate the order in which
*     expressions are to be evaluated (normal mathematical precedence is
*     used otherwise). The following symbols may be used to represent
*     constants:
*
*     - "pi"
*     - "e"
*
*     These symbols are also case in-sensitive.
*
*     The above operators and functions are used to combine together one
*     or more "unit symbols". The following base unit symbols are recognised:
*
*     - "m":  metre.
*     - "g":  gram.
*     - "s":  second.
*     - "rad":  radian.
*     - "sr":  steradian.
*     - "K":  Kelvin.
*     - "mol":  mole.
*     - "cd":  candela.
*
*     The following symbols for units derived fro the above basic units are
*     recognised:
*
*     - "sec":  second (1 s)
*     - "Hz":  Hertz  (1/s).
*     - "N":  Newton  (kg m/s**2).
*     - "J":  Joule  (N m).
*     - "W":  Watt  (J/s).
*     - "C":  Coulomb  (A s).
*     - "V":  Volt  (J/C).
*     - "Pa":  Pascal  (N/m**2).
*     - "Ohm":  Ohm  (V/A).
*     - "S":  Siemens  (A/V).
*     - "F":  Farad  (C/V).
*     - "Wb":  Weber  (V s).
*     - "T":  Tesla  (Wb/m**2).
*     - "H":  Henry  (Wb/A).
*     - "lm":  lumen  (cd sr).
*     - "lx":  lux  (lm/m**2).
*     - "deg":  degree  (pi/180 rad).
*     - "arcmin":  arc-minute  (1/60 deg).
*     - "arcsec":  arc-second  (1/3600 deg).
*     - "mas":  milli-arcsecond  (1/3600000 deg).
*     - "min":  minute  (60 s).
*     - "h":  hour  (3600 s).
*     - "d":  day  (86400 s).
*     - "yr":  year  (31557600 s).
*     - "a":  year  (31557600 s).
*     - "eV":  electron-Volt  (1.60217733E-19 J).
*     - "erg":  erg  (1.0E-7 J).
*     - "Ry":  Rydberg  (13.605692 eV).
*     - "solMass":  solar mass  (1.9891E30 kg).
*     - "u":  unified atomic mass unit  (1.6605387E-27 kg).
*     - "solLum":  solar luminosity  (3.8268E26 W).
*     - "Angstrom":  Angstrom  (1.0E-10 m).
*     - "Ang":  Angstrom
*     - "A":  Ampere
*     - "micron":  micron (1.0E-6 m).
*     - "solRad":  solar radius  (6.9599E8 m).
*     - "AU":  astronomical unit  (1.49598E11 m).
*     - "lyr":  light year  (9.460730E15 m).
*     - "pc":  parsec  (3.0867E16 m).
*     - "count":  count.
*     - "ct":  count.
*     - "adu":  analogue-to-digital converter unit.
*     - "photon":  photon.
*     - "ph":  photon.
*     - "Jy":  Jansky  (1.0E-26 W /m**2 /Hz).
*     - "Jan":  Jansky
*     - "mag":  magnitude.
*     - "G":  Gauss  (1.0E-4 T).
*     - "pixel":  pixel.
*     - "pix":  pixel.
*     - "barn":  barn  (1.0E-28 m**2).
*     - "D":  Debye  (1.0E-29/3 C.m).
*
*     In addition, any other unknown unit symbol may be used (but of course
*     no mapping will be possible between unknown units).
*
*     Unit symbols may be preceded with a numerical constant (for
*     instance "1000 m") or a standard multiplier symbol (for instance "km")
*     to represent some multiple of the unit. The following standard
*     multipliers are recognised:
*
*     - "d":   deci   (1.0E-1)
*     - "c":   centi  (1.0E-2)
*     - "m":   milli  (1.0E-3)
*     - "u":   micro  (1.0E-6)
*     - "n":   nano   (1.0E-9)
*     - "p":   pico   (1.0E-12)
*     - "f":   femto  (1.0E-15)
*     - "a":   atto   (1.0E-18)
*     - "z":   zepto  (1.0E-21)
*     - "y":   yocto  (1.0E-24)
*     - "da":  deca   (1.0E1)
*     - "h":   hecto  (1.0E2)
*     - "k":   kilo   (1.0E3)
*     - "M":   mega   (1.0E6)
*     - "G":   giga   (1.0E9)
*     - "T":   tera   (1.0E12)
*     - "P":   peta   (1.0E15)
*     - "E":   exa    (1.0E18)
*     - "Z":   zetta  (1.0E21)
*     - "Y":   yotta  (1.0E24)

*  Notes:
*     -  NULL values are returned without error if the supplied units are
*     incompatible (for instance, if the input and output units are "kg"
*     and "m" ).
*     -  NULL values are returned if this function is invoked with the
*     global error status set or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstMapping *result;
   UnitNode **units;
   UnitNode *in_tree;
   UnitNode *intemp;
   UnitNode *inv;
   UnitNode *labtree;
   UnitNode *newtest;
   UnitNode *out_tree;
   UnitNode *outtemp;
   UnitNode *src;
   UnitNode *testtree;
   UnitNode *tmp;
   UnitNode *totaltree;
   UnitNode *totlabtree;
   const char *c;
   const char *exp;
   int i;
   int nc;
   int nunits;
   int ipass;

/* Initialise */
   result = NULL;
   if( in_lab ) *out_lab = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* A quick check for a common simple case: if the two strings are
   identical, return a UnitMap.*/
   if( !strcmp( in, out ) ) {
      if( in_lab ) *out_lab = astStore( NULL, in_lab, strlen( in_lab ) + 1 );
      return (AstMapping *) astUnitMap( 1, "", status );
   }

/* More initialisation. */
   in_tree = NULL;
   out_tree = NULL;
   units = NULL;

/* Parse the input units string, producing a tree of UnitNodes which
   represents the input units. A pointer to the UnitNode at the head of
   the tree is returned if succesfull. Report a context message if this
   fails. The returned tree contains branch nodes which correspond to
   operators or functions, and leaf nodes which represent constant values
   or named basic units (m, s, g, K, etc). Each branch node has one or more
   "arguments" (i.e. child nodes) which are operated on or combined by
   the branch node in some way to produce the nodes "value". This value
   is then used as an argument for the node's parent node (if any). If
   the string supplied by the user refers to any known derived units (e.g. "N",
   Newton) then each such unit is represented in the returned tree by a
   complete sub-tree in which the head node corresponds to the derived
   unit (e.g. "N") and the leaf nodes correspond to the basic units needed
   to define the derived unit ( for instance, "m", "s" and "g" - metres,
   seconds and grammes), or numerical constants. Thus every leaf node in the
   returned tree will be a basic unit (i.e. a unit which is not defined in
   terms of other units), or a numerical constant. */
   in_tree = CreateTree( in, 1, 1, status );
   if( !astOK ) astError( AST__BADUN, "astUnitMapper: Error parsing input "
                          "units string '%s'.", status, in );

/* Do the same for the output units. */
   if( astOK ) {
      out_tree = CreateTree( out, 1, 1, status );
      if( !astOK ) astError( AST__BADUN, "astUnitMapper: Error parsing output "
                             "units string '%s'.", status, out );
   }

/* If a blank string is supplied for both input and output units, then
   assume a UnitMap is the appropriate Mapping. */
   if( !in_tree && !out_tree && astOK ) {
      result = (AstMapping *) astUnitMap( 1, "", status );
      if( in_lab ) *out_lab = astStore( NULL, in_lab, strlen( in_lab ) + 1 );

/* Otherwise, if we have both input and output trees... */
   } else if( in_tree && out_tree && astOK ) {

/* Locate all the basic units used within either of these two trees. An
   array is formed in which each element is a pointer to a UnitNode
   contained within one of the trees created above. Each basic unit
   referred to in either tree will have a single entry in this array
   (even if the unit is referred to more than once). */
      units = NULL;
      nunits = 0;
      LocateUnits( in_tree, &units, &nunits, status );
      LocateUnits( out_tree, &units, &nunits, status );

/* Due to the simple nature of the simplification process in SimplifyTree,
   the following alogorithm sometimes fails to find a Mapping form input
   to output units, but can find a Mapping from output to input units.
   In this latter case, we can get the required Mapping from input to
   output  simply by inverting the Mapign from output to input. So try
   first with the units in the original order. If this fails to find a
   Mapping, try again with the units swapped, and note that the final
   Mapping should be inverted before being used. */
      for( ipass = 0; ipass < 2; ipass++ ){
         if( ipass == 1 ) {
            tmp = in_tree;
            in_tree = out_tree;
            out_tree = tmp;
         }

/* We are going to create a new tree of UnitNodes in which the head node
   corresponds to the requested output units, and which has a single
   non-constant leaf node corresponding to the input units. Initialise a
   pointer to this new tree to indicate that it has not yet been created. */
         testtree = NULL;

/* Loop round each basic unit used in the definition of either the input
   or the output units (i.e. the elements of the array created above by
   "LocateUnits"). The unit selected by this loop is referred to as the
   "current" unit. On each pass through this loop, we create a tree which
   is a candidate for the final required tree (the "test tree" pointed to
   by the testtree pointer initialised above). In order for a mapping to
   be possible between input and output units, the test tree created on
   each pass through this loop must be equivalent to the test tree for the
   previous pass (in other words, all the test trees must be equivalent).
   We break out of the loop (and return a NULL Mapping) as soon as we find
   a test tree which differs from the previous test tree. */
         for( i = 0; i < nunits; i++ ) {

/* Create copies of the trees describing the input and output units, in which
   all units other than the current unit are set to a constant value of 1.
   This is done by replacing OP_LDVAR nodes (i.e. nodes which "load" the
   value of a named basic unit) by OP_LDCON nodes (i.e. nodes which load
   a specified constant value) in the tree copy. */
            intemp = FixUnits( in_tree, units[ i ], status );
            outtemp = FixUnits( out_tree, units[ i ], status );

/* Simplify these trees. An important side-effect of this simplification
   is that trees are "standardised" which allows them to be compared for
   equivalence. A single mathematical expression can often be represented
   in many different ways (for instance "A/B" is equivalent to "(B**(-1))*A").
   Standardisation is a process of forcing all equivalent representations
   into a single "standard" form. Without standardisation, trees representing
   the above two expressions would not be considered to be equivalent
   since thy would contain different nodes and have different structures.
   As a consequence of this standardisation, the "simplification" performed
   by SimplifyTree can sometimes actually make the tree more complicated
   (in terms of the number of nodes in the tree). */
            SimplifyTree( &intemp, 1, status );
            SimplifyTree( &outtemp, 1, status );

/* If either of the simplified trees does not depend on the current unit,
   then the node at the head of the simplified tree will have a constant
   value (because all the units other than the current unit have been fixed
   to a constant value of 1.0 above by FixUnits, leaving only the current
   unit to vary in value). If both simplified trees are constants, then
   neither tree depends on the current basic unit (i.e. references to the
   current basic unit cancel out within each string expression - for
   instance if converting from "m.s.Hz" to "km" and the current unit
   is "s", then the "s.Hz" term will cause the "s" units to cancel out). In
   this case ignore this basic unit and pass on to the next. */
            if( outtemp->con != AST__BAD && intemp->con != AST__BAD ) {

/* If just one simplified tree is constant, then the two units cannot
   match since one depends on the current basic unit and the other does
   not. Free any test tree from previous passes and break out of the loop. */
            } else if( outtemp->con != AST__BAD || intemp->con != AST__BAD ) {
               intemp = FreeTree( intemp, status );
               outtemp = FreeTree( outtemp, status );
               testtree = FreeTree( testtree, status );
               break;

/* If neither simplified tree is constant, both depend on the current
   basic unit and so we can continue to see if their dependencies are
   equivalent. */
            } else {

/* We are going to create a new tree which is the inverse of the above
   simplified "intemp" tree. That is, the new tree will have a head node
   corresponding to the current unit, and a single non-constant leaf node
   corresponding to the input units. Create an OP_LDVAR node which can be
   used as the leaf node for this inverted tree. If the input tree is
   inverted successfully, this root node becomes part of the inverted tree,
   and so does not need to be freed explicitly (it will be freed when the
   inverted tree is freed). */
               src = NewNode( NULL, OP_LDVAR, status );
               if( astOK ) src->name = astStore( NULL, "input_units", 12 );

/* Now produce the inverted input tree. If the tree cannot be inverted, a
   null pointer is returned. Check for this. Otherwise a pointer to the
   UnitNode at the head of the inverted tree is returned. */
               inv = InvertTree( intemp, src, status );
               if( inv ) {

/* Concatenate this tree (which goes from "input units" to "current unit")
   with the simplified output tree (which goes from "current unit" to
   "output units"), to get a new tree which goes from input units to output
   units. */
                  totaltree = ConcatTree( inv, outtemp, status );

/* Simplify this tree. */
                  SimplifyTree( &totaltree, 1, status );

/* Compare this simplified tree with the tree produced for the previous
   unit (if any). If they differ, we cannot map between the supplied
   units so annul the test tree and break out of the loop. If this is the
   first unit to be tested, use the total tree as the test tree for the
   next unit. */
                  if( testtree ) {
                     if( CmpTree( totaltree, testtree, 0, status ) ) testtree = FreeTree( testtree, status );
                     totaltree = FreeTree( totaltree, status );
                     if( !testtree ) break;
                  } else {
                     testtree = totaltree;
                  }
               }

/* If the input tree was inverted, free the inverted tree. */
               if( inv ) {
                  inv = FreeTree( inv, status );

/* If the input tree could not be inverted, we cannot convert between input
   and output units. Free the node which was created to be the root of the
   inverted tree (and which has consequently not been incorporated into the
   inverted tree), free any testtree and break out of the loop. */
               } else {
                  src = FreeTree( src, status );
                  testtree = FreeTree( testtree, status );
                  break;
               }
            }

/* Free the other trees. */
            intemp = FreeTree( intemp, status );
            outtemp = FreeTree( outtemp, status );

         }

/* If all the basic units used by either of the supplied system of units
   produced the same test tree, leave the "swap in and out units" loop. */
         if( testtree ) break;

      }

/* If the input and output units have been swapped, swap them back to
   their original order, and invert the test tree (if there is one). */
      if( ipass > 0 ) {
         tmp = in_tree;
         in_tree = out_tree;
         out_tree = tmp;
         if( testtree ) {
            src = NewNode( NULL, OP_LDVAR, status );
            if( astOK ) src->name = astStore( NULL, "input_units", 12 );
            newtest = InvertTree( testtree, src, status );
            FreeTree( testtree, status );
            testtree = newtest;
            if( !newtest ) src = FreeTree( src, status );
         }
      }

/* If all the basic units used by either of the supplied system of units
   produced the same test tree, create a Mapping which is equivalent to the
   test tree and return it. */
      if( testtree ) {
         result = MakeMapping( testtree, status );

/* We now go on to produce the output axis label from the supplied input
   axis label. Get a tree of UnitNodes which describes the supplied label
   associated with the input axis. The tree will have single OP_LDVAR node
   corresponding to the basic label (i.e. the label without any single
   argument functions or exponentiation operators applied). */
         if( in_lab && astOK ) {

/* Get a pointer to the first non-blank character, and store the number of
   characters to examine (this excludes any trailing white space). */
            exp = in_lab;
            while( isspace( *exp ) ) exp++;
            c = exp + strlen( exp ) - 1;
            while( c >= exp && isspace( *c ) ) c--;
            nc = c - exp + 1;

/* Create the tree. */
            labtree = MakeLabelTree( exp, nc, status );
            if( astOK ) {

/* Concatenate this tree (which goes from "basic label" to "input label")
   with the test tree found above (which goes from "input units" to "output
   units"), to get a tree which goes from basic label to output label. */
               totlabtree = ConcatTree( labtree, testtree, status );

/* Simplify this tree. */
               SimplifyTree( &totlabtree, 1, status );

/* Create the output label from this tree. */
               *out_lab = (char *) MakeExp( totlabtree, 0, 1, status );

/* Free the trees. */
               totlabtree = FreeTree( totlabtree, status );
               labtree = FreeTree( labtree, status );

/* Report a context error if the input label could not be parsed. */
            } else {
               astError( AST__BADUN, "astUnitMapper: Error parsing axis "
                         "label '%s'.", status, in_lab );
            }
         }

/* Free the units tree. */
         testtree = FreeTree( testtree, status );

      }
   }

/* Free resources. */
   in_tree = FreeTree( in_tree, status );
   out_tree = FreeTree( out_tree, status );
   units = astFree( units );

/* If an error has occurred, annul the returned Mapping. */
   if( !astOK ) {
      result = astAnnul( result );
      if( in_lab ) *out_lab = astFree( *out_lab );
   }

/* Return the result. */
   return result;

}

const char *astUnitNormaliser_( const char *in, int *status ){
/*
*+
*  Name:
*     astUnitNormalizer

*  Purpose:
*     Normalise a unit string into FITS-WCS format.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "unit.h"
*     const char *astUnitNormaliser( const char *in )

*  Class Membership:
*     Unit member function.

*  Description:
*     This function returns a standard FITS-WCS form of the supplied unit
*     string.

*  Parameters:
*     in
*        A string representation of the units, for instance "km/h".

*  Returned Value:
*     A pointer to a dynamically allocated string holding the normalized
*     unit string. It should be freed using astFree when no longer needed.

*  Notes:
*     -  An error will be reported if the units string cannot be parsed.
*     -  NULL is returned if this function is invoked with the
*     global error status set or if it should fail for any reason.
*-
*/

/* Local Variables: */
   UnitNode *in_tree;
   double dval;
   const char *result;

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Parse the input units string, producing a tree of UnitNodes which
   represents the input units. A pointer to the UnitNode at the head of
   the tree is returned if succesfull. Report a context message if this
   fails. */
   in_tree = CreateTree( in, 0, 1, status );
   if( in_tree ) {

/* Simplify the units expression, only doing genuine simplifications. */
      SimplifyTree( &in_tree, 1, status );

/* Invert literal constant unit multipliers. This is because a constant of
   say 1000 for a unit of "m" means "multiply the value in metres by 1000",
   but a unit string of "1000 m" means "value in units of 1000 m" (i.e.
   *divide* the value in metres by 1000). */
      InvertConstants( &in_tree, status );

/* Convert the tree into string form. */
      result = MakeExp( in_tree, 2, 1, status );

/* If the result is a constant value, return a blank string. */
      if( 1 == astSscanf( result, "%lg", &dval ) ) {
         *((char *) result) = 0;
      }

/* Free the tree. */
      in_tree = FreeTree( in_tree, status );

   } else {
      astError( AST__BADUN, "astUnitNormaliser: Error parsing input "
                "units string '%s'.", status, in );
   }

/* Return the result */
   return result;
}

static int Ustrcmp( const char *a, const char *b, int *status ){
/*
*  Name:
*     Ustrcmp

*  Purpose:
*     A case blind version of strcmp.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     int Ustrcmp( const char *a, const char *b, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     Returns 0 if there are no differences between the two strings, and 1
*     otherwise. Comparisons are case blind.

*  Parameters:
*     a
*        Pointer to first string.
*     b
*        Pointer to second string.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Zero if the strings match, otherwise one.

*  Notes:
*     -  This function does not consider the sign of the difference between
*     the two strings, whereas "strcmp" does.
*     -  This function attempts to execute even if an error has occurred.

*/

/* Local Variables: */
   const char *aa;         /* Pointer to next "a" character */
   const char *bb;         /* Pointer to next "b" character */
   int ret;                /* Returned value */

/* Initialise the returned value to indicate that the strings match. */
   ret = 0;

/* Initialise pointers to the start of each string. */
   aa = a;
   bb = b;

/* Loop round each character. */
   while( 1 ){

/* We leave the loop if either of the strings has been exhausted. */
      if( !(*aa ) || !(*bb) ){

/* If one of the strings has not been exhausted, indicate that the
   strings are different. */
         if( *aa || *bb ) ret = 1;

/* Break out of the loop. */
         break;

/* If neither string has been exhausted, convert the next characters to
   upper case and compare them, incrementing the pointers to the next
   characters at the same time. If they are different, break out of the
   loop. */
      } else {

         if( toupper( (int) *(aa++) ) != toupper( (int) *(bb++) ) ){
            ret = 1;
            break;
         }

      }

   }

/* Return the result. */
   return ret;

}

static int Ustrncmp( const char *a, const char *b, size_t n, int *status ){
/*
*  Name:
*     Ustrncmp

*  Purpose:
*     A case blind version of strncmp.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unit.h"
*     int Ustrncmp( const char *a, const char *b, size_t n, int *status )

*  Class Membership:
*     Unit member function.

*  Description:
*     Returns 0 if there are no differences between the first "n"
*     characters of the two strings, and 1 otherwise. Comparisons are
*     case blind.

*  Parameters:
*     a
*        Pointer to first string.
*     b
*        Pointer to second string.
*     n
*        The maximum number of characters to compare.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Zero if the strings match, otherwise one.

*  Notes:
*     -  This function does not consider the sign of the difference between
*     the two strings, whereas "strncmp" does.
*     -  This function attempts to execute even if an error has occurred.

*/

/* Local Variables: */
   const char *aa;         /* Pointer to next "a" character */
   const char *bb;         /* Pointer to next "b" character */
   int i;                  /* Character index */
   int ret;                /* Returned value */


/* Initialise the returned value to indicate that the strings match. */
   ret = 0;

/* Check pointer have been supplied. */
   if( !a || !b ) return ret;

/* Initialise pointers to the start of each string. */
   aa = a;
   bb = b;

/* Compare up to "n" characters. */
   for( i = 0; i < (int) n; i++ ){

/* We leave the loop if either of the strings has been exhausted. */
      if( !(*aa ) || !(*bb) ){

/* If one of the strings has not been exhausted, indicate that the
   strings are different. */
         if( *aa || *bb ) ret = 1;

/* Break out of the loop. */
         break;

/* If neither string has been exhausted, convert the next characters to
   upper case and compare them, incrementing the pointers to the next
   characters at the same time. If they are different, break out of the
   loop. */
      } else {

         if( toupper( (int) *(aa++) ) != toupper( (int) *(bb++) ) ){
            ret = 1;
            break;
         }

      }

   }

/* Return the result. */
   return ret;

}












/* The rest of this file contains functions which are of use for debugging
   this module. They are usually commented out.

static const char *DisplayTree( UnitNode *node, int ind ) {
   int i;
   char buf[200];
   const char *result;
   char *a;
   const char *arg[ 2 ];
   int rl;
   int slen;
   const opsym[ 100 ];

   result = "";

   for( i = 0; i < ind; i++ ) buf[ i ] = ' ';
   buf[ ind ] = 0;

   if( !node ) {
      printf( "%s <null>\n", buf );
   } else {

      printf( "%s Code: '%s' (%d)\n", buf, OpName( node->opcode ), node->opcode );
      printf( "%s Narg: %d\n", buf, node->narg );
      printf( "%s Constant: %g\n", buf, node->con );
      printf( "%s Name: %s\n", buf, node->name?node->name:"" );
      printf( "%s Unit: %s\n", buf, node->unit?node->unit->sym:"" );
      printf( "%s Mult: %s\n", buf, node->mult?node->mult->sym:"" );

      OpSym( node, opsym );
      slen = strlen( opsym );
      rl = slen;

      if( node->narg == 0 ) {
         result = astMalloc( rl + 1 );
         if( astOK ) strcpy( (char *) result, opsym );

      } else if( node->narg == 1 ) {
         rl += 2;
         printf( "%s Arg 0:\n", buf );
         arg[ 0 ] = DisplayTree( (node->arg)[ 0 ], ind + 2 );
         rl += strlen( arg[ 0 ] );

         result = astMalloc( rl + 1 );
         if( astOK ) {
            a = (char *) result;
            strcpy( a, opsym );
            a += slen;
            *(a++) = '(';
            strcpy( a, arg[0] );
            a += strlen( arg[ 0 ] );
            *(a++) = ')';
         }

      } else {
         rl += 4;
         for( i = 0; i < node->narg; i++ ) {
            printf( "%s Arg %d:\n", buf, i );
            arg[ i ] = DisplayTree( (node->arg)[ i ], ind + 2 );
            rl += strlen( arg[ i ] );
         }

         result = astMalloc( rl + 1 );
         if( astOK ) {
            a = (char *) result;
            *(a++) = '(';
            strcpy( a, arg[0] );
            a += strlen( arg[ 0 ] );
            *(a++) = ')';
            strcpy( a, opsym );
            a += slen;
            *(a++) = '(';
            strcpy( a, arg[1] );
            a += strlen( arg[ 1 ] );
            *(a++) = ')';
         }
      }
   }

   if( !astOK ) {
      astFree( (void *) result );
      result = "";
   }

   return result;
}

static const char *OpName( Oper op ) {
   const char *name;

   if( op ==  OP_LDCON ) {
      name = "LDCON";
   } else if( op ==  OP_LDVAR ) {
      name = "LDVAR";
   } else if( op ==  OP_LOG ) {
      name = "LOG";
   } else if( op ==  OP_LN ) {
      name = "LN";
   } else if( op ==  OP_EXP ) {
      name = "EXP";
   } else if( op ==  OP_SQRT ) {
      name = "SQRT";
   } else if( op ==  OP_POW ) {
      name = "POW";
   } else if( op ==  OP_DIV ) {
      name = "DIV";
   } else if( op ==  OP_MULT ) {
      name = "MULT";
   } else if( op ==  OP_LDPI ) {
      name = "LDPI";
   } else if( op ==  OP_LDE ) {
      name = "LDE";
   } else if( op ==  OP_NULL ) {
      name = "NULL";
   } else {
      name = "<unknown op code>";
   }

   return name;
}

static void OpSym( UnitNode *node, char *buff ) {
   const char *sym = NULL;

   if( node->con != AST__BAD ) {
      sprintf( buff, "%g", node->con );

   } else if( node->opcode ==  OP_LDVAR ) {
      sym = node->name;

   } else if( node->opcode ==  OP_LOG ) {
      sym = "log";

   } else if( node->opcode ==  OP_LN ) {
      sym = "ln";

   } else if( node->opcode ==  OP_EXP ) {
      sym = "exp";

   } else if( node->opcode ==  OP_SQRT ) {
      sym = "sqrt";

   } else if( node->opcode ==  OP_POW ) {
      sym = "**";

   } else if( node->opcode ==  OP_DIV ) {
      sym = "/";

   } else if( node->opcode ==  OP_MULT ) {
      sym = "*";

   } else if( node->opcode ==  OP_NULL ) {
      sym = "NULL";

   } else {
      sym = "<unknown op code>";
   }

   if( sym ) strcpy( buff, sym );
}

static const char *TreeExp( UnitNode *node ) {
   char buff[ 100 ];
   char buff2[ 100 ];

   if( node->narg == 0 ) {
      OpSym( node, buff );

   } else if( node->narg == 1 ) {
      OpSym( node, buff2 );
      sprintf( buff, "%s(%s)", buff2, TreeExp( node->arg[ 0 ] ) );

   } else if( node->narg == 2 ) {
      OpSym( node, buff2 );
      sprintf( buff, "(%s)%s(%s)", TreeExp( node->arg[ 0 ] ), buff2,
                                   TreeExp( node->arg[ 1 ] ) );
   }

   return astStore( NULL, buff, strlen( buff ) + 1 );
}

*/




