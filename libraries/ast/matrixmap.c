/*
*class++
*  Name:
*     MatrixMap

*  Purpose:
*     Map coordinates by multiplying by a matrix.

*  Constructor Function:
c     astMatrixMap
f     AST_MATRIXMAP

*  Description:
*     A MatrixMap is form of Mapping which performs a general linear
*     transformation. Each set of input coordinates, regarded as a
*     column-vector, are pre-multiplied by a matrix (whose elements
*     are specified when the MatrixMap is created) to give a new
*     column-vector containing the output coordinates. If appropriate,
*     the inverse transformation may also be performed.

*  Inheritance:
*     The MatrixMap class inherits from the Mapping class.

*  Attributes:
*     The MatrixMap class does not define any new attributes beyond
*     those which are applicable to all Mappings.

*  Functions:
c     The MatrixMap class does not define any new functions beyond those
f     The MatrixMap class does not define any new routines beyond those
*     which are applicable to all Mappings.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     DSB: D.S. Berry (Starlink)
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     9-FEB-1996 (DSB):
*        Original version.
*     13-NOV-1996 (DSB):
*        Updated to support attributes, I/O and an external interface.
*     3-JUN-1997 (DSB):
*        astMtrMult and astMtrRot made protected instead of public.
*     16-JUN-1997 (RFWS):
*        Tidied public prologues.
*     24-JUN-1997 (DSB):
*        Zero returned for coordinates which are indeterminate as a
*        result of using an inverted, non-square, diagonal matrix.
*     10-OCT-1997 (DSB):
*        o  The inverse matrix is no longer dumped by the Dump function.
*        Instead, it is re-calculated by the Load function.
*        o  The description of argument "form" in astMatrixMap corrected
*        to indicate that a value of 2 produces a unit matrix.
*        o  String values used to represent choices externally, instead
*        of integers.
*     24-NOV-1997 (DSB):
*        Use of error code AST__OPT replaced by AST__RDERR.
*     28-JAN-1998 (DSB):
*        Bug fix in astMtrMult: the matrix (forward or inverse) used for
*        the "a" MatrixMap was determined by the Invert flag of the other
*        ("this") MatrixMap.
*     14-APR-1998 (DSB):
*        Bug fix in Dump. Previously, matrix elements with value AST__BAD
*        were explicitly written out. Now they are not written out, since
*        AST__BAD can have different values on different machines. Missing
*        elements default to AST__BAD when read back in using astLoadMatrixMap.
*     20-APR-1998 (DSB):
*        Bug fix in astLoadMatrixMap: initialise the pointer to the inverse
*        matrix array to NULL if no inverse matrix is needed.
*     25-AUG-1998 (DSB):
*        - Transform changed so that bad input axis values are not
*        propagated to output axes which are independant of the input axis.
*        - CompressMatrix changed to allow a tolerance of DBL_EPSILON when
*        determining if a matrix is a unit matrix, or a diagonal matrix.
*        - MapMerge changed to allow MatrixMaps to swap with PermMaps
*        in order to move the MatrixMap closer to a Mapping with which it
*        could merge.
*     22-FEB-1999 (DSB):
*        Changed logic of MapMerge to avoid infinite looping.
*     5-MAY-1999 (DSB):
*        More corrections to MapMerge: Cleared up errors in the use of the
*        supplied invert flags, and corrected logic for deciding which
*        neighbouring Mapping to swap with.
*     16-JUL-1999 (DSB):
*        Fixed memory leaks in MatWin and MapMerge.
*     8-JAN-2003 (DSB):
*        Changed private InitVtab method to protected astInitatrixMapVtab
*        method.
*     11-SEP-2003 (DSB):
*        Increased tolerance on checks for unit matrices within
*        CompressMatrix. Now uses sqrt(DBL_EPSILON)*diag (previously was
*        DBL_EPSILON*DIAG ).
*     10-NOV-2003 (DSB):
*        Modified functions which swap a MatrixMap with another Mapping
*        (e.g. MatSwapPerm, etc), to simplify the returned Mappings.
*     13-JAN-2003 (DSB):
*        Modified the tolerance used by CompressMatrix when checking for
*        zero matrix elements. Old system compared each element to thre
*        size of the diagonal, but different scalings on different axes could
*        cause this to trat as zero values which should nto be treated as
*        zero.
*     23-APR-2004 (DSB):
*        Changes to simplification algorithm.
*     8-JUL-2004 (DSB):
*        astMtrMult - Report an error if either MatrixMap does not have a
*        defined forward transformation.
*     1-SEP-2004 (DSB):
*        Ensure do1 and do2 are initialised before use in MapMerge.
*     7-SEP-2005 (DSB):
*        Take account of the Invert flag when using the zoom factor from
*        a ZoomMap.
*     14-FEB-2006 (DSB):
*        Correct row/col confusion in CompressMatrix.
*     15-MAR-2006 (DSB):
*        Override astEqual.
*     15-MAR-2009 (DSB):
*        MapSplit: Only create the returned Mapping if it would have some
*        outputs. Also, do not create the returned Mapping if any output
*        depends on a mixture of selected and unselected inputs.
*     16-JUL-2009 (DSB):
*        MatPerm: Fix memory leak (mm2 was not being annulled).
*     2-OCT-2012 (DSB):
*        - Check for Infs as well as NaNs.
*        - In MapSplit do not split the MatrixMap if the resulting
*          matrix would contain only bad elements.
*        - Report an error if an attempt is made to create a MatrixMap
*          containing only bad elements.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS MatrixMap

/* Define identifiers for the different forms of matrix storage. */
#define FULL       0
#define DIAGONAL   1
#define UNIT       2

/* Macros which return the maximum and minimum of two values. */
#define MAX(aa,bb) ((aa)>(bb)?(aa):(bb))
#define MIN(aa,bb) ((aa)<(bb)?(aa):(bb))

/* Macro to check for equality of floating point values. We cannot
compare bad values directory because of the danger of floating point
exceptions, so bad values are dealt with explicitly. */
#define EQUAL(aa,bb) (((aa)==AST__BAD)?(((bb)==AST__BAD)?1:0):(((bb)==AST__BAD)?0:(fabs((aa)-(bb))<=1.0E5*MAX((fabs(aa)+fabs(bb))*DBL_EPSILON,DBL_MIN))))

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* Sets of points/coordinates */
#include "mapping.h"             /* Coordinate mappings (parent class) */
#include "matrixmap.h"           /* Interface definition for this class */
#include "pal.h"              /* SLALIB function definitions */
#include "permmap.h"
#include "zoommap.h"
#include "unitmap.h"
#include "winmap.h"

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Module Variables. */
/* ================= */

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;
static const char *Form[3] = { "Full", "Diagonal", "Unit" }; /* Text values
                                   used to represent storage form externally */

/* Pointers to parent class methods which are extended by this class. */
static AstPointSet *(* parent_transform)( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static int *(* parent_mapsplit)( AstMapping *, int, const int *, AstMapping **, int * );


#ifdef THREAD_SAFE
/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(MatrixMap)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(MatrixMap,Class_Init)
#define class_vtab astGLOBAL(MatrixMap,Class_Vtab)


#include <pthread.h>


#else


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstMatrixMapVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstMatrixMap *astMatrixMapId_( int, int, int, const double [], const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstMatrixMap *MatMat( AstMapping *, AstMapping *, int, int, int * );
static AstMatrixMap *MatPerm( AstMatrixMap *, AstPermMap *, int, int, int, int * );
static AstMatrixMap *MatZoom( AstMatrixMap *, AstZoomMap *, int, int, int * );
static AstMatrixMap *MtrMult( AstMatrixMap *, AstMatrixMap *, int * );
static AstMatrixMap *MtrRot( AstMatrixMap *, double, const double[], int * );
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static double *InvertMatrix( int, int, int, double *, int * );
static double Rate( AstMapping *, double *, int, int, int * );
static int Equal( AstObject *, AstObject *, int * );
static int FindString( int, const char *[], const char *, const char *, const char *, const char *, int * );
static int Ustrcmp( const char *, const char *, int * );
static int GetTranForward( AstMapping *, int * );
static int GetIsLinear( AstMapping *, int * );
static int GetTranInverse( AstMapping *, int * );
static int CanSwap( AstMapping *, AstMapping *, int, int, int *, int * );
static int MapMerge( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
static int PermOK( AstMapping *, int * );
static int ScalingRowCol( AstMatrixMap *, int, int * );
static void CompressMatrix( AstMatrixMap *, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Delete( AstObject *obj, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void ExpandMatrix( AstMatrixMap *, int * );
static void MatWin( AstMapping **, int *, int, int * );
static void MatPermSwap( AstMapping **, int *, int, int * );
static void PermGet( AstPermMap *, int **, int **, double **, int * );
static void SMtrMult( int, int, int, const double *, double *, double*, int * );
static int *MapSplit( AstMapping *, int, const int *, AstMapping **, int * );

/* Member functions. */
/* ================= */
static int CanSwap( AstMapping *map1, AstMapping *map2, int inv1, int inv2,
                    int *simpler, int *status ){
/*
*  Name:
*     CanSwap

*  Purpose:
*     Determine if two Mappings could be swapped.

*  Type:
*     Private function.

*  Synopsis:
*     #include "matrixmap.h"
*     int CanSwap( AstMapping *map1, AstMapping *map2, int inv1, int inv2,
*                  int *simpler, int *status )

*  Class Membership:
*     MatrixMap member function

*  Description:
*     This function returns a flag indicating if the pair of supplied
*     Mappings could be replaced by an equivalent pair of Mappings from the
*     same classes as the supplied pair, but in reversed order. Each pair
*     of Mappings is considered to be compunded in series. The supplied
*     Mapings are not changed in any way.

*  Parameters:
*     map1
*        The Mapping to be applied first.
*     map2
*        The Mapping to be applied second.
*     inv1
*        The invert flag to use with map1. A value of zero causes the forward
*        mapping to be used, and a non-zero value causes the inverse
*        mapping to be used.
*     inv2
*        The invert flag to use with map2.
*     simpler
*        Addresss of a location at which to return a flag indicating if
*        the swapped Mappings would be intrinsically simpler than the
*        original Mappings.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     1 if the Mappings could be swapped, 0 otherwise.

*  Notes:
*     -  One of the supplied pair of Mappings must be a MatrixMap.
*     -  A value of 0 is returned if an error has already occurred, or if
*     this function should fail for any reason.
*/

/* Local Variables: */
   AstMatrixMap *mat;        /* Pointer to MatrixMap Mapping */
   AstMapping *nomat;        /* Pointer to non-MatrixMap Mapping */
   const char *class1;       /* Pointer to map1 class string */
   const char *class2;       /* Pointer to map2 class string */
   const char *nomat_class;  /* Pointer to non-MatrixMap class string */
   double *consts;           /* Pointer to constants array */
   int *inperm;              /* Pointer to input axis permutation array */
   int *outperm;             /* Pointer to output axis permutation array */
   int i;                    /* Loop count */
   int invert[ 2 ];          /* Original invert flags */
   int nax;                  /* No. of in/out coordinates for the MatrixMap */
   int nin;                  /* No. of input coordinates for the PermMap */
   int nout;                 /* No. of output coordinates for the PermMap */
   int ret;                  /* Returned flag */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Initialise */
   ret = 0;
   *simpler = 0;

/* Temporarily set the Invert attributes of both Mappings to the supplied
   values. */
   invert[ 0 ] = astGetInvert( map1 );
   astSetInvert( map1, inv1 );

   invert[ 1 ] = astGetInvert( map2 );
   astSetInvert( map2, inv2 );

/* Get the classes of the two mappings. */
   class1 = astGetClass( map1 );
   class2 = astGetClass( map2 );
   if( astOK ){

/* Get a pointer to the MatrixMap and non-MatrixMap Mappings. */
      if( !strcmp( class1, "MatrixMap" ) ){
         mat = (AstMatrixMap *) map1;
         nomat = map2;
         nomat_class = class2;
      } else {
         nomat = map1;
         mat = (AstMatrixMap *) map2;
         nomat_class = class1;
      }

/* Get the number of input axes for the MatrixMap. */
      nax = astGetNin( mat );

/* If it is a WinMap, the Mappings can be swapped. */
      if( !strcmp( nomat_class, "WinMap" ) ){
         ret = 1;

/* If it is a PermMap, the Mappings can be swapped so long as:
   1) all links between input and output axes in the PermMap are
   bi-directional. This does not preclude the existence of unconnected
   axes, which do not have links (bi-directional or otherwise).
   2) The MatrixMap is square, and invertable.
   3) If the permMap is applied first, then each output of the PermMap
      which is assigned a constant value must correspond to a "scaling" row
      and column in the MatrixMap. I.e. if PermMap output axis "i" is
      assigned a constant value, then row i and column i of the following
      MatrixMap must contain only zeros, EXCEPT for the diagonal term (row
      i, column i) which must be non-zero. If the Mappings are in the other
      order, then the same applies to PermMap input axes assigned a constant
      value. */

/* Check the other Mapping is a PermMap, and that the MatrixMap is square
   and has an inverse. */
      } else if( !strcmp( nomat_class, "PermMap" ) &&
                 nax == astGetNout( mat ) && ( mat->form == UNIT ||
                 ( mat->i_matrix != NULL &&
                   mat->f_matrix != NULL ) ) ) {

/* Get the number of input and output coordinates for the PermMap. */
         nin = astGetNin( nomat );
         nout = astGetNout( nomat );

/* We need to know the axis permutation arrays and constants array for
   the PermMap. */
         PermGet( (AstPermMap *) nomat, &outperm, &inperm, &consts, status );
         if( astOK ) {

/* Indicate we can swap with the PermMap. */
            ret = 1;

/* Check each output axis. If any links between axes are found which are
   not bi-directional, indicate that we cannot swap with the PermMap. */
            for( i = 0; i < nout; i++ ){
               if( outperm[ i ] >= 0 && outperm[ i ] < nin ) {
                  if( inperm[ outperm[ i ] ] != i ) {
                     ret = 0;
                     break;
                  }
               }
            }

/* Check each input axis. If any links between axes are found which are
   not bi-directional, indicate that we cannot swap with the PermMap. */
            for( i = 0; i < nin; i++ ){
               if( inperm[ i ] >= 0 && inperm[ i ] < nout ) {
                  if( outperm[ inperm[ i ] ] != i ) {
                     ret = 0;
                     break;
                  }
               }
            }

/* If the PermMap is suitable, check that any constant values fed from the
   PermMap into the MatrixMap (in either forward or inverse direction)
   are not changed by the MatrixMap. This requires the row and column for
   each constant axis to be zeros, ecept for a value of 1.0 on the
   diagonal. First deal with the cases where the PermMap is applied
   first, so the outputs of the PermMap are fed into the MatrixMap in the
   forward direction. */
            if( ret && ( nomat == map1 ) ) {

               if( nout != nax ){
                  astError( AST__RDERR, "PermMap produces %d outputs, but the following"
                            "MatrixMap has %d inputs\n", status, nout, nax );
                  ret = 0;
               }

/* Consider each output axis of the PermMap. */
               for( i = 0; i < nout && astOK ; i++ ) {

/* If this PermMap output is assigned a constant... */
                  if( outperm[ i ] < 0 || outperm[ i ] >= nin ) {

/* Check the i'th row of the MatrixMap is all zero except for the i'th
   column which must be non-zero. If not indicate that the MatrixMap cannot
   swap with the PermMap and leave the loop. */
                     if( !ScalingRowCol( mat, i, status ) ) {
                        ret = 0;
                        break;
                     }
                  }
               }
            }

/* Now deal with the cases where the PermMap is applied second, so the inputs
   of the PermMap are fed into the MatrixMap in the inverse direction. */
            if( ret && ( nomat == map2 ) ) {

               if( nin != nax ){
                  astError( AST__RDERR, "Inverse PermMap produces %d inputs, but the "
                            "preceding MatrixMap has %d outputs\n", status, nin, nax );
                  ret = 0;
               }

/* Consider each input axis of the PermMap. */
               for( i = 0; i < nin && astOK; i++ ){

/* If this PermMap input is assigned a constant (by the inverse Mapping)... */
                  if( inperm[ i ] < 0 || inperm[ i ] >= nout ) {

/* Check the i'th row of the MatrixMap is all zero except for the i'th
   column which must be non-zero. If not indicate that the MatrixMap cannot
   swap with the PermMap and leave the loop. */
                     if( !ScalingRowCol( mat, i, status ) ) {
                        ret = 0;
                        break;
                     }
                  }
               }
            }

/* If we can swap with the PermMap, the swapped Mappings may be
   intrinsically simpler than the original mappings. */
            if( ret ) {

/* If the PermMap precedes the WinMap, this will be the case if the PermMap
   has more outputs than inputs. If the WinMap precedes the PermMap, this
   will be the case if the PermMap has more inputs than outputs. */
               *simpler = ( nomat == map1 ) ? nout > nin : nin > nout;
            }

/* Free the axis permutation and constants arrays. */
            outperm = (int *) astFree( (void *) outperm );
            inperm = (int *) astFree( (void *) inperm );
            consts = (double *) astFree( (void *) consts );
         }
      }
   }

/* Re-instate the original settings of the Invert attributes for the
   supplied MatrixMaps. */
   astSetInvert( map1, invert[ 0 ] );
   astSetInvert( map2, invert[ 1 ] );

/* Return the answer. */
   return astOK ? ret : 0;
}

static void CompressMatrix( AstMatrixMap *this, int *status ){
/*
*  Name:
*     CompressMatrix

*  Purpose:
*     If possible, reduce the amount of storage needed to store a MatrixMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "matrixmap.h"
*     void CompressMatrix( AstMatrixMap *this, int *status )

*  Class Membership:
*     MatrixMap member function.

*  Description:
*     The supplid MatrixMap is converted to its most compressed form
*     (i.e no element values if it is a unit matrix, diagonal elements only
*     if it is a diagonal matrix, or all elements otherwise).

*  Parameters:
*     this
*        A pointer to the MatrixMap to be compressed.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   double *a;                     /* Pointer to next element */
   double *colmax;                /* Pointer to array holding column max values */
   double *fmat;                  /* Pointer to compressed forward matrix */
   double *rowmax;                /* Pointer to array holding row max values */
   double mval;                   /* Matrix element value */
   int i;                         /* Loop count */
   int j;                         /* Loop count */
   int k;                         /* Loop count */
   int ncol;                      /* No. of columns in forward matrix */
   int ndiag;                     /* No. of diagonal elements in matrix */
   int new_form;                  /* Compressed storage form */
   int new_inv;                   /* New inverse requied? */
   int next_diag;                 /* Index of next diagonal element */
   int nrow;                      /* No. of rows in forward matrix */

/* Check the global error status. */
   if ( !astOK || !this ) return;

/* Initialise variables to avoid "used of uninitialised variable"
   messages from dumb compilers. */
   new_inv = 0;

/* Get the dimensions of the forward matrix. */
   if( astGetInvert( this ) ){
      nrow = astGetNin( this );
      ncol = astGetNout( this );
   } else {
      ncol = astGetNin( this );
      nrow = astGetNout( this );
   }

/* Store the number of diagonal elements in the matrix. This is the
   minimum of the number of rows and columns. */
   if( ncol < nrow ){
      ndiag = ncol;
   } else {
      ndiag = nrow;
   }

/* If the MatrixMap is already stored in UNIT form, it cannot be compressed
   any further. */
   if( this->form == UNIT){
      return;

/* Otherwise, if the MatrixMap is stored in DIAGONAL form, it could be
   compressed into a UNIT MatrixMap if all the supplied element values are
   one. */
   } else if( this->form == DIAGONAL ){
      new_form = UNIT;
      for( i = 0; i < ndiag; i++ ){
         if( !EQUAL( (this->f_matrix)[ i ], 1.0 ) ){
            new_form = DIAGONAL;
            break;
         }
      }

/* If it can be compressed, change the storage form and free the arrays
   holding the diagonal element values. */
      if( new_form == UNIT ) {
         this->f_matrix = (double *) astFree( (void *)( this->f_matrix ) );
         this->i_matrix = (double *) astFree( (void *)( this->i_matrix ) );
         this->form = UNIT;
      }

/* Otherwise, a full MatrixMap has been supplied, but this could be stored
   in a unit or diagonal MatrixMap if the element values are appropriate. */
   } else {
      new_form = FULL;

/* Find the maximum absolute value in each column. Scale by
   sqrt(DBL_EPSILON) to be come a lower limit for non-zero values. */
      colmax = astMalloc( ncol*sizeof( double ) );
      if( colmax ) {
         for( j = 0; j < ncol; j++ ) {
            colmax[ j ] = 0.0;
            i = j;
            for( k = 0; k < nrow; k++ ) {
               mval = (this->f_matrix)[ i ];
               if( mval != AST__BAD ) {
                  mval = fabs( mval );
                  if( mval > colmax[ j ] ) colmax[ j ] = mval;
               }
               i += ncol;
            }
            colmax[ j ] *= sqrt( DBL_EPSILON );
         }
      }

/* Find the maximum absolute value in each row. Scale by
   sqrt(DBL_EPSILON) to be come a lower limit for non-zero values. */
      rowmax = astMalloc( nrow*sizeof( double ) );
      if( rowmax ) {
         for( k = 0; k < nrow; k++ ) {
            rowmax[ k ] = 0.0;
            i = k*ncol;
            for( j = 0; j < ncol; j++ ) {
               mval = (this->f_matrix)[ i ];
               if( mval != AST__BAD ) {
                  mval = fabs( mval );
                  if( mval > rowmax[ k ] ) rowmax[ k ] = mval;
               }
               i++;
            }
            rowmax[ k ] *= sqrt( DBL_EPSILON );
         }
      }

/* Check memory can be used */
      if( astOK ) {

/* Initialise a flag indicating that the inverse matrix does not need to
   be re-calculated. */
         new_inv = 0;

/* Initially assume that the forward matrix is a unit matrix. */
         new_form = UNIT;

/* Store a pointer to the next matrix element. */
         a = this->f_matrix;

/* Loop through all the rows in the forward matrix array. */
         for( k = 0; k < nrow; k++ ) {

/* Loop through all the elements in this column. */
            for( j = 0; j < ncol; j++, a++ ) {

/* If this element is bad, use full form. */
               if( *a == AST__BAD ) {
                  new_form = FULL;

/* Otherwise, if this is a diagonal term, check its value. If it is not one,
   then the matrix cannot be a unit matrix, but it could still be a diagonal
   matrix. */
               } else {
                  if( j == k ) {
                     if( *a != 1.0 && new_form == UNIT ) new_form = DIAGONAL;

/* If this is not a diagonal element, and the element value is not zero,
   then the matrix is not a diagonal matrix. Allow a tolerance of
   SQRT(DBL_EPSILON) times the largest value in the same row or column as
   the current matrix element. That is, an element must be insignificant
   to both its row and its column to be considered as effectively zero.
   Replace values less than this limit with zero. */
                  } else {
                     mval = fabs( *a );
                     if( mval <= rowmax[ k ] &&
                         mval <= colmax[ j ] ) {

/* If the element will change value, set a flag indicating that the inverse
   matrix needs to be re-calculated. */
                        if( *a != 0.0 ) new_inv = 1;

/* Ensure this element value is zero. */
                        *a = 0.0;

                     } else {
                        new_form = FULL;
                     }
                  }
               }
            }
         }
      }

/* Free memory. */
      colmax = astFree( colmax );
      rowmax = astFree( rowmax );

/* If it can be compressed into a UNIT MatrixMap, change the storage form and
   free the arrays holding the element values. */
      if( new_form == UNIT ) {
         this->f_matrix = (double *) astFree( (void *)( this->f_matrix ) );
         this->i_matrix = (double *) astFree( (void *)( this->i_matrix ) );
         this->form = UNIT;

/* Otherwise, if it can be compressed into a DIAGONAL MatrixMap, copy the
   diagonal elements from the full forward matrix into a newly allocated
   array, use this array to replace the forward matrix array in the MatrixMap,
   create a new inverse matrix, and change the storage form. */
      } else if( new_form == DIAGONAL ) {
         fmat = astMalloc( sizeof(double)*(size_t)ndiag );
         if( fmat ){

            next_diag = 0;
            for( i = 0; i < ndiag; i++ ){
               fmat[ i ] = (this->f_matrix)[ next_diag ];
               next_diag += ncol + 1;
            }

            (void) astFree( (void *) this->f_matrix );
            (void) astFree( (void *) this->i_matrix );

            this->f_matrix = fmat;
            this->i_matrix = InvertMatrix( DIAGONAL, nrow, ncol, fmat, status );
            this->form = DIAGONAL;

         }

/* Calculate a new inverse matrix if necessary. */
      } else if( new_inv ) {
         (void) astFree( (void *) this->i_matrix );
         this->i_matrix = InvertMatrix( FULL, nrow, ncol, this->f_matrix, status );
      }
   }

   return;

}

static int Equal( AstObject *this_object, AstObject *that_object, int *status ) {
/*
*  Name:
*     Equal

*  Purpose:
*     Test if two MatrixMaps are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "matrixmap.h"
*     int Equal( AstObject *this, AstObject *that, int *status )

*  Class Membership:
*     MatrixMap member function (over-rides the astEqual protected
*     method inherited from the astMapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two MatrixMaps are equivalent.

*  Parameters:
*     this
*        Pointer to the first Object (a MatrixMap).
*     that
*        Pointer to the second Object.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the MatrixMaps are equivalent, zero otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstMatrixMap *that;
   AstMatrixMap *this;
   double *that_matrix;
   double *this_matrix;
   int i;
   int nin;
   int nout;
   int result;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain pointers to the two MatrixMap structures. */
   this = (AstMatrixMap *) this_object;
   that = (AstMatrixMap *) that_object;

/* Check the second object is a MatrixMap. We know the first is a
   MatrixMap since we have arrived at this implementation of the virtual
   function. */
   if( astIsAMatrixMap( that ) ) {

/* Get the number of inputs and outputs and check they are the same for both. */
      nin = astGetNin( this );
      nout = astGetNout( this );
      if( astGetNout( that ) == nout && astGetNin( that ) == nin ) {

/* Assume the MatrixMaps are equivalent. */
         result = 1;

/* Ensure both MatrixMaps are stored in full form. */
         ExpandMatrix( this, status );
         ExpandMatrix( that, status );

/* Get pointers to the arrays holding the elements of the forward matrix
   for both MatrixMaps. */
         if( astGetInvert( this ) ) {
            this_matrix = this->i_matrix;
         } else {
            this_matrix = this->f_matrix;
         }

         if( astGetInvert( that ) ) {
            that_matrix = that->i_matrix;
         } else {
            that_matrix = that->f_matrix;
         }

/* If either of the above arrays is not available, try to get the inverse
   matrix arrays. */
         if( !this_matrix || !that_matrix ) {
            if( astGetInvert( this ) ) {
               this_matrix = this->f_matrix;
            } else {
               this_matrix = this->i_matrix;
            }

            if( astGetInvert( that ) ) {
               that_matrix = that->f_matrix;
            } else {
               that_matrix = that->i_matrix;
            }
         }

/* If both arrays are now available compare their elements. */
         if( this_matrix && that_matrix ) {
            result = 1;
            for( i = 0; i < nin*nout; i++ ) {
               if( !EQUAL( this_matrix[ i ], that_matrix[ i ] ) ){
                  result = 0;
                  break;
               }
            }
         }

/* Ensure the supplied MatrixMaps are stored back in compressed form. */
         CompressMatrix( this, status );
         CompressMatrix( that, status );
      }
   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static void ExpandMatrix( AstMatrixMap *this, int *status ){
/*
*  Name:
*     ExpandMatrix

*  Purpose:
*     Ensure the MatrixMap is stored in full (non-compressed) form.

*  Type:
*     Private function.

*  Synopsis:
*     #include "matrixmap.h"
*     void ExpandMatrix( AstMatrixMap *this, int *status )

*  Class Membership:
*     MatrixMap member function.

*  Description:
*     If the supplid MatrixMap is stored in a compressed form (i.e no
*     element values if it is a unit matrix, diagonal elements only
*     if it is a diagonal matrix), it is expanded into a full MatrixMap
*     in which all elements are stored.

*  Parameters:
*     this
*        A pointer to the MatrixMap to be expanded.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   double *fmat;                  /* Pointer to full forward matrix */
   double *imat;                  /* Pointer to full inverse matrix */
   int i;                         /* Loop count */
   int ncol;                      /* No. of columns in forward matrix */
   int ndiag;                     /* No. of diagonal elements in matrix */
   int nrow;                      /* No. of rows in forward matrix */

/* Check the global error status. Also return if the MatrixMap
   pointer is null. */
   if ( !astOK || !this ) return;

/* Return without action if the MatrixMap is already in full form. */
   if( this->form == FULL ) return;

/* Get the dimensions of the forward matrix. */
   if( astGetInvert( this ) ){
      nrow = astGetNin( this );
      ncol = astGetNout( this );
   } else {
      ncol = astGetNin( this );
      nrow = astGetNout( this );
   }

/* Store the number of diagonal elements. */
   if( nrow > ncol ){
      ndiag = ncol;
   } else {
      ndiag = nrow;
   }

/* Allocate arrays to hold the full forward and inverse matrices. */
   fmat = (double *) astMalloc( sizeof( double )*(size_t)( nrow*ncol ) );
   imat = (double *) astMalloc( sizeof( double )*(size_t)( nrow*ncol ) );
   if( imat && fmat ){

/* Fill them both with zeros. */
      for( i = 0; i < nrow*ncol; i++ ) {
         fmat[ i ] = 0.0;
         imat[ i ] = 0.0;
      }

/* If a unit MatrixMap was supplied, put ones on the diagonals. */
      if( this->form == UNIT ){
         for( i = 0; i < ndiag; i++ ) {
            fmat[ i*( ncol + 1 ) ] = 1.0;
            imat[ i*( nrow + 1 ) ] = 1.0;
         }

/* If a diagonal MatrixMap was supplied, copy the diagonal terms from
   the supplied MatrixMap. */
      } else if( this->form == DIAGONAL ){
         for( i = 0; i < ndiag; i++ ) {
            fmat[ i*( ncol + 1 ) ] = (this->f_matrix)[ i ];
            imat[ i*( nrow + 1 ) ] = (this->i_matrix)[ i ];
         }
      }

/* Free any existing arrays in the MatrixMap and store the new ones. */
      (void) astFree( (void *) this->f_matrix );
      (void) astFree( (void *) this->i_matrix );

      this->f_matrix = fmat;
      this->i_matrix = imat;

/* Update the storage form. */
      this->form = FULL;

/* If either of the new matrices could not be allocated, ensure that
   both have been freed. */
   } else {
      fmat = (double *) astFree( (void *) fmat );
      imat = (double *) astFree( (void *) imat );
   }

   return;

}

static int FindString( int n, const char *list[], const char *test,
                       const char *text, const char *method,
                       const char *class, int *status ){
/*
*  Name:
*     FindString

*  Purpose:
*     Find a given string within an array of character strings.

*  Type:
*     Private function.

*  Synopsis:
*     #include "matrix.h"
*     int FindString( int n, const char *list[], const char *test,
*                     const char *text, const char *method, const char *class, int *status )

*  Class Membership:
*     MatrixMap method.

*  Description:
*     This function identifies a supplied string within a supplied
*     array of valid strings, and returns the index of the string within
*     the array. The test option may not be abbreviated, but case is
*     insignificant.

*  Parameters:
*     n
*        The number of strings in the array pointed to be "list".
*     list
*        A pointer to an array of legal character strings.
*     test
*        A candidate string.
*     text
*        A string giving a description of the object, parameter,
*        attribute, etc, to which the test value refers.
*        This is only for use in constructing error messages. It should
*        start with a lower case letter.
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The index of the identified string within the supplied array, starting
*     at zero.

*  Notes:
*     -  A value of -1 is returned if an error has already occurred, or
*     if this function should fail for any reason (for instance if the
*     supplied option is not specified in the supplied list).

*/

/* Local Variables: */
   int ret;                /* The returned index */

/* Check global status. */
   if( !astOK ) return -1;

/* Compare the test string with each element of the supplied list. Leave
   the loop when a match is found. */
   for( ret = 0; ret < n; ret++ ) {
      if( !Ustrcmp( test, list[ ret ], status ) ) break;
   }

/* Report an error if the supplied test string does not match any element
   in the supplied list. */
   if( ret >= n ) {
      astError( AST__RDERR, "%s(%s): Illegal value '%s' supplied for %s.", status,
                method, class, test, text );
      ret = -1;
   }

/* Return the answer. */
   return ret;
}

static int GetIsLinear( AstMapping *this_mapping, int *status ){
/*
*  Name:
*     GetIsLinear

*  Purpose:
*     Return the value of the IsLinear attribute for a MatrixMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     void GetIsLinear( AstMapping *this, int *status )

*  Class Membership:
*     MatrixMap member function (over-rides the protected astGetIsLinear
*     method inherited from the Mapping class).

*  Description:
*     This function returns the value of the IsLinear attribute for a
*     Frame, which is always one.

*  Parameters:
*     this
*        Pointer to the MatrixMap.
*     status
*        Pointer to the inherited status variable.
*/
   return 1;
}

static int Ustrcmp( const char *a, const char *b, int *status ){
/*
*  Name:
*     Ustrncmp

*  Purpose:
*     A case blind version of strcmp.

*  Type:
*     Private function.

*  Synopsis:
*     #include "matrixmap.h"
*     int Ustrcmp( const char *a, const char *b )

*  Class Membership:
*     MatrixMap member function.

*  Description:
*     Returns 0 if there are no differences between the two strings, and 1
*     otherwise. Comparisons are case blind.

*  Parameters:
*     a
*        Pointer to first string.
*     b
*        Pointer to second string.

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

void astInitMatrixMapVtab_(  AstMatrixMapVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitMatrixMapVtab

*  Purpose:
*     Initialise a virtual function table for a MatrixMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "matrixmap.h"
*     void astInitMatrixMapVtab( AstMatrixMapVtab *vtab, const char *name )

*  Class Membership:
*     MatrixMap vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the MatrixMap class.

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
   AstObjectVtab *object;        /* Pointer to Object component of Vtab */
   AstMappingVtab *mapping;      /* Pointer to Mapping component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitMappingVtab( (AstMappingVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsAMatrixMap) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstMappingVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */
   vtab->MtrRot = MtrRot;
   vtab->MtrMult = MtrMult;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   mapping = (AstMappingVtab *) vtab;

   parent_transform = mapping->Transform;
   mapping->Transform = Transform;

   parent_mapsplit = mapping->MapSplit;
   mapping->MapSplit = MapSplit;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   object->Equal = Equal;
   mapping->GetIsLinear = GetIsLinear;
   mapping->GetTranForward = GetTranForward;
   mapping->GetTranInverse = GetTranInverse;
   mapping->MapMerge = MapMerge;
   mapping->Rate = Rate;

/* Declare the destructor and copy constructor. */
   astSetDelete( (AstObjectVtab *) vtab, Delete );
   astSetCopy( (AstObjectVtab *) vtab, Copy );

/* Declare the class dump function. */
   astSetDump( vtab, Dump, "MatrixMap", "Matrix transformation" );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
}


static double *InvertMatrix( int form, int nrow, int ncol, double *matrix, int *status ){
/*
*  Name:
*     InvertMatrix

*  Purpose:
*     Invert a suplied matrix.

*  Type:
*     Private function.

*  Synopsis:
*     #include "matrixmap.h"
*     double *InvertMatrix( int form, int nrow, int ncol, double *matrix, int *status )

*  Class Membership:
*     MatrixMap member function.

*  Description:
*     This function returns a pointer to a matrix holding the inverse of
*     the supplied matrix, or a NULL pointer if the inverse is not defined.
*     The memory to store the inverse matrix is allocated internally, and
*     should be freed using astFree when no longer required.
*
*     The correspondence between a full matrix and its inverse is only
*     unique if the matrix is square, and so a NULL pointer is returned if
*     the supplied matrix is not square.

*  Parameters:
*     form
*        The form of the MatrixMap; UNIT, DIAGONAL or FULL.
*     nrow
*        Number of rows in the supplied matrix.
*     ncol
*        Number of columns in the supplied matrix.
*     matrix
*        A pointer to the input matrix. Elements should be stored in row
*        order (i.e. (row 1,column 1 ), (row 1,column 2 )... (row 2,column 1),
*        etc).
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the output matrix.

*  Notes:
*     -  A NULL pointer is returned if a unit matrix is supplied.
*     -  A NULL pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*     -  No error is reported if the inverse is not defined.
*/

/* Local Variables: */
   double det;                    /* Determinant of supplied matrix */
   double mval;                   /* Matrix element value */
   double *out;                   /* Pointer to returned inverse matrix */
   double *vector;                /* Pointer to vector used by palDmat */
   int i;                         /* Matrix element number */
   int *iw;                       /* Pointer to workspace used by palDmat */
   int nel;                       /* No. of elements in square matrix */
   int ndiag;                     /* No. of diagonal elements */
   int ok;                        /* Zero if any bad matrix values found */
   int sing;                      /* Zero if matrix is not singular */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Return a NULL pointer if the input matrix is NULL. */
   if( !matrix ) return NULL;

/* If a unit matrix map has been supplied, return NULL. */
   if( form == UNIT ){
      return NULL;

/* If a diagonal matrix has been supplied, allocate an array to hold
   the diagonal terms of the inverse matrix. Store the reciprocal
   of the input matrix diagonal terms in it. If any of the input diagonal
   terms are zero or BAD, set the associated elements of the inverse matrix
   BAD. */
   } else if( form == DIAGONAL ){
      if( nrow > ncol ) {
         ndiag = ncol;
      } else {
         ndiag = nrow;
      }

      out = (double *) astMalloc( sizeof( double )*(size_t)ndiag );

      if( out ) {
         for( i = 0; i < ndiag; i++ ) {
            mval = matrix[ i ];
            if( mval != 0.0 && mval != AST__BAD ){
               out[ i ] = 1.0/mval;
            } else {
               out[ i ] = AST__BAD;
            }
         }
      }

/* If a full matrix has been supplied, initialise the returned pointer. */
   } else {
      out = NULL;

/* Check that the matrix is square. */
      if( nrow == ncol ){

/* Find the number of elements in the matrix. */
         nel = nrow*ncol;

/* See if there are any bad values in the matrix. */
         ok = 1;
         for ( i=0; i<nel; i++ ) {
            if ( matrix[i] == AST__BAD ) {
               ok = 0;
               break;
            }
         }

/* Only continue if there are no bad matrix values. */
         if( ok ) {

/* Take a copy of the supplied matrix */
            out = (double *) astStore( NULL, (void *) matrix,
                                       astSizeOf( (void *) matrix ) );

/* The SLALIB function which inverts the matrix also applies the inverse
   matrix to a vector. We are not interested in the vector in this
   instance, but we still have to provide one for SLALIB to use. Allocate
   memory for the vector. */
            vector = (double *) astMalloc( sizeof(double)*(size_t) nrow );

/* If it was allocated succesfully, fill it with zeros. */
            if( astOK ){
               for ( i=0; i<nrow; i++ ) vector[i] = 0.0;

/* Obtain work space and attempt to invert the matrix using SLALIB, then
   free the work space. */
               iw = (int *) astMalloc( sizeof(int)*(size_t) nrow );
               if( astOK ) palDmat( nrow, out, vector, &det, &sing, iw );
               iw = (int *) astFree( (void *) iw );

            }

/* If the matrix could not be inverted, free the memory used to hold the
   square matrix, and return the NULL pointer. */
            if ( !astOK || sing != 0 ){
               out = (double *) astFree( (void *) out );
            }

/*  Free the memory used to hold the vector. */
            vector = (double *) astFree( (void *) vector );
         }
      }
   }

/* Return the pointer. */

   return out;

}

static int MapMerge( AstMapping *this, int where, int series, int *nmap,
                     AstMapping ***map_list, int **invert_list, int *status ) {
/*
*  Name:
*     MapMerge

*  Purpose:
*     Simplify a sequence of Mappings containing a MatrixMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     int MapMerge( AstMapping *this, int where, int series, int *nmap,
*                   AstMapping ***map_list, int **invert_list, int *status )

*  Class Membership:
*     MatrixMap method (over-rides the protected astMapMerge method
*     inherited from the Mapping class).

*  Description:
*     This function attempts to simplify a sequence of Mappings by
*     merging a nominated MatrixMap in the sequence with its neighbours,
*     so as to shorten the sequence if possible.
*
*     In many cases, simplification will not be possible and the
*     function will return -1 to indicate this, without further
*     action.
*
*     In most cases of interest, however, this function will either
*     attempt to replace the nominated MatrixMap with a Mapping which it
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
*        Pointer to the nominated MatrixMap which is to be merged with
*        its neighbours. This should be a cloned copy of the MatrixMap
*        pointer contained in the array element "(*map_list)[where]"
*        (see below). This pointer will not be annulled, and the
*        MatrixMap it identifies will not be modified by this function.
*     where
*        Index in the "*map_list" array (below) at which the pointer
*        to the nominated MatrixMap resides.
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
   AstMapping **maplt;   /* New mappings list pointer */
   AstMapping *map2;     /* Pointer to replacement Mapping */
   AstMapping *mc[2];    /* Copies of supplied Mappings to swap */
   AstMapping *smc0;     /* Simplied Mapping */
   AstMapping *smc1;     /* Simplied Mapping */
   AstMatrixMap *mm;     /* Pointer to supplied MatrixMap */
   AstMatrixMap *newmm;  /* Pointer to replacement MatrixMap */
   const char *class1;   /* Pointer to first Mapping class string */
   const char *class2;   /* Pointer to second Mapping class string */
   const char *nclass;   /* Pointer to neighbouring Mapping class */
   double *b;            /* Pointer to scale terms */
   int *invlt;           /* New invert flags list pointer */
   int do1;              /* Would a backward swap make a simplification? */
   int do2;              /* Would a forward swap make a simplification? */
   int i1;               /* Index of first MatrixMap to merge */
   int i2;               /* Index of last MatrixMap to merge */
   int i;                /* Loop counter */
   int ic[2];            /* Copies of supplied invert flags to swap */
   int invert;           /* Should the inverted Mapping be used? */
   int neighbour;        /* Index of Mapping with which to swap */
   int nin;              /* Number of input coordinates for MatrixMap */
   int nmapt;            /* No. of Mappings in list */
   int nout;             /* Number of output coordinates for MatrixMap */
   int nstep1;           /* No. of Mappings backwards to next mergable Mapping */
   int nstep2;           /* No. of Mappings forward to next mergable Mapping */
   int result;           /* Result value to return */
   int swaphi;           /* Can MatrixMap be swapped with higher neighbour? */
   int swaplo;           /* Can MatrixMap be swapped with lower neighbour? */
   int zoom;             /* Can MatrixMap be replaced by a ZoomMap? */

/* Initialise. */
   result = -1;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Initialise variables to avoid "used of uninitialised variable"
   messages from dumb compilers. */
   i1 = 0;
   i2 = 0;
   neighbour = 0;

/* Get the Invert attribute for the specified mapping. */
   invert = astGetInvert( ( *map_list )[ where ] );

/* Get the number of input and output axes for the MatrixMap. Swap these
   if the supplied invert flag is not the same as the Invert attribute of
   the Mapping. */
   if( ( invert && !( *invert_list )[ where ] ) ||
       ( !invert && ( *invert_list )[ where ] ) ) {
      nout = astGetNin( ( *map_list )[ where ] );
      nin = astGetNout( ( *map_list )[ where ] );

   } else {
      nin = astGetNin( ( *map_list )[ where ] );
      nout = astGetNout( ( *map_list )[ where ] );
   }

/* First of all, see if the MatrixMap can be replaced by a simpler Mapping,
   without reference to the neighbouring Mappings in the list.           */
/* ======================================================================*/
   map2 = NULL;
   mm = (AstMatrixMap *) ( *map_list )[ where ];

/* If the MatrixMap is a square unit matrix, it can be replaced by a
   UnitMap. */
   if( mm->form == UNIT && nin == nout ){
      map2 = (AstMapping *) astUnitMap( nin, "", status );

/* If the MatrixMap is a square diagonal matrix with equal diagonal
   terms, then it can be replaced by a ZoomMap. */
   } else if( mm->form == DIAGONAL && nin == nout &&
              mm->f_matrix && mm->i_matrix &&
             (mm->f_matrix)[ 0 ] != AST__BAD ){
      zoom = 1;
      b = mm->f_matrix + 1;
      for( i = 1; i < nin; i++ ){
         if( !EQUAL( *b, *( b - 1 ) ) ){
            zoom = 0;
            break;
         }
         b++;
      }

      if( zoom ){
         if( ( *invert_list )[ where ] ){
            map2 = (AstMapping *) astZoomMap( nin, (mm->i_matrix)[ 0 ], "", status );
         } else {
            map2 = (AstMapping *) astZoomMap( nin, (mm->f_matrix)[ 0 ], "", status );
         }
      }
   }

/* If the MatrixMap can be replaced, annul the MatrixMap pointer in the
   list and replace it with the new Mapping pointer, and indicate that the
   forward transformation of the returned Mapping should be used. */
   if( map2 ){
      (void) astAnnul( ( *map_list )[ where ] );
      ( *map_list )[ where ] = map2;
      ( *invert_list )[ where ] = 0;

/* Return the index of the first modified element. */
      result = where;

/* If the MatrixMap itself could not be simplified, see if it can be merged
   with the Mappings on either side of it in the list. */
/*==========================================================================*/
   } else {

/* Store the classes of the neighbouring Mappings in the list. */
       class1 = ( where > 0 ) ? astGetClass( ( *map_list )[ where - 1 ] ) : NULL;
       class2 = ( where < *nmap - 1 ) ? astGetClass( ( *map_list )[ where + 1 ] ) : NULL;

/* In series. */
/* ========== */
      if ( series ) {

/* We first look to see if the MatrixMap can be merged with one of its
   neighbours, resulting in a reduction of one in the number of Mappings
   in the list. MatrixMaps can merge directly with another MatrixMap, a
   ZoomMap, an invertable PermMap, or a UnitMap. */
         if( class1 && ( !strcmp( class1, "MatrixMap" ) ||
                         !strcmp( class1, "ZoomMap" ) ||
                         !strcmp( class1, "PermMap" ) ||
                         !strcmp( class1, "UnitMap" ) ) ){
            nclass = class1;
            i1 = where - 1;
            i2 = where;

         } else if( class2 && ( !strcmp( class2, "MatrixMap" ) ||
                                !strcmp( class2, "ZoomMap" ) ||
                                !strcmp( class2, "PermMap" ) ||
                                !strcmp( class2, "UnitMap" ) ) ){
            nclass = class2;
            i1 = where;
            i2 = where + 1;

         } else {
            nclass = NULL;
         }

/* Only some PermMaps can be merged with (those which have consistent
   forward and inverse mappings). If this is not one of them, set nclass
   NULL to indicate this. */
         if( nclass && !strcmp( nclass, "PermMap" ) &&
             !PermOK( ( *map_list )[ (i1==where)?i2:i1 ], status ) ) nclass = NULL;

/* If the MatrixMap can merge with one of its neighbours, create the merged
   Mapping. */
         if( nclass ){

            if( !strcmp( nclass, "MatrixMap" ) ){
               newmm = MatMat( ( *map_list )[ i1 ], ( *map_list )[ i2 ],
                               ( *invert_list )[ i1 ], ( *invert_list )[ i2 ], status );
               invert = 0;

            } else if( !strcmp( nclass, "ZoomMap" ) ){
               if( i1 == where ){
                  newmm = MatZoom( (AstMatrixMap *)( *map_list )[ i1 ],
                                   (AstZoomMap *)( *map_list )[ i2 ],
                              ( *invert_list )[ i1 ], ( *invert_list )[ i2 ], status );
               } else {
                  newmm = MatZoom( (AstMatrixMap *)( *map_list )[ i2 ],
                                   (AstZoomMap *)( *map_list )[ i1 ],
                           ( *invert_list )[ i2 ], ( *invert_list )[ i1 ], status );
               }
               invert = 0;

            } else if( !strcmp( nclass, "PermMap" ) ){
               if( i1 == where ){
                  newmm = MatPerm( (AstMatrixMap *)( *map_list )[ i1 ],
                                   (AstPermMap *)( *map_list )[ i2 ],
                           ( *invert_list )[ i1 ], ( *invert_list )[ i2 ], 1, status );
               } else {
                  newmm = MatPerm( (AstMatrixMap *)( *map_list )[ i2 ],
                                   (AstPermMap *)( *map_list )[ i1 ],
                           ( *invert_list )[ i2 ], ( *invert_list )[ i1 ], 0, status );
               }
               invert = 0;

            } else {
               newmm = astClone( ( *map_list )[ where ] );
               invert = ( *invert_list )[ where ];
            }

/* If succesfull... */
            if( astOK ){

/* Annul the first of the two Mappings, and replace it with the merged
   MatrixMap. Also set the invert flag. */
               (void) astAnnul( ( *map_list )[ i1 ] );
               ( *map_list )[ i1 ] = (AstMapping *) newmm;
               ( *invert_list )[ i1 ] = invert;

/* Annul the second of the two Mappings, and shuffle down the rest of the
   list to fill the gap. */
               (void) astAnnul( ( *map_list )[ i2 ] );
               for ( i = i2 + 1; i < *nmap; i++ ) {
                  ( *map_list )[ i - 1 ] = ( *map_list )[ i ];
                  ( *invert_list )[ i - 1 ] = ( *invert_list )[ i ];
               }

/* Clear the vacated element at the end. */
               ( *map_list )[ *nmap - 1 ] = NULL;
               ( *invert_list )[ *nmap - 1 ] = 0;

/* Decrement the Mapping count and return the index of the first
   modified element. */
               ( *nmap )--;
               result = i1;

            }

/* If the MatrixMap could not merge directly with either of its neighbours,
   we consider whether it would be worthwhile to swap the MatrixMap with
   either of its neighbours. This can only be done for certain classes
   of Mapping (WinMaps and some PermMaps), and will usually require both
   Mappings to be modified (unless they are commutative). The advantage of
   swapping the order of the Mappings is that it may result in the MatrixMap
   being adjacent to a Mapping with which it can merge directly on the next
   invocation of this function, thus reducing the number of Mappings
   in the list. */
         } else {

/* Set a flag if we could swap the MatrixMap with its higher neighbour. "do2"
   is returned if swapping the Mappings would simplify either of the Mappings. */
            if( where + 1 < *nmap ){
               swaphi = CanSwap(  ( *map_list )[ where ],
                                  ( *map_list )[ where + 1 ],
                                  ( *invert_list )[ where ],
                                  ( *invert_list )[ where + 1 ], &do2, status );
            } else {
               swaphi = 0;
               do2 = 0;
            }

/* If so, step through each of the Mappings which follow the MatrixMap,
   looking for a Mapping with which the MatrixMap could merge directly. Stop
   when such a Mapping is found, or if a Mapping is found with which the
   MatrixMap could definitely not swap. Note the number of Mappings which
   separate the MatrixMap from the Mapping with which it could merge (if
   any). */
            nstep2 = -1;
            if( swaphi ){
               for( i2 = where + 1; i2 < *nmap; i2++ ){

/* See if we can merge with this Mapping. If so, note the number of steps
   between the two Mappings and leave the loop. */
                  nclass = astGetClass( ( *map_list )[ i2 ] );
                  if( !strcmp( nclass, "MatrixMap" ) ||
                      !strcmp( nclass, "ZoomMap" ) ||
                      ( !strcmp( nclass, "PermMap" ) && PermOK( ( *map_list )[ i2 ], status ) ) ||
                      !strcmp( nclass, "UnitMap" ) ) {
                     nstep2 = i2 - where - 1;
                     break;
                  }

/* If there is no chance that we can swap with this Mapping, leave the loop
   with -1 for the number of steps to indicate that no merging is possible.
   MatrixMaps can swap with WinMaps and some permmaps. */
                  if( strcmp( nclass, "WinMap" ) &&
                      strcmp( nclass, "PermMap" ) ) {
                     break;
                  }

               }

            }

/* Do the same working forward from the MatrixMap towards the start of the map
   list. */
            if( where > 0 ){
               swaplo = CanSwap(  ( *map_list )[ where - 1 ],
                                  ( *map_list )[ where ],
                                  ( *invert_list )[ where - 1 ],
                                  ( *invert_list )[ where ], &do1, status );
            } else {
               swaplo = 0;
               do1 = 0;
            }

            nstep1 = -1;
            if( swaplo ){
               for( i1 = where - 1; i1 >= 0; i1-- ){

                  nclass = astGetClass( ( *map_list )[ i1 ] );
                  if( !strcmp( nclass, "MatrixMap" ) ||
                      ( !strcmp( nclass, "PermMap" ) && PermOK( ( *map_list )[ i1 ], status ) ) ||
                      !strcmp( nclass, "ZoomMap" ) ||
                      !strcmp( nclass, "UnitMap" ) ) {
                     nstep1 = where - 1 - i1;
                     break;
                  }

                  if( strcmp( nclass, "WinMap" ) &&
                      strcmp( nclass, "PermMap" ) ) {
                     break;
                  }

               }

            }

/* Choose which neighbour to swap with so that the MatrixMap moves towards the
   nearest Mapping with which it can merge. */
            if( do1 || (
                nstep1 != -1 && ( nstep2 == -1 || nstep2 > nstep1 ) ) ){
               nclass = class1;
               i1 = where - 1;
               i2 = where;
               neighbour = i1;
            } else if( do2 || nstep2 != -1 ){
               nclass = class2;
               i1 = where;
               i2 = where + 1;
               neighbour = i2;
            } else {
               nclass = NULL;
            }

/* If there is a target Mapping in the list with which the MatrixMap could
   merge, consider replacing the supplied Mappings with swapped Mappings to
   bring the MatrixMap closer to the target Mapping. */
            if( nclass ){

/* It is possible that the neighbouring Mapping with which we are about to
   swap could also merge with the target Mapping. When the neighbouring
   Mapping is reconsidered it may well swap the pair back to put itself nearer
   the target Mapping. We need to be careful not to end up in an infinite loop
   in which the pair of neighbouring Mappings are constantly swapped backwards
   and forwards as each attempts to put itself closer to the target Mapping.
   To prevent this, we only swap the pair of Mappings if the neighbouring
   Mapping could not itself merge with the target Mapping. Check to see
   if this is the case by attempting to merge the neighbouring Mapping with
   the target Mapping. */
               map2 = astClone( (*map_list)[ neighbour ] );
               nmapt = *nmap - neighbour;
               maplt = *map_list + neighbour;
               invlt = *invert_list + neighbour;
               result = astMapMerge( map2, 0, series, &nmapt, &maplt, &invlt );
               map2 = astAnnul( map2 );

/* If the above call produced a change in the  Mapping list, return the
   remaining number of mappings.. */
               if( result != -1 ){
                  *nmap = nmapt + neighbour;

/* Otherwise, if there was no change in the mapping list... */
               } else {
                  if (!strcmp( nclass, "WinMap" ) ){
                     MatWin( (*map_list) + i1, (*invert_list) + i1, where - i1, status );

                  } else if( !strcmp( nclass, "PermMap" ) ){
                     MatPermSwap( (*map_list) + i1, (*invert_list) + i1, where - i1, status );
                  }

/* Store the index of the first modified Mapping. */
                  result = i1;
               }

/* If there is no Mapping available for merging, it may still be
   advantageous to swap with a neighbour because the swapped Mapping may
   be simpler than the original Mappings. For instance, a PermMap may
   strip rows of the MatrixMap leaving only a UnitMap. */
            } else if( swaphi || swaplo ) {

/* Try swapping with each possible neighbour in turn. */
               for( i = 0; i < 2; i++ ) {

/*  Set up the class and pointers for the mappings to be swapped, first
    the lower neighbour, then the upper neighbour. */
                  if( i == 0 && swaplo ){
                     nclass = class1;
                     i1 = where - 1;
                     i2 = where;

                  } else if( i == 1 && swaphi ){
                     nclass = class2;
                     i1 = where;
                     i2 = where + 1;

                  } else {
                     nclass = NULL;
                  }

/* If we have a Mapping to swap with... */
                  if( nclass ) {

/* Take copies of the Mapping and Invert flag arrays so we do not change
   the supplied values. */
                     mc[ 0 ] = (AstMapping *) astCopy( ( (*map_list) + i1 )[0] );
                     mc[ 1 ] = (AstMapping *) astCopy( ( (*map_list) + i1 )[1] );
                     ic[ 0 ] = ( (*invert_list) + i1 )[0];
                     ic[ 1 ] = ( (*invert_list) + i1 )[1];

/* Swap these Mappings. */
                     if( !strcmp( nclass, "WinMap" ) ){
                        MatWin( mc, ic, where - i1, status );
                     } else if( !strcmp( nclass, "PermMap" ) ){
                        MatPermSwap( mc, ic, where - i1, status );
                     }

/* If neither of the swapped Mappings can be simplified further, then there
   is no point in swapping the Mappings, so just annul the map copies. */
                     smc0 = astSimplify( mc[0] );
                     smc1 = astSimplify( mc[1] );

                     if( astGetClass( smc0 ) == astGetClass( mc[0] ) &&
                         astGetClass( smc1 ) == astGetClass( mc[1] ) ) {

                        mc[ 0 ] = (AstMapping *) astAnnul( mc[ 0 ] );
                        mc[ 1 ] = (AstMapping *) astAnnul( mc[ 1 ] );

/* If one or both of the swapped Mappings could be simplified, then annul
   the supplied Mappings and return the swapped mappings, storing the index
   of the first modified Mapping. */
                     } else {
                        (void ) astAnnul( ( (*map_list) + i1 )[0] );
                        (void ) astAnnul( ( (*map_list) + i1 )[1] );

                        ( (*map_list) + i1 )[0] = mc[ 0 ];
                        ( (*map_list) + i1 )[1] = mc[ 1 ];

                        ( (*invert_list) + i1 )[0] = ic[ 0 ];
                        ( (*invert_list) + i1 )[1] = ic[ 1 ];

                        result = i1;
                        break;
                     }

/* Annul the simplied Mappings */
                     smc0 = astAnnul( smc0 );
                     smc1 = astAnnul( smc1 );

                  }
               }
            }
         }
      }
   }

/* Return the result. */
   return result;
}

static int *MapSplit( AstMapping *this_map, int nin, const int *in, AstMapping **map, int *status ){
/*
*  Name:
*     MapSplit

*  Purpose:
*     Create a Mapping representing a subset of the inputs of an existing
*     MatrixMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "matrixmap.h"
*     int *MapSplit( AstMapping *this, int nin, const int *in, AstMapping **map, int *status )

*  Class Membership:
*     MatrixMap method (over-rides the protected astMapSplit method
*     inherited from the Mapping class).

*  Description:
*     This function creates a new Mapping by picking specified inputs from
*     an existing MatrixMap. This is only possible if the specified inputs
*     correspond to some subset of the MatrixMap outputs. That is, there
*     must exist a subset of the MatrixMap outputs for which each output
*     depends only on the selected MatrixMap inputs, and not on any of the
*     inputs which have not been selected. In addition, outputs that are
*     not in this subset must not depend on any selected inputs. If these
*     conditions are not met by the supplied MatrixMap, then a NULL Mapping
*     is returned.

*  Parameters:
*     this
*        Pointer to the MatrixMap to be split (the MatrixMap is not actually
*        modified by this function).
*     nin
*        The number of inputs to pick from "this".
*     in
*        Pointer to an array of indices (zero based) for the inputs which
*        are to be picked. This array should have "nin" elements. If "Nin"
*        is the number of inputs of the supplied MatrixMap, then each element
*        should have a value in the range zero to Nin-1.
*     map
*        Address of a location at which to return a pointer to the new
*        Mapping. This Mapping will have "nin" inputs (the number of
*        outputs may be different to "nin"). A NULL pointer will be
*        returned if the supplied MatrixMap has no subset of outputs which
*        depend only on the selected inputs.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a dynamically allocated array of ints. The number of
*     elements in this array will equal the number of outputs for the
*     returned Mapping. Each element will hold the index of the
*     corresponding output in the supplied MatrixMap. The array should be
*     freed using astFree when no longer needed. A NULL pointer will
*     be returned if no output Mapping can be created.

*  Notes:
*     - If this function is invoked with the global error status set,
*     or if it should fail for any reason, then NULL values will be
*     returned as the function value and for the "map" pointer.
*/

/* Local Variables: */
   AstMatrixMap *this;        /* Pointer to MatrixMap structure */
   double *mat;               /* Pointer to matrix for supplied MatrixMap */
   double *pmat;              /* Pointer to row start in returned matrix */
   double *prow;              /* Pointer to row start in supplied matrix */
   double *rmat;              /* Pointer to matrix for returned MatrixMap */
   double el;                 /* Next element value in supplied matrix */
   int *result;               /* Pointer to returned array */
   int good;                  /* Would new matrix contain any good values/ */
   int i;                     /* Loop count */
   int icol;                  /* Column index within supplied MatrixMap */
   int iel;                   /* Index of next element from the input matrix */
   int irow;                  /* Row index within supplied MatrixMap */
   int isel;                  /* Does output depend on any selected inputs? */
   int ncol;                  /* Number of columns (inputs) in supplied MatrixMap */
   int nout;                  /* Number of outputs in returned MatrixMap */
   int nrow;                  /* Number of rows (outputs) in supplied MatrixMap */
   int ok;                    /* Are input indices OK? */
   int sel;                   /* Does any output depend on selected inputs? */
   int unsel;                 /* Does any output depend on unselected inputs? */

/* Initialise */
   result = NULL;
   *map = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Invoke the parent astMapSplit method to see if it can do the job. */
   result = (*parent_mapsplit)( this_map, nin, in, map, status );

/* If not, we provide a special implementation here. */
   if( !result ) {

/* Get a pointer to the MatrixMap structure. */
      this = (AstMatrixMap *) this_map;

/* Get the number of inputs and outputs. */
      ncol = astGetNin( this );
      nrow = astGetNout( this );

/* Check the supplied input indices are usable. */
      ok = 1;
      for( i = 0; i < nin; i++ ) {
         if( in[ i ] < 0 || in[ i ] >= ncol ) {
            ok = 0;
            break;
         }
      }

      if( ok ) {

/* Ensure the MatrixMap is stored in full form. */
         ExpandMatrix( this, status );

/* Allocate the largest array that could be necessary to hold the
   returned array of Mapping outputs. */
         result = astMalloc( sizeof(int)*(size_t) nrow );

/* Allocate the largest array that could be necessary to hold the
   matrix representing the returned MatrixMap. */
         rmat = astMalloc( sizeof(double)*(size_t) (nrow*ncol) );

/* Get the matrix which defines the current forward transformation. This
   takes into account whether the MatrixMap has been inverted or not. */
         if( astGetInvert( this ) ) {
            mat = this->i_matrix;
         } else {
            mat = this->f_matrix;
         }

/* We cannot create the require Mapping if the matrix is undefined. */
         if( !mat || !astOK ) {
            ok = 0;
            nout = 0;
            good = 0;

/* Otherwise, loop round all the rows in the matrix. */
         } else {
            nout = 0;
            good = 0;
            pmat = rmat;
            iel = 0;
            for( irow = 0; irow < nrow; irow++ ) {

/* Indicate that this output (i.e. row of the matrix) depends on neither
   selected nor unselected inputs as yet. */
               sel = 0;
               unsel = 0;

/* Save a pointer to the first element of this row in the MatrixMap
   matrix. */
               prow = mat + iel;

/* Loop round all the elements in the current row of the matrix. */
               for( icol = 0; icol < ncol; icol++ ) {

/* If this element is non-zero and non-bad, then output "irow" depends on
   input "icol". */
                  el = mat[ iel++ ];
                  if( el != 0.0 && el != AST__BAD ) {

/* Is input "icol" one of the selected inputs? */
                     isel = 0;
                     for( i = 0; i < nin; i++ ) {
                        if( in[ i ] == icol ) {
                           isel = 1;
                           break;
                        }
                     }

/* If so, note that this output depends on selected inputs. Otherwise note
   it depends on unselected inputs. */
                     if( isel ) {
                        sel = 1;
                     } else  {
                        unsel = 1;
                     }
                  }
               }

/* If this output depends only on selected inputs, we can include it in
   the returned Mapping.*/
               if( sel && !unsel ) {

/* Store the index of the output within the original MatrixMap. */
                  result[ nout ] = irow;

/* Increment the number of outputs in the returned Mapping. */
                  nout++;

/* Copy the elements of the current matrix row which correspond to the
   selected inputs into the new matrix. */
                  for( i = 0; i < nin; i++ ) {
                    if( astISGOOD( prow[ in[ i ] ] ) ) {
                        *(pmat++) = prow[ in[ i ] ];
                        good = 1;
                     }
                  }
               }

/* If this output depends on a selected input, but also depends on an
   unselected input, we cannot split the MatrixMap. */
               if( sel && unsel ) {
                  ok = 0;
                  break;
               }
            }
         }


/* If the returned Mapping can be created, create it. */
         if( ok && nout > 0 && good ) {
            *map = (AstMapping *) astMatrixMap( nin, nout, 0, rmat, "", status );

/* Otherwise, free the returned array. */
         } else {
            result = astFree( result );
         }

/* Free resources. */
         rmat = astFree( rmat );

/* Re-compress the supplied MatrixMap. */
         CompressMatrix( this, status );
      }
   }

/* Free returned resources if an error has occurred. */
   if( !astOK ) {
      result = astFree( result );
      *map = astAnnul( *map );
   }

/* Return the list of output indices. */
   return result;
}

static AstMatrixMap *MatMat( AstMapping *map1, AstMapping *map2, int inv1,
                             int inv2, int *status ){
/*
*  Name:
*     MatMat

*  Purpose:
*     Create a merged MatrixMap from two supplied MatrixMaps.

*  Type:
*     Private function.

*  Synopsis:
*     #include "matrixmap.h"
*     AstMatrixMap *MatMat( AstMapping *map1, AstMapping *map2, int inv1,
*                           int inv2, int *status )

*  Class Membership:
*     MatrixMap member function

*  Description:
*     This function creates a new MatrixMap which performs a mapping
*     equivalent to applying the two supplied MatrixMaps in series, in the
*     directions specified by the "invert" flags (the Invert attributes of
*     the supplied MatrixMaps are ignored).

*  Parameters:
*     map1
*        A pointer to the MatrixMap to apply first.
*     map2
*        A pointer to the MatrixMap to apply second.
*     inv1
*        The invert flag to use with map1. A value of zero causes the forward
*        mapping to be used, and a non-zero value causes the inverse
*        mapping to be used.
*     inv2
*        The invert flag to use with map2.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the new MatrixMap.

*  Notes:
*     -  The forward direction of the returned MatrixMap is equivalent to the
*     combined effect of the two supplied MatrixMap, operating in the
*     directions specified by "inv1" and "inv2".
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstMatrixMap *result;            /* Pointer to output MatrixMap */
   int invert[ 2 ];                 /* Original invert flags */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise the returned pointer. */
   result = NULL;

/* Temporarily set their Invert attributes to the supplied values. */
   invert[ 0 ] = astGetInvert( map1 );
   astSetInvert( map1, inv1 );

   invert[ 1 ] = astGetInvert( map2 );
   astSetInvert( map2, inv2 );

/* Create a new MatrixMap by multiplying them together. */
   result = astMtrMult( (AstMatrixMap *) map1, (AstMatrixMap *) map2 );

/* Re-instate the original settings of the Invert attributes for the
   supplied MatrixMaps. */
   astSetInvert( map1, invert[ 0 ] );
   astSetInvert( map2, invert[ 1 ] );

/* If an error has occurred, annull the returned MatrixMap. */
   if( !astOK ) result = astAnnul( result );

/* Return a pointer to the output MatrixMap. */
   return result;
}

static AstMatrixMap *MatPerm( AstMatrixMap *mm, AstPermMap *pm, int minv,
                              int pinv, int mat1, int *status ){
/*
*  Name:
*     MatPerm

*  Purpose:
*     Create a MatrixMap by merging a MatrixMap and a PermMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "matrixmap.h"
*     AstMatrixMap *MatPerm( AstMatrixMap *mm, AstPermMap *pm, int minv,
*                            int pinv, int mat1, int *status )

*  Class Membership:
*     MatrixMap member function

*  Description:
*     This function creates a new MatrixMap which performs a mapping
*     equivalent to applying the two supplied Mappings in series in the
*     directions specified by the "invert" flags (the Invert attributes of
*     the supplied MatrixMaps are ignored).

*  Parameters:
*     mm
*        A pointer to the MatrixMap.
*     pm
*        A pointer to the PermMap.
*     minv
*        The invert flag to use with mm. A value of zero causes the forward
*        mapping to be used, and a non-zero value causes the inverse
*        mapping to be used.
*     pinv
*        The invert flag to use with pm.
*     mat1
*        If non-zero, then "mm" is applied first followed by "pm". Otherwise,
*        "pm" is applied first followed by "mm".
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the new MatrixMap.

*  Notes:
*     -  The forward direction of the returned MatrixMap is equivalent to the
*     combined effect of the two supplied Mappings, operating in the
*     directions specified by "pinv" and "minv".
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstMatrixMap *mm2;            /* Pointer to an intermediate MatrixMap */
   AstMatrixMap *result;         /* Pointer to output MatrixMap */
   AstPointSet *pset1;           /* Pointer to a PointSet holding unpermuted unit vectors */
   AstPointSet *pset2;           /* Pointer to a PointSet holding permuted unit vectors */
   double *matrix;               /* Pointer to a matrix representing the PermMap */
   double *p;                    /* Pointer to next matrix element */
   double **ptr1;                /* Pointer to the data in pset1 */
   double **ptr2;                /* Pointer to the data in pset2 */
   int i;                        /* Axis index */
   int j;                        /* Point index */
   int nax;                      /* No. of axes in the PermMap */
   int old_minv;                 /* Original setting of MatrixMap Invert attribute */
   int old_pinv;                 /* Original setting of PermMap Invert attribute */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise the returned pointer. */
   result = NULL;

/* Temporarily set the Invert attributes of both Mappings to the supplied
   values. */
   old_minv = astGetInvert( mm );
   astSetInvert( mm, minv );

   old_pinv = astGetInvert( pm );
   astSetInvert( pm, pinv );

/* Get the number of axes in the PermMap. The PermMap will have the same
   number of input and output axes because a check has already been made on
   it to ensure that this is so (in function PermOK). */
   nax = astGetNin( pm );

/* We first represent the PermMap as a MatrixMap containing elements with
   values zero or one. Each row of this matrix is obtained by transforming a
   unit vector along each axis using the inverse PermMap. Allocate memory
   to hold the matrix array, and create a PointSet holding the unit
   vectors. */
   matrix = (double *) astMalloc( sizeof( double )*(size_t)( nax*nax ) );

   pset1 = astPointSet( nax, nax, "", status );
   ptr1 = astGetPoints( pset1 );

   pset2 = astPointSet( nax, nax, "", status );
   ptr2 = astGetPoints( pset2 );

   if( astOK ){
      for( i = 0; i < nax; i++ ){
         for( j = 0; j < nax; j++ ) ptr1[ i ][ j ] = 0.0;
         ptr1[ i ][ i ] = 1.0;
      }

/* Transform these unit vectors using the inverse PermMap. */
      (void) astTransform( pm, pset1, 0, pset2 );

/* Copy the transformed vectors into the matrix array. */
      p = matrix;
      for( j = 0; j < nax; j++ ){
         for( i = 0; i < nax; i++ ) *(p++) = ptr2[ i ][ j ];
      }

/* Create a MatrixMap holding this array. */
      mm2 = astMatrixMap( nax, nax, 0, matrix, "", status );

/* Create a new MatrixMap equal to the product of the supplied MatrixMap
   and the MatrixMap just created from the PermMap. */
      if( mat1 ){
         result = astMtrMult( mm, mm2 );
      } else {
         result = astMtrMult( mm2, mm );
      }

/* Free everything. */
      mm2 = astAnnul( mm2 ) ;
   }

   pset2 = astAnnul( pset2 );
   pset1 = astAnnul( pset1 );
   matrix = (double *) astFree( (void *) matrix );

/* Re-instate the original settings of the Invert attribute for the
   supplied Mappings. */
   astSetInvert( mm, old_minv );
   astSetInvert( pm, old_pinv );

/* If an error has occurred, annull the returned MatrixMap. */
   if( !astOK ) result = astAnnul( result );

/* Return a pointer to the output MatrixMap. */
   return result;
}

static void MatPermSwap( AstMapping **maps, int *inverts, int imm, int *status ){
/*
*  Name:
*     MatPermSwap

*  Purpose:
*     Swap a PermMap and a MatrixMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "matrixmap.h"
*     void MatPermSwap( AstMapping **maps, int *inverts, int imm )

*  Class Membership:
*     WinMap member function

*  Description:
*     A list of two Mappings is supplied containing a PermMap and a
*     MatrixMap. These Mappings are annulled, and replaced with
*     another pair of Mappings consisting of a PermMap and a MatrixMap
*     in the opposite order. These Mappings are chosen so that their
*     combined effect is the same as the original pair of Mappings.

*  Parameters:
*     maps
*        A pointer to an array of two Mapping pointers.
*     inverts
*        A pointer to an array of two invert flags.
*     imm
*        The index within "maps" of the MatrixMap.

*  Notes:
*     -  There are restictions on the sorts of PermMaps which can be
*     swapped with a MatrixMap -- see function CanSwap. It is assumed
*     that the supplied MatrixMap and PermMap satisfy these requirements.

*/

/* Local Variables: */
   AstMatrixMap *mm;         /* Pointer to the supplied MatrixMap */
   AstMatrixMap *mmnew;      /* Pointer to new MatrixMap */
   AstMatrixMap *smmnew;     /* Pointer to new simplified MatrixMap */
   AstPermMap *pm;           /* Pointer to the supplied PermMap */
   AstPermMap *pmnew;        /* Pointer to new PermMap */
   AstPermMap *spmnew;       /* Pointer to new simplified PermMap */
   double *consts;           /* Pointer to constants array */
   double *matrix;           /* Supplied array of matrix elements */
   double *out_el;           /* Pointer to next element of new MatrixMap */
   double *out_mat;          /* Matrix elements for new MatrixMap */
   double c;                 /* Constant */
   double matel;             /* Matrix element */
   int *inperm;              /* Pointer to input axis permutation array */
   int *outperm;             /* Pointer to output axis permutation array */
   int col;                  /* Index of matrix column */
   int i;                    /* Axis count */
   int k;                    /* Axis count */
   int nin;                  /* No. of axes in supplied PermMap */
   int nout;                 /* No. of axes in returned PermMap */
   int old_pinv;             /* Invert value for the supplied PermMap */
   int row;                  /* Index of matrix row */

/* Check the global error status. */
   if ( !astOK ) return;

/* Initialise variables to avoid "used of uninitialised variable"
   messages from dumb compilers. */
   mmnew = NULL;
   pmnew = NULL;

/* Store pointers to the supplied PermMap and the MatrixMap. */
   pm = (AstPermMap *) maps[ 1 - imm ];
   mm = (AstMatrixMap *) maps[ imm ];

/* Temporarily set the Invert attribute of the supplied PermMap to the
   supplied value. */
   old_pinv = astGetInvert( pm );
   astSetInvert( pm, inverts[ 1 - imm ] );

/* Ensure the MatrixMap is stored in full form. */
   ExpandMatrix( mm, status );

/* Store a pointer to the required array of matrix elements. */
   if( inverts[ imm ] ) {
      matrix = mm->i_matrix;
   } else {
      matrix = mm->f_matrix;
   }

/* Get the number of input and output axes of the PermMap. */
   nin = astGetNin( pm );
   nout = astGetNout( pm );

/* Allocate memory to hold the matrix elements for the swapped MatrixMap.
   The number of rows and olumns in the new matrix must equal the number of
   input or output axes for the PermMap, depending on whether the PermMap
   or MatrixMap is applied first. */
   if( imm == 0 ) {
      out_mat = (double *) astMalloc( sizeof( double )*(size_t)( nout*nout ) );
   } else {
      out_mat = (double *) astMalloc( sizeof( double )*(size_t)( nin*nin ) );
   }

/* We need to know the axis permutation arrays and constants array for
   the PermMap. */
   PermGet( pm, &outperm, &inperm, &consts, status );
   if( astOK ) {

/* First deal with cases where the MatrixMap is applied first. */
      if( imm == 0 ) {

/* Consider each output axis of the PermMap. */
         for( i = 0; i < nout; i++ ) {

/* If this output is connected to one of the input axes... */
            row = outperm[ i ];
            if( row >= 0 && row < nin ) {

/* Permute the row of the supplied matrix which feeds the corresponding
   PermMap input axis (i.e. axis outperm[k] ) using the forward PermMap.
   Store zeros for any output axes which are assigned constants. This forms
   row i of the new MatrixMap. */
               out_el = out_mat + nout*i;
               for( k = 0; k < nout; k++ ){
                  col = outperm[ k ];
                  if( col >= 0 && col < nin ) {
                     *(out_el++) = *( matrix + nin*row + col );
                  } else {
                     *(out_el++) = 0.0;
                  }
               }

/* If this output is asigned a constant value, use a "diagonal" vector for
   row i of the new MatrixMap (i.e. all zeros except for a 1.0 in column
   i ). */
            } else {
               out_el = out_mat + nout*i;
               for( k = 0; k < nout; k++ ) {
                  if( k != i ) {
                     *(out_el++) = 0.0;
                  } else {
                     *(out_el++) = 1.0;
                  }
               }
            }
         }

/* Create the new MatrixMap. */
         mmnew = astMatrixMap( nout, nout, 0, out_mat, "", status );

/* Any PermMap inputs which are assigned a constant value need to be
   changed now, since they will no longer be scaled by the inverse
   MatrixMap. CanSwap ensures that the inverse MatrixMap produces a
   simple scaling for constant axes, so we change the PermMap constant
   to be the constant AFTER scaling by the inverse MatrixMap.

   Consider each input axis of the PermMap. */
         for( i = 0; i < nin; i++ ) {

/* If this input is assigned a constant value... */
            if( inperm[ i ] < 0 ) {

/* Divide the supplied constant value by the corresponding diagonal term
   in the supplied MatrixMap. */
               c = consts[ -inperm[ i ] - 1 ];
               if( c != AST__BAD ) {
                  matel = matrix[ i*( nin + 1 ) ];
                  if( matel != 0.0 && matel != AST__BAD ) {
                     consts[ -inperm[ i ] - 1 ] /= matel;
                  } else {
                     consts[ -inperm[ i ] - 1 ] = AST__BAD;
                  }
               }
            }
         }

/* Now deal with cases where the PermMap is applied first. */
      } else {

/* Consider each input axis of the PermMap. */
         for( i = 0; i < nin; i++ ) {

/* If this input is connected to one of the output axes... */
            row = inperm[ i ];
            if( row >= 0 && row < nout ) {

/* Permute the row of the supplied matrix which feeds the corresponding
   PermMap output axis (i.e. axis inperm[k] ) using the inverse PermMap.
   Store zeros for any input axes which are assigned constants. This forms
   row i of the new MatrixMap. */
               out_el = out_mat + nin*i;
               for( k = 0; k < nin; k++ ){
                  col = inperm[ k ];
                  if( col >= 0 && col < nout ) {
                     *(out_el++) = *( matrix + nout*row + col );
                  } else {
                     *(out_el++) = 0.0;
                  }
               }

/* If this input is asigned a constant value, use a "diagonal" vector for
   row i of the new MatrixMap (i.e. all zeros except for a 1.0 in column
   i ). */
            } else {
               out_el = out_mat + nin*i;
               for( k = 0; k < nin; k++ ) {
                  if( k != i ) {
                     *(out_el++) = 0.0;
                  } else {
                     *(out_el++) = 1.0;
                  }
               }
            }
         }

/* Create the new MatrixMap. */
         mmnew = astMatrixMap( nin, nin, 0, out_mat, "", status );

/* Any PermMap outputs which are assigned a constant value need to be
   changed now, since they will no longer be scaled by the forward
   MatrixMap. CanSwap ensures that the forward MatrixMap produces a
   simple scaling for constant axes, so we change the PermMap constant
   to be the constant AFTER scaling by the forward MatrixMap.

   Consider each output axis of the PermMap. */
         for( i = 0; i < nout; i++ ) {

/* If this output is assigned a constant value... */
            if( outperm[ i ] < 0 ) {

/* Multiple the supplied constant value by the corresponding diagonal term in
   the supplied MatrixMap. */
               c = consts[ -outperm[ i ] - 1 ];
               if( c != AST__BAD ) {
                  matel = matrix[ i*( nout + 1 ) ];
                  if( matel != AST__BAD ) {
                     consts[ -outperm[ i ] - 1 ] *= matel;
                  } else {
                     consts[ -outperm[ i ] - 1 ] = AST__BAD;
                  }
               }
            }
         }
      }

/* Create a new PermMap (since the constants may have changed). */
      pmnew = astPermMap( nin, inperm, nout, outperm, consts, "", status );

/* Free the axis permutation and constants arrays. */
      outperm = (int *) astFree( (void *) outperm );
      inperm = (int *) astFree( (void *) inperm );
      consts = (double *) astFree( (void *) consts );
   }

/* Free the memory used to hold the new matrix elements. */
   out_mat = (double *) astFree( (void *) out_mat );

/* Ensure the supplied MatrixMap is stored back in compressed form. */
   CompressMatrix( mm, status );

/* Re-instate the original value of the Invert attribute of the supplied
   PermMap. */
   astSetInvert( pm, old_pinv );

   if( astOK ) {

/* Annul the supplied PermMap. */
      (void) astAnnul( pm );

/* Simplify the returned Mappings. */
      spmnew = astSimplify( pmnew );
      pmnew = astAnnul( pmnew );

      smmnew = astSimplify( mmnew );
      mmnew = astAnnul( mmnew );

/* Store a pointer to the new PermMap in place of the supplied MatrixMap. This
   PermMap should be used in its forward direction. */
      maps[ imm ] = (AstMapping *) spmnew;
      inverts[ imm ] = astGetInvert( spmnew );

/* Annul the supplied matrixMap. */
      (void) astAnnul( mm );

/* Store a pointer to the new MatrixMap. This MatrixMap should be used in
   its forward direction. */
      maps[ 1 - imm ] = (AstMapping *) smmnew;
      inverts[ 1 - imm ] = astGetInvert( smmnew );
   }

/* Return. */
   return;
}

static void MatWin( AstMapping **maps, int *inverts, int imm, int *status ){
/*
*  Name:
*     MatWin

*  Purpose:
*     Swap a WinMap and a MatrixMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "matrixmap.h"
*     void MatWin( AstMapping **maps, int *inverts, int imm, int *status )

*  Class Membership:
*     WinMap member function

*  Description:
*     A list of two Mappings is supplied containing a WinMap and a
*     MatrixMap. These Mappings are annulled, and replaced with
*     another pair of Mappings consisting of a WinMap and a MatrixMap
*     in the opposite order. These Mappings are chosen so that their
*     combined effect is the same as the original pair of Mappings.
*     The scale factors in the returned WinMap are always unity (i.e.
*     the differences in scaling get absorbed into the returned
*     MatrixMap).

*  Parameters:
*     maps
*        A pointer to an array of two Mapping pointers.
*     inverts
*        A pointer to an array of two invert flags.
*     imm
*        The index within "maps" of the MatrixMap.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstMatrixMap *m1;             /* Pointer to Diagonal scale factor MatrixMap */
   AstMatrixMap *m2;             /* Pointer to returned MatrixMap */
   AstMatrixMap *sm2;            /* Pointer to simplified returned MatrixMap */
   AstMatrixMap *mm;             /* Pointer to the supplied MatrixMap */
   AstPointSet *pset1;           /* Shift terms from supplied WinMap */
   AstPointSet *pset2;           /* Shift terms for returned WinMap */
   AstWinMap *w1;                /* Pointer to the returned WinMap */
   AstWinMap *sw1;               /* Pointer to the simplified returned WinMap */
   AstWinMap *wm;                /* Pointer to the supplied WinMap */
   double **ptr1;                /* Pointer to pset1 data */
   double **ptr2;                /* Pointer to pset2 data */
   double *a;                    /* Array of shift terms from supplied WinMap */
   double *aa;                   /* Pointer to next shift term */
   double *b;                    /* Array of scale terms from supplied WinMap */
   double *bb;                   /* Pointer to next scale term */
   int i;                        /* Axis count */
   int nin;                      /* No. of axes in supplied WinMap */
   int nout;                     /* No. of axes in returned WinMap */
   int old_minv;                 /* Invert value for the supplied MatrixMap */
   int old_winv;                 /* Invert value for the supplied WinMap */

/* Check the global error status. */
   if ( !astOK ) return;

/* Store pointers to the supplied WinMap and the MatrixMap. */
   wm = (AstWinMap *) maps[ 1 - imm ];
   mm = (AstMatrixMap *) maps[ imm ];

/* Temporarily set the Invert attribute of the supplied Mappings to the
   supplied values. */
   old_winv = astGetInvert( wm );
   astSetInvert( wm, inverts[ 1 - imm ] );

   old_minv = astGetInvert( mm );
   astSetInvert( mm, inverts[ imm ] );

/* Get copies of the shift and scale terms used by the WinMap. This
   also returns the number of axes in the WinMap. */
   nin = astWinTerms( wm, &a, &b );

/* Create a diagonal MatrixMap holding the scale factors from the
   supplied WinMap. */
   m1 = astMatrixMap( nin, nin, 1, b, "", status );

/* Create a PointSet holding a single position given by the shift terms
   in the supplied WinMap. */
   pset1 = astPointSet( 1, nin, "", status );
   ptr1 = astGetPoints( pset1 );
   if( astOK ){
      aa = a;
      for( i = 0; i < nin; i++ ) ptr1[ i ][ 0 ] = *(aa++);
   }

/* First deal with cases when the WinMap is applied first, followed by
   the MatrixMap. */
   if( imm == 1 ){

/* Multiply the diagonal matrix holding the WinMap scale factors by the
   supplied matrix. The resulting MatrixMap is the one to return in the
   map list. */
      m2 = astMtrMult( m1, mm );

/* Transform the position given by the shift terms from the supplied
   WinMap using the supplied MatrixMap to get the shift terms for
   the returned WinMap. */
      pset2 = astTransform( mm, pset1, 1, NULL );

/* Now deal with cases when the MatrixMap is applied first, followed by
   the WinMap. */
   } else {

/* Multiply the supplied MatrixMap by the diagonal matrix holding scale
   factors from the supplied WinMap. The resulting MatrixMap is the one to
   return in the map list. */
      m2 = astMtrMult( mm, m1 );

/* Transform the position given by the shift terms from the supplied
   WinMap using the inverse of the returned MatrixMap to get the shift
   terms for the returned WinMap. */
      pset2 = astTransform( m2, pset1, 0, NULL );

   }

/* Re-instate the original value of the Invert attributes of the supplied
   Mappings. */
   astSetInvert( wm, old_winv );
   astSetInvert( mm, old_minv );

/* Get pointers to the shift terms for the returned WinMap. */
   ptr2 = astGetPoints( pset2 );

/* Create the returned WinMap, initially with undefined corners. The number of
   axes in the WinMap must equal the number of shift terms. */
   nout = astGetNcoord( pset2 );
   w1 = astWinMap( nout, NULL, NULL, NULL, NULL, "", status );

/* If succesful, store the scale and shift terms in the WinMap. The scale
   terms are always unity. */
   if( astOK ){
      bb = w1->b;
      aa = w1->a;
      for( i = 0; i < nout; i++ ) {
         *(bb++) = 1.0;
         *(aa++) = ptr2[ i ][ 0 ];
      }

/* Replace the supplied Mappings and invert flags with the ones found
   above. Remember that the order of the Mappings is now swapped */
      (void) astAnnul( maps[ 0 ] );
      (void) astAnnul( maps[ 1 ] );

      sw1 = astSimplify( w1 );
      w1 = astAnnul( w1 );

      maps[ imm ] = (AstMapping *) sw1;
      inverts[ imm  ] = astGetInvert( sw1 );

      sm2 = astSimplify( m2 );
      m2 = astAnnul( m2 );

      maps[ 1 - imm ] = (AstMapping *) sm2;
      inverts[ 1 - imm  ] = astGetInvert( sm2 );

   }

/* Annul the MatrixMap and PointSet holding the scale and shift terms from the
   supplied WinMap. */
   m1 = astAnnul( m1 );
   pset1 = astAnnul( pset1 );
   pset2 = astAnnul( pset2 );

/* Free the copies of the scale and shift terms from the supplied WinMap. */
   b = (double *) astFree( (void *) b );
   a = (double *) astFree( (void *) a );

/* Return. */
   return;
}

static AstMatrixMap *MatZoom( AstMatrixMap *mm, AstZoomMap *zm, int minv,
                              int zinv, int *status ){
/*
*  Name:
*     MatZoom

*  Purpose:
*     Create a MatrixMap by merging a MatrixMap and a ZoomMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "matrixmap.h"
*     AstMatrixMap *MatZoom( AstMatrixMap *mm, AstZoomMap *zm, int minv,
*                            int zinv, int *status )

*  Class Membership:
*     MatrixMap member function

*  Description:
*     This function creates a new MatrixMap which performs a mapping
*     equivalent to applying the two supplied Mappings in series in the
*     directions specified by the "invert" flags (the Invert attributes of
*     the supplied MatrixMaps are ignored).

*  Parameters:
*     mm
*        A pointer to the MatrixMap.
*     zm
*        A pointer to the ZoomMap.
*     minv
*        The invert flag to use with mm. A value of zero causes the forward
*        mapping to be used, and a non-zero value causes the inverse
*        mapping to be used.
*     zinv
*        The invert flag to use with zm.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the new MatrixMap.

*  Notes:
*     -  The forward direction of the returned MatrixMap is equivalent to the
*     combined effect of the two supplied Mappings, operating in the
*     directions specified by "zinv" and "minv".
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstMatrixMap *mm2;            /* Pointer to intermediate MatrixMap */
   AstMatrixMap *result;         /* Pointer to output MatrixMap */
   double *matrix;               /* Pointer to diagonal matrix elements array */
   double zfac;                  /* Zoom factor */
   int i;                        /* Axis index */
   int nrow;                     /* No. of rows in the MatrixMap */
   int old_minv;                 /* Original setting of MatrixMap Invert attribute */
   int old_zinv;                 /* Original setting of ZoomMap Invert attribute */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise the returned pointer. */
   result = NULL;

/* Temporarily set the Invert attributes of both Mappings to the supplied
   values. */
   old_minv = astGetInvert( mm );
   astSetInvert( mm, minv );

   old_zinv = astGetInvert( zm );
   astSetInvert( zm, zinv );

/* Get the number of rows in the MatrixMap (i.e. the number of output
   axes). */
   nrow = astGetNout( mm );

/* Get the zoom factor implemented by the ZoomMap. Invert it if necessary
   since astGetZoom does not take account of the Invert setting.  */
   zfac = astGetZoom( zm );
   if( zinv ) zfac = 1.0 / zfac;

/* Create a diagonal matrix map in which each diagonal element is equal
   to the zoom factor. */
   matrix = (double *) astMalloc( sizeof( double )*(size_t) nrow );
   if( astOK ) {
      for( i = 0; i < nrow; i++ ) matrix[ i ] = zfac;
   }
   mm2 = astMatrixMap( nrow, nrow, 1, matrix, "", status );
   matrix = (double *) astFree( (void *) matrix );

/* Create a new MatrixMap holding the product of the supplied MatrixMap
   and the diagonal MatrixMap just created. */
   result = astMtrMult( mm, mm2 );
   mm2 = astAnnul( mm2 );

/* Re-instate the original settings of the Invert attribute for the
   supplied Mappings. */
   astSetInvert( mm, old_minv );
   astSetInvert( zm, old_zinv );

/* If an error has occurred, annull the returned MatrixMap. */
   if( !astOK ) result = astAnnul( result );

/* Return a pointer to the output MatrixMap. */
   return result;
}

/* Functions which access class attributes. */
/* ---------------------------------------- */
/* Implement member functions to access the attributes associated with
   this class using the macros defined for this purpose in the
   "object.h" file. For a description of each attribute, see the class
   interface (in the associated .h file). */

static AstMatrixMap *MtrMult( AstMatrixMap *this, AstMatrixMap *a, int *status ){
/*
*+
*  Name:
*     astMtrMult

*  Purpose:
*     Multiply a MatrixMap by another MatrixMap.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "matrixmap.h"
*     AstMatrixMap *MtrMult( astMatrixMap *this, astMatrixMap *a )

*  Class Membership:
*     MatrixMap method

*  Description:
*     This function multiples the matrices given by "this" and "a", returning
*     a pointer to a new MatrixMap holding the product "a x this".
*
*     The number of columns in the "a" matrix must match the number of
*     rows in the "this" matrix. The number of rows in the returned
*     MatrixMap is equal to the number of rows in "a", and the number of
*     columns is the same as the number of rows in "this".

*  Parameters:
*     this
*        Pointer to the first MatrixMap.
*     a
*        Pointer to a second MatrixMap.

*  Returned Value:
*     A pointer to the product MatrixMap.

*  Notes:
*     -  An error is reported if the two MatrixMaps have incompatible
*     shapes, or if either MatrixMap does not have a defined forward
*     transformation.
*     - A null Object pointer will also be returned if this function
*     is invoked with the AST error status set, or if it should fail
*     for any reason.
*-
*/

/* Local variables. */
   astDECLARE_GLOBALS        /* Pointer to thread-specific global data */
   AstMatrixMap *new;        /* New MatrixMap holding the product matrix */
   double *a_matrix;         /* Pointer to the forward "a" matrix */
   double *a_row;            /* Pointer to start of current row in "a" */
   double a_val;             /* Current element value from "a" */
   double factor;            /* Diagonal matrix term */
   double *new_matrix;       /* Pointer to the new forward "this" matrix */
   double *new_val;          /* Pointer to current output element value */
   double sum;               /* Dot product value */
   double *this_col;         /* Pointer to start of current column in "this" */
   double *this_matrix;      /* Pointer to the forward "this" matrix */
   double this_val;          /* Current element value from "this" */
   int col;                  /* Current output column number */
   int i;                    /* Loop count */
   int minrow;               /* Min. number of rows in "a" or "this" */
   int ncol_a;               /* No. of columns in the "a" MatrixMap */
   int ncol_this;            /* No. of columns in the "this" MatrixMap */
   int nrow_a;               /* No. of rows in the "a" MatrixMap */
   int nrow_this;            /* No. of rows in the "this" MatrixMap */
   int row;                  /* Current output row number */

/* Return a NULL pointer if an error has already occurred. */
   if ( !astOK ) return NULL;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialise */
   new = NULL;

/* Report an error if eitherof the MatrixMaps doe snot have a defined
   forward transformation.*/
   if( !astGetTranForward( this ) ){
      astError( AST__MTRML, "astMtrMult(%s): Cannot find the product of 2 "
                "MatrixMaps- the first MatrixMap has no forward transformation.", status,
                astClass(this) );
      return NULL;
   }

   if( !astGetTranInverse( this ) ){
      astError( AST__MTRML, "astMtrMult(%s): Cannot find the product of 2 "
                "MatrixMaps- the second MatrixMap has no forward transformation.", status,
                astClass(this) );
      return NULL;
   }

/* Report an error if the shapes of the two matrices are incompatible. */
   nrow_a = astGetNout( a );
   ncol_a = astGetNin( a );
   nrow_this = astGetNout( this );
   ncol_this = astGetNin( this );

   if( ncol_a != nrow_this && astOK ){
      astError( AST__MTRML, "astMtrMult(%s): Number of rows in the first "
                "MatrixMap (%d) does not equal number of columns in the "
                "second MatrixMap (%d).", status, astClass(this), nrow_this, ncol_a );
      return NULL;
   }

/* Store the minimum number of rows in either matrix for later use. */
   if( nrow_a < nrow_this ){
      minrow = nrow_a;
   } else {
      minrow = nrow_this;
   }

/* Ensure that "this" is stored in FULL form (i.e. with all elements
   stored explicitly, even if the matrix is a unit or diagonal matrix). */
   ExpandMatrix( this, status );

/* Store pointers to the current forward matrices (taking into
   account the current states of the Mapping inversion flags ). */
   this_matrix = astGetInvert( this ) ? this->i_matrix : this->f_matrix;
   a_matrix = astGetInvert( a ) ? a->i_matrix : a->f_matrix;

/* Get memory to hold the product matrix in full form. */
   new_matrix = (double *) astMalloc( sizeof( double )*
                                      (size_t)( nrow_a*ncol_this ) );
   if( astOK ){

/* First deal with cases where the "a" MatrixMap represents a unit
   matrix. */
      if( a->form == UNIT ){

/* Copy the required number of rows from "this" to "new". */
         (void) memcpy( (void *) new_matrix, (const void *) this_matrix,
                         sizeof(double)*(size_t)( minrow*ncol_this ) );

/* If there are insufficient rows in "this", append some zero-filled rows. */
         if( minrow < nrow_a ){
            for( i = minrow*ncol_this; i < nrow_a*ncol_this; i++ ){
               new_matrix[ i ] = 0.0;
            }
         }

/* Now deal with cases where the "a" MatrixMap represents a diagonal
   matrix. */
      } else if( a->form == DIAGONAL ){

/* Scale the required number of rows from "this" storing them in "new",
   and checking for bad values. */
         i = 0;

         for( row = 0; row < minrow; row++ ){
            factor = a_matrix[ row ];

            if( factor != AST__BAD ){

               for( col = 0; col < ncol_this; col++ ){
                  this_val = this_matrix[ i ];
                  if( this_val != AST__BAD ){
                     new_matrix[ i ] = this_val*factor;
                  } else {
                     new_matrix[ i ] = AST__BAD;
                  }
                  i++;
               }

            } else {

               for( col = 0; col < ncol_this; col++ ){
                  new_matrix[ i++ ] = AST__BAD;
               }

            }
         }

/* If there are insufficient rows in "this", append some zero-filled rows. */
         if( minrow < nrow_a ){
            for( i = minrow*ncol_this; i < nrow_a*ncol_this; i++ ){
               new_matrix[ i ] = 0.0;
            }
         }


/* Now deal with cases where the "a" MatrixMap represents a full, non-diagonal
   matrix. */
      } else {

/* Initialise a pointer to the next element in the product matrix. */
         new_val = new_matrix;

/* Get a pointer to the start of each row of the "a" matrix. */
         for( row = 0; row < nrow_a; row++ ){
            a_row = a_matrix + ncol_a*row;

/* Get a pointer to the start of each column of the "this" matrix. */
            for( col = 0; col < ncol_this; col++ ){
               this_col = this_matrix + col;

/* Form the dot product of the current row from "a", and the current
   column from "this", checking for bad values. */
               sum = 0.0;
               for( i = 0; i < ncol_a; i++ ){
                  a_val = a_row[ i ];
                  this_val = this_col[ i*ncol_this ];
                  if( a_val != AST__BAD && this_val != AST__BAD ){
                     sum += a_val*this_val;
                  } else {
                     sum = AST__BAD;
                     break;
                  }
               }

/* Store the output matrix element value. */
               *(new_val++) = sum;

            }
         }
      }

/* Create the new MatrixMap. */
      new = astInitMatrixMap( NULL, sizeof( AstMatrixMap ), !class_init,
                              &class_vtab, "MatrixMap", ncol_this, nrow_a,
                              FULL, new_matrix );

/* If possible, compress the new MatrixMap by removing off-diagonal zero
   elements. */
      CompressMatrix( new, status );

/* Re-compress the original "this" MatrixMap. */
      CompressMatrix( this, status );

   }

/* Free the memory used to hold the product matrix in full form. */
   new_matrix = (double *) astFree( (void *) new_matrix );

   return new;

}

static AstMatrixMap *MtrRot( AstMatrixMap *this, double theta,
                             const double axis[], int *status ){
/*
*+
*  Name:
*     astMtrRot

*  Purpose:
*     Multiply a MatrixMap by a rotation matrix.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "matrixmap.h"
*     AstMatrixMap *astMtrRot( astMatrixMap *this, double theta,
*                              const double axis[] )

*  Class Membership:
*     MatrixMap method.

*  Description:
*     This function creates a new MatrixMap which is a copy of "this",
*     rotated by a specified angle. It can only be used on MatrixMaps which
*     have either 2 or 3 output coordinates. In the 3-D case, the rotation
*     is about an arbitrary axis passing through the origin.

*  Parameters:
*     this
*        Pointer to the MatrixMap.
*     theta
*        The angle by which to rotate the matrix, in radians. If the matrix
*        is applied to a 2-D vector position, the resulting vector is
*        rotated clockwise about the origin (i.e. from the positive direction
*        of the second axis to the positive direction of the first axis). If
*        the vector positions are three dimensional, the rotation is clockwise
*        when looking along the vector given by "axis". Note, "theta" measures
f        when looking along the vector given by AXIS. Note, THETA measures
*        the movemement of the vectors relative to a fixed reference frame.
*        Alternatively, the reference frame can be thought of as rotating by
*        (-theta) relative to the fixed vectors.
*     axis
*        A 3-D vector specifying the axis of rotation. This parameter is
*        ignored if the output from MatrixMap is 2-dimensional.

*  Returned Value:
*     A pointer to the rotated MatrixMap.

*  Notes:
*     - A null Object pointer will also be returned if this function
*     is invoked with the AST error status set, or if it should fail
*     for any reason.
*-
*/

/* Local variables. */
   AstMatrixMap *new;        /* New MatrixMap holding the rotated matrix */
   double as,a,b,c,d,e,f,g;  /* Intermediate quantities */
   double axlen;             /* Length of axis vector */
   double axlen2;            /* Squared length of axis vector */
   double costh;             /* Cos(rotation angle) */
   double sinth;             /* Sin(rotation angle) */
   double rotmat[9];         /* Rotation matrix */
   double work[3];           /* Work space for matrix multiplication */
   int ncol;                 /* No. of columns in the MatrixMap */
   int nrow;                 /* No. of rows in the MatrixMap */

/* Return a NULL pointer if an error has already occurred. */
   if ( !astOK ) return NULL;

/* Initialise the returned MarixMap to be a copy of the supplied MatrixMap. */
   new = astCopy( this );

/* Save the cos and sin of the rotation angle for future use. */
   costh = cos( theta );
   sinth = sin( theta );

/* Return without changing the MatrixMap if the rotation angle is a
   multiple of 360 degrees. */
   if ( costh == 1.0 ) return new;

/* Get the dimensions of the MatrixMap. */
   nrow = astGetNout( new );
   ncol = astGetNin( new );

/* First do rotation of a plane about the origin. */
   if( nrow == 2 ){

/* Ensure that the MatrixMap is stored in full form rather than
   compressed form. */
      ExpandMatrix( new, status );

/* Form the 2x2 forward rotation matrix. Theta is the clockwise angle
   of rotation. */
      rotmat[0] = costh;
      rotmat[1] = sinth;
      rotmat[2] = -sinth;
      rotmat[3] = costh;

/*  Post-multiply the current forward matrix (depending on whether or not
    the MatrixMap has been inverted) by the forward rotation matrix. */
      if( !astGetInvert( new ) ){
         SMtrMult( 1, 2, ncol, rotmat, new->f_matrix, work, status );
      } else {
         SMtrMult( 1, 2, ncol, rotmat, new->i_matrix, work, status );
      }

/* Now form the 2x2 inverse rotation matrix (the diagonal elements
   don't change). */
      rotmat[1] = -sinth;
      rotmat[2] = sinth;

/*  Pre-multiply the current inverse matrix (depending on whether or
    not the MatrixMap has been inverted) by the inverse rotation matrix. */
      if( !astGetInvert( new ) ){
         SMtrMult( 0, ncol, 2, rotmat, new->i_matrix, work, status );
      } else {
         SMtrMult( 0, ncol, 2, rotmat, new->f_matrix, work, status );
      }

/*  See if the matrix can be stored as a UNIT or DIAGONAL matrix. */
      CompressMatrix( new, status );

/* Now do rotation of a volume about an axis passing through the origin. */
   } else if( nrow == 3 ){

/* Find the length of the axis vector. Report an error if it has zero
   length or has not been supplied. */
      if( axis ) {
         axlen2 = axis[0]*axis[0] + axis[1]*axis[1] + axis[2]*axis[2];
      } else {
         axlen2 = 0.0;
      }
      if( axlen2 <= 0.0 ) {
         astError( AST__MTRAX, "astMtrRot(%s): NULL or zero length "
                   "axis vector supplied.", status, astClass(new) );
      }
      axlen = sqrt( axlen2 );

/* Ensure that the MatrixMap is stored in full form rather than
   compressed form. */
      ExpandMatrix( new, status );

/* Form commonly used terms in the rotation matrix. */
      as = sinth/axlen;
      a = (1.0 - costh)/axlen2;
      b = a*axis[0]*axis[1];
      c = as*axis[2];
      d = a*axis[0]*axis[2];
      e = as*axis[1];
      f = a*axis[1]*axis[2];
      g = as*axis[0];

/* Form the 3x3 forward rotation matrix. Theta is the clockwise angle
   of rotation looking in the direction of the axis vector. */
      rotmat[0] = a*axis[0]*axis[0] + costh;
      rotmat[1] = b - c;
      rotmat[2] = d + e;
      rotmat[3] = b + c;
      rotmat[4] = a*axis[1]*axis[1] + costh;
      rotmat[5] = f - g;
      rotmat[6] = d - e;
      rotmat[7] = f + g;
      rotmat[8] = a*axis[2]*axis[2] + costh;

/*  Post-multiply the current forward matrix (depending on whether or not
    the MatrixMap has been inverted) by the forward rotation matrix. */
      if( !astGetInvert( new ) ){
         SMtrMult( 1, 3, ncol, rotmat, new->f_matrix, work, status );
      } else {
         SMtrMult( 1, 3, ncol, rotmat, new->i_matrix, work, status );
      }

/* Now form the 3x3 inverse rotation matrix (the diagonal elements
   don't change). */
      rotmat[1] = b + c;
      rotmat[2] = d - e;
      rotmat[3] = b - c;
      rotmat[5] = f + g;
      rotmat[6] = d + e;
      rotmat[7] = f - g;

/* Pre-multiply the current inverse matrix (depending on whether or
   not the MatrixMap has been inverted) by the inverse rotation matrix. */
      if( !astGetInvert( new ) ){
         SMtrMult( 0, ncol, 3, rotmat, new->i_matrix, work, status );
      } else {
         SMtrMult( 0, ncol, 3, rotmat, new->f_matrix, work, status );
      }

/*  See if the matrix can be stored as a UNIT or DIAGONAL matrix. */
      CompressMatrix( new, status );

/* Report an error if the matrix is not suitable for rotation. */
   } else {
      astError( AST__MTR23, "astMtrRot(%s): Cannot rotate a %dx%d"
                " MatrixMap.", status, astClass(new), nrow, ncol );
   }

/* Delete the new MatrixMap if an error has occurred. */
   if( !astOK ) new = astDelete( new );

   return new;

}

static void PermGet( AstPermMap *map, int **outperm, int **inperm,
                     double **consts, int *status ){
/*
*  Name:
*     PermGet

*  Purpose:
*     Get the axis permutation and constants array for a PermMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "matrixmap.h"
*     void PermGet( AstPermMap *map, int **outperm, int **inperm,
*                   double **const, int *status )

*  Class Membership:
*     MatrixMap member function

*  Description:
*     This function returns axis permutation and constants arrays which can
*     be used to create a PermMap which is equivalent to the supplied PermMap.

*  Parameters:
*     map
*        The PermMap.
*     outperm
*        An address at which to return a popinter to an array of ints
*        holding the output axis permutation array. The array should be
*        released using astFree when no longer needed.
*     inperm
*        An address at which to return a popinter to an array of ints
*        holding the input axis permutation array. The array should be
*        released using astFree when no longer needed.
*     consts
*        An address at which to return a popinter to an array of doubles
*        holding the constants array. The array should be released using
*        astFree when no longer needed.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     -  NULL pointers are returned if an error has already occurred, or if
*     this function should fail for any reason.
*/

/* Local Variables: */
   AstPointSet *pset1;       /* PointSet holding input positions for PermMap */
   AstPointSet *pset2;       /* PointSet holding output positions for PermMap */
   double **ptr1;            /* Pointer to pset1 data */
   double **ptr2;            /* Pointer to pset2 data */
   double *cnst;             /* Pointer to constants array */
   double cn;                /* Potential new constant value */
   double ip;                /* Potential output axis index */
   double op;                /* Potential input axis index */
   int *inprm;               /* Pointer to input axis permutation array */
   int *outprm;              /* Pointer to output axis permutation array */
   int i;                    /* Axis count */
   int nc;                   /* Number of constants stored so far */
   int nin;                  /* No. of input coordinates for the PermMap */
   int nout;                 /* No. of output coordinates for the PermMap */

/* Initialise. */
   if( outperm ) *outperm = NULL;
   if( inperm ) *inperm = NULL;
   if( consts ) *consts = NULL;

/* Check the global error status and the supplied pointers. */
   if ( !astOK || !outperm || !inperm || !consts ) return;

/* Get the number of input and output axes for the supplied PermMap. */
   nin = astGetNin( map );
   nout = astGetNout( map );

/* Allocate the memory for the returned arrays. */
   outprm = (int *) astMalloc( sizeof( int )* (size_t) nout );
   inprm = (int *) astMalloc( sizeof( int )* (size_t) nin );
   cnst = (double *) astMalloc( sizeof( double )* (size_t) ( nout + nin ) );

/* Returned the pointers to these arrays.*/
   *outperm = outprm;
   *inperm = inprm;
   *consts = cnst;

/* Create two PointSets, each holding two points, which can be used for
   input and output positions with the PermMap. */
   pset1 = astPointSet( 2, nin, "", status );
   pset2 = astPointSet( 2, nout, "", status );

/* Set up the two input positions to be [0,1,2...] and [-1,-1,-1,...]. The
   first position is used to enumerate the axes, and the second is used to
   check for constant axis values. */
   ptr1 = astGetPoints( pset1 );
   if( astOK ){
      for( i = 0; i < nin; i++ ){
         ptr1[ i ][ 0 ] = ( double ) i;
         ptr1[ i ][ 1 ] = -1.0;
      }
   }

/* Use the PermMap to transform these positions in the forward direction. */
   (void) astTransform( map, pset1, 1, pset2 );

/* No constant axis valeus found yet. */
   nc = 0;

/* Look at the mapped positions to determine the output axis permutation
   array. */
   ptr2 = astGetPoints( pset2 );
   if( astOK ){

/* Do each output axis. */
      for( i = 0; i < nout; i++ ){

/* If the output axis value is copied from an input axis value, the index
   of the appropriate input axis will be in the mapped first position. */
         op = ptr2[ i ][ 0 ];

/* If the output axis value is assigned a constant value, the result of
   mapping the two different input axis values will be the same. */
         cn = ptr2[ i ][ 1 ];
         if( op == cn ) {

/* We have found another constant. Store it in the constants array, and
   store the index of the constant in the output axis permutation array. */
            cnst[ nc ] = cn;
            outprm[ i ] = -( nc + 1 );
            nc++;

/* If the output axis values are different, then the output axis value
   must be copied from the input axis value. */
         } else {
            outprm[ i ] = (int) ( op + 0.5 );
         }
      }
   }

/* Now do the same thing to determine the input permutation array. */
   if( astOK ){
      for( i = 0; i < nout; i++ ){
         ptr2[ i ][ 0 ] = ( double ) i;
         ptr2[ i ][ 1 ] = -1.0;
      }
   }

   (void) astTransform( map, pset2, 0, pset1 );

   if( astOK ){

      for( i = 0; i < nin; i++ ){

         ip = ptr1[ i ][ 0 ];
         cn = ptr1[ i ][ 1 ];
         if( ip == cn ) {

            cnst[ nc ] = cn;
            inprm[ i ] = -( nc + 1 );
            nc++;

         } else {
            inprm[ i ] = (int) ( ip + 0.5 );
         }
      }
   }

/* Annul the PointSets. */
   pset1 = astAnnul( pset1 );
   pset2 = astAnnul( pset2 );

/* If an error has occurred, attempt to free the returned arrays. */
   if( !astOK ) {
      *outperm = (int *) astFree( (void *) *outperm );
      *inperm = (int *) astFree( (void *) *inperm );
      *consts = (double *) astFree( (void *) *consts );
   }

/* Return. */
   return;
}

static int PermOK( AstMapping *pm, int *status ){
/*
*  Name:
*     PermOK

*  Purpose:
*     Determine if a PermMap can be merged with a MatrixMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "matrixmap.h"
*     int PermOK( AstMapping *pm, int *status )

*  Class Membership:
*     PermMap member function

*  Description:
*     This function returns a flag indicating if the supplied PermMap
*     could be merged with a MatrixMap. For thios to be possible, the
*     PermMap must have the same number of input and output axes, and the
*     inverse and forward mappings must be consistent.

*  Parameters:
*     pm
*        The PermMap.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     1 if the PermMap can be merged, 0 otherwise.

*  Notes:
*     -  A value of 0 is returned if an error has already occurred, or if
*     this function should fail for any reason.
*/

/* Local Variables: */
   AstPointSet *pset1;       /* PointSet holding input positions for PermMap */
   AstPointSet *pset2;       /* PointSet holding output positions for PermMap */
   double **ptr1;            /* Pointer to pset1 data */
   int i;                    /* Loop count */
   int nin;                  /* No. of input coordinates for the PermMap */
   int nout;                 /* No. of output coordinates for the PermMap */
   int ret;                  /* Returned flag */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Initialise */
   ret = 0;

/* The PermMap must have the same number of input and output coordinates. */
   nin = astGetNin( pm );
   nout = astGetNout( pm );
   if( nin == nout ){

/* Create two PointSets, each holding two points, which can be used for
   the input and output positions with the PermMap. */
      pset1 = astPointSet( 2, nin, "", status );
      pset2 = astPointSet( 2, nout, "", status );

/* Set up the two input positions to be [1,2,3...] and [0,-1,-2,...] */
      ptr1 = astGetPoints( pset1 );
      if( astOK ){
         for( i = 0; i < nin; i++ ){
            ptr1[ i ][ 0 ] = ( double )( i + 1 );
            ptr1[ i ][ 1 ] = ( double )( -i );
         }

      }

/* Use the PermMap to transform these positions in the forward direction. */
      (void) astTransform( pm, pset1, 1, pset2 );

/* Now transform the results back again using the inverse PermMap. */
      (void) astTransform( pm, pset2, 0, pset1 );

/* See if the input positions have changed. If they have, then the PermMap
   does not have a consistent pair of transformations. If they have not,
   then the transformations must be consistent because we used two
   different input positions and only one could come out unchanged by
   chance. */
      if( astOK ){
         ret = 1;
         for( i = 0; i < nin; i++ ){
            if( ptr1[ i ][ 0 ] != ( double )( i + 1 ) ||
                ptr1[ i ][ 1 ] != ( double )( -i ) ){
               ret = 0;
               break;
            }
         }
      }

/* Annul the PointSets. */
      pset1 = astAnnul( pset1 );
      pset2 = astAnnul( pset2 );
   }

/* Return the answer. */
   return astOK ? ret : 0;
}

static double Rate( AstMapping *this, double *at, int ax1, int ax2, int *status ){
/*
*  Name:
*     Rate

*  Purpose:
*     Calculate the rate of change of a Mapping output.

*  Type:
*     Private function.

*  Synopsis:
*     #include "matrixmap.h"
*     result = Rate( AstMapping *this, double *at, int ax1, int ax2, int *status )

*  Class Membership:
*     MatrixMap member function (overrides the astRate method inherited
*     from the Mapping class ).

*  Description:
*     This function returns the rate of change of a specified output of
*     the supplied Mapping with respect to a specified input, at a
*     specified input position.

*  Parameters:
*     this
*        Pointer to the Mapping to be applied.
*     at
*        The address of an array holding the axis values at the position
*        at which the rate of change is to be evaluated. The number of
*        elements in this array should equal the number of inputs to the
*        Mapping.
*     ax1
*        The index of the Mapping output for which the rate of change is to
*        be found (output numbering starts at 0 for the first output).
*     ax2
*        The index of the Mapping input which is to be varied in order to
*        find the rate of change (input numbering starts at 0 for the first
*        input).
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The rate of change of Mapping output "ax1" with respect to input
*     "ax2", evaluated at "at", or AST__BAD if the value cannot be
*     calculated.

*/

/* Local Variables: */
   AstMatrixMap *map;
   double *matrix;
   double result;

/* Check inherited status */
   if( !astOK ) return AST__BAD;

/* Get a pointer to the MatrixMap structure. */
   map = (AstMatrixMap *) this;

/* Get a pointer to the array holding the required matrix elements, according
   to whether the MatrixMap has been inverted. */
   if( !astGetInvert( this ) ) {
      matrix = map->f_matrix;
   } else {
      matrix = map->i_matrix;
   }

/* First deal with full MatrixMaps in which all matrix elements are stored. */
   if( map->form == FULL ){
      result = matrix[ ax1*astGetNin( this ) + ax2 ];

/* For unit matrices, the rate is unity if the input and output axes are
   equal, and zero otherwise. */
   } else if( map->form == UNIT ){
      result = (ax1 == ax2 ) ? 1.0 : 0.0;

/* For diagonal matrices, the rate is zero for off diagonal elements and
   the matrix array stored the on-diagonal rates. */
   } else if( ax1 == ax2 ) {
      result = matrix[ ax1 ];

   } else {
      result = 0.0;
   }

/* Return the result. */
   return result;
}

static void SMtrMult( int post, int m, int n, const double *mat1,
                        double *mat2, double *work, int *status ){
/*
*  Name:
*     SMtrMult

*  Purpose:
*     Multiply a square matrix and a non-square matrix.

*  Type:
*     Private function.

*  Synopsis:
*     #include "matrixmap.h"
*     void SMtrMult( int post, int m, int n, const double *mat1,
*                    double *mat2, double *work, int *status )

*  Class Membership:
*     MatrixMap member function.

*  Description:
*     The matrix pointed to by "mat2" is modified by multiplying it by
*     the square matrix pointed to by "mat1". If "post" is 1, then:
*
*        mat2 -> mat1*mat2  (mat1 is mxm and mat2 is mxn)
*
*     If "post" is zero, then:
*
*        mat2 -> mat2*mat1  (mat1 is nxn and mat2 is mxn)
*
*     The restriction that "mat1" must be square is imposed so that the
*     returned matrix will have the same shape as the supplied matrix (mat1).

*  Parameters:
*     post
*        Specifies whether to post- or pre- multiply mat2 by mat1.
*     m
*        The number of rows in mat2.
*     n
*        The number of columns in mat2.
*     mat1
*        The multiplier matrix. It must be square of size m or n, depending
*        on "post".
*     mat2
*        The multiplicand matrix.
*     work
*        Pointer to work space containing room for m doubles (if "post"
*        is 1), or n doubles (if "post" is 0).
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     -  No error is reported if "mat2" is supplied NULL. In this case
*     it will also be returned NULL.
*/

/* Local Variables */
   double dot;             /* Output matrix element value */
   const double *mat1_col; /* Pointer to start of current column of mat1 */
   const double *mat1_row; /* Pointer to start of current row of mat1 */
   double *mat2_col;       /* Pointer to start of current column of mat2 */
   double *mat2_row;       /* Pointer to start of current row of mat2 */
   double cel;             /* Column element value */
   double rel;             /* Row element value */
   int i;                  /* Index of current output row */
   int j;                  /* Index of current output column */
   int k;                  /* Dot product index */

/* Do nothing if mat2 is NULL */
   if ( mat2 ){

/* First deal with cases where the supplied matrix is post-multiplied
   (i.e. the returned matrix is mat1*mat2). */
      if( post ){

/* Loop round each column of the output matrix, storing a pointer to
   the start of the corresponding column of mat2. */
         for( j=0; j<n; j++ ){
            mat2_col = mat2 + j;

/* Loop round each row of the output matrix, storing a pointer to
   the start of the corresponding row of mat1. */
            for( i=0; i<m; i++ ){
               mat1_row = mat1 + i*m;

/* Get the dot product of the corresponding row from mat1 and the
   corresponding column from mat2 and store it in the work array. */
               dot = 0.0;
               for( k=0; k<m; k++ ) {
                  rel = mat1_row[ k ];
                  cel = mat2_col[ k*n ];
                  if( rel != AST__BAD && cel != AST__BAD ){
                     dot += rel*cel;
                  } else {
                     dot = AST__BAD;
                     break;
                  }
               }
               work[ i ] = dot;
            }

/* Copy the values stored in the work array to the current column of
   the output matrix. */
            for( i=0; i<m; i++ ) mat2_col[ i*n ] = work[ i ];
         }

/* Now deal with cases where the supplied matrix is pre-multiplied
   (i.e. the returned matrix is mat2*mat1). */
      } else {

/* Loop round each row of the output matrix, storing a pointer to
   the start of the corresponding row of mat2. */
         for( i=0; i<m; i++ ){
            mat2_row = mat2 + i*n;

/* Loop round each column of the output matrix, storing a pointer to
   the start of the corresponding column of mat1. */
            for( j=0; j<n; j++ ){
               mat1_col = mat1 + j;

/* Get the dot product of the corresponding row from mat2 and the
   corresponding column from mat1 and store it in the work array. */
               dot = 0.0;
               for( k=0; k<n; k++ ) {
                  rel = mat2_row[ k ];
                  cel = mat1_col[ k*n ];
                  if( rel != AST__BAD && cel != AST__BAD ){
                     dot += rel*cel;
                  } else {
                     dot = AST__BAD;
                     break;
                  }
               }
               work[ j ] = dot;
            }

/* Copy the values stored in the work array to the current row of
   the output matrix. */
            for( j=0; j<n; j++ ) mat2_row[ j ] = work[ j ];
         }
      }
   }

   return;

}

static int GetTranForward( AstMapping *this, int *status ) {
/*
*
*  Name:
*     GetTranForward

*  Purpose:
*     Determine if a MatrixMap defines a forward coordinate transformation.

*  Type:
*     Private function.

*  Synopsis:
*     #include "matrixmap.h"
*     int GetTranForward( AstMapping *this, int *status )

*  Class Membership:
*     MatrixMap member function (over-rides the astGetTranForward method
*     inherited from the Mapping class).

*  Description:
*     This function returns a value indicating if the MatrixMap is able
*     to perform a forward coordinate transformation.

*  Parameters:
*     this
*        Pointer to the MatrixMap.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Zero if the forward coordinate transformation is not defined, or 1 if it
*     is.

*  Notes:
*     -  A value of zero will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstMatrixMap *map;            /* Pointer to MatrixMap to be queried */
   int invert;                   /* Has the mapping been inverted? */
   int result;                   /* The returned value */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the MatrixMap. */
   map = (AstMatrixMap *) this;

/* All unit MatrixMaps are defined in both directions. */
   if( map->form == UNIT ) {
      result = 1;

/* Otherwise, check that the appropriate array is defined in the
   MatrixMap. */
   } else {

/* Determine if the Mapping has been inverted. */
      invert = astGetInvert( this );

/* If OK, obtain the result. */
      if ( astOK ) {

         if( invert ){
            result = ( map->i_matrix != NULL );
         } else {
            result = ( map->f_matrix != NULL );
         }

      }

   }

/* Return the result. */
   return result;

}

static int GetTranInverse( AstMapping *this, int *status ) {
/*
*
*  Name:
*     GetTranInverse

*  Purpose:
*     Determine if a MatrixMap defines an inverse coordinate transformation.

*  Type:
*     Private function.

*  Synopsis:
*     #include "matrixmap.h"
*     int GetTranInverse( AstMapping *this, int *status )

*  Class Membership:
*     MatrixMap member function (over-rides the astGetTranInverse method
*     inherited from the Mapping class).

*  Description:
*     This function returns a value indicating if the MatrixMap is able
*     to perform an inverse coordinate transformation.

*  Parameters:
*     this
*        Pointer to the MatrixMap.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Zero if the inverse coordinate transformation is not defined, or 1 if it
*     is.

*  Notes:
*     -  A value of zero will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstMatrixMap *map;            /* Pointer to MatrixMap to be queried */
   int invert;                   /* Has the mapping been inverted? */
   int result;                   /* The returned value */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the MatrixMap. */
   map = (AstMatrixMap *) this;

/* All unit MatrixMaps are defined in both directions. */
   if( map->form == UNIT ) {
      result = 1;

/* Otherwise, check that the appropriate array is defined in the
   MatrixMap. */
   } else {

/* Determine if the Mapping has been inverted. */
      invert = astGetInvert( this );

/* If OK, obtain the result. */
      if ( astOK ) {

         if( invert ){
            result = ( map->f_matrix != NULL );
         } else {
            result = ( map->i_matrix != NULL );
         }

      }

   }

/* Return the result. */
   return result;

}

static AstPointSet *Transform( AstMapping *this, AstPointSet *in,
                               int forward, AstPointSet *out, int *status ) {
/*
*  Name:
*     Transform

*  Purpose:
*     Apply a MatrixMap to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "matrixmap.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     MatrixMap member function (over-rides the astTransform protected
*     method inherited from the Mapping class).

*  Description:
*     This function takes a MatrixMap and a set of points encapsulated in a
*     PointSet and transforms the points by multiplying them by the matrix.

*  Parameters:
*     this
*        Pointer to the MatrixMap.
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
*     match the number of columns in the MatrixMap being applied.
*     -  The number of coordinate values per point in the output PointSet will
*     equal the number of rows in the MatrixMap being applied.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstPointSet *result;          /* Pointer to output PointSet */
   AstMatrixMap *map;            /* Pointer to MatrixMap to be applied */
   double diag_term;             /* Current diagonal element value */
   double *indata;               /* Pointer to next input data value */
   double *matrix;               /* Pointer to start of matrix element array */
   double *matrix_element;       /* Pointer to current matrix element value */
   double *outdata;              /* Pointer to next output data value */
   double **ptr_in;              /* Pointer to input coordinate data */
   double **ptr_out;             /* Pointer to output coordinate data */
   double sum;                   /* Partial output value */
   double val;                   /* Data value */
   int in_coord;                 /* Index of output coordinate */
   int nax;                      /* Output axes for which input axes exist */
   int ncoord_in;                /* Number of coordinates per input point */
   int ncoord_out;               /* Number of coordinates per output point */
   int npoint;                   /* Number of points */
   int out_coord;                /* Index of output coordinate */
   int point;                    /* Loop counter for points */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the MatrixMap. */
   map = (AstMatrixMap *) this;

/* Apply the parent mapping using the stored pointer to the Transform member
   function inherited from the parent Mapping class. This function validates
   all arguments and generates an output PointSet if necessary, but does not
   actually transform any coordinate values. */
   result = (*parent_transform)( this, in, forward, out, status );

/* We will now extend the parent astTransform method by performing the
   calculations needed to generate the output coordinate values. */

/* Determine the numbers of points and coordinates per point from the input
   and output PointSets and obtain pointers for accessing the input and
   output coordinate values. */
   ncoord_in = astGetNcoord( in );
   ncoord_out = astGetNcoord( result );
   npoint = astGetNpoint( in );
   ptr_in = astGetPoints( in );
   ptr_out = astGetPoints( result );

/* Determine whether to apply the forward or inverse mapping, according to the
   direction specified and whether the mapping has been inverted. */
   if ( astGetInvert( map ) ) forward = !forward;

/* Get a pointer to the array holding the required matrix elements, according
   to the direction of mapping required. */
   if ( forward ) {
      matrix = map->f_matrix;
   } else {
      matrix = map->i_matrix;
   }

/* Perform coordinate arithmetic. */
/* ------------------------------ */
   if ( astOK ) {

/* First deal with full MatrixMaps in which all matrix elements are stored. */
      if( map->form == FULL ){

/* Loop to apply the matrix to each point in turn, checking for
   (and propagating) bad values in the process. The matrix elements are
   accessed sequentially in row order. The next matrix element to be
   used is identified by a pointer which is initialised to point to the
   first element of the matrix prior to processing each point. */
         for ( point = 0; point < npoint; point++ ) {
            matrix_element = matrix;

/* Each output co-ordinate value is created by summing the product of the
   corresponding input co-ordinates and the elements of one row of the
   matrix. */
            for ( out_coord = 0; out_coord < ncoord_out; out_coord++ ) {
               sum = 0.0;

               for ( in_coord = 0; in_coord < ncoord_in; in_coord++ ) {

/*  Check the current input coordinate value and the current matrix element.
    If the coordinate value is bad, then the output value will also be
    bad unless the matrix element is zero. That is, a zero matrix element
    results in the input coordinate value being ignored, even if it is bad.
    This prevents bad input values being propagated to output axes which
    are independant of the bad input axis. A bad matrix element always results
    in the output value being bad. In either of these cases, break out of the
    loop, remembering to advance the pointer to the next matrix element so
    that it points to the start of the next row ready for doing the next
    output coordinate. */
                  if ( ( ptr_in[ in_coord ][ point ] == AST__BAD &&
                                         (*matrix_element) != 0.0 ) ||
                       (*matrix_element) == AST__BAD ) {
                     sum = AST__BAD;
                     matrix_element += ncoord_in - in_coord;
                     break;

/*  If the input coordinate and the current matrix element are both
    valid, increment the sum by their product, and step to the next matrix
    element pointer If we arrive here with a bad input value, then the
    matrix element must be zero, in which case the running sum is left
    unchanged. */
                  } else {
                     if ( ptr_in[ in_coord ][ point ] != AST__BAD ) {
                        sum += ptr_in[ in_coord ][ point ] * (*matrix_element);
                     }
                     matrix_element++;
                  }
               }

/*  Store the output coordinate value. */
               ptr_out[ out_coord ][ point ] = sum;

            }

         }

/* Now deal with unit and diagonal MatrixMaps. */
      } else {

/* Find the number of output axes for which input data is available. */
         if( ncoord_in < ncoord_out ){
            nax = ncoord_in;
         } else {
            nax = ncoord_out;
         }

/* For unit matrices, copy the input axes to the corresponding output axes. */
         if( map->form == UNIT ){
            for( out_coord = 0; out_coord < nax; out_coord++ ) {
               (void) memcpy( ptr_out[ out_coord ],
                              (const void *) ptr_in[ out_coord ],
                              sizeof( double )*(size_t)npoint );
            }

/* For diagonal matrices, scale each input axis using the appropriate
   diagonal element from the matrix, and store in the output. */
         } else {
            for( out_coord = 0; out_coord < nax; out_coord++ ){
               diag_term = matrix[ out_coord ];
               outdata = ptr_out[ out_coord ];
               indata = ptr_in[ out_coord ];

               if( diag_term != AST__BAD ){
                  for( point = 0; point < npoint; point++ ){
                     val = *(indata++);
                     if( val != AST__BAD ){
                        *(outdata++) = diag_term*val;
                     } else {
                        *(outdata++) = AST__BAD;
                     }
                  }

               } else {
                  for( point = 0; point < npoint; point++ ){
                     *(outdata++) = AST__BAD;
                  }
               }
            }
         }

/* If there are any remaining output axes, fill the first one with zeros. */
         if( nax < ncoord_out ){
            outdata = ptr_out[ nax ];
            for( point = 0; point < npoint; point++ ) *(outdata++) = 0.0;

/* Copy this axis to any remaining output axes. */
            outdata = ptr_out[ nax ];
            for( out_coord = nax + 1; out_coord < ncoord_out; out_coord++ ) {
               (void) memcpy( ptr_out[ out_coord ], (const void *) outdata,
                              sizeof( double )*(size_t)npoint );
            }
         }
      }
   }

/* Return a pointer to the output PointSet. */
   return result;
}

static int ScalingRowCol( AstMatrixMap *map, int axis, int *status ){
/*
*  Name:
*     ScalingRowCol

*  Purpose:
*     Determine if a given row and column of a MatrixMap are zeros
*     with a non-zero diagonal term.

*  Type:
*     Private function.

*  Synopsis:
*     #include "matrixmap.h"
*     int ScalingRowCol( AstMatrixMap *map, int axis, int *status )

*  Class Membership:
*     MatrixMap member function

*  Description:
*     This function returns a flag indicating if a MatrixMap presents a
*     simple scaling for a given axis in both directions. The MatrixMap
*     must be square. A value of one is returned if every element of the
*     row and column corresponding to the given axis is zero, except for
*     the diagonal term which must be non-zero.

*  Parameters:
*     map
*        The MatrixMap.
*     axis
*        The zero-based index of the axis to check.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     1 if the row/column produces a simple scaling, 0 otherwise.

*/

/* Local Variables: */
   double *el;               /* Pointer to matrix element */
   int i;                    /* Element count */
   int ncol;                 /* No. of input coordinates */
   int ret;                  /* Returned flag */

/* Initialise */
   ret = 0;

/* Check the global error status. */
   if ( !astOK ) return ret;

/* If a unit or diagonal MatrixMap has been supplied, return 1. */
   if( map->form != FULL ){
      ret = 1;

/* If a full matrix has been supplied... */
   } else {

/* Assume the row/column gives a unit mapping. */
      ret = 1;

/* Get the number of input axes for the MatrixMap. */
      ncol = astGetNin( map );

/* Check that all elements of the "axis"th row are effectively zero, except
   for the "axis"th element which must be non-zero. */
      el = map->f_matrix + axis*ncol;
      for( i = 0; i < ncol; i++ ) {
         if( i == axis ) {
            if( fabs( *el ) <= DBL_EPSILON ) {
               ret = 0;
               break;
            }
         } else if( fabs( *el ) > DBL_EPSILON ) {
            ret = 0;
            break;
         }
         el++;
      }

/* Check that all elements of the "axis"th column are effectively zero, except
   for the "axis"th element which must be non-zero. */
      if( ret ) {
         el = map->f_matrix + axis;
         for( i = 0; i < ncol; i++ ) {
            if( i == axis ) {
               if( fabs( *el ) <= DBL_EPSILON ) {
                  ret = 0;
                  break;
               }
            } else if( fabs( *el ) > DBL_EPSILON ) {
               ret = 0;
               break;
            }
            el += ncol;
         }
      }
   }

/* Return the answer. */
   return astOK ? ret : 0;
}

/* Functions which access class attributes. */
/* ---------------------------------------- */
/* Implement member functions to access the attributes associated with
   this class using the macros defined for this purpose in the
   "object.h" file. For a description of each attribute, see the class
   interface (in the associated .h file). */

/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout, int *status ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for MatrixMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for MatrixMap objects.

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
*     -  This constructor makes a deep copy, including a copy of the matrix
*     element values associated with the input MatrixMap.
*/


/* Local Variables: */
   AstMatrixMap *in;             /* Pointer to input MatrixMap */
   AstMatrixMap *out;            /* Pointer to output MatrixMap */
   int nel;                      /* No. of elements in the matrix */
   int nin;                      /* No. of input coordinates */
   int nout;                     /* No. of output coordinates */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output MatrixMaps. */
   in = (AstMatrixMap *) objin;
   out = (AstMatrixMap *) objout;

/* Nullify the pointers stored in the output object since these will
   currently be pointing at the input data (since the output is a simple
   byte-for-byte copy of the input). Otherwise, the input data could be
   freed by accidient if the output object is deleted due to an error
   occuring in this function. */
   out->f_matrix = NULL;
   out->i_matrix = NULL;

/* If the input MatrixMap is a unit mapping, then no matrix elements are
   stored with it, so do nothing in this case. */
   if( out->form != UNIT ){

/* Obtain the number of stored values in the MatrixMap. This is independant of
   whether the Mapping has been inverted or not. If the MatrixMap is diagonal,
   only the diagonal terms are stored. */
      nin = astGetNin( in );
      nout = astGetNout( in );

      if( out->form == DIAGONAL ){
         if( nin < nout ){
            nel = nin;
         } else {
            nel = nout;
         }

      } else {
         nel = nin*nout;
      }

/* Store the forward matrix elements in the output MatrixMap. */
      out->f_matrix = (double *) astStore( NULL, (void *) in->f_matrix,
                                           sizeof( double )*(size_t) nel );

/* Store the inverse matrix elements (if defined) in the output
   MatrixMap. */
      if( in->i_matrix ){
         out->i_matrix = (double *) astStore( NULL, (void *) in->i_matrix,
                                              sizeof( double )*(size_t) nel );
      }

/* If an error has occurred, free the output MatrixMap arrays. */
      if( !astOK ) {
         out->f_matrix = (double *) astFree( (void *) out->f_matrix );
         out->i_matrix = (double *) astFree( (void *) out->i_matrix );
      }
   }

   return;

}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for MatrixMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for MatrixMap objects.

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
   AstMatrixMap *this;            /* Pointer to MatrixMap */

/* Obtain a pointer to the MatrixMap structure. */
   this = (AstMatrixMap *) obj;

/* Free the arrays used to store element values for forward and inverse
   matrices. */
   this->f_matrix = (double *) astFree( (void *) this->f_matrix );
   this->i_matrix = (double *) astFree( (void *) this->i_matrix );
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for MatrixMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the MatrixMap class to an output Channel.

*  Parameters:
*     this
*        Pointer to the MatrixMap whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

#define KEY_LEN 50               /* Maximum length of a keyword */

/* Local Variables: */
   AstMatrixMap *this;           /* Pointer to the MatrixMap structure */
   char buff[ KEY_LEN + 1 ];     /* Buffer for keyword string */
   int el;                       /* Element index */
   int nel;                      /* No. of elements in the matrix */
   int nin;                      /* No. of input coords */
   int nout;                     /* No. of output coords */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the MatrixMap structure. */
   this = (AstMatrixMap *) this_object;

/* Find the number of elements stored for each matrix. */
   nin = astGetNin( this );
   nout = astGetNout( this );

   if( this->form == FULL ){
      nel = nin*nout;

   } else if( this->form == DIAGONAL ){
      nel = MIN( nin, nout );

   } else {
      nel = 0;
   }

/* Write out values representing the instance variables for the
   MatrixMap class.  */

/* The forward matrix. The inverse matrix not written out since it can be
   re-calculated when the MatrixMap is read back in. Note BAD values are
   not written out as the AST__BAD value may differ on different machines.
   If a matrix element is not found when reading the matrix back in
   again (in astLoadMatrixMap), then it is assigned a default value of
   AST__BAD. */
   if( this->f_matrix ){
      for( el = 0; el < nel; el++ ){
         if( (this->f_matrix)[ el ] != AST__BAD ) {
            (void) sprintf( buff, "M%d", el );
            astWriteDouble( channel, buff, 1, 1, (this->f_matrix)[ el ],
                            "Forward matrix value" );
         }
      }
   }

/* The matrix storage form. */
   astWriteString( channel, "Form", 1, 1, Form[ this->form ],
                   "Matrix storage form" );

/* Undefine macros local to this function. */
#undef KEY_LEN
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAMatrixMap and astCheckMatrixMap functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(MatrixMap,Mapping)
astMAKE_CHECK(MatrixMap)

AstMatrixMap *astMatrixMap_( int nin, int nout, int form,
                             const double matrix[], const char *options, int *status, ...){
/*
*++
*  Name:
c     astMatrixMap
f     AST_MATRIXMAP

*  Purpose:
*     Create a MatrixMap.

*  Type:
*     Public function.

*  Synopsis:
c     #include "matrixmap.h"
c     AstMatrixMap *astMatrixMap( int nin, int nout, int form,
c                                 const double matrix[],
c                                 const char *options, ... )
f     RESULT = AST_MATRIXMAP( NIN, NOUT, FORM, MATRIX, OPTIONS, STATUS )

*  Class Membership:
*     MatrixMap constructor.

*  Description:
*     This function creates a new MatrixMap and optionally initialises
*     its attributes.
*
*     A MatrixMap is a form of Mapping which performs a general linear
*     transformation.  Each set of input coordinates, regarded as a
*     column-vector, are pre-multiplied by a matrix (whose elements
*     are specified when the MatrixMap is created) to give a new
*     column-vector containing the output coordinates. If appropriate,
*     the inverse transformation may also be performed.

*  Parameters:
c     nin
f     NIN = INTEGER (Given)
*        The number of input coordinates, which determines the number
*        of columns in the matrix.
c     nout
f     NOUT = INTEGER (Given)
*        The number of output coordinates, which determines the number
*        of rows in the matrix.
c     form
f     FORM = INTEGER (Given)
*        An integer which indicates the form in which the matrix
*        elements will be supplied.
*
c        A value of zero indicates that a full "nout" x "nin" matrix
f        A value of zero indicates that a full NOUT x NIN  matrix
c        of values will be supplied via the "matrix" parameter
f        of values will be supplied via the MATRIX argument
*        (below). In this case, the elements should be given in row
*        order (the elements of the first row, followed by the
*        elements of the second row, etc.).
*
*        A value of 1 indicates that only the diagonal elements of the
*        matrix will be supplied, and that all others should be
c        zero. In this case, the elements of "matrix" should contain
f        zero. In this case, the elements of MATRIX should contain
*        only the diagonal elements, stored consecutively.
*
*        A value of 2 indicates that a "unit" matrix is required,
*        whose diagonal elements are set to unity (with all other
c        elements zero).  In this case, the "matrix" parameter is
c        ignored and a NULL pointer may be supplied.
f        elements zero).  In this case, the MATRIX argument is not used.
c     matrix
f     MATRIX( * ) = DOUBLE PRECISION (Given)
*        The array of matrix elements to be used, stored according to
c        the value of "form".
f        the value of FORM.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new MatrixMap. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new MatrixMap. The syntax used is identical to that for the
f        AST_SET routine.
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
c     astMatrixMap()
f     AST_MATRIXMAP = INTEGER
*        A pointer to the new MatrixMap.

*  Notes:
*     - In general, a MatrixMap's forward transformation will always
*     be available (as indicated by its TranForward attribute), but
*     its inverse transformation (TranInverse attribute) will only be
*     available if the associated matrix is square and non-singular.
*     - As an exception to this, the inverse transformation is always
*     available if a unit or diagonal matrix is specified. In this
*     case, if the matrix is not square, one or more of the input
*     coordinate values may not be recoverable from a set of output
*     coordinates. Any coordinates affected in this way will simply be
*     set to the value zero.
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.

*  Status Handling:
*     The protected interface to this function includes an extra
*     parameter at the end of the parameter list descirbed above. This
*     parameter is a pointer to the integer inherited status
*     variable: "int *status".

*--
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstMatrixMap *new;            /* Pointer to new MatrixMap */
   va_list args;                 /* Variable argument list */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialise the MatrixMap, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitMatrixMap( NULL, sizeof( AstMatrixMap ), !class_init,
                           &class_vtab, "MatrixMap", nin, nout, form, matrix);

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new MatrixMap's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new MatrixMap. */
   return new;
}

AstMatrixMap *astMatrixMapId_( int nin, int nout, int form, const double matrix[],
                               const char *options, ... ) {
/*
*  Name:
*     astMatrixMapId_

*  Purpose:
*     Create a MatrixMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "matrixmap.h"
*     AstMatrixMap *astMatrixMapId_( int nin, int nout, int form,
*                                    const double matrix[], const char *options,
*                                    ... )

*  Class Membership:
*     MatrixMap constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astMatrixMap constructor function. It returns an ID value (instead
*     of a true C pointer) to external users, and must be provided
*     because astMatrixMap_ has a variable argument list which cannot be
*     encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     The variable argument list also prevents this function from
*     invoking astMatrixMap_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astMatrixMap_.

*  Returned Value:
*     The ID value associated with the new MatrixMap.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstMatrixMap *new;            /* Pointer to new MatrixMap */
   va_list args;                 /* Variable argument list */
   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the MatrixMap, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitMatrixMap( NULL, sizeof( AstMatrixMap ), !class_init, &class_vtab,
                         "MatrixMap", nin, nout, form, matrix );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new MatrixMap's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new MatrixMap. */
   return astMakeId( new );
}

AstMatrixMap *astInitMatrixMap_( void *mem, size_t size, int init,
                                 AstMatrixMapVtab *vtab, const char *name,
                                 int nin, int nout, int form,
                                 const double *matrix, int *status ) {
/*
*+
*  Name:
*     astInitMatrixMap

*  Purpose:
*     Initialise a MatrixMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "matrixmap.h"
*     AstMatrixMap *astInitMatrixMap( void *mem, size_t size, int init,
*                                 AstMatrixMapVtab *vtab, const char *name,
*                                 int nin, int nout, int form,
*                                 const double *matrix )

*  Class Membership:
*     MatrixMap initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new MatrixMap object. It allocates memory (if necessary) to accommodate
*     the MatrixMap plus any additional data associated with the derived class.
*     It then initialises a MatrixMap structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a MatrixMap at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the MatrixMap is to be initialised.
*        This must be of sufficient size to accommodate the MatrixMap data
*        (sizeof(MatrixMap)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the MatrixMap (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the MatrixMap
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the MatrixMap's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new MatrixMap.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the astGetClass
*        method).
*     nin
*        The number of input coordinate values per point. This is the
*        same as the number of columns in the matrix.
*     nout
*        The number of output coordinate values per point. This is the
*        same as the number of rows in the matrix.
*     form
*        If "form" is 2 or larger, then a unit MatrixMap is created. In this
*        case "matrix" is ignored and can be supplied as NULL. If "form" is
*        1, then a diagonal MatrixMap is created. In this case, the number of
*        values in "matrix" should be equal to the minimum of nin and nout,
*        and "matrix" should contain the corresponding diagonal terms, in row
*        order. If "form" is 0 or less, then a full MatrixMap is created, and
*        "matrix" should contain all nin*nout element values.
*     matrix
*        A pointer to an array of matrix element values. The values should be
*        supplied in row order. The content of this array is determined by
*        "form". If a full MatrixMap is to be created then the array starts
*        with (row 1, column 1), then comes (row 1, column 2), up to (row 1,
*        column nin), then (row 2, column 1), (row 2, column 2), and so on,
*        finishing with (row nout, column nin) ). An error is reported if a
*        NULL value is supplied unless "form" is 2 or more.

*  Returned Value:
*     A pointer to the new MatrixMap.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstMatrixMap *new;              /* Pointer to new MatrixMap */
   double *fmat;                   /* Pointer to the forward matrix */
   double *imat;                   /* Pointer to the inverse matrix */
   int i;                          /* Loop count */
   int nel;                        /* No. of elements in matrix array */
   int nuse;                       /* Number of usable matrix elements */
   int used_form;                  /* Form limited to 0, 1 or 2 */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitMatrixMapVtab( vtab, name );

/* Initialise. */
   new = NULL;

/* Report an error if a NULL matrix was supplied, unless a unit MatrixMap
   has been requested. */
   if( form < 2 && !matrix ){
      astError( AST__MTRMT, "astInitMatrixMap(%s): NULL matrix supplied.", status,
                name );

   } else {

/* Initialise a Mapping structure (the parent class) as the first component
   within the MatrixMap structure, allocating memory if necessary. Specify that
   the Mapping should be defined in both the forward and inverse directions. */
      new = (AstMatrixMap *) astInitMapping( mem, size, 0,
                                           (AstMappingVtab *) vtab, name,
                                            nin, nout, 1, 1 );

      if ( astOK ) {

/* Initialise the MatrixMap data. */
/* ---------------------------- */
/* If a unit MatrixMap is being created, then no additional storage is
   required. */
         if( form > 1 ){
            nel = 0;
            used_form = UNIT;

/* If a diagonal MatrixMap is being created, then memory is needed to hold
   the diagonal terms. */
         } else if( form == 1 ){
            if( nin < nout ){
               nel = nin;
            } else {
               nel = nout;
            }
            used_form = DIAGONAL;

/* If a full MatrixMap is being created, then memory is needed to hold
   all the terms. */
         } else {
            nel = nin*nout ;
            used_form = FULL;
         }

/* Allocate memory for the forward matrix, storing the supplied matrix
   values in it. */
         fmat = (double *) astStore( NULL, (void *) matrix,
                                     sizeof(double)*(size_t)nel );

/* Replace any NaNs by AST__BAD and count the number of usable values. */
         if( nel > 0 ) {
            nuse = 0;
            for( i = 0; i < nel; i++ ) {
               if( !astISFINITE(fmat[ i ]) ) {
                  fmat[ i ] = AST__BAD;
               } else if( fmat[ i ] != AST__BAD ) {
                  nuse++;
               }
            }

/* Report an error if there are no usable values. */
            if( nuse == 0 && astOK ) {
               astError( AST__MTRMT, "astInitMatrixMap(%s): Supplied matrix "
                         "contains only bad values.",  status, name );
            }
         }

/* Create an inverse matrix if possible. */
         imat = InvertMatrix( used_form, nout, nin, fmat, status );

/* Store the matrix arrays. */
         new->form = used_form;
         new->f_matrix = fmat;
         new->i_matrix = imat;

/* Attempt to compress the MatrixMap into DIAGONAL or UNIT form. */
         CompressMatrix( new, status );

/* If an error occurred, clean up by deleting the new MatrixMap. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Return a pointer to the new MatrixMap. */
   return new;
}

AstMatrixMap *astLoadMatrixMap_( void *mem, size_t size,
                                 AstMatrixMapVtab *vtab, const char *name,
                                 AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadMatrixMap

*  Purpose:
*     Load a MatrixMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "matrixmap.h"
*     AstMatrixMap *astLoadMatrixMap( void *mem, size_t size,
*                                     AstMatrixMapVtab *vtab, const char *name,
*                                     AstChannel *channel )

*  Class Membership:
*     MatrixMap loader.

*  Description:
*     This function is provided to load a new MatrixMap using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     MatrixMap structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a MatrixMap at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the MatrixMap is to be
*        loaded.  This must be of sufficient size to accommodate the
*        MatrixMap data (sizeof(MatrixMap)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the MatrixMap (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the MatrixMap structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstMatrixMap) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new MatrixMap. If this is NULL, a pointer
*        to the (static) virtual function table for the MatrixMap class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "MatrixMap" is used instead.

*  Returned Value:
*     A pointer to the new MatrixMap.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

#define KEY_LEN 50               /* Maximum length of a keyword */

   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
/* Local Variables: */
   AstMatrixMap *new;            /* Pointer to the new MatrixMap */
   char buff[ KEY_LEN + 1 ];     /* Buffer for keyword string */
   const char *form;             /* String form */
   int def;                      /* Is the matrix defined? */
   int el;                       /* Element index */
   int nel;                      /* No. of elements in the matrix */
   int nin;                      /* No. of input coords */
   int nout;                     /* No. of output coords */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this MatrixMap. In this case the
   MatrixMap belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstMatrixMap );
      vtab = &class_vtab;
      name = "MatrixMap";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitMatrixMapVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built MatrixMap. */
   new = astLoadMapping( mem, size, (AstMappingVtab *) vtab, name,
                         channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "MatrixMap" );

/* Now obtain the Matrix storage form from this list. */
      form = astReadString( channel, "form", Form[FULL] );
      new->form = FindString( 3, Form, form, "the MatrixMap component 'Form'",
                              "astRead", astGetClass( channel ), status );
      form = astFree( (void *) form );

/* Find the number of elements stored for each matrix. */
      nin = astGetNin( (AstMapping *) new );
      nout = astGetNout( (AstMapping *) new );

      if( new->form == FULL ){
         nel = nin*nout;

      } else if( new->form == DIAGONAL ){
         nel = MIN( nin, nout );

      } else {
         nel = 0;
      }

/* Allocate memory to hold the forward matrix. */
      new->f_matrix = (double *) astMalloc( sizeof(double)*(size_t)nel );

/* Now read the other data items from the list and use them to
   initialise the appropriate instance variable(s) for this class. */

/* The forward matrix. */
      if( new->f_matrix ){
         def = 0;

         for( el = 0; el < nel; el++ ){
            (void) sprintf( buff, "m%d", el );
            (new->f_matrix)[ el ] = astReadDouble( channel, buff, AST__BAD );
            if( (new->f_matrix)[ el ] != AST__BAD ) def = 1;
         }

/* Store a NULL pointer if no elements of the matrix were found. */
         if( !def ) new->f_matrix = (double *) astFree( (void *) new->f_matrix );

      }

/* Create an inverse matrix if possible, otherwise store a NULL pointer. */
      if( new->f_matrix ){
         new->i_matrix = InvertMatrix( new->form, nout, nin, new->f_matrix, status );
      } else {
         new->i_matrix = NULL;
      }

/* If an error occurred, clean up by deleting the new MatrixMap. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new MatrixMap pointer. */
   return new;

/* Undefine macros local to this function. */
#undef KEY_LEN
}

/* Virtual function interfaces. */
/* ============================ */
/* These provide the external interface to the virtual functions defined by
   this class. Each simply checks the global error status and then locates and
   executes the appropriate member function, using the function pointer stored
   in the object's virtual function table (this pointer is located using the
   astMEMBER macro defined in "object.h").

   Note that the member function may not be the one defined here, as it may
   have been over-ridden by a derived class. However, it should still have the
   same interface. */

AstMatrixMap *astMtrRot_( AstMatrixMap *this, double theta,
                                const double axis[], int *status ){
   if( !astOK ) return NULL;
   return (**astMEMBER(this,MatrixMap,MtrRot))( this, theta, axis, status );
}

AstMatrixMap *astMtrMult_( AstMatrixMap *this, AstMatrixMap *a, int *status ){
   if( !astOK ) return NULL;
   return (**astMEMBER(this,MatrixMap,MtrMult))( this, a, status );
}




