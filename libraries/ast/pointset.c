/*
*  Name:
*     pointset.c

*  Purpose:
*     Implement the PointSet class.

*  Description:
*     This file implements the PointSet class. For a description of
*     the class and its interface, see the .h file of the same name.

*  Inheritance:
*     The PointSet class inherits from the Object class.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     1-FEB-1996 (RFWS):
*        Original version.
*     27-SEP-1996 (RFWS):
*        Added external interface and I/O facilities.
*     8-JAN-2003 (DSB):
*        Changed private InitVtab method to protected astInitPointSetVtab
*        method.
*     9-SEP-2004 (DSB):
*        Added astPermPoints.
*     5-OCT-2004 (DSB):
*        Bug fix in astLoadPointSet - npoint was used as size for new array
*        of pointers (changed to ncoord).
*     19-OCT-2004 (DSB):
*        Added astSetNpoint.
*     2-NOV-2004 (DSB):
*        - Do not write out AST__BAD axis values in the Dump function.
*        - Override astEqual method.
*        - Add protected PointAccuracy attribute.
*     7-JAN-2005 (DSB):
*        Added astAppendPoints.
*     14-FEB-2006 (DSB):
*        Override astGetObjSize.
*     22-FEB-2006 (DSB):
*        Avoid allocating memory for "acc" unless needed.
*     1-MAY-2009 (DSB):
*        Added astBndPoints.
*     16-JUN-2011 (DSB):
*        Added astReplaceNan.
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS PointSet

/* Values that control the behaviour of the astReplaceNaN method. */
#define IGNORE_NANS  0
#define REPLACE_NANS 1
#define REPORT_NANS  2

/*
*
*  Name:
*     MAKE_CLEAR

*  Purpose:
*     Implement a method to clear a single value in a multi-valued attribute.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "pointset.h"
*     MAKE_CLEAR(attr,component,assign)

*  Class Membership:
*     Defined by the PointSet class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static void Clear<Attribute>( AstPointSet *this, int axis )
*
*     and an external interface function of the form:
*
*        void astClear<Attribute>_( AstPointSet *this, int axis )
*
*     which implement a method for clearing a single value in a specified
*     multi-valued attribute for an axis of a PointSet. The "axis" value
*     must be in the range 0 to (ncoord-1).

*  Parameters:
*     attr
*        The name of the attribute to be cleared, as it appears in the function
*        name (e.g. PointAccuracy in "astClearPointAccuracy").
*     component
*        The name of the class structure component that holds the attribute
*        value.
*     assign
*        An expression that evaluates to the value to assign to the component
*        to clear its value.

*  Notes:
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*
*/

/* Define the macro. */
#define MAKE_CLEAR(attr,component,assign) \
\
/* Private member function. */ \
/* ------------------------ */ \
static void Clear##attr( AstPointSet *this, int axis, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Validate the axis index. */ \
   if( axis < 0 || axis >= this->ncoord ){ \
      astError( AST__AXIIN, "%s(%s): Index (%d) is invalid for attribute " \
                #attr " - it should be in the range 1 to %d.", status, \
                "astClear" #attr, astGetClass( this ), \
                axis + 1, this->ncoord ); \
\
/* Assign the "clear" value. */ \
   } else if( this->component ){ \
      this->component[ axis ] = (assign); \
   } \
} \
\
/* External interface. */ \
/* ------------------- */ \
void astClear##attr##_( AstPointSet *this, int axis, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Invoke the required method via the virtual function table. */ \
   (**astMEMBER(this,PointSet,Clear##attr))( this, axis, status ); \
}


/*
*
*  Name:
*     MAKE_GET

*  Purpose:
*     Implement a method to get a single value in a multi-valued attribute.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "pointset.h"
*     MAKE_GET(attr,type,bad_value,assign)

*  Class Membership:
*     Defined by the PointSet class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static <Type> Get<Attribute>( AstPointSet *this, int axis )
*
*     and an external interface function of the form:
*
*        <Type> astGet<Attribute>_( AstPointSet *this, int axis )
*
*     which implement a method for getting a single value from a specified
*     multi-valued attribute for an axis of a PointSet. The "axis" value
*     must be in the range 0 to (ncoord-1).


*  Parameters:
*     attr
*        The name of the attribute whose value is to be obtained, as it
*        appears in the function name (e.g. PointAccuracy in "astGetPointAccuracy").
*     type
*        The C type of the attribute.
*     bad_value
*        A constant value to return if the global error status is set, or if
*        the function fails.
*     assign
*        An expression that evaluates to the value to be returned. This can
*        use the string "axis" to represent the zero-based value index.

*  Notes:
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*
*/

/* Define the macro. */
#define MAKE_GET(attr,type,bad_value,assign) \
\
/* Private member function. */ \
/* ------------------------ */ \
static type Get##attr( AstPointSet *this, int axis, int *status ) { \
   type result;                  /* Result to be returned */ \
\
/* Initialise */ \
   result = bad_value; \
\
/* Check the global error status. */ \
   if ( !astOK ) return result; \
\
/* Validate the axis index. */ \
   if( axis < 0 || axis >= this->ncoord ){ \
      astError( AST__AXIIN, "%s(%s): Index (%d) is invalid for attribute " \
                #attr " - it should be in the range 1 to %d.", status, \
                "astGet" #attr, astGetClass( this ), \
                axis + 1, this->ncoord ); \
\
/* Assign the result value. */ \
   } else { \
      result = (assign); \
   } \
\
/* Check for errors and clear the result if necessary. */ \
   if ( !astOK ) result = (bad_value); \
\
/* Return the result. */ \
   return result; \
} \
/* External interface. */ \
/* ------------------- */  \
type astGet##attr##_( AstPointSet *this, int axis, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return (bad_value); \
\
/* Invoke the required method via the virtual function table. */ \
   return (**astMEMBER(this,PointSet,Get##attr))( this, axis, status ); \
}

/*
*
*  Name:
*     MAKE_SET

*  Purpose:
*     Implement a method to set a single value in a multi-valued attribute
*     for a PointSet.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "pointset.h"
*     MAKE_SET(attr,type,component,assign,null)

*  Class Membership:
*     Defined by the PointSet class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static void Set<Attribute>( AstPointSet *this, int axis, <Type> value )
*
*     and an external interface function of the form:
*
*        void astSet<Attribute>_( AstPointSet *this, int axis, <Type> value )
*
*     which implement a method for setting a single value in a specified
*     multi-valued attribute for a PointSet. The "axis" value must be in
*     the range 0 to (ncoord-1).

*  Parameters:
*      attr
*         The name of the attribute to be set, as it appears in the function
*         name (e.g. PointAccuracy in "astSetPointAccuracy").
*      type
*         The C type of the attribute.
*      component
*         The name of the class structure component that holds the attribute
*         value.
*      assign
*         An expression that evaluates to the value to be assigned to the
*         component.
*      null
*         The value to initialise newly created array elements to.

*  Notes:
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*-
*/

/* Define the macro. */
#define MAKE_SET(attr,type,component,assign,null) \
\
/* Private member function. */ \
/* ------------------------ */ \
static void Set##attr( AstPointSet *this, int axis, type value, int *status ) { \
   int i; \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Validate the axis index. */ \
   if( axis < 0 || axis >= this->ncoord ){ \
      astError( AST__AXIIN, "%s(%s): Index (%d) is invalid for attribute " \
                #attr " - it should be in the range 1 to %d.", status, \
                "astSet" #attr, astGetClass( this ), \
                axis + 1, this->ncoord ); \
\
/* Store the new value in the structure component. */ \
   } else { \
      if( !this->component ){ \
         this->component = astMalloc( this->ncoord*sizeof( type ) ); \
         for( i = 0; i < this->ncoord; i++ ) { \
            this->component[ i ] = null; \
         } \
      } \
      this->component[ axis ] = (assign); \
   } \
} \
\
/* External interface. */ \
/* ------------------- */ \
void astSet##attr##_( AstPointSet *this, int axis, type value, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Invoke the required method via the virtual function table. */ \
   (**astMEMBER(this,PointSet,Set##attr))( this, axis, value, status ); \
}

/*
*
*  Name:
*     MAKE_TEST

*  Purpose:
*     Implement a method to test if a single value has been set in a
*     multi-valued attribute for a class.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "pointset.h"
*     MAKE_TEST(attr,assign)

*  Class Membership:
*     Defined by the PointSet class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static int Test<Attribute>( AstPointSet *this, int axis )
*
*     and an external interface function of the form:
*
*        int astTest<Attribute>_( AstPointSet *this, int axis )
*
*     which implement a method for testing if a single value in a specified
*     multi-valued attribute has been set for a class. The "axis" value
*     must be in the range 0 to (ncoord-1).

*  Parameters:
*      attr
*         The name of the attribute to be tested, as it appears in the function
*         name (e.g. PointAccuracy in "astTestPointAccuracy").
*      assign
*         An expression that evaluates to 0 or 1, to be used as the returned
*         value. This can use the string "axis" to represent the zero-based
*         index of the value within the attribute.

*  Notes:
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*-
*/

/* Define the macro. */
#define MAKE_TEST(attr,assign) \
\
/* Private member function. */ \
/* ------------------------ */ \
static int Test##attr( AstPointSet *this, int axis, int *status ) { \
   int result;                   /* Value to return */ \
\
   result= 0; \
\
/* Check the global error status. */ \
   if ( !astOK ) return result; \
\
/* Validate the axis index. */ \
   if( axis < 0 || axis >= this->ncoord ){ \
      astError( AST__AXIIN, "%s(%s): Index (%d) is invalid for attribute " \
                #attr " - it should be in the range 1 to %d.", status, \
                "astTest" #attr, astGetClass( this ), \
                axis + 1, this->ncoord ); \
\
/* Assign the result value. */ \
   } else { \
      result = (assign); \
   } \
\
/* Check for errors and clear the result if necessary. */ \
   if ( !astOK ) result = 0; \
\
/* Return the result. */ \
   return result; \
} \
/* External interface. */ \
/* ------------------- */ \
int astTest##attr##_( AstPointSet *this, int axis, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return 0; \
\
/* Invoke the required method via the virtual function table. */ \
   return (**astMEMBER(this,PointSet,Test##attr))( this, axis, status ); \
}

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "globals.h"             /* Thread-safe global data access */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* Interface definition for this class */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <stdarg.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

/* Module Variables. */
/* ================= */

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* This static variable is used to hold an IEEE 754 quiet double precision
   Nan value. */
static double ast_nan;

/* This static variable is used to hold an IEEE 754 quiet single precision
   Nan value. */
static float ast_nanf;

/* Enable or disable the astReplaceNan method. */
static int replace_nan = -1;

/* Pointers to parent class methods which are extended by this class. */
static const char *(* parent_getattrib)( AstObject *, const char *, int * );
static int (* parent_testattrib)( AstObject *, const char *, int * );
static void (* parent_clearattrib)( AstObject *, const char *, int * );
static void (* parent_setattrib)( AstObject *, const char *, int * );
static int (* parent_equal)( AstObject *, AstObject *, int * );
static int (* parent_getobjsize)( AstObject *, int * );

/* Define macros for accessing each item of thread specific global data. */
#ifdef THREAD_SAFE

/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0; \
   globals->GetAttrib_Buff[ 0 ] = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(PointSet)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(PointSet,Class_Init)
#define class_vtab astGLOBAL(PointSet,Class_Vtab)
#define getattrib_buff astGLOBAL(PointSet,GetAttrib_Buff)

static pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
#define LOCK_MUTEX1 pthread_mutex_lock( &mutex1 );
#define UNLOCK_MUTEX1 pthread_mutex_unlock( &mutex1 );

/* If thread safety is not needed, declare and initialise globals at static
   variables. */
#else

static char getattrib_buff[ 101 ];


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstPointSetVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#define LOCK_MUTEX1
#define UNLOCK_MUTEX1

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstPointSet *astPointSetId_( int, int, const char *, int *, ...);

/* Prototypes for Private Member Functions. */
/* ======================================== */
static const char *GetAttrib( AstObject *, const char *, int * );
static double **GetPoints( AstPointSet *, int * );
static int Equal( AstObject *, AstObject *, int * );
static int GetNcoord( const AstPointSet *, int * );
static int GetNpoint( const AstPointSet *, int * );
static int GetObjSize( AstObject *, int * );
static int ReplaceNaN( AstPointSet *, int * );
static int TestAttrib( AstObject *, const char *, int * );
static AstPointSet *AppendPoints( AstPointSet *, AstPointSet *, int * );
static void BndPoints( AstPointSet *, double *, double *, int * );
static void CheckPerm( AstPointSet *, const int *, const char *, int * );
static void ClearAttrib( AstObject *, const char *, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void PermPoints( AstPointSet *, int, const int[], int * );
static void SetAttrib( AstObject *, const char *, int * );
static void SetPoints( AstPointSet *, double **, int * );
static void SetNpoint( AstPointSet *, int, int * );
static void SetSubPoints( AstPointSet *, int, int, AstPointSet *, int * );

static double GetPointAccuracy( AstPointSet *, int, int * );
static int TestPointAccuracy( AstPointSet *, int, int * );
static void ClearPointAccuracy( AstPointSet *, int, int * );
static void SetPointAccuracy( AstPointSet *, int, double, int * );

/* Member functions. */
/* ================= */
static AstPointSet *AppendPoints( AstPointSet *this, AstPointSet *that, int *status ) {
/*
*+
*  Name:
*     astAppendPoints

*  Purpose:
*     Append one PointSet to another.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "pointset.h"
*     AstPointSet *astAppendPoints( AstPointSet *this, AstPointSet *that )

*  Class Membership:
*     PointSet method.

*  Description:
*     This function creates a new PointSet containing all the points in
*     "this" followed by all the points in "that".

*  Parameters:
*     this
*        Pointer to the first PointSet.
*     that
*        Pointer to the second PointSet.

*  Returned Value:
*     Pointer to the new PointSet.

*  Notes:
*     - Axis accuracies are copied from "this".
*     - The Ncoord attribute of the two PointSets must match.
*     - NULL will be returned if an error has already occurred, or if this
*     function should fail for any reason.
*-
*/

/* Local Variables: */
   AstPointSet *result;
   double **ptr;
   double **ptr1;
   double **ptr2;
   int ic;
   int n1;
   int n2;
   int ncoord;
   size_t nb2;
   size_t nb1;

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Check the two PointSets have the same Ncoord value. */
   ncoord = astGetNcoord( this );
   if( ncoord != astGetNcoord( that ) ) {
      astError( AST__NPTIN, "astAppendPoints(%s): Number of coordinates "
                "per point differ in the two supplied PointSets.", status,
                astGetClass( this ) );

/* Calculate the new size for the PointSet. */
   } else {
      n1 = astGetNpoint( this );
      n2 = astGetNpoint( that );

/* Create the new PointSet and get pointers to its data. */
      result = astPointSet( n1 + n2, ncoord, "", status );
      ptr1 = astGetPoints( this );
      ptr2 = astGetPoints( that );
      ptr = astGetPoints( result );
      if( astOK ) {

/* Copy the axis values for each coordinate in turn. */
         nb1 = sizeof( double )*(size_t) n1;
         nb2 = sizeof( double )*(size_t) n2;
         for( ic = 0; ic < ncoord; ic++ ) {
            memcpy( ptr[ ic ], ptr1[ ic ], nb1 );
            memcpy( ptr[ ic ] + n1, ptr2[ ic ], nb2 );
         }

/* Copy any axis accuracies from "this". */
         result->acc = this->acc ?
                  astStore( NULL, this->acc, sizeof( double )*(size_t) ncoord )
                  : NULL;
      }
   }

/* Annul the result if an error occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return the result. */
   return result;
}

static void BndPoints( AstPointSet *this, double *lbnd, double *ubnd, int *status ) {
/*
*+
*  Name:
*     astBndPoints

*  Purpose:
*     Find the axis bounds of the points in a PointSet.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "pointset.h"
*     void astBndPoints( AstPointSet *this, double *lbnd, double *ubnd )

*  Class Membership:
*     PointSet method.

*  Description:
*     This function returns the lower and upper limits of the axis values
*     of the points in a PointSet.

*  Parameters:
*     this
*        Pointer to the first PointSet.
*     lbnd
*        Pointer to an array in which to return the lowest value for
*        each coordinate. The length of the array should equal the number
*        returned by astGetNcoord.
*     ubnd
*        Pointer to an array in which to return the highest value for
*        each coordinate. The length of the array should equal the number
*        returned by astGetNcoord.

*-
*/

/* Local Variables: */
   double **ptr;
   double *p;
   double lb;
   double ub;
   int ic;
   int ip;
   int nc;
   int np;

/* Check the global error status. */
   if ( !astOK ) return;

/* Get pointers to the PointSet data, the number of axes adn the number
   of points. */
   ptr = astGetPoints( this );
   nc = astGetNcoord( this );
   np = astGetNpoint( this );

/* Check the pointers can be used safely. */
   if( astOK ) {

/* Loop round each axis. */
      for( ic = 0; ic < nc; ic++ ) {

/* Initialise the bounds for this axis. */
         lb = AST__BAD;
         ub = AST__BAD;

/* Search for the first good point. Use it to initialise the bounds and
   break out of the loop. */
         p = ptr[ ic ];
         for( ip = 0; ip < np; ip++,p++ ) {
            if( *p != AST__BAD ) {
               lb = ub = *p;
               break;
            }
         }

/* Search through the remaining points. Update the bounds if the axis
   value is good. */
         for( ; ip < np; ip++,p++ ) {
            if( *p != AST__BAD ) {
               if( *p < lb ) {
                  lb = *p;
               } else if( *p > ub ) {
                  ub = *p;
               }
            }
         }

/* Store the returned bounds. */
         lbnd[ ic ] = lb;
         ubnd[ ic ] = ub;
      }
   }
}

static void CheckPerm( AstPointSet *this, const int *perm, const char *method, int *status ) {
/*
*+
*  Name:
*     astCheckPerm

*  Purpose:
*     Check that an array contains a valid permutation.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "pointset.h"
*     void astCheckPerm( AstPointSet *this, const int *perm, const char *method )

*  Class Membership:
*     PointSet method.

*  Description:
*     This function checks the validity of a permutation array that
*     will be used to permute the order of a PointSet's axes. If the
*     permutation specified by the array is not valid, an error is
*     reported and the global error status is set. Otherwise, the
*     function returns without further action.

*  Parameters:
*     this
*        Pointer to the PointSet.
*     perm
*        Pointer to an array of integers with the same number of
*        elements as there are axes in the PointSet. For each axis, the
*        corresponding integer gives the (zero based) axis index to be
*        used to identify the axis values for that axis (using the
*        un-permuted axis numbering). To be valid, the integers in
*        this array should therefore all lie in the range zero to
*        (ncoord-1) inclusive, where "ncoord" is the number of PointSet
*        axes, and each value should occur exactly once.
*     method
*        Pointer to a constant null-terminated character string
*        containing the name of the method that invoked this function
*        to validate a permutation array. This method name is used
*        solely for constructing error messages.

*  Notes:
*     - Error messages issued by this function refer to the external
*     (public) numbering system used for axes (which is one-based),
*     whereas zero-based axis indices are used internally.
*-
*/

/* Local Variables: */
   int *there;                   /* Pointer to temporary array */
   int coord;                     /* Loop counter for axes */
   int ncoord;                   /* Number of PointSet axes */
   int valid;                    /* Permutation array is valid? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Initialise. */
   valid = 1;

/* Obtain the number of PointSet axes and allocate a temporary array of
   integers with the same number of elements. */
   ncoord = astGetNcoord( this );
   there = astMalloc( sizeof( int ) * (size_t) ncoord );
   if ( astOK ) {

/* Initialise the temporary array to zero. */
      for ( coord = 0; coord < ncoord; coord++ ) there[ coord ] = 0;

/* Scan the permutation array, checking that each permuted axis index it
   contains is within the correct range. Note an error and quit checking
   if an invalid value is found. */
      for ( coord = 0; coord < ncoord; coord++ ) {
         if ( ( perm[ coord ] < 0 ) || ( perm[ coord ] >= ncoord ) ) {
            valid = 0;
            break;

/* Use the temporary array to count how many times each valid axis index
   occurs. */
	 } else {
            there[ perm[ coord ] ]++;
	 }
      }

/* If all the axis indices were within range, check to ensure that each value
   occurred only once. */
      if ( valid ) {
         for ( coord = 0; coord < ncoord; coord++ ) {

/* Note an error and quit checking if any value did not occur exactly once. */
            if ( there[ coord ] != 1 ) {
               valid = 0;
               break;
	    }
	 }
      }
   }

/* Free the temporary array. */
   there = astFree( there );

/* If an invalid permutation was detected and no other error has
   occurred, then report an error (note we convert to one-based axis
   numbering in the error message). */
   if ( !valid && astOK ) {
      astError( AST__PRMIN, "%s(%s): Invalid coordinate permutation array.", status,
                method, astGetClass( this ) );
      astError( AST__PRMIN, "Each coordinate index should lie in the range 1 to %d "
                "and should occur only once.", status, ncoord );
   }
}

static void ClearAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for a PointSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pointset.h"
*     void ClearAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     PointSet member function (over-rides the astClearAttrib
*     protected method inherited from the Object class).

*  Description:
*     This function clears the value of a specified attribute for a
*     PointSet, so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the PointSet.
*     attrib
*        Pointer to a null-terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstPointSet *this;            /* Pointer to the PointSet structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the PointSet structure. */
   this = (AstPointSet *) this_object;

/* Check the attribute name and clear the appropriate attribute. */

/* Test if the name matches any of the read-only attributes of this
   class. If it does, then report an error. */
   if ( !strcmp( attrib, "ncoord" ) ||
        !strcmp( attrib, "npoint" ) ) {
      astError( AST__NOWRT, "astClear: Invalid attempt to clear the \"%s\" "
                "value for a %s.", status, attrib, astGetClass( this ) );
      astError( AST__NOWRT, "This is a read-only attribute." , status);

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_clearattrib)( this_object, attrib, status );
   }
}

static int Equal( AstObject *this_object, AstObject *that_object, int *status ) {
/*
*  Name:
*     Equal

*  Purpose:
*     Test if two PointSets are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pointset.h"
*     int Equal( AstObject *this, AstObject *that, int *status )

*  Class Membership:
*     PointSet member function (over-rides the astEqual protected
*     method inherited from the Object class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two PointSets are equivalent.

*  Parameters:
*     this
*        Pointer to the first PointSet.
*     that
*        Pointer to the second PointSet.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the PointSets are equivalent, zero otherwise.

*  Notes:
*     - The two PointSets are considered equivalent if they have the same
*     number of points, the same number of axis values per point, and the
*     same axis values to within the absolute tolerance specified by the
*     Accuracy attribute of the two PointSets.
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local constants: */
#define SMALL sqrt(DBL_MIN)

/* Local Variables: */
   AstPointSet *that;         /* Pointer to the second PointSet structure */
   AstPointSet *this;         /* Pointer to the first PointSet structure */
   double **ptr_that;         /* Pointer to axis values in second PointSet */
   double **ptr_this;         /* Pointer to axis values in first PointSet */
   double *p_that;            /* Pointer to next axis value in second PointSet */
   double *p_this;            /* Pointer to next axis value in first PointSet */
   double acc1;               /* Absolute accuracy for 1st PointSet axis value */
   double acc2;               /* Absolute accuracy for 2nd PointSet axis value */
   double acc;                /* Combined absolute accuracy */
   double acc_that;           /* PointAccuracy attribute for 2nd PointSet */
   double acc_this;           /* PointAccuracy attribute for 1st PointSet */
   int ic;                    /* Axis index */
   int ip;                    /* Point index */
   int nc;                    /* No. of axis values per point */
   int np;                    /* No. of points in each PointSet */
   int result;                /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Invoke the Equal method inherited from the parent Object class. This checks
   that the Objects are both of the same class (amongst other things). */
   if( (*parent_equal)( this_object, that_object, status ) ) {

/* Obtain pointers to the two PointSet structures. */
      this = (AstPointSet *) this_object;
      that = (AstPointSet *) that_object;

/* Check the number of points and the number of axis values per point are
   equal in the two PointSets. */
      np = astGetNpoint( this );
      nc = astGetNcoord( this );
      if( np == astGetNpoint( that ) && nc == astGetNcoord( that ) ) {

/* Get pointers to the axis values.*/
         ptr_this = astGetPoints( this );
         ptr_that = astGetPoints( that );
         if( astOK ) {

/* Assume the PointSets are equal until proven otherwise. */
            result = 1;

/* Loop round each axis, until we find a difference. */
            for( ic = 0; ic < nc && result; ic++ ) {

/* Get pointers to the next value for this axis. */
               p_this = ptr_this[ ic ];
               p_that = ptr_that[ ic ];

/* Get the absolute accuracies for this axis. The default value for this
   attribute is AST__BAD. */
               acc_this = astGetPointAccuracy( this, ic );
               acc_that = astGetPointAccuracy( that, ic );

/* If both accuracies are available, combine them in quadrature. */
               if( acc_this != AST__BAD && acc_that != AST__BAD ) {
                  acc = sqrt( acc_this*acc_this + acc_that*acc_that );

/* Loop round all points on this axis */
                  for( ip = 0; ip < np; ip++, p_this++, p_that++ ){

/* If either value is bad we do not need to compare values. */
                     if( *p_this == AST__BAD || *p_that == AST__BAD ) {

/* If one value is bad and one is good, they differ, so break. If both
   values are bad they are equal so we continue. */
                        if( *p_this != AST__BAD || *p_that != AST__BAD ) {
                           result = 0;
                           break;
                        }

/* Otherwise (if both axis values are good), compare axis values, and break if
   they differ by more than the absolute accuracy. */
                     } else if( fabs( *p_this - *p_that ) > acc ) {
                        result = 0;
                        break;
                     }
                  }

/* If either accuracy is unavailable, we use a default relative accuracy. */
               } else {

/* Loop round all points on this axis */
                  for( ip = 0; ip < np; ip++, p_this++, p_that++ ){

/* If either value is bad we do not need to compare values. */
                     if( *p_this == AST__BAD || *p_that == AST__BAD ) {

/* If one value is bad and one is good, they differ, so break. If both
   values are bad they are equal so we continue. */
                        if( *p_this != AST__BAD || *p_that != AST__BAD ) {
                           result = 0;
                           break;
                        }

/* Otherwise (if both axis values are good), find the absolute error for
   both values. */
                     } else {

                        if( acc_this == AST__BAD ) {
                           acc1 = fabs(*p_this)*DBL_EPSILON;
                           if( acc1 < SMALL ) acc1 = SMALL;
                           acc1 *= 1.0E3;
                        } else {
                           acc1 = acc_this;
                        }

                        if( acc_that == AST__BAD ) {
                           acc2 = fabs(*p_that)*DBL_EPSILON;
                           if( acc2 < SMALL ) acc2 = SMALL;
                           acc2 *= 1.0E3;
                        } else {
                           acc2 = acc_that;
                        }

/* Combine them in quadrature. */
                        acc = sqrt( acc1*acc1 + acc2*acc2 );

/* Compare axis values, and break if they differ by more than the
   absolute accuracy. */
                        if( fabs( *p_this - *p_that ) > acc ) {
                           result = 0;
                           break;
                        }
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
#undef SMALL
}

static const char *GetAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     GetAttrib

*  Purpose:
*     Get the value of a specified attribute for a PointSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pointset.h"
*     const char *GetAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     PointSet member function (over-rides the protected astGetAttrib
*     method inherited from the Object class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for a PointSet, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the PointSet.
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
*     within the PointSet, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the PointSet. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   astDECLARE_GLOBALS           /* Pointer to thread-specific global data */
   AstPointSet *this;            /* Pointer to the PointSet structure */
   const char *result;           /* Pointer value to return */
   int ncoord;                   /* Ncoord attribute value */
   int npoint;                   /* Npoint attribute value */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(this_object);

/* Obtain a pointer to the PointSet structure. */
   this = (AstPointSet *) this_object;

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "getattrib_buff" as a null-terminated string in an appropriate
   format.  Set "result" to point at the result string. */

/* Ncoord. */
/* ------- */
   if ( !strcmp( attrib, "ncoord" ) ) {
      ncoord = astGetNcoord( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", ncoord );
         result = getattrib_buff;
      }

/* Npoint. */
/* ------- */
   } else if ( !strcmp( attrib, "npoint" ) ) {
      npoint = astGetNpoint( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", npoint );
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

static int GetObjSize( AstObject *this_object, int *status ) {
/*
*  Name:
*     GetObjSize

*  Purpose:
*     Return the in-memory size of an Object.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pointset.h"
*     int GetObjSize( AstObject *this, int *status )

*  Class Membership:
*     PointSet member function (over-rides the astGetObjSize protected
*     method inherited from the Object class).

*  Description:
*     This function returns the in-memory size of the supplied PointSet,
*     in bytes.

*  Parameters:
*     this
*        Pointer to the Object.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The Object size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstPointSet *this;         /* Pointer to PointSet structure */
   int result;                /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointers to the PointSet structure. */
   this = (AstPointSet *) this_object;

/* Invoke the GetObjSize method inherited from the parent class, and then
   add on any components of the class structure defined by thsi class
   which are stored in dynamically allocated memory. */
   result = (*parent_getobjsize)( this_object, status );

   result += astTSizeOf( this->ptr );
   result += astTSizeOf( this->values );
   result += astTSizeOf( this->acc );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static int GetNcoord( const AstPointSet *this, int *status ) {
/*
*+
*  Name:
*     astGetNcoord

*  Purpose:
*     Get the number of coordinate values per point from a PointSet.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "pointset.h"
*     int astGetNcoord( const AstPointSet *this )

*  Class Membership:
*     PointSet method.

*  Description:
*     This function returns the number of coordinate values per point (1 or
*     more) for a PointSet.

*  Parameters:
*     this
*        Pointer to the PointSet.

*  Returned Value:
*     The number of coordinate values per point.

*  Notes:
*     -  A value of zero is returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Return the number of coordinate values. */
   return this->ncoord;
}

static int GetNpoint( const AstPointSet *this, int *status ) {
/*
*+
*  Name:
*     astGetNpoint

*  Purpose:
*     Get the number of points in a PointSet.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "pointset.h"
*     int astGetNpoint( const AstPointSet *this )

*  Class Membership:
*     PointSet method.

*  Description:
*     This function returns the number of points (1 or more) in a PointSet.

*  Parameters:
*     this
*        Pointer to the PointSet.

*  Returned Value:
*     The number of points.

*  Notes:
*     -  A value of zero is returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Return the number of points. */
   return this->npoint;
}

static double **GetPoints( AstPointSet *this, int *status ) {
/*
*+
*  Name:
*     astGetPoints

*  Purpose:
*     Get a pointer for the coordinate values associated with a PointSet.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "pointset.h"
*     double **astGetPoints( AstPointSet *this )

*  Class Membership:
*     PointSet method.

*  Description:
*     This function returns a pointer which grants access to the coordinate
*     values associated with a PointSet. If the PointSet has previously had
*     coordinate values associated with it, this pointer will identify these
*     values. Otherwise, it will point at a newly-allocated region of memory
*     (associated with the PointSet) in which new coordinate values may be
*     stored.

*  Parameters:
*     this
*        Pointer to the PointSet.

*  Returned Value:
*     A pointer to an array of type double* with ncoord elements (where ncoord
*     is the number of coordinate values per point). Each element of this array
*     points at an array of double, of size npoint (where npoint is the number
*     of points in the PointSet), containing the values of that coordinate for
*     each point in the set. Hence, the value of the i'th coordinate for the
*     j'th point (where i and j are counted from zero) is given by ptr[i][j]
*     where ptr is the returned pointer value.

*  Notes:
*     -  The returned pointer points at an array of pointers allocated
*     internally within the PointSet. The values in this array may be changed
*     by the caller, who is reponsible for ensuring that they continue to
*     point at valid arrays of coordinate values.
*     -  No attempt should be made to de-allocate memory allocated by a
*     PointSet to store coordinate values or pointers to them. This memory
*     will be freed when the PointSet is deleted.
*     -  No count is kept of the number of pointers issued for the PointSet
*     coordinate values. The caller must keep track of these.
*     -  A NULL pointer is returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   int i;                        /* Loop counter for coordinates */
   int nval;                     /* Number of values to be stored */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* If the PointSet has an existing array of pointers (which point at coordinate
   values), we will simply return a pointer to it. Otherwise, we must allocate
   space to hold new coordinate values. */
   if( !this->ptr ) {

/* Determine the number of coordinate values to be stored and allocate memory
   to hold them, storing the pointer to this values array in the PointSet
   structure. */
      nval = this->npoint * this->ncoord;
      this->values = (double *) astMalloc( sizeof( double ) * (size_t) nval );

#ifdef DEBUG
      for( i = 0; i < nval; i++ ) this->values[ i ] = 0.0;
#endif

/* If OK, also allocate memory for the array of pointers into this values
   array, storing a pointer to this pointer array in the PointSet structure. */
      if ( astOK ) {
         this->ptr = (double **) astMalloc( sizeof( double * )
                                            * (size_t) this->ncoord );

/* If OK, initialise the pointer array to point into the values array. */
         if ( astOK ) {
            for ( i = 0; i < this->ncoord; i++ ) {
               this->ptr[ i ] = this->values + ( i * this->npoint );
            }

/* If we failed to allocate the pointer array, then free the values array. */
         } else {
            this->values = (double *) astFree( (void *) this->values );
         }
      }

#ifdef DEBUG
   } else {

/* Check for bad values */
      if( this->values ) {
         int i, j;
         for( i = 0; astOK && i < this->ncoord; i++ ) {
            for( j = 0; j < this->npoint; j++ ) {
               if( !finite( (this->ptr)[ i ][ j ] ) ) {
                  astError( AST__INTER, "astGetPoints(PointSet): Non-finite "
                            "axis value returned.", status);
                  break;
               }
            }
         }
      }

#endif
   }

/* Return the required pointer. */
   return this->ptr;
}

void astInitPointSetVtab_(  AstPointSetVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitPointSetVtab

*  Purpose:
*     Initialise a virtual function table for a PointSet.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "pointset.h"
*     void astInitPointSetVtab( AstPointSetVtab *vtab, const char *name )

*  Class Membership:
*     PointSet vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the PointSet class.

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
   AstObjectVtab *object;        /* Pointer to Object component of Vtab */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   const char *envvar;           /* Pointer to environment variable value */
   size_t i;                     /* Index of next byte in NaN value */
   unsigned char *p;             /* Pointer to next byte in NaN value */

/* Check the local error status. */
   if ( !astOK ) return;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitObjectVtab( (AstObjectVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsAPointSet) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstObjectVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that
   provide virtual methods for this class. */
   vtab->AppendPoints = AppendPoints;
   vtab->BndPoints = BndPoints;
   vtab->GetNcoord = GetNcoord;
   vtab->GetNpoint = GetNpoint;
   vtab->GetPoints = GetPoints;
   vtab->PermPoints = PermPoints;
   vtab->ReplaceNaN = ReplaceNaN;
   vtab->SetPoints = SetPoints;
   vtab->SetNpoint = SetNpoint;
   vtab->SetSubPoints = SetSubPoints;

   vtab->GetPointAccuracy = GetPointAccuracy;
   vtab->SetPointAccuracy = SetPointAccuracy;
   vtab->TestPointAccuracy = TestPointAccuracy;
   vtab->ClearPointAccuracy = ClearPointAccuracy;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;

   parent_clearattrib = object->ClearAttrib;
   object->ClearAttrib = ClearAttrib;
   parent_getattrib = object->GetAttrib;
   object->GetAttrib = GetAttrib;
   parent_setattrib = object->SetAttrib;
   object->SetAttrib = SetAttrib;
   parent_testattrib = object->TestAttrib;
   object->TestAttrib = TestAttrib;
   parent_equal = object->Equal;
   object->Equal = Equal;
   parent_getobjsize = object->GetObjSize;
   object->GetObjSize = GetObjSize;

/* Declare the copy constructor, destructor and class dump function. */
   astSetCopy( vtab, Copy );
   astSetDelete( vtab, Delete );
   astSetDump( vtab, Dump, "PointSet", "Container for a set of points" );

/* Calculate single and double precision NaN values and store in static
   module variables. Setting all bits to 1 produces a quiet NaN. */
   LOCK_MUTEX1
   p = (unsigned char *) &ast_nan;
   for( i = 0; i < sizeof( ast_nan ); i++ ) *(p++) = 255;
   p = (unsigned char *) &ast_nanf;
   for( i = 0; i < sizeof( ast_nanf ); i++ ) *(p++) = 255;

/* See what  action the astReplaceNaN method should perform. This
   is determined by the value of the AST_REPLACE_NAN environment
   variable. Not set = do not check for NaNs, "1" = replace NaNs with
   AST__BAD silently, anything else = report an error if any NaNs are
   encountered. */
   if( replace_nan == -1 ) {
      envvar = getenv( "AST_REPLACE_NAN" );
      if( !envvar ) {
         replace_nan = IGNORE_NANS;
      } else if( !strcmp( envvar, "1" ) ) {
         replace_nan = REPLACE_NANS;
      } else {
         replace_nan = REPORT_NANS;
      }
   }
   UNLOCK_MUTEX1

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
}

double astCheckNaN_( double value ) {
/*
*+
*  Name:
*     astCheckNaN

*  Purpose:
*     Substitute a NaN for a supplied value if the supplied value is
*     AST__NAN.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "pointset.h"
*     double astCheckNaN( double value )

*  Class Membership:
*     PointSet method.

*  Description:
*     If the supplied double is AST__NAN then this function returns an
*     IEEE double precision NaN value. Otherwise it returns the supplied
*     value.

*  Parameters:
*     valuethis
*        The value to check.

*  Returned Value:
*     The suppleid value, or NaN.

*  Notes:
*     - This function does not check the inherited status.

*-
*/
   return ( value == AST__NAN ) ? ast_nan : value;
}

float astCheckNaNF_( float value ) {
/*
*+
*  Name:
*     astCheckNaNF

*  Purpose:
*     Substitute a NaN for a supplied value if the supplied value is
*     AST__NANF.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "pointset.h"
*     float astCheckNaNF( float value )

*  Class Membership:
*     PointSet method.

*  Description:
*     If the supplied float is AST__NANF then this function returns an
*     IEEE single precision NaN value. Otherwise it returns the supplied
*     value.

*  Parameters:
*     valuethis
*        The value to check.

*  Returned Value:
*     The suppleid value, or NaN.

*  Notes:
*     - This function does not check the inherited status.

*-
*/
   return ( value == AST__NANF ) ? ast_nanf : value;
}

static void PermPoints( AstPointSet *this, int forward, const int perm[], int *status ) {
/*
*+
*  Name:
*     astPermPoints

*  Purpose:
*     Permute the order of a PointSet's axes.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "pointset.h"
*     void astPermPoints( AstPointSet *this, int forward, const int perm[] )

*  Class Membership:
*     PointSet method.

*  Description:
*     This function permutes the order in which a PointSet's axes occur.

*  Parameters:
*     this
*        Pointer to the PointSet.
*     forward
*        The direction in which the permutation is to be applied. This
*        controls the use of the "perm" arrays. If a non-zero value is
*        given, then the indices into the "perm" array correspond to the
*        indices of the coordinates in the returned PointSet, and the
*        values stored in the "perm" array correspond to the indices of
*        the coordinates in the supplied PointSet. If a zero value is
*        given, then the indices into the "perm" array correspond to the
*        indices of the coordinates in the supplied PointSet, and the
*        values stored in the "perm" array correspond to the indices of
*        the coordinates in the returnedPointSet.
*     perm
*        An array of int (with one element for each axis of the PointSet)
*        which lists the axes in their new order. How this array is use
*        depends on the value supplied for "forward".

*  Notes:
*     - Only genuine permutations of the axis order are permitted, so
*     each axis must be referenced exactly once in the "perm" array.
*     - If more than one axis permutation is applied to a PointSet, the
*     effects are cumulative.
*-
*/

/* Local Variables: */
   double **old;                 /* Pointer to copy of old pointer array */
   int coord;                    /* Loop counter for axes */
   int ncoord;                   /* Number of axes */

/* Check the global error status. Return without action if no data is
   associated with the PointSet. */
   if ( !astOK || !this->ptr ) return;

/* Validate the permutation array, to check that it describes a genuine
   permutation. */
   CheckPerm( this, perm, "astPermPoints", status );

/* Obtain the number of PointSet axes. */
   ncoord = astGetNcoord( this );

/* Allocate memory and use it to store a copy of the old pointers array for
   the PointSet. */
   old = astStore( NULL, this->ptr, sizeof( double * ) * (size_t) ncoord );

/* Apply the new axis permutation cumulatively to the old one and store the
   result in the PointSet. */
   if ( astOK ) {
      if( forward ) {
         for ( coord = 0; coord < ncoord; coord++ ) {
            this->ptr[ coord ] = old[ perm[ coord ] ];
         }
      } else {
         for ( coord = 0; coord < ncoord; coord++ ) {
            this->ptr[ perm[ coord ] ] = old[ coord ];
         }
      }
   }

/* Free the temporary copy of the old array. */
   old = astFree( old );
}

static int ReplaceNaN( AstPointSet *this, int *status ) {
/*
*+
*  Name:
*     astReplaceNaN

*  Purpose:
*     Check for NaNs in a PointSet.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "pointset.h"
*     int astReplaceNaNs( AstPointSet *this )

*  Class Membership:
*     PointSet method.

*  Description:
*     The behaviour of this method is determined by the AST_REPLACE_NAN
*     environment variable. If AST_REPLACE_NAN is undefined, then the
*     method returns without action. If AST_REPLACE_NAN is "1", then the
*     method examines the supplied PointSet and replaces any NaN values
*     with AST__BAD. If AST_REPLACE_NAN has any other value, any NaNs in
*     the supplied PointSet are still replaced, but in addition an error
*     is reported.

*  Parameters:
*     this
*        Pointer to the PointSet.

*  Returned Value:
*     Non-zero if any NaN values were found in the PointSet. If AST_REPLACE_NAN
*     is undefined, then zero is always returned.

*  Notes:
*    The value of the AST_REPLACE_NAN environment variable is obtained
*    only once, when the PointSet virtual function table is first
*    created. This value is saved for all subsequent invocations of this
*    method.

*-
*/

/* Local Variables: */
   double **ptr;
   double *p0;
   double *p;
   int ic;
   int nc;
   int np;
   int result;

/* Initialise */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Unless the AST_REPALCE_NAN environment variable is undefined, check
   for NaNs in the supplied PointSet, replacing any with AST__BAD. */
   if( replace_nan != IGNORE_NANS ) {
      ptr = astGetPoints( this );
      if( ptr ) {
         nc = astGetNcoord( this );
         np = astGetNpoint( this );
         for( ic = 0; ic < nc; ic++ ) {
            p = ptr[ ic ];
            p0 = p + np;
            for( ; p < p0; p++ ) {
               if( astISNAN(*p) ) {
                  result = 1;
                  *p = AST__BAD;
               }
            }
         }

/* If any NaNs were found, and AST_REPLACE_NAN is not set to "1", report
   an error. */
         if( result && replace_nan == REPORT_NANS ) {
            astError( AST__ISNAN, "astReplaceNan(%s): One or more NaN values "
                      "were encountered within an AST PointSet.", status,
                      astGetClass( this ) );
         }
      }
   }

/* Return the result. */
   return result;
}

static void SetAttrib( AstObject *this_object, const char *setting, int *status ) {
/*
*  Name:
*     SetAttrib

*  Purpose:
*     Set an attribute value for a PointSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pointset.h"
*     void SetAttrib( AstObject *this, const char *setting )

*  Class Membership:
*     PointSet member function (over-rides the astSetAttrib protected
*     method inherited from the Object class).

*  Description:
*     This function assigns an attribute value for a PointSet, the
*     attribute and its value being specified by means of a string of
*     the form:
*
*        "attribute= value "
*
*     Here, "attribute" specifies the attribute name and should be in
*     lower case with no white space present. The value to the right
*     of the "=" should be a suitable textual representation of the
*     value to be assigned and this will be interpreted according to
*     the attribute's data type.  White space surrounding the value is
*     only significant for string attributes.

*  Parameters:
*     this
*        Pointer to the PointSet.
*     setting
*        Pointer to a null-terminated string specifying the new
*        attribute value.
*/

/* Local Variables: */
   AstPointSet *this;            /* Pointer to the PointSet structure */
   int len;                      /* Length of setting string */
   int nc;                       /* Number of characters read by astSscanf */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the PointSet structure. */
   this = (AstPointSet *) this_object;

/* Obtain the length of the setting string. */
   len = (int) strlen( setting );

/* Test for each recognised attribute in turn, using "astSscanf" to parse
   the setting string and extract the attribute value (or an offset to
   it in the case of string values). In each case, use the value set
   in "nc" to check that the entire string was matched. Once a value
   has been obtained, use the appropriate method to set it. */

/* Define a macro to see if the setting string matches any of the
   read-only attributes of this class. */
#define MATCH(attrib) \
        ( nc = 0, ( 0 == astSscanf( setting, attrib "=%*[^\n]%n", &nc ) ) && \
                  ( nc >= len ) )

/* Use this macro to report an error if a read-only attribute has been
   specified. */
   if ( MATCH( "ncoord" ) ||
        MATCH( "npoint" ) ) {
      astError( AST__NOWRT, "astSet: The setting \"%s\" is invalid for a %s.", status,
                setting, astGetClass( this ) );
      astError( AST__NOWRT, "This is a read-only attribute." , status);

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_setattrib)( this_object, setting, status );
   }

/* Undefine macros local to this function. */
#undef MATCH
}

static void SetNpoint( AstPointSet *this, int npoint, int *status ) {
/*
*+
*  Name:
*     astSetNpoint

*  Purpose:
*     Reduce the number of points in a PointSet.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "pointset.h"
*     void astSetNpoint( AstPointSet *this, int npoint )

*  Class Membership:
*     PointSet method.

*  Description:
*     This function reduces the number of points stored in a PointSet.
*     Points with indices beyond the new size will be discarded.

*  Parameters:
*     this
*        Pointer to the PointSet.
*     npoint
*        The new value for the number of points in the PointSet. Must be
*        less than or equal to the original size of the PointSet, and
*        greater than zero.
*-
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Check the new size is valid. */
   if( npoint < 1 || npoint > this->npoint ) {
      astError( AST__NPTIN, "astSetNpoint(%s): Number of points (%d) is "
                "not valid.", status, astGetClass( this ), npoint );
      astError( AST__NPTIN, "Should be in the range 1 to %d.", status, this->npoint );

/* Store the new size. */
   } else {
      this->npoint = npoint;
   }
}

static void SetPoints( AstPointSet *this, double **ptr, int *status ) {
/*
*+
*  Name:
*     astSetPoints

*  Purpose:
*     Associate coordinate values with a PointSet.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "pointset.h"
*     void astSetPoints( AstPointSet *this, double **ptr )

*  Class Membership:
*     PointSet method.

*  Description:
*     This function associates coordinate values with a PointSet by storing an
*     array of pointers to the values within the PointSet object. A pointer to
*     this pointer array will later be returned when astGetPoints is used to
*     locate the coordinate values. If values are already associated with the
*     PointSet, the array of pointers to them is over-written by the new values
*     (any internally allocated memory holding the actual coordinate values
*     first being freed).

*  Parameters:
*     this
*        Pointer to the PointSet.
*     ptr
*        Pointer to an array of type double* with ncoord elements (where ncoord
*        is the number of coordinate values per point in the PointSet). Each
*        element of this array should point at an array of double with npoint
*        elements (where npoint is the number of points in the PointSet),
*        containing the values of that coordinate for each point in the set.
*        Hence, the value of the i'th coordinate for the j'th point (where i
*        and j are counted from zero) should be given by ptr[i][j].

*  Returned Value:
*     void

*  Notes:
*     -  It is the caller's responsibility to ensure that the pointer supplied
*     points at a valid array of pointers that point at arrays of coordinate
*     values. This is only superficially validated by this function, which then
*     simply stores a copy of the supplied array of pointers for later use.
*     The caller must also manage any allocation (and freeing) of memory for
*     these coordinate values.
*     -  This functon makes a copy of the array of pointers supplied, but does
*     not copy the coordinate values they point at. If a PointSet containing a
*     copy of the coordinate values is required, internal memory should be
*     allocated within the PointSet by calling astGetPoints before storing any
*     pointer, and then copying the values into this memory. Alternatively,
*     using astCopy to produce a deep copy of a PointSet will also copy the
*     coordinate values.
*     -  A NULL pointer may be supplied as the "ptr" argument, in which case
*     any previously stored array of pointers will be cancelled (and internal
*     memory freed if necessary) and subsequent use of astGetPoints will then
*     cause memory to be allocated internally by the PointSet to hold new
*     values.
*-
*/

/* Local Variables: */
   int i;                        /* Loop counter for coordinates */

/* Check the global error status. */
   if ( !astOK ) return;

/* If the pointer supplied is not NULL, inspect each pointer in the array it
   points at to check that none of these are NULL. This validates (to some
   extent) the caller's data structure. Report an error and quit checking if a
   NULL pointer is found. */
   if ( ptr ) {
      for ( i = 0; i < this->ncoord; i++ ) {
         if ( !ptr[ i ] ) {
            astError( AST__PDSIN, "astSetPoints(%s): Invalid NULL pointer in "
                      "element %d of array of pointers to coordinate values.", status,
                      astGetClass( this ), i );
            break;
         }
      }
   }

/* Do not carry on if the data structure is obviously invalid. */
   if ( astOK ) {

/* Free any memory previously allocated to store coordinate values. */
      this->values = (double *) astFree( (void *) this->values );

/* If a new array of pointers has been provided, (re)allocate memory and store
   a copy of the array in it, saving a pointer to this copy in the PointSet
   structure. */
      if ( ptr ) {
         this->ptr = (double **) astStore( (void *) this->ptr,
                                           (const void *) ptr,
                                           sizeof( double * )
                                           * (size_t) this->ncoord );

/* If no pointer array was provided, free the previous one (if any). */
      } else {
         this->ptr = (double **) astFree( (void *) this->ptr );
      }
   }
}

static void SetSubPoints( AstPointSet *point1, int point, int coord,
                          AstPointSet *point2, int *status ) {
/*
*+
*  Name:
*     astSetSubPoints

*  Purpose:
*     Associate a subset of one PointSet with another PointSet.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "pointset.h"
*     void astSetSubPoints( AstPointSet *point1, int point, int coord,
*                           AstPointSet *point2 )

*  Class Membership:
*     PointSet method.

*  Description:
*     This function selects a subset of the coordinate values associated with
*     one PointSet and associates them with another PointSet. The second
*     PointSet may then be used to access the subset. Any previous coordinate
*     value association with the second PointSet is replaced.

*  Parameters:
*     point1
*        Pointer to the first PointSet, from which a subset is to be selected.
*     point
*        The index of the first point (counting from zero) which is to appear
*        in the subset (the number of points is determined by the size of the
*        second PointSet).
*     coord
*        The index of the first coordinate (counting from zero) which is to
*        appear in the subset (the number of coordinates is determined by the
*        size of the second PointSet).
*     point2
*        Second PointSet, with which the subset of coordinate values is to be
*        associated.

*  Returned Value:
*     void

*  Notes:
*     -  The range of points and coordinates selected must lie entirely within
*     the first PointSet.
*     -  This function does not make a copy of the coordinate values, but
*     merely stores pointers to the required subset of values associated with
*     the first PointSet. If a PointSet containing a copy of the subset's
*     coordinate values is required, then astCopy should be used to make a
*     deep copy from the second PointSet.
*     -  If the first PointSet does not yet have coordinate values associated
*     with it, then space will be allocated within it to hold values (so that
*     the second PointSet has somewhere to point at).
*-
*/

/* Local Variables: */
   double ** ptr2;               /* Pointer to new pointer array */
   double **ptr1;                /* Pointer to original pointer array */
   int i;                        /* Loop counter for coordinates */
   int ncoord1;                  /* Number of coordinates in first PointSet */
   int ncoord2;                  /* Number of coordinates in second PointSet */
   int npoint1;                  /* Number of points in first PointSet */
   int npoint2;                  /* Number of points in second PointSet */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain the sizes of both PointSets. */
   npoint1 = astGetNpoint( point1 );
   npoint2 = astGetNpoint( point2 );
   ncoord1 = astGetNcoord( point1 );
   ncoord2 = astGetNcoord( point2 );

/* Check if the range of points required lies within the first PointSet and
   report an error if it does not. */
   if ( astOK ) {
      if ( ( point < 0 ) || ( point + npoint2 > npoint1 ) ) {
         astError( AST__PTRNG, "astSetSubPoints(%s): Range of points in "
                   "output %s (%d to %d) lies outside the input %s extent "
                   "(0 to %d).", status,
                   astGetClass( point1 ), astGetClass( point2 ), point,
                   point + npoint2, astGetClass( point1 ), npoint1 );

/* Similarly check that the range of coordinates is valid. */
      } else if ( ( coord < 0 ) || ( coord + ncoord2 > ncoord1 ) ) {
         astError( AST__CORNG, "astSetSubPoints(%s): Range of coordinates in "
                   "output %s (%d to %d) lies outside the input %s extent "
                   "(0 to %d).", status,
                   astGetClass( point1 ), astGetClass( point2 ), coord,
                   coord + ncoord2, astGetClass( point1 ), ncoord1 );

/* Obtain a pointer for the coordinate values associated with the first
   PointSet (this will cause internal memory to be allocated if it is not
   yet associated with coordinate values). */
      } else {
         ptr1 = astGetPoints( point1 );

/* Allocate a temporary array to hold new pointer values. */
         ptr2 = (double **) astMalloc( sizeof( double * ) * (size_t) ncoord2 );

/* Initialise this pointer array to point at the required subset of coordinate
   values. */
         if ( astOK ) {
            for ( i = 0; i < ncoord2; i++ ) {
               ptr2[ i ] = ptr1[ i + coord ] + point;
            }

/* Associate the second PointSet with this new pointer array. This will free
   any internally allocated memory and replace any existing coordinate value
   association. */
            astSetPoints( point2, ptr2 );
	 }

/* Free the temporary pointer arry. */
         ptr2 = (double **) astFree( (void * ) ptr2 );
      }
   }
}

static int TestAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for a PointSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pointset.h"
*     int TestAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     PointSet member function (over-rides the astTestAttrib protected
*     method inherited from the Object class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate
*     whether a value has been set for one of a PointSet's attributes.

*  Parameters:
*     this
*        Pointer to the PointSet.
*     attrib
*        Pointer to a null-terminated string specifying the attribute
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
   AstPointSet *this;            /* Pointer to the PointSet structure */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the PointSet structure. */
   this = (AstPointSet *) this_object;

/* Check the attribute name and test the appropriate attribute. */

/* Test if the name matches any of the read-only attributes of this
   class. If it does, then return zero. */
   if ( !strcmp( attrib, "ncoord" ) ||
        !strcmp( attrib, "npoint" ) ) {
      result = 0;

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_testattrib)( this_object, attrib, status );
   }

/* Return the result, */
   return result;
}

/* Functions which access class attributes. */
/* ---------------------------------------- */

/*
*att+
*  Name:
*     PointAccuracy

*  Purpose:
*     The absolute accuracies for all points in the PointSet.

*  Type:
*     Protected attribute.

*  Synopsis:
*     Floating point.

*  Description:
*     This attribute holds the absolute accuracy for each axis in the
*     PointSet. It has a separate value for each axis. It is used when
*     comparing two PointSets using the protected astEqual method inherited
*     from the Object class. The default value for each axis is AST__BAD
*     which causes the a default accuracy of each axis value to be calculated
*     as  1.0E8*min( abs(axis value)*DBL_EPSILON, DBL_MIN ).

*  Applicability:
*     PointSet
*        All PointSets have this attribute.
*att-
*/
MAKE_CLEAR(PointAccuracy,acc,AST__BAD)
MAKE_GET(PointAccuracy,double,AST__BAD,this->acc?this->acc[axis]:AST__BAD)
MAKE_SET(PointAccuracy,double,acc,((value!=AST__BAD)?fabs(value):AST__BAD),AST__BAD)
MAKE_TEST(PointAccuracy,(this->acc?this->acc[axis]!=AST__BAD:0))

/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout, int *status ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for PointSet objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for PointSet objects.

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
*     -  This constructor makes a deep copy, including a copy of the coordinate
*     values (if any) associated with the input PointSet.
*/

/* Local Variables: */
   AstPointSet *in;              /* Pointer to input PointSet */
   AstPointSet *out;             /* Pointer to output PointSet */
   int i;                        /* Loop counter for coordinates */
   int nval;                     /* Number of values to store */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output PointSets. */
   in = (AstPointSet *) objin;
   out = (AstPointSet *) objout;

/* For safety, first clear any references to the input coordinate values from
   the output PointSet. */
   out->ptr = NULL;
   out->values = NULL;
   out->acc = NULL;

/* Copy axis accuracies. */
   if( in->acc ){
      out->acc = astStore( NULL, in->acc, sizeof( double )*(size_t) in->ncoord );
   }

/* If the input PointSet is associated with coordinate values, we must
   allocate memory in the output PointSet to hold a copy of them. */
   if ( in->ptr ) {

/* Determine the number of coordinate values to be stored and allocate memory
   to hold them, storing a pointer to this memory in the output PointSet. */
      nval = in->npoint * in->ncoord;
      out->values = (double *) astMalloc( sizeof( double ) * (size_t) nval );

/* If OK, also allocate memory for the array of pointers into this values
   array, storing a pointer to this pointer array in the output PointSet. */
      if ( astOK ) {
         out->ptr = (double **) astMalloc( sizeof( double * )
                                           * (size_t) in->ncoord );

/* If OK, initialise the new pointer array. */
         if ( astOK ) {
            for ( i = 0; i < in->ncoord; i++ ) {
               out->ptr[ i ] = out->values + ( i * in->npoint );
            }

/* If we failed to allocate the pointer array, then free the values array. */
         } else {
            out->values = (double *) astFree( (void *) out->values );
         }
      }

/* Copy the values for each coordinate from the input to the output. Use a
   memory copy to avoid floating point errors if the data are
   un-initialised. */
      if ( astOK ) {
         for ( i = 0; i < in->ncoord; i++ ) {
            (void) memcpy( (void *) out->ptr[ i ],
                           (const void *) in->ptr[ i ],
                           sizeof( double ) * (size_t) in->npoint );
         }
      }
   }
}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for PointSet objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for PointSet objects.

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
   AstPointSet *this;            /* Pointer to PointSet */

/* Obtain a pointer to the PointSet structure. */
   this = (AstPointSet *) obj;

/* Free memory holding axis accuracies. */
   this->acc = astFree( this->acc );

/* Free any pointer array and associated coordinate values array, */
   this->ptr = (double **) astFree( (void *) this->ptr );
   this->values = (double *) astFree( (void *) this->values );

/* Clear the remaining PointSet variables. */
   this->npoint = 0;
   this->ncoord = 0;
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for PointSet objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the PointSet class to an output Channel.

*  Parameters:
*     this
*        Pointer to the PointSet whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - It is not recommended that PointSets containing large numbers
*     of points be written out, as the coordinate data will be
*     formatted as text and this will not be very efficient.
*/

/* Local Constants: */
#define KEY_LEN 50               /* Maximum length of a keyword */

/* Local Variables: */
   AstPointSet *this;            /* Pointer to the PointSet structure */
   char key[ KEY_LEN + 1 ];      /* Buffer for keywords */
   int coord;                    /* Loop counter for coordinates */
   int i;                        /* Counter for coordinate values */
   int ival;                     /* Integer value */
   int makeComment;              /* Include a comment? */
   int point;                    /* Loop counter for points */
   int set;                      /* Attribute value set? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the PointSet structure. */
   this = (AstPointSet *) this_object;

/* Write out values representing the instance variables for the
   PointSet class.  Accompany these with appropriate comment strings,
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

/* Npoint. */
/* ------- */
   astWriteInt( channel, "Npoint", 1, 1, this->npoint,
                "Number of points" );

/* Ncoord. */
/* ------- */
   astWriteInt( channel, "Ncoord", 1, 1, this->ncoord,
                "Number of coordinates per point" );

/* Axis Acuracies. */
/* --------------- */
   for ( coord = 0; coord < this->ncoord; coord++ ) {
      if( astTestPointAccuracy( this, coord ) ) {
         (void) sprintf( key, "Acc%d", coord + 1 );
         astWriteDouble( channel, key, 1, 1, astGetPointAccuracy( this, coord ),
                         (coord == 0 ) ? "Axis accuracies..." : "" );
      }
   }

/* Coordinate data. */
/* ---------------- */
/* Write an "Empty" value to indicate whether or not the PointSet
   contains data. */
   ival = ( this->ptr == NULL );
   set = ( ival != 0 );
   astWriteInt( channel, "Empty", set, 0, ival,
                ival ? "PointSet is empty" :
                       "PointSet contains data" );

/* If it contains data, create a suitable keyword for each coordinate
   value in turn. */
   if ( this->ptr ) {
      makeComment = 1;
      i = 0;
      for ( point = 0; point < this->npoint; point++ ) {
         for ( coord = 0; coord < this->ncoord; coord++ ) {
            i++;
            (void) sprintf( key, "X%d", i );

/* Write the value out if good. Only supply a comment for the first good value. */
            if( this->ptr[ coord ][ point ] != AST__BAD ) {
               astWriteDouble( channel, key, 1, 1, this->ptr[ coord ][ point ],
                               ( makeComment ) ? "Coordinate values..." : "" );
               makeComment = 0;
            }
         }
      }
   }

/* Undefine macros local to this function. */
#undef KEY_LEN
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAPointSet and astCheckPointSet functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(PointSet,Object)
astMAKE_CHECK(PointSet)

AstPointSet *astPointSet_( int npoint, int ncoord, const char *options, int *status, ...) {
/*
*+
*  Name:
*     astPointSet

*  Purpose:
*     Create a PointSet.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "pointset.h"
*     AstPointSet *astPointSet( int npoint, int ncoord,
*                               const char *options, ..., int *status )

*  Class Membership:
*     PointSet constructor.

*  Description:
*     This function creates a new PointSet and optionally initialises its
*     attributes.

*  Parameters:
*     npoint
*        The number of points to be stored in the PointSet (must be at
*        least 1).
*     ncoord
*        The number of coordinate values associated with each point
*        (must be at least 1).
*     options
*        Pointer to a null terminated string containing an optional
*        comma-separated list of attribute assignments to be used for
*        initialising the new PointSet. The syntax used is the same as
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
*     A pointer to the new PointSet.

*  Notes:
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstPointSet *new;             /* Pointer to new PointSet */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the PointSet, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitPointSet( NULL, sizeof( AstPointSet ), !class_init,
                          &class_vtab, "PointSet", npoint, ncoord );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   PointSet's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new PointSet. */
   return new;
}

AstPointSet *astPointSetId_( int npoint, int ncoord,
                             const char *options, int *status, ...) {
/*
*  Name:
*     astPointSetId_

*  Purpose:
*     Create a PointSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pointset.h"
*     AstPointSet *astPointSetId_( int npoint, int ncoord,
*                                  const char *options, ... )

*  Class Membership:
*     PointSet constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astPointSet constructor function. It returns an ID value
*     (instead of a true C pointer) to external users, and must be
*     provided because astPointSet_ has a variable argument list which
*     cannot be encapsulated in a macro (where this conversion would
*     otherwise occur).
*
*     The variable argument list also prevents this function from
*     invoking astPointSet_ directly, so it must be a
*     re-implementation of it in all respects, except for the final
*     conversion of the result to an ID value.

*  Parameters:
*     As for astPointSet_.

*  Returned Value:
*     The ID value associated with the new PointSet.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstPointSet *new;             /* Pointer to new PointSet */
   va_list args;                 /* Variable argument list */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialise the PointSet, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitPointSet( NULL, sizeof( AstPointSet ), !class_init,
                          &class_vtab, "PointSet", npoint, ncoord );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   PointSet's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new PointSet. */
   return astMakeId( new );
}

AstPointSet *astInitPointSet_( void *mem, size_t size, int init,
                               AstPointSetVtab *vtab, const char *name,
                               int npoint, int ncoord, int *status ) {
/*
*+
*  Name:
*     astInitPointSet

*  Purpose:
*     Initialise a PointSet.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "pointset.h"
*     AstPointSet *astInitPointSet( void *mem, size_t size, int init,
*                                   AstPointSetVtab *vtab, const char *name,
*                                   int npoint, int ncoord )

*  Class Membership:
*     PointSet initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new PointSet object. It allocates memory (if necessary) to accommodate
*     the PointSet plus any additional data associated with the derived class.
*     It then initialises a PointSet structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a PointSet at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the PointSet is to be created. This
*        must be of sufficient size to accommodate the PointSet data
*        (sizeof(PointSet)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the PointSet (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the PointSet
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the PointSet's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new PointSet.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the Object
*        astClass function).
*     npoint
*        The number of points in the PointSet (must be at least 1).
*     ncoord
*        The number of coordinate values associated with each point (must be
*        at least 1).

*  Returned Value:
*     A pointer to the new PointSet.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstPointSet *new;             /* Pointer to new PointSet */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitPointSetVtab( vtab, name );

/* Initialise. */
   new = NULL;

/* Check the initialisation values for validity, reporting an error if
   necessary. */
   if ( npoint < 1 ) {
      astError( AST__NPTIN, "astInitPointSet(%s): Number of points (%d) is "
                "not valid.", status, name, npoint );
   } else if ( ncoord < 1 ) {
      astError( AST__NCOIN, "astInitPointSet(%s): Number of coordinates per "
                "point (%d) is not valid.", status, name, ncoord );
   }

/* Initialise an Object structure (the parent class) as the first component
   within the PointSet structure, allocating memory if necessary. */
   new = (AstPointSet *) astInitObject( mem, size, 0,
                                        (AstObjectVtab *) vtab, name );

   if ( astOK ) {

/* Initialise the PointSet data. */
/* ----------------------------- */
/* Store the number of points and number of coordinate values per point. */
      new->npoint = npoint;
      new->ncoord = ncoord;

/* Initialise pointers to the pointer array and associated coordinate
   values array. */
      new->ptr = NULL;
      new->values = NULL;
      new->acc = NULL;

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new object. */
   return new;
}

AstPointSet *astLoadPointSet_( void *mem, size_t size,
                               AstPointSetVtab *vtab, const char *name,
                               AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadPointSet

*  Purpose:
*     Load a PointSet.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "pointset.h"
*     AstPointSet *astLoadPointSet( void *mem, size_t size,
*                                   AstPointSetVtab *vtab, const char *name,
*                                   AstChannel *channel )

*  Class Membership:
*     PointSet loader.

*  Description:
*     This function is provided to load a new PointSet using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     PointSet structure in this memory, using data read from the
*     input Channel.

*  Parameters:
*     mem
*        A pointer to the memory into which the PointSet is to be
*        loaded.  This must be of sufficient size to accommodate the
*        PointSet data (sizeof(PointSet)) plus any data used by
*        derived classes. If a value of NULL is given, this function
*        will allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the PointSet (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the PointSet structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstPointSet) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new PointSet. If this is NULL, a pointer
*        to the (static) virtual function table for the PointSet class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "PointSet" is used instead.

*  Returned Value:
* A pointer to the new PointSet.

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
   AstPointSet *new;             /* Pointer to the new PointSet */
   char key[ KEY_LEN + 1 ];      /* Buffer for keywords */
   double acc;                   /* Accuracy value */
   int coord;                    /* Loop counter for coordinates */
   int empty;                    /* PointSet empty? */
   int i;                        /* Counter for coordinate values */
   int point;                    /* Loop counter for points */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this PointSet. In this case the
   PointSet belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstPointSet );
      vtab = &class_vtab;
      name = "PointSet";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitPointSetVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built PointSet. */
   new = astLoadObject( mem, size, (AstObjectVtab *) vtab, name,
                        channel );

   if ( astOK ) {

/* Initialise the PointSet's data pointers. */
      new->ptr = NULL;
      new->values = NULL;

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "PointSet" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* Npoint. */
/* ------- */
      new->npoint = astReadInt( channel, "npoint", 1 );
      if ( new->npoint < 1 ) new->npoint = 1;

/* Ncoord. */
/* ------- */
      new->ncoord = astReadInt( channel, "ncoord", 1 );
      if ( new->ncoord < 1 ) new->ncoord = 1;

/* Axis Acuracies. */
/* --------------- */
      new->acc = NULL;
      for ( coord = 0; coord < new->ncoord; coord++ ) {
         (void) sprintf( key, "acc%d", coord + 1 );
         acc = astReadDouble( channel, key, AST__BAD );
         if( !new->acc && acc != AST__BAD ) {
            new->acc = astMalloc( sizeof( double )*(size_t) new->ncoord );
            if( new->acc ) {
               for( i = 0; i < coord - 1; i++ ) new->acc[ i ] = AST__BAD;
            }
         }
         if( new->acc ) new->acc[ coord ] = acc;
      }

/* Coordinate data. */
/* ---------------- */
/* Read a value for the "Empty" keyword to see whether the PointSet
   contains data. */
      empty = astReadInt( channel, "empty", 0 );

/* If it does, allocate memory to hold the coordinate data and
   pointers. */
      if ( astOK && !empty ) {
         new->ptr = astMalloc( sizeof( double * ) * (size_t) new->ncoord );
         new->values = astMalloc( sizeof( double ) *
                                  (size_t) ( new->npoint * new->ncoord ) );
         if ( astOK ) {

/* Initialise the array of pointers into the main data array. */
            for ( coord = 0; coord < new->ncoord; coord++ ) {
               new->ptr[ coord ] = new->values + ( coord * new->npoint );
            }

/* Create a keyword for each coordinate value to be read. */
            i = 0;
            for ( point = 0; point < new->npoint; point++ ) {
               for ( coord = 0; coord < new->ncoord; coord++ ) {
                  i++;
                  (void) sprintf( key, "x%d", i );

/* Read and assign the values. */
                  new->ptr[ coord ][ point ] =
                     astReadDouble( channel, key, AST__BAD );
               }
            }
         }

/* If an error occurred, clean up by freeing the memory allocated
   above, thus emptying the PointSet. */
         if ( !astOK ) {
            new->ptr = astFree( new->ptr );
            new->values = astFree( new->values );
         }
      }

/* If an error occurred, clean up by deleting the new PointSet. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new PointSet pointer. */
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
int astGetNpoint_( const AstPointSet *this, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,PointSet,GetNpoint))( this, status );
}
int astGetNcoord_( const AstPointSet *this, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,PointSet,GetNcoord))( this, status );
}
double **astGetPoints_( AstPointSet *this, int *status ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,PointSet,GetPoints))( this, status );
}
void astPermPoints_( AstPointSet *this, int forward, const int perm[], int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,PointSet,PermPoints))( this, forward, perm, status );
}
void astSetPoints_( AstPointSet *this, double **ptr, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,PointSet,SetPoints))( this, ptr, status );
}
void astSetNpoint_( AstPointSet *this, int npoint, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,PointSet,SetNpoint))( this, npoint, status );
}
void astSetSubPoints_( AstPointSet *point1, int point, int coord,
                       AstPointSet *point2, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(point1,PointSet,SetSubPoints))( point1, point, coord, point2, status );
}
AstPointSet *astAppendPoints_( AstPointSet *this, AstPointSet *that, int *status ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,PointSet,AppendPoints))( this, that, status );
}
void astBndPoints_( AstPointSet *this, double *lbnd, double *ubnd, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,PointSet,BndPoints))( this, lbnd, ubnd, status );
}

int astReplaceNaN_( AstPointSet *this, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,PointSet,ReplaceNaN))( this, status );
}





