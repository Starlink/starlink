/*
*class++
*  Name:
*     PcdMap

*  Purpose:
*     Apply 2-dimensional pincushion/barrel distortion.

*  Constructor Function:
c     astPcdMap
f     AST_PCDMAP

*  Description:
*     A PcdMap is a non-linear Mapping which transforms 2-dimensional
*     positions to correct for the radial distortion introduced by some
*     cameras and telescopes. This can take the form either of pincushion
*     or barrel distortion, and is characterized by a single distortion
*     coefficient.
*
*     A PcdMap is specified by giving this distortion coefficient and the
*     coordinates of the centre of the radial distortion. The forward
*     transformation of a PcdMap applies the distortion:
*
*        RD = R * ( 1 + C * R * R )
*
*     where R is the undistorted radial distance from the distortion
*     centre (specified by attribute PcdCen), RD is the radial distance
*     from the same centre in the presence of distortion, and C is the
*     distortion coefficient (given by attribute Disco).
*
*     The inverse transformation of a PcdMap removes the distortion
*     produced by the forward transformation. The expression used to derive
*     R from RD is an approximate inverse of the expression above.

*  Inheritance:
*     The PcdMap class inherits from the Mapping class.

*  Attributes:
*     In addition to those attributes common to all Mappings, every
*     PcdMap also has the following attributes:
*
*     - Disco: PcdMap pincushion/barrel distortion coefficient
*     - PcdCen(axis): Centre coordinates of pincushion/barrel distortion

*  Functions:
c     The PcdMap class does not define any new functions beyond those
f     The PcdMap class does not define any new routines beyond those
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
*     DSB: David Berry (Starlink)

*  History:
*     18-MAY-1999 (DSB):
*        Original version.
*     25-OCT-1999 (DSB):
*        Fixed memory leak in MapMerge.
*     8-JAN-2003 (DSB):
*        Changed private InitVtab method to protected astInitPcdMapVtab
*        method.
*     23-AUG-2006 (DSB):
*        Override astEqual.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS PcdMap

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory management facilities */
#include "globals.h"             /* Thread-safe global data access */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* Sets of points/coordinates */
#include "unitmap.h"             /* Unit mappings */
#include "zoommap.h"             /* Zoom mappings */
#include "permmap.h"             /* Axis permutations */
#include "mapping.h"             /* Coordinate mappings (parent class) */
#include "channel.h"             /* I/O channels */
#include "pcdmap.h"              /* Interface definition for this class */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <float.h>
#include <math.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

/* Module Variables. */
/* ================= */

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* Pointers to parent class methods which are extended by this class. */
static AstPointSet *(* parent_transform)( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static const char *(* parent_getattrib)( AstObject *, const char *, int * );
static int (* parent_testattrib)( AstObject *, const char *, int * );
static void (* parent_clearattrib)( AstObject *, const char *, int * );
static void (* parent_setattrib)( AstObject *, const char *, int * );

/* Define macros for accessing each item of thread specific global data. */
#ifdef THREAD_SAFE

/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0; \
   globals->GetAttrib_Buff[ 0 ] = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(PcdMap)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(PcdMap,Class_Init)
#define class_vtab astGLOBAL(PcdMap,Class_Vtab)
#define getattrib_buff astGLOBAL(PcdMap,GetAttrib_Buff)



/* If thread safety is not needed, declare and initialise globals at static
   variables. */
#else

static char getattrib_buff[ 101 ];


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstPcdMapVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstPcdMap *astPcdMapId_( double, const double [2], const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */

static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static const char *GetAttrib( AstObject *, const char *, int * );
static double GetDisco( AstPcdMap *, int * );
static double GetPcdCen( AstPcdMap *, int, int * );
static int CanMerge( AstMapping *, AstMapping *, int, int, int * );
static int CanSwap( AstMapping *, AstMapping *, int, int, int * );
static int Equal( AstObject *, AstObject *, int * );
static int MapMerge( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
static int TestAttrib( AstObject *, const char *, int * );
static int TestDisco( AstPcdMap *, int * );
static int TestPcdCen( AstPcdMap *, int, int * );
static void ClearAttrib( AstObject *, const char *, int * );
static void ClearDisco( AstPcdMap *, int * );
static void ClearPcdCen( AstPcdMap *, int, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void PcdPerm( AstMapping **, int *, int, int * );
static void PcdZoom( AstMapping **, int *, int, int * );
static void PermGet( AstPermMap *, int **, int **, double **, int * );
static void SetAttrib( AstObject *, const char *, int * );
static void SetDisco( AstPcdMap *, double, int * );
static void SetPcdCen( AstPcdMap *, int, double, int * );

/* Function Macros */
/* =============== */
/* Macros which return the maximum and minimum of two values. */
#define MAX(aa,bb) ((aa)>(bb)?(aa):(bb))
#define MIN(aa,bb) ((aa)<(bb)?(aa):(bb))

/* Macro to check for equality of floating point values. We cannot
compare bad values directory because of the danger of floating point
exceptions, so bad values are dealt with explicitly. */
#define EQUAL(aa,bb) (((aa)==AST__BAD)?(((bb)==AST__BAD)?1:0):(((bb)==AST__BAD)?0:(fabs((aa)-(bb))<=1.0E5*MAX((fabs(aa)+fabs(bb))*DBL_EPSILON,DBL_MIN))))

/*
*
*  Name:
*     MAKE_CLEAR

*  Purpose:
*     Implement a method to clear a single value in a multi-valued attribute.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "pcdmap.h"
*     MAKE_CLEAR(attr,component,assign,nval)

*  Class Membership:
*     Defined by the PcdMap class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static void Clear<Attribute>( AstPcdMap *this, int axis )
*
*     and an external interface function of the form:
*
*        void astClear<Attribute>_( AstPcdMap *this, int axis )
*
*     which implement a method for clearing a single value in a specified
*     multi-valued attribute for an axis of a PcdMap.

*  Parameters:
*     attr
*        The name of the attribute to be cleared, as it appears in the function
*        name (e.g. PcdCen in "astClearPcdCen").
*     component
*        The name of the class structure component that holds the attribute
*        value.
*     assign
*        An expression that evaluates to the value to assign to the component
*        to clear its value.
*     nval
*        Specifies the number of values in the multi-valued attribute. The
*        "axis" values supplied to the created function should be in the
*        range zero to (nval - 1).

*  Notes:
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*
*/

/* Define the macro. */
#define MAKE_CLEAR(attr,component,assign,nval) \
\
/* Private member function. */ \
/* ------------------------ */ \
static void Clear##attr( AstPcdMap *this, int axis, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Validate the axis index. */ \
   if( axis < 0 || axis >= nval ){ \
      astError( AST__AXIIN, "%s(%s): Index (%d) is invalid for attribute " \
                #attr " - it should be in the range 1 to %d.", status, \
                "astClear" #attr, astGetClass( this ), \
                axis + 1, nval ); \
\
/* Assign the "clear" value. */ \
   } else { \
      this->component[ axis ] = (assign); \
   } \
} \
\
/* External interface. */ \
/* ------------------- */ \
void astClear##attr##_( AstPcdMap *this, int axis, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Invoke the required method via the virtual function table. */ \
   (**astMEMBER(this,PcdMap,Clear##attr))( this, axis, status ); \
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
*     #include "pcdmap.h"
*     MAKE_GET(attr,type,bad_value,assign,nval)

*  Class Membership:
*     Defined by the PcdMap class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static <Type> Get<Attribute>( AstPcdMap *this, int axis )
*
*     and an external interface function of the form:
*
*        <Type> astGet<Attribute>_( AstPcdMap *this, int axis )
*
*     which implement a method for getting a single value from a specified
*     multi-valued attribute for an axis of a PcdMap.

*  Parameters:
*     attr
*        The name of the attribute whose value is to be obtained, as it
*        appears in the function name (e.g. PcdCen in "astGetPcdCen").
*     type
*        The C type of the attribute.
*     bad_value
*        A constant value to return if the global error status is set, or if
*        the function fails.
*     assign
*        An expression that evaluates to the value to be returned. This can
*        use the string "axis" to represent the zero-based value index.
*     nval
*        Specifies the number of values in the multi-valued attribute. The
*        "axis" values supplied to the created function should be in the
*        range zero to (nval - 1).

*  Notes:
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*
*/

/* Define the macro. */
#define MAKE_GET(attr,type,bad_value,assign,nval) \
\
/* Private member function. */ \
/* ------------------------ */ \
static type Get##attr( AstPcdMap *this, int axis, int *status ) { \
   type result;                  /* Result to be returned */ \
\
/* Initialise */ \
   result = (bad_value); \
\
/* Check the global error status. */ \
   if ( !astOK ) return result; \
\
/* Validate the axis index. */ \
   if( axis < 0 || axis >= nval ){ \
      astError( AST__AXIIN, "%s(%s): Index (%d) is invalid for attribute " \
                #attr " - it should be in the range 1 to %d.", status, \
                "astGet" #attr, astGetClass( this ), \
                axis + 1, nval ); \
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
type astGet##attr##_( AstPcdMap *this, int axis, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return (bad_value); \
\
/* Invoke the required method via the virtual function table. */ \
   return (**astMEMBER(this,PcdMap,Get##attr))( this, axis, status ); \
}

/*
*
*  Name:
*     MAKE_SET

*  Purpose:
*     Implement a method to set a single value in a multi-valued attribute.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "pcdmap.h"
*     MAKE_SET(attr,type,component,assign,nval)

*  Class Membership:
*     Defined by the PcdMap class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static void Set<Attribute>( AstPcdMap *this, int axis, <Type> value )
*
*     and an external interface function of the form:
*
*        void astSet<Attribute>_( AstPcdMap *this, int axis, <Type> value )
*
*     which implement a method for setting a single value in a specified
*     multi-valued attribute for a PcdMap.

*  Parameters:
*      attr
*         The name of the attribute to be set, as it appears in the function
*         name (e.g. PcdCen in "astSetPcdCen").
*      type
*         The C type of the attribute.
*      component
*         The name of the class structure component that holds the attribute
*         value.
*      assign
*         An expression that evaluates to the value to be assigned to the
*         component.
*      nval
*         Specifies the number of values in the multi-valued attribute. The
*         "axis" values supplied to the created function should be in the
*         range zero to (nval - 1).

*  Notes:
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*-
*/

/* Define the macro. */
#define MAKE_SET(attr,type,component,assign,nval) \
\
/* Private member function. */ \
/* ------------------------ */ \
static void Set##attr( AstPcdMap *this, int axis, type value, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Validate the axis index. */ \
   if( axis < 0 || axis >= nval ){ \
      astError( AST__AXIIN, "%s(%s): Index (%d) is invalid for attribute " \
                #attr " - it should be in the range 1 to %d.", status, \
                "astSet" #attr, astGetClass( this ), \
                axis + 1, nval ); \
\
/* Store the new value in the structure component. */ \
   } else { \
      this->component[ axis ] = (assign); \
   } \
} \
\
/* External interface. */ \
/* ------------------- */ \
void astSet##attr##_( AstPcdMap *this, int axis, type value, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Invoke the required method via the virtual function table. */ \
   (**astMEMBER(this,PcdMap,Set##attr))( this, axis, value, status ); \
}

/*
*
*  Name:
*     MAKE_TEST

*  Purpose:
*     Implement a method to test if a single value has been set in a
*     multi-valued attribute.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "pcdmap.h"
*     MAKE_TEST(attr,assign,nval)

*  Class Membership:
*     Defined by the PcdMap class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static int Test<Attribute>( AstPcdMap *this, int axis )
*
*     and an external interface function of the form:
*
*        int astTest<Attribute>_( AstPcdMap *this, int axis )
*
*     which implement a method for testing if a single value in a specified
*     multi-valued attribute has been set for a class.

*  Parameters:
*      attr
*         The name of the attribute to be tested, as it appears in the function
*         name (e.g. PcdCen in "astTestPcdCen").
*      assign
*         An expression that evaluates to 0 or 1, to be used as the returned
*         value. This can use the string "axis" to represent the zero-based
*         index of the value within the attribute.
*      nval
*         Specifies the number of values in the multi-valued attribute. The
*         "axis" values supplied to the created function should be in the
*         range zero to (nval - 1).

*  Notes:
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*-
*/

/* Define the macro. */
#define MAKE_TEST(attr,assign,nval) \
\
/* Private member function. */ \
/* ------------------------ */ \
static int Test##attr( AstPcdMap *this, int axis, int *status ) { \
   int result;                   /* Value to return */ \
\
/* Initialise */ \
   result = 0; \
\
/* Check the global error status. */ \
   if ( !astOK ) return result; \
\
\
/* Validate the axis index. */ \
   if( axis < 0 || axis >= nval ){ \
      astError( AST__AXIIN, "%s(%s): Index (%d) is invalid for attribute " \
                #attr " - it should be in the range 1 to %d.", status, \
                "astTest" #attr, astGetClass( this ), \
                axis + 1, nval ); \
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
int astTest##attr##_( AstPcdMap *this, int axis, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return 0; \
\
/* Invoke the required method via the virtual function table. */ \
   return (**astMEMBER(this,PcdMap,Test##attr))( this, axis, status ); \
}

/* Member functions. */
/* ================= */
static int CanMerge( AstMapping *map1, AstMapping *map2, int inv1, int inv2, int *status ){
/*
*
*  Name:
*     CanMerge

*  Purpose:
*     Checks if two Mappings can be merged.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pcdmap.h"
*     int CanMerge( AstMapping *map1, AstMapping *map2, int inv1, int inv2, int *status )

*  Class Membership:
*     PcdMap internal utility function.

*  Description:
*     This function checks the two supplied Mappings to see if they
*     can be merged into a single Mapping. One of the pair must be a PcdMap.

*  Parameters:
*     map1
*        A pointer to the first mapping.
*     map2
*        A pointer to the second mapping.
*     inv1
*        The invert flag to use with the first mapping.
*     inv2
*        The invert flag to use with the second mapping.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     1 if the Mappings can be merged, zero otherwise.

*/

   AstPcdMap  *pcd;          /* Pointer to PcdMap Mapping */
   AstPcdMap  *pcd2;         /* Pointer to second PcdMap Mapping */
   AstMapping *nopcd;        /* Pointer to non-PcdMap Mapping */
   const char *class1;       /* Pointer to map1 class string */
   const char *class2;       /* Pointer to map2 class string */
   const char *nopcd_class;  /* Pointer to non-PcdMap class string */
   int invert[ 2 ];          /* Original invert flags */
   int ret;                  /* Returned flag */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Initialise */
   ret = 0;
   pcd = NULL;
   nopcd = NULL;

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

/* Get pointers to the PcdMap and the non-PcdMap Mapping. */
      if( !strcmp( class1, "PcdMap" ) ){
         pcd = (AstPcdMap * ) map1;
         nopcd = map2;
         nopcd_class = class2;
      } else if( !strcmp( class2, "PcdMap" ) ){
         nopcd = map1;
         pcd = (AstPcdMap * ) map2;
         nopcd_class = class1;
      } else {
         nopcd_class = "unusable";
      }

/* The Mappings can merge if the other Mapping is a UnitMap. */
      if( !strcmp( nopcd_class, "UnitMap" ) ){
         ret = 1;

/* They can also merge if the other Mapping is also a PcdMap, and is the
   invert of the other. */
      } else if( !strcmp( nopcd_class, "PcdMap" ) ){
         pcd2 = (AstPcdMap *) nopcd;

/* Check the distortion coefficients are equal. */
         if( EQUAL( astGetDisco( pcd ), astGetDisco( pcd2 ) ) ){

/* Check the axis 0 centres are equal. */
            if( EQUAL( astGetPcdCen( pcd, 0 ), astGetPcdCen( pcd2, 0 ) ) ){

/* Check the axis 1 centres are equal. */
               if( EQUAL( astGetPcdCen( pcd, 1 ), astGetPcdCen( pcd2, 1 ) ) ){

/* Check the Invert flags are different. */
                  if( astGetInvert( pcd ) != astGetInvert( pcd2 ) ){

/* If the Mappings have passed all these tests, they can be merged. */
                     ret = 1;
                  }
               }
            }
         }
      }
   }

/* Re-instate the original settings of the Invert attributes for the
   supplied Mappings. */
   astSetInvert( map1, invert[ 0 ] );
   astSetInvert( map2, invert[ 1 ] );

/* Return the answer. */
   return astOK ? ret : 0;
}

static int CanSwap( AstMapping *map1, AstMapping *map2, int inv1, int inv2, int *status ){
/*
*  Name:
*     CanSwap

*  Purpose:
*     Determine if two Mappings could be swapped.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pcdmap.h"
*     int CanSwap( AstMapping *map1, AstMapping *map2, int inv1, int inv2, int *status )

*  Class Membership:
*     PcdMap member function

*  Description:
*     This function returns a flag indicating if the pair of supplied
*     Mappings could be replaced by an equivalent pair of Mappings from the
*     same classes as the supplied pair, but in reversed order. Each pair
*     of Mappings is considered to be compounded in series. The supplied
*     Mappings are not changed in any way.

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
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     1 if the Mappings could be swapped, 0 otherwise.

*  Notes:
*     -  One of the supplied pair of Mappings must be a PcdMap.
*     -  A value of 0 is returned if the two Mappings could be merged into
*     a single Mapping.
*     -  A value of 0 is returned if an error has already occurred, or if
*     this function should fail for any reason.
*/

/* Local Variables: */
   AstMapping *nopcd;        /* Pointer to non-PcdMap Mapping */
   const char *class1;       /* Pointer to map1 class string */
   const char *class2;       /* Pointer to map2 class string */
   const char *nopcd_class;  /* Pointer to non-PcdMap class string */
   double *consts;           /* Pointer to constants array */
   int *inperm;              /* Pointer to input axis permutation array */
   int *outperm;             /* Pointer to output axis permutation array */
   int invert[ 2 ];          /* Original invert flags */
   int nin;                  /* No. of input coordinates for the PermMap */
   int nout;                 /* No. of output coordinates for the PermMap */
   int pcdinv;               /* Use inverted PcdMap? */
   int ret;                  /* Returned flag */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Initialise */
   ret = 0;

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

/* Get a pointer to the non-PcdMap Mapping. */
      if( !strcmp( class1, "PcdMap" ) ){
         nopcd = map2;
         nopcd_class = class2;
         pcdinv = inv1;
      } else {
         nopcd = map1;
         nopcd_class = class1;
         pcdinv = inv2;
      }

/* If the other Mapping is a ZoomMap, the Mappings can be swapped. */
      if( !strcmp( nopcd_class, "ZoomMap" ) ){
         ret = 1;

/* If it is a PermMap, the Mappings can be swapped so long as the PermMap
   simply swaps the two axes. */
      } else if( !strcmp( nopcd_class, "PermMap" ) ){

/* Get the number of input and output coordinates for the PermMap. */
         nin = astGetNin( nopcd );
         nout = astGetNout( nopcd );

/* Must be 2-dimensional to swap. */
         if( nin == 2 && nout == 2 ) {

/* Get the axis permutation arrays and constants array for the PermMap. */
            PermGet( (AstPermMap *) nopcd, &outperm, &inperm, &consts, status );
            if( astOK ) {

/* If the PermMap simply swaps the 2 axes (in both direction) we can
   swap the two mappings. */
               ret = ( outperm[0] == 1 && outperm[1] == 0 &&
                       inperm[0] == 1 && inperm[1] == 0 );

/* Free the axis permutation and constants arrays. */
               outperm = (int *) astFree( (void *) outperm );
               inperm = (int *) astFree( (void *) inperm );
               consts = (double *) astFree( (void *) consts );
            }
         }
      }
   }

/* Re-instate the original settings of the Invert attributes for the
   supplied Mappings. */
   astSetInvert( map1, invert[ 0 ] );
   astSetInvert( map2, invert[ 1 ] );

/* Return the answer. */
   return astOK ? ret : 0;
}

static void ClearAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for a PcdMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pcdmap.h"
*     void ClearAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     PcdMap member function (over-rides the astClearAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function clears the value of a specified attribute for a
*     PcdMap, so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the PcdMap.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstPcdMap *this;              /* Pointer to the PcdMap structure */
   int axis;                     /* Axis number */
   int len;                      /* Length of attrib string */
   int nc;                       /* No. characters read by astSscanf */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the PcdMap structure. */
   this = (AstPcdMap *) this_object;

/* Obtain the length of the "attrib" string. */
   len = strlen( attrib );

/* Check the attribute name and clear the appropriate attribute. */

/* PcdCen(axis). */
/* ------------- */
   if ( nc = 0,
               ( 1 == astSscanf( attrib, "pcdcen(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      astClearPcdCen( this, axis - 1 );

/* PcdCen. */
/* ------- */
   } else if ( !strcmp( attrib, "pcdcen" ) ) {
      astClearPcdCen( this, 0 );
      astClearPcdCen( this, 1 );

/* Disco. */
/* ------ */
   } else if ( !strcmp( attrib, "disco" ) ) {
      astClearDisco( this );

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
*     Test if two PcdMaps are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pcdmap.h"
*     int Equal( AstObject *this, AstObject *that, int *status )

*  Class Membership:
*     PcdMap member function (over-rides the astEqual protected
*     method inherited from the astMapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two PcdMaps are equivalent.

*  Parameters:
*     this
*        Pointer to the first Object (a PcdMap).
*     that
*        Pointer to the second Object.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the PcdMaps are equivalent, zero otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstPcdMap *that;
   AstPcdMap *this;
   int result;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain pointers to the two PcdMap structures. */
   this = (AstPcdMap *) this_object;
   that = (AstPcdMap *) that_object;

/* Check the second object is a PcdMap. We know the first is a
   PcdMap since we have arrived at this implementation of the virtual
   function. */
   if( astIsAPcdMap( that ) ) {

/* Check the Invert flags for the two PcdMaps are equal. */
      if( astGetInvert( this ) == astGetInvert( that ) ) {

/* Check all the attributes are equal. */
         if( astEQUAL( this->pcdcen[0], that->pcdcen[0] ) &&
             astEQUAL( this->pcdcen[1], that->pcdcen[1] ) &&
             astEQUAL( this->disco, that->disco ) ) {
            result = 1;
         }
      }
   }

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
*     Get the value of a specified attribute for a PcdMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pcdmap.h"
*     const char *GetAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     PcdMap member function (over-rides the protected astGetAttrib
*     method inherited from the Mapping class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for a PcdMap, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the PcdMap.
*     attrib
*        Pointer to a null terminated string containing the name of
*        the attribute whose value is required. This name should be in
*        lower case, with all white space removed.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     - Pointer to a null terminated string containing the attribute
*     value.

*  Notes:
*     - The returned string pointer may point at memory allocated
*     within the PcdMap, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the PcdMap. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstPcdMap *this;              /* Pointer to the PcdMap structure */
   const char *result;           /* Pointer value to return */
   double dval;                  /* Double attribute value */
   int axis;                     /* Axis number */
   int len;                      /* Length of attrib string */
   int nc;                       /* No. characters read by astSscanf */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(this_object);

/* Obtain a pointer to the PcdMap structure. */
   this = (AstPcdMap *) this_object;

/* Obtain the length of the attrib string. */
   len = strlen( attrib );

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "getattrib_buff" as a null terminated string in an appropriate
   format.  Set "result" to point at the result string. */

/* Disco. */
/* ------ */
   if ( !strcmp( attrib, "disco" ) ) {
      dval = astGetDisco( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%.*g", DBL_DIG, dval );
         result = getattrib_buff;
      }

/* PcdCen(axis). */
/* ------------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "pcdcen(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      dval = astGetPcdCen( this, axis - 1 );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%.*g", DBL_DIG, dval );
         result = getattrib_buff;
      }

/* PcdCen. */
/* ------- */
   } else if ( !strcmp( attrib, "pcdcen" ) ) {
      dval = astGetPcdCen( this, 0 );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%.*g", DBL_DIG, dval );
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

void astInitPcdMapVtab_(  AstPcdMapVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitPcdMapVtab

*  Purpose:
*     Initialise a virtual function table for a PcdMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "pcdmap.h"
*     void astInitPcdMapVtab( AstPcdMapVtab *vtab, const char *name )

*  Class Membership:
*     PcdMap vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the PcdMap class.

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
   will be used (by astIsAPcdMap) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstMappingVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */
   vtab->ClearDisco = ClearDisco;
   vtab->SetDisco = SetDisco;
   vtab->GetDisco = GetDisco;
   vtab->TestDisco = TestDisco;
   vtab->ClearPcdCen = ClearPcdCen;
   vtab->SetPcdCen = SetPcdCen;
   vtab->GetPcdCen = GetPcdCen;
   vtab->TestPcdCen = TestPcdCen;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   mapping = (AstMappingVtab *) vtab;

   parent_clearattrib = object->ClearAttrib;
   object->ClearAttrib = ClearAttrib;
   parent_getattrib = object->GetAttrib;
   object->GetAttrib = GetAttrib;
   parent_setattrib = object->SetAttrib;
   object->SetAttrib = SetAttrib;
   parent_testattrib = object->TestAttrib;
   object->TestAttrib = TestAttrib;
   object->Equal = Equal;

   parent_transform = mapping->Transform;
   mapping->Transform = Transform;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   mapping->MapMerge = MapMerge;

/* Declare the class dump function.*/
   astSetDump( vtab, Dump, "PcdMap", "Apply pincushion distortion" );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
}

static int MapMerge( AstMapping *this, int where, int series, int *nmap,
                     AstMapping ***map_list, int **invert_list, int *status ) {
/*
*  Name:
*     MapMerge

*  Purpose:
*     Simplify a sequence of Mappings containing a PcdMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     int MapMerge( AstMapping *this, int where, int series, int *nmap,
*                   AstMapping ***map_list, int **invert_list, int *status )

*  Class Membership:
*     PcdMap method (over-rides the protected astMapMerge method
*     inherited from the Mapping class).

*  Description:
*     This function attempts to simplify a sequence of Mappings by
*     merging a nominated PcdMap in the sequence with its neighbours,
*     so as to shorten the sequence if possible.
*
*     In many cases, simplification will not be possible and the
*     function will return -1 to indicate this, without further
*     action.
*
*     In most cases of interest, however, this function will either
*     attempt to replace the nominated PcdMap with a Mapping which it
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
*        Pointer to the nominated PcdMap which is to be merged with
*        its neighbours. This should be a cloned copy of the PcdMap
*        pointer contained in the array element "(*map_list)[where]"
*        (see below). This pointer will not be annulled, and the
*        PcdMap it identifies will not be modified by this function.
*     where
*        Index in the "*map_list" array (below) at which the pointer
*        to the nominated PcdMap resides.
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
   AstMapping *map2;     /* First mapping to check */
   AstMapping *newmap;   /* Pointer to replacement Mapping */
   AstMapping *mc[2];    /* Copies of supplied Mappings to swap */
   AstMapping *smc0;     /* Simplied Mapping */
   AstMapping *smc1;     /* Simplied Mapping */
   const char *class1;   /* Pointer to first Mapping class string */
   const char *class2;   /* Pointer to second Mapping class string */
   const char *nclass;   /* Pointer to neighbouring Mapping class */
   int i1;               /* Index of first PcdMap to merge */
   int i2;               /* Index of last PcdMap to merge */
   int i;                /* Loop counter */
   int ic[2];            /* Copies of supplied invert flags to swap */
   int invert;           /* Should the inverted Mapping be used? */
   int *invlt;           /* New invert flags list pointer */
   int neighbour;        /* Index of Mapping with which to swap */
   int nin;              /* Number of coordinates for PcdMap */
   int nmapt;            /* No. of Mappings in list */
   int nstep1;           /* No. of Mappings backwards to next mergable Mapping */
   int nstep2;           /* No. of Mappings forward to next mergable Mapping */
   int result;           /* Result value to return */
   int swaphi;           /* Can PcdMap be swapped with higher neighbour? */
   int swaplo;           /* Can PcdMap be swapped with lower neighbour? */

/* Initialise. */
   result = -1;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Initialise variables to avoid "used of uninitialised variable"
   messages from dumb compilers. */
   neighbour = 0;
   i1 = 0;
   i2 = 0;

/* Get the number of axes for the PcdMap. */
   nin = astGetNin( ( *map_list )[ where ] );

/* First of all, see if the PcdMap can be replaced by a simpler Mapping,
   without reference to the neighbouring Mappings in the list.           */
/* ======================================================================*/
/* If the distortion coefficient in the PcdMap is zero, the PcdMap can be
   replaced by a UnitMap. */
   if( EQUAL( astGetDisco( (AstPcdMap *) ( *map_list )[ where ] ), 0.0 ) ){

/* Annul the PcdMap pointer in the list and replace it with a UnitMap
   pointer, and indicate that the forward transformation of the returned
   UnitMap should be used. */
      (void) astAnnul( ( *map_list )[ where ] );
      ( *map_list )[ where ] = (AstMapping *) astUnitMap( 2, "", status );
      ( *invert_list )[ where ] = 0;

/* Return the index of the first modified element. */
      result = where;

/* If the PcdMap itself could not be simplified, see if it can be merged
   with the Mappings on either side of it in the list. */
   } else {

/* Store the classes of the neighbouring Mappings in the list. */
       class1 = ( where > 0 ) ? astGetClass( ( *map_list )[ where - 1 ] ) : NULL;
       class2 = ( where < *nmap - 1 ) ? astGetClass( ( *map_list )[ where + 1 ] ) : NULL;

/* In series. */
/* ========== */
      if ( series ) {

/* We first look to see if the PcdMap can be merged with one of its
   neighbours, resulting in a reduction of one in the number of Mappings
   in the list. A PcdMap can only merge directly with its own inverse, or
   a UnitMap. */
         if( class1 && CanMerge(  ( *map_list )[ where - 1 ],
                                  ( *map_list )[ where ],
                                  ( *invert_list )[ where - 1 ],
                                  ( *invert_list )[ where ], status ) ){
            nclass = class1;
            i1 = where - 1;
            i2 = where;

         } else if( class2 && CanMerge(  ( *map_list )[ where ],
                                  ( *map_list )[ where + 1 ],
                                  ( *invert_list )[ where ],
                                  ( *invert_list )[ where + 1 ], status ) ){
            nclass = class2;
            i1 = where;
            i2 = where + 1;

         } else {
            nclass = NULL;
         }

/* If the PcdMap can merge with one of its neighbours, create the merged
   Mapping. */
         if( nclass ){

/* If the neighbour is a PcdMap, it must be the inverse of the nominated
   Mapping, and so they merge to form a UnitMap. */
            if( !strcmp( nclass, "PcdMap" ) ){
               newmap = (AstMapping *) astUnitMap( 2, "", status );
               invert = 0;

/* If the neighbour is a UnitMap, they merge to form a clone of the
   nominated PcdMap. */
            } else {
               newmap = (AstMapping *) astClone( ( *map_list )[ where ] );
               invert = ( *invert_list )[ where ];
            }

/* If succesfull... */
            if( astOK ){

/* Annul the first of the two Mappings, and replace it with the merged
   Mapping. Also set the invert flag. */
               (void) astAnnul( ( *map_list )[ i1 ] );
               ( *map_list )[ i1 ] = newmap;
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

/* If the PcdMap could not merge directly with either of its neighbours,
   we consider whether it would be worthwhile to swap the PcdMap with
   either of its neighbours. This can only be done for certain classes
   of Mapping (ZoomMaps & some PermMaps), and will usually require both
   Mappings to be modified (unless they are commutative). The advantage of
   swapping the order of the Mappings is that it may result in the PcdMap
   being adjacent to a Mapping with which it can merge directly on the next
   invocation of this function, thus reducing the number of Mappings
   in the list. */
         } else {

/* Set a flag if we could swap the PcdMap with its higher neighbour. */
            if( where + 1 < *nmap ){
               swaphi = CanSwap(  ( *map_list )[ where ],
                                  ( *map_list )[ where + 1 ],
                                  ( *invert_list )[ where ],
                                  ( *invert_list )[ where + 1 ], status );
            } else {
               swaphi = 0;
            }

/* If so, step through each of the Mappings which follow the PcdMap,
   looking for a Mapping with which the PcdMap could merge directly. Stop
   when such a Mapping is found, or if a Mapping is found with which the
   PcdMap could definitely not swap. Note the number of Mappings which
   separate the PcdMap from the Mapping with which it could merge (if
   any). */
            nstep2 = -1;
            if( swaphi ){
               for( i2 = where + 1; i2 < *nmap; i2++ ){

/* See if we may be able to merge with this Mapping. If so, note the number
   of steps between the two Mappings and leave the loop. */
                  nclass = astGetClass( ( *map_list )[ i2 ] );

                  if( !strcmp( nclass, "UnitMap" ) ||
                      !strcmp( nclass, "PcdMap" ) ){
                     nstep2 = i2 - where - 1;
                     break;
                  }

/* If there is no chance that we can swap with this Mapping, leave the loop
   with -1 for the number of steps to indicate that no merging is possible.
   PcdMaps can swap with ZoomMaps and some PermMaps. */
                  if( strcmp( nclass, "ZoomMap" ) &&
                      strcmp( nclass, "PermMap" ) ) {
                     break;
                  }

               }

            }

/* Do the same working forward from the PcdMap towards the start of the map
   list. */
            if( where > 0 ){
               swaplo = CanSwap(  ( *map_list )[ where - 1 ],
                                  ( *map_list )[ where ],
                                  ( *invert_list )[ where - 1 ],
                                  ( *invert_list )[ where ], status );
            } else {
               swaplo = 0;
            }

            nstep1 = -1;
            if( swaplo ){
               for( i1 = where - 1; i1 >= 0; i1-- ){

                  nclass = astGetClass( ( *map_list )[ i1 ] );

                  if( !strcmp( nclass, "UnitMap" ) ||
                      !strcmp( nclass, "PcdMap" ) ){
                     nstep1 = where - 1 - i1;
                     break;
                  }

                  if( strcmp( nclass, "ZoomMap" ) &&
                      strcmp( nclass, "PermMap" ) ) {
                     break;
                  }

               }

            }

/* Choose which neighbour to swap with so that the PcdMap moves towards the
   nearest Mapping with which it can merge. */
            if( nstep1 != -1 && ( nstep2 == -1 || nstep2 > nstep1 ) ){
               nclass = class1;
               i1 = where - 1;
               i2 = where;
               neighbour = i1;
            } else if( nstep2 != -1 ){
               nclass = class2;
               i1 = where;
               i2 = where + 1;
               neighbour = i2;
            } else {
               nclass = NULL;
            }

/* If there is a target Mapping in the list with which the PcdMap could
   merge, replace the supplied Mappings with swapped Mappings to bring a
   PcdMap closer to the target Mapping. */
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

/* Otherwise, if there was no change in the mapping list, swap the
   Mappings. */
               } else {

                  if( !strcmp( nclass, "ZoomMap" ) ){
                     PcdZoom( (*map_list) + i1, (*invert_list) + i1, where - i1, status );

                  } else if( !strcmp( nclass, "PermMap" ) ){
                     PcdPerm( (*map_list) + i1, (*invert_list) + i1, where - i1, status );
                  }

/* Store the index of the first modified Mapping. */
                  result = i1;
               }

/* If there is no Mapping available for merging, it may still be
   advantageous to swap with a neighbour because the swapped Mapping may
   be simpler than the original Mappings. */
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
                     if( !strcmp( nclass, "ZoomMap" ) ){
                        PcdZoom( mc, ic, where - i1, status );
                     } else if( !strcmp( nclass, "PermMap" ) ){
                        PcdPerm( mc, ic, where - i1, status );
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

/* In parallel. */
/* ============ */
/* PcdMaps cannot combine in parallel with any other Mappings. */
      }
   }

/* Return the result. */
   return result;
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
*     #include "pcdmap.h"
*     void PermGet( AstPermMap *map, int **outperm, int **inperm,
*                   double **const, int *status )

*  Class Membership:
*     PcdMap member function

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

/* Initialise variables to avoid "used of uninitialised variable"
   messages from dumb compilers. */
   nc = 0;

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

/* Look at the mapped positions to determine the output axis permutation
   array. */
   ptr2 = astGetPoints( pset2 );
   if( astOK ){

/* No constant axis valeus found yet. */
      nc = 0;

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

static void SetAttrib( AstObject *this_object, const char *setting, int *status ) {
/*
*  Name:
*     SetAttrib

*  Purpose:
*     Set an attribute value for a PcdMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pcdmap.h"
*     void SetAttrib( AstObject *this, const char *setting, int *status )

*  Class Membership:
*     PcdMap member function (over-rides the astSetAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function assigns an attribute value for a PcdMap, the
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
*        Pointer to the PcdMap.
*     setting
*        Pointer to a null terminated string specifying the new attribute
*        value.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstPcdMap *this;              /* Pointer to the PcdMap structure */
   double dval;                  /* Double attribute value */
   int axis;                     /* Index for the axis */
   int len;                      /* Length of setting string */
   int nc;                       /* Number of characters read by astSscanf */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the PcdMap structure. */
   this = (AstPcdMap *) this_object;

/* Obtain the length of the setting string. */
   len = (int) strlen( setting );

/* Test for each recognised attribute in turn, using "astSscanf" to parse
   the setting string and extract the attribute value (or an offset to
   it in the case of string values). In each case, use the value set
   in "nc" to check that the entire string was matched. Once a value
   has been obtained, use the appropriate method to set it. */

/* Disco. */
/* ------ */
   if ( nc = 0,
        ( 1 == astSscanf( setting, "disco= %lg %n", &dval, &nc ) )
        && ( nc >= len ) ) {
      astSetDisco( this, dval );

/* PcdCen(axis). */
/* ------------ */
   } else if ( nc = 0,
               ( 2 == astSscanf( setting, "pcdcen(%d)= %lg %n",
                              &axis, &dval, &nc ) )
               && ( nc >= len ) ) {
      astSetPcdCen( this, axis - 1, dval );

/* PcdCen. */
/* ------- */
   } else if ( nc = 0,
        ( 1 == astSscanf( setting, "pcdcen= %lg %n", &dval, &nc ) )
        && ( nc >= len ) ) {
      astSetPcdCen( this, 0, dval );
      astSetPcdCen( this, 1, dval );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_setattrib)( this_object, setting, status );
   }

}

static int TestAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for a PcdMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pcdmap.h"
*     int TestAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     PcdMap member function (over-rides the astTestAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     a value has been set for one of a PcdMap's attributes.

*  Parameters:
*     this
*        Pointer to the PcdMap.
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
   AstPcdMap *this;              /* Pointer to the PcdMap structure */
   int axis;                     /* Axis number */
   int len;                      /* Length of attrib string */
   int nc;                       /* No. characters read by astSscanf */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the PcdMap structure. */
   this = (AstPcdMap *) this_object;

/* Obtain the length of the attrib string. */
   len = strlen( attrib );

/* Check the attribute name and test the appropriate attribute. */

/* Disco. */
/* ------ */
   if ( !strcmp( attrib, "disco" ) ) {
      result = astTestDisco( this );

/* PcdCen. */
/* ------- */
   } else if ( !strcmp( attrib, "pcdcen" ) ) {
      result = astTestPcdCen( this, 0 );

/* PcdCen(axis). */
/* ---------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "pcdcen(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      result = astTestPcdCen( this, axis - 1 );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_testattrib)( this_object, attrib, status );
   }

/* Return the result, */
   return result;
}

static AstPointSet *Transform( AstMapping *this, AstPointSet *in,
                               int forward, AstPointSet *out, int *status ) {
/*
*  Name:
*     Transform

*  Purpose:
*     Apply a PcdMap to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pcdmap.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     PcdMap member function (over-rides the astTransform protected
*     method inherited from the Mapping class).

*  Description:
*     This function takes a PcdMap and a set of points encapsulated in a
*     PointSet and transforms the points so as to map them into the
*     required window.

*  Parameters:
*     this
*        Pointer to the PcdMap.
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
*     match the number of coordinates for the PcdMap being applied.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstPointSet *result;          /* Pointer to output PointSet */
   AstPcdMap *map;               /* Pointer to PcdMap to be applied */
   double **ptr_in;              /* Pointer to input coordinate data */
   double **ptr_out;             /* Pointer to output coordinate data */
   double *axin_0;               /* Pointer to next input axis 0 value */
   double *axin_1;               /* Pointer to next input axis 1 value */
   double *axout_0;              /* Pointer to next output axis 0 value */
   double *axout_1;              /* Pointer to next output axis 1 value */
   int npoint;                   /* Number of points */
   int point;                    /* Loop counter for points */
   double dx;                    /* Undistorted X increment from centre  */
   double dy;                    /* Undistorted Y increment from centre */
   double dxp;                   /* Distorted X increment from centre  */
   double dyp;                   /* Distorted Y increment from centre */
   double disco;                 /* Distortion coefficient */
   double cen0;                  /* Centre of distortion on axis 0 */
   double cen1;                  /* Centre of distortion on axis 1 */
   double cr2;                   /* Constant */
   double a;                     /* Constant */
   double cr2a2;                 /* Constant */
   double f;                     /* Expansion factor */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the PcdMap. */
   map = (AstPcdMap *) this;

/* Apply the parent mapping using the stored pointer to the Transform member
   function inherited from the parent Mapping class. This function validates
   all arguments and generates an output PointSet if necessary, but does not
   actually transform any coordinate values. */
   result = (*parent_transform)( this, in, forward, out, status );

/* We will now extend the parent astTransform method by performing the
   calculations needed to generate the output coordinate values. */

/* Determine the numbers of points and coordinates per point from the input
   PointSet and obtain pointers for accessing the input and output coordinate
   values. */
   npoint = astGetNpoint( in );
   ptr_in = astGetPoints( in );
   ptr_out = astGetPoints( result );

/* Determine whether to apply the forward or inverse mapping, according to the
   direction specified and whether the mapping has been inverted. */
   if ( astGetInvert( map ) ) forward = !forward;

/* Get the distortion coefficient and centre. */
   disco = astGetDisco( map );
   cen0 = astGetPcdCen( map, 0 );
   cen1 = astGetPcdCen( map, 1 );

/* Perform coordinate arithmetic. */
/* ------------------------------ */
   if( astOK ){

/* Store pointers to the first input and output values on both axes. */
      axin_0 = ptr_in[ 0 ];
      axin_1 = ptr_in[ 1 ];
      axout_0 = ptr_out[ 0 ];
      axout_1 = ptr_out[ 1 ];

/* First deal with forward transformations. */
      if( forward ){

         for( point = 0; point < npoint; point++ ){
            if( *axin_0 != AST__BAD && *axin_1 != AST__BAD ){
               dx = ( *axin_0 - cen0 );
               dy = ( *axin_1 - cen1 );
               f = 1.0 + disco*( dx*dx + dy*dy );
               dxp = dx*f;
               dyp = dy*f;
               *(axout_0++) = dxp + cen0;
               *(axout_1++) = dyp + cen1;

            } else {
               *(axout_0++) = AST__BAD;
               *(axout_1++) = AST__BAD;
            }
            axin_0++;
            axin_1++;
         }

/* Now deal with inverse transformations. */
      } else {

         for( point = 0; point < npoint; point++ ){
            if( *axin_0 != AST__BAD && *axin_1 != AST__BAD ){
               dxp = ( *axin_0 - cen0 );
               dyp = ( *axin_1 - cen1 );

               cr2 = disco*( dxp*dxp + dyp*dyp );
               a = ( 2.0*cr2 + 1.0 )/( 3.0*cr2 + 1.0 );
               cr2a2 = cr2*a*a;
               f = ( 2.0*cr2a2*a + 1.0 )/( 3.0*cr2a2 + 1.0 );

               dx = dxp*f;
               dy = dyp*f;

/* The above approximate inverse is taken from SLA_UNPCD. If more accuracy
c   is needed, the following code can be uncommented to iterate to a better
c   solution.
c
c               fl = 1.0 + disco*( dx*dx + dy*dy );
c               dx = dxp/fl;
c               dy = dyp/fl;
c
c               df = fabs( fl );
c               while( df > fabs( fl*1.0E-6 ) ){
c                  f = 1.0 + disco*( dx*dx + dy*dy );
c                  df = fabs( f - fl );
c                  fl = f;
c
c                  dx = dxp/f;
c                  dy = dyp/f;
c               }
*/
               *(axout_0++) = dx + cen0;
               *(axout_1++) = dy + cen1;

            } else {
               *(axout_0++) = AST__BAD;
               *(axout_1++) = AST__BAD;
            }
            axin_0++;
            axin_1++;
         }

      }

   }

/* Return a pointer to the output PointSet. */
   return result;
}

static void PcdZoom( AstMapping **maps, int *inverts, int ipc, int *status ){
/*
*  Name:
*     PcdZoom

*  Purpose:
*     Swap a PcdMap and a ZoomMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pcdmap.h"
*     void PcdZoom( AstMapping **maps, int *inverts, int ipc, int *status )

*  Class Membership:
*     PcdMap member function

*  Description:
*     A list of two Mappings is supplied containing a PcdMap and a
*     ZoomMap. These Mappings are annulled, and replaced with
*     another pair of Mappings consisting of a PcdMap and a ZoomMap
*     in the opposite order. These Mappings are chosen so that their
*     combined effect is the same as the original pair of Mappings.

*  Parameters:
*     maps
*        A pointer to an array of two Mapping pointers.
*     inverts
*        A pointer to an array of two invert flags.
*     ipc
*        The index within "maps" of the PcdMap.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstPcdMap *pm2;               /* Pointer to the returned PcdMap */
   AstPcdMap *pm;                /* Pointer to the supplied PcdMap */
   AstZoomMap *zm2;              /* Pointer to the returned ZoomMap */
   AstZoomMap *zm;               /* Pointer to the supplied ZoomMap */
   double cen[2];                /* New distortion centre */
   double disc;                  /* Distortion coeff. for returned PcdMap */
   double disco;                 /* Distortion coeff. for supplied PcdMap */
   double xcen;                  /* Axis 0 centre for supplied PcdMap */
   double ycen;                  /* Axis 1 centre for supplied PcdMap */
   double zoom;                  /* Zoom factor for supplied ZoomMap */
   int old_pinv;                 /* Invert value for the supplied PcdMap */
   int old_zinv;                 /* Invert value for the supplied ZoomMap */

/* Check the global error status. */
   if ( !astOK ) return;

/* Store pointers to the supplied PcdMap and the ZoomMap. */
   pm = (AstPcdMap *) maps[ ipc ];
   zm = (AstZoomMap *) maps[ 1 - ipc ];

/* Temporarily set the Invert attribute of the supplied Mappings to the
   supplied values. */
   old_pinv = astGetInvert( pm );
   astSetInvert( pm, inverts[ ipc ] );

   old_zinv = astGetInvert( zm );
   astSetInvert( zm, inverts[ 1 - ipc ] );

/* Get the zoom factor from the ZoomMap. */
   zoom = astGetZoom( zm );

/* Get the distortion coefficient from the PcdMap. */
   disco = astGetDisco( pm );

/* Get the distortion centre from the PcdMap. */
   xcen = astGetPcdCen( pm, 0 );
   ycen = astGetPcdCen( pm, 1 );

/* Re-instate the original value of the Invert attributes of the supplied
   Mappings. */
   astSetInvert( pm, old_pinv );
   astSetInvert( zm, old_zinv );

/* Create the returned ZoomMap. */
   zm2 = astZoomMap( 2, zoom, "", status );

/* Find the attributes of the returned PcdMap. If the PCD map is applied
   first... */
   if( ipc == 0 ) {
      cen[ 0 ] = xcen*zoom;
      cen[ 1 ] = ycen*zoom;
      disc = disco/(zoom*zoom);

/* If the PCD map is applied second... */
   } else {
      cen[ 0 ] = xcen/zoom;
      cen[ 1 ] = ycen/zoom;
      disc = disco*zoom*zoom;
   }

/* Create the returned PcdMap. */
   pm2 = astPcdMap( disc, cen, "", status );
   if( inverts[ ipc ] ) astInvert( pm2 );

/* Replace the supplied Mappings with the ones created above, swapping the
   order. */
   if( astOK ){
      (void) astAnnul( pm );
      (void) astAnnul( zm );

      maps[ 1 - ipc ] = (AstMapping *) pm2;
      inverts[ 1 - ipc  ] = inverts[ ipc ];

      maps[ ipc ] = (AstMapping *) zm2;
      inverts[ ipc ] = 0;

   }

/* Return. */
   return;
}

static void PcdPerm( AstMapping **maps, int *inverts, int ipc, int *status ){
/*
*  Name:
*     PcdPerm

*  Purpose:
*     Swap a PcdMap and a PermMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pcdmap.h"
*     void PcdPerm( AstMapping **maps, int *inverts, int ipc, int *status )

*  Class Membership:
*     PcdMap member function

*  Description:
*     A list of two Mappings is supplied containing a PcdMap and a
*     PermMap. These Mappings are annulled, and replaced with
*     another pair of Mappings consisting of a PcdMap and a PermMap
*     in the opposite order. These Mappings are chosen so that their
*     combined effect is the same as the original pair of Mappings.

*  Parameters:
*     maps
*        A pointer to an array of two Mapping pointers.
*     inverts
*        A pointer to an array of two invert flags.
*     ipc
*        The index within "maps" of the PcdMap.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     -  It should have been checked previously that the PermMap simply
*     swaps axes 0 and 1. This is the only sort of PermMap which can be
*     used here.

*/

   AstPermMap *rm;               /* Pointer to the supplied PermMap */
   AstPermMap *rm2;              /* Pointer to the returned PermMap */
   AstPcdMap *pm;                /* Pointer to the supplied PcdMap */
   AstPcdMap *pm2;               /* Pointer to the returned PcdMap */
   double cen[2];                /* Centre for new PcdMap */
   double disco;                 /* Distortion coeff. for supplied PcdMap */
   double xcen;                  /* Axis 0 centre for supplied PcdMap */
   double ycen;                  /* Axis 1 centre for supplied PcdMap */
   int old_rinv;                 /* Invert value for the supplied PermMap */
   int old_pinv;                 /* Invert value for the supplied PcdMap */

/* Check the global error status. */
   if ( !astOK ) return;

/* Store pointers to the supplied PcdMap and the PermMap. */
   pm = (AstPcdMap *) maps[ ipc ];
   rm = (AstPermMap *) maps[ 1 - ipc ];

/* Temporarily set the Invert attribute of the supplied Mappings to the
   supplied values. */
   old_pinv = astGetInvert( pm );
   astSetInvert( pm, inverts[ ipc ] );

   old_rinv = astGetInvert( rm );
   astSetInvert( rm, inverts[ 1 - ipc ] );

/* Get the distortion coefficient from the PcdMap. */
   disco = astGetDisco( pm );

/* Get the distortion centre from the PcdMap. */
   xcen = astGetPcdCen( pm, 0 );
   ycen = astGetPcdCen( pm, 1 );

/* Re-instate the original value of the Invert attributes of the supplied
   Mappings. */
   astSetInvert( pm, old_pinv );
   astSetInvert( rm, old_rinv );

/* Create the returned PermMap (unchanged). */
   rm2 = astClone( rm );

/* Create the returned PcdMap. */
   cen[ 0 ] = ycen;
   cen[ 1 ] = xcen;
   pm2 = astPcdMap( disco, cen, "", status );
   if( inverts[ ipc ] ) astInvert( pm2 );

/* Replace the supplied Mappings with the ones created above, swapping the
   order. */
   if( astOK ){
      (void) astAnnul( pm );
      (void) astAnnul( rm );

      old_pinv = inverts[ ipc ];

      maps[ ipc ] = (AstMapping *) rm2;
      inverts[ ipc ] = inverts[ 1 - ipc ];

      maps[ 1 - ipc ] = (AstMapping *) pm2;
      inverts[ 1 - ipc  ] = old_pinv;
   }

/* Return. */
   return;
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
*     Disco

*  Purpose:
*     PcdMap pincushion/barrel distortion coefficient.

*  Type:
*     Public attribute.

*  Synopsis:
*     Double precision.

*  Description:
*     This attribute specifies the pincushion/barrel distortion coefficient
*     used by a PcdMap. This coefficient is set when the PcdMap is created,
*     but may later be modified. If the attribute is cleared, its default
*     value is zero, which gives no distortion. For pincushion distortion,
*     the value should be positive. For barrel distortion, it should be
*     negative.
*
*     Note that the forward transformation of a PcdMap applies the
*     distortion specified by this attribute and the inverse
*     transformation removes this distortion. If the PcdMap is inverted
c     (e.g. using astInvert), then the forward transformation will
f     (e.g. using AST_INVERT), then the forward transformation will
*     remove the distortion and the inverse transformation will apply
*     it. The distortion itself will still be given by the same value of
*     Disco.

*  Applicability:
*     PcdMap
*        All PcdMaps have this attribute.

*att--
*/
/* This ia a double value with a value of AST__BAD when undefined but
   yielding a default of 0.0. */
astMAKE_CLEAR(PcdMap,Disco,disco,AST__BAD)
astMAKE_GET(PcdMap,Disco,double,0.0,( ( this->disco == AST__BAD ) ?
                                      0.0 : this->disco ))
astMAKE_SET(PcdMap,Disco,double,disco,value)
astMAKE_TEST(PcdMap,Disco,( this->disco != AST__BAD ))


/*
*att++
*  Name:
*     PcdCen(axis)

*  Purpose:
*     Centre coordinates of pincushion/barrel distortion.

*  Type:
*     Public attribute.

*  Synopsis:
*     Floating point.

*  Description:
*     This attribute specifies the centre of the pincushion/barrel
*     distortion implemented by a PcdMap. It takes a separate value for
*     each axis of the PcdMap so that, for instance, the settings
*     "PcdCen(1)=345.0,PcdCen(2)=-104.4" specify that the pincushion
*     distortion is centred at positions of 345.0 and -104.4 on axes 1 and 2
*     respectively. This attribute is set when a PcdMap is created, but may
*     later be modified. If the attribute is cleared, the default value for
*     both axes is zero.

*  Applicability:
*     PcdMap
*        All PcdMaps have this attribute.

*  Notes:
*     - If no axis is specified, (e.g. "PcdCen" instead of
*     "PcdCen(2)"), then a "set" or "clear" operation will affect
*     the attribute value of both axes, while a "get" or "test"
*     operation will use just the PcdCen(1) value.
*att--
*/
/* The centre of the distortion. AST__BAD is stored if no value has been
   supplied, resulting in default values of zero being used. */
MAKE_CLEAR(PcdCen,pcdcen,AST__BAD,2)
MAKE_SET(PcdCen,double,pcdcen,value,2)
MAKE_TEST(PcdCen,( this->pcdcen[axis] != AST__BAD ),2)
MAKE_GET(PcdCen,double,0.0,(( this->pcdcen[axis] == AST__BAD ) ?
                              0.0 : this->pcdcen[axis] ),2)

/* Copy constructor. */
/* ----------------- */
/* No copy constructor is needed, as a byte-by-byte copy suffices. */

/* Destructor. */
/* ----------- */
/* No destructor is needed as no memory, etc. needs freeing. */

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for PcdMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the PcdMap class to an output Channel.

*  Parameters:
*     this
*        Pointer to the PcdMap whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstPcdMap *this;              /* Pointer to the PcdMap structure */
   double dval;                  /* Attribute value */
   int set;                      /* Is a value set? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the PcdMap structure. */
   this = (AstPcdMap *) this_object;

/* Write out values representing the instance variables for the
   PcdMap class.  Accompany these with appropriate comment strings,
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

/* PcdCen(0). */
/* ---------- */
   set = TestPcdCen( this, 0, status );
   dval = set ? GetPcdCen( this, 0, status ) : astGetPcdCen( this, 0 );
   astWriteDouble( channel, "PcdCn0", set, 1, dval, "Distortion centre on first axis" );

/* PcdCen(1). */
/* ---------- */
   set = TestPcdCen( this, 1, status );
   dval = set ? GetPcdCen( this, 1, status ) : astGetPcdCen( this, 1 );
   astWriteDouble( channel, "PcdCn1", set, 1, dval, "Distortion centre on second axis" );

/* Disco. */
/* ------ */
   set = TestDisco( this, status );
   dval = set ? GetDisco( this, status ) : astGetDisco( this );
   astWriteDouble( channel, "Disco", set, 1, dval, "Distortion coefficient" );

}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAPcdMap and astCheckPcdMap functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(PcdMap,Mapping)
astMAKE_CHECK(PcdMap)

AstPcdMap *astPcdMap_( double disco, const double pcdcen[2],
                       const char *options, int *status, ...) {
/*
*++
*  Name:
c     astPcdMap
f     AST_PCDMAP

*  Purpose:
*     Create a PcdMap.

*  Type:
*     Public function.

*  Synopsis:
c     #include "pcdmap.h"
c     AstPcdMap *astPcdMap( double disco, const double pcdcen[2],
c                           const char *options, ... )
f     RESULT = AST_PCDMAP( DISCO, PCDCEN, OPTIONS, STATUS )

*  Class Membership:
*     PcdMap constructor.

*  Description:
*     This function creates a new PcdMap and optionally initialises its
*     attributes.
*
*     A PcdMap is a non-linear Mapping which transforms 2-dimensional
*     positions to correct for the radial distortion introduced by some
*     cameras and telescopes. This can take the form either of pincushion
*     or barrel distortion, and is characterized by a single distortion
*     coefficient.
*
*     A PcdMap is specified by giving this distortion coefficient and the
*     coordinates of the centre of the radial distortion. The forward
*     transformation of a PcdMap applies the distortion:
*
*        RD = R * ( 1 + C * R * R )
*
*     where R is the undistorted radial distance from the distortion
*     centre (specified by attribute PcdCen), RD is the radial distance
*     from the same centre in the presence of distortion, and C is the
*     distortion coefficient (given by attribute Disco).
*
*     The inverse transformation of a PcdMap removes the distortion
*     produced by the forward transformation. The expression used to derive
*     R from RD is an approximate inverse of the expression above, obtained
*     from two iterations of the Newton-Raphson method. The mismatch between
*     the forward and inverse expressions is negligible for astrometric
*     applications (to reach 1 milliarcsec at the edge of the Anglo-Australian
*     Telescope triplet or a Schmidt field would require field diameters of
*     2.4 and 42 degrees respectively).
*
c     If a PcdMap is inverted (e.g. using astInvert) then the roles of the
f     If a PcdMap is inverted (e.g. using AST_INVERT) then the roles of the
*     forward and inverse transformations are reversed; the forward
*     transformation will remove the distortion, and the inverse
*     transformation will apply it.

*  Parameters:
c     disco
f     DISCO = DOUBLE PRECISION (Given)
*        The distortion coefficient. Negative values give barrel
*        distortion, positive values give pincushion distortion, and
*        zero gives no distortion.
c     pcdcen
f     PCDCEN( 2 ) = DOUBLE PRECISION (Given)
c        A 2-element array containing the coordinates of the centre of the
f        An array containing the coordinates of the centre of the
*        distortion.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new PcdMap. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new PcdMap. The syntax used is identical to that for the
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
c     astPcdMap()
f     AST_PCDMAP = INTEGER
*        A pointer to the new PcdMap.

*  Notes:
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
   AstPcdMap *new;              /* Pointer to new PcdMap */
   va_list args;                /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the PcdMap, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitPcdMap( NULL, sizeof( AstPcdMap ), !class_init, &class_vtab,
                        "PcdMap", disco, pcdcen );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new PcdMap's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new PcdMap. */
   return new;
}

AstPcdMap *astPcdMapId_( double disco, const double pcdcen[2],
                         const char *options, ... ) {
/*
*  Name:
*     astPcdMapId_

*  Purpose:
*     Create a PcdMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "pcdmap.h"
*     AstPcdMap *astPcdMapId_( double disco, const double pcdcen[2],
*                              const char *options, ... )

*  Class Membership:
*     PcdMap constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astPcdMap constructor function. It returns an ID value (instead
*     of a true C pointer) to external users, and must be provided
*     because astPcdMap_ has a variable argument list which cannot be
*     encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     The variable argument list also prevents this function from
*     invoking astPcdMap_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astPcdMap_.

*  Returned Value:
*     The ID value associated with the new PcdMap.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstPcdMap *new;              /* Pointer to new PcdMap */
   va_list args;                /* Variable argument list */

   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the PcdMap, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitPcdMap( NULL, sizeof( AstPcdMap ), !class_init, &class_vtab,
                        "PcdMap", disco, pcdcen );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new PcdMap's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new PcdMap. */
   return astMakeId( new );
}

AstPcdMap *astInitPcdMap_( void *mem, size_t size, int init,
                           AstPcdMapVtab *vtab, const char *name,
                           double disco, const double pcdcen[2], int *status ){
/*
*+
*  Name:
*     astInitPcdMap

*  Purpose:
*     Initialise a PcdMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "pcdmap.h"
*     AstPcdMap *astInitPcdMap( void *mem, size_t size, int init,
*                               AstPcdMapVtab *vtab, const char *name,
*                               double disco, const double pcdcen[2] )

*  Class Membership:
*     PcdMap initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new PcdMap object. It allocates memory (if necessary) to accommodate
*     the PcdMap plus any additional data associated with the derived class.
*     It then initialises a PcdMap structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a PcdMap at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the PcdMap is to be initialised.
*        This must be of sufficient size to accommodate the PcdMap data
*        (sizeof(PcdMap)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the PcdMap (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the PcdMap
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the PcdMap's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new PcdMap.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the astGetClass
*        method).
*     disco
*        Distortion coefficient.
*     pcdcen
*        Distortion centre.

*  Returned Value:
*     A pointer to the new PcdMap.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstPcdMap *new;              /* Pointer to new PcdMap */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitPcdMapVtab( vtab, name );

/* Initialise. */
   new = NULL;

/* Initialise a Mapping structure (the parent class) as the first component
   within the PcdMap structure, allocating memory if necessary. Specify that
   the Mapping should be defined in both the forward and inverse directions. */
   new = (AstPcdMap *) astInitMapping( mem, size, 0,
                                       (AstMappingVtab *) vtab, name,
                                       2, 2, 1, 1 );

   if ( astOK ) {

/* Initialise the PcdMap data. */
/* ---------------------------- */
/* Store the shift and scale for each axis. */
      new->pcdcen[0] = pcdcen[0];
      new->pcdcen[1] = pcdcen[1];
      new->disco = disco;

/* If an error occurred, clean up by deleting the new PcdMap. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new PcdMap. */
   return new;
}

AstPcdMap *astLoadPcdMap_( void *mem, size_t size,
                           AstPcdMapVtab *vtab, const char *name,
                           AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadPcdMap

*  Purpose:
*     Load a PcdMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "pcdmap.h"
*     AstPcdMap *astLoadPcdMap( void *mem, size_t size,
*                               AstPcdMapVtab *vtab, const char *name,
*                               AstChannel *channel )

*  Class Membership:
*     PcdMap loader.

*  Description:
*     This function is provided to load a new PcdMap using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     PcdMap structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a PcdMap at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the PcdMap is to be
*        loaded.  This must be of sufficient size to accommodate the
*        PcdMap data (sizeof(PcdMap)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the PcdMap (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the PcdMap structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstPcdMap) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new PcdMap. If this is NULL, a pointer
*        to the (static) virtual function table for the PcdMap class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "PcdMap" is used instead.

*  Returned Value:
*     A pointer to the new PcdMap.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstPcdMap *new;              /* Pointer to the new PcdMap */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this PcdMap. In this case the
   PcdMap belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstPcdMap );
      vtab = &class_vtab;
      name = "PcdMap";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitPcdMapVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built PcdMap. */
   new = astLoadMapping( mem, size, (AstMappingVtab *) vtab, name,
                         channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "PcdMap" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* PcdCen(0). */
/* ---------- */
      new->pcdcen[0] = astReadDouble( channel, "pcdcn0", AST__BAD );
      if ( TestPcdCen( new, 0, status ) ) SetPcdCen( new, 0, new->pcdcen[0], status );

/* PcdCen(1). */
/* ---------- */
      new->pcdcen[1] = astReadDouble( channel, "pcdcn1", AST__BAD );
      if ( TestPcdCen( new, 1, status ) ) SetPcdCen( new, 1, new->pcdcen[1], status );

/* Disco. */
/* ------ */
      new->disco = astReadDouble( channel, "disco", AST__BAD );
      if ( TestDisco( new, status ) ) SetDisco( new, new->disco, status );

/* If an error occurred, clean up by deleting the new PcdMap. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new PcdMap pointer. */
   return new;
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





