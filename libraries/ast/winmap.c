/*
*class++
*  Name:
*     WinMap

*  Purpose:
*     Map one window on to another by scaling and shifting each axis.

*  Constructor Function:
c     astWinMap
f     AST_WINMAP

*  Description:
*     A Winmap is a linear Mapping which transforms a rectangular
*     window in one coordinate system into a similar window in another
*     coordinate system by scaling and shifting each axis (the window
*     edges being parallel to the coordinate axes).
*
*     A WinMap is specified by giving the coordinates of two opposite
*     corners (A and B) of the window in both the input and output
*     coordinate systems.

*  Inheritance:
*     The WinMap class inherits from the Mapping class.

*  Attributes:
*     The WinMap class does not define any new attributes beyond those
*     which are applicable to all Mappings.

*  Functions:
c     The WinMap class does not define any new functions beyond those
f     The WinMap class does not define any new routines beyond those
*     which are applicable to all Mappings.

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
*     DSB: David Berry (Starlink)
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     23-OCT-1996 (DSB):
*        Original version.
*     4-MAR-1997 (RFWS):
*        Tidied public prologues.
*     11-MAR-1997 (DSB):
*        Added MapMerge method and associated bits.
*     30-JUN-1997 (DSB):
*        Bug fixed which caused the MapMerge method to generate a
*        segmentation violation.
*     24-MAR-1998 (RFWS):
*        Improved output format from Dump.
*     9-APR-1998 (DSB):
*        MapMerge modified to allow merging of WinMaps with ZoomMaps and
*        and UnitMaps in parallel.
*     4-SEP-1998 (DSB):
*        Improved MapMerge so that WinMaps can change places with a wider
*        range of PermMaps, allowing them to approach closer to a Mapping
*        with which they can merge.
*     22-FEB-1999 (DSB):
*        Corrected logic of MapMerge method to avoid infinite looping.
*     5-MAY-1999 (DSB):
*        More corrections to MapMerge: Cleared up errors in the use of the
*        supplied invert flags, and corrected logic for deciding which
*        neighbouring Mapping to swap with.
*     16-JUL-1999 (DSB):
*        Fixed memory leaks in WinMat and MapMerge.
*     8-JAN-2003 (DSB):
*        Changed private InitVtab method to protected astInitWinMapVtab
*        method.
*     8-SEP-2003 (DSB):
*        Allow WinMaps to swap with WcsMaps if possible.
*     10-NOV-2003 (DSB):
*        Modified functions which swap a WinMap with another Mapping
*        (e.g. WinPerm, etc), to simplify the returned Mappings.
*     23-APR-2004 (DSB):
*        Changes to simplification algorithm.
*     1-SEP-2004 (DSB):
*        Ensure do1 and do2 are initialised before use in MapMerge.
*     7-SEP-2005 (DSB):
*        Take account of the Invert flag when using the soom factor from
*        a ZoomMap.
*     14-FEB-2006 (DSB):
*        Override astGetObjSize.
*     15-MAR-2006 (DSB):
*        Override astEqual.
*     23-AUG-2006 (DSB):
*        Correct initialisation of "result" in the Equal function.
*     19-JAN-2007 (DSB):
*        Fix memory leak.
*     3-MAY-2013 (DSB):
*        Improve simplification by adding check for inverse pairs of
*        WinMaps in function WinWin.
*     23-APR-2015 (DSB):
*        Improve MapMerge. If a WinMap can merge with its next-but-one
*        neighbour, then swap the WinMap with its neighbour, so that
*        it is then next its next-but-one neighbour, and then merge the
*        two Mappings into a single Mapping. Previously, only the swap
*        was performed - not the merger. And the swap was only performed
*        if the intervening neighbour could not itself merge. This could
*        result in an infinite simplification loop, which was detected by
*        CmpMap and and aborted, resulting in no useful simplification.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS WinMap

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory management facilities */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* Sets of points/coordinates */
#include "matrixmap.h"           /* Linear mappings */
#include "unitmap.h"             /* Unit mappings */
#include "zoommap.h"             /* Zoom mappings */
#include "permmap.h"             /* Axis permutations */
#include "cmpmap.h"              /* Compound mappings */
#include "wcsmap.h"              /* Celestial projections */
#include "mapping.h"             /* Coordinate mappings (parent class) */
#include "channel.h"             /* I/O channels */
#include "winmap.h"              /* Interface definition for this class */

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
static int (* parent_getobjsize)( AstObject *, int * );
static AstPointSet *(* parent_transform)( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static const char *(* parent_getattrib)( AstObject *, const char *, int * );
static int (* parent_testattrib)( AstObject *, const char *, int * );
static void (* parent_clearattrib)( AstObject *, const char *, int * );
static void (* parent_setattrib)( AstObject *, const char *, int * );


#ifdef THREAD_SAFE
/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(WinMap)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(WinMap,Class_Init)
#define class_vtab astGLOBAL(WinMap,Class_Vtab)


#include <pthread.h>


#else


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstWinMapVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstWinMap *astWinMapId_( int, const double [], const double [],
                         const double [], const double [], const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */

static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static AstWinMap *WinUnit( AstWinMap *, AstUnitMap *, int, int, int * );
static AstWinMap *WinWin( AstMapping *, AstMapping *, int, int, int, int * );
static AstWinMap *WinZoom( AstWinMap *, AstZoomMap *, int, int, int, int, int * );
static int GetObjSize( AstObject *, int * );
static const char *GetAttrib( AstObject *, const char *, int * );
static double Rate( AstMapping *, double *, int, int, int * );
static int CanSwap( AstMapping *, AstMapping *, int, int, int *, int * );
static int Equal( AstObject *, AstObject *, int * );
static int GetIsLinear( AstMapping *, int * );
static int MapMerge( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
static int TestAttrib( AstObject *, const char *, int * );
static int WinTerms( AstWinMap *, double **, double **, int * );
static void ClearAttrib( AstObject *, const char *, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void PermGet( AstPermMap *, int **, int **, double **, int * );
static void SetAttrib( AstObject *, const char *, int * );
static void WinMat( AstMapping **, int *, int, int * );
static void WinPerm( AstMapping **, int *, int, int * );
static void WinWcs( AstMapping **, int *, int, int * );
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
*     #include "winmap.h"
*     int CanSwap( AstMapping *map1, AstMapping *map2, int inv1, int inv2,
*                  int *simpler, int *status )

*  Class Membership:
*     WinMap member function

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
*     -  One of the supplied pair of Mappings must be a WinMap.
*     -  A value of 0 is returned if the two Mappings could be merged into
*     a single Mapping.
*     -  A value of 0 is returned if an error has already occurred, or if
*     this function should fail for any reason.
*/

/* Local Variables: */
   AstMapping *nowin;        /* Pointer to non-WinMap Mapping */
   AstWinMap *win;           /* Pointer to the WinMap */
   const char *class1;       /* Pointer to map1 class string */
   const char *class2;       /* Pointer to map2 class string */
   const char *nowin_class;  /* Pointer to non-WinMap class string */
   double *consts;           /* Pointer to constants array */
   int *inperm;              /* Pointer to input axis permutation array */
   int *outperm;             /* Pointer to output axis permutation array */
   int axlat;                /* Latitude axis in WcsMap */
   int axlon;                /* Longitude axis in WcsMap */
   int i;                    /* Loop count */
   int invert[ 2 ];          /* Original invert flags */
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

/* Get a pointer to the non-WinMap Mapping. */
      if( !strcmp( class1, "WinMap" ) ){
         nowin = map2;
         nowin_class = class2;
         win = (AstWinMap *) map1;
      } else {
         nowin = map1;
         nowin_class = class1;
         win = (AstWinMap *) map2;
      }

/* If it is a MatrixMap, the Mappings can be swapped. */
      if( !strcmp( nowin_class, "MatrixMap" ) ){
         ret = 1;

/* If it is a WcsMap, the Mappings can be swapped if the WinMap is
   equivalent to a unit transformation on the celestial axes of the
   WcsMap. */
      } else if( !strcmp( nowin_class, "WcsMap" ) ){

/* Get the indices of the celestial coordinates inthe WcsMap. */
         axlat = astGetWcsAxis( (AstWcsMap *) nowin, 1 );
         axlon = astGetWcsAxis( (AstWcsMap *) nowin, 0 );

/* Check the shift and scale for these axes. */
         ret = ( win->a[ axlon ] == 0.0 && win->b[ axlon ] == 1.0 &&
                 win->a[ axlat ] == 0.0 && win->b[ axlat ] == 1.0 );

/* If it is a PermMap, the Mappings can be swapped so long as all links
   between input and output axes in the PermMap are bi-directional. This
   does not preclude the existence of unconnected axes, which do not
   have links (bi-directional or otherwise). */
      } else if( !strcmp( nowin_class, "PermMap" ) ){

/* Get the number of input and output coordinates. */
         nin = astGetNin( nowin );
         nout = astGetNout( nowin );

/* We need to know the axis permutation arrays and constants array for
   the PermMap. */
         PermGet( (AstPermMap *) nowin, &outperm, &inperm, &consts, status );
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

/* If we can swap with the PermMap, the swapped Mappings may be
   intrinsically simpler than the original mappings. */
            if( ret ) {

/* If the PermMap precedes the WinMap, this will be the case if the PermMap
   has more outputs than inputs. If the WinMap precedes the PermMap, this
   will be the case if the PermMap has more inputs than outputs. */
               *simpler = ( nowin == map1 ) ? nout > nin : nin > nout;
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

static void ClearAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for a WinMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "winmap.h"
*     void ClearAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     WinMap member function (over-rides the astClearAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function clears the value of a specified attribute for a
*     WinMap, so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the WinMap.
*     attrib
*        Pointer to a null-terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* At the moment the WinMap class has no attributes, so pass it on to the
   parent method for further interpretation. */
   (*parent_clearattrib)( this_object, attrib, status );

}

static int Equal( AstObject *this_object, AstObject *that_object, int *status ) {
/*
*  Name:
*     Equal

*  Purpose:
*     Test if two WinMaps are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "winmap.h"
*     int Equal( AstObject *this, AstObject *that, int *status )

*  Class Membership:
*     WinMap member function (over-rides the astEqual protected
*     method inherited from the astMapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two WinMaps are equivalent.

*  Parameters:
*     this
*        Pointer to the first Object (a WinMap).
*     that
*        Pointer to the second Object.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the WinMaps are equivalent, zero otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstWinMap *that;
   AstWinMap *this;
   double *a_that;
   double *a_this;
   double *b_that;
   double *b_this;
   int i;
   int nin;
   int result;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain pointers to the two WinMap structures. */
   this = (AstWinMap *) this_object;
   that = (AstWinMap *) that_object;

/* Check the second object is a WinMap. We know the first is a
   WinMap since we have arrived at this implementation of the virtual
   function. */
   if( astIsAWinMap( that ) ) {

/* Get the number of inputs and outputs and check they are the same for both. */
      nin = astGetNin( this );
      if( astGetNin( that ) == nin ) {

/* Assume the WinMaps are equivalent. */
         result = 1;

/* Compare the shift and scale terms from both WinMaps ignoring the
   setting of the Invert flag for the moment. */
         for( i = 0; i < nin; i++ ) {
            if( !astEQUAL( this->a[ i ], that->a[ i ] ) ||
                !astEQUAL( this->b[ i ], that->b[ i ] ) ) {
               result = 0;
               break;
            }
         }

/* If the scale and shifts are equal, check the Invert flags are equal. */
         if( result ) {
            result= ( astGetInvert( this ) == astGetInvert( that ) );

/* If the scale and shifts differ, there is still a chance that the
   WinMaps may be equivalent if their Invert flags differ. */
         } else if( astGetInvert( this ) != astGetInvert( that ) ) {

/* Create copies of the scale and shift terms from the two WinMaps, taking
   into account the setting of the Invert attribute. Finding the inverted
   terms involves arithmetic which introduces rounding errors, so this
   test is not as reliable as the above direct comparison of terms. */
            astWinTerms( this, &a_this, &b_this );
            astWinTerms( that, &a_that, &b_that );
            result = 1;

            for( i = 0; i < nin; i++ ) {
               if( !astEQUAL( a_this[ i ], a_that[ i ] ) ||
                   !astEQUAL( b_this[ i ], b_that[ i ] ) ) {
                  result = 0;
                  break;
               }
            }

/* Free resources */
            a_this = astFree( a_this );
            a_that = astFree( a_that );
            b_this = astFree( b_this );
            b_that = astFree( b_that );
         }
      }
   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static int GetIsLinear( AstMapping *this_mapping, int *status ){
/*
*  Name:
*     GetIsLinear

*  Purpose:
*     Return the value of the IsLinear attribute for a WinMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     void GetIsLinear( AstMapping *this, int *status )

*  Class Membership:
*     WinMap member function (over-rides the protected astGetIsLinear
*     method inherited from the Mapping class).

*  Description:
*     This function returns the value of the IsLinear attribute for a
*     Frame, which is always one.

*  Parameters:
*     this
*        Pointer to the WinMap.
*     status
*        Pointer to the inherited status variable.
*/
   return 1;
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
*     #include "winmap.h"
*     int GetObjSize( AstObject *this, int *status )

*  Class Membership:
*     WinMap member function (over-rides the astGetObjSize protected
*     method inherited from the parent class).

*  Description:
*     This function returns the in-memory size of the supplied WinMap,
*     in bytes.

*  Parameters:
*     this
*        Pointer to the WinMap.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The Object size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstWinMap *this;         /* Pointer to WinMap structure */
   int result;                /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointers to the WinMap structure. */
   this = (AstWinMap *) this_object;

/* Invoke the GetObjSize method inherited from the parent class, and then
   add on any components of the class structure defined by thsi class
   which are stored in dynamically allocated memory. */
   result = (*parent_getobjsize)( this_object, status );
   result += astTSizeOf( this->a );
   result += astTSizeOf( this->b );

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
*     Get the value of a specified attribute for a WinMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "winmap.h"
*     const char *GetAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     WinMap member function (over-rides the protected astGetAttrib
*     method inherited from the Mapping class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for a WinMap, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the WinMap.
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
*     within the WinMap, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the WinMap. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Constants: */
#define BUFF_LEN 50              /* Max. characters in result buffer */

/* Local Variables: */
   const char *result;           /* Pointer value to return */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* At the moment the WinMap class has no attributes, so pass it on to the
   parent method for further interpretation. */
   result = (*parent_getattrib)( this_object, attrib, status );

/* Return the result. */
   return result;

/* Undefine macros local to this function. */
#undef BUFF_LEN
}

void astInitWinMapVtab_(  AstWinMapVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitWinMapVtab

*  Purpose:
*     Initialise a virtual function table for a WinMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "winmap.h"
*     void astInitWinMapVtab( AstWinMapVtab *vtab, const char *name )

*  Class Membership:
*     WinMap vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the WinMap class.

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
   will be used (by astIsAWinMap) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstMappingVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */
   vtab->WinTerms = WinTerms;

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
   mapping->MapSplit = MapSplit;
   mapping->Rate = Rate;
   mapping->GetIsLinear = GetIsLinear;

/* Declare the class dump, copy and delete functions.*/
   astSetDump( vtab, Dump, "WinMap", "Map one window on to another" );
   astSetCopy( (AstObjectVtab *) vtab, Copy );
   astSetDelete( (AstObjectVtab *) vtab, Delete );

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
*     Simplify a sequence of Mappings containing a WinMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     int MapMerge( AstMapping *this, int where, int series, int *nmap,
*                   AstMapping ***map_list, int **invert_list, int *status )

*  Class Membership:
*     WinMap method (over-rides the protected astMapMerge method
*     inherited from the Mapping class).

*  Description:
*     This function attempts to simplify a sequence of Mappings by
*     merging a nominated WinMap in the sequence with its neighbours,
*     so as to shorten the sequence if possible.
*
*     In many cases, simplification will not be possible and the
*     function will return -1 to indicate this, without further
*     action.
*
*     In most cases of interest, however, this function will either
*     attempt to replace the nominated WinMap with a Mapping which it
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
*        Pointer to the nominated WinMap which is to be merged with
*        its neighbours. This should be a cloned copy of the WinMap
*        pointer contained in the array element "(*map_list)[where]"
*        (see below). This pointer will not be annulled, and the
*        WinMap it identifies will not be modified by this function.
*     where
*        Index in the "*map_list" array (below) at which the pointer
*        to the nominated WinMap resides.
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
   AstCmpMap *cm;        /* Pointer to neighbouring CmpMap */
   AstMapping **maplt;   /* New mappings list pointer */
   AstMapping *map2;     /* Pointer to replacement Mapping */
   AstMapping *mc[2];    /* Copies of supplied Mappings to swap */
   AstMapping *nc[2];    /* Copies of neighbouring Mappings to merge */
   AstMapping *smc0;     /* Simplified Mapping */
   AstMapping *smc1;     /* Simplified Mapping */
   AstMapping *simp1;    /* Simplified Mapping */
   AstMapping *simp2;    /* Simplified Mapping */
   AstMatrixMap *mtr;    /* Pointer to replacement MatrixMap */
   AstWinMap *newwm2;    /* Second component WinMap */
   AstWinMap *newwm;     /* Pointer to replacement WinMap */
   AstWinMap *oldwm;     /* Pointer to supplied WinMap */
   const char *class1;   /* Pointer to first Mapping class string */
   const char *class2;   /* Pointer to second Mapping class string */
   const char *nclass;   /* Pointer to neighbouring Mapping class */
   double *a;            /* Pointer to zero terms */
   double *b;            /* Pointer to scale terms */
   int *invlt;           /* New invert flags list pointer */
   int cmlow;            /* Is lower neighbour a CmpMap? */
   int diag;             /* Is WinMap equivalent to a diagonal matrix? */
   int do1;              /* Would a backward swap make a simplification? */
   int do2;              /* Would a forward swap make a simplification? */
   int i1;               /* Index of first WinMap to merge */
   int i2;               /* Index of last WinMap to merge */
   int i;                /* Loop counter */
   int ic[2];            /* Copies of supplied invert flags to swap */
   int inc[4];           /* Copies of supplied invert flags to merge */
   int invert;           /* Should the inverted Mapping be used? */
   int nin2;             /* No. of inputs for second component WinMap */
   int nin;              /* Number of coordinates for WinMap */
   int nmapt;            /* No. of Mappings in list */
   int nstep1;           /* No. of Mappings backwards to next mergable Mapping */
   int nstep2;           /* No. of Mappings forward to next mergable Mapping */
   int old_winv;         /* original Invert value for supplied WinMap */
   int result;           /* Result value to return */
   int ser;              /* Are Mappings applied in series? */
   int simpler;          /* Is the resulting Mapping simpler than original? */
   int swap;             /* Is there an advantage in swapping mappings? */
   int swaphi;           /* Can WinMap be swapped with higher neighbour? */
   int swaplo;           /* Can WinMap be swapped with lower neighbour? */

/* Initialise. */
   result = -1;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Initialise variables to avoid "used of uninitialised variable"
   messages from dumb compilers. */
   i1 = 0;
   i2 = 0;

/* Get the number of axes for the WinMap. */
   nin = astGetNin( ( *map_list )[ where ] );

/* Get a pointer to the WinMap. */
   oldwm = (AstWinMap *) this;

/* First of all, see if the WinMap can be replaced by a simpler Mapping,
   without reference to the neighbouring Mappings in the list.           */
/* ======================================================================*/
/* If the shift terms in the WinMap are all zero, the WinMap can be
   replaced by a diagonal MatrixMap (which is faster to compute). Check the
   shift terms. */
   diag = 1;
   newwm = (AstWinMap *) ( *map_list )[ where ];
   for( i = 0; i < nin; i++ ){
      if( !astEQUAL( ( newwm->a )[ i ], 0.0 ) ){
         diag = 0;
         break;
      }
   }

/* If all the shift terms are zero... */
   if( diag ){

/* Temporarily set the Invert attribute of the WinMap to the supplied
   value. */
      old_winv = astGetInvert( newwm );
      astSetInvert( newwm, ( *invert_list )[ where ] );

/* Get a copy of the scale terms from the WinMap. */
      astWinTerms( newwm, NULL, &b );

/* Create a diagonal MatrixMap holding the scale terms. */
      mtr = astMatrixMap( nin, nin, 1, b, "", status );

/* Restore the Invert attribute of the supplied WinMap. */
      astSetInvert( newwm, old_winv );

/* Free the memory used to hold the scale terms. */
      b = (double *) astFree( (void *) b );

/* Annul the WinMap pointer in the list and replace it with the MatrixMap
   pointer, and indicate that the forward transformation of the returned
   MatrixMap should be used. */
      (void) astAnnul( ( *map_list )[ where ] );
      ( *map_list )[ where ] = (AstMapping *) mtr;
      ( *invert_list )[ where ] = 0;

/* Return the index of the first modified element. */
      result = where;

/* If the WinMap itself could not be simplified, see if it can be merged
   with the Mappings on either side of it in the list. */
   } else {

/* Store the classes of the neighbouring Mappings in the list. */
       class1 = ( where > 0 ) ? astGetClass( ( *map_list )[ where - 1 ] ) : NULL;
       class2 = ( where < *nmap - 1 ) ? astGetClass( ( *map_list )[ where + 1 ] ) : NULL;

/* In series. */
/* ========== */
      if ( series ) {

/* We first look to see if the WinMap can be merged with one of its
   neighbours, resulting in a reduction of one in the number of Mappings
   in the list. WinMaps can only merge directly with another WinMap, a
   ZoomMap, or a UnitMap. */
         if( class1 && ( !strcmp( class1, "WinMap" ) ||
                         !strcmp( class1, "ZoomMap" ) ||
                         !strcmp( class1, "UnitMap" ) ) ){
            nclass = class1;
            i1 = where - 1;
            i2 = where;

         } else if( class2 && ( !strcmp( class2, "WinMap" ) ||
                                !strcmp( class2, "ZoomMap" ) ||
                                !strcmp( class2, "UnitMap" ) ) ){
            nclass = class2;
            i1 = where;
            i2 = where + 1;

         } else {
            nclass = NULL;
         }

/* If the WinMap can merge with one of its neighbours, create the merged
   Mapping. */
         if( nclass ){

            if( !strcmp( nclass, "WinMap" ) ){
               newwm = WinWin( ( *map_list )[ i1 ], ( *map_list )[ i2 ],
                               ( *invert_list )[ i1 ], ( *invert_list )[ i2 ],
                               1, status );
               invert = 0;

            } else if( !strcmp( nclass, "ZoomMap" ) ){
               if( i1 == where ){
                  newwm = WinZoom( (AstWinMap *)( *map_list )[ i1 ],
                                   (AstZoomMap *)( *map_list )[ i2 ],
                           ( *invert_list )[ i1 ], ( *invert_list )[ i2 ], 1, 1, status );
               } else {
                  newwm = WinZoom( (AstWinMap *)( *map_list )[ i2 ],
                                   (AstZoomMap *)( *map_list )[ i1 ],
                           ( *invert_list )[ i2 ], ( *invert_list )[ i1 ], 0, 1, status );
               }
               invert = 0;

            } else {
               newwm = astClone( ( *map_list )[ where ] );
               invert = ( *invert_list )[ where ];
            }

/* If succesfull... */
            if( astOK ){

/* Annul the first of the two Mappings, and replace it with the merged
   WinMap. Also set the invert flag. */
               (void) astAnnul( ( *map_list )[ i1 ] );
               ( *map_list )[ i1 ] = (AstMapping *) newwm;
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

/* If one of the neighbours is a (parallel) CmpMap, we convert the WinMap
   into an equivalent parallel CmpMap, and then merge this parallel
   CmpMap with the neighbouring parallel CmpMap to create a parallel CmpMap
   containing two series CmpMaps. */
         } else if( ( class1 && !strcmp( "CmpMap", class1 ) ) ||
                    ( class2 && !strcmp( "CmpMap", class2 ) ) ) {

/* Identify the WinMap and the CmpMap. */
            if( class1 && !strcmp( "CmpMap", class1 ) ) {
               i1 = where - 1;
               i2 = where;
               cm = (AstCmpMap *) ( *map_list )[ where - 1 ];
               cmlow = 1;

            } else {
               i1 = where;
               i2 = where + 1;
               cm = (AstCmpMap *) ( *map_list )[ where + 1 ];
               cmlow = 0;

            }

/* Temporarily set the required Invert attributes in the two Mappings. */
            inc[ 0 ] = astGetInvert( ( *map_list )[ i1 ] );
            astSetInvert( ( *map_list )[ i1 ], ( *invert_list )[ i1 ] );

            inc[ 1 ] = astGetInvert( ( *map_list )[ i2 ] );
            astSetInvert( ( *map_list )[ i2 ], ( *invert_list )[ i2 ] );

/* Now get pointers to the scale and zero terms of the nominated WinMap
   (these describe the forward transformation, taking into account the
   setting of the Invert flag). */
            (void) astWinTerms( oldwm , &a, &b );

/* Get pointers to the two components of the parallel CmpMap. */
            astDecompose( cm, mc, mc + 1, &ser, ic, ic + 1 );

/* Check component Mappings are combined in parallel. */
            map2 = NULL;
            if( astOK && !ser ) {

/* Temporarily set the required Invert attributes in the two component
   Mappings to the indicated values. */
               inc[ 2 ] = astGetInvert( mc[ 0 ] );
               astSetInvert( mc[ 0 ], ic[ 0 ] );

               inc[ 3 ] = astGetInvert( mc[ 1 ] );
               astSetInvert( mc[ 1 ], ic[ 1 ] );

/* Create the first of two corresponding WinMaps, initially with undefined
   corners. These could be combined into a parallel CmpMap which would be
   equivalent to the nominated WinMap. The number of inputs for each WinMap
   is equal to either the number of outputs or inputs of the corresponding
   component of the CmpMap, depending on whether the CmpMap is upper or lower
   neighbour. */
               nin = cmlow ? astGetNout( mc[ 0 ] ):astGetNin( mc[ 0 ] );
               newwm = astWinMap( nin, NULL, NULL, NULL, NULL, "", status );
               if( astOK ) {

/* Store the first "nin" scale and zero terms from the nominated WinMap
   in the new WinMap. */
                  for( i = 0; i < nin; i++ ) {
                     (newwm->a)[ i ] = a[ i ];
                     (newwm->b)[ i ] = b[ i ];
                  }
               }

/* Now create the second WinMap in the same way, which transforms the
   remaining outputs of the CmpMap. */
               nin2 = cmlow ? astGetNout( mc[ 1 ] ):astGetNin( mc[ 1 ] );
               newwm2 = astWinMap( nin2, NULL, NULL, NULL, NULL, "", status );
               if( astOK ) {

/* Store the remaining scale and zero terms from the nominated WinMap
   in the new WinMap. */
                  for( i = 0; i < nin2; i++ ) {
                     (newwm2->a)[ i ] = a[ i + nin ];
                     (newwm2->b)[ i ] = b[ i + nin ];
                  }
               }

/* Combine the two corresponding lower component Mappings into a series
   CmpMap, and likewise combine the two corresponding upper component
   Mappings into a series CmpMap. */
               if( cmlow ) {
                  nc[ 0 ] = (AstMapping *) astCmpMap( mc[ 0 ], newwm, 1, "", status );
                  nc[ 1 ] = (AstMapping *) astCmpMap( mc[ 1 ], newwm2, 1, "", status );
               } else {
                  nc[ 0 ] = (AstMapping *) astCmpMap( newwm, mc[ 0 ], 1, "", status );
                  nc[ 1 ] = (AstMapping *) astCmpMap( newwm2, mc[ 1 ], 1, "", status );
               }
               newwm = astAnnul( newwm );
               newwm2 = astAnnul( newwm2 );

/* Attempt to simplify each of the two new series CmpMaps. If neither of
   them simplify then there is no point in doing the current merger. In fact
   it would be dangerous to do so since we may end up in an infinite loop
   where the resulting parallel CmpMap gets converted back into the
   existing series CmpMap by the CmpMap MapMerge method, and then back
   again by this method, etc. */
               simp1 = astSimplify( nc[ 0 ] );
               simp2 = astSimplify( nc[ 1 ] );

/* Test if either could be simplified by checking if its pointer value
   has changed. */
               simpler = ( simp1 != nc[ 0 ] ) || ( simp2 != nc[ 1 ] );

/* If either CmpMap was simplified, then combine the two series CmpMap into
   a single parallel CmpMap. */
               if( simpler ) {
                  map2 = (AstMapping *) astCmpMap( simp1, simp2, 0, "", status );
               }

/* Re-instate the original Invert attributes in the two component Mappings. */
               astSetInvert( mc[ 0 ], inc[ 2 ] );
               astSetInvert( mc[ 1 ], inc[ 3 ] );

/* Free resources. */
               simp1 = astAnnul( simp1 );
               simp2 = astAnnul( simp2 );
               nc[ 0 ] = astAnnul( nc[ 0 ] );
               nc[ 1 ] = astAnnul( nc[ 1 ] );

            }

/* Free resources. */
            mc[ 0 ] = astAnnul( mc[ 0 ] );
            mc[ 1 ] = astAnnul( mc[ 1 ] );
            a = astFree( a );
            b = astFree( b );

/* Re-instate the original Invert attributes. */
            astSetInvert( ( *map_list )[ i1 ], inc[ 0 ] );
            astSetInvert( ( *map_list )[ i2 ], inc[ 1 ] );

/* If the above produced a new Mapping, annul the supplied pointers for
   the two merged Mappings, store the pointer for the new merged Mapping,
   and shuffle the remaining Mappings down to fill the space left. Nullify
   the end slot which is no longer used, reduce the number of Mappings in
   the list by 1, and return the index of the first modified Mapping. */
            if( map2 ) {
               (void) astAnnul( ( *map_list )[ i1 ] );
               (void) astAnnul( ( *map_list )[ i2 ] );
               ( *map_list )[ i1 ] = map2;
               ( *invert_list )[ i1 ] = 0;
               for( i = i2 + 1; i < *nmap; i++ ){
                  ( *map_list )[ i - 1 ] = ( *map_list )[ i ];
                  ( *invert_list )[ i - 1 ] = ( *invert_list )[ i ];
               }
               ( *map_list )[ *nmap - 1 ] = NULL;
               (*nmap)--;
               result = i1;
            }

/* If the WinMap could not merge directly with either of its neighbours,
   we consider whether it would be worthwhile to swap the WinMap with
   either of its neighbours. This can only be done for certain classes
   of Mapping (MatrixMap & some PermMaps & WcsMaps), and will usually require both
   Mappings to be modified (unless they are commutative). The advantage of
   swapping the order of the Mappings is that it may result in the WinMap
   being adjacent to a Mapping with which it can merge directly on the next
   invocation of this function, thus reducing the number of Mappings
   in the list. */
         } else {

/* Set a flag if we could swap the WinMap with its higher neighbour. "do2"
   is returned if swapping the Mappings would simplify either of the
   Mappings. */
            if( where + 1 < *nmap ){
               swaphi = CanSwap(  ( *map_list )[ where ],
                                  ( *map_list )[ where + 1 ],
                                  ( *invert_list )[ where ],
                                  ( *invert_list )[ where + 1 ], &do2, status );
            } else {
               swaphi = 0;
               do2 = 0;
            }

/* If so, step through each of the Mappings which follow the WinMap,
   looking for a Mapping with which the WinMap could merge directly. Stop
   when such a Mapping is found, or if a Mapping is found with which the
   WinMap could definitely not swap. Note the number of Mappings which
   separate the WinMap from the Mapping with which it could merge (if
   any). */
            nstep2 = -1;
            if( swaphi ){
               for( i2 = where + 1; i2 < *nmap; i2++ ){

/* See if we can merge with this Mapping. If so, note the number of steps
   between the two Mappings and leave the loop. */
                  nclass = astGetClass( ( *map_list )[ i2 ] );
                  if( !strcmp( nclass, "WinMap" ) ||
                      !strcmp( nclass, "ZoomMap" ) ||
                      !strcmp( nclass, "UnitMap" ) ) {
                     nstep2 = i2 - where - 1;
                     break;
                  }

/* If there is no chance that we can swap with this Mapping, leave the loop
   with -1 for the number of steps to indicate that no merging is possible.
   WinMaps can swap with MatrixMaps and some PermMaps. */
                  if( strcmp( nclass, "MatrixMap" ) &&
                      strcmp( nclass, "WcsMap" ) &&
                      strcmp( nclass, "PermMap" ) ) {
                     break;
                  }

               }

            }

/* Do the same working forward from the WinMap towards the start of the map
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
                  if( !strcmp( nclass, "WinMap" ) ||
                      !strcmp( nclass, "ZoomMap" ) ||
                      !strcmp( nclass, "UnitMap" ) ) {
                     nstep1 = where - 1 - i1;
                     break;
                  }

                  if( strcmp( nclass, "MatrixMap" ) &&
                      strcmp( nclass, "WcsMap" ) &&
                      strcmp( nclass, "PermMap" ) ) {
                     break;
                  }

               }

            }

/* Choose which neighbour to swap with so that the WinMap moves towards the
   nearest Mapping with which it can merge. */
            if( do1 || (
                nstep1 != -1 && ( nstep2 == -1 || nstep2 > nstep1 ) ) ){
               nclass = class1;
               i1 = where - 1;
               i2 = where;
            } else if( do2 || nstep2 != -1 ){
               nclass = class2;
               i1 = where;
               i2 = where + 1;
            } else {
               nclass = NULL;
            }

/* If there is a target Mapping in the list with which the WinMap could
   merge, replace the supplied Mappings with swapped Mappings to bring a
   WinMap closer to the target Mapping. */
            if( nclass ){

/* Swap the Mappings. */
               if( !strcmp( nclass, "MatrixMap" ) ){
                  WinMat( (*map_list) + i1, (*invert_list) + i1, where - i1, status );

               } else if( !strcmp( nclass, "PermMap" ) ){
                  WinPerm( (*map_list) + i1, (*invert_list) + i1, where - i1, status );

               } else if( !strcmp( nclass, "WcsMap" ) ){
                  WinWcs( (*map_list) + i1, (*invert_list) + i1, where - i1, status );
               }

/* And then merge them if possible. */
               if( where == i1 && where + 1 < *nmap ) {    /* Merging upwards */
                  map2 = astClone( (*map_list)[ where + 1 ] );
                  nmapt = *nmap - where - 1;
                  maplt = *map_list + where + 1;
                  invlt = *invert_list + where + 1;

                  (void) astMapMerge( map2, 0, series, &nmapt, &maplt, &invlt );
                  map2 = astAnnul( map2 );
                  *nmap = where + 1 + nmapt;

               } else if( where - 2 >= 0 ) {               /* Merging downwards */
                  map2 = astClone( (*map_list)[ where - 2 ] );
                  nmapt = *nmap - where + 2;
                  maplt = *map_list + where - 2 ;
                  invlt = *invert_list + where - 2;

                  (void) astMapMerge( map2, 0, series, &nmapt, &maplt, &invlt );
                  map2 = astAnnul( map2 );
                  *nmap = where - 2 + nmapt;
               }

               result = i1;

/* If there is no Mapping available for merging, it may still be
   advantageous to swap with a neighbour because the swapped Mapping may
   be simpler than the original Mappings. For instance, a PermMap may
   strip axes of the WinMap leaving only a UnitMap. Also, the two neighbours
   may be able to merge. */
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
                     if( !strcmp( nclass, "MatrixMap" ) ){
                        WinMat( mc, ic, where - i1, status );
                     } else if( !strcmp( nclass, "PermMap" ) ){
                        WinPerm( mc, ic, where - i1, status );
                     } else if( !strcmp( nclass, "WcsMap" ) ){
                        WinWcs( mc, ic, where - i1, status );
                     }

/* See if the two neighbouring Mappings can merge now that the nominated
   Mapping is no longer in between them. First get a list of Mapping
   pointers containing the two Mappings to be merged, and associated
   invert flags. */
                     if( i == 0 && where != *nmap - 1 ) {
                        nc[ 0 ] = astClone( mc[ 1 ] );
                        nc[ 1 ] = astClone( (*map_list)[ where + 1 ] );
                        inc[ 0 ] = ic[ 1 ];
                        inc[ 1 ] = (*invert_list)[ where + 1 ];

                     } else if( i == 1 && where > 0 ) {
                        nc[ 0 ] = astClone( (*map_list)[ where - 1 ] );
                        nc[ 1 ] = astClone( mc[ 0 ] );
                        inc[ 0 ] = (*invert_list)[ where - 1 ];
                        inc[ 1 ] = ic[ 0 ];

                     } else {
                        nc[ 0 ] = NULL;
                        nc[ 1 ] = NULL;
                     }

/* If both neighbours are available, use astMapMerge to see if it is
   possible to merge the two Mappings. */
                     swap = 0;
                     if( nc[ 0 ] && nc[ 1 ] ) {
                        nmapt = 2;
                        maplt = nc;
                        invlt = inc;
                        map2 = astClone( nc[ 0 ] );
                        swap = astMapMerge( map2, 0, series, &nmapt, &maplt, &invlt );
                        map2 = astAnnul( map2 );
                        if( swap == -1 ) {
                           map2 = astClone( nc[ 1 ] );
                           swap = astMapMerge( map2, 1, series, &nmapt, &maplt, &invlt );
                           map2 = astAnnul( map2 );
                        }
                        swap = ( nmapt < 2 ) ? 1 : 0;
                     }

/* Free resources. */
                     if( nc[ 0 ] ) nc[ 0 ] = astAnnul( nc[ 0 ] );
                     if( nc[ 1 ] ) nc[ 1 ] = astAnnul( nc[ 1 ] );

/* If the neighbours could not merge, see if either swapped Mapping can
   be simplified. */
                     if( !swap ) {
                        smc0 = astSimplify( mc[0] );
                        if(  smc0 != mc[0] ) {
                           swap = 1;
                        } else {
                           smc1 = astSimplify( mc[1] );
                           swap = ( smc1 != mc[1] );
                           smc1 = astAnnul( smc1 );
                        }
                        smc0 = astAnnul( smc0 );
                     }

/* If there is some point in swapping the Mappings, swap them in the
   supplied lists. Otherwise annul the swapped Mappings. */
                     if( swap ) {
                        (*map_list)[ i1 ] = astAnnul( (*map_list)[ i1 ] );
                        (*map_list)[ i2 ] = astAnnul( (*map_list)[ i2 ] );
                        (*map_list)[ i1 ] = mc[ 0 ];
                        (*map_list)[ i2 ] = mc[ 1 ];
                        (*invert_list)[ i1 ] = ic[ 0 ];
                        (*invert_list)[ i2 ] = ic[ 1 ];
                        result = i1;
                        break;

                     } else {
                        mc[ 0 ] = astAnnul( mc[ 0 ] );
                        mc[ 1 ] = astAnnul( mc[ 1 ] );
                     }
                  }
               }
            }
         }

/* In parallel. */
/* ============ */
/* WinMaps are combined in parallel with neighbouring WinMaps, ZoomMaps and
   UnitMaps. */
      } else {

/* We first look to see if the WinMap can be merged with one of its
   neighbours, resulting in a reduction of one in the number of Mappings
   in the list. WinMaps can only merge directly with another WinMap, a
   ZoomMap, or a UnitMap. */
         if( class1 && ( !strcmp( class1, "WinMap" ) ||
                         !strcmp( class1, "ZoomMap" ) ||
                         !strcmp( class1, "UnitMap" ) ) ){
            nclass = class1;
            i1 = where - 1;
            i2 = where;

         } else if( class2 && ( !strcmp( class2, "WinMap" ) ||
                                !strcmp( class2, "ZoomMap" ) ||
                                !strcmp( class2, "UnitMap" ) ) ){
            nclass = class2;
            i1 = where;
            i2 = where + 1;

         } else {
            nclass = NULL;
         }

/* If the WinMap can merge with one of its neighbours, create the merged
   Mapping. */
         if( nclass ){

            if( !strcmp( nclass, "WinMap" ) ){
               newwm = WinWin( ( *map_list )[ i1 ], ( *map_list )[ i2 ],
                               ( *invert_list )[ i1 ], ( *invert_list )[ i2 ],
                               0, status );
               invert = 0;

            } else if( !strcmp( nclass, "ZoomMap" ) ){
               if( i1 == where ){
                  newwm = WinZoom( (AstWinMap *)( *map_list )[ i1 ],
                                   (AstZoomMap *)( *map_list )[ i2 ],
                           ( *invert_list )[ i1 ], ( *invert_list )[ i2 ], 1, 0, status );
               } else {
                  newwm = WinZoom( (AstWinMap *)( *map_list )[ i2 ],
                                   (AstZoomMap *)( *map_list )[ i1 ],
                           ( *invert_list )[ i2 ], ( *invert_list )[ i1 ], 0, 0, status );
               }
               invert = 0;

            } else {
               if( i1 == where ){
                  newwm = WinUnit( (AstWinMap *)( *map_list )[ i1 ],
                                   (AstUnitMap *)( *map_list )[ i2 ],
                                   ( *invert_list )[ i1 ], 1, status );
               } else {
                  newwm = WinUnit( (AstWinMap *)( *map_list )[ i2 ],
                                   (AstUnitMap *)( *map_list )[ i1 ],
                                   ( *invert_list )[ i2 ], 0, status );
               }
               invert = 0;

            }

/* If succesfull... */
            if( astOK ){

/* Annul the first of the two Mappings, and replace it with the merged
   WinMap. Also set the invert flag. */
               (void) astAnnul( ( *map_list )[ i1 ] );
               ( *map_list )[ i1 ] = (AstMapping *) newwm;
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
*     WinMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "winmap.h"
*     int *MapSplit( AstMapping *this, int nin, const int *in, AstMapping **map, int *status )

*  Class Membership:
*     WinMap method (over-rides the protected astMapSplit method
*     inherited from the Mapping class).

*  Description:
*     This function creates a new Mapping by picking specified inputs from
*     an existing WinMap. This is only possible if the specified inputs
*     correspond to some subset of the WinMap outputs. That is, there
*     must exist a subset of the WinMap outputs for which each output
*     depends only on the selected WinMap inputs, and not on any of the
*     inputs which have not been selected. If this condition is not met
*     by the supplied WinMap, then a NULL Mapping is returned.

*  Parameters:
*     this
*        Pointer to the WinMap to be split (the WinMap is not actually
*        modified by this function).
*     nin
*        The number of inputs to pick from "this".
*     in
*        Pointer to an array of indices (zero based) for the inputs which
*        are to be picked. This array should have "nin" elements. If "Nin"
*        is the number of inputs of the supplied WinMap, then each element
*        should have a value in the range zero to Nin-1.
*     map
*        Address of a location at which to return a pointer to the new
*        Mapping. This Mapping will have "nin" inputs (the number of
*        outputs may be different to "nin"). A NULL pointer will be
*        returned if the supplied WinMap has no subset of outputs which
*        depend only on the selected inputs.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a dynamically allocated array of ints. The number of
*     elements in this array will equal the number of outputs for the
*     returned Mapping. Each element will hold the index of the
*     corresponding output in the supplied WinMap. The array should be
*     freed using astFree when no longer needed. A NULL pointer will
*     be returned if no output Mapping can be created.

*  Notes:
*     - If this function is invoked with the global error status set,
*     or if it should fail for any reason, then NULL values will be
*     returned as the function value and for the "map" pointer.
*/

/* Local Variables: */
   AstWinMap *newwm;          /* Pointer to returned WinMap */
   AstWinMap *this;           /* Pointer to WinMap structure */
   double *a;                 /* Pointer to zero terms */
   double *b;                 /* Pointer to scale terms */
   int *result;               /* Pointer to returned array */
   int i;                     /* Loop count */
   int iin;                   /* Mapping input index */
   int mnin;                  /* No. of Mapping inputs */
   int ok;                    /* Are input indices OK? */

/* Initialise */
   result = NULL;
   *map = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the WinMap structure. */
   this = (AstWinMap *) this_map;

/* Allocate memory for the returned array and create a WinMap with the
   required number of axes and undefined corners. */
   result = astMalloc( sizeof( int )*(size_t) nin );
   newwm = astWinMap( nin, NULL, NULL, NULL, NULL, "", status );
   *map = (AstMapping *) newwm;

/* Now get pointers to the scale and zero terms of the supplied WinMap
   (these describe the forward transformation, taking into account the
   setting of the Invert flag). */
   (void) astWinTerms( this , &a, &b );

/* Check pointers can be used safely. */
   if( astOK ) {

/* Store the required scale and zero terms from the supplied WinMap
   in the new WinMap. At the same time check that each axis is valid. */
      mnin = astGetNin( this );
      ok = 1;
      for( i = 0; i < nin; i++ ) {
         iin = in[ i ];
         if( iin >= 0 && iin < mnin ) {
            (newwm->a)[ i ] = a[ iin ];
            (newwm->b)[ i ] = b[ iin ];
            result[ i ] = iin;
         } else {
            ok = 0;
            break;
         }
      }

/* If the "in" array contained any invalid values, free the returned
   resources. */
      if( !ok ) {
         result = astFree( result );
         *map = astAnnul( *map );
      }
   }

/* Free resources. */
   a = astFree( a );
   b = astFree( b );

/* Free returned resources if an error has occurred. */
   if( !astOK ) {
      result = astFree( result );
      *map = astAnnul( *map );
   }

/* Return the list of output indices. */
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
*     #include "winmap.h"
*     void PermGet( AstPermMap *map, int **outperm, int **inperm,
*                   double **const, int *status )

*  Class Membership:
*     WinMap member function

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

static double Rate( AstMapping *this, double *at, int ax1, int ax2, int *status ){
/*
*  Name:
*     Rate

*  Purpose:
*     Calculate the rate of change of a Mapping output.

*  Type:
*     Private function.

*  Synopsis:
*     #include "winmap.h"
*     result = Rate( AstMapping *this, double *at, int ax1, int ax2, int *status )

*  Class Membership:
*     WinMap member function (overrides the astRate method inherited
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
   AstWinMap *map;
   double result;

/* Check inherited status */
   if( !astOK ) return AST__BAD;

/* Get a pointer to the WinMap structure. */
   map = (AstWinMap *) this;

/* If the input and output axes are not equal the result is zero. */
   if( ax1 != ax2 ) {
      result = 0.0;

/* Otherwise, return the scale factor for the axis, taking the reciprocal
   if the WinMap has been inverted. */
   } else {
      result = ( map->b )[ ax1 ];
      if( astGetInvert( map ) ) {
         if( result != 0.0 && result != AST__BAD ) {
            result = 1.0/result;
         } else {
            result = AST__BAD;
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
*     Set an attribute value for a WinMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "winmap.h"
*     void SetAttrib( AstObject *this, const char *setting )

*  Class Membership:
*     WinMap member function (over-rides the astSetAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function assigns an attribute value for a WinMap, the
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
*        Pointer to the WinMap.
*     setting
*        Pointer to a null-terminated string specifying the new attribute
*        value.
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* The WinMap class currently has no attributes, so pass it on to the parent
   method for further interpretation. */
   (*parent_setattrib)( this_object, setting, status );

}

static int TestAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for a WinMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "winmap.h"
*     int TestAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     WinMap member function (over-rides the astTestAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     a value has been set for one of a WinMap's attributes.

*  Parameters:
*     this
*        Pointer to the WinMap.
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
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* The WinMap class currently has no attributes, so pass it on to the parent
   method for further interpretation. */
   result = (*parent_testattrib)( this_object, attrib, status );

/* Return the result, */
   return result;
}

static AstPointSet *Transform( AstMapping *this, AstPointSet *in,
                               int forward, AstPointSet *out, int *status ) {
/*
*  Name:
*     Transform

*  Purpose:
*     Apply a WinMap to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "winmap.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     WinMap member function (over-rides the astTransform protected
*     method inherited from the Mapping class).

*  Description:
*     This function takes a WinMap and a set of points encapsulated in a
*     PointSet and transforms the points so as to map them into the
*     required window.

*  Parameters:
*     this
*        Pointer to the WinMap.
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
*     match the number of coordinates for the WinMap being applied.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstPointSet *result;          /* Pointer to output PointSet */
   AstWinMap *map;               /* Pointer to WinMap to be applied */
   const char *class;            /* Object class */
   double **ptr_in;              /* Pointer to input coordinate data */
   double **ptr_out;             /* Pointer to output coordinate data */
   double *axin;                 /* Pointer to next input axis value */
   double *axout;                /* Pointer to next output axis value */
   double *a;                    /* Pointer to next constant term */
   double *b;                    /* Pointer to next multiplicative term */
   double aa;                    /* Constant term */
   double bb;                    /* Multiplicative term */
   int coord;                    /* Loop counter for coordinates */
   int def;                      /* Is mapping defined? */
   int ncoord;                   /* Number of coordinates per point */
   int npoint;                   /* Number of points */
   int point;                    /* Loop counter for points */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise variables to avoid "used of uninitialised variable"
   messages from dumb compilers. */
   aa = 0.0;
   bb = 0.0;

/* Obtain a pointer to the WinMap. */
   map = (AstWinMap *) this;

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
   ncoord = astGetNcoord( in );
   npoint = astGetNpoint( in );
   ptr_in = astGetPoints( in );
   ptr_out = astGetPoints( result );

/* Determine whether to apply the forward or inverse mapping, according to the
   direction specified and whether the mapping has been inverted. */
   if ( astGetInvert( map ) ) forward = !forward;

/* Report an error if the WinMap does not contain any scales or shifts. */
   if( !(map->a && map->b) && astOK ){
      class = astGetClass( this );
      astError( AST__BADWM, "astTransform(%s): The supplied %s does not "
                "contain any window information.", status, class, class );
   }

/* Perform coordinate arithmetic. */
/* ------------------------------ */
   if( astOK ){

/* Store pointers to the shift and scale for the next axis. */
      a = map->a;
      b = map->b;

/* Apply the mapping to each axis. */
      for( coord = 0; coord < ncoord; coord++ ){

/* If either the scale or shift is bad indicate that the mapping is
   not defined on this axis. */
         if( *a == AST__BAD || *b == AST__BAD ){
            def = 0;

/* Otherwise, get the scale and offset factors for this axis, taking account of
   whether the mapping is inverted or not. If the mapping is undefined, set
   the "def" flag to indicate this. */
         } else {
            aa = *a;
            bb = *b;

            if( forward ){
               def = 1;

            } else if( bb != 0.0 ){
               bb = 1.0/bb;
               aa = -aa*bb;
               def = 1;

            } else {
               def = 0;
            }

         }

/* Store pointers to the first inpout and output values on this axis. */
         axin = ptr_in[ coord ];
         axout = ptr_out[ coord ];

/* If the mapping is defined, apply it to the supplied points. */
         if( def ){

            for( point = 0; point < npoint; point++ ){
               if( *axin != AST__BAD ){
                  *(axout++) = aa + bb*(*axin);
               } else {
                  *(axout++) = AST__BAD;
               }
               axin++;
            }

/* If the mapping is not defined, store bad values on this axis in the
   returned points. */
         } else {
            for( point = 0; point < npoint; point++ ) *(axout++) = AST__BAD;
         }

/* Point to the scale and shift for the next axis. */
         a++;
         b++;
      }

   }

/* Return a pointer to the output PointSet. */
   return result;
}

static void WinMat( AstMapping **maps, int *inverts, int iwm, int *status ){
/*
*  Name:
*     WinMat

*  Purpose:
*     Swap a WinMap and a MatrixMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "winmap.h"
*     void WinMat( AstMapping **maps, int *inverts, int iwm, int *status )

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
*     iwm
*        The index within "maps" of the WinMap.
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
   wm = (AstWinMap *) maps[ iwm ];
   mm = (AstMatrixMap *) maps[ 1 - iwm ];

/* Temporarily set the Invert attribute of the supplied Mappings to the
   supplied values. */
   old_winv = astGetInvert( wm );
   astSetInvert( wm, inverts[ iwm ] );

   old_minv = astGetInvert( mm );
   astSetInvert( mm, inverts[ 1 - iwm ] );

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
   if( iwm == 0 ){

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

      maps[ 1 - iwm ] = (AstMapping *) sw1;
      inverts[ 1 - iwm  ] = astGetInvert( sw1 );

      sm2 = astSimplify( m2 );
      m2 = astAnnul( m2 );

      maps[ iwm ] = (AstMapping *) sm2;
      inverts[ iwm  ] = astGetInvert( sm2 );

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

static void WinWcs( AstMapping **maps, int *inverts, int iwm, int *status ){
/*
*  Name:
*     WinWcs

*  Purpose:
*     Swap a WinMap and a WcsMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "winmap.h"
*     void WinWcs( AstMapping **maps, int *inverts, int iwm, int *status )

*  Class Membership:
*     WinMap member function

*  Description:
*     A list of two Mappings is supplied containing a WinMap and a
*     WcsMap. These Mappings are swapped.

*  Parameters:
*     maps
*        A pointer to an array of two Mapping pointers.
*     inverts
*        A pointer to an array of two invert flags.
*     iwm
*        The index within "maps" of the WinMap.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstMapping *m1;               /* Pointer to a Mapping */
   int inv;                      /* Invert value */

/* Check the global error status. */
   if ( !astOK ) return;

/* Simply swap the values (the CanSwap function will have checked that
   the WcsMap and WinMap can simply be swapped). */
   m1 = maps[ 0 ];
   maps[ 0 ] = maps[ 1 ];
   maps[ 1 ] = m1;

   inv = inverts[ 0 ];
   inverts[ 0 ] = inverts[ 1 ];
   inverts[ 1 ] = inv;

/* Return. */
   return;
}

static void WinPerm( AstMapping **maps, int *inverts, int iwm, int *status ){
/*
*  Name:
*     WinPerm

*  Purpose:
*     Swap a WinMap and a PermMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "winmap.h"
*     void WinPerm( AstMapping **maps, int *inverts, int iwm, int *status )

*  Class Membership:
*     WinMap member function

*  Description:
*     A list of two Mappings is supplied containing a WinMap and a
*     PermMap. These Mappings are annulled, and replaced with
*     another pair of Mappings consisting of a WinMap and a PermMap
*     in the opposite order. These Mappings are chosen so that their
*     combined effect is the same as the original pair of Mappings.

*  Parameters:
*     maps
*        A pointer to an array of two Mapping pointers.
*     inverts
*        A pointer to an array of two invert flags.
*     iwm
*        The index within "maps" of the WinMap.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     -  All links between input and output axes in the PermMap must
*     be bi-directional, but there can be unconnected axes, and there
*     need not be the same number of input and output axes.

*/

/* Local Variables: */
   AstPermMap *pm;               /* Pointer to the supplied PermMap */
   AstPermMap *p1;               /* Pointer to the returned PermMap */
   AstPermMap *sp1;              /* Pointer to the simplified returned PermMap */
   AstWinMap *w1;                /* Pointer to the returned WinMap */
   AstWinMap *sw1;               /* Pointer to the simplified returned PermMap */
   AstWinMap *wm;                /* Pointer to the supplied WinMap */
   double *a;                    /* Array of shift terms from supplied WinMap */
   double *aa;                   /* Pointer to next shift term */
   double *b;                    /* Array of scale terms from supplied WinMap */
   double *bb;                   /* Pointer to next scale term */
   double *consts;               /* Pointer to constants array */
   double c;                     /* A constant value */
   int *inperm;                  /* Pointer to input axis permutation array */
   int *outperm;                 /* Pointer to output axis permutation array */
   int i;                        /* Axis count */
   int j;                        /* Axis index */
   int nin;                      /* No. of axes in supplied WinMap */
   int npin;                     /* No. of input axes in supplied PermMap */
   int npout;                    /* No. of output axes in supplied PermMap */
   int old_pinv;                 /* Invert value for the supplied PermMap */
   int old_winv;                 /* Invert value for the supplied WinMap */


/* Check the global error status. */
   if ( !astOK ) return;

/* Initialise variables to avoid "used of uninitialised variable"
   messages from dumb compilers. */
   p1 = NULL;
   w1 = NULL;

/* Store pointers to the supplied WinMap and the PermMap. */
   wm = (AstWinMap *) maps[ iwm ];
   pm = (AstPermMap *) maps[ 1 - iwm ];

/* Temporarily set the Invert attribute of the supplied Mappings to the
   supplied values. */
   old_winv = astGetInvert( wm );
   astSetInvert( wm, inverts[ iwm ] );

   old_pinv = astGetInvert( pm );
   astSetInvert( pm, inverts[ 1 - iwm ] );

/* Get copies of the shift and scale terms used by the WinMap. This
   also returns the number of axes in the WinMap. */
   nin = astWinTerms( wm, &a, &b );

/* Get the axis permutation and constants arrays representing the
   PermMap. Note, no constants are used more than once in the returned
   arrays (i.e. duplicate constants are returned in "consts" if more than
   one axis uses a given constant). */
   PermGet( pm, &outperm, &inperm, &consts, status );

   if( astOK ) {

/* Get the number of input and output axes in the PermMap. */
      npin = astGetNin( pm );
      npout = astGetNout( pm );

/* First consider cases where the WinMap is applied first, followed by the
   PermMap. */
      if( iwm == 0 ) {

/* Create the new WinMap, initially with undefined corners. Its number
   of axes will equal the number of output axes of the PermMap. */
         w1 = astWinMap( npout, NULL, NULL, NULL, NULL, "", status );

/* Get pointers to the scale and shift terms for the new WinMap. */
         bb = w1->b;
         aa = w1->a;

/* Thinking of the forward CmpMap first, consider each of the output axes of
   the PermMap. */
         for( i = 0; i < npout; i++ ){

/* If the value for this output axis is derived from an input axis, copy the
   scale and shift terms from the corresponding input axis to the new
   WinMap. */
            j = outperm[ i ];
            if( j >= 0 && j < nin ) {
               aa[ i ] = a[ j ];
               bb[ i ] = b[ j ];

/* If this output axis is assigned a constant value, use zero and one for
   the shift and scale in order to preserve the constant value produced
   by the PermMap. */
            } else {
               aa[ i ] = 0.0;
               bb[ i ] = 1.0;
            }

         }

/* Now consider the inverse CmpMap. Any constants produced by the inverse
   PermMap would previously have been scaled by the inverse WinMap. Since
   there will be no inverse WinMap to perform this scaling in the returned
   Mappings, we need to change the constant values to be the values after
   the scaling which would have been applied by the WinMap. Consider each
   of the input axes of the PermMap.*/
         for( i = 0; i < npin; i++ ){

/* Skip axes which are not assigned a constant value. */
            if( inperm[ i ] < 0 ) {

/* Scale the constant term associated with this input axis using the
   inverse WinMap unless it is AST__BAD. */
               c = consts[ -inperm[ i ] - 1 ];
               if( c != AST__BAD ) {

                  if( a[ i ] != AST__BAD && b[ i ] != AST__BAD &&
                      b[ i ] != 0.0 ) {
                     consts[ -inperm[ i ] - 1 ] = ( c - a[ i ] )/b[ i ];
                  } else {
                     consts[ -inperm[ i ] - 1 ] = AST__BAD;
                  }

               }

            }

         }

/* Now consider cases where the PermMap is applied first, followed by the
   WinMap. */
      } else {

/* Create the new WinMap, initially with undefined corners. Its number
   of axes will equal the number of input axes of the PermMap. */
         w1 = astWinMap( npin, NULL, NULL, NULL, NULL, "", status );

/* Get pointers to the scale and shift terms for the new WinMap. */
         bb = w1->b;
         aa = w1->a;

/* Thinking first about the inverse WinMap, consider each of the input axes
   of the PermMap. */
         for( i = 0; i < npin; i++ ){

/* If the value for this input axis is derived from an output axis, copy the
   scale and shift terms from the corresponding output axis to the new
   WinMap. */
            j = inperm[ i ];
            if( j >= 0 && j < nin ) {
               aa[ i ] = a[ j ];
               bb[ i ] = b[ j ];

/* If this input axis is assigned a constant value, use zero and one for
   the shift and scale in order to preserve the constant value produced
   by the PermMap. */
            } else {
               aa[ i ] = 0.0;
               bb[ i ] = 1.0;
            }

         }

/* Now consider the forward WinMap. Any constants produced by the forward
   PermMap would previously have been scaled by the forward WinMap. Since
   there will be no forward WinMap to perform this scaling in the returned
   Mappings, we need to change the constant values to be the values after
   the scaling which would have been applied by the WinMap. Consider each
   of the output axes of the PermMap.*/
         for( i = 0; i < npout; i++ ){

/* Skip axes which are not assigned a constant value. */
            if( outperm[ i ] < 0 ) {

/* Scale the constant term associated with this input axis using the
   forward WinMap unless it is AST__BAD. */
               c = consts[ -outperm[ i ] - 1 ];
               if( c != AST__BAD ) {

                  if( a[ i ] != AST__BAD && b[ i ] != AST__BAD ) {
                     consts[ -outperm[ i ] - 1 ] = a[ i ] + c*b[ i ];
                  } else {
                     consts[ -outperm[ i ] - 1 ] = AST__BAD;
                  }

               }

            }

         }

      }

/* Create a new PermMap (since the constants may have changed). */
      p1 = astPermMap( npin, inperm, npout, outperm, consts, "", status );

/* Free the axis permutation and constants arrays. */
      outperm = (int *) astFree( (void *) outperm );
      inperm = (int *) astFree( (void *) inperm );
      consts = (double *) astFree( (void *) consts );
   }

/* Re-instate the original value of the Invert attributes of the supplied
   Mappings. */
   astSetInvert( wm, old_winv );
   astSetInvert( pm, old_pinv );

/* Replace the supplied Mappings with the ones created above, swapping the
   order. */
   if( astOK ){
      (void) astAnnul( wm );
      (void) astAnnul( pm );

      sp1 = astSimplify( p1 );
      p1 = astAnnul( p1 );

      sw1 = astSimplify( w1 );
      w1 = astAnnul( w1 );

      maps[ iwm ] = (AstMapping *) sp1;
      inverts[ iwm ] = 0;

      maps[ 1 - iwm ] = (AstMapping *) sw1;
      inverts[ 1 - iwm  ] = astGetInvert( sw1 );
   }

/* Free the copies of the scale and shift terms from the supplied WinMap. */
   b = (double *) astFree( (void *) b );
   a = (double *) astFree( (void *) a );

/* Return. */
   return;
}

static int WinTerms( AstWinMap *this, double **shift, double **scale, int *status ){
/*
*+
*  Name:
*     astWinTerms

*  Purpose:
*     Obtain the scale and shift terms used by a WinMap.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "winmap.h"
*     int astWinTerms( AstWinMap *this, double **shift, double **scale )

*  Class Membership:
*     WinMap mewthod.

*  Description:
*     This function returns copies of the scale and shift terms used by a
*     WinMap when transforming points. Each axis of the WinMap has a scale
*     term B, and a shift term A, and the transformation of a point is done
*     by applying these to each input axis value X in turn, to get the
*     output axis value B.X + A. The returned terms take into account the
*     current setting of the Invert attribute of the WinMap.

*  Parameters:
*     this
*        Pointer to the WinMap.
*     shift
*        The address of a location at which to return a pointer to the
*        start of a dynamically allocated array holding the shift terms
*        for each axis.
*     scale
*        The address of a location at which to return a pointer to the
*        start of a dynamically allocated array holding the scale terms
*        for each axis.

*  Returned Value:
*     The number of axes in the WinMap. This is the same as the number of
*     elements in the returned arrays.

*  Notes:
*     -  The returned arrays should be released using astFree when no
*     longer needed.
*     -  NULL pointers can be supplied for "scale" or "shift" if the
*     corresponding arrays are not required.
*     -  A value of zero will be returned, together with NULL pointers
*     for "scale" and "shift" if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   double *a;             /* Pointer to a copy of the shift term array */
   double *aa;            /* Pointer to the next shift term */
   double *b;             /* Pointer to a copy of the scale term array */
   double *bb;            /* Pointer to the next scale term */
   int i;                 /* Axis count */
   int result;            /* The returned number of axes */
   size_t absize;         /* Size of shift and scale arrays */

/* Initialise. */
   result = 0;
   if( scale ) *scale = NULL;
   if( shift ) *shift = NULL;

/* Check the global status. */
   if ( !astOK ) return result;

/* Get the number of axes in the WinMap. */
   result = astGetNin( this );

/* Create copies of the scale and shift terms from the WinMap. */
   absize = sizeof( double )*(size_t) result;
   b = (double *) astStore( NULL, (void *) this->b, absize );
   a = (double *) astStore( NULL, (void *) this->a, absize );

/* Check the pointers can be used. */
   if( astOK ){

/* If the WinMap is inverted, replace the scale and shift terms
   by the corresponding values for the inverted mapping. */
      if( astGetInvert( this ) ){
         bb = b;
         aa = a;

         for( i = 0; i < result; i++ ){
            if( *aa != AST__BAD && *bb != 0.0 && *bb != AST__BAD ){
               *bb = 1.0/(*bb);
               *aa *= -(*bb);
            } else {
               *bb = AST__BAD;
               *aa = AST__BAD;
            }

            aa++;
            bb++;

         }
      }

/* Store the required pointers, and free arrays which are not required. */
      if( scale ){
         *scale = b;
      } else {
         b = (double *) astFree( (void *) b );
      }

      if( shift ){
         *shift = a;
      } else {
         a = (double *) astFree( (void *) a );
      }

   }

/* If an error has occurred, free the arrays and return zero. */
   if( !astOK ){
      if( scale ) *scale = (double *) astFree( (void *) *scale );
      if( shift ) *shift = (double *) astFree( (void *) *shift );
      result = 0;
   }

/* Return the answer. */
   return result;

}

static AstWinMap *WinUnit( AstWinMap *wm, AstUnitMap *um, int winv,
                           int win1, int *status ){
/*
*  Name:
*     WinUnit

*  Purpose:
*     Create a WinMap by merging a WinMap and a UnitMap in parallel.

*  Type:
*     Private function.

*  Synopsis:
*     #include "winmap.h"
*     AstWinMap *WinUnit( AstWinMap *wm, AstUnitMap *um, int winv, int win1, int *status )

*  Class Membership:
*     WinMap member function

*  Description:
*     This function creates a new WinMap which performs a mapping
*     equivalent to applying the two supplied Mappings in parallel in
*     the directions specified by the "invert" flag (the Invert
*     attribute of the supplied WinMap is ignored).

*  Parameters:
*     wm
*        A pointer to the WinMap.
*     um
*        A pointer to the UnitMap.
*     winv
*        The invert flag to use with wm. A value of zero causes the forward
*        mapping to be used, and a non-zero value causes the inverse
*        mapping to be used.
*     win1
*        Indicates the order in which the Mappings should be applied.
*
*        If win1 is non-zero:
*           "wm" applies to the lower axis indices and "um" to the upper
*           axis indices.
*
*        If win1 is zero:
*           "um" applies to the lower axis indices and "wm" to the upper
*           axis indices.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the new WinMap.

*  Notes:
*     -  The forward direction of the returned WinMap is equivalent to the
*     combined effect of the two supplied Mappings, operating in the
*     directions specified by "winv".
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstWinMap *result;            /* Pointer to output WinMap */
   double *a;                    /* Pointer to shift term array */
   double *aa;                   /* Pointer to next shift term */
   double *ar;                   /* Pointer to next shift term in result */
   double *b;                    /* Pointer to scale term array */
   double *bb;                   /* Pointer to next scale term */
   double *br;                   /* Pointer to next scale term in result */
   int i;                        /* Axis index */
   int ninw;                     /* No. of axes in the WinMap */
   int ninu;                     /* No. of axes in the UnitMap */
   int old_winv;                 /* Original setting of WinMap Invert attribute */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise the returned pointer. */
   result = NULL;

/* Temporarily set the Invert attribute of the WinMap to the supplied
   value. */
   old_winv = astGetInvert( wm );
   astSetInvert( wm, winv );

/* Create copies of the scale and shift terms from the WinMap, and store the
   number of axes in it. */
   ninw = astWinTerms( wm, &a, &b );

/* Get the number of axes in the UnitMap. */
   ninu = astGetNin( um );

/* Create the merged WinMap with unspecified corners. */
   result = astWinMap( ninw + ninu, NULL, NULL, NULL, NULL, "", status );

/* Check the pointers can be used. */
   if( astOK ){

/* If the WinMap applies to the lower axis indices... */
      if( win1 ){

/* Use the scale and shift terms from the WinMap for the lower axes of
   the new WinMap. */
         aa = a;
         bb = b;
         ar = result->a;
         br = result->b;

         for( i = 0; i < ninw; i++ ){
            *(ar++) = *(aa++);
            *(br++) = *(bb++);
         }

/* Use the scale factor to 1.0 and the shift term to zero for the upper axes
   of the new WinMap. */
         for( i = 0; i < ninu; i++ ){
            *(ar++) = 0.0;
            *(br++) = 1.0;
         }

/* If the WinMap applies to the upper axis indices... */
      } else {

/* Use the scale factor to 1.0 and the shift term to zero for the lower axes
   of the new WinMap. */
         ar = result->a;
         br = result->b;

         for( i = 0; i < ninu; i++ ){
            *(ar++) = 0.0;
            *(br++) = 1.0;
         }

/* Use the scale and shift terms from the WinMap for the upper axes of
   the new WinMap. */
         aa = a;
         bb = b;

         for( i = 0; i < ninw; i++ ){
            *(ar++) = *(aa++);
            *(br++) = *(bb++);
         }
      }
   }

/* Free the copies of the scale and shift terms from the supplied WinMap. */
   b = (double *) astFree( (void *) b );
   a = (double *) astFree( (void *) a );

/* Re-instate the original setting of the Invert attribute for the
   supplied WinMap. */
   astSetInvert( wm, old_winv );

/* If an error has occurred, annull the returned WinMap. */
   if( !astOK ) result = astAnnul( result );

/* Return a pointer to the output WinMap. */
   return result;
}

static AstWinMap *WinWin( AstMapping *map1, AstMapping *map2, int inv1,
                          int inv2, int series, int *status ){
/*
*  Name:
*     WinWin

*  Purpose:
*     Create a merged WinMap from two supplied WinMaps.

*  Type:
*     Private function.

*  Synopsis:
*     #include "winmap.h"
*     AstWinMap *WinWin( AstMapping *map1, AstMapping *map2, int inv1,
*                        int inv2, int series, int *status )

*  Class Membership:
*     WinMap member function

*  Description:
*     This function creates a new WinMap which performs a mapping
*     equivalent to applying the two supplied WinMaps either in series
*     or parallel in the directions specified by the "invert" flags
*     (the Invert attributes of the supplied WinMaps are ignored).

*  Parameters:
*     map1
*        A pointer to the WinMap to apply first (if in series), or to the
*        lower axis indices (if in parallel)
*     map2
*        A pointer to the WinMap to apply second (if in series), or to the
*        upper axis indices (if in parallel)
*     inv1
*        The invert flag to use with map1. A value of zero causes the forward
*        mapping to be used, and a non-zero value causes the inverse
*        mapping to be used.
*     inv2
*        The invert flag to use with map2.
*     series
*        If non-zero, then the supplied WinMaps are combined in series.
*        Otherwise, they are combined in parallel.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the new WinMap.

*  Notes:
*     -  The forward direction of the returned WinMap is equivalent to the
*     combined effect of the two supplied WinMap, operating in the
*     directions specified by "inv1" and "inv2".
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstWinMap *result;            /* Pointer to output WinMap */
   AstWinMap *wm1;               /* Pointer to the first supplied WinMap */
   AstWinMap *wm2;               /* Pointer to the second supplied WinMap */
   double *a[ 2 ];               /* Pointers to shift term arrays */
   double *a0;                   /* Pointer to next shift term from WinMap 1 */
   double *a1;                   /* Pointer to next shift term from WinMap 2 */
   double *ar;                   /* Pointer to next shift term in result */
   double *b[ 2 ];               /* Pointers to scale term arrays */
   double *b0;                   /* Pointer to next scale term from WinMap 1 */
   double *b1;                   /* Pointer to next scale term from WinMap 2 */
   double *br;                   /* Pointer to next scale term in result */
   double amean;                 /* Geometric mean of the offset terms */
   int cancel;                   /* Do the two WinMaps cancel out? */
   int i;                        /* Axis index */
   int invert[ 2 ];              /* Array of invert flags */
   int nin[ 2 ];                 /* No. of axes in the two WinMaps */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise the returned pointer. */
   result = NULL;

/* Store pointers to the WinMaps. */
   wm1 = (AstWinMap *) map1;
   wm2 = (AstWinMap *) map2;

/* Temporarily set their Invert attributes to the supplied values. */
   invert[ 0 ] = astGetInvert( wm1 );
   astSetInvert( wm1, inv1 );

   invert[ 1 ] = astGetInvert( wm2 );
   astSetInvert( wm2, inv2 );

/* Create copies of the scale and shift terms from the two WinMaps,
   and store the number of axes in each WinMap. The scale and shift terms
   returned take into account the setting of the Invert attribute. */
   nin[ 0 ] = astWinTerms( wm1, a, b );
   nin[ 1 ] = astWinTerms( wm2, a + 1, b + 1 );

/* Check the pointers can be used. */
   if( astOK ){

/* Series */
/* ====== */
      if( series ){

/* Check for equal and opposite WinMaps. Do this explicitly using the
   supplied Mappings rather than the values returned by astWinTerms to
   avoid the affects of rounding errors in the inversions performed by
   astWinTerms. */
         if( ( inv1 == 0 ) != ( inv2 == 0 ) ) {
            cancel = 1;
            for( i = 0; i < nin[ 0 ]; i++ ){
               if( !astEQUAL( (wm1->a)[ i ], (wm2->a)[ i ] ) ||
                   !astEQUAL( (wm1->b)[ i ], (wm2->b)[ i ] ) ) {
                  cancel = 0;
                  break;
               }
            }
         } else {
            cancel = 0;
         }

/* If they cancel, just put unit values into the WinMap. */
         if( cancel ) {
            a0 = a[ 0 ];
            b0 = b[ 0 ];
            for( i = 0; i < nin[ 0 ]; i++ ){
               *(a0++) = 0.0;
               *(b0++) = 1.0;
            }

/* Otherwise, merge the scale and shift terms for the two WinMaps, overwriting
   the terms for the first WinMap. To be merged in series, both WinMaps must
   have the same number of axes, so it matters not whether we use nin[ 0 ]
   or nin[ 1 ] to specify the number of axes. Include rounding checks for values
   close to a unit mapping. */
         } else {
            a0 = a[ 0 ];
            b0 = b[ 0 ];
            a1 = a[ 1 ];
            b1 = b[ 1 ];
            for( i = 0; i < nin[ 0 ]; i++ ){

               if( *a0 != AST__BAD && *b0 != AST__BAD &&
                   *a1 != AST__BAD && *b1 != AST__BAD ){

                  amean = sqrt(fabs((*a0)*(*a1)));

                  *a0 *= (*b1);
                  *a0 += (*a1);
                  *b0 *= (*b1);

                  if( fabs( *a0 ) < amean*1E-15 ) *a0 = 0.0;
                  if( fabs( *b0 - 1.0 ) < 1E-15 ) *b0 = 1.0;

               } else {
                  *a0 = AST__BAD;
                  *b0 = AST__BAD;
                  *a1 = AST__BAD;
                  *b1 = AST__BAD;
               }

/* Move on to the next axis. */
               a0++;
               b0++;
               a1++;
               b1++;
            }
         }

/* Create the merged WinMap with unspecified corners. */
         result = astWinMap( nin[ 0 ], NULL, NULL, NULL, NULL, "", status );

/* Store the merged scale and shift terms in the new WinMap. The forward
   transformation of this WinMap then corresponds to the combination of the
   two supplied WinMaps, taking into account their invert flags. */
         a0 = a[ 0 ];
         b0 = b[ 0 ];
         ar = result->a;
         br = result->b;
         for( i = 0; i < nin[ 0 ]; i++ ){
            *(ar++) = *(a0++);
            *(br++) = *(b0++);
         }

/* Parallel */
/* ======== */
      } else {

/* Create the merged WinMap with unspecified corners. */
         result = astWinMap( nin[ 0 ] + nin[ 1 ], NULL, NULL, NULL, NULL, "", status );

/* Copy the scale and shift terms into the new WinMap. */
         a0 = a[ 0 ];
         b0 = b[ 0 ];
         a1 = a[ 1 ];
         b1 = b[ 1 ];
         ar = result->a;
         br = result->b;

         for( i = 0; i < nin[ 0 ]; i++ ){
            *(ar++) = *(a0++);
            *(br++) = *(b0++);
         }

         for( i = 0; i < nin[ 1 ]; i++ ){
            *(ar++) = *(a1++);
            *(br++) = *(b1++);
         }
      }
   }

/* Re-instate the original settings of the Invert attributes for the
   supplied WinMaps. */
   astSetInvert( wm1, invert[ 0 ] );
   astSetInvert( wm2, invert[ 1 ] );

/* Free the memory. */
   a[ 0 ] = (double *) astFree( (void *) a[ 0 ] );
   b[ 0 ] = (double *) astFree( (void *) b[ 0 ] );
   a[ 1 ] = (double *) astFree( (void *) a[ 1 ] );
   b[ 1 ] = (double *) astFree( (void *) b[ 1 ] );

/* If an error has occurred, annull the returned WinMap. */
   if( !astOK ) result = astAnnul( result );

/* Return a pointer to the output WinMap. */
   return result;
}

static AstWinMap *WinZoom( AstWinMap *wm, AstZoomMap *zm, int winv,
                           int zinv, int win1, int series, int *status ){
/*
*  Name:
*     WinZoom

*  Purpose:
*     Create a WinMap by merging a WinMap and a ZoomMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "winmap.h"
*     AstWinMap *WinZoom( AstWinMap *wm, AstZoomMap *zm, int winv,
*                         int zinv, int win1, int series, int *status )

*  Class Membership:
*     WinMap member function

*  Description:
*     This function creates a new WinMap which performs a mapping
*     equivalent to applying the two supplied Mappings in series or
*     parallel in the directions specified by the "invert" flags (the
*     Invert attributes of the supplied WinMaps are ignored).

*  Parameters:
*     wm
*        A pointer to the WinMap.
*     zm
*        A pointer to the ZoomMap.
*     winv
*        The invert flag to use with wm. A value of zero causes the forward
*        mapping to be used, and a non-zero value causes the inverse
*        mapping to be used.
*     zinv
*        The invert flag to use with zm.
*     win1
*        Indicates the order in which the Mappings should be applied.
*
*        If win1 is non-zero:
*           If in series:
*              "wm" is applied first followed by "zm".
*           If in parallel:
*              "wm" applies to the lower axis indices and "zm" to the upper
*              axis indices.
*
*        If win1 is zero:
*           If in series:
*              "zm" is applied first followed by "wm".
*           If in parallel:
*              "zm" applies to the lower axis indices and "wm" to the upper
*              axis indices.
*     series
*        Should be supplied non-zero if the Mappings are to be combined in
*        series.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the new WinMap.

*  Notes:
*     -  The forward direction of the returned WinMap is equivalent to the
*     combined effect of the two supplied Mappings, operating in the
*     directions specified by "zinv" and "winv".
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstWinMap *result;            /* Pointer to output WinMap */
   double *a;                    /* Pointer to shift term array */
   double *aa;                   /* Pointer to next shift term */
   double *ar;                   /* Pointer to next shift term in result */
   double *b;                    /* Pointer to scale term array */
   double *bb;                   /* Pointer to next scale term */
   double *br;                   /* Pointer to next scale term in result */
   double zfac;                  /* Zoom factor */
   int i;                        /* Axis index */
   int ninw;                     /* No. of axes in the WinMap */
   int ninz;                     /* No. of axes in the ZoomMap */
   int old_winv;                 /* Original setting of WinMap Invert attribute */
   int old_zinv;                 /* Original setting of ZoomMap Invert attribute */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise the returned pointer. */
   result = NULL;

/* Temporarily set the Invert attributes of both Mappings to the supplied
   values. */
   old_winv = astGetInvert( wm );
   astSetInvert( wm, winv );

   old_zinv = astGetInvert( zm );
   astSetInvert( zm, zinv );

/* Get the zoom factor implemented by the ZoomMap. Invert it if necessary
   since astGetZoom does not take account of the Invert setting.  */
   zfac = astGetZoom( zm );
   if( zinv ) zfac = 1.0 / zfac;

/* Create copies of the scale and shift terms from the WinMap, and store the
   number of axes in it. */
   ninw = astWinTerms( wm, &a, &b );

/* Check the pointers can be used. */
   if( astOK ){

/* First do series mode... */
      if( series ) {

/* Modify the WinMap scale and shift terms by the zoom factor. How this is
   done depends on which way round the Mappings are applied. */
         bb = b;
         aa = a;

         for( i = 0; i < ninw; i++ ){

            if( *aa != AST__BAD && *bb != AST__BAD && zfac != AST__BAD ){
               *bb *= zfac;
               if( win1 ) *aa *= zfac;
            } else {
               *bb = AST__BAD;
               *aa = AST__BAD;
            }

            aa++;
            bb++;
         }

/* Create the merged WinMap with unspecified corners. */
         result = astWinMap( ninw, NULL, NULL, NULL, NULL, "", status );

/* Store the merged scale and shift terms in the new WinMap. The forward
   transformation of this WinMap then corresponds to the combination of the
   two supplied Mappings, taking into account their invert flags. */
         aa = a;
         bb = b;
         ar = result->a;
         br = result->b;
         for( i = 0; i < ninw; i++ ){
            *(ar++) = *(aa++);
            *(br++) = *(bb++);
         }

/* Now do parallel mode... */
      } else {

/* Get the number of axes in the ZoomMap. */
         ninz = astGetNin( zm );

/* Create the merged WinMap with unspecified corners. */
         result = astWinMap( ninw + ninz, NULL, NULL, NULL, NULL, "", status );

/* If the WinMap applies to the lower axis indices... */
         if( win1 ) {

/* Use the scale and shift terms from the WinMap for the lower axes of
   the new WinMap. */
            aa = a;
            bb = b;
            ar = result->a;
            br = result->b;

            for( i = 0; i < ninw; i++ ){
               *(ar++) = *(aa++);
               *(br++) = *(bb++);
            }

/* Use the scale factor (with zero shift) from the ZoomMap for the upper axes
   of the new WinMap. */
            for( i = 0; i < ninz; i++ ){
               *(ar++) = 0.0;
               *(br++) = zfac;
            }

/* If the WinMap applies to the upper axis indices... */
         } else {

/* Use the scale factor (with zero shift) from the ZoomMap for the lower axes
   of the new WinMap. */
            ar = result->a;
            br = result->b;

            for( i = 0; i < ninz; i++ ){
               *(ar++) = 0.0;
               *(br++) = zfac;
            }

/* Use the scale and shift terms from the WinMap for the upper axes of
   the new WinMap. */
            aa = a;
            bb = b;

            for( i = 0; i < ninw; i++ ){
               *(ar++) = *(aa++);
               *(br++) = *(bb++);
            }
         }
      }
   }

/* Free the copies of the scale and shift terms from the supplied WinMap. */
   b = (double *) astFree( (void *) b );
   a = (double *) astFree( (void *) a );

/* Re-instate the original settings of the Invert attribute for the
   supplied Mappings. */
   astSetInvert( wm, old_winv );
   astSetInvert( zm, old_zinv );

/* If an error has occurred, annull the returned WinMap. */
   if( !astOK ) result = astAnnul( result );

/* Return a pointer to the output WinMap. */
   return result;
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
*     Copy constructor for WinMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for WinMap objects.

*  Parameters:
*     objin
*        Pointer to the WinMap to be copied.
*     objout
*        Pointer to the WinMap being constructed.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstWinMap *out;              /* Pointer to output WinMap */
   AstWinMap *in;               /* Pointer to input WinMap */
   int ncoord;                  /* No. of axes for the mapping */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the input and output WinMaps. */
   in= (AstWinMap *) objin;
   out = (AstWinMap *) objout;

/* Get the number of coordinates mapped by the WinMap. */
   ncoord = astGetNin( in );

/* Allocate memory holding copies of the scales and shifts window defining the
   mapping. */
   out->a = (double *) astStore( NULL, (void *) in->a,
                                  sizeof(double)*(size_t)ncoord );
   out->b = (double *) astStore( NULL, (void *) in->b,
                                  sizeof(double)*(size_t)ncoord );

/* If an error occurred, free any allocated memory. */
   if ( !astOK ) {
      out->a = (double *) astFree( (void *) out->a );
      out->b = (double *) astFree( (void *) out->b );
   }

}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for WinMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for WinMap objects.

*  Parameters:
*     obj
*        Pointer to the WinMap to be deleted.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - This destructor does nothing and exists only to maintain a
*     one-to-one correspondence between destructors and copy
*     constructors.
*/

/* Local Variables: */
   AstWinMap *this;              /* Pointer to WinMap */

/* Obtain a pointer to the WinMap structure. */
   this = (AstWinMap *) obj;

/* Free the memory holding the scales and shifts. */
   this->a = (double *) astFree( (void *) this->a );
   this->b = (double *) astFree( (void *) this->b );

}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for WinMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the WinMap class to an output Channel.

*  Parameters:
*     this
*        Pointer to the WinMap whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Constants: */
#define COMMENT_LEN 50           /* Maximum length of a comment string */
#define KEY_LEN 50               /* Maximum length of a keyword */

/* Local Variables: */
   AstWinMap *this;              /* Pointer to the WinMap structure */
   char buff[ KEY_LEN + 1 ];     /* Buffer for keyword string */
   char comment[ COMMENT_LEN + 1 ]; /* Buffer for comment string */
   int axis;                     /* Axis index */
   int ncoord;                   /* No. of axes for mapping */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the WinMap structure. */
   this = (AstWinMap *) this_object;

/* Get the number of coordinates to be mapped. */
   ncoord = astGetNin( this );

/* Write out values representing the instance variables for the
   WinMap class.  Accompany these with appropriate comment strings,
   possibly depending on the values being written.*/

/* The scales and shifts. */
   for( axis = 0; axis < ncoord; axis++ ){
      (void) sprintf( buff, "Sft%d", axis + 1 );
      (void) sprintf( comment, "Shift for axis %d", axis + 1 );
      astWriteDouble( channel, buff, (this->a)[ axis ] != 0.0, 0,
                      (this->a)[ axis ], comment );
      (void) sprintf( buff, "Scl%d", axis + 1 );
      (void) sprintf( comment, "Scale factor for axis %d", axis + 1 );
      astWriteDouble( channel, buff, (this->b)[ axis ] != 1.0, 0,
                      (this->b)[ axis ], comment );
   }

/* Undefine macros local to this function. */
#undef COMMENT_LEN
#undef KEY_LEN
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAWinMap and astCheckWinMap functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(WinMap,Mapping)
astMAKE_CHECK(WinMap)

AstWinMap *astWinMap_( int ncoord, const double c1_in[], const double c2_in[],
                       const double c1_out[], const double c2_out[],
                       const char *options, int *status, ...) {
/*
*++
*  Name:
c     astWinMap
f     AST_WINMAP

*  Purpose:
*     Create a WinMap.

*  Type:
*     Public function.

*  Synopsis:
c     #include "winmap.h"
c     AstWinMap *astWinMap( int ncoord,
c                           const double ina[],  const double inb[],
c                           const double outa[], const double outb[],
c                           const char *options, ... )
f     RESULT = AST_WINMAP( NCOORD, INA, INB, OUTA, OUTB, OPTIONS, STATUS )

*  Class Membership:
*     WinMap constructor.

*  Description:
*     This function creates a new WinMap and optionally initialises its
*     attributes.
*
*     A Winmap is a linear Mapping which transforms a rectangular
*     window in one coordinate system into a similar window in another
*     coordinate system by scaling and shifting each axis (the window
*     edges being parallel to the coordinate axes).
*
*     A WinMap is specified by giving the coordinates of two opposite
*     corners (A and B) of the window in both the input and output
*     coordinate systems.

*  Parameters:
c     ncoord
f     NCOORD = INTEGER (Given)
*        The number of coordinate values for each point to be
*        transformed (i.e. the number of dimensions of the space in
*        which the points will reside). The same number is applicable
*        to both input and output points.
c     ina
f     INA( NCOORD ) = DOUBLE PRECISION (Given)
c        An array containing the "ncoord"
f        An array containing the
*        coordinates of corner A of the window in the input coordinate
*        system.
c     inb
f     INB( NCOORD ) = DOUBLE PRECISION (Given)
c        An array containing the "ncoord"
f        An array containing the
*        coordinates of corner B of the window in the input coordinate
*        system.
c     outa
f     OUTA( NCOORD ) = DOUBLE PRECISION (Given)
c        An array containing the "ncoord"
f        An array containing the
*        coordinates of corner A of the window in the output coordinate
*        system.
c     outb
f     OUTB( NCOORD ) = DOUBLE PRECISION (Given)
c        An array containing the "ncoord"
f        An array containing the
*        coordinates of corner B of the window in the output coordinate
*        system.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new WinMap. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new WinMap. The syntax used is identical to that for the
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
c     astWinMap()
f     AST_WINMAP = INTEGER
*        A pointer to the new WinMap.

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
   AstWinMap *new;              /* Pointer to new WinMap */
   va_list args;                /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the WinMap, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitWinMap( NULL, sizeof( AstWinMap ), !class_init, &class_vtab,
                        "WinMap", ncoord, c1_in, c2_in, c1_out, c2_out );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new WinMap's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new WinMap. */
   return new;
}

AstWinMap *astWinMapId_( int ncoord, const double c1_in[], const double c2_in[],
                         const double c1_out[], const double c2_out[],
                         const char *options, ... ) {
/*
*  Name:
*     astWinMapId_

*  Purpose:
*     Create a WinMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "winmap.h"
*     AstWinMap *astWinMapId_( int ncoord, const double c1_in[],
*                              const double c2_in[], const double c1_out[],
*                              const double c2_out[],
*                              const char *options, ... )

*  Class Membership:
*     WinMap constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astWinMap constructor function. It returns an ID value (instead
*     of a true C pointer) to external users, and must be provided
*     because astWinMap_ has a variable argument list which cannot be
*     encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     The variable argument list also prevents this function from
*     invoking astWinMap_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astWinMap_.

*  Returned Value:
*     The ID value associated with the new WinMap.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstWinMap *new;              /* Pointer to new WinMap */
   va_list args;                /* Variable argument list */
   int *status;                 /* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the WinMap, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitWinMap( NULL, sizeof( AstWinMap ), !class_init, &class_vtab,
                        "WinMap", ncoord, c1_in, c2_in, c1_out, c2_out );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new WinMap's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new WinMap. */
   return astMakeId( new );
}

AstWinMap *astInitWinMap_( void *mem, size_t size, int init,
                           AstWinMapVtab *vtab, const char *name,
                           int ncoord, const double *c1_in,
                           const double *c2_in, const double *c1_out,
                           const double *c2_out, int *status ) {
/*
*+
*  Name:
*     astInitWinMap

*  Purpose:
*     Initialise a WinMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "winmap.h"
*     AstWinMap *astInitWinMap( void *mem, size_t size, int init,
*                               AstWinMapVtab *vtab, const char *name,
*                               int ncoord, const double *c1_in,
*                               const double *c2_in,
*                               const double *c1_out, const double *c2_out )

*  Class Membership:
*     WinMap initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new WinMap object. It allocates memory (if necessary) to accommodate
*     the WinMap plus any additional data associated with the derived class.
*     It then initialises a WinMap structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a WinMap at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the WinMap is to be initialised.
*        This must be of sufficient size to accommodate the WinMap data
*        (sizeof(WinMap)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the WinMap (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the WinMap
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the WinMap's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new WinMap.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the astGetClass
*        method).
*     ncoord
*        The number of coordinate values per point.
*     c1_in
*        The input coordinates of corner C1 of the window.
*     c2_in
*        The input coordinates of corner C2 of the window.
*     c1_out
*        The output coordinates of corner C1 of the window.
*     c2_out
*        The output coordinates of corner C2 of the window.

*  Returned Value:
*     A pointer to the new WinMap.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstWinMap *new;              /* Pointer to new WinMap */
   double denom;                /* Denominotor */
   int axis;                    /* Axis index */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitWinMapVtab( vtab, name );

/* Initialise. */
   new = NULL;

/* Initialise a Mapping structure (the parent class) as the first component
   within the WinMap structure, allocating memory if necessary. Specify that
   the Mapping should be defined in both the forward and inverse directions. */
   new = (AstWinMap *) astInitMapping( mem, size, 0,
                                       (AstMappingVtab *) vtab, name,
                                       ncoord, ncoord, 1, 1 );

   if ( astOK ) {

/* Initialise the WinMap data. */
/* ---------------------------- */
/* Allocate memory to hold the shift and scale for each axis. */
      new->a = (double *) astMalloc( sizeof(double)*(size_t)ncoord );
      new->b = (double *) astMalloc( sizeof(double)*(size_t)ncoord );

/* Check the pointers can be used */
      if( astOK ){

/* Calculater and store the shift and scale for each axis. */
         for( axis = 0; axis < ncoord; axis++ ){

/* If any of the corners have not been provided, store bad values. */
            if( !c1_in || !c1_out || !c2_in || !c2_out ) {
               (new->b)[ axis ] = AST__BAD;
               (new->a)[ axis ] = AST__BAD;

/* Otherwise, check the corners are good (not AST__BAD or NaN)... */
            } else if( astISGOOD(c2_in[ axis ]) && astISGOOD(c1_in[ axis ]) &&
                       astISGOOD(c2_out[ axis ]) && astISGOOD(c1_out[ axis ]) ){

               denom = c2_in[ axis ] - c1_in[ axis ];
               if( denom != 0.0 ){
                  (new->b)[ axis ] = ( c2_out[ axis ] - c1_out[ axis ] )/denom;
                  (new->a)[ axis ] = c1_out[ axis ] - (new->b)[ axis ]*c1_in[ axis ];
               } else {
                  (new->b)[ axis ] = AST__BAD;
                  (new->a)[ axis ] = AST__BAD;
               }

            } else {
               (new->b)[ axis ] = AST__BAD;
               (new->a)[ axis ] = AST__BAD;
            }

         }

      }

/* If an error occurred, clean up by deleting the new WinMap. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new WinMap. */
   return new;
}

AstWinMap *astLoadWinMap_( void *mem, size_t size,
                           AstWinMapVtab *vtab, const char *name,
                           AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadWinMap

*  Purpose:
*     Load a WinMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "winmap.h"
*     AstWinMap *astLoadWinMap( void *mem, size_t size,
*                               AstWinMapVtab *vtab, const char *name,
*                               AstChannel *channel )

*  Class Membership:
*     WinMap loader.

*  Description:
*     This function is provided to load a new WinMap using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     WinMap structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a WinMap at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the WinMap is to be
*        loaded.  This must be of sufficient size to accommodate the
*        WinMap data (sizeof(WinMap)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the WinMap (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the WinMap structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstWinMap) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new WinMap. If this is NULL, a pointer
*        to the (static) virtual function table for the WinMap class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "WinMap" is used instead.

*  Returned Value:
*     A pointer to the new WinMap.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Constants. */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
#define KEY_LEN 50               /* Maximum length of a keyword */

/* Local Variables: */
   AstWinMap *new;              /* Pointer to the new WinMap */
   char buff[ KEY_LEN + 1 ];    /* Buffer for keyword string */
   int axis;                    /* Axis index */
   int ncoord;                  /* The number of coordinate axes */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this WinMap. In this case the
   WinMap belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstWinMap );
      vtab = &class_vtab;
      name = "WinMap";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitWinMapVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built WinMap. */
   new = astLoadMapping( mem, size, (AstMappingVtab *) vtab, name,
                         channel );

   if ( astOK ) {

/* Get the number of axis for the mapping. */
      ncoord = astGetNin( (AstMapping *) new );

/* Allocate memory to hold the scales and shifts. */
      new->a = (double *) astMalloc( sizeof(double)*(size_t)ncoord );
      new->b = (double *) astMalloc( sizeof(double)*(size_t)ncoord );

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "WinMap" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* The scales and shifts. */
      for( axis = 0; axis < ncoord; axis++ ){
         (void) sprintf( buff, "sft%d", axis + 1 );
         (new->a)[ axis ] = astReadDouble( channel, buff, 0.0 );
         (void) sprintf( buff, "scl%d", axis + 1 );
         (new->b)[ axis ] = astReadDouble( channel, buff, 1.0 );
      }
   }

/* If an error occurred, clean up by deleting the new WinMap. */
   if ( !astOK ) new = astDelete( new );

/* Return the new WinMap pointer. */
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

int astWinTerms_( AstWinMap *this, double **scale, double **shift, int *status ){
   if( !astOK ) return 0;
   return (**astMEMBER(this,WinMap,WinTerms))( this, scale, shift, status );
}




