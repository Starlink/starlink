/*
*class++
*  Name:
*     CmpMap

*  Purpose:
*     Compound Mapping.

*  Constructor Function:
c     astCmpMap
f     AST_CMPMAP

*  Description:
*     A CmpMap is a compound Mapping which allows two component
*     Mappings (of any class) to be connected together to form a more
*     complex Mapping. This connection may either be "in series"
*     (where the first Mapping is used to transform the coordinates of
*     each point and the second mapping is then applied to the
*     result), or "in parallel" (where one Mapping transforms the
*     earlier coordinates for each point and the second Mapping
*     simultaneously transforms the later coordinates).
*
*     Since a CmpMap is itself a Mapping, it can be used as a
*     component in forming further CmpMaps. Mappings of arbitrary
*     complexity may be built from simple individual Mappings in this
*     way.

*  Inheritance:
*     The CmpMap class inherits from the Mapping class.

*  Attributes:
*     The CmpMap class does not define any new attributes beyond those
*     which are applicable to all Mappings.

*  Functions:
c     The CmpMap class does not define any new functions beyond those
f     The CmpMap class does not define any new routines beyond those
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
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     1-FEB-1996 (RFWS):
*        Original version.
*     25-SEP-1996 (RFWS):
*        Implemented external interface and I/O facilities.
*     12-DEC-1996 (RFWS):
*        Over-ride the astMapList method.
*     13-DEC-1996 (RFWS):
*        Over-ride the astSimplify method.
*     4-JUN-1997 (RFWS):
*        Eliminate any simplification when MapList is used. Instead,
*        over-ride the MapMerge method and implement all
*        simplification in this.
*     24-MAR-1998 (RFWS):
*        Fixed bug in testing of simplified invert flag in Simplify.
*     15-APR-1998 (RFWS):
*        Improved the MapMerge method to allow parallel combinations
*        of series CmpMaps to be replaced by series combinations of
*        parallel CmpMaps, and vice versa.
*     26-SEP-2001 (DSB):
*        Over-ride the astDecompose method.
*     8-JAN-2003 (DSB):
*        - Changed private InitVtab method to protected astInitCmpMapVtab
*        method.
*     8-JAN-2003 (DSB):
*        - Modified MapMerge so that a parallel CmpMap can swap with a
*        suitable PermMap lower neighbour.
*     23-APR-2004 (DSB):
*        - Modified Simplify to avoid infinite loops.
*     27-APR-2004 (DSB):
*        - Correction to MapMerge to prevent segvio if CmpMap and PermMap
*        cannot be swapped.
*     4-OCT-2004 (DSB):
*        Modify astMapList to return flag indicating presence of inverted
*        CmpMaps in supplied Mapping.
*     20-APR-2005 (DSB):
*        Modify MapMerge so that it will attempt to merge the first
*        and second CmpMaps in a list of series CmpMaps.
*     8-FEB-2006 (DSB):
*        Corrected logic within MapMerge for cases where a PermMap is
*        followed by a parallel CmpMap.
*     14-FEB-2006 (DSB):
*        Override astGetObjSize.
*     14-MAR-2006 (DSB):
*        - When checking for patterns in the simplification process,
*        require at least 30 samples in the waveform for evidence of a
*        pattern.
*        - Override astEqual.
*        - The constructor no longer reports an error if the resulting
*	 CmpMap cannot transform points in either direction. This is
*	 because it may be possible to simplify such a CmpMap and the
*	 simplified Mapping may have defined transformations. E.g. if a
*	 Mapping which has only a forward transformation is combined in
*	 series with its own inverse, the combination will simplify to a
*	 UnitMap (usually).
*     9-MAY-2006 (DSB):
*        - In Simplify, remove checks for patterns in the number of atomic
*        mappings when calling astSimplify recursively.
*     23-AUG-2006 (DSB):
*        - In Simplify, add checks for re-appearance of a Mapping that is
*        already being simplified at a higher levelin the call stack.
*     18-APR-2007 (DSB):
*        In Simplify: if the returned Mapping is not a CmpMap, always copy
*        the returned component Mapping (rather than cloning it) so that
*        the returned Mapping is not affected if user code subsequently
*        inverts the component Mapping via some other pointer.
*     12-MAR-2008 (DSB):
*        Modify MapSplit so that attempts to split the inverse
*        transformation if it cannot split the forward transformation.
*     30-JUL-2009 (DSB):
*        Ensure the PermMap has equal number of inputs and outputs when
*        swapping a PermMap and a CmpMap in astMapMerge.
*     3-JAN-2011 (DSB):
*        In MapSplit, certain classes of Mapping (e.g. PermMaps) can
*        produce a returned Mapping with zero outputs. Consider such
*        Mappings to be unsplitable.
*     11-JAN-2011 (DSB):
*        Improve simplification of serial combinations of parellel CmpMaps.
*     25-JAN-2011 (DSB):
*        Big improvement to the efficiency of the astMapSplit method.
*     24-JAN-2012 (DSB):
*        If efficient MapSplit fails to split (e.g. due to the presence
*        of PermMaps), then revert to the older slower method.
*     5-FEB-2013 (DSB):
*        Take account of Invert flags when combining parallel CmpMaps in
*        series.
*     29-APR-2013 (DSB):
*        In MapList, use the astDoNotSimplify method to check that it is
*        OK to expand the CmpMap.
*     23-APR-2015 (DSB):
*        In Simplify, prevent mappings that are known to cause infinite
*        loops from being nominated for simplification.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS CmpMap

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */

#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* Sets of points/coordinates */
#include "mapping.h"             /* Coordinate Mappings (parent class) */
#include "channel.h"             /* I/O channels */
#include "permmap.h"             /* Coordinate permutation Mappings */
#include "unitmap.h"             /* Unit transformations */
#include "cmpmap.h"              /* Interface definition for this class */
#include "frameset.h"            /* Interface definition for FrameSets */
#include "globals.h"             /* Thread-safe global data access */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <stdarg.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>

/* Module Variables. */
/* ================= */

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* Pointers to parent class methods which are extended by this class. */
static int (* parent_getobjsize)( AstObject *, int * );
static AstPointSet *(* parent_transform)( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static int (* parent_maplist)( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
static int *(* parent_mapsplit)( AstMapping *, int, const int *, AstMapping **, int * );

#if defined(THREAD_SAFE)
static int (* parent_managelock)( AstObject *, int, int, AstObject **, int * );
#endif


/* Define macros for accessing each item of thread specific global data. */
#ifdef THREAD_SAFE

/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0; \
   globals->Simplify_Depth = 0; \
   globals->Simplify_Stackmaps = NULL;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(CmpMap)

#define class_init astGLOBAL(CmpMap,Class_Init)
#define class_vtab astGLOBAL(CmpMap,Class_Vtab)
#define simplify_depth astGLOBAL(CmpMap,Simplify_Depth)
#define simplify_stackmaps astGLOBAL(CmpMap,Simplify_Stackmaps)



/* If thread safety is not needed, declare and initialise globals at static
   variables. */
#else

static int simplify_depth  = 0;
static AstMapping **simplify_stackmaps = NULL;


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstCmpMapVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstCmpMap *astCmpMapId_( void *, void *, int, const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstMapping *CombineMaps( AstMapping *, int, AstMapping *, int, int, int * );
static AstMapping *RemoveRegions( AstMapping *, int * );
static AstMapping *Simplify( AstMapping *, int * );
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static double Rate( AstMapping *, double *, int, int, int * );
static int *MapSplit( AstMapping *, int, const int *, AstMapping **, int * );
static int *MapSplit0( AstMapping *, int, const int *, AstMapping **, int, int * );
static int *MapSplit1( AstMapping *, int, const int *, AstMapping **, int * );
static int *MapSplit2( AstMapping *, int, const int *, AstMapping **, int * );
static int Equal( AstObject *, AstObject *, int * );
static int GetIsLinear( AstMapping *, int * );
static int MapList( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
static int MapMerge( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
static int PatternCheck( int, int, int **, int *, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Decompose( AstMapping *, AstMapping **, AstMapping **, int *, int *, int *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static int GetObjSize( AstObject *, int * );

#if defined(THREAD_SAFE)
static int ManageLock( AstObject *, int, int, AstObject **, int * );
#endif


/* Member functions. */
/* ================= */
static int Equal( AstObject *this_object, AstObject *that_object, int *status ) {
/*
*  Name:
*     Equal

*  Purpose:
*     Test if two CmpMaps are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpmap.h"
*     int Equal( AstObject *this, AstObject *that, int *status )

*  Class Membership:
*     CmpMap member function (over-rides the astEqual protected
*     method inherited from the astMapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two CmpMaps are equivalent.

*  Parameters:
*     this
*        Pointer to the first Object (a CmpMap).
*     that
*        Pointer to the second Object.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the CmpMaps are equivalent, zero otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstCmpMap *that;
   AstCmpMap *this;
   AstMapping **that_map_list;
   AstMapping **this_map_list;
   int *that_invert_list;
   int *this_invert_list;
   int i;
   int result;
   int that_inv;
   int that_nmap;
   int this_inv;
   int this_nmap;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain pointers to the two CmpMap structures. */
   this = (AstCmpMap *) this_object;
   that = (AstCmpMap *) that_object;

/* Check the second object is a CmpMap. We know the first is a
   CmpMap since we have arrived at this implementation of the virtual
   function. */
   if( astIsACmpMap( that ) ) {

/* Check they are both either parallel or series. */
      if( that->series == that->series ) {

/* Decompose the first CmpMap into a sequence of Mappings to be applied in
   series or parallel, as appropriate, and an associated list of
   Invert flags. */
         this_nmap = 0;
         this_map_list = NULL;
         this_invert_list = NULL;
         astMapList( (AstMapping *) this, this->series, astGetInvert( this ),
                     &this_nmap, &this_map_list, &this_invert_list );

/* Similarly decompose the second CmpMap. */
         that_nmap = 0;
         that_map_list = NULL;
         that_invert_list = NULL;
         astMapList( (AstMapping *) that, that->series, astGetInvert( that ),
                     &that_nmap, &that_map_list, &that_invert_list );

/* Check the decompositions yielded the same number of component
   Mappings. */
         if( that_nmap == this_nmap ) {

/* Check equality of every component. */
            for( i = 0; i < this_nmap; i++ ) {

/* Temporarily set the Mapping Invert flags to the required values,
   saving the original values so that they can be re-instated later.*/
               this_inv = astGetInvert( this_map_list[ i ] );
               astSetInvert( this_map_list[ i ], this_invert_list[ i ] );
               that_inv = astGetInvert( that_map_list[ i ] );
               astSetInvert( that_map_list[ i ], that_invert_list[ i ] );

/* Compare the two component Mappings for equality. */
               result = astEqual( this_map_list[ i ], that_map_list[ i ] );

/* Re-instate the original Invert flags. */
               astSetInvert( this_map_list[ i ], this_inv );
               astSetInvert( that_map_list[ i ], that_inv );

/* Leave the loop if the Mappings are not equal. */
               if( !result ) break;
            }
         }

/* Free resources */
         for( i = 0; i < this_nmap; i++ ) {
            this_map_list[ i ] = astAnnul( this_map_list[ i ] );
         }
         this_map_list = astFree( this_map_list );
         this_invert_list = astFree( this_invert_list );

         for( i = 0; i < that_nmap; i++ ) {
            that_map_list[ i ] = astAnnul( that_map_list[ i ] );
         }
         that_map_list = astFree( that_map_list );
         that_invert_list = astFree( that_invert_list );

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
*     Return the value of the IsLinear attribute for a CmpMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     void GetIsLinear( AstMapping *this, int *status )

*  Class Membership:
*     CmpMap member function (over-rides the protected astGetIsLinear
*     method inherited from the Mapping class).

*  Description:
*     This function returns the value of the IsLinear attribute for a
*     Frame, which is one if both component Mappings have a value of 1
*     for the IsLinear attribute.

*  Parameters:
*     this
*        Pointer to the CmpqMap.
*     status
*        Pointer to the inherited status variable.
*/
   AstCmpMap *this;
   this = (AstCmpMap *) this_mapping;
   return astGetIsLinear( this->map1 ) && astGetIsLinear( this->map2 );
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
*     #include "cmpmap.h"
*     int GetObjSize( AstObject *this, int *status )

*  Class Membership:
*     CmpMap member function (over-rides the astGetObjSize protected
*     method inherited from the parent class).

*  Description:
*     This function returns the in-memory size of the supplied CmpMap,
*     in bytes.

*  Parameters:
*     this
*        Pointer to the CmpMap.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The Object size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstCmpMap *this;         /* Pointer to CmpMap structure */
   int result;                /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointers to the CmpMap structure. */
   this = (AstCmpMap *) this_object;

/* Invoke the GetObjSize method inherited from the parent class, and then
   add on any components of the class structure defined by thsi class
   which are stored in dynamically allocated memory. */
   result = (*parent_getobjsize)( this_object, status );

   result += astGetObjSize( this->map1 );
   result += astGetObjSize( this->map2 );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static AstMapping *CombineMaps( AstMapping *mapping1, int invert1,
                                AstMapping *mapping2, int invert2,
                                int series, int *status ) {
/*
*  Name:
*     CombineMaps

*  Purpose:
*     Combine two Mappings with specified Invert flags into a CmpMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpmap.h"
*     AstMapping *CombineMaps( AstMapping *mapping1, int invert1,
*                              AstMapping *mapping2, int invert2,
*                              int series, int *status )

*  Class Membership:
*     CmpMap member function.

*  Description:
*     This function combines two Mappings into a CmpMap (compound
*     Mapping) as if their Invert flags were set to specified values
*     when the CmpMap is created. However, the individual Mappings are
*     returned with their Invert flag values unchanged from their
*     original state.

*  Parameters:
*     mapping1
*        Pointer to the first Mapping.
*     invert1
*        The (boolean) Invert flag value required for the first Mapping.
*     mapping2
*        Pointer to the second Mapping.
*     invert2
*        The (boolean) Invert flag value required for the second Mapping.
*     series
*        Whether the Mappings are to be combined in series (as opposed to
*        in parallel).
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the resulting compound Mapping (a CmpMap).

*  Notes:
*     - This function is a wrap-up for the astCmpMap constructor and
*     temporarily assigns the required Invert flag values while
*     creating the required CmpMap. However, it also takes account of
*     the possibility that the two Mapping pointers supplied may point
*     at the same Mapping.
*     - A null Object pointer (AST__NULL) will be returned if this
*     function is invoked with the AST error status set, or if it
*     should fail for any reason.
*/

/* Local Variables: */
   AstMapping *map1;             /* First temporary Mapping pointer */
   AstMapping *map2;             /* Second temporary Mapping pointer */
   AstMapping *result;           /* Pointer to result Mapping */
   int copy;                     /* Copy needed? */
   int inv1;                     /* First original Invert flag value */
   int inv2;                     /* Second original Invert flag value */
   int set1;                     /* First Invert flag originally set? */
   int set2;                     /* Second Invert flag originally set? */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Limit incoming values to 0 or 1. */
   invert1 = ( invert1 != 0 );
   invert2 = ( invert2 != 0 );

/* Obtain the Invert flag values for each Mapping. */
   inv1 = astGetInvert( mapping1 );
   inv2 = astGetInvert( mapping2 );

/* Also determine if these values are explicitly set. */
   set1 = astTestInvert( mapping1 );
   set2 = astTestInvert( mapping2 );

/* If both Mappings are actually the same but we need different Invert
   flag values to be set, then this can only be achieved by making a
   copy. Note if this is necessary. */
   copy = ( ( mapping1 == mapping2 ) && ( invert1 != invert2 ) );

/* Clone the first Mapping pointer. Do likewise for the second but
   make a copy instead if necessary. */
   map1 = astClone( mapping1 );
   map2 = copy ? astCopy( mapping2 ) : astClone( mapping2 );

/* If the Invert value for the first Mapping needs changing, make the
   change. */
   if ( invert1 != inv1 ) {
      if ( invert1 ) {
         astSetInvert( map1, 1 );
      } else {
         astClearInvert( map1 );
      }
   }

/* Similarly, change the Invert flag for the second Mapping if
   necessary. */
   if ( invert2 != inv2 ) {
      if ( invert2 ) {
         astSetInvert( map2, 1 );
      } else {
         astClearInvert( map2 );
      }
   }

/* Combine the two Mappings into a CmpMap. */
   result = (AstMapping *) astCmpMap( map1, map2, series, "", status );

/* If the first Mapping's Invert value was changed, restore it to its
   original state. */
   if ( invert1 != inv1 ) {
      if ( set1 ) {
         astSetInvert( map1, inv1 );
      } else {
         astClearInvert( map1 );
      }
   }

/* Similarly, restore the second Mapping's Invert value if
   necessary. This step is not needed, however, if a copy was made. */
   if ( ( invert2 != inv2 ) && !copy ) {
      if ( set2 ) {
         astSetInvert( map2, inv2 );
      } else {
         astClearInvert( map2 );
      }
   }

/* Annul the temporary Mapping pointers. */
   map1 = astAnnul( map1 );
   map2 = astAnnul( map2 );

/* If an error occurred, then annul the result pointer. */
   if ( !astOK ) result = astAnnul( result );

/* Return the result. */
   return result;
}

static void Decompose( AstMapping *this_mapping, AstMapping **map1,
                       AstMapping **map2, int *series, int *invert1,
                       int *invert2, int *status ) {
/*
*
*  Name:
*     Decompose

*  Purpose:
*     Decompose a Mapping into two component Mappings.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     void Decompose( AstMapping *this, AstMapping **map1,
*                     AstMapping **map2, int *series,
*                     int *invert1, int *invert2, int *status )

*  Class Membership:
*     CmpMap member function (over-rides the protected astDecompose
*     method inherited from the Mapping class).

*  Description:
*     This function returns pointers to two Mappings which, when applied
*     either in series or parallel, are equivalent to the supplied Mapping.
*
*     Since the Frame class inherits from the Mapping class, Frames can
*     be considered as special types of Mappings and so this method can
*     be used to decompose either CmpMaps or CmpFrames.

*  Parameters:
*     this
*        Pointer to the Mapping.
*     map1
*        Address of a location to receive a pointer to first component
*        Mapping.
*     map2
*        Address of a location to receive a pointer to second component
*        Mapping.
*     series
*        Address of a location to receive a value indicating if the
*        component Mappings are applied in series or parallel. A non-zero
*        value means that the supplied Mapping is equivalent to applying map1
*        followed by map2 in series. A zero value means that the supplied
*        Mapping is equivalent to applying map1 to the lower numbered axes
*        and map2 to the higher numbered axes, in parallel.
*     invert1
*        The value of the Invert attribute to be used with map1.
*     invert2
*        The value of the Invert attribute to be used with map2.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - Any changes made to the component Mappings using the returned
*     pointers will be reflected in the supplied Mapping.

*-
*/


/* Local Variables: */
   AstCmpMap *this;              /* Pointer to CmpMap structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the CmpMap structure. */
   this = (AstCmpMap *) this_mapping;

/* First deal with series mappings. */
   if( this->series ) {
      if( series ) *series = 1;

/* If the CmpMap has been inverted, return the Mappings in reverse
   order with inverted Invert falgs. */
      if( astGetInvert( this ) ) {
         if( map1 ) *map1 = astClone( this->map2 );
         if( map2 ) *map2 = astClone( this->map1 );
         if( invert1 ) *invert1 = this->invert2 ? 0 : 1;
         if( invert2 ) *invert2 = this->invert1 ? 0 : 1;

/* If the CmpMap has not been inverted, return the Mappings in their
   original order with their original Invert flags. */
      } else {
         if( map1 ) *map1 = astClone( this->map1 );
         if( map2 ) *map2 = astClone( this->map2 );
         if( invert1 ) *invert1 = this->invert1;
         if( invert2 ) *invert2 = this->invert2;
      }

/* Now deal with parallel mappings. */
   } else {
      if( series ) *series = 0;

/* The mappings are returned in their original order whether or not the
   CmpMap has been inverted. */
      if( map1 ) *map1 = astClone( this->map1 );
      if( map2 ) *map2 = astClone( this->map2 );

/* If the CmpMap has been inverted, return inverted Invert flags. */
      if( astGetInvert( this ) ) {
         if( invert1 ) *invert1 = this->invert1 ? 0 : 1;
         if( invert2 ) *invert2 = this->invert2 ? 0 : 1;

/* If the CmpMap has not been inverted, return the original Invert flags. */
      } else {
         if( invert1 ) *invert1 = this->invert1;
         if( invert2 ) *invert2 = this->invert2;
      }

   }
}

void astInitCmpMapVtab_(  AstCmpMapVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitCmpMapVtab

*  Purpose:
*     Initialise a virtual function table for a CmpMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "cmpmap.h"
*     void astInitCmpMapVtab( AstCmpMapVtab *vtab, const char *name )

*  Class Membership:
*     CmpMap vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the CmpMap class.

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
   will be used (by astIsACmpMap) to determine if an object belongs to
   this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstMappingVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that
   provide virtual methods for this class. */

/* None. */

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   mapping = (AstMappingVtab *) vtab;
   parent_getobjsize = object->GetObjSize;
   object->GetObjSize = GetObjSize;

#if defined(THREAD_SAFE)
   parent_managelock = object->ManageLock;
   object->ManageLock = ManageLock;
#endif

   parent_maplist = mapping->MapList;
   mapping->MapList = MapList;

   parent_transform = mapping->Transform;
   mapping->Transform = Transform;

   parent_mapsplit = mapping->MapSplit;
   mapping->MapSplit = MapSplit;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   object->Equal = Equal;
   mapping->Decompose = Decompose;
   mapping->MapMerge = MapMerge;
   mapping->Simplify = Simplify;
   mapping->RemoveRegions = RemoveRegions;
   mapping->GetIsLinear = GetIsLinear;

/* For some reason the CmpMap implementation of astRate can be immensely
   slow for complex Mapping, so it's currently disable until such time as
   I have time to sort it out.

   mapping->Rate = Rate;
*/

/* Declare the copy constructor, destructor and class dump function. */
   astSetCopy( vtab, Copy );
   astSetDelete( vtab, Delete );
   astSetDump( vtab, Dump, "CmpMap", "Compound Mapping" );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
}

#if defined(THREAD_SAFE)
static int ManageLock( AstObject *this_object, int mode, int extra,
                       AstObject **fail, int *status ) {
/*
*  Name:
*     ManageLock

*  Purpose:
*     Manage the thread lock on an Object.

*  Type:
*     Private function.

*  Synopsis:
*     #include "object.h"
*     AstObject *ManageLock( AstObject *this, int mode, int extra,
*                            AstObject **fail, int *status )

*  Class Membership:
*     CmpMap member function (over-rides the astManageLock protected
*     method inherited from the parent class).

*  Description:
*     This function manages the thread lock on the supplied Object. The
*     lock can be locked, unlocked or checked by this function as
*     deteremined by parameter "mode". See astLock for details of the way
*     these locks are used.

*  Parameters:
*     this
*        Pointer to the Object.
*     mode
*        An integer flag indicating what the function should do:
*
*        AST__LOCK: Lock the Object for exclusive use by the calling
*        thread. The "extra" value indicates what should be done if the
*        Object is already locked (wait or report an error - see astLock).
*
*        AST__UNLOCK: Unlock the Object for use by other threads.
*
*        AST__CHECKLOCK: Check that the object is locked for use by the
*        calling thread (report an error if not).
*     extra
*        Extra mode-specific information.
*     fail
*        If a non-zero function value is returned, a pointer to the
*        Object that caused the failure is returned at "*fail". This may
*        be "this" or it may be an Object contained within "this". Note,
*        the Object's reference count is not incremented, and so the
*        returned pointer should not be annulled. A NULL pointer is
*        returned if this function returns a value of zero.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*    A local status value:
*        0 - Success
*        1 - Could not lock or unlock the object because it was already
*            locked by another thread.
*        2 - Failed to lock a POSIX mutex
*        3 - Failed to unlock a POSIX mutex
*        4 - Bad "mode" value supplied.

*  Notes:
*     - This function attempts to execute even if an error has already
*     occurred.
*/

/* Local Variables: */
   AstCmpMap *this;       /* Pointer to CmpMap structure */
   int result;            /* Returned status value */

/* Initialise */
   result = 0;

/* Check the supplied pointer is not NULL. */
   if( !this_object ) return result;

/* Obtain a pointers to the CmpMap structure. */
   this = (AstCmpMap *) this_object;

/* Invoke the ManageLock method inherited from the parent class. */
   if( !result ) result = (*parent_managelock)( this_object, mode, extra,
                                                fail, status );

/* Invoke the astManageLock method on any Objects contained within
   the supplied Object. */
   if( !result ) result = astManageLock( this->map1, mode, extra, fail );
   if( !result ) result = astManageLock( this->map2, mode, extra, fail );

   return result;

}
#endif

static int MapList( AstMapping *this_mapping, int series, int invert,
                     int *nmap, AstMapping ***map_list, int **invert_list, int *status ) {
/*
*  Name:
*     MapList

*  Purpose:
*     Decompose a CmpMap into a sequence of simpler Mappings.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     int MapList( AstMapping *this, int series, int invert, int *nmap,
*                  AstMapping ***map_list, int **invert_list )

*  Class Membership:
*     CmpMap member function (over-rides the protected astMapList
*     method inherited from the Maping class).

*  Description:
*     This function decomposes a CmpMap into a sequence of simpler
*     Mappings which may be applied in sequence to achieve the same
*     effect. The CmpMap is decomposed as far as possible, but it is
*     not guaranteed that this will necessarily yield any more than
*     one Mapping, which may actually be the original CmpMap supplied.
*
*     This function is provided to support both the simplification of
*     CmpMaps, and the analysis of CmpMap structure so that particular
*     forms can be recognised.

*  Parameters:
*     this
*        Pointer to the CmpMap to be decomposed (the CmpMap is not
*        actually modified by this function).
*     series
*        If this value is non-zero, an attempt will be made to
*        decompose the CmpMap into a sequence of equivalent Mappings
*        which can be applied in series (i.e. one after the other). If
*        it is zero, the decomposition will instead yield Mappings
*        which can be applied in parallel (i.e. on successive sub-sets
*        of the input/output coordinates).
*     invert
*        The value to which the CmpMap's Invert attribute is to be
*        (notionally) set before performing the
*        decomposition. Normally, the value supplied here will be the
*        actual Invert value obtained from the CmpMap (e.g. using
*        astGetInvert).  Sometimes, however, when a CmpMap is
*        encapsulated within another structure, that structure may
*        retain an Invert value (in order to prevent external
*        interference) which should be used instead.
*
*        Note that the actual Invert value of the CmpMap supplied is
*        not used (or modified) by this function.
*     nmap
*        The address of an int which holds a count of the number of
*        individual Mappings in the decomposition. On entry, this
*        should count the number of Mappings already in the
*        "*map_list" array (below). On exit, it is updated to include
*        any new Mappings appended by this function.
*     map_list
*        Address of a pointer to an array of Mapping pointers. On
*        entry, this array pointer should either be NULL (if no
*        Mappings have yet been obtained) or should point at a
*        dynamically allocated array containing Mapping pointers
*        ("*nmap" in number) which have been obtained from a previous
*        invocation of this function.
*
*        On exit, the dynamic array will be enlarged to contain any
*        new Mapping pointers that result from the decomposition
*        requested. These pointers will be appended to any previously
*        present, and the array pointer will be updated as necessary
*        to refer to the enlarged array (any space released by the
*        original array will be freed automatically).
*
*        The new Mapping pointers returned will identify a sequence of
*        Mappings which, when applied in order, will perform a forward
*        transformation equivalent to that of the original CmpMap
*        (after its Invert flag has first been set to the value
*        requested above). The Mappings should be applied in series or
*        in parallel according to the type of decomposition requested.
*
*        All the Mapping pointers returned by this function should be
*        annulled by the caller, using astAnnul, when no longer
*        required. The dynamic array holding these pointers should
*        also be freed, using astFree.
*     invert_list
*        Address of a pointer to an array of int. On entry, this array
*        pointer should either be NULL (if no Mappings have yet been
*        obtained) or should point at a dynamically allocated array
*        containing Invert attribute values ("*nmap" in number) which
*        have been obtained from a previous invocation of this
*        function.
*
*        On exit, the dynamic array will be enlarged to contain any
*        new Invert attribute values that result from the
*        decomposition requested. These values will be appended to any
*        previously present, and the array pointer will be updated as
*        necessary to refer to the enlarged array (any space released
*        by the original array will be freed automatically).
*
*        The new Invert values returned identify the values which must
*        be assigned to the Invert attributes of the corresponding
*        Mappings (whose pointers are in the "*map_list" array) before
*        they are applied. Note that these values may differ from the
*        actual Invert attribute values of these Mappings, which are
*        not relevant.
*
*        The dynamic array holding these values should be freed by the
*        caller, using astFree, when no longer required.

*  Returned Value:
*     A non-zero value is returned if the supplied Mapping contained any
*     inverted CmpMaps.

*  Notes:
*     - It is unspecified to what extent the original CmpMap and the
*     individual (decomposed) Mappings are
*     inter-dependent. Consequently, the individual Mappings cannot be
*     modified without risking modification of the original CmpMap.
*     - If this function is invoked with the global error status set,
*     or if it should fail for any reason, then the *nmap value, the
*     list of Mapping pointers and the list of Invert values will all
*     be returned unchanged.
*/

/* Local Variables: */
   AstCmpMap *this;              /* Pointer to CmpMap structure */
   int invert1;                  /* Invert flag for first component Mapping */
   int invert2;                  /* Invert flag for second component Mapping */
   int r1;                       /* Value returned from first map list */
   int r2;                       /* Value returned from second map list */
   int result;                   /* Returned value */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Obtain a pointer to the CmpMap structure. */
   this = (AstCmpMap *) this_mapping;

/* Check if the CmpMap combines its component Mappings in the same way
   (series or parallel) as the decomposition requires. Also, do not
   expand CmpMaps that are not appropriate for simplification. */
   if ( this->series == series && !astDoNotSimplify( this ) ) {

/* If so, obtain the Invert attribute values to be applied to each
   component Mapping. */
      invert1 = this->invert1;
      invert2 = this->invert2;

/* If the CmpMap itself is inverted, also invert the Invert values to be
   applied to its components. */
      if ( invert ) {
         invert1 = !invert1;
         invert2 = !invert2;
      }

/* If the component Mappings are applied in series, then concatenate
   the Mapping lists obtained from each of them. Do this in reverse
   order if the CmpMap is inverted, since the second Mapping would be
   applied first in this case. */
      if ( series ) {
         if ( !invert ) {
            r1 = astMapList( this->map1, series, invert1,
                             nmap, map_list, invert_list );
            r2 = astMapList( this->map2, series, invert2,
                             nmap, map_list, invert_list );
         } else {
            r1 = astMapList( this->map2, series, invert2,
                             nmap, map_list, invert_list );
            r2 = astMapList( this->map1, series, invert1,
                             nmap, map_list, invert_list );
         }

/* If the component Mappings are applied in parallel, then concatenate
   the Mapping lists obtained from each of them. In this case,
   inverting the CmpMap has no effect on the order in which they are
   applied. */
      } else {
         r1 = astMapList( this->map1, series, invert1,
                          nmap, map_list, invert_list );
         r2 = astMapList( this->map2, series, invert2,
                          nmap, map_list, invert_list );
      }

/* Did we find any inverted CmpMaps? */
      result = invert || r1 || r2;

/* If the CmpMap does not combine its components in the way required
   by the decomposition (series or parallel), then we cannot decompose
   it. In this case it must be appended to the Mapping list as a
   single entity. We can use the parent class method to do this. */
   } else {
      result = ( *parent_maplist )( this_mapping, series, invert, nmap,
                                    map_list, invert_list, status );
   }

   return result;
}

static int MapMerge( AstMapping *this, int where, int series, int *nmap,
                     AstMapping ***map_list, int **invert_list, int *status ) {
/*
*  Name:
*     MapMerge

*  Purpose:
*     Simplify a sequence of Mappings containing a CmpMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     int MapMerge( AstMapping *this, int where, int series, int *nmap,
*                   AstMapping ***map_list, int **invert_list )

*  Class Membership:
*     CmpMap method (over-rides the protected astMapMerge method
*     inherited from the Mapping class).

*  Description:
*     This function attempts to simplify a sequence of Mappings by
*     merging a nominated CmpMap in the sequence with its neighbours,
*     so as to shorten the sequence if possible.
*
*     In many cases, simplification will not be possible and the
*     function will return -1 to indicate this, without further
*     action.
*
*     In most cases of interest, however, this function will either
*     attempt to replace the nominated CmpMap with one which it
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
*        Pointer to the nominated CmpMap which is to be merged with
*        its neighbours. This should be a cloned copy of the CmpMap
*        pointer contained in the array element "(*map_list)[where]"
*        (see below). This pointer will not be annulled, and the
*        CmpMap it identifies will not be modified by this function.
*     where
*        Index in the "*map_list" array (below) at which the pointer
*        to the nominated CmpMap resides.
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
   AstCmpMap *cmpmap1;           /* Pointer to first CmpMap */
   AstCmpMap *cmpmap2;           /* Pointer to second CmpMap */
   AstCmpMap *cmpmap;            /* Pointer to nominated CmpMap */
   AstCmpMap *new_cm;            /* Pointer to new CmpMap */
   AstMapping **map_list1;       /* Pointer to list of cmpmap1 component Mappings */
   AstMapping **map_list2;       /* Pointer to list of cmpmap2 component Mappings */
   AstMapping **new_map_list;    /* Extended Mapping list */
   AstMapping *map;              /* Pointer to nominated CmpMap */
   AstMapping *new1;             /* Pointer to new CmpMap */
   AstMapping *new2;             /* Pointer to new CmpMap */
   AstMapping *new;              /* Pointer to replacement Mapping */
   AstMapping *simp1;            /* Pointer to simplified Mapping */
   AstMapping *simp2;            /* Pointer to simplified Mapping */
   AstMapping *submap1;          /* A subset of mappings from cmpmap1 */
   AstMapping *submap2;          /* A subset of mappings from cmpmap2 */
   AstMapping *tmap2;            /* Temporary Mapping */
   AstMapping *tmap;             /* Temporary Mapping */
   AstPermMap *new_pm;           /* Pointer to new PermMap */
   AstPermMap *permmap1;         /* Pointer to first PermMap */
   AstUnitMap *unit;             /* UnitMap that feeds const PermMap i/p's */
   const char *class;            /* Pointer to Mapping class string */
   double *conperm;              /* Pointer to PermMap constants array */
   double *const_new;            /* Pointer to new PermMap constants array */
   double *p;                    /* Pointer to PermMap input position */
   double *q;                    /* Pointer to PermMap output position */
   double *qa;                   /* Pointer to 1st component output position */
   double *qb;                   /* Pointer to 2nd component output position */
   int *inperm;                  /* Pointer to copy of PermMap inperm array */
   int *inperm_new;              /* Pointer to new PermMap inperm array */
   int *invert_list1;            /* Pointer to list of cmpmap1 invert values */
   int *invert_list2;            /* Pointer to list of cmpmap2 invert values */
   int *new_invert_list;         /* Extended Invert flag list */
   int *outperm;                 /* Pointer to copy of PermMap outperm array */
   int *outperm_new;             /* Pointer to new PermMap outperm array */
   int aconstants;               /* Are all 1st component outputs constant? */
   int bconstants;               /* Are all 2nd component outputs constant? */
   int canswap;                  /* Can nominated Mapping swap with lower neighbour? */
   int i;                        /* Coordinate index */
   int iconid;                   /* Constant identifier in supplied PermMap */
   int imap1;                    /* Index of first Mapping */
   int imap2;                    /* Index of second Mapping */
   int imap;                     /* Loop counter for Mappings */
   int invert1;                  /* Invert flag for first CmpMap */
   int invert1a;                 /* Invert flag for sub-Mapping */
   int invert1b;                 /* Invert flag for sub-Mapping */
   int invert2;                  /* Invert flag for second CmpMap */
   int invert2a;                 /* Invert flag for sub-Mapping */
   int invert2b;                 /* Invert flag for sub-Mapping */
   int invert;                   /* Invert attribute value */
   int j;                        /* Coordinate index */
   int jmap1;                    /* Index of next component Mapping in cmpmap1 */
   int jmap2;                    /* Index of next component Mapping in cmpmap2 */
   int new_invert;               /* New Invert attribute value */
   int nin2a;                    /* No. input coordinates for sub-Mapping */
   int nin2b;                    /* No. input coordinates for sub-Mapping */
   int nmap1;                    /* Number of Mappings in cmpmap1 */
   int nmap2;                    /* Number of Mappings in cmpmap2 */
   int nout2a;                   /* No. of outputs for 1st component Mapping */
   int nout2b;                   /* No. of outputs for 2nd component Mapping */
   int npin;                     /* No. of inputs for original PermMap */
   int npin_new;                 /* No. of inputs for new PermMap */
   int npout;                    /* No. of outputs for original PermMap */
   int npout_new;                /* No. of outputs for new PermMap */
   int nunit;                    /* No. of PermMap i/p's fed by UnitMap */
   int oconid;                   /* Constant identifier in returned PermMap */
   int result;                   /* Result value to return */
   int set;                      /* Invert attribute set? */
   int simpler;                  /* Simplification possible? */
   int subin2;                   /* Number of inputs of submap2 */
   int subinv1;                  /* Invert attribute to use with submap1 */
   int subinv2;                  /* Invert attribute to use with submap2 */
   int subout1;                  /* Number of outputs of submap1 */

/* Initialise.*/
   result = -1;

/* Check the inherited status. */
   if ( !astOK ) return result;

/* Simplify the CmpMap on its own. */
/* =============================== */
/* Obtain a pointer to the nominated Mapping (which is a CmpMap). */
   map = ( *map_list )[ where ];
   cmpmap = (AstCmpMap *) map;

/* Determine if the Mapping's Invert attribute is set and obtain its
   value. */
   set = astTestInvert( map );
   invert = astGetInvert( map );

/* If necessary, change the Invert attribute to the value we want. We
   do this so that simplification (below) has a chance to absorb a
   non-zero Invert value into the implementation of the simplified
   Mapping (the preference being to have an Invert value of zero after
   simplification, if possible). */
   if ( invert != ( *invert_list )[ where ] ) {
      astSetInvert( map, ( *invert_list )[ where ] );
   }

/* Simplify the Mapping and obtain the new Invert value. */
   new = astSimplify( map );
   new_invert = astGetInvert( new );

/* If necessary, restore the original Mapping's Invert attribute to
   its initial state. */
   if ( invert != ( *invert_list )[ where ] ) {
      if ( set ) {
         astSetInvert( map, invert );
      } else {
         astClearInvert( map );
      }
   }

/* We must now determine if simplification has occurred. Since this is
   internal code, we can compare the two Mapping pointers directly to
   see whether "astSimplify" just cloned the pointer we gave it. If it
   did, then simplification was probably not possible, but check to
   see if the Invert attribute has changed to be sure. */
   if ( astOK ) {
      simpler = ( new != map ) || ( new_invert != ( *invert_list )[ where ] );

/* If simplification was successful, annul the original pointer in the
   Mapping list and replace it with the new one, together with its
   invert flag. */
      if ( simpler ) {
         (void) astAnnul( ( *map_list )[ where ] );
         ( *map_list )[ where ] = new;
         ( *invert_list )[ where ] = new_invert;

/* Return the result. */
         result = where;

/* Otherwise, annul the new Mapping pointer. */
      } else {
         new = astAnnul( new );

/* If the nominated CmpMap is a series CmpMap and the sequence of
   Mappings are being combined in series, or if the nominated CmpMap is
   a parallel CmpMap and the sequence of Mappings are being combined in
   parallel, replace the single CmpMap with the two component Mappings. */
         if( ( series && cmpmap->series ) ||
             ( !series && !cmpmap->series ) ) {

/* We are increasing the number of Mappings in the list, so we need to create
   new, larger, arrays to hold the list of Mapping pointers and invert flags. */
            new_map_list = astMalloc( ( *nmap + 1 )*sizeof( AstMapping * ) );
            new_invert_list = astMalloc( ( *nmap + 1 )*sizeof( int ) );
            if( astOK ) {

/* Copy the values prior to the nominated CmpMap. */
               for( i = 0; i < where; i++ ) {
                  new_map_list[ i ] = astClone( ( *map_list )[ i ] );
                  new_invert_list[ i ] = ( *invert_list )[ i ];
               }

/* Next insert the two components of the nominated CmpMap */
               new_map_list[ where ] = astClone( cmpmap->map1 );
               new_invert_list[ where ] = cmpmap->invert1;
               new_map_list[ where + 1 ] = astClone( cmpmap->map2 );
               new_invert_list[ where + 1 ] = cmpmap->invert2;

/* Now copy any values after the nominated CmpMap. */
               for( i = where + 1; i < *nmap; i++ ) {
                  new_map_list[ i + 1 ] = astClone( ( *map_list )[ i ] );
                  new_invert_list[ i + 1 ] = ( *invert_list )[ i ];
               }

/* Now annul the Object pointers in the supplied map list. */
               for( i = 0; i < *nmap; i++ ) {
                  (* map_list )[ i ] = astAnnul( ( *map_list )[ i ] );
               }

/* Free the memory holding the supplied Mapping and invert flag lists. */
               astFree( *map_list );
               astFree( *invert_list );

/* Return pointers to the new extended lists. */
               *map_list = new_map_list;
               *invert_list = new_invert_list;

/* Increase the number of Mappings in the list, and the index of
   the first modified Mapping. */
               (*nmap)++;
               result = where;

/* Indicate some simplification has taken place */
               simpler = 1;
            }
         }
      }

/* If no simplification has been done, merge adjacent CmpMaps. */
/* ========================================================== */
/* If the CmpMap would not simplify on its own, we now look for a
   neighbouring CmpMap with which it might merge. We use the previous
   Mapping, if suitable, since this will normally also have been fully
   simplified on its own. Check if a previous Mapping exists. */
      if( !simpler ) {
         if ( astOK && *nmap > 1 ) {

/* Obtain the indices of the two potential Mappings to be merged. imap1
   is the first Mapping, imap2 is the second. imapc is the CmpMap, imapn is
   the neighbouring Mapping. */
            if( where == 0 ) {
               imap1 = 0;
               imap2 = 1;
            } else {
               imap1 = where - 1;
               imap2 = where;
            }

/* Obtain the Class string of the neighbouring Mapping and determine if it
   is a CmpMap. */
            class = astGetClass( ( *map_list )[ (where>0)?where-1:1 ] );
            if ( astOK && !strcmp( class, "CmpMap" ) ) {

/* If suitable, obtain pointers to the two CmpMaps. */
               cmpmap1 = (AstCmpMap *) ( *map_list )[ imap1 ];
               cmpmap2 = (AstCmpMap *) ( *map_list )[ imap2 ];

/* Obtain the associated invert flag values. */
               invert1 = ( *invert_list )[ imap1 ];
               invert2 = ( *invert_list )[ imap2 ];

/* Extract the invert flags associated with each CmpMap sub-Mapping
   and combine these with the flag values obtained above so as to give
   the invert flag to be used with each individual sub-Mapping. */
               invert1a = cmpmap1->invert1;
               invert1b = cmpmap1->invert2;
               if ( invert1 ) {
                  invert1a = !invert1a;
                  invert1b = !invert1b;
               }
               invert2a = cmpmap2->invert1;
               invert2b = cmpmap2->invert2;
               if ( invert2 ) {
                  invert2a = !invert2a;
                  invert2b = !invert2b;
               }

/* Series CmpMaps in parallel. */
/* =========================== */
/* Now check if the CmpMaps can be merged. This may be possible if we
   are examining a list of Mappings combined in parallel and the two
   adjacent CmpMaps both combine their sub-Mappings in series. */
               if ( !series && cmpmap1->series && cmpmap2->series ) {

/* Form two new parallel CmpMaps with the sub-Mappings re-arranged so
   that when combined in series these new CmpMaps are equivalent to
   the original ones. In doing this, we must take account of the
   invert flags which apply to each sub-Mapping and also of the fact
   that the order in which the sub-Mappings are applied depends on the
   invert flags of the original CmpMaps. */
                  new1 = CombineMaps( invert1 ? cmpmap1->map2 : cmpmap1->map1,
                                      invert1 ? invert1b      : invert1a,
                                      invert2 ? cmpmap2->map2 : cmpmap2->map1,
                                      invert2 ? invert2b      : invert2a, 0, status );
                  new2 = CombineMaps( invert1 ? cmpmap1->map1 : cmpmap1->map2,
                                      invert1 ? invert1a      : invert1b,
                                      invert2 ? cmpmap2->map1 : cmpmap2->map2,
                                      invert2 ? invert2a      : invert2b, 0, status );

/* Having converted the parallel combination of series CmpMaps into a
   pair of equivalent parallel CmpMaps that can be combined in series,
   try and simplify each of these new CmpMaps. */
                  simp1 = astSimplify( new1 );
                  simp2 = astSimplify( new2 );

/* Test if either could be simplified by checking if its pointer value
   has changed. Also check if the Invert attribute has changed (not
   strictly necessary, but a useful safety feature in case of any
   rogue code which just changes this attribute instead of issuing a
   new pointer). */
                  simpler = ( simp1 != new1 ) || ( simp2 != new2 ) ||
                            astGetInvert( simp1 ) || astGetInvert( simp2 );

/* If either CmpMap was simplified, then combine the resulting
   Mappings in series to give the replacement CmpMap. */
                  if ( simpler ) new =
                               (AstMapping *) astCmpMap( simp1, simp2, 1, "", status );

/* Annul the temporary Mapping pointers. */
                  new1 = astAnnul( new1 );
                  new2 = astAnnul( new2 );
                  simp1 = astAnnul( simp1 );
                  simp2 = astAnnul( simp2 );

/* Parallel CmpMaps in series. */
/* =========================== */
/* A pair of adjacent CmpMaps can also potentially be merged if we are
   examining a list of Mappings combined in series and the two
   adjacent CmpMaps both combine their sub-Mappings in parallel. */
               } else if ( series && !cmpmap1->series && !cmpmap2->series ) {

/* Expand each of the two adjacent CmpMaps into a list of Mappings to be
   combined in parallel. */
                  map_list1 = map_list2 = NULL;
                  invert_list1 = invert_list2 = NULL;
                  nmap1 = nmap2 = 0;
                  (void) astMapList( (AstMapping *) cmpmap1, 0, invert1,
                                     &nmap1, &map_list1, &invert_list1 );
                  (void) astMapList( (AstMapping *) cmpmap2, 0, invert2,
                                     &nmap2, &map_list2, &invert_list2 );

/* We want to divide each of these lists into N sub-lists so that the
   outputs of the Mappings in the i'th sub-list from cmpmap1 can feed
   (i.e. equal in number) the inputs of the Mappings in the i'th sub-list
   from cmpmap2. If such a sub-list contains more than one Mapping we
   combine them together into a parallel CmpMap. Initialise a flag to
   indicate that we have not yet found any genuine simplification. */
                  simpler = 0;

/* Initialise the index of the next Mapping to be added into each
   sublist. */
                  jmap1 = jmap2 = 0;

/* Indicate both sublists are currently empty. */
                  subout1 = subin2 = 0;
                  new = submap1 = submap2 = NULL;
                  subinv1 = subinv2 = 0;

/* Loop round untill all Mappings have been used. */
                  while( jmap1 <= nmap1 && jmap2 <= nmap2 && astOK ) {

/* Note the number of outputs from submap1 and the number of inputs to
   submap2. If the Invert flag is not set to the required value for
   either Mapping, then inputs become outputs and vice-versa, so swap Nin
   and Nout. */
                     if( !submap1 ) {
                        subout1 = 0;
                     } else if( subinv1 == astGetInvert( submap1 ) ) {
                        subout1 = astGetNout( submap1 );
                     } else {
                        subout1 = astGetNin( submap1 );
                     }

                     if( !submap2 ) {
                        subin2 = 0;
                     } else if( subinv2 == astGetInvert( submap2 ) ) {
                        subin2 = astGetNin( submap2 );
                     } else {
                        subin2 = astGetNout( submap2 );
                     }

/* If sublist for cmpmap1 has too few outputs, add the next Mapping from
   the cmpmap1 list into the submap1 sublist. */
                     if( subout1 < subin2 ) {
                        tmap = CombineMaps( submap1, subinv1,
                                            map_list1[ jmap1 ],
                                            invert_list1[ jmap1 ], 0, status );
                        (void) astAnnul( submap1 );
                        submap1 = tmap;
                        subinv1 = 0;
                        jmap1++;

/* If sublist for cmpmap2 has too few inputs, add the next Mapping from
   the cmpmap2 list into the submap2 sublist. */
                     } else if( subin2 < subout1 ) {
                        tmap = CombineMaps( submap2, subinv2,
                                            map_list2[ jmap2 ],
                                            invert_list2[ jmap2 ], 0, status );
                        (void) astAnnul( submap2 );
                        submap2 = tmap;
                        subinv2 = 0;
                        jmap2++;

/* If submap1 can now feed submap2, combine them in series, and attempt to
   simplify it. */
                     } else {

/* Check this is not the first pass (when we do not have a submap1 or
   submap2). */
                        if( submap1 && submap2 ) {

/* Combine the Mappings in series and simplify. */
                           tmap = CombineMaps( submap1, subinv1, submap2,
                                               subinv2, 1, status );
                           submap1 = astAnnul( submap1 );
                           submap2 = astAnnul( submap2 );
                           tmap2 = astSimplify( tmap );
                           tmap = astAnnul( tmap );

/* Note if any simplification took place. */
                           if( tmap != tmap2 ||
                               astGetInvert( tmap ) != astGetInvert( tmap2 ) )
                                           simpler = 1;

/* Add the simplifed Mapping into the total merged Mapping (a parallel
   CmpMap). */
                           if( !new ) {
                              new = tmap2;
                           } else {
                              tmap = (AstMapping *) astCmpMap( new, tmap2, 0,
                                                               " ", status );
                              tmap2 = astAnnul( tmap2 );
                              (void) astAnnul( new );
                              new = tmap;
                           }
                        }

/* Reset submap1 to be the next Mapping from the cmpmap1 map list. First,
   save its old Invert flag and set it to the required value. */
                        if( jmap1 < nmap1 ) {
                           submap1 = astClone( map_list1[ jmap1 ] );
                           subinv1 = invert_list1[ jmap1 ];
                           jmap1++;
                        } else {
                           break;
                        }

/* Do the same for the second list. */
                        if( jmap2 < nmap2 ) {
                           submap2 = astClone( map_list2[ jmap2 ] );
                           subinv2 = invert_list2[ jmap2 ];
                           jmap2++;
                        } else {
                           break;
                        }
                     }
                  }

/* Free the lists of Mapping pointers and invert flags. */
                  if( map_list1 ) {
                     for( jmap1 = 0; jmap1 < nmap1; jmap1++ ) {
                        map_list1[ jmap1 ] = astAnnul( map_list1[ jmap1 ] );
                     }
                     map_list1 = astFree( map_list1 );
                  }
                  invert_list1 = astFree( invert_list1 );

                  if( map_list2 ) {
                     for( jmap2 = 0; jmap2 < nmap2; jmap2++ ) {
                        map_list2[ jmap2 ] = astAnnul( map_list2[ jmap2 ] );
                     }
                     map_list2 = astFree( map_list2 );
                  }
                  invert_list2 = astFree( invert_list2 );

               }
            }

/* Update Mapping list. */
/* ==================== */
/* If adjacent CmpMaps can be combined, then annul the original pointers. */
            if ( astOK && simpler ) {
               ( *map_list )[ imap1 ] = astAnnul( ( *map_list )[ imap1 ] );
               ( *map_list )[ imap2 ] = astAnnul( ( *map_list )[ imap2 ] );

/* Insert the pointer to the replacement CmpMap and initialise its
   invert flag. */
               ( *map_list )[ imap1 ] = new;
               ( *invert_list )[ imap1 ] = 0;

/* Loop to close the resulting gap by moving subsequent elements down
   in the arrays. */
               for ( imap = imap2 + 1; imap < *nmap; imap++ ) {
                  ( *map_list )[ imap - 1 ] = ( *map_list )[ imap ];
                  ( *invert_list )[ imap - 1 ] = ( *invert_list )[ imap ];
               }

/* Clear the vacated elements at the end. */
               ( *map_list )[ *nmap - 1 ] = NULL;
               ( *invert_list )[ *nmap - 1 ] = 0;

/* Decrement the Mapping count and return the index of the first
   modified element. */
               ( *nmap )--;
               result = imap1;
            }
         }
      }
   }

/* If we are merging the Mappings in series, and if the nominated CmpMap
   is a parallel CmpMap, and if the lower neighbour is a PermMap, it may
   be possible to swap the PermMap and the CmpMap. This may allow one of
   the two swapped Mappings to merge with its new neighbour.
   ==================================================================== */

/* Only do this if no simplification occurred above, and if the Mappings
   are being merged in series, and if the nominated Mapping is not the
   first in the list. */
   if( result == -1 && where > 0 ){

/* Obtain the indices of the two potential Mappings to be swapped. */
      imap1 = where - 1;
      imap2 = where;

/* Obtain a pointer to the CmpMap. */
      cmpmap2 = (AstCmpMap *) ( *map_list )[ imap2 ];

/* Obtain the Class string of the first (previous) Mapping and
   determine if it is a PermMap. Also check that the nominated Mapping is
   a parallel CmpMap. */
      class = astGetClass( ( *map_list )[ imap1 ] );
      if ( astOK && !strcmp( class, "PermMap" ) && !cmpmap2->series) {

/* Indicate we have no new Mapping to store. */
         new = NULL;

/* If suitable, obtain a pointer to the PermMap. */
         permmap1 = (AstPermMap *) ( *map_list )[ imap1 ];

/* Obtain the current values of the Invert attribute in the Mappings. */
         invert1 = astGetInvert( permmap1 );
         invert2 = astGetInvert( cmpmap2 );

/* Temporarily set the Invert attributes of both Mappings to the values
   supplied in the "invert_list" parameter. */
         astSetInvert( permmap1, ( *invert_list )[ imap1 ] );
         astSetInvert( cmpmap2, ( *invert_list )[ imap2 ] );

/* Get the number of inputs and outputs for the PermMap.*/
         npout = astGetNout( permmap1 );
         npin = astGetNin( permmap1 );

/* Get the number of inputs and outputs for the two components of the
   nominated parallel CmpMap. */
         nin2a = astGetNin( cmpmap2->map1 );
         nin2b = astGetNin( cmpmap2->map2 );
         nout2a = astGetNout( cmpmap2->map1 );
         nout2b = astGetNout( cmpmap2->map2 );

/* Get the input and output axis permutation arrays and the constants
   array from the PermMap */
         inperm =astGetInPerm( permmap1 );
         outperm =astGetOutPerm( permmap1 );
         conperm = astGetConstants( permmap1 );

/* In order to swap the Mappings, the PermMap outputs which feed the
   inputs of the first component of the parallel CmpMap must be copied
   from a contiguous block at the end of the list of PermMap inputs, or
   must all be assigned constant values. Likewise, the PermMap outputs which
   feed the inputs of the second component of the parallel CmpMap must be
   copied from a contiguous block at the beggining of the list of PermMap
   inputs or must be assigned constant values. Also, there must be a
   one-to-one correspondance between inputs and outputs in the PermMap.
   Check that the first block of nin2a PermMap outputs are copied from
   the last block of nin2a PermMap inputs (and vica-versa) or are constant. */
         canswap = ( npin == npout );
         aconstants = ( outperm[ 0 ] < 0 );

         for( i = 0, j = npin - nin2a; i < nin2a; i++, j++ ) {
            if( aconstants ) {
               if( outperm[ i ] >= 0 ) {
                  canswap = 0;
                  break;
               }

            } else if( outperm[ i ] != j || inperm[ j ] != i ) {
               canswap = 0;
               break;
            }
         }

/* Check that the first block of nin2b PermMap inputs are copied from
   the last block of nin2b PermMap outputs, and vica-versa. */
         bconstants = ( outperm[ nin2a ] < 0 );
         for( i = 0, j = npout - nin2b; i < nin2b; i++, j++ ) {
            if( bconstants ) {
               if( outperm[ j ] >= 0 ) {
                  canswap = 0;
                  break;
               }
            } else if( inperm[ i ] != j || outperm[ j ] != i ) {
               canswap = 0;
               break;
            }
         }

/* If the Mappings can be swapped.. */
         new_pm = NULL;
         new_cm = NULL;
         qa = NULL;
         qb = NULL;
         if( canswap ) {

/* Temporarily set the Invert attributes of the component Mappings to the
   values they had when the CmpMap was created. */
            invert2a = astGetInvert( cmpmap2->map1 );
            invert2b = astGetInvert( cmpmap2->map2 );
            astSetInvert( cmpmap2->map1, cmpmap2->invert1 );
            astSetInvert( cmpmap2->map2, cmpmap2->invert2 );

/* If any PermMap outputs are constant, we will need the results of
   transforming these constants using the CmpMap which follows. */
            if( aconstants || bconstants ) {

/* Transform a set of bad inputs using the PermMap. This will assign the
   PermMap constant to any fixed outputs. */
               p = astMalloc( sizeof( double )*(size_t) npin );
               q = astMalloc( sizeof( double )*(size_t) npout );
               qa = astMalloc( sizeof( double )*(size_t) nout2a );
               qb = astMalloc( sizeof( double )*(size_t) nout2b );
               if( astOK ) {
                  for( i = 0; i < npin; i++ ) p[ i ] = AST__BAD;
                  astTranN( permmap1, 1, npin, 1, p, 1, npout, 1, q );

/* Transform the PermMap outputs using the two component Mappings in the
   CmpMap. */
                  astTranN( cmpmap2->map1, 1, nin2a, 1, q, 1, nout2a, 1, qa );
                  astTranN( cmpmap2->map2, 1, nin2b, 1, q + nin2a, 1, nout2b, 1, qb );

               }
               p = astFree( p );
               q = astFree( q );
            }

/* If necessary, create a UnitMap to replace a Mapping which has constant
   outputs. The number of axes for the UnitMap is chosen to give the
   correct total number of inputs for the final parallel CmpMap. At the
   same time determine the number of inputs needed by the final PermMap. */
            if( aconstants ) {
               nunit = npin - nin2b;
               npin_new = nout2b + nunit;
            } else if( bconstants ) {
               nunit = npin - nin2a;
               npin_new = nout2a + nunit;
            } else {
               nunit = 0;
               npin_new = nout2a + nout2b;
            }
            unit = nunit ? astUnitMap( nunit, "", status ) : NULL;

/* Determine the number of outputs for the final PermMap and allocate memory
   for its permutation arrays. */
            npout_new = nout2a + nout2b;
            outperm_new = astMalloc( sizeof( int )*(size_t) npout_new );
            inperm_new = astMalloc( sizeof( int )*(size_t) npin_new );
            const_new = astMalloc( sizeof( double )*(size_t) ( npout_new + npin_new ) );
            if( astOK ) {
               oconid = 0;

/* First assign permutations for the second component Mapping, if used. */
               if( !bconstants ) {
                  for( i = 0, j = npout_new - nout2b; i < nout2b; i++,j++ ) {
                     inperm_new[ i ] = j;
                     outperm_new[ j ] = i;
                  }

/* Otherwise, store constants */
               } else {

                  for( i = 0; i < nunit; i++ ){
                     iconid = inperm[ i ];
                     if( iconid >= npout ) {
                        inperm_new[ i ] = npout_new;

                     } else if( iconid >= 0 ) {
                        astError( AST__INTER, "astMapMerge(CmpMap): Swapped PermMap "
                                  "input is not constant (internal AST programming "
                                  "error)." , status);
                        break;

                     } else {
                        inperm_new[ i ] = --oconid;
                        const_new[ -( oconid + 1 ) ] = conperm[ -( iconid + 1 ) ];
                     }
                  }

                  for( i = 0, j = npout_new - nout2b; i < nout2b; i++,j++ ) {
                     outperm_new[ j ] = --oconid;
                     const_new[ -( oconid + 1 ) ] = qb[ i ];
                  }

               }

/* Now assign permutations for the first component Mapping, if used. */
               if( !aconstants ) {
                  for( i = 0, j = npin_new - nout2a; i < nout2a; i++,j++ ) {
                     inperm_new[ j ] = i;
                     outperm_new[ i ] = j;
                  }

/* Otherwise, store constants */
               } else {

                  for( i = nout2b; i < npin_new; i++ ){
                     iconid = inperm[ i - nout2b + nin2b ];
                     if( iconid >= npout ) {
                        inperm_new[ i ] = npout_new;

                     } else if( iconid >= 0 ) {
                        astError( AST__INTER, "astMapMerge(CmpMap): Swapped PermMap "
                                  "input is not constant (internal AST programming "
                                  "error)." , status);
                        break;

                     } else {
                        inperm_new[ i ] = --oconid;
                        const_new[ -( oconid + 1 ) ] = conperm[ -( iconid + 1 ) ];
                     }
                  }

                  for( i = 0; i < nout2a; i++ ) {
                     outperm_new[ i ] = --oconid;
                     const_new[ -( oconid + 1 ) ] = qa[ i ];
                  }

               }

/* Create the new PermMap */
               new_pm = astPermMap( npin_new, inperm_new, npout_new,
                                    outperm_new, const_new, "", status );

/* Create the new CmpMap.*/
               if( aconstants ) {
                  if( unit ) {
                     new_cm = astCmpMap( cmpmap2->map2, unit, 0, "", status );
                  } else {
                     new_cm = astCopy( cmpmap2->map2 );
                  }

               } else if( bconstants ) {
                  if( unit ) {
                     new_cm = astCmpMap( unit, cmpmap2->map1, 0, "", status );
                  } else {
                     new_cm = astCopy( cmpmap2->map1 );
                  }

               } else{
                  new_cm = astCmpMap( cmpmap2->map2, cmpmap2->map1, 0, "", status );
               }

            }

/* Free Memory. */
            if( unit ) unit = astAnnul( unit );
            outperm_new = astFree( outperm_new );
            inperm_new = astFree( inperm_new );
            const_new = astFree( const_new );
            if( aconstants || bconstants ) {
               qa = astFree( qa );
               qb = astFree( qb );
            }

/* Re-instate the original Invert attributes in the component Mappings. */
            astSetInvert( cmpmap2->map1, invert2a );
            astSetInvert( cmpmap2->map2, invert2b );

         }

/* Release the arrays holding the input and output permutation arrays
   and constants copied from the PermMap. */
         inperm = astFree( inperm );
         outperm = astFree( outperm );
         conperm = astFree( conperm );

/* Re-instate the original values of the Invert attributes of both
   Mappings. */
         astSetInvert( permmap1, invert1 );
         astSetInvert( cmpmap2, invert2 );

/* If the Mappings can be swapped... */
         if( astOK && canswap ) {

/* Annul the supplied pointer to the two Mappings. */
            ( *map_list )[ imap1 ] = astAnnul( ( *map_list )[ imap1 ] );
            ( *map_list )[ imap2 ] = astAnnul( ( *map_list )[ imap2 ] );

/* Store the new PermMap pointer in the slot previously occupied by the
   nominated CmpMap pointer. Likewise, store the invert flag. */
            ( *map_list )[ imap2 ] = (AstMapping *) new_pm;
            ( *invert_list )[ imap2 ] = astGetInvert( new_pm );

/* Store the new PermMap pointer in the slot previously occupied by the
   nominated CmpMap pointer. Likewise, store the invert flag. */
            ( *map_list )[ imap1 ] = (AstMapping *) new_cm;
            ( *invert_list )[ imap1 ] = astGetInvert( new_cm );

/* Return the index of the first modified element. */
            result = imap1;

         }
      }
   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = -1;

/* Return the result. */
   return result;
}

static int *MapSplit1( AstMapping *this, int nin, const int *in, AstMapping **map, int *status ){
/*
*  Name:
*     MapSplit1

*  Purpose:
*     Create a Mapping representing a subset of the inputs of an existing
*     Mapping.

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpmap.h"
*     int *MapSplit1( AstMapping *this, int nin, const int *in, AstMapping **map )

*  Class Membership:
*     CmpMap method

*  Description:
*     This function performs the work for the astMapSplit method. It
*     first invokes the astMapSplit method to see if the forward
*     transformation of the supplied Mapping (not necessarily a CmpMap)
*     can be split as requested. If this is not possible it invokes MapSplit2
*     which attempts an inverse approach to the problem. For each possible
*     sub-sets of the Mapping outputs it call astMapSplit to see if the
*     sub-set of outputs are generated from the selected inputs.

*  Parameters:
*     this
*        Pointer to the Mapping to be split. It is not assumed to be a CmpMap.
*     nin
*        The number of inputs to pick from "this".
*     in
*        Pointer to an array of indices (zero based) for the inputs which
*        are to be picked. This array should have "nin" elements. If "Nin"
*        is the number of inputs of the supplied Mapping, then each element
*        should have a value in the range zero to Nin-1.
*     map
*        Address of a location at which to return a pointer to the new
*        Mapping. This Mapping will have "nin" inputs (the number of
*        outputs may be different to "nin"). A NULL pointer will be
*        returned if the supplied Mapping has no subset of outputs which
*        depend only on the selected inputs.

*  Returned Value:
*     A pointer to a dynamically allocated array of ints. The number of
*     elements in this array will equal the number of outputs for the
*     returned Mapping. Each element will hold the index of the
*     corresponding output in the supplied Mapping. The array should be
*     freed using astFree when no longer needed. A NULL pointer will
*     be returned if no output Mapping can be created.

*  Notes:
*     - If this function is invoked with the global error status set,
*     or if it should fail for any reason, then NULL values will be
*     returned as the function value and for the "map" pointer.
*/

/* Local Variables: */
   int *result;       /* Axis order to return */

/* Initialise */
   result = NULL;
   *map = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* First see if the forward transformation can be split as requested. */
   result = astMapSplit( this, nin, in, map );

/* If forward transformation could not be split, we attempt to split the
   inverse transformation by selecting every possible sub-set of Mapping
   outputs until one is found which is fed by the requested mapping inputs. */
   if( !result ) result = MapSplit2( this, nin, in, map, status );

/* Free returned resources if an error has occurred. */
   if( !astOK ) {
      result = astFree( result );
      *map = astAnnul( *map );
   }

/* Return the list of output indices. */
   return result;
}

static int *MapSplit2( AstMapping *this, int nin, const int *in, AstMapping **map, int *status ){
/*
*  Name:
*     MapSplit2

*  Purpose:
*     Create a Mapping representing a subset of the inputs of an existing
*     Mapping.

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpmap.h"
*     int *MapSplit2( AstMapping *this, int nin, const int *in, AstMapping **map )

*  Class Membership:
*     CmpMap method

*  Description:
*     This function attempts to split the supplied Mapping using an
*     inverse approach to the problem. For each possible sub-sets of the
*     Mapping outputs it call astMapSplit to see if the sub-set of outputs
*     are generated from the selected inputs.

*  Parameters:
*     this
*        Pointer to the Mapping to be split. It is not assumed to be a CmpMap.
*     nin
*        The number of inputs to pick from "this".
*     in
*        Pointer to an array of indices (zero based) for the inputs which
*        are to be picked. This array should have "nin" elements. If "Nin"
*        is the number of inputs of the supplied Mapping, then each element
*        should have a value in the range zero to Nin-1.
*     map
*        Address of a location at which to return a pointer to the new
*        Mapping. This Mapping will have "nin" inputs (the number of
*        outputs may be different to "nin"). A NULL pointer will be
*        returned if the supplied Mapping has no subset of outputs which
*        depend only on the selected inputs.

*  Returned Value:
*     A pointer to a dynamically allocated array of ints. The number of
*     elements in this array will equal the number of outputs for the
*     returned Mapping. Each element will hold the index of the
*     corresponding output in the supplied Mapping. The array should be
*     freed using astFree when no longer needed. A NULL pointer will
*     be returned if no output Mapping can be created.

*  Notes:
*     - If this function is invoked with the global error status set,
*     or if it should fail for any reason, then NULL values will be
*     returned as the function value and for the "map" pointer.
*/

/* Local Variables: */
   AstMapping *map2;  /* Subset Mapping */
   AstMapping *this2; /* Inverted copy of the supplied Mapping */
   int *out;          /* Selected output indices */
   int *result;       /* Axis order to return */
   int *result2;      /* Axis order for current output subset */
   int i;             /* Loop count */
   int iscmp;         /* Is "this" a CmpMap? */
   int j;             /* Loop count */
   int mout;          /* Number of selected outputs */
   int nin2;          /* Number of inputs fed by current outputs */
   int nout;          /* The number of outputs from the supplied Mapping */
   int ok;            /* Are all required inputs fed by current outputs? */

/* Initialise */
   result = NULL;
   *map = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get the number of Mapping outputs. */
   nout = astGetNout( this );

/* Get an inverted copy of the Mapping. We do this rather than inverting
   the supplied Maping in case an error occurs which may leave the
   supplied Mapping inverted. */
   this2 = astCopy( this );
   astInvert( this2 );

/* Note if the Mapping is a CmpMap. */
   iscmp = astIsACmpMap( this );

/* Allocate memory to hold the selected output indices. */
   out = astMalloc( nout*sizeof( int ) );

/* Loop round all useful subset sizes. */
   if( out ) {
      for( mout = 1; mout < nout && !result; mout++ ) {

/* Initialise the first subset of outputs to check at the current subset
   size. */
         for( i = 0; i < mout; i++ ) out[ i ] = 0;

/* Loop round all ways of picking a subset of "mout" outputs from the total
   available "nout" outputs. */
         while( ! result ) {

/* Skip this subset if it refers to any axis index more than once. */
            ok = 1;
            for( i = 1; i < mout && ok; i++ ) {
               for( j = 0; j < i; j++ ) {
                  if( out[ i ] == out[ j ] ) {
                     ok = 0;
                     break;
                  }
               }
            }
            if( ok ) {

/* Attempt to split the inverted Mapping using the current subset of
   outputs. Take care to avoid an infinite loop if "this" is a CmpMap. */
               if( iscmp ) {
                  result2 = MapSplit0( this2, mout, out, &map2, 1, status );
               } else {
                  result2 = astMapSplit( this2, mout, out, &map2 );
               }

/* If succesful... */
               if( result2 ) {

/* See if the inputs that feed the current subset of outputs are the same
   as the inputs specified by the caller (and in the same order). */
                  nin2 = astGetNout( map2 );
                  ok = ( nin2 == nin );
                  if( ok ) {
                     for( i = 0; i < nin; i++ ) {
                        if( in[ i ] != result2[ i ] ) {
                           ok = 0;
                           break;
                        }
                     }
                  }

/* If so, set up the values returned to the caller. */
                  if( ok ) {
                     result = astStore( result, out, mout*sizeof(int) );
                     astInvert( map2 );
                     *map = astClone( map2 );
                  }

/* Free resources. */
                  result2 = astFree( result2 );
                  map2 = astAnnul( map2 );
               }
            }

/* Increment the first axis index. */
            i = 0;
            out[ i ]++;

/* If the incremented axis index is now too high, reset it to zero and
   increment the next higher axis index. Do this until an incremented axis
   index is not too high. */
            while( out[ i ] == nout ) {
               out[ i++ ] = 0;

               if( i < mout ) {
                  out[ i ]++;
               } else {
                  break;
               }
            }

/* If all subsets have been checked break out of the loop. */
            if( i == mout ) break;

         }
      }
   }

/* Free resources. */
   out = astFree( out );
   this2 = astAnnul( this2 );

/* Free returned resources if an error has occurred. */
   if( !astOK ) {
      result = astFree( result );
      *map = astAnnul( *map );
   }

/* Return the list of output indices. */
   return result;
}

static int *MapSplit0( AstMapping *this_mapping, int nin, const int *in,
                       AstMapping **map, int reentry, int *status ){
/*
*  Name:
*     MapSplit0

*  Purpose:
*     Create a Mapping representing a subset of the inputs of an existing
*     CmpMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpmap.h"
*     int *MapSplit0( AstMapping *this, int nin, const int *in,
*                     AstMapping **map, int reentry, int *status )

*  Class Membership:
*     CmpMap method

*  Description:
*     This function creates a new Mapping by picking specified inputs from
*     an existing CmpMap. This is only possible if the specified inputs
*     correspond to some subset of the CmpMap outputs. That is, there
*     must exist a subset of the CmpMap outputs for which each output
*     depends only on the selected CmpMap inputs, and not on any of the
*     inputs which have not been selected. If this condition is not met
*     by the supplied CmpMap, then a NULL Mapping is returned.

*  Parameters:
*     this
*        Pointer to the CmpMap to be split (the CmpMap is not actually
*        modified by this function).
*     nin
*        The number of inputs to pick from "this".
*     in
*        Pointer to an array of indices (zero based) for the inputs which
*        are to be picked. This array should have "nin" elements. If "Nin"
*        is the number of inputs of the supplied CmpMap, then each element
*        should have a value in the range zero to Nin-1.
*     map
*        Address of a location at which to return a pointer to the new
*        Mapping. This Mapping will have "nin" inputs (the number of
*        outputs may be different to "nin"). A NULL pointer will be
*        returned if the supplied CmpMap has no subset of outputs which
*        depend only on the selected inputs.
*     reentry
*        Set to zero if this is a top level entry, and non-zero if it is
*        a recursive entry.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a dynamically allocated array of ints. The number of
*     elements in this array will equal the number of outputs for the
*     returned Mapping. Each element will hold the index of the
*     corresponding output in the supplied CmpMap. The array should be
*     freed using astFree when no longer needed. A NULL pointer will
*     be returned if no output Mapping can be created.

*  Notes:
*     - If this function is invoked with the global error status set,
*     or if it should fail for any reason, then NULL values will be
*     returned as the function value and for the "map" pointer.
*/

/* Local Variables: */
   AstCmpMap *this;
   AstMapping **map_list;
   AstMapping *amap;
   AstMapping *bmap;
   AstPermMap *pmap;
   int *aout;
   int *cin;
   int *cout;
   int *inp;
   int *invert_list;
   int *outp;
   int *p;
   int *result;
   int doperm;
   int i;
   int ibot;
   int ibotout;
   int iin;
   int imap;
   int iout;
   int itop;
   int j;
   int naout;
   int ncin;
   int ncout;
   int nmap;
   int npin;
   int npout;
   int ok;
   int old_inv;
   int t;


/* Initialise */
   result = NULL;
   *map = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the CmpMap structure. */
   this = (AstCmpMap *) this_mapping;

/* Get the number of inputs and outputs in the supplied CmpMap. */
   npin = astGetNin( this );
   npout = astGetNout( this );

/* Check all input axis indices are valid. */
   ok = 1;
   for( i = 0; i < nin; i++ ) {
      if( in[ i ] < 0 || in[ i ] >= npin ) {
         ok = 0;
         break;
      }
   }

/* If OK, proceed. */
   if( ok ) {

/* Initialise dynamic arrays of Mapping pointers and associated Invert
   flags. */
      nmap = 0;
      map_list = NULL;
      invert_list = NULL;

/* Decompose the CmpMap into a sequence of Mappings to be applied in
   series or parallel, as appropriate, and an associated list of
   Invert flags. */
      (void) astMapList( this_mapping, this->series, astGetInvert( this ),
                         &nmap, &map_list, &invert_list );

/* First handle lists of Mapping in series. */
      if( this->series ) {

/* Initialise the array of inputs to be split from the next component
   Mapping. */
         ncin = nin;
         cin = astStore( NULL, in, sizeof( int )*nin );

/* Loop round all the component Mappings that are combined in series to form
   the supplied CmpMap. */
         for( imap = 0; imap < nmap && cin; imap++ ) {

/* Temporarily reset the Invert attribute within the commponent Mapping back
   to the value it had when the CmpMap was created. */
            old_inv = astGetInvert( map_list[ imap ] );
            astSetInvert(  map_list[ imap ], invert_list[ imap ] );

/* Attempt to split the component Mapping using the current list of
   inputs. */
            cout = MapSplit1( map_list[ imap ], ncin, cin, &amap, status );

/* If the split could be done... */
            if( amap ) {

/* The outputs that correspond to the picked inputs become the inputs to
   be picked from the next component Mapping. */
               (void) astFree( cin );
               cin = cout;
               ncin = astGetNout( amap );

/* Combine the split Mapping in series with the earlier split Mappings. */
               if( *map ) {
                  bmap = (AstMapping *) astCmpMap( *map, amap, 1, " ", status );
                  amap = astAnnul( amap );
                  (void) astAnnul( *map );
                  *map = bmap;
               } else {
                  *map = amap;
               }

/* If the split could not be done, free the array of Mapping inputs to
   indicate that no more component Mappings need be checked. */
            } else {
               cin = astFree( cin );
               cout = astFree( cout );
            }

/* Re-instate the original value of the Invert attribute within the
   commponent Mapping. */
            astSetInvert(  map_list[ imap ], old_inv );
         }

/* Return the final array of output indices. */
         result = cin;

/* Now handle lists of Mapping in parallel. */
      } else {

/* Allocate work space. */
         outp = astMalloc( sizeof(int)*(size_t)nin );
         inp = astMalloc( sizeof(int)*(size_t)nin );
         cin = astMalloc( sizeof(int)*(size_t)npin );
         cout = astMalloc( sizeof(int)*(size_t)npout );
         if( astOK ) {

/* The caller may have selected the Mapping inputs in any order, so we
   need to create a PermMap which will permute the inputs from the
   requested order to the order used by the CmpMap. First fill the outperm
   work array with its own indices. */
            for( i = 0; i < nin; i++ ) outp[ i ] = i;

/* Sort the outperm work array so that it accesses the array of input indices
   in ascending order */
            for( j = nin - 1; j > 0; j-- ) {
               p = outp;
               for( i = 0; i < j; i++,p++ ) {
                  if( in[ p[0] ] > in[ p[1] ] ) {
                     t = p[0];
                     p[0] = p[1];
                     p[1] = t;
                  }
               }
            }

/* Create the inperm array which is the inverse of the above outperm
   array. Note if the permutation is necessary. */
            doperm = 0;
            for( i = 0; i < nin; i++ ) {
               if( outp[ i ] != i ) doperm = 1;
               inp[ outp[ i ] ] = i;
            }

/* Create a PermMap which reorders the inputs into ascending order. */
            pmap = doperm ? astPermMap( nin, inp, nin, outp, NULL, "", status ) : NULL;

/* Store the sorted input indices in the inp work array. */
            for( i = 0; i < nin; i++ ) {
               inp[ i ] = in[ outp[ i ] ];
            }

/* Initialise the index within the supplied CmpMap of the last (highest)
   input in the current component Mapping. */
            itop = -1;

/* Initialise the index within the supplied CmpMap of the first (lowest)
   output for the current component Mapping. */
            ibotout = 0;

/* Initialise the index within the supplied CmpMap of the current picked input. */
            iin = 0;

/* Initialise the index of the next returned output index. */
            ncout = 0;

/* Loop round all the component Mappings that are combined in series to form
   the supplied CmpMap. */
            for( imap = 0; imap < nmap && cout; imap++ ) {

/* Temporarily reset the Invert attribute within the component Mapping back
   to the value it had when the CmpMap was created. */
               old_inv = astGetInvert( map_list[ imap ] );
               astSetInvert(  map_list[ imap ], invert_list[ imap ] );

/* Get the index within the supplied CmpMap of the first (lowest) input in
   the current component Mapping. */
               ibot = itop + 1;

/* Get the index within the supplied CmpMap of the last (highest) input in
   the current component Mapping. */
               itop += astGetNin( map_list[ imap ] );

/* Get the zero-based indicies of the required inputs that feed the current
   component Mapping. */
               ncin = 0;
               while( iin < nin && inp[ iin ] <= itop ) {
                  cin[ ncin++ ] = inp[ iin++ ] - ibot;
               }

/* Skip components from which no inputs are being picked. */
               if( ncin > 0 ) {

/* Attempt to split the component Mapping using the current list of inputs. */
                  aout = MapSplit1( map_list[ imap ], ncin, cin, &amap,
                                    status );

/* If successful... */
                  if( amap ) {

/* Correct the output indices so that they refer to the numbering scheme
   of the total CmpMap, and append to the total list of output indices. */
                     naout = astGetNout( amap );
                     for( iout = 0; iout < naout; iout++ ) {
                        cout[ ncout++ ] = aout[ iout ] + ibotout;
                     }

/* Combine the split Mapping in parallel with the earlier split Mappings. */
                     if( *map ) {
                        bmap = (AstMapping *) astCmpMap( *map, amap, 0, " ",
                                                         status );
                        amap = astAnnul( amap );
                        (void) astAnnul( *map );
                        *map = bmap;
                     } else {
                        *map = amap;
                     }

/* If the component Mapping could not be split, free the cout array to
   indicate that no more component Mappings need be considered. */
                  } else {
                     cout = astFree( cout );
                  }

/* Free remaining resources. */
                  aout = astFree( aout );
               }

/* Update the index within the supplied CmpMap of the first (lowest) output in
   the next component Mapping. */
               ibotout += astGetNout( map_list[ imap ] );

/* Re-instate the original value of the Invert attribute within the
   commponent Mapping. */
               astSetInvert(  map_list[ imap ], old_inv );
            }

/* If the requested inputs could be split from the total CmpMap, add in any
   PermMap needed to re-order the inputs. */
            if( cout && ncout ){
               if( doperm ) {
                  bmap = (AstMapping *) astCmpMap( pmap, *map, 1, "", status );
                  (void) astAnnul( *map );
                  *map = bmap;
               }

/* Also return the list of output indices. */
               result = cout;
               cout = NULL;
            }

/* Free remaining resources. */
            if( pmap ) pmap = astAnnul( pmap );
         }
         outp = astFree( outp );
         inp = astFree( inp );
         cin = astFree( cin );
         cout = astFree( cout );
      }

/* Loop to annul all the Mapping pointers in the list. */
      for ( i = 0; i < nmap; i++ ) map_list[ i ] = astAnnul( map_list[ i ] );

/* Free the dynamic arrays. */
      map_list = astFree( map_list );
      invert_list = astFree( invert_list );

   }

/* Mappings that have no outputs cannot be used. */
   if( !result && *map ) *map = astAnnul( *map );

/* If the above method failed to split the CmpMap, we attempt to split the
   inverse transformation by selecting every possible sub-set of Mapping
   outputs until one is found which is fed by the requested mapping inputs. */
   if( !result && !reentry ) result = MapSplit2( this_mapping, nin, in, map,
                                                 status );

/* Free returned resources if an error has occurred. */
   if( !astOK ) {
      result = astFree( result );
      *map = astAnnul( *map );
   }

/* Return the list of output indices. */
   return result;
}

static int *MapSplit( AstMapping *this, int nin, const int *in,
                      AstMapping **map, int *status ){
/*
*  Name:
*     MapSplit

*  Purpose:
*     Create a Mapping representing a subset of the inputs of an existing
*     CmpMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpmap.h"
*     int *MapSplit( AstMapping *this, int nin, const int *in,
*                    AstMapping **map, int *status )

*  Class Membership:
*     CmpMap method (over-rides the protected astMapSplit method
*     inherited from the Mapping class).

*  Description:
*     This function is the main entry point for the astMapSplit method.
*     It is a simple wrapper for MapSplit0 which calls MapSplit0
*     indicating that this is a top-level entry.

*  Parameters:
*     this
*        Pointer to the CmpMap to be split (the CmpMap is not actually
*        modified by this function).
*     nin
*        The number of inputs to pick from "this".
*     in
*        Pointer to an array of indices (zero based) for the inputs which
*        are to be picked. This array should have "nin" elements. If "Nin"
*        is the number of inputs of the supplied CmpMap, then each element
*        should have a value in the range zero to Nin-1.
*     map
*        Address of a location at which to return a pointer to the new
*        Mapping. This Mapping will have "nin" inputs (the number of
*        outputs may be different to "nin"). A NULL pointer will be
*        returned if the supplied CmpMap has no subset of outputs which
*        depend only on the selected inputs.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a dynamically allocated array of ints. The number of
*     elements in this array will equal the number of outputs for the
*     returned Mapping. Each element will hold the index of the
*     corresponding output in the supplied CmpMap. The array should be
*     freed using astFree when no longer needed. A NULL pointer will
*     be returned if no output Mapping can be created.

*  Notes:
*     - If this function is invoked with the global error status set,
*     or if it should fail for any reason, then NULL values will be
*     returned as the function value and for the "map" pointer.
*/
   return MapSplit0( this, nin, in, map, 0, status );
}

static int PatternCheck( int val, int check, int **list, int *list_len, int *status ){
/*
*  Name:
*     Looping

*  Purpose:
*     Check for repeating patterns in a set of integer values.

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpmap.h"
*     int PatternCheck( int val, int nmap, int **mlist, int **nlist, int *list_len )

*  Class Membership:
*     CmpMap member function.

*  Description:
*     This function appends a supplied integer to a dynamic list, creating
*     or expanding the list if necessary.It then optionally, check the
*     list for evidence of repeating patterns. If such a pattern is
*     found, its wavelength is returned.

*  Parameters:
*     val
*        The integer value to add to the list.
*     check
*        Should a check for reating patterns be performed?
*     list
*        Address of a location at which is stored a pointer to an array
*        holding the values supplied on previous invocations of this
*        function. If a NULL pointer is supplied a new array is allocated.
*        On exit, the supplied value is appended to the end of the array. The
*        array is extended as necessary. The returned pointer should be
*        freed using astFree when no longer needed.
*     list_len
*        Address of a location at which is stored the number of elements
*        in the "list" array.

*  Returned Value:
*     A non-zero "wavelength" value is returned if there is a repeating
*     pattern is found in the "list" array. Otherwise, zero is returned.
*     The "wavelength" is the number of integer values which constitute a
*     single instance of the pattern.

*  Notes:
*     - A value of 1 is returned if this function is invoked with the AST
*     error status set, or if it should fail for any reason.
*/

/* Local Variables: */
   int *wave[ 30 ];          /* Pointers to start of waves */
   int iat;                  /* Index of elements added by this invocation */
   int jat;                  /* Index of element condiered next */
   int jlo;                  /* Earliest "mlist" entry to consider */
   int k;                    /* Index of element within pattern */
   int mxwave;               /* Max pattern length to consider */
   int iwave;                /* Index of current wave */
   int nwave;                /* Number of waves required to mark a pattern */
   int result;               /* Returned flag */
   int wavelen;              /* Current pattern length */

/* Check the global status. */
   if ( !astOK ) return 1;

/* Initialise */
   result = 0;

/* If no array has been supplied, create a new array. */
   if( !(*list) ) {
      *list = astMalloc( 100*sizeof( int ) );
      *list_len = 0;
   }

/* Store the new value in the array, extending it if necessary. */
   iat = (*list_len)++;
   *list = astGrow( *list, *list_len, sizeof( int ) );
   if( astOK ) {
      (*list)[ iat ] = val;

/* If required, determine the maximum "wavelength" for looping patterns to be
   checked, and store the earliest list entry to consider. We take 3 complete
   patterns as evidence of looping, but we only do the check when the
   list length is at least 30. */
      if( check && *list_len > 29 ){
         mxwave = iat/3;
         if( mxwave > 50 ) mxwave = 50;
         jlo = iat - 3*mxwave;

/* Search backwards from the end of "list" looking for the most recent
   occurence of the supplied "val" value. Limit the search to
   wavelengths of no more than the above limit. */
         jat = iat - 1;
         while( jat >= jlo ) {
            if( (*list)[ jat ] == val ) {

/* When an earlier occurrence of "val" is found, see if the values
   which precede it are the same as the values which precede the new
   element if "list" added by this invocation. We use 3 complete
   patterns as evidence of looping, unless the wavelength is 1 in which
   case we use 30 patterns (this is because wavelengths of 1 can occur
   in short sequences legitamately). */
               wavelen = iat - jat;

               if( wavelen == 1 ) {
                  nwave = 30;
                  if( nwave > iat ) nwave = iat;
               } else {
                  nwave = 3;
               }

               if( nwave*wavelen <= *list_len ) {
                  result = wavelen;
                  wave[ 0 ] = *list + *list_len - wavelen;
                  for( iwave = 1; iwave < nwave; iwave++ ) {
                     wave[ iwave ] = wave[ iwave - 1 ] - wavelen;
                  }

                  for( k = 0; k < wavelen; k++ ) {
                     for( iwave = 1; iwave < nwave; iwave++ ) {
                        if( *wave[ iwave ] != *wave[ 0 ] ) {
                           result = 0;
                           break;
                        }
                        wave[ iwave ]++;
                     }
                     wave[ 0 ]++;
                  }
               }

/* Break if we have found a repeating pattern. */
               if( result ) break;

            }
            jat--;
         }
      }
   }

   if( !astOK ) result= 1;

/* Return the result.*/
   return result;
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
*     #include "cmpmap.h"
*     result = Rate( AstMapping *this, double *at, int ax1, int ax2, int *status )

*  Class Membership:
*     CmpMap member function (overrides the astRate method inherited
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
   AstMapping *c1;
   AstMapping *c2;
   AstCmpMap *map;
   double result;
   int old_inv1;
   int old_inv2;
   int nin1;
   int nin2;
   double *at2;
   double r1;
   double r2;
   int nout1;
   int i;

/* Check inherited status */
   if( !astOK ) return AST__BAD;

/* Get a pointer to the CmpMap structure. */
   map = (AstCmpMap *) this;

/* Note the current Invert flags of the two component Mappings. */
   old_inv1 = astGetInvert( map->map1 );
   old_inv2 = astGetInvert( map->map2 );

/* Temporarily reset them to the values they had when the CmpMap was
   created. */
   astSetInvert( map->map1, map->invert1 );
   astSetInvert( map->map2, map->invert2 );

/* If the CmpMap itself has been inverted, invert the component Mappings.
   Also note the order in which the Mappings should be applied if in series. */
   if( !astGetInvert( this ) ) {
      c1 = map->map1;
      c2 = map->map2;
   } else {
      c1 = map->map2;
      c2 = map->map1;
      astInvert( c1 );
      astInvert( c2 );
   }

/* First deal with Mappings in series. */
   if( map->series ) {

/* Get the number of inputs to the two component Mappings. */
      nin1 = astGetNin( c1 );
      nin2 = astGetNin( c2 );

/* Allocate workspace to hold the result of transforming the supplied "at"
   position using the first component. */
      at2 = astMalloc( sizeof( double )*(size_t) nin2 );

/* Transform the supplied "at" position using the first component. */
      astTranN( c1, 1, nin1, 1, at, 1, nin2, 1, at2 );

/* The required rate of change is the sum of the products of the rate of
   changes of the two component mappings, summed over all the output axes
   of the first componment. */
      result = 0.0;
      for( i = 0; i < nin2; i++ ) {

/* Find the rate of change of output "i" of the first component with
   respect to input "ax2" at the supplied "at" position. */
         r1 = astRate( c1, at, i, ax2 );

/* Find the rate of change of output "ax1" of the second component with
   respect to input "i" at the transformed "at2" position. */
         r2 = astRate( c2, at2, ax1, i );

/* If both are good, increment the ryunning total by the product of the
   two rates. Otherwise, break. */
         if( r1 != AST__BAD && r2 != AST__BAD ) {
            result += r1*r2;
         } else {
            result = AST__BAD;
            break;
         }
      }

/* Free the workspace. */
      at2 = astFree( at2 );

/* Now deal with Mappings in parallel. */
   } else {

/* Get the number of inputs and outputs for the lower component Mappings. */
      nin1 = astGetNin( map->map1 );
      nout1 = astGetNout( map->map1 );

/* If both input and output relate to the lower component Mappings, use its
   astRate method. */
      if( ax1 < nout1 && ax2 < nin1 ) {
         result = astRate( map->map1, at, ax1, ax2 );

/* If both input and output relate to the upper component Mappings, use its
   astRate method. */
      } else if( ax1 >= nout1 && ax2 >= nin1 ) {
         result = astRate( map->map2, at + nin1, ax1 - nout1, ax2 - nin1 );

/* If input and output relate to different component Mappings, return
   zero. */
      } else {
         result = 0.0;
      }
   }

/* Reinstate the original Invert flags of the component Mappings .*/
   astSetInvert( map->map1, old_inv1 );
   astSetInvert( map->map2, old_inv2 );

/* Return the result. */
   return result;
}

static AstMapping *RemoveRegions( AstMapping *this_mapping, int *status ) {
/*
*  Name:
*     RemoveRegions

*  Purpose:
*     Remove any Regions from a Mapping.

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpmap.h"
*     AstMapping *RemoveRegions( AstMapping *this, int *status )

*  Class Membership:
*     CmpMap method (over-rides the astRemoveRegions method inherited
*     from the Mapping class).

*  Description:
*     This function searches the supplied Mapping (which may be a
*     compound Mapping such as a CmpMap) for any component Mappings
*     that are instances of the AST Region class. It then creates a new
*     Mapping from which all Regions have been removed. If a Region
*     cannot simply be removed (for instance, if it is a component of a
*     parallel CmpMap), then it is replaced with an equivalent UnitMap
*     in the returned Mapping.
*
*     The implementation provided by the CmpMap class invokes the
*     astRemoveRegions method on the two component Mappings, and joins
*     the results together into a new CmpMap.

*  Parameters:
*     this
*        Pointer to the original Region.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the modified mapping.

*  Notes:
*     - A NULL pointer value will be returned if this function is
*     invoked with the AST error status set, or if it should fail for
*     any reason.
*/

/* Local Variables: */
   AstCmpMap *new;               /* Pointer to new CmpMap */
   AstCmpMap *this;              /* Pointer to CmpMap structure */
   AstMapping *newmap1;          /* New first component Mapping */
   AstMapping *newmap2;          /* New second component Mapping */
   AstMapping *result;           /* Result pointer to return */
   int nax;                      /* Number of Frame axes */
   int unit1;                    /* Is new first Mapping a UnitMap? */
   int unit2;                    /* Is new second Mapping a UnitMap? */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the CmpMap. */
   this = (AstCmpMap *) this_mapping;

/* Invoke the astRemoveRegions method on the two component Mappings. */
   newmap1 = astRemoveRegions( this->map1 );
   newmap2 = astRemoveRegions( this->map2 );

/* If neither component was modified, just return a clone of the supplied
   pointer. */
   if( this->map1 == newmap1 && this->map2 == newmap2 ) {
      result = astClone( this );

/* Otherwise, we need to create a new Mapping to return. */
   } else {

/* The implementation of the astRemoveRegions method provided by the
   Region class returns a Frame rather than a UnitMap. But we need
   Mappings here, not Frames. So if either of these new Mappings is
   a Frame, replace it with an equivalent UnitMap. Also, get flags
   indicating if either Mapping is a UnitMap.*/
      if( astIsAFrame( newmap1 ) ) {
         nax = astGetNin( newmap1 );
         (void) astAnnul( newmap1 );
         newmap1 = (AstMapping *) astUnitMap( nax, " ", status );
         unit1 = 1;
      } else {
         unit1 = astIsAUnitMap( newmap1 );
      }

      if( astIsAFrame( newmap2 ) ) {
         nax = astGetNin( newmap2 );
         (void) astAnnul( newmap2 );
         newmap2 = (AstMapping *) astUnitMap( nax, " ", status );
         unit2 = 1;
      } else {
         unit2 = astIsAUnitMap( newmap2 );
      }

/* First handle series CmpMaps. */
      if( this->series ) {

/* Otherwise, if the second new Mapping is a UnitMap, return a copy of the
   first new Mapping (with the original Invert attribute) since the second
   one will have no effect. */
         if( unit1 ) {
            result = astCopy( newmap2 );
            astSetInvert( result, this->invert2 );
            if( astGetInvert( this ) ) astInvert( result );

/* Otherwise, if the second new Mapping is a UnitMap, return a copy of the
   first new Mapping (with the original Invert attribute) since the second
   one will have no effect. */
         } else if( unit2 ) {
            result = astCopy( newmap1 );
            astSetInvert( result, this->invert1 );
            if( astGetInvert( this ) ) astInvert( result );

/* If neither of the new Mappings is a UnitMap, return a new CmpMap
   containing the two new Mappings. We take a deep copy of the supplied
   CmpMap and then modify the Mappings os that we retain any extra
   information (such as invert flags) in the supplied CmpMap. */
         } else {
            new = astCopy( this );
            (void) astAnnul( new->map1 );
            (void) astAnnul( new->map2 );
            new->map1 = astClone( newmap1 );
            new->map2 = astClone( newmap2 );
            result = (AstMapping *) new;
         }

/* Now handle parallel CmpMaps. */
      } else {

/* If both new Mappings are UnitMaps, return an equivalent UnitMap. */
         if( unit1 && unit2 ) {
            result = (AstMapping *) astUnitMap( astGetNin( newmap1 ) +
                                                astGetNin( newmap2 ), " ",
                                                status );

/* Otherwise, return a new CmpMap containing the two new Mappings. */
         } else {
            new = astCopy( this );
            (void) astAnnul( new->map1 );
            (void) astAnnul( new->map2 );
            new->map1 = astClone( newmap1 );
            new->map2 = astClone( newmap2 );
            result = (AstMapping *) new;
         }
      }
   }

/* Free resources. */
   newmap1 = astAnnul( newmap1 );
   newmap2 = astAnnul( newmap2 );

/* Annul the returned Mapping if an error has occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return the result. */
   return result;
}

static AstMapping *Simplify( AstMapping *this_mapping, int *status ) {
/*
*  Name:
*     Simplify

*  Purpose:
*     Simplify a Mapping.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     AstMapping *Simplify( AstMapping *this, int *status )

*  Class Membership:
*     CmpMap method (over-rides the astSimplify method inherited from
*     the Mapping class).

*  Description:
*     This function simplifies a CmpMap to eliminate redundant
*     computational steps, or to merge separate steps which can be
*     performed more efficiently in a single operation.

*  Parameters:
*     this
*        Pointer to the original Mapping.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A new pointer to the (possibly simplified) Mapping.

*  Notes:
*     - A NULL pointer value will be returned if this function is
*     invoked with the AST error status set, or if it should fail for
*     any reason.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstCmpMap *this;              /* Pointer to CmpMap structure */
   AstMapping **map_list;        /* Mapping array pointer */
   AstMapping *map;              /* Pointer to cloned Mapping pointer */
   AstMapping *result;           /* Result pointer to return */
   AstMapping *tmp;              /* Temporary Mapping pointer */
   int *invert_list;             /* Invert array pointer */
   int *mlist;                   /* Point to list of modified Mapping indices */
   int *nlist;                   /* Point to list of Mapping counts */
   int i;                        /* Loop counter for Mappings */
   int improved;                 /* Simplification achieved? */
   int invert;                   /* Invert attribute value */
   int invert_n;                 /* Invert value for final Mapping */
   int mlist_len;                /* No. of entries in mlist */
   int nlist_len;                /* No. of entries in nlist */
   int modified;                 /* Index of first modified Mapping */
   int nmap;                     /* Mapping count */
   int nominated;                /* Index of nominated Mapping */
   int set;                      /* Invert attribute set? */
   int set_n;                    /* Invert set for final Mapping? */
   int simpler;                  /* Simplification possible? */
   int t;                        /* Temporary storage */
   int wlen1;                    /* Pattern wavelength for "modified" values */
   int wlen2;                    /* Pattern wavelength for "nmap" values */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(this_mapping);

/* It is possible for the astSimplify method to be called recursively from
   within astSimplify. It is also possible that the Mapping being
   simplified by the current invocation is the same as the Mapping being
   simplified by some recursive invocation higher up the call stack. If
   this happens we will get into an infinite loop, since we already know
   that simplifying the supplied Mapping will involve (eventually) a
   recursive call to astSimplify with the same Mapping. To avoid this
   looping, we note the Mappings supplied at each depth and first compare
   the supplied Mapping with the Mappings which are currently being
   simplified higher up the call stack. If the supplied Mapping is
   already being simplified at a higher level, then we return immediately
   without doing any simplification. Otherwise, we record the supplied
   Mapping pointer in a static list so that it is available to subsequent
   recursive invocations of this function. First compare the supplied
   Mapping with the Mappingsbeing simpliied higher up. Return without
   action if a match is found. */
   for( i = 0; i < simplify_depth; i++ ) {
      if( astEqual( this_mapping, simplify_stackmaps[ i ] ) ) {
         return astClone( this_mapping );
      }
   }

/* We have further work to do, so increment the recursion depth, extend
   the simplify_stackmaps array, and store the new Mapping in it for future use. */
   simplify_depth++;
   simplify_stackmaps = astGrow( simplify_stackmaps, simplify_depth, sizeof( AstMapping * ) );
   if( astOK ) {
      simplify_stackmaps[ simplify_depth - 1 ] = astClone( this_mapping );
   }

/* Obtain a pointer to the CmpMap structure. */
   this = (AstCmpMap *) this_mapping;

/* Initialise dynamic arrays of Mapping pointers and associated Invert
   flags. */
   nmap = 0;
   map_list = NULL;
   invert_list = NULL;

/* Decompose the CmpMap into a sequence of Mappings to be applied in
   series or parallel, as appropriate, and an associated list of
   Invert flags. If any inverted CmpMaps are found in the Mapping, then
   we can at least simplify the returned Mapping by swapping and
   inverting the components. Set "simpler" to indicate this. */
   simpler = astMapList( this_mapping, this->series, astGetInvert( this ), &nmap,
                         &map_list, &invert_list );

/* Each Mapping has a flag that indicates if the mapping is frozen (i.e. cannot
   be nominated for simplification). Mappings become frozen if nominating them
   would create an infinite loop in which neighbouring mappings argue as to
   their form. Freezing a mapping prevents the frozen mapping contributing any
   further to the argument, so the other Mapping "wins" the argument.
   Ensure no Mappings are frozen to begin with. */
   for( i = 0; i < nmap; i++ ) {
      map_list[ i ]->flags &= ~AST__FROZEN_FLAG;
   }

/* Initialise pointers to memory used to hold lists of the modified
   Mapping index and the number of mappings after each call of
   astMapMerge. */
   mlist = NULL;
   nlist = NULL;

/* Loop to simplify the sequence until a complete pass through it has
   been made without producing any improvement. */
   improved = 1;
   while ( astOK && improved ) {
      improved = 0;

/* Loop to nominate each Mapping in the sequence in turn. */
      nominated = 0;
      while ( astOK && ( nominated < nmap ) ) {

/* If the current nominated mapping has been frozen, then we do not allow
   it to suggest changes to the mapping sequence. Instead, just increment
   the index of the next mapping to be checked and continue on to the next
   pass round the while loop. */
         if( map_list[ nominated ]->flags & AST__FROZEN_FLAG ) {
            nominated++;
            continue;
         }

/* Clone a pointer to the nominated Mapping and attempt to merge it
   with its neighbours. Annul the cloned pointer afterwards. */
         map = astClone( map_list[ nominated ] );
         modified = astMapMerge( map, nominated, this->series,
                                 &nmap, &map_list, &invert_list );
         map = astAnnul( map );

/* Move on to nominate the next Mapping in the sequence. */
         nominated++;

/* Note if any simplification occurred above. */
         if( modified >= 0 ) {

/* Append the index of the first modified Mapping in the list and and check
   that there is no repreating pattern in the list. If there is, we are
   probably in a loop where one mapping class is making a change, and another
   is undoing the change. The Looping function returns the "wavelength"
   of any pattern found. If a pattern was discovered, we ignore it unless
   there is also a pattern in the "nmap" values - the wavelengths of the
   two patterns must be related by a integer factor. */
            wlen1 = PatternCheck( modified, 1, &mlist, &mlist_len, status );
            wlen2 = PatternCheck( nmap, wlen1, &nlist, &nlist_len, status );
            if( wlen1 && wlen2 ) {

/* Ensure wlen2 is larger than or equal to wlen1. */
               if( wlen1 > wlen2 ) {
                  t = wlen1;
                  wlen1 = wlen2;
                  wlen2 = t;
               }

/* See if wlen2 is an integer multiple of wlen1. If not, ignore the
   patterns. */
               if( ( wlen2 % wlen1 ) != 0 ) wlen1 = 0;
            }

/* If a repeating pattern is occurring, set the frozen flag in order to
   prevent the modified mapping from being modified any more. */
            if( wlen1 > 0 ) {
               map_list[ modified ]->flags |= AST__FROZEN_FLAG;

/* Otherwise, indicate we have improved the mapping and go round to test
   the next nominated mapping. */
            } else {
               improved = 1;
               simpler = 1;

/* If the simplification resulted in modification of an earlier
   Mapping than would normally be considered next, then go back to
   consider the modified one first. */
               if ( modified < nominated ) nominated = modified;
            }
         }
      }
   }

/* Free resources */
   mlist = astFree( mlist );
   nlist = astFree( nlist );

/* Construct the output Mapping. */
/* ============================= */
/* If no simplification occurred above, then simply clone a pointer to
   the original Mapping. */
   if ( astOK ) {
      if ( !simpler ) {
         result = astClone( this );

/* Otherwise, we must construct the result from the contents of the
   Mapping list. */
      } else {

/* If the simplified Mapping list has only a single element, then the
   output Mapping will not be a CmpMap. In this case, we cannot
   necessarily set the Invert flag of the Mapping to the value we want
   (because we must not modify the Mapping itself. */
         if ( nmap == 1 ) {

/* We must make a copy. Cloning is no good (even if the Mapping already
   has the Invert attribute value we want), since we want the returned
   Mapping to be independent of the original component Mappings, so that
   if user code inverts a component Mapping (via some other pre-existing
   pointer), the returned simplified Mapping is not affected. */
            result = astCopy( map_list[ 0 ] );

/* Either clear the copy's Invert attribute, or set it to 1, as
   required. */
            if ( invert_list[ 0 ] ) {
               astSetInvert( result, 1 );
            } else {
               astClearInvert( result );
            }

/* If the simplified Mapping sequence has more than one element, the
   output Mapping will be a CmpMap. In this case, we can set each
   individual Mapping element to have the Invert attribute value we
   want, so long as we return these attribute values to their original
   state again afterwards (once a Mapping is encapsulated inside a
   CmpMap, further external changes to its Invert attribute do not
   affect the behaviour of the CmpMap). */
         } else {

/* Determine if the Invert attribute for the last Mapping is set, and
   obtain its value. */
            set_n = astTestInvert( map_list[ nmap - 1 ] );
            invert_n = astGetInvert( map_list[ nmap - 1 ] );

/* Set this attribute to the value we want. */
            astSetInvert( map_list[ nmap - 1 ], invert_list[ nmap - 1 ] );

/* Loop through the Mapping sequence in reverse to merge it into an
   equivalent CmpMap. */
            for ( i = nmap - 1; i >= 0; i-- ) {

/* Simply clone the pointer to the last Mapping in the sequence (which
   will be encountered first). */
              if ( !result ) {
                  result = astClone( map_list[ i ] );

/* For subsequent Mappings, test if the Invert attribute is set and
   save its value. */
               } else {
                  set = astTestInvert( map_list[ i ] );
                  invert = astGetInvert( map_list[ i ] );

/* Set this attribute to the value required. */
                  astSetInvert( map_list[ i ], invert_list[ i ] );

/* Combine the Mapping with the CmpMap formed so far and replace the
   result pointer with the new pointer this produces, annulling the
   previous pointer. */
                  tmp = (AstMapping *) astCmpMap( map_list[ i ], result,
                                                  this->series, "", status );
                  (void) astAnnul( result );
                  result = tmp;

/* Restore the Invert attribute of the Mapping to its original
   state. */
                  if ( !set ) {
                     astClearInvert( map_list[ i ] );
                  } else {
                     astSetInvert( map_list[ i ], invert );
                  }
               }
            }

/* When all the Mappings have been merged into the CmpMap, restore the
   state of the Invert attribute for the final Mapping in the
   sequence. */
            if ( !set_n ) {
               astClearInvert( map_list[ nmap - 1  ] );
            } else {
               astSetInvert( map_list[ nmap - 1 ], invert_n );
            }
         }
      }
   }

/* Clean up. */
/* ========= */
/* Loop to annul all the Mapping pointers in the simplified list. */
   for ( i = 0; i < nmap; i++ ) map_list[ i ] = astAnnul( map_list[ i ] );

/* Free the dynamic arrays. */
   map_list = astFree( map_list );
   invert_list = astFree( invert_list );

/* Decrement the recursion depth and free the pointer to the supplied
   Mapping currently stored at the end of the simplify_stackmaps array. */
   simplify_depth--;
   if( astOK ) {
      simplify_stackmaps[ simplify_depth ] = astAnnul( simplify_stackmaps[ simplify_depth ] );
   }

/* If we are now at depth zero, free the simplify_stackmaps array. */
   if( simplify_depth == 0 ) simplify_stackmaps = astFree( simplify_stackmaps );

/* If an error occurred, annul the returned Mapping. */
   if ( !astOK ) result = astAnnul( result );

/* Return the result. */
   return result;
}

static AstPointSet *Transform( AstMapping *this, AstPointSet *in,
                               int forward, AstPointSet *out, int *status ) {
/*
*  Name:
*     Transform

*  Purpose:
*     Apply a CmpMap to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "cmpmap.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     CmpMap member function (over-rides the astTransform method inherited
*     from the Mapping class).

*  Description:
*     This function takes a CmpMap and a set of points encapsulated in a
*     PointSet and transforms the points so as to apply the required Mapping.
*     This implies applying each of the CmpMap's component Mappings in turn,
*     either in series or in parallel.

*  Parameters:
*     this
*        Pointer to the CmpMap.
*     in
*        Pointer to the PointSet associated with the input coordinate values.
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
*     match the number of coordinates for the CmpMap being applied.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstCmpMap *map;               /* Pointer to CmpMap to be applied */
   AstPointSet *result;          /* Pointer to output PointSet */
   AstPointSet *temp1;           /* Pointer to temporary PointSet */
   AstPointSet *temp2;           /* Pointer to temporary PointSet */
   AstPointSet *temp;            /* Pointer to temporary PointSet */
   int forward1;                 /* Use forward direction for Mapping 1? */
   int forward2;                 /* Use forward direction for Mapping 2? */
   int ipoint1;                  /* Index of first point in batch */
   int ipoint2;                  /* Index of last point in batch */
   int nin1;                     /* No. input coordinates for Mapping 1 */
   int nin2;                     /* No. input coordinates for Mapping 2 */
   int nin;                      /* No. input coordinates supplied */
   int nout1;                    /* No. output coordinates for Mapping 1 */
   int nout2;                    /* No. output coordinates for Mapping 2 */
   int nout;                     /* No. output coordinates supplied */
   int np;                       /* Number of points in batch */
   int npoint;                   /* Number of points to be transformed */

/* Local Constants: */
   const int nbatch = 2048;      /* Maximum points in a batch */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the CmpMap. */
   map = (AstCmpMap *) this;

/* Apply the parent Mapping using the stored pointer to the Transform member
   function inherited from the parent Mapping class. This function validates
   all arguments and generates an output PointSet if necessary, but does not
   actually transform any coordinate values. */
   result = (*parent_transform)( this, in, forward, out, status );

/* We now extend the parent astTransform method by applying the component
   Mappings of the CmpMap to generate the output coordinate values. */

/* Determine whether to apply the forward or inverse Mapping, according to the
   direction specified and whether the Mapping has been inverted. */
   if ( astGetInvert( map ) ) forward = !forward;

/* Check if either component Mapping's inversion flag has changed since it was
   used to construct the CmpMap. Set a "forward" flag for each Mapping to
   change the direction we will use, to compensate if necessary. (Such changes
   may have occurred if other pointers to the component Mappings are in
   circulation). */
   forward1 = forward;
   forward2 = forward;
   if ( map->invert1 != astGetInvert( map->map1 ) ) forward1 = !forward1;
   if ( map->invert2 != astGetInvert( map->map2 ) ) forward2 = !forward2;

/* Determine the number of points being transformed. */
   npoint = astGetNpoint( in );

/* Mappings in series. */
/* ------------------- */
/* If required, use the two component Mappings in series. To do this, we must
   apply one Mapping followed by the other, which means storing an intermediate
   result. Since this function may be invoked recursively and have to store an
   intermediate result on each occasion, the memory required may become
   excessive when transforming large numbers of points. To overcome this, we
   split the points up into smaller batches. */
   if ( astOK ) {
      if ( map->series ) {

/* Obtain the numbers of input and output coordinates. */
         nin = astGetNcoord( in );
         nout = astGetNcoord( result );

/* Loop to process all the points in batches, of maximum size nbatch points. */
         for ( ipoint1 = 0; ipoint1 < npoint; ipoint1 += nbatch ) {

/* Calculate the index of the final point in the batch and deduce the number of
   points (np) to be processed in this batch. */
            ipoint2 = ipoint1 + nbatch - 1;
            if ( ipoint2 > npoint - 1 ) ipoint2 = npoint - 1;
            np = ipoint2 - ipoint1 + 1;

/* Create temporary PointSets to describe the input and output points for this
   batch. */
            temp1 = astPointSet( np, nin, "", status );
            temp2 = astPointSet( np, nout, "", status );

/* Associate the required subsets of the input and output coordinates with the
   two PointSets. */
            astSetSubPoints( in, ipoint1, 0, temp1 );
            astSetSubPoints( result, ipoint1, 0, temp2 );

/* Apply the two Mappings in sequence and in the required order and direction.
   Store the intermediate result in a temporary PointSet (temp) which is
   created by the first Mapping applied. */
            if ( forward ) {
               temp = astTransform( map->map1, temp1, forward1, NULL );
               (void) astTransform( map->map2, temp, forward2, temp2 );
            } else {
               temp = astTransform( map->map2, temp1, forward2, NULL );
               (void) astTransform( map->map1, temp, forward1, temp2 );
            }

/* Delete the temporary PointSets after processing each batch of points. */
            temp = astDelete( temp );
            temp1 = astDelete( temp1 );
            temp2 = astDelete( temp2 );

/* Quit processing batches if an error occurs. */
            if ( !astOK ) break;
         }

/* Mappings in parallel. */
/* --------------------- */
/* If required, use the two component Mappings in parallel. Since we do not
   need to allocate any memory to hold intermediate coordinate values here,
   there is no need to process the points in batches. */
      } else {

/* Get the effective number of input and output coordinates per point for each
   Mapping (taking account of the direction in which each will be used to
   transform points). */
         nin1 = forward1 ? astGetNin( map->map1 ) : astGetNout( map->map1 );
         nout1 = forward1 ? astGetNout( map->map1 ) : astGetNin( map->map1 );
         nin2 = forward2 ? astGetNin( map->map2 ) : astGetNout( map->map2 );
         nout2 = forward2 ? astGetNout( map->map2 ) : astGetNin( map->map2 );

/* Create temporary PointSets to describe the input and output coordinates for
   the first Mapping. */
         temp1 = astPointSet( npoint, nin1, "", status );
         temp2 = astPointSet( npoint, nout1, "", status );

/* Associate the required subsets of the input and output coordinates with
   these PointSets. */
         astSetSubPoints( in, 0, 0, temp1 );
         astSetSubPoints( result, 0, 0, temp2 );

/* Use the astTransform method to apply the coordinate transformation described
   by the first Mapping. */
         (void) astTransform( map->map1, temp1, forward1, temp2 );

/* Delete the temporary PointSets. */
         temp1 = astDelete( temp1 );
         temp2 = astDelete( temp2 );

/* Create a new pair of temporary PointSets to describe the input and output
   coordinates for the second Mapping, and associate the required subsets of
   the input and output coordinates with these PointSets. */
         temp1 = astPointSet( npoint, nin2, "", status );
         temp2 = astPointSet( npoint, nout2, "", status );
         astSetSubPoints( in, 0, nin1, temp1 );
         astSetSubPoints( result, 0, nout1, temp2 );

/* Apply the coordinate transformation described by the second Mapping. */
         (void) astTransform( map->map2, temp1, forward2, temp2 );

/* Delete the two temporary PointSets. */
         temp1 = astDelete( temp1 );
         temp2 = astDelete( temp2 );
      }
   }

/* If an error occurred, clean up by deleting the output PointSet (if
   allocated by this function) and setting a NULL result pointer. */
   if ( !astOK ) {
      if ( !out ) result = astDelete( result );
      result = NULL;
   }

/* Return a pointer to the output PointSet. */
   return result;
}

/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout, int *status ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for CmpMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for CmpMap objects.

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
*     -  This constructor makes a deep copy, including a copy of the component
*     Mappings within the CmpMap.
*/

/* Local Variables: */
   AstCmpMap *in;                /* Pointer to input CmpMap */
   AstCmpMap *out;               /* Pointer to output CmpMap */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output CmpMaps. */
   in = (AstCmpMap *) objin;
   out = (AstCmpMap *) objout;

/* For safety, start by clearing any references to the input component
   Mappings from the output CmpMap. */
   out->map1 = NULL;
   out->map2 = NULL;

/* Make copies of these Mappings and store pointers to them in the output
   CmpMap structure. */
   out->map1 = astCopy( in->map1 );
   out->map2 = astCopy( in->map2 );
}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for CmpMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for CmpMap objects.

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
   AstCmpMap *this;              /* Pointer to CmpMap */

/* Obtain a pointer to the CmpMap structure. */
   this = (AstCmpMap *) obj;

/* Annul the pointers to the component Mappings. */
   this->map1 = astAnnul( this->map1 );
   this->map2 = astAnnul( this->map2 );

/* Clear the remaining CmpMap variables. */
   this->invert1 = 0;
   this->invert2 = 0;
   this->series = 0;
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for CmpMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the CmpMap class to an output Channel.

*  Parameters:
*     this
*        Pointer to the CmpMap whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstCmpMap *this;              /* Pointer to the CmpMap structure */
   int ival;                     /* Integer value */
   int set;                      /* Attribute value set? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the CmpMap structure. */
   this = (AstCmpMap *) this_object;

/* Write out values representing the instance variables for the CmpMap
   class.  Accompany these with appropriate comment strings, possibly
   depending on the values being written.*/

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

/* Series. */
/* ------- */
   ival = this->series;
   set = ( ival == 0 );
   astWriteInt( channel, "Series", set, 0, ival,
                ival ? "Component Mappings applied in series" :
                       "Component Mappings applied in parallel" );

/* First Invert flag. */
/* ------------------ */
   ival = this->invert1;
   set = ( ival != 0 );
   astWriteInt( channel, "InvA", set, 0, ival,
                ival ? "First Mapping used in inverse direction" :
                       "First Mapping used in forward direction" );

/* Second Invert flag. */
/* ------------------- */
   ival = this->invert2;
   set = ( ival != 0 );
   astWriteInt( channel, "InvB", set, 0, ival,
                ival ? "Second Mapping used in inverse direction" :
                       "Second Mapping used in forward direction" );

/* First Mapping. */
/* -------------- */
   astWriteObject( channel, "MapA", 1, 1, this->map1,
                   "First component Mapping" );

/* Second Mapping. */
/* --------------- */
   astWriteObject( channel, "MapB", 1, 1, this->map2,
                   "Second component Mapping" );
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsACmpMap and astCheckCmpMap functions using the
   macros defined for this purpose in the "object.h" header file. */
astMAKE_ISA(CmpMap,Mapping)
astMAKE_CHECK(CmpMap)

AstCmpMap *astCmpMap_( void *map1_void, void *map2_void, int series,
                       const char *options, int *status, ...) {
/*
*+
*  Name:
*     astCmpMap

*  Purpose:
*     Create a CmpMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "cmpmap.h"
*     AstCmpMap *astCmpMap( AstMapping *map1, AstMapping *map2, int series,
*                           const char *options, ... )

*  Class Membership:
*     CmpMap constructor.

*  Description:
*     This function creates a new CmpMap and optionally initialises its
*     attributes.

*  Parameters:
*     map1
*        Pointer to the first Mapping.
*     map2
*        Pointer to the second Mapping.
*     series
*        If a non-zero value is given, the two Mappings will be connected
*        together in series. A zero value requests that they be connected in
*        parallel.
*     options
*        Pointer to a null terminated string containing an optional
*        comma-separated list of attribute assignments to be used for
*        initialising the new CmpMap. The syntax used is the same as for the
*        astSet method and may include "printf" format specifiers identified
*        by "%" symbols in the normal way.
*     ...
*        If the "options" string contains "%" format specifiers, then an
*        optional list of arguments may follow it in order to supply values to
*        be substituted for these specifiers. The rules for supplying these
*        are identical to those for the astSet method (and for the C "printf"
*        function).

*  Returned Value:
*     A pointer to the new CmpMap.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-

*  Implementation Notes:
*     - This function implements the basic CmpMap constructor which is
*     available via the protected interface to the CmpMap class.  A
*     public interface is provided by the astCmpMapId_ function.
*     - Because this function has a variable argument list, it is
*     invoked by a macro that evaluates to a function pointer (not a
*     function invocation) and no checking or casting of arguments is
*     performed before the function is invoked. Because of this, the
*     "map1" and "map2" parameters are of type (void *) and are
*     converted and validated within the function itself.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstCmpMap *new;               /* Pointer to new CmpMap */
   AstMapping *map1;             /* Pointer to first Mapping structure */
   AstMapping *map2;             /* Pointer to second Mapping structure */
   va_list args;                 /* Variable argument list */

/* Initialise. */
   new = NULL;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return new;

/* Obtain and validate pointers to the Mapping structures provided. */
   map1 = astCheckMapping( map1_void );
   map2 = astCheckMapping( map2_void );
   if ( astOK ) {

/* Initialise the CmpMap, allocating memory and initialising the
   virtual function table as well if necessary. */
      new = astInitCmpMap( NULL, sizeof( AstCmpMap ), !class_init, &class_vtab,
                           "CmpMap", map1, map2, series );

/* If successful, note that the virtual function table has been
   initialised. */
      if ( astOK ) {
         class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new CmpMap's
   attributes. */
         va_start( args, status );
         astVSet( new, options, NULL, args );
         va_end( args );

/* If an error occurred, clean up by deleting the new object. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Return a pointer to the new CmpMap. */
   return new;
}

AstCmpMap *astCmpMapId_( void *map1_void, void *map2_void, int series,
                         const char *options, ... ) {
/*
*++
*  Name:
c     astCmpMap
f     AST_CMPMAP

*  Purpose:
*     Create a CmpMap.

*  Type:
*     Public function.

*  Synopsis:
c     #include "cmpmap.h"
c     AstCmpMap *astCmpMap( AstMapping *map1, AstMapping *map2, int series,
c                           const char *options, ... )
f     RESULT = AST_CMPMAP( MAP1, MAP2, SERIES, OPTIONS, STATUS )

*  Class Membership:
*     CmpMap constructor.

*  Description:
*     This function creates a new CmpMap and optionally initialises
*     its attributes.
*
*     A CmpMap is a compound Mapping which allows two component
*     Mappings (of any class) to be connected together to form a more
*     complex Mapping. This connection may either be "in series"
*     (where the first Mapping is used to transform the coordinates of
*     each point and the second mapping is then applied to the
*     result), or "in parallel" (where one Mapping transforms the
*     earlier coordinates for each point and the second Mapping
*     simultaneously transforms the later coordinates).
*
*     Since a CmpMap is itself a Mapping, it can be used as a
*     component in forming further CmpMaps. Mappings of arbitrary
*     complexity may be built from simple individual Mappings in this
*     way.

*  Parameters:
c     map1
f     MAP1 = INTEGER (Given)
*        Pointer to the first component Mapping.
c     map2
f     MAP2 = INTEGER (Given)
*        Pointer to the second component Mapping.
c     series
f     SERIES = LOGICAL (Given)
c        If a non-zero value is given for this parameter, the two
c        component Mappings will be connected in series. A zero
c        value requests that they are connected in parallel.
f        If a .TRUE. value is given for this argument, the two
f        component Mappings will be connected in series. A
f        .FALSE. value requests that they are connected in parallel.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new CmpMap. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new CmpMap. The syntax used is identical to that for the
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
c     astCmpMap()
f     AST_CMPMAP = INTEGER
*        A pointer to the new CmpMap.

*  Notes:
*     - If the component Mappings are connected in series, then using
*     the resulting CmpMap to transform coordinates will cause the
*     first Mapping to be applied, followed by the second Mapping. If
*     the inverse CmpMap transformation is requested, the two
*     component Mappings will be applied in both the reverse order and
*     the reverse direction.
*     - When connecting two component Mappings in series, the number
*     of output coordinates generated by the first Mapping (its Nout
*     attribute) must equal the number of input coordinates accepted
*     by the second Mapping (its Nin attribute).
*     - If the component Mappings of a CmpMap are connected in
*     parallel, then the first Mapping will be used to transform the
*     earlier input coordinates for each point (and to produce the
*     earlier output coordinates) and the second Mapping will be used
*     simultaneously to transform the remaining input coordinates (to
*     produce the remaining output coordinates for each point). If the
*     inverse transformation is requested, each Mapping will still be
*     applied to the same coordinates, but in the reverse direction.
*     - When connecting two component Mappings in parallel, there is
*     no restriction on the number of input and output coordinates for
*     each Mapping.
c     - Note that the component Mappings supplied are not copied by
c     astCmpMap (the new CmpMap simply retains a reference to
c     them). They may continue to be used for other purposes, but
c     should not be deleted. If a CmpMap containing a copy of its
c     component Mappings is required, then a copy of the CmpMap should
c     be made using astCopy.
f     - Note that the component Mappings supplied are not copied by
f     AST_CMPMAP (the new CmpMap simply retains a reference to
f     them). They may continue to be used for other purposes, but
f     should not be deleted. If a CmpMap containing a copy of its
f     component Mappings is required, then a copy of the CmpMap should
f     be made using AST_COPY.
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--

*  Implementation Notes:
*     - This function implements the external (public) interface to
*     the astCmpMap constructor function. It returns an ID value
*     (instead of a true C pointer) to external users, and must be
*     provided because astCmpMap_ has a variable argument list which
*     cannot be encapsulated in a macro (where this conversion would
*     otherwise occur).
*     - Because no checking or casting of arguments is performed
*     before the function is invoked, the "map1" and "map2" parameters
*     are of type (void *) and are converted from an ID value to a
*     pointer and validated within the function itself.
*     - The variable argument list also prevents this function from
*     invoking astCmpMap_ directly, so it must be a re-implementation
*     of it in all respects, except for the conversions between IDs
*     and pointers on input/output of Objects.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstCmpMap *new;               /* Pointer to new CmpMap */
   AstMapping *map1;             /* Pointer to first Mapping structure */
   AstMapping *map2;             /* Pointer to second Mapping structure */
   va_list args;                 /* Variable argument list */

   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialise. */
   new = NULL;

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Check the global status. */
   if ( !astOK ) return new;

/* Obtain the Mapping pointers from the ID's supplied and validate the
   pointers to ensure they identify valid Mappings. */
   map1 = astVerifyMapping( astMakePointer( map1_void ) );
   map2 = astVerifyMapping( astMakePointer( map2_void ) );
   if ( astOK ) {

/* Initialise the CmpMap, allocating memory and initialising the
   virtual function table as well if necessary. */
      new = astInitCmpMap( NULL, sizeof( AstCmpMap ), !class_init, &class_vtab,
                           "CmpMap", map1, map2, series );

/* If successful, note that the virtual function table has been initialised. */
      if ( astOK ) {
         class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new CmpMap's
   attributes. */
         va_start( args, options );
         astVSet( new, options, NULL, args );
         va_end( args );

/* If an error occurred, clean up by deleting the new object. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Return an ID value for the new CmpMap. */
   return astMakeId( new );
}

AstCmpMap *astInitCmpMap_( void *mem, size_t size, int init,
                           AstCmpMapVtab *vtab, const char *name,
                           AstMapping *map1, AstMapping *map2, int series, int *status ) {
/*
*+
*  Name:
*     astInitCmpMap

*  Purpose:
*     Initialise a CmpMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "cmpmap.h"
*     AstCmpMap *astInitCmpMap( void *mem, size_t size, int init,
*                               AstCmpMapVtab *vtab, const char *name,
*                               AstMapping *map1, AstMapping *map2,
*                               int series )

*  Class Membership:
*     CmpMap initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new CmpMap object. It allocates memory (if necessary) to
*     accommodate the CmpMap plus any additional data associated with the
*     derived class. It then initialises a CmpMap structure at the start
*     of this memory. If the "init" flag is set, it also initialises the
*     contents of a virtual function table for a CmpMap at the start of
*     the memory passed via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the CmpMap is to be initialised.
*        This must be of sufficient size to accommodate the CmpMap data
*        (sizeof(CmpMap)) plus any data used by the derived class. If a
*        value of NULL is given, this function will allocate the memory itself
*        using the "size" parameter to determine its size.
*     size
*        The amount of memory used by the CmpMap (plus derived class
*        data). This will be used to allocate memory if a value of NULL is
*        given for the "mem" parameter. This value is also stored in the
*        CmpMap structure, so a valid value must be supplied even if not
*        required for allocating memory.
*     init
*        A logical flag indicating if the CmpMap's virtual function table
*        is to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new CmpMap.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the Object
*        astClass function).
*     map1
*        Pointer to the first Mapping.
*     map2
*        Pointer to the second Mapping.
*     series
*        If a non-zero value is given, the two Mappings will be connected
*        together in series. A zero value requests that they be connected in
*        parallel.

*  Returned Value:
*     A pointer to the new CmpMap.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstCmpMap *new;               /* Pointer to new CmpMap */
   int map_f;                    /* Forward transformation defined? */
   int map_i;                    /* Inverse transformation defined? */
   int nin2;                     /* No. input coordinates for Mapping 2 */
   int nin;                      /* No. input coordinates for CmpMap */
   int nout1;                    /* No. output coordinates for Mapping 1 */
   int nout;                     /* No. output coordinates for CmpMap */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitCmpMapVtab( vtab, name );

/* Initialise. */
   new = NULL;

/* Determine in which directions each component Mapping is able to transform
   coordinates. Combine these results to obtain a result for the overall
   CmpMap. */
   map_f = astGetTranForward( map1 ) && astGetTranForward( map2 );
   map_i = astGetTranInverse( map1 ) && astGetTranInverse( map2 );
   if ( astOK ) {

/* If connecting the Mappings in series, check that the number of coordinates
   are compatible and report an error if they are not. */
      if ( series ) {
         nout1 = astGetNout( map1 );
         nin2 = astGetNin( map2 );
         if ( astOK && ( nout1 != nin2 ) ) {
            astError( AST__INNCO, "astInitCmpMap(%s): The number of output "
                      "coordinates per point (%d) for the first Mapping "
                      "supplied does not match the number of input "
                      "coordinates (%d) for the second Mapping.", status, name, nout1,
                      nin2 );
         }
      }
   }

/* If OK, determine the total number of input and output coordinates per point
   for the CmpMap. */
   if ( astOK ) {
      if ( series ) {
         nin = astGetNin( map1 );
         nout = astGetNout( map2 );
      } else {
         nin = astGetNin( map1 ) + astGetNin( map2 );
         nout = astGetNout( map1 ) + astGetNout( map2 );
      }

   } else {
      nin = 0;
      nout = 0;
   }

/* Initialise a Mapping structure (the parent class) as the first component
   within the CmpMap structure, allocating memory if necessary. Specify
   the number of input and output coordinates and in which directions the
   Mapping should be defined. */
   if ( astOK ) {
      new = (AstCmpMap *) astInitMapping( mem, size, 0,
                                          (AstMappingVtab *) vtab, name,
                                          nin, nout, map_f, map_i );

      if ( astOK ) {

/* Initialise the CmpMap data. */
/* --------------------------- */
/* Store pointers to the component Mappings. Extract Mappings if
   FrameSets are provided. */
         if( astIsAFrameSet( map1 ) ) {
            new->map1 = astGetMapping( (AstFrameSet *) map1, AST__BASE,
                                       AST__CURRENT );
         } else {
            new->map1 = astClone( map1 );
         }

         if( astIsAFrameSet( map2 ) ) {
            new->map2 = astGetMapping( (AstFrameSet *) map2, AST__BASE,
                                       AST__CURRENT );
         } else {
            new->map2 = astClone( map2 );
         }


/* Save the initial values of the inversion flags for these Mappings. */
         new->invert1 = astGetInvert( new->map1 );
         new->invert2 = astGetInvert( new->map2 );

/* Note whether the Mappings are joined in series (instead of in parallel),
   constraining this flag to be 0 or 1. */
         new->series = ( series != 0 );

/* If an error occurred, clean up by annulling the Mapping pointers and
   deleting the new object. */
         if ( !astOK ) {
            new->map1 = astAnnul( new->map1 );
            new->map2 = astAnnul( new->map2 );
            new = astDelete( new );
         }
      }
   }

/* Return a pointer to the new object. */
   return new;
}

AstCmpMap *astLoadCmpMap_( void *mem, size_t size,
                           AstCmpMapVtab *vtab, const char *name,
                           AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadCmpMap

*  Purpose:
*     Load a CmpMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "cmpmap.h"
*     AstCmpMap *astLoadCmpMap( void *mem, size_t size,
*                               AstCmpMapVtab *vtab, const char *name,
*                               AstChannel *channel )

*  Class Membership:
*     CmpMap loader.

*  Description:
*     This function is provided to load a new CmpMap using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     CmpMap structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a CmpMap at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the CmpMap is to be
*        loaded.  This must be of sufficient size to accommodate the
*        CmpMap data (sizeof(CmpMap)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the CmpMap (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the CmpMap structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstCmpMap) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new CmpMap. If this is NULL, a pointer to
*        the (static) virtual function table for the CmpMap class is
*        used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "CmpMap" is used instead.

*  Returned Value:
*     A pointer to the new CmpMap.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstCmpMap *new;               /* Pointer to the new CmpMap */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this CmpMap. In this case the
   CmpMap belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstCmpMap );
      vtab = &class_vtab;
      name = "CmpMap";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitCmpMapVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built CmpMap. */
   new = astLoadMapping( mem, size, (AstMappingVtab *) vtab, name,
                         channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "CmpMap" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* Series. */
/* ------- */
      new->series = astReadInt( channel, "series", 1 );
      new->series = ( new->series != 0 );

/* First Invert flag. */
/* ------------------ */
      new->invert1 = astReadInt( channel, "inva", 0 );
      new->invert1 = ( new->invert1 != 0 );

/* Second Invert flag. */
/* ------------------- */
      new->invert2 = astReadInt( channel, "invb", 0 );
      new->invert2 = ( new->invert2 != 0 );

/* First Mapping. */
/* -------------- */
      new->map1 = astReadObject( channel, "mapa", NULL );

/* Second Mapping. */
/* --------------- */
      new->map2 = astReadObject( channel, "mapb", NULL );

/* If an error occurred, clean up by deleting the new CmpMap. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new CmpMap pointer. */
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

/* None. */








