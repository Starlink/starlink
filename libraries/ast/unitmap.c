/*
*class++
*  Name:
*     UnitMap

*  Purpose:
*     Unit (null) Mapping.

*  Constructor Function:
c     astUnitMap
f     AST_UNITMAP

*  Description:
*     A UnitMap is a unit (null) Mapping that has no effect on the
*     coordinates supplied to it. They are simply copied. This can be
*     useful if a Mapping is required (e.g. to pass to another
c     function) but you do not want it to have any effect.
f     routine) but you do not want it to have any effect.
*     The Nin and Nout attributes of a UnitMap are always equal and
*     are specified when it is created.

*  Inheritance:
*     The UnitMap class inherits from the Mapping class.

*  Attributes:
*     The UnitMap class does not define any new attributes beyond
*     those which are applicable to all Mappings.

*  Functions:
c     The UnitMap class does not define any new functions beyond those
f     The UnitMap class does not define any new routines beyond those
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
*     DSB: David S Berry (JAC, Hawaii)

*  History:
*     7-FEB-1996 (RFWS):
*        Original version.
*     13-DEC-1996 (RFWS):
*        Over-ride the astMapMerge method.
*     8-JAN-2003 (DSB):
*        Changed private InitVtab method to protected astInitUnitMapVtab
*        method.
*     10-MAY-2006 (DSB):
*        Override astEqual.
*     17-FEB-2012 (DSB):
*        In Transform, do not copy the coordinate values if the input and
*        output array are the same.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS UnitMap

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "object.h"              /* Base Object class */
#include "memory.h"              /* AST memory management */
#include "pointset.h"            /* Sets of points/coordinates */
#include "mapping.h"             /* Coordinate mappings (parent class) */
#include "unitmap.h"             /* Interface definition for this class */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <stdarg.h>
#include <stddef.h>
#include <string.h>

/* Module Variables. */
/* ================= */

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* Pointers to parent class methods which are extended by this class. */
static AstPointSet *(* parent_transform)( AstMapping *, AstPointSet *, int, AstPointSet *, int * );


#ifdef THREAD_SAFE
/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(UnitMap)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(UnitMap,Class_Init)
#define class_vtab astGLOBAL(UnitMap,Class_Vtab)


#include <pthread.h>


#else


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstUnitMapVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstUnitMap *astUnitMapId_( int, const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static double Rate( AstMapping *, double *, int, int, int * );
static int *MapSplit( AstMapping *, int, const int *, AstMapping **, int * );
static int Equal( AstObject *, AstObject *, int * );
static int GetIsLinear( AstMapping *, int * );
static int MapMerge( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
static void Dump( AstObject *, AstChannel *, int * );

/* Member functions. */
/* ================= */
static int Equal( AstObject *this_object, AstObject *that_object, int *status ) {
/*
*  Name:
*     Equal

*  Purpose:
*     Test if two UnitMaps are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unitmap.h"
*     int Equal( AstObject *this, AstObject *that, int *status )

*  Class Membership:
*     UnitMap member function (over-rides the astEqual protected
*     method inherited from the astMapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two UnitMaps are equivalent.

*  Parameters:
*     this
*        Pointer to the first Object (a UnitMap).
*     that
*        Pointer to the second Object.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the UnitMaps are equivalent, zero otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstUnitMap *that;
   AstUnitMap *this;
   int result;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain pointers to the two UnitMap structures. */
   this = (AstUnitMap *) this_object;
   that = (AstUnitMap *) that_object;

/* Check the second object is a UnitMap. We know the first is a
   UnitMap since we have arrived at this implementation of the virtual
   function. */
   if( astIsAUnitMap( that ) ) {

/* Get the number of inputs check they are the same for both. */
      result = ( astGetNin( this ) == astGetNin( that ) );

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
*     Return the value of the IsLinear attribute for a UnitMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     void GetIsLinear( AstMapping *this, int *status )

*  Class Membership:
*     UnitMap member function (over-rides the protected astGetIsLinear
*     method inherited from the Mapping class).

*  Description:
*     This function returns the value of the IsLinear attribute for a
*     Frame, which is always one.

*  Parameters:
*     this
*        Pointer to the UnitMap.
*     status
*        Pointer to the inherited status variable.
*/
   return 1;
}

void astInitUnitMapVtab_(  AstUnitMapVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitUnitMapVtab

*  Purpose:
*     Initialise a virtual function table for a UnitMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "unitmap.h"
*     void astInitUnitMapVtab( AstUnitMapVtab *vtab, const char *name )

*  Class Membership:
*     UnitMap vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the UnitMap class.

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
   will be used (by astIsAUnitMap) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
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

   parent_transform = mapping->Transform;
   mapping->Transform = Transform;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   object->Equal = Equal;
   mapping->MapMerge = MapMerge;
   mapping->MapSplit = MapSplit;
   mapping->Rate = Rate;
   mapping->GetIsLinear = GetIsLinear;

/* Declare the class dump function. There is no copy constructor or
   destructor. */
   astSetDump( vtab, Dump, "UnitMap", "Unit (null) Mapping" );

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
*     Simplify a sequence of Mappings containing a UnitMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     int MapMerge( AstMapping *this, int where, int series, int *nmap,
*                   AstMapping ***map_list, int **invert_list, int *status )

*  Class Membership:
*     UnitMap method (over-rides the protected astMapMerge method
*     inherited from the Mapping class).

*  Description:
*     This function attempts to simplify a sequence of Mappings by
*     merging a nominated UnitMap in the sequence with its neighbours,
*     so as to shorten the sequence if possible.
*
*     In many cases, simplification will not be possible and the
*     function will return -1 to indicate this, without further
*     action.
*
*     In most cases of interest, however, this function will either
*     attempt to replace the nominated UnitMap with one which it
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
*        Pointer to the nominated UnitMap which is to be merged with
*        its neighbours. This should be a cloned copy of the UnitMap
*        pointer contained in the array element "(*map_list)[where]"
*        (see below). This pointer will not be annulled, and the
*        UnitMap it identifies will not be modified by this function.
*     where
*        Index in the "*map_list" array (below) at which the pointer
*        to the nominated UnitMap resides.
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
   AstMapping *new;              /* Pointer to replacement UnitMap */
   const char *class;            /* Pointer to Mapping class string */
   int i1;                       /* Index of first UnitMap to merge */
   int i2;                       /* Index of last UnitMap to merge */
   int i;                        /* Loop counter for Mappings */
   int ngone;                    /* Number of UnitMaps eliminated */
   int nin;                      /* Number of coordinates for UnitMap */
   int result;                   /* Result value to return */

/* Initialise. */
   result = -1;

/* Check the global error status. */
   if ( !astOK ) return result;

/* If the Mapping sequence consists of a single UnitMap, simply check
   if the asociated Invert flag is set, and clear it if necessary. */
   if ( *nmap == 1 ) {
      if ( ( *invert_list )[ 0 ] ) {
         ( *invert_list )[ 0 ] = 0;

/* Note if the Mapping sequence was modified. */
         result = 0;
      }

/* In series. */
/* ========== */
/* If a UnitMap occurs in series with any other Mapping, it can simply
   be eliminated, so annul its Mapping pointer. */
   } else if ( *nmap > 1 ) {
      if ( series ) {
         ( *map_list )[ where ] = astAnnul( ( *map_list )[ where ] );

/* Loop to move any following pointers and their Invert flags down in
   the array to fill the gap, and clear the vacated elements at the
   end of the arrays. */
         for ( i = where + 1; i < *nmap; i++ ) {
            ( *map_list )[ i - 1 ] = ( *map_list )[ i ];
            ( *invert_list )[ i - 1 ] = ( *invert_list )[ i ];
         }
         ( *map_list )[ *nmap - 1 ] = NULL;
         ( *invert_list )[ *nmap - 1 ] = 0;

/* Decrement the Mapping count and return the index of the first
   modified element. */
         ( *nmap )--;
         result = where;

/* In parallel. */
/* ============ */
/* If a UnitMap occurs in parallel with other Mappings, it can be
   merged with any UnitMaps on either side of itself. */
      } else {

/* Initialise indices to identify any adjacent UnitMaps and the number
   of coordinates to be handled by the replacement. */
         i1 = i2 = where;
         nin = astGetNin( ( *map_list )[ where ] );

/* Loop to inspect earlier Mappings, obtaining the Mapping Class name
   and checking it is "UnitMap". Quit looping as soon as any other
   class of Mapping is found. */
         while ( i1 - 1 >= 0 ) {
            class = astGetClass( ( *map_list )[ i1 - 1 ] );
            if ( !astOK || strcmp( class, "UnitMap" ) ) break;

/* Update the index of the earliest UnitMap that is to be merged and
   increment the total number of coordinates involved. */
            i1--;
            nin += astGetNin( ( *map_list )[ i1 ] );
         }

/* Repeat the above process to inspect any later Mappings in the
   sequence. */
         while ( i2 + 1 < *nmap ) {
            class = astGetClass( ( *map_list )[ i2 + 1 ] );
            if ( !astOK || strcmp( class, "UnitMap" ) ) break;
            i2++;
            nin += astGetNin( ( *map_list )[ i2 ] );
         }

/* Calculate the net number of Mappings that can be removed from the
   sequence and check if this is zero, meaning that there were no
   adjacent UnitMaps to merge with. */
         if ( astOK ) {
            ngone = i2 - i1;
            if ( !ngone ) {

/* If so, simply check if the nominated UnitMap's Invert flag is set,
   and clear it if necessary. */
               if ( ( *invert_list )[ where ] ) {
                  ( *invert_list )[ where ] = 0;

/* Note if the Mapping sequence was modified. */
                  result = where;
               }

/* If UnitMaps can be merged, create the replacement UnitMap. */
            } else {
               new = (AstMapping *) astUnitMap( nin, "", status );
               if ( astOK ) {

/* Annul the pointers to all the UnitMaps that are being replaced. */
                  for ( i = i1; i <= i2; i++ ) {
                     ( *map_list )[ i ] = astAnnul( ( *map_list )[ i ] );
                  }

/* Insert the new pointer and the associated Invert flag. */
                  ( *map_list )[ i1 ] = new;
                  ( *invert_list )[ i1 ] = 0;

/* Loop to close the resulting gap by moving subsequent elements down
   in the arrays. */
                  for ( i = i2 + 1; i < *nmap; i++ ) {
                     ( *map_list )[ i - ngone ] = ( *map_list )[ i ];
                     ( *invert_list )[ i - ngone ] = ( *invert_list )[ i ];
                  }

/* Clear the vacated elements at the end. */
                  for ( i = *nmap - ngone; i < *nmap; i++ ) {
                     ( *map_list )[ i ] = NULL;
                     ( *invert_list )[ i ] = 0;
                  }

/* Decrement the Mapping count and return the index of the first
   modified element. */
                  ( *nmap ) -= ngone;
                  result = i1;
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
*     UnitMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unitmap.h"
*     int *MapSplit( AstMapping *this, int nin, const int *in, AstMapping **map, int *status )

*  Class Membership:
*     UnitMap method (over-rides the protected astMapSplit method
*     inherited from the Mapping class).

*  Description:
*     This function creates a new Mapping by picking specified inputs from
*     an existing UnitMap. This is only possible if the specified inputs
*     correspond to some subset of the UnitMap outputs. That is, there
*     must exist a subset of the UnitMap outputs for which each output
*     depends only on the selected UnitMap inputs, and not on any of the
*     inputs which have not been selected. If this condition is not met
*     by the supplied UnitMap, then a NULL Mapping is returned.

*  Parameters:
*     this
*        Pointer to the UnitMap to be split (the UnitMap is not actually
*        modified by this function).
*     nin
*        The number of inputs to pick from "this".
*     in
*        Pointer to an array of indices (zero based) for the inputs which
*        are to be picked. This array should have "nin" elements. If "Nin"
*        is the number of inputs of the supplied UnitMap, then each element
*        should have a value in the range zero to Nin-1.
*     map
*        Address of a location at which to return a pointer to the new
*        Mapping. This Mapping will have "nin" inputs (the number of
*        outputs may be different to "nin"). A NULL pointer will be
*        returned if the supplied UnitMap has no subset of outputs which
*        depend only on the selected inputs.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a dynamically allocated array of ints. The number of
*     elements in this array will equal the number of outputs for the
*     returned Mapping. Each element will hold the index of the
*     corresponding output in the supplied UnitMap. The array should be
*     freed using astFree when no longer needed. A NULL pointer will
*     be returned if no output Mapping can be created.

*  Notes:
*     - If this function is invoked with the global error status set,
*     or if it should fail for any reason, then NULL values will be
*     returned as the function value and for the "map" pointer.
*/

/* Local Variables: */
   AstUnitMap *this;          /* Pointer to UnitMap structure */
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

/* Get a pointer to the UnitMap structure. */
   this = (AstUnitMap *) this_map;

/* Allocate memory for the returned array and create a UnitMap with the
   required number of axes. */
   result = astMalloc( sizeof( int )*(size_t) nin );
   *map = (AstMapping *) astUnitMap( nin, "", status );

/* Check pointers can be used safely. */
   if( astOK ) {

/* Store the required output axis indices. At the same time check that each
   axis is valid. */
      mnin = astGetNin( this );
      ok = 1;
      for( i = 0; i < nin; i++ ) {
         iin = in[ i ];
         if( iin >= 0 && iin < mnin ) {
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

/* Free returned resources if an error has occurred. */
   if( !astOK ) {
      result = astFree( result );
      *map = astAnnul( *map );
   }

/* Return the list of output indices. */
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
*     #include "unitmap.h"
*     result = Rate( AstMapping *this, double *at, int ax1, int ax2, int *status )

*  Class Membership:
*     UnitMap member function (overrides the astRate method inherited
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

   return ( ax1 == ax2 ) ? 1.0 : 0.0;
}

static AstPointSet *Transform( AstMapping *this, AstPointSet *in,
                               int forward, AstPointSet *out, int *status ) {
/*
*  Name:
*     Transform

*  Purpose:
*     Apply a UnitMap to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unitmap.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     UnitMap member function (over-rides the astTransform method inherited
*     from the Mapping class).

*  Description:
*     This function takes a UnitMap and a set of points encapsulated in a
*     PointSet and transforms the points so as to perform the identity
*     transformation (i.e. simply copies the coordinate values).

*  Parameters:
*     this
*        Pointer to the UnitMap.
*     in
*        Pointer to the PointSet holding the input coordinate data.
*     forward
*        A non-zero value indicates that the forward coordinate transformation
*        should be applied, while a zero value requests the inverse
*        transformation. In this case, both transformations are equivalent.
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
*     match the number of coordinates for the UnitMap being applied.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstPointSet *result;          /* Pointer to output PointSet */
   AstUnitMap *map;              /* Pointer to UnitMap to be applied */
   double **ptr_in;              /* Pointer to input coordinate data */
   double **ptr_out;             /* Pointer to output coordinate data */
   int coord;                    /* Loop counter for coordinates */
   int ncoord_in;                /* Number of coordinates per input point */
   int npoint;                   /* Number of points */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the UnitMap. */
   map = (AstUnitMap *) this;

/* Apply the parent mapping using the stored pointer to the Transform member
   function inherited from the parent Mapping class. This function validates
   all arguments and generates an output PointSet if necessary, but does not
   actually transform any coordinate values. */
   result = (*parent_transform)( this, in, forward, out, status );

/* We will now extend the parent astTransform method by performing the
   coordinate copy needed to generate the output coordinate values. */

/* Determine the numbers of points and coordinates per point from the input
   PointSet and obtain pointers for accessing the input and output coordinate
   values. */
   ncoord_in = astGetNcoord( in );
   npoint = astGetNpoint( in );
   ptr_in = astGetPoints( in );
   ptr_out = astGetPoints( result );

/* We need not determine whether to apply the forward or inverse
   transformation, as they are both the same. */

/* Copy the coordinate values. */
/* --------------------------- */
   if ( astOK ) {

/* Loop to copy the values for each coordinate. Use a memory copy for speed.
   Do not do the copy if the input and output arrays are the same. */
      for ( coord = 0; coord < ncoord_in; coord++ ) {
         if( ptr_out[ coord ] != ptr_in[ coord ] ) {
            (void) memcpy( (void *) ptr_out[ coord ],
                           (const void *) ptr_in[ coord ],
                           sizeof( double ) * (size_t) npoint );
         }
      }
   }

/* Return a pointer to the output PointSet. */
   return result;
}

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
*     Dump function for UnitMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the UnitMap class to an output Channel.

*  Parameters:
*     this
*        Pointer to the UnitMap whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstUnitMap *this;             /* Pointer to the UnitMap structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the UnitMap structure. */
   this = (AstUnitMap *) this_object;

/* Write out values representing the instance variables for the
   UnitMap class.  Accompany these with appropriate comment strings,
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

/* There are no values to write, so return without further action. */
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAUnitMap and astCheckUnitMap functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(UnitMap,Mapping)
astMAKE_CHECK(UnitMap)

AstUnitMap *astUnitMap_( int ncoord, const char *options, int *status, ...) {
/*
*++
*  Name:
c     astUnitMap
f     AST_UNITMAP

*  Purpose:
*     Create a UnitMap.

*  Type:
*     Public function.

*  Synopsis:
c     #include "unitmap.h"
c     AstUnitMap *astUnitMap( int ncoord, const char *options, ... )
f     RESULT = AST_UNITMAP( NCOORD, OPTIONS, STATUS )

*  Class Membership:
*     UnitMap constructor.

*  Description:
*     This function creates a new UnitMap and optionally initialises
*     its attributes.
*
*     A UnitMap is a unit (null) Mapping that has no effect on the
*     coordinates supplied to it. They are simply copied. This can be
*     useful if a Mapping is required (e.g. to pass to another
c     function) but you do not want it to have any effect.
f     routine) but you do not want it to have any effect.

*  Parameters:
c     ncoord
f     NCOORD = INTEGER (Given)
*        The number of input and output coordinates (these numbers are
*        necessarily the same).
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new UnitMap. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new UnitMap. The syntax used is identical to that for the
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
c     astUnitMap()
f     AST_UNITMAP = INTEGER
*        A pointer to the new UnitMap.

*  Notes:
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstUnitMap *new;              /* Pointer to new UnitMap */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the UnitMap, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitUnitMap( NULL, sizeof( AstUnitMap ), !class_init, &class_vtab,
                         "UnitMap", ncoord );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   UnitMap's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new UnitMap. */
   return new;
}

AstUnitMap *astUnitMapId_( int ncoord, const char *options, ... ) {
/*
*  Name:
*     astUnitMapId_

*  Purpose:
*     Create a UnitMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "unitmap.h"
*     AstUnitMap *astUnitMapId_( int ncoord, const char *options, ... )

*  Class Membership:
*     UnitMap constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astUnitMap constructor function. It returns an ID value (instead
*     of a true C pointer) to external users, and must be provided
*     because astUnitMap_ has a variable argument list which cannot be
*     encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     The variable argument list also prevents this function from
*     invoking astUnitMap_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astUnitMap_.

*  Returned Value:
*     The ID value associated with the new UnitMap.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstUnitMap *new;              /* Pointer to new UnitMap */
   va_list args;                 /* Variable argument list */

   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the UnitMap, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitUnitMap( NULL, sizeof( AstUnitMap ), !class_init, &class_vtab,
                         "UnitMap", ncoord );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   UnitMap's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new UnitMap. */
   return astMakeId( new );
}

AstUnitMap *astInitUnitMap_( void *mem, size_t size, int init,
                             AstUnitMapVtab *vtab, const char *name,
                             int ncoord, int *status ) {
/*
*+
*  Name:
*     astInitUnitMap

*  Purpose:
*     Initialise a UnitMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "unitmap.h"
*     AstUnitMap *astInitUnitMap( void *mem, size_t size, int init,
*                                 AstUnitMapVtab *vtab, const char *name,
*                                 int ncoord )

*  Class Membership:
*     UnitMap initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new UnitMap object. It allocates memory (if necessary) to accommodate
*     the UnitMap plus any additional data associated with the derived class.
*     It then initialises a UnitMap structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a UnitMap at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the UnitMap is to be initialised.
*        This must be of sufficient size to accommodate the UnitMap data
*        (sizeof(UnitMap)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the UnitMap (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the UnitMap
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the UnitMap's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new UnitMap.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the Object
*        astClass function).
*     ncoord
*        The number of coordinate values per point.

*  Returned Value:
*     A pointer to the new UnitMap.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstUnitMap *new;              /* Pointer to new UnitMap */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitUnitMapVtab( vtab, name );

/* Initialise. */
   new = NULL;

/* Initialise a Mapping structure (the parent class) as the first component
   within the UnitMap structure, allocating memory if necessary. Specify that
   the Mapping should be defined in both the forward and inverse directions. */
   new = (AstUnitMap *) astInitMapping( mem, size, 0,
                                        (AstMappingVtab *) vtab, name,
                                        ncoord, ncoord, 1, 1 );

      if ( astOK ) {

/* Initialise the UnitMap data. */
/* ---------------------------- */
/* There is nothing else to store. */

/* If an error occurred, clean up by deleting the new object (if any other
   resources had been allocated, we would free these first). */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new object. */
   return new;
}

AstUnitMap *astLoadUnitMap_( void *mem, size_t size,
                             AstUnitMapVtab *vtab, const char *name,
                             AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadUnitMap

*  Purpose:
*     Load a UnitMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "unitmap.h"
*     AstUnitMap *astLoadUnitMap( void *mem, size_t size,
*                                 AstUnitMapVtab *vtab, const char *name,
*                                 AstChannel *channel )

*  Class Membership:
*     UnitMap loader.

*  Description:
*     This function is provided to load a new UnitMap using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     UnitMap structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a UnitMap at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the UnitMap is to be
*        loaded.  This must be of sufficient size to accommodate the
*        UnitMap data (sizeof(UnitMap)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the UnitMap (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the UnitMap structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstUnitMap) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new UnitMap. If this is NULL, a pointer
*        to the (static) virtual function table for the UnitMap class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "UnitMap" is used instead.

*  Returned Value:
*     A pointer to the new UnitMap.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstUnitMap *new;              /* Pointer to the new UnitMap */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this UnitMap. In this case the
   UnitMap belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstUnitMap );
      vtab = &class_vtab;
      name = "UnitMap";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitUnitMapVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built UnitMap. */
   new = astLoadMapping( mem, size, (AstMappingVtab *) vtab, name,
                         channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "UnitMap" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* There are no values to read. */
/* ---------------------------- */

/* If an error occurred, clean up by deleting the new UnitMap. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new UnitMap pointer. */
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

/* There are none of these. */




