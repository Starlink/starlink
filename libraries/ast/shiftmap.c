/*
*class++
*  Name:
*     ShiftMap

*  Purpose:
*     Add a constant value to each coordinate.

*  Constructor Function:
c     astShiftMap
f     AST_SHIFTMAP

*  Description:
*     A ShiftMap is a linear Mapping which shifts each axis by a
*     specified constant value.

*  Inheritance:
*     The ShiftMap class inherits from the Mapping class.

*  Attributes:
*     The ShiftMap class does not define any new attributes beyond those
*     which are applicable to all Mappings.

*  Functions:
c     The ShiftMap class does not define any new functions beyond those
f     The ShiftMap class does not define any new routines beyond those
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

*  History:
*     15-AUG-2003 (DSB):
*        Original version.
*     14-FEB-2006 (DSB):
*        Override astGetObjSize.
*     10-MAY-2006 (DSB):
*        Override astEqual.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS ShiftMap

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
#include "mapping.h"             /* Coordinate mappings (parent class) */
#include "channel.h"             /* I/O channels */
#include "winmap.h"              /* Window mappings */
#include "shiftmap.h"            /* Interface definition for this class */

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


#ifdef THREAD_SAFE
/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(ShiftMap)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(ShiftMap,Class_Init)
#define class_vtab astGLOBAL(ShiftMap,Class_Vtab)


#include <pthread.h>


#else


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstShiftMapVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstShiftMap *astShiftMapId_( int, const double [], const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */

static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static int GetObjSize( AstObject *, int * );
static double Rate( AstMapping *, double *, int, int, int * );
static int MapMerge( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static int Equal( AstObject *, AstObject *, int * );
static int GetIsLinear( AstMapping *, int * );
static int *MapSplit( AstMapping *, int, const int *, AstMapping **, int * );

/* Member functions. */
/* ================= */

static int Equal( AstObject *this_object, AstObject *that_object, int *status ) {
/*
*  Name:
*     Equal

*  Purpose:
*     Test if two ShiftMaps are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "shiftmap.h"
*     int Equal( AstObject *this, AstObject *that, int *status )

*  Class Membership:
*     ShiftMap member function (over-rides the astEqual protected
*     method inherited from the astMapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two ShiftMaps are equivalent.

*  Parameters:
*     this
*        Pointer to the first Object (a ShiftMap).
*     that
*        Pointer to the second Object.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the ShiftMaps are equivalent, zero otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstShiftMap *that;
   AstShiftMap *this;
   int i;
   int nin;
   int nout;
   int result;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain pointers to the two ShiftMap structures. */
   this = (AstShiftMap *) this_object;
   that = (AstShiftMap *) that_object;

/* Check the second object is a ShiftMap. We know the first is a
   ShiftMap since we have arrived at this implementation of the virtual
   function. */
   if( astIsAShiftMap( that ) ) {

/* Get the number of inputs and outputs and check they are the same for both. */
      nin = astGetNin( this );
      nout = astGetNout( this );
      if( astGetNin( that ) == nin && astGetNout( that ) == nout ) {

/* If the Invert flags for the two ShiftMaps differ, it may still be possible
   for them to be equivalent. First compare the ShiftMaps if their Invert
   flags are the same. In this case all the attributes of the two ShiftMaps
   must be identical. */
         if( astGetInvert( this ) == astGetInvert( that ) ) {
            result = 1;
            for( i = 0; i < nin; i++ ) {
               if( !astEQUAL( this->shift[ i ], that->shift[ i ] ) ) {
                  result = 0;
                  break;
               }
            }

/* If the Invert flags for the two ShiftMaps differ, the attributes of the two
   ShiftMaps must be inversely related to each other. */
         } else {

            result = 1;
            for( i = 0; i < nin; i++ ) {
               if( !astEQUAL( this->shift[ i ], -(that->shift[ i ] ) ) ) {
                  result = 0;
                  break;
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

static int GetIsLinear( AstMapping *this_mapping, int *status ){
/*
*  Name:
*     GetIsLinear

*  Purpose:
*     Return the value of the IsLinear attribute for a ShiftMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     void GetIsLinear( AstMapping *this, int *status )

*  Class Membership:
*     ShiftMap member function (over-rides the protected astGetIsLinear
*     method inherited from the Mapping class).

*  Description:
*     This function returns the value of the IsLinear attribute for a
*     Frame, which is always one.

*  Parameters:
*     this
*        Pointer to the ShiftMap.
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
*     #include "shiftmap.h"
*     int GetObjSize( AstObject *this, int *status )

*  Class Membership:
*     ShiftMap member function (over-rides the astGetObjSize protected
*     method inherited from the parent class).

*  Description:
*     This function returns the in-memory size of the supplied ShiftMap,
*     in bytes.

*  Parameters:
*     this
*        Pointer to the ShiftMap.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The Object size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstShiftMap *this;         /* Pointer to ShiftMap structure */
   int result;                /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointers to the ShiftMap structure. */
   this = (AstShiftMap *) this_object;

/* Invoke the GetObjSize method inherited from the parent class, and then
   add on any components of the class structure defined by thsi class
   which are stored in dynamically allocated memory. */
   result = (*parent_getobjsize)( this_object, status );
   result += astTSizeOf( this->shift );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

void astInitShiftMapVtab_(  AstShiftMapVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitShiftMapVtab

*  Purpose:
*     Initialise a virtual function table for a ShiftMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "shiftmap.h"
*     void astInitShiftMapVtab( AstShiftMapVtab *vtab, const char *name )

*  Class Membership:
*     ShiftMap vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the ShiftMap class.

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
   will be used (by astIsAShiftMap) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstMappingVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   mapping = (AstMappingVtab *) vtab;
   parent_getobjsize = object->GetObjSize;
   object->GetObjSize = GetObjSize;

   parent_transform = mapping->Transform;
   mapping->Transform = Transform;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   object->Equal = Equal;
   mapping->MapMerge = MapMerge;
   mapping->Rate = Rate;
   mapping->MapSplit = MapSplit;
   mapping->GetIsLinear = GetIsLinear;

/* Declare the class dump, copy and delete functions.*/
   astSetDump( vtab, Dump, "ShiftMap", "Shift each coordinate axis" );
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
*     Simplify a sequence of Mappings containing a ShiftMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     int MapMerge( AstMapping *this, int where, int series, int *nmap,
*                   AstMapping ***map_list, int **invert_list, int *status )

*  Class Membership:
*     ShiftMap method (over-rides the protected astMapMerge method
*     inherited from the Mapping class).

*  Description:
*     This function attempts to simplify a sequence of Mappings by
*     merging a nominated ShiftMap in the sequence with its neighbours,
*     so as to shorten the sequence if possible.
*
*     In many cases, simplification will not be possible and the
*     function will return -1 to indicate this, without further
*     action.
*
*     In most cases of interest, however, this function will either
*     attempt to replace the nominated ShiftMap with a Mapping which it
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
*        Pointer to the nominated ShiftMap which is to be merged with
*        its neighbours. This should be a cloned copy of the ShiftMap
*        pointer contained in the array element "(*map_list)[where]"
*        (see below). This pointer will not be annulled, and the
*        ShiftMap it identifies will not be modified by this function.
*     where
*        Index in the "*map_list" array (below) at which the pointer
*        to the nominated ShiftMap resides.
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
   AstWinMap *w1;        /* Pointer to replacement Mapping */
   AstShiftMap *sm;      /* Pointer to this ShiftMap */
   double *aa;           /* Pointer to shift terms for new WinMap */
   double *bb;           /* Pointer to scale terms for new WinMap */
   int i;                /* Axis count */
   int nin;              /* Number of axes */
   int result;           /* Returned value */

/* Initialise. */
   result = -1;

/* Check the global error status. */
   if ( !astOK ) return result;

/* A ShiftMap is equivalent to a WinMap with unit scaling. The policy on
   simplifying a ShiftMap is to convert it to the equivalent WinMap and let
   the WinMap class do the simplifying. Create the returned WinMap, initially
   with undefined corners. */
   nin = astGetNin( this );
   w1 = astWinMap( nin, NULL, NULL, NULL, NULL, "", status );

/* If succesful, store the scale and shift terms in the WinMap. The scale
   terms are unity. */
   if( astOK ){
      sm = (AstShiftMap *) this;

      bb = w1->b;
      aa = w1->a;
      for( i = 0; i < nin; i++ ) {
         *(bb++) = 1.0;
         *(aa++) = ( *invert_list )[ where ] ? -(sm->shift)[ i ] : (sm->shift)[ i ];
      }

/* Replace the supplied ShiftMap with the new WinMap and reset the invert
   flag. */
      (void) astAnnul( ( *map_list )[ where ] );
      ( *map_list )[ where ] = (AstMapping *) w1;
      ( *invert_list )[ where ] = 0;

/* Return the index of the first modified element. */
      result = where;
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
*     ShiftMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "shiftmap.h"
*     int *MapSplit( AstMapping *this, int nin, const int *in, AstMapping **map, int *status )

*  Class Membership:
*     ShiftMap method (over-rides the protected astMapSplit method
*     inherited from the Mapping class).

*  Description:
*     This function creates a new Mapping by picking specified inputs from
*     an existing ShiftMap. This is only possible if the specified inputs
*     correspond to some subset of the ShiftMap outputs. That is, there
*     must exist a subset of the ShiftMap outputs for which each output
*     depends only on the selected ShiftMap inputs, and not on any of the
*     inputs which have not been selected. If this condition is not met
*     by the supplied ShiftMap, then a NULL Mapping is returned.

*  Parameters:
*     this
*        Pointer to the ShiftMap to be split (the ShiftMap is not actually
*        modified by this function).
*     nin
*        The number of inputs to pick from "this".
*     in
*        Pointer to an array of indices (zero based) for the inputs which
*        are to be picked. This array should have "nin" elements. If "Nin"
*        is the number of inputs of the supplied ShiftMap, then each element
*        should have a value in the range zero to Nin-1.
*     map
*        Address of a location at which to return a pointer to the new
*        Mapping. This Mapping will have "nin" inputs (the number of
*        outputs may be different to "nin"). A NULL pointer will be
*        returned if the supplied ShiftMap has no subset of outputs which
*        depend only on the selected inputs.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a dynamically allocated array of ints. The number of
*     elements in this array will equal the number of outputs for the
*     returned Mapping. Each element will hold the index of the
*     corresponding output in the supplied ShiftMap. The array should be
*     freed using astFree when no longer needed. A NULL pointer will
*     be returned if no output Mapping can be created.

*  Notes:
*     - If this function is invoked with the global error status set,
*     or if it should fail for any reason, then NULL values will be
*     returned as the function value and for the "map" pointer.
*/

/* Local Variables: */
   AstShiftMap *newsm;        /* Pointer to returned ShiftMap */
   AstShiftMap *this;         /* Pointer to ShiftMap structure */
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

/* Get a pointer to the ShiftMap structure. */
   this = (AstShiftMap *) this_map;

/* Allocate memory for the returned array and create a ShiftMap with the
   required number of axes and initially unsorted shifts. */
   result = astMalloc( sizeof( int )*(size_t) nin );
   newsm = astShiftMap( nin, this->shift, "", status );
   *map = (AstMapping *) newsm;

/* Check pointers can be used safely. */
   if( astOK ) {

/* Store the required shifts in the new ShiftMap. At the same time check
   that each axis is valid. */
      mnin = astGetNin( this );
      ok = 1;
      for( i = 0; i < nin; i++ ) {
         iin = in[ i ];
         if( iin >= 0 && iin < mnin ) {
            (newsm->shift)[ i ] = (this->shift)[ iin ];
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

/* If the indices are good, invert the returned ShiftMap if the supplied
   ShiftMap is inverted. */
      } else {
         if( astGetInvert( this ) ) astInvert( *map );
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
*     #include "shiftmap.h"
*     result = Rate( AstMapping *this, double *at, int ax1, int ax2, int *status )

*  Class Membership:
*     ShiftMap member function (overrides the astRate method inherited
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
*     Apply a ShiftMap to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "shiftmap.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     ShiftMap member function (over-rides the astTransform protected
*     method inherited from the Mapping class).

*  Description:
*     This function takes a ShiftMap and a set of points encapsulated in a
*     PointSet and transforms the points so as to map them into the
*     required window.

*  Parameters:
*     this
*        Pointer to the ShiftMap.
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
*     match the number of coordinates for the ShiftMap being applied.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstPointSet *result;          /* Pointer to output PointSet */
   AstShiftMap *map;             /* Pointer to ShiftMap to be applied */
   const char *class;            /* Object class */
   double **ptr_in;              /* Pointer to input coordinate data */
   double **ptr_out;             /* Pointer to output coordinate data */
   double *axin;                 /* Pointer to next input axis value */
   double *axout;                /* Pointer to next output axis value */
   double a;                     /* Shift for current axis */
   int coord;                    /* Loop counter for coordinates */
   int ncoord;                   /* Number of coordinates per point */
   int npoint;                   /* Number of points */
   int point;                    /* Loop counter for points */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the ShiftMap. */
   map = (AstShiftMap *) this;

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

/* Report an error if the ShiftMap does not contain any shifts. */
   if( !map->shift && astOK ){
      class = astGetClass( this );
      astError( AST__BADSM, "astTransform(%s): The supplied %s does not "
                "contain any shift information.", status, class, class );
   }

/* Perform coordinate arithmetic. */
/* ------------------------------ */
   if( astOK ){

/* Apply the mapping to each axis. */
      for( coord = 0; coord < ncoord; coord++ ){

/* Store pointers to the first input and output values on this axis. */
         axin = ptr_in[ coord ];
         axout = ptr_out[ coord ];

/* Get the value to add to each axis value. */
         a = (map->shift)[ coord ];

/* If the shift is bad store bad output values. */
         if( a == AST__BAD ){
            for( point = 0; point < npoint; point++ ) *(axout++) = AST__BAD;

/* Otherwise, shift this axis, taking account of whether the mapping is
   inverted or not. */
         } else {
            if( !forward ) a = -a;

            for( point = 0; point < npoint; point++ ){
               if( *axin != AST__BAD ){
                  *(axout++) = (*axin) + a;
               } else {
                  *(axout++) = AST__BAD;
               }
               axin++;
            }
         }
      }
   }

/* Return a pointer to the output PointSet. */
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
*     Copy constructor for ShiftMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for ShiftMap objects.

*  Parameters:
*     objin
*        Pointer to the ShiftMap to be copied.
*     objout
*        Pointer to the ShiftMap being constructed.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstShiftMap *out;              /* Pointer to output ShiftMap */
   AstShiftMap *in;               /* Pointer to input ShiftMap */
   int ncoord;                  /* No. of axes for the mapping */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the input and output ShiftMaps. */
   in= (AstShiftMap *) objin;
   out = (AstShiftMap *) objout;

/* Get the number of coordinates mapped by the ShiftMap. */
   ncoord = astGetNin( in );

/* Allocate memory holding copies of the shifts defining the mapping. */
   out->shift = (double *) astStore( NULL, (void *) in->shift,
                                     sizeof(double)*(size_t)ncoord );

/* If an error occurred, free any allocated memory. */
   if ( !astOK ) {
      out->shift = (double *) astFree( (void *) out->shift );
   }

}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for ShiftMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for ShiftMap objects.

*  Parameters:
*     obj
*        Pointer to the ShiftMap to be deleted.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - This destructor does nothing and exists only to maintain a
*     one-to-one correspondence between destructors and copy
*     constructors.
*/

/* Local Variables: */
   AstShiftMap *this;              /* Pointer to ShiftMap */

/* Obtain a pointer to the ShiftMap structure. */
   this = (AstShiftMap *) obj;

/* Free the memory holding the shifts. */
   this->shift = (double *) astFree( (void *) this->shift );

}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for ShiftMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the ShiftMap class to an output Channel.

*  Parameters:
*     this
*        Pointer to the ShiftMap whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Constants: */
#define COMMENT_LEN 50           /* Maximum length of a comment string */
#define KEY_LEN 50               /* Maximum length of a keyword */

/* Local Variables: */
   AstShiftMap *this;              /* Pointer to the ShiftMap structure */
   char buff[ KEY_LEN + 1 ];     /* Buffer for keyword string */
   char comment[ COMMENT_LEN + 1 ]; /* Buffer for comment string */
   int axis;                     /* Axis index */
   int ncoord;                   /* No. of axes for mapping */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the ShiftMap structure. */
   this = (AstShiftMap *) this_object;

/* Get the number of coordinates to be mapped. */
   ncoord = astGetNin( this );

/* Write out values representing the instance variables for the
   ShiftMap class.  Accompany these with appropriate comment strings,
   possibly depending on the values being written.*/

/* The shifts. */
   for( axis = 0; axis < ncoord; axis++ ){
      (void) sprintf( buff, "Sft%d", axis + 1 );
      (void) sprintf( comment, "Shift for axis %d", axis + 1 );
      astWriteDouble( channel, buff, (this->shift)[ axis ] != 0.0, 0,
                      (this->shift)[ axis ], comment );
   }

/* Undefine macros local to this function. */
#undef COMMENT_LEN
#undef KEY_LEN
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAShiftMap and astCheckShiftMap functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(ShiftMap,Mapping)
astMAKE_CHECK(ShiftMap)

AstShiftMap *astShiftMap_( int ncoord, const double shift[], const char *options, int *status, ...) {
/*
*++
*  Name:
c     astShiftMap
f     AST_SHIFTMAP

*  Purpose:
*     Create a ShiftMap.

*  Type:
*     Public function.

*  Synopsis:
c     #include "shiftmap.h"
c     AstShiftMap *astShiftMap( int ncoord, const double shift[],
c                               const char *options, ... )
f     RESULT = AST_SHIFTMAP( NCOORD, SHIFT, OPTIONS, STATUS )

*  Class Membership:
*     ShiftMap constructor.

*  Description:
*     This function creates a new ShiftMap and optionally initialises its
*     attributes.
*
*     A ShiftMap is a linear Mapping which shifts each axis by a
*     specified constant value.

*  Parameters:
c     ncoord
f     NCOORD = INTEGER (Given)
*        The number of coordinate values for each point to be
*        transformed (i.e. the number of dimensions of the space in
*        which the points will reside). The same number is applicable
*        to both input and output points.
c     shift
f     SHIFT( NCOORD ) = DOUBLE PRECISION (Given)
*        An array containing the values to be added on to the input
*        coordinates in order to create the output coordinates. A separate
*        value should be supplied for each coordinate.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new ShiftMap. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new ShiftMap. The syntax used is identical to that for the
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
c     astShiftMap()
f     AST_SHIFTMAP = INTEGER
*        A pointer to the new ShiftMap.

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
   AstShiftMap *new;              /* Pointer to new ShiftMap */
   va_list args;                  /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the ShiftMap, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitShiftMap( NULL, sizeof( AstShiftMap ), !class_init, &class_vtab,
                        "ShiftMap", ncoord, shift );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new ShiftMap's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new ShiftMap. */
   return new;
}

AstShiftMap *astShiftMapId_( int ncoord, const double shift[],
                             const char *options, ... ) {
/*
*  Name:
*     astShiftMapId_

*  Purpose:
*     Create a ShiftMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "shiftmap.h"
*     AstShiftMap *astShiftMapId_( int ncoord, const double shift[],
*                                  const char *options, ... )

*  Class Membership:
*     ShiftMap constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astShiftMap constructor function. It returns an ID value (instead
*     of a true C pointer) to external users, and must be provided
*     because astShiftMap_ has a variable argument list which cannot be
*     encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     The variable argument list also prevents this function from
*     invoking astShiftMap_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astShiftMap_.

*  Returned Value:
*     The ID value associated with the new ShiftMap.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstShiftMap *new;            /* Pointer to new ShiftMap */
   va_list args;                /* Variable argument list */

   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the ShiftMap, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitShiftMap( NULL, sizeof( AstShiftMap ), !class_init, &class_vtab,
                          "ShiftMap", ncoord, shift );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new ShiftMap's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new ShiftMap. */
   return astMakeId( new );
}

AstShiftMap *astInitShiftMap_( void *mem, size_t size, int init,
                              AstShiftMapVtab *vtab, const char *name,
                              int ncoord, const double *shift, int *status ) {
/*
*+
*  Name:
*     astInitShiftMap

*  Purpose:
*     Initialise a ShiftMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "shiftmap.h"
*     AstShiftMap *astInitShiftMap( void *mem, size_t size, int init,
*                                   AstShiftMapVtab *vtab, const char *name,
*                                   int ncoord, const double *shift )

*  Class Membership:
*     ShiftMap initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new ShiftMap object. It allocates memory (if necessary) to accommodate
*     the ShiftMap plus any additional data associated with the derived class.
*     It then initialises a ShiftMap structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a ShiftMap at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the ShiftMap is to be initialised.
*        This must be of sufficient size to accommodate the ShiftMap data
*        (sizeof(ShiftMap)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the ShiftMap (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the ShiftMap
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the ShiftMap's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new ShiftMap.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the astGetClass
*        method).
*     ncoord
*        The number of coordinate values per point.
*     shift
*        Pointer to an array of shifts, one for each coordinate.

*  Returned Value:
*     A pointer to the new ShiftMap.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstShiftMap *new;            /* Pointer to new ShiftMap */
   int axis;                    /* Axis index */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitShiftMapVtab( vtab, name );

/* Initialise. */
   new = NULL;

/* Initialise a Mapping structure (the parent class) as the first component
   within the ShiftMap structure, allocating memory if necessary. Specify that
   the Mapping should be defined in both the forward and inverse directions. */
   new = (AstShiftMap *) astInitMapping( mem, size, 0,
                                         (AstMappingVtab *) vtab, name,
                                         ncoord, ncoord, 1, 1 );

   if ( astOK ) {

/* Initialise the ShiftMap data. */
/* ---------------------------- */
/* Allocate memory to hold the shift for each axis. */
      new->shift = (double *) astMalloc( sizeof(double)*(size_t)ncoord );

/* Check the pointers can be used */
      if( astOK ){

/* Store the shift and scale for each axis. */
         for( axis = 0; axis < ncoord; axis++ ){
            (new->shift)[ axis ] = shift ? shift[ axis ] : AST__BAD;
         }

      }

/* If an error occurred, clean up by deleting the new ShiftMap. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new ShiftMap. */
   return new;
}

AstShiftMap *astLoadShiftMap_( void *mem, size_t size,
                               AstShiftMapVtab *vtab, const char *name,
                               AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadShiftMap

*  Purpose:
*     Load a ShiftMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "shiftmap.h"
*     AstShiftMap *astLoadShiftMap( void *mem, size_t size,
*                                   AstShiftMapVtab *vtab, const char *name,
*                                   AstChannel *channel )

*  Class Membership:
*     ShiftMap loader.

*  Description:
*     This function is provided to load a new ShiftMap using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     ShiftMap structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a ShiftMap at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the ShiftMap is to be
*        loaded.  This must be of sufficient size to accommodate the
*        ShiftMap data (sizeof(ShiftMap)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the ShiftMap (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the ShiftMap structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstShiftMap) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new ShiftMap. If this is NULL, a pointer
*        to the (static) virtual function table for the ShiftMap class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "ShiftMap" is used instead.

*  Returned Value:
*     A pointer to the new ShiftMap.

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
   AstShiftMap *new;            /* Pointer to the new ShiftMap */
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
   the first loader to be invoked for this ShiftMap. In this case the
   ShiftMap belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstShiftMap );
      vtab = &class_vtab;
      name = "ShiftMap";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitShiftMapVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built ShiftMap. */
   new = astLoadMapping( mem, size, (AstMappingVtab *) vtab, name,
                         channel );

   if ( astOK ) {

/* Get the number of axis for the mapping. */
      ncoord = astGetNin( (AstMapping *) new );

/* Allocate memory to hold the shifts. */
      new->shift = (double *) astMalloc( sizeof(double)*(size_t)ncoord );

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "ShiftMap" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* The shifts. */
      for( axis = 0; axis < ncoord; axis++ ){
         (void) sprintf( buff, "sft%d", axis + 1 );
         (new->shift)[ axis ] = astReadDouble( channel, buff, 0.0 );
      }
   }

/* If an error occurred, clean up by deleting the new ShiftMap. */
   if ( !astOK ) new = astDelete( new );

/* Return the new ShiftMap pointer. */
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





