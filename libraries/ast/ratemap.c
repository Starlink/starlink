/*
*class++
*  Name:
*     RateMap

*  Purpose:
*     Mapping which represents differentiation.

*  Constructor Function:
c     astRateMap
f     AST_RATEMAP

*  Description:
*     A RateMap is a Mapping which represents a single element of the
*     Jacobian matrix of another Mapping. The Mapping for which the
*     Jacobian is required is specified when the new RateMap is created,
*     and is referred to as the "encapsulated Mapping" below.
*
*     The number of inputs to a RateMap is the same as the number of inputs
*     to its encapsulated Mapping. The number of outputs from a RateMap
*     is always one. This one output equals the rate of change of a
*     specified output of the encapsulated Mapping with respect to a
*     specified input of the encapsulated Mapping (the input and output
*     to use are specified when the RateMap is created).
*
*     A RateMap which has not been inverted does not define an inverse
*     transformation. If a RateMap has been inverted then it will define
*     an inverse transformation but not a forward transformation.

*  Inheritance:
*     The RateMap class inherits from the Mapping class.

*  Attributes:
*     The RateMap class does not define any new attributes beyond those
*     which are applicable to all Mappings.

*  Functions:
c     The RateMap class does not define any new functions beyond those
f     The RateMap class does not define any new routines beyond those
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
*     DSB: David S. Berry (Starlink)

*  History:
*     10-FEB-2004 (DSB):
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
#define astCLASS RateMap

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* Sets of points/coordinates */
#include "mapping.h"             /* Coordinate Mappings (parent class) */
#include "channel.h"             /* I/O channels */
#include "ratemap.h"             /* Interface definition for this class */
#include "unitmap.h"             /* Unit Mappings */
#include "frame.h"               /* Frames */

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
static int (* parent_getobjsize)( AstObject *, int * );
static AstPointSet *(* parent_transform)( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static int *(* parent_mapsplit)( AstMapping *, int, const int *, AstMapping **, int * );

#if defined(THREAD_SAFE)
static int (* parent_managelock)( AstObject *, int, int, AstObject **, int * );
#endif


#ifdef THREAD_SAFE
/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(RateMap)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(RateMap,Class_Init)
#define class_vtab astGLOBAL(RateMap,Class_Vtab)


#include <pthread.h>


#else


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstRateMapVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstRateMap *astRateMapId_( void *, int, int, const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstMapping *RemoveRegions( AstMapping *, int * );
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static int MapMerge( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static int *MapSplit( AstMapping *, int, const int *, AstMapping **, int * );
static int Equal( AstObject *, AstObject *, int * );
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
*     Test if two RateMaps are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "ratemap.h"
*     int Equal( AstObject *this, AstObject *that, int *status )

*  Class Membership:
*     RateMap member function (over-rides the astEqual protected
*     method inherited from the astMapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two RateMaps are equivalent.

*  Parameters:
*     this
*        Pointer to the first Object (a RateMap).
*     that
*        Pointer to the second Object.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the RateMaps are equivalent, zero otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstRateMap *that;
   AstRateMap *this;
   int nin;
   int nout;
   int result;
   int that_inv;
   int this_inv;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain pointers to the two RateMap structures. */
   this = (AstRateMap *) this_object;
   that = (AstRateMap *) that_object;

/* Check the second object is a RateMap. We know the first is a
   RateMap since we have arrived at this implementation of the virtual
   function. */
   if( astIsARateMap( that ) ) {

/* Get the number of inputs and outputs and check they are the same for both. */
      nin = astGetNin( this );
      nout = astGetNout( this );
      if( astGetNin( that ) == nin && astGetNout( that ) == nout ) {

/* If the Invert flags for the two RateMaps differ, it may still be possible
   for them to be equivalent. First compare the RateMaps if their Invert
   flags are the same. In this case all the attributes of the two RateMaps
   must be identical. */
         if( astGetInvert( this ) == astGetInvert( that ) ) {

/* Temporarily re-instate the original Invert flag values. */
            this_inv = astGetInvert( this->map );
            that_inv = astGetInvert( that->map );
            astSetInvert( this->map, this->invert );
            astSetInvert( that->map, that->invert );

            if( astEqual( this->map, that->map ) &&
                this->iin == that->iin &&
                this->iout == that->iout ){
               result = 1;
            }

/* Restore the original Invert flag values. */
            astSetInvert( this->map, this_inv );
            astSetInvert( that->map, that_inv );

/* If the Invert flags for the two RateMaps differ, the attributes of the two
   RateMaps must be inversely related to each other. */
         } else {

/* In the specific case of a RateMap, Invert flags must be equal. */
            result = 0;

         }
      }
   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
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
*     #include "ratemap.h"
*     int GetObjSize( AstObject *this, int *status )

*  Class Membership:
*     RateMap member function (over-rides the astGetObjSize protected
*     method inherited from the parent class).

*  Description:
*     This function returns the in-memory size of the supplied RateMap,
*     in bytes.

*  Parameters:
*     this
*        Pointer to the RateMap.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The Object size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstRateMap *this;         /* Pointer to RateMap structure */
   int result;                /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointers to the RateMap structure. */
   this = (AstRateMap *) this_object;

/* Invoke the GetObjSize method inherited from the parent class, and then
   add on any components of the class structure defined by thsi class
   which are stored in dynamically allocated memory. */
   result = (*parent_getobjsize)( this_object, status );
   result += astGetObjSize( this->map );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

void astInitRateMapVtab_(  AstRateMapVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitRateMapVtab

*  Purpose:
*     Initialise a virtual function table for a RateMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "ratemap.h"
*     void astInitRateMapVtab( AstRateMapVtab *vtab, const char *name )

*  Class Membership:
*     RateMap vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the RateMap class.

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
   will be used (by astIsARateMap) to determine if an object belongs to
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

   mapping->RemoveRegions = RemoveRegions;

#if defined(THREAD_SAFE)
   parent_managelock = object->ManageLock;
   object->ManageLock = ManageLock;
#endif

   parent_transform = mapping->Transform;
   mapping->Transform = Transform;

   parent_mapsplit = mapping->MapSplit;
   mapping->MapSplit = MapSplit;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   object->Equal = Equal;
   mapping->MapMerge = MapMerge;

/* Declare the copy constructor, destructor and class dump function. */
   astSetCopy( vtab, Copy );
   astSetDelete( vtab, Delete );
   astSetDump( vtab, Dump, "RateMap", "Differential Mapping" );

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
*     RateMap member function (over-rides the astManageLock protected
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
   AstRateMap *this;       /* Pointer to RateMap structure */
   int result;             /* Returned status value */

/* Initialise */
   result = 0;

/* Check the supplied pointer is not NULL. */
   if( !this_object ) return result;

/* Obtain a pointers to the RateMap structure. */
   this = (AstRateMap *) this_object;

/* Invoke the ManageLock method inherited from the parent class. */
   if( !result ) result = (*parent_managelock)( this_object, mode, extra,
                                                fail, status );

/* Invoke the astManageLock method on any Objects contained within
   the supplied Object. */
   if( !result ) result = astManageLock( this->map, mode, extra, fail );

   return result;

}
#endif

static int MapMerge( AstMapping *this, int where, int series, int *nmap,
                     AstMapping ***map_list, int **invert_list, int *status ) {
/*
*  Name:
*     MapMerge

*  Purpose:
*     Simplify a sequence of Mappings containing a RateMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     int MapMerge( AstMapping *this, int where, int series, int *nmap,
*                   AstMapping ***map_list, int **invert_list, int *status )

*  Class Membership:
*     RateMap method (over-rides the protected astMapMerge method
*     inherited from the Mapping class).

*  Description:
*     This function attempts to simplify a sequence of Mappings by
*     merging a nominated RateMap in the sequence with its neighbours,
*     so as to shorten the sequence if possible.
*
*     In many cases, simplification will not be possible and the
*     function will return -1 to indicate this, without further
*     action.
*
*     In most cases of interest, however, this function will either
*     attempt to replace the nominated RateMap with one which it
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
*        Pointer to the nominated RateMap which is to be merged with
*        its neighbours. This should be a cloned copy of the RateMap
*        pointer contained in the array element "(*map_list)[where]"
*        (see below). This pointer will not be annulled, and the
*        RateMap it identifies will not be modified by this function.
*     where
*        Index in the "*map_list" array (below) at which the pointer
*        to the nominated RateMap resides.
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
   AstMapping *emap1;
   AstMapping *emap2;
   AstMapping *emap;
   AstMapping *smap;
   AstRateMap *map;
   AstRateMap *rmap1;
   AstRateMap *rmap2;
   int cancel;
   int map_inv;
   int nax;
   int old_inv2;
   int old_inv;
   int old_winv;
   int result;

/* Initialise. */
   result = -1;

/* Check the inherited status. */
   if ( !astOK ) return result;

/* Initialisation to avoid compiler warnings. */
   nax = 0;

/* Get a pointer to this RateMap. */
   map = (AstRateMap *) this;

/* Temporarily set its Invert flag to the requested value. */
   map_inv = astGetInvert( map );
   astSetInvert( map, ( *invert_list )[ where ] );

/* Get the encapsulated Mapping, and temporarily set its Invert attribute
   back to the value it had when the RateMap was created, saving the current
   Invert value so that it can be re-instated later. */
   emap = map->map;
   old_inv = astGetInvert( emap );
   astSetInvert( emap, map->invert );

/* First try to simplify the RateMap by simplifying its encapsulated
   Mapping. */
   smap = astSimplify( emap );

/* If any simplification took place, create a new RateMap with the
   simplified mapping. */
   if( smap != emap ) {
      (void) astAnnul( ( *map_list )[ where ] );
      ( *map_list )[ where ] = (AstMapping *) astRateMap( smap, map->iout, map->iin, "", status );
      result = where;

/* The only other simplication which can be performed is to cancel a RateMap
   with its own inverse in series. */
   } else if( series ) {

/* Indicate we have nothing to cancel with as yet. */
      cancel = -1;

/* First consider the lower neighbour. */
      if( where > 0 && astIsARateMap( ( *map_list )[ where - 1 ] ) ) {

/* Check the Invert flags are opposite */
         if( ( *invert_list )[ where ] != ( *invert_list )[ where - 1 ] ) {
            rmap1 = map;
            rmap2 = (AstRateMap *) ( *map_list )[ where - 1 ];

/* Check the input and output indices are equal. */
            if( rmap1->iin == rmap2->iin &&
                rmap1->iout == rmap2->iout ) {

/* Check the encapsulated Mappings are equal. */
               emap1 = emap;
               emap2 = rmap2->map;
               old_winv = astGetInvert( rmap2 );
               astSetInvert( rmap2, ( *invert_list )[ where - 1 ] );
               old_inv2 = astGetInvert( emap2 );
               astSetInvert( emap2,  rmap2->invert );

               if( astEqual( emap1, emap2 ) ) cancel = where - 1;

               astSetInvert( emap2,  old_inv2 );
               astSetInvert( rmap2, old_winv );

               nax = astGetNout( rmap1 );
            }
         }
      }

/* Likewise consider the upper neighbour. */
      if( cancel == -1 && where + 1 < *nmap &&
          astIsARateMap( ( *map_list )[ where + 1 ] ) ) {

         if( ( *invert_list )[ where ] != ( *invert_list )[ where + 1 ] ) {
            rmap1 = map;
            rmap2 = (AstRateMap *) ( *map_list )[ where + 1 ];
            if( rmap1->iin == rmap2->iin &&
                rmap1->iout == rmap2->iout ) {
               emap1 = emap;
               emap2 = rmap2->map;
               old_winv = astGetInvert( rmap2 );
               astSetInvert( rmap2, ( *invert_list )[ where + 1 ] );
               old_inv2 = astGetInvert( emap2 );
               astSetInvert( emap2,  rmap2->invert );

               if( astEqual( emap1, emap2 ) ) cancel = where + 1;

               astSetInvert( emap2,  old_inv2 );
               astSetInvert( rmap2, old_winv );

               nax = astGetNin( rmap1 );
            }
         }
      }

/* If we can cancel with a neightbour, do so. */
      if( cancel != -1 ) {
         (void) astAnnul( ( *map_list )[ where ] );
         (void) astAnnul( ( *map_list )[ cancel ] );
         ( *map_list )[ where ] = (AstMapping *) astUnitMap( nax, "", status );
         ( *invert_list )[ where ] = 0;
         ( *map_list )[ cancel ] = (AstMapping *) astUnitMap( nax, "", status );
         ( *invert_list )[ cancel ] = 0;
          result = ( cancel < where ) ? cancel : where;
      }
   }

/* Free resources. */
   smap = astAnnul( smap );

/* Reset the original Invert attribute for the encapsulated Mapping. */
   astSetInvert( emap, old_inv );

/* Reset the original Invert attribute for the specified RateMap */
   astSetInvert( map, map_inv );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = -1;

/* Return the result. */
   return result;
}

static int *MapSplit( AstMapping *this_map, int nin, const int *in, AstMapping **map, int *status ){
/*
*  Name:
*     MapSplit

*  Purpose:
*     Create a Mapping representing a subset of the inputs of an existing
*     RateMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "ratemap.h"
*     int *MapSplit( AstMapping *this, int nin, const int *in, AstMapping **map, int *status )

*  Class Membership:
*     RateMap method (over-rides the protected astMapSplit method
*     inherited from the Mapping class).

*  Description:
*     This function creates a new Mapping by picking specified inputs from
*     an existing RateMap. This is only possible if the specified inputs
*     correspond to some subset of the RateMap outputs. That is, there
*     must exist a subset of the RateMap outputs for which each output
*     depends only on the selected RateMap inputs, and not on any of the
*     inputs which have not been selected. If this condition is not met
*     by the supplied RateMap, then a NULL Mapping is returned.

*  Parameters:
*     this
*        Pointer to the RateMap to be split (the RateMap is not actually
*        modified by this function).
*     nin
*        The number of inputs to pick from "this".
*     in
*        Pointer to an array of indices (zero based) for the inputs which
*        are to be picked. This array should have "nin" elements. If "Nin"
*        is the number of inputs of the supplied RateMap, then each element
*        should have a value in the range zero to Nin-1.
*     map
*        Address of a location at which to return a pointer to the new
*        Mapping. This Mapping will have "nin" inputs (the number of
*        outputs may be different to "nin"). A NULL pointer will be
*        returned if the supplied RateMap has no subset of outputs which
*        depend only on the selected inputs.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a dynamically allocated array of ints. The number of
*     elements in this array will equal the number of outputs for the
*     returned Mapping. Each element will hold the index of the
*     corresponding output in the supplied RateMap. The array should be
*     freed using astFree when no longer needed. A NULL pointer will
*     be returned if no output Mapping can be created.

*  Notes:
*     - If this function is invoked with the global error status set,
*     or if it should fail for any reason, then NULL values will be
*     returned as the function value and for the "map" pointer.
*/

/* Local Variables: */
   AstMapping *emap;          /* Pointer to Mapping encapsulated by RateMap */
   AstMapping *remap;         /* Split Mapping encapsulated by RateMap */
   AstRateMap *this;          /* Pointer to RateMap structure */
   int *eres;                 /* Outputs used by split Mapping */
   int *result;               /* Array holding returned output inedx */
   int ax1;                   /* New index of output being differentiated */
   int ax2;                   /* New index of output being varied */
   int i;                     /* Loop count */
   int nout;                  /* No. of outputs in the split Mapping */
   int old_inv;               /* Original Invert flag for emap */

/* Initialise */
   result = NULL;
   *map = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Invoke the parent astMapSplit method to see if it can do the job. */
   result = (*parent_mapsplit)( this_map, nin, in, map, status );

/* If not, we provide a special implementation here. Note we cannot
   produce the Mapping if the RaterMap has been inverted. */
   if( !result && !astGetInvert( this_map ) ) {

/* Get a pointer to the RateMap structure. */
      this = (AstRateMap *) this_map;

/* Temporarily reset the Invert attribute of the encapsulated Mapping
   back to the value it had when the RateMap was created. */
      emap = this->map;
      old_inv = astGetInvert( emap );
      astSetInvert( emap, this->invert );

/* Attempt to split the encapsulated Mapping */
      eres = astMapSplit( emap, nin, in, &remap );

/* We can only continue if this was succesful. */
      if( eres ) {

/* Check that the input which the RateMap varies is one of the selected
   inputs. */
         ax2 = -1;
         for( i = 0; i < nin; i++ ) {
            if( in[ i ] == this->iin ) {
               ax2 = i;
               break;
            }
         }

/* Check that the output which the RateMap differentiates is one of the
   outputs of the "remap" Mapping. */
         ax1 = -1;
         nout = astGetNout( remap );
         for( i = 0; i < nout; i++ ) {
            if( eres[ i ] == this->iout ) {
               ax1 = i;
               break;
            }
         }

/* If possible create the required Mapping and returned array. */
         if( ax1 != -1 && ax2 != -1 ) {
            *map = (AstMapping *) astRateMap( remap, ax1, ax2, "", status );
            result = astMalloc( sizeof( int ) );
            if( astOK ) *result= 0;
         }

/* Free resources */
         eres = astFree( eres );
         remap = astAnnul( remap );
      }

/* Re-instate the original Invert flag in the Mapping encapsulated by the
   supplied RateMap. */
      astSetInvert( emap, old_inv );
   }

/* Free returned resources if an error has occurred. */
   if( !astOK ) {
      result = astFree( result );
      *map = astAnnul( *map );
   }

/* Return the list of output indices. */
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
*     #include "ratemap.h"
*     AstMapping *RemoveRegions( AstMapping *this, int *status )

*  Class Membership:
*     RateMap method (over-rides the astRemoveRegions method inherited
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
*     The implementation provided by the RateMap class invokes the
*     astRemoveRegions method on the encapsulated Mapping, and returns a
*     new RateMap containing the resulting Mapping.

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
   AstMapping *newmap;           /* New component Mapping */
   AstMapping *result;           /* Result pointer to return */
   AstRateMap *new;              /* Pointer to new RateMap */
   AstRateMap *this;             /* Pointer to RateMap structure */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the RateMap. */
   this = (AstRateMap *) this_mapping;

/* Invoke the astRemoveRegions method on the component Mapping. */
   newmap = astRemoveRegions( this->map );

/* If the Mapping was not modified, just return a clone of the supplied
   pointer. */
   if( this->map == newmap ) {
      result = astClone( this );

/* Otherwise, we need to create a new RateMap to return. */
   } else {

/* If the new Mapping is a Frame (as will be the case if the original
   Mapping was a Region), use a UnitMap instead. */
      if( astIsAFrame( newmap ) ) {
         (void) astAnnul( newmap );
         newmap = (AstMapping *) astUnitMap( astGetNin( this ), " ", status );
      }

/* Take a deep copy of the supplied RateMap and then modify the Mapping
   so that we retain any extra information in the supplied RateMap. */
      new = astCopy( this );
      (void) astAnnul( new->map );
      new->map = astClone( newmap );
      result = (AstMapping *) new;
   }

/* Free resources. */
   newmap = astAnnul( newmap );

/* Annul the returned Mapping if an error has occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return the result. */
   return result;
}

static AstPointSet *Transform( AstMapping *this, AstPointSet *in,
                               int forward, AstPointSet *out, int *status ) {
/*
*  Name:
*     Transform

*  Purpose:
*     Apply a RateMap to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "ratemap.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     RateMap member function (over-rides the astTransform method inherited
*     from the Mapping class).

*  Description:
*     This function takes a RateMap and a set of points encapsulated in a
*     PointSet and transforms the points so as to apply the required Mapping.
*     This implies applying each of the RateMap's component Mappings in turn,
*     either in series or in parallel.

*  Parameters:
*     this
*        Pointer to the RateMap.
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
*     match the number of coordinates for the RateMap being applied.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstMapping *emap;
   AstPointSet *result;
   AstRateMap *map;
   double **ptr2;
   double **ptr;
   double *pout;
   double *work;
   int ic;
   int iin;
   int iout;
   int ipoint;
   int ncoord;
   int npoint;
   int old_inv;

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the RateMap. */
   map = (AstRateMap *) this;

/* Apply the parent Mapping using the stored pointer to the Transform member
   function inherited from the parent Mapping class. This function validates
   all arguments and generates an output PointSet if necessary, but does not
   actually transform any coordinate values. */
   result = (*parent_transform)( this, in, forward, out, status );

/* We now extend the parent astTransform method by applying the component
   Mappings of the RateMap to generate the output coordinate values. */

/* Determine whether to apply the forward or inverse Mapping, according to the
   direction specified and whether the Mapping has been inverted. */
   if ( astGetInvert( map ) ) forward = !forward;

/* The RateMap class does not have an inverse transformation. */
   if( !forward ) {
      astError( AST__INTER, "astTransform(%s): The %s class does not have "
                "an inverse transformation (AST internal programming error).", status,
                astGetClass( this ), astGetClass( this ) );

/* Otherwise use the astRate method on the encapsulated Maping to
   determine the required rate of change at each supplied input point. */
   } else {

/* Temporarily reset the Invert attribute of the encapsulated Mapping
   back to the value it had when the RateMap was created. */
      emap = map->map;
      old_inv = astGetInvert( emap );
      astSetInvert( emap, map->invert );

/* Note the indices of the input and output to use. */
      iin = map->iin;
      iout = map->iout;

/* Get pointers to the axis values in the supplied PointSet. */
      ptr = astGetPoints( in );
      ncoord = astGetNcoord( in );
      npoint = astGetNpoint( in );

/* Work space to hold an input position. */
      work = astMalloc( sizeof( double )*(size_t) ncoord );

/* Get a pointer to the axis values in the results PointSet. */
      ptr2 = astGetPoints( result );
      pout = ptr2[ 0 ];
      if( astOK ) {

/* Loop round each point in the supplied PointSet. */
         for( ipoint = 0; ipoint < npoint; ipoint++ ) {

/* Copy this point into the work array. */
            for( ic = 0; ic < ncoord; ic++ ) work[ ic ] = ptr[ ic ][ ipoint ];

/* Find the rate of change of the specified output of the encapsulated
   Mapping with respect to the specified input. */
            *(pout++) = astRate( emap, work, iout, iin );
         }
      }

/* Re-instate the original Invert flag. */
      astSetInvert( emap, old_inv );

/* Free resources */
      work = astFree( work );

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
*     Copy constructor for RateMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for RateMap objects.

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
*     Mappings within the RateMap.
*/

/* Local Variables: */
   AstRateMap *in;                /* Pointer to input RateMap */
   AstRateMap *out;               /* Pointer to output RateMap */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output RateMaps. */
   in = (AstRateMap *) objin;
   out = (AstRateMap *) objout;

/* For safety, start by clearing any references to the input component
   Mappings from the output RateMap. */
   out->map = NULL;

/* Make copies of these Mappings and store pointers to them in the output
   RateMap structure. */
   out->map = astCopy( in->map );
}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for RateMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for RateMap objects.

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
   AstRateMap *this;              /* Pointer to RateMap */

/* Obtain a pointer to the RateMap structure. */
   this = (AstRateMap *) obj;

/* Annul the pointers to the component Mappings. */
   this->map = astAnnul( this->map );

/* Clear the remaining RateMap variables. */
   this->invert = 0;
   this->iin = 0;
   this->iout = 0;
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for RateMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the RateMap class to an output Channel.

*  Parameters:
*     this
*        Pointer to the RateMap whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstRateMap *this;              /* Pointer to the RateMap structure */
   int ival;                     /* Integer value */
   int set;                      /* Attribute value set? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the RateMap structure. */
   this = (AstRateMap *) this_object;

/* Write out values representing the instance variables for the RateMap
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

/* Input axis. */
/* ------------ */
   ival = this->iin;
   set = ( ival != 0 );
   astWriteInt( channel, "IIn", set, 0, ival, "Index of Mapping input" );

/* Output axis. */
/* ------------ */
   ival = this->iout;
   set = ( ival != 0 );
   astWriteInt( channel, "IOut", set, 0, ival, "Index of Mapping output" );

/* Invert flag. */
/* ------------ */
   ival = this->invert;
   set = ( ival != 0 );
   astWriteInt( channel, "Inv", set, 0, ival,
                ival ? "Mapping used in inverse direction" :
                       "Mapping used in forward direction" );

/* Mapping. */
/* -------- */
   astWriteObject( channel, "Map", 1, 1, this->map,
                   "Mapping to be differentiated" );

}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsARateMap and astCheckRateMap functions using the
   macros defined for this purpose in the "object.h" header file. */
astMAKE_ISA(RateMap,Mapping)
astMAKE_CHECK(RateMap)

AstRateMap *astRateMap_( void *map_void, int ax1, int ax2, const char *options, int *status, ...) {
/*
*+
*  Name:
*     astRateMap

*  Purpose:
*     Create a RateMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "ratemap.h"
*     AstRateMap *astRateMap( AstMapping *map, int ax1, int ax2, const char *options, int *status, ... )

*  Class Membership:
*     RateMap constructor.

*  Description:
*     This function creates a new RateMap and optionally initialises its
*     attributes.

*  Parameters:
*     map
*        Pointer to the Mapping to differentiate.
*     ax1
*        zero-based index of the "map" output which is to be differentiated.
*     ax2
*        Zero-based index of the "map" input which is to be varied.
*     options
*        Pointer to a null terminated string containing an optional
*        comma-separated list of attribute assignments to be used for
*        initialising the new RateMap. The syntax used is the same as for the
*        astSet method and may include "printf" format specifiers identified
*        by "%" symbols in the normal way.
*     status
*        Pointer to the inherited status variable.
*     ...
*        If the "options" string contains "%" format specifiers, then an
*        optional list of arguments may follow it in order to supply values to
*        be substituted for these specifiers. The rules for supplying these
*        are identical to those for the astSet method (and for the C "printf"
*        function).

*  Returned Value:
*     A pointer to the new RateMap.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-

*  Implementation Notes:
*     - This function implements the basic RateMap constructor which is
*     available via the protected interface to the RateMap class.  A
*     public interface is provided by the astRateMapId_ function.
*     - Because this function has a variable argument list, it is
*     invoked by a macro that evaluates to a function pointer (not a
*     function invocation) and no checking or casting of arguments is
*     performed before the function is invoked. Because of this, the
*     "map" parameter is of type (void *) and is converted and validated
*     within the function itself.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstRateMap *new;              /* Pointer to new RateMap */
   AstMapping *map;              /* Pointer to Mapping structure */
   va_list args;                 /* Variable argument list */

/* Initialise. */
   new = NULL;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return new;

/* Obtain and validate pointers to the Mapping structures provided. */
   map = astCheckMapping( map_void );
   if ( astOK ) {

/* Initialise the RateMap, allocating memory and initialising the
   virtual function table as well if necessary. */
      new = astInitRateMap( NULL, sizeof( AstRateMap ), !class_init, &class_vtab,
                           "RateMap", map, ax1, ax2 );

/* If successful, note that the virtual function table has been
   initialised. */
      if ( astOK ) {
         class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new RateMap's
   attributes. */
         va_start( args, status );
         astVSet( new, options, NULL, args );
         va_end( args );

/* If an error occurred, clean up by deleting the new object. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Return a pointer to the new RateMap. */
   return new;
}

AstRateMap *astRateMapId_( void *map_void, int ax1, int ax2,
                           const char *options, ... ) {
/*
*++
*  Name:
c     astRateMap
f     AST_RATEMAP

*  Purpose:
*     Create a RateMap.

*  Type:
*     Public function.

*  Synopsis:
c     #include "ratemap.h"
c     AstRateMap *astRateMap( AstMapping *map, int ax1, int ax2,
c                             const char *options, ... )
f     RESULT = AST_RATEMAP( MAP, AX1, AX2, OPTIONS, STATUS )

*  Class Membership:
*     RateMap constructor.

*  Description:
*     This function creates a new RateMap and optionally initialises
*     its attributes.
*
*     A RateMap is a Mapping which represents a single element of the
*     Jacobian matrix of another Mapping. The Mapping for which the
*     Jacobian is required is specified when the new RateMap is created,
*     and is referred to as the "encapsulated Mapping" below.
*
*     The number of inputs to a RateMap is the same as the number of inputs
*     to its encapsulated Mapping. The number of outputs from a RateMap
*     is always one. This one output equals the rate of change of a
*     specified output of the encapsulated Mapping with respect to a
*     specified input of the encapsulated Mapping (the input and output
*     to use are specified when the RateMap is created).
*
*     A RateMap which has not been inverted does not define an inverse
*     transformation. If a RateMap has been inverted then it will define
*     an inverse transformation but not a forward transformation.

*  Parameters:
c     map
f     MAP = INTEGER (Given)
*        Pointer to the encapsulated Mapping.
c     ax1
f     AX1 = INTEGER (Given)
*        Index of the output from the encapsulated Mapping for which the
*        rate of change is required. This corresponds to the delta
*        quantity forming the numerator of the required element of the
*        Jacobian matrix. The first axis has index 1.
c     ax2
f     AX2 = INTEGER (Given)
*        Index of the input to the encapsulated Mapping which is to be
*        varied. This corresponds to the delta quantity forming the
*        denominator of the required element of the Jacobian matrix.
*        The first axis has index 1.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new RateMap. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new RateMap. The syntax used is identical to that for the
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
c     astRateMap()
f     AST_RATEMAP = INTEGER
*        A pointer to the new RateMap.

*  Notes:
*     - The forward transformation of the encapsulated Mapping must be
*     defined.
c     - Note that the component Mappings supplied are not copied by
c     astRateMap (the new RateMap simply retains a reference to
c     them). They may continue to be used for other purposes, but
c     should not be deleted. If a RateMap containing a copy of its
c     component Mappings is required, then a copy of the RateMap should
c     be made using astCopy.
f     - Note that the component Mappings supplied are not copied by
f     AST_RATEMAP (the new RateMap simply retains a reference to
f     them). They may continue to be used for other purposes, but
f     should not be deleted. If a RateMap containing a copy of its
f     component Mappings is required, then a copy of the RateMap should
f     be made using AST_COPY.
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--

*  Implementation Notes:
*     - This function implements the external (public) interface to
*     the astRateMap constructor function. It returns an ID value
*     (instead of a true C pointer) to external users, and must be
*     provided because astRateMap_ has a variable argument list which
*     cannot be encapsulated in a macro (where this conversion would
*     otherwise occur).
*     - Because no checking or casting of arguments is performed
*     before the function is invoked, the "map" parameter is of type
*     (void *) and is converted from an ID value to a pointer and
*     validated within the function itself.
*     - The variable argument list also prevents this function from
*     invoking astRateMap_ directly, so it must be a re-implementation
*     of it in all respects, except for the conversions between IDs
*     and pointers on input/output of Objects.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstRateMap *new;              /* Pointer to new RateMap */
   AstMapping *map;              /* Pointer to Mapping structure */
   va_list args;                 /* Variable argument list */

/* Pointer to inherited status value */
   int *status;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialise. */
   new = NULL;

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Check the global status. */
   if ( !astOK ) return new;

/* Obtain the Mapping pointer from the ID supplied and validate the
   pointer to ensure it identifies a valid Mapping. */
   map = astVerifyMapping( astMakePointer( map_void ) );
   if ( astOK ) {

/* Initialise the RateMap, allocating memory and initialising the
   virtual function table as well if necessary. */
      new = astInitRateMap( NULL, sizeof( AstRateMap ), !class_init, &class_vtab,
                           "RateMap", map, ax1 - 1, ax2 - 1 );

/* If successful, note that the virtual function table has been initialised. */
      if ( astOK ) {
         class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new RateMap's
   attributes. */
         va_start( args, options );
         astVSet( new, options, NULL, args );
         va_end( args );

/* If an error occurred, clean up by deleting the new object. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Return an ID value for the new RateMap. */
   return astMakeId( new );
}

AstRateMap *astInitRateMap_( void *mem, size_t size, int init,
                           AstRateMapVtab *vtab, const char *name,
                           AstMapping *map, int ax1, int ax2, int *status ) {
/*
*+
*  Name:
*     astInitRateMap

*  Purpose:
*     Initialise a RateMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "ratemap.h"
*     AstRateMap *astInitRateMap( void *mem, size_t size, int init,
*                                 AstRateMapVtab *vtab, const char *name,
*                                 AstMapping *map, int ax1, int ax2 )

*  Class Membership:
*     RateMap initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new RateMap object. It allocates memory (if necessary) to
*     accommodate the RateMap plus any additional data associated with the
*     derived class. It then initialises a RateMap structure at the start
*     of this memory. If the "init" flag is set, it also initialises the
*     contents of a virtual function table for a RateMap at the start of
*     the memory passed via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the RateMap is to be initialised.
*        This must be of sufficient size to accommodate the RateMap data
*        (sizeof(RateMap)) plus any data used by the derived class. If a
*        value of NULL is given, this function will allocate the memory itself
*        using the "size" parameter to determine its size.
*     size
*        The amount of memory used by the RateMap (plus derived class
*        data). This will be used to allocate memory if a value of NULL is
*        given for the "mem" parameter. This value is also stored in the
*        RateMap structure, so a valid value must be supplied even if not
*        required for allocating memory.
*     init
*        A logical flag indicating if the RateMap's virtual function table
*        is to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new RateMap.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the Object
*        astClass function).
*     map
*        Pointer to the Mapping.
*     ax1
*        Zero-based index of output axis.
*     ax2
*        Zero-based index of input axis.

*  Returned Value:
*     A pointer to the new RateMap.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstRateMap *new;              /* Pointer to new RateMap */
   int nin;                      /* No. input coordinates for RateMap */
   int nout;                     /* No. output coordinates for RateMap */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitRateMapVtab( vtab, name );

/* Initialise. */
   new = NULL;

/* Report an error if "map" has no forward transformation. */
   if( !astGetTranForward( map ) && astOK ) {
      astError( AST__INTRD, "astInitRateMap(%s): The supplied Mapping "
              "is not able to transform coordinates in the forward direction.", status,
              name );
   }

/* Check that the input and output axis indices are valid. */
   nin = astGetNin( map );
   nout = astGetNout( map );
   if( ( ax1 < 0 || ax1 >= nout ) && astOK ) {
      astError( AST__INNCO, "astInitRateMap(%s): The output axis %d is out "
                "of range - it should be in the range 1 to %d.", status, name,
                ax1 + 1, nout );
   }
   if( ( ax2 < 0 || ax2 >= nin ) && astOK ) {
      astError( AST__INNCO, "astInitRateMap(%s): The input axis %d is out "
                "of range - it should be in the range 1 to %d.", status, name,
                ax2 + 1, nin );
   }

/* Initialise a Mapping structure (the parent class) as the first component
   within the RateMap structure, allocating memory if necessary. Specify
   the number of input and output coordinates and in which directions the
   Mapping should be defined. */
   if ( astOK ) {
      new = (AstRateMap *) astInitMapping( mem, size, 0,
                                          (AstMappingVtab *) vtab, name,
                                          nin, 1, 1, 0 );

      if ( astOK ) {

/* Initialise the RateMap data. */
/* --------------------------- */
/* Store a pointer to the encapsulated Mapping. */
         new->map = astClone( map );

/* Save the initial values of the inversion flag for this Mapping. */
         new->invert = astGetInvert( map );

/* Save the input and output axis indices. */
         new->iout = ax1;
         new->iin = ax2;

/* If an error occurred, clean up by annulling the Mapping pointers and
   deleting the new object. */
         if ( !astOK ) {
            new->map = astAnnul( new->map );
            new = astDelete( new );
         }
      }
   }

/* Return a pointer to the new object. */
   return new;
}

AstRateMap *astLoadRateMap_( void *mem, size_t size,
                             AstRateMapVtab *vtab, const char *name,
                             AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadRateMap

*  Purpose:
*     Load a RateMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "ratemap.h"
*     AstRateMap *astLoadRateMap( void *mem, size_t size,
*                                 AstRateMapVtab *vtab, const char *name,
*                                 AstChannel *channel )

*  Class Membership:
*     RateMap loader.

*  Description:
*     This function is provided to load a new RateMap using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     RateMap structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a RateMap at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the RateMap is to be
*        loaded.  This must be of sufficient size to accommodate the
*        RateMap data (sizeof(RateMap)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the RateMap (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the RateMap structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstRateMap) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new RateMap. If this is NULL, a pointer to
*        the (static) virtual function table for the RateMap class is
*        used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "RateMap" is used instead.

*  Returned Value:
*     A pointer to the new RateMap.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstRateMap *new;               /* Pointer to the new RateMap */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this RateMap. In this case the
   RateMap belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstRateMap );
      vtab = &class_vtab;
      name = "RateMap";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitRateMapVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built RateMap. */
   new = astLoadMapping( mem, size, (AstMappingVtab *) vtab, name,
                         channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "RateMap" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* Invert flag. */
/* ------------ */
      new->invert = astReadInt( channel, "inv", 0 );
      new->invert = ( new->invert != 0 );

/* Input and output axes. */
/* ---------------------- */
      new->iin = astReadInt( channel, "iin", 0 );
      new->iout = astReadInt( channel, "iout", 0 );

/* Mapping. */
/* -------- */
      new->map = astReadObject( channel, "map", NULL );

/* If an error occurred, clean up by deleting the new RateMap. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new RateMap pointer. */
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




