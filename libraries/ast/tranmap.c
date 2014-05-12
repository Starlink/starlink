/*
*class++
*  Name:
*     TranMap

*  Purpose:
*     Mapping with specified forward and inverse transformations.

*  Constructor Function:
c     astTranMap
f     AST_TRANMAP

*  Description:
*     A TranMap is a Mapping which combines the forward transformation of
*     a supplied Mapping with the inverse transformation of another
*     supplied Mapping, ignoring the un-used transformation in each
*     Mapping (indeed the un-used transformation need not exist).
*
*     When the forward transformation of the TranMap is referred to, the
*     transformation actually used is the forward transformation of the
*     first Mapping supplied when the TranMap was constructed. Likewise,
*     when the inverse transformation of the TranMap is referred to, the
*     transformation actually used is the inverse transformation of the
*     second Mapping supplied when the TranMap was constructed.

*  Inheritance:
*     The TranMap class inherits from the Mapping class.

*  Attributes:
*     The TranMap class does not define any new attributes beyond those
*     which are applicable to all Mappings.

*  Functions:
c     The TranMap class does not define any new functions beyond those
f     The TranMap class does not define any new routines beyond those
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
*     19-JAN-2005 (DSB):
*        Fix memory leak.
*     14-FEB-2006 (DSB):
*        - Over-ride the astDecompose method.
*        - Fix bug in MapSplit related to use of invert flags.
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
#define astCLASS TranMap

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
#include "permmap.h"             /* Coordinate permutation Mappings */
#include "cmpmap.h"              /* Compound Mappings */
#include "unitmap.h"             /* Unit Mappings */
#include "tranmap.h"             /* Interface definition for this class */
#include "frame.h"               /* Frames */

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
static int *(* parent_mapsplit)( AstMapping *, int, const int *, AstMapping **, int * );

#if defined(THREAD_SAFE)
static int (* parent_managelock)( AstObject *, int, int, AstObject **, int * );
#endif



#ifdef THREAD_SAFE
/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(TranMap)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(TranMap,Class_Init)
#define class_vtab astGLOBAL(TranMap,Class_Vtab)


#include <pthread.h>


#else


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstTranMapVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstTranMap *astTranMapId_( void *, void *, const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstMapping *RemoveRegions( AstMapping *, int * );
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static double Rate( AstMapping *, double *, int, int, int * );
static int *MapSplit( AstMapping *, int, const int *, AstMapping **, int * );
static int Equal( AstObject *, AstObject *, int * );
static int MapMerge( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void Decompose( AstMapping *, AstMapping **, AstMapping **, int *, int *, int *, int * );
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
*     Test if two TranMaps are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "tranmap.h"
*     int Equal( AstObject *this, AstObject *that, int *status )

*  Class Membership:
*     TranMap member function (over-rides the astEqual protected
*     method inherited from the astMapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two TranMaps are equivalent.

*  Parameters:
*     this
*        Pointer to the first Object (a TranMap).
*     that
*        Pointer to the second Object.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the TranMaps are equivalent, zero otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstTranMap *that;
   AstTranMap *this;
   int nin;
   int nout;
   int result;
   int that_inv1;
   int that_inv2;
   int this_inv1;
   int this_inv2;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain pointers to the two TranMap structures. */
   this = (AstTranMap *) this_object;
   that = (AstTranMap *) that_object;

/* Check the second object is a TranMap. We know the first is a
   TranMap since we have arrived at this implementation of the virtual
   function. */
   if( astIsATranMap( that ) ) {

/* Get the number of inputs and outputs and check they are the same for both. */
      nin = astGetNin( this );
      nout = astGetNout( this );
      if( astGetNin( that ) == nin && astGetNout( that ) == nout ) {

/* Temporarily re-instate the original Invert flag values. */
         that_inv1 = astGetInvert( that->map1 );
         that_inv2 = astGetInvert( that->map2 );
         this_inv1 = astGetInvert( this->map1 );
         this_inv2 = astGetInvert( this->map2 );

         astSetInvert( this->map1, this->invert1 );
         astSetInvert( this->map2, this->invert2 );
         astSetInvert( that->map1, that->invert1 );
         astSetInvert( that->map2, that->invert2 );

/* If the Invert flags for the two TranMaps differ, it may still be possible
   for them to be equivalent. First compare the TranMaps if their Invert
   flags are the same. In this case all the attributes of the two TranMaps
   must be identical. */
         if( astGetInvert( this ) == astGetInvert( that ) ) {
            if( astEqual( this->map1, that->map1 ) &&
                astEqual( this->map2, that->map2 ) ) {
               result = 1;
            }

/* If the Invert flags for the two TranMaps differ, the attributes of the two
   TranMaps must be inversely related to each other. */
         } else {

            astInvert( that->map1 );
            astInvert( that->map2 );

            if( astEqual( this->map1, that->map2 ) &&
                astEqual( this->map2, that->map1 ) ) {
               result = 1;
            }

         }

/* Restore the original Invert flag values. */
         astSetInvert( this->map1, this_inv1 );
         astSetInvert( this->map2, this_inv2 );
         astSetInvert( that->map1, that_inv1 );
         astSetInvert( that->map2, that_inv2 );
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
*     #include "tranmap.h"
*     int GetObjSize( AstObject *this, int *status )

*  Class Membership:
*     TranMap member function (over-rides the astGetObjSize protected
*     method inherited from the parent class).

*  Description:
*     This function returns the in-memory size of the supplied TranMap,
*     in bytes.

*  Parameters:
*     this
*        Pointer to the TranMap.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The Object size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstTranMap *this;         /* Pointer to TranMap structure */
   int result;                /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointers to the TranMap structure. */
   this = (AstTranMap *) this_object;

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
*     #include "tranmap.h"
*     void Decompose( AstMapping *this, AstMapping **map1,
*                     AstMapping **map2, int *series,
*                     int *invert1, int *invert2, int *status )

*  Class Membership:
*     TranMap member function (over-rides the protected astDecompose
*     method inherited from the Mapping class).

*  Description:
*     This function returns pointers to the two Mappings encapsulated by
*     a TranMap.

*  Parameters:
*     this
*        Pointer to the Mapping.
*     map1
*        Address of a location to receive a pointer to first component
*        Mapping (the forward Mapping).
*     map2
*        Address of a location to receive a pointer to second component
*        Mapping (the inverse Mapping).
*     series
*        Address of a location to receive a value indicating if the
*        component Mappings are applied in series or parallel. A non-zero
*        value means that the supplied Mapping is equivalent to applying map1
*        followed by map2 in series. A zero value means that the supplied
*        Mapping is equivalent to applying map1 to the lower numbered axes
*        and map2 to the higher numbered axes, in parallel. Zero is
*        returned for a TranMap.
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
   AstTranMap *this;             /* Pointer to TranMap structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the TranMap structure. */
   this = (AstTranMap *) this_mapping;

/* If the TranMap has been inverted, return the Mappings in reverse
   order with inverted Invert falgs. */
   if( astGetInvert( this ) ) {
      if( map1 ) *map1 = astClone( this->map2 );
      if( map2 ) *map2 = astClone( this->map1 );
      if( invert1 ) *invert1 = this->invert2 ? 0 : 1;
      if( invert2 ) *invert2 = this->invert1 ? 0 : 1;

/* If the TranMap has not been inverted, return the Mappings in their
   original order with their original Invert flags. */
   } else {
      if( map1 ) *map1 = astClone( this->map1 );
      if( map2 ) *map2 = astClone( this->map2 );
      if( invert1 ) *invert1 = this->invert1;
      if( invert2 ) *invert2 = this->invert2;
   }
}

void astInitTranMapVtab_(  AstTranMapVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitTranMapVtab

*  Purpose:
*     Initialise a virtual function table for a TranMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "tranmap.h"
*     void astInitTranMapVtab( AstTranMapVtab *vtab, const char *name )

*  Class Membership:
*     TranMap vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the TranMap class.

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
   will be used (by astIsATranMap) to determine if an object belongs to
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
   mapping->Decompose = Decompose;
   mapping->MapMerge = MapMerge;
   mapping->Rate = Rate;

/* Declare the copy constructor, destructor and class dump function. */
   astSetCopy( vtab, Copy );
   astSetDelete( vtab, Delete );
   astSetDump( vtab, Dump, "TranMap", "Compound Transformation Mapping" );

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
*     TranMap member function (over-rides the astManageLock protected
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
   AstTranMap *this;       /* Pointer to TranMap structure */
   int result;             /* Returned status value */

/* Initialise */
   result = 0;

/* Check the supplied pointer is not NULL. */
   if( !this_object ) return result;

/* Obtain a pointers to the TranMap structure. */
   this = (AstTranMap *) this_object;

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

static int MapMerge( AstMapping *this, int where, int series, int *nmap,
                     AstMapping ***map_list, int **invert_list, int *status ) {
/*
*  Name:
*     MapMerge

*  Purpose:
*     Simplify a sequence of Mappings containing a TranMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     int MapMerge( AstMapping *this, int where, int series, int *nmap,
*                   AstMapping ***map_list, int **invert_list, int *status )

*  Class Membership:
*     TranMap method (over-rides the protected astMapMerge method
*     inherited from the Mapping class).

*  Description:
*     This function attempts to simplify a sequence of Mappings by
*     merging a nominated TranMap in the sequence with its neighbours,
*     so as to shorten the sequence if possible.
*
*     In many cases, simplification will not be possible and the
*     function will return -1 to indicate this, without further
*     action.
*
*     In most cases of interest, however, this function will either
*     attempt to replace the nominated TranMap with one which it
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
*        Pointer to the nominated TranMap which is to be merged with
*        its neighbours. This should be a cloned copy of the TranMap
*        pointer contained in the array element "(*map_list)[where]"
*        (see below). This pointer will not be annulled, and the
*        TranMap it identifies will not be modified by this function.
*     where
*        Index in the "*map_list" array (below) at which the pointer
*        to the nominated TranMap resides.
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
   AstCmpMap *cmap;              /* Pointer to compound Mapping */
   AstMapping *cmap_f;           /* Pointer to compound Mapping */
   AstMapping *cmap_i;           /* Pointer to compound Mapping */
   AstMapping *hmap1;            /* Pointer to 1st comp of higher TranMap */
   AstMapping *hmap2;            /* Pointer to 2nd comp of higher TranMap */
   AstMapping *hmap_f;           /* Pointer to fwd Mapping of higher TranMap */
   AstMapping *hmap_i;           /* Pointer to inv Mapping of higher TranMap */
   AstMapping *map1;             /* Pointer to 1st comp of nominated TranMap */
   AstMapping *map2;             /* Pointer to 2nd comp of nominated TranMap */
   AstMapping *map_f;            /* Pointer to fwd Mapping of nominated TranMap */
   AstMapping *map_i;            /* Pointer to inv Mapping of nominated TranMap */
   AstMapping *smap;             /* Pointer to simplified Mapping */
   AstMapping *smap_f;           /* Pointer to simplified Mapping */
   AstMapping *smap_i;           /* Pointer to simplified Mapping */
   AstTranMap *hmap;             /* Pointer to higher TranMap */
   AstTranMap *map;              /* Pointer to this TranMap */
   AstTranMap *new;              /* Pointer to merged TranMap */
   int i;                        /* Loop count */
   int old_hinv1;                /* Original Invert flag for hmap->map1 */
   int old_hinv2;                /* Original Invert flag for hmap->map2 */
   int old_inv1;                 /* Original Invert flag for this->map1 */
   int old_inv2;                 /* Original Invert flag for this->map2 */
   int result;                   /* The value to return */

/* Initialise.*/
   result = -1;

/* Check the inherited status. */
   if ( !astOK ) return result;

/* Get a pointer to this TranMap. */
   map = (AstTranMap *) this;

/* Get the two component Mappings,and temporarily set their Invert
   attributes back to the values they had when the TranMap was created,
   saving their current Invert values so that they can be re-instated later. */
   map1 = map->map1;
   old_inv1 = astGetInvert( map1 );
   astSetInvert( map1, map->invert1 );

   map2 = map->map2;
   old_inv2 = astGetInvert( map2 );
   astSetInvert( map2, map->invert2 );

/* Simplify the TranMap on its own. */
/* ================================ */

/* If the TranMap is inverted, creat an equal TranMap which is not inverted.
   To do this, invert and swap the component Mappings. */
   if( ( *invert_list )[ where ] ) {
      astInvert( map1 );
      astInvert( map2 );
      new = astTranMap( map2, map1, "", status );
      astInvert( map1 );
      astInvert( map2 );

      (void) astAnnul( ( *map_list )[ where ] );
      ( *map_list )[ where ] = (AstMapping *) new;
      ( *invert_list )[ where ] = 0;
      result = where;

/* Otherwise, try to simplify each of the component Mappings. */
   } else {
      smap_f = astSimplify( map1 );
      smap_i = astSimplify( map2 );

/* Assume some simplification took place if the pointers have changed. */
      if( smap_f != map1 || smap_i != map2 ) {

/* Construct a new TranMap from these simplifgied Mappings. */
         (void) astAnnul( ( *map_list )[ where ] );
         ( *map_list )[ where ] = (AstMapping *) astTranMap( smap_f, smap_i, "", status );
         result = where;

/* Otherwise, if the both component Mappings are defined in both directions... */
      } else if( astGetTranForward( map1 ) && astGetTranInverse( map1 ) &&
                 astGetTranForward( map2 ) && astGetTranInverse( map2 ) ) {

/* Form a series CmpMap from the two component Mappings, with the second
   Mapping inverted. */
         astInvert( map2 );
         cmap = astCmpMap( map1, map2, 1, "", status );
         astInvert( map2 );

/* If this CmpMap simplifies to a UnitMap, then the two components of the
   TranMap are equal, and so we can replace the entire TranMap with either
   of its components. Note, we leave the supplied invert flag unchanged,
   since the copycreated below refers to the Mapping as it was when the
   TranMap was created. However, we invert the returned Mapping if
   necessary. */
         smap = astSimplify( cmap );
         if( astIsAUnitMap( smap ) ) {
            (void) astAnnul( ( *map_list )[ where ] );
            ( *map_list )[ where ] = astCopy( map1 );
            if( ( *invert_list )[ where ] ) astInvert( ( *map_list )[ where ] );
            result = where;
         }

/* Release resources. */
         smap = astAnnul( smap );
         cmap = astAnnul( cmap );
      }

/* Release resources. */
      smap_f = astAnnul( smap_f );
      smap_i = astAnnul( smap_i );
   }

/* Merge the TranMap with a neighbouring TranMap. */
/* ============================================== */
/* Only do this if no change was made above, and we are combining the
   Mappings in series. */
   if( result == -1 && series ) {

/* Is the higher neighbour a TranMap? */
      if( where < ( *nmap - 1 ) &&
          astIsATranMap( ( *map_list )[ where + 1 ] ) ){

/* Get the two component Mappings of the higher TranMap, and temporarily set
   their Invert attributes back to the values they had when the TranMap was
   created, saving their current Invert values so that they can be re-instated
   later. */
         hmap = (AstTranMap *) ( *map_list )[ where + 1 ];

         hmap1 = hmap->map1;
         old_hinv1 = astGetInvert( hmap1 );
         astSetInvert( hmap1, hmap->invert1 );

         hmap2 = hmap->map2;
         old_hinv2 = astGetInvert( hmap2 );
         astSetInvert( hmap2, hmap->invert2 );

/* Get the Mappings which defines the forward and inverse transformation of
   the lower TranMap ("this"). Then, map_f and map_i are pointers to
   Mappings which could be used to construct a new TranMap which would be
   equivalent to "this" with the supplied invert setting. */
         if( ( *invert_list )[ where ] ) {
            map_f = map2;
            map_i = map1;
            astInvert( map_f );
            astInvert( map_i );
         } else {
            map_f = map1;
            map_i = map2;
         }

/* Likewise, get the Mappings which defines the forward and inverse
   transformation of the higher TranMap. */
         if( ( *invert_list )[ where + 1 ] ) {
            hmap_f = hmap2;
            hmap_i = hmap1;
            astInvert( hmap_f );
            astInvert( hmap_i );
         } else {
            hmap_f = hmap1;
            hmap_i = hmap2;
         }

/* Combine the two forward Mappings together into a series CmpMap, and
   simplify it. */
         cmap_f = (AstMapping *) astCmpMap( map_f,  hmap_f, 1, "", status );
         smap_f = astSimplify( cmap_f );

/* Do the same for the inverse Mappings */
         cmap_i = (AstMapping *) astCmpMap( map_i,  hmap_i, 1, "", status );
         smap_i = astSimplify( cmap_i );

/* Was any simplification performed? We assume this is the case if the
   either of the simplied pointer differs from the original pointer. */
         if( cmap_f != smap_f || cmap_i != smap_i ) {

/* In which case,construct a new TranMap from the simplified Mappings. */
            new = astTranMap( smap_f, smap_i, "", status );

         } else {
            new = NULL;
         }

/* Free resources.*/
         cmap_f = astAnnul( cmap_f );
         smap_f = astAnnul( smap_f );
         cmap_i = astAnnul( cmap_i );
         smap_i = astAnnul( smap_i );

/* Re-instate the original Invert values for the component Mappings of
   the higher TranMap. */
         astSetInvert( hmap1, old_hinv1 );
         astSetInvert( hmap2, old_hinv2 );

/* If we have a new TranMap, annul the first of the two Mappings, and replace
   it with the merged TranMap. Also set the invert flag. */
         if( new ) {
            (void) astAnnul( ( *map_list )[ where ] );
            ( *map_list )[ where ] = (AstMapping *) new;
            ( *invert_list )[ where ] = 0;

/* Annul the second of the two Mappings, and shuffle down the rest of the
   list to fill the gap. */
            (void) astAnnul( ( *map_list )[ where + 1 ] );
            for ( i = where + 2; i < *nmap; i++ ) {
               ( *map_list )[ i - 1 ] = ( *map_list )[ i ];
               ( *invert_list )[ i - 1 ] = ( *invert_list )[ i ];
            }

/* Clear the vacated element at the end. */
            ( *map_list )[ *nmap - 1 ] = NULL;
            ( *invert_list )[ *nmap - 1 ] = 0;

/* Decrement the Mapping count and return the index of the first
   modified element. */
            ( *nmap )--;
            result = where;
         }
      }
   }

/* Re-instate the original Invert values for the component Mappings. */
   astSetInvert( map1, old_inv1 );
   astSetInvert( map2, old_inv2 );

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
*     TranMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "tranmap.h"
*     int *MapSplit( AstMapping *this, int nin, const int *in, AstMapping **map, int *status )

*  Class Membership:
*     TranMap method (over-rides the protected astMapSplit method
*     inherited from the Mapping class).

*  Description:
*     This function creates a new Mapping by picking specified inputs from
*     an existing TranMap. This is only possible if the specified inputs
*     correspond to some subset of the TranMap outputs. That is, there
*     must exist a subset of the TranMap outputs for which each output
*     depends only on the selected TranMap inputs, and not on any of the
*     inputs which have not been selected. If this condition is not met
*     by the supplied TranMap, then a NULL Mapping is returned.

*  Parameters:
*     this
*        Pointer to the TranMap to be split (the TranMap is not actually
*        modified by this function).
*     nin
*        The number of inputs to pick from "this".
*     in
*        Pointer to an array of indices (zero based) for the inputs which
*        are to be picked. This array should have "nin" elements. If "Nin"
*        is the number of inputs of the supplied TranMap, then each element
*        should have a value in the range zero to Nin-1.
*     map
*        Address of a location at which to return a pointer to the new
*        Mapping. This Mapping will have "nin" inputs (the number of
*        outputs may be different to "nin"). A NULL pointer will be
*        returned if the supplied TranMap has no subset of outputs which
*        depend only on the selected inputs.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a dynamically allocated array of ints. The number of
*     elements in this array will equal the number of outputs for the
*     returned Mapping. Each element will hold the index of the
*     corresponding output in the supplied TranMap. The array should be
*     freed using astFree when no longer needed. A NULL pointer will
*     be returned if no output Mapping can be created.

*  Notes:
*     - If this function is invoked with the global error status set,
*     or if it should fail for any reason, then NULL values will be
*     returned as the function value and for the "map" pointer.
*/

/* Local Variables: */
   AstMapping *fmap;          /* Pointer to forward Mapping in supplied TranMap */
   AstMapping *imap;          /* Pointer to inverse Mapping in supplied TranMap */
   AstMapping *rfmap;         /* Pointer to split forward Mapping */
   AstMapping *rimap;         /* Pointer to split inverse Mapping */
   AstTranMap *this;          /* Pointer to TranMap structure */
   int *ires;                 /* I/ps of inv Mapping dependent on selected o/ps */
   int *out;                  /* O/ps of fwd Mapping dependent on selected i/ps */
   int *result;               /* Pointer to returned array */
   int finv;                  /* Invert flag to use with fmap */
   int i;                     /* Loop count */
   int iinv;                  /* Invert flag to use with imap */
   int nout;                  /* No. of outputs dependent on selected inputs */
   int ok;                    /* Can required Mapping be created? */
   int old_finv;              /* Original Invert flag for fmap */
   int old_iinv;              /* Original Invert flag for imap */

/* Initialise */
   result = NULL;
   *map = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Invoke the parent astMapSplit method to see if it can do the job. */
   result = (*parent_mapsplit)( this_map, nin, in, map, status );

/* If not, we provide a special implementation here. */
   if( !result ) {

/* Get a pointer to the TranMap structure. */
      this = (AstTranMap *) this_map;

/* Get pointers to the forward and inverse Mappings, taking into account
   whether the TranMap has been inverted. */
      if( !astGetInvert( this ) ) {
         fmap = this->map1;
         finv = this->invert1;
         imap = this->map2;
         iinv = this->invert2;
      } else {
         imap = this->map1;
         iinv = !( this->invert1 );
         fmap = this->map2;
         finv = !( this->invert2 );
      }

/* Temporarily set the Invert flag of both Mappings back to their
   original values. */
      old_finv = astGetInvert( fmap );
      astSetInvert( fmap, finv );
      old_iinv = astGetInvert( imap );
      astSetInvert( imap, iinv );

/* Try to split the forward Mapping. */
      out = astMapSplit( fmap, nin, in, &rfmap );

/* Check the split could be done. */
      if( out ) {

/* Get the number of outputs which are fed by the selected inputs. */
         nout = astGetNout( rfmap );

/* See if the inverse Mapping can be split using these outputs as inputs. */
         astInvert( imap );
         ires = astMapSplit( imap, nout, out, &rimap );
         astInvert( imap );
         if( ires ) {
            astInvert( rimap );

/* Check that the resulting inputs are the same as the supplied inputs. */
            if( astGetNin( rimap ) == nin ) {
               ok = 1;
               for( i = 0; i < nin; i++ ) {
                  if( in[ i ] != ires[ i ] ) {
                     ok = 0;
                     break;
                  }
               }

/* If so create the required new TranMap. */
               if( ok ) {
                  *map = (AstMapping *) astTranMap( rfmap, rimap, "", status );
                  result = out;
               }
            }

/* Free resources. */
            ires = astFree( ires );
            rimap = astAnnul( rimap );
         }

         if( !result ) out = astFree( out );
         rfmap = astAnnul( rfmap );
      }

/* Re-instate the Invert flags of the component Mappings. */
      astSetInvert( fmap, old_finv );
      astSetInvert( imap, old_iinv );
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
*     #include "tranmap.h"
*     result = Rate( AstMapping *this, double *at, int ax1, int ax2, int *status )

*  Class Membership:
*     TranMap member function (overrides the astRate method inherited
*     from the Mapping class ).

*  Description:
*     This function returns the rate of change of a specified output of
*     the supplied Mapping with respect to a specified input, at a
*     specified input position. Also evaluates the second derivative.

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
   AstTranMap *map;
   AstMapping *cmap;
   double result;
   int cinv;
   int old_inv;

/* Check inherited status */
   if( !astOK ) return AST__BAD;

/* Get a pointer to the TranMap structure. */
   map = (AstTranMap *) this;

/* Choose the component Mapping to use, and get its original Invert
   value. Invert this if the TranMap itself has been inverted (this is
   because the astRate function has no "invert" argument so we need to
   invert the Mapping before calling astRate). */
   if( astGetInvert( this ) ) {
      cmap = map->map2;
      cinv = !(map->invert2);
   } else {
      cmap = map->map1;
      cinv = map->invert1;
   }

/* Temporarily set the Invert flag of the component Mapping back to its
   original value. */
   old_inv = astGetInvert( cmap );
   astSetInvert( cmap, cinv );

/* Use the astRate method of the component Mapping. */
   result = astRate( cmap, at, ax1, ax2 );

/* Re-instate the Invert flag of the component Mapping. */
   astSetInvert( cmap, old_inv );

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
*     #include "tranmap.h"
*     AstMapping *RemoveRegions( AstMapping *this, int *status )

*  Class Membership:
*     TranMap method (over-rides the astRemoveRegions method inherited
*     from the Mapping class).

*  Description:
*     This function searches the supplied Mapping (which may be a
*     compound Mapping such as a TranMap) for any component Mappings
*     that are instances of the AST Region class. It then creates a new
*     Mapping from which all Regions have been removed. If a Region
*     cannot simply be removed (for instance, if it is a component of a
*     parallel TranMap), then it is replaced with an equivalent UnitMap
*     in the returned Mapping.
*
*     The implementation provided by the TranMap class invokes the
*     astRemoveRegions method on the two component Mappings, and joins
*     the results together into a new TranMap.

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
   AstTranMap *new;              /* Pointer to new TranMap */
   AstTranMap *this;             /* Pointer to TranMap structure */
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

/* Get a pointer to the TranMap. */
   this = (AstTranMap *) this_mapping;

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

/* If both new Mappings are UnitMaps, return an equivalent UnitMap. */
      if( unit1 && unit2 ) {
         result = (AstMapping *) astUnitMap( astGetNin( newmap1 ) +
                                             astGetNin( newmap2 ), " ",
                                             status );

/* Otherwise, return a new TranMap containing the two new Mappings. */
      } else {
         new = astCopy( this );
         (void) astAnnul( new->map1 );
         (void) astAnnul( new->map2 );
         new->map1 = astClone( newmap1 );
         new->map2 = astClone( newmap2 );
         result = (AstMapping *) new;
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

static AstPointSet *Transform( AstMapping *this, AstPointSet *in,
                               int forward, AstPointSet *out, int *status ) {
/*
*  Name:
*     Transform

*  Purpose:
*     Apply a TranMap to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "tranmap.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     TranMap member function (over-rides the astTransform method inherited
*     from the Mapping class).

*  Description:
*     This function takes a TranMap and a set of points encapsulated in a
*     PointSet and transforms the points so as to apply the required Mapping.
*     This implies applying each of the TranMap's component Mappings in turn,
*     either in series or in parallel.

*  Parameters:
*     this
*        Pointer to the TranMap.
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
*     match the number of coordinates for the TranMap being applied.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstMapping *cmap;             /* Mapping which defines the required transformation */
   AstPointSet *result;          /* Pointer to output PointSet */
   AstTranMap *map;              /* Pointer to TranMap to be applied */
   int cinv;                     /* Invert flag when TranMap was created */
   int old_inv;                  /* Invert flag on entry to this function */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the TranMap. */
   map = (AstTranMap *) this;

/* Apply the parent Mapping using the stored pointer to the Transform member
   function inherited from the parent Mapping class. This function validates
   all arguments and generates an output PointSet if necessary, but does not
   actually transform any coordinate values. */
   result = (*parent_transform)( this, in, forward, out, status );

/* We now extend the parent astTransform method by applying the component
   Mappings of the TranMap to generate the output coordinate values. */

/* Determine whether to apply the forward or inverse Mapping, according to the
   direction specified and whether the Mapping has been inverted. */
   if ( astGetInvert( map ) ) forward = !forward;

/* Choose the component Mapping to use, and get its original Invert value. */
   if( forward ) {
      cmap = map->map1;
      cinv = map->invert1;
   }else {
      cmap = map->map2;
      cinv = map->invert2;
   }

/* Temporarily set the Invert flag of the component Mapping back to its
   original value. */
   old_inv = astGetInvert( cmap );
   astSetInvert( cmap, cinv );

/* Use the Transform method of the component Mapping. */
   result = astTransform( cmap, in, forward, out );

/* Re-instate the Invert flag of the component Mapping. */
   astSetInvert( cmap, old_inv );

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
*     Copy constructor for TranMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for TranMap objects.

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
*     Mappings within the TranMap.
*/

/* Local Variables: */
   AstTranMap *in;                /* Pointer to input TranMap */
   AstTranMap *out;               /* Pointer to output TranMap */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output TranMaps. */
   in = (AstTranMap *) objin;
   out = (AstTranMap *) objout;

/* For safety, start by clearing any references to the input component
   Mappings from the output TranMap. */
   out->map1 = NULL;
   out->map2 = NULL;

/* Make copies of these Mappings and store pointers to them in the output
   TranMap structure. */
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
*     Destructor for TranMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for TranMap objects.

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
   AstTranMap *this;              /* Pointer to TranMap */

/* Obtain a pointer to the TranMap structure. */
   this = (AstTranMap *) obj;

/* Annul the pointers to the component Mappings. */
   this->map1 = astAnnul( this->map1 );
   this->map2 = astAnnul( this->map2 );

/* Clear the remaining TranMap variables. */
   this->invert1 = 0;
   this->invert2 = 0;
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for TranMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the TranMap class to an output Channel.

*  Parameters:
*     this
*        Pointer to the TranMap whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstTranMap *this;              /* Pointer to the TranMap structure */
   int ival;                     /* Integer value */
   int set;                      /* Attribute value set? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the TranMap structure. */
   this = (AstTranMap *) this_object;

/* Write out values representing the instance variables for the TranMap
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
                   "Mapping for forward transformation" );

/* Second Mapping. */
/* --------------- */
   astWriteObject( channel, "MapB", 1, 1, this->map2,
                   "Mapping for inverse transformation" );
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsATranMap and astCheckTranMap functions using the
   macros defined for this purpose in the "object.h" header file. */
astMAKE_ISA(TranMap,Mapping)
astMAKE_CHECK(TranMap)

AstTranMap *astTranMap_( void *map1_void, void *map2_void, const char *options, int *status, ...) {
/*
*+
*  Name:
*     astTranMap

*  Purpose:
*     Create a TranMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "tranmap.h"
*     AstTranMap *astTranMap( AstMapping *map1, AstMapping *map2, const char *options, int *status, ... )

*  Class Membership:
*     TranMap constructor.

*  Description:
*     This function creates a new TranMap and optionally initialises its
*     attributes.

*  Parameters:
*     map1
*        Pointer to the first Mapping (which deinfes the forward
*        transformation).
*     map2
*        Pointer to the second Mapping (which deinfes the inverse
*        transformation).
*     options
*        Pointer to a null terminated string containing an optional
*        comma-separated list of attribute assignments to be used for
*        initialising the new TranMap. The syntax used is the same as for the
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
*     A pointer to the new TranMap.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-

*  Implementation Notes:
*     - This function implements the basic TranMap constructor which is
*     available via the protected interface to the TranMap class.  A
*     public interface is provided by the astTranMapId_ function.
*     - Because this function has a variable argument list, it is
*     invoked by a macro that evaluates to a function pointer (not a
*     function invocation) and no checking or casting of arguments is
*     performed before the function is invoked. Because of this, the
*     "map1" and "map2" parameters are of type (void *) and are
*     converted and validated within the function itself.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstTranMap *new;              /* Pointer to new TranMap */
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

/* Initialise the TranMap, allocating memory and initialising the
   virtual function table as well if necessary. */
      new = astInitTranMap( NULL, sizeof( AstTranMap ), !class_init, &class_vtab,
                           "TranMap", map1, map2 );

/* If successful, note that the virtual function table has been
   initialised. */
      if ( astOK ) {
         class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new TranMap's
   attributes. */
         va_start( args, status );
         astVSet( new, options, NULL, args );
         va_end( args );

/* If an error occurred, clean up by deleting the new object. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Return a pointer to the new TranMap. */
   return new;
}

AstTranMap *astTranMapId_( void *map1_void, void *map2_void,
                           const char *options, ... ) {
/*
*++
*  Name:
c     astTranMap
f     AST_TRANMAP

*  Purpose:
*     Create a TranMap.

*  Type:
*     Public function.

*  Synopsis:
c     #include "tranmap.h"
c     AstTranMap *astTranMap( AstMapping *map1, AstMapping *map2,
c                           const char *options, ... )
f     RESULT = AST_TRANMAP( MAP1, MAP2, OPTIONS, STATUS )

*  Class Membership:
*     TranMap constructor.

*  Description:
*     This function creates a new TranMap and optionally initialises
*     its attributes.
*
*     A TranMap is a Mapping which combines the forward transformation of
*     a supplied Mapping with the inverse transformation of another
*     supplied Mapping, ignoring the un-used transformation in each
*     Mapping (indeed the un-used transformation need not exist).
*
*     When the forward transformation of the TranMap is referred to, the
*     transformation actually used is the forward transformation of the
*     first Mapping supplied when the TranMap was constructed. Likewise,
*     when the inverse transformation of the TranMap is referred to, the
*     transformation actually used is the inverse transformation of the
*     second Mapping supplied when the TranMap was constructed.

*  Parameters:
c     map1
f     MAP1 = INTEGER (Given)
*        Pointer to the first component Mapping, which defines the
*        forward transformation.
c     map2
f     MAP2 = INTEGER (Given)
*        Pointer to the second component Mapping, which defines the
*        inverse transformation.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new TranMap. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new TranMap. The syntax used is identical to that for the
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
c     astTranMap()
f     AST_TRANMAP = INTEGER
*        A pointer to the new TranMap.

*  Notes:
*     - The number of output coordinates generated by the two Mappings
*     (their Nout attribute) must be equal, as must the number of input
*     coordinates accepted by each Mapping (their Nin attribute).
*     - The forward transformation of the first Mapping must exist.
*     - The inverse transformation of the second Mapping must exist.
c     - Note that the component Mappings supplied are not copied by
c     astTranMap (the new TranMap simply retains a reference to
c     them). They may continue to be used for other purposes, but
c     should not be deleted. If a TranMap containing a copy of its
c     component Mappings is required, then a copy of the TranMap should
c     be made using astCopy.
f     - Note that the component Mappings supplied are not copied by
f     AST_TRANMAP (the new TranMap simply retains a reference to
f     them). They may continue to be used for other purposes, but
f     should not be deleted. If a TranMap containing a copy of its
f     component Mappings is required, then a copy of the TranMap should
f     be made using AST_COPY.
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

*  Implementation Notes:
*     - This function implements the external (public) interface to
*     the astTranMap constructor function. It returns an ID value
*     (instead of a true C pointer) to external users, and must be
*     provided because astTranMap_ has a variable argument list which
*     cannot be encapsulated in a macro (where this conversion would
*     otherwise occur).
*     - Because no checking or casting of arguments is performed
*     before the function is invoked, the "map1" and "map2" parameters
*     are of type (void *) and are converted from an ID value to a
*     pointer and validated within the function itself.
*     - The variable argument list also prevents this function from
*     invoking astTranMap_ directly, so it must be a re-implementation
*     of it in all respects, except for the conversions between IDs
*     and pointers on input/output of Objects.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstTranMap *new;               /* Pointer to new TranMap */
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

/* Initialise the TranMap, allocating memory and initialising the
   virtual function table as well if necessary. */
      new = astInitTranMap( NULL, sizeof( AstTranMap ), !class_init, &class_vtab,
                           "TranMap", map1, map2 );

/* If successful, note that the virtual function table has been initialised. */
      if ( astOK ) {
         class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new TranMap's
   attributes. */
         va_start( args, options );
         astVSet( new, options, NULL, args );
         va_end( args );

/* If an error occurred, clean up by deleting the new object. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Return an ID value for the new TranMap. */
   return astMakeId( new );
}

AstTranMap *astInitTranMap_( void *mem, size_t size, int init,
                           AstTranMapVtab *vtab, const char *name,
                           AstMapping *map1, AstMapping *map2, int *status ) {
/*
*+
*  Name:
*     astInitTranMap

*  Purpose:
*     Initialise a TranMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "tranmap.h"
*     AstTranMap *astInitTranMap( void *mem, size_t size, int init,
*                               AstTranMapVtab *vtab, const char *name,
*                               AstMapping *map1, AstMapping *map2 )

*  Class Membership:
*     TranMap initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new TranMap object. It allocates memory (if necessary) to
*     accommodate the TranMap plus any additional data associated with the
*     derived class. It then initialises a TranMap structure at the start
*     of this memory. If the "init" flag is set, it also initialises the
*     contents of a virtual function table for a TranMap at the start of
*     the memory passed via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the TranMap is to be initialised.
*        This must be of sufficient size to accommodate the TranMap data
*        (sizeof(TranMap)) plus any data used by the derived class. If a
*        value of NULL is given, this function will allocate the memory itself
*        using the "size" parameter to determine its size.
*     size
*        The amount of memory used by the TranMap (plus derived class
*        data). This will be used to allocate memory if a value of NULL is
*        given for the "mem" parameter. This value is also stored in the
*        TranMap structure, so a valid value must be supplied even if not
*        required for allocating memory.
*     init
*        A logical flag indicating if the TranMap's virtual function table
*        is to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new TranMap.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the Object
*        astClass function).
*     map1
*        Pointer to the first Mapping.
*     map2
*        Pointer to the second Mapping.

*  Returned Value:
*     A pointer to the new TranMap.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstTranMap *new;               /* Pointer to new TranMap */
   int nin;                      /* No. input coordinates for TranMap */
   int nout;                     /* No. output coordinates for TranMap */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitTranMapVtab( vtab, name );

/* Initialise. */
   new = NULL;

/* Report an error if map1 has no forward transformation. */
   if( !astGetTranForward( map1 ) && astOK ) {
      astError( AST__INTRD, "astInitTranMap(%s): The first supplied Mapping "
              "is not able to transform coordinates in the forward direction.", status,
              name );
   }

/* Report an error if map2 has no inverse transformation. */
   if( !astGetTranInverse( map2 ) && astOK ) {
      astError( AST__INTRD, "astInitTranMap(%s): The second supplied Mapping "
              "is not able to transform coordinates in the inverse direction.", status,
              name );
   }

/* Check that the number of coordinates are compatible and report an error if
   they are not. */
   nout = astGetNout( map1 );
   if ( astGetNout( map2 ) != nout && astOK ) {
      astError( AST__INNCO, "astInitTranMap(%s): The number of output "
                      "coordinates per point (%d) for the first Mapping "
                      "supplied does not match the number of output "
                      "coordinates (%d) for the second Mapping.", status, name, nout,
                      astGetNout( map2 ) );
   }

   nin = astGetNin( map1 );
   if ( astGetNin( map2 ) != nin && astOK ) {
      astError( AST__INNCO, "astInitTranMap(%s): The number of input "
                      "coordinates per point (%d) for the first Mapping "
                      "supplied does not match the number of input "
                      "coordinates (%d) for the second Mapping.", status, name, nin,
                      astGetNin( map2 ) );
   }

/* Initialise a Mapping structure (the parent class) as the first component
   within the TranMap structure, allocating memory if necessary. Specify
   the number of input and output coordinates and in which directions the
   Mapping should be defined. */
   if ( astOK ) {
      new = (AstTranMap *) astInitMapping( mem, size, 0,
                                          (AstMappingVtab *) vtab, name,
                                          nin, nout, 1, 1 );

      if ( astOK ) {

/* Initialise the TranMap data. */
/* --------------------------- */
/* Store pointers to the component Mappings. */
         new->map1 = astClone( map1 );
         new->map2 = astClone( map2 );

/* Save the initial values of the inversion flags for these Mappings. */
         new->invert1 = astGetInvert( map1 );
         new->invert2 = astGetInvert( map2 );

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

AstTranMap *astLoadTranMap_( void *mem, size_t size,
                           AstTranMapVtab *vtab, const char *name,
                           AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadTranMap

*  Purpose:
*     Load a TranMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "tranmap.h"
*     AstTranMap *astLoadTranMap( void *mem, size_t size,
*                               AstTranMapVtab *vtab, const char *name,
*                               AstChannel *channel )

*  Class Membership:
*     TranMap loader.

*  Description:
*     This function is provided to load a new TranMap using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     TranMap structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a TranMap at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the TranMap is to be
*        loaded.  This must be of sufficient size to accommodate the
*        TranMap data (sizeof(TranMap)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the TranMap (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the TranMap structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstTranMap) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new TranMap. If this is NULL, a pointer to
*        the (static) virtual function table for the TranMap class is
*        used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "TranMap" is used instead.

*  Returned Value:
*     A pointer to the new TranMap.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstTranMap *new;               /* Pointer to the new TranMap */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this TranMap. In this case the
   TranMap belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstTranMap );
      vtab = &class_vtab;
      name = "TranMap";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitTranMapVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built TranMap. */
   new = astLoadMapping( mem, size, (AstMappingVtab *) vtab, name,
                         channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "TranMap" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

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

/* If an error occurred, clean up by deleting the new TranMap. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new TranMap pointer. */
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




