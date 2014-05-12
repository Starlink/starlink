/*
*class++
*  Name:
*     SelectorMap

*  Purpose:
*     A Mapping that locates positions within one of a set of alternate
*     Regions.

*  Constructor Function:
c     astSelectorMap
f     AST_SELECTORMAP

*  Description:
*     A SelectorMap is a Mapping that identifies which Region contains
*     a given input position.
*
*     A SelectorMap encapsulates a number of Regions that all have the same
*     number of axes and represent the same coordinate Frame. The number of
*     inputs (Nin attribute) of the SelectorMap equals the number of axes
*     spanned by one of the encapsulated Region. All SelectorMaps have only
*     a single output. SelectorMaps do not define an inverse transformation.
*
*     For each input position, the forward transformation of a SelectorMap
*     searches through the encapsulated Regions (in the order supplied when
*     the SelectorMap was created) until a Region is found which contains
*     the input position. The index associated with this Region is
*     returned as the SelectorMap output value (the index value is the
*     position of the Region within the list of Regions supplied when the
*     SelectorMap was created, starting at 1 for the first Region). If an
*     input position is not contained within any Region, a value of zero is
*     returned by the forward transformation.
*
*     If a compound Mapping contains a SelectorMap in series with its own
*     inverse, the combination of the two adjacent SelectorMaps will be
*     replaced by a UnitMap when the compound Mapping is simplified using
c     astSimplify.
f     AST_SIMPLIFY.
*
*     In practice, SelectorMaps are often used in conjunction with SwitchMaps.

*  Inheritance:
*     The SelectorMap class inherits from the Mapping class.

*  Attributes:
*     The SelectorMap class does not define any new attributes beyond those
*     which are applicable to all Mappings.

*  Functions:
c     The SelectorMap class does not define any new functions beyond those
f     The SelectorMap class does not define any new routines beyond those
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
*     15-MAR-2006 (DSB):
*        Original version.
*     18-MAY-2006 (DSB):
*        - Change logic for detecting interior points in function Transform.
*        - Added BADVAL to contructor argument list.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS SelectorMap

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
#include "unitmap.h"             /* Unit Mappings */
#include "channel.h"             /* I/O channels */
#include "selectormap.h"         /* Interface definition for this class */

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

#if defined(THREAD_SAFE)
static int (* parent_managelock)( AstObject *, int, int, AstObject **, int * );
#endif


#ifdef THREAD_SAFE
/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(SelectorMap)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(SelectorMap,Class_Init)
#define class_vtab astGLOBAL(SelectorMap,Class_Vtab)


#include <pthread.h>


#else


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstSelectorMapVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstSelectorMap *astSelectorMapId_( int, void **, double, const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static int Equal( AstObject *, AstObject *, int * );
static int GetObjSize( AstObject *, int * );
static int MapMerge( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );

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
*     Test if two SelectorMaps are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "selectormap.h"
*     int Equal( AstObject *this, AstObject *that, int *status )

*  Class Membership:
*     SelectorMap member function (over-rides the astEqual protected
*     method inherited from the astMapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two SelectorMaps are equivalent.

*  Parameters:
*     this
*        Pointer to the first Object (a SelectorMap).
*     that
*        Pointer to the second Object.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the SelectorMaps are equivalent, zero otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstSelectorMap *that;
   AstSelectorMap *this;
   int i;
   int nin;
   int nreg;
   int result;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain pointers to the two SelectorMap structures. */
   this = (AstSelectorMap *) this_object;
   that = (AstSelectorMap *) that_object;

/* Check the second object is a SelectorMap. We know the first is a
   SelectorMap since we have arrived at this implementation of the virtual
   function. */
   if( astIsASelectorMap( that ) ) {

/* Check they have the same number of inputs. */
      nin = astGetNin( this );
      if( astGetNin( that ) == nin ) {

/* Check they contain the same number of Regions, and have the same badval. */
         nreg = this->nreg;
         if( that->nreg == nreg ||
             astEQUAL( that->badval, this->badval) ) {

/* Loop over the Regions, breaking as soon as two unequal Regions are
   found. */
            result = 1;
            for( i = 0; i < nreg; i++ ) {
               if( !astEqual( this->reg[ i ], that->reg[ i ] ) ) {
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

static int GetObjSize( AstObject *this_object, int *status ) {
/*
*  Name:
*     GetObjSize

*  Purpose:
*     Return the in-memory size of an Object.

*  Type:
*     Private function.

*  Synopsis:
*     #include "selectormap.h"
*     int GetObjSize( AstObject *this, int *status )

*  Class Membership:
*     SelectorMap member function (over-rides the astGetObjSize protected
*     method inherited from the parent class).

*  Description:
*     This function returns the in-memory size of the supplied SelectorMap,
*     in bytes.

*  Parameters:
*     this
*        Pointer to the SelectorMap.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The Object size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstSelectorMap *this;
   int i;
   int result;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointers to the SelectorMap structure. */
   this = (AstSelectorMap *) this_object;

/* Invoke the GetObjSize method inherited from the parent class, and then
   add on any components of the class structure defined by this class
   which are stored in dynamically allocated memory. */
   result = (*parent_getobjsize)( this_object, status );

   for( i = 0; i < this->nreg; i++ ) {
      result += astGetObjSize( this->reg[ i ] );
   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

void astInitSelectorMapVtab_(  AstSelectorMapVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitSelectorMapVtab

*  Purpose:
*     Initialise a virtual function table for a SelectorMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "selectormap.h"
*     void astInitSelectorMapVtab( AstSelectorMapVtab *vtab, const char *name )

*  Class Membership:
*     SelectorMap vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the SelectorMap class.

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
   will be used (by astIsASelectorMap) to determine if an object belongs to
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

   parent_transform = mapping->Transform;
   mapping->Transform = Transform;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   object->Equal = Equal;
   mapping->MapMerge = MapMerge;

/* Declare the copy constructor, destructor and class dump function. */
   astSetCopy( vtab, Copy );
   astSetDelete( vtab, Delete );
   astSetDump( vtab, Dump, "SelectorMap", "Region identification Mapping" );

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
*     SelectorMap member function (over-rides the astManageLock protected
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
   AstSelectorMap *this;       /* Pointer to SelectorMap structure */
   int i;                      /* Loop count */
   int result;                 /* Returned status value */

/* Initialise */
   result = 0;

/* Check the supplied pointer is not NULL. */
   if( !this_object ) return result;

/* Obtain a pointers to the SelectorMap structure. */
   this = (AstSelectorMap *) this_object;

/* Invoke the ManageLock method inherited from the parent class. */
   if( !result ) result = (*parent_managelock)( this_object, mode, extra,
                                                fail, status );

/* Invoke the astManageLock method on any Objects contained within
   the supplied Object. */
   for( i = 0; i < this->nreg; i++ ) {
      if( !result ) result = astManageLock( this->reg[ i ], mode, extra, fail );
   }

   return result;

}
#endif

static int MapMerge( AstMapping *this, int where, int series, int *nmap,
                     AstMapping ***map_list, int **invert_list, int *status ) {
/*
*  Name:
*     MapMerge

*  Purpose:
*     Simplify a sequence of Mappings containing a SelectorMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     int MapMerge( AstMapping *this, int where, int series, int *nmap,
*                   AstMapping ***map_list, int **invert_list, int *status )

*  Class Membership:
*     SelectorMap method (over-rides the protected astMapMerge method
*     inherited from the Mapping class).

*  Description:
*     This function attempts to simplify a sequence of Mappings by
*     merging a nominated SelectorMap in the sequence with its neighbours,
*     so as to shorten the sequence if possible.
*
*     In many cases, simplification will not be possible and the
*     function will return -1 to indicate this, without further
*     action.
*
*     In most cases of interest, however, this function will either
*     attempt to replace the nominated SelectorMap with one which it
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
*        Pointer to the nominated SelectorMap which is to be merged with
*        its neighbours. This should be a cloned copy of the SelectorMap
*        pointer contained in the array element "(*map_list)[where]"
*        (see below). This pointer will not be annulled, and the
*        SelectorMap it identifies will not be modified by this function.
*     where
*        Index in the "*map_list" array (below) at which the pointer
*        to the nominated SelectorMap resides.
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
   AstMapping *new;
   AstRegion **sreg;
   AstSelectorMap *map;
   AstSelectorMap *slneb;
   int equal;
   int i;
   int ilo;
   int nreg;
   int result;
   int simp;

/* Initialise.*/
   result = -1;

/* Check the inherited status. */
   if ( !astOK ) return result;

/* Get a pointer to this SelectorMap, and note the number of Regions. */
   map = (AstSelectorMap *) this;
   nreg = map->nreg;

/* Attempt to simplify the SelectorMap on its own. */
/* ============================================= */

/* Try to simplify each of the encapsulated Regions, noting if any
   simplification takes place. */
   simp = 0;
   sreg = astMalloc( sizeof( AstRegion * )*nreg );
   if( astOK ) {
      for( i = 0; i < nreg; i++ ) {
         sreg[ i ] = astSimplify( map->reg[ i ] );
         simp = simp || ( sreg[ i ] != map->reg[ i ] );
      }

/* If any simplification took place, construct a new SelectorMap from these
   simplified Mappings. */
      if( simp ) {
         (void) astAnnul( ( *map_list )[ where ] );
         ( *map_list )[ where ] = (AstMapping *) astSelectorMap( nreg,
                                                          (void **) sreg,
                                                          map->badval, "", status );
         result = where;
      }

/* Release resources. */
      if( sreg ) {
         for( i = 0; i < nreg; i++ ) sreg[ i ] = astAnnul( sreg[ i ] );
         sreg = astFree( sreg );
      }
   }

/* If possible, merge the SelectorMap with a neighbouring SelectorMap. */
/* =============================================================== */
/* Only do this if no change was made above, and we are combining the
   Mappings in series. */
   if( result == -1 && series ) {

/* Is the higher neighbour a SelectorMap? If so get a pointer to it, and
   note the index of the lower of the two adjacent SelectorMaps. */
      if( where < ( *nmap - 1 ) &&
          astIsASelectorMap( ( *map_list )[ where + 1 ] ) ){
         slneb = (AstSelectorMap *) ( *map_list )[ where + 1 ];
         ilo = where;

/* If not, is the lower neighbour a SelectorMap? If so get a pointer to it, and
   note the index of the lower of the two adjacent SelectorMaps. */
      } else if( where > 0 &&
                 astIsASelectorMap( ( *map_list )[ where - 1 ] ) ){
         slneb = (AstSelectorMap *) ( *map_list )[ where - 1 ];
         ilo =  where - 1;

      } else {
         slneb = NULL;
      }

/* If a neighbouring SelectorMap was found, we can replace the pair by a
   UnitMap if the two SelectorMaps are equal but have opposite values for
   their Invert flags. Temporarily invert the neighbour, then compare
   the two SelectorMaps for equality, then re-invert the neighbour. */
      if( slneb ) {
         astInvert( slneb );
         equal = astEqual( map, slneb );
         astInvert( slneb );

/* If the two SelectorMaps are equal but opposite, annul the first of the two
   Mappings, and replace it with a UnitMap. Also set the invert flag. */
         if( equal ) {
            new = (AstMapping *) astUnitMap( astGetNin( ( *map_list )[ ilo ] ), "", status );
            (void) astAnnul( ( *map_list )[ ilo ] );
            ( *map_list )[ ilo ] = new;
            ( *invert_list )[ ilo ] = 0;

/* Annul the second of the two Mappings, and shuffle down the rest of the
   list to fill the gap. */
            (void) astAnnul( ( *map_list )[ ilo + 1 ] );
            for ( i = ilo + 2; i < *nmap; i++ ) {
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

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = -1;

/* Return the result. */
   return result;
}

static AstPointSet *Transform( AstMapping *this, AstPointSet *in,
                               int forward, AstPointSet *out, int *status ) {
/*
*  Name:
*     Transform

*  Purpose:
*     Apply a SelectorMap to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "selectormap.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     SelectorMap member function (over-rides the astTransform method inherited
*     from the Mapping class).

*  Description:
*     This function takes a SelectorMap and a set of points encapsulated in a
*     PointSet and transforms the points so as to apply the required Mapping.
*     This implies applying each of the SelectorMap's component Mappings in turn,
*     either in series or in parallel.

*  Parameters:
*     this
*        Pointer to the SelectorMap.
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
*     match the number of coordinates for the SelectorMap being applied.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstPointSet *ps1;
   AstPointSet *ps2;
   AstPointSet *result;
   AstPointSet *tps;
   AstRegion *reg;
   AstSelectorMap *map;
   double **ptr_out;
   double **ptr1;
   double **ptr2;
   double **tptr;
   double *p2;
   double *pout;
   double badval;
   int bad;
   int closed;
   int icoord;
   int ipoint;
   int ireg;
   int ncoord;
   int npoint;

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the SelectorMap. */
   map = (AstSelectorMap *) this;

/* Apply the parent Mapping using the stored pointer to the Transform member
   function inherited from the parent Mapping class. This function validates
   all arguments and generates an output PointSet if necessary, but does not
   actually transform any coordinate values. */
   result = (*parent_transform)( this, in, forward, out, status );

/* We now extend the parent astTransform method by applying the component
   Mappings of the SelectorMap to generate the output coordinate values. */

/* Check we are implementing the original forward transformation (the
   inverse transformation is not defined). */
   if( forward != astGetInvert( this ) ) {

/* Get the number of input axes and the number of points. */
      ncoord = astGetNcoord( in );
      npoint = astGetNpoint( in );

/* Create two temporary PointSets to hold copies of the input points. */
      ps1 = astCopy( in );
      ptr1 = astGetPoints( ps1 );
      ps2 = astPointSet( npoint, ncoord, "", status );
      ptr2 = astGetPoints( ps2 );

/* Get a pointer to the output data */
      ptr_out = astGetPoints( result );
      if( astOK ) {

/* Initialise the output array to hold -1 at any points that have
   bad input axis values, and zero at all other points. */
         pout = ptr_out[ 0 ];
         for( ipoint = 0; ipoint < npoint; ipoint++ ) {
            bad = 0;
            for( icoord = 0; icoord < ncoord; icoord++ ) {
               if( ptr1[ icoord ][ ipoint ] == AST__BAD ) {
                  bad = 1;
                  break;
               }
            }
            *(pout++) = bad ? -1 : 0;
         }

/* Loop round all Regions. */
         for( ireg = 1; ireg <= map->nreg; ireg++ ) {
            reg = map->reg[ ireg - 1 ];

/* Temporarily Negate the Region. */
            astNegate( reg );
            closed = astGetClosed( reg );
            astSetClosed( reg, !closed );

/* Transform the remaining input positions. Good input positions which
   are within the Region will be bad in the output. */
            ps2 = astTransform( reg, ps1, 1, ps2 );

/* Loop round all positions. */
            p2 = ptr2[ 0 ];
            pout = ptr_out[ 0 ];
            for( ipoint = 0; ipoint < npoint; ipoint++, p2++, pout++ ) {

/* Any position that has not already been assigned to a Region and is bad
   in the output PointSet must be contained within the current Region, so
   assign the (one-based) index of the current Region to the output element. */
               if( *pout == 0 && *p2 == AST__BAD ) *pout = ireg;
            }

/* Negate the Region to get it back to its original state. */
            astSetClosed( reg, closed );
            astNegate( reg );

/* Swap the input and output PointSets. */
            tps = ps1;
            ps1 = ps2;
            ps2 = tps;
            tptr = ptr1;
            ptr1 = ptr2;
            ptr2 = tptr;
         }

/* Replace -1 values in the output (that indicate that the input position
   had at least one bad axis value) with the "badval".*/
         badval = map->badval;
         pout = ptr_out[ 0 ];
         for( ipoint = 0; ipoint < npoint; ipoint++, pout++ ) {
            if( *pout == -1 ) *pout = badval;
         }
      }

/* Free resources. */
      ps1 = astAnnul( ps1 );
      ps2 = astAnnul( ps2 );
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
*     Copy constructor for SelectorMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for SelectorMap objects.

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
*     -  This constructor makes a deep copy, including a copy of the
*     Regions within the SelectorMap.
*/

/* Local Variables: */
   AstSelectorMap *in;                /* Pointer to input SelectorMap */
   AstSelectorMap *out;               /* Pointer to output SelectorMap */
   int i;                             /* Loop count */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output SelectorMaps. */
   in = (AstSelectorMap *) objin;
   out = (AstSelectorMap *) objout;

/* For safety, start by clearing any references to the input Regions. */
   out->reg = NULL;
   out->nreg = 0;

/* Make copies of the Regions, and store pointers to them in the output
   SelectorMap structure. */
   out->reg = astMalloc( sizeof( AstRegion * )*( in->nreg ) );
   if( astOK ) {
      for( i = 0; i < in->nreg; i++ ) {
         out->reg[ i ] = astCopy( in->reg[ i ] );
      }
      out->nreg = in->nreg;
   }

}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for SelectorMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for SelectorMap objects.

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
   AstSelectorMap *this;              /* Pointer to SelectorMap */
   int i;

/* Obtain a pointer to the SelectorMap structure. */
   this = (AstSelectorMap *) obj;

/* Free dynamically allocated resources. */
   for( i = 0; i < this->nreg; i++ ) {
      this->reg[ i ] = astAnnul( this->reg[ i ] );
   }
   this->reg = astFree( this->reg );

/* Clear the remaining SelectorMap variables. */
   this->nreg = 0;
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for SelectorMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the SelectorMap class to an output Channel.

*  Parameters:
*     this
*        Pointer to the SelectorMap whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstSelectorMap *this;
   int i;
   char buf[ 20 ];

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the SelectorMap structure. */
   this = (AstSelectorMap *) this_object;

/* Write out values representing the instance variables for the SelectorMap
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

/* Loop to dump each Region. */
/* ------------------------- */
/* The coordinate Frame of the Regions is defined by the first Region.
   The Frame information is omitted from the second and subsequent
   Regions by setting the protected RegionFS attribute to zero. */
   for( i = 0; i < this->nreg; i++ ) {
      sprintf( buf, "Reg%d", i + 1 );
      if( i > 0 ) astSetRegionFS( this->reg[ i ], 0 );
      astWriteObject( channel, buf, 1, 1, this->reg[ i ],
                      "Region of input space" );
      if( i > 0 ) astClearRegionFS( this->reg[ i ] );
   }

/* BadVal. */
/* ------- */
   if( this->badval != AST__BAD ) {
      astWriteDouble( channel, "BadVal", 1, 1, this->badval,
                      "Output value for bad input positions" );
   }

}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsASelectorMap and astCheckSelectorMap functions using the
   macros defined for this purpose in the "object.h" header file. */
astMAKE_ISA(SelectorMap,Mapping)
astMAKE_CHECK(SelectorMap)

AstSelectorMap *astSelectorMap_( int nreg, void **regs_void, double badval,
                                 const char *options, int *status, ...) {
/*
*+
*  Name:
*     astSelectorMap

*  Purpose:
*     Create a SelectorMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "selectormap.h"
*     AstSelectorMap *astSelectorMap( int nreg, AstRegion **regs,
*                                     double badval, const char *options, ... )

*  Class Membership:
*     SelectorMap constructor.

*  Description:
*     This function creates a new SelectorMap and optionally initialises its
*     attributes.

*  Parameters:
*     nreg
*        The number of Regions supplied.
*     regs
*        An array of pointers to the Regions. Deep copies of these
*        Regions are taken.
*     badval
*        The value to be returned by the forward transformation of the
*        SelectorMap for any input positions that have a bad (AST__BAD)
*        value on any axis.
*     options
*        Pointer to a null terminated string containing an optional
*        comma-separated list of attribute assignments to be used for
*        initialising the new SelectorMap. The syntax used is the same as for the
*        astSet method and may include "printf" format specifiers identified
*        by "%" symbols in the normal way.
*     ...
*        If the "options" string contains "%" format specifiers, then an
*        optional list of arguments may follow it in order to supply values to
*        be substituted for these specifiers. The rules for supplying these
*        are identical to those for the astSet method (and for the C "printf"
*        function).

*  Returned Value:
*     A pointer to the new SelectorMap.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-

*  Implementation Notes:
*     - This function implements the basic SelectorMap constructor which is
*     available via the protected interface to the SelectorMap class.  A
*     public interface is provided by the astSelectorMapId_ function.
*     - Because this function has a variable argument list, it is
*     invoked by a macro that evaluates to a function pointer (not a
*     function invocation) and no checking or casting of arguments is
*     performed before the function is invoked. Because of this, the
*     "map1" and "map2" parameters are of type (void *) and are
*     converted and validated within the function itself.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstSelectorMap *new;          /* Pointer to new SelectorMap */
   AstRegion **regs;             /* Array of Region pointers */
   int i;                        /* Region index */
   va_list args;                 /* Variable argument list */

/* Initialise. */
   new = NULL;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return new;

/* Report an error if no Regions have been supplied. */
   if( nreg <= 0 ) astError( AST__BDPAR, "astSelectorMap(SelectorMap): "
                            "Bad number of Regions (%d) specified.", status, nreg );

/* Otherwise create an array to hold the Region pointers. */
   regs = astMalloc( sizeof( AstRegion * )*nreg );

/* Obtain and validate pointers to the Region structures provided. */
   if( astOK ) {
      for( i = 0; i < nreg; i++ ) {
         regs[ i ] = astCheckRegion( regs_void[ i ] );
      }
   }

   if ( astOK ) {

/* Initialise the SelectorMap, allocating memory and initialising the
   virtual function table as well if necessary. */
      new = astInitSelectorMap( NULL, sizeof( AstSelectorMap ), !class_init, &class_vtab,
                              "SelectorMap", nreg, regs, badval );

/* If successful, note that the virtual function table has been
   initialised. */
      if ( astOK ) {
         class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new SelectorMap's
   attributes. */
         va_start( args, status );
         astVSet( new, options, NULL, args );
         va_end( args );

/* If an error occurred, clean up by deleting the new object. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Free memory used to hold the Regions pointers. */
   regs = astFree( regs );

/* Return a pointer to the new SelectorMap. */
   return new;
}

AstSelectorMap *astSelectorMapId_( int nreg, void **regs_void, double badval,
                                   const char *options, ... ) {
/*
*++
*  Name:
c     astSelectorMap
f     AST_SELECTORMAP

*  Purpose:
*     Create a SelectorMap.

*  Type:
*     Public function.

*  Synopsis:
c     #include "selectormap.h"
c     AstSelectorMap *astSelectorMap( int nreg, AstRegion *regs[],
c                                     double badval, const char *options, ... )
f     RESULT = AST_SELECTORMAP( NREG, REGS, BADVAL, OPTIONS, STATUS )

*  Class Membership:
*     SelectorMap constructor.

*  Description:
*     This function creates a new SelectorMap and optionally initialises
*     its attributes.
*
*     A SelectorMap is a Mapping that identifies which Region contains
*     a given input position.
*
*     A SelectorMap encapsulates a number of Regions that all have the same
*     number of axes and represent the same coordinate Frame. The number of
*     inputs (Nin attribute) of the SelectorMap equals the number of axes
*     spanned by one of the encapsulated Region. All SelectorMaps have only
*     a single output. SelectorMaps do not define an inverse transformation.
*
*     For each input position, the forward transformation of a SelectorMap
*     searches through the encapsulated Regions (in the order supplied when
*     the SelectorMap was created) until a Region is found which contains
*     the input position. The index associated with this Region is
*     returned as the SelectorMap output value (the index value is the
*     position of the Region within the list of Regions supplied when the
*     SelectorMap was created, starting at 1 for the first Region). If an
*     input position is not contained within any Region, a value of zero is
*     returned by the forward transformation.
*
*     If a compound Mapping contains a SelectorMap in series with its own
*     inverse, the combination of the two adjacent SelectorMaps will be
*     replaced by a UnitMap when the compound Mapping is simplified using
c     astSimplify.
f     AST_SIMPLIFY.
*
*     In practice, SelectorMaps are often used in conjunction with SwitchMaps.

*  Parameters:
c     nreg
f     NREG = INTEGER (Given)
*        The number of supplied Regions.
c     regs
f     REGS( NREG ) = INTEGER (Given)
*        An array of pointers to the Regions. All the supplied Regions must
*        relate to the same coordinate Frame. The number of axes in this
*        coordinate Frame defines the number of inputs for the SelectorMap.
c     badval
f     BADVAL = DOUBLE PRECISION (Given)
*        The value to be returned by the forward transformation of the
*        SelectorMap for any input positions that have a bad (AST__BAD)
*        value on any axis.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new SelectorMap. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new SelectorMap. The syntax used is identical to that for the
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
c     astSelectorMap()
f     AST_SELECTORMAP = INTEGER
*        A pointer to the new SelectorMap.

*  Notes:
*     - Deep copies are taken of the supplied Regions. This means that
*     any subsequent changes made to the component Regions using the
*     supplied pointers will have no effect on the SelectorMap.
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--

*  Implementation Notes:
*     - This function implements the external (public) interface to
*     the astSelectorMap constructor function. It returns an ID value
*     (instead of a true C pointer) to external users, and must be
*     provided because astSelectorMap_ has a variable argument list which
*     cannot be encapsulated in a macro (where this conversion would
*     otherwise occur).
*     - Because no checking or casting of arguments is performed
*     before the function is invoked, the "map1" and "map2" parameters
*     are of type (void *) and are converted from an ID value to a
*     pointer and validated within the function itself.
*     - The variable argument list also prevents this function from
*     invoking astSelectorMap_ directly, so it must be a re-implementation
*     of it in all respects, except for the conversions between IDs
*     and pointers on input/output of Objects.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstSelectorMap *new;          /* Pointer to new SelectorMap */
   AstRegion **regs;             /* Array of Region pointers */
   int i;                        /* Region index */
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

/* Report an error if no Regions have been supplied. */
   if( nreg <= 0 ) astError( AST__BDPAR, "astSelectorMap(SelectorMap): "
                             "Bad number of Regions (%d) specified.", status, nreg );

/* Create an array to hold the Region pointers. */
   regs = astMalloc( sizeof( AstRegion * )*nreg );

/* Obtain and validate pointers to the Region structures provided. */
   if( astOK ) {
      for( i = 0; i < nreg; i++ ) {
         regs[ i ] = astVerifyRegion( astMakePointer(regs_void[ i ]) );
      }
   }

   if ( astOK ) {

/* Initialise the SelectorMap, allocating memory and initialising the
   virtual function table as well if necessary. */
      new = astInitSelectorMap( NULL, sizeof( AstSelectorMap ), !class_init,
                                &class_vtab, "SelectorMap", nreg, regs,
                                badval );

/* If successful, note that the virtual function table has been
   initialised. */
      if ( astOK ) {
         class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new SelectorMap's
   attributes. */
         va_start( args, options );
         astVSet( new, options, NULL, args );
         va_end( args );

/* If an error occurred, clean up by deleting the new object. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Free memory used to hold the Regions pointers. */
   regs = astFree( regs );

/* Return an ID value for the new SelectorMap. */
   return astMakeId( new );
}

AstSelectorMap *astInitSelectorMap_( void *mem, size_t size, int init,
                                     AstSelectorMapVtab *vtab, const char *name,
                                     int nreg, AstRegion **regs, double badval, int *status ) {
/*
*+
*  Name:
*     astInitSelectorMap

*  Purpose:
*     Initialise a SelectorMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "selectormap.h"
*     AstSelectorMap *astInitSelectorMap( void *mem, size_t size, int init,
*                                         AstSelectorMapVtab *vtab, const char *name,
*                                         int nreg, AstRegion **regs, double badval )

*  Class Membership:
*     SelectorMap initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new SelectorMap object. It allocates memory (if necessary) to
*     accommodate the SelectorMap plus any additional data associated with the
*     derived class. It then initialises a SelectorMap structure at the start
*     of this memory. If the "init" flag is set, it also initialises the
*     contents of a virtual function table for a SelectorMap at the start of
*     the memory passed via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the SelectorMap is to be initialised.
*        This must be of sufficient size to accommodate the SelectorMap data
*        (sizeof(SelectorMap)) plus any data used by the derived class. If a
*        value of NULL is given, this function will allocate the memory itself
*        using the "size" parameter to determine its size.
*     size
*        The amount of memory used by the SelectorMap (plus derived class
*        data). This will be used to allocate memory if a value of NULL is
*        given for the "mem" parameter. This value is also stored in the
*        SelectorMap structure, so a valid value must be supplied even if not
*        required for allocating memory.
*     init
*        A logical flag indicating if the SelectorMap's virtual function table
*        is to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new SelectorMap.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the Object
*        astClass function).
*     nreg
*        The number of Regions supplied.
*     regs
*        An array holdiong pointers to the Regions. Deep copies are taken
*        of these Regions.
*     badval
*        The value to be returned by the forward transformation of the
*        SelectorMap for any input positions that have a bad (AST__BAD)
*        value on any axis.

*  Returned Value:
*     A pointer to the new SelectorMap.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstFrame *f0;                 /* Frame from first Region */
   AstFrame *f1;                 /* Frame from current Region */
   AstSelectorMap *new;          /* Pointer to new SelectorMap */
   int equal;                    /* Are Frames equal? */
   int i;                        /* Loop count */
   int nin;                      /* No. input coordinates for SelectorMap */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitSelectorMapVtab( vtab, name );

/* Initialise. */
   new = NULL;

/* Check that all Regions refer to the same Frame. */
   f0 = astRegFrame( regs[ 0 ] );
   for( i = 1; i < nreg; i++ ) {
      f1 = astRegFrame( regs[ i ] );
      equal = astEqual( f1, f0 );
      f1 = astAnnul( f1 );

      if( !equal ) {
         if( astOK ) {
            astError( AST__BADNI, "astInitSelectorMap(%s): Region "
                      "number %d does not refer to the same coordinate "
                      "Frame as the first Region.", status, name, i + 1 );
         }
      }
   }

   nin = astGetNin( regs[ 0 ] );
   f0 = astAnnul( f0 );

/* Initialise a Mapping structure (the parent class) as the first component
   within the SelectorMap structure, allocating memory if necessary. Specify
   the number of input and output coordinates and in which directions the
   Mapping should be defined. */
   if ( astOK ) {
      new = (AstSelectorMap *) astInitMapping( mem, size, 0,
                                             (AstMappingVtab *) vtab, name,
                                             nin, 1, 1, 0 );
      if ( astOK ) {

/* Initialise the SelectorMap data. */
/* -------------------------------- */

/* Create an array for the Region pointers. */
         new->reg = astMalloc( sizeof( AstRegion * )*nreg );

/* Store pointers to deep copies of the Regions. */
         if( astOK ) {
            new->nreg = nreg;
            for( i = 0; i < nreg; i++ ) {
               new->reg[ i ] = astCopy( regs[ i ] );
            }
         } else {
            new->nreg = 0;
         }

/* Store other items */
         new->badval = badval;

/* If an error occurred, clean up by deleting the new object. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Return a pointer to the new object. */
   return new;
}

AstSelectorMap *astLoadSelectorMap_( void *mem, size_t size,
                                     AstSelectorMapVtab *vtab, const char *name,
                                     AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadSelectorMap

*  Purpose:
*     Load a SelectorMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "selectormap.h"
*     AstSelectorMap *astLoadSelectorMap( void *mem, size_t size,
*                                         AstSelectorMapVtab *vtab, const char *name,
*                                         AstChannel *channel )

*  Class Membership:
*     SelectorMap loader.

*  Description:
*     This function is provided to load a new SelectorMap using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     SelectorMap structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a SelectorMap at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the SelectorMap is to be
*        loaded.  This must be of sufficient size to accommodate the
*        SelectorMap data (sizeof(SelectorMap)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the SelectorMap (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the SelectorMap structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstSelectorMap) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new SelectorMap. If this is NULL, a pointer to
*        the (static) virtual function table for the SelectorMap class is
*        used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "SelectorMap" is used instead.

*  Returned Value:
*     A pointer to the new SelectorMap.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstSelectorMap *new;
   AstFrameSet *fs;
   AstRegion *reg;
   int i;
   char buf[ 20 ];

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this SelectorMap. In this case the
   SelectorMap belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstSelectorMap );
      vtab = &class_vtab;
      name = "SelectorMap";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitSelectorMapVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built SelectorMap. */
   new = astLoadMapping( mem, size, (AstMappingVtab *) vtab, name,
                         channel );
   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "SelectorMap" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */


/* Loop to load each Region. */
/* ------------------------- */
      new->reg = NULL;
      i = 0;
      fs = NULL;
      while( astOK ) {
         sprintf( buf, "reg%d", i + 1 );
         reg = astReadObject( channel, buf, NULL );
         if( reg ) {
            new->reg = astGrow( new->reg, i + 1, sizeof( AstRegion *) );
            if( astOK ) {
               new->reg[ i ] = reg;

/* All but the first Region may have a dummy FrameSet rather than the
   correct FrameSet. The correct FrameSet will be a copy of the FrameSet
   from the first Region. */
               if( i == 0 ) {
                  fs = astGetRegFS( reg );
               } else if( astRegDummyFS( reg ) ){
                  astSetRegFS( reg, fs );
               }

               i++;
            }
         } else {
            break;
         }
      }

      fs = astAnnul( fs );

/* Number of Regions. */
      new->nreg = i;


/* BadVal. */
/* ------- */
      new->badval = astReadDouble( channel, "badval", AST__BAD );

/* If an error occurred, clean up by deleting the new SelectorMap. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new SelectorMap pointer. */
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




