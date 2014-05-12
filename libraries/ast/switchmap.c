/*
*class++
*  Name:
*     SwitchMap

*  Purpose:
*     A Mapping that encapsulates a set of alternate Mappings.

*  Constructor Function:
c     astSwitchMap
f     AST_SWITCHMAP

*  Description:
*     A SwitchMap is a Mapping which represents a set of alternate
*     Mappings, each of which is used to transform positions within a
*     particular region of the input or output coordinate system of the
*     SwitchMap.
*
*     A SwitchMap can encapsulate any number of Mappings, but they must
*     all have the same number of inputs (Nin attribute value) and the
*     same number of outputs (Nout attribute value). The SwitchMap itself
*     inherits these same values for its Nin and Nout attributes. Each of
*     these Mappings represents a "route" through the switch, and are
*     referred to as "route" Mappings below. Each route Mapping transforms
*     positions between the input and output coordinate space of the entire
*     SwitchMap, but only one Mapping will be used to transform any given
*     position. The selection of the appropriate route Mapping to use with
*     any given input position is made by another Mapping, called the
*     "selector" Mapping. Each SwitchMap encapsulates two selector
*     Mappings in addition to its route Mappings; one for use with the
*     SwitchMap's forward transformation (called the "forward selector
*     Mapping"), and one for use with the SwitchMap's inverse transformation
*     (called the "inverse selector Mapping"). The forward selector Mapping
*     must have the same number of inputs as the route Mappings, but
*     should have only one output. Likewise, the inverse selector Mapping
*     must have the same number of outputs as the route Mappings, but
*     should have only one input.
*
*     When the SwitchMap is used to transform a position in the forward
*     direction (from input to output), each supplied input position is
*     first transformed by the forward transformation of the forward selector
*     Mapping. This produces a single output value for each input position
*     referred to as the selector value. The nearest integer to the selector
*     value is found, and is used to index the array of route Mappings (the
*     first supplied route Mapping has index 1, the second route Mapping has
*     index 2, etc). If the nearest integer to the selector value is less
*     than 1 or greater than the number of route Mappings, then the SwitchMap
*     output position is set to a value of AST__BAD on every axis. Otherwise,
*     the forward transformation of the selected route Mapping is used to
*     transform the supplied input position to produce the SwitchMap output
*     position.
*
*     When the SwitchMap is used to transform a position in the inverse
*     direction (from "output" to "input"), each supplied "output" position
*     is first transformed by the inverse transformation of the inverse
*     selector Mapping. This produces a selector value for each "output"
*     position. Again, the nearest integer to the selector value is found,
*     and is used to index the array of route Mappings. If this selector
*     index value is within the bounds of the array of route Mappings, then
*     the inverse transformation of the selected route Mapping is used to
*     transform the supplied "output" position to produce the SwitchMap
*     "input" position. If the selector index value is outside the bounds
*     of the array of route Mappings, then the SwitchMap "input" position is
*     set to a value of AST__BAD on every axis.
*
*     In practice, appropriate selector Mappings should be chosen to
*     associate a different route Mapping with each region of coordinate
*     space. Note that the SelectorMap class of Mapping is particularly
*     appropriate for this purpose.
*
*     If a compound Mapping contains a SwitchMap in series with its own
*     inverse, the combination of the two adjacent SwitchMaps will be
*     replaced by a UnitMap when the compound Mapping is simplified using
c     astSimplify.
f     AST_SIMPLIFY.

*  Inheritance:
*     The SwitchMap class inherits from the Mapping class.

*  Attributes:
*     The SwitchMap class does not define any new attributes beyond those
*     which are applicable to all Mappings.

*  Functions:
c     The SwitchMap class does not define any new functions beyond those
f     The SwitchMap class does not define any new routines beyond those
*     which are applicable to all Mappings.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the Research Councils

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
*     13-MAR-2006 (DSB):
*        Original version.
*     17-MAR-2006 (DSB):
*        Guard against AST__BAD selector values.
*     9-MAY-2006 (DSB):
*        Check selector Mapping pointers are not NULL before calling
*        astEqual in Equal.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS SwitchMap

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
#include "switchmap.h"           /* Interface definition for this class */
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

#if defined(THREAD_SAFE)
static int (* parent_managelock)( AstObject *, int, int, AstObject **, int * );
#endif



#ifdef THREAD_SAFE
/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(SwitchMap)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(SwitchMap,Class_Init)
#define class_vtab astGLOBAL(SwitchMap,Class_Vtab)


#include <pthread.h>


#else


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstSwitchMapVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstSwitchMap *astSwitchMapId_( void *, void *, int, void **, const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstMapping *RemoveRegions( AstMapping *, int * );
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static double Rate( AstMapping *, double *, int, int, int * );
static int Equal( AstObject *, AstObject *, int * );
static int GetObjSize( AstObject *, int * );
static int MapMerge( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static AstMapping *GetSelector( AstSwitchMap *, int, int *, int * );
static AstMapping *GetRoute( AstSwitchMap *, double, int *, int * );

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
*     Test if two SwitchMaps are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "switchmap.h"
*     int Equal( AstObject *this, AstObject *that, int *status )

*  Class Membership:
*     SwitchMap member function (over-rides the astEqual protected
*     method inherited from the astMapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two SwitchMaps are equivalent.

*  Parameters:
*     this
*        Pointer to the first Object (a SwitchMap).
*     that
*        Pointer to the second Object.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the SwitchMaps are equivalent, zero otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstMapping *fsmap1;
   AstMapping *fsmap2;
   AstMapping *ismap1;
   AstMapping *ismap2;
   AstMapping *rmap1;
   AstMapping *rmap2;
   AstSwitchMap *that;
   AstSwitchMap *this;
   int fsinv1;
   int fsinv2;
   int isinv1;
   int i;
   int isinv2;
   int nroute;
   int result;
   int rinv1;
   int rinv2;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain pointers to the two SwitchMap structures. */
   this = (AstSwitchMap *) this_object;
   that = (AstSwitchMap *) that_object;

/* Check the second object is a SwitchMap. We know the first is a
   SwitchMap since we have arrived at this implementation of the virtual
   function. */
   if( astIsASwitchMap( that ) ) {

/* Check they have the same number of route mappings. */
      nroute = this->nroute;
      if( that->nroute == nroute ) {

/* Get the forward selector Mappings from the two SwitchMaps. */
         fsmap1 = GetSelector( this, 1, &fsinv1, status );
         fsmap2 = GetSelector( that, 1, &fsinv2, status );

/* Are they equal? */
         if( ( !fsmap1 && !fsmap2 ) ||
             ( fsmap1 && fsmap2 && astEqual( fsmap1, fsmap2 ) ) ) {

/* Get the inverse selector Mappings from the two SwitchMaps. */
            ismap1 = GetSelector( this, 0, &isinv1, status );
            ismap2 = GetSelector( that, 0, &isinv2, status );

/* Are they equal? */
            if( ( !ismap1 && !ismap2 ) ||
                ( ismap1 && ismap2 && astEqual( ismap1, ismap2 ) ) ) {

/* Loop over the route mappings, breaking as soon as two unequal route
   Mappings are found. Re-instate the original values for the route
   Mapping Invert flag after testing the route Mappings for equality. */
               result = 1;
               for( i = 0; result && i < nroute; i++ ) {
                  rmap1 = GetRoute( this, (double) ( i + 1 ), &rinv1, status );
                  rmap2 = GetRoute( that, (double) ( i + 1 ), &rinv2, status );
                  if( !astEqual( rmap1, rmap2 ) ) result = 0;
                  astSetInvert( rmap2, rinv2 );
                  astSetInvert( rmap1, rinv1 );
               }
            }

/* Reinstate the invert flags for the inverse selector Mappings. Ensure
   this is done in the opposite order to which the selector Mappings were
   obtained (in case they are in fact the same Mapping). */
            if( ismap2 ) astSetInvert( ismap2, isinv2 );
            if( ismap1 ) astSetInvert( ismap1, isinv1 );
         }

/* Reinstate the invert flags for the forward selector Mappings. Ensure
   this is done in the oppsote order to which the selector Mappings were
   obtained (in case they are in fact the same Mapping). */
         if( fsmap2 ) astSetInvert( fsmap2, fsinv2 );
         if( fsmap1 ) astSetInvert( fsmap1, fsinv1 );
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
*     #include "switchmap.h"
*     int GetObjSize( AstObject *this, int *status )

*  Class Membership:
*     SwitchMap member function (over-rides the astGetObjSize protected
*     method inherited from the parent class).

*  Description:
*     This function returns the in-memory size of the supplied SwitchMap,
*     in bytes.

*  Parameters:
*     this
*        Pointer to the SwitchMap.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The Object size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstSwitchMap *this;
   int i;
   int result;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointers to the SwitchMap structure. */
   this = (AstSwitchMap *) this_object;

/* Invoke the GetObjSize method inherited from the parent class, and then
   add on any components of the class structure defined by this class
   which are stored in dynamically allocated memory. */
   result = (*parent_getobjsize)( this_object, status );

   result += astGetObjSize( this->fsmap );
   result += astGetObjSize( this->ismap );

   for( i = 0; i < this->nroute; i++ ) {
      result += astGetObjSize( this->routemap[ i ] );
   }

   result += astGetObjSize( this->routeinv );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static AstMapping *GetRoute( AstSwitchMap *this, double sel, int *inv, int *status ){
/*
*  Name:
*     GetRoute

*  Purpose:
*     Return a pointer to a route Mapping, handling all Invert flags.

*  Type:
*     Private function.

*  Synopsis:
*     #include "switchmap.h"
*     AstMapping *GetRoute( AstSwitchMap *this, double sel, int *inv, int *status )

*  Class Membership:
*     SwitchMap method.

*  Description:
*     This function returns a pointer to a route Mapping (specified by a
*     floating point selector value) for the given SwitchMap, taking account
*     of the state of the Invert flag of both the route Mapping and the
*     SwitchMap.

*  Parameters:
*     this
*        Pointer to the SwitchMap.
*     sel
*        The selector value. The nearest integer value (minus 1) is used
*        to index the array of route Mappings stored in the SwitchMap. A
*        NULL pointer is returned if the selector value is out of range.
*     inv
*        Pointer to an int in which to return the original value of the
*        Invert flag of the returned Mapping. The astSetInvert method
*        should be used to re-instate this value once all use of the Mapping
*        has been completed.
*     status
*        Pointer to the inherited status variable.

*  Returns:
*     A pointer to the route Mapping to use. Note, the returned pointer
*     should NOT be annulled when no longer needed. NULL is returned
*     (without error) if the SwitchMap does not have a route Mapping for the
*     requested selector value. The forward transformation of the
*     returned Mapping will implenment the forward transformation of the
*     required route Mapping (and vice-versa).

*/

/* Local Variables: */
   AstMapping *ret;
   int rindex;

/* Initialise */
   ret = NULL;

/* Check the global error status. */
   if ( !astOK ) return ret;

/* Check selector value is good. */
   if( sel != AST__BAD ) {

/* Convert the supplied floating point selector value into an integer
   index into the array of route Mappings held in the supplied SwitchMap. */
      rindex = (int)( sel + 0.5 ) - 1;

/* Return the null pointer if the index is out of range. */
      if( rindex >= 0 && rindex < this->nroute ) {

/* Get the required route Mapping. */
         ret = ( this->routemap )[ rindex ];

/* Return its original invert flag. */
         *inv = astGetInvert( ret );

/* Set the Invert flag back to the value it had when the SwitchMap was
   created. */
         astSetInvert( ret, this->routeinv[ rindex ] );

/* If the SwitchMap has since been inverted, also invert the returned
   route Mapping, so that the forward transformation of the returned
   Mapping implements the forward transformation of the supplied
   SwitchMap (and vice-versa). */
         if( astGetInvert( this ) ) astInvert( ret );
      }
   }

/* Return the pointer. */
   return ret;

}

static AstMapping *GetSelector( AstSwitchMap *this, int fwd, int *inv, int *status ){
/*
*  Name:
*     GetSelector

*  Purpose:
*     Return a pointer to a selector Mapping, handling all Invert flags.

*  Type:
*     Private function.

*  Synopsis:
*     #include "switchmap.h"
*     AstMapping *GetSelector( AstSwitchMap *this, int fwd, int *inv, int *status )

*  Class Membership:
*     SwitchMap method.

*  Description:
*     This function returns a pointer to either the forward or inverse
*     selector Mapping for the given SwitchMap, taking account of the
*     state of the Invert flag of bothe the selector Mapping and the
*     SwitchMap.

*  Parameters:
*     this
*        Pointer to the SwitchMap.
*     fwd
*        If non-zero, return the forward selector Mapping. Otherwise,
*        return the inverse selector Mapping.
*     inv
*        Pointer to an int in which to return the original value of the
*        Invert flag of the returned Mapping. The astSetInvert method
*        should be used to re-instate this value once all use of the Mapping
*        has been completed.
*     status
*        Pointer to the inherited status variable.

*  Returns:
*     A pointer to the selector Mapping to use. Note, the returned pointer
*     should NOT be annulled when no longer needed. NULL is returned
*     (without error) if the SwitchMap does not have a Mapping for the
*     requested selector.

*/

/* Local Variables: */
   AstMapping *ret;
   int swinv;

/* Initialise */
   ret = NULL;

/* Check the global error status. */
   if ( !astOK ) return ret;

/* See if the SwitchMap has been inverted. */
   swinv = astGetInvert( this );

/* If the SwitchMap has been inverted, the forward and inverse selector
   Mappings should be reversed. */
   if( ( !swinv && !fwd ) || ( swinv && fwd ) ){
      ret = this->ismap;
      if( ret ) {
         *inv = astGetInvert( ret );
         astSetInvert( ret, this->isinv );
      }

   } else {
      ret = this->fsmap;
      if( ret ) {
         *inv = astGetInvert( ret );
         astSetInvert( ret, this->fsinv );
      }
   }

   if( ret && swinv ) astInvert( ret );

/* Return the pointer. */
   return ret;

}

void astInitSwitchMapVtab_(  AstSwitchMapVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitSwitchMapVtab

*  Purpose:
*     Initialise a virtual function table for a SwitchMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "switchmap.h"
*     void astInitSwitchMapVtab( AstSwitchMapVtab *vtab, const char *name )

*  Class Membership:
*     SwitchMap vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the SwitchMap class.

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
   will be used (by astIsASwitchMap) to determine if an object belongs to
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
   mapping->Rate = Rate;
   mapping->RemoveRegions = RemoveRegions;

/* Declare the copy constructor, destructor and class dump function. */
   astSetCopy( vtab, Copy );
   astSetDelete( vtab, Delete );
   astSetDump( vtab, Dump, "SwitchMap", "Alternate regionalised Mapping" );

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
*     SwitchMap member function (over-rides the astManageLock protected
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
   AstSwitchMap *this;       /* Pointer to SwitchMap structure */
   int i;                      /* Loop count */
   int result;                 /* Returned status value */

/* Initialise */
   result = 0;

/* Check the supplied pointer is not NULL. */
   if( !this_object ) return result;

/* Obtain a pointers to the SwitchMap structure. */
   this = (AstSwitchMap *) this_object;

/* Invoke the ManageLock method inherited from the parent class. */
   if( !result ) result = (*parent_managelock)( this_object, mode, extra,
                                                fail, status );

/* Invoke the astManageLock method on any Objects contained within
   the supplied Object. */
   if( !result ) result = astManageLock( this->fsmap, mode, extra, fail );
   if( !result ) result = astManageLock( this->ismap, mode, extra, fail );
   for( i = 0; i < this->nroute; i++ ) {
      if( !result ) result = astManageLock( this->routemap[ i ], mode,
                                            extra, fail );
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
*     Simplify a sequence of Mappings containing a SwitchMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     int MapMerge( AstMapping *this, int where, int series, int *nmap,
*                   AstMapping ***map_list, int **invert_list, int *status )

*  Class Membership:
*     SwitchMap method (over-rides the protected astMapMerge method
*     inherited from the Mapping class).

*  Description:
*     This function attempts to simplify a sequence of Mappings by
*     merging a nominated SwitchMap in the sequence with its neighbours,
*     so as to shorten the sequence if possible.
*
*     In many cases, simplification will not be possible and the
*     function will return -1 to indicate this, without further
*     action.
*
*     In most cases of interest, however, this function will either
*     attempt to replace the nominated SwitchMap with one which it
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
*        Pointer to the nominated SwitchMap which is to be merged with
*        its neighbours. This should be a cloned copy of the SwitchMap
*        pointer contained in the array element "(*map_list)[where]"
*        (see below). This pointer will not be annulled, and the
*        SwitchMap it identifies will not be modified by this function.
*     where
*        Index in the "*map_list" array (below) at which the pointer
*        to the nominated SwitchMap resides.
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
   AstSwitchMap *map;
   AstMapping *new;
   int i;
   int nroute;
   int result;
   int fsinv_old;
   int isinv_old;
   int *rinv_old;
   AstMapping *sfsmap;
   AstMapping *sismap;
   int simp;
   AstMapping **srmap;
   AstSwitchMap *swneb;
   int ilo;
   int equal;

/* Initialise.*/
   result = -1;

/* Check the inherited status. */
   if ( !astOK ) return result;

/* Get a pointer to this SwitchMap, and note the number of route Mappings. */
   map = (AstSwitchMap *) this;
   nroute = map->nroute;

/* Temporarily put the Invert flag of all encapsulated Mappings (both
   route and selector) back to the values they had when the SwitchMap was
   created, noting their current values so that they can be re-instated
   later. If the SwitchMap itself has been inverted, swap all the original
   invert flags. */
   if( map->fsmap ) {
      fsinv_old = astGetInvert( map->fsmap );
      astSetInvert( map->fsmap, map->fsinv );
   } else {
      fsinv_old = 0;
   }

   if( map->ismap ) {
      isinv_old = astGetInvert( map->ismap );
      astSetInvert( map->ismap, map->isinv );
   } else {
      isinv_old = 0;
   }

   rinv_old = astMalloc( sizeof( int )*nroute );
   if( astOK ) {
      for( i = 0; i < nroute; i++ ) {
         rinv_old[ i ] = astGetInvert( map->routemap[ i ] );
         astSetInvert( map->routemap[ i ], map->routeinv[ i ] );
      }
   }

/* If possible, merge the SwitchMap with a neighbouring SwitchMap. */
/* =============================================================== */
/* Only do this if we are combining the Mappings in series. */
   if( series ) {

/* Is the higher neighbour a SwitchMap? If so get a pointer to it, and
   note the index of the lower of the two adjacent SwitchMaps. */
      if( where < ( *nmap - 1 ) &&
          astIsASwitchMap( ( *map_list )[ where + 1 ] ) ){
         swneb = (AstSwitchMap *) ( *map_list )[ where + 1 ];
         ilo = where;

/* If not, is the lower neighbour a SwitchMap? If so get a pointer to it, and
   note the index of the lower of the two adjacent SwitchMaps. */
      } else if( where > 0 &&
                 astIsASwitchMap( ( *map_list )[ where - 1 ] ) ){
         swneb = (AstSwitchMap *) ( *map_list )[ where - 1 ];
         ilo =  where - 1;

      } else {
         swneb = NULL;
      }

/* If a neighbouring SwitchMap was found, we can replace the pair by a
   UnitMap if the two SwitchMaps are equal but have opposite values for
   their Invert flags. Temporarily invert the neighbour, then compare
   the two SwitchMaps for equality, then re-invert the neighbour. */
      if( swneb ) {
         astInvert( swneb );
         equal = astEqual( map, swneb );
         astInvert( swneb );

/* If the two SwitchMaps are equal but opposite, annul the first of the two
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

/* Attempt to simplify the SwitchMap on its own. */
/* ============================================= */
/* Only do this if no change was made above. */
   if( result == -1 ) {

/* If the SwitchMap is inverted, create an equal SwitchMap which is not
   inverted. To do this, invert and swap the selector Mappings, and
   invert all the route Mappings. We use astSetInvert rather than astInvert
   because two or more more stored pointers may point to the same Mapping
   in which case that Mapping would be inverted more than once with
   unpredictable results. */
      if( ( *invert_list )[ where ] ) {
         if( map->fsmap ) astSetInvert( map->fsmap, !(map->fsinv) );
         if( map->ismap ) astSetInvert( map->ismap, !(map->isinv) );
         for( i = 0; i < nroute; i++ ) {
            astSetInvert( map->routemap[ i ], !(map->routeinv[ i ]) );
         }

         new = (AstMapping *) astSwitchMap( map->ismap, map->fsmap, nroute, (void **) map->routemap, "", status );

         (void) astAnnul( ( *map_list )[ where ] );
         ( *map_list )[ where ] = (AstMapping *) new;
         ( *invert_list )[ where ] = 0;
         result = where;

/* Otherwise, try to simplify each of the encapsulated Mappings, noting
   if any simplification takes place. */
      } else {
         sfsmap = ( map->fsmap ) ? astSimplify( map->fsmap ) : NULL;
         sismap = ( map->ismap ) ? astSimplify( map->ismap ) : NULL;
         simp = ( sfsmap != map->fsmap ) || ( sismap != map->ismap );

         srmap = astMalloc( sizeof( AstMapping * )*nroute );
         if( astOK ) {
            for( i = 0; i < nroute; i++ ) {
               srmap[ i ] = astSimplify( map->routemap[ i ] );
               simp = simp || ( srmap[ i ] != map->routemap[ i ] );
            }
         }

/* If any simplification took place, construct a new SwitchMap from these
    simplified Mappings. */
         if( simp ) {
            (void) astAnnul( ( *map_list )[ where ] );
            ( *map_list )[ where ] = (AstMapping *) astSwitchMap( sfsmap, sismap,
                                                    nroute, (void **) srmap, "", status );
            result = where;
         }

/* Release resources. */
         if( sfsmap ) sfsmap = astAnnul( sfsmap );
         if( sismap ) sismap = astAnnul( sismap );
         if( srmap ) {
            for( i = 0; i < nroute; i++ ) srmap[ i ] = astAnnul( srmap[ i ] );
            srmap = astFree( srmap );
         }
      }
   }

/* Re-instate the original Invert values for the encapsulated Mappings. */
   if( map->fsmap ) astSetInvert( map->fsmap, fsinv_old );
   if( map->ismap ) astSetInvert( map->ismap, isinv_old );
   if( rinv_old ) {
      for( i = 0; i < nroute; i++ ) {
         astSetInvert( map->routemap[ i ], rinv_old[ i ] );
      }
      rinv_old = astFree( rinv_old );
   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = -1;

/* Return the result. */
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
*     #include "switchmap.h"
*     result = Rate( AstMapping *this, double *at, int ax1, int ax2, int *status )

*  Class Membership:
*     SwitchMap member function (overrides the astRate method inherited
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
   AstSwitchMap *map;
   AstMapping *smap;
   AstMapping *rmap;
   double result;
   double sel;
   int fsinv;
   int rinv;
   int nin;

/* Initialise. */
   result = AST__BAD;

/* Check inherited status */
   if( !astOK ) return result;

/* Get a pointer to the SwitchMap structure. */
   map = (AstSwitchMap *) this;

/* Get a pointer to the effective foward selector Mapping, and its current
   invert flag (this takes account of whether the SwtichMap has been
   inverted or not). This call resets the selector's invert flag temporarily
   back to the value it had when the SwitchMap was created. */
   smap = GetSelector( map, 1, &fsinv, status );

/* If the SwitchMap has no forward selector Mapping, return AST__BAD. */
   if( smap ) {

/* Get the number of inputs */
      nin = astGetNin( smap );

/* Transform the supplied position using the selector Mapping. The output
   value is the selector value that indicates which route Mapping to use. */
      astTranN( smap, 1, nin, 1, at, 1, 1, 1, &sel );

/* Get the index of the route Mapping to use, and check it is valid (if
   not, return AST__BAD if not). This takes account of whether the
   SwitchMap has been inverted, and also temporarily re-instates the
   original value of the route Mapping's Invert flag . */
      rmap = GetRoute( map, sel, &rinv, status );
      if( rmap ) {

/* Use the astRate method of the route Mapping. */
         result = astRate( rmap, at, ax1, ax2 );

/* Reset the Invert flag for the route Mapping. */
         astSetInvert( rmap, rinv );
      }

/* Reset the Invert flag for the selector Mapping. */
      astSetInvert( smap, fsinv );
   }

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
*     #include "switchmap.h"
*     AstMapping *RemoveRegions( AstMapping *this, int *status )

*  Class Membership:
*     SwitchMap method (over-rides the astRemoveRegions method inherited
*     from the Mapping class).

*  Description:
*     This function searches the supplied Mapping (which may be a
*     compound Mapping such as a SwitchMap) for any component Mappings
*     that are instances of the AST Region class. It then creates a new
*     Mapping from which all Regions have been removed. If a Region
*     cannot simply be removed (for instance, if it is a component of a
*     parallel SwitchMap), then it is replaced with an equivalent UnitMap
*     in the returned Mapping.
*
*     The implementation provided by the SwitchMap class invokes the
*     astRemoveRegions method on all the component Mappings, and joins
*     the results together into a new SwitchMap.

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
   AstMapping **temp;            /* Array of new route Mappings */
   AstMapping *newfsmap;         /* New forward selector Mapping */
   AstMapping *newismap;         /* New inverse selector Mapping */
   AstMapping *result;           /* Result pointer to return */
   AstSwitchMap *new;            /* Pointer to new SwitchMap */
   AstSwitchMap *this;           /* Pointer to SwitchMap structure */
   int changed;                  /* Has any mapping been changed? */
   int i;                        /* Loop count */
   int nax;                      /* Number of Frame axes */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the SwitchMap. */
   this = (AstSwitchMap *) this_mapping;

/* Allocate an array to hold the modified Mapping pointers. */
   temp = astMalloc( sizeof( AstMapping *)*( this->nroute ) );
   if( astOK ) {

/* Invoke the astRemoveRegions method on all the component Mappings. */
      changed = 0;
      for( i = 0; i < this->nroute; i++ ) {
         temp[ i ] = astRemoveRegions( this->routemap[ i ] );

/* Note if any Mapping was changed. */
         if( temp[ i ] != this->routemap[ i ] ) {
            changed = 1;

/* The implementation of the astRemoveRegions method provided by the
   Region class returns a Frame rather than a UnitMap. But we need
   Mappings here, not Frames. So if the new Mapping is a Frame, replace
   it with an equivalent UnitMap. */
            if( astIsAFrame( temp[ i ] ) ) {
               nax = astGetNin( temp[ i ] );
               (void) astAnnul( temp[ i ] );
               temp[ i ] = (AstMapping *) astUnitMap( nax, " ", status );
            }
         }
      }

/* And on the other ancillary Mappings */
      if( this->fsmap ) {
         newfsmap = astRemoveRegions( this->fsmap );
         if( newfsmap != this->fsmap ) {
            changed = 1;
            if( astIsAFrame( newfsmap ) ) {
               nax = astGetNin( newfsmap );
               (void) astAnnul( newfsmap );
               newfsmap = (AstMapping *) astUnitMap( nax, " ", status );
            }
         }

      } else {
         newfsmap = NULL;
      }

      if( this->ismap ) {
         newismap = astRemoveRegions( this->ismap );
         if( newismap != this->ismap ) {
            changed = 1;
            if( astIsAFrame( newismap ) ) {
               nax = astGetNin( newismap );
               (void) astAnnul( newismap );
               newismap = (AstMapping *) astUnitMap( nax, " ", status );
            }
         }

      } else {
         newismap = NULL;
      }

/* If no component was modified, just return a clone of the supplied
   pointer. */
      if( ! changed ) {
         result = astClone( this );

/* Otherwise, we need to create a new Mapping to return. We take a deep
   copy of the supplied SwitchMap and then modify the Mappings so that
   we retain any extra information in the supplied SwitchMap. */
      } else {
         new = astCopy( this );

         for( i = 0; i < this->nroute; i++ ) {
            (void) astAnnul( new->routemap[ i ] );
            new->routemap[ i ] = astClone( temp[ i ] );
         }

         if( newfsmap ) {
            (void) astAnnul( new->fsmap );
            new->fsmap = astClone( newfsmap );
         }

         if( newismap ) {
            (void) astAnnul( new->ismap );
            new->ismap = astClone( newismap );
         }

         result = (AstMapping *) new;
      }

/* Free resources. */
      for( i = 0; i < this->nroute; i++ ) {
         temp[ i ] = astAnnul( temp[ i ] );
      }

      if( newfsmap ) newfsmap = astAnnul( newfsmap );
      if( newismap ) newismap = astAnnul( newismap );
   }

   temp = astFree( temp );

/* Annul the returned Mapping if an error has occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return the result. */
   return result;
}

int astSwitchList_( AstSwitchMap *this, int invert, int *nmap,
                    AstMapping ***map_list, int **invert_list, int *status ) {
/*
*+
*  Name:
*     astSwitchList

*  Purpose:
*     Extract the selector and route Mappings from a SwitchMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "switchmap.h"
*     int astSwitchList( AstSwitchMap *this, int invert, int *nmap,
*                        AstMapping ***map_list, int **invert_list )

*  Class Membership:
*     SwitchMap member function.

*  Description:
*     This function extracts the route and selector Mappings form a
*     SwitchMap.

*  Parameters:
*     this
*        Pointer to the SwitchMap to be decomposed (it is not actually
*        modified by this function).
*     invert
*        The value to which the SwitchMap's Invert attribute is to be
*        (notionally) set before performing the decomposition. Normally,
*        the value supplied here will be the actual Invert value obtained
*        from the SwitchMap (e.g. using astGetInvert).  Sometimes, however,
*        when a SwitchMap is encapsulated within another structure, that
*        structure may retain an Invert value (in order to prevent external
*        interference) which should be used instead.
*
*        Note that the actual Invert value of the SwitchMap supplied is
*        not used (or modified) by this function.
*     nmap
*        The address of an int in which to return a count of the number of
*        individual Mappings in the decomposition. The supplied value is
*        ignored.
*     map_list
*        Address of a pointer to an array of Mapping pointers. The value
*        supplied on entry is ignored. On exit, it points at a dynamically
*        allocated array containing Mapping pointers ("*nmap" in number) that
*        result from the decomposition requested.
*
*        The returned Mapping pointers returned will identify the following
*        sequence of Mappings; forward selector mapping (or NULL if the
*        SwitchMap has no forward selector Mapping), inverse selector
*        mapping (or NULL if the SwitchMap has no inverse selector Mapping),
*        the route Mappings in the order they were supplied when the
*        SwitchMap was constructed.
*
*        All the Mapping pointers returned by this function should be
*        annulled by the caller, using astAnnul, when no longer
*        required. The dynamic array holding these pointers should
*        also be freed, using astFree.
*     invert_list
*        Address of a pointer to an array of int. The value supplied on
*        entry is ignored. On exit, it points at a dynamically allocated
*        array containing Invert attribute values ("*nmap" in number) that
*        result from the decomposition requested.
*
*        The returned Invert values returned identify the values which must
*        be assigned to the Invert attributes of the corresponding
*        Mappings (whose pointers are in the "*map_list" array) before
*        they are applied. Note that these values may differ from the
*        actual Invert attribute values of these Mappings, which are
*        not relevant.
*
*        The dynamic array holding these values should be freed by the
*        caller, using astFree, when no longer required.

*  Returned Value:
*     The number of route Mappings stored in the SwitchMap.

*  Notes:
*     - It is unspecified to what extent the original SwitchMap and the
*     individual (decomposed) Mappings are inter-dependent. Consequently,
*     the individual Mappings cannot be modified without risking
*     modification of the original SwitchMap.
*     - If this function is invoked with the global error status set,
*     or if it should fail for any reason, then the *nmap value, the
*     list of Mapping pointers and the list of Invert values will all
*     be returned unchanged.
*-
*/

/* Local Variables: */
   AstMapping *map;              /* Pointer to Mapping to return */
   int inv;                      /* Original Invert flag for Mapping */
   int i;                        /* Route Mapping index */
   int oldinv;                   /* Original Invert flag for SwitchMap */
   int result;                   /* Returned value */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Store the numbe of route Mappings */
   result = this->nroute;
   *nmap = result + 2;

/* Allocate the required arrays. */
   *map_list = astMalloc( sizeof( AstMapping * )*(size_t) *nmap );
   *invert_list = astMalloc( sizeof( int )*(size_t) *nmap );

/* Check the pointers can be used safely. */
   if( astOK ) {

/* Temporaily set the requested Invert flag for the SwitchMap. */
      oldinv = astGetInvert( this );
      astSetInvert( this, invert );

/* Get the forward selector Mapping. */
      map = GetSelector( this, 1, &inv, status );

/* If the SwitchMap has a forward selector Mapping, return a clone of the
   Mapping pointer, and the invert flag to be used with it, then
   re-instate the original invert flag value (which was modified by
   GetSelector). */
      if( map ) {
         ( *map_list )[ 0 ] = astClone( map );
         ( *invert_list )[ 0 ] = astGetInvert( map );
         astSetInvert( map, inv );

/* If the SwitchMap does not has a forward selector Mapping, return a
   NULL pointer. */
      } else {
         ( *map_list )[ 0 ] = NULL;
         ( *invert_list )[ 0 ] = 0;
      }

/* Likewise, get and return the inverse selector Mapping.*/
      map = GetSelector( this, 0, &inv, status );
      if( map ) {
         ( *map_list )[ 1 ] = astClone( map );
         ( *invert_list )[ 1 ] = astGetInvert( map );
         astSetInvert( map, inv );
      } else {
         ( *map_list )[ 1 ] = NULL;
         ( *invert_list )[ 1 ] = 0;
      }

/* Loop round all route Mappings. */
      for( i = 0; i < result; i++ ){

/* Get the next route Mapping. */
         map = GetRoute( this, (double) i + 1.0, &inv, status );

/* If the SwitchMap has a route Mapping for the current selector value,
   return a clone of the Mapping pointer, and the invert flag to be used
   with it, then re-instate the original invert flag value (which was
   modified by GetRoute). */
         if( map ) {
            ( *map_list )[ i + 2 ] = astClone( map );
            ( *invert_list )[ i + 2 ] = astGetInvert( map );
            astSetInvert( map, inv );

/* If the SwitchMap does not has a route Mapping for the current selector
   value, return a NULL pointer. */
         } else {
            ( *map_list )[ i + 2 ] = NULL;
            ( *invert_list )[ i + 2 ] = 0;
         }

      }

/* Re-instate the original Ivert flag for the SwitchMap. */
      astSetInvert( this, oldinv );

   }

/* If an error has occurred, free the returned arrays. */
   if( !astOK ) {
      *map_list = astFree( *map_list );
      *invert_list= astFree( *invert_list );
      result= 0;
      *nmap = 0;
   }

/* Return the result */
   return result;
}

static AstPointSet *Transform( AstMapping *this, AstPointSet *in,
                               int forward, AstPointSet *out, int *status ) {
/*
*  Name:
*     Transform

*  Purpose:
*     Apply a SwitchMap to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "switchmap.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     SwitchMap member function (over-rides the astTransform method inherited
*     from the Mapping class).

*  Description:
*     This function takes a SwitchMap and a set of points encapsulated in a
*     PointSet and transforms the points so as to apply the required Mapping.
*     This implies applying each of the SwitchMap's component Mappings in turn,
*     either in series or in parallel.

*  Parameters:
*     this
*        Pointer to the SwitchMap.
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
*     match the number of coordinates for the SwitchMap being applied.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstMapping *rmap;
   AstMapping *selmap;
   AstPointSet *ps1;
   AstPointSet *ps1a;
   AstPointSet *ps2;
   AstPointSet *ps2a;
   AstPointSet *result;
   AstPointSet *selps;
   AstSwitchMap *map;
   double **in_ptr;
   double **out_ptr;
   double **ptr1;
   double **ptr2;
   double **sel_ptr;
   double *outv;
   double *sel;
   int *popmap;
   int iroute;
   int ipoint;
   int j;
   int k;
   int maxpop;
   int ncin;
   int ncout;
   int npoint;
   int nroute;
   int rindex;
   int rinv;
   int selinv;
   int totpop;

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the SwitchMap. */
   map = (AstSwitchMap *) this;

/* Apply the parent Mapping using the stored pointer to the Transform member
   function inherited from the parent Mapping class. This function validates
   all arguments and generates an output PointSet if necessary, but does not
   actually transform any coordinate values. */
   result = (*parent_transform)( this, in, forward, out, status );

/* We now extend the parent astTransform method by applying the component
   Mappings of the SwitchMap to generate the output coordinate values. */

/* Get the number of input and output coords. */
   if( forward ) {
      ncin = astGetNin( this );
      ncout = astGetNout( this );
   } else {
      ncin = astGetNout( this );
      ncout = astGetNin( this );
   }

/* Get the appropriate selector Mapping. */
   selmap = GetSelector( map, forward, &selinv, status );

/* Transform the supplied positions using the above selector Mapping. */
   selps = astTransform( selmap, in, forward, NULL );

/* Get a pointer to the array holding the selector value. */
   sel_ptr = astGetPoints( selps );

/* Get a pointer to the array holding the input values. */
   in_ptr = astGetPoints( in );

/* Get a pointer to the array in which to store the results, and the total
   number of points being transformed. */
   out_ptr = astGetPoints( result );
   npoint = astGetNpoint( result );

/* We now count how many positions are to be tranformed by each of the
   route Mappings. */
   nroute = map->nroute;
   popmap = astMalloc( sizeof( int )*nroute );
   if( astOK ) {
      for( iroute = 0; iroute < nroute; iroute++ ) popmap[ iroute ] = 0;

      sel = sel_ptr[ 0 ];
      for( ipoint = 0; ipoint < npoint; ipoint++,sel++ ) {
         if( *sel != AST__BAD ) {
            rindex = (int)( *sel + 0.5 ) - 1;
            if( rindex >= 0 && rindex < nroute ) ( popmap[ rindex ] )++;
         }
      }

/* Find the number of points transformed by the most popular route Mapping.
   Also find the total number of points transformed by any route Mapping. */
      totpop = 0;
      maxpop = 0;
      for( iroute = 0; iroute < nroute; iroute++ ) {
         if( popmap[ iroute ] > maxpop ) maxpop = popmap[ iroute ];
         totpop += popmap[ iroute ];
      }
      if( maxpop == 0 ) maxpop = 1;

/* If some of the points are not transformed by any route Mapping.
   Initialise the whole output array to hold AST__BAD at every point. */
      if( totpop < npoint ) {
         for( j = 0; j < ncout; j++ ) {
            outv = out_ptr[ j ];
            for( ipoint = 0; ipoint < npoint; ipoint++ ) *(outv++) = AST__BAD;
         }
      }

/* Create a PointSet large enough to hold all the supplied positions
   which are to be transformed by the most popular route Mapping. */
      ps1 = astPointSet( maxpop, ncin, "", status );
      ptr1 = astGetPoints( ps1 );

/* Create a PointSet large enough to hold all the output positions
   created by the most popular route Mapping. */
      ps2 = astPointSet( maxpop, ncout, "", status );
      ptr2 = astGetPoints( ps2 );
      if( astOK ) {

/* Loop round each route Mapping which is used by at least 1 point. */
         for( iroute = 0; iroute < nroute; iroute++ ) {
            if( popmap[ iroute ] >0 ) {
               rmap = GetRoute( map, (double)( iroute + 1 ), &rinv, status );

/* Construct two PointSets of the correct size to hold the input and
   output points to be processed with the current route Mapping. We
   re-use the memory allocated for the largest route Mapping's PointSet. */
               if( popmap[ iroute ] != maxpop ) {
                  ps1a = astPointSet( popmap[ iroute ], ncin, "", status );
                  astSetPoints( ps1a, ptr1 );
                  ps2a = astPointSet( popmap[ iroute ], ncout, "", status );
                  astSetPoints( ps2a, ptr2 );
               } else {
                  ps1a = astClone( ps1 );
                  ps2a = astClone( ps2 );
               }

/* Fill the input PointSet with the input positions which are to be
   transformed using the current route Mapping. */
               sel = sel_ptr[ 0 ];
               k = 0;
               for( ipoint = 0; ipoint < npoint; ipoint++,sel++ ) {
                  if( *sel != AST__BAD ) {
                     rindex = (int)( *sel + 0.5 ) - 1;
                     if( rindex == iroute ) {
                        for( j = 0; j < ncin; j++ ) {
                           ptr1[ j ][ k ] = in_ptr[ j ][ ipoint ];
                        }
                        k++;
                     }
                  }
               }

/* Use the route Mapping to transform this PointSet. */
               (void) astTransform( rmap, ps1a, forward, ps2a );

/* Copy the axis values from the resulting PointSet back into the results
   array. */
               sel = sel_ptr[ 0 ];
               k = 0;
               for( ipoint = 0; ipoint < npoint; ipoint++,sel++ ) {
                  if( *sel != AST__BAD ) {
                     rindex = (int)( *sel + 0.5 ) - 1;
                     if( rindex == iroute ) {
                        for( j = 0; j < ncout; j++ ) {
                           out_ptr[ j ][ ipoint ] = ptr2[ j ][ k ];
                        }
                        k++;
                     }
                  }
               }

/* Free resources. */
               ps1a = astAnnul( ps1a );
               ps2a = astAnnul( ps2a );

/* Re-instate the Invert flag for the route Mapping. */
               astSetInvert( rmap, rinv );
            }
         }
      }

/* Free resources. */
      ps1 = astAnnul( ps1 );
      ps2 = astAnnul( ps2 );
   }

   selps = astAnnul( selps );
   popmap = astFree( popmap );

/* Re-instate the Invert flag of the selector Mapping. */
   astSetInvert( selmap, selinv );

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
*     Copy constructor for SwitchMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for SwitchMap objects.

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
*     Mappings within the SwitchMap.
*/

/* Local Variables: */
   AstSwitchMap *in;                /* Pointer to input SwitchMap */
   AstSwitchMap *out;               /* Pointer to output SwitchMap */
   int i;                           /* Loop count */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output SwitchMaps. */
   in = (AstSwitchMap *) objin;
   out = (AstSwitchMap *) objout;

/* For safety, start by clearing any references to the input component
   Mappings,etc, from the output SwitchMap. */
   out->fsmap = NULL;
   out->ismap = NULL;
   out->routemap = NULL;
   out->routeinv = NULL;

/* Make copies of these Mappings, etc, and store pointers to them in the output
   SwitchMap structure. */
   if( in->fsmap ) out->fsmap = astCopy( in->fsmap );
   if( in->ismap ) out->ismap = astCopy( in->ismap );

   out->routemap = astMalloc( sizeof( AstMapping * )*( in->nroute ) );
   out->routeinv = astMalloc( sizeof( int )*( in->nroute ) );
   if( astOK ) {
      for( i = 0; i < in->nroute; i++ ) {
         out->routemap[ i ] = astCopy( in->routemap[ i ] );
         out->routeinv[ i ] = in->routeinv[ i ];
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
*     Destructor for SwitchMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for SwitchMap objects.

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
   AstSwitchMap *this;              /* Pointer to SwitchMap */
   int i;

/* Obtain a pointer to the SwitchMap structure. */
   this = (AstSwitchMap *) obj;

/* Free dynamically allocated resources. */
   if( this->fsmap ) this->fsmap = astAnnul( this->fsmap );
   if( this->ismap ) this->ismap = astAnnul( this->ismap );
   for( i = 0; i < this->nroute; i++ ) {
      this->routemap[ i ] = astAnnul( this->routemap[ i ] );
   }
   this->routemap = astFree( this->routemap );
   this->routeinv = astFree( this->routeinv );

/* Clear the remaining SwitchMap variables. */
   this->nroute = 0;
   this->fsinv = 0;
   this->isinv = 0;
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for SwitchMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the SwitchMap class to an output Channel.

*  Parameters:
*     this
*        Pointer to the SwitchMap whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstSwitchMap *this;
   int ival;
   int set;
   int i;
   char buf[ 20 ];

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the SwitchMap structure. */
   this = (AstSwitchMap *) this_object;

/* Write out values representing the instance variables for the SwitchMap
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

/* Forward selector Mapping */
/* ------------------------ */
   if( this->fsmap ) {
      astWriteObject( channel, "FSMap", 1, 1, this->fsmap,
                      "Forward selector Mapping" );

/* Forward selector Invert flag. */
/* ----------------------------- */
      ival = this->fsinv;
      set = ( ival != 0 );
      astWriteInt( channel, "FSInv", set, 0, ival,
                   ival ? "Fwd selector used in inverse direction" :
                          "Fwd selector used in forward direction" );
   }


/* Inverse selector Mapping */
/* ------------------------ */
   if( this->ismap ) {
      astWriteObject( channel, "ISMap", 1, 1, this->ismap,
                      "Inverse selector Mapping" );

/* Forward selector Invert flag. */
/* ----------------------------- */
      ival = this->isinv;
      set = ( ival != 0 );
      astWriteInt( channel, "ISInv", set, 0, ival,
                   ival ? "Inv selector used in inverse direction" :
                          "Inv selector used in forward direction" );
   }

/* Loop to dump each route Mapping and its invert flag. */
/* ---------------------------------------------------- */
   for( i = 0; i < this->nroute; i++ ) {
      sprintf( buf, "RMap%d", i + 1 );
      astWriteObject( channel, buf, 1, 1, this->routemap[ i ],
                      "Route Mapping" );

      ival = this->routeinv[ i ];
      set = ( ival != 0 );
      sprintf( buf, "RInv%d", i + 1 );
      astWriteInt( channel, buf, set, 0, ival,
                   ival ? "Route Mapping used in inverse direction" :
                          "Route Mapping used in forward direction" );
   }

}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsASwitchMap and astCheckSwitchMap functions using the
   macros defined for this purpose in the "object.h" header file. */
astMAKE_ISA(SwitchMap,Mapping)
astMAKE_CHECK(SwitchMap)

AstSwitchMap *astSwitchMap_( void *fsmap_void, void *ismap_void, int nroute,
                             void **routemaps_void, const char *options, int *status, ...) {
/*
*+
*  Name:
*     astSwitchMap

*  Purpose:
*     Create a SwitchMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "switchmap.h"
*     AstSwitchMap *astSwitchMap( AstMapping *fsmap, AstMapping *ismap,
*                                 int nroute, AstMapping **routemaps,
*                                 const char *options, ... )

*  Class Membership:
*     SwitchMap constructor.

*  Description:
*     This function creates a new SwitchMap and optionally initialises its
*     attributes.

*  Parameters:
*     fsmap
*        Pointer to the forward selector Mapping
*     ismap
*        Pointer to the inverse selector Mapping
*     nroute
*        The number of route Mappings.
*     routemaps
*        An array of pointers to the route Mappings.
*     options
*        Pointer to a null terminated string containing an optional
*        comma-separated list of attribute assignments to be used for
*        initialising the new SwitchMap. The syntax used is the same as for the
*        astSet method and may include "printf" format specifiers identified
*        by "%" symbols in the normal way.
*     ...
*        If the "options" string contains "%" format specifiers, then an
*        optional list of arguments may follow it in order to supply values to
*        be substituted for these specifiers. The rules for supplying these
*        are identical to those for the astSet method (and for the C "printf"
*        function).

*  Returned Value:
*     A pointer to the new SwitchMap.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-

*  Implementation Notes:
*     - This function implements the basic SwitchMap constructor which is
*     available via the protected interface to the SwitchMap class.  A
*     public interface is provided by the astSwitchMapId_ function.
*     - Because this function has a variable argument list, it is
*     invoked by a macro that evaluates to a function pointer (not a
*     function invocation) and no checking or casting of arguments is
*     performed before the function is invoked. Because of this, the
*     "map1" and "map2" parameters are of type (void *) and are
*     converted and validated within the function itself.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstSwitchMap *new;            /* Pointer to new SwitchMap */
   AstMapping *fsmap;            /* Pointer to fwd selector Mapping */
   AstMapping *ismap;            /* Pointer to inv selector Mapping */
   AstMapping **routemaps;       /* Array of route Mapping pointers */
   int i;                        /* Route Mappings index */
   va_list args;                 /* Variable argument list */

/* Initialise. */
   new = NULL;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return new;

/* Report an error if no route Mappings have been supplied. */
   if( nroute <= 0 ) astError( AST__BDPAR, "astSwitchMap(SwitchMap): "
                               "Bad number of route Mappings (%d) specified.", status,
                               nroute );

/* Otherwise create an array to hold the route Mapping pointers. */
   routemaps = astMalloc( sizeof( AstMapping * )*nroute );

/* Obtain and validate pointers to the Mapping structures provided. */
   if( astOK ) {
      fsmap = fsmap_void ? astCheckMapping( fsmap_void ) : NULL;
      ismap = ismap_void ? astCheckMapping( ismap_void ) : NULL;
      for( i = 0; i < nroute; i++ ) {
         routemaps[ i ] = astCheckMapping( routemaps_void[ i ] );
      }
   }

   if ( astOK ) {

/* Initialise the SwitchMap, allocating memory and initialising the
   virtual function table as well if necessary. */
      new = astInitSwitchMap( NULL, sizeof( AstSwitchMap ), !class_init, &class_vtab,
                              "SwitchMap", fsmap, ismap, nroute, routemaps );

/* If successful, note that the virtual function table has been
   initialised. */
      if ( astOK ) {
         class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new SwitchMap's
   attributes. */
         va_start( args, status );
         astVSet( new, options, NULL, args );
         va_end( args );

/* If an error occurred, clean up by deleting the new object. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Free memory used to hold the route Mapping pointers. */
   routemaps = astFree( routemaps );

/* Return a pointer to the new SwitchMap. */
   return new;
}

AstSwitchMap *astSwitchMapId_( void *fsmap_void, void *ismap_void, int nroute,
                               void **routemaps_void, const char *options, ... ) {
/*
*++
*  Name:
c     astSwitchMap
f     AST_SWITCHMAP

*  Purpose:
*     Create a SwitchMap.

*  Type:
*     Public function.

*  Synopsis:
c     #include "switchmap.h"
c     AstSwitchMap *astSwitchMap( AstMapping *fsmap, AstMapping *ismap,
c                                 int nroute, AstMapping *routemaps[],
c                                 const char *options, ... )
f     RESULT = AST_SWITCHMAP( FSMAP, ISMAP, NROUTE, ROUTEMAPS, OPTIONS,
f                             STATUS )

*  Class Membership:
*     SwitchMap constructor.

*  Description:
*     This function creates a new SwitchMap and optionally initialises
*     its attributes.
*
*     A SwitchMap is a Mapping which represents a set of alternate
*     Mappings, each of which is used to transform positions within a
*     particular region of the input or output coordinate system of the
*     SwitchMap.
*
*     A SwitchMap can encapsulate any number of Mappings, but they must
*     all have the same number of inputs (Nin attribute value) and the
*     same number of outputs (Nout attribute value). The SwitchMap itself
*     inherits these same values for its Nin and Nout attributes. Each of
*     these Mappings represents a "route" through the switch, and are
*     referred to as "route" Mappings below. Each route Mapping transforms
*     positions between the input and output coordinate space of the entire
*     SwitchMap, but only one Mapping will be used to transform any given
*     position. The selection of the appropriate route Mapping to use with
*     any given input position is made by another Mapping, called the
*     "selector" Mapping. Each SwitchMap encapsulates two selector
*     Mappings in addition to its route Mappings; one for use with the
*     SwitchMap's forward transformation (called the "forward selector
*     Mapping"), and one for use with the SwitchMap's inverse transformation
*     (called the "inverse selector Mapping"). The forward selector Mapping
*     must have the same number of inputs as the route Mappings, but
*     should have only one output. Likewise, the inverse selector Mapping
*     must have the same number of outputs as the route Mappings, but
*     should have only one input.
*
*     When the SwitchMap is used to transform a position in the forward
*     direction (from input to output), each supplied input position is
*     first transformed by the forward transformation of the forward selector
*     Mapping. This produces a single output value for each input position
*     referred to as the selector value. The nearest integer to the selector
*     value is found, and is used to index the array of route Mappings (the
*     first supplied route Mapping has index 1, the second route Mapping has
*     index 2, etc). If the nearest integer to the selector value is less
*     than 1 or greater than the number of route Mappings, then the SwitchMap
*     output position is set to a value of AST__BAD on every axis. Otherwise,
*     the forward transformation of the selected route Mapping is used to
*     transform the supplied input position to produce the SwitchMap output
*     position.
*
*     When the SwitchMap is used to transform a position in the inverse
*     direction (from "output" to "input"), each supplied "output" position
*     is first transformed by the inverse transformation of the inverse
*     selector Mapping. This produces a selector value for each "output"
*     position. Again, the nearest integer to the selector value is found,
*     and is used to index the array of route Mappings. If this selector
*     index value is within the bounds of the array of route Mappings, then
*     the inverse transformation of the selected route Mapping is used to
*     transform the supplied "output" position to produce the SwitchMap
*     "input" position. If the selector index value is outside the bounds
*     of the array of route Mappings, then the SwitchMap "input" position is
*     set to a value of AST__BAD on every axis.
*
*     In practice, appropriate selector Mappings should be chosen to
*     associate a different route Mapping with each region of coordinate
*     space. Note that the SelectorMap class of Mapping is particularly
*     appropriate for this purpose.
*
*     If a compound Mapping contains a SwitchMap in series with its own
*     inverse, the combination of the two adjacent SwitchMaps will be
*     replaced by a UnitMap when the compound Mapping is simplified using
c     astSimplify.
f     AST_SIMPLIFY.

*  Parameters:
c     fsmap
f     FSMAP = INTEGER (Given)
*        Pointer to the forward selector Mapping. This must have a
*        defined forward transformation, but need not have a defined
*        inverse transformation. It must have one output, and the number of
*        inputs must match the number of inputs of each of the supplied
*        route Mappings.
c        NULL
f        AST__NULL
*        may be supplied, in which case the SwitchMap will have an undefined
*        forward Mapping.
c     ismap
f     ISMAP = INTEGER (Given)
*        Pointer to the inverse selector Mapping. This must have a
*        defined inverse transformation, but need not have a defined
*        forward transformation. It must have one input, and the number of
*        outputs must match the number of outputs of each of the supplied
*        route Mappings.
c        NULL
f        AST__NULL
*        may be supplied, in which case the SwitchMap will have an undefined
*        inverse Mapping.
c     nroute
f     NROUTE = INTEGER (Given)
*        The number of supplied route Mappings.
c     routemaps
f     ROUTEMAPS( NROUTE ) = INTEGER (Given)
*        An array of pointers to the route Mappings. All the supplied
*        route Mappings must have common values for the Nin and Nout
*        attributes, and these values define the number of inputs and
*        outputs of the SwitchMap.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new SwitchMap. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new SwitchMap. The syntax used is identical to that for the
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
c     astSwitchMap()
f     AST_SWITCHMAP = INTEGER
*        A pointer to the new SwitchMap.

*  Notes:
c     - Note that the component Mappings supplied are not copied by
c     astSwitchMap (the new SwitchMap simply retains a reference to
c     them). They may continue to be used for other purposes, but
c     should not be deleted. If a SwitchMap containing a copy of its
c     component Mappings is required, then a copy of the SwitchMap should
c     be made using astCopy.
f     - Note that the component Mappings supplied are not copied by
f     AST_SWITCHMAP (the new SwitchMap simply retains a reference to
f     them). They may continue to be used for other purposes, but
f     should not be deleted. If a SwitchMap containing a copy of its
f     component Mappings is required, then a copy of the SwitchMap should
f     be made using AST_COPY.
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--

*  Implementation Notes:
*     - This function implements the external (public) interface to
*     the astSwitchMap constructor function. It returns an ID value
*     (instead of a true C pointer) to external users, and must be
*     provided because astSwitchMap_ has a variable argument list which
*     cannot be encapsulated in a macro (where this conversion would
*     otherwise occur).
*     - Because no checking or casting of arguments is performed
*     before the function is invoked, the "map1" and "map2" parameters
*     are of type (void *) and are converted from an ID value to a
*     pointer and validated within the function itself.
*     - The variable argument list also prevents this function from
*     invoking astSwitchMap_ directly, so it must be a re-implementation
*     of it in all respects, except for the conversions between IDs
*     and pointers on input/output of Objects.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstSwitchMap *new;            /* Pointer to new SwitchMap */
   AstMapping *fsmap;            /* Pointer to fwd selector Mapping */
   AstMapping *ismap;            /* Pointer to inv selector Mapping */
   AstMapping **routemaps;       /* Array of route Mapping pointers */
   int i;                        /* Route Mappings index */
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

/* Report an error if no route Mappings have been supplied. */
   if( nroute <= 0 ) astError( AST__BDPAR, "astSwitchMap(SwitchMap): "
                               " Bad number of route Mappings (%d) specified.", status,
                               nroute );

/* Otherwise create an array to hold the route Mapping pointers. */
   routemaps = astMalloc( sizeof( AstMapping * )*nroute );

/* Obtain and validate pointers to the Mapping structures provided. */
   if( astOK ) {
      fsmap = fsmap_void ? astCheckMapping( astMakePointer(fsmap_void) ) : NULL;
      ismap = ismap_void ? astCheckMapping( astMakePointer(ismap_void) ) : NULL;
      for( i = 0; i < nroute; i++ ) {
         routemaps[ i ] = astVerifyMapping( astMakePointer(routemaps_void[ i ]) );
      }
   }

   if ( astOK ) {

/* Initialise the SwitchMap, allocating memory and initialising the
   virtual function table as well if necessary. */
      new = astInitSwitchMap( NULL, sizeof( AstSwitchMap ), !class_init, &class_vtab,
                              "SwitchMap", fsmap, ismap, nroute, routemaps );

/* If successful, note that the virtual function table has been
   initialised. */
      if ( astOK ) {
         class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new SwitchMap's
   attributes. */
         va_start( args, options );
         astVSet( new, options, NULL, args );
         va_end( args );

/* If an error occurred, clean up by deleting the new object. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Free memory used to hold the route Mapping pointers. */
   routemaps = astFree( routemaps );

/* Return an ID value for the new SwitchMap. */
   return astMakeId( new );
}

AstSwitchMap *astInitSwitchMap_( void *mem, size_t size, int init,
                                 AstSwitchMapVtab *vtab, const char *name,
                                 AstMapping *fsmap, AstMapping *ismap,
                                 int nroute, AstMapping **routemaps, int *status ) {
/*
*+
*  Name:
*     astInitSwitchMap

*  Purpose:
*     Initialise a SwitchMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "switchmap.h"
*     AstSwitchMap *astInitSwitchMap( void *mem, size_t size, int init,
*                                     AstSwitchMapVtab *vtab, const char *name,
*                                     AstMapping *fsmap, AstMapping *ismap,
*                                     int nroute, AstMapping **routemaps )

*  Class Membership:
*     SwitchMap initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new SwitchMap object. It allocates memory (if necessary) to
*     accommodate the SwitchMap plus any additional data associated with the
*     derived class. It then initialises a SwitchMap structure at the start
*     of this memory. If the "init" flag is set, it also initialises the
*     contents of a virtual function table for a SwitchMap at the start of
*     the memory passed via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the SwitchMap is to be initialised.
*        This must be of sufficient size to accommodate the SwitchMap data
*        (sizeof(SwitchMap)) plus any data used by the derived class. If a
*        value of NULL is given, this function will allocate the memory itself
*        using the "size" parameter to determine its size.
*     size
*        The amount of memory used by the SwitchMap (plus derived class
*        data). This will be used to allocate memory if a value of NULL is
*        given for the "mem" parameter. This value is also stored in the
*        SwitchMap structure, so a valid value must be supplied even if not
*        required for allocating memory.
*     init
*        A logical flag indicating if the SwitchMap's virtual function table
*        is to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new SwitchMap.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the Object
*        astClass function).
*     fsmap
*        Pointer to the forward selector Mapping.
*     ismap
*        Pointer to the inverse selector Mapping.
*     nroute
*        The number of route Mappings supplied.
*     routemaps
*        An array holdiong pointers to the route Mappings.

*  Returned Value:
*     A pointer to the new SwitchMap.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstSwitchMap *new;            /* Pointer to new SwitchMap */
   int i;                        /* Loop count */
   int nin;                      /* No. input coordinates for SwitchMap */
   int nout;                     /* No. output coordinates for SwitchMap */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitSwitchMapVtab( vtab, name );

/* Initialise. */
   new = NULL;

/* Check that all route Mappings have common values for Nin and Nout.*/
   nin = astGetNin( routemaps[ 0 ] );
   nout = astGetNout( routemaps[ 0 ] );
   for( i = 1; i < nroute; i++ ) {
      if( nin != astGetNin( routemaps[ i ] ) ){
         if( astOK ) {
            astError( AST__BADNI, "astInitSwitchMap(%s): Route Mapping "
                      "number %d has %d input(s) but the first route "
                      "Mapping has %d input(s).", status, name, i + 1,
                      astGetNin( routemaps[ i ] ), nin );
         }

      } else if( nout != astGetNout( routemaps[ i ] ) ){
         if( astOK ) {
            astError( AST__BADNO, "astInitSwitchMap(%s): Route Mapping "
                      "number %d has %d output(s) but the first route "
                      "Mapping has %d output(s).", status, name, i + 1,
                      astGetNin( routemaps[ i ] ), nin );
         }
      }
   }

/* If supplied, report an error if fsmap has no forward transformation,
   or if it has an incorrect number of inputs or output. */
   if( fsmap && astOK ) {
      if( !astGetTranForward( fsmap ) ) {
         astError( AST__INTRD, "astInitSwitchMap(%s): The forward selector Mapping "
              "is not able to transform coordinates in the forward direction.", status,
              name );

      } else if( astGetNin( fsmap ) != nin ){
         astError( AST__BADNI, "astInitSwitchMap(%s): The forward selector "
                   "Mapping has %d input(s) but the SwitchMap has %d "
                   "input(s).", status, name, astGetNin( fsmap ), nin );

      } else if( astGetNout( fsmap ) != 1 ){
         astError( AST__BADNO, "astInitSwitchMap(%s): The forward selector "
                   "Mapping has %d outputs but should only have 1.", status, name,
                   astGetNout( fsmap ) );
      }
   }

/* If supplied, report an error if ismap has no inverse transformation,
   or if it has an incorrect number of inputs or outputs. */
   if( ismap && astOK ) {
      if( !astGetTranInverse( ismap ) ) {
         astError( AST__INTRD, "astInitSwitchMap(%s): The inverse selector Mapping "
              "is not able to transform coordinates in the inverse direction.", status,
              name );
      } else if( nout != astGetNout( ismap ) ){
         astError( AST__BADNO, "astInitSwitchMap(%s): The inverse selector "
                   "Mapping has %d output(s) but the SwitchMap has %d "
                   "output(s).", status, name, astGetNout( ismap ), nout );

      } else if( astGetNin( ismap ) != 1 ){
         astError( AST__BADNI, "astInitSwitchMap(%s): The inverse selector "
                   "Mapping has %d inputs but should only have 1.", status, name,
                   astGetNin( ismap ) );

      }
   }

/* Report an error if neither ismap nor fsmap were supplied. */
   if( !fsmap && !ismap && astOK ) {
      astError( AST__INTRD, "astInitSwitchMap(%s): No selector Mappings "
                "supplied.", status, name );
   }

/* Initialise a Mapping structure (the parent class) as the first component
   within the SwitchMap structure, allocating memory if necessary. Specify
   the number of input and output coordinates and in which directions the
   Mapping should be defined. */
   if ( astOK ) {
      new = (AstSwitchMap *) astInitMapping( mem, size, 0,
                                             (AstMappingVtab *) vtab, name,
                                             nin, nout,
                                             ( fsmap != NULL ),
                                             ( ismap != NULL ) );
      if ( astOK ) {

/* Initialise the SwitchMap data. */
/* --------------------------- */
/* Store pointers to the selector Mappings. */
         new->fsmap = fsmap ? astClone( fsmap ) : NULL;
         new->ismap = ismap ? astClone( ismap ) : NULL;

/* Save the initial values of the inversion flags for these Mappings. */
         new->fsinv = fsmap ? astGetInvert( fsmap ) : 0;
         new->isinv = ismap ? astGetInvert( ismap ) : 0;

/* Create arrays for the route Mappings. */
         new->routemap = astMalloc( sizeof( AstMapping * )*nroute );
         new->routeinv = astMalloc( sizeof( int )*nroute );

/* Store pointers to the route Mappings and their invert flags. */
         if( astOK ) {
            new->nroute = nroute;
            for( i = 0; i < nroute; i++ ) {
               new->routemap[ i ] = astClone( routemaps[ i ] );
               new->routeinv[ i ] = astGetInvert( routemaps[ i ] );
            }
         } else {
            new->nroute = 0;
         }

/* If an error occurred, clean up by deleting the new object. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Return a pointer to the new object. */
   return new;
}

AstSwitchMap *astLoadSwitchMap_( void *mem, size_t size,
                                 AstSwitchMapVtab *vtab, const char *name,
                                 AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadSwitchMap

*  Purpose:
*     Load a SwitchMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "switchmap.h"
*     AstSwitchMap *astLoadSwitchMap( void *mem, size_t size,
*                                     AstSwitchMapVtab *vtab, const char *name,
*                                     AstChannel *channel )

*  Class Membership:
*     SwitchMap loader.

*  Description:
*     This function is provided to load a new SwitchMap using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     SwitchMap structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a SwitchMap at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the SwitchMap is to be
*        loaded.  This must be of sufficient size to accommodate the
*        SwitchMap data (sizeof(SwitchMap)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the SwitchMap (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the SwitchMap structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstSwitchMap) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new SwitchMap. If this is NULL, a pointer to
*        the (static) virtual function table for the SwitchMap class is
*        used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "SwitchMap" is used instead.

*  Returned Value:
*     A pointer to the new SwitchMap.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstSwitchMap *new;
   AstMapping *rmap;
   int i;
   char buf[ 20 ];

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this SwitchMap. In this case the
   SwitchMap belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstSwitchMap );
      vtab = &class_vtab;
      name = "SwitchMap";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitSwitchMapVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built SwitchMap. */
   new = astLoadMapping( mem, size, (AstMappingVtab *) vtab, name,
                         channel );
   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "SwitchMap" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* Forward Selector Mapping and its Invert flag. */
/* --------------------------------------------- */
      new->fsmap = astReadObject( channel, "fsmap", NULL );
      new->fsinv = astReadInt( channel, "fsinv", 0 );
      new->fsinv = ( new->fsinv != 0 );

/* Inverse Selector Mapping and its Invert flag. */
/* --------------------------------------------- */
      new->ismap = astReadObject( channel, "ismap", NULL );
      new->isinv = astReadInt( channel, "isinv", new->fsinv );
      new->isinv = ( new->isinv != 0 );

/* Loop to load each route Mapping and its invert flag. */
/* ---------------------------------------------------- */
      new->routemap = NULL;
      new->routeinv = NULL;
      i = 0;
      while( astOK ) {
         sprintf( buf, "rmap%d", i + 1 );
         rmap = astReadObject( channel, buf, NULL );
         if( rmap ) {
            new->routemap = astGrow( new->routemap, i + 1, sizeof( AstMapping *) );
            new->routeinv = astGrow( new->routeinv, i + 1, sizeof( int ) );
            if( astOK ) {
               new->routemap[ i ] = rmap;
               sprintf( buf, "rinv%d", i + 1 );
               new->routeinv[ i ] = astReadInt( channel, buf, 0 );
               new->routeinv[ i ] = ( new->routeinv[ i ] != 0 );
               i++;
            }
         } else {
            break;
         }
      }

/* Number of route Mappings. */
      new->nroute = i;

/* If an error occurred, clean up by deleting the new SwitchMap. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new SwitchMap pointer. */
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




