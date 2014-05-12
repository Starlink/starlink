/*
*class++
*  Name:
*     SphMap

*  Purpose:
*     Map 3-d Cartesian to 2-d spherical coordinates

*  Constructor Function:
c     astSphMap
f     AST_SPHMAP

*  Description:
*     A SphMap is a Mapping which transforms points from a
*     3-dimensional Cartesian coordinate system into a 2-dimensional
*     spherical coordinate system (longitude and latitude on a unit
*     sphere centred at the origin). It works by regarding the input
*     coordinates as position vectors and finding their intersection
*     with the sphere surface. The inverse transformation always
*     produces points which are a unit distance from the origin
*     (i.e. unit vectors).

*  Inheritance:
*     The SphMap class inherits from the Mapping class.

*  Attributes:
*     In addition to those attributes common to all Mappings, every
*     SphMap also has the following attributes:
*
*     - UnitRadius: SphMap input vectors lie on a unit sphere?
*     - PolarLong: The longitude value to assign to either pole

*  Functions:
c     The SphMap class does not define any new functions beyond those
f     The SphMap class does not define any new routines beyond those
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
*     24-OCT-1996 (DSB):
*        Original version.
*     5-MAR-1997 (RFWS):
*        Tidied public prologues.
*     24-MAR-1998 (RFWS):
*        Override the astMapMerge method.
*     4-SEP-1998 (DSB):
*        Added UnitRadius attribute.
*     8-JAN-2003 (DSB):
*        Changed private InitVtab method to protected astInitSphMapVtab
*        method.
*     11-JUN-2003 (DSB):
*        Added PolarLong attribute.
*     10-MAY-2006 (DSB):
*        Override astEqual.
*     5-NOV-2013 (DSB):
*        Modify MapMerge so that it can spot and simplify an
*        (inverted SphMap,MatrixMap,SphMap) sequence in which the
*        MatrixMap just magnifies or reflects the radius vector.
*     25-MAR-2014 (DSB):
*        Correct 5-NOV-2013 MapMerge change.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS SphMap

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
#include "mapping.h"             /* Coordinate mappings (parent class) */
#include "channel.h"             /* I/O channels */
#include "unitmap.h"             /* Unit (identity) Mappings */
#include "sphmap.h"              /* Interface definition for this class */
#include "pal.h"                 /* SLA transformations */
#include "wcsmap.h"              /* For the AST__DPIBY2 (etc) constants */
#include "matrixmap.h"           /* Matrix mappings */
#include "winmap.h"              /* Shift and scale mappings */
#include "zoommap.h"             /* Scale mappings */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <float.h>
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
astMAKE_INITGLOBALS(SphMap)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(SphMap,Class_Init)
#define class_vtab astGLOBAL(SphMap,Class_Vtab)
#define getattrib_buff astGLOBAL(SphMap,GetAttrib_Buff)



/* If thread safety is not needed, declare and initialise globals at static
   variables. */
#else

static char getattrib_buff[ 101 ];


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstSphMapVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstSphMap *astSphMapId_( const char *, ...);

/* Prototypes for Private Member Functions. */
/* ======================================== */
static int GetUnitRadius( AstSphMap *, int * );
static int TestUnitRadius( AstSphMap *, int * );
static void ClearUnitRadius( AstSphMap *, int * );
static void SetUnitRadius( AstSphMap *, int, int * );

static double GetPolarLong( AstSphMap *, int * );
static int TestPolarLong( AstSphMap *, int * );
static void ClearPolarLong( AstSphMap *, int * );
static void SetPolarLong( AstSphMap *, double, int * );

static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static const char *GetAttrib( AstObject *, const char *, int * );
static int Equal( AstObject *, AstObject *, int * );
static int MapMerge( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
static int TestAttrib( AstObject *, const char *, int * );
static void ClearAttrib( AstObject *, const char *, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void SetAttrib( AstObject *, const char *, int * );

/* Member functions. */
/* ================= */
static void ClearAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for a SphMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "sphmap.h"
*     void ClearAttrib( AstObject *this, const char *attrib, int *status, int *status )

*  Class Membership:
*     SphMap member function (over-rides the astClearAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function clears the value of a specified attribute for a
*     SphMap, so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the SphMap.
*     attrib
*        Pointer to a null-terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstSphMap *this;             /* Pointer to the SphMap structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the SphMap structure. */
   this = (AstSphMap *) this_object;

/* UnitRadius */
/* ---------- */
   if ( !strcmp( attrib, "unitradius" ) ) {
      astClearUnitRadius( this );

/* PolarLong */
/* --------- */
   } else if ( !strcmp( attrib, "polarlong" ) ) {
      astClearPolarLong( this );

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
*     Test if two SphMaps are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "sphmap.h"
*     int Equal( AstObject *this, AstObject *that, int *status, int *status )

*  Class Membership:
*     SphMap member function (over-rides the astEqual protected
*     method inherited from the astMapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two SphMaps are equivalent.

*  Parameters:
*     this
*        Pointer to the first Object (a SphMap).
*     that
*        Pointer to the second Object.
*     status
*        Pointer to the inherited status variable.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the SphMaps are equivalent, zero otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstSphMap *that;
   AstSphMap *this;
   int nin;
   int nout;
   int result;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain pointers to the two SphMap structures. */
   this = (AstSphMap *) this_object;
   that = (AstSphMap *) that_object;

/* Check the second object is a SphMap. We know the first is a
   SphMap since we have arrived at this implementation of the virtual
   function. */
   if( astIsASphMap( that ) ) {

/* Get the number of inputs and outputs and check they are the same for both. */
      nin = astGetNin( this );
      nout = astGetNout( this );
      if( astGetNin( that ) == nin && astGetNout( that ) == nout ) {

/* If the Invert flags for the two SphMaps differ, it may still be possible
   for them to be equivalent. First compare the SphMaps if their Invert
   flags are the same. In this case all the attributes of the two SphMaps
   must be identical. */
         if( astGetInvert( this ) == astGetInvert( that ) ) {

            if( astEQUAL( this->polarlong, that->polarlong ) &&
                          this->unitradius == that->unitradius ){
               result = 1;
            }

/* If the Invert flags for the two SphMaps differ, the attributes of the two
   SphMaps must be inversely related to each other. */
         } else {

/* In the specific case of a SphMap, Invert flags must be equal. */
            result = 0;

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
*     Get the value of a specified attribute for a SphMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "sphmap.h"
*     const char *GetAttrib( AstObject *this, const char *attrib, int *status, int *status )

*  Class Membership:
*     SphMap member function (over-rides the protected astGetAttrib
*     method inherited from the Mapping class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for a SphMap, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the SphMap.
*     attrib
*        Pointer to a null-terminated string containing the name of
*        the attribute whose value is required. This name should be in
*        lower case, with all white space removed.
*     status
*        Pointer to the inherited status variable.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     - Pointer to a null-terminated string containing the attribute
*     value.

*  Notes:
*     - The returned string pointer may point at memory allocated
*     within the SphMap, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the SphMap. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   astDECLARE_GLOBALS           /* Pointer to thread-specific global data */
   AstSphMap *this;              /* Pointer to the SphMap structure */
   const char *result;           /* Pointer value to return */
   double dval;                  /* Double precision attribute value */
   int ival;                     /* Int attribute value */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(this_object);

/* Obtain a pointer to the SphMap structure. */
   this = (AstSphMap *) this_object;

/* UnitRadius. */
/* ----------- */
   if ( !strcmp( attrib, "unitradius" ) ) {
      ival = astGetUnitRadius( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", ival );
         result = getattrib_buff;
      }

/* PolarLong */
/* --------- */
   } else if ( !strcmp( attrib, "polarlong" ) ) {
      dval = astGetPolarLong( this );
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

void astInitSphMapVtab_(  AstSphMapVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitSphMapVtab

*  Purpose:
*     Initialise a virtual function table for a SphMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "sphmap.h"
*     void astInitSphMapVtab( AstSphMapVtab *vtab, const char *name )

*  Class Membership:
*     SphMap vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the SphMap class.

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
   will be used (by astIsASphMap) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstMappingVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */
   vtab->ClearUnitRadius = ClearUnitRadius;
   vtab->SetUnitRadius = SetUnitRadius;
   vtab->GetUnitRadius = GetUnitRadius;
   vtab->TestUnitRadius = TestUnitRadius;

   vtab->ClearPolarLong = ClearPolarLong;
   vtab->SetPolarLong = SetPolarLong;
   vtab->GetPolarLong = GetPolarLong;
   vtab->TestPolarLong = TestPolarLong;

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

   parent_transform = mapping->Transform;
   mapping->Transform = Transform;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   object->Equal = Equal;
   mapping->MapMerge = MapMerge;

/* Declare the class dump, copy and delete functions.*/
   astSetDump( vtab, Dump, "SphMap", "Cartesian to Spherical mapping" );
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
*     Simplify a sequence of Mappings containing a SphMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "sphmap.h"
*     int MapMerge( AstMapping *this, int where, int series, int *nmap,
*                   AstMapping ***map_list, int **invert_list, int *status, int *status )

*  Class Membership:
*     SphMap method (over-rides the protected astMapMerge method
*     inherited from the Mapping class).

*  Description:
*     This function attempts to simplify a sequence of Mappings by
*     merging a nominated SphMap in the sequence with its neighbours,
*     so as to shorten the sequence if possible.
*
*     In many cases, simplification will not be possible and the
*     function will return -1 to indicate this, without further
*     action.
*
*     In most cases of interest, however, this function will either
*     attempt to replace the nominated SphMap with one which it
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
*        Pointer to the nominated SphMap which is to be merged with
*        its neighbours. This should be a cloned copy of the SphMap
*        pointer contained in the array element "(*map_list)[where]"
*        (see below). This pointer will not be annulled, and the
*        SphMap it identifies will not be modified by this function.
*     where
*        Index in the "*map_list" array (below) at which the pointer
*        to the nominated SphMap resides.
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
   AstMapping *new;              /* Pointer to replacement Mapping */
   AstMatrixMap *mm;             /* Pointer to MatrixMap */
   AstWinMap *wm;                /* The new WinMap */
   const char *class;            /* Pointer to Mapping class string */
   double absval;                /* Absolute value fo each diagonal element */
   double diag[ 3 ];             /* The diagonal matrix elements */
   double polarlong;             /* Value of PolarLong attribute */
   int imap1;                    /* Index of first SphMap */
   int imap2;                    /* Index of second SphMap */
   int imap;                     /* Loop counter for Mappings */
   int result;                   /* Result value to return */
   int simpler;                  /* Mappings simplified? */

/* Initialise the returned result. */
   result = -1;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Further initialisation. */
   new = NULL;
   simpler = 0;

/* We will only handle the case of SphMaps in series and will consider
   merging the nominated SphMap with the Mapping which follows
   it. Check that there is such a Mapping. */
   if ( series && ( ( where + 1 ) < *nmap ) ) {

/* Obtain the indices of the two potential SphMaps to be merged. */
      imap1 = where;
      imap2 = where + 1;

/* Obtain the Class string of the second Mapping and determine if it
   is a SphMap. */
      class = astGetClass( ( *map_list )[ imap2 ] );
      if ( astOK && !strcmp( class, "SphMap" ) ) {

/* Check if the first SphMap is applied in the inverse direction and
   the second in the forward direction. This combination can be
   simplified if the PolarLongitude attributes are equal.. */
         if( ( *invert_list )[ imap1 ] && !( *invert_list )[ imap2 ] ) {
            simpler = astEQUAL( astGetPolarLong( ( *map_list )[ imap1 ] ),
                                astGetPolarLong( ( *map_list )[ imap2 ] ) );

/* If the first SphMap is applied in the forward direction and the second in
   the inverse direction, the combination can only be simplified if the
   input vectors to the first SphMap all have unit length (as indicated by
   the UnitRadius attribute). */
         } else if( !( *invert_list )[ imap1 ] && ( *invert_list )[ imap2 ] ) {
            simpler = astGetUnitRadius( ( *map_list )[ imap1 ] );
         }
      }

/* If the two SphMaps can be simplified, create a UnitMap to replace
   them. */
      if ( simpler ) {
         new = (AstMapping *) astUnitMap( 2, "", status );

/* Annul the pointers to the SphMaps. */
         if ( astOK ) {
            ( *map_list )[ imap1 ] = astAnnul( ( *map_list )[ imap1 ] );
            ( *map_list )[ imap2 ] = astAnnul( ( *map_list )[ imap2 ] );

/* Insert the pointer to the replacement Mapping and initialise its
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

/* Another possible simplification is if the nominated Mapping is an inverted
   SphMap followed in series by a ZoomMap or diagonal MatrixMap that has
   diagonal elements of equal magnitude, which is then followed by a
   non-inverted SphMap. This is equivalent to a 3D rotation of a pair of
   (longitude,latitude) angles. The MatrixMap/ZoomMap may magnify the
   radius vector, but this will not alter the angles. Any difference in
   signs amongst the diagonal elements will cause a reflection or reversal
   of the corresponbding angles, which can be represented by a WinMap. We
   do not need to consider the other possibility (that the nominated
   SphMap is the *last* Mapping in such a sequence of three), since we
   will already have discovered such a sequence on an earlier invocation
   of this function. */
   if( series && !simpler && ( *invert_list )[ where ] &&
       where + 2 < *nmap  ) {

/* Check the third Mapping is a non-inverted SphMap. */
      class = astGetClass( ( *map_list )[ where + 2 ] );
      if( astOK && !strcmp( class, "SphMap" ) &&
          !( *invert_list )[ where + 2 ] ) {

/* Check the second Mapping is a ZoomMap, or a diagonal MatrixMap that
   has diagonal elements of equal magnitude. Since the Mapping is
   sandwiched between the two SphMaps, we know it must have 3 inputs and
   3 outputs. Record the corresponding diagonal values. The state of the
   Invert flag does not matter since it will only affect the degree to
   which the radius vector is magnified - it will not change the signs of
   any diagonal elements. */
         class = astGetClass( ( *map_list )[ where + 1 ] );
         if( astOK && !strcmp( class, "ZoomMap" ) ) {
            diag[ 0 ] = astGetZoom( ( *map_list )[ where + 1 ] );
            if( diag[ 0 ] != 0.0 ) {
               diag[ 1 ] = diag[ 0 ];
               diag[ 2 ] = diag[ 0 ];
            } else {
               class = NULL;
            }

         } else if( astOK && !strcmp( class, "MatrixMap" ) ) {
            mm = (AstMatrixMap *)  ( *map_list )[ where + 1 ];
            if( mm->form == 1 && mm->f_matrix ) {
               diag[ 0 ] = mm->f_matrix[ 0 ];
               if( diag[ 0 ] != 0.0 ) {
                  diag[ 1 ] = mm->f_matrix[ 1 ];
                  diag[ 2 ] = mm->f_matrix[ 2 ];

                  absval = fabs( diag[ 0 ] );
                  if( !astEQUAL( fabs( diag[ 1 ] ), absval ) ||
                      !astEQUAL( fabs( diag[ 2 ] ), absval ) ) {
                     class = NULL;
                  }

               } else {
                  class = NULL;
               }

            } else {
               class = NULL;
            }

         } else {
            class = NULL;
         }

      } else {
         class = NULL;
      }

/* We can only make changes if above conditions were met. */
      if( class ) {

/* Create a WinMap that modifies the (longitude,latitude) values, initially
   with undefined corners. */
         wm = astWinMap( 2, NULL, NULL, NULL, NULL, "", status );

/* Store appropriate scales and offsets in the WinMap. These just depend on
   the signs of the matrix diagonal elements since we know the magnitudes of
   these elements are all equal. */
         if( diag[ 0 ] < 0.0 ) {
            if( diag[ 1 ] < 0.0 ) {
               wm->a[ 0 ] = AST__DPI;
               wm->b[ 0 ] = 1.0;
            } else {
               wm->a[ 0 ] = AST__DPI;
               wm->b[ 0 ] = -1.0;
            }

         } else {
            if( diag[ 1 ] < 0.0 ) {
               wm->a[ 0 ] = 0.0;
               wm->b[ 0 ] = -1.0;
            } else {
               wm->a[ 0 ] = 0.0;
               wm->b[ 0 ] = 1.0;
            }
         }

         if( diag[ 2 ] < 0.0 ) {
            wm->a[ 1 ] = 0.0;
            wm->b[ 1 ] = -1.0;
         } else {
            wm->a[ 1 ] = 0.0;
            wm->b[ 1 ] = 1.0;
         }

/* We are aiming to replace the supplied (SphMap,MatrixMap,SphMap)
   combination with (WinMap,SphMap,SphMap), leaving us with an inverted
   and non-inverted SphMap side by side. This is on the understanding
   that a subsequent call to this function will combine these two
   adjacent SphMaps into a UnitMap. But this will only happen if the
   adjacent SphMaps have equal values for their PolarLong attributes. The
   change of (SphMap,MatrixMap) to (WinMap,SphMap) will change the value
   of the PolarLong attribute in the first SphMap, so we need to work out
   this changed value and check that it is the same as the PolarLong
   value of the second SphMap. If they are different, there is no point
   making any changes since the two SphMaps cannot be merged into a
   UnitMap. So get the PolarLong value from the supplied first SphMap. */
         polarlong = astGetPolarLong( ( *map_list )[ where ] );

/* Modified the PolarLong value to take account of the change from
   (SphMap,MatrixMap) to (WinMap,SphMap). */
         polarlong =  wm->a[ 0 ] + wm->b[ 0 ]*polarlong;

/* Check this is the same as the PolarLong value in the second SphMap. */
         if( astEQUAL( polarlong, astGetPolarLong( ( *map_list )[ where + 2 ] ) ) ) {

/* All is good, so we can now change the supplied Mappings list. First
   change the PolarLong value in the first SphMap. */
            astSetPolarLong( ( *map_list )[ where ], polarlong );

/* Annul The MatrixMap or ZoomMap. */
            (void) astAnnul( ( *map_list )[ where + 1 ] );

/* Move the first SphMap to the slot left vacant by the annulled
   MatrixMap or ZoomMap. */
            ( *map_list )[ where + 1 ] = ( *map_list )[ where ];
            ( *invert_list )[ where + 1 ] = ( *invert_list )[ where ];

/* Store the new WinMap in the place of the SphMap. */
            ( *map_list )[ where ] = astClone( wm );
            ( *invert_list )[ where ] = 0;

/* Return the index of the first modified element. */
            result = where;
         }

/* Free resources. */
         wm = astAnnul( wm );
      }
   }

/* If an error occurred, clear the returned result. */
   if ( !astOK ) result = -1;

/* Return the result. */
   return result;
}

static void SetAttrib( AstObject *this_object, const char *setting, int *status ) {
/*
*  Name:
*     SetAttrib

*  Purpose:
*     Set an attribute value for a SphMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "sphmap.h"
*     void SetAttrib( AstObject *this, const char *setting )

*  Class Membership:
*     SphMap member function (over-rides the astSetAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function assigns an attribute value for a SphMap, the
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
*        Pointer to the SphMap.
*     setting
*        Pointer to a null-terminated string specifying the new attribute
*        value.
*/

/* Local Variables: */
   AstSphMap *this;              /* Pointer to the SphMap structure */
   double dval;                  /* Double precision attribute value */
   int len;                      /* Length of setting string */
   int ival;                     /* Int attribute value */
   int nc;                       /* Number of characters read by astSscanf */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the SphMap structure. */
   this = (AstSphMap *) this_object;

/* Obtain the length of the setting string. */
   len = (int) strlen( setting );

/* UnitRadius */
/* ---------- */
   if ( nc = 0,
        ( 1 == astSscanf( setting, "unitradius= %d %n", &ival, &nc ) )
        && ( nc >= len ) ) {
      astSetUnitRadius( this, ival );

/* PolarLong */
/* --------- */
   } else if ( nc = 0,
        ( 1 == astSscanf( setting, "polarlong= %lf %n", &dval, &nc ) )
        && ( nc >= len ) ) {
      astSetPolarLong( this, dval );

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
*     Test if a specified attribute value is set for a SphMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "sphmap.h"
*     int TestAttrib( AstObject *this, const char *attrib, int *status, int *status )

*  Class Membership:
*     SphMap member function (over-rides the astTestAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     a value has been set for one of a SphMap's attributes.

*  Parameters:
*     this
*        Pointer to the SphMap.
*     attrib
*        Pointer to a null-terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if a value has been set, otherwise zero.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstSphMap *this;             /* Pointer to the SphMap structure */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the SphMap structure. */
   this = (AstSphMap *) this_object;

/* UnitRadius */
/* ---------- */
   if ( !strcmp( attrib, "unitradius" ) ) {
      result = astTestUnitRadius( this );

/* PolarLong */
/* --------- */
   } else if ( !strcmp( attrib, "polarlong" ) ) {
      result = astTestPolarLong( this );

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
*     Apply a SphMap to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "sphmap.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status, int *status )

*  Class Membership:
*     SphMap member function (over-rides the astTransform protected
*     method inherited from the Mapping class).

*  Description:
*     This function takes a SphMap and a set of points encapsulated in a
*     PointSet and transforms the points from Cartesian coordinates to
*     spherical coordinates.

*  Parameters:
*     this
*        Pointer to the SphMap.
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
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the output (possibly new) PointSet.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*     -  The number of coordinate values per point in the input PointSet must
*     match the number of coordinates for the SphMap being applied.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstPointSet *result;          /* Pointer to output PointSet */
   AstSphMap *map;               /* Pointer to SphMap to be applied */
   double **ptr_in;              /* Pointer to input coordinate data */
   double **ptr_out;             /* Pointer to output coordinate data */
   int npoint;                   /* Number of points */
   int point;                    /* Loop counter for points */
   double *p0;                   /* Pointer to x axis value */
   double *p1;                   /* Pointer to y axis value */
   double *p2;                   /* Pointer to z axis value */
   double *q0;                   /* Pointer to longitude value */
   double *q1;                   /* Pointer to latitude value */
   double mxerr;                 /* Largest value which is effectively zero */
   double polarlong;             /* Longitude at either pole */
   double v[3];                  /* Vector for a single point */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the SphMap. */
   map = (AstSphMap *) this;

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

/* Perform coordinate arithmetic. */
/* ------------------------------ */
   if( astOK ){

/* First deal with forward mappings from Cartesian to Spherical. */
      if( forward ){

/* Get the longitude to return at either pole. */
         polarlong = astGetPolarLong( this );

/* Store pointers to the input Cartesian axes. */
         p0 = ptr_in[ 0 ];
         p1 = ptr_in[ 1 ];
         p2 = ptr_in[ 2 ];

/* Store pointers to the output Spherical axes. */
         q0 = ptr_out[ 0 ];
         q1 = ptr_out[ 1 ];

/* Apply the mapping to every point. */
         for( point = 0; point < npoint; point++ ){
            if( *p0 != AST__BAD && *p1 != AST__BAD && *p2 != AST__BAD ){
               v[0] = *p0;
               v[1] = *p1;
               v[2] = *p2;

/* At either pole, return the longitude equal to PolarLong attribute. */
               mxerr = fabs( 1000.0*v[ 2 ] )*DBL_EPSILON;
               if( fabs( v[ 0 ] ) < mxerr && fabs( v[ 1 ] ) < mxerr ) {
                  if( v[ 2 ] < 0.0 ) {
                     *(q0++) = polarlong;
                     *(q1++) = -AST__DPIBY2;
                  } else if( v[ 2 ] > 0.0 ) {
                     *(q0++) = polarlong;
                     *(q1++) = AST__DPIBY2;
                  } else {
                     *(q0++) = AST__BAD;
                     *(q1++) = AST__BAD;
                  }

/* Otherwise use a SLALIB function to do the conversion (SLALIB always
   returns zero at either pole which is why we make the above check). */
               } else {
                  palDcc2s( v, q0++, q1++ );
               }

            } else {
               *(q0++) = AST__BAD;
               *(q1++) = AST__BAD;
            }
            p0++;
            p1++;
            p2++;
         }

/* Now deal with inverse mappings from Spherical to Cartesian. */
      } else {

/* Store pointers to the input Spherical axes. */
         q0 = ptr_in[ 0 ];
         q1 = ptr_in[ 1 ];

/* Store pointers to the output Cartesian axes. */
         p0 = ptr_out[ 0 ];
         p1 = ptr_out[ 1 ];
         p2 = ptr_out[ 2 ];

/* Apply the mapping to every point. */
         for( point = 0; point < npoint; point++ ){
            if( *q0 != AST__BAD && *q1 != AST__BAD ){
               palDcs2c( *q0, *q1, v );
               *(p0++) = v[ 0 ];
               *(p1++) = v[ 1 ];
               *(p2++) = v[ 2 ];
            } else {
               *(p0++) = AST__BAD;
               *(p1++) = AST__BAD;
               *(p2++) = AST__BAD;

            }
            q0++;
            q1++;
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

/* UnitRadius */
/* ---------- */
/*
*att++
*  Name:
*     UnitRadius

*  Purpose:
*     SphMap input vectors lie on a unit sphere?

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer (boolean).

*  Description:
*     This is a boolean attribute which indicates whether the
*     3-dimensional vectors which are supplied as input to a SphMap
*     are known to always have unit length, so that they lie on a unit
*     sphere centred on the origin.
*
c     If this condition is true (indicated by setting UnitRadius
c     non-zero), it implies that a CmpMap which is composed of a
c     SphMap applied in the forward direction followed by a similar
c     SphMap applied in the inverse direction may be simplified
c     (e.g. by astSimplify) to become a UnitMap. This is because the
c     input and output vectors will both have unit length and will
c     therefore have the same coordinate values.
f     If this condition is true (indicated by setting UnitRadius
f     non-zero), it implies that a CmpMap which is composed of a
f     SphMap applied in the forward direction followed by a similar
f     SphMap applied in the inverse direction may be simplified
f     (e.g. by AST_SIMPLIFY) to become a UnitMap. This is because the
f     input and output vectors will both have unit length and will
f     therefore have the same coordinate values.
*
*     If UnitRadius is zero (the default), then although the output
*     vector produced by the CmpMap (above) will still have unit
*     length, the input vector may not have. This will, in general,
*     change the coordinate values, so it prevents the pair of SphMaps
*     being simplified.

*  Notes:
*     - This attribute is intended mainly for use when SphMaps are
*     involved in a sequence of Mappings which project (e.g.) a
*     dataset on to the celestial sphere. By regarding the celestial
*     sphere as a unit sphere (and setting UnitRadius to be non-zero)
*     it becomes possible to cancel the SphMaps present, along with
*     associated sky projections, when two datasets are aligned using
*     celestial coordinates. This often considerably improves
*     performance.
*     - Such a situations often arises when interpreting FITS data and
*     is handled automatically by the FitsChan class.
*     - The value of the UnitRadius attribute is used only to control
*     the simplification of Mappings and has no effect on the value of
*     the coordinates transformed by a SphMap. The lengths of the
*     input 3-dimensional Cartesian vectors supplied are always
*     ignored, even if UnitRadius is non-zero.

*  Applicability:
*     SphMap
*        All SphMaps have this attribute.
*att--
*/
astMAKE_CLEAR(SphMap,UnitRadius,unitradius,-1)
astMAKE_GET(SphMap,UnitRadius,int,0,(this->unitradius == -1 ? 0 : this->unitradius))
astMAKE_SET(SphMap,UnitRadius,int,unitradius,( value ? 1 : 0 ))
astMAKE_TEST(SphMap,UnitRadius,( this->unitradius != -1 ))

/* PolarLong */
/* --------- */
/*
*att++
*  Name:
*     PolarLong

*  Purpose:
*     The longitude value to assign to either pole

*  Type:
*     Public attribute.

*  Synopsis:
*     Double precision.

*  Description:
*     This attribute holds the longitude value, in radians, to be
*     returned when a Cartesian position corresponding to either the north
*     or south pole is transformed into spherical coordinates. The
*     default value is zero.

*  Applicability:
*     SphMap
*        All SphMaps have this attribute.
*att--
*/
astMAKE_CLEAR(SphMap,PolarLong,polarlong,AST__BAD)
astMAKE_GET(SphMap,PolarLong,double,0.0,(this->polarlong == AST__BAD ? 0.0 : this->polarlong))
astMAKE_SET(SphMap,PolarLong,double,polarlong,value)
astMAKE_TEST(SphMap,PolarLong,( this->polarlong != AST__BAD ))

/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout, int *status ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for SphMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status, int *status, int *status )

*  Description:
*     This function implements the copy constructor for SphMap objects.

*  Parameters:
*     objin
*        Pointer to the SphMap to be copied.
*     objout
*        Pointer to the SphMap being constructed.
*     status
*        Pointer to the inherited status variable.
*     status
*        Pointer to the inherited status variable.
*     status
*        Pointer to the inherited status variable.

*/

}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for SphMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status, int *status )

*  Description:
*     This function implements the destructor for SphMap objects.

*  Parameters:
*     obj
*        Pointer to the SphMap to be deleted.
*     status
*        Pointer to the inherited status variable.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - This destructor does nothing and exists only to maintain a
*     one-to-one correspondence between destructors and copy
*     constructors.
*/


}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for SphMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status, int *status, int *status, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the SphMap class to an output Channel.

*  Parameters:
*     this
*        Pointer to the SphMap whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*     status
*        Pointer to the inherited status variable.
*     status
*        Pointer to the inherited status variable.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstSphMap *this;              /* Pointer to the SphMap structure */
   double dval;                  /* Double precision attribute value */
   int ival;                     /* Integer value */
   int set;                      /* Attribute value set? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the SphMap structure. */
   this = (AstSphMap *) this_object;

/* Write out values representing the instance variables for the
   SphMap class.  Accompany these with appropriate comment strings,
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

/* UnitRadius. */
/* ------- */
   set = TestUnitRadius( this, status );
   ival = set ? GetUnitRadius( this, status ) : astGetUnitRadius( this );
   if( ival ) {
      astWriteInt( channel, "UntRd", set, 0, ival, "All input vectors have unit length" );
   } else {
      astWriteInt( channel, "UntRd", set, 0, ival, "Input vectors do not all have unit length" );
   }

/* PolarLong. */
/* ---------- */
   set = TestPolarLong( this, status );
   dval = set ? GetPolarLong( this, status ) : astGetPolarLong( this );
   astWriteDouble( channel, "PlrLg", set, 1, dval, "Polar longitude (rad.s)" );

}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsASphMap and astCheckSphMap functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(SphMap,Mapping)
astMAKE_CHECK(SphMap)

AstSphMap *astSphMap_( const char *options, int *status, ...) {
/*
*++
*  Name:
c     astSphMap
f     AST_SPHMAP

*  Purpose:
*     Create a SphMap.

*  Type:
*     Public function.

*  Synopsis:
c     #include "sphmap.h"
c     AstSphMap *astSphMap( const char *options, ... )
f     RESULT = AST_SPHMAP( OPTIONS, STATUS )

*  Class Membership:
*     SphMap constructor.

*  Description:
*     This function creates a new SphMap and optionally initialises
*     its attributes.
*
*     A SphMap is a Mapping which transforms points from a
*     3-dimensional Cartesian coordinate system into a 2-dimensional
*     spherical coordinate system (longitude and latitude on a unit
*     sphere centred at the origin). It works by regarding the input
*     coordinates as position vectors and finding their intersection
*     with the sphere surface. The inverse transformation always
*     produces points which are a unit distance from the origin
*     (i.e. unit vectors).

*  Parameters:
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new SphMap. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new SphMap. The syntax used is identical to that for the
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
c     astSphMap()
f     AST_SPHMAP = INTEGER
*        A pointer to the new SphMap.

*  Notes:
*     - The spherical coordinates are longitude (positive
*     anti-clockwise looking from the positive latitude pole) and
*     latitude. The Cartesian coordinates are right-handed, with the x
*     axis (axis 1) at zero longitude and latitude, and the z axis
*     (axis 3) at the positive latitude pole.
*     - At either pole, the longitude is set to the value of the
*     PolarLong attribute.
*     - If the Cartesian coordinates are all zero, then the longitude
*     and latitude are set to the value AST__BAD.
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.

*  Status Handling:
*     The protected interface to this function includes an extra
*     parameter at the end of the parameter list descirbed above. This
*     parameter is a pointer to the integer inherited status
*     variable: "int *status".


*  Status Handling:
*     The protected interface to this function includes an extra
*     parameter at the end of the parameter list descirbed above. This
*     parameter is a pointer to the integer inherited status
*     variable: "int *status".


*  Status Handling:
*     The protected interface to this function includes an extra
*     parameter at the end of the parameter list descirbed above. This
*     parameter is a pointer to the integer inherited status
*     variable: "int *status".

*--
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstSphMap *new;              /* Pointer to new SphMap */
   va_list args;                /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the SphMap, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitSphMap( NULL, sizeof( AstSphMap ), !class_init, &class_vtab,
                        "SphMap" );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new SphMap's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new SphMap. */
   return new;
}

AstSphMap *astSphMapId_( const char *options, ...) {
/*
*  Name:
*     astSphMapId_

*  Purpose:
*     Create a SphMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "sphmap.h"
*     AstSphMap *astSphMapId_( const char *options, ... )

*  Class Membership:
*     SphMap constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astSphMap constructor function. It returns an ID value (instead
*     of a true C pointer) to external users, and must be provided
*     because astSphMap_ has a variable argument list which cannot be
*     encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     The variable argument list also prevents this function from
*     invoking astSphMap_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astSphMap_.

*  Returned Value:
*     The ID value associated with the new SphMap.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstSphMap *new;              /* Pointer to new SphMap */
   va_list args;                /* Variable argument list */
   int *status;                 /* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the SphMap, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitSphMap( NULL, sizeof( AstSphMap ), !class_init, &class_vtab,
                        "SphMap" );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new SphMap's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new SphMap. */
   return astMakeId( new );
}

AstSphMap *astInitSphMap_( void *mem, size_t size, int init,
                           AstSphMapVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitSphMap

*  Purpose:
*     Initialise a SphMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "sphmap.h"
*     AstSphMap *astInitSphMap( void *mem, size_t size, int init,
*                               AstSphMapVtab *vtab, const char *name )

*  Class Membership:
*     SphMap initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new SphMap object. It allocates memory (if necessary) to accommodate
*     the SphMap plus any additional data associated with the derived class.
*     It then initialises a SphMap structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a SphMap at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the SphMap is to be initialised.
*        This must be of sufficient size to accommodate the SphMap data
*        (sizeof(SphMap)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the SphMap (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the SphMap
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the SphMap's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new SphMap.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the astGetClass
*        method).

*  Returned Value:
*     A pointer to the new SphMap.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstSphMap *new;              /* Pointer to new SphMap */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitSphMapVtab( vtab, name );

/* Initialise. */
   new = NULL;

/* Initialise a Mapping structure (the parent class) as the first component
   within the SphMap structure, allocating memory if necessary. Specify that
   the Mapping should be defined in both the forward and inverse directions. */
   new = (AstSphMap *) astInitMapping( mem, size, 0,
                                       (AstMappingVtab *) vtab, name,
                                       3, 2, 1, 1 );

   if ( astOK ) {

/* Initialise the SphMap data. */
/* --------------------------- */
/* Are all input vectors of unit length? Store a value of -1 to indicate that
   no value has yet been set. This will cause a default value of 0 (no, i.e.
   input vectors are not all of unit length) to be used. */
      new->unitradius = -1;
      new->polarlong = AST__BAD;

   }

/* Return a pointer to the new SphMap. */
   return new;
}

AstSphMap *astLoadSphMap_( void *mem, size_t size,
                           AstSphMapVtab *vtab, const char *name,
                           AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadSphMap

*  Purpose:
*     Load a SphMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "sphmap.h"
*     AstSphMap *astLoadSphMap( void *mem, size_t size,
*                               AstSphMapVtab *vtab, const char *name,
*                               AstChannel *channel )

*  Class Membership:
*     SphMap loader.

*  Description:
*     This function is provided to load a new SphMap using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     SphMap structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a SphMap at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the SphMap is to be
*        loaded.  This must be of sufficient size to accommodate the
*        SphMap data (sizeof(SphMap)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the SphMap (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the SphMap structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstSphMap) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new SphMap. If this is NULL, a pointer
*        to the (static) virtual function table for the SphMap class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "SphMap" is used instead.

*  Returned Value:
*     A pointer to the new SphMap.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

#define KEY_LEN 50               /* Maximum length of a keyword */

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstSphMap *new;               /* Pointer to the new SphMap */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if( !astOK ) return new;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this SphMap. In this case the
   SphMap belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstSphMap );
      vtab = &class_vtab;
      name = "SphMap";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitSphMapVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built SphMap. */
   new = astLoadMapping( mem, size, (AstMappingVtab *) vtab, name,
                         channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "SphMap" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* UnitRadius. */
/* ----------- */
      new->unitradius = astReadInt( channel, "untrd", -1 );
      if ( TestUnitRadius( new, status ) ) SetUnitRadius( new, new->unitradius, status );

/* PolarLong. */
/* ---------- */
      new->polarlong = astReadDouble( channel, "plrlg", AST__BAD );
      if ( TestPolarLong( new, status ) ) SetPolarLong( new, new->polarlong, status );

   }

/* If an error occurred, clean up by deleting the new SphMap. */
   if ( !astOK ) new = astDelete( new );

/* Return the new SphMap pointer. */
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





