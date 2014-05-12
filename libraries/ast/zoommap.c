/*
*class++
*  Name:
*     ZoomMap

*  Purpose:
*     Zoom coordinates about the origin.

*  Constructor Function:
c     astZoomMap
f     AST_ZOOMMAP

*  Description:
*     The ZoomMap class implements a Mapping which performs a "zoom"
*     transformation by multiplying all coordinate values by the same
*     scale factor (the inverse transformation is performed by
*     dividing by this scale factor). The number of coordinate values
*     representing each point is unchanged.

*  Inheritance:
*     The ZoomMap class inherits from the Mapping class.

*  Attributes:
*     In addition to those attributes common to all Mappings, every
*     ZoomMap also has the following attributes:
*
*     - Zoom: ZoomMap scale factor

*  Functions:
c     The ZoomMap class does not define any new functions beyond those
f     The ZoomMap class does not define any new routines beyond those
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
*     DSB: David S. Berry (Starlink)

*  History:
*     1-FEB-1996 (RFWS):
*        Original version.
*     18-JUL-1996 (RFWS):
*        Updated to support attributes and an external interface.
*     10-SEP-1996 (RFWS):
*        Added I/O facilities.
*     4-JUN-1997 (RFWS):
*        Over-ride the MapMerge method to provide ZoomMap
*        simplification facilities.
*     8-JAN-2003 (DSB):
*        Changed private InitVtab method to protected astInitZoomMapVtab
*        method.
*     10-MAY-2006 (DSB):
*        Override astEqual.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS ZoomMap

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "globals.h"             /* Thread-safe global data access */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* Sets of points/coordinates */
#include "mapping.h"             /* Coordinate mappings (parent class) */
#include "channel.h"             /* I/O channels */
#include "unitmap.h"             /* Unit Mappings */
#include "matrixmap.h"           /* Matrix Mappings */
#include "zoommap.h"             /* Interface definition for this class */

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
astMAKE_INITGLOBALS(ZoomMap)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(ZoomMap,Class_Init)
#define class_vtab astGLOBAL(ZoomMap,Class_Vtab)
#define getattrib_buff astGLOBAL(ZoomMap,GetAttrib_Buff)



/* If thread safety is not needed, declare and initialise globals at static
   variables. */
#else

static char getattrib_buff[ 101 ];


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstZoomMapVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstZoomMap *astZoomMapId_( int, double, const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static const char *GetAttrib( AstObject *, const char *, int * );static double GetZoom( AstZoomMap *, int * );
static double Rate( AstMapping *, double *, int, int, int * );
static int *MapSplit( AstMapping *, int, const int *, AstMapping **, int * );
static int Equal( AstObject *, AstObject *, int * );
static int GetIsLinear( AstMapping *, int * );
static int MapMerge( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
static int TestAttrib( AstObject *, const char *, int * );
static int TestZoom( AstZoomMap *, int * );
static void ClearAttrib( AstObject *, const char *, int * );
static void ClearZoom( AstZoomMap *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void SetAttrib( AstObject *, const char *, int * );
static void SetZoom( AstZoomMap *, double, int * );

/* Member functions. */
/* ================= */
static void ClearAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for a ZoomMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "zoommap.h"
*     void ClearAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     ZoomMap member function (over-rides the astClearAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function clears the value of a specified attribute for a
*     ZoomMap, so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the ZoomMap.
*     attrib
*        Pointer to a null-terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstZoomMap *this;             /* Pointer to the ZoomMap structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the ZoomMap structure. */
   this = (AstZoomMap *) this_object;

/* Check the attribute name and clear the appropriate attribute. */

/* Zoom. */
/* ----- */
   if ( !strcmp( attrib, "zoom" ) ) {
      astClearZoom( this );

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
*     Test if two ZoomMaps are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "zoommap.h"
*     int Equal( AstObject *this, AstObject *that, int *status )

*  Class Membership:
*     ZoomMap member function (over-rides the astEqual protected
*     method inherited from the astMapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two ZoomMaps are equivalent.

*  Parameters:
*     this
*        Pointer to the first Object (a ZoomMap).
*     that
*        Pointer to the second Object.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the ZoomMaps are equivalent, zero otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstZoomMap *that;
   AstZoomMap *this;
   int nin;
   int nout;
   int result;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain pointers to the two ZoomMap structures. */
   this = (AstZoomMap *) this_object;
   that = (AstZoomMap *) that_object;

/* Check the second object is a ZoomMap. We know the first is a
   ZoomMap since we have arrived at this implementation of the virtual
   function. */
   if( astIsAZoomMap( that ) ) {

/* Get the number of inputs and outputs and check they are the same for both. */
      nin = astGetNin( this );
      nout = astGetNout( this );
      if( astGetNin( that ) == nin && astGetNout( that ) == nout ) {

/* If the Invert flags for the two ZoomMaps differ, it may still be possible
   for them to be equivalent. First compare the ZoomMaps if their Invert
   flags are the same. In this case all the attributes of the two ZoomMaps
   must be identical. */
         if( astGetInvert( this ) == astGetInvert( that ) ) {
            result = ( astEQUAL( this->zoom, that->zoom ) );

/* If the Invert flags for the two ZoomMaps differ, the attributes of the two
   ZoomMaps must be inversely related to each other. */
         } else {
            result = ( astEQUAL( this->zoom, 1.0/that->zoom ) );

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
*     Get the value of a specified attribute for a ZoomMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "zoommap.h"
*     const char *GetAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     ZoomMap member function (over-rides the protected astGetAttrib
*     method inherited from the Mapping class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for a ZoomMap, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the ZoomMap.
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
*     within the ZoomMap, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the ZoomMap. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   astDECLARE_GLOBALS           /* Pointer to thread-specific global data */
   AstZoomMap *this;             /* Pointer to the ZoomMap structure */
   const char *result;           /* Pointer value to return */
   double zoom;                  /* Zoom attribute value */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(this_object);

/* Obtain a pointer to the ZoomMap structure. */
   this = (AstZoomMap *) this_object;

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "getattrib_buff" as a null-terminated string in an appropriate
   format.  Set "result" to point at the result string. */

/* Zoom. */
/* ----- */
   if ( !strcmp( attrib, "zoom" ) ) {
      zoom = astGetZoom( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%.*g", DBL_DIG, zoom );
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

static int GetIsLinear( AstMapping *this_mapping, int *status ){
/*
*  Name:
*     GetIsLinear

*  Purpose:
*     Return the value of the IsLinear attribute for a ZoomMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     void GetIsLinear( AstMapping *this, int *status )

*  Class Membership:
*     ZoomMap member function (over-rides the protected astGetIsLinear
*     method inherited from the Mapping class).

*  Description:
*     This function returns the value of the IsLinear attribute for a
*     Frame, which is always one.

*  Parameters:
*     this
*        Pointer to the ZoomMap.
*     status
*        Pointer to the inherited status variable.
*/
   return 1;
}

void astInitZoomMapVtab_(  AstZoomMapVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitZoomMapVtab

*  Purpose:
*     Initialise a virtual function table for a ZoomMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "zoommap.h"
*     void astInitZoomMapVtab( AstZoomMapVtab *vtab, const char *name )

*  Class Membership:
*     ZoomMap vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the ZoomMap class.

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
   will be used (by astIsAZoomMap) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstMappingVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */
   vtab->ClearZoom = ClearZoom;
   vtab->GetZoom = GetZoom;
   vtab->SetZoom = SetZoom;
   vtab->TestZoom = TestZoom;

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
   mapping->MapSplit = MapSplit;
   mapping->Rate = Rate;
   mapping->GetIsLinear = GetIsLinear;

/* Declare the class dump function. There is no copy constructor or
   destructor. */
   astSetDump( vtab, Dump, "ZoomMap", "Zoom about the origin" );

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
*     Simplify a sequence of Mappings containing a ZoomMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     int MapMerge( AstMapping *this, int where, int series, int *nmap,
*                   AstMapping ***map_list, int **invert_list, int *status )

*  Class Membership:
*     ZoomMap method (over-rides the protected astMapMerge method
*     inherited from the Mapping class).

*  Description:
*     This function attempts to simplify a sequence of Mappings by
*     merging a nominated ZoomMap in the sequence with its neighbours,
*     so as to shorten the sequence if possible.
*
*     In many cases, simplification will not be possible and the
*     function will return -1 to indicate this, without further
*     action.
*
*     In most cases of interest, however, this function will either
*     attempt to replace the nominated ZoomMap with one which it
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
*        Pointer to the nominated ZoomMap which is to be merged with
*        its neighbours. This should be a cloned copy of the ZoomMap
*        pointer contained in the array element "(*map_list)[where]"
*        (see below). This pointer will not be annulled, and the
*        ZoomMap it identifies will not be modified by this function.
*     where
*        Index in the "*map_list" array (below) at which the pointer
*        to the nominated ZoomMap resides.
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
   AstMapping *map;              /* Pointer to Mapping */
   AstMapping *new;              /* Pointer to replacement Mapping */
   const char *class;            /* Pointer to Mapping class string */
   double *zooms;                /* Pointer to zoom factor array */
   double maxzoom;               /* Maximum zoom factor */
   double minzoom;               /* Minimum zoom factor */
   double zoom;                  /* Zoom factor */
   int coord;                    /* Loop counter for coordinates */
   int imap1;                    /* Index of first ZoomMap */
   int imap2;                    /* Index of last ZoomMap */
   int imap;                     /* Loop counter for Mappings */
   int ncoord;                   /* Number of input/output coordinates */
   int ngone;                    /* Number of Mappings eliminated */
   int nin;                      /* Total number of input coordinates */
   int nzoom;                    /* Number of zoom factors */
   int result;                   /* Result value to return */
   int simpler;                  /* Mapping(s) simplified? */
   int single;                   /* Replacement is a single ZoomMap? */
   int unit;                     /* Replacement Mapping is a UnitMap? */

/* Initialise the returned result. */
   result = -1;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Further initialisation. */
   new = NULL;
   ngone = 0;
   simpler = 0;

/* In series. */
/* ---------- */
/* Handle the case where the Mappings are connected in series. */
   if ( series ) {

/* Obtain the effective zoom factor of the nominated Mapping (which is
   a ZoomMap) when its Invert attribute is set to the value given. */
      zoom = astGetZoom( ( *map_list )[ where ] );
      if ( ( *invert_list )[ where ] ) zoom = 1.0 / zoom;

/* Search adjacent lower-numbered Mappings and identify the class to
   which they belong. */
      imap1 = where;
      while ( astOK && ( ( imap1 - 1 ) >= 0 ) ) {
         map = ( *map_list )[ imap1 - 1 ];
         class = astGetClass( map );
         if ( astOK ) {

/* For each ZoomMap found, obtain the effective zoom factor (taking
   account of the associated invert flag value), and accumulate the
   overall zoom factor to be used for the simplified Mapping. */
            if ( !strcmp( class, "ZoomMap" ) ) {
               if ( ( *invert_list )[ imap1 - 1 ] ) {
                  zoom /= astGetZoom( map );
               } else {
                  zoom *= astGetZoom( map );
               }

/* UnitMaps have an effective zoom factor of unity, so we include them
   but they so have no effect. Quit looping when the first Mapping is
   found which is not a ZoomMap or a UnitMap. */
            } else if ( strcmp( class, "UnitMap" ) ) {
               break;
            }
            imap1--;
         }
      }

/* Similarly search adjacent higher-numbered ZoomMaps and UnitMaps and
   accumulate their effective zoom factors. */
      imap2 = where;
      while ( astOK && ( ( imap2 + 1 ) < *nmap ) ) {
         map = ( *map_list )[ imap2 + 1 ];
         class = astGetClass( map );
         if ( astOK ) {
            if ( !strcmp( class, "ZoomMap" ) ) {
               if ( ( *invert_list )[ imap2 + 1 ] ) {
                  zoom /= astGetZoom( map );
               } else {
                  zoom *= astGetZoom( map );
               }
            } else if ( strcmp( class, "UnitMap" ) ) {
               break;
            }
            imap2++;
         }
      }

/* Determine how many Mappings can be eliminated by condensing those
   found above into a single Mapping. */
      ngone = imap2 - imap1;

/* Determine if the replacement Mapping can be a UnitMap. This will be
   the case only if the accumulated zoom factor is unity (within some
   tolerable error). */
      unit = ( fabs( zoom - 1.0 ) <= ( 8.0 * DBL_EPSILON ) );

/* Determine if simplification is possible. This will be so if (a)
   Mappings can be eliminated ("ngone" is non-zero), or (b) the
   replacement can be a UnitMap, or (c) where there was initially only
   one ZoomMap, its invert flag was set (it will always be cleared in
   the replacement Mapping). */
      simpler = ngone || unit || ( *invert_list )[ where ];

/* Do nothing more unless simplification is possible. */
      if ( simpler ) {

/* Obtain the number of input/output coordinates for the replacement
   Mapping and create the appropriate replacement. */
         nin = astGetNin( ( *map_list )[ where ] );
         if ( unit ) {
            new = (AstMapping *) astUnitMap( nin, "", status );
         } else {
            new = (AstMapping *) astZoomMap( nin, zoom, "", status );
         }
      }

/* In parallel. */
/* ------------ */
/* Handle the case where the Mappings are connected in parallel. */
   } else {

/* Obtain the number of input/output coordinates for the nominated Mapping. */
      nin = astGetNin( ( *map_list )[ where ] );

/* Search adjacent lower-numbered Mappings and identify the class to
   which they belong. */
      imap1 = where;
      while ( astOK && ( ( imap1 - 1 ) >= 0 ) ) {
         map = ( *map_list )[ imap1 - 1 ];
         class = astGetClass( map );
         if ( astOK ) {

/* Quit looping when the first Mapping is found which is not a ZoomMap
   or a UnitMap. */
            if ( strcmp( class, "ZoomMap" ) &&
                 strcmp( class, "UnitMap" ) ) break;

/* Accumulate the total number of input/output coordinates for each
   adjacent ZoomMap and UnitMap found. */
            nin += astGetNin( map );
            imap1--;
         }
      }

/* Similarly search adjacent higher-numbered Mappings and accumulate
   the number of input/output coordinates.*/
      imap2 = where;
      while ( astOK && ( ( imap2 + 1 ) < *nmap ) ) {
         map = ( *map_list )[ imap2 + 1 ];
         class = astGetClass( map );
         if ( astOK ) {
            if ( strcmp( class, "ZoomMap" ) &&
                 strcmp( class, "UnitMap" ) ) break;
            nin += astGetNin( map );
            imap2++;
         }
      }

/* Allocate memory for an array of zoom factors, one for each
   input/output coordinate in the replacement Mapping. */
      zooms = astMalloc( sizeof( double ) * (size_t) nin );
      if ( astOK ) {

/* Initialise. */
         nzoom = 0;
         minzoom = DBL_MAX;
         maxzoom = -DBL_MAX;

/* Loop through all the ZoomMaps and UnitMaps being merged and again
   identify the class to which they belong. */
         for ( imap = imap1; astOK && ( imap <= imap2 ); imap++ ) {
            map = ( *map_list )[ imap ];
            class = astGetClass( map );
            if ( astOK ) {

/* Set a default zoom factor of unity (for UnitMaps). If the Mapping
   is a ZoomMap, replace this with the effective zoom factor, taking
   account of the associated invert flag. */
               zoom = 1.0;
               if ( !strcmp( class, "ZoomMap" ) ) {
                  if ( ( *invert_list )[ imap ] ) {
                     zoom = 1.0 / astGetZoom( map );
                  } else {
                     zoom = astGetZoom( map );
                  }
               }

/* Replicate the zoom factor in the zoom factor array, once for each
   input/output coordinate associated with the current Mapping. */
               ncoord = astGetNin( map );
               for ( coord = 0; coord < ncoord; coord++ ) {
                  zooms[ nzoom++ ] = zoom;
               }

/* Store the minimum and maximum zoom factors encountered. */
               if ( zoom < minzoom ) minzoom = zoom;
               if ( zoom > maxzoom ) maxzoom = zoom;
            }
         }

/* Determine how many Mappings can be eliminated by condensing those
   found above into a single Mapping. */
         ngone = imap2 - imap1;

/* Determine whether the replacement Mapping can be a single
   ZoomMap. This will be so if all the effective zoom factors were the
   same. */
         single = ( minzoom == maxzoom );

/* Determine if the replacement Mapping can be a UnitMap. This will be
   so if all the effective zoom factors were equal to unity. */
         unit = single && ( minzoom == 1.0 );

/* Determine if simplification is possible. This will be so if (a)
   Mappings can be eliminated ("ngone" is non-zero), or (b) the
   replacement can be a UnitMap, or (c) where there was initially only
   one ZoomMap, its invert flag was set (it will always be cleared in
   the replacement Mapping). */
         simpler = ngone || unit || ( *invert_list )[ where ];

/* Do nothing more unless simplification is possible. */
         if ( simpler ) {

/* Create a replacement UnitMap or ZoomMap if appropriate. */
            if ( unit ) {
               new = (AstMapping *) astUnitMap( nin, "", status );
            } else if ( single ) {
               new = (AstMapping *) astZoomMap( nin, minzoom, "", status );

/* Otherwise, replace the original ZoomMaps and UnitMaps with a
   diagonal MatrixMap containing the zoom factors as its diagonal
   elements. */
            } else {
               new = (AstMapping *) astMatrixMap( nin, nin, 1, zooms, "", status );
            }
         }
      }

/* Free the array of zoom factors. */
      zooms = astFree( zooms );
   }

/* If OK, and simplification is possible, we now have a replacement
   Mapping and can insert it into the Mapping list. */
   if ( astOK ) {
      if ( simpler ) {

/* Annul the pointers to all the ZoomMaps that are being replaced. */
         for ( imap = imap1; imap <= imap2; imap++ ) {
            ( *map_list )[ imap ] = astAnnul( ( *map_list )[ imap ] );
         }

/* Insert the new pointer and the associated invert flag. */
         ( *map_list )[ imap1 ] = new;
         ( *invert_list )[ imap1 ] = 0;

/* Loop to close the resulting gap by moving subsequent elements down
   in the arrays. */
         for ( imap = imap2 + 1; imap < *nmap; imap++ ) {
            ( *map_list )[ imap - ngone ] = ( *map_list )[ imap ];
            ( *invert_list )[ imap - ngone ] = ( *invert_list )[ imap ];
         }

/* Clear the vacated elements at the end. */
         for ( imap = *nmap - ngone; imap < *nmap; imap++ ) {
            ( *map_list )[ imap ] = NULL;
            ( *invert_list )[ imap ] = 0;
         }

/* Decrement the Mapping count and return the index of the first
   modified element. */
         ( *nmap ) -= ngone;
         result = imap1;
      }

/* If an error occurred, but a new Mapping has been created, then
   annul it. */
   } else {
      if ( new ) new = astAnnul( new );
   }

/* If an error occurred, clear the returned result. */
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
*     ZoomMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "zoommap.h"
*     int *MapSplit( AstMapping *this, int nin, const int *in, AstMapping **map, int *status )

*  Class Membership:
*     ZoomMap method (over-rides the protected astMapSplit method
*     inherited from the Mapping class).

*  Description:
*     This function creates a new Mapping by picking specified inputs from
*     an existing ZoomMap. This is only possible if the specified inputs
*     correspond to some subset of the ZoomMap outputs. That is, there
*     must exist a subset of the ZoomMap outputs for which each output
*     depends only on the selected ZoomMap inputs, and not on any of the
*     inputs which have not been selected. If this condition is not met
*     by the supplied ZoomMap, then a NULL Mapping is returned.

*  Parameters:
*     this
*        Pointer to the ZoomMap to be split (the ZoomMap is not actually
*        modified by this function).
*     nin
*        The number of inputs to pick from "this".
*     in
*        Pointer to an array of indices (zero based) for the inputs which
*        are to be picked. This array should have "nin" elements. If "Nin"
*        is the number of inputs of the supplied ZoomMap, then each element
*        should have a value in the range zero to Nin-1.
*     map
*        Address of a location at which to return a pointer to the new
*        Mapping. This Mapping will have "nin" inputs (the number of
*        outputs may be different to "nin"). A NULL pointer will be
*        returned if the supplied ZoomMap has no subset of outputs which
*        depend only on the selected inputs.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a dynamically allocated array of ints. The number of
*     elements in this array will equal the number of outputs for the
*     returned Mapping. Each element will hold the index of the
*     corresponding output in the supplied ZoomMap. The array should be
*     freed using astFree when no longer needed. A NULL pointer will
*     be returned if no output Mapping can be created.

*  Notes:
*     - If this function is invoked with the global error status set,
*     or if it should fail for any reason, then NULL values will be
*     returned as the function value and for the "map" pointer.
*/

/* Local Variables: */
   AstZoomMap *this;          /* Pointer to ZoomMap structure */
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

/* Get a pointer to the ZoomMap structure. */
   this = (AstZoomMap *) this_map;

/* Allocate memory for the returned array and create a ZoomMap with the
   required number of axes. */
   result = astMalloc( sizeof( int )*(size_t) nin );
   *map = (AstMapping *) astZoomMap( nin, astGetZoom( this ), "", status );

/* Set its Invert attribute to be like the supplied ZoomMap. */
   astSetInvert( *map, astGetInvert( this ) );

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
*     #include "zoommap.h"
*     result = Rate( AstMapping *this, double *at, int ax1, int ax2, int *status )

*  Class Membership:
*     ZoomMap member function (overrides the astRate method inherited
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
   double result;

/* Check inherited status */
   if( !astOK ) return AST__BAD;

/* Result is zero if the axes differ */
   if( ax1 != ax2 ) {
      result = 0.0;

/* Otherwise, get the zoom factor. */
   } else {
      result = astGetZoom( (AstZoomMap *) this );

/* If the ZoomMap has been inverted, return the reciprocal of the zoom
   factor. */
      if ( astGetInvert( this ) ) {
         if( result != AST__BAD && result != 0.0 ) {
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
*     Set an attribute value for a ZoomMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "zoommap.h"
*     void SetAttrib( AstObject *this, const char *setting )

*  Class Membership:
*     ZoomMap member function (over-rides the astSetAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function assigns an attribute value for a ZoomMap, the
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
*        Pointer to the ZoomMap.
*     setting
*        Pointer to a null-terminated string specifying the new attribute
*        value.
*/

/* Local Variables: */
   AstZoomMap *this;             /* Pointer to the ZoomMap structure */
   double zoom;                  /* Zoom attribute value */
   int len;                      /* Length of setting string */
   int nc;                       /* Number of characters read by astSscanf */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the ZoomMap structure. */
   this = (AstZoomMap *) this_object;

/* Obtain the length of the setting string. */
   len = (int) strlen( setting );

/* Test for each recognised attribute in turn, using "astSscanf" to parse
   the setting string and extract the attribute value (or an offset to
   it in the case of string values). In each case, use the value set
   in "nc" to check that the entire string was matched. Once a value
   has been obtained, use the appropriate method to set it. */

/* Zoom. */
/* ----- */
   if ( nc = 0,
        ( 1 == astSscanf( setting, "zoom= %lf %n", &zoom, &nc ) )
        && ( nc >= len ) ) {
      astSetZoom( this, zoom );

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
*     Test if a specified attribute value is set for a ZoomMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "zoommap.h"
*     int TestAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     ZoomMap member function (over-rides the astTestAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     a value has been set for one of a ZoomMap's attributes.

*  Parameters:
*     this
*        Pointer to the ZoomMap.
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
   AstZoomMap *this;             /* Pointer to the ZoomMap structure */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the ZoomMap structure. */
   this = (AstZoomMap *) this_object;

/* Check the attribute name and test the appropriate attribute. */

/* Zoom. */
/* ----- */
   if ( !strcmp( attrib, "zoom" ) ) {
      result = astTestZoom( this );

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
*     Apply a ZoomMap to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "zoommap.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     ZoomMap member function (over-rides the astTransform protected
*     method inherited from the Mapping class).

*  Description:
*     This function takes a ZoomMap and a set of points encapsulated in a
*     PointSet and transforms the points so as to apply the required zoom
*     factor.

*  Parameters:
*     this
*        Pointer to the ZoomMap.
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
*     match the number of coordinates for the ZoomMap being applied.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstPointSet *result;          /* Pointer to output PointSet */
   AstZoomMap *map;              /* Pointer to ZoomMap to be applied */
   double **ptr_in;              /* Pointer to input coordinate data */
   double **ptr_out;             /* Pointer to output coordinate data */
   double scale;                 /* Scale factor to implement zoom */
   int coord;                    /* Loop counter for coordinates */
   int ncoord_in;                /* Number of coordinates per input point */
   int npoint;                   /* Number of points */
   int point;                    /* Loop counter for points */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the ZoomMap. */
   map = (AstZoomMap *) this;

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
   ncoord_in = astGetNcoord( in );
   npoint = astGetNpoint( in );
   ptr_in = astGetPoints( in );
   ptr_out = astGetPoints( result );

/* Determine whether to apply the forward or inverse mapping, according to the
   direction specified and whether the mapping has been inverted. */
   if ( astGetInvert( map ) ) forward = !forward;

/* Perform coordinate arithmetic. */
/* ------------------------------ */
/* Generate the coordinate scale factor, according to the direction
   of mapping required. */
   scale = astGetZoom( map );
   if ( !forward && astOK ) scale = 1.0 / scale;

/* Loop to apply the scale factor to all the coordinate values, checking for
   (and propagating) bad values in the process. */
   if ( astOK ) {
      for ( coord = 0; coord < ncoord_in; coord++ ) {
         for ( point = 0; point < npoint; point++ ) {
            if ( ptr_in[ coord ][ point ] == AST__BAD ) {
               ptr_out[ coord ][ point ] = AST__BAD;
            } else {
               ptr_out[ coord ][ point ] = ptr_in[ coord ][ point ] * scale;
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

/*
*att++
*  Name:
*     Zoom

*  Purpose:
*     ZoomMap scale factor.

*  Type:
*     Public attribute.

*  Synopsis:
*     Double precision.

*  Description:
*     This attribute holds the ZoomMap scale factor, by which
*     coordinate values are multiplied (by the forward transformation)
*     or divided (by the inverse transformation). This factor is set
*     when a ZoomMap is created, but may later be modified. The
*     default value is unity.
*
c     Note that if a ZoomMap is inverted (e.g. by using astInvert),
f     Note that if a ZoomMap is inverted (e.g. by using AST_INVERT),
*     then the reciprocal of this zoom factor will, in effect, be
*     used.

*  Applicability:
*     ZoomMap
*        All ZoomMaps have this attribute.

*  Notes:
*     - The Zoom attribute may not be set to zero.
*att--
*/
/* This ia a double value with a value of 0.0 when undefined but
   yielding a default of 1.0. Setting it explicitly to 0.0 is not
   permitted except via astClearZoom. */
astMAKE_CLEAR(ZoomMap,Zoom,zoom,0.0)
astMAKE_GET(ZoomMap,Zoom,double,1.0,( ( this->zoom == 0.0 ) ?
                                      1.0 : this->zoom ))

/* Check for an attempt to set a value of zero and report an error if
   necessary (leaving the Zoom value unchanged). */
astMAKE_SET(ZoomMap,Zoom,double,zoom,(
            ( value != 0.0 ) ?
            value :
            ( astError( AST__ZOOMI,
                       "astSetZoom(%s): A zoom factor of zero is not allowed.", status,
                        astGetClass( this ) ),
              this->zoom ) ))
astMAKE_TEST(ZoomMap,Zoom,( this->zoom != 0.0 ))

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
*     Dump function for ZoomMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the ZoomMap class to an output Channel.

*  Parameters:
*     this
*        Pointer to the ZoomMap whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstZoomMap *this;             /* Pointer to the ZoomMap structure */
   double dval;                  /* Double value */
   int set;                      /* Attribute value set? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the ZoomMap structure. */
   this = (AstZoomMap *) this_object;

/* Write out values representing the instance variables for the
   ZoomMap class.  Accompany these with appropriate comment strings,
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

/* Zoom. */
/* ----- */
   set = TestZoom( this, status );
   dval = set ? GetZoom( this, status ) : astGetZoom( this );
   astWriteDouble( channel, "Zoom", set, 1, dval, "Zoom factor" );
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAZoomMap and astCheckZoomMap functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(ZoomMap,Mapping)
astMAKE_CHECK(ZoomMap)

AstZoomMap *astZoomMap_( int ncoord, double zoom, const char *options, int *status, ...) {
/*
*++
*  Name:
c     astZoomMap
f     AST_ZOOMMAP

*  Purpose:
*     Create a ZoomMap.

*  Type:
*     Public function.

*  Synopsis:
c     #include "zoommap.h"
c     AstZoomMap *astZoomMap( int ncoord, double zoom,
c                             const char *options, ... )
f     RESULT = AST_ZOOMMAP( NCOORD, ZOOM, OPTIONS, STATUS )

*  Class Membership:
*     ZoomMap constructor.

*  Description:
*     This function creates a new ZoomMap and optionally initialises its
*     attributes.
*
*     A ZoomMap is a Mapping which "zooms" a set of points about the
*     origin by multiplying all coordinate values by the same scale
*     factor (the inverse transformation is performed by dividing by
*     this scale factor).

*  Parameters:
c     ncoord
f     NCOORD = INTEGER (Given)
*        The number of coordinate values for each point to be
*        transformed (i.e. the number of dimensions of the space in
*        which the points will reside). The same number is applicable
*        to both input and output points.
c     zoom
f     ZOOM = DOUBLE PRECISION (Given)
*        Initial scale factor by which coordinate values should be
*        multiplied (by the forward transformation) or divided (by the
*        inverse transformation). This factor may subsequently be
*        changed via the ZoomMap's Zoom attribute. It may be positive
*        or negative, but should not be zero.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new ZoomMap. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new ZoomMap. The syntax used is identical to that for the
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
c     astZoomMap()
f     AST_ZOOMMAP = INTEGER
*        A pointer to the new ZoomMap.

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
   AstZoomMap *new;              /* Pointer to new ZoomMap */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the ZoomMap, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitZoomMap( NULL, sizeof( AstZoomMap ), !class_init, &class_vtab,
                         "ZoomMap", ncoord, zoom );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new ZoomMap's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new ZoomMap. */
   return new;
}

AstZoomMap *astZoomMapId_( int ncoord, double zoom,
                           const char *options, ... ) {
/*
*  Name:
*     astZoomMapId_

*  Purpose:
*     Create a ZoomMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "zoommap.h"
*     AstZoomMap *astZoomMapId_( int ncoord, double zoom,
*                                const char *options, ... )

*  Class Membership:
*     ZoomMap constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astZoomMap constructor function. It returns an ID value (instead
*     of a true C pointer) to external users, and must be provided
*     because astZoomMap_ has a variable argument list which cannot be
*     encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     The variable argument list also prevents this function from
*     invoking astZoomMap_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astZoomMap_.

*  Returned Value:
*     The ID value associated with the new ZoomMap.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstZoomMap *new;              /* Pointer to new ZoomMap */
   va_list args;                 /* Variable argument list */

   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the ZoomMap, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitZoomMap( NULL, sizeof( AstZoomMap ), !class_init, &class_vtab,
                         "ZoomMap", ncoord, zoom );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new ZoomMap's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new ZoomMap. */
   return astMakeId( new );
}

AstZoomMap *astInitZoomMap_( void *mem, size_t size, int init,
                             AstZoomMapVtab *vtab, const char *name,
                             int ncoord, double zoom, int *status ) {
/*
*+
*  Name:
*     astInitZoomMap

*  Purpose:
*     Initialise a ZoomMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "zoommap.h"
*     AstZoomMap *astInitZoomMap( void *mem, size_t size, int init,
*                                 AstZoomMapVtab *vtab, const char *name,
*                                 int ncoord, double zoom )

*  Class Membership:
*     ZoomMap initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new ZoomMap object. It allocates memory (if necessary) to accommodate
*     the ZoomMap plus any additional data associated with the derived class.
*     It then initialises a ZoomMap structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a ZoomMap at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the ZoomMap is to be initialised.
*        This must be of sufficient size to accommodate the ZoomMap data
*        (sizeof(ZoomMap)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the ZoomMap (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the ZoomMap
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the ZoomMap's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new ZoomMap.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the astGetClass
*        method).
*     ncoord
*        The number of coordinate values per point.
*     zoom
*        The scale factor by which coordinate values are multiplied (by the
*        forward mapping) or divided (by the inverse mapping). This factor may
*        not be zero.

*  Returned Value:
*     A pointer to the new ZoomMap.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstZoomMap *new;              /* Pointer to new ZoomMap */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitZoomMapVtab( vtab, name );

/* Initialise. */
   new = NULL;

/* Check the initialisation value(s) for validity, reporting an error if
   necessary. Prefix the error report with the name of the function and the
   class of object being processed. */
   if ( zoom == 0.0 ) {
      astError( AST__ZOOMI, "astInitZoomMap(%s): A zoom factor of zero is not "
                "allowed.", status, name );
   } else {

/* Initialise a Mapping structure (the parent class) as the first component
   within the ZoomMap structure, allocating memory if necessary. Specify that
   the Mapping should be defined in both the forward and inverse directions. */
      new = (AstZoomMap *) astInitMapping( mem, size, 0,
                                           (AstMappingVtab *) vtab, name,
                                           ncoord, ncoord, 1, 1 );

      if ( astOK ) {

/* Initialise the ZoomMap data. */
/* ---------------------------- */
/* Store the zoom factor. */
         new->zoom = zoom;

/* If an error occurred, clean up by deleting the new ZoomMap. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Return a pointer to the new ZoomMap. */
   return new;
}

AstZoomMap *astLoadZoomMap_( void *mem, size_t size,
                             AstZoomMapVtab *vtab, const char *name,
                             AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadZoomMap

*  Purpose:
*     Load a ZoomMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "zoommap.h"
*     AstZoomMap *astLoadZoomMap( void *mem, size_t size,
*                                 AstZoomMapVtab *vtab, const char *name,
*                                 AstChannel *channel )

*  Class Membership:
*     ZoomMap loader.

*  Description:
*     This function is provided to load a new ZoomMap using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     ZoomMap structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a ZoomMap at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the ZoomMap is to be
*        loaded.  This must be of sufficient size to accommodate the
*        ZoomMap data (sizeof(ZoomMap)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the ZoomMap (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the ZoomMap structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstZoomMap) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new ZoomMap. If this is NULL, a pointer
*        to the (static) virtual function table for the ZoomMap class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "ZoomMap" is used instead.

*  Returned Value:
*     A pointer to the new ZoomMap.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstZoomMap *new;              /* Pointer to the new ZoomMap */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this ZoomMap. In this case the
   ZoomMap belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstZoomMap );
      vtab = &class_vtab;
      name = "ZoomMap";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitZoomMapVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built ZoomMap. */
   new = astLoadMapping( mem, size, (AstMappingVtab *) vtab, name,
                         channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "ZoomMap" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* Zoom. */
/* ----- */
      new->zoom = astReadDouble( channel, "zoom", 0.0 );
      if ( TestZoom( new, status ) ) SetZoom( new, new->zoom, status );

/* If an error occurred, clean up by deleting the new ZoomMap. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new ZoomMap pointer. */
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





