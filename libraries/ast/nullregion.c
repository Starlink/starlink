/*
*class++
*  Name:
*     NullRegion

*  Purpose:
*     A boundless region within a Frame.

*  Constructor Function:
c     astNullRegion
f     AST_NULLREGION

*  Description:
*     The NullRegion class implements a Region with no bounds within a Frame.
*     If the Negated attribute of a NullRegion is false, the NullRegion
*     represents a Region containing no points. If the Negated attribute of
*     a NullRegion is true, the NullRegion represents an infinite Region
*     (that is, all points in the coordinate system are inside the NullRegion).

*  Inheritance:
*     The NullRegion class inherits from the Region class.

*  Attributes:
*     The NullRegion class does not define any new attributes beyond
*     those which are applicable to all Regions.

*  Functions:
c     The NullRegion class does not define any new functions beyond those
f     The NullRegion class does not define any new routines beyond those
*     which are applicable to all Regions.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     11-OCT-2004 (DSB):
*        Original version.
*     20-JAN-2009 (DSB):
*        Over-ride astRegBasePick.
*     26-JAN-2009 (DSB):
*        Over-ride astMapMerge.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS NullRegion

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* Sets of points/coordinates */
#include "region.h"              /* Coordinate regions (parent class) */
#include "channel.h"             /* I/O channels */
#include "nullregion.h"          /* Interface definition for this class */
#include "mapping.h"             /* Position mappings */
#include "circle.h"              /* Circle regions */
#include "prism.h"               /* Extruded Regions */
#include "unitmap.h"             /* Unit Mapping */
#include "cmpframe.h"            /* Compound Frames */
#include "cmpmap.h"              /* Compound Mappings */

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
static AstMapping *(* parent_simplify)( AstMapping *, int * );


#ifdef THREAD_SAFE
/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(NullRegion)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(NullRegion,Class_Init)
#define class_vtab astGLOBAL(NullRegion,Class_Vtab)


#include <pthread.h>


#else


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstNullRegionVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstNullRegion *astNullRegionId_( void *, void *, const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstMapping *Simplify( AstMapping *, int * );
static AstPointSet *RegBaseMesh( AstRegion *, int * );
static AstPointSet *RegMesh( AstRegion *, int * );
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static AstRegion *RegBasePick( AstRegion *this, int, const int *, int * );
static AstRegion *GetDefUnc( AstRegion *, int * );
static AstRegion *MergeNullRegion( AstNullRegion *, AstRegion *, int, int * );
static int MapMerge( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
static int Overlap( AstRegion *, AstRegion *, int * );
static int OverlapX( AstRegion *, AstRegion *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void RegBaseBox( AstRegion *this, double *, double *, int * );
static void GetRegionBounds( AstRegion *this, double *, double *, int * );

/* Member functions. */
/* ================= */
static AstRegion *GetDefUnc( AstRegion *this, int *status ) {
/*
*+
*  Name:
*     astGetDefUnc

*  Purpose:
*     Obtain a pointer to the default uncertainty Region for a given Region.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "nullregion.h"
*     AstRegion *astGetDefUnc( AstRegion *this )

*  Class Membership:
*     NullRegion member function (over-rides the astGetDefUnc protected
*     method inherited from the Region class).

*  Description:
*     This function returns a pointer to a Region which represents the
*     default uncertainty associated with a position on the boundary of the
*     given  Region. The returned Region refers to the base Frame within the
*     FrameSet encapsulated by the supplied Region.

*  Parameters:
*     this
*        Pointer to the Region.

*  Returned Value:
*     A pointer to the Region. This should be annulled (using astAnnul)
*     when no longer needed.

*  Notes:
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstRegion *result;
   double *cen;
   double rad;
   int i;
   int n;

/* Initialise */
   result = NULL;

/* Check inherited status */
   if( !astOK ) return result;

/* Create a Circle centred on the origin with zero radius. */
   n = astGetNaxes( this );
   cen = astMalloc( sizeof(double)*(size_t) n );
   if( cen ) {
      for( i = 0; i < n; i++ ) cen[ i ] = 0.0;
      rad = 0.0;
      result = (AstRegion *) astCircle( this, 1, cen, &rad, NULL, "", status );
      cen = astFree( cen );
   }

/* Return the default uncertainty Region. */
   return result;
}

void astInitNullRegionVtab_(  AstNullRegionVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitNullRegionVtab

*  Purpose:
*     Initialise a virtual function table for a NullRegion.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "nullregion.h"
*     void astInitNullRegionVtab( AstNullRegionVtab *vtab, const char *name )

*  Class Membership:
*     NullRegion vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the NullRegion class.

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
   AstRegionVtab *region;        /* Pointer to Region component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitRegionVtab( (AstRegionVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsANullRegion) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstRegionVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   mapping = (AstMappingVtab *) vtab;
   region = (AstRegionVtab *) vtab;

   parent_transform = mapping->Transform;
   mapping->Transform = Transform;

   parent_simplify = mapping->Simplify;
   mapping->Simplify = Simplify;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   mapping->MapMerge = MapMerge;

   region->GetDefUnc = GetDefUnc;
   region->Overlap = Overlap;
   region->OverlapX = OverlapX;
   region->RegBaseBox = RegBaseBox;
   region->RegBaseMesh = RegBaseMesh;
   region->GetRegionBounds = GetRegionBounds;
   region->RegMesh = RegMesh;
   region->RegBasePick = RegBasePick;

/* Declare the copy constructor, destructor and class dump
   functions. */
   astSetDump( vtab, Dump, "NullRegion", "Boundless region" );

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
*     Simplify a sequence of Mappings containing a NullRegion.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     int MapMerge( AstMapping *this, int where, int series, int *nmap,
*                   AstMapping ***map_list, int **invert_list, int *status )

*  Class Membership:
*     NullRegion method (over-rides the protected astMapMerge method
*     inherited from the Region class).

*  Description:
*     This function attempts to simplify a sequence of Mappings by
*     merging a nominated NullRegion in the sequence with its neighbours,
*     so as to shorten the sequence if possible.
*
*     In many cases, simplification will not be possible and the
*     function will return -1 to indicate this, without further
*     action.
*
*     In most cases of interest, however, this function will either
*     attempt to replace the nominated NullRegion with a Mapping which it
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
*        Pointer to the nominated NullRegion which is to be merged with
*        its neighbours. This should be a cloned copy of the NullRegion
*        pointer contained in the array element "(*map_list)[where]"
*        (see below). This pointer will not be annulled, and the
*        NullRegion it identifies will not be modified by this function.
*     where
*        Index in the "*map_list" array (below) at which the pointer
*        to the nominated NullRegion resides.
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
   AstNullRegion *oldint;  /* Pointer to supplied NullRegion */
   AstMapping *map;      /* Pointer to adjacent Mapping */
   AstMapping *new;      /* Simplified or merged Region */
   int i1;               /* Index of first Mapping merged */
   int i;                /* Loop counter */
   int result;           /* Result value to return */

/* Initialise. */
   result = -1;
   i1 = -1;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the NullRegion. */
   oldint = (AstNullRegion *) this;

/* First of all, see if the NullRegion can be replaced by a simpler Region,
   without reference to the neighbouring Regions in the list.           */
/* =====================================================================*/

/* Try to simplify the NullRegion. If the pointer value has changed, we assume
   some simplification took place. */
   new = astSimplify( oldint );
   if( new != (AstMapping *) oldint ) {

/* Annul the NullRegion pointer in the list and replace it with the new Region
   pointer, and indicate that the forward transformation of the returned
   Region should be used (not really needed but keeps things clean). */
      (void) astAnnul( ( *map_list )[ where ] );
      ( *map_list )[ where ] = new;
      ( *invert_list )[ where ] = 0;

/* Return the index of the first modified element. */
      result = where;

/* If the NullRegion itself could not be simplified, see if it can be merged
   with the Regions on either side of it in the list. We can only merge
   in parallel. */
/* =====================================================================*/
   } else if( ! series ){
      new = astAnnul( new );

/* Attempt to merge the NullRegion with its lower neighbour (if any). */
      if( where > 0 ) {
         i1 = where - 1;
         map = ( *map_list )[ where - 1 ];
         if( astIsARegion( map ) ) {
            new = (AstMapping *) MergeNullRegion( oldint, (AstRegion *) map,
                                                  0, status );
         }
      }

/* If this did not produced a merged Region, attempt to merge the NullRegion
   with its upper neighbour (if any). */
      if( !new && where < *nmap - 1 ) {
         i1 = where;
         map = ( *map_list )[ where + 1 ];
         if( astIsARegion( map ) ) {
            new = (AstMapping *) MergeNullRegion( oldint, (AstRegion *) map,
                                                  1, status );
         }
      }

/* If succesfull... */
      if( new ){

/* Annul the first of the two Mappings, and replace it with the merged
   Region. Also clear the invert flag. */
         (void) astAnnul( ( *map_list )[ i1 ] );
         ( *map_list )[ i1 ] = new;
         ( *invert_list )[ i1 ] = 0;

/* Annul the second of the two Mappings, and shuffle down the rest of the
   list to fill the gap. */
         (void) astAnnul( ( *map_list )[ i1 + 1 ] );
         for ( i = i1 + 2; i < *nmap; i++ ) {
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

   } else {
      new = astAnnul( new );
   }

/* Return the result. */
   return result;
}

static AstRegion *MergeNullRegion( AstNullRegion *this, AstRegion *reg,
                                   int intfirst, int *status ) {
/*
*  Name:
*     MergeNullRegion

*  Purpose:
*     Attempt to merge a NullRegion with another Region to form a Region of
*     higher dimensionality.

*  Type:
*     Private function.

*  Synopsis:
*     #include "nullregion.h"
*     AstRegion *MergeNullRegion( AstNullRegion *this, AstRegion *reg,
*                                 int intfirst, int *status )

*  Class Membership:
*     NullRegion member function.

*  Description:
*     This function attempts to combine the supplied Regions together
*     into a Region of higher dimensionality.

*  Parameters:
*     this
*        Pointer to a NullRegion.
*     reg
*        Pointer to another Region.
*     intfirst
*        If non-zero, then the NullRegion axes are put first in the new Region.
*        Otherwise, the other Region's axes are put first.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     A pointer to a new region, or NULL if the supplied Regions could
*     not be merged.
*/

/* Local Variables: */
   AstFrame *bfrm;           /* Pointer to base Frame for "result" */
   AstFrame *cfrm;           /* Pointer to current Frame for "result" */
   AstFrame *frm_reg;        /* Pointer to Frame from "reg" */
   AstFrame *frm_this;       /* Pointer to Frame from "this" */
   AstMapping *bcmap;        /* Base->current Mapping for "result" */
   AstMapping *map_reg;      /* Base->current Mapping from "reg" */
   AstMapping *map_this;     /* Base->current Mapping from "this" */
   AstMapping *sbunc;        /* Simplified uncertainty */
   AstRegion *bunc;          /* Base Frame uncertainty Region */
   AstRegion *new;           /* Pointer to new NullRegion in base Frame */
   AstRegion *result;        /* Pointer to returned NullRegion in current Frame */
   AstRegion *unc_reg;       /* Current Frame uncertainty Region from "reg" */
   AstRegion *unc_this;      /* Current Frame uncertainty Region from "this" */
   double fac_reg;           /* Ratio of used to default MeshSize for "reg" */
   double fac_this;          /* Ratio of used to default MeshSize for "this" */
   int msz_reg;              /* Original MeshSize for "reg" */
   int msz_reg_set;          /* Was MeshSize originally set for "reg"? */
   int msz_this;             /* Original MeshSize for "this" */
   int msz_this_set;         /* Was MeshSize originally set for "this"? */
   int nax;                  /* Number of axes in "result" */
   int nax_reg;              /* Number of axes in "reg" */
   int nax_this;             /* Number of axes in "this" */
   int neg_reg;              /* Negated attribute value for other supplied Region */
   int neg_this;             /* Negated attribute value for supplied NullRegion  */

/* Initialise */
   result = NULL;

/* Check the local error status. */
   if ( !astOK ) return result;

/* Get the Closed attributes of the two Regions. They must be the same in
   each Region if we are to merge the Regions. In addition, in order to
   merge, either both Regions must have a defined uncertainty, or neither
   Region must have a defined Uncertainty. */
   if( astGetClosed( this ) == astGetClosed( reg ) &&
       astTestUnc( this ) == astTestUnc( reg ) ) {

/* Get the Nagated attributes of the two Regions. */
      neg_this = astGetNegated( this );
      neg_reg = astGetNegated( reg );

/* Get the number of axes in the two supplied Regions. */
      nax_reg = astGetNaxes( reg );
      nax_this = astGetNaxes( this );

/* If the Regions can be combined, get the number of axes the
   combination will have. */
      nax = nax_reg + nax_this;

/* Get the base Frames from the two Region FrameSets, and combine them
   into a single CmpFrame that will be used to create any new Region. */
      frm_this = astGetFrame( ((AstRegion *) this)->frameset, AST__BASE );
      frm_reg = astGetFrame( reg->frameset, AST__BASE );

      if( intfirst ) {
         bfrm = (AstFrame *) astCmpFrame( frm_this, frm_reg, "", status );
      } else {
         bfrm = (AstFrame *) astCmpFrame( frm_reg, frm_this, "", status );
      }

      frm_this = astAnnul( frm_this );
      frm_reg = astAnnul( frm_reg );

/* Indicate we do not yet have a merged Region. */
      new = NULL;

/* We only check for merging with another NullRegion (other classes such
   as Box and Interval check for merging of NullRegions with other classes).
   The result will be an NullRegion. Both Regions must have the same value
   for the Negated flag. */
      if( astIsANullRegion( reg ) && neg_this == neg_reg ) {
         new = (AstRegion *) astNullRegion( bfrm, NULL, "", status );

/* Propagate remaining attributes of the supplied Region to it. */
         astRegOverlay( new, this, 1 );

/* Ensure the Negated flag is set correctly in the returned NullRegion. */
         if( neg_this ) {
            astSetNegated( new, neg_this );
         } else {
            astClearNegated( new );
         }

/* If both the supplied Regions have uncertainty, assign the new Region an
   uncertainty. */
         if( astTestUnc( this ) && astTestUnc( reg ) ) {

/* Get the uncertainties from the two supplied Regions. */
            unc_this = astGetUncFrm( this, AST__BASE );
            unc_reg = astGetUncFrm( reg, AST__BASE );

/* Combine them into a single Region (a Prism), in the correct order. */
            if( intfirst ) {
               bunc = (AstRegion *) astPrism( unc_this, unc_reg, "", status );
            } else {
               bunc = (AstRegion *) astPrism( unc_reg, unc_this, "", status );
            }

/* Attempt to simplify the Prism. */
            sbunc = astSimplify( bunc );

/* Use the simplified Prism as the uncertainty for the returned Region. */
            astSetUnc( new, sbunc );

/* Free resources. */
            sbunc = astAnnul( sbunc );
            bunc = astAnnul( bunc );
            unc_reg = astAnnul( unc_reg );
            unc_this = astAnnul( unc_this );
         }

/* Get the current Frames from the two Region FrameSets, and combine them
   into a single CmpFrame. */
         frm_this = astGetFrame( ((AstRegion *) this)->frameset, AST__CURRENT );
         frm_reg = astGetFrame( reg->frameset, AST__CURRENT );

         if( intfirst ) {
            cfrm = (AstFrame *) astCmpFrame( frm_this, frm_reg, "", status );
         } else {
            cfrm = (AstFrame *) astCmpFrame( frm_reg, frm_this, "", status );
         }

/* Get the base -> current Mappings from the two Region FrameSets, and
   combine them into a single parallel CmpMap that connects bfrm and cfrm. */
         map_this = astGetMapping( ((AstRegion *) this)->frameset, AST__BASE,
                                   AST__CURRENT );
         map_reg = astGetMapping( reg->frameset, AST__BASE, AST__CURRENT );

         if( intfirst ) {
            bcmap = (AstMapping *) astCmpMap( map_this, map_reg, 0, "",
                                              status );
         } else {
            bcmap = (AstMapping *) astCmpMap( map_reg, map_this, 0, "",
                                              status );
         }

/* Map the new Region into the new current Frame. */
         result = astMapRegion( new, bcmap, cfrm );

/* The filling factor in the returned is the product of the filling
   factors for the two supplied Regions. */
         if( astTestFillFactor( reg ) || astTestFillFactor( this ) ) {
            astSetFillFactor( result, astGetFillFactor( reg )*
                                      astGetFillFactor( this ) );
         }

/* If the MeshSize value is set in either supplied Region, set a value
   for the returned Region which scales the default value by the
   product of the scaling factors for the two supplied Regions. First see
   if either MeshSize value is set. */
         msz_this_set = astTestMeshSize( this );
         msz_reg_set = astTestMeshSize( reg );
         if( msz_this_set || msz_reg_set ) {

/* If so, get the two MeshSize values (one of which may be a default
   value), and then clear them so that the default value will be returned
   in future. */
            msz_this = astGetMeshSize( this );
            msz_reg = astGetMeshSize( reg );
            astClearMeshSize( this );
            astClearMeshSize( reg );

/* Get the ratio of the used MeshSize to the default MeshSize for both
   Regions. */
            fac_this = (double)msz_this/(double)astGetMeshSize( this );
            fac_reg = (double)msz_reg/(double)astGetMeshSize( reg );

/* The MeshSize of the returned Returned is the default value scaled by
   the product of the two ratios found above. */
            astSetMeshSize( result, fac_this*fac_reg*astGetMeshSize( result ) );

/* Re-instate the original MeshSize values for the supplied Regions (if
   set) */
            if( msz_this_set ) astSetMeshSize( this, msz_this );
            if( msz_reg_set ) astSetMeshSize( reg, msz_reg );
         }

/* Free remaining resources */
         frm_this = astAnnul( frm_this );
         frm_reg = astAnnul( frm_reg );
         map_this = astAnnul( map_this );
         map_reg = astAnnul( map_reg );
         bcmap = astAnnul( bcmap );
         new = astAnnul( new );
         cfrm = astAnnul( cfrm );
      }
   }

/* If an error has occurred, annul the returned pointer. */
   if( !astOK ) result = astAnnul( result );

/* Return the result. */
   return result;
}

static int Overlap( AstRegion *this, AstRegion *that, int *status ){
/*
*  Name:
*     Overlap

*  Purpose:
*     Test if two regions overlap each other.

*  Type:
*     Private function.

*  Synopsis:
*     #include "nullregion.h"
*     int Overlap( AstRegion *this, AstRegion *that )

*  Class Membership:
*     NullRegion member function (over-rides the astOverlap method inherited
*     from the Region class).

*  Description:
*     This function returns an integer value indicating if the two
*     supplied Regions overlap. The two Regions are converted to a commnon
*     coordinate system before performing the check. If this conversion is
*     not possible (for instance because the two Regions represent areas in
*     different domains), then the check cannot be performed and a zero value
*     is returned to indicate this.

*  Parameters:
*     this
*        Pointer to the first Region.
*     that
*        Pointer to the second Region.

*  Returned Value:
*     astOverlap()
*        A value indicating if there is any overlap between the two Regions.
*        Possible values are:
*
*        0 - The check could not be performed because the second Region
*            could not be mapped into the coordinate system of the first
*            Region.
*
*        1 - There is no overlap between the two Regions.
*
*        2 - The first Region is completely inside the second Region.
*
*        3 - The second Region is completely inside the first Region.
*
*        4 - There is partial overlap between the two Regions.
*
*        5 - The Regions are identical.
*
*        6 - The second Region is the negation of the first Region.

*  Notes:
*     - The returned values 5 and 6 do not check the value of the Closed
*     attribute in the two Regions.
*     - A value of zero will be returned if this function is invoked with the
*     AST error status set, or if it should fail for any reason.

* Implementation Notes:
*    - This function is simply a wrap-up for the OverlapX function
*    which performs the required processing but swaps the order of the
*    two arguments. This is a trick to allow the astOverlap
*    method to be over-ridden by derived classes on the basis of the
*    class of either of the two arguments.
*/

/* Check the inherited status. */
   if ( !astOK ) return 0;

/* Invoke the private "OverlapX" member function with the two arguments
   swapped. */
   return OverlapX( that, this, status );
}

static int OverlapX( AstRegion *that, AstRegion *this, int *status ){
/*
*  Name:
*     OverlapX

*  Purpose:
*     Test if two regions overlap each other.

*  Type:
*     Private function.

*  Synopsis:
*     #include "nullregion.h"
*     int OverlapX( AstRegion *that, AstRegion *this )

*  Class Membership:
*     NullRegion member function (over-rides the astOverlapX method inherited
*     from the Region class).

*  Description:
*     This function performs the processing for the public astOverlap
*     method (as inherited from the Region class and over-ridden by the
*     NullRegion class) and has exactly the same interface except that
*     the order of the two arguments is swapped. This is a trick
*     to allow the astOverlap method to be over-ridden by derived
*     classes on the basis of the class of either of its two
*     arguments.
*
*     See the astOverlap method for details of the interface.

*/

/* Local Variables: */
   AstFrameSet *fs;               /* FrameSet connecting Region Frames */
   int result;                    /* Returned value */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Check that the Regions can be compared meaningfully. */
   fs = astConvert( this, that, "" );
   if( fs ) {
      fs = astAnnul( fs );

/* If both the supplied Regions are NullRegion... */
      if( astIsANullRegion( this ) && astIsANullRegion( that ) ) {

/* If the Negated attributes are equal, the Regions are identical.
   Otherwise they are mutually exclusive. */
         if( astGetNegated( this ) == astGetNegated( that ) ) {
            result = 5;
         } else {
            result = 6;
         }

/* If one of the supplied Region is a NullRegion containing no points,
   then there is no overlap. */
      } else if( ( astIsANullRegion( this ) && !astGetNegated( this ) ) ||
                 ( astIsANullRegion( that ) && !astGetNegated( that ) ) ) {
         result = 1;

/* If "that" is infinite and "this" is not infinite, then "this" is
   entirely inside "that". */
      } else if( astIsANullRegion( that ) && astGetNegated( that ) ) {
         result = 2;

/* If "this" is infinite and "that" is not infinite, then "that" is
   entirely inside "this". */
      } else if( astIsANullRegion( this ) && astGetNegated( this ) ){
         result = 3;

/* Otherwise there is partial overlap. */
      } else {
         result = 4;
      }
   }

/* If not OK, return zero. */
   if( !astOK ) result = 0;

/* Return the result. */
   return result;
}

static void RegBaseBox( AstRegion *this_region, double *lbnd, double *ubnd, int *status ){
/*
*  Name:
*     RegBaseBox

*  Purpose:
*     Returns the bounding box of an un-negated Region in the base Frame of
*     the encapsulated FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "nullregion.h"
*     void RegBaseBox( AstRegion *this, double *lbnd, double *ubnd, int *status )

*  Class Membership:
*     NullRegion member function (over-rides the astRegBaseBox protected
*     method inherited from the Region class).

*  Description:
*     This function returns the upper and lower axis bounds of a Region in
*     the base Frame of the encapsulated FrameSet, assuming the Region
*     has not been negated. That is, the value of the Negated attribute
*     is ignored.

*  Parameters:
*     this
*        Pointer to the Region.
*     lbnd
*        Pointer to an array in which to return the lower axis bounds
*        covered by the Region in the base Frame of the encapsulated
*        FrameSet. It should have at least as many elements as there are
*        axes in the base Frame.
*     ubnd
*        Pointer to an array in which to return the upper axis bounds
*        covered by the Region in the base Frame of the encapsulated
*        FrameSet. It should have at least as many elements as there are
*        axes in the base Frame.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstNullRegion *this;          /* Pointer to NullRegion structure */
   int i;                        /* Axis index */
   int nc;                       /* No. of axes in base Frame */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the NullRegion structure */
   this = (AstNullRegion *) this_region;

/* Get the number of base Frame axes. */
   nc = astGetNin( this_region->frameset );

/* Set the upper bound less than the lower bound to indicate that the region
   contains no points. */
   for( i = 0; i < nc; i++ ) {
      lbnd[ i ] = 1.0;
      ubnd[ i ] = -1.0;
   }

}

static AstPointSet *RegBaseMesh( AstRegion *this, int *status ){
/*
*  Name:
*     RegBaseMesh

*  Purpose:
*     Return a PointSet containing a mesh of points on the boundary of a
*     Region in its base Frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "nullregion.h"
*     AstPointSet *astRegBaseMesh( AstRegion *this, int *status )

*  Class Membership:
*     NullRegion member function (over-rides the astRegBaseMesh protected
*     method inherited from the Region class).

*  Description:
*     This function returns a PointSet containing a mesh of points on the
*     boundary of the Region. The points refer to the base Frame of
*     the encapsulated FrameSet. Note, a NullRegion has no boundary. This
*     is indicated by returned a PointSet containing a single point with a
*     value of AST__BAD on every axis.

*  Parameters:
*     this
*        Pointer to the Region.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*    Pointer to a new PointSet containing a single point with value of
*    AST__BAD on every axis.

*-
*/

/* Local Variables: */
   AstPointSet *result;
   double **ptr;
   int i;
   int nc;

/* Initialise */
   result = NULL;

/* Check the inherited status */
   if( !astOK ) return result;

/* If the Region structure contains a pointer to a PointSet holding
   a previously created mesh, return it. */
   if( this->basemesh ) {
      result = astClone( this->basemesh );

/* Otherwise, create a new mesh. */
   } else {

/* Get the number of base Frame axes */
      nc = astGetNin( this->frameset );

/* Create the PointSet. */
      result = astPointSet( 1, nc, "", status );

/* Get a pointer to the axis values. */
      ptr = astGetPoints( result );

/* If OK, store AST__BAD on every axis. */
      if( ptr ) for( i = 0; i < nc; i++ ) ptr[ i ][ 0 ] = AST__BAD;

/* Same the returned pointer in the Region structure so that it does not
   need to be created again next time this function is called. */
      if( astOK && result ) this->basemesh = astClone( result );

   }

/* Return the result. */
   return result;
}

static AstRegion *RegBasePick( AstRegion *this_region, int naxes,
                               const int *axes, int *status ){
/*
*  Name:
*     RegBasePick

*  Purpose:
*     Return a Region formed by picking selected base Frame axes from the
*     supplied Region.

*  Type:
*     Private function.

*  Synopsis:
*     #include "nullregion.h"
*     AstRegion *RegBasePick( AstRegion *this, int naxes, const int *axes,
*                             int *status )

*  Class Membership:
*     NullRegion member function (over-rides the astRegBasePick protected
*     method inherited from the Region class).

*  Description:
*     This function attempts to return a Region that is spanned by selected
*     axes from the base Frame of the encapsulated FrameSet of the supplied
*     Region. This may or may not be possible, depending on the class of
*     Region. If it is not possible a NULL pointer is returned.

*  Parameters:
*     this
*        Pointer to the Region.
*     naxes
*        The number of base Frame axes to select.
*     axes
*        An array holding the zero-based indices of the base Frame axes
*        that are to be selected.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the Region, or NULL if no region can be formed.

*  Notes:
*    - A NULL pointer is returned if an error has already occurred, or if
*    this function should fail for any reason.
*/

/* Local Variables: */
   AstFrame *bfrm;         /* The base Frame in the supplied Region */
   AstFrame *frm;          /* The base Frame in the returned Region */
   AstRegion *bunc;        /* The uncertainty in the supplied Region */
   AstRegion *result;      /* Returned Region */
   AstRegion *unc;         /* The uncertainty in the returned Region */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the base Frame of the encapsulated FrameSet. */
   bfrm = astGetFrame( this_region->frameset, AST__BASE );

/* Create a Frame by picking the selected axes from the base Frame of the
   encapsulated FrameSet. */
   frm = astPickAxes( bfrm, naxes, axes, NULL );

/* Get the uncertainty Region (if any) within the base Frame of the supplied
   Region, and select the required axes from it. If the resulting Object
   is not a Region, annul it so that the returned Region will have no
   uncertainty. */
   if( astTestUnc( this_region ) ) {
      bunc = astGetUncFrm( this_region, AST__BASE );
      unc = astPickAxes( bunc, naxes, axes, NULL );
      bunc = astAnnul( bunc );

      if( ! astIsARegion( unc ) ) unc = astAnnul( unc );

   } else {
      unc = NULL;
   }

/* Create the new NullRegion. */
   result = (AstRegion *) astNullRegion( frm, unc, "", status );

/* Free resources */
   frm = astAnnul( frm );
   bfrm = astAnnul( bfrm );
   if( unc ) unc = astAnnul( unc );

/* Return a NULL pointer if an error has occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return the result. */
   return result;
}

static void GetRegionBounds( AstRegion *this_region, double *lbnd, double *ubnd, int *status ){
/*
*  Name:
*     GetRegionBounds

*  Purpose:
*     Returns the bounding box of an un-negated Region in the current Frame of
*     the encapsulated FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "nullregion.h"
*     void astGetRegionBounds( AstRegion *this, double *lbnd, double *ubnd, int *status )

*  Class Membership:
*     NullRegion member function (over-rides the astGetRegionBounds protected
*     method inherited from the Region class).

*  Description:
*     This function returns the upper and lower axis bounds of a Region in
*     the current Frame of the encapsulated FrameSet, assuming the Region
*     has not been negated. That is, the value of the Negated attribute
*     is ignored.

*  Parameters:
*     this
*        Pointer to the Region.
*     lbnd
*        Pointer to an array in which to return the lower axis bounds
*        covered by the Region in the current Frame of the encapsulated
*        FrameSet. It should have at least as many elements as there are
*        axes in the Region.
*     ubnd
*        Pointer to an array in which to return the upper axis bounds
*        covered by the Region in the current Frame of the encapsulated
*        FrameSet. It should have at least as many elements as there are
*        axes in the Region.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstNullRegion *this;          /* Pointer to NullRegion structure */
   int i;                        /* Axis index */
   int nc;                       /* No. of axes in base Frame */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the NullRegion structure */
   this = (AstNullRegion *) this_region;

/* Get the number of base Frame axes. */
   nc = astGetNin( this_region->frameset );

/* Set the upper bound less than the lower bound to indicate that the region
   contains no points. */
   for( i = 0; i < nc; i++ ) {
      lbnd[ i ] = 1.0;
      ubnd[ i ] = -1.0;
   }

}

static AstPointSet *RegMesh( AstRegion *this, int *status ){
/*
*  Name:
*     RegMesh

*  Purpose:
*     Return a PointSet containing points spread over the boundary of a
*     Region.

*  Type:
*     Private function.

*  Synopsis:
*     #include "nullregion.h"
*     AstPointSet *astRegMesh( AstRegion *this, int *status )

*  Class Membership:
*     NullRegion member function (over-rides the astRegMesh protected
*     method inherited from the Region class).

*  Description:
*     This function returns a PointSet containing a mesh of points on the
*     boundary of the Region. The points refer to the current Frame of
*     the encapsulated FrameSet. Note, a NullRegion has no boundary and
*     so an error is reported if this function is called.

*  Parameters:
*     this
*        Pointer to the Region.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     NULL pointer.

*  Notes:
*     - This implementation reports and error and returns NULL since a
*     NullRegion has no boundary.
*-
*/

   astError( AST__INTER, "astRegMesh(%s): The %s class does not implement "
             "the astRegMesh method inherited from the Region class "
             "(internal AST programming error).", status, astGetClass( this ),
             astGetClass( this ) );
   return NULL;
}

static AstMapping *Simplify( AstMapping *this_mapping, int *status ) {
/*
*  Name:
*     Simplify

*  Purpose:
*     Simplify the Mapping represented by a Region.

*  Type:
*     Private function.

*  Synopsis:
*     #include "nullregion.h"
*     AstMapping *Simplify( AstMapping *this, int *status )

*  Class Membership:
*     NullRegion method (over-rides the astSimplify method inherited
*     from the Region class).

*  Description:
*     This function invokes the parent Region Simplify method, and then
*     performs any further region-specific simplification.
*
*     If the Mapping from base to current Frame is not a UnitMap, this
*     will include attempting to fit a new Region to the boundary defined
*     in the current Frame.

*  Parameters:
*     this
*        Pointer to the original Region.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the simplified Region. A cloned pointer to the
*     supplied Region will be returned if no simplication could be
*     performed.

*  Notes:
*     - A NULL pointer value will be returned if this function is
*     invoked with the AST error status set, or if it should fail for
*     any reason.
*/

/* Local Variables: */
   AstFrame *frm;             /* Current Frame */
   AstMapping *map;           /* Base->current Mapping */
   AstMapping *result;        /* Result pointer to return */
   AstRegion *new;            /* Simplified Region */
   AstRegion *this;           /* Pointer to supplied Region structure */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the supplied Region structure. */
   this = (AstRegion *) this_mapping;

/* Invoke the parent Simplify method inherited from the Region class. This
   will simplify the encapsulated FrameSet and uncertainty Region. */
   new = (AstRegion *) (*parent_simplify)( this_mapping, status );

/* Is the Mapping from base Frame to current Frame in the Region a
   UnitMap? If so, no simplification is possible. */
   map = astGetMapping( new->frameset, AST__BASE, AST__CURRENT );
   if( astIsAUnitMap( map ) ){
      result = astClone( new );

   } else {

/* Create a NullRegion from the current Frame. */
      frm = astGetFrame( new->frameset, AST__CURRENT );
      result = (AstMapping *) astNullRegion( frm, astGetUnc( new, 0 ), "", status );

/* Free resources. */
      frm = astAnnul( frm );

   }
   map = astAnnul( map );
   new = astAnnul( new );

/* If any simplification could be performed, copy Region attributes from
   the supplied Region to the returned Region, and return a pointer to it.
   If the supplied Region had no uncertainty, ensure the returned Region
   has no uncertainty. Otherwise, return a clone of the supplied pointer. */
   if( result != this_mapping ) astRegOverlay( result, this, 1 );

/* If an error occurred, annul the returned pointer. */
   if ( !astOK ) result = astAnnul( result );

/* Return the result. */
   return result;
}

static AstPointSet *Transform( AstMapping *this_mapping, AstPointSet *in,
                               int forward, AstPointSet *out, int *status ) {
/*
*  Name:
*     Transform

*  Purpose:
*     Apply a NullRegion to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "nullregion.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     NullRegion member function (over-rides the astTransform protected
*     method inherited from the Mapping class).

*  Description:
*     This function takes a NullRegion and a set of points encapsulated in a
*     PointSet and transforms the points by setting axis values to
*     AST__BAD for all points which are outside the region. Points inside
*     the region are copied unchanged from input to output.

*  Parameters:
*     this
*        Pointer to the NullRegion.
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
*     -  The forward and inverse transformations are identical for a
*     Region.
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*     -  The number of coordinate values per point in the input PointSet must
*     match the number of axes in the Frame represented by the NullRegion.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstNullRegion *this;          /* Pointer to NullRegion */
   AstPointSet *result;          /* Pointer to output PointSet */
   double **ptr_out;             /* Pointer to output coordinate data */
   double *p;                    /* Pointer to next axis value */
   int coord;                    /* Zero-based index for coordinates */
   int ncoord_out;               /* No. of coordinates per output point */
   int npoint_out;               /* No. of points */
   int point;                    /* Loop counter for points */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the NullRegion structure. */
   this = (AstNullRegion *) this_mapping;

/* Apply the parent mapping using the stored pointer to the Transform member
   function inherited from the parent Region class. This function validates
   all arguments and generates an output PointSet if necessary,
   containing a copy of the input PointSet. */
   result = (*parent_transform)( this_mapping, in, forward, out, status );

/* We will now extend the parent astTransform method by performing the
   calculations needed to generate the output coordinate values. */

/* If the NullRegion has been inverted, it represents an infinite region
   which includes all points, so just retain the copy of the supplied
   PointSet created by the parent Transform method above. If the NullRegion
   has not been inverted, it contains no points and so set all output points
   bad. */
   if( !astGetNegated( this ) ) {
      ncoord_out = astGetNcoord( result );
      npoint_out = astGetNpoint( result );
      ptr_out = astGetPoints( result );
      if ( astOK ) {
         for ( coord = 0; coord < ncoord_out; coord++ ) {
            p = ptr_out[ coord ];
            for ( point = 0; point < npoint_out; point++ ) {
               *(p++) = AST__BAD;
            }
         }
      }
   }

/* Annul the result if an error has occurred. */
   if( !astOK ) result = astAnnul( result );

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
/* None */

/* Destructor. */
/* ----------- */
/* None */

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for NullRegion objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the NullRegion class to an output Channel.

*  Parameters:
*     this
*        Pointer to the NullRegion whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstNullRegion *this;                 /* Pointer to the NullRegion structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the NullRegion structure. */
   this = (AstNullRegion *) this_object;

/* Write out values representing the instance variables for the
   NullRegion class.  Accompany these with appropriate comment strings,
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
/* Implement the astIsANullRegion and astCheckNullRegion functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(NullRegion,Region)
astMAKE_CHECK(NullRegion)

AstNullRegion *astNullRegion_( void *frame_void, AstRegion *unc, const char *options, int *status, ...) {
/*
*++
*  Name:
c     astNullRegion
f     AST_NULLREGION

*  Purpose:
*     Create a NullRegion.

*  Type:
*     Public function.

*  Synopsis:
c     #include "nullregion.h"
c     AstNullRegion *astNullRegion( AstFrame *frame, AstRegion *unc, const char *options, ... )
f     RESULT = AST_NULLREGION( FRAME, UNC, OPTIONS, STATUS )

*  Class Membership:
*     NullRegion constructor.

*  Description:
*     This function creates a new NullRegion and optionally initialises its
*     attributes.
*
*     A NullRegion is a Region with no bounds. If the Negated attribute of a
*     NullRegion is false, the NullRegion represents a Region containing no
*     points. If the Negated attribute of a NullRegion is true, the NullRegion
*     represents an infinite Region containing all points within the
*     coordinate system.

*  Parameters:
c     frame
f     FRAME = INTEGER (Given)
*        A pointer to the Frame in which the region is defined. A deep
*        copy is taken of the supplied Frame. This means that any
*        subsequent changes made to the Frame using the supplied pointer
*        will have no effect the Region.
c     unc
f     UNC = INTEGER (Given)
*        An optional pointer to an existing Region which specifies the
*        uncertainties associated with positions in the supplied Frame.
*        The uncertainty in any point in the Frame is found by shifting the
*        supplied "uncertainty" Region so that it is centred at the point
*        being considered. The area covered by the shifted uncertainty
*        Region then represents the uncertainty in the position. The
*        uncertainty is assumed to be the same for all points.
*
*        If supplied, the uncertainty Region must be of a class for which
*        all instances are centro-symetric (e.g. Box, Circle, Ellipse, etc.)
*        or be a Prism containing centro-symetric component Regions. A deep
*        copy of the supplied Region will be taken, so subsequent changes to
*        the uncertainty Region using the supplied pointer will have no
*        effect on the created NullRegion. Alternatively,
f        a null Object pointer (AST__NULL)
c        a NULL Object pointer
*        may be supplied, in which case a default uncertainty of zero is
*        used.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new NullRegion. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new NullRegion. The syntax used is identical to that for the
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
c     astNullRegion()
f     AST_NULLREGION = INTEGER
*        A pointer to the new NullRegion.

*  Notes:
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstFrame *frame;              /* Pointer to Frame structure */
   AstNullRegion *new;           /* Pointer to new NullRegion */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Obtain and validate a pointer to the supplied Frame structure. */
   frame = astCheckFrame( frame_void );

/* Initialise the NullRegion, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitNullRegion( NULL, sizeof( AstNullRegion ), !class_init,
                            &class_vtab, "NullRegion", frame, unc );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new NullRegion's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new NullRegion. */
   return new;
}

AstNullRegion *astNullRegionId_( void *frame_void, void *unc_void,
                                 const char *options, ... ) {
/*
*  Name:
*     astNullRegionId_

*  Purpose:
*     Create a NullRegion.

*  Type:
*     Private function.

*  Synopsis:
*     #include "nullregion.h"
*     AstNullRegion *astNullRegionId_( AstFrame *frame, AstRegion *unc, const char *options, ... )

*  Class Membership:
*     NullRegion constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astNullRegion constructor function. It returns an ID value (instead
*     of a true C pointer) to external users, and must be provided
*     because astNullRegion_ has a variable argument list which cannot be
*     encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     The variable argument list also prevents this function from
*     invoking astNullRegion_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astNullRegion_.

*  Returned Value:
*     The ID value associated with the new NullRegion.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstFrame *frame;              /* Pointer to Frame structure */
   AstNullRegion *new;           /* Pointer to new NullRegion */
   AstRegion *unc;               /* Pointer to Region structure */
   va_list args;                 /* Variable argument list */

   int *status;                  /* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Obtain a Frame pointer from the supplied ID and validate the
   pointer to ensure it identifies a valid Frame. */
   frame = astVerifyFrame( astMakePointer( frame_void ) );

/* Obtain a Region pointer from the supplied "unc" ID and validate the
   pointer to ensure it identifies a valid Region . */
   unc = unc_void ? astCheckRegion( astMakePointer( unc_void ) ) : NULL;

/* Initialise the NullRegion, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitNullRegion( NULL, sizeof( AstNullRegion ), !class_init,
                            &class_vtab, "NullRegion", frame, unc );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new NullRegion's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new NullRegion. */
   return astMakeId( new );
}

AstNullRegion *astInitNullRegion_( void *mem, size_t size, int init,
                                   AstNullRegionVtab *vtab, const char *name,
                                   AstFrame *frame, AstRegion *unc, int *status ) {
/*
*+
*  Name:
*     astInitNullRegion

*  Purpose:
*     Initialise a NullRegion.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "nullregion.h"
*     AstNullRegion *astInitNullRegion_( void *mem, size_t size, int init,
*                                        AstNullRegionVtab *vtab, const char *name,
*                                        AstFrame *frame, AstRegion *unc  )

*  Class Membership:
*     NullRegion initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new NullRegion object. It allocates memory (if necessary) to accommodate
*     the NullRegion plus any additional data associated with the derived class.
*     It then initialises a NullRegion structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a NullRegion at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the NullRegion is to be initialised.
*        This must be of sufficient size to accommodate the NullRegion data
*        (sizeof(NullRegion)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the NullRegion (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the NullRegion
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the NullRegion's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new NullRegion.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the astGetClass
*        method).
*     frame
*        A pointer to the Frame in which the region is defined.
*     unc
*        A pointer to a Region which specifies the uncertainty of positions
*        within the supplied Frame. A NULL pointer can be supplied, in which
*        case default uncertainties of zero are used. If an uncertainty
*        Region is supplied, it must be either a Box, a Circle or an Ellipse,
*        and its encapsulated Frame must be related to the Frame supplied
*        for parameter "frame" (i.e. astConvert should be able to find a
*        Mapping between them). The centre of the supplied uncertainty
*        Region is immaterial since it will be re-centred on the point
*        being tested before use. A deep copy is taken of the supplied
*        Region.

*  Returned Value:
*     A pointer to the new NullRegion.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstNullRegion *new;       /* Pointer to new NullRegion */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitNullRegionVtab( vtab, name );

/* Initialise a Region structure (the parent class) as the first component
   within the NullRegion structure, allocating memory if necessary. */
   new = (AstNullRegion *) astInitRegion( mem, size, 0, (AstRegionVtab *) vtab,
                                          name, frame, NULL, unc );

/* If an error occurred, clean up by deleting the new NullRegion. */
   if ( !astOK ) new = astDelete( new );

/* Return a pointer to the new NullRegion. */
   return new;
}

AstNullRegion *astLoadNullRegion_( void *mem, size_t size, AstNullRegionVtab *vtab,
                                   const char *name, AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadNullRegion

*  Purpose:
*     Load a NullRegion.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "nullregion.h"
*     AstNullRegion *astLoadNullRegion( void *mem, size_t size, AstNullRegionVtab *vtab,
*                                       const char *name, AstChannel *channel )

*  Class Membership:
*     NullRegion loader.

*  Description:
*     This function is provided to load a new NullRegion using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     NullRegion structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a NullRegion at the start of the memory
*     passed via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory into which the NullRegion is to be
*        loaded.  This must be of sufficient size to accommodate the
*        NullRegion data (sizeof(NullRegion)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the NullRegion (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the NullRegion structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstNullRegion) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new NullRegion. If this is NULL, a pointer
*        to the (static) virtual function table for the NullRegion class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "NullRegion" is used instead.

*  Returned Value:
*     A pointer to the new NullRegion.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstNullRegion *new;              /* Pointer to the new NullRegion */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this NullRegion. In this case the
   NullRegion belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstNullRegion );
      vtab = &class_vtab;
      name = "NullRegion";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitNullRegionVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built NullRegion. */
   new = astLoadRegion( mem, size, (AstRegionVtab *) vtab, name,
                        channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "NullRegion" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* There are no values to read. */
/* ---------------------------- */

/* If an error occurred, clean up by deleting the new NullRegion. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new NullRegion pointer. */
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










