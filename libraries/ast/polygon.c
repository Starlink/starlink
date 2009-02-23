/*
*class++
*  Name:
*     Polygon

*  Purpose:
*     A polygonal region within a 2-dimensional Frame.

*  Constructor Function:
c     astPolygon
f     AST_POLYGON

*  Description:
*     The Polygon class implements a polygonal area, defined by a
*     collection of vertices, within a 2-dimensional Frame. The vertices
*     are connected together by geodesic curves within the encapsulated Frame.
*     For instance, if the encapsulated Frame is a simple Frame then the
*     geodesics will be straight lines, but if the Frame is a SkyFrame then 
*     the geodesics will be great circles. Note, the vertices must be
*     supplied in an order such that the inside of the polygon is to the
*     left of the boundary as the vertices are traversed. Supplying them
*     in the reverse order will effectively negate the polygon.
*
*     Within a SkyFrame, neighbouring vertices are always joined using the 
*     shortest path. Thus if an edge of 180 degrees or more in length is
*     required, it should be split into section each of which is less
*     than 180 degrees. The closed path joining all the vertices in order
*     will divide the celestial sphere into two disjoint regions. The
*     inside of the polygon is the region which is circled in an
*     anti-clockwise manner (when viewed from the outside of the celestial
*     sphere) when moving through the list of vertices in the order in
*     which they were supplied when the Polygon was created (i.e. the
*     inside is to the left of the boundary when moving through the
*     vertices in the order supplied).

*  Inheritance:
*     The Polygon class inherits from the Region class.

*  Attributes:
*     The Polygon class does not define any new attributes beyond
*     those which are applicable to all Regions.

*  Functions:
c     The Polygon class does not define any new functions beyond those
f     The Polygon class does not define any new routines beyond those
*     which are applicable to all Regions.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public Licence as
*     published by the Free Software Foundation; either version 2 of
*     the Licence, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public Licence for more details.
*     
*     You should have received a copy of the GNU General Public Licence
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S. Berry (Starlink)

*  History:
*     26-OCT-2004 (DSB):
*        Original version.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS Polygon

/* Macros which return the maximum and minimum of two values. */
#define MAX(aa,bb) ((aa)>(bb)?(aa):(bb))
#define MIN(aa,bb) ((aa)<(bb)?(aa):(bb))

/* Macro to check for equality of floating point values. We cannot
   compare bad values directory because of the danger of floating point
   exceptions, so bad values are dealt with explicitly. */
#define EQUAL(aa,bb) (((aa)==AST__BAD)?(((bb)==AST__BAD)?1:0):(((bb)==AST__BAD)?0:(fabs((aa)-(bb))<=1.0E9*MAX((fabs(aa)+fabs(bb))*DBL_EPSILON,DBL_MIN))))

/* Macros specifying whether a point is inside, outside or on the
   boundary of the polygon. */
#define UNKNOWN 0
#define IN      1
#define OUT     2
#define ON      3

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
#include "box.h"                 /* Box Regions */
#include "wcsmap.h"              /* Definitons of AST__DPI etc */
#include "polygon.h"             /* Interface definition for this class */
#include "mapping.h"             /* Position mappings */
#include "unitmap.h"             /* Unit Mapping */
#include "pal.h"              /* SLALIB library interface */
#include "frame.h"               /* Coordinate system description */

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
static void (* parent_setregfs)( AstRegion *, AstFrame *, int * );
static void (* parent_resetcache)( AstRegion *, int * );


#ifdef THREAD_SAFE
/* Define how to initialise thread-specific globals. */ 
#define GLOBAL_inits \
   globals->Class_Init = 0; 

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(Polygon)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(Polygon,Class_Init)
#define class_vtab astGLOBAL(Polygon,Class_Vtab)


#include <pthread.h>


#else


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstPolygonVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstPolygon *astPolygonId_( void *, int, int, const double *, void *, const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstMapping *Simplify( AstMapping *, int * );
static AstPointSet *RegBaseMesh( AstRegion *, int * );
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static double Polywidth( AstFrame *, AstLineDef **, int, int, double[ 2 ], int * );
static int RegPins( AstRegion *, AstPointSet *, AstRegion *, int **, int * );
static void Cache( AstPolygon *, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void RegBaseBox( AstRegion *this, double *, double *, int * );
static void ResetCache( AstRegion *this, int * );
static void SetRegFS( AstRegion *, AstFrame *, int * );

/* Member functions. */
/* ================= */
static void Cache( AstPolygon *this, int *status ){
/*
*  Name:
*     Cache

*  Purpose:
*     Calculate intermediate values and cache them in the Polygon structure.

*  Type:
*     Private function.

*  Synopsis:
*     #include "polygon.h"
*     void Cache( AstPolygon *this, int *status )

*  Class Membership:
*     Polygon member function 

*  Description:
*     This function uses the PointSet stored in the parent Region to calculate 
*     some intermediate values which are useful in other methods. These
*     values are stored within the Polygon structure.

*  Parameters:
*     this
*        Pointer to the Polygon.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstFrame *frm;       /* Pointer to base Frame in Polygon */
   double **ptr;        /* Pointer to data in the encapsulated PointSet */
   double end[ 2 ];     /* Start position for edge */
   double maxwid;       /* Maximum polygon width found so far */
   double polcen[ 2 ];  /* Polygon centre perpendicular to current edge */
   double polwid;       /* Polygon width perpendicular to current edge */
   double start[ 2 ];   /* Start position for edge */
   int i;               /* Axis index */
   int nv;              /* Number of vertices in Polygon */

/* Check the global error status. */
   if ( !astOK ) return;

/* Do Nothing if the cached information is up to date. */
   if( this->stale ) {

/* Get a pointer to the base Frame. */
      frm = astGetFrame( ((AstRegion *) this)->frameset, AST__BASE );
   
/* Get the number of vertices. */
      nv = astGetNpoint( ((AstRegion *) this)->points );
   
/* Get pointers to the coordinate data in the parent Region structure. */
      ptr = astGetPoints( ((AstRegion *) this)->points );
   
/* Free any existing edge information in the Polygon structure. */
      if( this->edges ) {
         for( i = 0; i < nv; i++ ) {
            this->edges[ i ] = astFree( this->edges[ i ] );
         }
   
/* Allocate memory to store new edge information if necessary. */
      } else {
         this->edges = astMalloc( sizeof( AstLineDef *)*(size_t) nv );
      }
   
/* Check pointers can be used safely. */
      if( this->edges ) {
   
/* Create and store a description of each edge. */
         start[ 0 ] = ptr[ 0 ][ nv - 1];
         start[ 1 ] = ptr[ 1 ][ nv - 1];
         for( i = 0; i < nv; i++ ) {
            end[ 0 ] = ptr[ 0 ][ i ];
            end[ 1 ] = ptr[ 1 ][ i ];
            this->edges[ i ] = astLineDef( frm, start, end );
            start[ 0 ] = end[ 0 ];
            start[ 1 ] = end[ 1 ];
         }     

/* We now look for a point that is inside the polygon. We want a point
   that is well within the polygon, since points that are only just inside
   the polygon can give numerical problems. Loop round each edge with 
   non-zero length. */
         maxwid = -1.0;
         for( i = 0; i < nv; i++ ) {
            if( this->edges[ i ]->length > 0.0 ) {

/* We define another line perpendicular to the current edge, passing
   through the mid point of the edge, extending towards the inside of the
   polygon. The following function returns the distance we can travel
   along this line before we hit any of the other polygon edges. It also
   puts the position corresponding to half that distance into "polcen". */
               polwid = Polywidth( frm, this->edges, i, nv, polcen, status );

/* If the width of the polygon perpendicular to the current edge is
   greater than the width perpdeicular to any other edge, record the
   width and also store the current polygon centre. */
               if( polwid > maxwid ) {
                  maxwid = polwid;
                  (this->in)[ 0 ] = polcen[ 0 ];                  
                  (this->in)[ 1 ] = polcen[ 1 ];                  
               }
            }
         }

/* Report an error if no point was found. */
         if( maxwid < 0 && astOK ) {
            if( astOK ) astError( AST__BADIN, "astInitPolygon(%s): Degenerate "
                                  "(zero-area) polygon supplied.", status,
                                  astGetClass( this ) );
         }
      }
   
/* Free resources */
      frm = astAnnul( frm );

/* Indicate cached information is up to date. */
      this->stale = 0;

   }
}

void astInitPolygonVtab_(  AstPolygonVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitPolygonVtab

*  Purpose:
*     Initialise a virtual function table for a Polygon.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "polygon.h"
*     void astInitPolygonVtab( AstPolygonVtab *vtab, const char *name )

*  Class Membership:
*     Polygon vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the Polygon class.

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
   astDECLARE_GLOBALS;           /* Pointer to thread-specific global data */
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
   will be used (by astIsAPolygon) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->check = &class_check;

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

   parent_setregfs = region->SetRegFS;
   region->SetRegFS = SetRegFS;

   parent_resetcache = region->ResetCache;
   region->ResetCache = ResetCache;

   region->RegPins = RegPins;
   region->RegBaseMesh = RegBaseMesh;
   region->RegBaseBox = RegBaseBox;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */

/* Declare the copy constructor, destructor and class dump
   functions. */
   astSetDump( vtab, Dump, "Polygon", "Polygonal region" );
   astSetDelete( vtab, Delete );
   astSetCopy( vtab, Copy );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised. */
   if( vtab == &class_vtab ) class_init = 1;

}

static double Polywidth( AstFrame *frm, AstLineDef **edges, int i, int nv, 
                         double cen[ 2 ], int *status ){
/*
*  Name:
*     Polywidth

*  Purpose:
*     Find the width of a polygon perpendicular to a given edge.

*  Type:
*     Private function.

*  Synopsis:
*     #include "polygon.h"
*     double Polywidth( AstFrame *frm, AstLineDef **edges, int i, int nv, 
*                       double cen[ 2 ], int *status )

*  Class Membership:
*     Polygon member function 

*  Description:
*     This function defines a line perpendicular to a given polygon edge, 
*     passing through the mid point of the edge, extending towards the 
*     inside of the polygon. It returns the distance that can be travelled
*     along this line before any of the other polygon edges are hit (the
*     "width" of the polygon perpendicular to the given edge). It also 
*     puts the position corresponding to half that distance into "cen".

*  Parameters:
*     frm
*        The Frame in which the lines are defined.
*     edges
*        Array of "nv" pointers to AstLineDef structures, each defining an 
*        edge of the polygon.
*     i
*        The index of the edge that is to define the polygon width.
*     nv
*        Total number of edges.
*     cen
*        An array into which are put the coords of the point half way
*        along the polygon width line.
*     status
*        Pointer to the inherited status variable.

*  Returnd Value:
*     The width of the polygon perpendicular to the given edge.

*/

/* Local Variables: */
   AstLineDef *line;
   double *cross;
   double d;        
   double end[ 2 ];
   double l1;
   double l2;
   double result;
   double start[ 2 ];
   int j;

/* Check the global error status. */
   result = AST__BAD;
   if ( !astOK ) return result;

/* Create a Line description for a line perpendicular to the specified
   edge, passing through the mid point of the edge, and extending towards
   the inside of the polygon. First move away from the start along the 
   line to the mid point. This gives the start of the new line. */
   l1 = 0.5*( edges[ i ]->length );
   astLineOffset( frm, edges[ i ], l1, 0.0, start );

/* We now move away from this position at right angles to the line. We
   start off by moving 5 times the length of the specified edge. For
   some Frames (e.g. SkyFrames) this may result in a position that is 
   much too closer (i.e. if it goes all the wat round the great circle
   and comes back to the beginning). Therefore, we check that the end
   point is the requested distance from the start point, and if not, we
   halve the length of the line and try again. */
   l2 = 10.0*l1;
   while( 1 ) {
      astLineOffset( frm, edges[ i ], l1, l2, end );
      d = astDistance( frm, start, end );
      if( d != AST__BAD && fabs( d - l2 ) < 1.0E-6*l2 ) break;
      l2 *= 0.5;
   }

/* Create a description of the required line. */
   line = astLineDef( frm, start, end );

/* Loop round every edge, except for the supplied edge. */
   for( j = 0; j < nv; j++ ) {
      if( j != i ) {

/* Find the position at which the line created above crosses the current
   edge. Skip to the next edge if the line does not intersect the edge. */
         if( astLineCrossing( frm, line, edges[ j ], &cross ) ) {

/* Find the distance between the crossing point and the line start. */
            d = astDistance( frm, start, cross );

/* If this is less than the smallest found so far, record it. */
            if( d != AST__BAD && ( d < result || result == AST__BAD ) ) {
               result = d;
            }
         }

/* Free resources */
         cross = astFree( cross );
      }
   }
   line = astFree( line );

/* If a width was found, return the point half way across the polygon. */
   if( result != AST__BAD ) {
      astOffset( frm, start, end, 0.5*result, cen );
   }

/* Return the width. */
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
*     #include "polygon.h"
*     void RegBaseBox( AstRegion *this, double *lbnd, double *ubnd, int *status )

*  Class Membership:
*     Polygon member function (over-rides the astRegBaseBox protected
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
   AstFrame *frm;             /* Pointer to encapsulated Frame */
   AstPointSet *pset;         /* Pointer to PointSet defining the Region */
   AstPolygon *this;          /* Pointer to Polygon structure */
   AstRegion *reg;            /* Base Frame equivalent of supplied Polygon */
   double **ptr;              /* Pointer to PointSet data */
   double *x;                 /* Pointer to next X axis value */
   double *y;                 /* Pointer to next Y axis value */
   int ip;                    /* Point index */
   int np;                    /* No. of points in PointSet */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the Polygon structure. */
   this = (AstPolygon *) this_region;

/* If the base Frame bounding box has already been found, return the
   values stored in the Polygon structure. */
   if( this->lbnd[ 0 ] != AST__BAD ) {
      lbnd[ 0 ] = this->lbnd[ 0 ];
      lbnd[ 1 ] = this->lbnd[ 1 ];
      ubnd[ 0 ] = this->ubnd[ 0 ];
      ubnd[ 1 ] = this->ubnd[ 1 ];

/* If the base Frame bounding box has not yet been found, find it now and
   store it in the Polygon structure. */
   } else {

/* Get the axis values for the PointSet which defines the location and
   extent of the region in the base Frame of the encapsulated FrameSet. */
      pset = this_region->points;
      ptr = astGetPoints( pset );
      np = astGetNpoint( pset );

/* Find the upper and lower bounds of the box enclosing all the vertices. */
      lbnd[ 0 ] = DBL_MAX;
      lbnd[ 1 ] = DBL_MAX;
      ubnd[ 0 ] = -DBL_MAX;
      ubnd[ 1 ] = -DBL_MAX;
      x = ptr[ 0 ];
      y = ptr[ 1 ];
      for( ip = 0; ip < np; ip++, x++, y++ ) {
         if( *x < lbnd[ 0 ] ) lbnd[ 0 ] = *x;
         if( *x > ubnd[ 0 ] ) ubnd[ 0 ] = *x;
         if( *y < lbnd[ 1 ] ) lbnd[ 1 ] = *y;
         if( *y > ubnd[ 1 ] ) ubnd[ 1 ] = *y;
      }

/* Get a pointer to the base Frame in the frameset encapsulated by the
   parent Region structure. */
      frm = astGetFrame( this_region->frameset, AST__BASE );

/* The astNormBox requires a Mapping which can be used to test points in 
   this base Frame. Create a copy of the Polygon and then set its
   FrameSet so that the current Frame in the copy is the same as the base
   Frame in the original. */
      reg = astCopy( this );
      astSetRegFS( reg, frm );
      astSetNegated( reg, 0 );

/* Normalise this box. */
      astNormBox( frm, lbnd, ubnd, reg );

/* Free resources */
      reg = astAnnul( reg );
      frm = astAnnul( frm );

/* Store it in the olygon structure for future use. */
      this->lbnd[ 0 ] = lbnd[ 0 ];
      this->lbnd[ 1 ] = lbnd[ 1 ];
      this->ubnd[ 0 ] = ubnd[ 0 ];
      this->ubnd[ 1 ] = ubnd[ 1 ];

   }
}

static AstPointSet *RegBaseMesh( AstRegion *this_region, int *status ){
/*
*  Name:
*     RegBaseMesh

*  Purpose:
*     Return a PointSet containing a mesh of points on the boundary of a 
*     Region in its base Frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "polygon.h"
*     AstPointSet *astRegBaseMesh( AstRegion *this, int *status )

*  Class Membership:
*     Polygon member function (over-rides the astRegBaseMesh protected
*     method inherited from the Region class).

*  Description:
*     This function returns a PointSet containing a mesh of points on the
*     boundary of the Region. The points refer to the base Frame of
*     the encapsulated FrameSet.

*  Parameters:
*     this
*        Pointer to the Region.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the PointSet. Annul the pointer using astAnnul when it 
*     is no longer needed.

*  Notes:
*    - A NULL pointer is returned if an error has already occurred, or if
*    this function should fail for any reason.

*/

/* Local Variables: */
   AstFrame *frm;                 /* Base Frame in encapsulated FrameSet */
   AstPointSet *result;           /* Returned pointer */
   AstPolygon *this;              /* The Polygon structure */
   double **rptr;                 /* Pointers to returned mesh data */
   double **vptr;                 /* Pointers to vertex data */
   double *lens;                  /* Pointer to work space holding edge lengths */
   double d;                      /* Length of this edge */
   double delta;                  /* Angular separation of points */
   double end[ 2 ];               /* End position */
   double mp;                     /* No. of mesh points per unit distance */
   double p[ 2 ];                 /* Position in 2D Frame */
   double start[ 2 ];             /* Start position */
   double total;                  /* Total length of polygon */
   int ip;                        /* Point index */
   int iv;                        /* Vertex index */
   int n;                         /* No. of points on this edge */
   int next;                      /* Index of next point in returned PointSet */
   int np;                        /* No. of points in returned PointSet */
   int nv;                        /* No. of polygon vertices */

/* Initialise */
   result= NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* If the Region structure contains a pointer to a PointSet holding 
   a previously created mesh, return it. */
   if( this_region->basemesh ) {
      result = astClone( this_region->basemesh );

/* Otherwise, create a new mesh. */
   } else {

/* Get a pointer to the Polygon structure. */
      this = (AstPolygon *) this_region;

/* Get a pointer to the base Frame in the encapsulated FrameSet. */
      frm = astGetFrame( this_region->frameset, AST__BASE );

/* Get the number of vertices and pointers to the vertex axis values. */
      nv = astGetNpoint( this_region->points );
      vptr = astGetPoints( this_region->points );

/* Allocate memory to hold the geodesic length of each edge. */
      lens = astMalloc( sizeof( double )*(size_t) nv );

      if( astOK ) {

/* Find the total geodesic distance around the boundary. */
         total = 0.0;

         start[ 0 ] = vptr[ 0 ][ 0 ];                                 
         start[ 1 ] = vptr[ 1 ][ 0 ];                                 

         for( iv = 1; iv < nv; iv++ ) {
            end[ 0 ] = vptr[ 0 ][ iv ];                                 
            end[ 1 ] = vptr[ 1 ][ iv ];                                 

            d = astDistance( frm, start, end );
            if( d != AST__BAD ) total += fabs( d );              
            lens[ iv ] = d;
            start[ 0 ] = end[ 0 ];
            start[ 1 ] = end[ 1 ];
         }

         end[ 0 ] = vptr[ 0 ][ 0 ];                                 
         end[ 1 ] = vptr[ 1 ][ 0 ];                                 

         d = astDistance( frm, start, end );
         if( d != AST__BAD ) total += fabs( d );              
         lens[ 0 ] = d;

/* Find the number of mesh points per unit geodesic distance. */
         if( total > 0.0 ){ 
            mp = astGetMeshSize( this )/total;         

/* Find the total number of mesh points required. */
            np = 0;
            for( iv = 0; iv < nv; iv++ ) {
               if( lens[ iv ] != AST__BAD ) np += 1 + (int)( lens[ iv ]*mp );
            }

/* Create a suitable PointSet to hold the returned positions. */
            result = astPointSet( np, 2, "", status );
            rptr = astGetPoints( result );
            if( astOK ) {

/* Initialise the index of the next point to be added to the returned
   PointSet. */
               next = 0; 

/* Loop round each good edge of the polygon. The edge ends at vertex "iv". */
               start[ 0 ] = vptr[ 0 ][ 0 ];                                 
               start[ 1 ] = vptr[ 1 ][ 0 ];                                 
      
               for( iv = 1; iv < nv; iv++ ) {
                  end[ 0 ] = vptr[ 0 ][ iv ];                                 
                  end[ 1 ] = vptr[ 1 ][ iv ];                                 
                  if( lens[ iv ] != AST__BAD ) {

/* Add the position of the starting vertex to the returned mesh. */
                     rptr[ 0 ][ next ] = start[ 0 ];
                     rptr[ 1 ][ next ] = start[ 1 ];
                     next++;

/* Find the number of points on this edge, and the geodesic distance
   between them. */
                     n = 1 + (int) ( lens[ iv ]*mp ); 

/* If more than one point, find the distance between points. */
                     if( n > 1 ) {
                        delta = lens[ iv ]/n;

/* Loop round the extra points. */
                        for( ip = 1; ip < n; ip++ ) {

/* Find the position of the next mesh point. */
                           astOffset( frm, start, end, delta*ip, p );

/* Add it to the mesh. */
                           rptr[ 0 ][ next ] = p[ 0 ];
                           rptr[ 1 ][ next ] = p[ 1 ];
                           next++;

                        }
                     }
                  }

/* The end of this edge becomes the start of the next. */
                  start[ 0 ] = end[ 0 ];
                  start[ 1 ] = end[ 1 ];
               }
      
/* Now do the edge which ends at the first vertex. */
               end[ 0 ] = vptr[ 0 ][ 0 ];                                 
               end[ 1 ] = vptr[ 1 ][ 0 ];                                 
               if( lens[ 0 ] != AST__BAD ) {
                  rptr[ 0 ][ next ] = start[ 0 ];
                  rptr[ 1 ][ next ] = start[ 1 ];
                  next++;

                  n = 1 + (int)( lens[ 0 ]*mp ); 
                  if( n > 1 ) {
                     delta = lens[ 0 ]/n;
                     for( ip = 1; ip < n; ip++ ) {
                        astOffset( frm, start, end, delta*ip, p );
                        rptr[ 0 ][ next ] = p[ 0 ];
                        rptr[ 1 ][ next ] = p[ 1 ];
                        next++;
                     }
                  }
               }

/* Check the PointSet size was correct. */
               if( next != np && astOK ) {
                  astError( AST__INTER, "astRegBaseMesh(%s): Error in the "
                            "allocated PointSet size (%d) - should have "
                            "been %d (internal AST programming error).", status, 
                            astGetClass( this ), np, next );
               }

/* Save the returned pointer in the Region structure so that it does not
   need to be created again next time this function is called. */
               if( astOK ) this_region->basemesh = astClone( result );

            }

         } else if( astOK ) {
            astError( AST__BADIN, "astRegBaseMesh(%s): The boundary of "
                      "the supplied %s has an undefined length.", status,
                      astGetClass( this ), astGetClass( this ) );
         }

      }

/* Free resources. */
      frm = astAnnul( frm );
      lens = astFree( lens );
   }

/* Annul the result if an error has occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return a pointer to the output PointSet. */
   return result;
}

static int RegPins( AstRegion *this_region, AstPointSet *pset, AstRegion *unc,
                    int **mask, int *status ){
/*
*  Name:
*     RegPins

*  Purpose:
*     Check if a set of points fall on the boundary of a given Polygon.

*  Type:
*     Private function.

*  Synopsis:
*     #include "polygon.h"
*     int RegPins( AstRegion *this, AstPointSet *pset, AstRegion *unc,
*                  int **mask, int *status )

*  Class Membership:
*     Polygon member function (over-rides the astRegPins protected
*     method inherited from the Region class).

*  Description:
*     This function returns a flag indicating if the supplied set of
*     points all fall on the boundary of the given Polygon.
*
*     Some tolerance is allowed, as specified by the uncertainty Region
*     stored in the supplied Polygon "this", and the supplied uncertainty
*     Region "unc" which describes the uncertainty of the supplied points.

*  Parameters:
*     this
*        Pointer to the Polygon.
*     pset
*        Pointer to the PointSet. The points are assumed to refer to the 
*        base Frame of the FrameSet encapsulated by "this".
*     unc
*        Pointer to a Region representing the uncertainties in the points
*        given by "pset". The Region is assumed to represent the base Frame 
*        of the FrameSet encapsulated by "this". Zero uncertainity is assumed 
*        if NULL is supplied.
*     mask
*        Pointer to location at which to return a pointer to a newly
*        allocated dynamic array of ints. The number of elements in this
*        array is equal to the value of the Npoint attribute of "pset".
*        Each element in the returned array is set to 1 if the
*        corresponding position in "pset" is on the boundary of the Region
*        and is set to zero otherwise. A NULL value may be supplied
*        in which case no array is created. If created, the array should
*        be freed using astFree when no longer needed.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Non-zero if the points all fall on the boundary of the given
*     Region, to within the tolerance specified. Zero otherwise.

*/

/* Local variables: */
   AstFrame *frm;               /* Base Frame in supplied Polygon */
   AstPointSet *pset1;          /* Pointer to copy of supplied PointSet */
   AstPointSet *pset2;          /* Pointer to PointSet holding resolved components */
   AstPolygon *this;            /* Pointer to the Polygon structure. */
   AstRegion *tunc;             /* Uncertainity Region from "this" */
   double **ptr1;               /* Pointer to axis values in "pset1" */
   double **ptr2;               /* Pointer to axis values in "pset2" */
   double **vptr;               /* Pointer to axis values at vertices */
   double edge_len;             /* Length of current edge */
   double end[2];               /* Position of end of current edge */ 
   double l1;                   /* Length of bounding box diagonal */
   double l2;                   /* Length of bounding box diagonal */
   double lbnd_tunc[2];         /* Lower bounds of "this" uncertainty Region */ 
   double lbnd_unc[2];          /* Lower bounds of supplied uncertainty Region */ 
   double par;                  /* Parallel component */
   double parmax;               /* Max acceptable parallel component */
   double prp;                  /* Perpendicular component */
   double start[2];             /* Position of start of current edge */ 
   double ubnd_tunc[2];         /* Upper bounds of "this" uncertainty Region */ 
   double ubnd_unc[2];          /* Upper bounds of supplied uncertainty Region */ 
   double wid;                  /* Width of acceptable margin around polygon */
   int *m;                      /* Pointer to next mask value */
   int i;                       /* Edge index */
   int ip;                      /* Point index */
   int np;                      /* No. of supplied points */
   int nv;                      /* No. of vertices */
   int result;                  /* Returned flag */

/* Initialise */
   result = 0;
   if( mask ) *mask = NULL;

/* Check the inherited status. */
   if( !astOK ) return result;

/* Get a pointer to the Polygon structure. */
   this = (AstPolygon *) this_region;

/* Check the supplied PointSet has 2 axis values per point. */
   if( astGetNcoord( pset ) != 2 && astOK ) {
      astError( AST__INTER, "astRegPins(%s): Illegal number of axis "
                "values per point (%d) in the supplied PointSet - should be "
                "2 (internal AST programming error).", status, astGetClass( this ),
                astGetNcoord( pset ) );
   }

/* Get the number of axes in the uncertainty Region and check it is also 2. */
   if( unc && astGetNaxes( unc ) != 2 && astOK ) {
      astError( AST__INTER, "astRegPins(%s): Illegal number of axes (%d) "
                "in the supplied uncertainty Region - should be 2 "
                "(internal AST programming error).", status, astGetClass( this ),
                astGetNaxes( unc ) );
   }

/* Get pointers to the axis values at the polygon vertices. */
   vptr = astGetPoints( this_region->points );

/* Get the number of vertices. */
   nv = astGetNpoint( this_region->points );

/* Take a copy of the supplied PointSet and get pointers to its axis
   values,and its size */
   pset1 = astCopy( pset );
   ptr1 = astGetPoints( pset1 );
   np = astGetNpoint( pset1 );

/* Create a PointSet to hold the resolved components and get pointers to its 
   axis data. */
   pset2 = astPointSet( np, 2, "", status );
   ptr2 = astGetPoints( pset2 );

/* Create a mask array if required. */
   if( mask ) *mask = astMalloc( sizeof(int)*(size_t) np );

/* We now find the maximum distance on each axis that a point can be from the 
   boundary of the Polygon for it still to be considered to be on the boundary.
   First get the Region which defines the uncertainty within the Polygon 
   being checked (in its base Frame), and get its bounding box. The current
   Frame of the uncertainty Region is the same as the base Frame of the
   Polygon. */
   tunc = astGetUncFrm( this, AST__BASE );      
   astGetUncBounds( tunc, lbnd_tunc, ubnd_tunc ); 

/* Find the geodesic length within the base Frame of "this" of the diagonal of 
   the bounding box. */
   frm = astGetFrame( this_region->frameset, AST__BASE );
   l1 = astDistance( frm, lbnd_tunc, ubnd_tunc );

/* Also get the Region which defines the uncertainty of the supplied points
   and get its bounding box in the same Frame. */
   if( unc ) {
      astGetUncBounds( unc, lbnd_unc, ubnd_unc ); 

/* Find the geodesic length of the diagonal of this bounding box. */
      l2 = astDistance( frm, lbnd_unc, ubnd_unc );

/* Assume zero uncertainty if no "unc" Region was supplied. */
   } else {
      l2 = 0.0;
   }

/* The required border width is half of the total diagonal of the two bounding 
   boxes. */   
   if( astOK ) {
      wid = 0.5*( l1 + l2 );

/* Loop round each edge of the polygon. Edge "i" starts at vertex "i-1"
   and ends at vertex "i". Edge zero starts at vertex "nv-1" and ends at
   vertex zero. */
      start[ 0 ] = vptr[ 0 ][ nv - 1 ];
      start[ 1 ] = vptr[ 1 ][ nv - 1 ];
      for( i = 0; i < nv; i++ ) {
         end[ 0 ] = vptr[ 0 ][ i ];
         end[ 1 ] = vptr[ 1 ][ i ];

/* Find the length of this edge. */
         edge_len = astDistance( frm, start, end );

/* Resolve all the supplied mesh points into components parallel and 
   perpendicular to this edge. */
         (void) astResolvePoints( frm, start, end, pset1, pset2 );

/* A point is effectively on this edge if the parallel component is
   greater than (-wid) and less than (edge_len+wid) AND the perpendicular
   component has an absolute value less than wid. Identify such positions
   and set them bad in pset1. */
         parmax = edge_len + wid;
         for( ip = 0; ip < np; ip++ ) {
            par = ptr2[ 0 ][ ip ];
            prp = ptr2[ 1 ][ ip ];

            if( par != AST__BAD && prp != AST__BAD ) {
               if( par > -wid && par < parmax && prp > -wid && prp < wid ) {
                  ptr1[ 0 ][ ip ] = AST__BAD;
                  ptr1[ 1 ][ ip ] = AST__BAD;
               }
            } 
         }

/* The end of the current edge becomes the start of the next. */
         start[ 0 ] = end[ 0 ];
         start[ 1 ] = end[ 1 ];
      }

/* See if any good points are left in pset1. If so, it means that those
   points were not on any edge of hte Polygon. We use two alogorithms
   here depending on whether we are creating a mask array, since we can 
   abort the check upon finding the first good point if we are not
   producing a mask. */
      result = 1;
      if( mask ) {
         m = *mask;
         for( ip = 0; ip < np; ip++, m++ ) {
            if( ptr1[ 0 ][ ip ] != AST__BAD && 
                ptr1[ 1 ][ ip ] != AST__BAD ) {
               *m = 0;
               result = 0;
            } else {
               *m = 1;
            } 
         }
      } else {
         for( ip = 0; ip < np; ip++, m++ ) {
            if( ptr1[ 0 ][ ip ] != AST__BAD && 
                ptr1[ 1 ][ ip ] != AST__BAD ) {
               result = 0;
               break;
            } 
         }
      }
   }

/* Free resources. */
   tunc = astAnnul( tunc );
   frm = astAnnul( frm );
   pset1 = astAnnul( pset1 );
   pset2 = astAnnul( pset2 );

/* If an error has occurred, return zero. */
   if( !astOK ) {
      result = 0;
      if( mask ) *mask = astAnnul( *mask );
   }

/* Return the result. */
   return result;
}

static void ResetCache( AstRegion *this, int *status ){
/*
*  Name:
*     ResetCache

*  Purpose:
*     Clear cached information within the supplied Region.

*  Type:
*     Private function.

*  Synopsis:
*     #include "polygon.h"
*     void ResetCache( AstRegion *this, int *status )

*  Class Membership:
*     Region member function (overrides the astResetCache method
*     inherited from the parent Region class).

*  Description:
*     This function clears cached information from the supplied Region 
*     structure.

*  Parameters:
*     this
*        Pointer to the Region.
*     status
*        Pointer to the inherited status variable.
*/
   if( this ) {
      ( (AstPolygon *) this )->stale = 1;
      ( (AstPolygon *) this )->lbnd[ 0 ] = AST__BAD;
      (*parent_resetcache)( this, status );
   }
}

static void SetRegFS( AstRegion *this_region, AstFrame *frm, int *status ) {
/*
*  Name:
*     SetRegFS

*  Purpose:
*     Stores a new FrameSet in a Region

*  Type:
*     Private function.

*  Synopsis:
*     #include "polygon.h"
*     void SetRegFS( AstRegion *this_region, AstFrame *frm, int *status )

*  Class Membership:
*     Polygon method (over-rides the astSetRegFS method inherited from
*     the Region class).

*  Description:
*     This function creates a new FrameSet and stores it in the supplied
*     Region. The new FrameSet contains two copies of the supplied
*     Frame, connected by a UnitMap.

*  Parameters:
*     this
*        Pointer to the Region.
*     frm
*        The Frame to use.
*     status
*        Pointer to the inherited status variable.

*/


/* Check the global error status. */
   if ( !astOK ) return;

/* Invoke the parent method to store the FrameSet in the parent Region
   structure. */
   (* parent_setregfs)( this_region, frm, status );

/* Indicate cached information eeds re-calculating. */
   astResetCache( this_region );
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
*     #include "polygon.h"
*     AstMapping *Simplify( AstMapping *this, int *status )

*  Class Membership:
*     Polygon method (over-rides the astSimplify method inherited
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
   AstMapping *map;           /* Base -> current Mapping */
   AstMapping *result;        /* Result pointer to return */
   AstPointSet *mesh;         /* Mesh of current Frame positions */
   AstPointSet *ps2;          /* Polygon PointSet in current Frame */
   AstPolygon *newpol;        /* New Polygon */
   AstRegion *new;            /* Pointer to simplified Region */
   AstRegion *this;           /* Pointer to supplied Region structure */
   AstRegion *unc;            /* Pointer to uncertainty Region */
   double **ptr2;             /* Pointer axis values in "ps2" */
   double *mem;               /* Pointer to array holding new vertex coords */
   double *p;                 /* Pointer to next vertex coords */
   double *q;                 /* Pointer to next vertex coords */
   int iv;                    /* Vertex index */
   int nv;                    /* Number of vertices in polygon */
   int ok;                    /* Are the new polygon vertices good? */
   int simpler;               /* Has some simplication taken place? */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the supplied Region structure. */
   this = (AstRegion *) this_mapping;

/* Invoke the parent Simplify method inherited from the Region class. This
   will simplify the encapsulated FrameSet and uncertainty Region. */
   new = (AstRegion *) (*parent_simplify)( this_mapping, status );

/* Note if any simplification took place. This is assumed to be the case
   if the pointer returned by the above call is different to the supplied
   pointer. */
   simpler = ( new != this );

/* We attempt to simplify the Polygon by re-defining it within its current 
   Frame. Transforming the Polygon from its base to its current Frame may 
   result in the region no longer being an polygon. We test this by 
   transforming a set of bounds on the Polygon boundary. This can only be
   done if the current Frame is 2-dimensional. Also, there is only any
   point in doing it if the Mapping from base to current Frame in the
   Polygon is not a UnitMap. */
   map = astGetMapping(  new->frameset, AST__BASE, AST__CURRENT );
   if( !astIsAUnitMap( map ) && astGetNout( map ) == 2 ) {

/* Get a pointer to the Frame represented by the Polgon. */
      frm = astGetFrame( new->frameset, AST__CURRENT );

/* Get a mesh of points covering the Polygon in this Frame. */
      mesh = astRegMesh( new );

/* Get the Region describing the positional uncertainty in this Frame. */
      unc = astGetUncFrm( new, AST__CURRENT );

/* Get the positions of the vertices within this Frame. */
      ps2 = astRegTransform( this, this->points, 1, NULL, NULL );
      ptr2 = astGetPoints( ps2 );

/* Get the number of vertices. */
      nv = astGetNpoint( ps2 );

/* Re-organise the vertex axis values into the form required by the
   Polygon constructor function. */
      mem = astMalloc( sizeof( double)*(size_t)( 2*nv ) );
      if( astOK ) {
         ok = 1;
         p = mem;
         q = ptr2[ 0 ];
         for( iv = 0; iv < nv; iv++ ) {
            if( ( *(p++) = *(q++) ) == AST__BAD ) ok = 0;
         }
         q = ptr2[ 1 ];
         for( iv = 0; iv < nv; iv++ ) *(p++) = *(q++);

/* Create a new Polygon using these transformed vertices. */
         newpol = ok ? astPolygon( frm, nv, nv, mem, unc, "", status ) : NULL;

/* See if all points within the mesh created from the original Polygon fall 
   on the boundary of the new Polygon, to within the uncertainty of the 
   Region. */
         if( newpol && astRegPins( newpol, mesh, NULL, NULL ) ) {

/* If so, use the new Polygon in place of the original Region. */
            (void) astAnnul( new );
            new = astClone( newpol );
            simpler =1;
         }

/* Free resources. */
         if( newpol ) newpol = astAnnul( newpol );
      }

      frm = astAnnul( frm );
      mesh = astAnnul( mesh );
      unc = astAnnul( unc );
      ps2 = astAnnul( ps2 );
      mem = astFree( mem );
   }
   map = astAnnul( map );

/* If any simplification could be performed, copy Region attributes from 
   the supplied Region to the returned Region, and return a pointer to it.
   Otherwise, return a clone of the supplied pointer. */
   if( simpler ){
      astRegOverlay( new, this );
      result = (AstMapping *) new;

   } else {
      new = astAnnul( new );
      result = astClone( this );
   }

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
*     Apply a Polygon to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "polygon.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     Polygon member function (over-rides the astTransform protected
*     method inherited from the Mapping class).

*  Description:
*     This function takes a Polygon and a set of points encapsulated in a
*     PointSet and transforms the points by setting axis values to
*     AST__BAD for all points which are outside the region. Points inside
*     the region are copied unchanged from input to output.

*  Parameters:
*     this
*        Pointer to the Polygon.
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
*     match the number of axes in the Frame represented by the Polygon.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstFrame *frm;                /* Pointer to base Frame in FrameSet */
   AstLineDef *a;                /* Line from inside point to test point */
   AstLineDef *b;                /* Polygon edge */
   AstPointSet *in_base;         /* PointSet holding base Frame input positions*/
   AstPointSet *result;          /* Pointer to output PointSet */
   AstPolygon *this;             /* Pointer to Polygon */
   double **ptr_in;              /* Pointer to input base Frame coordinate data */
   double **ptr_out;             /* Pointer to output current Frame coordinate data */
   double *px;                   /* Pointer to array of first axis values */
   double *py;                   /* Pointer to array of second axis values */
   double p[ 2 ];                /* Current test position */
   int closed;                   /* Is the boundary part of the Region? */
   int i;                        /* Edge index */
   int icoord;                   /* Coordinate index */
   int in_region;                /* Is the point inside the Region? */
   int ncoord_out;               /* No. of current Frame axes */
   int ncross;                   /* Number of crossings */
   int neg;                      /* Has the Region been negated? */
   int npoint;                   /* No. of input points */
   int nv;                       /* No. of vertices */
   int point;                    /* Loop counter for input points */
   int pos;                      /* Is test position in, on, or outside boundary? */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the Polygon structure. */
   this = (AstPolygon *) this_mapping;

/* Apply the parent mapping using the stored pointer to the Transform member
   function inherited from the parent Region class. This function validates
   all arguments and generates an output PointSet if necessary,
   containing a copy of the input PointSet. */
   result = (*parent_transform)( this_mapping, in, forward, out, status );

/* Get the number of points to be transformed. */
   npoint = astGetNpoint( result );

/* Get a pointer to the output axis values. */
   ptr_out = astGetPoints( result );   

/* Find the number of axes in the current Frame. This need not be 2 (the
   number of axes in the *base* Frame must be 2 however). */
   ncoord_out = astGetNcoord( result );

/* We will now extend the parent astTransform method by performing the
   calculations needed to generate the output coordinate values. */

/* First use the encapsulated FrameSet to transform the supplied positions
   from the current Frame in the encapsulated FrameSet (the Frame
   represented by the Region), to the base Frame (the Frame in which the
   Region is defined). This call also returns a pointer to the base Frame
   of the encapsulated FrameSet. Note, the returned pointer may be a
   clone of the "in" pointer, and so we must be carefull not to modify the
   contents of the returned PointSet. */
   in_base = astRegTransform( this, in, 0, NULL, &frm );
   ptr_in = astGetPoints( in_base );

/* Get the number of vertices in the polygon. */
   nv = astGetNpoint( ((AstRegion *) this)->points );

/* See if the boundary is part of the Region. */
   closed = astGetClosed( this );

/* See if the Region has been negated. */
   neg = astGetNegated( this );

/* Perform coordinate arithmetic. */
/* ------------------------------ */
   if ( astOK ) {
      px = ptr_in[ 0 ];
      py = ptr_in[ 1 ];

/* Loop round each supplied point in the base Frame of the polygon. */
      for ( point = 0; point < npoint; point++, px++, py++ ) {

/* If the input point is bad, indicate that bad output values should be
   returned. */
         if( *px == AST__BAD || *py == AST__BAD ) {
            in_region = 0;

/* Otherwise, we first determine if the point is inside, outside, or on, 
   the Polygon boundary. Initialially it is unknown. */
         } else {

/* Ensure cached information is available.*/
            Cache( this, status );

/* Create a definition of the line from a point which is inside the
   polygon to the supplied point. This is a structure which includes
   cached intermediate information which can be used to speed up
   subsequent calculations. */
            p[ 0 ] = *px;
            p[ 1 ] = *py;
            a = astLineDef( frm, this->in, p );

/* We now dtermine the number of times this line crosses the polygon
   boundary. Initialise the number of crossings to zero. */
            ncross = 0;
            pos = UNKNOWN;

/* Loop rouind all edges of the polygon. */
            for( i = 0; i < nv; i++ ) {
               b = this->edges[ i ];

/* If this point is on the current edge, then we need do no more checks
   since we know it is either inside or outside the polygon (depending on
   whether the polygon is closed or not). */
               if( astLineContains( frm, b, 0, p ) ) {
                  pos = ON;
                  break;

/* Otherwise, see if the two lines cross within their extent. If so,
   increment the number of crossings. */
               } else if( astLineCrossing( frm, b, a, NULL ) ) {
                  ncross++;
               }
            }

/* Free resources */
            a = astFree( a );

/* If the position is not on the boundary, it is inside the boundary if
   the number of crossings is even, and outside otherwise. */
            if( pos == UNKNOWN ) pos = ( ncross % 2 == 0 )? IN : OUT;

/* Whether the point is in the Region depends on whether the point is
   inside the polygon boundary, whether the Polygon has been negated, and
   whether the polygon is closed. */
            if( neg ) {
               if( pos == IN ) {
                  in_region = 0;
               } else if( pos == OUT ) {
                  in_region = 1;
               } else if( closed ) {
                  in_region = 1;
               } else {
                  in_region = 0;
               }

            } else {
               if( pos == IN ) {
                  in_region = 1;
               } else if( pos == OUT ) {
                  in_region = 0;
               } else if( closed ) {
                  in_region = 1;
               } else {
                  in_region = 0;
               }
            }
         }

/* If the point is not inside the Region, store bad output values. */
         if( !in_region ) {
            for ( icoord = 0; icoord < ncoord_out; icoord++ ) {
               ptr_out[ icoord ][ point ] = AST__BAD;
            }
         }
      } 
   }

/* Free resources */
   in_base = astAnnul( in_base );
   frm = astAnnul( frm );

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
static void Copy( const AstObject *objin, AstObject *objout, int *status ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for Polygon objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for Polygon objects.

*  Parameters:
*     objin
*        Pointer to the object to be copied.
*     objout
*        Pointer to the object being constructed.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     -  This constructor makes a deep copy.
*/

/* Local Variables: */
   AstPolygon *in;                /* Pointer to input Polygon */
   AstPolygon *out;               /* Pointer to output Polygon  */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output Polygons. */
   in = (AstPolygon *) objin;
   out = (AstPolygon *) objout;

/* For safety, first clear any references to the input memory from
   the output Polygon. */
   out->edges = NULL;

/* Indicate cached information needs nre-calculating. */
   astResetCache( (AstPolygon *) out );
}


/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for Polygon objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for Polygon objects.

*  Parameters:
*     obj
*        Pointer to the object to be deleted.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     This function attempts to execute even if the global error status is
*     set.
*/

/* Local Variables: */
   AstPointSet *ps;                  /* Pointer to PointSet inside Region */
   AstPolygon *this;                 /* Pointer to Polygon */
   int i;                            /* Index of vertex */
   int istat;                        /* Original AST error status */
   int nv;                           /* Number of vertices */
   int rep;                          /* Original error reporting state */

/* Obtain a pointer to the Polygon structure. */
   this = (AstPolygon *) obj;

/* Annul all resources. */
   ps = ((AstRegion *) this)->points;
   if( this->edges && ps ) {
 
/* Ensure we get a value for "nv" even if an error has occurred. */
      istat = astStatus;
      astClearStatus;
      rep = astReporting( 0 );

      nv = astGetNpoint( ps );

      astSetStatus( istat );
      astReporting( rep );

/* Free the structures holding the edge information. */
      for( i = 0; i < nv; i++ ) {
         this->edges[ i ] = astFree( this->edges[ i ] );
      }
      this->edges = astFree( this->edges );

   }
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for Polygon objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the Polygon class to an output Channel.

*  Parameters:
*     this
*        Pointer to the Polygon whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstPolygon *this;                 /* Pointer to the Polygon structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Polygon structure. */
   this = (AstPolygon *) this_object;

/* Write out values representing the instance variables for the
   Polygon class.  Accompany these with appropriate comment strings,
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
/* Implement the astIsAPolygon and astCheckPolygon functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(Polygon,Region,check,&class_check)
astMAKE_CHECK(Polygon)

AstPolygon *astPolygon_( void *frame_void, int npnt, int dim, 
                         const double *points, AstRegion *unc,
                         const char *options, int *status, ...) {
/*
*++
*  Name:
c     astPolygon
f     AST_POLYGON

*  Purpose:
*     Create a Polygon.

*  Type:
*     Public function.

*  Synopsis:
c     #include "polygon.h"
c     AstPolygon *astPolygon( AstFrame *frame, int npnt, int dim, 
c                             const double *points, AstRegion *unc,
c                             const char *options, ... )
f     RESULT = AST_POLYGON( FRAME, NPNT, DIM, POINTS, UNC, OPTIONS, STATUS )

*  Class Membership:
*     Polygon constructor.

*  Description:
*     This function creates a new Polygon object and optionally initialises 
*     its attributes.
*
*     The Polygon class implements a polygonal area, defined by a
*     collection of vertices, within a 2-dimensional Frame. The vertices
*     are connected together by geodesic curves within the encapsulated Frame.
*     For instance, if the encapsulated Frame is a simple Frame then the
*     geodesics will be straight lines, but if the Frame is a SkyFrame then 
*     the geodesics will be great circles. Note, the vertices must be
*     supplied in an order such that the inside of the polygon is to the
*     left of the boundary as the vertices are traversed. Supplying them
*     in the reverse order will effectively negate the polygon.
*
*     Within a SkyFrame, neighbouring vertices are always joined using the 
*     shortest path. Thus if an edge of 180 degrees or more in length is
*     required, it should be split into section each of which is less
*     than 180 degrees. The closed path joining all the vertices in order
*     will divide the celestial sphere into two disjoint regions. The
*     inside of the polygon is the region which is circled in an
*     anti-clockwise manner (when viewed from the outside of the celestial
*     sphere) when moving through the list of vertices in the order in
*     which they were supplied when the Polygon was created (i.e. the
*     inside is to the left of the boundary when moving through the
*     vertices in the order supplied).

*  Parameters:
c     frame
f     FRAME = INTEGER (Given)
*        A pointer to the Frame in which the region is defined. It must
*        have exactly 2 axes. A deep copy is taken of the supplied Frame. 
*        This means that any subsequent changes made to the Frame using the 
*        supplied pointer will have no effect the Region.
c     npnt
f     NPNT = INTEGER (Given)
*        The number of points in the Region. 
c     dim
f     DIM = INTEGER (Given)
c        The number of elements along the second dimension of the "points"
f        The number of elements along the first dimension of the POINTS
*        array (which contains the point coordinates). This value is
*        required so that the coordinate values can be correctly
*        located if they do not entirely fill this array. The value
c        given should not be less than "npnt".
f        given should not be less than NPNT.
c     points
f     POINTS( DIM, 2 ) = DOUBLE PRECISION (Given)
c        The address of the first element of a 2-dimensional array of 
c        shape "[2][dim]" giving the physical coordinates of the vertices.
c        These should be stored such that the value of coordinate 
c        number "coord" for point number "pnt" is found in element 
c        "in[coord][pnt]".
f        A 2-dimensional array giving the physical coordinates of the
f        vertices. These should be stored such that the value of coordinate 
f        number COORD for point number PNT is found in element IN(PNT,COORD).
c     unc
f     UNC = INTEGER (Given)
*        An optional pointer to an existing Region which specifies the 
*        uncertainties associated with the boundary of the Box being created. 
*        The uncertainty in any point on the boundary of the Box is found by 
*        shifting the supplied "uncertainty" Region so that it is centred at 
*        the boundary point being considered. The area covered by the
*        shifted uncertainty Region then represents the uncertainty in the
*        boundary position. The uncertainty is assumed to be the same for
*        all points.
*
*        If supplied, the uncertainty Region must be of a class for which 
*        all instances are centro-symetric (e.g. Box, Circle, Ellipse, etc.) 
*        or be a Prism containing centro-symetric component Regions. A deep 
*        copy of the supplied Region will be taken, so subsequent changes to 
*        the uncertainty Region using the supplied pointer will have no 
*        effect on the created Box. Alternatively, 
f        a null Object pointer (AST__NULL) 
c        a NULL Object pointer 
*        may be supplied, in which case a default uncertainty is used 
*        equivalent to a box 1.0E-6 of the size of the Box being created.
*
*        The uncertainty Region has two uses: 1) when the 
c        astOverlap
f        AST_OVERLAP 
*        function compares two Regions for equality the uncertainty
*        Region is used to determine the tolerance on the comparison, and 2)
*        when a Region is mapped into a different coordinate system and
*        subsequently simplified (using 
c        astSimplify),
f        AST_SIMPLIFY),
*        the uncertainties are used to determine if the transformed boundary 
*        can be accurately represented by a specific shape of Region.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new Polygon. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new Polygon. The syntax used is identical to that for the
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
c     astPolygon()
f     AST_POLYGON = INTEGER
*        A pointer to the new Polygon.

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
   astDECLARE_GLOBALS;           /* Pointer to thread-specific global data */
   AstFrame *frame;              /* Pointer to Frame structure */
   AstPolygon *new;            /* Pointer to new Polygon */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Obtain and validate a pointer to the supplied Frame structure. */
   frame = astCheckFrame( frame_void );

/* Initialise the Polygon, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitPolygon( NULL, sizeof( AstPolygon ), !class_init, 
                         &class_vtab, "Polygon", frame, npnt, 
                         dim, points, unc );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new Polygon's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new Polygon. */
   return new;
}

AstPolygon *astPolygonId_( void *frame_void, int npnt, int dim,
                           const double *points, void *unc_void, 
                           const char *options, ... ) {
/*
*  Name:
*     astPolygonId_

*  Purpose:
*     Create a Polygon.

*  Type:
*     Private function.

*  Synopsis:
*     #include "polygon.h"
*     AstPolygon *astPolygonId_( void *frame_void, int npnt,
*                                int dim, const double *points, void *unc_void,
*                                const char *options, ... )

*  Class Membership:
*     Polygon constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astPolygon constructor function. It returns an ID value (instead
*     of a true C pointer) to external users, and must be provided
*     because astPolygon_ has a variable argument list which cannot be
*     encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     The variable argument list also prevents this function from
*     invoking astPolygon_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astPolygon_.

*  Returned Value:
*     The ID value associated with the new Polygon.
*/

/* Local Variables: */
   astDECLARE_GLOBALS;           /* Pointer to thread-specific global data */
   AstFrame *frame;              /* Pointer to Frame structure */
   AstPolygon *new;              /* Pointer to new Polygon */
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

/* Initialise the Polygon, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitPolygon( NULL, sizeof( AstPolygon ), !class_init, 
                         &class_vtab, "Polygon", frame, npnt, dim, 
                         points, unc );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new Polygon's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new Polygon. */
   return astMakeId( new );
}


AstPolygon *astInitPolygon_( void *mem, size_t size, int init, AstPolygonVtab *vtab, 
                             const char *name, AstFrame *frame, int npnt, 
                             int dim, const double *points, AstRegion *unc, int *status ) {
/*
*+
*  Name:
*     astInitPolygon

*  Purpose:
*     Initialise a Polygon.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "polygon.h"
*     AstPolygon *astInitPolygon( void *mem, size_t size, int init, AstPolygonVtab *vtab, 
*                                 const char *name, AstFrame *frame, int npnt, 
*                                 int dim, const double *points, AstRegion *unc )

*  Class Membership:
*     Polygon initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new Polygon object. It allocates memory (if necessary) to accommodate
*     the Polygon plus any additional data associated with the derived class.
*     It then initialises a Polygon structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a Polygon at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the Polygon is to be initialised.
*        This must be of sufficient size to accommodate the Polygon data
*        (sizeof(Polygon)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the Polygon (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the Polygon
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the Polygon's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new Polygon.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the astGetClass
*        method).
*     frame
*        A pointer to the Frame in which the region is defined.
*     npnt
*        The number of points in the Region. 
*     dim
*        The number of elements along the second dimension of the "points"
*        array (which contains the point coordinates). This value is
*        required so that the coordinate values can be correctly
*        located if they do not entirely fill this array. The value
*        given should not be less than "npnt".
*     points
*        The address of the first element of a 2-dimensional array of 
*        shape "[2][dim]" giving the physical coordinates of the 
*        points. These should be stored such that the value of coordinate 
*        number "coord" for point number "pnt" is found in element 
*        "in[coord][pnt]".
*     unc
*        A pointer to a Region which specifies the uncertainty in the
*        supplied positions (all points in the new Polygon being 
*        initialised are assumed to have the same uncertainty). A NULL 
*        pointer can be supplied, in which case default uncertainties equal 
*        to 1.0E-6 of the dimensions of the new Polygon's bounding box are 
*        used. If an uncertainty Region is supplied, it must be either a Box, 
*        a Circle or an Ellipse, and its encapsulated Frame must be related
*        to the Frame supplied for parameter "frame" (i.e. astConvert
*        should be able to find a Mapping between them). Two positions 
*        the "frame" Frame are considered to be co-incident if their 
*        uncertainty Regions overlap. The centre of the supplied
*        uncertainty Region is immaterial since it will be re-centred on the 
*        point being tested before use. A deep copy is taken of the supplied 
*        Region.

*  Returned Value:
*     A pointer to the new Polygon.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstPolygon *new;        /* Pointer to new Polygon */
   AstPointSet *pset;        /* Pointer to PointSet holding points */
   const double *q;          /* Pointer to next supplied axis value */
   double **ptr;             /* Pointer to data in pset */
   double *p;                /* Pointer to next PointSet axis value */
   int i;                    /* Axis index */
   int j;                    /* Point index */
   int nin;                  /* No. of axes */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitPolygonVtab( vtab, name );

/* Initialise. */
   new = NULL;

/* Check the number of axis values per position is correct. */
   nin = astGetNaxes( frame );
   if( nin != 2 ) {
      astError( AST__BADIN, "astInitPolygon(%s): The supplied %s has %d "
                "axes - polygons must have exactly 2 axes.", status, name,
                astGetClass( frame ), nin );

/* If so create a PointSet and store the supplied points in it. Check
   none are bad. */
   } else {
      pset = astPointSet( npnt, 2, "", status );
      ptr = astGetPoints( pset );
      for( i = 0; i < 2 && astOK; i++ ) {
         p = ptr[ i ];
         q = points + i*dim;
         for( j = 0; j < npnt; j++ ) {
            if( (*(p++) = *(q++)) == AST__BAD ) {
               astError( AST__BADIN, "astInitPolygon(%s): One or more "
                         "bad axis values supplied for the vertex "
                         "number %d.", status, name, j + 1 );
               break;
            }
         }
      }

/* Initialise a Region structure (the parent class) as the first component
   within the Polygon structure, allocating memory if necessary. */
      new = (AstPolygon *) astInitRegion( mem, size, 0, (AstRegionVtab *) vtab, 
                                          name, frame, pset, unc );
      if ( astOK ) {

/* Initialise the Polygon data. */
/* ------------------------------ */
         new->lbnd[ 0 ] = AST__BAD;
         new->ubnd[ 0 ] = AST__BAD;
         new->lbnd[ 1 ] = AST__BAD;
         new->ubnd[ 1 ] = AST__BAD;
         new->edges = NULL;
         new->stale = 1;

/* If an error occurred, clean up by deleting the new Polygon. */
         if ( !astOK ) new = astDelete( new );
      }

/* Free resources. */
      pset = astAnnul( pset );

   }

/* Return a pointer to the new Polygon. */
   return new;
}

AstPolygon *astLoadPolygon_( void *mem, size_t size, AstPolygonVtab *vtab, 
                             const char *name, AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadPolygon

*  Purpose:
*     Load a Polygon.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "polygon.h"
*     AstPolygon *astLoadPolygon( void *mem, size_t size, AstPolygonVtab *vtab, 
*                                 const char *name, AstChannel *channel )

*  Class Membership:
*     Polygon loader.

*  Description:
*     This function is provided to load a new Polygon using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     Polygon structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a Polygon at the start of the memory
*     passed via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory into which the Polygon is to be
*        loaded.  This must be of sufficient size to accommodate the
*        Polygon data (sizeof(Polygon)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the Polygon (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the Polygon structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstPolygon) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new Polygon. If this is NULL, a pointer
*        to the (static) virtual function table for the Polygon class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "Polygon" is used instead.

*  Returned Value:
*     A pointer to the new Polygon.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS;           /* Pointer to thread-specific global data */
   AstPolygon *new;              /* Pointer to the new Polygon */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this Polygon. In this case the
   Polygon belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstPolygon );
      vtab = &class_vtab;
      name = "Polygon";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitPolygonVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built Polygon. */
   new = astLoadRegion( mem, size, (AstRegionVtab *) vtab, name,
                        channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "Polygon" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */


/* There are no values to read. */
/* ---------------------------- */

/* Initialise other class properties. */
      new->lbnd[ 0 ] = AST__BAD;
      new->ubnd[ 0 ] = AST__BAD;
      new->lbnd[ 1 ] = AST__BAD;
      new->ubnd[ 1 ] = AST__BAD;
      new->edges = NULL;
      new->stale = 1;

/* If an error occurred, clean up by deleting the new Polygon. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new Polygon pointer. */
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


















