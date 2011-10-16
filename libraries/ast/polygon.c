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
*     anti-clockwise manner (when viewed from the inside of the celestial
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
c     In addition to those functions applicable to all Regions, the
c     following functions may also be applied to all Polygons:
f     In addition to those routines applicable to all Regions, the
f     following routines may also be applied to all Polygons:
*
c     - astDownsize: Reduce the number of vertices in a Polygon.
f     - AST_DOWNSIZE: Reduce the number of vertices in a Polygon.
c     - astOutline<X>: Create a Polygon outlining values in a pixel array
f     - AST_OUTLINE<X>: Create a Polygon outlining values in a pixel array

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     28-MAY-2009 (DSB):
*        Added astDownsize.
*     29-MAY-2009 (DSB):
*        Added astOutline<X>.
*     30-JUN-2009 (DSB):
*        Override astGetBounded.
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
#include "pal.h"                 /* SLALIB library interface */
#include "frame.h"               /* Coordinate system description */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <float.h>
#include <math.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>

/* Type definitions. */
/* ================= */

/* A structure that holds information about an edge of the new Polygon
   being created by astDownsize. The edge is a line betweeen two of the
   vertices of the original Polygon. */
typedef struct Segment {
   int i1;        /* Index of starting vertex within old Polygon */
   int i2;        /* Index of ending vertex within old Polygon */
   double error;  /* Max geodesic distance from any old vertex to the line */
   int imax;      /* Index of the old vertex at which max error is reached */
   struct Segment *next;/* Pointer to next Segment in a double link list */
   struct Segment *prev;/* Pointer to previous Segment in a double link list */
} Segment;


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

/* Define a macro that expands to a single prototype for function
   FindInsidePoint for a given data type and operation. */
#define FINDINSIDEPOINT_PROTO0(X,Xtype,Oper) \
static void FindInsidePoint##Oper##X( Xtype, const Xtype *, const int[2], const int[2], int *, int *, int *, int * );

/* Define a macro that expands to a set of prototypes for all operations
   for function FindInsidePoint for a given data type. */
#define FINDINSIDEPOINT_PROTO(X,Xtype) \
FINDINSIDEPOINT_PROTO0(X,Xtype,LT) \
FINDINSIDEPOINT_PROTO0(X,Xtype,LE) \
FINDINSIDEPOINT_PROTO0(X,Xtype,EQ) \
FINDINSIDEPOINT_PROTO0(X,Xtype,GE) \
FINDINSIDEPOINT_PROTO0(X,Xtype,GT) \
FINDINSIDEPOINT_PROTO0(X,Xtype,NE)

/* Use the above macros to define all FindInsidePoint prototypes for all
   data types and operations. */
#if HAVE_LONG_DOUBLE     /* Not normally implemented */
FINDINSIDEPOINT_PROTO(LD,long double)
#endif
FINDINSIDEPOINT_PROTO(D,double)
FINDINSIDEPOINT_PROTO(L,long int)
FINDINSIDEPOINT_PROTO(UL,unsigned long int)
FINDINSIDEPOINT_PROTO(I,int)
FINDINSIDEPOINT_PROTO(UI,unsigned int)
FINDINSIDEPOINT_PROTO(S,short int)
FINDINSIDEPOINT_PROTO(US,unsigned short int)
FINDINSIDEPOINT_PROTO(B,signed char)
FINDINSIDEPOINT_PROTO(UB,unsigned char)
FINDINSIDEPOINT_PROTO(F,float)

/* Define a macro that expands to a single prototype for function
   TraceEdge for a given data type and operation. */
#define TRACEEDGE_PROTO0(X,Xtype,Oper) \
static AstPointSet *TraceEdge##Oper##X( Xtype, const Xtype *, const int[2], const int[2], int, int, int, int, int, int * );

/* Define a macro that expands to a set of prototypes for all operations
   for function TraceEdge for a given data type. */
#define TRACEEDGE_PROTO(X,Xtype) \
TRACEEDGE_PROTO0(X,Xtype,LT) \
TRACEEDGE_PROTO0(X,Xtype,LE) \
TRACEEDGE_PROTO0(X,Xtype,EQ) \
TRACEEDGE_PROTO0(X,Xtype,GE) \
TRACEEDGE_PROTO0(X,Xtype,GT) \
TRACEEDGE_PROTO0(X,Xtype,NE)

/* Use the above macros to define all TraceEdge prototypes for all
   data types and operations. */
#if HAVE_LONG_DOUBLE     /* Not normally implemented */
TRACEEDGE_PROTO(LD,long double)
#endif
TRACEEDGE_PROTO(D,double)
TRACEEDGE_PROTO(L,long int)
TRACEEDGE_PROTO(UL,unsigned long int)
TRACEEDGE_PROTO(I,int)
TRACEEDGE_PROTO(UI,unsigned int)
TRACEEDGE_PROTO(S,short int)
TRACEEDGE_PROTO(US,unsigned short int)
TRACEEDGE_PROTO(B,signed char)
TRACEEDGE_PROTO(UB,unsigned char)
TRACEEDGE_PROTO(F,float)

/* Non-generic function prototypes. */
static AstMapping *Simplify( AstMapping *, int * );
static AstPointSet *DownsizePoly( AstPointSet *, double, int, AstFrame *, int * );
static AstPointSet *RegBaseMesh( AstRegion *, int * );
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static AstPolygon *Downsize( AstPolygon *, double, int, int * );
static Segment *AddToChain( Segment *, Segment *, int * );
static Segment *NewSegment( Segment *, int, int, int, int * );
static Segment *RemoveFromChain( Segment *, Segment *, int * );
static double Polywidth( AstFrame *, AstLineDef **, int, int, double[ 2 ], int * );
static int GetBounded( AstRegion *, int * );
static int IntCmp( const void *, const void * );
static int RegPins( AstRegion *, AstPointSet *, AstRegion *, int **, int * );
static int RegTrace( AstRegion *, int, double *, double **, int * );
static void Cache( AstPolygon *, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void FindMax( Segment *, AstFrame *, double *, double *, int, int, int * );
static void RegBaseBox( AstRegion *this, double *, double *, int * );
static void ResetCache( AstRegion *this, int * );
static void SetPointSet( AstPolygon *, AstPointSet *, int * );
static void SetRegFS( AstRegion *, AstFrame *, int * );
static void SmoothPoly( AstPointSet *, int, double, int * );

/* Member functions. */
/* ================= */
static Segment *AddToChain( Segment *head, Segment *seg, int *status ){
/*
*  Name:
*     AddToChain

*  Purpose:
*     Add a Segment into the linked list of Segments, maintaining the
*     required order in the list.

*  Type:
*     Private function.

*  Synopsis:
*     #include "polygon.h"
*     Segment *AddToChain( Segment *head, Segment *seg, int *status )

*  Class Membership:
*     Polygon member function

*  Description:
*     The linked list of Segments maintained by astDownsize is searched
*     from the high error end (the head), until a Segment is foound which
*     has a lower error than the supplied segment. The supplied Segment
*     is then inserted into the list at that point.

*  Parameters:
*     head
*        The Segment structure at the head of the list (i.e. the segment
*        with maximum error).
*     seg
*        The Segment to be added into the list.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the link head (which will have changed if "seg" has a
*     higher error than the original head).

*/

/* Local Variables: */
   Segment *tseg;

/* Check the global error status. */
   if ( !astOK ) return head;

/* If the list is empty, return the supplied segment as the new list
   head. */
   if( !head ) {
      head = seg;

/* If the supplied segment has a higher error than the original head,
   insert the new segment in front of the original head. */
   } else if( seg->error > head->error ){
      seg->next = head;
      head->prev = seg;
      head = seg;

/* Otherwise, move down the list from the head until a segment is found
   which has a lower error than the supplied Segment. Then insert the
   supplied segment into the list in front of it. */
   } else {
      tseg = head;
      seg->next = NULL;

      while( tseg->next ) {
         if( seg->error > tseg->next->error ) {
            seg->next = tseg->next;
            seg->prev = tseg;
            tseg->next->prev = seg;
            tseg->next = seg;
            break;
         }
         tseg = tseg->next;
      }

      if( !seg->next ) {
         tseg->next = seg;
         seg->prev = tseg;
      }
   }

/* Return the new head. */
   return head;
}

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

/* Do nothing if the cached information is up to date. */
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
         this->startsat = astMalloc( sizeof( double )*(size_t) nv );
      }

/* Check pointers can be used safely. */
      if( this->edges ) {

/* Create and store a description of each edge. Also form the total
   distance round the polygon, and the distance from the first vertex
   at which each edge starts. */
         this->totlen = 0.0;
         start[ 0 ] = ptr[ 0 ][ nv - 1 ];
         start[ 1 ] = ptr[ 1 ][ nv - 1 ];

         for( i = 0; i < nv; i++ ) {
            end[ 0 ] = ptr[ 0 ][ i ];
            end[ 1 ] = ptr[ 1 ][ i ];
            this->edges[ i ] = astLineDef( frm, start, end );
            start[ 0 ] = end[ 0 ];
            start[ 1 ] = end[ 1 ];

            this->startsat[ i ] = this->totlen;
            this->totlen += this->edges[ i ]->length;
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
               if( polwid > maxwid && polwid != AST__BAD ) {
                  maxwid = polwid;
                  (this->in)[ 0 ] = polcen[ 0 ];
                  (this->in)[ 1 ] = polcen[ 1 ];
               }
            }
         }

/* If no width was found it probably means that the polygon vertices were
   given in clockwise order, resulting in the above process probing the
   infinite extent outside the polygonal hole. In this case any point
   outside the hole will do, so we use the current contents of the
   "polcen" array. Set a flag indicating if the vertices are stored in
   anti-clockwise order. */
         if( maxwid < 0.0 ) {
            (this->in)[ 0 ] = polcen[ 0 ];
            (this->in)[ 1 ] = polcen[ 1 ];
            this->acw = 0;
         } else {
            this->acw = 1;
         }
      }

/* Free resources */
      frm = astAnnul( frm );

/* Indicate cached information is up to date. */
      this->stale = 0;
   }
}

static AstPolygon *Downsize( AstPolygon *this, double maxerr, int maxvert,
                             int *status ) {
/*
*++
*  Name:
c     astDownsize
f     AST_DOWNSIZE

*  Purpose:
*     Reduce the number of vertices in a Polygon.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "polygon.h"
c     AstPolygon *astDownsize( AstPolygon *this, double maxerr, int maxvert )
f     RESULT = AST_DOWNSIZE( THIS, MAXERR, MAXVERT, STATUS )

*  Class Membership:
*     Polygon method.

*  Description:
*     This function returns a pointer to a new Polygon that contains a
*     subset of the vertices in the supplied Polygon. The subset is
*     chosen so that the returned Polygon is a good approximation to
*     the supplied Polygon, within the limits specified by the supplied
*     parameter values. That is, the density of points in the returned
*     Polygon is greater at points where the curvature of the boundary of
*     the supplied Polygon is greater.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Polygon.
c     maxerr
f     MAXERR = DOUBLE PRECISION (Given)
*        The maximum allowed discrepancy between the supplied and
*        returned Polygons, expressed as a geodesic distance within the
*        Polygon's coordinate frame. If this is zero or less, the
*        returned Polygon will have the number of vertices specified by
c        maxvert.
f        MAXVERT.
c     maxvert
f     MAXVERT = INTEGER (Given)
*        The maximum allowed number of vertices in the returned Polygon.
*        If this is less than 3, the number of vertices in the returned
*        Polygon will be the minimum needed to achieve the maximum
*        discrepancy specified by
c        maxerr.
f        MAXERR.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astDownsize()
f     AST_DOWNSIZE = INTEGER
*        Pointer to the new Polygon.

*  Notes:
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--
*/

/* Local Variables: */
   AstFrame *frm;         /* Base Frame from the Polygon */
   AstPointSet *pset;     /* PointSet holding vertices of downsized polygon */
   AstPolygon *result;    /* Returned pointer to new Polygon */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the base Frame of the Polygon. */
   frm = astGetFrame( ((AstRegion *) this)->frameset, AST__BASE );

/* Create a PointSet holding the vertices of the downsized polygon. */
   pset = DownsizePoly( ((AstRegion *) this)->points, maxerr, maxvert,
                        frm, status );

/* Take a deep copy of the supplied Polygon. */
   result = astCopy( this );

/* Change the PointSet within the result Polygon to the one created above. */ \
   SetPointSet( result, pset, status ); \

/* Free resources. */
   frm = astAnnul( frm );
   pset = astAnnul( pset );

/* If an error occurred, annul the returned Polygon. */
   if ( !astOK ) result = astAnnul( result );

/* Return the result. */
   return result;
}

static AstPointSet *DownsizePoly( AstPointSet *pset, double maxerr,
                                  int maxvert, AstFrame *frm, int *status ) {
/*
*  Name:
*     DownsizePoly

*  Purpose:
*     Reduce the number of vertices in a Polygon.

*  Type:
*     Private function.

*  Synopsis:
*    #include "polygon.h"
*    AstPointSet *DownsizePoly( AstPointSet *pset, double maxerr, int maxvert,
*                               AstFrame *frm, int *status )

*  Class Membership:
*     Polygon member function.

*  Description:
*     This function returns a pointer to a new PointSet that contains a
*     subset of the vertices in the supplied PointSet. The subset is
*     chosen so that the returned polygon is a good approximation to
*     the supplied polygon, within the limits specified by the supplied
*     parameter values. That is, the density of points in the returned
*     polygon is greater at points where the curvature of the boundary of
*     the supplied polygon is greater.

*  Parameters:
*     pset
*        Pointer to the PointSet holding the polygon vertices.
*     maxerr
*        The maximum allowed discrepancy between the supplied and
*        returned Polygons, expressed as a geodesic distance within the
*        Polygon's coordinate frame. If this is zero or less, the
*        returned Polygon will have the number of vertices specified by
*        maxvert.
*     maxvert
*        The maximum allowed number of vertices in the returned Polygon.
*        If this is less than 3, the number of vertices in the returned
*        Polygon will be the minimum needed to achieve the maximum
*        discrepancy specified by
*        maxerr.
*     frm
*        Pointer to the Frame in which the polygon is defined.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*        Pointer to the new PointSet.

*  Notes:
*     - A null Object pointer (AST__NULL) will be returned if this
*     function is invoked with the AST error status set, or if it
*     should fail for any reason.
*/

/* Local Variables: */
   AstPointSet *result;   /* Returned pointer to new PointSet */
   Segment *head;         /* Pointer to new polygon edge with highest error */
   Segment *seg1;         /* Pointer to new polygon edge */
   Segment *seg2;         /* Pointer to new polygon edge */
   Segment *seg3;         /* Pointer to new polygon edge */
   double **ptr;          /* Pointer to arrays of axis values */
   double *x;             /* Pointer to array of X values for old Polygon */
   double *xnew;          /* Pointer to array of X values for new Polygon */
   double *y;             /* Pointer to array of Y values for old Polygon */
   double *ynew;          /* Pointer to array of Y values for new Polygon */
   double x1;             /* Lowest X value at any vertex */
   double x2;             /* Highest X value at any vertex */
   double y1;             /* Lowest Y value at any vertex */
   double y2;             /* Highest Y value at any vertex */
   int *newpoly;          /* Holds indices of retained input vertices */
   int i1;                /* Index of first vertex added to output polygon */
   int i1x;               /* Index of vertex with lowest X */
   int i1y;               /* Index of vertex with lowest Y */
   int i2;                /* Index of second vertex added to output polygon */
   int i2x;               /* Index of vertex with highest X */
   int i2y;               /* Index of vertex with highest Y */
   int i3;                /* Index of third vertex added to output polygon */
   int iadd;              /* Normalised vertex index */
   int iat;               /* Index at which to store new vertex index */
   int newlen;            /* Number of vertices currently in new Polygon */
   int nv;                /* Number of vertices in old Polygon */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get the number of vertices in the supplied polygon. */
   nv = astGetNpoint( pset );

/* If the maximum error allowed is zero, and the maximum number of
   vertices is equal to or greater than the number in the supplied
   polygon, just return a deep copy of the supplied PointSet. */
   if( maxerr <= 0.0 && ( maxvert < 3 || maxvert >= nv ) ) {
      result = astCopy( pset );

/* Otherwise... */
   } else {

/* Get pointers to the X and Y arrays holding the vertex coordinates in
   the supplied polygon. */
      ptr = astGetPoints( pset );
      x = ptr[ 0 ];
      y = ptr[ 1 ];

/* Allocate memory for an array to hold the original indices of the vertices
   to be used to create the returned Polygon. This array is expanded as
   needed. */
      newpoly = astMalloc( 10*sizeof( int ) );

/* Check the pointers can be used safely. */
      if( astOK ) {

/* We first need to decide on three widely spaced vertices which form a
   reasonable triangular approximation to the whole polygon. First find
   the vertices with the highest and lowest Y values, and the highest and
   lowest X values. */
         x1 = DBL_MAX;
         x2 = -DBL_MAX;
         y1 = DBL_MAX;
         y2 = -DBL_MAX;

         i1y = i1x = 0;
         i2y = i2x = nv/2;

         for( i3 = 0; i3 < nv; i3++ ) {
            if( y[ i3 ] < y1 ) {
               y1 = y[ i3 ];
               i1y = i3;
            } else if( y[ i3 ] > y2 ) {
               y2 = y[ i3 ];
               i2y = i3;
            }

            if( x[ i3 ] < x1 ) {
               x1 = x[ i3 ];
               i1x = i3;
            } else if( x[ i3 ] > x2 ) {
               x2 = x[ i3 ];
               i2x = i3;
            }
         }

/* Use the axis which spans the greater range. */
         if( y2 - y1 > x2 - x1 ) {
            i1 = i1y;
            i2 = i2y;
         } else {
            i1 = i1x;
            i2 = i2x;
         }

/* The index with vertex i1 is definitely going to be one of our three
   vertices. We are going to use the line from i1 to i2 to choose the two
   other vertices to use. Create a structure describing the segment of the
   Polygon from the lowest value on the selected axis (X or Y) to the
   highest value. As always, the polygons is traversed in an anti-clockwise
   direction. */
         seg1 = NewSegment( NULL, i1, i2, nv, status );

/* Find the vertex within this segment which is furthest away from the
   line on the right hand side (as moving from vertex i1 to vertex i2). */
         FindMax( seg1, frm, x, y, nv, 0, status );

/* Likewise, create a structure describing the remained of the Polygon
   (i.e. the segment from the highest value on the selected axis to the
   lowest value). Then find the vertex within this segment which is
   furthest away from the line on the right hand side. */
         seg2 = NewSegment( NULL, i2, i1, nv, status );
         FindMax( seg2, frm, x, y, nv, 0, status );

/* Select the segment for which the found vertex is furthest from the
   line. */
         if( seg2->error > seg1->error ) {

/* If the second segment, we will use the vertex that is farthest from
   the line as one of our threee vertices. To ensure that movement from
   vertex i1 to i2 to i3 is anti-clockwise, we must use this new vertex
   as vertex i3, not i2. */
            i3 = seg2->imax;

/* Create a description of the polygon segment from vertex i1 to i3, and
   find the vertex which is furthest to the right of the line joining the
   two vertices. We use this as the middle vertex (i2). */
            seg1 = NewSegment( seg1, i1, i3, nv, status );
            FindMax( seg1, frm, x, y, nv, 0, status );
            i2 = seg1->imax;

/* Do the same if we are choosing the other segment, ordering the
   vertices to retain anti-clockwise movement from i1 to i2 to i3. */
         } else {
            i2 = seg1->imax;
            seg1 = NewSegment( seg1, i2, i1, nv, status );
            FindMax( seg1, frm, x, y, nv, 0, status );
            i3 = seg1->imax;
         }

/* Ensure the vertex indices are in the first cycle. */
         if( i2 >= nv ) i2 -= nv;
         if( i3 >= nv ) i3 -= nv;

/* Create Segment structures to describe each of these three edges. */
         seg1 = NewSegment( seg1, i1, i2, nv, status );
         seg2 = NewSegment( seg2, i2, i3, nv, status );
         seg3 = NewSegment( NULL, i3, i1, nv, status );

/* Record these 3 vertices in an array holding the original indices of
   the vertices to be used to create the returned Polygon. */
         newpoly[ 0 ] = i1;
         newpoly[ 1 ] = i2;
         newpoly[ 2 ] = i3;

/* Indicate the new polygon currently has 3 vertices. */
         newlen = 3;

/* Search the old vertices between the start and end of segment 3, looking
   for the vertex which lies furthest from the line of segment 3. The
   residual between this point and the line is stored in the Segment
   structure, as is the index of the vertex at which this maximum residual
   occurred. */
         FindMax( seg3, frm, x, y, nv, 1, status );

/* The "head" variable points to the head of a double linked list of
   Segment structures. This list is ordered by residual, so that the
   Segment with the maximum residual is at the head, and the Segment
   with the minimum residual is at the tail. Initially "seg3" is at the
   head. */
         head = seg3;

/* Search the old vertices between the start and end of segment 1, looking
   for the vertex which lies furthest from the line of segment 1. The
   residual between this point and the line is stored in the Segment
   structure, as is the index of the vertex at which this maximum residual
   occurred. */
         FindMax( seg1, frm, x, y, nv, 1, status );

/* Insert segment 1 into the linked list of Segments, at a position that
   maintains the ordering of the segments by error. Thus the head of the
   list will still have the max error. */
         head = AddToChain( head, seg1, status );

/* Do the same for segment 2. */
         FindMax( seg2, frm, x, y, nv, 1, status );
         head = AddToChain( head, seg2, status );

/* If the maximum allowed number of vertices in the output Polygon is
   less than 3, allow any number of vertices up to the number in the
   input Polygon (termination will then be determined just by "maxerr"). */
         if( maxvert < 3 ) maxvert = nv;

/* Loop round adding an extra vertex to the returned Polygon until the
   maximum residual between the new and old polygons is no more than
   "maxerr". Abort early if the specified maximum number of vertices is
   reached. */
         while( head->error > maxerr && newlen < maxvert ) {

/* The segment at the head of the list has the max error (that is, it is
   the segment that departs most from the supplied Polygon). To make the
   new polygon a better fit to the old polygon, we add the vertex that is
   furthest away from this segment to the new polygon. Remember that a
   polygon is cyclic so if the vertex has an index that is greater than the
   number of vertices in the old polygon, reduce the index by the number
   of vertices in the old polygon. */
            iadd = head->imax;
            if( iadd >= nv ) iadd -= nv;
            iat = newlen++;
            newpoly = astGrow( newpoly, newlen, sizeof( int ) );
            if( !astOK ) break;
            newpoly[ iat ] = iadd;

/* We now split the segment that had the highest error into two segments.
   The split occurs at the vertex that had the highest error. */
            seg1 = NewSegment( NULL, head->imax, head->i2, nv, status );
            seg2 = head;
            seg2->i2 = head->imax;

/* We do not know where these two new segments should be in the ordered
   linked list, so remove them from the list. */
            head = RemoveFromChain( head, seg1, status );
            head = RemoveFromChain( head, seg2, status );

/* Find the vertex that deviates most from the first of these two new
   segments, and then add the segment into the list of vertices, using
   the maximum deviation to determine the position of the segment within
   the list. */
            FindMax( seg1, frm, x, y, nv, 1, status );
            head = AddToChain( head, seg1, status );

/* Do the same for the second new segment. */
            FindMax( seg2, frm, x, y, nv, 1, status );
            head = AddToChain( head, seg2, status );
         }

/* Now we have reached the required accuracy, free resources. */
         while( head ) {
            seg1 = head;
            head = head->next;
            seg1 = astFree( seg1 );
         }

/* If no vertices have been left out, return a deep copy of the supplied
   PointSet. */
         if( newlen == nv ) {
            result = astCopy( pset );

/* Otherwise, sort the indices of the vertices to be retained so that they
   are in the same order as they were in the supplied Polygon. */
         } else if( astOK ){
            qsort( newpoly, newlen, sizeof( int ), IntCmp );

/* Create a new PointSet and get pointers to its axis values. */
            result = astPointSet( newlen, 2, " ", status );
            ptr = astGetPoints( result );
            xnew = ptr[ 0 ];
            ynew = ptr[ 1 ];

/* Copy the axis values for the retained vertices from the old to the new
   PointSet. */
            if( astOK ) {
               for( iat = 0; iat < newlen; iat++ ) {
                  *(xnew++) = x[ newpoly[ iat ] ];
                  *(ynew++) = y[ newpoly[ iat ] ];
               }
            }
         }
      }

/* Free resources. */
      newpoly = astFree( newpoly );
   }

/* If an error occurred, annul the returned PointSet. */
   if ( !astOK ) result = astAnnul( result );

/* Return the result. */
   return result;
}

/*
*  Name:
*     FindInsidePoint

*  Purpose:
*     Find a point that is inside the required outline.

*  Type:
*     Private function.

*  Synopsis:
*     #include "polygon.h"
*     void FindInsidePoint<Oper><X>( <Xtype> value, const <Xtype> array[],
*                                    const int lbnd[ 2 ], const int ubnd[ 2 ],
*                                    int *inx, int *iny, int *iv,
*                                    int *status );

*  Class Membership:
*     Polygon member function

*  Description:
*     The central pixel in the array is checked to see if its value meets
*     the requirements implied by <Oper> and "value". If so, its pixel
*     indices and vector index are returned> if not, a spiral search is
*     made until such a pixel value is found.

*  Parameters:
*     value
*        The data value defining valid pixels.
*     array
*        The data array.
*     lbnd
*        The lower pixel index bounds of the array.
*     ubnd
*        The upper pixel index bounds of the array.
*     inx
*        Pointer to an int in which to return the X pixel index of the
*        first point that meets the requirements implied by <oper> and
*        "value".
*     iny
*        Pointer to an int in which to return the Y pixel index of the
*        first point that meets the requirements implied by <oper> and
*        "value".
*     iv
*        Pointer to an int in which to return the vector index of the
*        first point that meets the requirements implied by <oper> and
*        "value".
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - <Oper> must be one of LT, LE, EQ, NE, GE, GT.


*/

/* Define a macro to implement the function for a specific data
   type and operation. */
#define MAKE_FINDINSIDEPOINT(X,Xtype,Oper,OperI) \
static void FindInsidePoint##Oper##X( Xtype value, const Xtype array[], \
                                      const int lbnd[ 2 ], const int ubnd[ 2 ], \
                                      int *inx, int *iny, int *iv, \
                                      int *status ){ \
\
/* Local Variables: */ \
   const Xtype *pv;  /* Pointer to next data value to test */ \
   const char *text; /* Pointer to text describing oper */ \
   int cy;           /* Central row index */ \
   int iskin;        /* Index of spiral layer being searched */ \
   int nskin;        /* Number of spiral layers to search */ \
   int nx;           /* Pixel per row */ \
   int tmp;          /* Temporary storage */ \
   int xhi;          /* High X pixel index bound of current skin */ \
   int xlo;          /* Low X pixel index bound of current skin */ \
   int yhi;          /* High X pixel index bound of current skin */ \
   int ylo;          /* Low X pixel index bound of current skin */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Find number of pixels in one row of the array. */ \
   nx = ( ubnd[ 0 ] - lbnd[ 0 ] + 1 ); \
\
/* Find the pixel indices of the central pixel */ \
   *inx = ( ubnd[ 0 ] + lbnd[ 0 ] )/2; \
   *iny = ( ubnd[ 1 ] + lbnd[ 1 ] )/2; \
\
/* Initialise the vector index and pointer to refer to the central pixel. */ \
   *iv = ( *inx - lbnd[ 0 ] ) + nx*( *iny - lbnd[ 1 ] ) ; \
   pv = array + (*iv); \
\
/* Test the pixel value, returning if it is valid. This \
   relies on the compiler optimisation to remove the "if" statements \
   for all but the operation appropriate to the function being defined. */ \
   if( OperI == AST__LT ) { \
      if( *pv < value ) return; \
\
   } else if( OperI == AST__LE ) { \
      if( *pv <= value ) return; \
\
   } else if( OperI == AST__EQ ) { \
      if( *pv == value ) return; \
\
   } else if( OperI == AST__NE ) { \
      if( *pv != value ) return; \
\
   } else if( OperI == AST__GE ) { \
      if( *pv >= value ) return; \
\
   } else { \
      if( *pv > value ) return; \
   } \
\
/* The central pixel is invalid if we arrive here. So we need to do a \
   spiral search out from the centre looking for a valid pixel. Record \
   the central row index (this is the row at which we jump to the next \
   outer skin when doing the spiral search below). */ \
   cy = *iny; \
\
/* Find how many skins can be searched as part of the spiral search \
   before the edge of the array is encountered. */ \
   nskin = ubnd[ 0 ] - *inx; \
   tmp = *inx - lbnd[ 0 ]; \
   if( tmp < nskin ) nskin = tmp; \
   tmp = ubnd[ 1 ] - *iny; \
   if( tmp < nskin ) nskin = tmp; \
   tmp = *iny - lbnd[ 1 ]; \
   if( tmp < nskin ) nskin = tmp; \
\
/* Initialise the skin box bounds to be just the central pixel. */ \
   xlo = xhi = *inx; \
   ylo = yhi = *iny; \
\
/* Loop round each skin looking for a valid test pixel. */ \
   for( iskin = 0; iskin < nskin; iskin++ ) { \
\
/* Increment the upper and lower bounds of the box forming the next \
   skin. */ \
      xhi++; \
      xlo--; \
      yhi++; \
      ylo--; \
\
/* Initialise things for the first pixel in the new skin by moving one \
   pixel to the right. */ \
      pv++; \
      (*iv)++; \
      (*inx)++; \
\
/* Move up the right hand edge of the box corresponding to the current \
   skin. We start at the middle of the right hand edge. */ \
      for( *iny = cy; *iny <= yhi; (*iny)++ ) { \
\
/* Test the pixel value, returning if it is valid. This relies on the \
   compiler optimisation to remove the "if" statements for all but the \
   operation appropriate to the function being defined. */ \
         if( OperI == AST__LT ) { \
            if( *pv < value ) return; \
\
         } else if( OperI == AST__LE ) { \
            if( *pv <= value ) return; \
\
         } else if( OperI == AST__EQ ) { \
            if( *pv == value ) return; \
\
         } else if( OperI == AST__NE ) { \
            if( *pv != value ) return; \
\
         } else if( OperI == AST__GE ) { \
            if( *pv >= value ) return; \
\
         } else { \
            if( *pv > value ) return; \
         } \
\
/* Move up a pixel. */ \
         pv += nx; \
         *iv += nx; \
      } \
\
/* Move down a pixel so that *iny == yhi. */ \
      pv -= nx; \
      *iv -= nx; \
      (*iny)--; \
\
/* Move left along the top edge of the box corresponding to the current \
   skin. */ \
      for( *inx = xhi; *inx >= xlo; (*inx)-- ) { \
\
/* Test the pixel value, returning if it is valid. */ \
         if( OperI == AST__LT ) { \
            if( *pv < value ) return; \
\
         } else if( OperI == AST__LE ) { \
            if( *pv <= value ) return; \
\
         } else if( OperI == AST__EQ ) { \
            if( *pv == value ) return; \
\
         } else if( OperI == AST__NE ) { \
            if( *pv != value ) return; \
\
         } else if( OperI == AST__GE ) { \
            if( *pv >= value ) return; \
\
         } else { \
            if( *pv > value ) return; \
         } \
\
/* Move left a pixel. */ \
         pv--; \
         (*iv)--; \
      } \
\
/* Move right a pixel so that *inx == xlo. */ \
      pv++; \
      (*iv)++; \
      (*inx)++; \
\
/* Move down along the left hand edge of the box corresponding to the current \
   skin. */ \
      for( *iny = yhi; *iny >= ylo; (*iny)-- ) { \
\
/* Test the pixel value, returning if it is valid. */ \
         if( OperI == AST__LT ) { \
            if( *pv < value ) return; \
\
         } else if( OperI == AST__LE ) { \
            if( *pv <= value ) return; \
\
         } else if( OperI == AST__EQ ) { \
            if( *pv == value ) return; \
\
         } else if( OperI == AST__NE ) { \
            if( *pv != value ) return; \
\
         } else if( OperI == AST__GE ) { \
            if( *pv >= value ) return; \
\
         } else { \
            if( *pv > value ) return; \
         } \
\
/* Move down a pixel. */ \
         pv -= nx; \
         *iv -= nx; \
      } \
\
/* Move up a pixel so that *iny == ylo. */ \
      pv += nx; \
      *iv += nx; \
      (*iny)++; \
\
/* Move right along the bottom edge of the box corresponding to the current \
   skin. */ \
      for( *inx = xlo; *inx <= xhi; (*inx)++ ) { \
\
/* Test the pixel value, returning if it is valid. */ \
         if( OperI == AST__LT ) { \
            if( *pv < value ) return; \
\
         } else if( OperI == AST__LE ) { \
            if( *pv <= value ) return; \
\
         } else if( OperI == AST__EQ ) { \
            if( *pv == value ) return; \
\
         } else if( OperI == AST__NE ) { \
            if( *pv != value ) return; \
\
         } else if( OperI == AST__GE ) { \
            if( *pv >= value ) return; \
\
         } else { \
            if( *pv > value ) return; \
         } \
\
/* Move right a pixel. */ \
         pv++; \
         (*iv)++; \
      } \
\
/* Move left a pixel so that *inx == xhi. */ \
      pv--; \
      (*iv)--; \
      (*inx)--; \
\
/* Move up the right hand edge of the box correspionding to the current \
   skin. We stop just below the middle of the right hand edge. */ \
      for( *iny = ylo; *iny < cy; (*iny)++ ) { \
\
/* Test the pixel value, returning if it is valid. This relies on the \
   compiler optimisation to remove the "if" statements for all but the \
   operation appropriate to the function being defined. */ \
         if( OperI == AST__LT ) { \
            if( *pv < value ) return; \
\
         } else if( OperI == AST__LE ) { \
            if( *pv <= value ) return; \
\
         } else if( OperI == AST__EQ ) { \
            if( *pv == value ) return; \
\
         } else if( OperI == AST__NE ) { \
            if( *pv != value ) return; \
\
         } else if( OperI == AST__GE ) { \
            if( *pv >= value ) return; \
\
         } else { \
            if( *pv > value ) return; \
         } \
\
/* Move up a pixel. */ \
         pv += nx; \
         *iv += nx; \
      } \
   } \
\
/* Report an error if no inside pooint could be found. */ \
   if( OperI == AST__LT ) { \
      text = "less than"; \
   } else if( OperI == AST__LE ) { \
      text = "less than or equal to"; \
   } else if( OperI == AST__EQ ) { \
      text = "equal to"; \
   } else if( OperI == AST__NE ) { \
      text = "not equal to"; \
   } else if( OperI == AST__GE ) { \
      text = "greater than or equal to"; \
   } else { \
      text = "greater than"; \
   } \
   astError( AST__NONIN, "astOutline"#X": Could not find a pixel value %s " \
             "%g in the supplied array.", status, text, (double) value ); \
}

/* Define a macro that uses the above macro to to create implementations
   of FindInsidePoint for all operations. */
#define MAKEALL_FINDINSIDEPOINT(X,Xtype) \
MAKE_FINDINSIDEPOINT(X,Xtype,LT,AST__LT) \
MAKE_FINDINSIDEPOINT(X,Xtype,LE,AST__LE) \
MAKE_FINDINSIDEPOINT(X,Xtype,EQ,AST__EQ) \
MAKE_FINDINSIDEPOINT(X,Xtype,GE,AST__GE) \
MAKE_FINDINSIDEPOINT(X,Xtype,GT,AST__GT) \
MAKE_FINDINSIDEPOINT(X,Xtype,NE,AST__NE)

/* Expand the above macro to generate a function for each required
   data type and operation. */
#if HAVE_LONG_DOUBLE     /* Not normally implemented */
MAKEALL_FINDINSIDEPOINT(LD,long double)
#endif
MAKEALL_FINDINSIDEPOINT(D,double)
MAKEALL_FINDINSIDEPOINT(L,long int)
MAKEALL_FINDINSIDEPOINT(UL,unsigned long int)
MAKEALL_FINDINSIDEPOINT(I,int)
MAKEALL_FINDINSIDEPOINT(UI,unsigned int)
MAKEALL_FINDINSIDEPOINT(S,short int)
MAKEALL_FINDINSIDEPOINT(US,unsigned short int)
MAKEALL_FINDINSIDEPOINT(B,signed char)
MAKEALL_FINDINSIDEPOINT(UB,unsigned char)
MAKEALL_FINDINSIDEPOINT(F,float)

/* Undefine the macros. */
#undef MAKE_FINDINSIDEPOINT
#undef MAKEALL_FINDINSIDEPOINT

static void FindMax( Segment *seg, AstFrame *frm, double *x, double *y,
                     int nv, int abs, int *status ){
/*
*  Name:
*     FindMax

*  Purpose:
*     Find the maximum discrepancy between a given line segment and the
*     Polygon being downsized.

*  Type:
*     Private function.

*  Synopsis:
*     #include "polygon.h"
*     void FindMax( Segment *seg, AstFrame *frm, double *x, double *y,
*                   int nv, int abs, int *status )

*  Class Membership:
*     Polygon member function

*  Description:
*     The supplied Segment structure describes a range of vertices in
*     the polygon being downsized. This function checks each of these
*     vertices to find the one that lies furthest from the line joining the
*     first and last vertices in the segment. The maximum error, and the
*     vertex index at which this maximum error is found, is stored in the
*     Segment structure.

*  Parameters:
*     seg
*        The structure describing the range of vertices to check. It
*        corresponds to a candidate edge in the downsized polygon.
*     frm
*        The Frame in which the positions are defined.
*     x
*        Pointer to the X axis values in the original Polygon.
*     y
*        Pointer to the Y axis values in the original Polygon.
*     nv
*        Total number of vertics in the old Polygon..
*     abs
*        If non-zero, then the stored maximum is the position with
*        maximum absolute error. Otherwise, the stored maximum is the
*        position with maximum positive error (positive errors are to the
*        right when travelling from start to end of the segment).
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstPointSet *pset1; /* PointSet holding vertex positions */
   AstPointSet *pset2; /* PointSet holding offset par/perp components */
   double **ptr1;      /* Pointers to "pset1" data arrays */
   double **ptr2;      /* Pointers to "pset2" data arrays */
   double *px;         /* Pointer to next X value */
   double *py;         /* Pointer to next Y value */
   double ax;          /* X value at start */
   double ay;          /* Y value at start */
   double ba;          /* Distance between a and b */
   double bax;         /* X increment from a to b */
   double bay;         /* Y increment from a to b */
   double cax;         /* X increment from a to c */
   double cay;         /* Y increment from a to c */
   double end[ 2 ];    /* Position of starting vertex */
   double error;       /* Error value for current vertex */
   double start[ 2 ];  /* Position of starting vertex */
   int i1;             /* Starting index (always in first cycle) */
   int i2;             /* Ending index converted to first cycle */
   int i2b;            /* Upper vertex limit in first cycle */
   int i;              /* Loop count */
   int n;              /* Number of vertices to check */

/* Check the global error status. */
   if ( !astOK ) return;

/* Stuff needed for handling cyclic redundancy of vertex indices. */
   i1 = seg->i1;
   i2 = seg->i2;
   n = i2 - i1 - 1;
   i2b = i2;
   if( i2 >= nv ) {
      i2 -= nv;
      i2b = nv;
   }

/* If the segment has no intermediate vertices, set the segment error to
   zero and return. */
   if( n < 1 ) {
      seg->error = 0.0;
      seg->imax = i1;

/* For speed, we use simple plane geometry if the Polygon is defined in a
   simple Frame. */
   } else if( !strcmp( astGetClass( frm ), "Frame" ) ) {

/* Point "a" is the vertex that marks the start of the segment. Point "b"
   is the vertex that marks the end of the segment. */
      ax = x[ i1 ];
      ay = y[ i1 ];
      bax = x[ i2 ] - ax;
      bay = y[ i2 ] - ay;
      ba = sqrt( bax*bax + bay*bay );

/* Initialise the largest error found so far. */
      seg->error = -1.0;

/* Check the vertices from the start (plus one) up to the end (minus one)
   or the last vertex (which ever comes first). */
      for( i = i1 + 1; i < i2b; i++ ) {

/* Position "c" is the vertex being tested. Find the squared distance from
   "c" to the line joining "a" and "b". */
         cax = x[ i ] - ax;
         cay = y[ i ] - ay;
         error = ( bay*cax - cay*bax )/ba;
         if( abs ) error = fabs( error );

/* If this is the largest value found so far, record it. Note the error
   here is a squared distance. */
         if( error > seg->error ) {
            seg->error = error;
            seg->imax = i;
         }
      }

/* If the end vertex is in the next cycle, check the remaining vertex
   posI would have thought a telentitions in the same way. */
      if( i2b != i2 ) {

         for( i = 0; i < i2; i++ ) {

            cax = x[ i ] - ax;
            cay = y[ i ] - ay;
            error = ( bay*cax - cay*bax )/ba;
            if( abs ) error = fabs( error );

            if( error > seg->error ) {
               seg->error = error;
               seg->imax = i + i2b;
            }

         }
      }

/* If the polygon is not defined in a simple Frame, we use the overloaded
   Frame methods to do the geometry. */
   } else {

/* Create a PointSet to hold the positions of the vertices to test. We do
   not need to test the start or end vertex. */
      pset1 = astPointSet( n, 2, " ", status );
      ptr1 = astGetPoints( pset1 );
      if( astOK ) {

/* Copy the vertex axis values form the start (plus one) up to the end
   (minus one) vertex or the last vertex (which ever comes first). */
         px = ptr1[ 0 ];
         py = ptr1[ 1 ];

         for( i = i1 + 1; i < i2b; i++ ){
            *(px++) = x[ i ];
            *(py++) = y[ i ];
         }

/* If the end vertex is in the next cycle, copy the remaining vertex
   positions into the PointSet. */
         if( i2b != i2 ) {
            for( i = 0; i < i2; i++ ) {
               *(px++) = x[ i ];
               *(py++) = y[ i ];
            }
         }

/* Record the start and end vertex positions. */
         start[ 0 ] = x[ i1 ];
         start[ 1 ] = y[ i1 ];
         end[ 0 ] = x[ i2 ];
         end[ 1 ] = y[ i2 ];

/* Resolve the vector from the start to each vertex into two components,
   parallel and perpendicular to the start->end vector. */
         pset2 = astResolvePoints( frm, start, end, pset1, NULL );
         ptr2 = astGetPoints( pset2 );
         if( astOK ) {

/* Find the vertex with largest perpendicular component. */
            seg->error = -1.0;
            py = ptr2[ 1 ];
            for( i = 1; i <= n; i++ ) {

               error = *(py++);
               if( abs ) error = fabs( error );

               if( error > seg->error ) {
                  seg->error = error;
                  seg->imax = i + i1;
               }

            }
         }

/* Free resources. */
         pset2 = astAnnul( pset2 );
      }
      pset1 = astAnnul( pset1 );
   }
}

static int GetBounded( AstRegion *this, int *status ) {
/*
*  Name:
*     GetBounded

*  Purpose:
*     Is the Region bounded?

*  Type:
*     Private function.

*  Synopsis:
*     #include "polygon.h"
*     int GetBounded( AstRegion *this, int *status )

*  Class Membership:
*     Polygon method (over-rides the astGetBounded method inherited from
*     the Region class).

*  Description:
*     This function returns a flag indicating if the Region is bounded.
*     The implementation provided by the base Region class is suitable
*     for Region sub-classes representing the inside of a single closed
*     curve (e.g. Circle, Interval, Box, etc). Other sub-classes (such as
*     CmpRegion, PointList, etc ) may need to provide their own
*     implementations.

*  Parameters:
*     this
*        Pointer to the Region.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Non-zero if the Region is bounded. Zero otherwise.

*/

/* Local Variables: */
   int neg;                  /* Has the Polygon been negated? */
   int result;               /* Returned result */

/* Initialise */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Ensure cached information is available. */
   Cache( (AstPolygon *) this, status );

/* See if the Polygon has been negated. */
   neg = astGetNegated( this );

/* If the polygon vertices are stored in anti-clockwise order, then the
   polygon is bounded if it has not been negated. */
   if( ( (AstPolygon *) this)->acw ) {
      result = (! neg );

/* If the polygon vertices are stored in clockwise order, then the
   polygon is bounded if it has been negated. */
   } else {
      result = neg;
   }

/* Return the result. */
   return result;
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
   will be used (by astIsAPolygon) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstRegionVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */
   vtab->Downsize = Downsize;

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
   region->RegTrace = RegTrace;
   region->GetBounded = GetBounded;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */

/* Declare the copy constructor, destructor and class dump
   functions. */
   astSetDump( vtab, Dump, "Polygon", "Polygonal region" );
   astSetDelete( vtab, Delete );
   astSetCopy( vtab, Copy );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
}

static int IntCmp( const void *a, const void *b ){
/*
*  Name:
*     IntCmp

*  Purpose:
*     An integer comparison function for the "qsort" function.

*  Type:
*     Private function.

*  Synopsis:
*     #include "polygon.h"
*     int IntCmp( const void *a, const void *b )

*  Class Membership:
*     Polygon member function

*  Description:
*     See the docs for "qsort".

*  Parameters:
*     a
*        Pointer to the first int
*     b
*        Pointer to the second int

*  Returnd Value:
*     Positive, negative or zero, depending on whether a is larger than,
*     equal to, or less than b.

*/

   return *((int*)a) - *((int*)b);
}

static Segment *NewSegment( Segment *seg, int i1, int i2, int nvert,
                            int *status ){
/*
*  Name:
*     NewSegment

*  Purpose:
*     Initialise a structure describing a segment of the new Polygon
*     created by astDownsize.

*  Type:
*     Private function.

*  Synopsis:
*     #include "polygon.h"
*     Segment *NewSegment( Segment *seg, int i1, int i2, int nvert,
*                          int *status )

*  Class Membership:
*     Polygon member function

*  Description:
*     This function initialises the contents of a structure describing
*     the specified range of vertices within a Polygon. The cyclic nature
*     of vertex indices is taken into account.
*
*     If no structure is supplied, memory is allocated to hold a new
*     structure.

*  Parameters:
*     seg
*        Pointer to a structure to initialise, or NULL if a new structure
*        is to be allocated.
*     i1
*        The index of a vertex within the old Polygon (supplied to
*        astDownsize) that marks the start of the new line segment in
*        the downsized polygon.
*     i2
*        The index of a vertex within the old Polygon (supplied to
*        astDownsize) that marks the end of the new line segment in
*        the downsized polygon.
*     nvert
*        Total number of vertics in the old Polygon..
*     status
*        Pointer to the inherited status variable.

*  Returnd Value:
*     Pointer to the initialised Segment structure. It should be freed using
*     astFree when no longer needed.

*/

/* Local Variables: */
   Segment *result;

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Get a pointer to the structure to be initialised, allocating memory
   for a new structure if none was supplied. */
   result = seg ? seg : astMalloc( sizeof( Segment ) );

/* Check the pointer can be used safely. */
   if( result ) {

/* If the supplied ending index is less than the starting index, the
   ending index must have gone all the way round the polygon and started
   round again. Increase the ending index by the number of vertices to
   put it in the same cycle as the starting index. */
      if( i2 < i1 ) i2 += nvert;

/* If the supplied starting index is within the first cycle (i.e. zero ->
   nvert-1 ), use the indices as they are (which may mean that the ending
   index is greater than nvert, but this is handled correctly by other
   functions). */
      if( i1 < nvert ) {
         result->i1 = i1;
         result->i2 = i2;

/* If the supplied starting index is within the second cycle (i.e. nvert
   or greater) the ending index will be even greater, so we can reduce
   both by "nvert" to put them both in the first cycle. The goal is that
   the starting index should always be in the first cycle, but the ending
   index may possibly be in the second cycle. */
      } else {
         result->i1 = i1 - nvert;
         result->i2 = i2 - nvert;
      }

/* Nullify the links to other Segments */
      result->next = NULL;
      result->prev = NULL;
   }

/* Return the pointer to the new Segment structure. */
   return result;
}

/*
*++
*  Name:
c     astOutline<X>
f     AST_OUTLINE<X>

*  Purpose:
*     Create a new Polygon outling values in a 2D data grid.

*  Type:
*     Public function.

*  Synopsis:
c     #include "polygon.h"
c     AstPolygon *astOutline<X>( <Xtype> value, int oper, const <Xtype> array[],
c                                const int lbnd[2], const int ubnd[2], double maxerr,
c                                int maxvert, const int inside[2], int starpix )
f     RESULT = AST_OUTLINE<X>( VALUE, OPER, ARRAY, LBND, UBND, MAXERR,
f                              MAXVERT, INSIDE, STARPIX, STATUS )

*  Class Membership:
*     Polygon method.

*  Description:
*     This is a set of functions that create a Polygon enclosing a single
*     contiguous set of pixels that have a specified value within a gridded
*     2-dimensional data array (e.g. an image).
*
*     A basic 2-dimensional Frame is used to represent the pixel coordinate
*     system in the returned Polygon. The Domain attribute is set to
*     "PIXEL", the Title attribute is set to "Pixel coordinates", and the
*     Unit attribute for each axis is set to "pixel". All other
*     attributes are left unset. The nature of the pixel coordinate system
*     is determined by parameter
c     "starpix".
f     STARPIX.
*
*     The
c     "maxerr" and "maxvert"
f     MAXERR and MAXVERT
*     parameters can be used to control how accurately the returned
*     Polygon represents the required region in the data array. The
*     number of vertices in the returned Polygon will be the minimum
*     needed to achieve the required accuracy.
*
*     You should use a function which matches the numerical type of the
*     data you are processing by replacing <X> in the generic function
*     name
c     astOutline<X>
f     AST_OUTLINE<X>
c     by an appropriate 1- or 2-character type code. For example, if you
*     are procesing data with type
c     "float", you should use the function astOutlineF
f     REAL, you should use the function AST_OUTLINER
*     (see the "Data Type Codes" section below for the codes appropriate to
*     other numerical types).

*  Parameters:
c     value
f     VALUE = <Xtype> (Given)
*        A data value that specifies the pixels to be outlined.
c     oper
f     OPER = INTEGER (Given)
*        Indicates how the
c        "value"
f        VALUE
*        parameter is used to select the outlined pixels. It can
*        have any of the following values:
c        - AST__LT: outline pixels with value less than "value".
c        - AST__LE: outline pixels with value less than or equal to "value".
c        - AST__EQ: outline pixels with value equal to "value".
c        - AST__NE: outline pixels with value not equal to "value".
c        - AST__GE: outline pixels with value greater than or equal to "value".
c        - AST__GT: outline pixels with value greater than "value".
f        - AST__LT: outline pixels with value less than VALUE.
f        - AST__LE: outline pixels with value less than or equal to VALUE.
f        - AST__EQ: outline pixels with value equal to VALUE.
f        - AST__NE: outline pixels with value not equal to VALUE.
f        - AST__GE: outline pixels with value greater than or equal to VALUE.
f        - AST__GT: outline pixels with value greater than VALUE.
c     array
f     ARRAY( * ) = <Xtype> (Given)
c        Pointer to a
f        A
*        2-dimensional array containing the data to be processed.  The
*        numerical type of this array should match the 1- or
*        2-character type code appended to the function name (e.g. if
c        you are using astOutlineF, the type of each array element
c        should be "float").
f        you are using AST_OUTLINER, the type of each array element
f        should be REAL).
*
*        The storage order of data within this array should be such
*        that the index of the first grid dimension varies most
*        rapidly and that of the second dimension least rapidly
c        (i.e. Fortran array indexing is used).
f        (i.e. normal Fortran array storage order).
c     lbnd
f     LBND( 2 ) = INTEGER (Given)
c        Pointer to an array of two integers
f        An array
*        containing the coordinates of the centre of the first pixel
*        in the input grid along each dimension.
c     ubnd
f     UBND( 2) = INTEGER (Given)
c        Pointer to an array of two integers
f        An array
*        containing the coordinates of the centre of the last pixel in
*        the input grid along each dimension.
*
c        Note that "lbnd" and "ubnd" together define the shape
f        Note that LBND and UBND together define the shape
*        and size of the input grid, its extent along a particular
c        (j'th) dimension being ubnd[j]-lbnd[j]+1 (assuming the
c        index "j" to be zero-based). They also define
f        (J'th) dimension being UBND(J)-LBND(J)+1. They also define
*        the input grid's coordinate system, each pixel having unit
*        extent along each dimension with integral coordinate values
*        at its centre or upper corner, as selected by parameter
c        "starpix".
f        STARPIX.
c     maxerr
f     MAXERR = DOUBLE PRECISION (Given)
*        Together with
c        "maxvert",
f        MAXVERT,
*        this determines how accurately the returned Polygon represents
*        the required region of the data array. It gives the target
*        discrepancy between the returned Polygon and the accurate outline
*        in the data array, expressed as a number of pixels. Insignificant
*        vertices are removed from the accurate outline, one by one, until
*        the number of vertices remaining in the returned Polygon equals
c        "maxvert",
f        MAXVERT,
*        or the largest discrepancy between the accurate outline and the
*        returned Polygon is greater than
c        "maxerr". If "maxerr"
f        MAXERR. If MAXERR
*        is zero or less, its value is ignored and the returned Polygon will
*        have the number of vertices specified by
c        "maxvert".
f        MAXVERT.
c     maxvert
f     MAXVERT = INTEGER (Given)
*        Together with
c        "maxerr",
f        MAXERR,
*        this determines how accurately the returned Polygon represents
*        the required region of the data array. It gives the maximum
*        allowed number of vertices in the returned Polygon. Insignificant
*        vertices are removed from the accurate outline, one by one, until
*        the number of vertices remaining in the returned Polygon equals
c        "maxvert",
f        MAXVERT,
*        or the largest discrepancy between the accurate outline and the
*        returned Polygon is greater than
c        "maxerr". If "maxvert"
f        MAXERR. If MAXVERT
*        is less than 3, its value is ignored and the number of vertices in
*        the returned Polygon will be the minimum needed to ensure that the
*        discrepancy between the accurate outline and the returned
*        Polygon is less than
c        "maxerr".
f        MAXERR.
c     inside
f     INSIDE( 2 ) = INTEGER (Given)
c        Pointer to an array of two integers
f        An array
*        containing the indices of a pixel known to be inside the
*        required region. This is needed because the supplied data
*        array may contain several disjoint areas of pixels that satisfy
*        the criterion specified by
c        "value" and "oper".
f        VALUE and OPER.
*        In such cases, the area described by the returned Polygon will
*        be the one that contains the pixel specified by
c        "inside".
f        INSIDE.
*        If the specified pixel is outside the bounds given by
c        "lbnd" and "ubnd",
f        LBND and UBND,
*        or has a value that does not meet the criterion specified by
c        "value" and "oper",
f        VALUE and OPER,
*        then this function will search for a suitable pixel. The search
*        starts at the central pixel and proceeds in a spiral manner until
*        a pixel is found that meets the specified crierion.
c     starpix
f     STARPIX = LOGICAL (Given)
*        A flag indicating the nature of the pixel coordinate system used
*        to describe the vertex positions in the returned Polygon. If
c        non-zero,
f        .TRUE.,
*        the standard Starlink definition of pixel coordinate is used in
*        which a pixel with integer index I spans a range of pixel coordinate
*        from (I-1) to I (i.e. pixel corners have integral pixel coordinates).
c        If zero,
f        If .FALSE.,
*        the definition of pixel coordinate used by other AST functions
c        such as astResample, astMask,
f        such as AST_RESAMPLE, AST_MASK,
*        etc., is used. In this definition, a pixel with integer index I
*        spans a range of pixel coordinate from (I-0.5) to (I+0.5) (i.e.
*        pixel centres have integral pixel coordinates).
c     boxsize
f     BOXSIZE = INTEGER (Given)
*        The full width in pixels of a smoothing box to be applied to the
*        polygon vertices before downsizing the polygon to a smaller number
*        of vertices. If an even number is supplied, the next larger odd
*        number is used. Values of one or zero result in no smoothing.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astOutline<X>()
f     AST_OUTLINE<X> = INTEGER
*        The number of pixels to which a value of
c        "badval"
f        BADVAL
*        has been assigned.

*  Notes:
*     - This function proceeds by first finding a very accurate polygon,
*     and then removing insignificant vertices from this fine polygon
*     using
c     astDownsize.
f     AST_DOWNSIZE.
*     - The returned Polygon is the outer boundary of the contiguous set
*     of pixels that includes ths specified "inside" point, and satisfy
*     the specified value requirement. This set of pixels may potentially
*     include "holes" where the pixel values fail to meet the specified
*     value requirement. Such holes will be ignored by this function.
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.

*  Data Type Codes:
*     To select the appropriate masking function, you should
c     replace <X> in the generic function name astOutline<X> with a
f     replace <X> in the generic function name AST_OUTLINE<X> with a
*     1- or 2-character data type code, so as to match the numerical
*     type <Xtype> of the data you are processing, as follows:
c     - D: double
c     - F: float
c     - L: long int
c     - UL: unsigned long int
c     - I: int
c     - UI: unsigned int
c     - S: short int
c     - US: unsigned short int
c     - B: byte (signed char)
c     - UB: unsigned byte (unsigned char)
f     - D: DOUBLE PRECISION
f     - R: REAL
f     - I: INTEGER
f     - UI: INTEGER (treated as unsigned)
f     - S: INTEGER*2 (short integer)
f     - US: INTEGER*2 (short integer, treated as unsigned)
f     - B: BYTE (treated as signed)
f     - UB: BYTE (treated as unsigned)
*
c     For example, astOutlineD would be used to process "double"
c     data, while astOutlineS would be used to process "short int"
c     data, etc.
f     For example, AST_OUTLINED would be used to process DOUBLE
f     PRECISION data, while AST_OUTLINES would be used to process
f     short integer data (stored in an INTEGER*2 array), etc.
f
f     For compatibility with other Starlink facilities, the codes W
f     and UW are provided as synonyms for S and US respectively (but
f     only in the Fortran interface to AST).

*--
*/
/* Define a macro to implement the function for a specific data
   type. Note, this function cannot be a virtual function since the
   argument list does not include a Polygon, and so no virtual function
   table is available. */
#define MAKE_OUTLINE(X,Xtype) \
AstPolygon *astOutline##X##_( Xtype value, int oper, const Xtype array[], \
                              const int lbnd[2], const int ubnd[2], double maxerr, \
                              int maxvert, const int inside[2], int starpix, \
                              int *status ) { \
\
/* Local Variables: */ \
   AstFrame *frm;            /* Frame in which to define the Polygon */ \
   AstPointSet *candidate;   /* Candidate polygon vertices */ \
   AstPointSet *pset;        /* PointSet holding downsized polygon vertices */ \
   AstPolygon *result;       /* Result value to return */ \
   const Xtype *pv;          /* Pointer to next test point */ \
   Xtype v;                  /* Value of current pixel */ \
   double **ptr;             /* Pointers to PointSet arrays */ \
   int boxsize;              /* Half width of smoothign box in vertices */ \
   int inx;                  /* X index of inside point */ \
   int iny;                  /* Y index of inside point */ \
   int iv;                   /* Vector index of next test pixel */ \
   int ixv;                  /* X pixel index of next test point */ \
   int nv0;                  /* Number of vertices in accurate outline */ \
   int nx;                   /* Length of array x axis */ \
   int smooth;               /* Do we need to smooth the polygon? */ \
   int stop_at_invalid;      /* Indicates when to stop rightwards search */ \
   int tmp;                  /* Alternative boxsize */ \
   int valid;                /* Does current pixel meet requirements? */ \
   static double junk[ 6 ] = {0.0, 0.0, 1.0, 1.0, 0.0, 1.0 }; /* Junk poly */ \
\
/* Initialise. */ \
   result = NULL; \
\
/* Check the global error status. */ \
   if ( !astOK ) return result; \
\
/* Avoid compiler warnings. */ \
   iv = 0; \
\
/* If we are going to be smoothing the polygon before downsizing it, we \
   need to ensure that the full polygon is retained within TraceEdge. if \
   this is not the case, TraceEdge can remove all vertices from straight \
   lines, except for the vertices that mark the beinning and end of the \
   straight line. */ \
   smooth = ( maxerr > 0.0 || maxvert > 2 ); \
\
/* Store the X dimension of the array. */ \
   nx = ubnd[ 0 ] - lbnd[ 0 ] + 1; \
\
/* See if a valid inside point was supplied. It must be inside the bounds \
   of the array, and must have a pixel value that meets the specified \
   requirement. */ \
   inx = inside[ 0 ]; \
   iny = inside[ 1 ]; \
   valid = ( inx >= lbnd[ 0 ] && inx <= ubnd[ 0 ] && \
             iny >= lbnd[ 1 ] && iny <= ubnd[ 1 ] ); \
\
   if( valid  ) { \
      iv = ( inx - lbnd[ 0 ] ) + (iny - lbnd[ 1 ] )*nx; \
      v = array[ iv ]; \
\
      if( oper == AST__LT ) { \
         valid = ( v < value ); \
\
      } else if( oper == AST__LE ) { \
         valid = ( v <= value ); \
\
      } else if( oper == AST__EQ ) { \
         valid = ( v == value ); \
\
      } else if( oper == AST__NE ) { \
         valid = ( v != value ); \
\
      } else if( oper == AST__GE ) { \
         valid = ( v >= value ); \
\
      } else if( oper == AST__GT ) { \
         valid = ( v > value ); \
\
      } else if( astOK ){ \
         astError( AST__OPRIN, "astOutline"#X": Invalid operation code " \
                   "(%d) supplied (programming error).", status, oper ); \
      } \
   } \
\
/* If no valid inside point was supplied, find one now. */ \
   if( !valid ) { \
\
      if( oper == AST__LT ) { \
         FindInsidePointLT##X( value, array, lbnd, ubnd, &inx, &iny, &iv, status ); \
\
      } else if( oper == AST__LE ) { \
         FindInsidePointLE##X( value, array, lbnd, ubnd, &inx, &iny, &iv, status ); \
\
      } else if( oper == AST__EQ ) { \
         FindInsidePointEQ##X( value, array, lbnd, ubnd, &inx, &iny, &iv, status ); \
\
      } else if( oper == AST__NE ) { \
         FindInsidePointNE##X( value, array, lbnd, ubnd, &inx, &iny, &iv, status ); \
\
      } else if( oper == AST__GE ) { \
         FindInsidePointGE##X( value, array, lbnd, ubnd, &inx, &iny, &iv, status ); \
\
      } else if( oper == AST__GT ) { \
         FindInsidePointGT##X( value, array, lbnd, ubnd, &inx, &iny, &iv, status ); \
\
      } else if( astOK ){ \
         astError( AST__OPRIN, "astOutline"#X": Invalid operation code " \
                   "(%d) supplied (programming error).", status, oper ); \
      } \
   } \
\
/* We now need to find a point on the boundary of the region containing \
   the inside point. Starting at the inside point, move to the right \
   through the array until a pixel is found which fails to meet the value \
   requirement or the edge of the array is reached. */ \
\
   candidate = NULL; \
   pv = array + iv; \
   ixv = inx; \
   stop_at_invalid = 1; \
\
   while( ++ixv <= ubnd[ 0 ] ) { \
\
/* Get the next pixel value. */ \
      v = *(++pv); \
\
/* See if it meets the value requirement. */ \
      if( oper == AST__LT ) { \
         valid = ( v < value ); \
\
      } else if( oper == AST__LE ) { \
         valid = ( v <= value ); \
\
      } else if( oper == AST__EQ ) { \
         valid = ( v == value ); \
\
      } else if( oper == AST__NE ) { \
         valid = ( v != value ); \
\
      } else if( oper == AST__GE ) { \
         valid = ( v >= value ); \
\
      } else if( oper == AST__GT ) { \
         valid = ( v > value ); \
\
      } else if( astOK ){ \
         astError( AST__OPRIN, "astOutline"#X": Invalid operation code " \
                   "(%d) supplied (programming error).", status, oper ); \
         break; \
      } \
\
/* If we are currently looking for the next invalid pixel, and this pixel \
   is invalid... */ \
      if( stop_at_invalid ) { \
         if( ! valid ) { \
\
/* The current pixel may be on the required polygon, or it may be on the  \
   edge of a hole contained within the region being outlined. We would  \
   like to jump over such holes so that we can continue to look for the  \
   real edge of the region being outlined. In order to determine if we  \
   have reached a hole, we trace the edge that passes through the current  \
   pixel, forming a candidate polygon in the process. In the process, We  \
   see if the inside point falls within this candidate polygon. If it does  \
   then the polygon is accepted as the required polygon. Otherwise, it is  \
   rejected as a mere hole, and we continue moving away from the inside  \
   point, looking for a new edge. */ \
            if( oper == AST__LT ) { \
               candidate = TraceEdgeLT##X( value, array, lbnd, ubnd, iv - 1, \
                                           ixv - 1, iny, starpix, smooth, status ); \
\
            } else if( oper == AST__LE ) { \
               candidate = TraceEdgeLE##X( value, array, lbnd, ubnd, iv - 1, \
                                           ixv - 1, iny, starpix, smooth, status ); \
\
            } else if( oper == AST__EQ ) { \
               candidate = TraceEdgeEQ##X( value, array, lbnd, ubnd, iv - 1, \
                                           ixv - 1, iny, starpix, smooth, status ); \
\
            } else if( oper == AST__NE ) { \
               candidate = TraceEdgeNE##X( value, array, lbnd, ubnd, iv - 1, \
                                           ixv - 1, iny, starpix, smooth, status ); \
\
            } else if( oper == AST__GE ) { \
               candidate = TraceEdgeGE##X( value, array, lbnd, ubnd, iv - 1, \
                                           ixv - 1, iny, starpix, smooth, status ); \
\
            } else if( oper == AST__GT ) { \
               candidate = TraceEdgeGT##X( value, array, lbnd, ubnd, iv - 1, \
                                           ixv - 1, iny, starpix, smooth, status ); \
\
            } else if( astOK ){ \
               astError( AST__OPRIN, "astOutline"#X": Invalid operation code " \
                         "(%d) supplied (programming error).", status, oper ); \
            } \
\
/* If the candidate polygon is the required polygon, break out of the \
   loop. Otherwise, indicate that we want to continue moving right, \
   across the hole, until we reach the far side of the hole (i.e. find   \
   the next valid pixel). */ \
            if( candidate ) { \
               break; \
            } else { \
               stop_at_invalid = 0; \
            } \
         } \
\
/* If we are currently looking for the next valid pixel, and the current \
   pixel is valid... */ \
      } else if( valid ) { \
\
/* We have reached the far side of a hole. Continue moving right, looking \
   now for the next invalid pixel (which may be on the required polygon). */ \
         stop_at_invalid = 1; \
      } \
   } \
\
/* If we have not yet found the required polygon, we must have reached \
   the right hand edge of the array. So we now follow the edge of the \
   array round until we meet the boundary of the required region. */ \
   if( !candidate ) { \
      if( oper == AST__LT ) { \
         candidate = TraceEdgeLT##X( value, array, lbnd, ubnd, iv - 1, \
                                     ixv - 1, iny, starpix, smooth, status ); \
\
      } else if( oper == AST__LE ) { \
         candidate = TraceEdgeLE##X( value, array, lbnd, ubnd, iv - 1, \
                                     ixv - 1, iny, starpix, smooth, status ); \
\
      } else if( oper == AST__EQ ) { \
         candidate = TraceEdgeEQ##X( value, array, lbnd, ubnd, iv - 1, \
                                     ixv - 1, iny, starpix, smooth, status ); \
\
      } else if( oper == AST__NE ) { \
         candidate = TraceEdgeNE##X( value, array, lbnd, ubnd, iv - 1, \
                                     ixv - 1, iny, starpix, smooth, status ); \
\
      } else if( oper == AST__GE ) { \
         candidate = TraceEdgeGE##X( value, array, lbnd, ubnd, iv - 1, \
                                     ixv - 1, iny, starpix, smooth, status ); \
\
      } else if( oper == AST__GT ) { \
         candidate = TraceEdgeGT##X( value, array, lbnd, ubnd, iv - 1, \
                                     ixv - 1, iny, starpix, smooth, status ); \
\
      } else if( astOK ){ \
         astError( AST__OPRIN, "astOutline"#X": Invalid operation code " \
                   "(%d) supplied (programming error).", status, oper ); \
      } \
   } \
\
/* If required smooth the full resolution polygon before downsizing it. */ \
   if( smooth ) { \
\
/* Initially, set the boxsize to be equal to the required accouracy. */ \
      if( maxerr > 0 ) { \
         boxsize = (int) maxerr; \
      } else { \
         boxsize = INT_MAX; \
      } \
\
/* Determine a second box size equal to the average number of vertices in \
   the accurate outline, per vertex in the returned Polygon. */ \
      nv0 = astGetNpoint( candidate ); \
      if( maxvert > 2 ) { \
         tmp = nv0/(2*maxvert); \
      } else { \
         tmp = INT_MAX; \
      } \
\
/* Use the minimum of the two box sizes. */ \
      if( tmp < boxsize ) boxsize = tmp; \
\
/* Ensure the box is sufficiently small to allow at least 10 full boxes \
   (=20 half boxes) around the polygon. */ \
      tmp = nv0/20; \
      if( tmp < boxsize ) boxsize = tmp; \
      if( boxsize == 0 ) boxsize = 1; \
\
/* Smooth the polygon. */ \
      SmoothPoly( candidate, boxsize, 1.0, status ); \
   } \
\
/* Reduce the number of vertices in the outline. */ \
   frm = astFrame( 2, "Domain=PIXEL,Unit(1)=pixel,Unit(2)=pixel," \
                   "Title=Pixel coordinates", status ); \
   pset = DownsizePoly( candidate, maxerr, maxvert, frm, status ); \
\
/* Create a default Polygon with 3 junk vertices. */ \
   result = astPolygon( frm, 3, 3, junk, NULL, " ", status ); \
\
/* Change the PointSet within the Polygon to the one created above. */ \
   SetPointSet( result, pset, status ); \
\
/* Free resources. Note, we need to free the arrays within the candidate \
   PointSet explicitly, since they were not created as part of the \
   construction of the PointSet (see TraceEdge). */ \
   pset = astAnnul( pset ); \
   frm = astAnnul( frm ); \
   ptr = astGetPoints( candidate ); \
   if( astOK ) { \
      astFree( ptr[ 0 ] ); \
      astFree( ptr[ 1 ] ); \
   } \
   candidate = astAnnul( candidate ); \
\
/* If an error occurred, clear the returned result. */ \
   if ( !astOK ) result = astAnnul( result ); \
\
/* Return the result. */ \
   return result; \
}


/* Expand the above macro to generate a function for each required
   data type. */
#if HAVE_LONG_DOUBLE     /* Not normally implemented */
MAKE_OUTLINE(LD,long double)
#endif
MAKE_OUTLINE(D,double)
MAKE_OUTLINE(L,long int)
MAKE_OUTLINE(UL,unsigned long int)
MAKE_OUTLINE(I,int)
MAKE_OUTLINE(UI,unsigned int)
MAKE_OUTLINE(S,short int)
MAKE_OUTLINE(US,unsigned short int)
MAKE_OUTLINE(B,signed char)
MAKE_OUTLINE(UB,unsigned char)
MAKE_OUTLINE(F,float)

/* Undefine the macros. */
#undef MAKE_OUTLINE

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
*     The width of the polygon perpendicular to the given edge, or
*     AST__BAD if the width cannot be determined (usually because the
*     vertices been supplied in a clockwise direction, effectively
*     negating the Polygon).

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
   much too close (i.e. if it goes all the way round the great circle
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
   edge. Skip to the next edge if the line does not intersect the edge
   within the length of the edge. */
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

/* The usual reason for not finding a width is if the polygon vertices
   are supplied in clockwise order, effectively negating the polygon, and
   resulting in the "inside" of the polygon being the infinite region
   outside a polygonal hole. In this case, the end point of the line
   perpendicular to the initial edge can be returned as a representative
   "inside" point. */
   } else {
      cen[ 0 ] = end[ 0 ];
      cen[ 1 ] = end[ 1 ];
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
   double dist;               /* Offset along an axis */
   double x0;                 /* The first X axis value */
   double y0;                 /* The first Y axis value */
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

/* Get a pointer to the base Frame in the frameset encapsulated by the
   parent Region structure. */
      frm = astGetFrame( this_region->frameset, AST__BASE );

/* Find the upper and lower bounds of the box enclosing all the vertices.
   The box is expressed in terms of axis offsets from the first vertex, in
   order to avoid problems with boxes that cross RA=0 or RA=12h */
      lbnd[ 0 ] = 0.0;
      lbnd[ 1 ] = 0.0;
      ubnd[ 0 ] = 0.0;
      ubnd[ 1 ] = 0.0;

      x = ptr[ 0 ];
      y = ptr[ 1 ];

      x0 = *x;
      y0 = *y;

      for( ip = 0; ip < np; ip++, x++, y++ ) {

         dist =  astAxDistance( frm, 1, x0, *x );
         if( dist < lbnd[ 0 ] ) {
            lbnd[ 0 ] = dist;
         } else if( dist > ubnd[ 0 ] ) {
            ubnd[ 0 ] = dist;
         }

         dist =  astAxDistance( frm, 2, y0, *y );
         if( dist < lbnd[ 1 ] ) {
            lbnd[ 1 ] = dist;
         } else if( dist > ubnd[ 1 ] ) {
            ubnd[ 1 ] = dist;
         }

      }

/* Convert the box bounds to absolute values, rather than values relative
   to the first vertex. */
      lbnd[ 0 ] += x0;
      lbnd[ 1 ] += y0;
      ubnd[ 0 ] += x0;
      ubnd[ 1 ] += y0;

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
   astGetRegionBounds( tunc, lbnd_tunc, ubnd_tunc );

/* Find the geodesic length within the base Frame of "this" of the diagonal of
   the bounding box. */
   frm = astGetFrame( this_region->frameset, AST__BASE );
   l1 = astDistance( frm, lbnd_tunc, ubnd_tunc );

/* Also get the Region which defines the uncertainty of the supplied points
   and get its bounding box in the same Frame. */
   if( unc ) {
      astGetRegionBounds( unc, lbnd_unc, ubnd_unc );

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

static int RegTrace( AstRegion *this_region, int n, double *dist, double **ptr,
                     int *status ){
/*
*+
*  Name:
*     RegTrace

*  Purpose:
*     Return requested positions on the boundary of a 2D Region.

*  Type:
*     Private function.

*  Synopsis:
*     #include "polygon.h"
*     int astTraceRegion( AstRegion *this, int n, double *dist, double **ptr );

*  Class Membership:
*     Polygon member function (overrides the astTraceRegion method
*     inherited from the parent Region class).

*  Description:
*     This function returns positions on the boundary of the supplied
*     Region, if possible. The required positions are indicated by a
*     supplied list of scalar parameter values in the range zero to one.
*     Zero corresponds to some arbitrary starting point on the boundary,
*     and one corresponds to the end (which for a closed region will be
*     the same place as the start).

*  Parameters:
*     this
*        Pointer to the Region.
*     n
*        The number of positions to return. If this is zero, the function
*        returns without action (but the returned function value still
*        indicates if the method is supported or not).
*     dist
*        Pointer to an array of "n" scalar parameter values in the range
*        0 to 1.0.
*     ptr
*        A pointer to an array of pointers. The number of elements in
*        this array should equal tthe number of axes in the Frame spanned
*        by the Region. Each element of the array should be a pointer to
*        an array of "n" doubles, in which to return the "n" values for
*        the corresponding axis. The contents of the arrays are unchanged
*        if the supplied Region belongs to a class that does not
*        implement this method.

*  Returned Value:
*     Non-zero if the astTraceRegion method is implemented by the class
*     of Region supplied, and zero if not.

*-
*/

/* Local Variables; */
   AstFrame *frm;
   AstMapping *map;
   AstPointSet *bpset;
   AstPointSet *cpset;
   AstPolygon *this;
   double **bptr;
   double d;
   double p[ 2 ];
   int i;
   int j0;
   int j;
   int ncur;
   int nv;
   int monotonic;

/* Check inherited status, and the number of points to return, returning
   a non-zero value to indicate that this class supports the astRegTrace
   method. */
   if( ! astOK || n == 0 ) return 1;

/* Get a pointer to the Polygon structure. */
   this = (AstPolygon *) this_region;

/* Ensure cached information is available. */
   Cache( this, status );

/* Get a pointer to the base Frame in the encapsulated FrameSet. */
   frm = astGetFrame( this_region->frameset, AST__BASE );

/* We first determine the required positions in the base Frame of the
   Region, and then transform them into the current Frame. Get the
   base->current Mapping, and the number of current Frame axes. */
   map = astGetMapping( this_region->frameset, AST__BASE, AST__CURRENT );

/* If it's a UnitMap we do not need to do the transformation, so put the
   base Frame positions directly into the supplied arrays. */
   if( astIsAUnitMap( map ) ) {
      bpset = NULL;
      bptr = ptr;
      ncur = 2;

/* Otherwise, create a PointSet to hold the base Frame positions (known
   to be 2D since this is an polygon). */
   } else {
      bpset = astPointSet( n, 2, " ", status );
      bptr = astGetPoints( bpset );
      ncur = astGetNout( map );
   }

/* Check the pointers can be used safely. */
   if( astOK ) {

/* Get the number of vertices. */
      nv = astGetNpoint( this_region->points );

/* If we have a reasonable number of pointsand there are a reasonable
   number of vertices, we can be quicker if we know if the parameteric
   distances are monotonic increasing. Find out now. */
      if( n > 5 && nv > 5 ) {

         monotonic = 1;
         for( i = 1; i < n; i++ ) {
            if( dist[ i ] < dist[ i - 1 ] ) {
               monotonic = 0;
               break;
            }
         }

      } else {
         monotonic = 0;
      }

/* Loop round each point. */
      j0 = 1;
      for( i = 0; i < n; i++ ) {

/* Get the required round the polygon, starting from vertex zero. */
         d = dist[ i ]*this->totlen;

/* Loop round each vertex until we find one which is beyond the required
   point. If the supplied distances are monotonic increasing, we can
   start the checks at the same vertex that was used for the previous
   since we know there will never be a backward step. */
         for( j = j0; j < nv; j++ ) {
            if( this->startsat[ j ] > d ) break;
         }

/* If the distances are monotonic increasing, record the vertex that we
   have reached so far. */
         if( monotonic ) j0 = j;

/* Find the distance to travel beyond the previous vertex. */
         d -= this->startsat[ j - 1 ];

/* Find the position, that is the required distance from the previous
   vertex towards the next vertex. */
         astLineOffset( frm, this->edges[ j - 1 ], d, 0.0, p );

/* Store the resulting axis values. */
         bptr[ 0 ][ i ] = p[ 0 ];
         bptr[ 1 ][ i ] = p[ 1 ];
      }
   }

/* If required, transform the base frame positions into the current
   Frame, storing them in the supplied array. Then free resources. */
   if( bpset ) {
      cpset = astPointSet( n, ncur, " ", status );
      astSetPoints( cpset, ptr );

      (void) astTransform( map, bpset, 1, cpset );

      cpset = astAnnul( cpset );
      bpset = astAnnul( bpset );
   }

/* Free remaining resources. */
   map = astAnnul( map );
   frm = astAnnul( frm );

/* Return a non-zero value to indicate that this class supports the
   astRegTrace method. */
   return 1;
}

static Segment *RemoveFromChain( Segment *head, Segment *seg, int *status ){
/*
*  Name:
*     RemoveFromChain

*  Purpose:
*     Remove a Segment from the link list maintained by astDownsize.

*  Type:
*     Private function.

*  Synopsis:
*     #include "polygon.h"
*     Segment *RemoveFromChain( Segment *head, Segment *seg, int *status )

*  Class Membership:
*     Polygon member function

*  Description:
*     The supplied Segment is removed form the list, and the gap is
*     closed up.

*  Parameters:
*     head
*        The Segment structure at the head of the list.
*     seg
*        The Segment to be removed from the list.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the link head (which will have changed if "seg" was the
*     original head).

*/

/* Check the global error status. */
   if ( !astOK ) return head;

/* If the Segment was the original head, make the next segment the new
   head. */
   if( head == seg ) head = seg->next;

/* Close up the links between the Segments on either side of the segment
   being removed. */
   if( seg->prev ) seg->prev->next = seg->next;
   if( seg->next ) seg->next->prev = seg->prev;

/* Nullify the links in the segment being removed. */
   seg->next = NULL;
   seg->prev = NULL;

/* Return the new head. */
   return head;
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

static void SetPointSet( AstPolygon *this, AstPointSet *pset, int *status ){
/*
*  Name:
*     SetPointSet

*  Purpose:
*     Store a new set of vertices in an existing Polygon.

*  Type:
*     Private function.

*  Synopsis:
*     #include "polygon.h"
*     void SetPointSet( AstPolygon *this, AstPointSet *pset, int *status )

*  Class Membership:
*     Polygon member function

*  Description:
*     The PointSet in the supplied Polygon is annulled, are stored in the supplied
*     Polygon, replacing the existing data pointers.

*  Parameters:
*     this
*        Pointer to the Polygon to be changed.
*     pset
*        The PointSet containing the enw vertex information.
*     status
*        Pointer to the inherited status variable.

*/


/* Check the global error status. */
   if ( !astOK ) return;

/* Annul the pointer to the PointSet already in the supplied Polygon. */
   (void) astAnnul( ((AstRegion *) this)->points );

/* Store a clone of the supplie dnew PointSet pointer. */
   ((AstRegion *) this)->points = astClone( pset );

/* Indicate the cached information in the polygon will need to be
   re-calculated when needed. */
   astResetCache( this );
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
   If the supplied Region had no uncertainty, ensure the returned Region
   has no uncertainty. Otherwise, return a clone of the supplied pointer. */
   if( simpler ){
      astRegOverlay( new, this, 1 );
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

static void SmoothPoly( AstPointSet *pset, int boxsize, double strength,
                        int *status ) {
/*
*  Name:
*     SmoothPoly

*  Purpose:
*     Smoooth a polygon assuming plane geometry.

*  Type:
*     Private function.

*  Synopsis:
*     #include "polygon.h"
*     void SmoothPoly( AstPointSet *pset, int boxsize, double strength,
*                      int *status )

*  Class Membership:
*     Polygon member function

*  Description:
*     This function smooths a polygon, without changing the number of
*     vertices. It assumes plane geometry, so should not be used to
*     smooth polygons defined within a SkyFrame or CmpFrame.
*
*     Each vertex is replaced by a new vertex determined as follows: the
*     mean X and Y axis value of the vertices in a section of the polygon
*     centred on the vertex being replaced are found (the length of the
*     section is given by parameter "boxsize"). The new vertex position
*     is then the weighted mean of this mean (X,Y) position and the old
*     vertex position. The weight for the mean (X,Y) position is given
*     by parameter "strength", and the weight for the old vertex
*     position is (1.0 - strength)

*  Parameters:
*     pset
*        A PointSet holding the polygon vertices.
*     boxsize
*        Half width of the box filter, given as a number of vertices.
*     strength
*        The weight to use for the mean (X,Y) position when finding each
*        new vertex position. Should be in the range 0.0 to 1.0. A value
*        of zero results in no change being made to the polygon. A value
*        of 1.0 results in the returned polygon being fully smoothed.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   double **ptr;
   double *newx;
   double *newy;
   double *nx;
   double *ny;
   double *oldx;
   double *oldy;
   double *ox;
   double *oy;
   double *px;
   double *py;
   double *qx;
   double *qy;
   double a;
   double b;
   double sx;
   double sy;
   int half_width;
   int i;
   int nv;
   int top;

/* Check the global error status. */
   if ( !astOK ) return;

/* Get the number of vertices. */
   nv = astGetNpoint( pset );

/* Get pointers to arrays holding the supplied vertex positions. */
   ptr = astGetPoints( pset );
   oldx = ptr[ 0 ];
   oldy = ptr[ 1 ];

/* Allocate arrays to hold the returned vertex positions. */
   newx = astMalloc( nv*sizeof( double ) );
   newy = astMalloc( nv*sizeof( double ) );

/* Check these pointers can be used safely. */
   if( astOK ) {

/* Get weighting factors for the fully smoothed and unsmoothed positions. */
      a = strength;
      b = 1.0 - a;

/* Ensure the box size is sufficiently small for there to be room for
   two boxes along the polygon. */
      half_width = nv/4 - 1;
      if( boxsize < half_width ) half_width = boxsize;
      if( half_width < 1 ) half_width = 1;

/* Modify the weight for the fully smoothed position to include the
   normalisation factor needed to account for the box width. */
      a /= 2*half_width + 1;

/* Find the sum of the x and y coordinates within a box centred on the
   first vertex. This includes vertices from the end of the polygon. */
      px = oldx + 1;
      qx = oldx + nv;
      sx = (oldx)[ 0 ];

      py = oldy + 1;
      qy = oldy + nv;
      sy = (oldy)[ 0 ];

      for( i = 0; i < half_width; i++ ) {
         sx += *(px++) + *(--qx);
         sy += *(py++) + *(--qy);
      }

/* Replacing vertices within the first half box will include vertices at
   both ends of the polygon. Set up the pointers accordingly, and then
   find replacements for each vertex in the first half box.*/
      ox = oldx;
      oy = oldy;
      nx = newx;
      ny = newy;
      for( i = 0; i < half_width; i++ ) {

/* Form the new vertex (x,y) values as the weighted mean of the mean
   (x,y) values in the box, and the old (x,y) values. */
         *(nx++) = a*sx + b*( *(ox++) );
         *(ny++) = a*sy + b*( *(oy++) );

/* Add in the next vertex X and Y axis values to the running sums, and
   remove the position that has now passed out of the box. */
         sx += *(px++) - *(qx++);
         sy += *(py++) - *(qy++);
      }

/* Adjust the pointer for the rest of the polygon, up to one half box away
   from the end. In this section, the smoothing box does not touch either
   end of the polygon. */
      top = nv - half_width - 1;
      qx = oldx;
      qy = oldy;
      for( ; i < top; i++ ){

/* Form the new vertex (x,y) values as the weighted mean of the mean
   (x,y) values in the box, and the old (x,y) values. */
         *(nx++) = a*sx + b*( *(ox++) );
         *(ny++) = a*sy + b*( *(oy++) );

/* Add in the next vertex X and Y axis values to the running sums, and
   remove the position that has now passed out of the box. */
         sx += *(px++) - *(qx++);
         sy += *(py++) - *(qy++);
      }

/* Now do the last half box (which includes vertices from the start of
   the polygon). */
      top = nv;
      px = oldx;
      py = oldy;
      for( ; i < top; i++ ){
         *(nx++) = a*sx + b*( *(ox++) );
         *(ny++) = a*sy + b*( *(oy++) );
         sx += *(px++) - *(qx++);
         sy += *(py++) - *(qy++);
      }

/* Replace the data points in the PointSet. */
      ptr[ 0 ] = newx;
      ptr[ 1 ] = newy;
      oldx = astFree( oldx );
      oldy = astFree( oldy );
   }
}

/*
*  Name:
*     TraceEdge

*  Purpose:
*     Find a point that is inside the required outline.

*  Type:
*     Private function.

*  Synopsis:
*     #include "polygon.h"
*     void TraceEdge<Oper><X>( <Xtype> value, const <Xtype> array[],
*                              const int lbnd[ 2 ], const int ubnd[ 2 ],
*                              int iv0, int ix0, int iy0, int starpix,
*                              int full, int *status );

*  Class Membership:
*     Polygon member function

*  Description:
*     This function forms a polygon enclosing the region of the data
*     array specified by <Oper> and "value". If this polygon contains
*     the point "(inx,iny)", then a PointSet is returned holding the
*     pixel coordinates of the Polygon vertices. If the polygon
*     does not contain "(inx,iny)", a NULL pointer is returned.
*
*     Each vertex in the polygon corresponds to a corner of a pixel in
*     the data array.

*  Parameters:
*     value
*        The data value defining valid pixels.
*     array
*        The data array.
*     lbnd
*        The lower pixel index bounds of the array.
*     ubnd
*        The upper pixel index bounds of the array.
*     iv0
*        The vector index of a pixel inside the region such that the
*        pixel to the right is NOT inside the region. This defines the
*        start of the polygon.
*     ix0
*        The X pixel index of the pixel specified by "iv0".
*     inx
*        The X pixel index of a point which must be inside the polygon
*        for the polygon to be acceptable.
*     iny
*        The Y pixel index of a point which must be inside the polygon
*        for the polygon to be acceptable.
*     starpix
*        If non-zero, the usual Starlink definition of pixel coordinate
*        is used (integral values at pixel corners). Otherwise, the
*        system used by other AST functions such as astResample is used
*        (integral values at pixel centres).
*     full
*        If non-zero, the full polygon is stored. If zero, vertices in the
*        middle of straight sections of the Polygon are omitted.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a PointSet holding the vertices of the polygon, or
*     NULL if the polygon did not contain "(inx,iny)".

*  Notes:
*     - <Oper> must be one of LT, LE, EQ, GE, GT, NE.


*/

/* Define a macro to implement the function for a specific data
   type and operation. */
#define MAKE_TRACEEDGE(X,Xtype,Oper,OperI) \
static AstPointSet *TraceEdge##Oper##X( Xtype value, const Xtype array[], \
                                        const int lbnd[ 2 ], const int ubnd[ 2 ], \
                                        int iv0, int ix0, int iy0, \
                                        int starpix, int full, \
                                        int *status ){ \
\
/* Local Variables: */ \
   AstPointSet *result; /* Pointer to text describing oper */ \
   const Xtype *pa;     /* Pointer to current valid pixel value */ \
   const Xtype *pb;     /* Pointer to neigbouring valid pixel value */ \
   const Xtype *pc;     /* Pointer to neigbouring valid pixel value */ \
   double *ptr[ 2 ];    /* PointSet data pointers */ \
   double *xvert;       /* Pointer to array holding vertex X axis values */ \
   double *yvert;       /* Pointer to array holding vertex Y axis values */ \
   double xx;           /* Pixel X coord at corner */ \
   double yy;           /* Pixel Y coord at corner */ \
   int at;              /* The pixel corner to draw to */ \
   int done;            /* Have we arrived back at the start of the polygon? */ \
   int ii;              /* Index of new vertex */ \
   int ix;              /* X pixel index of current valid pixel */ \
   int iy;              /* Y pixel index of current valid pixel */ \
   int nright;          /* Overall number of right hand turns along polygon */ \
   int nvert;           /* Number of vertices */ \
   int nx;              /* Pixels per row */ \
\
/* Initialise */ \
   result = NULL; \
\
/* Check the global error status. */ \
   if ( !astOK ) return result; \
\
\
/* Initialise pointers to arrays holding the X and Y pixel coordinates at \
   the vertices of the polygon. */ \
   xvert = NULL; \
   yvert = NULL; \
   nvert = 0; \
\
/* Find number of pixels in one row of the array. */ \
   nx = ( ubnd[ 0 ] - lbnd[ 0 ] + 1 ); \
\
/* The four corners of a pixel are numbered as follows: 0=bottom left, \
   1=top left, 2=top right, 3=bottom right. The following algorithm moves \
   along pixel edges, from corner to corner, using the above numbering \
   scheme to identify the corners. We start the polygon by moving from the \
   bottom right to the top right corner of pixel "(ix0,iy0)". */ \
   ix = ix0; \
   iy = iy0; \
   at = 2; \
\
/* Store a pointer to the first good pixel value. */ \
   pa = array + ( ix - lbnd[ 0 ] ) + nx*( iy - lbnd[ 1 ] ) ; \
\
/* We count the number of times the polygon turns to the right compared \
   to the left. Initialise it to zero. */ \
   nright = 0; \
\
/* Loop round tracing out the polygon until we arrive back at the \
   beginning. The Polygon class requires that the inside of the polygon \
   is to the left as we move round the polygon in an anti-clockwise \
   direction. So at each corner, we attempt to move to the next \
   anti-clockwise good pixel corner. */ \
   done = 0; \
   while( !done ) { \
\
/* If we have arrived at the bottom left corner of the good pixel, we must \
   have come from the top left corner since all movements around a pixel \
   must be anti-clockwise. */ \
      if( at == 0 ) { \
\
/* Note the pixel coordinates at the bottom left corner of the current \
   pixel. */ \
         if( starpix ) { \
            xx = ix - 1.0; \
            yy = iy - 1.0; \
         } else { \
            xx = ix - 0.5; \
            yy = iy - 0.5; \
         } \
\
/* Get a pointer to lower left pixel value */ \
         pb = pa - nx - 1; \
\
/* Get a pointer to lower mid pixel value. */ \
         pc = pb + 1; \
\
/* If the lower left pixel is within the array and meets the validity \
   requirements, move to the left along its top edge. */ \
         if( iy > lbnd[ 1 ] && ix > lbnd[ 0 ] && ISVALID(*pb,OperI,value) ) { \
            nright++; \
            pa = pb; \
            at = 1; \
            ix--; \
            iy--; \
\
/* Otherwise, if lower mid pixel is good, move down its left edge. */ \
         } else if( iy > lbnd[ 1 ] && ISVALID(*pc,OperI,value) ) { \
            pa = pc; \
            at = 0; \
            iy--; \
\
/* Otherwise, move to the right along the bottom edge of the current pixel. */ \
         } else { \
            nright--; \
            at = 3; \
         } \
\
/* If the polygon bends at this point, or if we will be smoothing the \
   polygon, append the pixel coordinates at this pixel corner to the \
   polygon. */ \
         if( full || pa != pc ) ADD( xx, yy ); \
\
/* If we have arrived at the top left corner of the good pixel, we must \
   have come from the top right corner. */ \
      } else if( at == 1 ) { \
\
/* Note the pixel coordinates at the top left corner of the current \
   pixel. */ \
         if( starpix ) { \
            xx = ix - 1.0; \
            yy = iy; \
         } else { \
            xx = ix - 0.5; \
            yy = iy + 0.5; \
         } \
\
/* Get a pointer to upper left pixel value */ \
         pb = pa + nx - 1; \
\
/* Get a pointer to mid left pixel value. */ \
         pc = pa - 1; \
\
/* If upper left pixel is good, move up its left edge. */ \
         if( iy < ubnd[ 1 ] && ix > lbnd[ 0 ] && ISVALID(*pb,OperI,value) ) { \
            nright++; \
            pa = pb; \
            at = 2; \
            ix--; \
            iy++; \
\
/* Otherwise, if left mid pixel is good, move left along its top edge. */ \
         } else if( ix > lbnd[ 0 ] && ISVALID(*pc,OperI,value) ) { \
            pa = pc; \
            at = 1; \
            ix--; \
\
/* Otherwise, move down the left edge of the current pixel. */ \
         } else { \
            nright--; \
            at = 0; \
         } \
\
/* If the polygon bends at this point, or if we will be smoothing the \
   polygon, append the pixel coordinates at this pixel corner to the \
   polygon. */ \
         if( full || pa != pc ) ADD( xx, yy ); \
\
/* If we have arrived at the top right corner of the good pixel, we must \
   have come from the bottom right corner. */ \
      } else if( at == 2 ) { \
\
/* Note the pixel coordinates at the top right corner of the current \
   pixel. */ \
         if( starpix ) { \
            xx = ix; \
            yy = iy; \
         } else { \
            xx = ix + 0.5; \
            yy = iy + 0.5; \
         } \
\
/* Pointer to upper right pixel value */ \
         pb = pa + nx + 1; \
\
/* Pointer to top mid pixel value. */ \
         pc = pa + nx; \
\
/* If upper right pixel is good, move right along its bottom edge. */ \
         if( iy < ubnd[ 1 ] && ix < ubnd[ 0 ] && ISVALID(*pb,OperI,value) ){ \
            nright++; \
            pa = pb; \
            at = 3; \
            ix++; \
            iy++; \
\
/* Otherwise, if upper mid pixel is good, move up its right edge. */ \
         } else if( iy < ubnd[ 1 ] && ISVALID(*pc,OperI,value) ) { \
            pa = pc; \
            at = 2; \
            iy++; \
\
/* Otherwise, move left along the top edge of the current pixel. */ \
         } else { \
            nright--; \
            at = 1; \
         } \
\
/* If the polygon bends at this point, or if we will be smoothing the \
   polygon, append the pixel coordinates at this pixel corner to the \
   polygon. */ \
         if( full || pa != pc ) ADD( xx, yy ); \
\
/* Arrived at bottom right corner of good pixel from lower left. */ \
      } else { \
\
/* Note the pixel coordinates at the bottom right corner of the current \
   pixel. */ \
         if( starpix ) { \
            xx = ix; \
            yy = iy - 1.0; \
         } else { \
            xx = ix + 0.5; \
            yy = iy - 0.5; \
         } \
\
/* Pointer to lower right pixel value */ \
         pb = pa - ( nx - 1 ); \
\
/* Pointer to mid right pixel value. */ \
         pc = pa + 1; \
\
/* If lower right pixel is good, move down its left edge. */ \
         if( iy > lbnd[ 1 ] && ix < ubnd[ 0 ] && ISVALID(*pb,OperI,value) ) { \
            nright++; \
            pa = pb; \
            at = 0; \
            ix++; \
            iy--; \
\
/* Otherwise, if right mid pixel is good, move right along its lower edge. */ \
         } else if( ix < ubnd[ 0 ] && ISVALID(*pc,OperI,value) ) { \
            pa = pc; \
            at = 3; \
            ix++; \
\
/* Otherwise, move up the left edge of the current pixel. */ \
         } else { \
            nright--; \
            at = 2; \
         } \
\
/* If the polygon bends at this point, or if we will be smoothing the \
   polygon, append the pixel coordinates at this pixel corner to the \
   polygon. */ \
         if( full || pa != pc ) ADD( xx, yy ); \
      } \
\
/* If we have arrived back at the start, break out of the loop. */ \
      if( ix == ix0 && iy == iy0 && at == 2 ) done = 1; \
   } \
\
/* If we have circled round to the right, the polygon will not enclosed \
   the specified position, so free resources and return a NULL pointer. */ \
   if( nright > 0 ) { \
      xvert = astFree( xvert ); \
      yvert = astFree( yvert ); \
\
/* If we have circled round to the left, the polygon will enclose \
   the specified position, so create and return a PointSet. */ \
   } else { \
      result = astPointSet( nvert, 2, " ", status ); \
      ptr[ 0 ] = xvert; \
      ptr[ 1 ] = yvert; \
      astSetPoints( result, ptr ); \
   } \
\
/* Annul the returned PointSet if anythign went wrong. */ \
   if( !astOK && result ) result = astAnnul( result ); \
\
/* Return the PointSet pointer. */ \
   return result; \
}

/* Define a macro for testing if a pixel value <V> satisfies the requirements
   specified by <Oper> and <Value>. Compiler optimisation should remove
   all the "if" testing from this expression. */
#define ISVALID(V,OperI,Value) ( \
   ( OperI == AST__LT ) ? ( (V) < Value ) : ( \
      ( OperI == AST__LE ) ? ( (V) <= Value ) : ( \
         ( OperI == AST__EQ ) ? ( (V) == Value ) : ( \
            ( OperI == AST__GE ) ? ( (V) >= Value ) : ( \
               ( OperI == AST__NE ) ? ( (V) != Value ) : ( \
                  (V) > Value \
               ) \
            ) \
         ) \
      ) \
   ) \
)

/* Define a macro to add a vertex position to dynamically allocated
   arrays of X and Y positions. */
#define ADD(X,Y) {\
   ii = nvert++; \
   xvert = (double *) astGrow( xvert, nvert, sizeof( double ) ); \
   yvert = (double *) astGrow( yvert, nvert, sizeof( double ) ); \
   if( astOK ) { \
      xvert[ ii ] = (X); \
      yvert[ ii ] = (Y); \
   } \
}

/* Define a macro that uses the above macro to to create implementations
   of TraceEdge for all operations. */
#define MAKEALL_TRACEEDGE(X,Xtype) \
MAKE_TRACEEDGE(X,Xtype,LT,AST__LT) \
MAKE_TRACEEDGE(X,Xtype,LE,AST__LE) \
MAKE_TRACEEDGE(X,Xtype,EQ,AST__EQ) \
MAKE_TRACEEDGE(X,Xtype,NE,AST__NE) \
MAKE_TRACEEDGE(X,Xtype,GE,AST__GE) \
MAKE_TRACEEDGE(X,Xtype,GT,AST__GT)

/* Expand the above macro to generate a function for each required
   data type and operation. */
#if HAVE_LONG_DOUBLE     /* Not normally implemented */
MAKEALL_TRACEEDGE(LD,long double)
#endif
MAKEALL_TRACEEDGE(D,double)
MAKEALL_TRACEEDGE(L,long int)
MAKEALL_TRACEEDGE(UL,unsigned long int)
MAKEALL_TRACEEDGE(I,int)
MAKEALL_TRACEEDGE(UI,unsigned int)
MAKEALL_TRACEEDGE(S,short int)
MAKEALL_TRACEEDGE(US,unsigned short int)
MAKEALL_TRACEEDGE(B,signed char)
MAKEALL_TRACEEDGE(UB,unsigned char)
MAKEALL_TRACEEDGE(F,float)

/* Undefine the macros. */
#undef MAKE_TRACEEDGE
#undef MAKEALL_TRACEEDGE
#undef ISVALID

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

/* We now determine the number of times this line crosses the polygon
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
   out->startsat = NULL;

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
      this->startsat = astFree( this->startsat );

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

/* A flag indicating the convention used for determining the interior of
   the polygon. A zero value indicates that the old AST system is in
   use (inside to the left when moving anti-clockwise round the vertices
   as viewed from the outside of the celestial sphere). A non-zero value
   indicates the STC system is in use (inside to the left when moving
   anti-clockwise round the vertices as viewed from the inside of the
   celestial sphere). AST currently uses the STC system. */
   astWriteInt( channel, "Order", 1, 0, 1, "Polygon uses STC vertex order convention" );
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAPolygon and astCheckPolygon functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(Polygon,Region)
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
*     anti-clockwise manner (when viewed from the inside of the celestial
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
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
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
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstFrame *frame;              /* Pointer to Frame structure */
   AstPolygon *new;              /* Pointer to new Polygon */
   AstRegion *unc;               /* Pointer to Region structure */
   va_list args;                 /* Variable argument list */

   int *status;                  /* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

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
         new->startsat = NULL;
         new->totlen = 0.0;
         new->acw = 1;
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
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstPolygon *new;              /* Pointer to the new Polygon */
   int order;                    /* Is the new (STC) order convention used? */

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

/* A flag indicating what order the vertices are stored in. See the Dump
   function. */
      order = astReadInt( channel, "order", 0 );

/* Initialise other class properties. */
      new->lbnd[ 0 ] = AST__BAD;
      new->ubnd[ 0 ] = AST__BAD;
      new->lbnd[ 1 ] = AST__BAD;
      new->ubnd[ 1 ] = AST__BAD;
      new->edges = NULL;
      new->startsat = NULL;
      new->totlen = 0.0;
      new->acw = 1;
      new->stale = 1;

/* If the order in which the vertices were written used the old AST
   convention, negate the Polygon so that it is consistent with the
   current conevtion (based on STC). */
      if( ! order ) astNegate( new );

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


AstPolygon *astDownsize_( AstPolygon *this, double maxerr, int maxvert,
                          int *status ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,Polygon,Downsize))( this, maxerr, maxvert, status );
}


