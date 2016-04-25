/*
*class++
*  Name:
*     Circle

*  Purpose:
*     A circular or spherical region within a Frame.

*  Constructor Function:
c     astCircle
f     AST_CIRCLE

*  Description:
*     The Circle class implements a Region which represents a circle or
*     sphere within a Frame.

*  Inheritance:
*     The Circle class inherits from the Region class.

*  Attributes:
*     The Circle class does not define any new attributes beyond
*     those which are applicable to all Regions.

*  Functions:
c     In addition to those functions applicable to all Regions, the
c     following functions may also be applied to all Circles:
f     In addition to those routines applicable to all Regions, the
f     following routines may also be applied to all Circles:
*
c     - astCirclePars: Get the geometric parameters of the Circle
f     - AST_CIRCLEPARS: Get the geometric parameters of the Circle

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
*     31-AUG-2004 (DSB):
*        Original version.
*     4-NOV-2013 (DSB):
*        Modify RegPins so that it can handle uncertainty regions that straddle
*        a discontinuity. Previously, such uncertainty Regions could have a huge
*        bounding box resulting in matching region being far too big.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS Circle

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
#include "circle.h"              /* Interface definition for this class */
#include "ellipse.h"             /* Interface definition for ellipse class */
#include "mapping.h"             /* Position mappings */
#include "unitmap.h"             /* Unit Mapping */

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
astMAKE_INITGLOBALS(Circle)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(Circle,Class_Init)
#define class_vtab astGLOBAL(Circle,Class_Vtab)


#include <pthread.h>


#else


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstCircleVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstCircle *astCircleId_( void *, int, const double[], const double[], void *, const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstMapping *Simplify( AstMapping *, int * );
static AstPointSet *RegBaseMesh( AstRegion *, int * );
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static double *CircumPoint( AstFrame *, int, const double *, double, int * );
static double *RegCentre( AstRegion *this, double *, double **, int, int, int * );
static int RegPins( AstRegion *, AstPointSet *, AstRegion *, int **, int * );
static int RegTrace( AstRegion *, int, double *, double **, int * );
static void Cache( AstCircle *, int * );
static void CalcPars( AstFrame *, AstPointSet *, double *, double *, double *, int * );
static void CirclePars( AstCircle *, double *, double *, double *, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void RegBaseBox( AstRegion *this, double *, double *, int * );
static void ResetCache( AstRegion *this, int * );
static void SetRegFS( AstRegion *, AstFrame *, int * );

/* Member functions. */
/* ================= */

AstRegion *astBestCircle_( AstPointSet *mesh, double *cen, AstRegion *unc, int *status ){
/*
*+
*  Name:
*     astBestCircle

*  Purpose:
*     Find the best fitting Circle through a given mesh of points.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "circle.h"
*     AstRegion *astBestCircle( AstPointSet *mesh, double *cen, AstRegion *unc )

*  Class Membership:
*     Circle member function

*  Description:
*     This function finds the best fitting Circle through a given mesh of
*     points.

*  Parameters:
*     mesh
*        Pointer to a PointSet holding the mesh of points. They are
*        assumed to be in the Frame represented by "unc".
*     cen
*        Pointer to an array holding the coordinates of the new Circle
*        centre.
*     unc
*        A Region representing the uncertainty associated with each point
*        on the mesh.

*  Returned Value:
*     Pointer to the best fitting Circle. It will inherit the positional
*     uncertainty and Frame represented by "unc".

*  Notes:
*    - A NULL pointer is returned if an error has already occurred, or if
*    this function should fail for any reason.

*-
*/

/* Local Variables: */
   AstRegion *result;
   double *p;
   double rad;
   double **ptr;
   double d;
   double s2r;
   double p0;
   int ic;
   int ip;
   int n;
   int nc;
   int np;

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get no. of points in the mesh, and the number of axis values per point. */
   np = astGetNpoint( mesh );
   nc = astGetNcoord( mesh );

/* Get pointers to the axis values. */
   ptr = astGetPoints( mesh );

/* Check pointers can be used safely */
   if( astOK ) {

/* We find ther sum of the squared axis increments from the supplied
   centre to each of the supplied points. Initialise the sum to zero. */
      s2r = 0.0;
      n = 0;

/* Loop round all axes. */
      for( ic = 0; ic < nc; ic++ ) {
         p = ptr[ ic ];
         p0 = cen[ ic ];

/* Loop round all values for this axis. */
         for( ip = 0; ip < np; ip++, p++ ) {
            if( *p != AST__BAD ) {

/* Increment the sums */
               d = *p - p0;
               s2r += d*d;
               n++;

            }
         }
      }

/* Find the RMS distance of the points from the supplied centre. This is
   the radius of the best fitting circle. */
      if( n > 0 ) {
         rad = sqrt( nc*s2r/n );

/* Create the returned Region. */
         result = (AstRegion *) astCircle( unc, 1, cen, &rad, unc, "", status );
      }
   }

/* Return NULL if anything went wrong. */
   if( !astOK ) result = astAnnul( result );

/* Return the result.*/
   return result;
}

static void Cache( AstCircle *this, int *status ){
/*
*  Name:
*     Cache

*  Purpose:
*     Calculate intermediate values and cache them in the Circle structure.

*  Type:
*     Private function.

*  Synopsis:
*     #include "circle.h"
*     void Cache( AstCircle *this, int *status )

*  Class Membership:
*     Circle member function

*  Description:
*     This function uses the PointSet stored in the parent Region to calculate
*     some intermediate values which are useful in other methods. These
*     values are stored within the Circle structure.

*  Parameters:
*     this
*        Pointer to the Circle.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstFrame *frm;
   double *centre;
   double *lb;
   double *ub;
   double radius;
   int i;
   int nc;

/* Check the global error status. */
   if ( !astOK ) return;

/* Do Nothing if the cached information is up to date. */
   if( this->stale ) {

/* Get a pointer to the base Frame and the number of base axes. */
      frm = astGetFrame( ((AstRegion *) this)->frameset, AST__BASE );
      nc = astGetNaxes( frm );

/* Allocate memory to hold the centre coords. */
      centre = astMalloc( sizeof( double )*astGetNaxes( frm ) );

/* Get the radius and centre of the Circle in the base Frame, using the
   centre and circumference positions stored in the parent Region structure. */
      CalcPars( frm, ( (AstRegion *) this)->points, centre, &radius, NULL,
                status );

/* Allocate memory to store the base frame bounding box. This is just
   initialised here. It is set properly when the astRegBaseMesh
   function is called. This box should not be used unless the "basemesh"
   component of the parent Region structure is set to a non-null value. */
      lb = (double *) astMalloc( sizeof( double )*(size_t) nc );
      ub = (double *) astMalloc( sizeof( double )*(size_t) nc );

/* Initialise the bounding box. */
      for( i = 0; astOK && i < nc; i++ ) {
         lb[ i ] = -DBL_MAX;
         ub[ i ] = DBL_MAX;
      }

/* If everything went OK, store these values in the Circle structure. */
      if( astOK ) {
         this->radius = radius;

         astFree( this->centre );
         this->centre = centre;
         centre = NULL;

         astFree( this->lb );
         this->lb = lb;
         lb = NULL;

         astFree( this->ub );
         this->ub = ub;
         ub = NULL;
      }

/* Free resources */
      frm = astAnnul( frm );
      if( centre ) centre = astFree( centre );

/* Indicate cached information is up to date. */
      this->stale = 0;
   }
}

static void CalcPars( AstFrame *frm, AstPointSet *pset, double *centre,
                      double *radius, double *p1, int *status ){
/*
*  Name:
*     CalcPars

*  Purpose:
*     Calculate the geometric parameters of the supplied Circle.

*  Type:
*     Private function.

*  Synopsis:
*     #include "circle.h"
*     double *CalcPars( AstFrame *frm, AstPointSet *pset, double *centre,
*                       double *radius, double *p1, int *status )

*  Class Membership:
*     Circle member function

*  Description:
*     This function uses the supplied PointSet to calculate the geometric
*     parameters that describe the a crcle. These values are returned in
*     a newly allocated dynamic array.

*  Parameters:
*     frm
*        Pointer to the Frame in which the circle is defined.
*     pset
*        Pointer to a PointSet. The first point should be the circle
*        centre, and the second point should be a point on the circle
*        circumference.
*     centre
*        An array in which to return the axis values at the circle centre.
*        The length of this array should be no less than the number of
*        axes in "frm".
*     radius
*        Pointer to a double in which to return the circle radius,
*        expressed as a geodesic distance in the supplied Frame.
*     p1
*        An array in which to return the coordinates of a point on the
*        circumference of the circle. The length of this array should be
*        no less than the number of axes in "frm". Can be NULL if the
*        circumference position is not needed.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   double **ptr;
   double *circum;
   int i;
   int nc;

/* Check the global error status. */
   if ( !astOK ) return;

/* Get and the number of axes. */
   nc = astGetNaxes( frm );

/* Get pointers to the coordinate data in the supplied PointSet. */
   ptr = astGetPoints( pset );

/* If no p1 array was supplied, create a temporary work array to hold the
   circumference position. */
   if( !p1 ) {
      circum = astMalloc( sizeof( double )*nc );
   } else {
      circum = p1;
   }

/* Check pointers can be used safely. */
   if( ptr ) {

/* Copy the two points in to the allocated memory. */
      for( i = 0; i < nc; i++ ) {
         centre[ i ] = ptr[ i ][ 0 ];
         circum[ i ] = ptr[ i ][ 1 ];
      }

/* Return the geodesic distance between these two points as the radius. */
      *radius = astDistance( frm, centre, circum );
   }

/* Free any work array. */
   if( !p1 ) circum = astFree( circum );
}

static void CirclePars( AstCircle *this, double *centre, double *radius,
                        double *p1, int *status ){
/*
*++
*  Name:
c     astCirclePars
f     AST_CIRCLEPARS

*  Purpose:
*     Returns the geometric parameters of an Circle.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "circle.h"
c     void astCirclePars( AstCircle *this, double *centre, double *radius,
c                         double *p1 )
f     CALL AST_CIRCLEPARS( THIS, CENTRE, RADIUS, P1, STATUS )

*  Class Membership:
*     Region method.

*  Description:
c     This function
f     This routine
*     returns the geometric parameters describing the supplied Circle.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Region.
c     centre
f     CENTRE( * ) = DOUBLE PRECISION (Returned)
c        Pointer to an array
f        An array
*        in which to return the coordinates of the Circle centre.
*        The length of this array should be no less than the number of
*        axes in the associated coordinate system.
c     radius
f     RADIUS = DOUBLE PRECISION (Returned)
*        Returned holding the radius of the Circle, as an geodesic
*        distance in the associated coordinate system.
c     p1
f     P1( * ) = DOUBLE PRECISION (Returned)
c        Pointer to an array
f        An array
*        in which to return the coordinates of a point on the
*        circumference of the Circle. The length of this array should be
*        no less than the number of axes in the associated coordinate system.
c        A NULL pointer can be supplied if the circumference position is
c        not needed.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
*     - If the coordinate system represented by the Circle has been
*     changed since it was first created, the returned parameters refer
*     to the new (changed) coordinate system, rather than the original
*     coordinate system. Note however that if the transformation from
*     original to new coordinate system is non-linear, the shape
*     represented by the supplied Circle object may not be an accurate
*     circle.
*--
*/

/* Local Variables: */
   AstRegion *this_region;  /* Parent Region pointer */
   AstFrame *frm;           /* Current Frame represented by the Circle */
   AstPointSet *pset;       /* PointSet holding PointList axis values */

/* Check the inherited status. */
   if( !astOK ) return;

/* Store a pointer to the parent region structure. */
   this_region = (AstRegion *) this;

/* Transform the base Frame axis values into the current Frame. */
   pset = astTransform( this_region->frameset, this_region->points, 1, NULL );

/* Get the Circle frame. */
   frm = astGetFrame( this_region->frameset, AST__CURRENT );

/* Calculate the required parameters. */
   CalcPars( frm, pset, centre, radius, p1, status );

/* Free resources */
   frm = astAnnul( frm );
   pset = astAnnul( pset );
}

static double *CircumPoint( AstFrame *frm, int nax, const double *centre,
                            double radius, int *status ){
/*
*  Name:
*     CircumPoint

*  Purpose:
*     Find a point on the circumference of the circle.

*  Type:
*     Private function.

*  Synopsis:
*     #include "circle.h"
*     double *CircumPoint( AstFrame *frm, int nax, const double *centre,
*                          double radius, int *status )

*  Class Membership:
*     Circle member function

*  Description:
*     This function returns a dynamically allocated array containing the
*     axis values at a point on the circumference of the circle specified
*     by a given centre and radius. The returned point is the point at
*     which the circle crosses the first axis.

*  Parameters:
*     frm
*        Pointer to the Frame in which the circle is defined.
*     nax
*        The number of axes in the Frame.
*     centre
*        An array holding the axis values at the circle centre.
*     radius
*        The circle radius, expressed as a geodesic distance in the
*        supplied Frame.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a 1D array holding the axis values at the point where
*     the circle crosses the first frame axis. The length of this array
*     will equal the number of axes in the supsplied Frame. It should be
*     freed using astFree when no longer needed.

*/

/* Local Variables: */
   double *circum;
   double *work;
   int i;

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Allocate the returned array. */
   circum = astMalloc( sizeof( double)*(size_t) nax );

/* Allocate work space */
   work = astMalloc( sizeof( double)*(size_t) nax );

/* Check pointers can be used safely. */
   if( astOK ) {

/* Find the coords of a point that is offset away from the centre
   position along the first axis. We use the supplied radius value as a
   convenient offset length, but the actual length used is not critical. */
      for( i = 0; i < nax; i++ ) work[ i ] = centre[ i ];
      work[ 0 ] = astAxOffset( frm, 1, work[ 0 ], radius );

/* Offset away from the centre position, towards the position found
   above, going the distance specified by the supplied radius. */
      astOffset( frm, centre, work, radius, (double *) circum );
   }

/* Free resources. */
   work = astFree( work );

/* Return the result. */
   return circum;
}

void astInitCircleVtab_(  AstCircleVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitCircleVtab

*  Purpose:
*     Initialise a virtual function table for a Circle.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "circle.h"
*     void astInitCircleVtab( AstCircleVtab *vtab, const char *name )

*  Class Membership:
*     Circle vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the Circle class.

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
   will be used (by astIsACircle) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstRegionVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */
   vtab->CirclePars = CirclePars;

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
   region->RegTrace = RegTrace;
   region->RegBaseMesh = RegBaseMesh;
   region->RegBaseBox = RegBaseBox;
   region->RegCentre = RegCentre;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */

/* Declare the copy constructor, destructor and class dump
   functions. */
   astSetDelete( vtab, Delete );
   astSetCopy( vtab, Copy );
   astSetDump( vtab, Dump, "Circle", "Circular or spherical region" );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
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
*     #include "circle.h"
*     void RegBaseBox( AstRegion *this, double *lbnd, double *ubnd, int *status )

*  Class Membership:
*     Circle member function (over-rides the astRegBaseBox protected
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
   AstCircle *this;              /* Pointer to Circle structure */
   AstFrame *frm;                /* Pointer to base Frame */
   const char *class;            /* Pointer to class name */
   int i;                        /* Axis index */
   int nb;                       /* No. of axes in base Frame */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the Circle structure */
   this = (AstCircle *) this_region;

/* Ensure cached information is available. */
   Cache( this, status );

/* Get a pointer to the base Frame in the Region, and get the number of
   axes. */
   frm = astGetFrame( this_region->frameset, AST__BASE );
   nb = astGetNaxes( frm );

/* If the Frame is a simple Frame, we can assume plane geometry. */
   class = astGetClass( frm );
   if( class && !strcmp( class, "Frame" ) ) {
      for( i = 0; i < nb; i++ ) {
         lbnd[ i ] = ( this->centre )[ i ] - this->radius;
         ubnd[ i ] = ( this->centre )[ i ] + this->radius;
      }

/* If the Frame is not a simple Frame we cannot assume plane geometry. */
   } else {

/* The bounding box of the mesh returned by astRegBaseMesh is used as the
   bounding box of the Circle. These bounds are cached in the Circle
   structure by astRegBaseMesh. Ensure astRegBaseMesh has been invoked,
   so that it is safe to use the cached bounding box. */
      if( !this_region->basemesh ) (void) astAnnul( astRegBaseMesh( this ) );

/* Store the bounding box. */
      for( i = 0; i < nb; i++ ) {
         lbnd[ i ] = this->lb[ i ];
         ubnd[ i ] = this->ub[ i ];
      }
   }

/* Free resources. */
   frm = astAnnul( frm );
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
*     #include "circle.h"
*     AstPointSet *astRegBaseMesh( AstRegion *this, int *status )

*  Class Membership:
*     Circle member function (over-rides the astRegBaseMesh protected
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
*     Pointer to the PointSet. The axis values in this PointSet will have
*     associated accuracies derived from the accuracies which were
*     supplied when the Region was created.

*  Notes:
*    - A NULL pointer is returned if an error has already occurred, or if
*    this function should fail for any reason.

*/

/* Local Constants: */
#define NP_EDGE 50                /* No. of points for determining geodesic */

/* Local Variables: */
   AstBox *box;                   /* Bounding box for this Circle */
   AstCircle *this;               /* The Circle structure */
   AstRegion *reg;                /* Copy of supplied Circle */
   AstFrame *frm;                 /* Base Frame in encapsulated FrameSet */
   AstPointSet *result;           /* Returned pointer */
   double **ptr;                  /* Pointers to data */
   double *p1;                    /* Pointer to array holding a single point */
   double *p2;                    /* Pointer to array holding a single point */
   double angle;                  /* Angular position of point */
   double delta;                  /* Angular separation of points */
   double dist;                   /* Offset along an axis */
   double lbx;                    /* Lower x bound of mesh bounding box */
   double lby;                    /* Lower y bound of mesh bounding box */
   double p[ 2 ];                 /* Position in 2D Frame */
   double ubx;                    /* Upper x bound of mesh bounding box */
   double uby;                    /* Upper y bound of mesh bounding box */
   int i;                         /* Point index */
   int j;                         /* Axis index */
   int naxes;                     /* No. of axes in base Frame */
   int np;                        /* No. of points in returned PointSet */

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

/* Get a pointer to the Circle structure. */
      this = (AstCircle *) this_region;

/* Get a pointer to the base Frame in the encapsulated FrameSet. */
      frm = astGetFrame( this_region->frameset, AST__BASE );

/* Get the number of axes in the base Frame */
      naxes = astGetNaxes( frm );

/* Get the requested number of points to put on the mesh. */
      np = astGetMeshSize( this );

/* Ensure cached information is available. */
      Cache( (AstCircle *) this, status );

/* First deal with 1-D "circles" (where we ignore MeshSize). */
      if( naxes == 1 ) {

/* The boundary of a 1-D circle consists of 2 points - the two extreme values.
   Create a PointSet to hold 2 1-D values, and store the extreme values. */
         result = astPointSet( 2, 1, "", status );
         ptr = astGetPoints( result );
         if( astOK ) {
            ptr[ 0 ][ 0 ] = ( this->centre )[ 0 ] - this->radius;
            ptr[ 0 ][ 1 ] = ( this->centre )[ 0 ] + this->radius;
         }

/* Store the bounding box in the Circle structure. */
         this->lb[ 0 ] = ptr[ 0 ][ 0 ];
         this->ub[ 0 ] = ptr[ 0 ][ 1 ];

/* Now deal with 2-D circles. */
      } else if( naxes == 2 ){

/* Store the angular increment between points. */
         delta = 2*AST__DPI/np;

/* Create a suitable PointSet to hold the returned positions. */
         result = astPointSet( np, 2, "", status );
         ptr = astGetPoints( result );
         if( astOK ) {

/* Initialise the bounding box of the mesh points. */
            lbx = DBL_MAX;
            ubx = -DBL_MAX;
            lby = DBL_MAX;
            uby = -DBL_MAX;

/* Loop round each point. */
            angle = 0.0;
            for( i = 0; i < np; i++ ) {

/* Work out where the end of the radius vector at this angle is, and
   store in the returned PointSet. */
               astOffset2( frm, this->centre, angle, this->radius, p );
               ptr[ 0 ][ i ] = p[ 0 ];
               ptr[ 1 ][ i ] = p[ 1 ];

/* Update the bounds of the mesh bounding box. The box is expressed in
   terms of axis offsets from the centre, in order to avoid problems with
   boxes that cross RA=0 or RA=12h */
               if( p[ 0 ] != AST__BAD && p[ 1 ] != AST__BAD ){
                  dist =  astAxDistance( frm, 1, this->centre[ 0 ], p[ 0 ] );
                  if( dist < lbx ) {
                     lbx = dist;
                  } else if( dist > ubx ) {
                     ubx = dist;
                  }
                  dist =  astAxDistance( frm, 2, this->centre[ 1 ], p[ 1 ] );
                  if( dist < lby ) {
                     lby = dist;
                  } else if( dist > uby ) {
                     uby = dist;
                  }
               }

/* Increment the angular position of the next mesh point. */
               angle += delta;
            }

/* Store the bounding box in the Circle structure. */
            this->lb[ 0 ] = this->centre[ 0 ] + lbx;
            this->lb[ 1 ] = this->centre[ 1 ] + lby;
            this->ub[ 0 ] = this->centre[ 0 ] + ubx;
            this->ub[ 1 ] = this->centre[ 1 ] + uby;
         }

/* Now deal with circles with more than 2 dimensions. Producing an evenly
   spread mesh of points over a sphere is a complex task (see e.g.
   http://www.eso.org/science/healpix/ ). This implementation does not
   attempt to produce a genuinely even spread. Instead it simply uses the
   mesh for the bounding box of the sphere, and projects each point on to
   the surface of the sphere. */
      } else {

/* Allocate memory to hold an approximation of the circle bounding box. */
         p1 = astMalloc( sizeof( double )*(size_t) naxes );
         p2 = astMalloc( sizeof( double )*(size_t) naxes );

/* Get an approximation to the bounding box, and initialise the real
   bounding box of the mesh points. */
         if( astOK ) {
            memcpy( p1, this->centre, sizeof( double )*(size_t) naxes );
            for( j = 0; j < naxes; j++ ) {
               p1[ j ] += this->radius;
               astOffset( frm, this->centre, p1, this->radius, p2 );
               p1[ j ] = this->centre[ j ];
               this->ub[ j ] = p2[ j ];
            }
         }

/* Create a Box region which just encompasses the circle. */
         box = astBox( frm, 0, this->centre, this->ub, NULL, "", status );

/* Get a mesh covering this box. */
         astSetMeshSize( box, np );
         result = astRegBaseMesh( box );
         ptr = astGetPoints( result );
         np = astGetNpoint( result  );

/* Allocate memory for a single point */
         if( astOK ) {

/* Initialise the real bounding box of the mesh points. */
            for( j = 0; j < naxes; j++ ) {
               this->lb[ j ] = DBL_MAX;
               this->ub[ j ] = -DBL_MAX;
            }

/* Move each point in this mesh radially so that its distance from the centre
   equals the radius of this Circle. */
            for( i = 0; i < np; i++ ) {
               for( j = 0; j < naxes; j++ ) p1[ j ] = ptr[ j ][ i ];
               astOffset( frm, this->centre, p1, this->radius, p2 );

               for( j = 0; j < naxes; j++ ) {
                  ptr[ j ][ i ] = p2[ j ];

/* Update the bounds of the mesh bounding box. */
                  if( p2[ j ] != AST__BAD ){
                     if( p2[ j ] < this->lb[ j ] ) {
                        this->lb[ j ] = p2[ j ];
                     } else if( p2[ j ] > this->ub[ j ] ) {
                        this->ub[ j ] = p2[ j ];
                     }
                  }
               }
            }
         }

/* Same the returned pointer in the Region structure so that it does not
   need to be created again next time this function is called. */
         if( astOK && result ) this_region->basemesh = astClone( result );

/* Free resources. */
         p1 = astFree( p1 );
         p2 = astFree( p2 );
         box = astAnnul( box );
      }

/* Extend the bounding box if it contains any singularies. The astNormBox
   requires a Mapping which can be used to test points in the base Frame.
   Create a copy of the Circle and then set its FrameSet so that the current
   Frame in the copy is the same as the base Frame in the original. */
      reg = astCopy( this );
      astSetRegFS( reg, frm );
      astSetNegated( reg, 0 );

/* Normalise this box. */
      astNormBox( frm, this->lb, this->ub, reg );

/* Free resources. */
      reg = astAnnul( reg );
      frm = astAnnul( frm );
   }

/* Annul the result if an error has occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return a pointer to the output PointSet. */
   return result;
}

static double *RegCentre( AstRegion *this_region, double *cen, double **ptr,
                          int index, int ifrm, int *status ){
/*
*  Name:
*     RegCentre

*  Purpose:
*     Re-centre a Region.

*  Type:
*     Private function.

*  Synopsis:
*     #include "circle.h"
*     double *RegCentre( AstRegion *this, double *cen, double **ptr,
*                        int index, int ifrm, int *status )

*  Class Membership:
*     Circle member function (over-rides the astRegCentre protected
*     method inherited from the Region class).

*  Description:
*     This function shifts the centre of the supplied Region to a
*     specified position, or returns the current centre of the Region.

*  Parameters:
*     this
*        Pointer to the Region.
*     cen
*        Pointer to an array of axis values, giving the new centre.
*        Supply a NULL value for this in order to use "ptr" and "index" to
*        specify the new centre.
*     ptr
*        Pointer to an array of pointers, one for each axis in the Region.
*        Each pointer locates an array of axis values. This is the format
*        returned by the PointSet method astGetPoints. Only used if "cen"
*        is NULL.
*     index
*        The index of the point within the arrays identified by "ptr" at
*        which is stored the coords for the new centre position. Only used
*        if "cen" is NULL.
*     ifrm
*        Should be AST__BASE or AST__CURRENT. Indicates whether the centre
*        position is supplied and returned in the base or current Frame of
*        the FrameSet encapsulated within "this".
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     If both "cen" and "ptr" are NULL then a pointer to a newly
*     allocated dynamic array is returned which contains the centre
*     coords of the Region. This array should be freed using astFree when
*     no longer needed. If either of "ptr" or "cen" is not NULL, then a
*     NULL pointer is returned.

*  Notes:
*    - Some Region sub-classes do not have a centre. Such classes will report
*    an AST__INTER error code if this method is called.
*/

/* Local Variables: */
   AstFrame *frm;      /* Pointer to base Frame */
   AstCircle *this;    /* Pointer to Circle structure */
   double **rptr;      /* Data pointers for Region PointSet */
   double *bc;         /* Base Frame centre position */
   double *circum;     /* Base frame circumference position */
   double *result;     /* Returned pointer */
   double *tmp;        /* Temporary array pointer */
   double axval;       /* Axis value */
   int ic;             /* Coordinate index */
   int ncb;            /* Number of base frame coordinate values per point */
   int ncc;            /* Number of current frame coordinate values per point */

/* Initialise */
   result = NULL;

/* Check the local error status. */
   if ( !astOK ) return result;

/* Get a pointer to the Circle structure. */
   this = (AstCircle *) this_region;

/* Get the number of axis values per point in the base and current Frames. */
   ncb = astGetNin( this_region->frameset );
   ncc = astGetNout( this_region->frameset );

/* Ensure cached information is available. */
   Cache( this, status );

/* If the centre coords are to be returned, return either a copy of the
   base Frame centre coords, or transform the base Frame centre coords
   into the current Frame. */
   if( !ptr && !cen ) {
      if( ifrm == AST__CURRENT ) {
         result = astRegTranPoint( this_region, this->centre, 1, 1 );
      } else {
         result = astStore( NULL, this->centre, sizeof( double )*ncb );
      }

/* Otherwise, we store the supplied new centre coords and return a NULL
   pointer. */
   } else {

/* Get a pointer to the base Frame in the Region's FrameSet. */
      frm = astGetFrame( this_region->frameset, AST__BASE );

/* Get a pointer to the axis values stored in the Region structure. */
      rptr = astGetPoints( this_region->points );

/* Check pointers can be used safely */
      if( astOK ) {

/* If the centre position was supplied in the current Frame, find the
   corresponding base Frame position... */
         if( ifrm == AST__CURRENT ) {
            if( cen ) {
               bc = astRegTranPoint( this_region, cen, 1, 0 );
            } else {
               tmp = astMalloc( sizeof( double)*(size_t)ncc );
               if( astOK ) {
                  for( ic = 0; ic < ncc; ic++ ) tmp[ ic ] = ptr[ ic ][ index ];
               }
               bc = astRegTranPoint( this_region, tmp, 1, 0 );
               tmp = astFree( tmp );
            }

/* Replace any bad centre values with their current values. */
            for( ic = 0; ic < ncb; ic++ ) {
               if( bc[ ic ] ==  AST__BAD ) bc[ ic ] = this->centre[ ic ];
            }

/* ... and change the coords in the parent Region structure and the cached
   coords in the Circle structure. */
            circum = CircumPoint( frm, ncb, bc, this->radius, status );
            if( circum ) {
               for( ic = 0; ic < ncb; ic++ ) {
                  rptr[ ic ][ 0 ] = bc[ ic ];
                  rptr[ ic ][ 1 ] = circum[ ic ];
                  this->centre[ ic ] = bc[ ic ];
               }
            }

/* Free resources */
            circum = astFree( circum );
            bc = astFree( bc );

/* If the centre position was supplied in the base Frame, use the
   supplied "cen" or "ptr" pointer directly to change the coords in the
   parent Region structure and the cached coords in the Circle structure. */
         } else {
            for( ic = 0; ic < ncb; ic++ ) {
               axval = cen ? cen[ ic ] : ptr[ ic ][ index ];
               if( axval != AST__BAD ) this->centre[ ic ] = axval;
            }

            circum = CircumPoint( frm, ncb, this->centre, this->radius,
                                  status );
            if( circum ) {
               for( ic = 0; ic < ncb; ic++ ) {
                  rptr[ ic ][ 0 ] = this->centre[ ic ];
                  rptr[ ic ][ 1 ] = circum[ ic ];
               }
               circum = astFree( circum );
            }
         }
      }

/* Free resources */
      frm = astAnnul( frm );
   }

/* Return the result. */
   return result;
}

static int RegPins( AstRegion *this_region, AstPointSet *pset, AstRegion *unc,
                    int **mask, int *status ){
/*
*  Name:
*     RegPins

*  Purpose:
*     Check if a set of points fall on the boundary of a given Circle.

*  Type:
*     Private function.

*  Synopsis:
*     #include "circle.h"
*     int RegPins( AstRegion *this, AstPointSet *pset, AstRegion *unc,
*                  int **mask, int *status ){

*  Class Membership:
*     Circle member function (over-rides the astRegPins protected
*     method inherited from the Region class).

*  Description:
*     This function returns a flag indicating if the supplied set of
*     points all fall on the boundary of the given Circle.
*
*     Some tolerance is allowed, as specified by the uncertainty Region
*     stored in the supplied Circle "this", and the supplied uncertainty
*     Region "unc" which describes the uncertainty of the supplied points.

*  Parameters:
*     this
*        Pointer to the Circle.
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
   AstCircle *large_circle;     /* Circle slightly larger than "this" */
   AstCircle *small_circle;     /* Circle slightly smaller than "this" */
   AstCircle *this;             /* Pointer to the Circle structure. */
   AstFrame *frm;               /* Base Frame in supplied Circle */
   AstPointSet *ps1;            /* Points masked by larger Circle */
   AstPointSet *ps2;            /* Points masked by larger and smaller Circlees */
   AstRegion *tunc;             /* Uncertainity Region from "this" */
   double **ptr;                /* Pointer to axis values in "ps2" */
   double *lbnd_tunc;           /* Lower bounds of "this" uncertainty Region */
   double *lbnd_unc;            /* Lower bounds of supplied uncertainty Region */
   double *p;                   /* Pointer to next axis value */
   double *safe;                /* An interior point in "this" */
   double *ubnd_tunc;           /* Upper bounds of "this" uncertainty Region */
   double *ubnd_unc;            /* Upper bounds of supplied uncertainty Region */
   double drad;                 /* Radius increment corresponding to border width */
   double l1;                   /* Length of bounding box diagonal */
   double l2;                   /* Length of bounding box diagonal */
   double rad;                  /* Radius of Circle */
   int i;                       /* Axis index */
   int j;                       /* Point index */
   int nc;                      /* No. of axes in Circle base frame */
   int np;                      /* No. of supplied points */
   int result;                  /* Returned flag */

/* Initialise */
   result = 0;
   if( mask ) *mask = NULL;

/* Check the inherited status. */
   if( !astOK ) return result;

/* Get a pointer to the Circle structure. */
   this = (AstCircle *) this_region;

/* Get the number of base Frame axes in the Circle, and check the supplied
   PointSet has the same number of axis values per point. */
   frm = astGetFrame( this_region->frameset, AST__BASE );
   nc = astGetNaxes( frm );
   if( astGetNcoord( pset ) != nc && astOK ) {
      astError( AST__INTER, "astRegPins(%s): Illegal number of axis "
                "values per point (%d) in the supplied PointSet - should be "
                "%d (internal AST programming error).", status, astGetClass( this ),
                astGetNcoord( pset ), nc );
   }

/* Get the number of axes in the uncertainty Region and check it is the
   same as above. */
   if( unc && astGetNaxes( unc ) != nc && astOK ) {
      astError( AST__INTER, "astRegPins(%s): Illegal number of axes (%d) "
                "in the supplied uncertainty Region - should be "
                "%d (internal AST programming error).", status, astGetClass( this ),
                astGetNaxes( unc ), nc );
   }

/* Get the centre of the region in the base Frame. We use this as a "safe"
   interior point within the region. */
   safe = astRegCentre( this, NULL, NULL, 0, AST__BASE );

/* We now find the maximum distance on each axis that a point can be from the
   boundary of the Circle for it still to be considered to be on the boundary.
   First get the Region which defines the uncertainty within the Circle being
   checked (in its base Frame), re-centre it on the interior point found
   above (to avoid problems if the uncertainty region straddles a
   discontinuity), and get its bounding box. */
   tunc = astGetUncFrm( this, AST__BASE );
   if( safe ) astRegCentre( tunc, safe, NULL, 0, AST__CURRENT );
   lbnd_tunc = astMalloc( sizeof( double )*(size_t) nc );
   ubnd_tunc = astMalloc( sizeof( double )*(size_t) nc );
   astGetRegionBounds( tunc, lbnd_tunc, ubnd_tunc );

/* Find the geodesic length within the base Frame of "this" of the diagonal of
   the bounding box. */
   l1 = astDistance( frm, lbnd_tunc, ubnd_tunc );

/* Also get the Region which defines the uncertainty of the supplied
   points and get its bounding box. First re-centre the uncertainty at the
   interior position to avoid problems from uncertainties that straddle a
   discontinuity. */
   if( unc ) {
      if( safe ) astRegCentre( unc, safe, NULL, 0, AST__CURRENT );
      lbnd_unc = astMalloc( sizeof( double )*(size_t) nc );
      ubnd_unc = astMalloc( sizeof( double )*(size_t) nc );
      astGetRegionBounds( unc, lbnd_unc, ubnd_unc );

/* Find the geodesic length of the diagonal of this bounding box. */
      l2 = astDistance( frm, lbnd_unc, ubnd_unc );

/* Use a zero sized box "unc" if no box was supplied. */
   } else {
      lbnd_unc = NULL;
      ubnd_unc = NULL;
      l2 = 0.0;
   }

/* Ensure cached information is available. */
   Cache( this, status );

/* The required border width is half of the total diagonal of the two bounding
   boxes. */
   if( astOK ) {
      drad = 0.5*( l1 + l2 );

/* Create two new Circle, one of which is larger than "this" by the amount
   found above, and the other of which is smaller than "this" by the amount
   found above. */
      rad = this->radius + 0.5*drad;
      large_circle = astCircle( frm, 1, this->centre, &rad, NULL, "", status );
      rad = this->radius - 0.5*drad;
      small_circle = astCircle( frm, 1, this->centre, &rad, NULL, "", status );

/* Negate the smaller region.*/
      astNegate( small_circle );

/* Points are on the boundary of "this" if they are inside both the large
   Circle and the negated small Circle. First transform the supplied PointSet
   using the large Circle, then transform them using the negated smaller
   Circle. */
      ps1 = astTransform( large_circle, pset, 1, NULL );
      ps2 = astTransform( small_circle, ps1, 1, NULL );

/* Get a point to the resulting axis values, and the number of axis
   values per axis. */
      ptr = astGetPoints( ps2 );
      np = astGetNpoint( ps2 );

/* If a mask array is to be returned, create one. */
      if( mask ) {
         *mask = astMalloc( sizeof(int)*(size_t) np );

/* Check all the resulting points, setting mask values for all of them. */
         if( astOK ) {

/* Initialise the mask elements on the basis of the first axis values */
            result = 1;
            p = ptr[ 0 ];
            for( j = 0; j < np; j++ ) {
               if( *(p++) == AST__BAD ) {
                  result = 0;
                  (*mask)[ j ] = 0;
               } else {
                  (*mask)[ j ] = 1;
               }
            }

/* Now check for bad values on other axes. */
            for( i = 1; i < nc; i++ ) {
               p = ptr[ i ];
               for( j = 0; j < np; j++ ) {
                  if( *(p++) == AST__BAD ) {
                     result = 0;
                     (*mask)[ j ] = 0;
                  }
               }
            }
         }

/* If no output mask is to be made, we can break out of the check as soon
   as the first bad value is found. */
      } else if( astOK ) {
         result = 1;
         for( i = 0; i < nc && result; i++ ) {
            p = ptr[ i ];
            for( j = 0; j < np; j++ ) {
               if( *(p++) == AST__BAD ) {
                  result = 0;
                  break;
               }
            }
         }
      }

/* Free resources. */
      large_circle = astAnnul( large_circle );
      small_circle = astAnnul( small_circle );
      ps1 = astAnnul( ps1 );
      ps2 = astAnnul( ps2 );
   }

   tunc = astAnnul( tunc );
   frm = astAnnul( frm );
   lbnd_tunc = astFree( lbnd_tunc );
   ubnd_tunc = astFree( ubnd_tunc );
   if( unc ) lbnd_unc = astFree( lbnd_unc );
   if( unc ) ubnd_unc = astFree( ubnd_unc );
   safe = astFree( safe );

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
*     #include "circle.h"
*     int astTraceRegion( AstRegion *this, int n, double *dist, double **ptr );

*  Class Membership:
*     Circle member function (overrides the astTraceRegion method
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
   AstCircle *this;
   AstFrame *frm;
   AstMapping *map;
   AstPointSet *bpset;
   AstPointSet *cpset;
   double **bptr;
   double angle;
   double p[ 2 ];
   int i;
   int ncur;
   int result;

/* Initialise */
   result = 0;

/* Check inherited status. */
   if( ! astOK ) return result;

/* Get a pointer to the base Frame in the encapsulated FrameSet. */
   frm = astGetFrame( this_region->frameset, AST__BASE );

/* Check it is 2-dimensional. */
   if( astGetNaxes( frm ) == 2 ) result = 1;

/* Check we have some points to find. */
   if( result && n > 0 ) {

/* Get a pointer to the Circle structure. */
      this = (AstCircle *) this_region;

/* Ensure cached information is available. */
      Cache( this, status );

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

/* Otherwise, create a PointSet to hold the base Frame positions. */
      } else {
         bpset = astPointSet( n, 2, " ", status );
         bptr = astGetPoints( bpset );
         ncur = astGetNout( map );
      }

/* Check the pointers can be used safely. */
      if( astOK ) {

/* Loop round each point. Get the angle around the circle, and offset
   along that angle to find the point that is one radius away from the
   centre. Copy the results into the required arrays. */
         for( i = 0; i < n; i++ ) {
            angle = dist[ i ]*2*AST__DPI;
            astOffset2( frm, this->centre, angle, this->radius, p );
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
   }
   frm = astAnnul( frm );

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
*     #include "circle.h"
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
      ( (AstCircle *) this )->stale = 1;
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
*     #include "circle.h"
*     void SetRegFS( AstRegion *this_region, AstFrame *frm, int *status )

*  Class Membership:
*     Circle method (over-rides the astSetRegFS method inherited from
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

/* Re-calculate cached information. */
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
*     #include "circle.h"
*     AstMapping *Simplify( AstMapping *this, int *status )

*  Class Membership:
*     Circle method (over-rides the astSimplify method inherited
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
   AstMapping *map;           /* Base -> current Mapping */
   AstMapping *result;        /* Result pointer to return */
   AstPointSet *mesh;         /* Mesh of current Frame positions */
   AstPointSet *ps2;          /* Circle PointSet in current Frame */
   AstRegion *new;            /* Pointer to simplified Region */
   AstRegion *newreg;         /* Equivalent circle or ellipse */
   AstRegion *this;           /* Pointer to supplied Region structure */
   AstRegion *unc;            /* Pointer to uncertainty Region */
   double **ptr2;             /* Pointer axis values in "ps2" */
   double *cen;               /* Pointer to array holding new centre coords */
   int ic;                    /* Axis index */
   int nc;                    /* No. of axis values per point */
   int ok;                    /* Was the new centre found OK? */
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

/* If the Mapping from base to current Frame is not a UnitMap, we attempt
   to simplify the Circle by re-defining it within its current Frame.
   Transforming the Circle from its base to its current Frame may result in
   the region no longer being a circle. We test this by transforming a set of
   bounds on the Circle boundary. */
   map = astGetMapping( new->frameset, AST__BASE, AST__CURRENT );
   if( !astIsAUnitMap( map ) ){

/* Get a mesh of points covering the Circle in its current Frame. */
      mesh = astRegMesh( new );

/* Get the Region describing the positional uncertainty within the Circle in
   its current Frame. */
      unc = astGetUncFrm( new, AST__CURRENT );

/* Transform the PointSet holding the circle centre into the current
   Frame, and copy the axis values into a new array. */
      ps2 = astRegTransform( this, this->points, 1, NULL, NULL );
      nc = astGetNcoord( ps2 );
      cen = astMalloc( sizeof( double )*(size_t) nc );
      ptr2 = astGetPoints( ps2 );
      if( astOK ) {

         ok = 1;
         for( ic = 0; ic < nc; ic++ ) {
            cen[ ic ] = ptr2[ ic ][ 0 ];
            if( cen[ ic ] == AST__BAD ) ok = 0;
         }

/* Find the best fitting Circle (defined in the current Frame) through these
   points */
         newreg = ok ? astBestCircle( mesh, cen, unc ) : NULL;

/* See if all points within this mesh fall on the boundary of the best
   fitting Circle, to within the uncertainty of the Region. */
         if( newreg && astRegPins( newreg, mesh, NULL, NULL ) ) {

/* If so, use the new Circle in place of the original. */
            (void) astAnnul( new );
            new = astClone( newreg );

/* Otherwise, if the region is 2-d we see if an Ellipse can represent the
   mesh. */
         } else if( ok && nc == 2 ){

/* Find the best fitting Ellipse (defined in the current Frame) through these
   points */
            if( newreg ) (void) astAnnul( newreg );
            newreg = astBestEllipse( mesh, cen, unc );

/* See if all points within this mesh fall on the boundary of the best
   fitting Ellipse, to within the uncertainty of the Region. */
            if( newreg && astRegPins( newreg, mesh, NULL, NULL ) ) {

/* If so, use the new Ellipse in place of the original. */
               (void) astAnnul( new );
               new = astClone( newreg );
               simpler = 1;
            }
         }

/* Free resources. */
         if( newreg ) newreg = astAnnul( newreg );
      }

      ps2 = astAnnul( ps2 );
      cen = astFree( cen );
      mesh = astAnnul( mesh );
      unc = astAnnul( unc );
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

static AstPointSet *Transform( AstMapping *this_mapping, AstPointSet *in,
                               int forward, AstPointSet *out, int *status ) {
/*
*  Name:
*     Transform

*  Purpose:
*     Apply a Circle to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "circle.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     Circle member function (over-rides the astTransform protected
*     method inherited from the Mapping class).

*  Description:
*     This function takes a Circle and a set of points encapsulated in a
*     PointSet and transforms the points by setting axis values to
*     AST__BAD for all points which are outside the region. Points inside
*     the region are copied unchanged from input to output.

*  Parameters:
*     this
*        Pointer to the Circle.
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
*     match the number of axes in the Frame represented by the Circle.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstCircle *this;            /* Pointer to Circle */
   AstFrame *frm;                /* Pointer to base Frame in FrameSet */
   AstPointSet *pset_tmp;        /* Pointer to PointSet holding base Frame positions*/
   AstPointSet *result;          /* Pointer to output PointSet */
   double **ptr_out;             /* Pointer to output coordinate data */
   double **ptr_tmp;             /* Pointer to base Frame coordinate data */
   double *work;                 /* Pointer to array holding single base point */
   double d;                     /* Base-Frame distance from centre to point */
   int closed;                   /* Is the boundary part of the Region? */
   int coord;                    /* Zero-based index for coordinates */
   int inside;                   /* Is the point inside the Region? */
   int ncoord_out;               /* No. of coordinates per output point */
   int ncoord_tmp;               /* No. of coordinates per base Frame point */
   int neg;                      /* Has the Region been negated? */
   int npoint;                   /* No. of points */
   int point;                    /* Loop counter for points */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the Circle structure. */
   this = (AstCircle *) this_mapping;

/* Apply the parent mapping using the stored pointer to the Transform member
   function inherited from the parent Region class. This function validates
   all arguments and generates an output PointSet if necessary,
   containing a copy of the input PointSet. */
   result = (*parent_transform)( this_mapping, in, forward, out, status );

/* We will now extend the parent astTransform method by performing the
   calculations needed to generate the output coordinate values. */

/* First use the encapsulated FrameSet to transform the supplied positions
   from the current Frame in the encapsulated FrameSet (the Frame
   represented by the Region), to the base Frame (the Frame in which the
   Region is defined). This call also returns a pointer to the base Frame
   of the encapsulated FrameSet. Note, the returned pointer may be a
   clone of the "in" pointer, and so we must be carefull not to modify the
   contents of the returned PointSet. */
   pset_tmp = astRegTransform( this, in, 0, NULL, &frm );

/* Determine the numbers of points and coordinates per point from the base
   Frame PointSet and obtain pointers for accessing the base Frame and output
   coordinate values. */
   npoint = astGetNpoint( pset_tmp );
   ncoord_tmp = astGetNcoord( pset_tmp );
   ptr_tmp = astGetPoints( pset_tmp );
   ncoord_out = astGetNcoord( result );
   ptr_out = astGetPoints( result );

/* Get work space for one base Frame position */
   work = astMalloc( sizeof( double )*(size_t) ncoord_tmp );

/* See if the boundary is part of the Region. */
   closed = astGetClosed( this );

/* See if the Region has been negated. */
   neg = astGetNegated( this );

/* Perform coordinate arithmetic. */
/* ------------------------------ */
   if ( astOK ) {

/* Ensure cached information is available. */
      Cache( this, status );

/* Loop round each point */
      for ( point = 0; point < npoint; point++ ) {

/* Copy the base Frame position into a work array. */
         for ( coord = 0; coord < ncoord_tmp; coord++ ) {
            work[ coord ] = ptr_tmp[ coord ][ point ];
         }

/* Find the geodesic distance from the centre of the Circle in the base
   Frame. */
         d = astDistance( frm, this->centre, work );

/* Now consider whether this radius value puts the point in or out of the
   Circle. */
         if( d != AST__BAD ){
            if( neg ) {
               if( closed ) {
                  inside = ( d >= this->radius );
               } else {
                  inside = ( d > this->radius );
               }
            } else {
               if( closed ) {
                  inside = ( d <= this->radius );
               } else {
                  inside = ( d < this->radius );
               }
            }
         } else {
            inside = 0;
         }

/* If the point is outside, store bad output values. */
         if( !inside ) {
            for ( coord = 0; coord < ncoord_out; coord++ ) {
               ptr_out[ coord ][ point ] = AST__BAD;
            }
         }
      }
   }

/* Free resources */
   work = astFree( work );
   pset_tmp = astAnnul( pset_tmp );
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
*     Copy constructor for Region objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for Region objects.

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
   AstCircle *in;             /* Pointer to input Circle */
   AstCircle *out;            /* Pointer to output Circle */
   int nax;                   /* Number of base Frame axes */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output Circles. */
   in = (AstCircle *) objin;
   out = (AstCircle *) objout;

/* For safety, first clear any references to the input memory from
   the output Circle. */
   out->centre = NULL;
   out->lb = NULL;
   out->ub = NULL;

/* Copy dynamic memory contents */
   nax = astGetNin( ((AstRegion *) in)->frameset );
   out->centre = astStore( NULL, in->centre,
                           sizeof( double )*(size_t)nax );
   out->lb = astStore( NULL, in->lb, sizeof( double )*(size_t)nax );
   out->ub = astStore( NULL, in->ub, sizeof( double )*(size_t)nax );
}


/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for Circle objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for Circle objects.

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
   AstCircle *this;                 /* Pointer to Circle */

/* Obtain a pointer to the Circle structure. */
   this = (AstCircle *) obj;

/* Annul all resources. */
   this->centre = astFree( this->centre );
   this->lb = astFree( this->lb );
   this->ub = astFree( this->ub );
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for Circle objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the Circle class to an output Channel.

*  Parameters:
*     this
*        Pointer to the Circle whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstCircle *this;                 /* Pointer to the Circle structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Circle structure. */
   this = (AstCircle *) this_object;

/* Write out values representing the instance variables for the
   Circle class.  Accompany these with appropriate comment strings,
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
/* Implement the astIsACircle and astCheckCircle functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(Circle,Region)
astMAKE_CHECK(Circle)

AstCircle *astCircle_( void *frame_void, int form, const double centre[],
                       const double point[], AstRegion *unc,
                       const char *options, int *status, ...) {
/*
*++
*  Name:
c     astCircle
f     AST_CIRCLE

*  Purpose:
*     Create a Circle.

*  Type:
*     Public function.

*  Synopsis:
c     #include "circle.h"
c     AstCircle *astCircle( AstFrame *frame, int form, const double centre[],
c                           const double point[], AstRegion *unc,
c                           const char *options, ... )
f     RESULT = AST_CIRCLE( FRAME, FORM, CENTRE, POINT, UNC, OPTIONS, STATUS )

*  Class Membership:
*     Circle constructor.

*  Description:
*     This function creates a new Circle and optionally initialises its
*     attributes.
*
*     A Circle is a Region which represents a circle or sphere within the
*     supplied Frame.

*  Parameters:
c     frame
f     FRAME = INTEGER (Given)
*        A pointer to the Frame in which the region is defined. A deep
*        copy is taken of the supplied Frame. This means that any
*        subsequent changes made to the Frame using the supplied pointer
*        will have no effect the Region.
c     form
f     FORM = INTEGER (Given)
*        Indicates how the circle is described by the remaining parameters.
*        A value of zero indicates that the circle is specified by a
*        centre position and a position on the circumference. A value of one
*        indicates that the circle is specified by a centre position and a
*        scalar radius.
c     centre
f     CENTRE( * ) = DOUBLE PRECISION (Given)
c        An array of double, with one element for each Frame axis
f        An array with one element for each Frame axis
*        (Naxes attribute) containing the coordinates at the centre of
*        the circle or sphere.
c     point
f     POINT( * ) = DOUBLE PRECISION (Given)
c        If "form"
f        If FORM
*        is zero, then this array should have one element for each Frame
*        axis (Naxes attribute), and should be supplied holding the
*        coordinates at a point on the circumference of the circle or sphere.
c        If "form"
f        If FORM
*        is one, then this array should have one element only which should
*        be supplied holding the scalar radius of the circle or sphere,
*        as a geodesic distance within the Frame.
c     unc
f     UNC = INTEGER (Given)
*        An optional pointer to an existing Region which specifies the
*        uncertainties associated with the boundary of the Circle being created.
*        The uncertainty in any point on the boundary of the Circle is found by
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
*        effect on the created Circle. Alternatively,
f        a null Object pointer (AST__NULL)
c        a NULL Object pointer
*        may be supplied, in which case a default uncertainty is used
*        equivalent to a box 1.0E-6 of the size of the Circle being created.
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
c        initialising the new Circle. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new Circle. The syntax used is identical to that for the
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
c     astCircle()
f     AST_CIRCLE = INTEGER
*        A pointer to the new Circle.

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
   AstCircle *new;               /* Pointer to new Circle */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Obtain and validate a pointer to the supplied Frame structure. */
   frame = astCheckFrame( frame_void );

/* Initialise the Circle, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitCircle( NULL, sizeof( AstCircle ), !class_init, &class_vtab,
                     "Circle", frame, form, centre, point, unc );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new Circle's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new Circle. */
   return new;
}

AstCircle *astCircleId_( void *frame_void, int form, const double centre[],
                         const double point[], void *unc_void,
                         const char *options, ... ) {
/*
*  Name:
*     astCircleId_

*  Purpose:
*     Create a Circle.

*  Type:
*     Private function.

*  Synopsis:
*     #include "circle.h"
*     AstCircle *astCircleId_( AstFrame *frame, int form, const double centre[],
*                              const double point[], AstRegion *unc,
*                              const char *options, ... )

*  Class Membership:
*     Circle constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astCircle constructor function. It returns an ID value (instead
*     of a true C pointer) to external users, and must be provided
*     because astCircle_ has a variable argument list which cannot be
*     encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     The variable argument list also prevents this function from
*     invoking astCircle_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astCircle_.

*  Returned Value:
*     The ID value associated with the new Circle.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstFrame *frame;              /* Pointer to Frame structure */
   AstCircle *new;               /* Pointer to new Circle */
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

/* Initialise the Circle, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitCircle( NULL, sizeof( AstCircle ), !class_init, &class_vtab,
                        "Circle", frame, form, centre, point, unc );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new Circle's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new Circle. */
   return astMakeId( new );
}

AstCircle *astInitCircle_( void *mem, size_t size, int init, AstCircleVtab *vtab,
                           const char *name, AstFrame *frame, int form,
                           const double centre[], const double point[],
                           AstRegion *unc, int *status ) {
/*
*+
*  Name:
*     astInitCircle

*  Purpose:
*     Initialise a Circle.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "circle.h"
*     AstCircle *astInitCircle_( void *mem, size_t size, int init, AstCircleVtab *vtab,
*                                const char *name, AstFrame *frame, int form,
*                                const double centre[], const double point[],
*                                AstRegion *unc )

*  Class Membership:
*     Circle initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new Circle object. It allocates memory (if necessary) to accommodate
*     the Circle plus any additional data associated with the derived class.
*     It then initialises a Circle structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a Circle at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the Circle is to be initialised.
*        This must be of sufficient size to accommodate the Circle data
*        (sizeof(Circle)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the Circle (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the Circle
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the Circle's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new Circle.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the astGetClass
*        method).
*     frame
*        A pointer to the Frame in which the region is defined.
*     form
*        Indicates how the "point" parameter should be interpreted.
*        Should be either 0 or 1.
*     centre
*        An array of double, with one element for each Frame axis (Naxes
*        attribute) containing the coordinates of the circle centre.
*     point
*        If "form" is zero, this should be an array of double, with one
*        element for each Frame axis (Naxes attribute) containing the
*        coordinates at any point on the circumference. If "form" is one,
*        this should be the address of a double containing the scalar
*        radius of the circle or sphere.
*     unc
*        A pointer to a Region which specifies the uncertainty in the
*        supplied positions (all points on the boundary of the new Circle
*        being initialised are assumed to have the same uncertainty). A NULL
*        pointer can be supplied, in which case default uncertainties equal to
*        1.0E-6 of the dimensions of the new Circle's bounding box are used.
*        If an uncertainty Region is supplied, it must be either a Box, a
*        Circle or an Ellipse, and its encapsulated Frame must be related
*        to the Frame supplied for parameter "frame" (i.e. astConvert
*        should be able to find a Mapping between them). Two positions
*        the "frame" Frame are considered to be co-incident if their
*        uncertainty Regions overlap. The centre of the supplied
*        uncertainty Region is immaterial since it will be re-centred on the
*        point being tested before use. A deep copy is taken of the supplied
*        Region.

*  Returned Value:
*     A pointer to the new Circle.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstCircle *new;           /* Pointer to new Circle */
   AstPointSet *pset;        /* PointSet to pass to Region initialiser */
   const double *circum;     /* Pointer to circumference position */
   double **ptr;             /* Pointer to coords data in pset */
   int i;                    /* Axis index */
   int nc;                   /* No. of axes */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitCircleVtab( vtab, name );

/* Initialise. */
   new = NULL;

/* Check the supplied value for "form" is legal. */
   if( form != 0 && form != 1 && astOK ) {
      astError( AST__BADIN, "astInitCircle(%s): The value supplied for "
                "parameter \"form\" (%d) is illegal - it should be 0 or 1 "
                "(programming error).", status, name, form );
   }

/* Get the number of axis values required for each position. */
   nc = astGetNaxes( frame );

/* If the circle radius has been supplied, find a point on the circle
   circumference. */
   if( form == 1 ) {
      circum = CircumPoint( frame, nc, centre, *point, status );

/* Otherwise, use the supplied circumference point. */
   } else {
      circum = point;
   }

/* Create a PointSet to hold the centre position, and a point on the
   circumference, and get points to the data arrays. */
   pset = astPointSet( 2, nc, "", status );
   ptr = astGetPoints( pset );

/* Copy the centre and circumference coordinates into the PointSet, checking
   that no bad values have been supplied. */
   for( i = 0; astOK && i < nc; i++ ) {
      if( centre[ i ] == AST__BAD ) {
         astError( AST__BADIN, "astInitCircle(%s): The value of axis %d is "
                   "undefined at the circle centre.", status, name, i + 1 );
      }
      if( astOK && circum[ i ] == AST__BAD ) {
         astError( AST__BADIN, "astInitCircle(%s): The value of axis %d is "
                   "undefined on the circumference of the circle.", status, name, i + 1 );
      }
      ptr[ i ][ 0 ] = centre[ i ];
      ptr[ i ][ 1 ] = circum[ i ];
   }

/* Check pointers can be used safely. */
   if( astOK ) {

/* Initialise a Region structure (the parent class) as the first component
   within the Circle structure, allocating memory if necessary. */
      new = (AstCircle *) astInitRegion( mem, size, 0, (AstRegionVtab *) vtab,
                                         name, frame, pset, unc );

      if ( astOK ) {

/* Initialise the Circle data. */
/* ------------------------ */
         new->stale = 1;
         new->centre = NULL;
         new->lb = NULL;
         new->ub = NULL;
         Cache( new, status );

/* If an error occurred, clean up by deleting the new Circle. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Free resources. */
   pset = astAnnul( pset );
   if( form == 1 ) circum = astFree( (void *) circum );

/* Return a pointer to the new Circle. */
   return new;
}

AstCircle *astLoadCircle_( void *mem, size_t size, AstCircleVtab *vtab,
                           const char *name, AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadCircle

*  Purpose:
*     Load a Circle.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "circle.h"
*     AstCircle *astLoadCircle( void *mem, size_t size, AstCircleVtab *vtab,
*                               const char *name, AstChannel *channel )

*  Class Membership:
*     Circle loader.

*  Description:
*     This function is provided to load a new Circle using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     Circle structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a Circle at the start of the memory
*     passed via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory into which the Circle is to be
*        loaded.  This must be of sufficient size to accommodate the
*        Circle data (sizeof(Circle)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the Circle (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the Circle structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstCircle) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new Circle. If this is NULL, a pointer
*        to the (static) virtual function table for the Circle class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "Circle" is used instead.

*  Returned Value:
*     A pointer to the new Circle.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstCircle *new;              /* Pointer to the new Circle */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this Circle. In this case the
   Circle belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstCircle );
      vtab = &class_vtab;
      name = "Circle";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitCircleVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built Circle. */
   new = astLoadRegion( mem, size, (AstRegionVtab *) vtab, name,
                        channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "Circle" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* There are no values to read. */
/* ---------------------------- */

/* Cache intermediate results in the Circle structure */
      new->centre = NULL;
      new->lb = NULL;
      new->ub = NULL;
      new->stale = 1;
      Cache( new, status );

/* If an error occurred, clean up by deleting the new Circle. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new Circle pointer. */
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


void astCirclePars_( AstCircle *this, double *centre, double *radius,
                     double *p1, int *status ){
   if ( !astOK ) return;
   (**astMEMBER(this,Circle,CirclePars))( this, centre, radius,
                                          p1, status );
}






