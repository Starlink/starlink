/*
*class++
*  Name:
*     Ellipse

*  Purpose:
*     An elliptical region within a 2-dimensional Frame.

*  Constructor Function:
c     astEllipse
f     AST_ELLIPSE

*  Description:
*     The Ellipse class implements a Region which represents a ellipse
*     within a 2-dimensional Frame.

*  Inheritance:
*     The Ellipse class inherits from the Region class.

*  Attributes:
*     The Ellipse class does not define any new attributes beyond
*     those which are applicable to all Regions.

*  Functions:
c     In addition to those functions applicable to all Regions, the
c     following functions may also be applied to all Ellipses:
f     In addition to those routines applicable to all Regions, the
f     following routines may also be applied to all Ellipses:
*
c     - astEllipsePars: Get the geometric parameters of the Ellipse
f     - AST_ELLIPSEPARS: Get the geometric parameters of the Ellipse

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
*     7-SEP-2004 (DSB):
*        Original version.
*     4-NOV-2013 (DSB):
*        Modify RegPins so that it can handle uncertainty regions that straddle
*        a discontinuity. Previously, such uncertainty Regions could have a huge
*        bounding box resulting in matching region being far too big.
*     6-JAN-2014 (DSB):
*        Ensure cached information is available in RegCentre even if no new
*        centre is supplied.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS Ellipse

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
#include "circle.h"              /* Interface definition for circle class */
#include "ellipse.h"             /* Interface definition for this class */
#include "mapping.h"             /* Position mappings */
#include "unitmap.h"             /* Unit Mapping */
#include "pal.h"                 /* Positional astronomy library */

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
astMAKE_INITGLOBALS(Ellipse)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(Ellipse,Class_Init)
#define class_vtab astGLOBAL(Ellipse,Class_Vtab)


#include <pthread.h>


#else


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstEllipseVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstEllipse *astEllipseId_( void *, int, const double[2], const double[2], const double[2], void *, const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstMapping *Simplify( AstMapping *, int * );
static AstPointSet *RegBaseMesh( AstRegion *, int * );
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static double *RegCentre( AstRegion *this, double *, double **, int, int, int * );
static int RegPins( AstRegion *, AstPointSet *, AstRegion *, int **, int * );
static int RegTrace( AstRegion *, int, double *, double **, int * );
static void Cache( AstEllipse *, int * );
static void CalcPars( AstFrame *, double[2], double[2], double[2], double *, double *, double *, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void EllipsePars( AstEllipse *, double[2], double *, double *, double *, double[2], double[2], int * );
static void RegBaseBox( AstRegion *this, double *, double *, int * );
static void ResetCache( AstRegion *this, int * );
static void SetRegFS( AstRegion *, AstFrame *, int * );

/* Member functions. */
/* ================= */

AstRegion *astBestEllipse_( AstPointSet *mesh, double *cen, AstRegion *unc, int *status ){
/*
*+
*  Name:
*     astBestEllipse

*  Purpose:
*     Find the best fitting Ellipse through a given mesh of points.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "ellipse.h"
*     AstRegion *astBestEllipse( AstPointSet *mesh, double *cen, AstRegion *unc )

*  Class Membership:
*     Ellipse member function

*  Description:
*     This function finds the best fitting Ellipse through a given mesh of
*     points. Ellispes are always 2-dimensional.

*  Parameters:
*     mesh
*        Pointer to a PointSet holding the mesh of points. They are
*        assumed to be in the Frame represented by "unc".
*     cen
*        Pointer to an array holding the coordinates of the new Ellipse
*        centre.
*     unc
*        A Region representing the uncertainty associated with each point
*        on the mesh.

*  Returned Value:
*     Pointer to the best fitting Ellipse. It will inherit the positional
*     uncertainty and Frame represented by "unc".

*  Implementation Deficiencies:
*    - The method used by this function is not very accurate, and assumes
*    that the supplied mesh provides uniform coverage of the entire ellipse.

*  Notes:
*    - A NULL pointer is returned if an error has already occurred, or if
*    this function should fail for any reason.

*-
*/

/* Local Variables: */
   AstFrame *frm;
   AstPointSet *ps2;
   AstRegion *result;
   double **ptr2;
   double **ptr;
   double *ang;
   double *dist;
   double *px;
   double *py;
   double a0;
   double a;
   double aa[2];
   double at;
   double b;
   double c0;
   double c1;
   double c2;
   double c;
   double d;
   double den;
   double e;
   double f;
   double mn;
   double mx;
   double p[2];
   double pa[2];
   double pb[2];
   double r1;
   double r2;
   double r3;
   double smn;
   double t1;
   double t2;
   double t3;
   int ip;
   int maxat;
   int np;
   double sw;

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get no. of points in the mesh. */
   np = astGetNpoint( mesh );

/* Get pointers to the axis values. */
   ptr = astGetPoints( mesh );

/* Allocate work space */
   dist = astMalloc( sizeof( double )*(size_t) np );
   ang = astMalloc( sizeof( double )*(size_t) np );

/* Get a pointer to the Frame represented by "unc". This is the Frame to
   which the supplied mesh points refer. */
   frm = astGetFrame( unc->frameset, AST__CURRENT );

/* Check pointers can be used safely */
   if( astOK ) {

/* Find the first mesh point which is at a non-zero distance from the
   centre. */
      px = ptr[ 0 ];
      py = ptr[ 1 ];
      for( ip = 0; ip < np; ip++, px++, py++ ) {
         p[ 0 ] = *px;
         p[ 1 ] = *py;
         dist[ ip ] = astDistance( frm, cen, p );
         if( dist[ ip ] != AST__BAD && dist[ ip ] != 0.0 ) {
            break;
         } else {
            ang[ ip ] = AST__BAD;
            dist[ ip ] = AST__BAD;
         }
      }

/* Find a point which is this distance away from the centre along the second
   axis. This point is used to define zero angle when calling astAngle
   below. */
      astOffset2( frm, cen, 0.0, dist[ ip ], aa );
      ang[ ip ] = astAngle( frm, aa, cen, p );

/* Get the distance from the centre to each of the remaining mesh points. Also
   find the orientation of the radial lines through the centre to each mesh
   point. At the same time, find the index of the point with the largest
   radial distance. */
      maxat = ip;
      r2 = dist[ maxat ];
      ip++;
      px++;
      py++;
      for( ; ip < np; ip++, px++, py++ ) {
         p[ 0 ] = *px;
         p[ 1 ] = *py;
         dist[ ip ] = astDistance( frm, cen, p );
         ang[ ip ] = astAngle( frm, aa, cen, p );
         if( dist[ ip ] != AST__BAD && dist[ ip ] > r2 ) {
            r2 = dist[ ip ];
            maxat = ip;
         }
      }

/* Find the higher index neighbouring point, wrapping back to the start
   of the list when the end is reached. Note the radius and position angle
   at this neighbouring point. */
      t2 = 0.0;
      r3 = AST__BAD;
      t3 = AST__BAD;
      a0 = ang[ maxat ];
      for( ip = maxat + 1; ip < np; ip++ ) {
         if( dist[ ip ] != AST__BAD ) {
            r3 = dist[ ip ];
            t3 = palDrange( ang[ ip ] - a0 );
            break;
         }
      }
      if( r3 == AST__BAD ) {
         for( ip = 0; ip < maxat; ip++ ) {
            if( dist[ ip ] != AST__BAD ) {
               r3 = dist[ ip ];
               t3 = palDrange( ang[ ip ] - a0 );
               break;
            }
         }
      }

/* Find the lower index neighbouring point, wrapping back to the end
   of the list when the start is reached. Note the radius and position angle
   at this neighbouring point. */
      r1 = AST__BAD;
      t1 = AST__BAD;
      for( ip = maxat - 1; ip > -1; ip-- ) {
         if( dist[ ip ] != AST__BAD ) {
            r1 = dist[ ip ];
            t1 = palDrange( ang[ ip ] - a0 );
            break;
         }
      }
      if( r1 == AST__BAD ) {
         for( ip = np - 1; ip > maxat; ip-- ) {
            if( dist[ ip ] != AST__BAD ) {
               r1 = dist[ ip ];
               t1 = palDrange( ang[ ip ] - a0 );
               break;
            }
         }
      }

/* Fit a quadratic through the three pairs of (radius,angle) values. The
   centre point (r2,t2) is the point which is furthest from the centre,
   and the other two are the neighbouring points found above. */
      a = r2 - r1;
      b = t2 - t1;
      c = t2*t2 - t1*t1;
      d = r3 - r2;
      e = t3 - t2;
      f = t3*t3 - t2*t2;

      den = c*e - b*f;
      if( den != 0.0 ) {

/* The co-efficients of the interpolating polynomial... */
         c1 = ( d*c - a*f )/den;
         c2 = ( a*e - d*b )/den;
         c0 = r1 - c1*t1 - c2*t1*t1;

/* Find the largest radius (the turning point of the quadratic), and the
   angle at which it occurs. */
         if( c2 < 0.0 ) {
            mx = ( 4*c0*c2 - c1*c1 )/( 4*c2 );
            at = a0 - c1/( 2*c2 );
         } else {
            mx = r2;
            at = a0 - t2;
         }

/* This point is the end of the ellipse primary axis. Find its (x,y)
   coords, and store in "pa". */
         astOffset2( frm, cen, at, mx, pa );

/* Resolve all the supplied points into components parallel and
   perpendicular to the line joining the centre and "pa". */
         ps2 = astResolvePoints( frm, cen, pa, mesh, NULL );
         ptr2 = astGetPoints( ps2 );
         if( astOK ) {

/* For each other mesh point, work out the length of the secondary
   axis which would result if we used that point to define the ellipse.
   Find the mean of these secondary axis lengths, weighted by the length
   of the y component to reduce influence of poor conditioning at very
   low y. */
            smn = 0.0;
            sw = 0.0;
            px = ptr2[ 0 ];
            py = ptr2[ 1 ];
            for( ip = 0; ip < np; ip++, px++, py++ ) {
               if( *px != AST__BAD && *py != AST__BAD ) {
                  den = mx*mx - (*px)*(*px);
                  if( den > 0.0 ) {
                     smn += fabs( mx*(*py)*(*py) )/sqrt( den );
                     sw += fabs( *py );
                  }
               }
            }

            if( sw > 0 ) {
               mn = smn/sw;

/* Find the coords at the end of the mean secondary axis. */
               astOffset2( frm, cen, at + AST__DPIBY2, mn, pb );

/* Create the Ellipse to return. */
               result = (AstRegion *) astEllipse( frm, 0, cen, pa, pb, unc, "", status );
            }
         }

/* Free resources. */
         ps2 = astAnnul( ps2 );

      }
   }

   dist = astFree( dist );
   ang = astFree( ang );
   frm = astAnnul( frm );

/* Return NULL if anything went wrong. */
   if( !astOK ) result = astAnnul( result );

/* Return the result.*/
   return result;
}

void astInitEllipseVtab_(  AstEllipseVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitEllipseVtab

*  Purpose:
*     Initialise a virtual function table for a Ellipse.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "ellipse.h"
*     void astInitEllipseVtab( AstEllipseVtab *vtab, const char *name )

*  Class Membership:
*     Ellipse vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the Ellipse class.

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
   will be used (by astIsAEllipse) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstRegionVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */
   vtab->EllipsePars = EllipsePars;

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
   region->RegCentre = RegCentre;
   region->RegTrace = RegTrace;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */

/* Declare the copy constructor, destructor and class dump
   functions. */
   astSetDelete( vtab, Delete );
   astSetCopy( vtab, Copy );
   astSetDump( vtab, Dump, "Ellipse", "Elliptical region" );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
}

static void Cache( AstEllipse *this, int *status ){
/*
*  Name:
*     Cache

*  Purpose:
*     Calculate intermediate values and cache them in the Ellipse structure.

*  Type:
*     Private function.

*  Synopsis:
*     #include "ellipse.h"
*     void Cache( AstEllipse *this, int *status )

*  Class Membership:
*     Ellipse member function

*  Description:
*     This function uses the PointSet stored in the parent Region to calculate
*     some intermediate values which are useful in other methods. These
*     values are stored within the Ellipse structure.

*  Parameters:
*     this
*        Pointer to the Ellipse.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstFrame *frm;       /* Pointer to base Frame in Ellipse */
   double **ptr;        /* Pointer to data in the encapsulated PointSet */
   double *centre;      /* Array holding centre coords */
   double *point1;      /* Array holding coords at end of primary axis */
   double *point2;      /* Array holding coords at another point on ellipse */
   double a;            /* The half-length of the primary axis */
   double angle;        /* Orientation of primary axis */
   double b;            /* The half-length of the secondary axis */
   int i;               /* Axis index */

/* Check the global error status. */
   if ( !astOK ) return;

/* Do Nothing if the cached information is up to date. */
   if( this->stale ) {

/* Get a pointer to the base Frame. */
      frm = astGetFrame( ((AstRegion *) this)->frameset, AST__BASE );

/* Allocate memory. */
      centre = (double *) astMalloc( sizeof( double )*2 );
      point1 = (double *) astMalloc( sizeof( double )*2 );
      point2 = (double *) astMalloc( sizeof( double )*2 );

/* Get pointers to the coordinate data in the parent Region structure. */
      ptr = astGetPoints( ((AstRegion *) this)->points );

/* Check pointers can be used safely. */
      if( astOK ) {

/* Copy the points in to the allocated memory. */
         for( i = 0; i < 2; i++ ) {
            centre[ i ] = ptr[ i ][ 0 ];
            point1[ i ] = ptr[ i ][ 1 ];
            point2[ i ] = ptr[ i ][ 2 ];
         }

/* Calculate the geometric parameters of the ellipse. */
         CalcPars( frm, centre, point1, point2, &a, &b, &angle, status );

/* Check the returned values. */
         if( a <= 0.0 || a == AST__BAD || b <= 0.0 || b == AST__BAD ) {
            if( astOK ) astError( AST__BADIN, "astInitEllipse(%s): The "
                                  "supplied points do not determine an "
                                  "ellipse.", status, astGetClass( this ) );
         }

/* Store useful things in the Ellipse structure. */
         if( astOK ) {
           astFree( this->centre );
           this->centre = centre;
           centre = NULL;

           astFree( this->point1 );
           this->point1 = point1;
           point1 = NULL;

           this->a = a;
           this->b = b;
           this->angle = angle;
         }
      }

/* Initialise the bounding box. This is set properly when the astRegBaseMesh
   function is called. These variables should not be used unless the
   "basemesh" component of the parent Region structure is set to a non-null
   value. */
      this->lbx = -DBL_MAX;
      this->ubx = DBL_MAX;
      this->lby = -DBL_MAX;
      this->uby = DBL_MAX;

/* Free resources */
      frm = astAnnul( frm );
      if( centre ) centre = astFree( centre );
      if( point1 ) point1 = astFree( point1 );
      point2 = astFree( point2 );

/* Indicate cached information is up to date. */
      this->stale = 0;

   }
}

static void CalcPars( AstFrame *frm, double centre[2], double point1[2],
                      double point2[2], double *a, double *b,
                      double *angle, int *status ){
/*
*  Name:
*     CalcPars

*  Purpose:
*     Calculate ellipse parameters.

*  Type:
*     Private function.

*  Synopsis:
*     #include "ellipse.h"
*     void CalcPars( AstFrame *frm, double centre[2], double point1[2],
*                    double point2[2], double *a, double *b, double *angle,
*                    int *status )

*  Class Membership:
*     Ellipse member function

*  Description:
*     This function uses the supplied positions to calculate the
*     geometric parameters of an ellipse.

*  Parameters:
*     frm
*        Pointer to the Frame in which the positions are defined.
*     centre;
*        Array holding centre coords.
*     point1
*        Array holding coords at end of primary axis
*     point2
*        Array holding coords at another point on ellipse. On exit it
*        holds the coords at the end of the secondary axis.
*     a
*        Pointer to location at which to store the half-length of the
*        primary axis.
*     b
*        Pointer to location at which to store the half-length of the
*        secondary axis.
*     angle
*        Pointer to location at which to store the angle from the
*        positive direction of the second Frame axis to the primary
*        ellipse axis, in radians. Rotation from the second to the first
*        Frame axis is positive.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   double point3[ 2 ];  /* Array holding a point on the primary axis */
   double x;            /* The offset parallel to the primary axis */
   double y;            /* The offset perpendicular to the primary axis */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get the geodesic distance between the centre and point 1 (the end of
   the primary axis of the ellipse). This is the half length of the
   primary axis of the ellipse (the axis which joins the centre position to
   point 1). */
   *a = astDistance( frm, centre, point1 );

/* Find the point (point3) on the primary axis which is closest to point 2,
   and thus get the geodesic offsets (resolved parallel and perpendicular to
   the primary axis) between the centre and point 2. */
   if( *a > 0.0 ) {
      astResolve( frm, centre, point1, point2, point3, &x, &y );

/* Find the half-length of the secondary ellipse axis. */
      if( astOK ) {
         *b = (*a)*(*a) - x*x;
         if( *b > 0.0 ) *b = (*a)*y/sqrt( *b );
      } else {
         *b = *a;
      }

/* Find the angle from the positive direction of the second axis to the
   primary ellipse axis. */
      point3[ 0 ] = centre[ 0 ];
      point3[ 1 ] = centre[ 1 ] + fabs( 0.1*(*a) );
      *angle = astAngle( frm, point3, centre, point1 );

/* Find the end point of the secondary axis. */
      (void) astOffset2( frm, centre, *angle + AST__DPIBY2, *b, point2 );
   }
}

static void EllipsePars( AstEllipse *this, double centre[2], double *a,
                         double *b, double *angle, double p1[2],
                         double p2[2], int *status ){
/*
*++
*  Name:
c     astEllipsePars
f     AST_ELLIPSEPARS

*  Purpose:
*     Returns the geometric parameters of an Ellipse.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "ellipse.h"
c     void astEllipsePars( AstEllipse *this, double centre[2], double *a,
c                          double *b, double *angle, double p1[2], double p2[2] )
f     CALL AST_ELLIPSEPARS( THIS, CENTRE, A, B, ANGLE, P1, P2, STATUS )

*  Class Membership:
*     Region method.

*  Description:
c     This function
f     This routine
*     returns the geometric parameters describing the supplied ellipse.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Region.
c     centre
f     CENTRE( 2 ) = DOUBLE PRECISION (Returned)
*        The coordinates of the Ellipse centre are returned in this arrays.
c     a
f     A = DOUBLE PRECISION (Returned)
*        Returned holding the half-length of the first axis of the
*        ellipse.
c     b
f     B = DOUBLE PRECISION (Returned)
*        Returned holding the half-length of the second axis of the
*        ellipse.
c     angle
f     ANGLE = DOUBLE PRECISION (Returned)
*        If the coordinate system in which the Ellipse is defined has
*        axes (X,Y), then
c        "*angle"
f        ANGLE
*        is returned holding the angle from the positive direction of
*        the Y axis to the first axis of the ellipse, in radians.
*        Positive rotation is in the same sense as rotation from the
*        positive direction of Y to the positive direction of X.
c     p1
f     P1( 2 ) = DOUBLE PRECISION (Returned)
*        An array in which to return the coordinates at one of the two ends
*        of the first axis  of the ellipse.
c        A NULL pointer can be supplied if these coordinates are not needed.
c     p2
f     P2( 2 ) = DOUBLE PRECISION (Returned)
*        An array in which to return the coordinates at one of the two ends
*        of the second axis  of the ellipse.
c        A NULL pointer can be supplied if these coordinates are not needed.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
*     - If the coordinate system represented by the Ellipse has been
*     changed since it was first created, the returned parameters refer
*     to the new (changed) coordinate system, rather than the original
*     coordinate system. Note however that if the transformation from
*     original to new coordinate system is non-linear, the shape
*     represented by the supplied Ellipse object may not be an accurate
*     ellipse.
*     - Values of AST__BAD are returned for the parameters without error
*     if the ellipse is degenerate or undefined.
*--
*/

/* Local Variables: */
   AstFrame *frm;           /* Current Frame represented by the Ellipse */
   AstPointSet *pset;       /* PointSet holding PointList axis values */
   AstRegion *this_region;  /* Parent Region pointer */
   double **ptr;            /* Pointer to axes values in the PointList */
   double *point1;          /* Pointer to "p1" or "buf1" */
   double *point2;          /* Pointer to "p2" or "buf2" */
   double buf1[2];          /* Local substitute array for "p1" */
   double buf2[2];          /* Local substitute array for "p2" */
   int i;                   /* Axis index */

/* Check the inherited status. */
   if( !astOK ) return;

/* Store a pointer to the parent region structure. */
   this_region = (AstRegion *) this;

/* Transform the base Frame axis values into the current Frame. */
   pset = astTransform( this_region->frameset, this_region->points, 1, NULL );

/* Get pointers to the coordinate data. */
   ptr = astGetPoints( pset );

/* Choose the arrays to use - supplied arrays if possible, local arrays
   otherwise. */
   if( p1 ) {
      point1 = p1;
   } else {
      point1 = buf1;
   }
   if( p2 ) {
      point2 = p2;
   } else {
      point2 = buf2;
   }

/* Check pointers can be used safely. */
   if( astOK ) {

/* Copy the points in to separate arrays. */
      for( i = 0; i < 2; i++ ) {
         centre[ i ] = ptr[ i ][ 0 ];
         point1[ i ] = ptr[ i ][ 1 ];
         point2[ i ] = ptr[ i ][ 2 ];
      }

/* Get the Ellipse frame. */
      frm = astGetFrame( this_region->frameset, AST__CURRENT );

/* Calculate the geometric parameters of the ellipse. */
      CalcPars( frm, centre, point1, point2, a, b, angle, status );

/* Ensure no zero values are returned. */
      if( *a <= 0.0  || *b <= 0.0 ) {
         *a = AST__BAD;
         *b = AST__BAD;
         *angle = AST__BAD;
      }

/* Free resources */
      frm = astAnnul( frm );
   }
   pset = astAnnul( pset );
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
*     #include "ellipse.h"
*     void RegBaseBox( AstRegion *this, double *lbnd, double *ubnd, int *status )

*  Class Membership:
*     Ellipse member function (over-rides the astRegBaseBox protected
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
   AstEllipse *this;             /* Pointer to Ellipse structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the Ellipse structure */
   this = (AstEllipse *) this_region;

/* The bounding box of the mesh returned by astRegBaseMesh is used as the
   bounding box of the Ellipse. These bounds are cached in the Ellipse
   structure by astRegBaseMesh. Ensure astRegBaseMesh has been invoked,
   so that it is safe to use the cached bounding box. */
   if( !this_region->basemesh ) (void) astAnnul( astRegBaseMesh( this ) );

/* Store the bounding box. */
   lbnd[ 0 ] = this->lbx;
   ubnd[ 0 ] = this->ubx;
   lbnd[ 1 ] = this->lby;
   ubnd[ 1 ] = this->uby;

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
*     #include "ellipse.h"
*     AstPointSet *astRegBaseMesh( AstRegion *this, int *status )

*  Class Membership:
*     Ellipse member function (over-rides the astRegBaseMesh protected
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

/* Local Constants: */
#define NP_EDGE 50                /* No. of points for determining geodesic */

/* Local Variables: */
   AstEllipse *this;              /* The Ellipse structure */
   AstFrame *frm;                 /* Base Frame in encapsulated FrameSet */
   AstPointSet *result;           /* Returned pointer */
   AstRegion *reg;                /* Copy of supplied Ellipse */
   double **ptr;                  /* Pointers to data */
   double ang;                    /* Position angular of primary axis at "dx" */
   double angle;                  /* Ellipse parametric angle at point */
   double delta;                  /* Angular separation of points */
   double dist;                   /* Offset along an axis */
   double dx;                     /* Primary axis offset */
   double dy;                     /* Secondary axis offset */
   double lbnd[2];                /* Lower bounding box bounds */
   double lbx;                    /* Lower x bound of mesh bounding box */
   double lby;                    /* Lower y bound of mesh bounding box */
   double p2[ 2 ];                /* Position in 2D Frame */
   double p[ 2 ];                 /* Position in 2D Frame */
   double ubnd[2];                /* Upper bounding box bounds */
   double ubx;                    /* Upper x bound of mesh bounding box */
   double uby;                    /* Upper y bound of mesh bounding box */
   int i;                         /* Point index */
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

/* Initialise the bounding box of the mesh points. */
      lbx = DBL_MAX;
      ubx = -DBL_MAX;
      lby = DBL_MAX;
      uby = -DBL_MAX;

/* Get a pointer to the Ellipse structure. */
      this = (AstEllipse *) this_region;

/* Ensure cached information is available. */
      Cache( this, status );

/* Get a pointer to the base Frame in the encapsulated FrameSet. */
      frm = astGetFrame( this_region->frameset, AST__BASE );

/* Get the requested number of points to put on the mesh. */
      np = astGetMeshSize( this );

/* Store the angular increment between points. */
      delta = 2*AST__DPI/np;

/* Create a suitable PointSet to hold the returned positions. */
      result = astPointSet( np, 2, "", status );
      ptr = astGetPoints( result );
      if( astOK ) {

/* Loop round each point. The angle is the parametric angle, phi, where
   the ellipse is defined by:

   dx = a.cos( phi )
   dy = a.sin( phi )

   measured from the primary ellipse. Positive in the sense of rotation from
   axis 2 to axis 1.  */
         angle = 0.0;
         for( i = 0; i < np; i++ ) {

/* Find the offsets from the centre. "dx" is geodesic distance along the
   primary axis, and dy is geodesic distance along the secondary axis. */
            dx = this->a*cos( angle );
            dy = this->b*sin( angle );

/* Now find the point which corresponds to this dx and dy, taking account
   of the potential spherical geometry of hte coordinate system. First
   move a distance "dx" from the centre along the primary axis. The
   function value returned is the direction of the geodesic curve at the
   end point. That is, the angle (in radians) between the positive direction
   of the second axis and the continuation of the geodesic curve at the
   requested end point. */
            ang = astOffset2( frm, this->centre, this->angle, dx, p );

/* Now move a distance "dy" from the point found above at right angles to
   the primary axis. */
            astOffset2( frm, p, ang + AST__DPIBY2, dy, p2 );

/* Store the resulting axis values. */
            ptr[ 0 ][ i ] = p2[ 0 ];
            ptr[ 1 ][ i ] = p2[ 1 ];

/* Update the bounds of the mesh bounding box. The box is expressed in
   terms of axis offsets from the centre, in order to avoid problems with
   boxes that cross RA=0 or RA=12h */
            if( p2[ 0 ] != AST__BAD && p2[ 1 ] != AST__BAD ){

               dist =  astAxDistance( frm, 1, this->centre[ 0 ], p2[ 0 ] );
               if( dist < lbx ) {
                  lbx = dist;
               } else if( dist > ubx ) {
                  ubx = dist;
               }

               dist =  astAxDistance( frm, 1, this->centre[ 1 ], p2[ 1 ] );
               if( dist < lby ) {
                  lby = dist;
               } else if( dist > uby ) {
                  uby = dist;
               }
            }

/* Increment the angular position of the next mesh point. */
            angle += delta;
         }
      }

/* Save the returned pointer in the Region structure so that it does not
   need to be created again next time this function is called. Also cache
   the bounding box in the Ellipse structure. */
      if( astOK && result ) {
         this_region->basemesh = astClone( result );

/* Extend the bounding box if it contains any singularies. The astNormBox
   requires a Mapping which can be used to test points in the base Frame.
   Create a copy of the Circle and then set its FrameSet so that the current
   Frame in the copy is the same as the base Frame in the original. */
         reg = astCopy( this );
         astSetRegFS( reg, frm );
         astSetNegated( reg, 0 );

/* Normalise this box. */
         lbnd[ 0 ] = this->centre[ 0 ] + lbx;
         lbnd[ 1 ] = this->centre[ 1 ] + lby;
         ubnd[ 0 ] = this->centre[ 0 ] + ubx;
         ubnd[ 1 ] = this->centre[ 1 ] + uby;
         astNormBox( frm, lbnd, ubnd, reg );

/* Save this box */
         this->lbx = lbnd[ 0 ];
         this->ubx = ubnd[ 0 ];
         this->lby = lbnd[ 1 ];
         this->uby = ubnd[ 1 ];

/* Free resources. */
         reg = astAnnul( reg );
      }
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
*     #include "ellipse.h"
*     double *RegCentre( AstRegion *this, double *cen, double **ptr,
*                        int index, int ifrm, int *status )

*  Class Membership:
*     Ellipse member function (over-rides the astRegCentre protected
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
*        Pointer to an array of points, one for each axis in the Region.
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
   AstEllipse *this;   /* Pointer to Ellipse structure */
   AstFrame *frm;      /* Base Frame */
   double **rptr;      /* Data pointers for Region PointSet */
   double *bc;         /* Base Frame centre position */
   double *result;     /* Returned pointer */
   double *tmp;        /* Temporary array pointer */
   double a[ 2 ];      /* Original position */
   double angle;       /* Orietentation of offset from old to new centre */
   double axval;       /* Axis value */
   double b[ 2 ];      /* New position */
   double dist;        /* Distance from old to new centre */
   double newcen[ 2 ]; /* New centre */
   int ic;             /* Coordinate index */
   int ip;             /* Position index */
   int ncb;            /* Number of base frame coordinate values per point */
   int ncc;            /* Number of current frame coordinate values per point */

/* Initialise */
   result = NULL;

/* Check the local error status. */
   if ( !astOK ) return result;

/* Get a pointer to the Ellipse structure. */
   this = (AstEllipse *) this_region;

/* Ensure cached information is available. */
   Cache( this, status );

/* Get the number of axis values per point in the current Frame. */
   ncc = astGetNout( this_region->frameset );

/* An ellipse always has 2 base frame axes. */
   ncb = 2;

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

/* If the centre position was supplied in the base Frame, use the
   supplied "cen" or "ptr" pointer directly to change the coords in the
   parent Region structure and the cached coords in the Ellipse structure. */
         } else {
            bc = newcen;
            for( ic = 0; ic < ncb; ic++ ) {
               axval = cen ? cen[ ic ] : ptr[ ic ][ index ];
               newcen[ ic ] = ( axval != AST__BAD ) ? axval : this->centre[ ic ];
            }
         }

/* Find the direction and length of the offset between the old and new
   centre. */
         frm = astGetFrame( this_region->frameset, AST__BASE );
         angle = astAxAngle( frm, this->centre, bc, 2 );
         dist = astDistance( frm, this->centre, bc );

/* Shift each point in the parent Region structure by the same length and
   direction. */
         for( ip = 0; ip < 3; ip++ ) {
            a[ 0 ] = rptr[ ip ][ 0 ];
            a[ 1 ] = rptr[ ip ][ 1 ];
            astOffset2( frm, a, angle, dist, b );
            rptr[ ip ][ 0 ] = b[ 0 ];
            rptr[ ip ][ 1 ] = b[ 1 ];
         }

/* Indicate that the cache is stale. */
         astResetCache( this );

/* Free resources */
         frm = astAnnul( frm );
         if( bc != newcen ) bc = astFree( bc );
      }
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
*     Check if a set of points fall on the boundary of a given Ellipse.

*  Type:
*     Private function.

*  Synopsis:
*     #include "ellipse.h"
*     int RegPins( AstRegion *this, AstPointSet *pset, AstRegion *unc,
*                  int **mask, int *status )

*  Class Membership:
*     Ellipse member function (over-rides the astRegPins protected
*     method inherited from the Region class).

*  Description:
*     This function returns a flag indicating if the supplied set of
*     points all fall on the boundary of the given Ellipse.
*
*     Some tolerance is allowed, as specified by the uncertainty Region
*     stored in the supplied Ellipse "this", and the supplied uncertainty
*     Region "unc" which describes the uncertainty of the supplied points.

*  Parameters:
*     this
*        Pointer to the Ellipse.
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
   AstEllipse *large_ellipse;   /* Ellipse slightly larger than "this" */
   AstEllipse *small_ellipse;   /* Ellipse slightly smaller than "this" */
   AstEllipse *this;            /* Pointer to the Ellipse structure. */
   AstFrame *frm;               /* Base Frame in supplied Ellipse */
   AstPointSet *ps1;            /* Points masked by larger Ellipse */
   AstPointSet *ps2;            /* Points masked by larger and smaller Ellipsees */
   AstRegion *tunc;             /* Uncertainity Region from "this" */
   double **ptr;                /* Pointer to axis values in "ps2" */
   double *p;                   /* Pointer to next axis value */
   double *safe;                /* An interior point in "this" */
   double drad;                 /* Radius increment corresponding to border width */
   double l1;                   /* Length of bounding box diagonal */
   double l2;                   /* Length of bounding box diagonal */
   double lbnd_tunc[2];         /* Lower bounds of "this" uncertainty Region */
   double lbnd_unc[2];          /* Lower bounds of supplied uncertainty Region */
   double lim;                  /* Smallest semi-minor/major axis length */
   double p1[2];                /* New ellipse axis lengths */
   double ubnd_tunc[2];         /* Upper bounds of "this" uncertainty Region */
   double ubnd_unc[2];          /* Upper bounds of supplied uncertainty Region */
   int i;                       /* Axis index */
   int j;                       /* Point index */
   int np;                      /* No. of supplied points */
   int result;                  /* Returned flag */

/* Initialise */
   result = 0;
   if( mask ) *mask = NULL;

/* Check the inherited status. */
   if( !astOK ) return result;

/* Get a pointer to the Ellipse structure. */
   this = (AstEllipse *) this_region;

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

/* Get the centre of the region in the base Frame. We use this as a "safe"
   interior point within the region. */
   safe = astRegCentre( this, NULL, NULL, 0, AST__BASE );

/* We now find the maximum distance on each axis that a point can be from the
   boundary of the Ellipse for it still to be considered to be on the boundary.
   First get the Region which defines the uncertainty within the Ellipse being
   checked (in its base Frame), re-centre it on the interior point found
   above (to avoid problems if the uncertainty region straddles a
   discontinuity), and get its bounding box. */
   tunc = astGetUncFrm( this, AST__BASE );
   if( safe ) astRegCentre( tunc, safe, NULL, 0, AST__CURRENT );
   astGetRegionBounds( tunc, lbnd_tunc, ubnd_tunc );

/* Find the geodesic length within the base Frame of "this" of the diagonal of
   the bounding box. */
   frm = astGetFrame( this_region->frameset, AST__BASE );
   l1 = astDistance( frm, lbnd_tunc, ubnd_tunc );

/* Also get the Region which defines the uncertainty of the supplied
   points and get its bounding box. First re-centre the uncertainty at the
   interior position to avoid problems from uncertainties that straddle a
   discontinuity. */
   if( unc ) {
      if( safe ) astRegCentre( unc, safe, NULL, 0, AST__CURRENT );
      astGetRegionBounds( unc, lbnd_unc, ubnd_unc );

/* Find the geodesic length of the diagonal of this bounding box. */
      l2 = astDistance( frm, lbnd_unc, ubnd_unc );

/* Assume zero uncertainty if no "unc" Region was supplied. */
   } else {
      l2 = 0.0;
   }

/* Ensure cached information is available. */
   Cache( this, status );

/* The required border width is half of the total diagonal of the two bounding
   boxes. */
   if( astOK ) {
      drad = 0.5*( l1 + l2 );

/* Create two new Ellipse, one of which is larger than "this" by the amount
   found above, and the other of which is smaller than "this" by the amount
   found above. */
      p1[ 0 ] = this->a + 0.5*drad;
      p1[ 1 ] = this->b + 0.5*drad;
      large_ellipse = astEllipse( frm, 1, this->centre, p1, &(this->angle),
                                  NULL, " ", status );

      p1[ 0 ] = this->a - 0.5*drad;
      p1[ 1 ] = this->b - 0.5*drad;
      lim = 1.0E-6*drad;
      if( p1[ 0 ] < lim ) p1[ 0 ] = lim;
      if( p1[ 1 ] < lim ) p1[ 1 ] = lim;
      small_ellipse = astEllipse( frm, 1, this->centre, p1, &(this->angle),
                                  NULL, " ", status );

/* Negate the smaller region.*/
      astNegate( small_ellipse );

/* Points are on the boundary of "this" if they are inside both the large
   Ellipse and the negated small Ellipse. First transform the supplied PointSet
   using the large Ellipse, then transform them using the negated smaller
   Ellipse. */
      ps1 = astTransform( large_ellipse, pset, 1, NULL );
      ps2 = astTransform( small_ellipse, ps1, 1, NULL );

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
            for( i = 1; i < 2; i++ ) {
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
         for( i = 0; i < 2 && result; i++ ) {
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
      large_ellipse = astAnnul( large_ellipse );
      small_ellipse = astAnnul( small_ellipse );
      ps1 = astAnnul( ps1 );
      ps2 = astAnnul( ps2 );
   }

   tunc = astAnnul( tunc );
   frm = astAnnul( frm );
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
*     #include "ellipse.h"
*     int astTraceRegion( AstRegion *this, int n, double *dist, double **ptr );

*  Class Membership:
*     Ellipse member function (overrides the astTraceRegion method
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
   AstEllipse *this;
   AstFrame *frm;
   AstMapping *map;
   AstPointSet *bpset;
   AstPointSet *cpset;
   double **bptr;
   double ang;
   double angle;
   double dx;
   double dy;
   double p2[ 2 ];
   double p[ 2 ];
   int i;
   int ncur;

/* Check inherited status, and the number of points to return, returning
   a non-zero value to indicate that this class supports the astRegTrace
   method. */
   if( ! astOK || n == 0 ) return 1;

/* Get a pointer to the Ellipse structure. */
   this = (AstEllipse *) this_region;

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
   to be 2D since this is an ellipse). */
   } else {
      bpset = astPointSet( n, 2, " ", status );
      bptr = astGetPoints( bpset );
      ncur = astGetNout( map );
   }

/* Check the pointers can be used safely. */
   if( astOK ) {

/* Loop round each point. */
      for( i = 0; i < n; i++ ) {

/* The supplied scalar parameter values are the parametric angles, phi,
   where the ellipse is defined by:

   dx = a.cos( phi )
   dy = a.sin( phi )

   measured from the primary ellipse. Positive in the sense of rotation from
   axis 2 to axis 1.  */
         angle = dist[ i ]*2*AST__DPI;

/* Find the offsets from the centre. "dx" is geodesic distance along the
   primary axis, and dy is geodesic distance along the secondary axis. */
         dx = this->a*cos( angle );
         dy = this->b*sin( angle );

/* Now find the point which corresponds to this dx and dy, taking account
   of the potential spherical geometry of hte coordinate system. First
   move a distance "dx" from the centre along the primary axis. The
   function value returned is the direction of the geodesic curve at the
   end point. That is, the angle (in radians) between the positive direction
   of the second axis and the continuation of the geodesic curve at the
   requested end point. */
         ang = astOffset2( frm, this->centre, this->angle, dx, p );

/* Now move a distance "dy" from the point found above at right angles to
   the primary axis. */
         astOffset2( frm, p, ang + AST__DPIBY2, dy, p2 );

/* Store the resulting axis values. */
         bptr[ 0 ][ i ] = p2[ 0 ];
         bptr[ 1 ][ i ] = p2[ 1 ];
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

static void ResetCache( AstRegion *this, int *status ){
/*
*  Name:
*     ResetCache

*  Purpose:
*     Clear cached information within the supplied Region.

*  Type:
*     Private function.

*  Synopsis:
*     #include "ellipse.h"
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
      ( (AstEllipse *) this )->stale = 1;
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
*     #include "ellipse.h"
*     void SetRegFS( AstRegion *this_region, AstFrame *frm, int *status )

*  Class Membership:
*     Ellipse method (over-rides the astSetRegFS method inherited from
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

/* Indicate that cached information will need to be re-calculated before
   it is next used. */
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
*     #include "ellipse.h"
*     AstMapping *Simplify( AstMapping *this, int *status )

*  Class Membership:
*     Ellipse method (over-rides the astSimplify method inherited
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
   AstPointSet *ps2;          /* Ellipse PointSet in current Frame */
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

/* We attempt to simplify the Ellipse by re-defining it within its current
   Frame. Transforming the Ellipse from its base to its current Frame may
   result in the region no longer being an ellipse. We test this by
   transforming a set of bounds on the Ellipse boundary. */
   map = astGetMapping( new->frameset, AST__BASE, AST__CURRENT );

/* Get a mesh of points covering the Ellipse in its current Frame. */
   mesh = astRegMesh( new );

/* Get the Region describing the positional uncertainty within the Ellipse in
   its current Frame. */
   unc = astGetUncFrm( new, AST__CURRENT );

/* Transform the PointSet holding the ellipse centre into the current
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

/* If so, use the new Circle in place of the original Region. */
         (void) astAnnul( new );
         new = astClone( newreg );
         simpler =1;

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

/* If so, use the new Ellipse in place of the original Region. */
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
*     Apply a Ellipse to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "ellipse.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     Ellipse member function (over-rides the astTransform protected
*     method inherited from the Mapping class).

*  Description:
*     This function takes a Ellipse and a set of points encapsulated in a
*     PointSet and transforms the points by setting axis values to
*     AST__BAD for all points which are outside the region. Points inside
*     the region are copied unchanged from input to output.

*  Parameters:
*     this
*        Pointer to the Ellipse.
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
*     match the number of axes in the Frame represented by the Ellipse.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstEllipse *this;             /* Pointer to Ellipse */
   AstFrame *frm;                /* Pointer to base Frame in FrameSet */
   AstPointSet *pset_res;        /* Pointer to PointSet holding resolved components */
   AstPointSet *pset_tmp;        /* Pointer to PointSet holding base Frame positions*/
   AstPointSet *result;          /* Pointer to output PointSet */
   double **ptr_out;             /* Pointer to output coordinate data */
   double **ptr_res;             /* Pointer to resolved components coordinate data */
   double *px;                   /* Pointer to array of primary axis components */
   double *py;                   /* Pointer to array of secondary axis components */
   double c1;                    /* Constant */
   double c2;                    /* Constant */
   double d;                     /* Elliptical distance to current point */
   int closed;                   /* Is the boundary part of the Region? */
   int coord;                    /* Zero-based index for coordinates */
   int inside;                   /* Is the point inside the Region? */
   int ncoord_out;               /* No. of coordinates per output point */
   int neg;                      /* Has the Region been negated? */
   int npoint;                   /* No. of points */
   int point;                    /* Loop counter for points */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the Ellipse structure. */
   this = (AstEllipse *) this_mapping;

/* Apply the parent mapping using the stored pointer to the Transform member
   function inherited from the parent Region class. This function validates
   all arguments and generates an output PointSet if necessary,
   containing a copy of the input PointSet. */
   result = (*parent_transform)( this_mapping, in, forward, out, status );

/* Ensure cached information is available. */
   Cache( this, status );

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

/* Resolve all the base Frame positions into components parallel to and
   perpendicular to the primary axis, relative to the ellipse centre. The
   components are returned in a new PointSet. */
   pset_res = astResolvePoints( frm, this->centre, this->point1, pset_tmp, NULL );

/* Determine the numbers of points from the component PointSet and obtain
   pointers for accessing the component and output coordinate values. */
   npoint = astGetNpoint( pset_res );
   ptr_res = astGetPoints( pset_res );
   ncoord_out = astGetNcoord( result );
   ptr_out = astGetPoints( result );

/* See if the boundary is part of the Region. */
   closed = astGetClosed( this );

/* See if the Region has been negated. */
   neg = astGetNegated( this );

/* Form some frequently needed constants. */
   c1 = 1.0/(this->a*this->a);
   c2 = 1.0/(this->b*this->b);

/* Perform coordinate arithmetic. */
/* ------------------------------ */
   if ( astOK ) {
      px = ptr_res[ 0 ];
      py = ptr_res[ 1 ];

/* Loop round each point */
      for ( point = 0; point < npoint; point++, px++, py++ ) {

/* Bad input points result in bad output points */
         if( *px == AST__BAD || *py == AST__BAD ) {
            inside = 0;

/* If the input points are good... */
         } else {

/* Find the elliptical distance from the centre to the supplied point (the
   ellipse circumference has an "elliptical distance" of 1.0 at all points).*/
            d = c1*(*px)*(*px) + c2*(*py)*(*py);

/* Now consider whether this radius value puts the point in or out of the
   Ellipse. */
            if( d != AST__BAD ){
               if( neg ) {
                  if( closed ) {
                     inside = ( d >= 1.0 );
                  } else {
                     inside = ( d > 1.0 );
                  }
               } else {
                  if( closed ) {
                     inside = ( d <= 1.0 );
                  } else {
                     inside = ( d < 1.0 );
                  }
               }
            } else {
               inside = 0;
            }
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
   pset_tmp = astAnnul( pset_tmp );
   pset_res = astAnnul( pset_res );
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
*     Copy constructor for Ellipse objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for Ellipse objects.

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
   AstEllipse *in;             /* Pointer to input Ellipse */
   AstEllipse *out;            /* Pointer to output Ellipse */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output Ellipses. */
   in = (AstEllipse *) objin;
   out = (AstEllipse *) objout;

/* For safety, first clear any references to the input memory from
   the output Ellipse. */
   out->centre = NULL;
   out->point1 = NULL;

/* Copy dynamic memory contents */
   out->centre = astStore( NULL, in->centre, sizeof( double )*2 );
   out->point1 = astStore( NULL, in->point1, sizeof( double )*2 );
}


/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for Ellipse objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for Ellipse objects.

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
   AstEllipse *this;                 /* Pointer to Ellipse */

/* Obtain a pointer to the Ellipse structure. */
   this = (AstEllipse *) obj;

/* Annul all resources. */
   this->centre = astFree( this->centre );
   this->point1 = astFree( this->point1 );
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for Ellipse objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the Ellipse class to an output Channel.

*  Parameters:
*     this
*        Pointer to the Ellipse whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstEllipse *this;                 /* Pointer to the Ellipse structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Ellipse structure. */
   this = (AstEllipse *) this_object;

/* Write out values representing the instance variables for the
   Ellipse class.  Accompany these with appropriate comment strings,
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
/* Implement the astIsAEllipse and astCheckEllipse functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(Ellipse,Region)
astMAKE_CHECK(Ellipse)

AstEllipse *astEllipse_( void *frame_void, int form, const double centre[2],
                         const double point1[2], const double point2[2],
                         AstRegion *unc, const char *options, int *status, ...) {
/*
*++
*  Name:
c     astEllipse
f     AST_ELLIPSE

*  Purpose:
*     Create a Ellipse.

*  Type:
*     Public function.

*  Synopsis:
c     #include "ellipse.h"
c     AstEllipse *astEllipse( AstFrame *frame, int form, const double centre[2],
c                             const double point1[2], const double point2[2],
c                             AstRegion *unc, const char *options, ... )
f     RESULT = AST_ELLIPSE( FRAME, FORM, CENTRE, POINT1, POINT2, UNC, OPTIONS,
f                           STATUS )

*  Class Membership:
*     Ellipse constructor.

*  Description:
*     This function creates a new Ellipse and optionally initialises its
*     attributes.
*
*     A Ellipse is a Region which represents a elliptical area within the
*     supplied 2-dimensional Frame.

*  Parameters:
c     frame
f     FRAME = INTEGER (Given)
*        A pointer to the Frame in which the region is defined. It must
*        have exactly 2 axes. A deep copy is taken of the supplied Frame.
*        This means that any subsequent changes made to the Frame using the
*        supplied pointer will have no effect the Region.
c     form
f     FORM = INTEGER (Given)
*        Indicates how the ellipse is described by the remaining parameters.
*        A value of zero indicates that the ellipse is specified by a
*        centre position and two positions on the circumference. A value of
*        one indicates that the ellipse is specified by its centre position,
*        the half-lengths of its two axes, and the orientation of its first
*        axis.
c     centre
f     CENTRE( 2 ) = DOUBLE PRECISION (Given)
c        An array of 2 doubles,
f        An array
*        containing the coordinates at the centre of
*        the ellipse.
c     point1
f     POINT1( 2 ) = DOUBLE PRECISION (Given)
c        An array of 2 doubles. If "form"
f        If FORM
*        is zero, this array should contain the coordinates of one of the four
*        points where an axis of the ellipse crosses the circumference of the
*        ellipse.
c        If "form"
f        If FORM
*        is one, it should contain the lengths of semi-major and
*        semi-minor axes of the ellipse, given as geodesic distances
*        within the Frame.
c     point2
f     POINT2( 2 ) = DOUBLE PRECISION (Given)
c        An array of 2 doubles. If "form"
f        If FORM
*        is zero, this array should containing the coordinates at some other
*        point on the circumference of the ellipse, distinct from
c        "point1". If "form"
f        POINT1. If FORM
*        is one, the first element of this array should hold the angle
*        between the second axis of the Frame and the first ellipse axis
*        (i.e. the ellipse axis which is specified first in the
c        "point1"
f        POINT1
*        array), and the second element will be ignored. The angle should be
*        given in radians, measured positive in the same sense as rotation
*        from the positive direction of the second Frame axis to the positive
*        direction of the first Frame axis.
c     unc
f     UNC = INTEGER (Given)
*        An optional pointer to an existing Region which specifies the
*        uncertainties associated with the boundary of the Ellipse being created.
*        The uncertainty in any point on the boundary of the Ellipse is found by
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
*        effect on the created Ellipse. Alternatively,
f        a null Object pointer (AST__NULL)
c        a NULL Object pointer
*        may be supplied, in which case a default uncertainty is used
*        equivalent to a box 1.0E-6 of the size of the Ellipse being created.
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
c        initialising the new Ellipse. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new Ellipse. The syntax used is identical to that for the
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
c     astEllipse()
f     AST_ELLIPSE = INTEGER
*        A pointer to the new Ellipse.

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
   AstEllipse *new;              /* Pointer to new Ellipse */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Obtain and validate a pointer to the supplied Frame structure. */
   frame = astCheckFrame( frame_void );

/* Initialise the Ellipse, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitEllipse( NULL, sizeof( AstEllipse ), !class_init, &class_vtab,
                         "Ellipse", frame, form, centre, point1, point2, unc );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new Ellipse's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new Ellipse. */
   return new;
}

AstEllipse *astEllipseId_( void *frame_void, int form, const double centre[2],
                         const double point1[2], const double point2[2],
                         void *unc_void, const char *options, ... ) {
/*
*  Name:
*     astEllipseId_

*  Purpose:
*     Create a Ellipse.

*  Type:
*     Private function.

*  Synopsis:
*     #include "ellipse.h"
*     AstEllipse *astEllipseId( void *frame_void, int form, const double centre[2],
*                         const double point1[2], const double point2[2],
*                         void *unc_void, const char *options, ..., int *status )

*  Class Membership:
*     Ellipse constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astEllipse constructor function. It returns an ID value (instead
*     of a true C pointer) to external users, and must be provided
*     because astEllipse_ has a variable argument list which cannot be
*     encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     The variable argument list also prevents this function from
*     invoking astEllipse_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astEllipse_.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The ID value associated with the new Ellipse.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstFrame *frame;              /* Pointer to Frame structure */
   AstEllipse *new;              /* Pointer to new Ellipse */
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

/* Initialise the Ellipse, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitEllipse( NULL, sizeof( AstEllipse ), !class_init, &class_vtab,
                        "Ellipse", frame, form, centre, point1, point2, unc );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new Ellipse's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new Ellipse. */
   return astMakeId( new );
}

AstEllipse *astInitEllipse_( void *mem, size_t size, int init, AstEllipseVtab *vtab,
                             const char *name, AstFrame *frame, int form,
                             const double centre[2], const double point1[2],
                             const double point2[2], AstRegion *unc, int *status ){
/*
*+
*  Name:
*     astInitEllipse

*  Purpose:
*     Initialise a Ellipse.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "ellipse.h"
*     AstEllipse *astInitEllipse( void *mem, size_t size, int init,
*                                 AstEllipseVtab *vtab, const char *name,
*                                 AstFrame *frame, const double centre[2],
*                                 const double point1[2], const double point2[2],
*                                 AstRegion *unc )

*  Class Membership:
*     Ellipse initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new Ellipse object. It allocates memory (if necessary) to accommodate
*     the Ellipse plus any additional data associated with the derived class.
*     It then initialises a Ellipse structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a Ellipse at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the Ellipse is to be initialised.
*        This must be of sufficient size to accommodate the Ellipse data
*        (sizeof(Ellipse)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the Ellipse (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the Ellipse
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the Ellipse's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new Ellipse.
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
*        attribute) containing the coordinates of the ellipse centre.
*     point1
*        An array of double, with one element for each Frame axis (Naxes
*        attribute). If "form" is zero, it should contain the coordinates at
*        the end of one of the axes of the ellipse. If "form" is one, it
*        should contain the semi-major and semi-minor axes of the ellipse.
*     point2
*        An array of double, with one element for each Frame axis (Naxes
*        attribute). If "form" is zero, it should contain the coordinates at
*        some other point on the circumference of the ellipse. If "form" is
*        one, element [1] is ignored and element [0] should contain the
*        angle from the second frame axis to the first ellipse axis, given in
*        radians, measured positive in the same sense as rotation from the
*        positive direction of the second Frame axis to the positive
*        direction of the first Frame axis. The "first" ellipse axis is
*        whichever of the semi-major or semi-minor axis is specified first in
*        the "point1" array.
*     unc
*        A pointer to a Region which specifies the uncertainty in the
*        supplied positions (all points on the boundary of the new Ellipse
*        being initialised are assumed to have the same uncertainty). A NULL
*        pointer can be supplied, in which case default uncertainties equal to
*        1.0E-6 of the dimensions of the new Ellipse's bounding box are used.
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
*     A pointer to the new Ellipse.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstEllipse *new;          /* Pointer to new Ellipse */
   AstPointSet *pset;        /* PointSet to pass to Region initialiser */
   double **ptr;             /* Pointer to coords data in pset */
   const double *p1;         /* Pointer to circumference point 1 */
   const double *p2;         /* Pointer to circumference point 2 */
   int i;                    /* axis index */
   int nc;                   /* No. of axes */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitEllipseVtab( vtab, name );

/* Initialise. */
   new = NULL;

/* Check the supplied value for "form" is legal. */
   if( form != 0 && form != 1 && astOK ) {
      astError( AST__BADIN, "astInitEllipse(%s): The value supplied for "
                "parameter \"form\" (%d) is illegal - it should be 0 or 1 "
                "(programming error).", status, name, form );
   }

/* Get the number of axis values required for each position. */
   nc = astGetNaxes( frame );

/* Report an error if the Frame is not 2-dimensional. */
   if( nc != 2 ) {
      astError( AST__BADIN, "astInitEllipse(%s): The supplied %s has %d "
                "axes - ellipses must have exactly 2 axes.", status, name,
                astGetClass( frame ), nc );
   }

/* If the ellipse is specified by axis lengths and orientation, find two
   points on the circumference (ends of the two ellipse axes). */
   if( form == 1 ) {
      p1 = astMalloc( sizeof( double )*2 );
      p2 = astMalloc( sizeof( double )*2 );
      if( astOK ) {
         astOffset2( frame, centre, *point2, point1[ 0 ], (double *) p1 );
         astOffset2( frame, centre, *point2 + AST__DPIBY2, point1[ 1 ],
                     (double *) p2 );
      }

/* If the ellipse is specified by two points on the circumference, use
   them. */
   } else {
      p1 = point1;
      p2 = point2;
   }

/* Create a PointSet to hold the supplied values, and get points to the
   data arrays. */
   pset = astPointSet( 3, nc, " ", status );
   ptr = astGetPoints( pset );

/* Copy the supplied coordinates into the PointSet, checking that no bad
   values have been supplied. */
   for( i = 0; astOK && i < nc; i++ ) {
      if( centre[ i ] == AST__BAD ) {
         astError( AST__BADIN, "astInitEllipse(%s): The value of axis %d is "
                   "undefined at the ellipse centre.", status, name, i + 1 );
      }
      if( astOK && p1[ i ] == AST__BAD ) {
         astError( AST__BADIN, "astInitEllipse(%s): The value of axis %d is "
                   "undefined at point 1 on the circumference of "
                   "the ellipse.", status, name, i + 1 );
      }
      if( astOK && p2[ i ] == AST__BAD ) {
         astError( AST__BADIN, "astInitEllipse(%s): The value of axis %d is "
                   "undefined at point 2 on the circumference of "
                   "the ellipse.", status, name, i + 1 );
      }
      ptr[ i ][ 0 ] = centre[ i ];
      ptr[ i ][ 1 ] = p1[ i ];
      ptr[ i ][ 2 ] = p2[ i ];
   }

/* Check pointers can be used safely. */
   if( astOK ) {

/* Initialise a Region structure (the parent class) as the first component
   within the Ellipse structure, allocating memory if necessary. */
      new = (AstEllipse *) astInitRegion( mem, size, 0, (AstRegionVtab *) vtab,
                                         name, frame, pset, unc );

      if ( astOK ) {

/* Initialise the Ellipse data. */
/* ------------------------ */
         new->stale = 1;

/* If an error occurred, clean up by deleting the new Ellipse. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Free resources. */
   pset = astAnnul( pset );
   if( form == 1 ) {
      p1 = astFree( (void *) p1 );
      p2 = astFree( (void *) p2 );
   }

/* Return a pointer to the new Ellipse. */
   return new;
}

AstEllipse *astLoadEllipse_( void *mem, size_t size, AstEllipseVtab *vtab,
                           const char *name, AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadEllipse

*  Purpose:
*     Load a Ellipse.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "ellipse.h"
*     AstEllipse *astLoadEllipse( void *mem, size_t size, AstEllipseVtab *vtab,
*                               const char *name, AstChannel *channel )

*  Class Membership:
*     Ellipse loader.

*  Description:
*     This function is provided to load a new Ellipse using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     Ellipse structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a Ellipse at the start of the memory
*     passed via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory into which the Ellipse is to be
*        loaded.  This must be of sufficient size to accommodate the
*        Ellipse data (sizeof(Ellipse)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the Ellipse (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the Ellipse structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstEllipse) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new Ellipse. If this is NULL, a pointer
*        to the (static) virtual function table for the Ellipse class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "Ellipse" is used instead.

*  Returned Value:
*     A pointer to the new Ellipse.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstEllipse *new;              /* Pointer to the new Ellipse */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this Ellipse. In this case the
   Ellipse belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstEllipse );
      vtab = &class_vtab;
      name = "Ellipse";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitEllipseVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built Ellipse. */
   new = astLoadRegion( mem, size, (AstRegionVtab *) vtab, name,
                        channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "Ellipse" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* There are no values to read. */
/* ---------------------------- */

/* Indicate that no cache intermediate results are yet available in the
   Ellipse structure */
      new->stale = 1;

/* If an error occurred, clean up by deleting the new Ellipse. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new Ellipse pointer. */
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

void astEllipsePars_( AstEllipse *this, double centre[2], double *a,
                      double *b, double *angle, double p1[2], double p2[2],
                      int *status ){
   if ( !astOK ) return;
   (**astMEMBER(this,Ellipse,EllipsePars))( this, centre, a, b,
                                            angle, p1, p2, status );
}






