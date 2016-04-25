/*
*class++
*  Name:
*     Box

*  Purpose:
*     A box region with sides parallel to the axes of a Frame.

*  Constructor Function:
c     astBox
f     AST_BOX

*  Description:
*     The Box class implements a Region which represents a box with sides
*     parallel to the axes of a Frame (i.e. an area which encloses a given
*     range of values on each axis). A Box is similar to an Interval, the
*     only real difference being that the Interval class allows some axis
*     limits to be unspecified. Note, a Box will only look like a box if
*     the Frame geometry is approximately flat. For instance, a Box centred
*     close to a pole in a SkyFrame will look more like a fan than a box
*     (the Polygon class can be used to create a box-like region close to a
*     pole).

*  Inheritance:
*     The Box class inherits from the Region class.

*  Attributes:
*     The Box class does not define any new attributes beyond
*     those which are applicable to all Regions.

*  Functions:
c     The Box class does not define any new functions beyond those
f     The Box class does not define any new routines beyond those
*     which are applicable to all Regions.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils
*     Copyright (C) 2008-2009 Science & Technology Facilities Council.
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
*     22-MAR-2004 (DSB):
*        Original version.
*     14-FEB-2006 (DSB):
*        Override astGetObjSize.
*     5-JUN-2007 (DSB):
*        Improve astSimplify algorithm.
*     6-JUN-2007 (DSB):
*        Change the iterating algorithm in MakeGrid so that it uses
*        pixel index rather than axis value. This is more robust against
*        rounding errors.
*     9-OCT-2007 (DSB):
*        - Fix bug in RegBaseMesh that could cause incorrect meshes for 2D
*        Boxes.
*        - In RegBaseMesh, use flat geometry if all axes come from simple
*        frames or from 1-dimensional specialist frames.
*     26-MAY-2008 (DSB):
*        Fix bug in RegBaseMesh that caused an error to be reported if
*        the Box occupies a single point.
*     20-JAN-2009 (DSB):
*        Over-ride astRegBasePick.
*     26-JAN-2009 (DSB):
*        Over-ride astMapMerge.
*     12-JUL-2009 (DSB):
*        Modify Simplify so that if a Box can be split into two
*        simpler components and then joined together into a Prism, it
*        does so. This is because being able to express a Region in
*        its current Frame is more important than having the simplest
*        possible structure.
*     28-FEB-2011 (DSB):
*        Do not assume the first axis value is good in function BestBox.
*     22-MAR-2011 (DSB):
*        Improve uniformity of points within grid produced by astRegBaseGrid.
*     16-JUL-2013 (DSB):
*        Use a more robust algorithm for determining the order of the
*        vertices whan a Box is simplified to a Polygon. The old method
*        sometimes resulted in an unbounded "inside-out" polygon.
*     4-NOV-2013 (DSB):
*        Modify RegPins so that it can handle uncertainty regions that straddle
*        a discontinuity. Previously, such uncertainty Regions could have a huge
*        bounding box resulting in matching region being far too big.
*     10-APR-2014 (DSB):
*        More work (in function Cache() ) on handling uncertainty regions that straddle
*        a discontinuity. This time ensure that the extent of the box on each axis takes
*        account of the possibly cyclic nature of the base Frame.
*     25-4-2016 (DSB):
*        Remove the unused box shrinking facility (a hang over from the
*        days when the RegBaseGrid function operated by creating multiple
*        meshes on the surface of the box, shrinking the box each time).
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS Box

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "cmpmap.h"              /* Compound Mappings */
#include "cmpframe.h"            /* Compound Frames */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* Sets of points/coordinates */
#include "region.h"              /* Coordinate regions (parent class) */
#include "channel.h"             /* I/O channels */
#include "box.h"                 /* Interface definition for this class */
#include "polygon.h"             /* Interface definition for this class */
#include "mapping.h"             /* Position mappings */
#include "unitmap.h"             /* Unit Mappings */
#include "permmap.h"             /* Axis permutation Mappings */
#include "interval.h"            /* Axis interval regions */
#include "nullregion.h"          /* Empty regions */
#include "pointlist.h"           /* List of points in a Frame */
#include "prism.h"               /* Extruded regions */

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
static int (* parent_getobjsize)( AstObject *, int * );
static AstPointSet *(* parent_transform)( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static AstMapping *(* parent_simplify)( AstMapping *, int * );
static void (* parent_setnegated)( AstRegion *, int, int * );
static void (* parent_setclosed)( AstRegion *, int, int * );
static void (* parent_clearnegated)( AstRegion *, int * );
static void (* parent_clearclosed)( AstRegion *, int * );
static void (* parent_setunc)( AstRegion *, AstRegion *, int * );
static void (* parent_setregfs)( AstRegion *, AstFrame *, int * );
static void (* parent_resetcache)( AstRegion *, int * );


#ifdef THREAD_SAFE
/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(Box)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(Box,Class_Init)
#define class_vtab astGLOBAL(Box,Class_Vtab)


#include <pthread.h>


#else


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstBoxVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstBox *astBoxId_( void *, int, const double[], const double[], void *, const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstBox *BestBox( AstFrame *, AstPointSet *, AstRegion *, int * );
static AstMapping *Simplify( AstMapping *, int * );
static AstPointSet *RegBaseGrid( AstRegion *, int * );
static AstPointSet *RegBaseMesh( AstRegion *, int * );
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static AstRegion *MergeBox( AstBox *, AstRegion *, int, int * );
static AstRegion *RegBasePick( AstRegion *this, int, const int *, int * );
static double *GeoCorner( AstFrame *, int, double *, double *, double *, int * );
static double *GeoLengths( AstFrame *, int, double *, double *, double *, int * );
static double *RegCentre( AstRegion *this, double *, double **, int, int, int * );
static int GetObjSize( AstObject *, int * );
static int MakeGrid( int, double **, int, double *, double *, int *, int, int, double, int * );
static int MapMerge( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
static int RegPins( AstRegion *, AstPointSet *, AstRegion *, int **, int * );
static int RegTrace( AstRegion *, int, double *, double **, int * );
static void BoxPoints( AstBox *, double *, double *, int *);
static void Cache( AstBox *, int, int * );
static void ClearClosed( AstRegion *, int * );
static void ClearNegated( AstRegion *, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void RegBaseBox( AstRegion *this, double *, double *, int * );
static void ResetCache( AstRegion *this, int * );
static void SetClosed( AstRegion *, int, int * );
static void SetNegated( AstRegion *, int, int * );
static void SetRegFS( AstRegion *, AstFrame *, int * );
static void SetUnc( AstRegion *, AstRegion *, int * );

/* Member functions. */
/* ================= */

void BoxPoints( AstBox *this, double *centre, double *corner, int *status) {
/*
*+
*  Name:
*     astBoxPoints

*  Purpose:
*     Return the defining points of a Box.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "box.h"
*     astBoxPoints( AstBox *this, double *centre, double *corner )

*  Class Membership:
*     Region virtual function.

*  Description:
*     This function returns the axis values at the points defining the
*     supplied Box.

*  Parameters:
*     this
*        Pointer to the Box.
*     centre
*        A pointer to an array in which to return the centre position of
*        the Box, in the base Frame of the encapsulated FrameSet.
*     corner
*        A pointer to an array in which to return the position of a corner
*        of the Box, in the base Frame of the encapsulated FrameSet.

*  Notes:
*     - It is assumed that the length of the supplied arrays is at least
*     equal to the number of axes in the base frame of the encapsulated
*     FrameSet.
*-
*/

/* Local Variables: */
   AstPointSet *pset;
   double **ptr;
   int nc;
   int i;

/* Check the inherited status. */
   if( !astOK ) return;

/* Get a pointer to the PointSet holding the points defining the Box. */
   pset = ((AstRegion *) this)->points;

/* Get a pointer to the PointSet's data arrays. */
   ptr = astGetPoints( pset );

/* See how many axes each point in the PointSet has. */
   nc = astGetNcoord( pset );

/* Copy the centre and corner positions form the PointSet into the
   supplied arrays. */
   for( i = 0; i < nc; i++ ) {
      centre[ i ] = ptr[ i ] [ 0 ];
      corner[ i ] = ptr[ i ] [ 1 ];
   }

}

static void ClearClosed( AstRegion *this, int *status ){
/*
*  Name:
*     ClearClosed

*  Purpose:
*     Clear the Closed attribute of a Region.

*  Type:
*     Private function.

*  Synopsis:
*     #include "box.h"
*     void ClearClosed( AstRegion *this, int *status )

*  Class Membership:
*     Box member function (over-rides the protected astClearClosed
*     method inherited from the Region class).

*  Description:
*     This function clears the Closed attribute of the supplied Region.

*  Parameters:
*     this
*        Pointer to the Region.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   int old;

/* Check the global error status. */
   if ( !astOK ) return;

/* Get the original attribute value */
   old = astGetClosed( this );

/* Invoke the clear method inherited from the parent Region class */
   (*parent_clearclosed)( this, status );

/* If the new value is not the same as the old value, inidcatethat we
   need to re-calculate the cached information in the Box. */
   if( astGetClosed( this ) != old ) astResetCache( this );
}

static void ClearNegated( AstRegion *this, int *status ){
/*
*  Name:
*     ClearNegated

*  Purpose:
*     Clear the Negated attribute of a Region.

*  Type:
*     Private function.

*  Synopsis:
*     #include "box.h"
*     void ClearNegated( AstRegion *this, int *status )

*  Class Membership:
*     Box member function (over-rides the protected astClearNegated
*     method inherited from the Region class).

*  Description:
*     This function clears the Negated attribute of the supplied Region.

*  Parameters:
*     this
*        Pointer to the Region.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   int old;

/* Check the global error status. */
   if ( !astOK ) return;

/* Get the original attribute value */
   old = astGetNegated( this );

/* Invoke the clear method inherited from the parent Region class */
   (*parent_clearnegated)( this, status );

/* If the new value is not the same as the old value, inidcatethat we
   need to re-calculate the cached information in the Box. */
   if( astGetNegated( this ) != old ) astResetCache( this );
}

static AstBox *BestBox( AstFrame *frm, AstPointSet *mesh, AstRegion *unc, int *status ){
/*
*  Name:
*     BestBox

*  Purpose:
*     Find the best fitting Box through a given mesh of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "box.h"
*     AstBox *BestBox( AstFrame *frm, AstPointSet *mesh, AstRegion *unc, int *status )

*  Class Membership:
*     Box member function

*  Description:
*     This function finds the best fitting Box through a given mesh of points.

*  Parameters:
*     frm
*        Defines the geometry of the axes.
*     mesh
*        Pointer to a PointSet holding the mesh of points. They are
*        assumed to be in the Frame represented by "unc".
*     unc
*        A Region representing the uncertainty associated with each point
*        on the mesh.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the best fitting Region. It will inherit the positional
*     uncertainty and Frame represented by "unc". NULL is returned if all
*     the supplied positions are bad.

*  Notes:
*    - A NULL pointer is returned if an error has already occurred, or if
*    this function should fail for any reason.

*/

/* Local Variables: */
   AstBox *result;
   double **ptr;
   double *axval;
   double *blbnd;
   double *bubnd;
   double *lbnd;
   double *p;
   double *ubnd;
   double eps;
   double lb;
   double lim2;
   double lim;
   double liml;
   double limu;
   double mxl;
   double mxu;
   double org;
   double sxl2;
   double sxl;
   double sxu2;
   double sxu;
   double ub;
   double p0;
   double d;
   double dinc;
   int ic;
   int ip;
   int nc;
   int np;
   int nxl;
   int nxu;
   int ok;

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get no. of points in the mesh, and the number of axis values per point. */
   np = astGetNpoint( mesh );
   nc = astGetNcoord( mesh );

/* Get pointers to the axis values. */
   ptr = astGetPoints( mesh );

/* Allocate work space. */
   lbnd = astMalloc( sizeof( double )*(size_t) nc );
   ubnd = astMalloc( sizeof( double )*(size_t) nc );

   blbnd = astMalloc( sizeof( double )*(size_t) nc );
   bubnd = astMalloc( sizeof( double )*(size_t) nc );

   axval = astMalloc( sizeof( double )*(size_t) np );

/* Check pointers can be used safely */
   if( axval ) {

/* Get the bounding box of the uncertainty region. */
      astGetRegionBounds( unc, lbnd, ubnd );

/* We fit the box one axis at a time. */
      ok = 1;
      for( ic = 0; ic < nc; ic++ ) {

/* Find the first good value on this axis. */
         for( ip = 0; ip < np; ip++ ) {
            org = ptr[ ic ][ ip ];
            axval[ ip ] = org;
            if( org != AST__BAD ) break;
         }

/* Abort if all the axis values werebad. */
         if( ip >= np ) {
            ok = 0;
            break;
         }

/* Find the upper and lower limits of the supplied mesh on this axis. */
         lb = 0.0;
         ub = 0.0;
         ip++;
         p = ptr[ ic ] + ip;
         p0 = org;
         d = 0.0;
         for( ; ip < np; ip++, p++ ) {
            dinc = astAxDistance( frm, ic + 1, p0, *p );
            if( dinc != AST__BAD ) {
               d += dinc;
               if( d < lb ) lb = d;
               if( d > ub ) ub = d;
            }
            axval[ ip ] = org + d;
            p0 = *p;
         }

/* Now convert these relative offsets to actual axis values. */
         lb += org;
         ub += org;

/* Now scan the list of axis values again, looking for values which are
   "close to" either lower or upper bound. These will be the points which
   are on the faces of the box which are orthogonal to the current axis. Here
   "close to" means within 20 times the uncertainty associated with this
   axis, or one tenth of the box size (which ever is smaller). We find the
   mean and standard deviation of such "close" values, and then do a
   single sigma-clipping iteration (at 3-sigma) to get rid of any values
   which are on one of the other faces of the box. */

         lim = 20*( ubnd[ ic ] - lbnd[ ic ] );
         lim2 = 0.1*( ub - lb );
         if( lim2 < lim ) lim = lim2;

         sxl = 0.0;
         sxl2 = 0.0;
         nxl = 0;
         sxu = 0.0;
         sxu2 = 0.0;
         nxu = 0;

         p = axval;
         for( ip = 0; ip < np; ip++, p++ ) {
            if( *p != AST__BAD ) {
               if( fabs( *p - lb ) <= lim ) {
                  sxl += *p;
                  sxl2 += (*p)*(*p);
                  nxl++;
               } else if( fabs( *p - ub ) <= lim ) {
                  sxu += *p;
                  sxu2 += (*p)*(*p);
                  nxu++;
               }
            }
         }

         if( nxl > 0 ) {
            mxl = sxl/nxl;
            liml = sxl2/nxl - mxl*mxl;
            eps = 100*mxl*DBL_EPSILON;
            if( liml < eps*eps ) {
               liml = eps;
            } else {
               liml = 3.0*sqrt( liml );
            }
         } else {
            mxl = lb;
            liml = 0.0;
         }

         if( nxu > 0 ) {
            mxu = sxu/nxu;
            limu = sxu2/nxu - mxu*mxu;
            eps = 100*mxu*DBL_EPSILON;
            if( limu < eps*eps ) {
               limu = eps;
            } else {
               limu = 3.0*sqrt( limu );
            }

         } else {
            mxu = ub;
            limu = 0.0;
         }

         sxl = 0.0;
         nxl = 0;
         sxu = 0.0;
         nxu = 0;

         p = axval;
         for( ip = 0; ip < np; ip++, p++ ) {
            if( *p != AST__BAD ) {
               if( fabs( *p - mxl ) <= liml ) {
                  sxl += *p;
                  nxl++;
               } else if( fabs( *p - mxu ) <= limu ) {
                  sxu += *p;
                  nxu++;
               }
            }
         }

         if( nxl > 0 ) {
            mxl = sxl/nxl;
         } else {
            mxl = lb;
         }

         if( nxu > 0 ) {
            mxu = sxu/nxu;
         } else {
            mxu = ub;
         }

/* The resulting mean axis values are the bounds of the required box on
   the current axis.*/
         blbnd[ ic ] = mxl;
         bubnd[ ic ] = mxu;
      }

/* If possible, create the returned Box. */
      if( ok ) result = astBox( unc, 1, blbnd, bubnd, unc, " ", status );
   }

/* Free resources */
   lbnd = astFree( lbnd );
   ubnd = astFree( ubnd );
   blbnd = astFree( blbnd );
   bubnd = astFree( bubnd );
   axval = astFree( axval );

/* Return NULL if anything went wrong. */
   if( !astOK ) result = astAnnul( result );

/* Return the result.*/
   return result;
}

static void Cache( AstBox *this, int lohi, int *status ){
/*
*  Name:
*     Cache

*  Purpose:
*     Calculate intermediate values and cache them in the Box structure.

*  Type:
*     Private function.

*  Synopsis:
*     #include "box.h"
*     void Cache( AstRegion *this, int lohi, int *status )

*  Class Membership:
*     Box member function

*  Description:
*     This function uses the PointSet stored in the parent Region to calculate
*     some intermediate values which are useful in other methods. These
*     values are stored within the Box structure.

*  Parameters:
*     this
*        Pointer to the Box.
*     lohi
*        Are the lo and hi arrays to be used?
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstFrame *frm;
   AstPointSet *pset;
   AstRegion *unc;
   double **ptr;
   double *centre;
   double *extent;
   double *hi;
   double *lbnd_unc;
   double *geolen;
   double *lo;
   double *ubnd_unc;
   double wid;
   int i;
   int nc;

/* Check the global error status. Also return if the currently cached
   information is usable. */
   if ( !astOK || !this->stale ) return;

/* Get the number of base Frame axes. */
   nc = astGetNin( ((AstRegion *)this)->frameset );

/* Allocate memory to store the half-width of the box on each axis. */
   extent = (double *) astMalloc( sizeof( double )*(size_t) nc );

/* Allocate memory to store the centre of the box on each axis. */
   centre = (double *) astMalloc( sizeof( double )*(size_t) nc );

/* Allocate memory to store the high and low bounds. */
   hi = (double *) astMalloc( sizeof( double )*(size_t) nc );
   lo = (double *) astMalloc( sizeof( double )*(size_t) nc );

/* Memory to store the uncertainty bounding box */
   lbnd_unc = astMalloc( sizeof( double)*(size_t) nc );
   ubnd_unc = astMalloc( sizeof( double)*(size_t) nc );

/* Memory to store the geodesic half-dimensions of the box. */
   geolen = astMalloc( sizeof( double)*(size_t) nc );

/* Get pointers to the coordinate data in the parent Region structure. */
   pset = ((AstRegion *) this)->points;
   ptr = astGetPoints( pset );

/* Check pointers can be used safely. */
   if( ptr ) {

/* Store the centre and corner axis values. */
      for( i = 0; i < nc; i++ ) {
         centre[ i ] = ptr[ i ][ 0 ];
         hi[ i ] = ptr[ i ][ 1 ];
      }

/* Calculate the geodesic half-dimensions of the box. */
      frm = astGetFrame( ((AstRegion *) this)->frameset, AST__BASE );
      GeoLengths( frm, nc, centre, hi, geolen, status );

/* Calculate the half-width and store in the above array. Also store the
   lower and upper bounds. */
      for( i = 0; i < nc; i++ ) {
         extent[ i ] = fabs( astAxDistance( frm, i + 1, ptr[ i ][ 1 ],
                                            centre[ i ] ) );
         lo[ i ] = centre[ i ] - extent[ i ];
         hi[ i ] = centre[ i ] + extent[ i ];
      }

      frm = astAnnul( frm );

/* Store the pointers to these arrays in the Box structure, and indicate
   the information is usable. */
      if( astOK ) {
         astFree( this->extent );
         astFree( this->centre );
         astFree( this->lo );
         astFree( this->hi );
         astFree( this->geolen );
         this->extent = extent;
         this->centre = centre;
         this->lo = lo;
         this->hi = hi;
         this->geolen = geolen;
         this->stale = 0;
         extent = NULL;
         centre = NULL;
         lo = NULL;
         hi = NULL;
         geolen = NULL;
      }

/* If lo and hi values are to be used, ensure they are expanded to at
   least the width of an uncertainty box. */
      if( lohi ) {

/* If we are dealing with an unnegated closed Box or a negated open
   Box, ensure that the box does not have zero width on any axis. We do
   this by ensuring that the extent on all axes is at least half the
   width  of the bounding box of the uncertainty Region. */
         if(  astGetNegated( this ) != astGetClosed( this ) ) {

/* Get the bounding box of the uncertainty Region in the base Frame of
   the supplied Box. */
            unc = astGetUncFrm( this, AST__BASE );
            astGetRegionBounds( unc, lbnd_unc, ubnd_unc );

/* Ensure the extents are at least half the width of the uncertainty
   bounding box. */
            for ( i = 0; i < nc; i++ ) {
               wid = 0.5*( ubnd_unc[ i ] - lbnd_unc[ i ] );
               if( this->extent[ i ] < wid ) {
                  this->extent[ i ] = wid;
                  this->lo[ i ] = this->centre[ i ] - wid;
                  this->hi[ i ] = this->centre[ i ] + wid;
               }
            }

/* Free resources. */
            unc = astAnnul( unc );
         }
      }
   }

/* Annul the memory allocated above if an error occurred. */
   if( !astOK ) {
      extent = astFree( extent );
      centre = astFree( centre );
      lo = astFree( lo );
      hi = astFree( hi );
   }

/* Free other resources */
   lbnd_unc = astFree( lbnd_unc );
   ubnd_unc = astFree( ubnd_unc );

}

static double *GeoCorner( AstFrame *frm, int nc, double *centre,
                          double *geolen, double *corner, int *status ){
/*
*  Name:
*     GeoCorner

*  Purpose:
*     Find the corner position implied by the supplied centre position
*     and geodesic box dimensions.

*  Type:
*     Private function.

*  Synopsis:
*     #include "box.h"
*     double *GeoCorner( AstFrame *frm, int nc, double *centre,
*                        double *geolen, double *corner, int *status )

*  Class Membership:
*     Box member function

*  Description:
*     This function returns the corner position that is implied by the
*     supplied centre position and geodesic box dimensions. The returned
*     corner position is found by offsetting away from the supplied
*     centre position along each axis in turn, by the geodesic distance
*     specified in "geolen".

*  Parameters:
*     frm
*        Defines the geometry of the axes.
*     nc
*        The number of Frame axes.
*     centre
*        Pointer to an array holding the box centre axis values.
*     geolen
*        Pointer to an array holding the geodesic distance corresponding
*        to each half axis of the box.
*     corner
*        Pointer to an array in which to store the axis values at the
*        returned corner position. If this is NULL a new array is
*        allocated and a pointer to it returned as the function value.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the array holding the corner axis values. If a non-NULL
*     value is supplied for parameter "corner", then the same value will
*     be returned as the function value. Otherwise, the returned value
*     will be a pointer to a newly allocated array that should be freed
*     using astFree when no longer needed.

*/

/* Local Variables: */
   double *p1;
   double *p2;
   double *p3;
   double *pt;
   double *result;
   double *work1;
   double *work2;
   double off;
   double off0;
   int i;

/* Initialise */
   result = corner;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Ensure we have a results array. */
   if( ! result ) result = astMalloc( sizeof( double )*nc );

/* Also allocate two work arrays to hold a single position. */
   work1 = astMalloc( sizeof( double )*nc );
   work2 = astMalloc( sizeof( double )*nc );

/* Check the pointers can be used safely. */
   if( astOK ) {

/* Select which array to use as the initial results array so that the
   final results end up in the returned array. */
      if( ( nc % 2 ) == 0 ) {
         p1 = result;
         p2 = work1;
         p3 = work2;
      } else {
         p1 = work2;
         p2 = work1;
         p3 = result;
      }

/* Initialise the current corner position to be at the centre of the box. */
      for( i = 0; i < nc; i++ ) p1[ i ] = centre[ i ];

/* Loop round offsetting along each side of the box. */
      for( i = 0; i < nc; i++ ) {

/* In the p2 array put the axis values at a point which is offset
   slightly along the current axis away from the current "corner"
   position (p1). */
         memcpy( p2, p1, sizeof( double )*nc );

         if( geolen[ i ] != 0.0 ) {
            off = 0.0001*fabs( geolen[ i ] );
         } else {
            off = 1.0E-6;
         }

         off0 = fabs( 1.0E-10*centre[ i ] );
         if( off < off0 ) off = off0;
         p2[ i ] += off;

/* Offset away from the current corner position (p1) towards the position
   found above (p2), moving by the geodesic distance supplied for this axis.
   Put the resulting axis values in p3. */
         astOffset( frm, p1, p2, geolen[ i ], p3 );

/* Swap the p3 and p1 arrays so that the offset position found above (p3)
   becomes the starting position (p1) for the next offset. */
         pt = p1;
         p1 = p3;
         p3 = pt;
      }
   }

/* Free resources */
   work1 = astFree( work1 );
   work2 = astFree( work2 );

/* Return the result */
   return result;
}

static double *GeoLengths( AstFrame *frm, int nc, double *centre,
                           double *corner, double *geolen, int *status ){
/*
*  Name:
*     GeoLengths

*  Purpose:
*     Find the geodesic dimensions of a box.

*  Type:
*     Private function.

*  Synopsis:
*     #include "box.h"
*      double *GeoLengths( AstFrame *frm, int nc, double *centre,
*                          double *corner, double *geolen, int *status )

*  Class Membership:
*     Box member function

*  Description:
*     This function returns half the geodesic distance along each edge of
*     the supplied box.

*  Parameters:
*     frm
*        Defines the geometry of the axes.
*     nc
*        The number of Frame axes.
*     centre
*        Pointer to an array holding rhe box centre axis values.
*     corner
*        Pointer to an array holding the axis values at the corner
*        position.
*     geolen
*        Pointer to an array in which to return the geodesic distances
*        corresponding to each half axis of the box. If this is NULL a
*        new array is allocated and a pointer to it returned as the
*        function value.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the array holding the geodesic half-dimensions of the box.
*     If a non-NULL value is supplied for parameter "corner", then the same
*     value will be returned as the function value. Otherwise, the
*     returned value will be a pointer to a newly allocated array that
*     should be freed using astFree when no longer needed.

*/

/* Local Variables: */
   double *result;
   double *p1;
   double *p2;
   int i;

/* Initialise */
   result = geolen;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Ensure we have a results array. */
   if( ! result ) result = astMalloc( sizeof( double )*nc );

/* Also allocate two work arrays to hold a single position. */
   p1 = astMalloc( sizeof( double )*nc );
   p2 = astMalloc( sizeof( double )*nc );

/* Check the pointers can be used safely. */
   if( astOK ) {

/* Initialise the coords as the start and end of the line. */
      memcpy( p1, centre, sizeof( double )*nc );
      memcpy( p2, centre, sizeof( double )*nc );

/* Loop round finding the geodesic half-length of each side of the box. */
      for( i = 0; i < nc; i++ ) {

/* The end of the line is the same as the start of the line, except that
   it has the corner value for the current axis. */
         p2[ i ] = corner[ i ];

/* Find and return the geodesic distance along the line (i.e. from p1 to
   p2). */
         result[ i ] = astDistance( frm, p1, p2 );

/* The start of the next line wil lbe at the end of the current line. */
         p1[ i ] = corner[ i ];
      }
   }

/* Free resources */
   p1 = astFree( p1 );
   p2 = astFree( p2 );

/* Return the result */
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
*     #include "box.h"
*     int GetObjSize( AstObject *this, int *status )

*  Class Membership:
*     Box member function (over-rides the astGetObjSize protected
*     method inherited from the parent class).

*  Description:
*     This function returns the in-memory size of the supplied Box,
*     in bytes.

*  Parameters:
*     this
*        Pointer to the Box.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The Object size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstBox *this;         /* Pointer to Box structure */
   int result;                /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointers to the Box structure. */
   this = (AstBox *) this_object;

/* Invoke the GetObjSize method inherited from the parent class, and then
   add on any components of the class structure defined by thsi class
   which are stored in dynamically allocated memory. */
   result = (*parent_getobjsize)( this_object, status );

   result += astTSizeOf( this->extent );
   result += astTSizeOf( this->centre );
   result += astTSizeOf( this->lo );
   result += astTSizeOf( this->hi );
   result += astTSizeOf( this->geolen );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

void astInitBoxVtab_(  AstBoxVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitBoxVtab

*  Purpose:
*     Initialise a virtual function table for a Box.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "box.h"
*     void astInitBoxVtab( AstBoxVtab *vtab, const char *name )

*  Class Membership:
*     Box vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the Box class.

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
   AstObjectVtab *object;        /* Pointer to Object component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitRegionVtab( (AstRegionVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsABox) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstRegionVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */
   vtab->BoxPoints = BoxPoints;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   mapping = (AstMappingVtab *) vtab;
   region = (AstRegionVtab *) vtab;

   parent_getobjsize = object->GetObjSize;
   object->GetObjSize = GetObjSize;

   parent_transform = mapping->Transform;
   mapping->Transform = Transform;

   parent_simplify = mapping->Simplify;
   mapping->Simplify = Simplify;

   parent_setnegated = region->SetNegated;
   region->SetNegated = SetNegated;

   parent_setunc = region->SetUnc;
   region->SetUnc = SetUnc;

   parent_setclosed = region->SetClosed;
   region->SetClosed = SetClosed;

   parent_clearnegated = region->ClearNegated;
   region->ClearNegated = ClearNegated;

   parent_clearclosed = region->ClearClosed;
   region->ClearClosed = ClearClosed;

   parent_setregfs = region->SetRegFS;
   region->SetRegFS = SetRegFS;

   parent_resetcache = region->ResetCache;
   region->ResetCache = ResetCache;

   mapping->MapMerge = MapMerge;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   region->RegBaseGrid = RegBaseGrid;
   region->RegBaseMesh = RegBaseMesh;
   region->RegBasePick = RegBasePick;
   region->RegBaseBox = RegBaseBox;
   region->RegPins = RegPins;
   region->RegTrace = RegTrace;
   region->RegCentre = RegCentre;

/* Declare the copy constructor, destructor and class dump
   functions. */
   astSetDelete( vtab, Delete );
   astSetCopy( vtab, Copy );
   astSetDump( vtab, Dump, "Box", "Axis intervals" );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
}

static int MakeGrid( int naxes, double **ptr, int ip, double *lbnd,
                     double *ubnd, int *np_axes, int np_axis, int iaxis,
                     double axval, int *status ){
/*
*  Name:
*     MakeGrid

*  Purpose:
*     Create a grid covering the entire volume of a specified box.

*  Type:
*     Private function.

*  Synopsis:
*     #include "box.h"
*     int MakeGrid( int naxes, double **ptr, int ip, double *lbnd,
*                   double *ubnd, int *np_axes, int np_axis, int iaxis,
*                   double axval, int *status )

*  Class Membership:
*     Box member function

*  Description:
*     This function creates an evenly sampled grid covering a given
*     volume of n-D space, putting the coordinates at the sample points
*     into a supplied array and returning the number of samples added.
*     Optionally, the volume can be assumed to have a constant value on
*     a specified axis.

*  Parameters:
*     naxes
*        The number of axes.
*     ptr
*        Pointer to an array with "naxes" elements. Each element is a
*        pointer to an array in which to store the values for the axis.
*     ip
*        The index of the first point to be added to the "ptr" arrays.
*     lbnd
*        Pointer to an array containing the lower axis bounds of the
*        volume to be sampled.
*     ubnd
*        Pointer to an array containing the upper axis bounds of the
*        volume to be sampled.
*     np_axes
*        Pointer to an array with one element for every axis, giving the
*        number of samples along each axis (except, optionally, the axis
*        specified by "iaxis"). The first sample (sample 0) for each axis
*        will be at the lower bound given in "lbnd" and the last sample
*        (sample "np_axes[iax]-1") will be at the upper bound given in
*        "ubnd". If NULL, then the single scalar avalue given by
*        "np_axis" is used for all axes.
*     np_axis
*        The constant number of samples along every axis (except, optionally,
*        the axis specified by "iaxis"). The first sample (sample 0) for each
*        axis will be at the lower bound given in "lbnd" and the last sample
*        (sample "np_axis-1") will be at the upper bound given in "ubnd".
*        The supplied value is only used if "np_axes" is NULL.
*     iaxis
*        The index of an axis which has constant value in the volume, or
*        -1 if all axes are to span the full volume given by lbnd/ubnd.
*        The values in "lbnd" and "ubnd" are ignored for this axis and all
*        sample positions will have the axis value given by "axval".
*     axval
*        The constant value for the axis with index "iaxis". Ignored if
*        "iaxis" is -1.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The number of points added to the "ptr" arrays.

*/

/* Local Variables: */
   double *step;         /* Pointer to array holding axis step sizes */
   int *maxi;            /* Pointer to array of maximum index values */
   int *pi;              /* Pointer to array holding current sample indices */
   int i;                /* Axis index */
   int ipp;              /* Index of next point */
   int nsamp;            /* Number of samples along the axis */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Allocate memory to hold the max indices on each axis. */
   maxi = astMalloc( sizeof( int )*(size_t) naxes );

/* Allocate memory to hold the indices of the current position.*/
   pi = astMalloc( sizeof( int )*(size_t) naxes );

/* Allocate memory to hold the step size for each axis. */
   step = astMalloc( sizeof( double )*(size_t) naxes );
   if( astOK ) {

/* For every axis, set up the step size, initialise the current position to
   the lower bound, and store a modified upper limit which includes some
   safety marging to allow for rounding errors. */
      for( i = 0; i < naxes; i++ ) {
         nsamp = np_axes ? np_axes[ i ] : np_axis;
         step[ i ] = ( ubnd[ i ] - lbnd[ i ] )/( nsamp - 1 );
         pi[ i ] = 0;
         maxi[ i ] = nsamp - 1;
      }

      if( iaxis >= 0 ) {
         maxi[ iaxis ] = 0;
         step[ iaxis ] = 0.0;
         pi[ iaxis ] = 0;
      }

/* Initialise the index of the next position to store. */
      ipp = ip;

/* Loop round adding points to the array until the whole volume has been
   done. */
      i = 0;
      while( i < naxes ) {

/* Add the current point to the supplied array,and increment the index of
   the next point to add. */
         for( i = 0; i < naxes; i++ ) {
            if( i == iaxis ) {
               ptr[ i ][ ipp ] = axval;
            } else {
               ptr[ i ][ ipp ] = lbnd[ i ] + pi[ i ]*step[ i ];
            }
         }
         ipp++;

/* We now move the current position on to the next sample */
         i = 0;
         while( i < naxes ) {
            pi[ i ]++;
            if( pi[ i ] > maxi[ i ] ) {
               pi[ i ] = 0;
               i++;
            } else {
               break;
            }
         }
      }
   } else {
      ipp = ip;
   }

/* Free resources. */
   maxi = astFree( maxi );
   pi = astFree( pi );
   step = astFree( step );

/* Return the result. */
   return astOK ? ( ipp - ip ): 0 ;
}

static int MapMerge( AstMapping *this, int where, int series, int *nmap,
                     AstMapping ***map_list, int **invert_list, int *status ) {
/*
*  Name:
*     MapMerge

*  Purpose:
*     Simplify a sequence of Mappings containing a Box.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     int MapMerge( AstMapping *this, int where, int series, int *nmap,
*                   AstMapping ***map_list, int **invert_list, int *status )

*  Class Membership:
*     Box method (over-rides the protected astMapMerge method
*     inherited from the Region class).

*  Description:
*     This function attempts to simplify a sequence of Mappings by
*     merging a nominated Box in the sequence with its neighbours,
*     so as to shorten the sequence if possible.
*
*     In many cases, simplification will not be possible and the
*     function will return -1 to indicate this, without further
*     action.
*
*     In most cases of interest, however, this function will either
*     attempt to replace the nominated Box with a Mapping which it
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
*        Pointer to the nominated Box which is to be merged with
*        its neighbours. This should be a cloned copy of the Box
*        pointer contained in the array element "(*map_list)[where]"
*        (see below). This pointer will not be annulled, and the
*        Box it identifies will not be modified by this function.
*     where
*        Index in the "*map_list" array (below) at which the pointer
*        to the nominated Box resides.
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
   AstBox *oldbox;       /* Pointer to supplied Box */
   AstMapping *map;      /* Pointer to adjacent Mapping */
   AstMapping *new;      /* Simplified or merged Region */
   int i1;               /* Index of first Mapping merged */
   int i;                /* Loop counter */
   int result;           /* Result value to return */

/* Initialise. */
   result = -1;
   i1 = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the Box. */
   oldbox = (AstBox *) this;

/* First of all, see if the Box can be replaced by a simpler Region,
   without reference to the neighbouring Regions in the list.           */
/* =====================================================================*/

/* Try to simplify the box. If the pointer value has changed, we assume
   some simplification took place. */
   new = astSimplify( oldbox );
   if( new != (AstMapping *) oldbox ) {

/* Annul the Box pointer in the list and replace it with the new Region
   pointer, and indicate that the forward transformation of the returned
   Region should be used (not really needed but keeps things clean). */
      (void) astAnnul( ( *map_list )[ where ] );
      ( *map_list )[ where ] = new;
      ( *invert_list )[ where ] = 0;

/* Return the index of the first modified element. */
      result = where;

/* If the Box itself could not be simplified, see if it can be merged
   with the Regions on either side of it in the list. We can only merge
   in parallel. */
/* =====================================================================*/
   } else if( ! series ){
      new = astAnnul( new );

/* Attempt to merge the Box with its lower neighbour (if any). */
      if( where > 0 ) {
         i1 = where - 1;
         map = ( *map_list )[ where - 1 ];
         if( astIsARegion( map ) ) {
            new = (AstMapping *) MergeBox( oldbox, (AstRegion *) map, 0,
                                           status );
         }
      }

/* If this did not produced a merged Region, attempt to merge the Box with its
   upper neighbour (if any). */
      if( !new && where < *nmap - 1 ) {
         i1 = where;
         map = ( *map_list )[ where + 1 ];
         if( astIsARegion( map ) ) {
            new = (AstMapping *) MergeBox( oldbox, (AstRegion *) map, 1,
                                           status );
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

static AstRegion *MergeBox( AstBox *this, AstRegion *reg, int boxfirst,
                            int *status ) {
/*
*  Name:
*     MergeBox

*  Purpose:
*     Attempt to merge a Box with another Region to form a Region of higher
*     dimensionality.

*  Type:
*     Private function.

*  Synopsis:
*     #include "box.h"
*     AstRegion *MergeBox( AstBox *this, AstRegion *reg, int boxfirst, int *status )

*  Class Membership:
*     Box member function.

*  Description:
*     This function attempts to combine the supplied Regions together
*     into a Region of higher dimensionality.

*  Parameters:
*     this
*        Pointer to a Box.
*     reg
*        Pointer to another Region.
*     boxfirst
*        If non-zero, then the Box axes are put first in the new Region.
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
   AstPointSet *pset_new;    /* PointSet holding PointList axis values for new */
   AstPointSet *pset_reg;    /* PointSet holding PointList axis values for reg */
   AstRegion *bunc;          /* Base Frame uncertainty Region */
   AstRegion *new;           /* Pointer to new Interval in base Frame */
   AstRegion *result;        /* Pointer to returned Interval in current Frame */
   AstRegion *unc_reg;       /* Current Frame uncertainty Region from "reg" */
   AstRegion *unc_this;      /* Current Frame uncertainty Region from "this" */
   double **ptr_new;         /* Pointers to arrays holding new axis values */
   double **ptr_reg;         /* Pointers to arrays holding reg axis values */
   double *centre;           /* Array to hold box centre axis values */
   double *corner;           /* Array to hold box corner axis values */
   double *lbnd;             /* Array to hold lower axis bounds */
   double *p;                /* Pointer to next input value */
   double *q;                /* Pointer to next output value */
   double *ubnd;             /* Array to hold upper axis bounds */
   double fac_reg;           /* Ratio of used to default MeshSize for "reg" */
   double fac_this;          /* Ratio of used to default MeshSize for "this" */
   double temp;              /* Temporary storage */
   int i;                    /* Loop count */
   int j;                    /* Loop count */
   int msz_reg;              /* Original MeshSize for "reg" */
   int msz_reg_set;          /* Was MeshSize originally set for "reg"? */
   int msz_this;             /* Original MeshSize for "this" */
   int msz_this_set;         /* Was MeshSize originally set for "this"? */
   int nax;                  /* Number of axes in "result" */
   int nax_reg;              /* Number of axes in "reg" */
   int nax_this;             /* Number of axes in "this" */
   int neg_reg;              /* Negated attribute value for other supplied Region */
   int neg_this;             /* Negated attribute value for supplied Box  */
   int npnt;                 /* Number of points in PointList */
   int ok;                   /* Can supplied Regions be merged? */

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

      if( boxfirst ) {
         bfrm = (AstFrame *) astCmpFrame( frm_this, frm_reg, "", status );
      } else {
         bfrm = (AstFrame *) astCmpFrame( frm_reg, frm_this, "", status );
      }

      frm_this = astAnnul( frm_this );
      frm_reg = astAnnul( frm_reg );

/* Indicate we do not yet have a merged Region. */
      new = NULL;

/* First attempt to merge with another Box. The result will be a Box. Both
   Boxes must be un-negated. */
      if( astIsABox( reg ) && !neg_this && !neg_reg ) {

/* Allocate memory to store the centre and corner of the returned Box. */
         centre = astMalloc( sizeof( double )*(size_t) nax );
         corner = astMalloc( sizeof( double )*(size_t) nax );

/* Copy the centres and corners from the supplied Boxes into the above
   arrays, in the requested order. */
         if( boxfirst ) {
            astBoxPoints( this, centre, corner );
            astBoxPoints( reg, centre + nax_this, corner + nax_this );
         } else {
            astBoxPoints( reg, centre, corner );
            astBoxPoints( this, centre + nax_reg, corner + nax_reg );
         }

/*  Create the new Box, initially with no uncertainty. */
         new = (AstRegion *) astBox( bfrm, 0, centre, corner, NULL, "",
                                     status );

/* Free resources .*/
         centre = astFree( centre );
         corner = astFree( corner );

/* Now attempt to merge with an Interval. The result will be an Interval.
   Both Intervals must be un-negated. */
      } else if( astIsAInterval( reg )  && !neg_this && !neg_reg ) {

/* Allocate memory to store the bounds of the returned Interval. */
         lbnd = astMalloc( sizeof( double )*(size_t) nax );
         ubnd = astMalloc( sizeof( double )*(size_t) nax );

/* Copy the centre and corner from the supplied Box into the required part
   of the above arrays. */
         if( boxfirst ) {
            centre = lbnd;
            corner = ubnd;
         } else {
            centre = lbnd + nax_reg;
            corner = ubnd + nax_reg;
         }
         astBoxPoints( this, centre, corner );

/* Convert these centre and corner positions into upper and lower bounds. */
         if( astOK ) {
            for( i = 0; i < nax_this; i++ ) {
               centre[ i ] = 2*centre[ i ] - corner[ i ];
               if( centre[ i ] > corner[ i ] ) {
                  temp = centre[ i ];
                  centre[ i ] = corner[ i ];
                  corner[ i ] = temp;
               }
            }
         }

/* Get the bounds from the interval and add them into the above arrays. */
         if( boxfirst ) {
            astIntervalPoints( reg, lbnd + nax_this, ubnd + nax_this );
         } else {
            astIntervalPoints( reg, lbnd, ubnd );
         }

/*  Create the new Interval, initially with no uncertainty. */
         new = (AstRegion *) astInterval( bfrm, lbnd, ubnd, NULL, "",
                                          status );

/* Free resources .*/
         lbnd = astFree( lbnd );
         ubnd = astFree( ubnd );

/* Now attempt to merge with a NullRegion. The result will be an
   Interval. The NullRegion must be negated and the Box must not. */
      } else if( astIsANullRegion( reg ) && !neg_this && neg_reg ) {

/* Allocate memory to store the bounds of the returned Interval. */
         lbnd = astMalloc( sizeof( double )*(size_t) nax );
         ubnd = astMalloc( sizeof( double )*(size_t) nax );

/* Copy the centre and corner from the supplied Box into the required part
   of the above arrays. */
         if( boxfirst ) {
            centre = lbnd;
            corner = ubnd;
         } else {
            centre = lbnd + nax_reg;
            corner = ubnd + nax_reg;
         }
         astBoxPoints( this, centre, corner );

/* Convert these centre and corner positions into upper and lower bounds. */
         if( astOK ) {
            for( i = 0; i < nax_this; i++ ) {
               centre[ i ] = 2*centre[ i ] - corner[ i ];
               if( centre[ i ] > corner[ i ] ) {
                  temp = centre[ i ];
                  centre[ i ] = corner[ i ];
                  corner[ i ] = temp;
               }
            }

/* Fill the other axes with bad values to indicate they are unbounded. */
            if( boxfirst ) {
               for( i = nax_this; i < nax; i++ ) {
                  lbnd[ i ] = AST__BAD;
                  ubnd[ i ] = AST__BAD;
               }
            } else {
               for( i = 0; i < nax_reg; i++ ) {
                  lbnd[ i ] = AST__BAD;
                  ubnd[ i ] = AST__BAD;
               }
            }

/*  Create the new Interval, initially with no uncertainty. */
            new = (AstRegion *) astInterval( bfrm, lbnd, ubnd, NULL, "",
                                             status );
         }

/* Free resources .*/
         lbnd = astFree( lbnd );
         ubnd = astFree( ubnd );

/* Now attempt to merge with a PointList. The result will be a PointList.
   Both Regions must be un-negated. */
      } else if( astIsAPointList( reg ) && !neg_this && !neg_reg ) {

/* We can only do this if the Box has zero width on each axis (i.e.
   represents a point). Get the Box centre and corner. */
         centre = astMalloc( sizeof( double )*(size_t) nax_this );
         corner = astMalloc( sizeof( double )*(size_t) nax_this );
         astBoxPoints( this, centre, corner );

/* Get the size of the Box's uncertainty region. */
         lbnd = astMalloc( sizeof( double )*(size_t) nax_this );
         ubnd = astMalloc( sizeof( double )*(size_t) nax_this );
         bunc = astGetUncFrm( this, AST__BASE );
         astGetRegionBounds( bunc, lbnd, ubnd );

/* Set "ok" to zero if the Box does not have zero width on any axis. Here
   "zero width" means a width less than half the uncertainty on the axis. */
         if( astOK ) {

            ok = 1;
            for( i = 0; i < nax_this; i++ ) {
               if( fabs( centre[ i ] - corner[ i ] ) >
                   0.25*fabs( ubnd[ i ] - lbnd[ i ] ) ) {
                  ok = 0;
                  break;
               }
            }

/* If the Box is a point, we go on to create a new PointList. */
            if( ok ) {

/* Get a PointSet holding the axis values in the supplied PointList data.
   Also get the number of points in the PointSet and pointers to the arrays
   holding the axis values. */
               astPointListPoints( reg, &pset_reg );
               npnt = astGetNpoint( pset_reg );
               ptr_reg = astGetPoints( pset_reg );

/*  Create a new PointSet with room for the same number of points, but
    with the extra required axes. Get pointers to its axis arrays. */
               pset_new = astPointSet( npnt, nax, "", status );
               ptr_new = astGetPoints( pset_new );

/* Copy the PointList axis values into the new PointSet, and then include
   the extra axis values defined by the Box to each point. */
               if( astOK ) {

                  for( j = 0; j < nax_reg; j++ ) {
                     p = ptr_reg[ j ];
                     q = ptr_new[ boxfirst ? nax_this + j : j ];
                     for( i = 0; i < npnt; i++ ) *(q++) = *(p++);
                  }

                  for( j = 0; j < nax_this; j++ ) {
                     p = centre + j;
                     q = ptr_new[ boxfirst ? j : nax_reg + j ];
                     for( i = 0; i < npnt; i++ ) *(q++) = *p;
                  }

/*  Create the new PointList, initially with no uncertainty. */
                  new = (AstRegion *) astPointList( bfrm, pset_new, NULL,
                                                    "", status );
               }

/* Free resources .*/
               pset_new = astAnnul( pset_new );
               pset_reg = astAnnul( pset_reg );
            }
         }
         centre = astFree( centre );
         corner = astFree( corner );
         lbnd = astFree( lbnd );
         ubnd = astFree( ubnd );
         bunc = astAnnul( bunc );

      }

/* If a new Region was created above, propagate remaining attributes of
   the supplied Region to it. */
      if( new ) {
         astRegOverlay( new, this, 1 );

/* The above Prism constructors create the Prism with the correct value
   for the Nagated attribute (i.e. zero). Ensure the above call to
   astRegOverlay has not changed this. */
         astClearNegated( new );

/* If both the supplied Regions have uncertainty, assign the new Region an
   uncertainty. */
         if( astTestUnc( this ) && astTestUnc( reg ) ) {

/* Get the uncertainties from the two supplied Regions. */
            unc_this = astGetUncFrm( this, AST__BASE );
            unc_reg = astGetUncFrm( reg, AST__BASE );

/* Combine them into a single Region (a Prism), in the correct order. */
            if( boxfirst ) {
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

         if( boxfirst ) {
            cfrm = (AstFrame *) astCmpFrame( frm_this, frm_reg, "", status );
         } else {
            cfrm = (AstFrame *) astCmpFrame( frm_reg, frm_this, "", status );
         }

/* Get the base -> current Mappings from the two Region FrameSets, and
   combine them into a single parallel CmpMap that connects bfrm and cfrm. */
         map_this = astGetMapping( ((AstRegion *) this)->frameset, AST__BASE,
                                   AST__CURRENT );
         map_reg = astGetMapping( reg->frameset, AST__BASE, AST__CURRENT );

         if( boxfirst ) {
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
      bfrm = astAnnul( bfrm );
   }

/* If an error has occurred, annul the returned pointer. */
   if( !astOK ) result = astAnnul( result );

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
*     #include "box.h"
*     void RegBaseBox( AstRegion *this, double *lbnd, double *ubnd, int *status )

*  Class Membership:
*     Box member function (over-rides the astRegBaseBox protected
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
   AstBox *this;                 /* Pointer to Box structure */
   double axcen;                 /* Central axis value */
   double axlen;                 /* Half width of box on axis */
   int i;                        /* Axis index */
   int nc;                       /* No. of axes in base Frame */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the Box structure */
   this = (AstBox *) this_region;

/* Ensure cached information is up to date. */
   Cache( this, 0, status );

/* Get the number of base Frame axes in the Region. */
   nc = astGetNin( this_region->frameset );

/* The first point is the centre of the box, the second point is the half
   size of the box on each axis.*/
   for( i = 0; i < nc; i++ ) {
      axcen = this->centre[ i ];
      axlen = this->extent[ i ];
      lbnd[ i ] = axcen - axlen;
      ubnd[ i ] = axcen + axlen;
   }
}

static AstPointSet *RegBaseGrid( AstRegion *this, int *status ){
/*
*  Name:
*     RegBaseGrid

*  Purpose:
*     Return a PointSet containing points spread through the volume of a
*     Region.

*  Type:
*     Private function.

*  Synopsis:
*     #include "box.h"
*     AstPointSet *RegBaseGrid( AstRegion *this, int *status )

*  Class Membership:
*     Box member function (over-rides the astRegBaseGrid protected
*     method inherited from the Region class).

*  Description:
*     This function returns a PointSet containing a set of points spread
*     through the volume of the supplied Box. The points refer to the base
*     Frame of the encapsulated FrameSet.

*  Parameters:
*     this
*        Pointer to the Region.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the PointSet. If the Region is unbounded, a NULL pointer
*     will be returned.

*  Notes:
*    - A NULL pointer is returned if an error has already occurred, or if
*    this function should fail for any reason.
*/

/* Local Variables: */
   AstFrame *frm;                 /* Base Frame in encapsulated FrameSet */
   AstPointSet *result;           /* Returned pointer */
   double *lbnd;                  /* Pointer to array of lower bounds of box */
   double *ubnd;                  /* Pointer to array of upper bounds of box */
   int meshsize;                  /* Requested size of grid */
   int naxes;                     /* No. of axes in base Frame */

/* Initialise */
   result = NULL;

/* Check the local error status. */
   if ( !astOK ) return NULL;

/* If the Region structure contains a pointer to a PointSet holding
   a previously created grid, return it. */
   if( this->basegrid ) {
      result = astClone( this->basegrid );

/* Otherwise, create a new one, but only if the Box is bounded. */
   } else if( astGetBounded( this ) ) {

/* Get the base Frame in the Region's FrameSet. */
      frm = astGetFrame( this->frameset, AST__BASE );

/* Get the number of axes in the base Frame */
      naxes = astGetNaxes( frm );

/* Get the bounds of the Region in the base Frame. */
      lbnd = astMalloc( sizeof( double )*(size_t) naxes );
      ubnd = astMalloc( sizeof( double )*(size_t) naxes );
      astRegBaseBox( this, lbnd, ubnd );

/* Get the number of points which would be used to create a boundary
   mesh. We use the same number to determine the number of points in the
   grid. */
      meshsize = astGetMeshSize( this );

/* Create the PointSet holding the grid. */
      result = astFrameGrid( frm, meshsize, lbnd, ubnd );

/* Save the returned pointer in the Region structure so that it does not
   need to be created again next time this function is called. */
      if( astOK && result ) this->basegrid = astClone( result );

/* Free remaining resources. */
      frm = astAnnul( frm );
      lbnd = astFree( lbnd );
      ubnd = astFree( ubnd );
   }

/* Annul the result if an error occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return the result */
   return result;
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
*     #include "box.h"
*     AstPointSet *RegBaseMesh( AstRegion *this, int *status )

*  Class Membership:
*     Box member function (over-rides the astRegBaseMesh protected
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
#define NP_EDGE 50                /* No. of points for determining geodesic
                                     length of each axis. */

/* Local Variables: */
   AstFrame *frm;                 /* Base Frame in encapsulated FrameSet */
   AstFrame *pfrm0;               /* Primary Frame defining axis 0 */
   AstFrame *pfrm1;               /* Primary Frame defining axis 1 */
   AstPointSet *result;           /* Returned pointer */
   const char *class0;            /* Pointer to class string from pfrm0 */
   const char *class1;            /* Pointer to class string from pfrm1 */
   double **ptr;                  /* Pointers to data */
   double *ax;                    /* Pointer to next first axis value */
   double *ay;                    /* Pointer to next second axis value */
   double *lbnd;                  /* Pointer to array of lower bounds of box */
   double *ubnd;                  /* Pointer to array of lower bounds of box */
   double c[ 5 ][ 2 ];            /* Positions of corners for 2D boxes */
   double dx;                     /* Increment along first axis of 2D box */
   double dy;                     /* Increment along second axis of 2D box */
   double edge_len[ 4 ];          /* Length of each edge of 2D boundary */
   double lax;                    /* Previous value on first axis */
   double lay;                    /* Previous value on second axis */
   double len;                    /* Length of edge of 2D boundary */
   double p0[ 2 ];                /* Position in 2D Frame */
   double p1[ 2 ];                /* Position in 2D Frame */
   double ppd;                    /* Points per unit distance */
   double total_len;              /* Total length of 2D boundary */
   int flat;                      /* Assume Frame geometry is flat? */
   int i;                         /* Point index */
   int iaxis;                     /* Axis index */
   int iedge;                     /* Edge index */
   int ip;                        /* Index of next point */
   int metric;                    /* Does Frame have a usable metric? */
   int nax0;                      /* No. of axes in first primary Frame */
   int nax1;                      /* No. of axes in second primary Frame */
   int naxes;                     /* No. of axes in base Frame */
   int np;                        /* No. of points in returned PointSet */
   int np0;                       /* No. of points per edge */
   int np_axis;                   /* No. of points per axis in ND box */
   int np_edge[ 4 ];              /* No. of points per edge in 2D box */
   int paxis;                     /* Axis index in primary Frame */
   int single;                    /* Does the Box occupy a single point? */

/* Initialise */
   result= NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* If the Region structure contains a pointer to a PointSet holding
   a previously created mesh, return it. */
   if( this->basemesh ) {
      result = astClone( this->basemesh );

/* Otherwise, create a new mesh. */
   } else {

/* Get the base Frame in the Region's FrameSet. */
      frm = astGetFrame( this->frameset, AST__BASE );

/* Get the number of axes in the base Frame */
      naxes = astGetNaxes( frm );

/* Get the bounds of the Region in the base Frame. */
      lbnd = astMalloc( sizeof( double )*(size_t) naxes );
      ubnd = astMalloc( sizeof( double )*(size_t) naxes );
      astRegBaseBox( this, lbnd, ubnd );

/* Get the requested number of points to put on the mesh. */
      np = astGetMeshSize( this );

/* See if the box occupies a single point. */
      single = 1;
      for( i = 0; i < naxes; i++ ) {
         if( ubnd[ i ] > lbnd[ i ] ) {
            single = 0;
            break;
         }
      }

/* If so, we return a PointSet holding a single point. */
      if( single ) {
         result = astPointSet( 1, naxes, "", status );
         ptr = astGetPoints( result );
         if( astOK ) {
            for( i = 0; i < naxes; i++ ) {
               ptr[ i ][ 0 ] = 0.5*( lbnd[ 0 ] + ubnd[ 0 ] );
            }
         }

/* Otherwise, first deal with 1-D boxes. */
      } else if( naxes == 1 ) {

/* The boundary of a 1-D box consists of 2 points - the two extreme values.
   Create a PointSet to hold 2 1-D values, and store the extreme values. */
         result = astPointSet( 2, 1, "", status );
         ptr = astGetPoints( result );
         if( astOK ) {
            ptr[ 0 ][ 0 ] = lbnd[ 0 ];
            ptr[ 0 ][ 1 ] = ubnd[ 0 ];
         }

/* Now deal with 2-D boxes. */
      } else if( naxes == 2 ){

/* Store the coords of each corner for easy access. */
         c[ 0 ][ 0 ] = lbnd[ 0 ];
         c[ 0 ][ 1 ] = lbnd[ 1 ];
         c[ 1 ][ 0 ] = lbnd[ 0 ];
         c[ 1 ][ 1 ] = ubnd[ 1 ];
         c[ 2 ][ 0 ] = ubnd[ 0 ];
         c[ 2 ][ 1 ] = ubnd[ 1 ];
         c[ 3 ][ 0 ] = ubnd[ 0 ];
         c[ 3 ][ 1 ] = lbnd[ 1 ];
         c[ 4 ][ 0 ] = lbnd[ 0 ];
         c[ 4 ][ 1 ] = lbnd[ 1 ];

/* See if we can assume that the frame has flat geometry. This is the case
   if both axes belong to simple Frames, or are 1D, but may not be the case
   if either axis does not belong to a simple Frame that has more than 1
   axis. */
         astPrimaryFrame( frm, 0, &pfrm0, &paxis );
         astPrimaryFrame( frm, 1, &pfrm1, &paxis );
         class0 = astGetClass( pfrm0 );
         class1 = astGetClass( pfrm1 );
         nax0 = astGetNaxes( pfrm0 );
         nax1 = astGetNaxes( pfrm1 );
         if( astOK ) {
            flat = ( !strcmp( class0, "Frame" ) || nax0 == 1 ) &&
                   ( !strcmp( class1, "Frame" ) || nax1 == 1 );
         } else {
            flat = 0;
         }

/* Our choice of distribution of points depends on whether the axes have the
   same units or not. If the axes have the same units, or if they share the
   same primary Frame, then we assume that the axes span some space in which
   a single "distance" is defined. If not, (e.g. for a Frame representing
   frequency in Hz on one axis - typical value 1.0E10, and slit position in
   metres on the other axis - typical value 1.0E-2) we assume that "distance"
   is defined differently for each axis. The primary frame requirement is
   because some classes of Frame (e.g. SkyFrame) have a defined distance,
   even though the Units attributes for its axes may differ (since Units
   refers to the external representation - e.g. hours and degrees for a
   SkyFrame - rather than the internal representation). */
         if( astOK && !strcmp( astGetUnit( frm, 0 ), astGetUnit( frm, 1 ) ) ) {
            metric = 1;
         } else {
            metric = ( pfrm0 == pfrm1 );
         }

/* If we have a usable metric, distribute the points according to the aspect
   ratio of the box (i.e. the shorter box sides gets fewer points). */
         if( metric ){

/* Find the approximate geodesic length of each edge of the box. Since
   the edges of the box may not correspond to single geodesic (e.g. a
   line of constant latitude is not a geodesic), we do this by testing
   a set of points along each edge of the box and finding the total of the
   geodesic distances between each pair of adjacent points. Initialise the
   total length round all edges. */
            total_len = 0.0;

/* Do each edge in turn.*/
            for( iedge = 0; iedge < 4; iedge++ ) {

/* Find the increment in x and y between adjacent points along this edge.
   We put a point at each end of the edge. Note, one of dx or dy will be
   zero since the edges are parallel to the axes. */
               dx = ( c[ iedge + 1 ][ 0 ] - c[ iedge ][ 0 ] )/( NP_EDGE - 1 );
               dy = ( c[ iedge + 1 ][ 1 ] - c[ iedge ][ 1 ] )/( NP_EDGE - 1 );

/* Store the coords of the first point. */
               p0[ 0 ] = c[ iedge ][ 0 ];
               p0[ 1 ] = c[ iedge ][ 1 ];

/* Initialise the length of this edge and loop round the remaining points. */
               len = 0.0;
               for( i = 1; i < NP_EDGE; i++ ) {

/* Save the position of the previous point. */
                  p1[ 0 ] = p0[ 0 ];
                  p1[ 1 ] = p0[ 1 ];

/* Find the position of the new point. */
                  p0[ 0 ] = p1[ 0 ] + dx;
                  p0[ 1 ] = p1[ 1 ] + dy;

/* Increment the length of the edge by the geodesic distance from this point
   to the previous point. If the Frame is flat, this is simple (given that we
   know that one of dx and dy must be zero). */
                  if( flat ) {
                     len += fabs( dx + dy );
                  } else {
                     len += astDistance( frm, p0, p1 );
                  }
               }

/* Save the length of this edge, and also form the total length round all
   edges. */
               edge_len[ iedge ] = len;
               total_len += len;
            }

/* Find the average number of points per unit geodesic distance around the
   boundary. */
            if( total_len > 0.0 ) {
               ppd = np / total_len;

/* Use this, with the geodesic length of each edge found above, to find the
   number of points to put along each edge of the boundary, and update the
   total number of points to use. In the returned boundary we put a point at
   the start of each edge but not at the end (since the end of one edge will
   be the start of the next edge). */
               np = 0;
               for( iedge = 0; iedge < 4; iedge++ ) {
                  np_edge[ iedge ] = (int) ( edge_len[ iedge ]*ppd );
                  if( np_edge[ iedge ] == 0 ) np_edge[ iedge ] = 1;
                  np += np_edge[ iedge ];
               }

/* Report error if total length round box is zero. */
            } else if( astOK ) {
               astError( AST__INTER, "astRegBaseMesh(%s): Distance around "
                         "box perimeter is zero (internal AST programming "
                         "error).", status, astGetClass( this ) );
            }

/* If the Frame has no usable metric, give an equal number of points
   (equal to a quarter of the total) to all 4 sides of the box. */
         } else {
            np0 = (int) ( 0.25*np );
            np = 0;
            for( iedge = 0; iedge < 4; iedge++ ) {
               np_edge[ iedge ] = np0;
               np += np_edge[ iedge ];
            }
         }

/* Create a PointSet with enough room and get a pointer to its data arrays. */
         result = astPointSet( np, 2, "", status );
         ptr = astGetPoints( result );
         if( astOK ) {

/* Initialise pointers to the first element for each axis in the returned
   PointSet. */
            ax = ptr[ 0 ];
            ay = ptr[ 1 ];

/* Loop round each edge. */
            for( iedge = 0; iedge < 4; iedge++ ) {

/* Find the increment in x and y between adjacent points along this edge. */
               dx = ( c[ iedge + 1 ][ 0 ] - c[ iedge ][ 0 ] )/ np_edge[ iedge ];
               dy = ( c[ iedge + 1 ][ 1 ] - c[ iedge ][ 1 ] )/ np_edge[ iedge ];

/* Store the first point. */
               lax = c[ iedge ][ 0 ];
               lay = c[ iedge ][ 1 ];
               *(ax++) = lax;
               *(ay++) = lay;

/* Loop round the remaining points, incrementing the pointers at the same
   time. */
               for( i = 1; i < np_edge[ iedge ]; i++, ax++, ay++ ) {

/* Find the position of the new point and store it. */
                  lax += dx;
                  lay += dy;
                  *ax = lax;
                  *ay = lay;
               }
            }
         }

/* Free resources. */
         pfrm0 = astAnnul( pfrm0 );
         pfrm1 = astAnnul( pfrm1 );

/* Now deal with boxes with more than 2 dimensions. */
      } else {

/* Number of samples along each edge of the hyper-cube. */
         np_axis = 1 + (int) pow( np/(2*naxes), 1.0/(naxes-1) );
         if( np_axis < 2 ) np_axis = 2;

/* Each face of the hyper-cube will have np_axis**(naxes-1) points, and there
   are 2*naxes faces. Create a PointSet with the correct number of
   points. */
         np = 2*naxes;
         for( iaxis = 1; iaxis < naxes; iaxis++ ) np *= np_axis;
         result = astPointSet( np, naxes, "", status );
         ptr = astGetPoints( result );
         if( astOK ) {

/* Initialise the index of the next point to add into the PointSet. */
            ip = 0;

/* Create the upper and lower faces for each axis in turn. */
            for( iaxis = 0; iaxis < naxes; iaxis++ ) {

/* First do the upper face for this axis. */
               ip += MakeGrid( naxes, ptr, ip, lbnd, ubnd, NULL, np_axis,
                               iaxis, ubnd[ iaxis ], status );

/* Now do the lower face for this axis. */
               ip += MakeGrid( naxes, ptr, ip, lbnd, ubnd, NULL, np_axis,
                               iaxis, lbnd[ iaxis ], status );
            }

/* Remove any unused space at the end of the PointSet. */
            if( ip < np ) astSetNpoint( result, ip );
         }
      }

/* Same the returned pointer in the Region structure so that it does not
   need to be created again next time this function is called. */
      if( astOK && result ) this->basemesh = astClone( result );

/* Free resources. */
      frm = astAnnul( frm );
      lbnd = astFree( lbnd );
      ubnd = astFree( ubnd );
   }

/* Annul the result if an error has occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return a pointer to the output PointSet. */
   return result;

/* Undefine macros local to this function. */
#undef NP_EDGE

}

static AstRegion *RegBasePick( AstRegion *this_region, int naxes, const int *axes,
                               int *status ){
/*
*  Name:
*     RegBasePick

*  Purpose:
*     Return a Region formed by picking selected base Frame axes from the
*     supplied Region.

*  Type:
*     Private function.

*  Synopsis:
*     #include "box.h"
*     AstRegion *RegBasePick( AstRegion *this, int naxes, const int *axes,
*                             int *status )

*  Class Membership:
*     Box member function (over-rides the astRegBasePick protected
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
   AstPointSet *pset;      /* Holds axis values defining the supplied Region */
   AstRegion *bunc;        /* The uncertainty in the supplied Region */
   AstRegion *result;      /* Returned Region */
   AstRegion *unc;         /* The uncertainty in the returned Region */
   double **ptr;           /* Holds axis values defining the supplied Region */
   double *cen;            /* Base Frm axis values at centre of returned Box */
   double *cor;            /* Base Frm axis values at a corner of returned Box */
   int i;                  /* Index of axis within returned Region */

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

/* Get pointers to the coordinate data in the parent Region structure. */
   pset = this_region->points;
   ptr = astGetPoints( pset );

/* Get space to hold the centre and corner of the Box in the new Frame. */
   cen = astMalloc( sizeof( *cen )*naxes );
   cor = astMalloc( sizeof( *cor )*naxes );

/* Check pointers can be used safely. */
   if( astOK ) {

/* Copy the centre and corner axis values for the selected axes into the
   arrays allocated above. */
      for( i = 0; i < naxes; i++ ) {
         cen[ i ] = ptr[ axes[ i ] ][ 0 ];
         cor[ i ] = ptr[ axes[ i ] ][ 1 ];
      }

/* Create the new Box. */
      result = (AstRegion *) astBox( frm, 0, cen, cor, unc, "", status );
   }

/* Free resources */
   frm = astAnnul( frm );
   bfrm = astAnnul( bfrm );
   if( unc ) unc = astAnnul( unc );
   cen = astFree( cen );
   cor = astFree( cor );

/* Return a NULL pointer if an error has occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return the result. */
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
*     #include "box.h"
*     double *RegCentre( AstRegion *this, double *cen, double **ptr,
*                        int index, int ifrm, int *status )

*  Class Membership:
*     Box member function (over-rides the astRegCentre protected
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
*    an AST__INTER error code if this method is called with either "ptr" or
*    "cen" not NULL. If "ptr" and "cen" are both NULL, then no error is
*    reported if this method is invoked on a Region of an unsuitable class,
*    but NULL is always returned.

*/

/* Local Variables: */
   AstBox *this;       /* Pointer to Box structure */
   AstFrame *frm;      /* Pointer to Box's base Frame */
   double **rptr;      /* Data pointers for Region PointSet */
   double *bc;         /* Base Frame centre position */
   double *corner;     /* Array holding corner axis values */
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

/* Get a pointer to the Box structure. */
   this = (AstBox *) this_region;

/* First ensure cached information (which includes the centre coords)
   is up to date. */
   Cache( this, 0, status );

/* Get the number of axis values per point in the base and current Frames. */
   ncb = astGetNin( this_region->frameset );
   ncc = astGetNout( this_region->frameset );

/* If the centre coords are to be returned, return either a copy of the
   base Frame centre coords, or transform the base Frame centre coords
   into the current Frame. First ensure cached information (which
   includes the centre coords) is up to date. */
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

/* If the centre position was supplied in the base Frame, store the
   centre coords in this->centre, skipping bad values. */
         } else {
            bc = this->centre;
            for( ic = 0; ic < ncb; ic++ ) {
               axval = cen ? cen[ ic ] : ptr[ ic ][ index ];
               if( axval != AST__BAD ) bc[ ic ] = axval;
            }
         }

/* Find the coordinates at the new box corner. */
         frm = astGetFrame( this_region->frameset, AST__BASE );
         corner = GeoCorner( frm, ncb, bc, this->geolen, NULL, status );
         frm = astAnnul( frm );

/* ... and change the coords in the parent Region structure. */
         for( ic = 0; ic < ncb; ic++ ) {
            rptr[ ic ][ 0 ] = bc[ ic ];
            rptr[ ic ][ 1 ] = corner[ ic ];
         }

/* Free resources */
         if( ifrm == AST__CURRENT ) bc = astFree( bc );
         corner = astFree( corner );

/* Indicate the cached info in the Box structure is out of date. */
         astResetCache( this );

/* Any base Frame mesh or grid is now no good, so annul it. */
         if( this_region->basemesh ) this_region->basemesh = astAnnul( this_region->basemesh );
         if( this_region->basegrid ) this_region->basegrid = astAnnul( this_region->basegrid );

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
*     Check if a set of points fall on the boundary of a given Box.

*  Type:
*     Private function.

*  Synopsis:
*     #include "box.h"
*     int RegPins( AstRegion *this, AstPointSet *pset, AstRegion *unc,
*                  int **mask, int *status )

*  Class Membership:
*     Box member function (over-rides the astRegPins protected
*     method inherited from the Region class).

*  Description:
*     This function returns a flag indicating if the supplied set of
*     points all fall on the boundary of the given Box.
*
*     Some tolerance is allowed, as specified by the uncertainty Region
*     stored in the supplied Box "this", and the supplied uncertainty
*     Region "unc" which describes the uncertainty of the supplied points.

*  Parameters:
*     this
*        Pointer to the Box.
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
   AstBox *large_box;           /* Box slightly larger than "this" */
   AstBox *small_box;           /* Box slightly smaller than "this" */
   AstBox *this;                /* Pointer to the Box structure. */
   AstFrame *frm;               /* Base Frame in supplied Box */
   AstPointSet *ps1;            /* Points masked by larger Box */
   AstPointSet *ps2;            /* Points masked by larger and smaller Boxes */
   AstRegion *tunc;             /* Uncertainity Region from "this" */
   double **ptr;                /* Pointer to axis values in "ps2" */
   double *large;               /* A corner position in the larger Box */
   double *safe;                /* An interior point in "this" */
   double *lbnd_tunc;           /* Lower bounds of "this" uncertainty Region */
   double *lbnd_unc;            /* Lower bounds of supplied uncertainty Region */
   double *p;                   /* Pointer to next axis value */
   double *small;               /* A corner position in the smaller Box */
   double *ubnd_tunc;           /* Upper bounds of "this" uncertainty Region */
   double *ubnd_unc;            /* Upper bounds of supplied uncertainty Region */
   double *wid;                 /* Widths of "this" border */
   int i;                       /* Axis index */
   int j;                       /* Point index */
   int nc;                      /* No. of axes in Box base frame */
   int np;                      /* No. of supplied points */
   int result;                  /* Returned flag */

/* Initialise */
   result = 0;
   if( mask ) *mask = NULL;

/* Check the inherited status. */
   if( !astOK ) return result;

/* Get a pointer to the Box structure. */
   this = (AstBox *) this_region;

/* Ensure cached information is up to date. */
   Cache( this, 0, status );

/* Get the number of base Frame axes in the Box, and check the supplied
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
   boundary of the Box for it still to be considered to be on the boundary.
   First get the Region which defines the uncertainty within the Box being
   checked (in its base Frame), re-centre it on the interior point found
   above (to avoid problems if the uncertainty region straddles a
   discontinuity), and get its bounding box. */
   tunc = astGetUncFrm( this, AST__BASE );
   if( safe ) astRegCentre( tunc, safe, NULL, 0, AST__CURRENT );
   lbnd_tunc = astMalloc( sizeof( double )*(size_t) nc );
   ubnd_tunc = astMalloc( sizeof( double )*(size_t) nc );
   astGetRegionBounds( tunc, lbnd_tunc, ubnd_tunc );

/* Also get the Region which defines the uncertainty of the supplied
   points and get its bounding box. First re-centre the uncertainty at the
   interior position to avoid problems from uncertainties that straddle a
   discontinuity. */
   if( unc ) {
      if( safe ) astRegCentre( unc, safe, NULL, 0, AST__CURRENT );
      lbnd_unc = astMalloc( sizeof( double )*(size_t) nc );
      ubnd_unc = astMalloc( sizeof( double )*(size_t) nc );
      astGetRegionBounds( unc, lbnd_unc, ubnd_unc );
   } else {
      lbnd_unc = NULL;
      ubnd_unc = NULL;
   }

/* The required border width for each axis is half of the total width
   of the two bounding boxes. Use a zero sized box "unc" if no box was
   supplied. */
   wid = astMalloc( sizeof( double )*(size_t) nc );
   large = astMalloc( sizeof( double )*(size_t) nc );
   small = astMalloc( sizeof( double )*(size_t) nc );
   if( astOK ) {
      if( unc ) {
         for( i = 0; i < nc; i++ ) {
            wid[ i ] = 0.5*( fabs( astAxDistance( frm, i + 1, lbnd_tunc[ i ], ubnd_tunc[ i ] ) )
                           + fabs( astAxDistance( frm, i + 1, lbnd_unc[ i ], ubnd_unc[ i ] ) ) );
         }
      } else {
         for( i = 0; i < nc; i++ ) {
            wid[ i ] = fabs( 0.5*astAxDistance( frm, i + 1, lbnd_tunc[ i ], ubnd_tunc[ i ] ) );
         }
      }

/* Create two new Boxes, one of which is larger than "this" by the widths
   found above, and the other of which is smaller than "this" by the widths
   found above. */
      for( i = 0; i < nc; i++ ) {
         large[ i ] = this->centre[ i ]  + this->extent[ i ] + wid[ i ];
         small[ i ] = this->extent[ i ] - wid[ i ];
         if( small[ i ] < 0.0 ) small[ i ] = 0.0;
         small[ i ] += this->centre[ i ];
      }

      large_box = astBox( frm, 0, this->centre, large, NULL, "", status );
      small_box = astBox( frm, 0, this->centre, small, NULL, "", status );

/* Negate the smaller region.*/
      astNegate( small_box );

/* Points are on the boundary of "this" if they are inside both the large
   box and the negated smallbox. First transform the supplied PointSet
   using the large box, then transform them using the negated smaller Box. */
      ps1 = astTransform( large_box, pset, 1, NULL );
      ps2 = astTransform( small_box, ps1, 1, NULL );

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
      large_box = astAnnul( large_box );
      small_box = astAnnul( small_box );
      ps1 = astAnnul( ps1 );
      ps2 = astAnnul( ps2 );
   }

   tunc = astAnnul( tunc );
   frm = astAnnul( frm );
   lbnd_tunc = astFree( lbnd_tunc );
   ubnd_tunc = astFree( ubnd_tunc );
   if( unc ) lbnd_unc = astFree( lbnd_unc );
   if( unc ) ubnd_unc = astFree( ubnd_unc );
   wid = astFree( wid );
   large = astFree( large );
   small = astFree( small );
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
*     #include "box.h"
*     int astTraceRegion( AstRegion *this, int n, double *dist, double **ptr );

*  Class Membership:
*     Box member function (overrides the astTraceRegion method
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
   AstMapping *map;
   AstPointSet *bpset;
   AstPointSet *cpset;
   double **bptr;
   double d;
   double lbnd[ 2 ];
   double ubnd[ 2 ];
   int i;
   int ncur;
   int result;

/* Initialise */
   result = 0;

/* Check inherited status. */
   if( ! astOK ) return result;

/* Check it is 2-dimensional. */
   if( astGetNin( this_region->frameset ) == 2 ) result = 1;

/* Check we have some points to find. */
   if( result && n > 0 ) {

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

/* Get the bounds of the Region in the base Frame. */
         astRegBaseBox( this_region, lbnd, ubnd );

/* Loop round each point. Each edge of the box covers a parameteric
   distance of 0.25, regardless of the aspect ratio of the box. */
         for( i = 0; i < n; i++ ) {

/* The right hand edge starts at 0.75 (parameter increases top to bottom). */
            d = 4*dist[ i ] - 3;
            if( d > 0 ) {
               bptr[ 0 ][ i ] = ubnd[ 0 ];
               bptr[ 1 ][ i ] = ( 1.0 - d )*ubnd[ 1 ] + d*lbnd[ 1 ];

/* The top edge starts at 0.5 (parameter increases left to right). */
            } else {
               d += 1.0;
               if( d > 0 ) {
                  bptr[ 0 ][ i ] = ( 1.0 - d )*lbnd[ 0 ] + d*ubnd[ 0 ];
                  bptr[ 1 ][ i ] = ubnd[ 1 ];

/* The left hand edge starts at 0.25 (parameter increases bottom to top). */
               } else {
                  d += 1.0;
                  if( d > 0 ) {
                     bptr[ 0 ][ i ] = lbnd[ 0 ];
                     bptr[ 1 ][ i ] = ( 1.0 - d )*lbnd[ 1 ] + d*ubnd[ 1 ];

/* The bottom edge starts at 0.0 (parameter increases right to left). */
                  } else {
                     d += 1.0;
                     bptr[ 0 ][ i ] = ( 1.0 - d )*ubnd[ 0 ] + d*lbnd[ 0 ];
                     bptr[ 1 ][ i ] = lbnd[ 1 ];
                  }
               }
            }
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
*     #include "box.h"
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
      ((AstBox *) this )->stale = 1;
      (*parent_resetcache)( this, status );
   }
}

static void SetClosed( AstRegion *this, int value, int *status ){
/*
*  Name:
*     SetClosed

*  Purpose:
*     Set a value for the Closed attribute of a Region.

*  Type:
*     Private function.

*  Synopsis:
*     #include "box.h"
*     void SetClosed( AstRegion *this, int value, int *status )

*  Class Membership:
*     Box member function (over-rides the protected astSetClosed
*     method inherited from the Region class).

*  Description:
*     This function sets a new value for the Closed attribute of a Region.

*  Parameters:
*     this
*        Pointer to the Region.
*     value
*        The new attribute value.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   int old;

/* Check the global error status. */
   if ( !astOK ) return;

/* Get the original attribute value */
   old = astGetClosed( this );

/* Invoke the set method inherited from the parent Region class */
   (*parent_setclosed)( this, value, status );

/* If the new value is not the same as the old value, indicate that we
   need to re-calculate the cached information in the Box. */
   if( value != old ) astResetCache( this );
}

static void SetNegated( AstRegion *this, int value, int *status ){
/*
*  Name:
*     SetNegated

*  Purpose:
*     Set a value for the Negated attribute of a Region.

*  Type:
*     Private function.

*  Synopsis:
*     #include "box.h"
*     void SetNegated( AstRegion *this, int value, int *status )

*  Class Membership:
*     Box member function (over-rides the protected astSetNegated
*     method inherited from the Region class).

*  Description:
*     This function sets a new value for the Negated attribute of a Region.

*  Parameters:
*     this
*        Pointer to the Region.
*     value
*        The new attribute value.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   int old;

/* Check the global error status. */
   if ( !astOK ) return;

/* Get the original attribute value */
   old = astGetNegated( this );

/* Invoke the set method inherited from the parent Region class */
   (*parent_setnegated)( this, value, status );

/* If the new value is not the same as the old value, indicate that we
   need to re-calculate the cached information in the Box. */
   if( value != old ) astResetCache( this );
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
*     #include "box.h"
*     void SetRegFS( AstRegion *this_region, AstFrame *frm, int *status )

*  Class Membership:
*     Box method (over-rides the astSetRegFS method inherited from
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

/* Indicate that we need to re-calculate the cached information in the Box. */
   astResetCache( this_region );
}

static void SetUnc( AstRegion *this, AstRegion *unc, int *status ){
/*
*  Name:
*     SetUnc

*  Purpose:
*     Store uncertainty information in a Region.

*  Type:
*     Private function.

*  Synopsis:
*     #include "box.h"
*     void SetUnc( AstRegion *this, AstRegion *unc, int *status )

*  Class Membership:
*     Box method (over-rides the astSetUnc method inherited from the
*     Region class).

*  Description:
*     Each Region (of any class) can have an "uncertainty" which specifies
*     the uncertainties associated with the boundary of the Region. This
*     information is supplied in the form of a second Region. The uncertainty
*     in any point on the boundary of a Region is found by shifting the
*     associated "uncertainty" Region so that it is centred at the boundary
*     point being considered. The area covered by the shifted uncertainty
*     Region then represents the uncertainty in the boundary position.
*     The uncertainty is assumed to be the same for all points.
*
*     The uncertainty is usually specified when the Region is created, but
*     this function allows it to be changed at any time.

*  Parameters:
*     this
*        Pointer to the Region which is to be assigned a new uncertainty.
*     unc
*        Pointer to the new uncertainty Region. This must be either a Box,
*        a Circle or an Ellipse. A deep copy of the supplied Region will be
*        taken, so subsequent changes to the uncertainty Region using the
*        supplied pointer will have no effect on the Region "this".
*     status
*        Pointer to the inherited status variable.
*/

/* Check the inherited status. */
   if( !astOK ) return;

/* Invoke the astSetUnc method inherited from the parent Region class. */
   (*parent_setunc)( this, unc, status );

/* Indicate that we need to re-calculate the cached information in the Box. */
   astResetCache( this );
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
*     #include "box.h"
*     AstMapping *Simplify( AstMapping *this, int *status )

*  Class Membership:
*     Box method (over-rides the astSimplify method inherited
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
   AstBox *box;                  /* Pointer to Box structure */
   AstBox *newbox;               /* Pointer to simpler Box */
   AstFrame *frm;                /* Pointer to current Frame */
   AstMapping *map;              /* Base -> current Mapping */
   AstMapping *result;           /* Result pointer to return */
   AstPointSet *basemesh;        /* Mesh of base Frame positions */
   AstPointSet *mesh;            /* Mesh of current Frame positions */
   AstPointSet *ps1;             /* Box corners in base Frame */
   AstPointSet *ps2;             /* Box corners in current Frame */
   AstPolygon *newpoly;          /* New Polygon to replace Box */
   AstRegion *prism;             /* Prism combining all axes */
   AstRegion *new;               /* Pointer to simplified Region */
   AstRegion *this;              /* Pointer to supplied Region structure */
   AstRegion *unc;               /* Pointer to uncertainty Region */
   double **ptr1;                /* Pointers to axis values in ps1 */
   double **ptr2;                /* Pointers to axis values in ps2 */
   double *constants;            /* Axis constants array */
   double *lbnd;                 /* Lower bounds for new Box */
   double *ubnd;                 /* Upper bounds for new Box */
   double corners[8];            /* Box corners in current Frame */
   double k;                     /* Axis constant value */
   double lb;                    /* Lower axis bound */
   double ub;                    /* Upper axis bound */
   int *inperm;                  /* Input axis permutation array */
   int *outperm;                 /* Output axis permutation array */
   int closed;                   /* Was original Region closed? */
   int feed;                     /* Source of value for current axis */
   int right_handed;             /* Is the new Frame right handed? */
   int ic;                       /* Axis index */
   int isInterval;               /* Is the simplified Box an Interval */
   int isNull;                   /* Is the simplified Box a NullRegion? */
   int neg;                      /* Was original Region negated? */
   int nin;                      /* No. of base Frame axes (Mapping inputs) */
   int nout;                     /* No. of current Frame axes (Mapping outputs) */
   int simpler;                  /* Has some simplication taken place? */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the Region structure. */
   this = (AstRegion *) this_mapping;

/* Invoke the parent Simplify method inherited from the Region class. This
   will simplify the encapsulated FrameSet and uncertainty Region. */
   new = (AstRegion *) (*parent_simplify)( this_mapping, status );

/* Note if any simplification took place. This is assumed to be the case
   if the pointer returned by the above call is different to the supplied
   pointer. */
   simpler = ( new != this );

/* Get the Mapping from base to current Frame. */
   map = astGetMapping( new->frameset, AST__BASE, AST__CURRENT );

/* Get the number of inputs and outputs for the PermMap */
   nin = astGetNin( map );
   nout = astGetNout( map );

/* If the Mapping from base to current Frame is a PermMap, we now explicitly
   how to swap the axes of the Box to produce either a new Box or an
   Interval. */
   if( astIsAPermMap( map ) ){

/* See if the new Box is Negated and/or Closed.*/
      neg = astGetNegated( new );
      closed = astGetClosed( new );

/* Get a pointer to the Box structure. */
      newbox = (AstBox *) new;

/* Ensure cached information is up to date. */
      Cache( newbox, 0, status );

/* Get the input and output permutation arrays and the array of constants
   from the PermMap. */
      inperm = astGetInPerm( map );
      outperm = astGetOutPerm( map );
      constants = astGetConstants( map );

/* Allocate memory to hold the axis bounds for the box in the current
   Frame. */
      lbnd = astMalloc( sizeof( double )*(size_t) nout );
      ubnd = astMalloc( sizeof( double )*(size_t) nout );

/* Check pointers can be used safely. */
      if( astOK ) {

/* Set flags indicating that the Box can be simplified, and does not result
   in a NullRegion or an Interval. */
         simpler = 1;
         isNull = 0;
         isInterval = 0;

/* Check each output (which corresponds to an axis in the current Frame
   of the Box). */
         for( ic = 0; ic < nout; ic++ ) {

/* Find the input (a base Frame axis) which feeds this output when the
   forward transformation of the PermMap is used. */
            feed = outperm[ ic ];

/* If this output is fed a constant (i.e. does not depend on any of the
   inputs), then the output axis limits are equal to this constant. */
            if( feed < 0 ) {
               lbnd[ ic ] = constants[ (-feed) - 1 ];
               ubnd[ ic ] = constants[ (-feed) - 1 ];

/* If this output is fed the constant AST__BAD (i.e. does not depend on any of
   the inputs), then the output axis is unbounded and we will consequently
   create an Interval rather than a Box. */
            } else if( feed >= nin ) {
               lbnd[ ic ] = AST__BAD;
               ubnd[ ic ] = AST__BAD;

/* If this output is fed the value of an input, then the output limits
   are equal to the corresponding input limits. */
            } else {
               lbnd[ ic ] = newbox->centre[ feed ] - newbox->extent[ feed ];
               ubnd[ ic ] = newbox->centre[ feed ] + newbox->extent[ feed ];
            }

/* If either bound is missing we will produce an Interval rather than a
   Box. */
            if( lbnd[ ic ] == AST__BAD || ubnd[ ic ] == AST__BAD ) {
               isInterval = 1;
            }
         }

/* If any base Frame axes are not present in the current Frame, we need
   to check that the PermMap selects a slice which intersects the base
   Frame box. If the slice does not intersect the base Frame box, then
   the resulting Region willbe NullRegion. To do this, we check each
   element in the input axis permutation array, "inperm". */
         for( ic = 0; ic < nin; ic++ ) {

/* Find the output (a current Frame axis) which feeds this input when the
   inverse transformation of the PermMap is used. */
            feed = inperm[ ic ];

/* If this input is fed a constant (i.e. does not depend on any of the
   outputs), then we must check that this constant is within the range of
   axis values covered by the Box. If not then the simplified Box represents
   a slice through the coordinate system which does not pass through the
   original Box. In this case the simplified Region is a NullRegion
   instead of a Box. */
            if( feed < 0 ) {
               k = constants[ (-feed) - 1 ];
               lb = newbox->centre[ ic ] - newbox->extent[ ic ];
               ub = newbox->centre[ ic ] + newbox->extent[ ic ];

               if( closed == neg ) {
                  isNull = ( k <= lb || k >= ub );
               } else {
                  isNull = ( k < lb || k > ub );
               }

/* If this input is fed the constant AST__BAD (i.e. does not depend on any of
   the outputs), then the input axis is unbounded and will consequently
   extend beyond the Box. In this case the simplified Region will be a
   NullRegion. */
            } else if( feed >= nout ) {
               isNull = 1;

/* If this input is fed the value of an output, then check that the
   relationship is bi-directional. If it is not, we cannot simplify the
   Box. */
            } else if( outperm[ feed ] != ic ) {
               simpler = 0;
               break;
            }
         }

/* If the Box can be simplified, create a new Region of an appropriate
   class. */
         if( simpler ) {

/* Get any uncertainty from the supplied Box (it will already have been
   simplified by the parent Simplify method). */
            unc = astTestUnc( new ) ? astGetUncFrm( new, AST__CURRENT ) : NULL;

/* Get the Frame represented by the Region. */
            frm = astGetFrame( new->frameset, AST__CURRENT );

/* We can now replace the original Region with the simplified Region so annul
   the original pointer. */
            new = astAnnul( new );

/* Create a new Region of the required class. */
            if( isNull ) {
               new = (AstRegion *) astNullRegion( frm, unc, "", status );

            } else if( isInterval ){
               new = (AstRegion *) astInterval( frm, lbnd, ubnd, unc, "", status );

            } else {
               new = (AstRegion *) astBox( frm, 1, lbnd, ubnd, unc, "", status );
            }

/* If the original box was Negated. Negate the new one. */
            if( neg ) astNegate( new );

/* Free resources */
            if( unc ) unc = astAnnul( unc );
            frm = astAnnul( frm );

         }
      }

/* Free resources */
      inperm = astFree( inperm );
      outperm = astFree( outperm );
      constants = astFree( constants );
      lbnd = astFree( lbnd );
      ubnd = astFree( ubnd );

/* If the Mapping from base to current Frame is not a PermMap or a UnitMap, we
   attempt to simplify the Box by re-defining it within its current Frame.
   Transforming the box from its base to its current FRame may result in
   the region no longer being a box. We test this by transforming a set of
   bounds on the box boundary. */
   } else if( !astIsAUnitMap( map ) ){

/* Get pointer to current Frame. */
      frm = astGetFrame( new->frameset, AST__CURRENT );

/* Get a mesh of points covering the Box in its current Frame. */
      mesh = astRegMesh( new );

/* Get the Region describing the positional uncertainty within the Box in
   its current Frame. */
      unc = astGetUncFrm( new, AST__CURRENT );

/* Find the best fitting box (defined in the current Frame) through these
   points */
      newbox = BestBox( frm, mesh, unc, status );

/* See if all points within this mesh fall on the boundary of the best
   fitting Box, to within the uncertainty of the Region. */
      if( newbox && astRegPins( newbox, mesh, NULL, NULL ) ) {

/* If so, check that the inverse is true (we need to transform the
   simplified boxes mesh into the base Frame of he original box for use by
   astRegPins). */
         (void) astAnnul( mesh );
         mesh = astRegMesh( newbox );
         basemesh = astTransform( map, mesh, 0, NULL );
         if( astRegPins( new, basemesh, NULL, NULL ) ) {

/* If so, use the new Box in place of the original Box. */
            (void) astAnnul( new );
            new = astClone( newbox );
            simpler = 1;
         }

/* Free resources. */
         basemesh = astAnnul( basemesh );

/* If the transformed Box is not itself a Box, see if it can be
   represented accurately by a Polygon. This is only possible for
   2-dimensional Boxes. */
      } else if( nin == 2 && nout == 2 ) {

/* Create a PointSet holding the base Frame axis values at the four
   corners of the Box. */
         ps1 = astPointSet( 4, 2, "", status );
         ptr1 = astGetPoints( ps1 );
         if( astOK ) {
            box = (AstBox *) new;
            Cache( box, 0, status );

/* The order in which the polygon vertices are stored determines whether
   the interior or exterior of the polygon forms the inside of the
   Region. We want the inside to be the interior. First create a Polygon
   in which the vertices are stored in clockwise order within the
   new coordinate Frame. If the new Polygon is not bounded, use
   anti-clockwise order. */
            for( right_handed = 0; right_handed < 2; right_handed++ ) {

               if( right_handed ) {
                  ptr1[ 0 ][ 0 ] = box->centre[ 0 ] - box->extent[ 0 ];
                  ptr1[ 1 ][ 0 ] = box->centre[ 1 ] + box->extent[ 1 ];

                  ptr1[ 0 ][ 1 ] = box->centre[ 0 ] - box->extent[ 0 ];
                  ptr1[ 1 ][ 1 ] = box->centre[ 1 ] - box->extent[ 1 ];

                  ptr1[ 0 ][ 2 ] = box->centre[ 0 ] + box->extent[ 0 ];
                  ptr1[ 1 ][ 2 ] = box->centre[ 1 ] - box->extent[ 1 ];

                  ptr1[ 0 ][ 3 ] = box->centre[ 0 ] + box->extent[ 0 ];
                  ptr1[ 1 ][ 3 ] = box->centre[ 1 ] + box->extent[ 1 ];

               } else {
                  ptr1[ 0 ][ 3 ] = box->centre[ 0 ] - box->extent[ 0 ];
                  ptr1[ 1 ][ 3 ] = box->centre[ 1 ] + box->extent[ 1 ];

                  ptr1[ 0 ][ 2 ] = box->centre[ 0 ] - box->extent[ 0 ];
                  ptr1[ 1 ][ 2 ] = box->centre[ 1 ] - box->extent[ 1 ];

                  ptr1[ 0 ][ 1 ] = box->centre[ 0 ] + box->extent[ 0 ];
                  ptr1[ 1 ][ 1 ] = box->centre[ 1 ] - box->extent[ 1 ];

                  ptr1[ 0 ][ 0 ] = box->centre[ 0 ] + box->extent[ 0 ];
                  ptr1[ 1 ][ 0 ] = box->centre[ 1 ] + box->extent[ 1 ];
               }

/* Transform the Box corners into the current Frame. */
               ps2 = astTransform( map, ps1, 1, NULL );
               ptr2 = astGetPoints( ps2 );
               if( astOK ) {

/* Create a Polygon from these points. */
                  for( ic = 0; ic < 4; ic++ ) {
                     corners[ ic ] = ptr2[ 0 ][ ic ];
                     corners[ 4 + ic ] = ptr2[ 1 ][ ic ];
                  }
                  newpoly = astPolygon( frm, 4, 4, corners, unc, "", status );

/* If the Polygon is bounded, break out of the loop. */
                  if( astGetBounded( newpoly ) ) break;
               }

/* Free resources. */
               newpoly = astAnnul( newpoly );
               ps2 = astAnnul( ps2 );
            }

/* See if all points within the Box mesh fall on the boundary of this
   Polygon, to within the uncertainty of the Region. */
            if( astRegPins( newpoly, mesh, NULL, NULL ) ) {

/* If so, use the new Polygon in place of the original Box. */
               (void) astAnnul( new );
               new = astClone( newpoly );
               simpler = 1;
            }

/* Free resources. */
            newpoly = astAnnul( newpoly );
         }

         ps1 = astAnnul( ps1 );
      }

/* If we have yet been able to produce a simpler region, we now try
   splitting the Box into two separate Boxes defined in separate
   coordinate Frames. If either of these two Boxes can be simplified,
   create a Prism containing the two simplified Boxes, and attempt to
   simplify the Prism. Otherwise a clone of "new" is returned by
   astConvertToPrism. */
      if( !simpler ) {
         prism = astConvertToPrism( new );

         if( prism != new ) {
            simpler = 1;
            (void) astAnnul( new );
            new = prism;

         } else {
            prism = astAnnul( prism );
         }
      }

      frm = astAnnul( frm );
      mesh = astAnnul( mesh );
      unc = astAnnul( unc );
      if( newbox ) newbox = astAnnul( newbox );
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

static AstPointSet *Transform( AstMapping *this, AstPointSet *in,
                               int forward, AstPointSet *out, int *status ) {
/*
*  Name:
*     Transform

*  Purpose:
*     Apply a Box to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "box.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     Box member function (over-rides the astTransform protected
*     method inherited from the Mapping class).

*  Description:
*     This function takes a Box and a set of points encapsulated in a
*     PointSet and transforms the points by setting axis values to
*     AST__BAD for all points which are outside the region. Points inside
*     the region are copied unchanged from input to output.

*  Parameters:
*     this
*        Pointer to the Box.
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
*     match the number of axes in the Frame represented by the Box.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstBox *box;                  /* Pointer to Box */
   AstFrame *frm;                /* Pointer to base Frame in FrameSet */
   AstPointSet *pset_tmp;        /* Pointer to PointSet holding base Frame positions*/
   AstPointSet *result;          /* Pointer to output PointSet */
   AstRegion *reg;               /* Pointer to Region */
   double **ptr_out;             /* Pointer to output coordinate data */
   double **ptr_tmp;             /* Pointer to base Frame coordinate data */
   double axval;                 /* Input axis value */
   int closed;                   /* Is the boundary part of the Region? */
   int coord;                    /* Zero-based index for coordinates */
   int ncoord_out;               /* No. of coordinates per output point */
   int ncoord_tmp;               /* No. of coordinates per base Frame point */
   int neg;                      /* Is the Box negated?*/
   int npoint;                   /* No. of points */
   int ok;                       /* Is the point inside the Region? */
   int point;                    /* Loop counter for points */


/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain pointers to the Regionand to the Box. */
   reg = (AstRegion *) this;
   box = (AstBox *) this;

/* Apply the parent mapping using the stored pointer to the Transform member
   function inherited from the parent Region class. This function validates
   all arguments and generates an output PointSet if necessary,
   containing a copy of the input PointSet. */
   result = (*parent_transform)( this, in, forward, out, status );

/* We will now extend the parent astTransform method by performing the
   calculations needed to generate the output coordinate values. */

/* First use the encapsulated FrameSet to transform the supplied positions
   from the current Frame in the encapsulated FrameSet (the Frame
   represented by the Region), to the base Frame (the Frame in which the
   Region is defined). This call also returns a pointer to the base Frame
   of the encapsulated FrameSet. Note, the returned pointer may be a
   clone of the "in" pointer, and so we must be carefull not to modify the
   contents of the returned PointSet. */
   pset_tmp = astRegTransform( reg, in, 0, NULL, &frm );

/* Determine the numbers of points and coordinates per point from the base
   Frame PointSet and obtain pointers for accessing the base Frame and output
   coordinate values. */
   npoint = astGetNpoint( pset_tmp );
   ncoord_tmp = astGetNcoord( pset_tmp );
   ptr_tmp = astGetPoints( pset_tmp );
   ncoord_out = astGetNcoord( result );
   ptr_out = astGetPoints( result );

/* See if the boundary is part of the Region. */
   closed = astGetClosed( reg );

/* See if the Box is negated */
   neg = astGetNegated( reg );

/* Ensire the cached information is up to date. */
   Cache( box, 1, status );

/* Perform coordinate arithmetic. */
/* ------------------------------ */
   if ( astOK ) {

/* The logic used to combine axis values for negated and un-negated boxes
   is different. For negated boxes, a position is in the region if *any
   one* axis is not "close" to the box centre. */
      if( neg ) {

/* Loop round each point */
         for ( point = 0; point < npoint; point++ ) {

/* Assume the point is outside the Region (since the Region is
   negated, this means assuming it is within the box). */
            ok = 0;

/* Loop round each axis value at this point. We break as soon as we find
   a bad axis value or an axis value which is outside the box. */
            for ( coord = 0; coord < ncoord_tmp; coord++ ) {

/* The point is definiately not in the Region if any input axis value is bad. */
               axval = ptr_tmp[ coord ][ point ];
               if( axval == AST__BAD ) {
                  break;

/* Otherwise check the current axis value, depending on whether the
   boundary is included in the Region or not. Break as soon as an axis
   value is found which is outside the box limits (i.e. in the Region). */
               } else if( !astAxIn( frm, coord, box->lo[ coord ], box->hi[ coord ],
                                    axval, !closed ) ) {
                  ok = 1;
                  break;
               }
            }

/* If this point is not inside the Region store bad output axis values. */
            if( !ok ) {
               for ( coord = 0; coord < ncoord_out; coord++ ) {
                  ptr_out[ coord ][ point ] = AST__BAD;
               }
            }
         }

/* For un-negated boxes, a position is in the region if *all* axes are "close"
   to the box centre. */
      } else {

/* Loop round each point */
         for ( point = 0; point < npoint; point++ ) {

/* Assume the point is within the Region (i.e.inside the box). */
            ok = 1;

/* Loop round each axis value at this point. We break when we find a bad
   input point or if any axis value is outside the box. */
            for ( coord = 0; coord < ncoord_tmp; coord++ ) {

/* The point is not in the Region if any input axis value is bad. */
               axval = ptr_tmp[ coord ][ point ];
               if( axval == AST__BAD ) {
                  ok = 0;
                  break;

/* Otherwise check the current axis value, depending on whether the
   boundary is included in the Region or not. Break as soon as an axis
   value is found which is outside the box limits (i.e. outside the Region). */
               } else if( !astAxIn( frm, coord, box->lo[ coord ], box->hi[ coord ],
                                    axval, closed ) ) {
                  ok = 0;
                  break;
               }
            }

/* If this point is outside the Region store bad output axis values. */
            if( !ok ) {
               for ( coord = 0; coord < ncoord_out; coord++ ) {
                  ptr_out[ coord ][ point ] = AST__BAD;
               }
            }
         }
      }
   }

/* Free resources */
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
   AstBox *in;                /* Pointer to input Box */
   AstBox *out;               /* Pointer to output Box */
   int nax;                   /* Number of base Frame axes */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output Boxs. */
   in = (AstBox *) objin;
   out = (AstBox *) objout;

/* For safety, first clear any references to the input memory from
   the output Box. */
   out->extent = NULL;
   out->centre = NULL;
   out->lo = NULL;
   out->hi = NULL;
   out->geolen = NULL;

/* Copy dynamic memory contents */
   nax = astGetNin( ((AstRegion *) in)->frameset );
   out->extent = astStore( NULL, in->extent,
                           sizeof( double )*(size_t)nax );
   out->centre = astStore( NULL, in->centre,
                           sizeof( double )*(size_t)nax );
   out->lo = astStore( NULL, in->lo,
                           sizeof( double )*(size_t)nax );
   out->hi = astStore( NULL, in->hi,
                           sizeof( double )*(size_t)nax );
   out->geolen = astStore( NULL, in->geolen,
                           sizeof( double )*(size_t)nax );
}


/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for Box objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for Box objects.

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
   AstBox *this;                 /* Pointer to Box */

/* Obtain a pointer to the Box structure. */
   this = (AstBox *) obj;

/* Annul all resources. */
   this->extent = astFree( this->extent );
   this->centre = astFree( this->centre );
   this->lo = astFree( this->lo );
   this->hi = astFree( this->hi );
   this->geolen = astFree( this->geolen );
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for Box objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the Box class to an output Channel.

*  Parameters:
*     this
*        Pointer to the Box whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Write out values representing the instance variables for the
   Box class.  Accompany these with appropriate comment strings,
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
/* Implement the astIsABox and astCheckBox functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(Box,Region)
astMAKE_CHECK(Box)

AstBox *astBox_( void *frame_void, int form, const double point1[],
                 const double point2[], AstRegion *unc, const char *options, int *status, ...) {
/*
*++
*  Name:
c     astBox
f     AST_BOX

*  Purpose:
*     Create a Box.

*  Type:
*     Public function.

*  Synopsis:
c     #include "box.h"
c     AstBox *astBox( AstFrame *frame, int form, const double point1[],
c                     const double point2[], AstRegion *unc,
c                     const char *options, ... )
f     RESULT = AST_BOX( FRAME, FORM, POINT1, POINT2, UNC, OPTIONS, STATUS )

*  Class Membership:
*     Box constructor.

*  Description:
*     This function creates a new Box and optionally initialises its
*     attributes.
*
*     The Box class implements a Region which represents a box with sides
*     parallel to the axes of a Frame (i.e. an area which encloses a given
*     range of values on each axis). A Box is similar to an Interval, the
*     only real difference being that the Interval class allows some axis
*     limits to be unspecified. Note, a Box will only look like a box if
*     the Frame geometry is approximately flat. For instance, a Box centred
*     close to a pole in a SkyFrame will look more like a fan than a box
*     (the Polygon class can be used to create a box-like region close to a
*     pole).

*  Parameters:
c     frame
f     FRAME = INTEGER (Given)
*        A pointer to the Frame in which the region is defined. A deep
*        copy is taken of the supplied Frame. This means that any
*        subsequent changes made to the Frame using the supplied pointer
*        will have no effect the Region.
c     form
f     FORM = INTEGER (Given)
*        Indicates how the box is described by the remaining parameters.
*        A value of zero indicates that the box is specified by a centre
*        position and a corner position. A value of one indicates that the
*        box is specified by a two opposite corner positions.
c     point1
f     POINT1( * ) = DOUBLE PRECISION (Given)
c        An array of double, with one element for each Frame axis
f        An array with one element for each Frame axis
*        (Naxes attribute). If
c        "form"
f        FORM
*        is zero, this array should contain the coordinates at the centre of
*        the box.
c        If "form"
f        If FORM
*        is one, it should contain the coordinates at the corner of the box
*        which is diagonally opposite the corner specified by
c        "point2".
f        POINT2.
c     point2
f     POINT2( * ) = DOUBLE PRECISION (Given)
c        An array of double, with one element for each Frame axis
f        An array with one element for each Frame axis
*        (Naxes attribute) containing the coordinates at any corner of the
*        box.
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
c        initialising the new Box. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new Box. The syntax used is identical to that for the
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
c     astBox()
f     AST_BOX = INTEGER
*        A pointer to the new Box.

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
   AstBox *new;                  /* Pointer to new Box */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Obtain and validate a pointer to the supplied Frame structure. */
   frame = astCheckFrame( frame_void );

/* Initialise the Box, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitBox( NULL, sizeof( AstBox ), !class_init, &class_vtab,
                     "Box", frame, form, point1, point2, unc );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new Box's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new Box. */
   return new;
}

AstBox *astBoxId_( void *frame_void, int form, const double point1[],
                   const double point2[], void *unc_void, const char *options,
                   ... ) {
/*
*  Name:
*     astBoxId_

*  Purpose:
*     Create a Box.

*  Type:
*     Private function.

*  Synopsis:
*     #include "box.h"
*     AstBox *astBoxId_( AstFrame *frame, int form, const double point1[],
*                        const double point2[], AstRegion *unc,
*                        const char *options, ... )

*  Class Membership:
*     Box constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astBox constructor function. It returns an ID value (instead
*     of a true C pointer) to external users, and must be provided
*     because astBox_ has a variable argument list which cannot be
*     encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     The variable argument list also prevents this function from
*     invoking astBox_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astBox_.

*  Returned Value:
*     The ID value associated with the new Box.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstFrame *frame;              /* Pointer to Frame structure */
   AstBox *new;                  /* Pointer to new Box */
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

/* Initialise the Box, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitBox( NULL, sizeof( AstBox ), !class_init, &class_vtab,
                     "Box", frame, form, point1, point2, unc );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new Box's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new Box. */
   return astMakeId( new );
}

AstBox *astInitBox_( void *mem, size_t size, int init, AstBoxVtab *vtab,
                     const char *name, AstFrame *frame, int form,
                     const double point1[], const double point2[],
                     AstRegion *unc, int *status ) {
/*
*+
*  Name:
*     astInitBox

*  Purpose:
*     Initialise a Box.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "box.h"
*     AstBox *astInitBox_( void *mem, size_t size, int init, AstBoxVtab *vtab,
*                         const char *name, AstFrame *frame, int form,
*                         const double point1[], const double point2[],
*                         AstRegion *unc )

*  Class Membership:
*     Box initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new Box object. It allocates memory (if necessary) to accommodate
*     the Box plus any additional data associated with the derived class.
*     It then initialises a Box structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a Box at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the Box is to be initialised.
*        This must be of sufficient size to accommodate the Box data
*        (sizeof(Box)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the Box (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the Box
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the Box's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new Box.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the astGetClass
*        method).
*     frame
*        A pointer to the Frame in which the region is defined.
*     form
*        Indicates how the box is described by the remaining parameters.
*        A value of zero indicates that the box is specified by a centre
*        position and a corner position. A value of one indicates that the
*        box is specified by a two opposite corner positions.
*     point1
*        An array of double, with one element for each Frame axis (Naxes
*        attribute). If "form" is zero, this array should contain the
*        coordinates at the centre of the box. If "form" is one, it should
*        contain the coordinates at the corner of the box which is diagonally
*        opposite the corner specified by "point2".
*     point2
*        An array of double, with one element for each Frame axis (Naxes
*        attribute) containing the coordinates at any of the corners of
*        the box.
*     unc
*        A pointer to a Region which specifies the uncertainty in the
*        supplied positions (all points on the boundary of the new Box
*        being initialised are assumed to have the same uncertainty). A NULL
*        pointer can be supplied, in which case default uncertainties equal to
*        1.0E-6 of the dimensions of the new Box's bounding box are used.
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
*     A pointer to the new Box.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstBox *new;              /* Pointer to new Box */
   AstPointSet *pset;        /* PointSet to pass to Region initialiser */
   double **ptr;             /* Pointer to coords data in pset */
   int i;                    /* axis index */
   int nc;                   /* No. of axes */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitBoxVtab( vtab, name );

/* Initialise. */
   new = NULL;

/* Get the number of axis values required for each position. */
   nc = astGetNaxes( frame );

/* Create a PointSet to hold the supplied values, and get points to the
   data arrays. */
   pset = astPointSet( 2, nc, "", status );
   ptr = astGetPoints( pset );

/* Copy the supplied coordinates into the PointSet, checking that no bad
   values have been supplied. */
   for( i = 0; astOK && i < nc; i++ ) {
      if( point1[ i ] == AST__BAD ) {
         astError( AST__BADIN, "astInitBox(%s): The value of axis %d is "
                   "undefined at point 1 of the box.", status, name, i + 1 );
         break;
      }
      if( point2[ i ] == AST__BAD ) {
         astError( AST__BADIN, "astInitBox(%s): The value of axis %d is "
                   "undefined at point 2 of the box.", status, name, i + 1 );
         break;
      }
      ptr[ i ][ 0 ] = point1[ i ];
      ptr[ i ][ 1 ] = point2[ i ];
   }

/* If two corners were supplied, find and store the centre. */
   if( form == 1 ) {
      for( i = 0; i < nc; i++ ) {
         ptr[ i ][ 0 ] = 0.5*( point1[ i ] + point2[ i ] );
      }
   }

/* Check pointers can be used safely. */
   if( astOK ) {

/* Initialise a Region structure (the parent class) as the first component
   within the Box structure, allocating memory if necessary. */
      new = (AstBox *) astInitRegion( mem, size, 0, (AstRegionVtab *) vtab,
                                      name, frame, pset, unc );

      if ( astOK ) {

/* Initialise the Box data. */
/* ------------------------ */
         new->extent = NULL;
         new->centre = NULL;
         new->lo = NULL;
         new->hi = NULL;
         new->geolen = NULL;
         new->stale = 1;

/* If an error occurred, clean up by deleting the new Box. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Free resources. */
   pset = astAnnul( pset );

/* Return a pointer to the new Box. */
   return new;
}

AstBox *astLoadBox_( void *mem, size_t size, AstBoxVtab *vtab,
                     const char *name, AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadBox

*  Purpose:
*     Load a Box.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "box.h"
*     AstBox *astLoadBox( void *mem, size_t size, AstBoxVtab *vtab,
*                         const char *name, AstChannel *channel )

*  Class Membership:
*     Box loader.

*  Description:
*     This function is provided to load a new Box using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     Box structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a Box at the start of the memory
*     passed via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory into which the Box is to be
*        loaded.  This must be of sufficient size to accommodate the
*        Box data (sizeof(Box)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the Box (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the Box structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstBox) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new Box. If this is NULL, a pointer
*        to the (static) virtual function table for the Box class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "Box" is used instead.

*  Returned Value:
*     A pointer to the new Box.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstBox *new;              /* Pointer to the new Box */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this Box. In this case the
   Box belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstBox );
      vtab = &class_vtab;
      name = "Box";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
          astInitBoxVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built Box. */
   new = astLoadRegion( mem, size, (AstRegionVtab *) vtab, name,
                        channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "Box" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* There are no values to read. */
/* ---------------------------- */

/* Initialise Box data */
      new->extent = NULL;
      new->centre = NULL;
      new->lo = NULL;
      new->hi = NULL;
      new->geolen = NULL;
      new->stale = 1;

/* If an error occurred, clean up by deleting the new Box. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new Box pointer. */
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


void astBoxPoints_( AstBox *this, double *centre, double *corner,
                    int *status) {
   if ( !astOK ) return;
   (**astMEMBER(this,Box,BoxPoints))( this, centre, corner, status );
   return;
}















