/*
*+
*  Name:
*     ndfcentroffset

*  Type of Module:
*     C extension to Tcl.

*  Purpose:
*     Get an offset between a pair of NDFs by centroding matched objects.

*  Usage:
*     ndfcentroffset ndfA frameA ndfB frameB points zoom

*  Description:
*     Given a pair of NDFs and lists of points in the chosen coordinate
*     system of each, this command will centroid the positions in
*     each NDF and attempt to match them up (only failing if the
*     centroid corresponding to the item in one list is too far
*     from the one in the other list, or no centroid could be found for
*     one or both).  Having done that, it will average the offsets
*     implied by each one to generate an accurate offset between the
*     two NDFs, and return lists of the accurately centroided points 
*     for each NDF.

*  Arguments:
*     ndfA = ndf object
*        The first NDF.
*     frameA = string
*        The frame in which the coordinates of the centroidable features
*        in the first NDF are supplied and returned.  May be a frame index
*        integer, a domain name, or CURRENT or BASE.
*     ndfB = ndf object
*        The second NDF.
*     frameB = string
*        The frame in which the coordinates of the centroidable features
*        in the second NDF are supplied and returned.  May be a frame index
*        integer, a domain name, or CURRENT or BASE.
*     points = list of quads
*        This list contains the approximate positions of identified 
*        objects in the two NDFs.  Each element of the list must give
*        the X and Y coordinates of a point for each NDF in the 
*        corresponding coordinate system, in the form {XA YA XB YB}.
*     zoom = real 
*        The zoom factor at which the objects were identified.  This is
*        used to match up the centroids, on the basis that if the display
*        of the image was all squashed up small the user is unlikely to
*        have selected positions to pixel accuracy.  However, this use
*        is a bit questionable, since in typical usage the user may 
*        have zoomed and unzoomed the display several times while 
*        marking points, while the zoom value passed in is likely to
*        be just the most recent value for the zoom factor.

*  Return Value:
*     The return value is normally a four-element list of the form
*     {nmatch xoffset yoffset matchpts}, where the elements of the list
*     have the following meanings:
*        nmatch = integer
*           The number of points which were successfully matched up
*           (centroided to similar positions) in both images.
*        xoffset = real
*           The accurate X coordinate of the origin of ndfA in the 
*           coordinates of ndfB.
*        yoffset = real
*           The accurate Y coordinate of the origin of ndfA in the
*           coordinates of ndfB.
*        matchpts = list of quads
*           Centroided coordinates of each of the points which were 
*           successfully centroided and matched up.  This contains
*           nmatch elements in the same form, {XA YA XB YB}, as the
*           points list.
*
*    If no points were successfully matched, the return value is the
*    single value 0.

*  Authors:
*     MBT: Mark Taylor (STARLINK)

*  History:
*     5-SEP-2000 (MBT):
*        Initial version.

*-
*/

#include <stdlib.h>
#include "tcl.h"
#include "tclndf.h"
#include "cnf.h"
#include "dat_par.h"
#include "ast.h"

   int NdfCentroffset( ClientData clientData, Tcl_Interp *interp, int objc,
                       Tcl_Obj *CONST objv[] ) {
      char *arglist = "ndfA frameA ndfB frameB points zoom";
                   /*  1    2      3    4      5      6      */
      double *xpos[ 2 ];
      double *ypos[ 2 ];
      double *xmat[ 2 ];
      double *ymat[ 2 ];
      double *gxpos[ 2 ];
      double *gypos[ 2 ];
      double *gxmat[ 2 ];
      double *gymat[ 2 ];
      double xoffset;
      double yoffset;
      double zoom;
      int i;
      int iframe[ 2 ];
      int j;
      int nmatch;
      int npoint;
      int oc;
      int xdim[ 2 ];
      int ydim[ 2 ];
      Ndf *ndf[ 2 ];
      Tcl_Obj *result;
      Tcl_Obj **ov;

/* Check syntax. */
      if ( objc != 7 ) {
         Tcl_WrongNumArgs( interp, 1, objv, arglist );
         return TCL_ERROR;
      }

/* Extract and validate ndf object arguments. */
      if ( NdfGetNdfFromObj( interp, objv[ 1 ], &ndf[ 0 ] ) != TCL_OK ||
           NdfGetNdfFromObj( interp, objv[ 3 ], &ndf[ 1 ] ) != TCL_OK ) {
         return TCL_ERROR;
      }

/* Extract and validate WCS frame index arguments. */
      if ( NdfGetIframeFromObj( interp, objv[ 2 ], ndf[ 0 ]->wcs, 
                                &iframe[ 0 ] ) != TCL_OK ||
           NdfGetIframeFromObj( interp, objv[ 4 ], ndf[ 1 ]->wcs,
                                &iframe[ 1 ] ) != TCL_OK ) {
         return TCL_ERROR;
      }

/* Extract and validate list arguments. */
      if ( Tcl_ListObjGetElements( interp, objv[ 5 ], &oc, &ov ) != TCL_OK ) {
         result = Tcl_GetObjResult( interp );
         Tcl_AppendStringsToObj( result, "Usage: ", Tcl_GetString( objv[ 0 ] ),
                                 " ", arglist, (char *) NULL );
         Tcl_SetObjResult( interp, result );
         return TCL_ERROR;
      }
      npoint = oc;
      xpos[ 0 ] = malloc( npoint * sizeof( double ) );
      ypos[ 0 ] = malloc( npoint * sizeof( double ) );
      xpos[ 1 ] = malloc( npoint * sizeof( double ) );
      ypos[ 1 ] = malloc( npoint * sizeof( double ) );
      for ( j = 0; j < npoint; j++ ) {
         int poc = 0;
         Tcl_Obj **pov;
         if ( Tcl_ListObjGetElements( interp, ov[ j ], &poc, &pov ) != TCL_OK
           || poc != 4
           || Tcl_GetDoubleFromObj( interp, pov[ 0 ], xpos[ 0 ] + j ) != TCL_OK
           || Tcl_GetDoubleFromObj( interp, pov[ 1 ], ypos[ 0 ] + j ) != TCL_OK
           || Tcl_GetDoubleFromObj( interp, pov[ 2 ], xpos[ 1 ] + j ) != TCL_OK
           || Tcl_GetDoubleFromObj( interp, pov[ 3 ], ypos[ 1 ] + j ) != TCL_OK
              ) {
            result = Tcl_NewStringObj( 
                     "points list elements must be numeric quads", -1 );
            Tcl_SetObjResult( interp, result );
            return TCL_ERROR;
         }
      }

/* Extract and validate numeric arguments. */
      if ( Tcl_GetDoubleFromObj( interp, objv[ 6 ], &zoom ) != TCL_OK ) {
         result = Tcl_GetObjResult( interp );
         Tcl_AppendStringsToObj( result, "Usage: ", Tcl_GetString( objv[ 0 ] ),
                                 " ", arglist, (char *) NULL );
         Tcl_SetObjResult( interp, result );
         return TCL_ERROR;
      }

/* Get the dimensions of the NDFs. */
      for ( i = 0; i < 2; i++ ) {
         xdim[ i ] = ndf[ i ]->ubnd[ 0 ] - ndf[ i ]->lbnd[ 0 ] + 1;
         ydim[ i ] = ndf[ i ]->ubnd[ 1 ] - ndf[ i ]->lbnd[ 1 ] + 1;
      }

/* Allocate some more workspace. */
      for ( i = 0; i < 2; i++ ) {
         gxpos[ i ] = malloc( npoint * sizeof( double ) );
         gypos[ i ] = malloc( npoint * sizeof( double ) );
         xmat[ i ] = malloc( npoint * sizeof( double ) );
         ymat[ i ] = malloc( npoint * sizeof( double ) );
         gxmat[ i ] = malloc( npoint * sizeof( double ) );
         gymat[ i ] = malloc( npoint * sizeof( double ) );
      }

/* Call fortran routines which do the calculations. */
      ASTCALL(
         int dummy_lbnd[ 2 ];
         int maxit;
         int ssize;
         double cscale;
         double maxshift;
         double toler;
         AstMapping *map[ 2 ];
         DECLARE_CHARACTER( type0, DAT__SZTYP );
         DECLARE_CHARACTER( type1, DAT__SZTYP );
         const F77_LOGICAL_TYPE true = F77_TRUE;
         const F77_LOGICAL_TYPE false = F77_FALSE;

/* Start an AST context. */
         astBegin;

/* Ensure that both arrays are mapped. */
         for ( i = 0; i < 2; i++ ) {
            if ( ! ndf[ i ]->mapped ) domapdata( ndf[ i ], status );
         }

/* Prepare the string arguments for fortran. */
         cnfExprt( ndf[ 0 ]->mtype, type0, DAT__SZTYP );
         cnfExprt( ndf[ 1 ]->mtype, type1, DAT__SZTYP );

/* Transform the point lists into grid coordinates. */
         for ( i = 0; i < 2; i++ ) {
            map[ i ] = astGetMapping( ndf[ i ]->wcs, iframe[ i ], AST__BASE );
            astTran2( map[ i ], npoint, xpos[ i ], ypos[ i ], 1, 
                      gxpos[ i ], gypos[ i ] );
         }

/* Set up some parameters for the centroid location. */
         cscale = max( 1.0, 1.0 / zoom );
         ssize = (int) rint( cscale * 9.0 );
         maxshift = cscale * 5.5;
         toler = min( 0.5, cscale * 0.5 );
         maxit = 5;

/* We set these values to unity so that prevent ccd1_cen2 effectivly returns
   positions in BASE frame, rather than Pixel frame, coordinates. */
         dummy_lbnd[ 0 ] = 1;
         dummy_lbnd[ 1 ] = 1;

/* Locate the matched centroids. */
         F77_CALL(ccd1_cen2)( CHARACTER_ARG(type0), 
                              (F77_POINTER_TYPE) &ndf[ 0 ]->data,
                              INTEGER_ARG(&xdim[ 0 ]), INTEGER_ARG(&ydim[ 0 ]),
                              INTEGER_ARG(dummy_lbnd),
                              CHARACTER_ARG(type1),
                              (F77_POINTER_TYPE) &ndf[ 1 ]->data,
                              INTEGER_ARG(&xdim[ 1 ]), INTEGER_ARG(&ydim[ 1 ]),
                              INTEGER_ARG(dummy_lbnd),
                              DOUBLE_ARG(gxpos[ 0 ]), DOUBLE_ARG(gypos[ 0 ]),
                              DOUBLE_ARG(gxpos[ 1 ]), DOUBLE_ARG(gypos[ 1 ]),
                              INTEGER_ARG(&npoint), INTEGER_ARG(&ssize),
                              LOGICAL_ARG(&true), DOUBLE_ARG(&maxshift),
                              INTEGER_ARG(&maxit), DOUBLE_ARG(&toler),
                              DOUBLE_ARG(gxmat[ 0 ]), DOUBLE_ARG(gymat[ 0 ]),
                              DOUBLE_ARG(gxmat[ 1 ]), DOUBLE_ARG(gymat[ 1 ]),
                              INTEGER_ARG(&nmatch), INTEGER_ARG(status)
                              TRAIL_ARG(type0) TRAIL_ARG(type1) );

/* Transform the centroided positions back from grid into frame coordinates. */
         if ( nmatch ) {
            for ( i = 0; i < 2; i++ ) {
               astTran2( map[ i ], nmatch, gxmat[ i ], gymat[ i ], 0,
                         xmat[ i ], ymat[ i ] );
            }

/* Get accurate X and Y offsets by averaging the values for each of the
   matched points. */
            F77_CALL(ccg1_mdifd)( LOGICAL_ARG(&false), DOUBLE_ARG(xmat[ 0 ]),
                                  DOUBLE_ARG(xmat[ 1 ]), INTEGER_ARG(&nmatch),
                                  DOUBLE_ARG(&xoffset), INTEGER_ARG(status) );
            F77_CALL(ccg1_mdifd)( LOGICAL_ARG(&false), DOUBLE_ARG(ymat[ 0 ]),
                                  DOUBLE_ARG(ymat[ 1 ]), INTEGER_ARG(&nmatch),
                                  DOUBLE_ARG(&yoffset), INTEGER_ARG(status) );
         }

/* Exit the AST context. */
         astEnd;
      )

/* Set the result object. */
      result = Tcl_NewIntObj( nmatch );
      if ( nmatch > 0 ) {
         Tcl_Obj *matchpts;
         Tcl_ListObjAppendElement( interp, result, 
                                   Tcl_NewDoubleObj( xoffset ) );
         Tcl_ListObjAppendElement( interp, result, 
                                   Tcl_NewDoubleObj( yoffset ) );
         matchpts = Tcl_NewListObj( 0, (Tcl_Obj **) NULL );
         for ( i = 0; i < nmatch; i++ ) {
            Tcl_Obj *ov[ 4 ];
            ov[ 0 ] = Tcl_NewDoubleObj( xmat[ 0 ][ i ] );
            ov[ 1 ] = Tcl_NewDoubleObj( ymat[ 0 ][ i ] );
            ov[ 2 ] = Tcl_NewDoubleObj( xmat[ 1 ][ i ] );
            ov[ 3 ] = Tcl_NewDoubleObj( ymat[ 1 ][ i ] );
            Tcl_ListObjAppendElement( interp, matchpts, 
                                      Tcl_NewListObj( 4, ov ) );
         }
         Tcl_ListObjAppendElement( interp, result, matchpts );
      }
      Tcl_SetObjResult( interp, result );

/* Free workspace memory. */
      for ( i = 0; i < 2; i++ ) {
         free( xpos[ i ] );
         free( ypos[ i ] );
         free( xmat[ i ] );
         free( ymat[ i ] );
         free( gxpos[ i ] );
         free( gypos[ i ] );
         free( gxmat[ i ] );
         free( gymat[ i ] );
      }

/* Return with success status. */
      return TCL_OK;
   }


/* $Id$ */
