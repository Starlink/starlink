/*
*+
*  Name:
*     ndfcentroffset

*  Type of Module:
*     C extension to Tcl.

*  Purpose:
*     Get an offset between a pair of NDFs by centroiding matched objects.

*  Usage:
*     ndfcentroffset ndfA frameA ndfB frameB points zoom

*  Description:
*     Given a pair of NDFs or NDF Sets (or one of each) and lists of 
*     points in the chosen coordinate system of each, this command
*     will centroid the positions in each NDF and attempt to match 
*     them up.  It will fail if the centroid corresponding to the item
*     in one list is too far from the one in the other list, or no 
*     centroid could be found for one or both, or the position appears
*     outside the bounds of any of the NDFs in either set.  If
*     the position is within the bounds of more than one NDF in
*     a single NDF Set (this is unlikely to be the case for sensible
*     data) the first such will be used to find the centroid.
*     If centroids can be found for both NDF Sets, the offsets implied
*     by each one will be averaged to generate an accurate offset 
*     between the two Sets, and return lists of the accurately 
*     centroided points for each one.

*  Arguments:
*     ndfA = ndf-or-ndfset object
*        The first NDF or NDF Set.
*     frameA = string
*        The frame in which the coordinates of the centroidable features
*        in the first NDF are supplied and returned.  May be a frame index
*        integer, a domain name, or CURRENT or BASE.
*     ndfB = ndf-or-ndfset object
*        The second NDF or NDF Set.
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
*     12-MAR-2001 (MBT):
*        Upgraded for use with Sets.

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
      double gxmat[ 2 ];
      double gymat[ 2 ];
      double **gxpos[ 2 ];
      double **gypos[ 2 ];
      double xoffset;
      double yoffset;
      double zoom;
      int i;
      int *iframes[ 2 ];
      int j;
      int nmatch;
      int nndf[ 2 ];
      int npoint;
      int oc;
      int xdim;
      int ydim;
      AstMapping **map[ 2 ];
      NdfOrNdfset *ndfset[ 2 ];
      Ndf **ndfs[ 2 ];
      Tcl_Obj *result;
      Tcl_Obj **ov;
      const F77_LOGICAL_TYPE true = F77_TRUE;
      const F77_LOGICAL_TYPE false = F77_FALSE;

/* Check syntax. */
      if ( objc != 7 ) {
         Tcl_WrongNumArgs( interp, 1, objv, arglist );
         return TCL_ERROR;
      }

/* Extract and validate ndf object arguments. */
      if ( NdfGetNdfFromObj( interp, objv[ 1 ], &ndfset[ 0 ] ) != TCL_OK ||
           NdfGetNdfFromObj( interp, objv[ 3 ], &ndfset[ 1 ] ) != TCL_OK ) {
         return TCL_ERROR;
      }

/* Get a list of ndf objects to work with. */
      for ( i = 0; i < 2; i++ ) {
         if ( ndfset[ i ]->nmember ) {
            nndf[ i ] = ndfset[ i ]->nmember;
            ndfs[ i ] = ndfset[ i ]->content.ndfs;
         }
         else {
            nndf[ i ] = 1;
            ndfs[ i ] = &ndfset[ i ];
         }
      }

/* Allocate space for frame indices. */
      for ( i = 0; i < 2; i++ ) {
         iframes[ i ] = malloc( max( 1, nndf[ i ] ) * sizeof( int ) );
         if ( tclmemok( interp, iframes[ i ] ) != TCL_OK ) {
            return TCL_ERROR;
         }
      }

/* Extract and validate WCS frame index arguments. */
      if ( NdfGetIframesFromObj( interp, objv[ 2 ], ndfset[ 0 ], 
                                 iframes[ 0 ] ) != TCL_OK ||
           NdfGetIframesFromObj( interp, objv[ 4 ], ndfset[ 1 ],
                                 iframes[ 1 ] ) != TCL_OK ) {
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
      if ( tclmemok( interp, xpos[ 0 ] ) != TCL_OK ||
           tclmemok( interp, ypos[ 0 ] ) != TCL_OK ||
           tclmemok( interp, xpos[ 1 ] ) != TCL_OK ||
           tclmemok( interp, ypos[ 1 ] ) != TCL_OK ) {
         return TCL_ERROR;
      }
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

/* Allocate some more workspace. */
      for ( i = 0; i < 2; i++ ) {
         map[ i ] = malloc( nndf[ i ] * sizeof( AstMapping * ) );
         xmat[ i ] = malloc( npoint * sizeof( double ) );
         ymat[ i ] = malloc( npoint * sizeof( double ) );
         gxpos[ i ] = malloc( nndf[ i ] * sizeof( double * ) );
         gypos[ i ] = malloc( nndf[ i ] * sizeof( double * ) );
         if ( tclmemok( interp, xmat[ i ] ) != TCL_OK ||
              tclmemok( interp, ymat[ i ] ) != TCL_OK ||
              tclmemok( interp, gxpos[ i ] ) != TCL_OK ||
              tclmemok( interp, gypos[ i ] ) != TCL_OK ) {
            return TCL_ERROR;
         }
         for ( j = 0; j < nndf[ i ]; j++ ) {
            gxpos[ i ][ j ] = malloc( npoint * sizeof( double ) );
            gypos[ i ][ j ] = malloc( npoint * sizeof( double ) );
            if ( tclmemok( interp, gxpos[ i ][ j ] ) != TCL_OK ||
                 tclmemok( interp, gypos[ i ][ j ] ) != TCL_OK ) {
               return TCL_ERROR;
            }
         }
      }

/* Define a macro to call a type-dependent function. */
#define CASE_CENT(Xccdtype,Xcnftype,Xgentype) \
                        case Xccdtype: \
\
/* Begin an error context. */ \
                           errMark(); \
\
/* Call the centroiding routine. */ \
                           F77_CALL(ccg1_cen##Xgentype)( \
                                     DOUBLE_ARG(&gxpos[ i ][ j ][ k ]), \
                                     DOUBLE_ARG(&gypos[ i ][ j ][ k ]), \
                                     Xcnftype##_ARG(ndf1->data), \
                                     INTEGER_ARG(&xdim), INTEGER_ARG(&ydim), \
                                     INTEGER_ARG(&ssize), LOGICAL_ARG(&true), \
                                     DOUBLE_ARG(&maxshift), \
                                     INTEGER_ARG(&maxit), \
                                     DOUBLE_ARG(&toler), \
                                     DOUBLE_ARG(&gxmat[ i ]), \
                                     DOUBLE_ARG(&gymat[ i ]), \
                                     INTEGER_ARG(status) ); \
\
/* Record whether the centroiding was successful, and if not annul the \
   error status. */ \
                           if ( *status == SAI__OK ) { \
                              done[ i ] = 1; \
                           } \
                           else { \
                              errAnnul( status ); \
                           } \
\
/* Exit the error context. */ \
                           errRlse(); \
                           break;

/* Call fortran routines which do the calculations. */
      ASTCALL(
         int done[ 2 ];
         int k;
         int maxit;
         int ssize;
         double cscale;
         double maxshift;
         double toler;
         Ndf1 *ndf1;

/* Start an AST context. */
         astBegin;

/* Ensure that all NDFs are mapped. */
         for ( i = 0; i < 2; i++ ) {
            for ( j = 0; j < nndf[ i ]; j++ ) {
               if ( ! ndfs[ i ][ j ]->content.ndf1->mapped ) {
                  domapdata( ndfs[ i ][ j ]->content.ndf1, status );
               }
            }
         }

/* Map each of the points from its coordinates in the displayed frame to */
/* GRID coordinates of each of the NDFs in its set.  This is required    */
/* so that we can see within which of the NDFs each point lies.          */
/* The overheads on astTran2 are quite high, so it is faster to do       */
/* them all here. */
         for ( i = 0; i < 2; i++ ) {
            for ( j = 0; j < nndf[ i ]; j++ ) {
               map[ i ][ j ] = astGetMapping( ndfs[ i ][ j ]->wcs, 
                                              iframes[ i ][ j ], AST__BASE );
               astTran2( map[ i ][ j ], npoint, xpos[ i ], ypos[ i ], 1, 
                         gxpos[ i ][ j ], gypos[ i ][ j ] );
            }
         }

/* Set up some parameters for the centroid location. */
         cscale = max( 1.0, 1.0 / zoom );
         ssize = (int) rint( cscale * 9.0 );
         maxshift = cscale * 5.5;
         toler = min( 0.5, cscale * 0.5 );
         maxit = 5;

/* Loop over each point, attempting to centroid it in both sets. */
         nmatch = 0;
         for ( k = 0; k < npoint; k++ ) {

/* Loop over both sets. */
            for ( i = 0; i < 2; i++ ) {

/* Find which NDF in this set contains this point. */
               done[ i ] = 0;
               for ( j = 0; j < nndf[ i ]; j++ ) {
                  ndf1 = ndfs[ i ][ j ]->content.ndf1;

/* Get the bounds. */
                  xdim = ndf1->ubnd[ 0 ] - ndf1->lbnd[ 0 ] + 1;
                  ydim = ndf1->ubnd[ 1 ] - ndf1->lbnd[ 1 ] + 1;

                  if ( ! done[ i ] &&
                       ( i == 0 || done[ 0 ] ) &&
                       gxpos[ i ][ j ][ k ] >= 0.5 &&
                       gxpos[ i ][ j ][ k ] <= 0.5 + (double) xdim &&
                       gypos[ i ][ j ][ k ] >= 0.5 &&
                       gypos[ i ][ j ][ k ] <= 0.5 + (double) ydim ) {

/* Use the macro we have defined to invoke one of the typed subroutines. */
                     switch ( CCD_TYPE( ndf1->mtype ) ) {
                        CASE_CENT(CCD_TYPE_B,BYTE,b)
                        CASE_CENT(CCD_TYPE_UB,UBYTE,ub)
                        CASE_CENT(CCD_TYPE_W,WORD,w)
                        CASE_CENT(CCD_TYPE_UW,UWORD,uw)
                        CASE_CENT(CCD_TYPE_I,INTEGER,i)
                        CASE_CENT(CCD_TYPE_R,REAL,r)
                        CASE_CENT(CCD_TYPE_D,DOUBLE,d)
                        default:
                           *status = SAI__ERROR;
                           errRep( "OFFSET_BADTYP", "Bad type value", status );
                     }

/* If the centroiding was successful, transform back to original coordinates. */
                     if ( done[ i ] ) {
                        astTran2( map[ i ][ j ], 1, &gxmat[ i ], &gymat[ i ],
                                  0, &xmat[ i ][ nmatch ], 
                                  &ymat[ i ][ nmatch ] );
                     }
                  }
               }
            }

/* Increment the number of matches if we managed to centroid it   */
/* successfully in both sets. */   
            if ( done[ 0 ] && done[ 1 ] ) {
               nmatch++;
            }
         }

/* Exit the AST context. */
         astEnd;

/* Get accurate X and Y offsets by averaging the values for each of the */
/* matched points. */
         if ( astOK && nmatch > 0 ) {
            F77_CALL(ccg1_mdifd)( LOGICAL_ARG(&false), DOUBLE_ARG(xmat[ 0 ]),
                                  DOUBLE_ARG(xmat[ 1 ]), INTEGER_ARG(&nmatch),
                                  DOUBLE_ARG(&xoffset), INTEGER_ARG(status) );
            F77_CALL(ccg1_mdifd)( LOGICAL_ARG(&false), DOUBLE_ARG(ymat[ 0 ]),
                                  DOUBLE_ARG(ymat[ 1 ]), INTEGER_ARG(&nmatch),
                                  DOUBLE_ARG(&yoffset), INTEGER_ARG(status) );
         }
      )

/* Undefine the macro we defined. */
#undef CASE_CENT

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
         for ( j = 0; j < nndf[ i ]; j++ ) {
            free( gxpos[ i ][ j ] );
            free( gypos[ i ][ j ] );
         }
         free( map[ i ] );
         free( gxpos[ i ] );
         free( gypos[ i ] );
         free( xpos[ i ] );
         free( ypos[ i ] );
         free( xmat[ i ] );
         free( ymat[ i ] );
         free( iframes[ i ] );
      }

/* Return with success status. */
      return TCL_OK;
   }


/* $Id$ */
