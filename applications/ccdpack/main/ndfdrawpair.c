/*
*+
*  Name:
*     ndfdrawpair

*  Purpose:
*     Allow plotting of two NDFs on a GWM device.

*  Language:
*     Starlink C

*  Type of Module:
*     C extension to Tcl.

*  Description:
*     The ndfdrawpair command causes two NDFs or NDF Sets to be
*     drawn on the plotting device specified by the device parameter.
*     The entirety of the plotting surface is used, and the routine
*     can generate and cache pixel representations of the NDF in such a
*     way that repeatedly plotting the same images at different
*     positions is done quite efficiently.  The coordinate offset
*     of each NDF is specified, and if there is any overlap of good
*     pixels between the two, such pixels are drawn as the average of
*     the two NDFs.  The plotting surface is taken to extend from
*     (xorigin, yorigin) to (xorigin + xdev, yorigin + ydev), so if
*     any parts of the NDFs extend outside the rectangle thus defined,
*     they will be clipped.  The NDFs are drawn plotted scaled between
*     the percentile values given by the loperc and hiperc values.
*
*     The dimensions of the plotting device and positions of the NDFs
*     are given in 'view coordinates'.

*  Arguments:
*     device = string
*        Name of a PGPLOT output device on which to plot the images.
*     xorigin = real
*        X coordinate of the lower left corner of the plotting surface
*        in view coordinates.
*     yorigin = real
*        Y coordinate of the lower left corner of the plotting surface
*        in view coordinates.
*     xdev = integer
*        X size of the plotting device in view coordinates.
*     ydev = integer
*        Y size of the plotting device in view coordinates.
*     zoom = double
*        The factor by which the given frames should be magnified.
*        Resampling is done into frameA/frameB but with a magnification of
*        zoom added on, and a unit length in the resulting frame gives
*        the size of one plotting pixel (though not necessarily one
*        screen pixel, which is determined by the size of the plotting
*        device).
*     ndfsetA = ndf-or-ndfset object
*        The first NDF or NDF Set to plot.
*     frameA = string
*        Indicates the WCS frame into which the first NDF should be
*        resampled; may be a numerical frame index, a domain name, or
*        one of the special strings "CURRENT" or "BASE".
*     xoffA = real
*        X offset by which to translate the first NDF in frame coordinates.
*     yoffA = real
*        Y offset by which to translate the first NDF in frame coordinates.
*     lopercA = real
*        Percentile in the data of the first NDF to correspond to the
*        lower colour index for plotting (0 <= lopercA <= hipercA <= 100).
*     hipercA = real
*        Percentile in the data of the first NDF to correspond to the
*        upper colour index for plotting (0 <= lopercA <= hipercA <= 100).
*     ndfsetB = ndf-or-ndfset object
*        The second NDF or NDF Set to plot.
*     frameB = string
*        Indicates the WCS frame into which the second NDF should be
*        resampled; may be a numerical frame index, a domain name, or
*        one of the special strings "CURRENT" or "BASE".
*     xoffB = real
*        X offset by which to translate the second NDF in frame coordinates.
*     yoffB = real
*        Y offset by which to translate the second NDF in frame coordinates.
*     lopercB = real
*        Percentile in the data of the second NDF to correspond to the
*        lower colour index for plotting (0 <= lopercB <= hipercB <= 100).
*     hipercB = real
*        Percentile in the data of the second NDF to correspond to the
*        upper colour index for plotting (0 <= lopercB <= hipercB <= 100).

*  Usage:
*     ndfdrawpair device xorigin yorigin xdev ydev zoom
*                 ndfsetA frameA xoffA yoffA lopercA hipercA
*                 ndfsetB frameB xoffB yoffB lopercB hipercB

*  Return Value:
*     The return value is a count of the pixels in the overlap region
*     which have contributions from both the NDFs.  Thus if a zero is
*     returned, no pixels are shared between the two NDFs (though note
*     this does not necessarily mean there is no overlap; there could
*     be an overlap composed entirely of bad pixels).

*  Copyright:
*     Copyright (C) 2000-2001 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-SEP-2000 (MBT):
*        Initial version.
*     12-MAR-2001 (MBT):
*        Upgraded for use with Sets.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/


#include <stdio.h>
#include <stdlib.h>
#include "cpgplot.h"
#include "tcl.h"
#include "tclndf.h"
#include "ccdaux.h"

   int NdfDrawpair( ClientData clientData, Tcl_Interp *interp, int objc,
                    Tcl_Obj *CONST objv[] );

/* Declare object function to be executed in the background. */

   Tcl_ObjCmdProc do_ndfdrawpair;

/* Structure for passing arguments from driver routine to background
   routine. */

   struct ndfdrawpair_args_t {
      int **pixbloc;
      NdfOrNdfset **ndfset;
      double *xoff;
      double *yoff;
      int **iframes;
      char *device;
      double shrink;
      double xdev;
      double xorigin;
      double ydev;
      double yorigin;
      double zoom;
   };


   int NdfDrawpair( ClientData clientData, Tcl_Interp *interp, int objc,
                    Tcl_Obj *CONST objv[] ) {
/*
*  Note that all the calculations in this routine are done using fortran-like
*  arrays, since the C wrappers for the PGPLOT routines which do the actual
*  plotting are merely wrappers and don't use C-like arrays or anything.
*  Thus the first dimension of a pixel array changes most quickly, and
*  the first element of the array in each dimension is 1, not 0.
*/

      const char *arglist = "device xorigin yorigin xdev ydev zoom "
                  /*   1      2       3       4    5    6      */
                      "ndfsetA frameA xoffA yoffA lopercA hipercA "
                  /*   7       8      9     10    11      12      */
                      "ndfsetB frameB xoffB yoffB lopercB hipercB ";
                  /*   13      14     15    16    17      18      */
      int retval;
      int *iframes[ 2 ];
      double xoff[ 2 ];
      double yoff[ 2 ];
      double xorigin;
      double yorigin;
      double xdev;
      double ydev;
      double psize;
      double shrink;
      double zoom;
      float xplo;
      float xphi;
      float yplo;
      float yphi;
      int *pixbloc[ 2 ];
      double loperc[ 2 ];
      double hiperc[ 2 ];
      char *device;
      int i;
      int locolour;
      int hicolour;
      int const badcolour = 0;
      Tcl_Obj *result;
      Tcl_Obj argob;
      Tcl_Obj *ov[ 1 ];
      NdfOrNdfset *ndfset[ 2 ];
      struct ndfdrawpair_args_t args;

/* Check syntax. */
      if ( objc != 19 ) {
         Tcl_WrongNumArgs( interp, 1, objv, arglist );
         return TCL_ERROR;
      }

/* Extract and validate string arguments. */
      device = Tcl_GetString( objv[ 1 ] );

/* Extract and validate numeric arguments. */
      if ( Tcl_GetDoubleFromObj( interp, objv[ 2 ], &xorigin ) != TCL_OK ||
           Tcl_GetDoubleFromObj( interp, objv[ 3 ], &yorigin ) != TCL_OK ||
           Tcl_GetDoubleFromObj( interp, objv[ 4 ], &xdev ) != TCL_OK ||
           Tcl_GetDoubleFromObj( interp, objv[ 5 ], &ydev ) != TCL_OK ||
           Tcl_GetDoubleFromObj( interp, objv[ 6 ], &zoom ) != TCL_OK ||
           Tcl_GetDoubleFromObj( interp, objv[ 9 ], &xoff[ 0 ] ) != TCL_OK ||
           Tcl_GetDoubleFromObj( interp, objv[ 10 ], &yoff[ 0 ] ) != TCL_OK ||
           Tcl_GetDoubleFromObj( interp, objv[ 11 ], &loperc[ 0 ] ) != TCL_OK ||
           Tcl_GetDoubleFromObj( interp, objv[ 12 ], &hiperc[ 0 ] ) != TCL_OK ||
           Tcl_GetDoubleFromObj( interp, objv[ 15 ], &xoff[ 1 ] ) != TCL_OK ||
           Tcl_GetDoubleFromObj( interp, objv[ 16 ], &yoff[ 1 ] ) != TCL_OK ||
           Tcl_GetDoubleFromObj( interp, objv[ 17 ], &loperc[ 1 ] ) != TCL_OK ||
           Tcl_GetDoubleFromObj( interp, objv[ 18 ], &hiperc[ 1 ] ) != TCL_OK
         ) {
         result = Tcl_GetObjResult( interp );
         Tcl_AppendStringsToObj( result, "Usage: ", Tcl_GetString( objv[ 0 ] ),
                                 " ", arglist, (char *) NULL );
         Tcl_SetObjResult( interp, result );
         return TCL_ERROR;
      }

/* Extract and validate ndf object arguments. */
      if ( NdfGetNdfFromObj( interp, objv[ 7 ], &ndfset[ 0 ] ) != TCL_OK ||
           NdfGetNdfFromObj( interp, objv[ 13 ], &ndfset[ 1 ] ) != TCL_OK ) {
         return TCL_ERROR;
      }

/* Allocate memory for frame indices. */
      iframes[ 0 ] = malloc( max( 1, ndfset[ 0 ]->nmember ) * sizeof( int ) );
      iframes[ 1 ] = malloc( max( 1, ndfset[ 1 ]->nmember ) * sizeof( int ) );
      if ( tclmemok( interp, iframes[ 0 ] ) != TCL_OK ||
           tclmemok( interp, iframes[ 1 ] ) != TCL_OK ) {
         return TCL_ERROR;
      }

/* Extract and validate frame objects. */
      if ( NdfGetIframesFromObj( interp, objv[ 8 ], ndfset[ 0 ],
                                 iframes[ 0 ] ) != TCL_OK ||
           NdfGetIframesFromObj( interp, objv[ 14 ], ndfset[ 1 ],
                                 iframes[ 1 ] ) != TCL_OK ) {
         return TCL_ERROR;
      }

/* Open the PGPLOT plotting device. */
      if ( cpgopen( device ) <= 0 ) {
         result = Tcl_NewStringObj( "", 0 );
         Tcl_AppendObjToObj( result, objv[ 0 ] );
         Tcl_AppendStringsToObj( result,
                                 ": failed to open plotting device \"",
                                 device, "\"", (char *) NULL );
         Tcl_SetObjResult( interp, result );
         return TCL_ERROR;
      }

/* Query the plotting device for highest and lowest available colour indices. */
      cpgqcir( &locolour, &hicolour );

/* We will not require any surrounding space for annotation, so instruct
   PGPLOT to use the entire device surface for plotting. */
      cpgsvp( 0.0, 1.0, 0.0, 1.0 );

/* Get the size of the viewport in pixels. */
      cpgqvsz( 3, &xplo, &xphi, &yplo, &yphi );

/* Close PGPLOT down for now. */
      cpgclos();

/* Update pending Tcl events. */
      tclupdate();

/* This prevents us from resampling into a frame which has many units for
   a single pixel, which would be a waste. */
      STARCALL(
         psize = getpixelsize( ndfset[ 0 ], *iframes[ 0 ], status );
      )
      shrink = ( psize * zoom < 2.1 ) ? 1.0 : 1.0 / ( psize * zoom );

/* Generate the scaled data array ready for plotting.
   This wants to be done in the main process, since it may cache the
   calculated data block in the NDF structure. */
      for ( i = 0; i < 2; i++ ) {
         STARCALL(
            pixbloc[ i ] = getpixbloc( ndfset[ i ], iframes[ i ],
                                       zoom * shrink, loperc[ i ], hiperc[ i ],
                                       locolour, hicolour, badcolour, status );
         )
      }

/* The rest of the task can be performed as an independent background
   process, since it may take a significant amount of time and we want
   Tcl events to continue to be serviced. */

/* Set up the arguments block for the rest of the processing. */
      argob.internalRep.otherValuePtr = &args;
      ov[ 0 ] = &argob;
      args.pixbloc = pixbloc;
      args.ndfset = ndfset;
      args.xoff = xoff;
      args.yoff = yoff;
      args.iframes = iframes;
      args.device = device;
      args.shrink = shrink;
      args.xdev = xdev;
      args.xorigin = xorigin;
      args.ydev = ydev;
      args.yorigin = yorigin;
      args.zoom = zoom;

/* Call the routine which will do the rest of the processing. */
/* I was experimenting with doing this in a subprocess so that the Tcl
   event loop was attended to during processing.  this is why
   do_ndfdrawpair is implemented as a separate routine here.  It can
   be invoked in this way using the call:
      return tclbgcmd( (ClientData) do_ndfdrawpair, interp, 1, ov )
   However, this doesn't sem to solve enough problems to be worthwhile,
   so it is currently written to execute in the main process. */
      retval = ((Tcl_ObjCmdProc *) do_ndfdrawpair)( (ClientData) NULL,
                                                    interp, 1, ov );
      free( iframes[ 0 ] );
      free( iframes[ 1 ] );
      return retval;
   }


   int do_ndfdrawpair( ClientData clientData, Tcl_Interp *interp, int objc,
                       Tcl_Obj *CONST objv[] ) {
      int const badcolour = 0;
      int i;
      int ngood = 0;
      int xpix[ 2 ];
      int xpof[ 2 ];
      int ypix[ 2 ];
      int ypof[ 2 ];
      double lbox[ 2 ][ 2 ];
      double ubox[ 2 ][ 2 ];
      double plotzoom;

/* Set values from the structure passed in the argument list. */
      struct ndfdrawpair_args_t *pargs = objv[ 0 ]->internalRep.otherValuePtr;
      int **pixbloc = pargs->pixbloc;
      NdfOrNdfset **ndfset = pargs->ndfset;
      double *xoff = pargs->xoff;
      double *yoff = pargs->yoff;
      int **iframes = pargs->iframes;
      char *device = pargs->device;
      double shrink = pargs->shrink;
      double xdev = pargs->xdev;
      double xorigin = pargs->xorigin;
      double ydev = pargs->ydev;
      double yorigin = pargs->yorigin;
      double zoom = pargs->zoom;

      plotzoom = zoom * shrink;

/* Calculate the position and extent of the pixel array. */
      for ( i = 0; i < 2; i++ ) {
         STARCALL(
            getbbox( ndfset[ i ], iframes[ i ], lbox[ i ], ubox[ i ], status );
         )
         lbox[ i ][ 0 ] += xoff[ i ];
         ubox[ i ][ 0 ] += xoff[ i ];
         lbox[ i ][ 1 ] += yoff[ i ];
         ubox[ i ][ 1 ] += yoff[ i ];

/* Store dimensions and positions of the pixel blocks in screen pixel
   coordinates.  This gives us integer values for the geometry which can
   be used in pixel-like contexts.  Although the PGPLOT routines mostly
   require floating point numbers, it is easier to put things in the
   right place if we use numbers which are in fact integers. */
         xpix[ i ] = ndfset[ i ]->plotarray->xdim;
         ypix[ i ] = ndfset[ i ]->plotarray->ydim;
         xpof[ i ] = zoom * shrink * lbox[ i ][ 0 ];
         ypof[ i ] = zoom * shrink * lbox[ i ][ 1 ];
      }

/* Start up PGPLOT.  We use the same invocations here as in the driver
   routine. */
      if ( cpgopen( device ) <= 0 ) {
         Tcl_SetObjResult( interp,
                           Tcl_NewStringObj( "Failed to open plotting device",
                                             -1 ) );
         return TCL_ERROR;
      }
      cpgsvp( 0.0, 1.0, 0.0, 1.0 );
      cpgswin( xorigin, xorigin + xdev, yorigin, yorigin + ydev );

/* Begin PGPLOT buffering. */
      cpgbbuf();

/* Plot both arrays to the graphics device.  Note fortran-like specification
   of array indices to PGPLOT, and use of a fortran-like array in memory. */
      for ( i = 0; i < 2; i++ ) {
         cpgpixl( pixbloc[ i ], xpix[ i ], ypix[ i ],
                  1, xpix[ i ], 1, ypix[ i ],
                  lbox[ i ][ 0 ] + 0.5 / plotzoom,
                  lbox[ i ][ 0 ] + ( xpix[ i ] - 0.5 ) / plotzoom,
                  lbox[ i ][ 1 ] + 0.5 / plotzoom,
                  lbox[ i ][ 1 ] + ( ypix[ i ] - 0.5 ) / plotzoom );

/* The following call is more general (in that it doesn't rely on knowing
 * the size of the GWM we are plottting on) but unlikely to allow PGPLOT
 * to optimise the plotting. */
/*
 *     cpgpixl( pixbloc[ i ], xpix[ i ], ypix[ i ],
 *                1, xpix[ i ], 1, ypix[ i ],
 *                lbox[ i ][ 0 ], ubox[ i ][ 0 ],
 *                lbox[ i ][ 1 ], ubox[ i ][ 1 ] );
 */
      }

/* Check if there is any overlap between the two images. */
      if ( xpof[ 0 ] < xpof[ 1 ] + xpix[ 1 ] &&
           xpof[ 1 ] < xpof[ 0 ] + xpix[ 0 ] &&
           ypof[ 0 ] < ypof[ 1 ] + ypix[ 1 ] &&
           ypof[ 1 ] < ypof[ 0 ] + ypix[ 0 ] ) {

/* Calculate the overlap as a simple average of the two images. */
         int x;
         int y;
         int dat;
         int dat0;
         int dat1;
         int *overp;
         double oxlo;
         double oxhi;
         double oylo;
         double oyhi;
         int xlo = max( xpof[ 0 ], xpof[ 1 ] );
         int ylo = max( ypof[ 0 ], ypof[ 1 ] );
         int xhi = min( xpof[ 0 ] + xpix[ 0 ], xpof[ 1 ] + xpix[ 1 ] );
         int yhi = min( ypof[ 0 ] + ypix[ 0 ], ypof[ 1 ] + ypix[ 1 ] );

/* Calculate the positioning of the overlap in frame coordinates. */
         oxlo = xlo / zoom / shrink;
         oxhi = xhi / zoom / shrink;
         oylo = ylo / zoom / shrink;
         oyhi = yhi / zoom / shrink;

/* Allocate temporary memory for storing the overlap block. */
         overp = malloc( sizeof( int ) * ( xhi - xlo ) * ( yhi - ylo ) );

/* Loop through the overlap region in each block. */
         ngood = 0;
         for ( y = ylo; y < yhi; y++ ) {
            for ( x = xlo; x < xhi; x++ ) {
               dat0 = pixbloc[ 0 ][ x - xpof[ 0 ] +
                                    ( y - ypof[ 0 ] ) * xpix[ 0 ] ];
               dat1 = pixbloc[ 1 ][ x - xpof[ 1 ] +
                                    ( y - ypof[ 1 ] ) * xpix[ 1 ] ];

/* If both pixels are good, average them.  If one is bad, use the other one.
   If both are bad, write a bad result. */
               dat = ( dat0 == badcolour )
                        ? dat1
                        : ( dat1 == badcolour )
                             ? dat0
                             : ( dat0 + dat1 ) / 2;

/* Count the good pixels */
               if ( dat != badcolour ) ngood++;

/* Write to the output array. */
               overp[ x - xlo + ( y - ylo ) * ( xhi - xlo ) ] = dat;
            }
         }

/* Plot the overlap block over the pixels which have already been drawn. */
         cpgpixl( overp, xhi - xlo, yhi - ylo, 1, xhi - xlo, 1, yhi - ylo,
                  oxlo + 0.5 / plotzoom,
                  oxlo + ( xhi - xlo - 0.5 ) / plotzoom,
                  oylo + 0.5 / plotzoom,
                  oylo + ( yhi - ylo - 0.5 ) / plotzoom );
     /*  cpgpixl( overp, xhi - xlo, yhi - ylo, 1, xhi - xlo, 1, yhi - ylo,
      *           oxlo, oxhi, oylo, oyhi );
      */

/* Free the workspace. */
         free( overp );
      }

/* End PGPLOT buffering. */
      cpgebuf();

/* Close the plotting device. */
      cpgclos();

/* Set the result. */
      Tcl_SetObjResult( interp, Tcl_NewIntObj( ngood ) );

/* Return with success status. */
      return TCL_OK;
   }


/* $Id$ */
