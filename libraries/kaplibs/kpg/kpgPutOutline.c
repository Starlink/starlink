/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "prm_par.h"
#include "star/hds.h"
#include "ndf.h"
#include "ast.h"
#include "kaplibs.h"

/* Module variables */
static char **text;
static int nc;
static int maxlinelen;

/* Prototypes for private functions. */
static void mysink( const char *line );

/* The name of the NDF extension in which the STC-S polygon is stored. */
#define XNAME "OUTLINE"

/* The required accuracy of the outline, in pixels. */
#define ACC 3.0

void kpgPutOutline( int indf, float wlim, int convex, int *status ){
/*
*+
*  Name:
*     kpgPutOutline

*  Purpose:
*     Create and store an STC polygon describing the spatial extent of an NDF.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     void kpgPutOutline( int indf, float wlim, int convex, int *status )

*  Arguments:
*     indf = int (Given)
*        Identifier for the NDF. Must be 2-D or 3-D. The WCS must contain
*        a SkyFrame, and the first two pixel axes must map onto the two
*        sky axes.
*     wlim = float (Given)
*        Minimum fraction of good values per pixel required when
*        collapsing a 3D NDF. Ignored if the NDF has only two significant
*        pixel axes.
*     convex = int (Given)
*        Indicates the nature of the polygon. If zero, the polygon will
*        be a simple outline that hugs the edges of the good pixels. If
*        the NDF contains multiple dis-contiguous regions of good pixels,
*        the outline will be of a randomly selected contiguous clump of
*        good pixels. If "convex" is non-zero, the polygon will be the
*        shortest polygon that encloses all good pixels in the NDF. Such
*        a polygon will not in general hug the edges of the good pixels.
*     status = int * (Given and Returned)
*        Pointer to global status.

*  Description:
*     If the NDF is 3D it is collapsed onto the spatial pixel axes (which
*     must be pixel axes 1 and 2), and all pixels with less than the specified
*     fraction of good values in each pixel are set bad. An AST Polygon is
*     then created describing the good values in the collapsed array. This
*     polygon is mapped into the SKY Frame and converted to an STC-S
*     description, which is stored in an extension called "OUTLINE" in
*     the supplied NDF (the extension is a character array).

*  Authors:
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     17-FEB-2014 (DSB):
*        Initial version.

*  Copyright:
*     Copyright (C) 2014 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/


/* Local Variables: */
   AstFrame *frm2d;
   AstFrameSet *wcs;
   AstMapping *map2d;
   AstPolygon *poly = NULL;
   AstRegion *region;
   AstStcsChan *chan;
   HDSLoc *cloc = NULL;
   HDSLoc *xloc = NULL;
   char xtype[10];
   const double *data;
   const double *p0;
   double radius;
   int *data_2d = NULL;
   int *p1;
   int axes[ 2 ];
   int dims[ NDF__MXDIM ];
   int iaxis;
   int iel;
   int inside[ 2 ];
   hdsdim i;
   int iz;
   int latax;
   int lbnd[ 2 ];
   int lim;
   int lonax;
   int ndim;
   int nel;
   int nwcs;
   int out[ NDF__MXDIM ];
   int there;
   int ubnd[ 2 ];

/* Check the inherited status */
   if (*status != SAI__OK) return;

/* Start an AST context. */
   astBegin;

/* Get the pixel dimensions of the NDF. */
   ndfDim( indf, NDF__MXDIM, dims, &ndim, status );

/* Report an error if it is not 2- or 3- dimensional. */
   if( ndim != 2 && ndim != 3 && *status == SAI__OK ) {
      *status = SAI__ERROR;
      ndfMsg( "N", indf );
      msgSeti( "I", ndim );
      errRep( "", "kpgPutOutline: Cannot create a polygon outlining the good "
              "data in '^N' because the NDF has ^I axes (must be 2 or 3).",
              status );
   }

/* Get the WCS FrameSet. */
   kpg1Gtwcs( indf, &wcs, status );

/* Get the 2D Frame spanning the first two WCS axes, and check it is a
   SkyFrame. */
   axes[ 0 ] = 1;
   axes[ 1 ] = 2;
   frm2d = astPickAxes( wcs, 2, axes, NULL );
   if( !astIsASkyFrame( frm2d ) && *status == SAI__OK ) {
      *status = SAI__ERROR;
      ndfMsg( "N", indf );
      errRep( "", "kpgPutOutline: Cannot create a polygon outlining the good "
              "data in '^N' because the first two WCS axes are not sky axes.",
              status );
   }

/* Get the Mapping from the first two grid axes to the corresponding WCS
   axes, and check they are the sky axes. */
   astMapSplit( wcs, 2, axes, out, &map2d );
   if( !map2d || astGetI( map2d, "Nout" ) != 2 ||
       ( out[ 0 ] + out[ 1 ] != 3 ) ) {
      *status = SAI__ERROR;
      ndfMsg( "N", indf );
      errRep( "", "kpgPutOutline: Cannot create a polygon outlining the good "
              "data in '^N' because the sky axes cannot be split from the "
              "other axes.", status );
   }

/* Map the NDF's Data array. */
   ndfMap( indf, "Data", "_DOUBLE", "Read", (void *) &data, &nel, status );

/* If the data array is truly 3D, collapse it befdore finding the polygon.
   The collapsed array holds the number of valid spectral values at each
   spatial position. */
   if( ndim == 3 && dims[ 2 ] > 1 ) {
      nel = dims[ 0 ]*dims[ 1 ];
      data_2d = astCalloc( nel, sizeof( *data_2d ) );
      if( *status == SAI__OK ) {
         p0 = data;
         for( iz = 0; iz < dims[ 2 ]; iz++ ) {
            p1 = data_2d;
            for( iel = 0; iel < nel; iel++,p1++ ) {
               if( *(p0++) != VAL__BADD ) (*p1)++;
            }
         }

/* Create the outline polygon enclosing all pixels for which the number
   of collapsed spectral values is at least 50% of the maximum. The
   polygon is reduced to the lowest number of vertices that retain an
   accuracy of 3 pixels. The number of vertices used is never more than
   500, even if this produces an error larger than 3 pixels. The polygon
   is defined in GRID coords. */
         lbnd[ 0 ] = 1;
         lbnd[ 1 ] = 1;
         ubnd[ 0 ] = dims[ 0 ];
         ubnd[ 1 ] = dims[ 1 ];
         lim = (int) ( 0.5 + wlim*dims[ 2 ] );
         if( lim == 0 ) lim = 1;
         if( convex ) {
            poly = astConvexI( lim, AST__GE, data_2d, lbnd, ubnd, 0 );
         } else {
            inside[ 0 ] = -1;
            inside[ 1 ] = -1;
            poly = astOutlineI( lim, AST__GE, data_2d, lbnd, ubnd, ACC,
                                500, inside, 0 );
         }

/* Clear up. */
         data_2d= astFree( data_2d );
      }

/* Otherwise, handle 2D arrays. No need to collapse these. */
   } else {
      lbnd[ 0 ] = 1;
      lbnd[ 1 ] = 1;
      ubnd[ 0 ] = dims[ 0 ];
      ubnd[ 1 ] = dims[ 1 ];
      if( convex ) {
         poly = astConvexD( VAL__BADD, AST__NE, data, lbnd, ubnd, 0 );
      } else {
         inside[ 0 ] = -1;
         inside[ 1 ] = -1;
         poly = astOutlineD( VAL__BADD, AST__NE, data, lbnd, ubnd, ACC,
                             500, inside, 0 );
      }
   }

/* Report an error if no polygon was created. */
   if( ! poly ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         ndfMsg( "N", indf );
         errRep( "", "kpgPutOutline: Cannot create a polygon outlining the good "
                 "data in '^N' because no good values were found.", status );
      }

/* Map the above polygon from GRID coords into the current WCS Frame. */
   } else {
      region = astMapRegion( poly, map2d, frm2d );

/* Create an StcsChan that can be used to convert the AST Region into an
   STC-S polygon description. It is split into strings of up to 80
   characters. */
      text = NULL;
      nc = 0;
      chan = astStcsChan( NULL, mysink, "indent=1,StcsLength=80" );

/* Do the conversion. */
      if( astWrite( chan, region ) == 0 && *status == SAI__OK ) {
         *status = SAI__ERROR;
         ndfMsg( "N", indf );
         errRep( "", "kpgPutOutline: Cannot create a polygon outlining the good "
                 "data in '^N' because the polygon could not be converted "
                 "to STC-S format.", status );
      }

/* Create an NDF extension to hold the STC-S polygon, deleting any
   pre-existing extension first. */
      ndfXstat( indf, XNAME, &there, status );
      if( there ) ndfXdel( indf, XNAME, status );
      sprintf( xtype, "_CHAR*%d", maxlinelen );
      ndfXnew( indf, XNAME, xtype, 1, &nc, &xloc, status );

/* Copy the strings into the extension, freeing the memory at the same time. */
      for( i = 1; i <= nc; i++ ) {
         datCell( xloc, 1, &i, &cloc, status );
         datPut0C( cloc, text[ i - 1 ], status );
         text[ i - 1 ] = astFree( text[ i - 1 ] );
         datAnnul( &cloc, status );
      }

/* Free resources. */
      datAnnul( &xloc, status );
      text = astFree( text );
   }
   ndfUnmap( indf, "*", status );

/* End the AST context. */
   astBegin;
}


/* Function to append a line of text to an expanding dynamically
   allocated array of strings. Called by the StcsChan class. */
static void mysink( const char *line ){
   int linelen = astChrLen( line );
   if( linelen > 0 ) {
      text = astGrow( text, ++nc, sizeof( *text ) );
      if( text ) {
         text[ nc - 1 ] = astStore( NULL, line, linelen + 1 );
         text[ nc - 1 ][ linelen ] = 0;
         if( linelen > maxlinelen ) maxlinelen = linelen;
      }
   }
}
