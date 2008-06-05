/*
*+
*  Name:
*     smf_store_outputbounds

*  Purpose:
*     Write output bounds to screen and parameters. 

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_store_outputbounds (const int lbnd_out[3], const int ubnd_out[3],
*                        const AstFrameSet * wcsout, const AstSkyFrame *oskyfrm,
*                        const AstMapping * oskymap, int *status) {


*  Arguments:
*     lbnd_out = const int[3] (Given)
*        Lower pixel bounds of the output map. Can have up to 3 values
*        depending on the dimensionality of wcsout.
*     lbnd_out = const int[3] (Given)
*        Upper pixel bounds of the output map. Can have up to 3 values
*        depnding on the dimensionality of wcsout.
*     wcsout = const AstFrameSet * (Given)
*        Output frameset. Can be 2d or 3d frameset. lbnd_out and ubnd_out
*        must be the correct size.
*     oskyfrm = const AstSkyFrame * (Given)
*        Output sky frame (presumably split from wcsout). For a 2d wcsout
*        this can be NULL since the frameset can be used as a frame.
*     oskymap = const AstMapping * (Given)
*        Output sky mapping (presumably split from wcsout). For a 2d wcsout
*        this can be NULL since the frame set can act as a mapping.
*     status = int * (Given & Returned)
*       Inherited status. 

*  Description:
*     Reports the WCS bounds of the output map and stores the coordinates
*     of the bounding box in parameters FLBND, FUBND, FTR, FBR, FTL and FBL.

*  Authors:
*     DSB: David S Berry (JAC, UCLan)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     12-JAN-2007 (DSB/TIMJ):
*        Initial version in smurf_makecube.
*     04-JUN-2008 (TIMJ):
*        Factor out into separate function.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2007, 2008 Science & Technology Facilities Council.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"
#include "prm_par.h"
#include "mers.h"
#include "par.h"

/* SMURF includes */
#include "libsmf/smf.h"

void
smf_store_outputbounds (const int lbnd_out[3], const int ubnd_out[3],
                        const AstFrameSet * wcsout, 
                        const AstSkyFrame *oskyfrm,
                        const AstMapping * oskymap, int *status) {

  double corner[2];          /* WCS of a corner (SKY) */
  int i;                     /* loop counter */
  double glbnd_out[ 3 ];     /* double prec Lower GRID bounds for output map */
  double gubnd_out[ 3 ];     /* double prec Upper GRID bounds for output map */
  double gx_in[ 4 ];         /* X Grid coordinates of four corners */
  double gx_out[ 4 ];        /* X WCS coordinates of four corners */
  double gy_in[ 4 ];         /* Y Grid coordinates of four corners */
  double gy_out[ 4 ];        /* Y WCS coordinates of four corners */
  int ndims;                 /* Number of active dimensions */
  char tmpstr[10];           /* temporary unit string */
  double wcslbnd_out[3];     /* Array of lower bounds of output cube */
  double wcsubnd_out[3];     /* Array of upper bounds of output cube */


  if (*status != SAI__OK) return;

/* work out how many dimensions we have */
  ndims = astGetI( wcsout, "Naxes");

/* Calculate and output the WCS bounds (matching NDFTRACE output). The bounds 
   are normalised. Celestial coordinates will use radians. */
   for( i = 0; i < ndims; i++ ) {
     glbnd_out[ i ] = 0.5;
     gubnd_out[ i ] = ubnd_out[ i ] - lbnd_out[i] + 1.5;
   }

   for( i = 0; i < ndims; i++ ) {
     astMapBox( wcsout, glbnd_out, gubnd_out, 1, i+1, &(wcslbnd_out[ i ]), 
                &(wcsubnd_out[ i ]), NULL, NULL );
   }

   astNorm( wcsout, wcslbnd_out );
   astNorm( wcsout, wcsubnd_out );

   parPut1d( "FLBND", ndims,  wcslbnd_out, status );
   parPut1d( "FUBND", ndims,  wcsubnd_out, status );

   msgOutif( MSG__NORM, "WCS_WBND1",
	     "   Output cube WCS bounds:", status );

   for( i = 0; i < ndims && *status == SAI__OK; i++ ) {
     msgSetc( "L", astFormat( wcsout, i+1, wcslbnd_out[i]));
     msgSetc( "U", astFormat( wcsout, i+1, wcsubnd_out[i]));

     if( i == 2 ) {
       sprintf( tmpstr, "unit(%d)", i+1 );
       msgSetc( "UNT", astGetC( wcsout, tmpstr ));
     } else {
       msgSetc( "UNT", "" );
     }

     sprintf( tmpstr, "label(%d)", i + 1 );
     msgSetc( "LAB", astGetC( wcsout, tmpstr ) );

     msgOutif( MSG__NORM, "WCS_WBND2",
	       "        ^LAB: ^L -> ^U ^UNT", status );
   }

   /* if we have a 2d frameset we can use that directly rather
      than having to split the mapping or provide explicit 2d
      versions. Since we know that MAKECUBE already calculates
      2d mappings/frames and we also know that MAKEMAP doesn't
      we provide some logic here to switch on Naxes */
   if (ndims == 2) {
     if (oskyfrm == NULL) oskyfrm = (AstSkyFrame*)wcsout;
     if (oskymap == NULL) oskymap = (AstMapping*)wcsout;
   }

/* Now also calculate the spatial coordinates of the four corners (required
   for CADC science archive. First, calculate input GRID coordinates for 4 
   corners: TR, TL, BR, BL. Use pixel centres for reporting. This is 
   important for cases where the pixels are very large and we want to make 
   sure that we are conservative with the database reporting. */

   gx_in[ 0 ] = ubnd_out[ 0 ] - lbnd_out[ 0 ] + 1.0; /* Right */
   gx_in[ 1 ] = 1.0;                                 /* Left */
   gx_in[ 2 ] = gx_in[ 0 ];                          /* Right */
   gx_in[ 3 ] = gx_in[ 1 ];                          /* Left */
   gy_in[ 0 ] = ubnd_out[ 1 ] - lbnd_out[ 1 ] + 1.0; /* Top */
   gy_in[ 1 ] = gy_in[ 0 ];                          /* Top */
   gy_in[ 2 ] = 1.0;                                 /* Bottom */
   gy_in[ 3 ] = gy_in[ 2 ];                          /* Bottom */

   astTran2( oskymap, 4, gx_in, gy_in, 1, gx_out, gy_out );
   
/* Horrible code duplication */
   corner[ 0 ] = gx_out[ 0 ];
   corner[ 1 ] = gy_out[ 0 ];
   astNorm( oskyfrm, corner );
   parPut1d( "FTR", 2, corner, status );

   corner[ 0 ] = gx_out[ 1 ];
   corner[ 1 ] = gy_out[ 1 ];
   astNorm( oskyfrm, corner );
   parPut1d( "FTL", 2, corner, status );

   corner[ 0 ] = gx_out[ 2 ];
   corner[ 1 ] = gy_out[ 2 ];
   astNorm( oskyfrm, corner );
   parPut1d( "FBR", 2, corner, status );

   corner[ 0 ] = gx_out[ 3 ];
   corner[ 1 ] = gy_out[ 3 ];
   astNorm( oskyfrm, corner );
   parPut1d( "FBL", 2, corner, status );

}
