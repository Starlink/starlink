/*
*+
*  Name:
*     smf_polang

*  Purpose:
*     Get the angle from the polarimetric reference direction to the
*     focal plane Y axis

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_polang( int indf, AstFitsChan *fc, double epoch, double *alpha,
*                 double *el, double *az, double *a, double *b, int *status );

*  Arguments:
*     indf = int (Given)
*        NDF identifier for input map. Must have a POLANAL Frame in its
*        WCS FrameSet. The current Frame must be a SkyFrame.
*     fc = AstFitsChan * (Given)
*        A pointer to a FitsChan holding the contents of the FITS extension
*        of the NDF. May be NULL.
*     epoch = double (Given)
*        The Julian epoch at the centre of the data. This defines the
*        relationship between (RA,Dec) and (Az,El). If VAL__BADD is
*        supplied, the value used is the Epoch attribute in the SKY Frame
*        of the WCS FrameSet of the NDF.
*     alpha = double * (Returned)
*        Returned holding the approximate angle from the polarimetric
*        reference direction to the focal plane Y axis, in radians. Positive
*        in the sense of rotation from celestial north (e.g. Dec) to celestial
*        east (e.g. RA).
*     el = double * (Returned)
*        Returned holding the central elevation at the supplied epoch
*        (radians).
*     az = double * (Returned)
*        Returned holding the central azimuth at the supplied epoch
*        (radians).
*     a = double * (Returned)
*        Returned holding the value on the first SKY axis at the pixel origin.
*     b = double * (Returned)
*        Returned holding the value on the second SKY axis at the pixel origin.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function returns the approximate angle from the polarimetric
*     reference direction to the focal plane Y axis (in radians) for the POL2
*     map in the supplied NDF. The actual angle varies through the course
*     of the observation due to sky rotation. The returned value is the
*     value appropriate for the mid point of the observation, as determined
*     by the FITS headers in the supplied NDF.

*  Authors:
*     DSB: David S Berry (EAO):
*     {enter_new_authors_here}

*  History:
*     3-JUN-2019 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2019 East Asian Observatory.
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

/* Starlink includes */
#include "mers.h"
#include "sae_par.h"
#include "ast.h"
#include "star/pal.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_polang"

void smf_polang( int indf, AstFitsChan *fc, double epoch, double *alpha,
                 double *el, double *az, double *a, double *b, int *status ){

/* Local Variables */
   AstFrame *frm;
   AstFrameSet *iwcs;
   AstMapping *map;
   double beta;
   double d;
   double p1[ 2 ];
   double p2[ 2 ];
   double pixpos[ NDF__MXDIM ][ 2 ];
   double polpos[ NDF__MXDIM ][ 2 ];
   double skypos[ NDF__MXDIM ][ 2 ];
   int i;
   int ipix;
   int ipol;
   int isky;
   int npix;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Begin an AST object context. */
   astBegin;

/* If not supplied, get a FitsChan holding the contents of the FITS
   extension in the supplied NDF. */
   if( !fc ) kpgGtfts( indf, &fc, status );

/* Get the WCS FrameSet from the NDF. */
   ndfGtwcs( indf, &iwcs, status );

/* Find the PIXEL Frame. May not be 2-dimensional. */
   kpg1Asffr( iwcs, "PIXEL", &ipix, status );

/* Attempt to find a SKY Frame. If found, it will always be 2-dimensional
   (any spectral axis will be stripped by kpg1Asffr). Note, this does not
   change the current Frame in the FrameSet. */
   kpg1Asffr( iwcs, "SKY", &isky, status );
   if( isky == AST__NOFRAME && *status == SAI__OK ) {
      *status = SAI__ERROR;
      ndfMsg( "N", indf );
      errRep( " ", "No SKY Frame found in the WCS information in '^N'.",
              status );
   }

/* Get the Mapping from PIXEL to SKY and use it to get the SKY position of
   the pixel origin. Return the axis values in "a" and "b". */
   map = astGetMapping( iwcs, ipix, isky );
   npix = astGetI( map, "Nin" );
   for( i = 0; i < npix; i++ ) {
      pixpos[ i ][ 0 ] = 1.0;
   }
   astTranN( map, 1, npix, 2, (const double *)pixpos, 1, 2, 2, (double *)skypos );
   *a = skypos[ 0 ][ 0 ];
   *b = skypos[ 1 ][ 0 ];

/* Ensure the sky frame is the current Frame, and then set it to represent
   AzEl coords at the centre of the observation. First override the original
   Epoch with the supplied Epoch, if good (note, use a Frame pointer
   rather than the FrameSet pointer when changing the Epoch to avoid
   changing the Mappings in the FrameSet). */
   astSetI( iwcs, "Current", isky );
   if( epoch != VAL__BADD ) {
      frm = astGetFrame( iwcs, isky );
      astSetD( frm, "Epoch", epoch );
      frm = astAnnul( frm );
   }
   astSet( iwcs, "System=AZEL" );

/* Attempt to find a POLANAL Frame in the FrameSet. If found, it will always
   be 2-dimensional (any spectral axis will be stripped by kpg1Asffr). */
   kpg1Asffr( iwcs, "POLANAL", &ipol, status );
   if( ipol == AST__NOFRAME && *status == SAI__OK ) {
      *status = SAI__ERROR;
      ndfMsg( "N", indf );
      errRep( " ", "No POLANAL Frame found in the WCS information in '^N'.",
              status );
   }

/* Get the Mapping from PIXEL to POLANAL. */
   map = astGetMapping( iwcs, ipix, ipol );

/* Transform two PIXEL positions into POLANAL - the pixel origin and a
   point one pixel away from the pixel origin. Use pixel=1 on any
   non-spatial axes (spatial axes are assumed to be axes 0 and 1). */
   pixpos[ 0 ][ 0 ] = 0.0;
   pixpos[ 1 ][ 0 ] = 0.0;
   pixpos[ 0 ][ 1 ] = 0.0;
   pixpos[ 1 ][ 1 ] = 1.0;
   for( i = 2; i < npix; i++ ) {
      pixpos[ i ][ 0 ] = 1.0;
      pixpos[ i ][ 1 ] = 1.0;
   }

   astTranN( map, 2, npix, 2, (const double *)pixpos, 1, 2, 2,
             (double *)polpos );

/* Find the distance between the two points in POLANAL. */
   frm = astGetFrame( iwcs, ipol );
   p1[ 0 ] = polpos[ 0 ][ 0 ];
   p1[ 1 ] = polpos[ 1 ][ 0 ];
   p2[ 0 ] = polpos[ 0 ][ 1 ];
   p2[ 1 ] = polpos[ 1 ][ 1 ];
   d = astDistance( frm, p1, p2 );

/* Within the POLANAL Frame, move this distance away from the point
   corresponding to the pixel origin, along the first POLANAL axis (i.e.
   along the polarimetric reference direction). */
   polpos[ 0 ][ 1 ] = p1[ 0 ] + d;
   polpos[ 1 ][ 1 ] = p1[ 1 ];

/* Transform the two POLANAL positions into AzEl. */
   map = astGetMapping( iwcs, ipol, isky );
   astTranN( map, 2, 2, 2, (const double *)polpos, 1, 2, 2, (double *)skypos );

/* Get the angle from the positive elevation axis to the line joining these
   two points, measured positive in the same sense as rotation from North to
   East. */
   beta = -palDbear( skypos[0][0], skypos[1][0], skypos[0][1], skypos[1][1] );

/* The angle from the positive elevation axis to the positive focal
   plane Y axis, measured positive in the sense of rotation from North
   to East, is equal to the central elevation. The difference between these
   two angles is the required angle - the angle from the polarimetric
   reference direction (the first axis in POLANAL) to the focal plane Y
   axis (alpha). Also return the central azimuth and elevation. */
   *az = skypos[ 0 ][ 0 ];
   *el = skypos[ 1 ][ 0 ];
   *alpha = *el - beta;

/* End the AST object context. */
   astEnd;

}



