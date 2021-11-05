#include "sae_par.h"
#include "cupid.h"
#include "mers.h"

void cupidGCListClump( size_t iclump, int ndim, double *par, double chisq,
                       hdsdim *lbnd, double rms, double sum, int *status ){
/*
*+
*  Name:
*     cupidGCListClump

*  Purpose:
*     Add a clump to the output list.

*  Language:
*     Starlink C

*  Synopsis:
*     void cupidGCListClump( size_t iclump, int ndim, double *par, double chisq,
*                            hdsdim *lbnd, double rms, double sum, int *status )

*  Description:
*     This function adds a clump to the output list.

*  Parameters:
*     iclump
*        The (1-based) index of the clump to be added to the list.
*     ndim
*        The number of pixel axes in the NDF.
*     par
*        The parameters describing the Gaussian fit to the clump (see
*        cupidGCFit.cgen for a description)..
*     lbnd
*        Lower pixel bounds of supplied data array.
*     chisq
*        The chi-squared associated with the fit.
*     rms
*        The RMS noise level.
*     sum
*        The data sum in the clump.
*     status
*        Pointer to the inherited status value.

*  Copyright:
*     Copyright (C) 2007 Science and Technology Facilities Council.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

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
*     DSB: David S. Berry
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     5-OCT-2005 (DSB):
*        Original version.
*     14-JAN-2008 (TIMJ):
*        Use msgOutif instead of ilevel
*     5-NOV-2021 (DSB):
*        Added argument sum.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   msglev_t curlev;     /* Current message filter level */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* Report information to standard output if requested. */
   curlev = msgIflev( NULL, status );
   if( curlev == MSG__VERB || curlev == MSG__DEBUG ) {
     msgBlankif( MSG__DEBUG, status );
     msgSetk( "N", iclump );
     msgOutif( MSG__VERB, "", "Clump ^N:", status );
   } else {
      msgSetk( "N", iclump );
      msgOutif( MSG__DEBUG1, "", "   Storing clump ^N:", status );
   }

   msgSetd( "V", chisq );
   msgOutif( MSG__DEBUG, "", "   Chi-squared: ^V", status );
   msgSetd( "S", sum );
   msgOutif( MSG__DEBUG, "", "   Data sum: ^S", status );

   msgSetd( "V", par[ 0 ]*rms );
   msgOutif( MSG__DEBUG, "", "   Peak intensity: ^V", status );
   msgSetd( "V", par[ 1 ]*rms );
   msgOutif( MSG__DEBUG, "", "   Constant background: ^V", status );
   msgSetd( "V", par[ 2 ] + (double) lbnd[ 0 ] - 1.5 );
   msgOutif( MSG__DEBUG, "", "   Centre on 1st axis: ^V", status );
   msgSetd( "V", par[ 3 ] );
   msgOutif( MSG__DEBUG, "", "   FWHM on 1st axis: ^V", status );

   if( ndim > 1 ) {
     msgSetd( "V", par[ 4 ] + (double) lbnd[ 1 ] - 1.5 );
     msgOutif( MSG__DEBUG, "", "   Centre on 2nd axis: ^V", status );
     msgSetd( "V", par[ 5 ] );
     msgOutif( MSG__DEBUG, "", "   FWHM on 2nd axis: ^V", status );
     msgSetd( "V", par[ 6 ] );
     msgOutif( MSG__DEBUG, "", "   Position angle: ^V", status );

     if( ndim > 2 ) {
       msgSetd( "V", par[ 7 ] + (double) lbnd[ 2 ] - 1.5 );
       msgOutif( MSG__DEBUG, "", "   Centre on vel axis: ^V", status );
       msgSetd( "V", par[ 8 ] );
       msgOutif( MSG__DEBUG, "", "   FWHM on vel axis: ^V", status );
       msgSetd( "V", par[ 9 ] );
       msgOutif( MSG__DEBUG, "", "   Vel gradient on 1st axis: ^V", status );
       msgSetd( "V", par[ 10 ] );
       msgOutif( MSG__DEBUG, "", "   Vel gradient on 2nd axis: ^V", status );
     }
   }
}
