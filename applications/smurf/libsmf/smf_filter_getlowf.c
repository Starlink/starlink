/*+
*  Name:
*     smf_filter_getlowf

*  Purpose:
*     Get the period corresponding to the lowest frequency passed by a
*     filter.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     void smf_filter_getlowf( smfFilter *filt, smfHead *hdr, double *period,
*                              int *status )

*  Arguments:
*     filt = smfFilter * (Given)
*        Pointer to the structure describing the filter.
*     hdr = smfHead *(Given)
*        Supplied the sampling frequency needed to convert seconds into
*        samples. If NULL, "period" is returned in seconds instead of samples.
*     period = double * (Returned)
*        The period corresponding to the lowest frequency passed by the
*        filter. In samples if "hdr" is supplied, and in seconds otherwise.
*     status = int * (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine finds the lowest frequency passed by a filter. In order
*     to handle soft-edged filters, this is taken to be the lowest frequency
*     at which the filter has a response greater than 0.5. This frequency
*     is then converted to a time period and thus to a number of time samples.
*
*     Currently, only real 1-dimensional filters are supported.

*  Authors:
*     David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     17-OCY-2013 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2013 Science & Technology Facilities Council.

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
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "smf.h"

void smf_filter_getlowf( smfFilter *filt, smfHead *hdr, double *period, int *status ){

/* Local variables */
   dim_t i;

/* Initialise returned value. */
   *period = VAL__BADD;

/* Check the inherited status. */
   if (*status != SAI__OK) return;

/* Check supplied values. */
   if( filt->isComplex ) {
      *status = SAI__ERROR;
      errRep( "", "smf_filter_getlowf: Supplied filter is complex.", status );

   } else if( filt->ndims > 1 ) {
      *status = SAI__ERROR;
      errRep( "", "smf_filter_getlowf: Supplied filter is not 1-dimensional.", status );

   } else if( !filt->real ) {
      *status = SAI__ERROR;
      errRep( "", "smf_filter_getlowf: Supplied filter has not been initialised.", status );

/* If all is OK, find the first element with gain greater than 0.5 */
   } else {

      for( i = 0; i < filt->fdims[ 0 ]; i++ ) {
         if( filt->real[ i ] > 0.5 ) {

/* Store the perido corresponding to the mid frequency, and leave the loop. */
            *period = 1.0/( ( i + 0.5 )*filt->df[ 0 ] );
            break;
         }
      }

/* Report an error if the filter passes nothing. */
      if( *period == VAL__BADD ) {
         *status = SAI__ERROR;
         errRep( "", "smf_filter_getlowf: Supplied filter has no value greater than 0.5.", status );

/* If a header was supplied, convert the period from seconds to samples. */
      } else if( hdr ){
         *period /= hdr->steptime;
      }
   }

}



