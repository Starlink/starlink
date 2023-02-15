/*
*+
*  Name:
*     smf_get_findex

*  Purpose:
*     Calculate index along frequency axis in FFT to given frequency

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     index = smf_get_findex( double f, double df, dim_t nf, int *status )

*  Arguments:
*     f = double (Given)
*        Frequency in Hz
*     df = double (Given)
*        Frequency step size in FFT
*     nf = double (Given)
*        Number of frequencies in FFT (= number time slices/2 + 1)
*     status = int * (Given and Returned)
*        Pointer to inherited status.

*  Returned Value:
*     Index to the nearest frequency in the FFT to the frequency f.

*  Description:

*     Calculate the index of the nearest frequency in an FFT to a
*     given frequency in Hz (using rounding). If df <= 0, or f < 0
*     SMF__INFREQ status is set. If a frequency higher than Nyquist is
*     requested, the return value will be truncated to Nyquist (nf-1). Also,
*     very low frequencies could be rounded to 0 (DC).

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-09-15 (EC)
*        Initial version.
*     2011-06-09 (EC)
*        Modify behaviour to truncate high-frequencies to Nyquist, and round
*        low frequencies to 0 as well.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008,2011 University of British Columbia.
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

#include <math.h>
#include "sae_par.h"
#include "mers.h"
#include "smf.h"
#include "smf_err.h"

#define FUNC_NAME "smf_get_findex"

dim_t smf_get_findex( double f, double df, dim_t nf, int *status ) {
  dim_t retval=0;

   /* Check the inherited status */
   if( *status != SAI__OK ) return retval;

   if( f < 0 ) {
     *status = SMF__INFREQ;
     errRep( "", FUNC_NAME ": f is < 0, possible programming error", status );
     return retval;
   }

   if( df <= 0 ) {
     *status = SMF__INFREQ;
     errRep( "", FUNC_NAME ": df is <= 0, possible programming error", status );
     return retval;
   }

   if( nf == 0 ) {
     *status = SAI__ERROR;
     errRep( "", FUNC_NAME ": nf is 0, possible programming error", status );
     return retval;
   }

   /* If we get here, retval is guaranteed to be >= 0, and not infinity */
   retval = (dim_t) round( f/df );

   if( retval > nf ) {
     msgOutiff( MSG__DEBUG, "", FUNC_NAME
              ": Frequency %lf Hz is > Nyquist %lf Hz, "
              "truncating to Nyquist." , status, f, df*nf);
     retval = nf-1;
   }

   if( (f >= 0) && (retval == 0) ) {
     msgOutiff( MSG__DEBUG, "", FUNC_NAME
              ": warning, low frequency %lf Hz was rounded to 0 Hz "
              "(data stream is too short to sample this frequency)", status,
              f );
   }

   return retval;
}

