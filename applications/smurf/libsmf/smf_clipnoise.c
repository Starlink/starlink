/*
*+
*  Name:
*     smf_clipnoise

*  Purpose:
*     Low-level routine for clipping noisy outliers

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_clipnoise( double *clipdata, dim_t ndata, int cliplog,
*                    double cliplow, double cliphigh, dim_t *nclipped,
*                    int *status )

*  Arguments:
*     clipdata = double * (Given)
*        Buffer containing values to be clipped (ignore values with VAL__BADD)
*     ndata = dim_t (Given)
*        Number of elements in data
*     cliplog = int (Given)
*        If set do clipping based on log of data instead of data
*     cliplow = double (Given)
*        Throw out values this many standard deviations below median. Set to
*        value <= 0 to disable low-outlier clipping.
*     cliphigh = double (Given)
*        Throw out values this many standard deviations above median. Set to
*        value <= 0 to disable high-outlier clipping.
*     nclipped = dim_t * (Given and Returned)
*        Number of values that were clipped (can be NULL)
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     An iterative clip of the data (or its log) is done to first
*     identify the median, and the standard deviation. The median is
*     used as the central measure after each iteration to minimize the
*     impact of outliers, and there are 3 steps from 5- to 1-sigma. At
*     the end of this procedure the median should lie very close to the
*     mode, but the standard deviation of the 1-sigma clipped data will be
*     under-predicted, so it is multiplied by 1.85 to give the standard
*     deviation for a non-clipped Gaussian. This final median and standard
*     deviation are then compared to cliphigh and cliplow to set values in
*     data to VAL__BADD.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     EC: Ed Chapin (UBC)
*     {enter_new_authors_here}

*  Notes:

*  History:
*     2011-06-23 (EC):
*        Initial version moved private routine out of smurf_calcnoise
*     2011-08-23 (TIMJ):
*        Disable clipping if we have too few bolometers.

*  Copyright:
*     Copyright (C) 2011 Science and Technology Facilities Council.
*     Copyright (C) 2011 University of British Columbia.
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

#include "smf.h"
#include "smf_err.h"

#include "sae_par.h"
#include "mers.h"

#define FUNC_NAME "smf_clipnoise"

void smf_clipnoise( double *clipdata, dim_t ndata, int cliplog,
                    double cliplow, double cliphigh, dim_t *nclipped,
                    int *status ) {

  const float clips[] = {5,3,1};
  const dim_t nclips = sizeof(clips)/sizeof(*clips);
  dim_t i;
  dim_t nlow=0;
  dim_t nhigh=0;
  double *work=NULL;
  double median, mean, sigma;

  if( *status != SAI__OK ) return;

  if( cliplog ) msgOutif( MSG__DEBUG, "", FUNC_NAME
                          ":   taking log10 of the data", status );

  if( (cliphigh > 0) || (cliplow > 0 ) ) {
    work = astCalloc( ndata, sizeof(*work) );

    /* Copy the data, or its log, into a buffer */
    if( *status == SAI__OK ) {
      for( i=0; i<ndata; i++ ) {
        if( (clipdata[i] != VAL__BADD) && (clipdata[i] > 0) ) {
          work[i] = cliplog ? log10(clipdata[i]) : clipdata[i];
        } else {
          work[i] = VAL__BADD;
        }
      }
    }

    /* Measure the clipped median, mean and standard deviation. We
       step down from 5- to 1-sigma, using the median rather than the
       mean as our central measure to ensure robustness against large
       outliers. This should end up near the mode of the distribution,
       although the RMS of the sample will under-estimate, by nearly a
       factor of 2, the standard deviation for a Gaussian distribution
       since we've clipped so much of the wings. We scale it back up
       so that it would give the right answer for a Gaussian. */

    smf_clipped_stats1D( work, nclips, clips, 1, ndata, NULL, 0, 0,
                         &mean, &sigma, &median, 1, NULL, status );

    /* Assume that we do not need to clip if we can not get good
       statistics */
    if (*status == SMF__INSMP) {
      errAnnul( status );
      msgOutif(MSG__NORM, "",
               "Noise clipping disabled as there are too few bolometers",
               status );
      goto CLEANUP;
    }

    sigma *= 1.85;

    msgOutiff( MSG__DEBUG, "", FUNC_NAME
               ":   mean=%lg median=%lg standard dev=%lg",
               status, mean, median, sigma );

    /* Then clip the high/low outliers relative to the median */
    if( *status==SAI__OK ) {
      for( i=0; i<ndata; i++ ) {
        if( work[i] != VAL__BADD ) {
          double d = work[i] - median;

          if( (cliphigh>0) &&
              (d >= sigma*cliphigh) ) {

            clipdata[i] = VAL__BADD;
            nhigh++;
          }

          if( (cliplow>0) &&
              (-d >= sigma*cliplow) ) {

            clipdata[i] = VAL__BADD;
            nlow++;
          }
        }
      }
    }

    if( nhigh ) msgOutiff( MSG__VERB, "", FUNC_NAME
                           ":   clipped %zu values >= %lg",
                           status, nhigh,
                           cliplog ? pow(10,median + sigma*cliphigh) :
                           median + sigma*cliphigh );
    if( nlow ) msgOutiff( MSG__VERB, "", FUNC_NAME
                          ":   clipped %zu values <= %lg",
                          status, nlow,
                          cliplog ? pow(10,median - sigma*cliplow) :
                          median - sigma*cliplow );

    /* Return number of clipped values */
    if( nclipped ) *nclipped = nhigh+nlow;

  CLEANUP:
    /* Free temporary buffer */
    if( work ) work = astFree( work );
  }
}
