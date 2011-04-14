/*
*+
*  Name:
*     smf_mask_noisy

*  Purpose:
*     Calculate the noise of each bolometer and remove the noisiest

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_mask_noisy( smfWorkForce *wf, smfData *data, smfData **noise,
*                     double sigclip, int zeropad, int * status );

*  Arguments:
*     wf = smfWorkForce * (Given)
*        Pointer to a pool of worker threads. Can be NULL.
*     data = smfData * (Given and Returned)
*        The data that will be flagged
*     noise = smfData ** (Returned)
*        Optionally return pointer to smfData containing noise map. Can be NULL.
*     sigclip = double (Given)
*        Number of standard deviations above the mean to clip noisy bolometers.
*        Returns immediately unless this value is greater than zero.
*     zeropad = int (Given)
*        Pad with zeros instead of with artificial data prior to doing
*        the FFTs?
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Calculates the noise for each bolometer for a specific frequency
*     range using smf_bolonoise. Then for any bolometers that are out
*     of specification it sets their quality to bad using SMF__Q_NOISE

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     EC: Ed Chapin (UBC)
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     Uses 5 sigma clipping 5 times to work out the mean and standard
*     deviation of the noise data. It does this because noise data have
*     a long tail.

*  History:
*     2010-07-02 (TIMJ):
*        Initial version
*     2010-07-07 (TIMJ):
*        Use new quality scheme.
*     2010-09-16 (EC):
*        Optionally return noisemap
*     2010-09-28 (DSB):
*        Added zeropad argument.
*     2010-10-08 (DSB):
*        Fill gaps before calling smf_bolonoise.
*     2011-04-14 (DSB):
*        Remove gap filling since it is done in smf_fft_data (called by
*        bolonoise).
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010-2011 Science and Technology Facilities Council.
*     Copyright (C) 2010 University of British Columbia.
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

#include "smf.h"

#include "sae_par.h"
#include "mers.h"

void smf_mask_noisy( smfWorkForce *wf, smfData *data, smfData **noise,
                     double sigclip, int zeropad, int * status ) {

  const float clips[] = { 5, 5, 5, 5, 5 };  /* Clip levels for noise data */
  size_t i;
  double mean = VAL__BADD;     /* Mean noise */
  dim_t nbolo = 0;             /* Number of bolometers */
  const size_t nclips = sizeof(clips)/sizeof(*clips); /* number of clip levels */
  size_t ngood = 0;            /* Number of good bolometers */
  double *noisedata = NULL;    /* Pointer to the noise data array */
  smfData * noisemap = NULL;   /* Somewhere to receive the result */
  double sigma = VAL__BADD;    /* Standard deviation of noise */

  if (*status != SAI__OK) return;

  if (sigclip <= 0.0) return;

  /* Work out how many bolometers we have */
  smf_get_dims( data, NULL, NULL, &nbolo, NULL, NULL,
                NULL, NULL, status );

  /* Create some space for the result */
  smf_create_bolfile( NULL, 1, data, "Noise", "blahs Hz**-0.5",
                      0, &noisemap, status );
  if (noisemap) noisedata = (noisemap->pntr)[0];

  /* Calculate the noise on each bolometer */
  smf_bolonoise( wf, data, 0, 0.5, SMF__F_WHITELO,
                 SMF__F_WHITEHI, 0, zeropad ? SMF__MAXAPLEN : SMF__BADSZT,
                 (noisemap->pntr)[0], NULL, NULL, status );

  /* Now need to convert this to noise by square rooting */
  if (*status == SAI__OK) {
    for (i=0; i<nbolo; i++) {
      if (noisedata[i] != VAL__BADD) {
        noisedata[i] = sqrt( noisedata[i] );
      }
    }
  }

  /* we do not have a routine to work out the median that is fast
     and thread-safe. smf_find_median uses KAPLIBS. GSL requires that
     we sort the data. For now just use a clipped MEAN */

  smf_clipped_stats1D( (noisemap->pntr)[0], nclips, clips, 1, nbolo,
                       NULL, 0, 0, &mean, &sigma, &ngood, status );

  /* Now create a mask and then mask the quality. We only mask bolometers
     that are too noisy. We also do not mask bolometers that were already
     masked. We want the statistics to reflect what we actually masked
     and not any previous masking. This will miss any bolometers masked
     out by smf_bolonoise because they had zero power. */
  if (noisedata) {
    size_t nmasked = 0;
    double thrhi = mean + (sigclip * sigma);
    for (i = 0; i<nbolo; i++) {
      if (noisedata[i] == VAL__BADD) {
        noisedata[i] = 0.0;
      } else if ( noisedata[i] > thrhi ) {
        nmasked ++;
        noisedata[i] = VAL__BADD;
      }
    }
    msgOutiff( MSG__VERB, "", "Removed %zu bolometers with noise exceeding %g %s Hz**-0.5",
               status, nmasked, thrhi, data->hdr->units );
  }

  /* The mask has to be inside a smfArray */
  if (*status == SAI__OK) {
    smfArray *masks = smf_create_smfArray( status );
    if (masks) masks->owndata = 0;  /* someone else owns the smfData */
    smf_addto_smfArray( masks, noisemap, status );
    smf_apply_mask( data, masks, SMF__BBM_QUAL, SMF__Q_NOISE,
                    status );
    smf_close_related( &masks, status );
  }

  /* Give noisemap back to caller if requested, or close it */
  if( noise ) {
    *noise = noisemap;
  } else {
    smf_close_file( &noisemap, status );
  }

}
