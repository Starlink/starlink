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
*     smf_mask_noisy( ThrWorkForce *wf, smfData *data, AstKeyMap *kmap,
*                     smfData **noise, double sigcliphigh, double sigcliplow,
*                     int cliplog, int zeropad, int * status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads. Can be NULL.
*     data = smfData * (Given and Returned)
*        The data that will be flagged
*     kmap = AstKeyMap * (Given)
*        KeyMap holding parameters for NOI model.
*     noise = smfData ** (Returned)
*        Optionally return pointer to smfData containing noise map. Can be NULL.
*     sigcliphigh = double (Given)
*        Number of standard deviations above median to clip noisy bolometers.
*        Ignored unless value is greater than zero.
*     sigcliplow = double (Given)
*        Number of standard deviations below median to clip noisy bolometers.
*        Ignored unless value is greater than zero.
*     cliplog = int (Given)
*        If set, calculate statistics on log of noise instead of noise
*     zeropad = int (Given)
*        Pad with zeros instead of with artificial data prior to doing
*        the FFTs?
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Calculates the noise for each bolometer for a specific frequency
*     range using smf_bolonoise. Then for any bolometers that are out
*     of specification it sets their quality to bad using
*     SMF__Q_NOISE.  The log of the noise values can be used instead
*     of the values themselves if the distribution has large
*     tails. The clipping itself is done with smf_clipnoise.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     EC: Ed Chapin (UBC)
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*

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
*     2011-04-20 (TIMJ):
*        Use rt(s) instead of /rt(Hz) for noise units.
*     2011-06-23 (EC):
*        - Move clipping into smf_clean_smfArray call, enabling us to use
*          log instead of actual values if desired.
*        - sigcliphigh and sigcliplow instead of sigclip
*        - median instead of mean for central measure
*     2015-06-03 (DSB):
*        Added argument kmap, and allow clipping to be done on the basis
*        of any variances associated with the supplied smfData.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010-2011 Science and Technology Facilities Council.
*     Copyright (C) 2010-2011 University of British Columbia.
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

#include "sae_par.h"
#include "mers.h"

#define FUNC_NAME "smf_mask_noise"

void smf_mask_noisy( ThrWorkForce *wf, smfData *data, AstKeyMap *kmap,
                     smfData **noise, double sigcliphigh, double sigcliplow,
                     int cliplog, int zeropad, int * status ){

  size_t i;
  dim_t nbolo = 0;             /* Number of bolometers */
  double *noisedata = NULL;    /* Pointer to the noise data array */
  smfData * noisemap = NULL;   /* Somewhere to receive the result */
  double *work = NULL;         /* Temporary work array */
  int usevar;                  /* Get noise fom the supplied Variance values? */

  if (*status != SAI__OK) return;

  if( (sigcliphigh <= 0.0) && (sigcliplow <= 0.0) ) return;

  /* Work out how many bolometers we have */
  smf_get_dims( data, NULL, NULL, &nbolo, NULL, NULL,
                NULL, NULL, status );

  /* Create some space for the result */
  smf_create_bolfile( wf, NULL, 1, data, "Noise", "blahs s**0.5",
                      SMF__MAP_VAR, &noisemap, status );
  if (noisemap) noisedata = (noisemap->pntr)[0];

  /* Calculate the noise on each bolometer. If the supplied smfData has a
     variance component AND the noi.usevar parameter is non-zero, then
     use the mean variance associated with each bolometer. Otherwise, find
     the noise in the specified frequency range using an FFT. */
  astMapGet0I( kmap, "USEVAR", &usevar );
  if( usevar && data->pntr[1] ) {
     msgOutif( MSG__VERB, "", FUNC_NAME
            ": Using supplied variances to flag outlier noise values. ", status );
     smf_collapse( wf, data->pntr[1], data->qual, SMF__Q_FIT, data->dims[0],
                   data->dims[1], data->dims[2],
                   data->isTordered?2:0, (double **) noisemap->pntr, NULL,
                   NULL, status );
  } else {
     smf_bolonoise( wf, data, -1.0, 0, 0.5, SMF__F_WHITELO,
                    SMF__F_WHITEHI, 0, zeropad ? SMF__MAXAPLEN : SMF__BADSZT,
                    (noisemap->pntr)[0], NULL, NULL, status );
  }

  /* Now need to convert this to noise by square rooting */
  if (*status == SAI__OK) {
    for (i=0; i<nbolo; i++) {
      if (noisedata[i] != VAL__BADD) {
        noisedata[i] = sqrt( noisedata[i] );
      }
    }
  }

  /* Now create a mask and then mask the quality. We only mask bolometers
     that are too noisy. We also do not mask bolometers that were already
     masked. We want the statistics to reflect what we actually masked
     and not any previous masking. This will miss any bolometers masked
     out by smf_bolonoise because they had zero power. */

  msgOutif( MSG__VERB, "", FUNC_NAME
            ": Checking for bolometers with outlier noise values. ", status );

  msgOutiff( MSG__VERB, "", FUNC_NAME
             ": Units are (%s s**0.5): ", status, data->hdr->units );

  work = astCalloc( nbolo, sizeof(*work) );

  if( *status == SAI__OK ) memcpy( work, noisedata, nbolo*sizeof(*work) );

  smf_clipnoise( noisedata, nbolo, cliplog, sigcliplow, sigcliphigh, NULL,
                 status );

  /* The only bad values that should appear in noisedata are new bolometers
     that were clipped by smf_clipnoise, not bolometers that were bad before
     for other reasons. */
  if( *status == SAI__OK ) {
    for( i=0; i<nbolo; i++ ) {
      if( (noisedata[i]==VAL__BADD) && (work[i] == VAL__BADD) ) {
        noisedata[i] = 0;
      }
    }
  }

  if( work ) work = astFree( work );

  /* The mask has to be inside a smfArray */
  if (*status == SAI__OK) {
    smfArray *masks = smf_create_smfArray( status );
    if (masks) masks->owndata = 0;  /* someone else owns the smfData */
    smf_addto_smfArray( masks, noisemap, status );
    smf_apply_mask( wf, data, masks, SMF__BBM_QUAL, SMF__Q_NOISE,
                    status );
    smf_close_related( wf, &masks, status );
  }

  /* Give noisemap back to caller if requested, or close it */
  if( noise ) {
    *noise = noisemap;
  } else {
    smf_close_file( wf, &noisemap, status );
  }
}
