/*
*+
*  Name:
*     smf_addfakemap

*  Purpose:
*     Add signal for a fake source onto the bolometer data values.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_addfakemap( ThrWorkForce *wf, smfArray *res, smfArray *ext,
*                          smfArray *lut, int *lbnd_out, int *ubnd_out,
*                          AstKeyMap *keymap, double chunkfactor,
*                          int contchunk, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     res = smfArray * (Given)
*        Pointer to a smfArray holding the bolometer time-stream values for
*        all scuba-2 subarrays.
*     ext = smfArray * (Given)
*        Pointer to a smfArray holding the extinction model for all
*        scuba-2 subarrays. Should be NULL if no extinction model is
*        being used.
*     lut = smfArray * (Given)
*        Pointer to a smfArray holding the map pixel index for each
*        bolometer value in all scuba-2 subarrays.
*     lbnd_out = int * (Given)
*        A 2-element array - the lower pixel index bounds of the output map.
*     ubnd_out = int * (Given)
*        A 2-element array - the upper pixel index bounds of the output map.
*     config = AstKeyMap * (Given)
*        A KeyMap holding all configuration parameters.
*     chunkfactor = double (Given)
*        The calibration correction factor to use for the current chunk.
*        The values sampled from the fakemap are divided by this factor
*        before being added onto the time-series data. This is in addition
*        to any scaling specified by the "fakescale" config parameter.
*     contchunk = int (Given)
*        Zero based index of current time series data chunk.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function modifies the supplied bolometer time-stream data by
*     adding on data sampled from a user-supplied "fakemap".

*  Authors:
*     David S Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     7-OCT-2015 (DSB):
*        Original version.
*     10-APR-2018 (DSB):
*        Added parameter "chunkfactor".
*     10-MAY-2018 (DSB):
*        Allow each chunk of data to have a different fakescale.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2015-2018 East Asian Observatory.
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
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_typ.h"
#include "libsmf/smf_err.h"

void smf_addfakemap( ThrWorkForce *wf, smfArray *res, smfArray *ext,
                     smfArray *lut, int *lbnd_out, int *ubnd_out,
                     AstKeyMap *keymap, double chunkfactor, int contchunk,
                     int *status ){

/* Local Variables: */
   char *fakemap;
   const char *tempstr;
   double *extptr;
   double *fakestream = NULL;
   double *fmapdata;
   double *resptr;
   double fakedelay;
   double fakescale;
   int *lutptr;
   int fakemce;
   int fakendf;
   int nmap;
   int tndf;
   size_t idx;
   dim_t k;
   dim_t dsize;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* If a value has been set for the FAKEMAP configuration parameter, get
   it and use it as the path to the fakemap. If no value has been set
   for FAKEMAP, return immediately without changing the time-series. */
   tempstr = NULL;
   astMapGet0C( keymap, "FAKEMAP", &tempstr );
   if( !tempstr ) return;

/* Take a copy since the pointer returned by astMapGet0C points to a
   static buffer that may get re-used. */
   fakemap = astStore( NULL, tempstr, strlen( tempstr ) + 1 );
   if( *status == SAI__OK ) {
      msgOutf( "", "smf_addfakemap: loading external fakemap `%s'", status, fakemap );

/* Get the other required config parameters. FAKESCALE can have a
   different value for each chunk, but this is not currently true of FAKEMCE
   or FAKEDELAY. */
      fakescale = smf_chunkpar( res->sdata[0], "FAKESCALE", "fakemap scale factor",
                                keymap, contchunk, status );
      astMapGet0I( keymap, "FAKEMCE", &fakemce );
      astMapGet0D( keymap, "FAKEDELAY", &fakedelay );

/* Modify the fakescale value to incorporate the scaling requested by
   parameter "chunkfactor". */
      if( chunkfactor != 0.0 ) {
         fakescale /= chunkfactor;
      } else if( *status == SAI__OK ) {
         errRep( " ", "SMF_ADDFAKEMAP: Illegal zero value supplied for "
                 "parameter 'chunkfactor'.", status );
      }

/* Open the NDF, get a section from it matching the bounds of the output map,
   then close the original NDF - retaining the section. .  */
      ndfFind( NULL, fakemap, &tndf, status );
      ndfSect( tndf, 2, lbnd_out, ubnd_out, &fakendf, status );
      ndfAnnul( &tndf, status );

/* Map the data as double precision */
      ndfMap( fakendf, "DATA", "_DOUBLE", "READ", (void **) &fmapdata,
              &nmap, status );

/* Check the NDF was mapped successfully. */
      if( *status == SAI__OK  ) {

/* Add in the signal from the map to each subarray (s8a, s8b, etc) in turn. */
         for( idx = 0; idx < res->ndat; idx++ ) {

/* Get the number of data elements for the current subarray. */
            smf_get_dims( res->sdata[idx], NULL, NULL, NULL, NULL, &dsize,
                          NULL, NULL, status );

/* Get a pointer to the bolometer data values for the current subarray. */
            resptr = res->sdata[idx]->pntr[0];

/* Get a pointer to an array holding the index of the map pixel into
   which each bolometer value will be placed. */
            lutptr = lut->sdata[idx]->pntr[0];

/* If we will later be filtering the data to remove the MCE response or
   delay, we need to apply the opposite effects the fake data before adding
   it to the real data, so that the later filtering will affect only the
   real data and not the fake data. */
            if( fakemce || fakedelay != 0.0 ) {

/* Sample the fake map at the position of each sample, applying
   extinction correction or not as required. Note that fakestream is set to
   0 wherever there are no data, bad value encountered, etc. The quality
   normally flags wherever there are gaps (which then get filled), but in
   the case where fmapdata are missing values, QUALITY won't know about it,
   and we'll get junk when we do the filtering. A better way to do this
   would be to actually (temporarily) set some sort of quality (so that we
   can gap fill)... but probably not worth the effort. */
               fakestream = astGrow( fakestream, dsize, sizeof(*fakestream));
               if( ext ) {
                  extptr = ext->sdata[idx]->pntr[0];

                  for( k = 0; k < dsize; k++ ) {
                     if( (resptr[k] != VAL__BADD) && (lutptr[k] != VAL__BADI) &&
                         (extptr[k] > 0) && (fmapdata[lutptr[k]] != VAL__BADD) &&
                         (resptr[k] != VAL__BADD) ) {
                        fakestream[k] = fakescale*fmapdata[lutptr[k]] / extptr[k];
                     } else {
                        fakestream[k] = 0;
                     }
                  }
               } else {
                  for( k=0; k<dsize; k++ ) {
                     if( (resptr[k] != VAL__BADD) && (lutptr[k] != VAL__BADI) &&
                         (fmapdata[lutptr[k]] != VAL__BADD) &&
                         (resptr[k] != VAL__BADD) ) {
                        fakestream[k] = fakescale*fmapdata[lutptr[k]];
                     } else {
                        fakestream[k] = 0;
                     }
                  }
               }

/* Apply any delay specified by the "fakedelay" config parameter, and
   also smooth with the MCE response. These are done in the opposite order
   to that used in smf_clean_smfArray. We temporarily hijack the RES smfData
   for this purpose. So replace the pointer to the data array used by RES
   with the pointer to the fake time-stream data (we have already saved a
   copy of the original pointer in "resptr"). */
               res->sdata[idx]->pntr[0] = fakestream;

               smfFilter *filt = smf_create_smfFilter(res->sdata[idx], status);
               if( fakedelay != 0.0 ) {
                  msgOutiff( MSG__VERB, "", "smf_addfakemap: delay fake data "
                             "by %.4lf s", status, fakedelay );
                  smf_filter_delay( filt, fakedelay, status );
               }

               if( fakemce ) {
                  msgOutif( MSG__VERB, "", "smf_addfakemap: convolve fake "
                            "data with anti-aliasing filter", status );
                  smf_filter_mce( filt, 1, status );
               }

               smf_update_quality( wf, res->sdata[idx], 1, NULL, 0, 0.05, status );
               smf_filter_execute( wf, res->sdata[idx], filt, 0, 0, status );

               filt = smf_free_smfFilter( filt, status );

               res->sdata[idx]->pntr[0] = resptr;

/* Add the modified fake time stream data onto the residuals. */
               for( k = 0; k < dsize; k++ ) {
                  if( resptr[k] != VAL__BADD && fakestream[k] != VAL__BADD ){
                     resptr[k] += fakestream[k];
                  }
               }

/* If we will not be filtering the data later to remove the MCE response
   or delay, we do not need to first find the intermediate fake time stream
   data. So we can use less memory :-) */
            } else {

/* Version in which we are applying extinction correction */
               if( ext ) {
                  extptr = ext->sdata[idx]->pntr[0];

                  for( k = 0; k < dsize; k++ ) {
                     if( (resptr[k] != VAL__BADD) && (lutptr[k] != VAL__BADI) &&
                        (extptr[k] > 0) && (fmapdata[lutptr[k]] != VAL__BADD) &&
                        (resptr[k] != VAL__BADD) ) {
                        resptr[k] += fakescale*fmapdata[lutptr[k]] / extptr[k];
                     }
                  }

/* Version in which we are not applying extinction correction */
               } else {
                  for( k = 0; k < dsize; k++ ) {
                     if( (resptr[k] != VAL__BADD) && (lutptr[k] != VAL__BADI) &&
                        (fmapdata[lutptr[k]] != VAL__BADD) &&
                        (resptr[k] != VAL__BADD) ) {
                        resptr[k] += fakescale*fmapdata[lutptr[k]];
                     }
                  }
               }
            }

/* Next scuba-2 subarray... */
         }

/* Free resources. */
         fakestream = astFree( fakestream );
      }
      ndfAnnul( &fakendf, status );
   }
   fakemap = astFree( fakemap );
}


