/*
*+
*  Name:
*     smf_clean_smfData

*  Purpose:
*     Perform basic data cleaning operations on a 3-d smfData

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_clean_smfData( smfWorkForce *wf, smfData *data,
*                        AstKeyMap *keymap, int *status )

*  Arguments:
*     wf = smfWorkForce * (Given)
*        Pointer to a pool of worker threads. Can be NULL.
*     data = smfData * (Given and Returned)
*        The data that will be flagged
*     keymap = AstKeyMap* (Given)
*        keymap containing parameters to control cleaning
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine is a wrapper for a number of basic data cleaning
*     operations which are controlled by parameters stored in the
*     AstKeyMap. The keywords that are understood, the general cleaning
*     procedures to which they apply, and the order in which they are
*     executed are listed here (for a full description of their
*     meaning and defaults values see documentation in
*     dimmconfig.lis):
*
*     Sync Quality : BADFRAC
*     DC steps     : DCFITBOX, DCMAXSTEPS, DCTHRESH, DCSMOOTH
*     Flag spikes  : SPIKETHRESH, SPIKEBOX
*     Slew speed   : FLAGSTAT
*     Dark squids  : DKCLEAN
*     Gap filling  : FILLGAPS
*     Baselines    : ORDER
*     Filtering    : FILT_EDGELOW, FILT_EDGEHIGH, FILT_NOTCHLOW, FILT_NOTCHHIGH,
*                    APOD, FILT_WLIM
*     Noisy Bolos  : NOISECLIP

*  Notes:
*     The resulting dataOrder of the cube is not guaranteed, so smf_dataOrder
*     should be called upon return if it is important. smf_get_cleanpar does
*     all of the parsing of the key/value pairs in the AstKeyMap.

*  Authors:
*     Edward Chapin (UBC)
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-05-31 (EC):
*        Initial Version factored out of smurf_sc2clean and smf_iteratemap
*     2010-06-25 (DSB):
*        Move apodisation to smf_filter_execute.
*     2010-09-10 (DSB):
*        Change smf_fix_steps argument list.
*     2010-09-15 (DSB):
*        Call smf_flag_spikes2 instead of smf_flag_spikes.
*     2010-09-15 (EC):
*        Use smf_fit_poly directly instead of smf_scanfit
*     2010-09-16 (EC):
*        Further simplification since smf_fit_poly can remove the poly as well

*  Copyright:
*     Copyright (C) 2010 Univeristy of British Columbia.
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
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

#define FUNC_NAME "smf_clean_smfData"

void smf_clean_smfData( smfWorkForce *wf, smfData *data,
                        AstKeyMap *keymap, int *status ) {

  /* Local Variables */
  double badfrac;           /* Fraction of bad samples to flag bad bolo */
  dim_t dcfitbox;           /* width of box for measuring DC steps */
  int dcmaxsteps;           /* number of DC steps/min. to flag bolo bad */
  dim_t dcsmooth;           /* median filter width before finding DC steps */
  double dcthresh;          /* n-sigma threshold for primary DC steps */
  int dofft;                /* are we doing a freq.-domain filter? */
  int dkclean;              /* Flag for dark squid cleaning */
  int fillgaps;             /* Flag to do gap filling */
  smfFilter *filt=NULL;     /* Frequency domain filter */
  double flagstat;          /* Threshold for flagging stationary regions */
  size_t nflag;             /* Number of elements flagged */
  double noiseclip = 0;     /* Sigma clipping based on noise */
  int order;                /* Order of polynomial for baseline fitting */
  double spikethresh;       /* Threshold for finding spikes */
  size_t spikebox=0;        /* Box size for spike finder */
  struct timeval tv1, tv2;  /* Timers */

  /* Main routine */
  if (*status != SAI__OK) return;

    /*** TIMER ***/
    smf_timerinit( &tv1, &tv2, status );

  /* Check for valid inputs */

  if( data->ndims != 3 ) {
    *status = SMF__WDIM;
    errRepf( "", FUNC_NAME ": Supplied smfData has %zu dims, needs 3", status,
             data->ndims );
    return;
  }

  if( !keymap ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL AstKeyMap supplied", status );
    return;
  }

  /* Get cleaning parameters */
  smf_get_cleanpar( keymap, &badfrac, &dcfitbox, &dcmaxsteps,
                    &dcthresh, &dcsmooth, &dkclean,
                    &fillgaps, NULL, NULL, NULL, NULL, NULL, NULL,
                    &flagstat, &order, &spikethresh, &spikebox, &noiseclip,
                    status );

  /* Update quality by synchronizing to the data array VAL__BADD values */
  msgOutif(MSG__VERB,"", FUNC_NAME ": update quality", status);
  smf_update_quality( data, 1, NULL, 0, badfrac, status );

  /*** TIMER ***/
  msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME ":   ** %f s updating quality",
             status, smf_timerupdate(&tv1,&tv2,status) );

  /* Fix DC steps */
  if( dcthresh && dcfitbox ) {
    msgOutiff(MSG__VERB, "", FUNC_NAME
              ": Flagging bolos with %lf-sigma DC steps in %" DIM_T_FMT " "
              "samples as bad, using %" DIM_T_FMT "-sample median filter and max %d "
              "DC steps per min before flagging entire bolo bad...", status,
              dcthresh, dcfitbox, dcsmooth, dcmaxsteps);

    smf_fix_steps( wf, data, dcthresh, dcsmooth, dcfitbox, dcmaxsteps,
                   &nflag, NULL, NULL, NULL, status );

    msgOutiff(MSG__VERB, "", FUNC_NAME": ...%zd flagged\n", status, nflag);

    /*** TIMER ***/
    msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME ":   ** %f s fixing DC steps",
               status, smf_timerupdate(&tv1,&tv2,status) );
  }

  /* Flag Spikes */
  if( spikethresh ) {
    msgOutif(MSG__VERB," ", FUNC_NAME ": flag spikes...", status);
    smf_flag_spikes( wf, data, SMF__Q_FIT, spikethresh, spikebox,
                     &nflag, status );
    msgOutiff(MSG__VERB,"", FUNC_NAME ": ...found %zd", status, nflag );

    /*** TIMER ***/
    msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME ":   ** %f s flagging spikes",
               status, smf_timerupdate(&tv1,&tv2,status) );
  }

  /*  Flag periods of stationary pointing */
  if( flagstat ) {
    if( data->hdr && data->hdr->allState ) {
      msgOutiff( MSG__VERB, "", FUNC_NAME
                 ": Flagging regions with slew speeds < %lf arcsec/sec", status,
                 flagstat );
      smf_flag_stationary( data, flagstat, &nflag, status );
      msgOutiff( MSG__VERB,"", "%zu new time slices flagged", status, nflag);

      /*** TIMER ***/
      msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                 ":   ** %f s flagging stationary",
                 status, smf_timerupdate(&tv1,&tv2,status) );
    } else {
      msgOutif( MSG__DEBUG, "", FUNC_NAME
                ": Skipping flagstat because no header present", status );
    }
  }

  /* Clean out the dark squid signal */
  if( dkclean ) {
    msgOutif(MSG__VERB, "", FUNC_NAME ": Cleaning dark squid signals from data.",
             status);
    smf_clean_dksquid( data, 0, 100, NULL, 0, 0, 0, status );

    /*** TIMER ***/
    msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME ":   ** %f s DKSquid cleaning",
               status, smf_timerupdate(&tv1,&tv2,status) );
  }

  /* Gap filling */
  if( fillgaps ) {
    msgOutif(MSG__VERB, "", FUNC_NAME ": Gap filling.", status);
    smf_fillgaps( wf, data, SMF__Q_GAP, status );

    /*** TIMER ***/
    msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME ":   ** %f s gap filling",
               status, smf_timerupdate(&tv1,&tv2,status) );
  }

  /* Remove baselines */
  if( order >= 0 ) {
    msgOutiff( MSG__VERB,"", FUNC_NAME
               ": Fitting and removing %i-order polynomial baselines",
               status, order );

    smf_fit_poly( wf, data, order, 1, NULL, status );

    /*** TIMER ***/
    msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
               ":   ** %f s removing poly baseline",
               status, smf_timerupdate(&tv1,&tv2,status) );
  }

  /* filter the data */
  filt = smf_create_smfFilter( data, status );
  smf_filter_fromkeymap( filt, keymap, &dofft, status );
  if( (*status == SAI__OK) && dofft ) {
    msgOutif( MSG__VERB, "", FUNC_NAME ": frequency domain filter", status );
    smf_filter_execute( wf, data, filt, status );

    /*** TIMER ***/
    msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME ":   ** %f s filtering data",
               status, smf_timerupdate(&tv1,&tv2,status) );
  }
  filt = smf_free_smfFilter( filt, status );

  /* Noise mask */
  if( (*status == SAI__OK) && (noiseclip > 0.0) ) {
    smf_mask_noisy( wf, data, noiseclip, status );

    /*** TIMER ***/
    msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME ":   ** %f s masking noisy",
               status, smf_timerupdate(&tv1,&tv2,status) );
  }
}
