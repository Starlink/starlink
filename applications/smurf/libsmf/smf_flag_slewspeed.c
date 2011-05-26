/*
*+
*  Name:
*     smf_flag_slewspeed

*  Purpose:
*     Flag regions of data during which telescope was stationary

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_flag_slewspeed( smfData *data, double smin, double smax,
*                          size_t *nflagged, double *average_speed,
*                          int *status )

*  Arguments:
*     data = smfData * (Given and Returned)
*        The data that will be flagged
*     smin = double (Given)
*        Speed threshold (arcsec/sec) below which data are flagged. Ignored
*        if set to 0.
*     smax = double (Given)
*        Speed threshold (arcsec/sec) above which data are flagged. Ignored
*        if set to 0.
*     nflagged = size_t * (Returned)
*        The number of new time samples that were flagged. May be NULL.
*     average_speed = double * (Returned)
*        Average speed of the telescope in arcsec/sec in non-flagegd region.
*        May be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Portions of scan data taken while the telescope was stationary
*     is not useful for producing maps. This routine flags data with
*     SMF__Q_STAT that were taken when the requested speed in tracking
*     coordinates was less than 5 arcsec/sec.

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2009-01-05 (EC):
*        Initial Version
*     2010-03-31 (EC):
*        Don't flag regions of padding
*     2010-09-21 (EC):
*        Add average_speed to interface
*     2010-09-23 (EC):
*        - rename sthresh to smin, add smax parameter
*        - rename function from smf_flag_stationary to smf_flag_slewspeed

*  Copyright:
*     Copyright (C) 2009-2010 University of British Columbia.
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
#include "star/slalib.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

/* Other includes */
#include <math.h>

#define FUNC_NAME "smf_flag_slewspeed"

void smf_flag_slewspeed( smfData *data, double smin, double smax,
                         size_t *nflagged, double *average_speed,
                         int *status ) {

  /* Local Variables */
  double accel;                 /* Current acceleration */
  JCMTState *allState=NULL;     /* JCMT state information */
  double avspeed=0;             /* average speed in arcsec/sec */
  size_t bstride;               /* Bolometers stride */
  smf_qual_t *flag=NULL;        /* Array indicating which samples to flag */
  dim_t i;                      /* Loop Counter */
  dim_t j;                      /* Loop Counter */
  double pos1_ac1=0;            /* Coordinates in 3-sample neighbourhood */
  double pos1_ac2=0;            /* "                                     */
  double pos2_ac1=0;            /* "                                     */
  double pos2_ac2=0;            /* "                                     */
  double pos3_ac1;              /* "                                     */
  double pos3_ac2;              /* "                                     */
  size_t navspeed=0;            /* Number of samples to calc avspeed */
  dim_t nbolo=0;                /* Number of bolometers */
  size_t nflag=0;               /* Number of new flagged samples */
  dim_t ntslice=0;              /* Number of time slices */
  smf_qual_t *qua=NULL;         /* Pointer to quality flags */
  double sep1;                  /* Angular separation between samples */
  double sep2;                  /* Angular separation between samples */
  double speed;                 /* Current speed */
  double steptime=0;            /* Step time in seconds */
  size_t tstride;               /* Time stride */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* obtain data dimensions */
  smf_get_dims( data,  NULL, NULL, &nbolo, &ntslice, NULL, &bstride, &tstride,
                status );

  /* other validity checks */
  if( (*status==SAI__OK) && (ntslice<3) ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": Need at least three time steps", status );
  }

  if( smin < 0 ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": smin must be >= 0", status );
  }

  if( smax < 0 ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": smax must be >= 0", status );
  }

  if( !data->hdr ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": Supplied smfData has no header", status );
  } else if( !data->hdr->allState ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": Supplied smfData has no JCMTState in its header",
            status );
  } else if( data->hdr->steptime <= 0 ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": Supplied smfData has invalid STEPTIME",
            status );
  }

  qua = smf_select_qualpntr( data, NULL, status );

  if( !qua ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": No valid QUALITY array was provided", status );
    return;
  }

  /* Allocate flag array */
  flag = astCalloc( ntslice, sizeof(*flag) );

  /* Initial conditions */
  if( *status == SAI__OK ) {
    allState = data->hdr->allState;
    steptime = data->hdr->steptime;
    /* Use tracking coordinate system so that we are measuring motion
       relative to target. Use demand rather than actual since it is
       a cleaner data set and doesn't require filtering. */
    pos1_ac1 = allState[0].tcs_tr_dc1;
    pos1_ac2 = allState[0].tcs_tr_dc2;
    pos2_ac1 = allState[1].tcs_tr_dc1;
    pos2_ac2 = allState[1].tcs_tr_dc2;
  }

  /* Loop over time slice */
  for( i=1; (*status==SAI__OK)&&i<(ntslice-1); i++ ) {
    /* Get new coordinates at third position */
    pos3_ac1 = allState[i+1].tcs_tr_dc1;
    pos3_ac2 = allState[i+1].tcs_tr_dc2;

    /* calculate angular separations between sets of positions in arcsec */
    sep1 = slaDsep( pos1_ac1, pos1_ac2, pos2_ac1, pos2_ac2 ) * DR2AS;
    sep2 = slaDsep( pos2_ac1, pos2_ac2, pos3_ac1, pos3_ac2 ) * DR2AS;

    /* Average speed in arcsec/sec */
    speed = (sep1 + sep2)/(2*steptime);

    /* Acceleration magnitude in arcsec/sec^2 (currently ignored) */
    accel = fabs( (sep2-sep1)/(steptime*steptime) );

    if( (smin && (speed < smin)) || (smax && (speed > smax)) ) {
      /* Does this time step need to be flagged? */
      flag[i] = 1;
    } else {
      /* Update measurement of avspeed */
      avspeed += speed;
      navspeed ++;
    }

    /* Update first two positions */
    pos1_ac1 = pos2_ac1;
    pos1_ac2 = pos2_ac2;
    pos2_ac1 = pos3_ac1;
    pos2_ac2 = pos3_ac2;
  }

  /* Set first and last flag values to nearest estimate */
  if( *status == SAI__OK ) {
    flag[0] = flag[1];
    flag[ntslice-1] = flag[ntslice-2];

    /* Set quality bits -- wherever we don't have padding */
    for( i=0; i<ntslice; i++ ) {
      if( flag[i] && !(qua[i*tstride]&SMF__Q_PAD) ) {
        /* Is this a new flagged sample? */
        if( !(qua[i*tstride]&SMF__Q_STAT) ) nflag++;

        /* Set flag bits for all detectors */
        for( j=0; j<nbolo; j++ ) {
          qua[i*tstride+j*bstride] |= SMF__Q_STAT;
        }
      }
    }
  }

  /* Return nflagged */
  if( nflagged ) *nflagged = nflag;

  /* Return average_speed */
  if( navspeed ) {
    avspeed /= navspeed;
  } else {
    avspeed = VAL__BADD;
  }

  if( average_speed ) *average_speed = avspeed;

  /* Clean up */
  if( flag ) flag = astFree( flag );
}
