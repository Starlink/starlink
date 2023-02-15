/*
*+
*  Name:
*     smf_flatten

*  Purpose:
*     Low-level FLATFIELD implementation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_flatten( smfData *data, AstKeyMap * heateffmap, int *status );

*  Arguments:
*     data = smfData* (Given and Returned)
*        Pointer to a smfData struct
*     heateffmap = AstKeyMap * (Given)
*        Details of heater efficiency data to be applied during flatfielding.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This subroutine calls the low-level sc2math_flatten subroutine.

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2005-12-06 (AGG):
*        Initial test version.
*     26-JUL-2006 (TIMJ):
*        Remove sc2store_struct
*     2008-03-10 (AGG):
*        Add check for bad values on output from flatfield routine and
*        set quality to SMF__Q_BADS
*     2008-03-14 (AGG):
*        Check for quality array after flatfield and set if present
*     2010-03-09 (TIMJ):
*        Change type of flatfield method in smfDA
*     2010-03-19 (EC):
*        Renamed SMF__Q_BADS to SMF__Q_BADDA
*     2011-09-07 (TIMJ):
*        Apply heater efficiency data after flatfielding.
*     2012-04-03 (TIMJ):
*        Report cases where all bolometers are disabled by flatfielding.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010-2012 Science & Technology Facilities Council.
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     2005-2008 University of British Columbia.
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

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* Standard includes */
#include <string.h>
#include <stdio.h>

/* Starlink includes */
#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"
#include "msg_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

/* SC2DA includes */
#include "sc2da/sc2math.h"

void smf_flatten ( smfData *data, AstKeyMap * heateffmap, int *status ) {

  smfDA *da = NULL;            /* Pointer to struct containing flatfield info */
  double *dataArr = NULL;      /* Pointer to flatfielded data array */
  dim_t i;                     /* Loop counter */
  dim_t ndat;                  /* Total number of data points */
  dim_t nboll;                 /* Number of bolometers */
  dim_t nframes;               /* Number of frames (timeslices) */
  dim_t ngood;                 /* Number of good bolometers in flatfield */
  void *pntr[3];               /* Array of pointers for DATA, QUALITY & VARIANCE */
  smf_qual_t *qual;         /* Pointer to quality array */

  if ( *status != SAI__OK ) return;

  if ( data == NULL ) {
    *status = SAI__ERROR;
    errRep("smf_flatten", "Null data structure passed to smf_flatten", status);
    return;
  }

  /* Check that we have actually have everything we need */
  da = data->da;
  pntr[0] = (data->pntr)[0];
  dataArr = pntr[0];

  if ( da == NULL ) {
    *status = SAI__ERROR;
    errRep("smf_flatten", "No flatfield information in data structure", status);
    return;
  }

  if ( dataArr == NULL ) {
    *status = SAI__ERROR;
    errRep("smf_flatten", "Null data array in data structure", status);
    return;
  }

  /* Calculate the number of bolometer and number of frames (timeslices) */
  nboll = (data->dims)[0]*(data->dims)[1];
  nframes = (data->dims)[2];

  /* Flatfielder */
  ngood = sc2math_flatten( nboll, nframes, smf_flat_methstring(da->flatmeth,status), da->nflat, da->flatcal,
                           da->flatpar, dataArr, status);

  if (ngood == 0) {
    msgOutif( MSG__QUIET, "",
              "         **************************************", status );
    smf_smfFile_msg( data->file, "FILE", 1, "<unknown>");
    msgOutif( MSG__QUIET, "",
              "WARNING: All bolos in ^FILE disabled with bad flatfield.",
              status );
    msgOutif( MSG__QUIET, "",
              "         **************************************", status );
  }

  /* Update units and title if we have a header */
  smf_set_clabels( "Flatfielded", NULL, SIPREFIX "W", data->hdr, status);

  /* Now correct for heater efficiency */
  if (heateffmap && da->refres != VAL__BADD && *status == SAI__OK) {
    /* Do not apply the correction if these data do not know the
       reference resistance. We only know the refres if we have been
       flatfielded by a system that knows we have heater efficiencies */
    if (da->refres != VAL__BADD) {
      char arrayidstr[32];
      smf_fits_getS( data->hdr, "ARRAYID", arrayidstr, sizeof(arrayidstr),
                     status );
      if (astMapHasKey( heateffmap, arrayidstr )) {
        void * tmp = NULL;
        astMapGet0P( heateffmap, arrayidstr, &tmp );
        if (tmp) {
          smfData * heateff = tmp;
          smf_scale_bols( NULL, data, heateff, NULL, "HEATEFF", 0, status );
          msgOutiff(MSG__VERB, "", "Applying heater efficiency data for array '%s'",
                    status, arrayidstr);
        } else {
          msgOutiff( MSG__QUIET, "", "Unable to find heater efficiency data "
                     " for array '%s' (possible programming error)", status, arrayidstr );
        }
      } else {
        /* Do not warn for simulated data */
        if (strncmp( arrayidstr, "SIM", 3 ) != 0 ) {
          msgOutiff( MSG__QUIET, "", "Unable to find heater efficiency data "
                     " for array '%s'", status, arrayidstr );
        }
      }
    } else {
      msgOutif(MSG__QUIET, "", "Using old-style flatfield so not applying "
               "heater efficiency data. Consider re-running calcflat or using a flatramp.", status );
    }
  }

  /* Now check for a QUALITY array */
  qual = data->qual;
  if ( qual != NULL ) {
    /* Check for BAD values from flatfield routine and set QUALITY
       accordingly. Any bad values at this point means that those
       samples were flagged as such by the DA system and thus should
       be assigned a quality value of SMF__Q_BADDA */
    msgOutif(MSG__DEBUG, "",
             "smfData has a valid QUALITY array: setting SMF__Q_BADDA flags",
             status);
    ndat = nboll * nframes;
    for (i=0; i<ndat; i++) {
      if ( dataArr[i] == VAL__BADD ) {
        qual[i] |= SMF__Q_BADDA;
      }
    }
  } else {
    msgOutif(MSG__DEBUG, "", "smfData has no QUALITY array", status);
  }

}

