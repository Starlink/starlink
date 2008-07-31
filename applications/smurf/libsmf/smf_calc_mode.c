/*
*+
*  Name:
*     smf_calc_mode

*  Purpose:
*     Calculate observing mode and observation type and update smfHead

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_calc_mode( smfHead *hdr, int * status );

*  Arguments:
*     hdr = smfHead * (Given and Returned)
*        Header. obsmode and obstype entries are updated.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function examines the FITS header to determine the observing mode
*     and observation type. If the instrument is INST__NONE they are set to
*     SMF__TYP_NULL and SMF__OBS_NULL.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-07-24 (TIMJ):
*        Initial version.
*     2008-07-30 (EC):
*        Don't fail if smfData is not data from a known instrument.

*  Notes:
*     This function relies on an accurate hdr->instrument. i.e. call
*     smf_inst_get first.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2006-2007 University of British Columbia.
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

/* System includes */
#include <strings.h>

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"

#define FUNC_NAME "smf_calc_mode"

void smf_calc_mode ( smfHead * hdr, int * status ) {
  
  char sam_mode[SZFITSCARD+1];   /* Value of SAM_MODE header */
  char obs_type[SZFITSCARD+1];   /* value of OBS_TYPE header */

  smf_obstype type = SMF__TYP_NULL;   /* temporary type */
  smf_obsmode mode = SMF__OBS_NULL;   /* temporary mode */

  if (*status != SAI__OK) return;

  if (hdr == NULL) {
    *status = SAI__ERROR;
    errRep( " ", "Null pointer supplied to " FUNC_NAME, status );
    return;
  }

  /* Proceed if we're using a valid instrument */
  if( hdr->instrument != INST__NONE ) {

    /* Read the relevant headers */
    smf_fits_getS( hdr, "SAM_MODE", sam_mode, sizeof(sam_mode), status );
    smf_fits_getS( hdr, "OBS_TYPE", obs_type, sizeof(obs_type), status );

    /* start with sample type */
    if (strcasecmp( sam_mode, "SCAN" ) == 0) {
      mode = SMF__OBS_SCAN;
    } else if (strcasecmp( sam_mode, "STARE" ) == 0) {
      mode = SMF__OBS_STARE;
    } else if (strcasecmp( sam_mode, "DREAM" ) == 0) {
      mode = SMF__OBS_DREAM;
    } else {
      if (*status != SAI__OK) {
        *status = SAI__ERROR;
        msgSetc( "MOD", sam_mode );
        errRep( " ", "Unrecognized observing mode '^MOD'", status );
      }
    }

    /* obs type */
    if (strcasecmp( obs_type, "SCIENCE" ) == 0) {
      type = SMF__TYP_SCIENCE;
    } else if (strcasecmp( obs_type, "POINTING" ) == 0) {
      type = SMF__TYP_POINTING;
    } else if (strcasecmp( obs_type, "FOCUS" ) == 0) {
      type = SMF__TYP_FOCUS;
    } else if (strcasecmp( obs_type, "SKYDIP" ) == 0) {
      type = SMF__TYP_SKYDIP;
    } else if (strcasecmp( obs_type, "FLATFIELD" ) == 0) {
      type = SMF__TYP_FLATFIELD;
    } else if (strcasecmp( obs_type, "NOISE" ) == 0) {
      type = SMF__TYP_NOISE;
    } else {
      if (*status != SAI__OK) {
        *status = SAI__ERROR;
        msgSetc( "TYP", obs_type );
        errRep( " ", "Unrecognized observation type '^TYP'", status );
      }
    }
  }

  hdr->obstype = type;
  hdr->obsmode = mode;

}


