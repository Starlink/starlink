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
*        Header. obsmode, obstype, swmode, seqtyp and inbeam entries are updated.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function examines the FITS header to determine the natuve of the
*     observing mode. Determines the observing mode, switching mode, the observation type,
*     sequence type and whether anything is in the beam.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-07-24 (TIMJ):
*        Initial version.
*     2008-07-30 (EC):
*        Don't fail if smfData is not data from a known instrument.
*     2009-04-24 (TIMJ):
*        Add ACSIS observing modes.
*     2010-02-19 (TIMJ):
*        Recognize fast flatfields.
*     2010-08-09 (TIMJ):
*        Add INBEAM support.
*     2010-12-06 (TIMJ):
*        Look for engineering modes.
*     2011-02-08 (TIMJ):
*        Add SETUP as an obs type

*  Notes:
*     This function relies on an accurate hdr->instrument. i.e. call
*     smf_inst_get first.

*  Copyright:
*     Copyright (C) 2008-2011 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

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
#include "star/one.h"

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_calc_mode"

static smf_obstype smf__parse_obstype ( char obs_type[], int *status );

void smf_calc_mode ( smfHead * hdr, int * status ) {

  char sam_mode[SZFITSTR];   /* Value of SAM_MODE header */
  char obs_type[SZFITSTR];   /* value of OBS_TYPE header */
  char sw_mode[SZFITSTR];    /* value of SW_MODE header */
  char seq_type[SZFITSTR];   /* value of SEQ_TYPE header */
  char inbeamstr[SZFITSTR];  /* value of INBEAM header */

  smf_obstype type = SMF__TYP_NULL;   /* temporary type */
  smf_obstype stype = SMF__TYP_NULL;   /* temporary seq type */
  smf_obsmode mode = SMF__OBS_NULL;   /* temporary mode */
  smf_swmode  swmode = SMF__SWM_NULL; /* Switching mode */
  smf_inbeam_t inbeam = SMF__INBEAM_NOTHING; /* what is in beam? */

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
    smf_fits_getS( hdr, "SW_MODE", sw_mode, sizeof(sw_mode), status );
    smf_fits_getS( hdr, "OBS_TYPE", obs_type, sizeof(obs_type), status );

    /* INBEAM can be undef */
    inbeamstr[0] = '\0';
    smf_getfitss( hdr, "INBEAM", inbeamstr, sizeof(inbeamstr), status );

    /* SEQ_TYPE is "new" */
    if ( *status == SAI__OK ) {
      smf_fits_getS( hdr, "SEQ_TYPE", seq_type, sizeof(seq_type), status );
      if (*status == SMF__NOKWRD ) {
        errAnnul( status );
        one_strlcpy( seq_type, obs_type, sizeof(seq_type), status );
      }
    }

    /* start with sample type */
    if (strcasecmp( sam_mode, "SCAN" ) == 0 ||
        strcasecmp( sam_mode, "RASTER") == 0 ) {
      mode = SMF__OBS_SCAN;
    } else if (strcasecmp( sam_mode, "STARE" ) == 0) {
      mode = SMF__OBS_STARE;
    } else if (strcasecmp( sam_mode, "DREAM" ) == 0) {
      mode = SMF__OBS_DREAM;
    } else if (strcasecmp( sam_mode, "JIGGLE" ) == 0) {
      mode = SMF__OBS_JIGGLE;
    } else if (strcasecmp( sam_mode, "GRID" ) == 0) {
      mode = SMF__OBS_GRID;
    } else {
      if (*status != SAI__OK) {
        *status = SAI__ERROR;
        msgSetc( "MOD", sam_mode );
        errRep( " ", "Unrecognized observing mode '^MOD'", status );
      }
    }

    /* switching mode: options are "none", "pssw", "chop", "freqsw", "self" */
    if (strcasecmp( sw_mode, "NONE" ) == 0 ) {
      swmode = SMF__SWM_NULL;
    } else if (strcasecmp( sw_mode, "PSSW" ) == 0) {
      swmode = SMF__SWM_PSSW;
    } else if (strcasecmp( sw_mode, "CHOP" ) == 0) {
      swmode = SMF__SWM_CHOP;
    } else if (strcasecmp( sw_mode, "SELF" ) == 0) {
      swmode = SMF__SWM_SELF;
    } else if (strcasecmp( sw_mode, "FREQSW" ) == 0) {
      swmode = SMF__SWM_FREQSW;
    } else {
      if (*status != SAI__OK) {
        *status = SAI__ERROR;
        msgSetc( "MOD", sw_mode );
        errRep( " ", "Unrecognized switching mode '^MOD'", status );
      }
    }

    /* obs type */
    type = smf__parse_obstype( obs_type, status );
    stype = smf__parse_obstype( seq_type, status );

    /* in beam (convert to upper case to make it case insensitive) */
    astChrCase( NULL, inbeamstr, 1, 0 );
    if ( smf_pattern_extract( inbeamstr, "(POL)", NULL, NULL, 0, status ) ) {
      inbeam |= SMF__INBEAM_POL;
    }
    if ( smf_pattern_extract( inbeamstr, "(FTS)", NULL, NULL, 0, status ) ) {
      inbeam |= SMF__INBEAM_FTS;
    }
    if ( smf_pattern_extract( inbeamstr, "(BODY)", NULL, NULL, 0, status ) ) {
      inbeam |= SMF__INBEAM_BLACKBODY;
      /* We could consider ensuring FTS is not set in this case */
    }
  }

  hdr->obstype = type;
  hdr->seqtype = stype;
  hdr->obsmode = mode;
  hdr->swmode = swmode;
  hdr->inbeam = inbeam;

}


/* Factor out the OBS_TYPE checking for re-use in SEQ_TYPE checking */

static smf_obstype smf__parse_obstype ( char obs_type[], int *status ) {
  smf_obstype type = SMF__TYP_NULL;

  if (*status != SAI__OK) return type;

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
  } else if (strcasecmp( obs_type, "FASTFLAT" ) == 0) {
    type = SMF__TYP_FASTFLAT;
  } else if (strcasecmp( obs_type, "NOISE" ) == 0) {
    type = SMF__TYP_NOISE;
  } else if (strcasecmp( obs_type, "HEATRAMP" ) == 0) {
    type = SMF__TYP_HEATRAMP;
  } else if (strcasecmp( obs_type, "BIASRAMP" ) == 0) {
    type = SMF__TYP_BIASRAMP;
  } else if (strcasecmp( obs_type, "BIASSAW" ) == 0) {
    type = SMF__TYP_BIASSAW;
  } else if (strcasecmp( obs_type, "NEP" ) == 0) {
    type = SMF__TYP_NEP;
  } else if (strcasecmp( obs_type, "RAMP" ) == 0) {
    type = SMF__TYP_RAMP;
  } else if (strcasecmp( obs_type, "IV_CURVES_M" ) == 0) {
    type = SMF__TYP_IV_CURVES_M;
  } else if (strcasecmp( obs_type, "IV_CURVES_H" ) == 0) {
    type = SMF__TYP_IV_CURVES_H;
  } else if (strcasecmp( obs_type, "OPEN_LOOP_G" ) == 0) {
    type = SMF__TYP_OPEN_LOOP_G;
  } else if (strcasecmp( obs_type, "SETUP" ) == 0) {
    type = SMF__TYP_SETUP;
  } else {
    if (*status == SAI__OK) {
      *status = SAI__ERROR;
      msgSetc( "TYP", obs_type );
      errRep( " ", "Unrecognized observation type '^TYP'", status );
    }
  }
  return type;
}
