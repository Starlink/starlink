/*
*+
*  Name:
*     smf_find_airmass_interval

*  Purpose:
*     Find the earliest or latest airmass/elevation values in a smfHead

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_find_airmass_interval( const smfHead * hdr, double *amstart,
*                                double * amend, double *elstart, double *elend,
*                                int * status );

*  Arguments:
*     hdr = const smfHead* (Given)
*        Header struct from data struct.
*     amstart = double * (Given)
*        Pointer to double to receive start airmass value. Can be NULL.
*     amend = double * (Given)
*        Pointer to double to receive end airmass value. Can be NULL.
*     elstart = double * (Given)
*        Pointer to double to receive start elevation value in radians.
*        Can be NULL.
*     elend = double * (Given)
*        Pointer to double to receive end elevation value in radians.
*        Can be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine determines the start and end airmass and elevation.
*     Preferentially uses the JCMTSTATE information but will fall back
*     to using the FITS header. If a value can not be determined it will
*     be set to VAL__BADD.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2011-04-12 (TIMJ):
*        Original version moved from smf_correct_extinction.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
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

#include <stdio.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "msg_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "smf.h"
#include "smurf_par.h"
#include "smurf_typ.h"

void smf_find_airmass_interval( const smfHead *hdr, double * oamstart, double *oamend,
                                double *oelstart, double *oelend, int *status ) {

  double amend = VAL__BADD;
  double amstart = VAL__BADD;
  double elend = VAL__BADD;
  double elstart = VAL__BADD;

  if (*status != SAI__OK) return;

  /* it is possible to have gaps in the TCS part of jcmtstate which causes
     difficulties in determining telescope elevation.
     We can not necessarily assume that first and last airmasses are known so
     we walk through all JCMTState.
  */
  if (hdr->allState) {
    JCMTState * curstate = hdr->allState;
    dim_t nframes = hdr->nframes;
    dim_t k;
    for ( k=0; k<nframes && (*status == SAI__OK) ; k++) {
      amstart = curstate->tcs_airmass;
      elstart = curstate->tcs_az_ac2;
      if (amstart != VAL__BADD && elstart != VAL__BADD
          && amstart != 0.0 ) {
        break;
      }
      curstate++;
    }
    /* reset and start from end */
    curstate = &((hdr->allState)[nframes-1]);
    for ( k=nframes; k>0 && (*status == SAI__OK) ; k--) {
      amend = curstate->tcs_airmass;
      elend = curstate->tcs_az_ac2;
      if (amend != VAL__BADD && elend != VAL__BADD
          && amend != 0.0 ) {
        break;
      }
      curstate--;
    }
  } else {
    smf_getfitsd( hdr, "AMSTART", &amstart, status );
    smf_getfitsd( hdr, "ELSTART", &elstart, status );
    smf_getfitsd( hdr, "AMEND", &amend, status );
    smf_getfitsd( hdr, "ELEND", &elend, status );
    if (elstart != VAL__BADD) elstart *= DD2R;
    if (elend != VAL__BADD) elend *= DD2R;

  }

  /* copy out the results */
  if ( oamstart ) *oamstart = amstart;
  if ( oamend ) *oamend = amend;
  if ( oelstart ) *oelstart = elstart;
  if ( oelend ) *oelend = elend;

}
