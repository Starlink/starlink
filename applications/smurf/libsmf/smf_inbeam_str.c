/*
*+
*  Name:
*     smf_inbeam_str

*  Purpose:
*     Convert inbeam bit mask into a string

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void smf_inbeam_str( smf_inbeam_t inbeam, char * inbeamstr,
*              dim_t lenstr, int * status );

*  Arguments:
*     inbeam = smf_inbeam_t (Given)
*        Bit mask indicating which systems are in the beam
*     inbeamstr = char * (Given & Returned)
*        Buffer to receive string.
*     lenstr = dim_t (Given)
         Length of inbeamstr.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Returns a string represenation of an inbeam bit mask. Since
*     more than one thing can be in the beam the string is constructed
*     dynamically.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-08-09 (TIMJ):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
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
#include "star/one.h"

typedef struct {
  smf_inbeam_t inbeam;
  const char * str;
} smfBeamPair;

void
smf_inbeam_str ( smf_inbeam_t inbeam, char *inbeamstr, dim_t lenstr, int * status ) {

  #define NPAIRS 3
  const smfBeamPair beampairs[] = {
    { SMF__INBEAM_POL, "POL" },
    { SMF__INBEAM_FTS, "FTS" },
    { SMF__INBEAM_BLACKBODY, "BLACKBODY" } };

  int i;
  int started = 0;    /* indicate whether we already have a string copied in */

  if (inbeamstr) inbeamstr[0] = '\0';
  if (*status != SAI__OK) return;

  for ( i = 0; i < NPAIRS; i++ ) {
    const smfBeamPair bp = beampairs[i];
    if (inbeam & bp.inbeam) {
      if (started) one_strlcat( inbeamstr, " ", lenstr, status );
      one_strlcat( inbeamstr, bp.str, lenstr, status );
      started = 1;
    }
  }

}
