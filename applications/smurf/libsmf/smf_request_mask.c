/*
*+
*  Name:
*     smf_request_mask

*  Purpose:
*     Request mask files from parameter.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_request_mask( const char *param, smfArray ** bbms, int *status);

*  Arguments:
*     param = const char * (Given)
*        Name of ADAM parameter to use when requesting bad bolometer mask
*        group of files. Usually "BBM".
*     bbms = smfArray** (Returned)
*        Pointer to smfArray* that will be returned as a valid smfArray
*        if the parameter contained files. If a NULL parameter is given this
*        pointer will be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Request a group of files from the named parameter, open them and
*     store them all in a smfArray. A null parameter response will result
*     in a null pointer rather than a smfArray.

*  Notes:
*     - Calls kpg1Rgndf to obtain files.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     AGG: Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-12-11 (TIMJ):
*        Initial version. Culled from smurf_qlmakemap.c:28576
*     2010-01-08 (AGG):
*        Change BPM to BBM.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Faciltiies Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "star/grp.h"
#include "sae_par.h"
#include "par_err.h"
#include "mers.h"
#include "star/kaplibs.h"

#include "smf.h"

void smf_request_mask( const char *param, smfArray ** bbms, int *status) {
  Grp * bbmgrp = NULL;
  size_t nbbm;


  /* initialise return value */
  *bbms = NULL;

  if (*status != SAI__OK) return;

  kpg1Rgndf( param, 0, 1, "", &bbmgrp, &nbbm, status );
  if (*status == PAR__NULL) {
    bbms = NULL;
    errAnnul( status );
  } else {
    smf_open_group( bbmgrp, NULL, bbms, status );
  }
  if (bbmgrp) grpDelet( &bbmgrp, status );
}
