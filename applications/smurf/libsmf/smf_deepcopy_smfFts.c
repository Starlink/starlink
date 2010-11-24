/*
*+
*  Name:
*     smf_deepcopy_smfFts

*  Purpose:
*     Copy all elements of a smfFts structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     newFts = smf_deepcopy_smfFts(const smfData* old, int* status);

*  Arguments:
*     old = const smfData* (Given)
*        Pointer to smfData containing smfFts to be copied
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     smf_deepcopy_smfFts = smfFts*
*        Pointer to newly created smfFts. NULL on error.

*  Description:
*     This function copies all information from an existing smfFts
*     structure and all the internal structures to a new smfFts
*     structure.

*  Notes:
*     - Free this memory using smf_close_file, via a smfData structure.

*  Authors:
*     Coskun Oba (COBA, UoL)
*     {enter_new_authors_here}

*  History:
*     2010-09-16 (COBA):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008, 2010 Science and Technology Facilities Council.
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.

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
#include <stdlib.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "star/one.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_deepcopy_smfFts"

smfFts*
smf_deepcopy_smfFts(const smfData* old, int* status)
{
  if(*status != SAI__OK) { return NULL; }
  if(old == NULL) { return NULL; }

  smfData* zpd    = NULL; /* Pointer to polynomial coefficients */
  smfData* fpm    = NULL; /* Pointer to polynomial coefficients */
  smfData* sigma  = NULL; /* Pointer to standard deviations */
  smfFts* newFts  = NULL; /* Pointer to new smfFts struct */
  smfFts* oldFts  = NULL; /* Pointer to old smfFts struct */

  oldFts = old->fts;
  if(oldFts == NULL) { return NULL; }

  if(oldFts->zpd) {
    zpd = smf_deepcopy_smfData(
              oldFts->zpd, 0,
              SMF__NOCREATE_VARIANCE |
              SMF__NOCREATE_QUALITY |
              SMF__NOCREATE_HEAD |
              SMF__NOCREATE_FILE |
              SMF__NOCREATE_DA |
              SMF__NOCREATE_FTS,
              0, 0, status);
  }

  if(oldFts->fpm) {
    fpm = smf_deepcopy_smfData(
              oldFts->fpm, 0,
              SMF__NOCREATE_VARIANCE |
              SMF__NOCREATE_QUALITY |
              SMF__NOCREATE_HEAD |
              SMF__NOCREATE_FILE |
              SMF__NOCREATE_DA |
              SMF__NOCREATE_FTS,
              0, 0, status);
  }
  if(oldFts->sigma) {
    sigma = smf_deepcopy_smfData(
              oldFts->sigma, 0,
              SMF__NOCREATE_VARIANCE |
              SMF__NOCREATE_QUALITY |
              SMF__NOCREATE_HEAD |
              SMF__NOCREATE_FILE |
              SMF__NOCREATE_DA |
              SMF__NOCREATE_FTS,
              0, 0, status);
  }

  newFts = smf_construct_smfFts(newFts, zpd, fpm, sigma, status);

  return newFts;
}
