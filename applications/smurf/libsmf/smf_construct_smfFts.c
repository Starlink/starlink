/*
*+
*  Name:
*     smf_construct_smfFts

*  Purpose:
*     Populate a smfFts structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     pntr = smf_construct_smfFts(
                smfFts* tofill,
                smfData* fpm,
                smfData* sigma,
                double wnFactor,
                int * status );

*  Arguments:
*     tofill = smfFts* (Given)
*        If non-NULL, this is the struct filled by this routine. Else,
*        a smfFts is allocated and returned.
*     fpm = smfData* (Given)
*        Pointer to coefficients of the fitting polynomial.
*     sigma = smfData* (Given)
*        Pointer to standard deviations.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     smf_construct_smfFts = smfFts*
*        Pointer to newly created smfFts (NULL on error)
*        If "tofill" is non-NULL, returns the "tofill" pointer

*  Description:
*     This function (optionally) allocates memory for a smfFts structure and
*     all the internal structures. The structure is initialised.

*  Notes:
*     - Pointers are only stored, not copied.

*  Authors:
*     Coskun Oba (COBA, UoL)
*     {enter_new_authors_here}

*  History:
*     2010-09-16 (COBA):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008,2010 Science and Technology Facilities Council.
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

#define FUNC_NAME "smf_construct_smfFts"

smfFts*
smf_construct_smfFts(smfFts* tofill, smfData* fpm, smfData* sigma, int* status)
{
  if(*status != SAI__OK) { return NULL; }

  if(tofill == NULL) {
    smfFts* fts = smf_create_smfFts(status);
    if(*status == SAI__OK) {
      fts->fpm = fpm;
      fts->sigma = sigma;
      return fts;
    } else {
      msgOutif( MSG__VERB,
                " ",
                "Unable to construct smfFts structure!",
                status);
      return NULL;
    }
  }

  return tofill;
}
