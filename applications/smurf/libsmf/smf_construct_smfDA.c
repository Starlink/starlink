/*
*+
*  Name:
*     smf_construct_smfDA

*  Purpose:
*     Populate a smfDA structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     pntr = smf_construct_smfDA( smfDA * tofill, int *dksquid,
*                    double * flatcal, double * flatpar, 
*                    const char * flatname, int nflat, int * status );

*  Arguments:
*     tofill = smfDA* (Given)
*        If non-NULL, this is the struct filled by this routine. Else,
*        a smfDA is allocated and returned.
*     dksquid = int* (Given)
*        Pointer to dark squid values
*     flatcal = double* (Given)
*        Pointer to array of flat calibration values.
*     flatpar = double* (Given)
*        Pointer to array of flat parameters
*     flatname = const char * (Given)
*        Name of flatfield algorithm. The string is copied.
*     nflat = int (Given)
*        Number of flatfield parameters per bolometer
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     smf_construct_smfDA = smfDa*
*        Pointer to newly created smfDA (NULL on error)
*        If "tofill" is non-NULL, returns the "tofill" pointer

*  Description:
*     This function (optionally) allocates memory for a smfDA structure and
*     all the internal structures. The structure is initialised.

*  Notes:
*     - Pointers are only stored, not copied.
*     - Free this memory using smf_close_file, via a smfData structure.
*     - Memory for the flatfield structures is not allocated by this
*       routine.
*     - Can be freed with a smf_free if sc2store resources are freed first.

*  Authors:
*     Tim Jenness (TIMJ)
*     {enter_new_authors_here}

*  History:
*     2006-01-26 (TIMJ):
*        Initial version.
*     2006-01-26 (TIMJ):
*        No longer have dksquid.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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
#include "ndf.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_construct_smfDA"

smfDA *
smf_construct_smfDA( smfDA * tofill, double * flatcal,
		     double * flatpar, const char * flatname, int nflat,
		     int * status ) {

  smfDA * da = NULL;   /* File components */

  da = tofill;
  if (*status != SAI__OK) return da;

  if (tofill == NULL) {
    da = smf_malloc( 1, sizeof(smfDA), 0, status );
  }

  if (*status == SAI__OK) {
    da->flatcal = flatcal;
    da->flatpar = flatpar;
    if (flatname != NULL) {
      strncpy(da->flatname, flatname, SC2STORE_FLATLEN);
      (da->flatname)[SC2STORE_FLATLEN-1] = '\0';
    } else {
      (da->flatname)[0] = '\0';
    }
    da->nflat = 0;
  }

  return da;
}
