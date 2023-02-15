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
*     pntr = smf_construct_smfDA( smfDA * tofill, smfData *dksquid,
*                    double * flatcal, double * flatpar,
*                    smf_flatmeth flatmeth, int nflat, double refres,
*                    double * heatval, dim_t nheat, int * status );

*  Arguments:
*     tofill = smfDA* (Given)
*        If non-NULL, this is the struct filled by this routine. Else,
*        a smfDA is allocated and returned.
*     dksquid = smfData* (Given)
*        Pointer to dark squid values
*     flatcal = double* (Given)
*        Pointer to array of flat calibration values.
*     flatpar = double* (Given)
*        Pointer to array of flat parameters
*     flatmeth = smf_flatmeth (Given)
*        Flatfield method enum.
*     nflat = int (Given)
*        Number of flatfield parameters per bolometer
*     refres = double (Given)
*        Reference resistance used to calculate the flatfield
*     heatval = double * (Given)
*        Pointer to array of heater values used for flatfield calculations.
*     nheat = dim_t (Given)
*        Number of elements in heatval.
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
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-01-26 (TIMJ):
*        Initial version.
*     2006-01-26 (TIMJ):
*        No longer have dksquid.
*     2006-03-29 (AGG):
*        Use smf_create_smfDA to create an empty smfDA
*     2008-07-11 (TIMJ):
*        use one_strlcpy. Add dksquid.
*     2010-03-09 (TIMJ):
*        flatname is now flatmeth
*     2011-09-08 (TIMJ):
*        Add refres
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008,2010-2011 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

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

#define FUNC_NAME "smf_construct_smfDA"

smfDA *
smf_construct_smfDA( smfDA * tofill, smfData * dksquid, double * flatcal,
		     double * flatpar, smf_flatmeth flatmeth,
                     int nflat, double refres, double * heatval,
                     dim_t nheat, int * status ) {

  smfDA * da = NULL;   /* File components */

  da = tofill;
  if (*status != SAI__OK) return da;

  /* If we have a null pointer then create the barebones smfDA
     struct */
  if (tofill == NULL) {
    da = smf_create_smfDA( status );
  }

  if (*status == SAI__OK) {
    da->dksquid = dksquid;
    da->flatcal = flatcal;
    da->flatpar = flatpar;
    da->heatval = heatval;
    da->nflat = nflat;
    da->nheat = nheat;
    da->refres = refres;
    da->flatmeth = flatmeth;
  } else {
    msgOutif(MSG__VERB," ",
	     "Unable to allocate memory for new smfDA", status);
    return NULL;
  }

  return da;
}
