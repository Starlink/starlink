/*
*+
*  Name:
*     smf_model_getexpptr

*  Purpose:
*     Returns pointer to expmodel function given smf_modeltype

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_expmodelptr *smf_model_getexpptr( smf_modeltype type, int *status);

*  Arguments:
*     type = smf_modeltype
*        Enumerated model type (Given)
*     name = const char *name (Returned)
*        String corresponding to model type
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     The DIMM uses an ordered array of function pointers to each model
*     calculation. Given a smf_modeltype return the appropriate function
*     pointer.

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2010-05-19 (EC):
*        Initial Version
*     2010-06-08 (EC):
*        Add SMF__TWO
*     {enter_further_changes_here}

*  Copyright:
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
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

/* Other includes */
#include <stdio.h>

#define FUNC_NAME "smf_model_getexpptr"

smf_expmodelptr smf_model_getexpptr( smf_modeltype type, int *status) {

  /* Local Variables */
  smf_expmodelptr retval = NULL;

  /* Main routine */
  if (*status != SAI__OK) return NULL;

  switch( type ) {

  case SMF__DKS:
    retval = (smf_expmodelptr) &smf_expmodel_dks;
    break;

  case SMF__TWO:
    retval = (smf_expmodelptr) &smf_expmodel_two;
    break;

  default:
    msgSetc( "NM", smf_model_getname(type, status) );
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME
            ": Invalid smf_modeltype given (^NM), or no function available.",
	   status);
  }

  return retval;
}
