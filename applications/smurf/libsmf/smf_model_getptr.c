/*
*+
*  Name:
*     smf_model_getptr

*  Purpose:
*     Returns pointer to calcmodel function given smf_modeltype

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calcmodelptr *smf_model_getptr( smf_modeltype type, int *status);

*  Arguments:
*     type = smf_modeltype
*        Enumerated model type (Given)
*     name = const char *name (Returned)
*        String corresponding to model type
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2007-02-12 (EC):
*        Initial Version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
*     All Rights Reserved.

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

#define FUNC_NAME "smf_model_getptr"

smf_calcmodelptr *smf_model_getptr( smf_modeltype type, int *status) {

  /* Local Variables */
  char *retval = NULL;

  /* Main routine */
  if (*status != SAI__OK) return;

  switch( type ) {

  case SMF__AST:
    retval = &smf_calcmodel_ast;
    break;
    
  case SMF__COM:
    retval = &smf_calcmodel_com;
    break;

  default:
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Invalid smf_modeltype given, or no function available.",
	   status);        
  }

  return retval;
}
