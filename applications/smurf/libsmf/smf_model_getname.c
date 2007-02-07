/*
*+
*  Name:
*     smf_model_getname

*  Purpose:
*     Return strings representation of a smf_modeltype

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     const char *smf_model_getname( smf_modeltype type, int *status);

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
*     2006-07-06 (EC):
*        Initial Version
*     2006-08-16 (EC):
*        Use group expressions for model names (expects _flat for igrp names)
*        Changed technique/interface to look like smf_dtype_string
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

#define FUNC_NAME "smf_model_getname"

char *smf_model_getname( smf_modeltype type, int *status) {

  /* Local Variables */
  char *retval = NULL;

  /* Main routine */
  if (*status != SAI__OK) return;

  switch( type ) {

  case SMF__CUM:
    retval = "*|flat|cum|";
    break;

  case SMF__RES:
    retval = "*|flat|res|";
    break;

  case SMF__AST:
    retval = "*|flat|ast|";
    break;
    
  case SMF__COM:
    retval = "*|flat|com|";
    break;

  case SMF__NOI:
    retval = "*|flat|noi|";
    break;

  default:
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Invalid smf_modeltype given.", status);        
  }

  return retval;
}
