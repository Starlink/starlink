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
*     The DIMM uses an ordered array of function pointers to each model
*     calculation. Given a smf_modeltype return the appropriate function
*     pointer.

*  Notes:
*     This function will not return a pointer to smf_calmodel_ast since its
*     prototype has the additional parameter "lut" and is a special case.

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2007-02-12 (EC):
*        Initial Version
*     2007-05-17 (EC)
*        Added SMF__NOI
*     2007-07-10 (EC):
*        Fixed problem with function pointer cast
*     2007-08-21 (EC):
*        Fixed up warnings caused by incorrect return type
*     2009-03-12 (EC):
*        Added SMF__FLT
*     2010-05-13 (TIMJ):
*        Added SMF__PLN
*     2010-05-27 (TIMJ):
*        Add SMF__SMO
*     2010-06-08 (EC):
*        Add SMF__TWO
*     2014-12-18 (DSB):
*        Added SSN.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     Copyright (C) 2007-2009 University of British Columbia.
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

#define FUNC_NAME "smf_model_getptr"

smf_calcmodelptr smf_model_getptr( smf_modeltype type, int *status) {

  /* Local Variables */
  smf_calcmodelptr retval = NULL;

  /* Main routine */
  if (*status != SAI__OK) return NULL;

  switch( type ) {

  case SMF__COM:
    retval = (smf_calcmodelptr) &smf_calcmodel_com;
    break;

  case SMF__EXT:
    retval = (smf_calcmodelptr) &smf_calcmodel_ext;
    break;

  case SMF__NOI:
    retval = (smf_calcmodelptr) &smf_calcmodel_noi;
    break;

  case SMF__DKS:
    retval = (smf_calcmodelptr) &smf_calcmodel_dks;
    break;

  case SMF__GAI:
    retval = (smf_calcmodelptr) &smf_calcmodel_gai;
    break;

  case SMF__FLT:
    retval = (smf_calcmodelptr) &smf_calcmodel_flt;
    break;

  case SMF__PLN:
    retval = (smf_calcmodelptr) &smf_calcmodel_pln;
    break;

  case SMF__SMO:
    retval = (smf_calcmodelptr) &smf_calcmodel_smo;
    break;

  case SMF__SSN:
    retval = (smf_calcmodelptr) &smf_calcmodel_ssn;
    break;

  case SMF__TWO:
    retval = (smf_calcmodelptr) &smf_calcmodel_two;
    break;

  case SMF__TMP:
    retval = (smf_calcmodelptr) &smf_calcmodel_tmp;
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
