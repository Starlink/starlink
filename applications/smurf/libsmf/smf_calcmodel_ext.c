/*
*+
*  Name:
*     smf_calcmodel_ext

*  Purpose:
*     Apply EXTinction correction

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calcmodel_ext( smfDIMMData *dat, int chunk, AstKeyMap *keymap, 
*			 smfArray **allmodel, int flags, int *status)

*  Arguments:
*     dat = smfDIMMData * (Given)
*        Struct of pointers to information required by model calculation
*     chunk = int (Given)
*        Index of time chunk in allmodel to be calculated
*     keymap = AstKeyMap * (Given)
*        Parameters that control the iterative map-maker
*     allmodel = smfArray ** (Returned)
*        Array of smfArrays (each time chunk) to hold result of model calc
*     flags = int (Given )
*        Control flags: only execute if SMF__DIMM_FIRSTITER set
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Dummy routine since EXT is filled once by smf_model_create

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2007-03-05 (EC):
*        Initial Version
*        Modified data array indexing to avoid unnecessary multiplies
*     2007-07-10 (EC):
*        Use smfArray instead of smfData
*     2008-03-04 (EC)
*        Modified interface to use smfDIMMData
*     2008-03-30 (EC)
*        Stripped out functionality. Model is now filled by smf_model_create.
*        Will be applied to data inside smf_calcmodel_ast and smf_iteratemap.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
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
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_calcmodel_ext"

void smf_calcmodel_ext( smfDIMMData *dat, int chunk, AstKeyMap *keymap, 
			smfArray **allmodel, int flags, int *status) {

  /* This is a dummy function, so just return... */
  return;
}



