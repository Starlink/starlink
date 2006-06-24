/*
*+
*  Name:
*     smf_addto_smfArray

*  Purpose:
*     Add smfDatas to a smfArray structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_addto_smfArray( smfArray *ary, const smfData *data, int * status );

*  Arguments:
*     ary = smfArray* (Given and Returned)
*        Pointer to smfArray to populate
*     data = const smfData* (Given)
*        Pointer to smfData to be added
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function adds smfDatas to a smfArray structure.

*  Notes:
*     - Free this memory using smf_close_file
*     - Data arrays are not populated by this routine. The pointers
*       are set to NULL.

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-06-02 (AGG):
*        Initial version
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

#define FUNC_NAME "smf_addto_smfArray"

void smf_addto_smfArray( smfArray *ary, const smfData *data, int * status ) {

  size_t ndat;
  int i;

  if (*status != SAI__OK) return;

  ndat = ary->ndat;

  /* Check for valid number of smfData pointers */
  if ( ndat < 1 || ndat > SMF__MXSMF) {
    if ( *status == SAI__OK ) {
      msgSeti("N",ndat);
      *status = SAI__ERROR;
      errRep(FUNC_NAME, 
	     "Invalid number of smfDatas, ^N. Possible programming error?", 
	     status);
      return;
    }
  }

  /* Loop over pointers in the smfArray: set */
  for ( i=0; i<ndat; i++) {
    /* Check if pointer is NULL */
    if ( (ary->sdata)[i] == NULL ) {
      (ary->sdata)[i] = data;
      break;
    } else if ( i == ndat - 1 ) {
      if ( *status == SAI__OK ) {
	*status = SAI__ERROR;
	errRep(FUNC_NAME, 
	       "Unable to add smfData to current smfArray: all pointers set", 
	       status);
      }
    }
  }

  return;
}
