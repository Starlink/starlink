/*
*+
*  Name:
*     smf_select_pntr

*  Purpose:
*     Select the proper typed pointer from a smfData

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_select_pntr( void *pntr[2], smf_dtype dtype, double **ddata,
*                       double **dvar, int **idata, int **ivar, int *status );

*  Arguments:
*     pntr = void * [2] (Given)
*        Array of void* pointers. Eg from a smfData.
*     dtype = smf_dtype (Given)
*        Data type associated with pntr[].
*     ddata = double** (Returned)
*        *ddata set to pntr[0] if the smfData is a SMF__DOUBLE type.
*        NULL otherwise. Not accessed if not SMF__DOUBLE.
*     dvar = double** (Returned)
*        *dvar set to pntr[1] if the smfData is a SMF__DOUBLE type.
*        NULL otherwise. dvar can be NULL
*     idata = int** (Returned)
*        *idata set to pntr[0] if the smfData is a SMF__INTEGER type.
*        NULL otherwise. Not accessed if not SMF__INTEGER.
*     ivar = int** (Returned)
*        *ivar set to pntr[1] if the smfData is a SMF__INTEGER type.
*        NULL otherwise. ivar can be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status. Will be set to bad status if the
*        data type is neither INTEGER nor DOUBLE.

*  Description:
*     Selects void pointer of correct type.

*  Notes:

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     27-AUG-2008 (TIMJ):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
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

#include "sae_par.h"
#include "smf_typ.h"
#include "smf.h"
#include "mers.h"

#define FUNC_NAME "smf_select_pntr"

void
smf_select_pntr( void * const pntr[2], smf_dtype dtype, double **ddata, double **dvar,
                 int **idata, int **ivar, int *status ) {

  if (ddata) *ddata = NULL;
  if (dvar) *dvar = NULL;
  if (idata) *idata = NULL;
  if (ivar) *ivar = NULL;

  if (*status != SAI__OK) return;

  switch (dtype) {

  case SMF__DOUBLE:
    if (ddata) {
      *ddata = pntr[0];
    } else {
      *status = SAI__ERROR;
      errRep( " ","Data are _DOUBLE but not double pointer supplied",
              status );
    }
    /* variance is optional */
    if (dvar) *dvar = pntr[1];
    break;

  case SMF__INTEGER:
    if (idata) {
      *idata = pntr[0];
    } else {
      *status = SAI__ERROR;
      errRep( " ","Data are _INTEGER but not double pointer supplied",
              status );
    }
    /* variance is optional */
    if (ivar) *ivar = pntr[1];
    break;

  default:
    msgSetc("DT", smf_dtype_str(dtype, status) );
    *status = SAI__ERROR;
    errRep(" ", "Data type ^DT not supported by " FUNC_NAME, status );
  }

}
