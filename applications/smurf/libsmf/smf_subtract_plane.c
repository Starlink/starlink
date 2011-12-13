/*
*+
*  Name:
*     smf_subtract_plane

*  Purpose:
*     Wrapper for low-level sky fitting and removal routine

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_subtract_plane( smfData *data, smfArray *array,
*                         const char *fittype, int *status )

*  Arguments:
*     data = smfData* (Given and Returned)
*        Pointer to input data struct, can be NULL
*     array = smfArray* (Given and Returned)
*        Pointer to input array of related data structs, can be NULL
*     fittype = char* (Given)
*        Fit-type for PLANE sky-removal method
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine is a wrapper to select the desired sky subtraction
*     method when the PLANE method is specified in smurf_remsky. The
*     choice is determined by which input is set to NULL. If both are
*     NULL then an error is issued. No check is made to see if both
*     are valid pointers but in this case smf_subtract_plane1 will be
*     called by default as its non-NULL status is checked first.
*
*     Alternatively, the lower-level routines may be called directly
*     as each one is self-contained.


*  Notes:
*     - See also smf_subtract_plane1 and smf_subtract_plane2

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-02-24 (AGG):
*        Initial test version
*     2006-10-12 (AGG):
*        New version to act as wrapper for lower-level routines. See
*        smf_subtract_plane1.c for original incarnation of this file.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 University of British Columbia.
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

/* Standard includes */
#include <stdio.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "msg_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "smf.h"
#include "smurf_par.h"
#include "smurf_typ.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_subtract_plane"

void smf_subtract_plane( smfData *data, smfArray *array, const char *fittype,
			 int *status ) {

  double meansky;
  /* Check status */
  if (*status != SAI__OK) return;

  /* Select sky subtraction method based on input parameters */
  if ( data == NULL ) {
    if ( array != NULL ) {
      smf_subtract_plane2( array, fittype, &meansky, status );
    } else {
      if ( *status == SAI__OK ) {
	*status = SAI__ERROR;
	errRep( FUNC_NAME, "No input data to smf_subtract_plane", status );
      }
    }
  } else {
    smf_subtract_plane1( data, fittype, &meansky, status );
  }

}
