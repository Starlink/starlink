/*
*+
*  Name:
*     smf_copytime

*  Purpose:
*     Copy one struct timeval to another

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_copytime( struct timeval *target, struct timeval *source,int *status);

*  Arguments:
*     target = struct timeval* (Given)
*        Pointer to the destination timeval
*     source = struct timeval* (Given)
*        Pointer to the source timeval
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:

*  Description:
*     Copy all the elements in source timeval struct to target.

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2009-04-23 (EC):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2009 University of British Columbia.
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

/* System includes */
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "prm_par.h"


/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_copytime"

void smf_copytime( struct timeval *target, struct timeval *source,
                     int *status) {

  if (*status != SAI__OK) return;

  if( !target || !source ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL pointers to timeval structs supplied.",
            status );
    return;
  }

  target->tv_sec = source->tv_sec;
  target->tv_usec = source->tv_usec;

  return;
}
