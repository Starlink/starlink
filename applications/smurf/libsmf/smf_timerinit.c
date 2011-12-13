/*
*+
*  Name:
*     smf_timerinit

*  Purpose:
*     Initialize two timeval structs for use as a timer

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_timerinit( struct timeval *tv1, struct timeval *tv2,int *status);

*  Arguments:
*     tv1 = struct timeval* (Given)
*        Pointer to the first time
*     tv2 = struct timeval* (Given)
*        Pointer to the second time
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:

*  Description:
*     This functionn initializes two timers to the current time of day

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

#define FUNC_NAME "smf_timerinit"

void smf_timerinit( struct timeval *tv1, struct timeval *tv2, int *status) {

  if (*status != SAI__OK) return;

  if( !tv1 || !tv2 ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL pointers to timeval structs supplied.",
            status );
    return;
  }

  gettimeofday( tv1, NULL );
  gettimeofday( tv2, NULL );

  return;
}
