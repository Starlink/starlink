/*
*+
*  Name:
*     smf_difftime

*  Purpose:
*     Calculate the difference in seconds between two timevals

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     diff = smf_difftime( struct timeval *tv1,struct timeval *tv2,int *status);

*  Arguments:
*     tv1 = struct timeval* (Given)
*        Pointer to the first timeval struct (set with gettimeofday)
*     tv2 = struct timeval* (Given)
*        Pointer to the second timeval struct (set with gettimeofday)
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     The elapsed time from tv1 to tv2 in seconds. Returns 0 on bad status.

*  Description:
*     Calculate elapsed times

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-11-25 (EC):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 University of British Columbia.
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

#define FUNC_NAME "smf_difftime"

double smf_difftime( struct timeval *tv1, struct timeval *tv2, int *status) {

  if (*status != SAI__OK) return 0;

  if( !tv1 || !tv2 ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL pointers to timeval structs supplied.",
            status );
    return 0;
  }

  return (double)(tv2->tv_sec-tv1->tv_sec) +
    (1.0E-6*(double)(tv2->tv_usec-tv1->tv_usec));
}
