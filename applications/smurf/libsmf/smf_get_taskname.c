/*
*+
*  Name:
*     smf_get_taskname

*  Purpose:
*     Gets task name associated with this action.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_get_taskname( char * taskname, char * prvname, int *status );

*  Arguments:
*     taskname = char* (Returned)
*        Name of the task invoked for this action. At least
*        PAR__SZNAM+1 characters must be allocated for this buffer.
*        Can be a NULL pointer.
*     prvname = char* (Returned)
*        String to use in provenance tracking. At least 2 * PAR_SZNAM+1
*        characters should be allocated for this buffer. Can be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Gets the ADAM task name associated with this action and the
*     string to be used for provenance identification.

*  Notes:
*     Uses the task_get_name PCS function.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-05-23 (TIMJ):
*        First version.
*     29-JUL-2009 (TIMJ):
*        Use official C interface to taskGetName
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2009 Science and Technology Facilities Council.
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

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "smf.h"

#include "par.h"
#include "star/task_adam.h"
#include "sae_par.h"

void
smf_get_taskname( char * intaskname, char * prvname, int * status ) {

  char tmptaskname[PAR__SZNAM+1]; /* somewhere to store task name if needed */
  char *taskname = NULL;     /* External or internal buffer */

  if (*status != SAI__OK) return;

  /* decide whether we are copying to the supplied buffer */
  if (intaskname) {
    taskname = intaskname;
  } else {
    taskname = tmptaskname;
  }

  /* initialise buffers and get the task name*/
  taskGetName( taskname, PAR__SZNAM+1, status );

  /* Now the provenance name if needed */
  if (prvname) {
    snprintf( prvname, 2*PAR__SZNAM+1, "%s:%s",PACKAGE_UPCASE,taskname);
    prvname[2*PAR__SZNAM] = '\0';
  }
}
