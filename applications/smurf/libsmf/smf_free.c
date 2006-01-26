/*
*+
*  Name:
*     smf_free

*  Purpose:
*     Low-level SMURF free

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_free( void * pntr, int * status );

*  Arguments:
*     pntr = void * (Given)
*        Memory location to be freed.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the SMURF free routine. It should be used in preference
*     to system free() and should be paired with a SMURF allocation routine.

*  Authors:
*     Tim Jenness (TIMJ)
*     {enter_new_authors_here}

*  History:
*     2006-01-25 (TIMJ):
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research
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

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"

#define FUNC_NAME "smf_free"

void
smf_free( void * pntr, int * status ) {
  astFree( pntr );
}
