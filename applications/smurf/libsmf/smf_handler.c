/*
*+
*  Name:
*     smf_handler

*  Purpose:
*     Called in response to an interupt signal.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_handler( int sig )

*  Arguments:
*     sig = int (Given)
*        The raised signal type (should always be SIGINT).

*  Description:
*     When an interupt is detected (e.g. control-C) this function is
*     called. It sets the smf_interupt flag non-zero and returns. I tried
*     to do cleverer things here (asking the user how to handle the
*     interupt), but something seems to foul up the stdio system (it works
*     OK in other ADAM apps though!). Might it be a threading issue?

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1-MAY-2012 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
#include <signal.h>

/* SMURF includes */
#include "smf.h"

/* A flag declared in smf_iteratemap used to indicate that an interupt has
   occurred. */
extern volatile sig_atomic_t smf_interupt;

void smf_handler( int sig __attribute__((unused))){
   smf_interupt = 1;
}

