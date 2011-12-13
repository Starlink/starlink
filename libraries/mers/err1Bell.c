/*
*+
*  Name:
*     err1Bell

*  Purpose:
*     Deliver an ASCII BEL character.

*  Language:
*    Starlink ANSI C

*  Invocation:
*     CALL ERR1_BELL( STATUS )

*  Description:
*     A bell character is delivered to the user. If the user interface
*     in use supports this character, this will ring a bell on the
*     terminal.

*  Arguments:
*     status = int * (Given & Returned)
*        The global status. Not examined on input. If there is an
*        error delivering the bell the returned status will be set
*        to the error value overwriting any pre-existing error status.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1-OCT-1993 (PCTR):
*        Original version.
*     25-JUL-2008 (TIMJ):
*        Rewrite in C
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "mers1.h"
#include "sae_par.h"

void err1Bell( int * status ) {

  int istat = SAI__OK;     /* Local status */

  /*  deliver the bell character. */
  err1Prerr( "\a", status );

  /* Check the local status and return it on error */
  if (istat != SAI__OK) *status = istat;

}
