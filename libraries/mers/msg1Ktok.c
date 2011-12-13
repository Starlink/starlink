/*
*+
*  Name:
*     msg1Ktok

*  Purpose:
*     Clear the message token table.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     msg1Ktok();

*  Description:
*     Clear all the message tokens at the current context level.

*  Arguments:
*     None

*  Algorithm:
*     Call emsExpnd with bad status - just kills tokens

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     AJC: A.J.Chipperfield  (STARLINK, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     22-FEB-2001 (AJC):
*        Original version - to avoid use of EMS internal EMS1_KTOK
*     24-JUL-2008 (TIMJ):
*        Rewrite in C.
*     23-DEC-2008 (TIMJ):
*        New emsExpnd API.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "mers1.h"
#include "ems.h"
#include "sae_par.h"

void msg1Ktok ( void ) {
  int istat;                    /* Local status */
  int lenstr;                /* Length of returned string */
  char string[8];               /* Dummy string */

  /* Set status to bad and call emsExpnd */
  istat = SAI__ERROR;
  emsExpnd( " ", string, sizeof(string), 0, &lenstr, &istat );
}

