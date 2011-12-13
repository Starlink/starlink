/*
*+
*  Name:
*     msg1Prtln

*  Purpose:
*     Send a line for printing on the relevant device

*  Language:
*     Starlink ANSI C

*  Invocation:
*     msg1Prtln(const char * text, int * status);

*  Description:
*     Clear all the message tokens at the current context level.

*  Arguments:
*     text = const char * (Given)
*       Text to deliver to output device
*     status = int * (Given and Returned)
*       Global status.

*  Notes:
*     - The ADAM implementation calls subParWrmsg();

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25-JUL-2008 (TIMJ):
*        Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "mers1.h"
#include "star/subpar.h"
#include "sae_par.h"

void msg1Prtln( const char * text, int * status ) {
  if (*status != SAI__OK) return;
  subParWrmsg( text, status );
}
