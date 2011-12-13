/*
*+
*  Name:
*     msgBlank

*  Purpose:
*     Output a blank line.

*  Language:
*    Starlink ANSI C

*  Invocation:
*     msgBlank( int * status );

*  Description:
*     A blank line is output to the user. If the status argument is not
*     set to SAI__OK on entry, no action is taken. If an output error
*     occurs, an error report is made and the status argument returned
*     set to MSG__OPTER.

*  Arguments:
*     status = int * (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Call msgBlankif with normal priority.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1990, 1991, 1992 Science & Engineering Research Council.
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
*     24-SEP-1990 (PCTR):
*        Original version.
*     24-JAN-1991 (PCTR):
*        Changed to use MSG1_PRINT (i.e. environment independent).
*     26-AUG-1992 (PCTR):
*        Output the blank line conditionally, assuming MSG__NORM to be
*        the priority.
*     10-SEP-2008 (TIMJ):
*        Rewrite in C. Use msgOut.
*     23-DEC-2008 (TIMJ):
*        Call msgBlankif with normal priority.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "merswrap.h"
#include "msg_par.h"

void msgBlank( int * status ) {
  msgBlankif( MSG__NORM, status );
}
