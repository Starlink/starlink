/*
*+
*  Name:
*     msgFlusherr

*  Purpose:
*     Flush the current error context as if it was an MSG message

*  Language:
*     Starlink ANSI C

*  Invocation:
*     msgFlusherr(int * status);

*  Description:
*     Ensure that all pending error messages in the current error
*     context have been output to the user. On successful completion, the
*     error context is annulled and the status argument reset to SAI__OK;
*     if an error occurs during output of the error messages, the
*     error context is not anulled and the status argument is returned
*     set to ERR__OPTER. The messages are output in the same way as using
*     MSG_OUTIF in QUIET mode except that error messages are prepended with
*     "#" similarly to normal error messages (instead of "!").

*  Arguments:
*     status = int * (Returned)
*        The global status: it is set to SAI__OK on return if the
*        error message output is successful; if not, it is set to
*        ERR__OPTER.

*  Algorithm:
*     -  Call err1Flush with bell disabled and msg mode enabled.

*  Copyright:
*     Copyright (C) 2009 Science and Technology Facilities Council.

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
*     5-JAN-2009 (TIMJ):
*        Copy from errFlush and set usemsg to true.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "mers1.h"
#include "merswrap.h"

void msgFlusherr ( int * status ) {
  int errbel = 0;

  /*  Call internal flush routine with BEL disabled and msg enabled. */
  err1Flush( 1, &errbel, status );
}
