/*
*+
*  Name:
*     MSG_FLUSHERR

*  Purpose:
*     Flush the current error context as if it was an MSG message.

*  Language:
*     Starlink ANSI C (Callable from Fortran)

*  Invocation:
*     CALL MSG_FLUSHERR( STATUS )

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
*     STATUS = INTEGER (Returned)
*        The global status: it is set to SAI__OK on return if the
*        error message output is successful; if not, it is set to
*        ERR__OPTER.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1983, 1984, 1989-1991, 1994 Science & Engineering
*     Research Council. Copyright (C) 1997, 1999, 2001 Central Laboratory
*     of the Research Councils. All Rights Reserved.

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
*     JRG: Jack Giddings (UCL)
*     SLW: Sid Wright (UCL)
*     BDK: Dennis Kelly (ROE)
*     RFWS: R.F. Warren-Smith (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A.J. Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David S Berry(JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     5-JAN-2009 (TIMJ):
*        Copy from err_flush.c to call msgFlusherr
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "merswrap.h"
#include "mers_f77.h"

F77_SUBROUTINE(msg_flusherr)( INTEGER(STATUS) ) {
  int status;
  msgFlusherr( &status );
  F77_EXPORT_INTEGER( status, *STATUS );
}
