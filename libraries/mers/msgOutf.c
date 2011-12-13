/*
*+
*  Name:
*     msgOutf

*  Purpose:
*     Output a formatted message.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     msgOutf( msglev_t prior, const char * param, const char * text,
*               int * status, ... );

*  Description:
*     Any tokens in supplied message are expanded and the result is
*     output to the user. If the status argument is not set to SAI__OK
*     on entry, no action is taken except that the values of any
*     existing message tokens are always left undefined after a call to
*     msgOut. If an output error occurs, an error is reported and the
*     status argument is returned set to MSG__OPTER.
*
*     A call to msgOut is equivalent to a call to msgOutif with the
*     message output priority set to MSG__NORM.
*
*     sprintf-style formatting is applied.

*  Arguments:
*     param = const char * (Given)
*        The message name.
*     text = const char * (Given)
*        The message text.
*     status = int * (Given and Returned)
*        The global status.
*     ... = variadic arguments required by sprintf (Given)

*  Notes:
*     Formatting is applied after token replacement. Tokens containing
*     "%" will not be treated as format specifiers. Keyword
*     associations will be disabled since they also use "%".
*
*     Using printf formatting can be useful for simplifying code that
*     does not require deferred token handling. See also msgFmt() for
*     formatting tokens.


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
*     24-DEC-2008 (TIMJ):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "msg_par.h"
#include "mers1.h"
#include "merswrap.h"

#include <stdarg.h>

void msgOutf( const char * param, const char * text, int *status, ... ) {
  va_list args;
  va_start( args, status );
  /* duplicated msgOutiff since we do not have a va_list version as
     intermediary (msgOutifv) - not currently warranted. */
  msg1Outif( MSG__NORM, param, text, 1, args, status );
  va_end( args );
}
