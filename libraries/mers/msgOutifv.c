/*
*+
*  Name:
*     msgOutifv

*  Purpose:
*     Conditionally deliver the formatted text of a message to the user.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     msgOutifv( msglev_t prior, const char * param, const char * text,
*                va_list args, int * status);

*  Description:
*     Depending upon the given value of the given message priority and
*     the message filtering level set using msgIfset, the message
*     text is either expanded and output to the user or discarded.
*     The values of any existing message tokens are always annulled by
*     a call to msgOutif. If an output error occurs, an error is
*     reported and the status argument returned set to MSG__OPTER.
*
*     sprintf-style formatting is applied using variadic arguments.

*  Arguments:
*     prior = msglev_t (Given)
*        Message output filter. This may be one of these values:
*
*           -  MSG__QUIET = always output the message, regardless of the
*           output filter setting; this can be overridden by setting the
*           filter level to MSG___NONE.
*           -  MSG__NORM = output the message if the current output
*           filter is set to either MSG__NORM or MSG__VERB or MSG__DEBUGnn;
*           -  MSG__VERB = output the message only if the current
*           output filter is set to MSG__VERB or MSG__DEBUGnn;
*           -  MSG__DEBUG = out the message only if the current
*           output filter is set to MSG__DEBUGnn.
*           -  MSG__DEBUGnn = output the message only if the current output
*           filter is less than or equal to MSG__DEBUGnn. 1 <= NN <= 20.
*
*        Here, the collating sequence:
*
*           MSG__QUIET < MSG__NORM < MSG__VERB < MSG__DEBUG < MSG__DEBUGnn
*
*        may be assumed. Any other value will result in an error report
*        and the status being returned set to MSG__INVIF: no further
*        action will be taken. MSG__NONE can not be specified as a priority
*        since that is used as a level indicating that all messages should
*        be surpressed. MSG__ALL can also not be a priority since that level
*        indicates that all messages should be displayed.
*     param = const char * (Given)
*        The message name.
*     text = const char * (Given)
*        The message text.
*     args = va_list (Given)
*        Variadic arguments for sprintf processing.
*     status = int * (Given and Returned)
*        The global status.

*  Notes:
*     Formatting is applied after token replacement. Tokens containing
*     "%" will not be treated as format specifiers. Keyword
*     associations will be disabled since they also use "%".
*
*     Using printf formatting can be useful for simplifying code that
*     does not require deferred token handling. See also msgFmt() for
*     formatting tokens.


*  Copyright:
*     Copyright (C) 2009 Science and Technology Facilities Council.
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
*     24-FEB-2009 (TIMJ):
*        Original version. Copied from msgOutiff.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "msg_par.h"
#include "mers1.h"
#include "merswrap.h"

#include <stdarg.h>

void msgOutifv( msglev_t prior, const char * param, const char * text,
		va_list args, int * status) {
  msg1Outif( prior, param, text, 1, args, status );
}
