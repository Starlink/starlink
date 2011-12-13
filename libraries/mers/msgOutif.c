/*
*+
*  Name:
*     msgOutif

*  Purpose:
*     Conditionally deliver the text of a message to the user.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     msgOutif( msglev_t prior, const char * param, const char * text,
*               int * status );

*  Description:
*     Depending upon the given value of the given message priority and
*     the message filtering level set using msgIfset, the message
*     text is either expanded and output to the user or discarded.
*     The values of any existing message tokens are always annulled by
*     a call to msgOutif. If an output error occurs, an error is
*     reported and the status argument returned set to MSG__OPTER.

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
*     status = int * (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2008, 2009 Science and Technology Facilities Council.
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
*     Copyright (C) 1996, 1999, 2001 Central Laboratory of the Research Councils.
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
*     AJC: A. J. Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     EC: Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     10-JUN-1991 (PCTR):
*        Original version.
*     26-AUG-1992 (PCTR):
*        Call MSG1_FORM and MSG1_PRINT directly, instead of MSG_OUT
*        (MSG_OUT no calls MSG_OUTIF with PRIOR set to MSG__NORM).
*     25-JAN-1996 (AJC):
*        re-format CHARACTER declarations
*     15-SEP-1999 (AJC):
*        Add CLEAN argument to call MSG1_FORM
*     22-FEB-2001 (AJC):
*        Use MSG1_KTOK not EMS1_KTOK
*     02-MAY-2008 (EC):
*        Fixed logic for MSG__DEBUG
*     24-JUL-2008 (TIMJ):
*        Use common block accessor
*     10-SEP-2008 (TIMJ):
*        Rewrite in C
*     23-DEC-2008 (TIMJ):
*        Use msglev_t rather than simple integer.
*     24-DEC-2008 (TIMJ):
*        Now calls msg1Outif.
*     09-JAN-2009 (TIMJ):
*        Update prologue for message levels.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "msg_par.h"
#include "mers1.h"
#include "merswrap.h"

#include <stdarg.h>

void msgOutif( msglev_t prior, const char * param, const char * text,
               int * status) {
  va_list args;
  msg1Outif( prior, param, text, 0, args, status );
}
