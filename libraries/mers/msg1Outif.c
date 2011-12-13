/*
*+
*  Name:
*     msg1Outif

*  Purpose:
*     Conditionally deliver the text of a message to the user (internal).

*  Language:
*     Starlink ANSI C

*  Invocation:
*     msg1Outif( msglev_t prior, const char * param, const char * text,
*                int useformat, va_list args, int * status );

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
*     useformat = Logical (Given)
*        If true, "text" is processed using sprintf to expand format
*        specifiers using the supplied "args".
*     args = va_list (Given)
*        Variadic arguments for sprintf processing.
*     status = int * (Given and Returned)
*        The global status.

*  Notes:
*     If "format" is true, printf-style formatting will be applied using
*     the supplied va_list argument. Formatting is applied after token
*     replacement. Tokens containing "%" will not be treated as format
*     specifiers. Keyword associations will be disabled since they also
*     use "%".
*
*     Using printf formatting can be useful for simplifying code that
*     does not require deferred token handling. See also msgFmt() for
*     formatting tokens.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
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
*        Internal copy of msgOutif. Supports sprintf processing.
*     09-JAN-2009 (TIMJ):
*        Extend range of filters.
*     12-JAN-2009 (TIMJ):
*        New API for msg1Print
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "star/util.h"
#include "sae_par.h"
#include "msg_par.h"
#include "msg_err.h"
#include "mers1.h"
#include "ems.h"
#include "merswrap.h"

#include <stdio.h>
#include <stdarg.h>

void msg1Outif( msglev_t prior, const char * param, const char * text,
               int useformat, va_list args, int * status) {

  char msgstr[MSG__SZMSG+1];    /* Message string */
  char fstr[MSG__SZMSG+2];      /* temp string for sprintf - bigger than
                                   msgstr to allow us to trap truncation
                                   after formatting. */

  /*  Check inherited global status. */
  if (*status != SAI__OK) {

    /*     Call MSG1_KTOK to annul any defined message tokens.*/
    msg1Ktok();
  } else {

    /*     The given status is OK, so check that the given value of the
     *     output filter is allowed. NONE and ALL are not allowed as input
     *     messaging levels. */
    if (prior < MSG__QUIET || prior >= MSG__ALL) {

      /*        The given message filtering level is out of range: set the
       *        returned status and report an error. (Mark and subsequently
       *        release an error context to prevent token name clashes.) */
      emsMark();
      *status = MSG__INVIF;
      emsSeti( "PRIOR", prior );
      emsRep( "MSG_OUTIF_INVIF",
              "MSG_OUTIF: Invalid message filtering value:  ^PRIOR",
              status );
      emsRlse();

      /*        Annul the message token table. */
      msg1Ktok();

    } else {

      /*        Conditionally output the given message. NONE will always
                fail and ALL will always pass. */
      if (prior <= msg1Gtinf() ) {

        /*           Form the output message string. */
        msg1Form( param, text, !msg1Gtstm(), useformat, sizeof(msgstr), msgstr,
                  status );

        /*           Handle sprintf processing. */
        if (useformat) {
          /* format, and then copy back */
          vsnprintf(fstr, sizeof(fstr), msgstr, args );
          star_strellcpy( msgstr, fstr, sizeof(msgstr) );
        }

        /*           Deliver the message string. */
        msg1Print( msgstr, "", status );
      } else {

        /*           Call MSG1_KTOK to annul any defined message tokens, even
         *           though the message was not output. */
        msg1Ktok();
      }
    }
  }
}
