/*
*+
*  Name:
*     msgOutif

*  Purpose:
*     Conditionally deliver the text of a message to the user.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     msgOutif( int prior, const char * param, const char * text,
*               int * status );

*  Description:
*     Depending upon the given value of the given message priority and 
*     the message filtering level set using msgIfset, the message
*     text is either expanded and output to the user or discarded.
*     The values of any existing message tokens are always annulled by 
*     a call to msgOutif. If an output error occurs, an error is 
*     reported and the status argument returned set to MSG__OPTER.

*  Arguments:
*     prior = int (Given)
*        Message output filter. This may be one of three values:
*        
*           -  MSG__QUIET = always output the message, regardless of the
*           output filter setting;
*           -  MSG__NORM = output the message if the current output
*           filter is set to either MSG__NORM or MSG__VERB or MSG__DEBUG;
*           -  MSG__VERB = output the message only if the current
*           output filter is set to MSG__VERB or MSG__DEBUG;
*           -  MSG__DEBUG = out the message only if the current
*           output filter is set to MSG__DEBUG.
*
*        Here, the collating sequence:
*
*           MSG__QUIET < MSG__NORM < MSG__VERB < MSG__DEBUG
*           
*        may be assumed. Any other value will result in an error report
*        and the status being returned set to MSG__INVIF: no further 
*        action will be taken.
*     param = const char * (Given)
*        The message name.
*     text = const char * (Given)
*        The message text.
*     status = int * (Given and Returned)
*        The global status.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

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
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "sae_par.h"
#include "msg_par.h"
#include "msg_err.h"
#include "mers1.h"
#include "ems.h"
#include "merswrap.h"

void msgOutif( int prior, const char * param, const char * text, int * status) {

  char msgstr[MSG__SZMSG];    /* Message string */

  /*  Check inherited global status. */
  if (*status != SAI__OK) {

    /*     Call MSG1_KTOK to annul any defined message tokens.*/
    msg1Ktok();
  } else {

    /*     The given status is OK, so check that the given value of the
     *     output filter is allowed. */
    if (prior < MSG__QUIET || prior > MSG__DEBUG) {

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

      /*        Conditionally output the given message.*/
      if (prior <= msg1Gtinf() ) {

        /*           Form the output message string. */
        msg1Form( param, text, !msg1Gtstm(), sizeof(msgstr), msgstr,
                  status );

        /*           Deliver the message string. */
        msg1Print( msgstr, status );
      } else {

        /*           Call MSG1_KTOK to annul any defined message tokens, even
         *           though the message was not output. */
        msg1Ktok();
      }
    }
  }
}
