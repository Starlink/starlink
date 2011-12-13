/*
*+
*  Name:
*     msg1Print

*  Purpose:
*     Deliver the text of a message to the user.

*  Language:
*    Starlink ANSI C

*  Invocation:
*     msg1Print( const char * text, int * status );

*  Description:
*     This uses the parameter system to deliver the message text to the
*     user. Trailing blanks are removed. If the delivery fails, the
*     message is given as an error report and a subsequent explanatory
*     error report made. This should ensure that the message is seen by
*     the user.

*  Arguments:
*     text = const char * (Given)
*        The message text.
*     prefix = const char * (Given)
*        Text to be prepended to the first line of output. Subsequent lines
*        will be prepended with the first character of the prefix and some
*        spaces for padding if the prefix is non-zero length. If text is
*        zero length no prefix will be used.
*     status = int * (Given and Returned)
*        The global status.

*  Implementation Notes:
*     -  This subroutine is shared by both ADAM and standalone implementation.
*        System specific message delivery is done via msg1Prtln.

*  Copyright:
*     Copyright (C) 2008, 2009 Science and Technology Facilities Council.
*     Copyright (C) 1982-1984, 1987, 1989-1992, 1994 Science &
*     Engineering Research Council. Copyright (C) 1999, 2001, 2004 Central
*     Laboratory of the Research Councils.
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
*     JRG: Jack Giddings (UCL)
*     SLW: Sid  Wright (UCL)
*     BDK: Dennis Kelly (ROE)
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A.J. Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1982 (JRG):
*        Original version.
*     10-Mar-1983 (SLW):
*        Modified to use SAE I/O.
*     13-NOV-1984 (BDK):
*        ADAM version.
*     3-NOV-1987 (BDK):
*        Remove trap on message length.
*     19-SEP-1989 (PCTR):
*        Converted to new prologue and layout.
*     13-MAR-1990 (PCTR):
*        Changed the name of the subroutine and included error
*        reporting.
*        and EMS_ calls.
*     26-JUN-1991 (PCTR):
*        Added mark and release to prevent message tokens being annulled
*        on error.
*     8-AUG-1991 (PCTR):
*        Added use of EMS1_PFORM to reformat the output messages.
*     19-AUG-1992 (PCTR):
*        Changed argument list in EMS1_PFORM call.
*     26-AUG-1992 (PCTR):
*        Changed call to SUBPAR_WRITE to SUBPAR_WRMSG.
*     3-SEP-1992 (PCTR):
*        Deliver the message text via the error reporting system on
*        failure.
*     12-MAY-1994 (AJC):
*        EMS1_PFORM renamed EMS1_RFORM
*     14-DEC-1994 (AJC):
*        Avoid concatenation of argument text
*     21-JUL-1999 (AJC):
*        Add tuning parameters MSGWSZ and MSGSTM
*     26-FEB-2001 (AJC):
*        Use MSG1_RFORM nor EMS1_RFORM
*     1-JUL-2004 (DSB):
*        Use MSG1_GT... functions to get the values from the MSG_CMN
*        common blocks rather than directly accessing the common blocks
*        (which are initialised in a different shared library).
*     25-JUL-2008 (TIMJ):
*        The ADAM and standalone versions were identical in all ways
*        except for the call to subpar or write (and the corresponding
*        check of inherited or I/O status). Move the system specific
*        code to a new routine to aid code reuse.
*     10-SEP-2008 (TIMJ):
*        Rewrite in C
*     12-JAN-2009 (TIMJ):
*        Add prefix option. See err1Print and err1Flush.
*     9-FEB-2009 (TIMJ):
*        Error reporting used wrong message token
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "sae_par.h"
#include "msg_err.h"
#include "msg_sys.h"
#include "msg_par.h"
#include "mers1.h"
#include "ems.h"
#include "star/util.h"

#include <string.h>

void msg1Print( const char * text, const char * prefix, int * status ) {


  int iposn;                     /* Character position for text */
  int lstart;                    /* position to start formatting string */
  int leng;                      /* String length */
  int istat = SAI__OK;           /* Local status */
  int oplen;                     /* Output string length */
  char line[MSG__SZMSG+1];       /* Output line of text */
  const char fixstr[] = "     "; /* Fixed part of constr */
  char constr[32];               /* continuation tab string if prefix set */
  int contab = 0;                /* length of constr */

  /*  Check the inherited global status.  */
  if (*status != SAI__OK) return;

  /*  Create a new error context */
  emsMark();

  /*  Find the filled length of the message text string and write it
   *  to the standard output stream. */
  leng = strlen( text );

  /*  If the text is not blank, then continue. */
  if (leng > 0) {

    if (msg1Gtstm()) {

      /*     Output with no messing */
      star_strlcpy( line, prefix, sizeof(line) );
      star_strlcat( line, text, sizeof(line) );
      msg1Prtln( line, &istat );
    } else {

      /* Precalculate continuation string */
      constr[0] = prefix[0];
      if (strlen(prefix) > 0) {
        constr[1] = '\0';
        star_strlcat( constr, fixstr, sizeof(constr) );
        contab = strlen(constr);
      }

      /*     Call MSG1_RFORM to load the output line and deliver it. */
      iposn = 0;

      /*     Handle prefix */
      star_strlcpy( line, prefix, sizeof(line) );
      lstart = strlen( line );

      /*     Loop to deliver the message in line-sized chunks. */
      ems1Rform( text, msg1Gtwsz() - lstart, &iposn, &(line[lstart]), &oplen );
      msg1Prtln( line, &istat );

      while ( iposn != 0 && istat == SAI__OK) {
        star_strlcpy( line, constr, sizeof(line) );
        ems1Rform( text, msg1Gtwsz() - contab, &iposn, &(line[contab]), &oplen );
        msg1Prtln( line, &istat );
      }
    }
  } else {

    /*     If there is no text, then send a blank message. */
    msg1Prtln( "", &istat );
  }

  /*  If the message cannot be delivered, then annul the current error
   *  context and report the error. */
  if (istat != SAI__OK) {
    *status = istat;
    emsAnnul( &istat );
    emsMark();
    emsSetc ( "OPLINE", line );
    emsRep( "MSG_PRINT_MESS",
            "msg1Print: ^OPLINE", status );
    *status = MSG__OPTER;
    emsRep( "MSG_PRINT_OPTER",
            "Error encountered during message output", status );
    emsRlse();
  }

  /*  Release the current error context. */
  emsRlse();
}
