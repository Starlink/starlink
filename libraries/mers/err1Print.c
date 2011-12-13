/*
*+
*  Name:
*     err1Print

*  Purpose:
*     Split the text of an error message up for delivery to the user.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     err1Print( const char * text, const char * text,int * errbel,
*                int * status);

*  Description:
*     The text of the given error message is split into lines of length
*     ERR__SZOUT. Each line in than delivered to the user by a call to
*     err1Prerr

*  Arguments:
*     text = const char * (Given)
*        Text to be output.
*     prefix = const char * (Given)
*        This is the text to be attached to the first line of output.
*        Any continuation lines will use the first character from this
*        string. Do not prepend this to "text".
*     errbel = int * (Given & Returned)
*        If true, an attempt will be made to ring a terminal bell
*        in addition to flushing the error messages. Will be set to
*        false if the bell was rung.
*     status = int * (Returned)
*        The global status. Returned if status is set bad to this routine,
*        else it is not touched.

*  Copyright:
*     Copyright (C) 2008, 2009 Science and Technology Facilities Council.
*     Copyright (C) 1983, 1985, 1987, 1989-1994 Science & Engineering
*     Research Council. Copyright (C) 1997, 1999, 2001 Central
*     Laboratory of the Research Councils. All Rights Reserved.

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
*     AJC: A.J.Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     10-MAR-1983 (SLW):
*        Modified to use SAI I/O.
*     10-APR-1985 (BDK):
*        ADAM version.
*     4-NOV-1987 (BDK):
*        Use ERR_SZMSG.
*     7-AUG-1989 (RFWS):
*        Converted to new prologue layout and added comments.
*     12-SEP-1989 (PCTR):
*        Completed code tidy-up.
*     9-APR-1990 (PCTR):
*        Removed unreferenced include file.
*     8-AUG-1991 (PCTR):
*        Re-written to wrap error messages and call ERR1_PRERR.
*     19-AUG-1992 (PCTR):
*        Changed argument list in EMS1_PFORM call.
*     3-SEP-1992 (PCTR):
*        Remove the LSTAT check in the delivery loop.
*     4-OCT-1993 (PCTR):
*        Added bell character behaviour.
*     12-MAY-1994 (AJC):
*        EMS1_PFORM renamed EMS1_RFORM
*     15-AUG-1997 (AJC):
*        Use NEQV to compare ERRBEL
*     21-JUL-1999 (AJC):
*        Add tuning parameters ERRWSZ and ERRSTM
*     26-FEB-2001 (AJC):
*        Change EMS1_RFORM to MSG1_RFORM
*     28-JUL-2008 (TIMJ):
*        Add extra argument so that we do not need ERRBEL common block.
*     30-JUL-2008 (TIMJ):
*        Rewrite in C
*     12-JAN-2009 (TIMJ):
*        Fix counting when no bell character.
*        Add prefix to API so that we can trap for cases where we would
*        normally get an output line with just the prefix (since ems1Rform
*        will return '!  ' for an input string of '!  very_long_word').
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "sae_par.h"
#include "err_par.h"
#include "merswrap.h"
#include "ems.h"
#include "mers1.h"
#include "star/util.h"

#include <string.h>

void err1Print( const char * text, const char * prefix,
                int *errbel, int *status) {


  /*  Local Variables: */
  const char fixstr[] = "     "; /* Fixed part of continuation string */
  char constr[32];               /* Continuation tab string */
  int contab;                    /* Length of continuation string */
  int iposn;                     /* Character position for text */
  int leng;                      /* String length */
  int lstat = SAI__OK;           /* Local status */
  int lstart;                    /* Start index of line */
  int oplen;                     /* Output string length */
  char line[ERR__SZMSG+1];       /* Output line of text */


  /* Tuning parameters */
  int errwsz;
  int errstm;

  /* Get tuning parameters */
  err1Gtglbl( &errwsz, &errstm, NULL );

  /*  Get length of text to send. */
  leng = strlen( text );

  /*  Check whether a bell character is to be delivered and initialise the
   *  output line. Include the prefix. */
  if (*errbel) {
    star_strlcpy(line, "\a", sizeof(line) );
    *errbel = 0;
  } else {
    line[0] = '\0';
  }
  star_strlcat( line, prefix, sizeof(line) );
  lstart = strlen( line );

  /* precalculate continuation string */
  constr[0] = prefix[0];
  constr[1] = '\0';
  star_strlcat( constr, fixstr, sizeof(constr) );
  contab = strlen( constr );

  /*  If the text is not blank, then continue. */
  if (leng > 0) {

    /*     Loop to split the line of text into sensible lengths for output,
     *     then write them to the error stream. First, initialise the
     *     character pointer. */
    iposn = 0;

    /*     Now output the message in a way determined by the relevant tuning
     *     parameters. */
    if (errstm) {
      if (lstart > 0) err1Prerr( line, &lstat);
      err1Prerr( text, &lstat );
    } else {
      /*     Call MSG1_RFORM to load the first output line and deliver
       *     the result. */
      ems1Rform( text, errwsz - lstart, &iposn, &(line[lstart]), &oplen );
      err1Prerr( line, &lstat );

      /*     Loop to continue the remainder of the message.*/
      while (iposn != 0) {

        /*        Re-initialise the output line for a continuation. */
        star_strlcpy( line, constr, sizeof(line) );

        /*        Call MSG1_RFORM to load the continuation line and write the
         *        result. */
        ems1Rform( text, errwsz - contab, &iposn, &(line[contab]), &oplen);
        err1Prerr( line, &lstat );
      }
    }
  } else {

    /*     If there is no text, then send a blank message. */
    err1Prerr( prefix, &lstat );
  }

  /*  Check I/O status and set STATUS if necessary. */
  if (lstat != SAI__OK) {
    *status = lstat;
  }
}
