
/*
*+
*  Name:
*     err1Flush

*  Purpose:
*     Flush the current error context (internal version).

*  Language:
*     Starlink ANSI C

*  Invocation:
*     err1Flush( int * errbel, int * status );

*  Description:
*     Ensure that all pending error messages in the current error 
*     context have been output to the user. On successful completion, the 
*     error context is annulled and the status argument reset to SAI__OK;
*     if an error occurs during output of the error messages, the 
*     error context is not anulled and the status argument is returned 
*     set to ERR__OPTER. The first argument controls whether a bell is
*     requested in addition to the error messages.

*  Arguments:
*     errbel = int * (Given & Returned)
*        If true, an attempt will be made to ring a terminal bell
*        in addition to flushing the erro messages. Will be set to
*        false if the bell was rung.
*     status = int * (Returned)
*        The global status: it is set to SAI__OK on return if the 
*        error message output is successful; if not, it is set to 
*        ERR__OPTER.

*  Algorithm:
*     -  Call emsEload to get the error message contents at the current
*     context.
*     -  Call err1Print to deliver the error message(s) to the user.
*     -  Call emsAnnul to annul the error table.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     JRG: Jack Giddings (UCL)
*     SLW: Sid Wright (UCL)
*     BDK: Dennis Kelly (ROE)
*     RFWS: R.F. Warren-Smith (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A.J. Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     17-ARP-1983 (SLW):
*        Added MARK and RELEASE mods.
*     14-NOV-1984 (BDK):
*        Change name of ERR_PRINT.
*     7-AUG-1989 (RFWS):
*        Converted to new prologue layout and added comments.
*     11-SEP-1989 (PCTR):
*        Completed code tidy-up.
*     1-MAR-1990 (PCTR):
*        Converted to use EMS_ calls where possible, and changed the 
*        behaviour of STATUS. 
*     9-APR-1990 (PCTR):
*        Removed unreferenced declarations and replaced DO WHILE construct
*        with ANSI Fortran 77 equivalent.
*     6-JUN-1991 (PCTR):
*        Attempt to print all the pending messages regardless of 
*        output errors.
*     26-JUN-1991 (PCTR):
*        Added mark and release to prevent message tokens being annulled
*        on error.
*      3-AUG-1994 (AJC):
*        Flush ERR_FLUSH error message also
*     15-AUG-1997 (AJC):
*        Use NEQV to compare ERRBEL
*      7-SEP-1999 (AJC):
*        Avoid repetition of messages in 'reveal' mode  
*     20-FEB-2001 (AJC):
*        EMS1_TUNE renamed EMS_TUNE
*        Use EMS_ELOAD not EMS1_ECOPY
*          (means have to add !'s here)
*        Allow for !'s in LINE length
*        Check for NOMSG at base level is not an error
*     26-JUL-2008 (TIMJ):
*        Move to internal shared function that can be called from ERR_FBEL
*        and ERR_FLUSH. This allows us to remove the need for the ERRBEL
*        COMMON block entry.
*     31-JUL-2008 (TIMJ):
*        Use common block accessor functions rather than COMMON itself
*     2-AUG-2008 (TIMJ):
*        Rewrite in C
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "err_par.h"
#include "err_err.h"
#include "mers1.h"
#include "ems.h"
#include "ems_err.h"
#include "ems_par.h"
#include "sae_par.h"
#include "star/one.h"

#define SZTABS 3

void err1Flush ( int * errbel, int * status ) {

  char line[ERR__SZMSG+1+SZTABS]; /* Output line buffer */
  char opstr[ERR__SZMSG+1];       /* Error message string */
  char param[ERR__SZPAR+1];       /* Message name */
  char tabs[SZTABS+1];            /* Line tab */

  int istat = SAI__ERROR;  /* Internal status */
  int level;            /* Error message context */
  int ostat = SAI__OK;  /* Status from ONE */
  int opcnt = 0;        /* Output line counter */
  int parlen = 0;       /* Length of parameter name */
  int oplen = 0;        /* Length of error message string */
  int lstat = SAI__OK;  /* Status returned from ems1Putc */
  int pstat = SAI__OK;  /* Status returned by err1Print */
  int nomsg;            /* If there is really no message */


  /*  Set initial TABS value - fatal programming error if this fails */
  one_strlcpy( tabs, "!! ", sizeof(tabs), &ostat ); 
  if (ostat != SAI__OK) {
    err1Print("!  errFlush: Error encountered during message output",
	      errbel, &pstat );
    *status = ERR__OPTER;
    return;
  }

  /*  Loop to get the error messages at the current context level. */
  while ( 1 ) {

    /* Get the last STATUS in case it's needed after EMS_ELOAD has annulled */
    emsStat( &lstat );

    /* Get the error message. */
    emsEload( param, &parlen, opstr, &oplen, &istat );

    /* Check for no messages to flush in the base context (i.e. because 
     * they have been delivered immediately to the user). 
     * In that case, just output BEL if required */
    nomsg = 0;
    if (istat == EMS__NOMSG) {
      emsLevel( &level );
      if (level == EMS__BASE && lstat != SAI__OK) nomsg = 1;
    }

    if (nomsg) {
      /* Check if any lines have been delivered. */
      if (opcnt == 0) {
	/*  Check whether a bell character is to be delivered: if so,
	 *  deliver it and reset the bell flag. */
	if (*errbel) {
	  err1Bell( &pstat );
	  *errbel = 0;
	}
	emsAnnul( &istat );
      }

      /* Repeat the loop. Next will be 'no more messages' but this is needed
       * to reset EMS_ELOAD. */
      continue;

    } else if (istat != SAI__OK) {

      /* Construct the output line */
      one_strlcpy( line, tabs, sizeof(line), &ostat );

      if (oplen > 0) {
	/* Do not need msg1Putc here because we are just appending
	   the message to the tab and there is no possibility of overrun
	   if ERR__SZMSG matches EMS__SZMSG */
	one_strlcat( line, opstr, sizeof(line), &ostat );
      }

      /* Continue to print messages regardless of output errors. */
      err1Print( line, errbel, &pstat );
      opcnt++;

      /* Only the first message of a flush has '!! ' prepended.
       *        Subsequent messages have '!  ' */
      one_strlcpy( tabs, "!  ", sizeof(tabs), &ostat );

      continue;
    } else {

      /* End of messages from emsEload */
	break;
    }
  }

  /*  End of the error messages in the current context: if no output error 
   *  has occurred, annul the current error context. Ensure 'reveal' is not
   *  operative in EMS to avoid duplicate message output. */
  if (pstat == SAI__OK) {
    int emsrvl;
    emsrvl = emsStune( "REVEAL", 0, &pstat );
    emsAnnul( status );
    (void)emsStune( "REVEAL", emsrvl, &pstat );

  } else {

    /* Report an error message if an output error has occurred.
     * Don't annul the context in this case.
     * Output it as the last of the current flush */
    err1Print("!  errFlush: Error encountered during message output",
	      errbel, &pstat );
    *status = ERR__OPTER;
  }

}
