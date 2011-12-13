/*
*+
*  Name:
*     err1Prerr

*  Purpose:
*     Deliver the text of an error message to the user.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     err1Prerr( const char * text, int * status );

*  Description:
*     This uses subParWrerr to send a message to the user.
*     Trailing blanks are removed.
*     If subParrWrerr fails, an attempt is made to write the message
*     on STDERR. If that fails, or if STDERR was not a TTY, attempt to
*     output on STDOUT.
*     That is, we make every effort to ensure that the user sees the message.

*  Arguments:
*     text = const char * (Given)
*        Text to be output.
*     status = int (Given and Returned)
*        The global status.

*  Implementation Notes:
*     -  This is the UNIX ADAM version of ERR1_PRERR.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     AJC: A.J.Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-JUN-1991 (PCTR):
*        Original version.
*     8-AUG-1991 (PCTR):
*        EMS Vn. 1.3 changes.
*     19-SEP-1994 (AJC):
*        Change to using SUBPAR_WRERR.
*        Avoid outputting to STDOUT if both it and STDERR are TTY.
*     24-JUL-2008 (TIMJ):
*        Rewrite in C
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "star/subpar.h"
#include "mers1.h"

#include "sae_par.h"
#include "err_err.h"
#include "err_par.h"

#include <unistd.h>
#include <stdio.h>

void err1Prerr( const char * text, int * status ) {

  int errstat = 0;          /* status from printf */
  int lstat = SAI__OK;      /* Local status */


  /* Fortran version stripped trailing space. We assume that the
     fortran interface to this routine has already done that */

  /*  Attempt to deliver the message via the user interface. */
  subParWrerr( text, &lstat );

  /*  Check the returned status and attempt to report the message on
   *  STDERR. If that fails or if STDERR was not a terminal try STDOUT. */
  if (lstat != SAI__OK) {

    /* Set the returned status */
    *status = ERR__OPTER;

    /* Write the message to STDERR */
    errstat = fprintf( stderr, "%s", text );
    fflush(stderr);

    /* If that failed or STDERR was not a TTY, try STDOUT */
    if (errstat < 0 || !isatty( STDERR_FILENO ) ) {

      /* Write the message to STDOUT */
      printf( "%s", text );
      fflush(stdout);
    }
  }
}

