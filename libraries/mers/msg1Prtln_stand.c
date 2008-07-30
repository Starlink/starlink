/*
*+
*  Name:
*     msg1Prtln

*  Purpose:
*     Send a line for printing on the relevant device

*  Language:
*     Starlink ANSI C

*  Invocation:
*     msg1Prtln(const char * text, int * status);

*  Description:
*     Clear all the message tokens at the current context level.

*  Arguments:
*     text = const char * (Given)
*       Text to deliver to output device
*     status = int * (Given and Returned)
*       Global status.

*  Notes:
*     - The standalone implementation uses printf

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25-JUL-2008 (TIMJ):
*        Original version
*     29-JUL-2008 (TIMJ):
*        More robust error handling.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "mers1.h"
#include "star/subpar.h"
#include "sae_par.h"
#include "msg_err.h"
#include "ems.h"

#include <string.h>
#include <stdio.h>
#include <errno.h>

void msg1Prtln( const char * text, int * status ) {
  int err;                /* return value from printf */
  size_t len = 0;         /* Length of input text */

  if (*status != SAI__OK) return;

  /* how many characters do we expect to deliver (including newline) */
  len = strlen( text ) + 1;

  /* Note that we must add the newline */
  err = printf( "%s\n", text );

  if (err < 0) {
    *status = MSG__OPTER;
    emsMark();
    emsSyser( "ERR", errno );
    emsRep( "MSG_PRINT_MESS",
	    "msg1Prtln: Error printing message to stdout: ^ERR", status );
    emsRlse();
  } else if ((size_t)err != len) {
    emsMark();
    *status = MSG__OPTER;
    emsSeti( "NEX", len );
    emsSeti( "NGOT", err );
    emsRep("MSG_PRINT_MESS",
	    "msg1Prtln: Error printing message to stdout. Printed ^NGOT"
	    " characters but expected to print ^NEX", status );
    emsRlse();
  }

}


/* Now provide the Fortran interface until it is no longer required */

#include "f77.h"
F77_SUBROUTINE(msg1_prtln)( CHARACTER(TEXT), INTEGER(STATUS) TRAIL(TEXT));

F77_SUBROUTINE(msg1_prtln)( CHARACTER(TEXT), INTEGER(STATUS) TRAIL(TEXT)) {
  int status;
  char *text;
  GENPTR_CHARACTER(TEXT);

  F77_IMPORT_INTEGER( *STATUS, status );
  text = starMallocAtomic( TEXT_length + 1 );
  F77_IMPORT_CHARACTER( TEXT, TEXT_length, text );

  msg1Prtln( text, &status );
  F77_EXPORT_INTEGER( status, *STATUS );
}
