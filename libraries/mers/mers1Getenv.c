/*
*+
*  Name:
*     mers1Getenv

*  Purpose:
*     Retrieve an integer value from an environment variable

*  Language:
*     Starlink ANSI C

*  Description:
*     Given a mode and tuning parameter, see if a corresponding environment
*     variable exists and if it does read the value and convert it to an
*     integer. Error status is set if the value can not be converted to
*     an integer. Returns 0 if the environment variable did not exist.

*  Invocation:
*     value = mers1Getenv( int usemsg, const char * param,
*                          int * status );

*  Arguments:
*     usemsg = int (Given)
*        True if "MSG" is to be used, false if "ERR" is to be used.
*     param = const char * (Given)
*        The tuning parameter to be set (case insensitive).
*     status = int * (Given and Returned)
*        The global status. Set to bad if the environment variable does
*        exist but could not be converted to an integer.

*  Return Value:
*     Returns the value of the environment variable.
*     Returns -1 if the environment variable did not exist or could not
*     be converted to an integer.

*  Copyright:
*     Copyright (C) 2008-2009 Science and Technology Facilities Council.
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
*     12-SEP-2008 (TIMJ):
*        Original version.
*     01-JUL-2009 (TIMJ):
*        Do not rely on errno when determining whether strod worked.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "star/util.h"
#include "mers1.h"
#include "sae_par.h"
#include "ems.h"
#include "err_err.h"
#include "msg_err.h"

#include <stdlib.h>
#include <ctype.h>

int mers1Getenv( int usemsg, const char * param, int *status ) {

  char envvar[32];      /* environment variable name */
  char *c;              /* pointer to character in name */
  char *val;            /* environ value */
  int retval = -1;
  long result = 0;

  if (*status != SAI__OK) return retval;

  /* copy prefix and param into the buffer */
  if (usemsg) {
    star_strlcpy( envvar, "MSG_", sizeof(envvar) );
  } else {
    star_strlcpy( envvar, "ERR_", sizeof(envvar) );
  }
  star_strlcat( envvar, param, sizeof(envvar) );

  /* upper case it */
  c = envvar;
  while ( *c ) {
    *c = toupper(*c);
    c++;
  }

  /* Query the environment */
  val = getenv( envvar );

  if (val) {
    /* Convert to an integer */
    char *endptr = NULL;
    result = strtol( val, &endptr, 10 );

    if (result == 0  && endptr == val) {
      if (usemsg) {
        *status = MSG__BDENV;
        emsSetc( "SYS", "msgTune");
      } else {
        *status = ERR__BDENV;
        emsSetc( "SYS", "errTune");
      }
      emsSetc( "EV", envvar );
      emsSetc( "VAL", val );

      emsRep( "MERS_TUNE_BDENV",
              "^SYS: Failed to convert environment variable "
              "^EV (^VAL) to integer", status );
    } else {
      retval = result;
    }

  }

  return retval;
}
