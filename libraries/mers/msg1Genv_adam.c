
/*
*+
*  Name:
*    msg1Genv

*  Purpose:
*     Get the message text for a specified message parameter from the
*     interface module.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     result = msg1Genv( const char * param, char *msgstr, size_t msglen );

*  Description:
*     This routine makes an enquiry of the parameter system to get the
*     message text from the interface module for the specified message
*     parameter.

*  Arguments:
*     param = const char * (Given)
*        The message parameter name.
*     msgstr = char * (Returned)
*        The message text.
*     msglen = size_t (Given)
*        Allocated size of "msgstr".

*  Return Value:
*     Returns true if the parameter was found. Otherwise false.

*  Implementation Notes:
*     -  This function is only for use in the ADAM version of msg.
*     -  This function makes calls to subParFindpar and subParGet0c.

*  Algorithm:
*     -  If the parameter name is not null, then get the message text from
*     parameter system.

*  Copyright:
*     Copyright (C) 1982-1984, 1986, 1989-1991 Science & Engineering
*     Research Council. Copyright (C) 2008 Science and Technology Facilities
*     Council.
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
*     SLW: Sid Wright  (UCL)
*     BDK: Dennis Kelly (ROE)
*     PCTR: P.C.T. Rees (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1982 (JRG):
*        Original version.
*     17-AUG-1983 (SLW):
*        New parameter system interface.
*     13-NOV-1984 (BDK):
*        ADAM parameter system.
*     16-DEC-1986 (BDK):
*        Call SUBPAR not PAR.
*     18-SEP-1989 (PCTR):
*        Converted to new prologue and layout.
*     13-MAR-1990 (PCTR):
*        Changed function name.
*     22-OCT-1991 (PCTR):
*        Added EMS_MARK and EMS_RLSE to annul any error messages
*        from SUBPAR on error.
*     05-SEP-2008 (TIMJ):
*        Rewrite in C.
*     20-NOV-2019 (DSB):
*        - Change || to && when testing if a valid parameter name has
*        been supplied.
*        - Skip leading spaces in parameter name. This means that names
*        that are just whitespace are ignored.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "sae_par.h"
#include "star/subpar.h"
#include "ems.h"

#include "mers1.h"

#include <string.h>
#include <ctype.h>

int msg1Genv(  const char * param, char *msgstr, size_t msglen ) {

  size_t pd;   /* Parameter system ID */
  int status = SAI__OK;   /* Local status */
  int retval = 0;

  /*  Initialise the returned string. */
  msgstr[0] = '\0';

  /*  Mark a new error reporting context. */
  emsMark();

  /* Skip any leading spaces in the parameter name */
  while( isspace( *param ) ) param++;

  /*  Check that PARAM actually contains a parameter name. */
  if (param && strlen(param) > 0) {

    /* PARAM does contain a parameter name, so attempt to get the
     * message text from the parameter system. */
    subParFindpar( param, &pd, &status );
    subParGet0c( pd, msgstr, msglen, &status );

    if (status == SAI__OK) {
      retval = 1;
    } else {
      /* Annul the error context and abort. */
      emsAnnul( &status );
    }
  }

  /*  Release the error reporting context. */
  emsRlse();

  return retval;
}
