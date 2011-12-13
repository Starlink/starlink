/*
*+
*  Name:
*     msg1Gkey

*  Purpose:
*     Get the keyword for the specified parameter.

*  Language:
*    Starlink ANSI C

*  Invocation:
*    result = msg1Gkey( const char * param, char refstr[],
*                        size_t reflen );

*  Description:
*     This routine makes an enquiry of the parameter system to
*     get the keyword associated with the specified message parameter.

*  Arguments:
*     param = const char * (Given)
*        The parameter name.
*     keystr = char * (Returned)
*        The keyword.
*     reflen = size_t (Returned)
*        The length of the keyword buffer.

*  Return Value:
*     int = boolean indicating whether the parameter was found.

*  Implementation Notes:
*     -  This function is only for use in the ADAM version of msg.
*     -  This function makes calls to subParFindpar and subParGetkey.

*  Algorithm:
*     -  Ask the parameter system for the keyword.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1982-1984, 1989-1991 Science & Engineering Research Council.
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
*     SLW: Sid Wright (UCL)
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
*     18-SEP-1989 (PCTR):
*        Converted to new prologue and layout.
*     13-MAR-1990 (PCTR):
*        Changed function name.
*     22-OCT-1991 (PCTR):
*        Added EMS_MARK and EMS_RLSE to annul any error messages from
*        SUBPAR on error.
*     10-SEP-2008 (TIMJ):
*        Rewrite in C
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "sae_par.h"
#include "ems.h"
#include "star/subpar.h"
#include "mers1.h"

int msg1Gkey( const char * param, char * keystr, size_t keylen ) {

  int status = SAI__OK;                 /* Local status */
  size_t namecode;                      /* Parameter namecode */
  int retval;

  /*  Initialise the returned value of MSG1_GKEY.*/
  retval = 0;

  /*  Initialise the returned string. */
  keystr[0] = '\0';

  /*  Set new error reporting context */
  emsMark();

  /*  Attempt to get the keyword associated with PARAM from the parameter
   *  system. */
  subParFindpar( param, &namecode, &status );
  subParGetkey( namecode, keystr, keylen, &status );

  if (status == SAI__OK) {
    /*     Set returned arguments for normal successful completion. */
    retval = 1;
  } else {
    /*     Annul the error context and abort. */
    emsAnnul( &status );
  }

  /*  Release the error reporting context. */
  emsRlse();

  return retval;
}
