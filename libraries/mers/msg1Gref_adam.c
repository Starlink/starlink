/*
*+
*  Name:
*     msg1Gref

*  Purpose:
*     Get the reference for the specified parameter.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     result = msg1Gref( const char * param, char refstr[],
*                        size_t reflen );

*  Description:
*     This routine makes an enquiry of the parameter system to
*     get the absolute object, device or file name (i.e. reference)
*     associated with the specified parameter.

*  Arguments:
*     param = const char * (Given)
*        The parameter name.
*     refstr = char * (Returned)
*        The reference. Buffer size of "reflen"
*     reflen = size_t (Given)
*        The length of the reference buffer .

*  Return Value:
*     int = boolean indicating whether the parameter was found.

*  Implementation Notes:
*     -  This function is only for use in the ADAM version of msg
*     -  This function makes calls to subParFindpar and subParGref.

*  Algorithm:
*     subParGref attempts to get a name via a valid locator from the parameter
*     system.
*     -  If this fails and the parameter type indicates a name, it gets
*     the name from the parameter table.

*  Copyright:
*     Copyright (C) 2002 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     AJC: Alan Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-DEC-2002 (AJC):
*        Original version.
*     9-SEP-2008 (TIMJ):
*        Rewrite in C.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "sae_par.h"
#include "ems.h"
#include "mers1.h"

#include "star/subpar.h"

int msg1Gref( const char * param, char *refstr, size_t reflen ) {

  int status = SAI__OK;                 /* Local status */
  size_t namecode;                      /* Parameter namecode */
  int retval;

  /*  Initialise the returned value of MSG1_GREF.*/
  retval = 0;

  /*  Initialise the returned string. */
  refstr[0] = '\0';

  /*  Set new error reporting context */
  emsMark();

  /*  Get the parameteer namecode and then the reference */
  subParFindpar( param, &namecode, &status );
  retval = subParGref( namecode, refstr, reflen );

  /*  Annul any error reports and release the error context */
  if (status != SAI__OK) emsAnnul( &status );
  emsRlse();

  return retval;
}
