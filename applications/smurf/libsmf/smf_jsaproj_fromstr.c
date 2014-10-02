/*
*+
*  Name:
*     smf_jsaproj_fromstr

*  Purpose:
*     Convert a string to a JSA projection type.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_jsaproj_t smf_jsaproj_fromstr( const char *str, int rep, int *status )

*  Arguments:
*     str = const char * (Given)
*        The string - should be one of "HPX", "HPX12", "XPHN" or "XPHS"
*        (case insensitive).
*     rep = int (Given)
*        If none-zero, report an error if the supplied string is not
*        recognised.
*     status = int * (Given)
*        Pointer to the inherited status variable.

*  Returned Value:
*     The JSA projection identifier. SMF__JSA_NULL is returned if the
*     supplied string is unknown, or if an error has already occurred.

*  Description:
*     This function returns an identifier for the type of JSA projection
*     specified by the supplied string.

*  Authors:
*     DSB: David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     2-OCT-2014 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2014 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ast.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/jsatiles.h"

smf_jsaproj_t smf_jsaproj_fromstr( const char *str, int rep, int *status ){

/* Local Variables: */
   smf_jsaproj_t result;

/* Initialise the returned value. */
   result = SMF__JSA_NULL;

/* Check inherited status */
   if( *status != SAI__OK ) return result;

/* Do it. */
   if( astChrMatch( str, "HPX" ) ){
      result = SMF__JSA_HPX;

   } else if( astChrMatch( str, "HPX12" ) ){
      result = SMF__JSA_HPX12;

   } else if( astChrMatch( str, "XPHN" ) ){
      result = SMF__JSA_XPHN;

   } else if( astChrMatch( str, "XPHS" ) ){
      result = SMF__JSA_XPHS;

   } else if( rep ){
      *status = SAI__ERROR;
      errRepf( " ", "Bad JSA projection type '%s' supplied.", status, str );
   }

   return result;
}



