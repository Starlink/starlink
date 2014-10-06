/*
*+
*  Name:
*     smf_jsaproj_tostr

*  Purpose:
*     Convert JSA projection type to a string.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     const char *smf_jsaproj_tostr( smf_jsaproj_t proj )

*  Arguments:
*     proj = smf_jsaproj_t (Given)
*        The JSA projection identifier.

*  Returned Value:
*     The corresponding string - one of "HPX", "HPX12", "XPHN" or "XPHS".

*  Description:
*     This function returns a pointer to a string describing a supplied
*     JSA projection type.

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
#include "ast.h"
#include "mers.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/jsatiles.h"

const char *smf_jsaproj_tostr( smf_jsaproj_t proj ){

/* Local Variables: */
   const char *result;

/* Do it. */
   if( proj == SMF__JSA_HPX ) {
      result = "HPX";

   } else if( proj == SMF__JSA_HPX12 ) {
      result = "HPX12";

   } else if( proj == SMF__JSA_XPHN ) {
      result = "XPHN";

   } else if( proj == SMF__JSA_XPHS ) {
      result = "XPHS";

   } else {
      result = "Unknown JSA projection";
   }

   return result;
}



