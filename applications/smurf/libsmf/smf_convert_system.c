/*
*+
*  Name:
*     smf_convert_system

*  Purpose:
*     Convert a JCMT label for a celestial coordinate system into the
*     corresponding AST SkyFrame System value.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     result = smf_convert_system( const char *label );

*  Arguments:
*     label = const char * (Given)
*        The JCMT label for the coordinate system.
*     status = int * (Given and Returned)
*        The inherited status value.

*  Returned Value:
*     A point to a static string holding the equivalent AST value, or
*     "" if there is no equivalent AST value.

*  Description:
*     This function converts a JCMT label for a celestial coordinate system 
*     into the corresponding AST SkyFrame System value. It reports an error
*     if the system is not supported by AST.

*  Authors:
*     DSB: David S. Berry (JAC, UCLan)

*  History:
*     25-SEP-2006 (DSB):
*        Original version.

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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

#include <string.h>

/* Starlink includes */
#include "mers.h"
#include "sae_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_convert_system"

const char *smf_convert_system( const char *label, int *status ){

/* Local Variables */
   const char *result = "";   /* Returned pointer */

/* Check inherited status */
   if (*status != SAI__OK) return result;

/* Compare the supplied labelwith each known type. */
   if( !strcmp( label, "AZEL" ) ) {
      result = "AZEL";

   } else if( !strcmp( label, "APP" ) ) {
      result = "GAPPT";

   } else if( !strcmp( label, "GAL" ) ) {
      result = "GALACTIC";

   } else if( !strcmp( label, "ICRS" ) ||
              !strcmp( label, "ICRF" ) ) {
      result = "ICRS";

   } else if( !strcmp( label, "B1950" ) ) {
      result = "FK4";

   } else if( !strcmp( label, "J2000" ) ) {
      result = "FK5";

   } else {
      *status = SAI__ERROR;
      msgSetc( "LAB", label );
      errRep( FUNC_NAME, "The JCMT coordinate system \"^LAB\" does not "
              "have an equivalent AST System value.", status );
   }

   return result;
}
