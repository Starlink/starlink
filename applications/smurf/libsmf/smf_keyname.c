/*
*+
*  Name:
*     smf_keyname

*  Purpose:
*     Returns the full name for a config parameter, including optional
*     qualifier.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     const char *smf_keyname( AstKeyMap *keymap, const char *basename,
*                              const char *qualifier, char *buf, dim_t lbuf,
*                              int *status )

*  Arguments:
*     keymap = AstKeyMap * (Given)
*        The KeyMap from which the parameter value is to be obtained.
*     basename = const char * (Given)
*        The unqualified parameter name.
*     qualifier = const char * (Given)
*        The qualified to use.
*     buf = char * (Returned)
*        A buffer in which to store the full config parameter name, with
*        qualifier.
*     lbuf = dim_t (Given)
*        The length of "buf".
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     A pointer to the config parameter name to use. The "basename"
*     pointer is returned if an error occurs.

*  Description:
*     The "qualifier" string is appended to the end of the "basename"
*     string, and the KeyMap is searched for an entry with the resulting
*     total string. If the entry is found, a pointer to the total string
*     is returned as the function value. Otherwise, a pointer to the
*     basename is returned.

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     18-MAR-2013 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2013 Science & Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "ast.h"
#include "star/one.h"
#include "sae_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

const char *smf_keyname( AstKeyMap *keymap, const char *basename,
                         const char *qualifier, char *buf, dim_t lbuf,
                         int *status ){

/* Local Variables */
   const char *result;

/* Initialise... */
   result = basename;

/* Check the inherited status. Also return if no qualifier was supplied. */
   if( *status != SAI__OK || !qualifier ) return result;

/* Form the full qualified name, within the supplied buffer. */
   (void) one_strlcpy( buf, basename, lbuf, status );
   (void) one_strlcat( buf, qualifier, lbuf, status );

/* If the KeyMap contains an entry with this name, and the entry has
   a defined value, return a pointer to the name. */
   if( astMapDefined( keymap, buf ) ) result = buf;

/* Return the result. */
   return result;
}


