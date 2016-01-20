/*
*+
*  Name:
*     smf_get_global0I

*  Purpose:
*     Get a scalar integer value from the smurf globals keymap.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     result = smf_get_global0I( const char *name, int def, int *status )

*  Arguments:
*     name = const char * (Given)
*        The key name for the required item.
*     def = int (Given)
*        The default value to use if the required item is not present in
*        the globals keymap.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     The required item value.

*  Description:
*     This function returns the scalar integer value associated with the
*     given key within the smurf globals keymap. This is a keymap created
*     in smurf_mon.c, and which is accessable by all smf functions.
*
*     The globals KeyMap should be unlocked on entry, and will be
*     unlocked on exit.

*  Authors:
*     David Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     20-JAN-2016 (DSB):
*        Original version.

*  Copyright:
*     Copyright (C) 2016 East Asian Observatory.
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
#include "mers.h"
#include "sae_par.h"
#include "ast.h"

/* SMURF includes */
#include "libsmf/smf.h"

/* Main entry point . */
int smf_get_global0I( const char *name, int def, int *status ){

/* Local Variables: */
   int result;

/* Check inherited status. */
   if( *status != SAI__OK ) return def;

/* Lock the smurf globals keymap pointer for use by the current thread.
   If it is currently locked by another thread, wait until it is
   released. */
   astLock( smurf_global_keymap, 1 );

/* Get the value, assigning the supplied default value if there is no
   value in the KeyMap. */
   if( !astMapGet0I( smurf_global_keymap, name, &result ) ) result = def;

/* Unlock the smurf globals keymap pointer so that it can be used by
   other threads. */
   astUnlock( smurf_global_keymap, 1 );

/* Return the required value. */
   return result;
}

