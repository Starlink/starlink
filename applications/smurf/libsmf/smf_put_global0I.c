/*
*+
*  Name:
*     smf_put_global0I

*  Purpose:
*     Put a scalar integer value into the smurf globals keymap.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_put_global0I( const char *name, int value, int *status )

*  Arguments:
*     name = const char * (Given)
*        The key name for the item.
*     value = int (Given)
*        The value of the item to put into the keymap.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function puts a scalar integer value into smurf globals
*     keymap, associating it with the supplied key name.
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
void smf_put_global0I( const char *name, int value, int *status ){

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* Lock the smurf globals keymap pointer for use by the current thread.
   If it is currently locked by another thread, wait until it is
   released. */
   astLock( smurf_global_keymap, 1 );

/* Put the value into the KeyMap. */
   astMapPut0I( smurf_global_keymap, name, value, NULL );

/* Unlock the smurf globals keymap pointer so that it can be used by
   other threads. */
   astUnlock( smurf_global_keymap, 1 );

}

