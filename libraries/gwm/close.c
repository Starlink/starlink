#include <X11/Xlib.h>
#include "sae_par.h"
#include "f77.h"
#include "gwm_for.h"

/******************************************************************************/

F77_SUBROUTINE(gwm_close) ( INTEGER(status) )

/*
*+
*  Name:
*     GWM_CLOSE
*
*  Purpose:
*     Close the X client-server connection.
*
*  Language:
*     C
*
*  Invocation:
*     CALL GWM_CLOSE( STATUS )
*
*  Description:
*     Close the X client-server connection established by GWM_OPEN.
*     The standard error handler is restored.
*
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink RAL)
*     {enter_new_authors_here}
*
*  History:
*      2-OCT-1991 (NE):
*        Orignal version
*      7-NOV-1991 (DLT):
*        global variable definitions moved to function body
*     {enter_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}
*-
*/

{
#include "gwm_global.h"

GENPTR_INTEGER(status)

/* If status is set on entry reinstate the default handler and return */
if ( *status != SAI__OK )
   {
   XSetErrorHandler( None );
   return;
   }

/* Break the client server connection */
XCloseDisplay( GWM_display_id );

/* Reinstate the handler operative when GWM_OPEN was called */
XSetErrorHandler( X_error_handler );

return;
}
