#include <X11/Xlib.h>
#include "sae_par.h"
#include "f77.h"
#include "gwm_for.h"

/******************************************************************************/

F77_SUBROUTINE(gwm_close) ( INTEGER(status) )

/*
*+
*  Name :
*     GWM_CLOSE
*
*  Purpose :
*     Close the X client-server connection.
*
*  Language :
*     C
*
*  Invocation :
*     CALL GWM_CLOSE( STATUS )
*
*  Description :
*     Close the X client-server connection established by GWM_OPEN.
*     The standard error handler is restored.
*
*  Arguments :
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink RAL)
*     {enter_new_authors_here}
*
*  History :
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
