#include <X11/Xlib.h>
#include "sae_par.h"
#include "gwm.h"
#include "gwm_err.h"
#include "gwm_for.h"
#include "cnf.h"
#include "f77.h"
#include "ems.h"

/******************************************************************************/

F77_SUBROUTINE(gwm_dswin) ( CHARACTER(wname), INTEGER(status)
                            TRAIL(wname) )

/*
*+
*  Name :
*     GWM_DSWIN
*
*  Purpose :
*     Destroy a GWM window.
*
*  Language :
*     C
*
*  Invocation :
*     CALL GWM_DSWIN( WNAME, STATUS )
*
*  Description :
*     Destroy a GWM window. The GWM window having the given name on
*     the current X display is destroyed.
*
*  Arguments :
*     WNAME = CHARACTER * ( * ) (Given)
*        The window name.
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

GENPTR_CHARACTER(wname)
GENPTR_INTEGER(status)

/* Local variables */
char *lwname;
int lstat;

/* Check status on entry */
if ( *status != SAI__OK ) return;

/* Copy the FORTRAN string to a local C string */
lwname = cnf_creim( wname, wname_length );

/* Destroy the window with this name */
lstat = GWM_DestroyWindow( GWM_display_id, lwname );

/* Free the CNF resources */
cnf_free( lwname );

/* Return an error if the GWM function failed */
if ( lstat != GWM_SUCCESS )
   {
   *status = GWM__NOWIN;
   ems_setc_c( "TOKEN", lwname, wname_length );
   ems_rep_c( "GWM_DSWIN_NOWIN", "Unable to destroy GWM window : ^TOKEN",
              status );
   return;
   }

return;
}
