#include <X11/Xlib.h>
#include <string.h>
#include "gwm_err.h"
#include "ems.h"

/******************************************************************************/

int gwm_xtrap ( Display *display_id, XErrorEvent *error )

/*
*+
*  Name :
*     GWM_XTRAP
*
*  Purpose :
*     GWM error handler for X errors.
*
*  Language :
*     C
*
*  Description :
*     GWM error handler for X errors. This is set up in GWM_OPEN.
*     The errors are reported using EMS.
*
*  Arguments :
*     display_id = Display* (Given)
*        The display identifier
*     error = XErrorEvent* (Given)
*        The error event data structure
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     {enter_new_authors_here}
*
*  History :
*      2-OCT-1991 (NE):
*        Orignal version
*     {enter_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}
*-
*/

{
/* Local variables */
char string[64];
int status;
int stlen=64;

/* Construct the error message from the Error Event */
XGetErrorText( display_id, (int)error->error_code, string, stlen );

/* Report this error using EMS */
status = GWM__XERR;
ems_rep_c( "GWM_XTRAP_XERR", strcat( "X error : ", string ), &status );

return 0;
}

