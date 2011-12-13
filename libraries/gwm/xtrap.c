#include <X11/Xlib.h>
#include <string.h>
#include "gwm_err.h"
#include "ems.h"

/******************************************************************************/

int gwm_xtrap ( Display *display_id, XErrorEvent *error )

/*
*+
*  Name:
*     GWM_XTRAP
*
*  Purpose:
*     GWM error handler for X errors.
*
*  Language:
*     C
*
*  Description:
*     GWM error handler for X errors. This is set up in GWM_OPEN.
*     The errors are reported using EMS.
*
*  Arguments:
*     display_id = Display* (Given)
*        The display identifier
*     error = XErrorEvent* (Given)
*        The error event data structure
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
*     {enter_new_authors_here}
*
*  History:
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
emsRep( "GWM_XTRAP_XERR", strcat( "X error : ", string ), &status );

return 0;
}

