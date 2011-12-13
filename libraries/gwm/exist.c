#include <X11/Xlib.h>
#include "sae_par.h"
#include "gwm.h"
#include "gwm_err.h"
#include "gwm_for.h"
#include "cnf.h"
#include "f77.h"

/******************************************************************************/

F77_SUBROUTINE(gwm_exist) ( CHARACTER(wname), LOGICAL(exists),
                            INTEGER(status) TRAIL(wname) )

/*
*+
*  Name:
*     GWM_EXIST
*
*  Purpose:
*     Inquire if a GWM window of the given name exists.
*
*  Language:
*     C
*
*  Invocation:
*     CALL GWM_EXIST( WNAME, EXISTS, STATUS )
*
*  Description:
*     Inquire if a GWM window of the given name exists. The current X
*     display is searched for a GWM window with the given name. The
*     result of the search is returned in the EXISTS argument.
*
*  Arguments:
*     WNAME = CHARACTER * ( * ) (Given)
*        The window name.
*     EXISTS = LOGICAL (Returned)
*        True if a GWM window of the given name exists, otherwise false.
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
*     13-SEP-1991 (NE):
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
GENPTR_LOGICAL(exists)
GENPTR_INTEGER(status)

/* Local variables */
char *lwname;
int lstat;
Window window_id;

/* Check status on entry */
if ( *status != SAI__OK ) return;

/* Copy the FORTRAN string to a local C string */
lwname = cnf_creim( wname, wname_length );

/* Look for an existing window with this name */
lstat = GWM_FindWindow( GWM_display_id, lwname, &window_id );

/* Free the CNF resources */
cnf_free( lwname );

/* Return the success or failure of the search in the EXISTS argument */
if ( lstat == GWM_SUCCESS )
   *exists = F77_TRUE;
else
   *exists = F77_FALSE;

return;
}
