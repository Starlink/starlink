#include <X11/Xlib.h>
#include "sae_par.h"
#include "gwm_err.h"
#include "gwm_for.h"
#include "cnf.h"
#include "f77.h"
#include "ems.h"

/******************************************************************************/

F77_SUBROUTINE(gwm_open) ( CHARACTER(disply), LOGICAL(usedef),
                           INTEGER(status) TRAIL(disply) )

/*
*+
*  Name:
*     GWM_OPEN
*
*  Purpose:
*     Establish the X client-server connection.
*
*  Language:
*     C
*
*  Invocation:
*     CALL GWM_OPEN( DISPLY, USEDEF, STATUS )
*
*  Description:
*     Establish the X client-server connection. The display name
*     specifies the node on which the server is running. Most
*     applications will use the default device in which case the
*     logical argument USEDEF should be set to true and the display
*     name is ignored. An error handler is established which reports
*     errors via EMS. This routine has to be called before any of
*     the other GWM FORTRAN interface routines. The connection is
*     terminated by the routine GWM_CLOSE.
*
*  Arguments:
*     DISPLY = CHARACTER * ( * ) (Given)
*        Display name specifying the network node name and display
*        number of the workstation. The format of the name will be
*        "hostname:number<.screen>" if the transport mechanism is
*        TCP/IP or "hostname::number<.screen>" if the transport
*        mechanism is DECNET. The "hostname" is the name of the
*        host machine to which the display is physically connected.
*        The "number" is the number of the server on the host machine.
*        A single CPU can have one or more servers which are usually
*        numbered starting with zero. On a multiple-screen workstation
*        the optional "screen" number indicates the screen to use.
*        Examples are "cpu:0", "cpu::0", "cpu:0.1" or "cpu::0.1".
*        If USEDEF is true the display name is ignored.
*     USEDEF = LOGICAL (Given)
*        If true use the default display, otherwise use the display
*        specified by the DISPLY argument.
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

GENPTR_CHARACTER(disply)
GENPTR_LOGICAL(usedef)
GENPTR_INTEGER(status)

/* Local variables */
int i;
char *ldisply;
extern int gwm_xtrap(Display*, XErrorEvent*);

/* Check status on entry */
if ( *status != SAI__OK ) return;

/* Make the client server connection */
/* If USEDEF is true use the default display */
if ( F77_ISTRUE(*usedef) )
   GWM_display_id = XOpenDisplay( "" );

/* Otherwise use the given display name */
/* Copy the FORTRAN string to a local C string */
else
   {
   ldisply = cnf_creim( disply, disply_length );
   GWM_display_id = XOpenDisplay( ldisply );
   cnf_free( ldisply );
   }

/* Check that the connection was established */
if ( !GWM_display_id )
   {
   *status = GWM__NODIS;
   emsRep( "GWM_OPEN_NODIS", "Unable to open display", status );
   return;
   }

/* Establish the error handler */
#if 0
X_error_handler = XSetErrorHandler( &gwm_xtrap );
#endif
X_error_handler = XSetErrorHandler( gwm_xtrap );

/* Initialise the window charactersitics to accept the defaults */
for ( i = 0; i < wc_nchars; i++ ) GWM_wc.yesno[i] = 0;

return;
}
