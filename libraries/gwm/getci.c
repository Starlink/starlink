#include <X11/Xlib.h>
#include "sae_par.h"
#include "gwm.h"
#include "gwm_err.h"
#include "gwm_for.h"
#include "gwm_sys.h"
#include "cnf.h"
#include "f77.h"
#include "ems.h"

/******************************************************************************/

F77_SUBROUTINE(gwm_getci) ( CHARACTER(wname), INTEGER(idim),
                            INTEGER_ARRAY(indexs), INTEGER(ncols),
                            INTEGER(status) TRAIL(wname) )

/*
*+
*  Name:
*     GWM_GETCI
*
*  Purpose:
*     Inquire the number of colours and the colour indices allocated
*     to the given window.
*
*  Language:
*     C
*
*  Invocation:
*     CALL GWM_GETCI( WNAME, IDIM, INDEXS, NCOLS, STATUS )
*
*  Description:
*     Inquire the number of colours and the colour indices allocated
*     to the given window.
*
*  Arguments:
*     WNAME = CHARACTER * ( * ) (Given)
*        The window name.
*     IDIM = INTEGER (Given)
*        The declared size of the INDEXS array.
*     INDEXS = INTEGER(IDIM) (Returned)
*        Array containing the colour indices allocated to the window.
*        If the number of allocated colours (NCOLS) is greater than
*        the array size (IDIM) only the first IDIM indices are returned.
*     NCOLS = INTEGER (Returned)
*        The number of colours allocated to the window.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
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
*     DLT: David Terrett (Rutherford Appleton Laboratory)
*     {enter_new_authors_here}
*
*  History:
*       9-OCT-1991 (NE):
*        Orignal version
*       1-NOV-1991 (DLT):
*        Corrected number of arguments on GENPTR_INTEGER_ARRAY
*      7-NOV-1991 (DLT):
*        global variable definitions moved to function body
*      1-DEC-1994 (DLT):
*        correct data types in call to GetColTable
*     {enter_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}
*-
*/

{
#include "gwm_global.h"

GENPTR_CHARACTER(wname)
GENPTR_INTEGER(idim)
GENPTR_INTEGER_ARRAY(indexs)
GENPTR_INTEGER(ncols)
GENPTR_INTEGER(status)

/* Local variables */
char *lwname;
int i, lstat;
unsigned long *cols, lncols;
Window window_id;

/* Check status on entry */
if ( *status != SAI__OK ) return;

/* Copy the FORTRAN string to a local C string */
lwname = cnf_creim( wname, wname_length );

/* Look for an existing window with this name */
lstat = GWM_FindWindow( GWM_display_id, lwname, &window_id );

/* Free the CNF resources */
cnf_free( lwname );

/* Return an error if the window was not found */
if ( lstat != GWM_SUCCESS )
   {
   *status = GWM__WINNF;
   emsSetnc( "TOKEN", lwname, wname_length );
   emsRep( "GWM_SETCI_WINNF", "Window not found : ^TOKEN", status );
   return;
   }

/* Inquire the colour indices and the number of colours */
lstat = GWM_GetColTable( GWM_display_id, window_id, &cols, &lncols );

/* Return an error if a problem occured */
if ( lstat != GWM_SUCCESS )
   {
   *status = GWM__ERROR;
   emsRep( "GWM_SETCI_ERROR", "Error inquiring colours", status );
   return;
   }

/* Copy the indices into the output array, up to a maximum of IDIM */
for ( i = 0; i < ( *idim < lncols ? *idim : lncols ); i++ )
   indexs[i] = *cols++;
*ncols = (int)lncols;
return;
}
