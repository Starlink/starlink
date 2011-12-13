/*
*+
*  Name:
*     ERR_ANNUL

*  Purpose:
*     Annul the contents of the current error context.

*  Language:
*     Starlnk Fortran 77

*  Invocation:
*     CALL ERR_ANNUL( STATUS )

*  Description:
*     Any error messages pending output in the current error context are
*     annulled, i.e. deleted. The values of any existing message tokens
*     become undefined and the value of the status argument is reset to
*     SAI__OK.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The global status: it is set to SAI__OK on return.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1983, 1989 Science & Engineering Research Council.
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
*     JRG: Jack Giddings (UCL)
*     SLW: Sid Wright (UCL)
*     RFWS: R.F. Warren-Smith (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     17-Apr-1983 (SLW):
*        Added MARK and RELEASE mods.
*     20-JUN-1989 (RFWS):
*        Updated prologue, comments and layout.
*     11-SEP-1989 (PCTR):
*        Completed code tidy-up.
*     15-DEC-1989 (PCTR):
*        Converted to call EMS_ANNUL.
*     18-JUL-2008 (TIMJ):
*        Now in C. Calls errAnnul.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "merswrap.h"
#include "mers_f77.h"

F77_SUBROUTINE(err_annul)( INTEGER(STATUS) ) {
  int status;
  F77_IMPORT_INTEGER( *STATUS, status );
  errAnnul( &status );
  F77_EXPORT_INTEGER( status, *STATUS );
}
