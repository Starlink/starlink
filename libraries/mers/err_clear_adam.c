/*
*+
*  Name:
*     ERR_CLEAR

*  Purpose:
*     Return the error table to the default context and flush its
*     contents.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     CALL ERR_CLEAR( STATUS )

*  Description:
*     The Error Reporting System is returned to its default context
*     level and any pending messages are flushed. This routine
*     effectively resets the Error Reporting System:
*
*        -  unlike ERR_FLUSH, no 'faulty application' error message is
*        reported if it is called when there are no error messages
*        pending output, or if it is called with the status value set
*        to SAI__OK;
*        -  the error table is always annulled by a call to ERR_CLEAR,
*        irrespective of any message output errors which may occur.
*
*     On exit, the status is always returned as SAI__OK.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The global status.

*  Implementation Notes:
*     This subroutine is for use only with the ADAM implementation of
*     the Error Reporting System.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 1995, 2001 Central Laboratory of the Research Councils.
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
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A.J.Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-JUN-1991 (PCTR):
*        Original version.
*     14-DEC-1995 (AJC):
*        Correct EMS1_IEPND to LOGICAL type
*     16-FEB-2001 (AJC):
*        Avoid EMS internals
*     29-JUL-2008 (TIMJ):
*        Call errClear.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "merswrap.h"
#include "mers_f77.h"

F77_SUBROUTINE(err_clear)( INTEGER(STATUS) ) {
  int status;
  errClear( &status );
  F77_EXPORT_INTEGER( status, *STATUS );
}
