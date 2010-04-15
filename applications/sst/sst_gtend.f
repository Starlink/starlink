      SUBROUTINE SST_GTEND( LINE, FOUND, STATUS )
*+
*  Name:
*     SST_GTEND

*  Purpose:
*     Indentify an END statement at the end of a Fortran program
*     module.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_GTEND( LINE, FOUND, STATUS )

*  Description:
*     The routine inspects a line of Fortran source code and determines
*     whether it is an END statement which terminates a program module.
*     If it is, then FOUND is returned .TRUE., otherwise FOUND is
*     returned .FALSE..

*  Arguments:
*     LINE = CHARACTER * ( * ) (Given)
*        The Fortran source code line to be inspected. Only the
*        "statement" part of the line should be supplied, the statement
*        number, etc. being omitted.
*     FOUND = LOGICAL (Returned)
*        Whether the line is an END statement.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  It is assumed that spaces are used sensibly (i.e. do not
*     appear inside the END keyword) and that the statement does not
*     span lines.

*  Algorithm:
*     -  Initialise.
*     -  Find the first and last non-blank characters in the line.
*     -  If the line is not blank, then see if it contains 'END' and
*     nothing else.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify it under
*     the terms of the GNU General Public License as published by the Free Software
*     Foundation; either version 2 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,but WITHOUT ANY
*     WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
*     PARTICULAR PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License along with
*     this program; if not, write to the Free Software Foundation, Inc., 59 Temple
*     Place,Suite 330, Boston, MA  02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-AUG-1989 (RFWS):
*        Original version.
*     8-AUG-1990 (RFWS):
*        Changed name to SST_GTEND.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) LINE

*  Arguments Returned:
      LOGICAL FOUND

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Local Variables:
      INTEGER F                  ! First non-blank character position
      INTEGER L                  ! Last non-blank character position

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      FOUND = .FALSE.

*  Find the first and last non-blank characters in the line.
      CALL CHR_FANDL( LINE, F, L )

*  If the line is not blank, then see if it contains 'END' and nothing
*  else.
      IF( L .GE. F ) FOUND = CHR_SIMLR( LINE( F : L ), 'END' )

      END
* @(#)sst_gtend.f   1.1   94/12/05 11:31:26   96/07/05 10:27:27
