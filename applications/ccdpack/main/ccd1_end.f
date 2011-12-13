      SUBROUTINE CCD1_END( STATUS )
*+
*  Name:
*     CCD1_END

*  Purpose:
*     To close the log file and write an end of task message.

*  Language:
*     Starlink_fortran

*  Invocation:
*     CALL CCD1_END( STATUS )

*  Description:
*     The routines just closes the log file system and write out a blank
*     line and the terminator *-

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-JUN-1991 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CCD1_HVUSR
      LOGICAL CCD1_HVUSR         ! Returns true if a user is present
                                 ! if so the delimeter is not written.
*.

*  This routine alway executes regardless of STATUS.
      CALL ERR_BEGIN( STATUS )

*  Add some blanks
      IF ( .NOT. CCD1_HVUSR( STATUS ) ) THEN
         CALL CCD1_MSG( ' ', '*-', STATUS )
      ELSE
         CALL CCD1_MSGL( '*-', STATUS )
      END IF
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Close the log file.
      CALL CCD1_CLLOG( STATUS )
      CALL ERR_END( STATUS )
      END
*  $Id$
