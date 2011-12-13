      LOGICAL FUNCTION CCD1_HVUSR( STATUS )
*+
*  Name:
*     CCD1_HVUSR

*  Purpose:
*     To determine if a user is present who may be prompted.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CCD1_HVUSR( STATUS )

*  Description:
*     The routine returns true if a user if present who may be prompted
*     for information.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     CCD1_HVUSR = LOGICAL
*        The value is true if the present session is not BATCH.

*  Notes:
*     Previously called lib$getjpi on VMS.

*  Copyright:
*     Copyright (C) 1991-1992 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     31-JUL-1991 (PDRAPER):
*        Original version.
*     11-FEB-1992 (PDRAPER):
*        Changed to use PSX routine to make portable.
*     30-AUG-1995 (PDRAPER):
*        Added check so that when run from TCL it assumes that no user
*        is present.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER MODE * ( 11 )    ! Current mode

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check if we are running from TCL. This is assumed to be if the
*  ADAM_TASK_TYPE environment variable is set to I.
      CALL PSX_GETENV( 'ADAM_TASK_TYPE', MODE, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         MODE = ' '
      END IF
      IF ( MODE .EQ. 'I' ) THEN
         CCD1_HVUSR = .FALSE.
      ELSE

*  Get the name of the terminal.
         CALL PSX_ISATTY( 0, CCD1_HVUSR, STATUS )
      END IF
      END
* $Id$
