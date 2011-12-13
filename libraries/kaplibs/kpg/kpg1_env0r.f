      SUBROUTINE KPG1_ENV0R( VARNAM, RVAL, STATUS )
*+
*  Name:
*     KPG1_ENV0R

*  Purpose:
*     Reads a floating-point value from an environment variable.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ENV0R( VARNAM, RVAL, STATUS )

*  Description:
*     This routine reads a floating-point value from a specified
*     environment variable. No error occurs if the environment variable
*     is not defined, or has a non-numeric value, and the supplied value
*     is returned unchanged.

*  Arguments:
*     VARNAM = CHARACTER * ( * ) (Given)
*        The environment variable to check.
*     RVAL = REAL (Returned)
*        The value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-SEP-1999 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PSX_ERR'          ! PSX error constants

*  Arguments Given:
      CHARACTER VARNAM*(*)

*  Arguments Returned:
      REAL RVAL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER VAL*40           ! Value of the environment variable
      INTEGER ISTAT              ! CHR status
      REAL RRVAL                 ! Value read from the variable
*.

*  Check the inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Attempt to get the value of the environment variable.
      CALL PSX_GETENV( VARNAM, VAL, STATUS )

*  If the environment variable was not defined, annul the error.
      IF( STATUS .EQ. PSX__NOENV ) THEN
         CALL ERR_ANNUL( STATUS )

*  If no error occurred, attempt to read a real value from the
*  environment variable's value.
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         ISTAT = SAI__OK
         CALL CHR_CTOR( VAL, RRVAL, ISTAT )
         IF( ISTAT .EQ. SAI__OK ) RVAL = RRVAL
      END IF

      END
