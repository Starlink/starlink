      SUBROUTINE KPG1_DEBUG( DEBUG, PACK, STATUS )
*+
*  Name:
*     KPG1_DEBUG

*  Purpose:
*     Determines whether the specified package should report debug
*     diagnostics

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_DEBUG( VERB, PACK, STATUS )

*  Description:
*     This routine returns a logical flag indicating if a specified
*     applications package should report debug diagnostics. This is the
*     case if the environment variable <PACK>_DEBUG is defined (the value
*     assigned to the environment variable is immaterial).

*  Arguments:
*     DEBUG = LOGICAL (Returned)
*        Should the package run in debug mode? Returned .FALSE, if an
*        error has already occurred, or if this routine should fail for
*        any reason.
*     PACK = CHARACTER * ( * ) (Given)
*        The name of the package (e.g. "KAPPA", "POLPACK").
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine attempts to execute even if STATUS is set to an
*     error on entry.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     5-DEC-2001 (DSB):
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

*  Arguments Returned:
      LOGICAL DEBUG

*  Arguments Given:
      CHARACTER PACK*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER NAME*40          ! Name of the environment variable
      CHARACTER ENV*40           ! Value of the environment variable
      INTEGER IAT                ! Length of the environment variable name
*.

*  Begin a new error reporting context.
      CALL ERR_BEGIN( STATUS )

*  Form the name of the environment variable.
      NAME = ' '
      IAT = 0
      CALL CHR_APPND( PACK, NAME, IAT )
      CALL CHR_APPND( '_DEBUG', NAME, IAT )
      CALL CHR_UCASE( NAME )

*  Attempt to get the value of the environment variable.
      CALL PSX_GETENV( NAME( : IAT ), ENV, STATUS )

*  If the environment variable was not defined, annul the error, and
*  indicate that debug mode should not be used.
      IF( STATUS .EQ. PSX__NOENV ) THEN
         CALL ERR_ANNUL( STATUS )
         DEBUG = .FALSE.

      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         DEBUG = .TRUE.

      END IF

*  End the error reporting context.
      CALL ERR_END( STATUS )

      END
