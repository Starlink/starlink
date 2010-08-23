      SUBROUTINE KPG1_VERB( VERB, PACK, STATUS )
*+
*  Name:
*     KPG1_VERB

*  Purpose:
*     Determine whether the specified package should report verbose
*     messages.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_VERB( VERB, PACK, STATUS )

*  Description:
*     This routine returns a logical flag indicating if a specified
*     applications package should report verbose information. This is the
*     case if the environment variable <PACK>_VERBOSE is defined (the value
*     assigned to the environment variable is immaterial).

*  Arguments:
*     VERB = LOGICAL (Returned)
*        Should the package run in verbose mode? Returned .FALSE, if an
*        error has already occurred, or if this routine should fail for
*        any reason.
*     PACK = CHARACTER * ( * ) (Given)
*        The name of the package (e.g. "KAPPA", "POLPACK", etc.).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine attempts to execute even if STATUS is set to an
*     error on entry.

*  Copyright:
*     Copyright (C) 1998, 2000 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-SEP-1998 (DSB):
*        Original version.
*     10-APR-2000 (DSB):
*        Added argument PACK.
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
      LOGICAL VERB

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
      CALL CHR_APPND( '_VERBOSE', NAME, IAT )
      CALL CHR_UCASE( NAME )

*  Attempt to get the value of the environment variable.
      CALL PSX_GETENV( NAME( : IAT ), ENV, STATUS )

*  If the environment variable was not defined, annul the error, and
*  indicate that verbose mode should not be used.
      IF( STATUS .EQ. PSX__NOENV ) THEN
         CALL ERR_ANNUL( STATUS )
         VERB = .FALSE.

      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         VERB = .TRUE.

      END IF

*  End the error reporting context.
      CALL ERR_END( STATUS )

      END
