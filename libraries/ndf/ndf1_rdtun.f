      SUBROUTINE NDF1_RDTUN( NAME, DFLT, VALUE, STATUS )
*+
*  Name:
*     NDF1_RDTUN

*  Purpose:
*     Read a tuning parameter value from an environment variable.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_RDTUN( NAME, DFLT, VALUE, STATUS )

*  Description:
*     The routine reads an NDF_ system tuning parameter from an
*     environment variable and attempts to convert it to an integer. If
*     it succeeds, this integer value is returned.  Otherwise, a
*     default value is returned.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        Name of the environment variable.
*     DFLT = INTEGER (Given)
*        The default value to be returned if no environment variable
*        value can be obtained.
*     VALUE = INTEGER (Returned)
*        The returned value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     18-OCT-1993 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Arguments Given:
      CHARACTER * ( * ) NAME
      INTEGER DFLT

*  Arguments Returned:
      INTEGER VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( VAL__SZI ) CVAL ! Environment variable value
      INTEGER ISTAT              ! Local status value
      INTEGER LVAL               ! Length of environment variable value
      LOGICAL DEF                ! Environment variable defined?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Attempt to translate the environment variable.
      CALL NDF1_GTENV( NAME, DEF, CVAL, LVAL, STATUS )

*  If a translation was found, attempt to convert it to an integer
*  value.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( DEF ) THEN
            ISTAT = SAI__OK
            CALL CHR_CTOI( CVAL, VALUE, ISTAT )

*  If not successful, then use the default value.
            IF ( ISTAT .NE. SAI__OK ) VALUE = DFLT

*  Also use the default if the environment variable had no translation.
         ELSE
            VALUE = DFLT
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_RDTUN', STATUS )

      END
