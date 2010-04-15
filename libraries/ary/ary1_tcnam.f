      SUBROUTINE ARY1_TCNAM( LOC, NAME, STATUS )
*+
*  Name:
*     ARY1_TCNAM

*  Purpose:
*     Generate a temporary data component name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_TCNAM( LOC, NAME, STATUS )

*  Description:
*     The routine generates a name which may be used to create a
*     temporary component in a specified data structure. The name is
*     chosen so that it does not clash with any existing component
*     name.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        Locator to data structure in which a temporary component is
*        required.
*     NAME = CHARACTER * ( * ) (Returned)
*        Temporary component name.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Set an initial name.
*     -  Test if a component of that name is already present. If not,
*     then return that name.
*     -  If such a component is already present, then increment a
*     counter and use it to generate a new name.
*     -  Repeat the process until a suitable name is found.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-SEP-1989 (RFWS):
*        Original version.
*     1-MAR-1990 (RFWS):
*        Corrected incorrect SAVE statement position.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants

*  Arguments Given:
      CHARACTER * ( * ) LOC

*  Arguments Returned:
      CHARACTER * ( * ) NAME

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZNAM ) TNAME ! Possible name to test
      INTEGER I                  ! Counter for generating names
      INTEGER NCH                ! Number of formatted characters
      LOGICAL THERE              ! Whether a component exists

*  Local Data:
      SAVE I
      DATA I / 1 /               ! Initial value of counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      THERE = .FALSE.

*  Generate a name from the counter.
      TNAME = 'TEMP_'
      CALL CHR_ITOC( I, TNAME( 6 : ), NCH )

*  See if a component with the current name already exists.
1     CONTINUE                   ! Start of 'DO WHILE' loop
      CALL DAT_THERE( LOC, TNAME, THERE, STATUS )
      IF ( ( STATUS .EQ. SAI__OK ) .AND. THERE ) THEN

*  If so, then increment the counter and use it to generate a new name.
         I = I + 1
         CALL CHR_ITOC( I, TNAME( 6 : ), NCH )
         GO TO 1
      END IF

*  Return the name.
      CALL ARY1_CCPY( TNAME, NAME, STATUS )

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_TCNAM', STATUS )

      END
