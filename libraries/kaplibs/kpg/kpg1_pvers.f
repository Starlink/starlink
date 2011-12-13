      SUBROUTINE KPG1_PVERS( TEXT, MAJ, MIN, REV, STATUS )
*+
*  Name:
*     KPG1_PVERS

*  Purpose:
*     Parses a package version string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PVERS( TEXT, MAJ, MIN, REV, STATUS )

*  Description:
*     This routine splits the supplied package version string (e.g.
*     "V0.13-6") into 3 integers corresponding to the major version
*     number, minor version number and revision number. The leading "V"
*     can be omitted, and trailing fields can be ommitted (they default
*     to zero).

*  Arguments:
*     TEXT = CHARACTER * ( * ) (Given and Returned)
*        The version string. Blanks are removed and it is converted to
*        upper case on exit.
*     MAJ = INTEGER (Returned)
*        The major version number.
*     MIN = INTEGER (Returned)
*        The minor version number.
*     REV = INTEGER (Returned)
*        The revision number.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-NOV-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given and Returned:
      CHARACTER TEXT*(*)

*  Arguments Returned:
      INTEGER MAJ
      INTEGER MIN
      INTEGER REV

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      INTEGER I                  ! Index of next character to be checked
      INTEGER MARK               ! Index of end marker
      INTEGER TLEN               ! Index of last non-blank character
*.

*  Initialise
      MAJ = 0
      MIN = 0
      REV = 0

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Remove spaces, and convert to upper case.
      CALL CHR_RMBLK( TEXT )
      CALL CHR_UCASE( TEXT )

*  Store the used length of the string.
      TLEN = CHR_LEN( TEXT )

*  Skip over any leading "V".
      IF( TEXT( 1 : 1 ) .EQ. 'V' ) THEN
         I = 2
      ELSE
         I = 1
      END IF

*  Find the first dot (if any).
      MARK = INDEX( TEXT ( I : ), '.' )
      IF( MARK .EQ. 0 ) THEN
         MARK = TLEN + 1
      ELSE
         MARK = MARK + I - 1
      END IF

*  If the major version number field is null, report an error.
      IF( MARK .LE. I ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_PVERS_ERR1', 'Missing major version '//
     :                 'number.', STATUS )
         GO TO 999
      END IF

*  If the major version number field is blank, report an error.
      IF( TEXT( I : MARK - 1 ) .EQ. ' ' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_PVERS_ERR2', 'Missing major version '//
     :                 'number.', STATUS )
         GO TO 999
      END IF

*  Attempt to read an integer from the major version number field.
      CALL CHR_CTOI( TEXT( I : MARK - 1 ), MAJ, STATUS )

*  Report an error if this failed.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'KPG1_PVERS_ERR3', 'Invalid major version '//
     :                 'number.', STATUS )
         GO TO 999
      END IF

*  Move on to look at the first character following the dot.
      I = MARK + 1

*  If we have not reached the end of the string, check for a minor version
*  number.
      IF( I .LE. TLEN ) THEN

*  Look for the first "-".
         MARK = INDEX( TEXT ( I : ), '-' )
         IF( MARK .EQ. 0 ) THEN
            MARK = TLEN + 1
         ELSE
            MARK = MARK + I - 1
         END IF

*  If the minor version number field is null, report an error.
         IF( MARK .LE. I ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPG1_PVERS_ERR4', 'Missing minor version '//
     :                    'number.', STATUS )
            GO TO 999
         END IF

*  If the minor version number field is blank, report an error.
         IF( TEXT( I : MARK - 1 ) .EQ. ' ' ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPG1_PVERS_ERR5', 'Missing minor version '//
     :                    'number.', STATUS )
            GO TO 999
         END IF

*  Attempt to read an integer from the minor version number field.
         CALL CHR_CTOI( TEXT( I : MARK - 1 ), MIN, STATUS )

*  Report an error if this failed.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'KPG1_PVERS_ERR6', 'Invalid minor version '//
     :                    'number.', STATUS )
            GO TO 999
         END IF

*  Move on to look at the first character following the "-".
         I = MARK + 1

*  If we have not reached the end of the string, check for a revision
*  number.
         IF( I .LE. TLEN ) THEN

*  Attempt to read an integer from the revision number field.
            CALL CHR_CTOI( TEXT( I : TLEN ), REV, STATUS )

*  Report an error if this failed.
            IF( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'KPG1_PVERS_ERR7', 'Invalid revision '//
     :                       'number.', STATUS )
               GO TO 999
            END IF

         END IF

      END IF

 999  CONTINUE

      END
