      SUBROUTINE SST_CAPWD( STR, STATUS )
*+
*  Name:
*     SST_CAPWD

*  Purpose:
*     Capitalise words in a character string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_CAPWD( STR, STATUS )

*  Description:
*     The routine converts the initial character of each word in the
*     character string supplied to upper case and converts all other
*     characters to lower case. In this context a word is a sequence of
*     non-blank characters following a blank (plus the first sequence
*     in the string).

*  Arguments:
*     STR = CHARACTER * ( * ) (Given and Returned)
*        The character string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     6-MAR-1990 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given and Returned:
      CHARACTER * ( * ) STR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for characters
      LOGICAL LBLANK             ! Last character was blank?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      LBLANK = .TRUE.

*  Loop to process each character.
      DO 1 I = 1, LEN( STR )

*  Note if the last character was a blank.
         IF ( STR( I : I ) .EQ. ' ' ) THEN
            LBLANK = .TRUE.

*  Otherwise, convert to upper or lower case, as required.
         ELSE
            IF ( LBLANK ) THEN
               CALL CHR_UCASE( STR( I : I ) )
            ELSE
               CALL CHR_LCASE( STR( I : I ) )
            END IF

*  Note if the last character was not a blank.
            LBLANK = .FALSE.
         END IF
1     CONTINUE

      END
* @(#)sst_capwd.f   1.1   94/12/05 11:31:22   96/07/05 10:27:31
