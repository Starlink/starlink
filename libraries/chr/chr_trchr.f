      SUBROUTINE CHR_TRCHR( FROM, TO, STRING, STATUS )
*+
*  Name:
*     CHR_TRCHR

*  Purpose:
*     Translate the specified characters in a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_TRCHR( FROM, TO, STRING, STATUS )

*  Description:
*     Translate a specified set of characters within a string. The
*     character translation is controlled by the translation table
*     given by the character strings FROM and TO. Any characters
*     not appearing in the translation table are left unchanged.
*     If the status is set on entry, no action is taken. If the
*     strings FROM and TO are unequal in length, STATUS is returned
*     set to SAI__ERROR.

*  Arguments:
*     FROM = CHARACTER * ( * ) (Given)
*        A string specifying the characters to be translated.
*     TO = CHARACTER * ( * ) (Given)
*        A string specifying the translation values for each of the
*        characters in the FROM argument. The lengths of the FROM and
*        TO arguments must be the same.
*     STRING = CHARACTER * ( * ) (Given and Returned)
*        The string to be translated. Any character matching one of the
*        characters specified in the FROM argument is converted to the
*        corresponding character specified in the TO argument. All
*        other characters are left unchanged.
*     STATUS = INTEGER (Given and Returned)
*        The global status: returned set to SAI__ERROR if FROM and TO
*        have unequal lengths.

*  Copyright:
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-AUG-1990 (RFWS):
*        Original version.
*     20-FEB-1991 (PCTR):
*        Converted for CHR_ and added status check.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) FROM
      CHARACTER * ( * ) TO

*  Arguments Given and Returned:
      CHARACTER * ( * ) STRING

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER I                  ! Loop counter for string characters
      INTEGER IT                 ! Position in translation table

*.

*  Check inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the comparative lengths of FROM and TO strings.
      IF ( LEN( FROM ) .NE. LEN( TO ) ) THEN
         STATUS = SAI__ERROR
      ELSE

*     Perform the translation.
         DO 10 I = 1, LEN( STRING )
            IT = INDEX( FROM, STRING( I : I ) )
            IF ( IT .NE. 0 ) STRING( I : I ) = TO( IT : IT )
 10      CONTINUE
      END IF

      END
