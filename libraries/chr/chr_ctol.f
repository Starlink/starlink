      SUBROUTINE CHR_CTOL( STRING, LVALUE, STATUS )
*+
*  Name:
*     CHR_CTOL

*  Purpose:
*     Read a LOGICAL value from a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_CTOL( STRING, LVALUE, STATUS )

*  Description:
*     The given string is decoded as a logical value. TRUE, T, YES,
*     Y and FALSE, F, NO, N are recognised, regardless of case. Other
*     strings result in STATUS being set to SAI__ERROR.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The string from which a LOGICAL value is to be read.
*     LVALUE = LOGICAL (Returned)
*        The resulting LOGICAL value.
*     STATUS = INTEGER (Given and Returned)
*        The status value: if this value is not SAI__OK on input,
*        the routine returns without action; if the routine does
*        not complete successfully, STATUS is returned set to
*        SAI__ERROR.

*  Algorithm:
*     Check that the used length of the string is .LE.5. If so,
*     convert the string to uppercase and check against the valid
*     values and setting LVALUE appropriately. If the string is not
*     a recognized logical value, set status to SAI__ERROR

*  Copyright:
*     Copyright (C) 1982, 1984, 1988, 1989, 1994 Science & Engineering Research Council.
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
*     JRG: Jack Giddings (UCL)
*     ACD: A.C. Davenhall (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-MAR-1982 (JRG):
*        Original version.
*     19-NOV-1984 (ACD):
*        Documentation improved.
*     3-SEP-1988 (AJC):
*        Documentation improved.
*     21-DEC-1988 (AJC):
*        Rewrite for efficiency.
*     3-MAR-1989 (AJC):
*        Use correct length of string.
*     16-AUG-1989 (AJC):
*        Use SAE_PAR.
*     10-MAR-1994 (ACC for PCTR):
*        Modifications to prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER STRING * ( * )

*  Arguments Returned:
      LOGICAL LVALUE

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! String length (ignoring trailing blanks)

*  Local Variables:
      INTEGER SLEN               ! Used length of string

      CHARACTER * 5 LSTR         ! Local string - uppercase

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the used length of the given string.
      SLEN = CHR_LEN( STRING )

*  Check that used length is in range.
      IF ( SLEN .LE. 5 ) THEN

*     The length is OK, so convert the string to uppercase.
         LSTR = STRING
         CALL CHR_UCASE( LSTR )

*     Check the string value.
         IF ( ( LSTR .EQ. 'Y' ) .OR. ( LSTR .EQ. 'YES' ) .OR.
     :        ( LSTR .EQ. 'T') .OR. ( LSTR .EQ. 'TRUE' ) ) THEN

*        For TRUE.
            LVALUE = .TRUE.
         ELSE IF ( ( LSTR .EQ. 'N' ) .OR. ( LSTR .EQ. 'NO') .OR.
     :             ( LSTR .EQ. 'F') .OR. ( LSTR .EQ. 'FALSE' ) ) THEN

*        For FALSE.
            LVALUE = .FALSE.
         ELSE

*        For invalid strings return SAI__ERROR.
            STATUS = SAI__ERROR
         END IF
      ELSE
         STATUS = SAI__ERROR
      END IF

*  Check the returned status value and set the returned LOGICAL value
*  on error.
      IF ( STATUS .EQ. SAI__ERROR ) LVALUE = .FALSE.

      END
