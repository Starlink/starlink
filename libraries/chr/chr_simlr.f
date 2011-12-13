      LOGICAL FUNCTION CHR_SIMLR( STR1, STR2 )
*+
*  Name:
*     CHR_SIMLR

*  Purpose:
*     Return whether two strings are equal, apart from case.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CHR_SIMLR( STR1, STR2 )

*  Description:
*     Determine whether two strings are the same, ignoring
*     distinctions between upper and lowercase letters.
*     Their lengths must be identical after removing trailing blanks.

*  Arguments:
*     STR1 = CHARACTER * ( * ) (Given)
*        The first string.
*     STR2 = CHARACTER * ( * ) (Given)
*        The second string.

*  Returned Value:
*     CHR_SIMLR = LOGICAL
*        Returned as .TRUE. if the two strings are the same
*        ignoring case distinctions; otherwise .FALSE.

*  Algorithm:
*     Portable version:
*        Get the string lengths using CHR_LEN. Compare the strings
*        until they differ. Each character is converted to lowercase
*        for the comparison.
*     VAX-specific version:
*        The portable method was found to be rather slow so a
*        VAX-specific method using STR$CASE_BLIND_COMPARE has
*        also been coded.

*  Copyright:
*     Copyright (C) 1983, 1984, 1988, 1989, 1990, 1991, 1994 Science & Engineering Research Council.
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
*     ASOC5: Dave Baines (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     DLT: D.L. Terrett (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     20-JUN-1984 (ASOC5):
*        Optimised version.
*     2-SEP-1988 (AJC):
*        Remove INCLUDE 'SAE_PAR'.
*     7-OCT-1988 (AJC):
*        Improve documentation.
*     26-MAY-1989 (AJC):
*        Use STR$CASE_BLIND_COMPARE.
*     25-JAN-1990 (DLT):
*        Use local variables in portable version because of DECstation
*        compiler bugs.
*     31-OCT-1991 (PCTR):
*        Version 1.3 changes.
*     10-MAR-1994 (ACC for PCTR):
*        Modifications to prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER STR1 * ( * )
      CHARACTER STR2 * ( * )

*  External References:
      CHARACTER * 1 CHR_LOWER    ! Convert alphabetic to lowercase

      INTEGER CHR_LEN            ! String length (ignoring trailing blanks)

*  Portable version.
*  Local Variables:
      INTEGER I                  ! Length of STR1

      CHARACTER * 1 C1           ! Lowercase character from STR1
      CHARACTER * 1 C2           ! Lowercase character from STR2

*.

*  Initialise the string length.
      I = CHR_LEN( STR1 )
      CHR_SIMLR = ( CHR_LEN( STR2 ) .EQ. I )

      IF ( CHR_SIMLR ) THEN

*     Loop to test equality for each character.
*     DO WHILE loop.
 10      CONTINUE
         IF ( I .GT. 0 ) THEN
            C1 = CHR_LOWER( STR1( I : I ) )
            C2 = CHR_LOWER( STR2( I : I ) )
            CHR_SIMLR = ( C1 .EQ. C2 )
            IF ( .NOT. CHR_SIMLR ) GO TO 20
            I = I - 1
         GO TO 10
         END IF
 20      CONTINUE
      END IF

*  VMS-specific version.
*  External References:
*     INTEGER STR$CASE_BLIND_COMPARE

*.

*     IF ( STR$CASE_BLIND_COMPARE( STR1, STR2 ) .EQ. 0 ) THEN
*        CHR_SIMLR = .TRUE.
*     ELSE
*        CHR_SIMLR = .FALSE.
*     END IF

      END
