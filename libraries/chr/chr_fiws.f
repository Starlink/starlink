      SUBROUTINE CHR_FIWS( STRING, IPOSN, STATUS )
*+
*  Name:
*     CHR_FIWS

*  Purpose:
*     Find the start of the next word within a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_FIWS( STRING, IPOSN, STATUS )

*  Description:
*     Find the start of the next word, signified by the character not
*     being a word delimiter, i.e. SPACE, TAB, or COMMA. Note that the
*     end of the current word is not found before looking for the start
*     of the next. This routine is expected to be used in conjunction
*     with CHR_FIWE.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The string to be searched.
*     IPOSN = INTEGER (Given and Returned)
*        The given value is the character position within the string
*        at which searching is to start. If IPOSN is less than 1, the
*        search starts at position 1. The returned value is the
*        character position at which the next word starts. If IPOSN
*        already points to a character within a word, it is returned
*        unchanged. If no word is found, IPOSN is returned pointing to
*        the end of the string, and STATUS is returned set.
*     STATUS = INTEGER (Given and Returned)
*        The status value: if this value is not SAI__OK on entry,
*        the routine returns without action; if no word is found, STATUS
*        is returned set to CHR__WNOTF.
*        Note: The CHR__WNOTF symbolic constant is defined in the CHR_ERR
*        include file.

*  Copyright:
*     Copyright (C) 1989, 1990, 1994 Science & Engineering Research Council.
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
*     JHF: Jon Fairclough (JACH)
*     AJC: A.J. Chipperfield (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-OCT-1989 (JHF):
*        Original version.
*     16-AUG-1989 (AJC):
*        Changed name FIND_WORD_START to FIWS.
*        Change variable names for consistency with CHR.
*        Improve prologue documentation.
*        Change to ADAM__OK to SAI__OK.
*     8-JUL-1990 (PCTR):
*        Changed TAB from symbolic constant to variable.
*     14-FEB-1990 (PCTR):
*        Improved portability.
*     10-MAR-1994 (ACC for PCTR):
*        Modifications to prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAI constants
      INCLUDE 'CHR_ERR'          ! CHR error codes

*  Arguments Given:
      CHARACTER * ( * ) STRING

      INTEGER IPOSN

*  Status:
      INTEGER STATUS

*  External References:
      CHARACTER * 1 CHR_ACHR     ! Return the specified ASCII character

*  Local constants:
      INTEGER TABVAL             ! ASCII TAB
      PARAMETER ( TABVAL = 9 )

*  Local Variables:
      CHARACTER CVALUE           ! Value of character string element
      CHARACTER TAB              ! TAB character value

      INTEGER ISTART             ! Loop start index
      INTEGER LENGTH             ! Declared length of string

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise TAB.
      TAB = CHR_ACHR( TABVAL )

*  Determine the declared length of the string.
      LENGTH = LEN( STRING )

*  Check that the start position is before the end of the string.
      IF ( IPOSN .LE. LENGTH ) THEN

*     Initialise negative or zero start positions to 1.
         IF ( IPOSN .LT. 1 ) IPOSN = 1

*     Perform search.
         ISTART = IPOSN

         DO 10 IPOSN = ISTART, LENGTH
            CVALUE = STRING( IPOSN : IPOSN )
            IF ( ( CVALUE .NE. ' ' ) .AND.
     :           ( CVALUE .NE. TAB ) .AND.
     :           ( CVALUE .NE. ',' ) ) GO TO 20
 10      CONTINUE

*     No word found, so return the position to point to the end
*     of the string and set the returned status.
         IPOSN = LENGTH
         STATUS = CHR__WNOTF
 20      CONTINUE
      ELSE

*     On error, return the position to point to the end of the string
*     and set the returned status.
         IPOSN = LENGTH
         STATUS = CHR__WNOTF
      END IF

      END
