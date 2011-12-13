      SUBROUTINE KPG1_ARCOL( NDF, INLIST, EXLIST, LENGTH, STATUS )
*+
*  Name:
*     KPG1_ARCOL

*  Purpose:
*     Forms a list of the available array components in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ARCOL( NDF, INLIST, EXLIST, LENGTH, STATUS )

*  Description:
*     This routine takes a list of array components and checks whether
*     or not each is defined within an NDF, and if it is, copy the
*     component name to the output list.

*  Arguments:
*     NDF = CHARACTER * ( * ) (Given)
*        The identifier of the NDF to be inspected for certain array
*        components.
*     INLIST = CHARACTER * ( * ) (Given)
*        A comma-separated list of the array components whose presence
*        is to be tested.  Valid component names are 'Data', 'Quality',
*        'Variance', and 'Error'.  The component names may be
*        abbreviated.
*     EXLIST = CHARACTER * ( * ) (Returned)
*        A comma-separated list of the array components of INLIST that
*        are defined in the NDF.  Its length should be as long as the
*        fully expanded list of components.  The components are tested
*        in the order supplied.  Note that mixed case components are
*        returned so that they may be used in reports to users.  Thus
*        the  returned values are 'Data', 'Quality', 'Variance', and
*        'Error' for the array components DATA, QUALITY, VARIANCE, and
*        ERROR respectively.
*     LENGTH = INTEGER (Returned)
*        The effective length of the output list. Returned equal to 1 if
*        an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1994 September 27 (MJC):
*        Original version.
*     1994 December 14 (DSB):
*        Call to NDF_STATE for ERROR component changed to check
*        for "Variance" instead of "Error".
*     20-NOV-1998 (DSB):
*        Return LENGTH equal to 1 if an error occurs.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! SSE global definitions
      INCLUDE 'CHR_ERR'        ! CHR error constants

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) INLIST

*  Arguments Returned:
      CHARACTER * ( * ) EXLIST
      INTEGER LENGTH

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of string less trailing blanks
      LOGICAL CHR_ABBRV          ! Is string an abbreviation of another?

*  Local Variables:
      INTEGER CEND               ! Character pointer to the end of word
      INTEGER CSTART             ! Character pointer to the beginning of
                                 ! a word
      INTEGER INLEN              ! Length of the input list
      LOGICAL LOOP               ! Loop for another word in input list
      LOGICAL THERE              ! The NDF array component is defined
      CHARACTER * ( 8 ) WORD     ! A word from the input list

*.

*  Initialise.
      LENGTH = 1

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Define the effective length of the input list.
      INLEN = CHR_LEN( INLIST )

*  Break up the input string into words.
      LOOP = .TRUE.
      LENGTH = 0
      CSTART = 1
      CEND = 1
      DO WHILE ( LOOP .AND. STATUS .EQ. SAI__OK )

*  Find the end of the next word (this also allows for a space adjacent
*  to the comma).  If no word delimiter is found before the end of the
*  string, annul the error.
         CALL CHR_FIWE( INLIST, CEND, STATUS )
         IF ( STATUS .EQ. CHR__EOSNT ) CALL ERR_ANNUL( STATUS )

*  Extract the word and make it uppercase for the comparison.
         WORD = INLIST( CSTART:CEND )
         CALL CHR_UCASE( WORD )

*  Check whether it is valid for each of the components.  Just use one
*  character to compare, as the values all have different initial
*  characters.
         IF ( CHR_ABBRV( WORD, 'DATA', 1 ) ) THEN

*  There is no need to check that there is a DATA component, because it
*  would not be a valid NDF.  Insert a word delimiter after the first
*  word.
            IF ( LENGTH .GT. 0 ) CALL CHR_APPND( ',', EXLIST, LENGTH )
            CALL CHR_APPND( 'Data', EXLIST, LENGTH )

*  Search for QUALITY.
         ELSE IF ( CHR_ABBRV( WORD, 'QUALITY', 1 ) ) THEN

*  Check that there is a QUALITY component.
            CALL NDF_STATE( NDF, 'Quality', THERE, STATUS )
            IF ( THERE ) THEN

*  Insert a word delimiter after the first word.  Append the mixed-case
*  component name.
               IF ( LENGTH .GT. 0 )
     :           CALL CHR_APPND( ',', EXLIST, LENGTH )
               CALL CHR_APPND( 'Quality', EXLIST, LENGTH )
            END IF

*  Search for VARIANCE.
         ELSE IF ( CHR_ABBRV( WORD, 'VARIANCE', 1 ) ) THEN

*  Check that there is a VARIANCE component.
            CALL NDF_STATE( NDF, 'Variance', THERE, STATUS )
            IF ( THERE ) THEN

*  Insert a word delimiter after the first word.  Append the mixed-case
*  component name.
               IF ( LENGTH .GT. 0 )
     :           CALL CHR_APPND( ',', EXLIST, LENGTH )
               CALL CHR_APPND( 'Variance', EXLIST, LENGTH )
            END IF

*  Search for ERROR.
         ELSE IF ( CHR_ABBRV( WORD, 'ERROR', 1 ) ) THEN

*  Check that there is a VARIANCE component (NDF_STATE will not accept
*  a component name of ERROR).
            CALL NDF_STATE( NDF, 'Variance', THERE, STATUS )
            IF ( THERE ) THEN

*  Insert a word delimiter after the first word.  Append the mixed-case
*  component name.
               IF ( LENGTH .GT. 0 )
     :           CALL CHR_APPND( ',', EXLIST, LENGTH )
               CALL CHR_APPND( 'Error', EXLIST, LENGTH )
            END IF
         END IF

*  Move the character pointer in the input list to after the last
*  character of the current word
         CSTART = CEND + 1

*  Has the last word been extracted?
         IF ( CSTART .GT. INLEN ) THEN
            LOOP = .FALSE.

*  Find the start of the next word. Move the pointer that will be used
*  to find the end of the next word to be located in that word.
         ELSE
            CALL CHR_FIWS( INLIST, CSTART, STATUS )
            CEND = CSTART
         END IF
      END DO

*  Check for error.
      IF( STATUS .NE. SAI__OK ) LENGTH = 1

      END
