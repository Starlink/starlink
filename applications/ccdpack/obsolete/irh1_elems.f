      SUBROUTINE IRH1_ELEMS( IDH, GRPEXP, DEPTH, FILE, STATUS )
*+
*  Name:
*     IRH1_ELEMS

*  Purpose:
*     Store the elements within a group expression in the associated
*     group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH1_ELEMS( IDH, GRPEXP, DEPTH, FILE, STATUS )

*  Description:
*     The given group expression is divided into elements, and each
*     element is appended to the end of the group identified by IDH.
*     The group is extended if necessary to make room for the new
*     elements.  Any text occuring within the group expression after a
*     comment character (given by symbolic constant IRH__COMC) is
*     ignored. Element delimiters occuring within nested parentheses
*     are ignored.
*
*  Arguments:
*     IDH = INTEGER (Given)
*        The identifier for the group to which the group expression
*        refers. The group must previously have been created by
*        a call to IRH1_GTIDH.
*     GRPEXP = CHARACTER (Given)
*        A group expression specifying names to be appended to the
*        given group (see ID/9 for a description of the format of group
*        expressions). 
*     DEPTH = INTEGER (Given)
*        The depth of indirection at which the group expression was
*        given. A depth of zero implies that the group expression was
*        given directly (no indirection).
*     FILE = CHARACTER (Given)
*        The name of the indirection file in which the given group
*        expression was found. If DEPTH is given as zero, then FILE is
*        ignored.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-MAY-1991 (DSB):
*        Original version.
*     31-JAN-1992 (DSB):
*        Modified to ignore element delimiters occuring within nested
*        parentheses.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRH_PAR'          ! IRH constants.

*  Arguments Given:
      INTEGER IDH
      CHARACTER GRPEXP*(*)
      INTEGER DEPTH
      CHARACTER FILE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Function giving used length of a
                                 ! string.

*  Local Variables:
      CHARACTER C*1              ! A character fromthe expression.
      INTEGER CC                 ! Position of current character.
      INTEGER COMM               ! Offset to the first comment
                                 ! character.
      INTEGER ENDP                ! Position of the last character of the
                                 ! current element, within the group
                                 ! expression.
      INTEGER EXPLEN             ! The no. of characters in the group
                                 ! expression.
      LOGICAL MORE               ! True if more elements are to be
                                 ! processed.
      INTEGER NLEVEL             ! Level of parenthesis nesting.
      INTEGER POINT              ! Position of character following the
                                 ! previous delimiter.
      INTEGER START              ! Position of the first character of
                                 ! the current element, within the
                                 ! group expression.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the group expression is blank, return immediately.
      IF( GRPEXP .EQ. ' ' ) RETURN

*  Find the start of any comment in the string.
      COMM = INDEX( GRPEXP, IRH__COMC )
      
*  Find the used length of the string excluding any comment.
      IF( COMM .EQ. 1 ) THEN
         EXPLEN = 0
      ELSE IF( COMM .NE. 0 ) THEN
         EXPLEN = CHR_LEN( GRPEXP( :COMM - 1 ) )
      ELSE
         EXPLEN = CHR_LEN( GRPEXP )      
      END IF

*  Initialise the start of the next element within the group expression
*  to be the first character in the group expression.
      START = 1

*  Initialise the level of nesting.
      NLEVEL = 0

*  Loop round until no more elements remain to be processed.
      MORE = .TRUE.
      DO WHILE( MORE )

*  Find the next element delimiter character (given by IRH__DELC) to
*  occur at zero levels of parenthesis nesting.
         POINT = START
 10      ENDP = INDEX( GRPEXP( POINT: ), IRH__DELC ) + POINT - 2

*  If no delimiter was found in the remaining part of the group
*  expression, set the end of the current element to the last character
*  in the string.
         IF( ENDP .EQ. POINT - 2 ) THEN
            ENDP = EXPLEN

*  Update the current level of nesting within parentheses.
         ELSE
            DO CC = POINT, ENDP
               C = GRPEXP( CC : CC )

               IF( C .EQ. '(' ) THEN
                  NLEVEL = NLEVEL + 1

               ELSE IF( C .EQ. ')' ) THEN
                  NLEVEL = NLEVEL - 1

               END IF

            END DO

*  If the level of nesting is greater than zero, find the next
*  delimiter.
            IF( NLEVEL .GT. 0 ) THEN
               POINT = ENDP + 2
               GO TO 10
            END IF

         END IF      

*  If the current element has non-zero length...
         IF( ENDP .GE. START ) THEN

*  ... append the element to the end of the group.
            CALL IRH1_PTELM( IDH, 0, GRPEXP( START:ENDP ), DEPTH, FILE,
     :                       0, 0, STATUS )

         END IF

*  Set the position of the start of the next element.
         START = ENDP + 2

*  If the start of the next element is beyond the last character, or if
*  an error was reported in IRH1_PTELM, flag that no more elements are
*  to be processed.
         IF( START .GT. EXPLEN .OR. STATUS .NE. SAI__OK ) MORE = .FALSE.

      END DO

      END
* $Id$
