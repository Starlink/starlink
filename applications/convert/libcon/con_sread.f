      SUBROUTINE CON_SREAD( LU, V1, V2, V3, JSTAT )
*+
*  Name:
*     CON_SREAD

*  Purpose:
*     Reads, conditions, paraphrases and classifies a string.
*
*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_SREAD( LU, V1, V2, V3, JSTAT )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine reads, conditions, paraphrases and classifies a
*     string.
*
*     A string is input from the nominated I/O unit LU (in A format)
*     and, unless an end-of-file was detected, three versions of it are
*     created.
*
*     Version 1 (V1) is as input except that spaces replace any
*     non-printing characters (e.g. TABs).
*
*     Version 2 (V2) is the same as version 1 except that any leading
*     spaces or trailing comments are removed.
*
*     Version 3 (V3) is the same as version 2 except that outside
*      "string arguments", lowercase is converted to uppercase.
*
*  Arguments:
*     LU = INTEGER (Given)
*        I/O unit for input.
*     V1 = CHARACTER * ( * ) (Returned)
*        String with TABS etc. eliminated.
*     V2 = CHARACTER * ( * ) (Returned)
*        String left justified and without comments.
*     V3 = CHARACTER * ( * ) (Returned)
*        String as in V2 except that outside string argument lowercase
*        is converted to uppercase.
*     JSTAT = INTEGER (Returned)
*        Local status:  -1 = end-of-file; 0 = not a comment;
*        +1 = comment.
*
*  Notes:
*     -  "String arguments" are groups of characters enclosed by pairs
*     of either single or double quotes.  The contents are protected
*     against conversion to uppercase.  The delimiters are ultimately
*     removed by means of the GETSTR routine.
*
*     -  A '!' character is only interpreted as start of comment
*     if it is not within a string argument.
*
*     -  If end-of-file is detected, spaces are returned for V1,
*     V2 and V3.
*
*     -  The algorithm uses several characters which are outside the
*     ANSI FORTRAN 77 set (exclamation point, double quote, tilde,
*     lowercase).  Different machine types may need a rewrite.
*
*     -  This subroutine was suggested by the RDNEXT routine of
*     Russell Owen (UW/ARC).
*
*  Authors:
*     PTW: P.T.Wallace (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1993 July 30 (MJC):
*        Original based on SREAD of KAPPA.
*     {enter_any_changes_here}
*
*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER LU

*  Arguments Returned:
      CHARACTER *( * ) V1
      CHARACTER *( * ) V2
      CHARACTER *( * ) V3
      INTEGER JSTAT

*  Local Variables:
      CHARACTER C                ! A character in the string
      CHARACTER CDELIM           ! "String argument has started" flag
      LOGICAL COMENT             ! True if a comment has started
      INTEGER I1                 ! Loop counter
      INTEGER I2                 ! Pointer
      INTEGER L1                 ! Length of the first string
      INTEGER L2                 ! Length of the second string

*.

*  Obtain the string lengths.
      L1 = LEN( V1 )
      L2 = LEN( V2 )

*  Initialise second pointer.
      I2 = 0

*  Preset the first two strings to spaces.
      V1 = ' '
      V2 = ' '

*  Reset the "string argument has started" flag.
      CDELIM = ' '

*  Reset the "comment has started" flag.
      COMENT = .FALSE.

*  Preset the status to EOF.
      JSTAT = -1

*  Read the string.
      READ( LU,'(A)',END = 999 ) V1

*  Not EOF:  preset the status to "comment".
      JSTAT = 1

*  Step along the raw input string.
      DO I1 = 1, L1

*     Next character.
         C = V1( I1:I1 )

*     Replace non-printing characters with spaces.
         IF ( C .LT. ' ' .OR. C. GT. '~' ) C = ' '

*     "String argument" delimiter character?
         IF ( .NOT. COMENT .AND.
     :        C .EQ. '"' .OR. C .EQ. '''' ) THEN

*        Yes:  leading or trailing?
            IF ( CDELIM .EQ. ' ' ) THEN

*           Leading:  remember.
               CDELIM = C

            ELSE IF ( C .EQ. CDELIM ) THEN

*           Trailing:  remember.
               CDELIM = ' '

            END IF

         END IF

*     Store character in first string.
         V1( I1:I1 ) = C

*     Start of comment?
         IF ( CDELIM .EQ. ' ' .AND. C .EQ. '!' ) COMENT = .TRUE.

*     Are we within a comment?
         IF ( .NOT. COMENT ) THEN

*        No:  leading space?
            IF ( I2 .NE. 0 .OR. C .NE. ' ' ) THEN

*           No:  reset "comment" flag.
               JSTAT = 0

*           Store character in string 2 unless already filled.  Update
*           the pointer within the string.
               IF ( I2 .LT. L2 ) THEN
                  I2 = I2 + 1
                  V2( I2:I2 ) = C
               END IF
            END IF
         END IF
      END DO

*  Generate uppercase version and exit
  999 CONTINUE
      V3 = V2
      CALL CHR_UCASE( V3 )

      END
