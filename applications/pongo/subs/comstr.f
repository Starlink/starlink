      LOGICAL FUNCTION COMSTR( FULSTR, STR )
*+
*  Name:
*     COMSTR

*  Purpose:
*     Compares two strings -- abbreviation allowed.

*  Language:
*     {routine_language}

*  Invocation:
*     RESULT = COMSTR( FULSTR, STR )

*  Description:
*     This routine returns .TRUE. if the string 'STR' is a valid
*     shortened form of the string 'FULSTR'.  The strings can be made
*     up of one or more 'words' separated by the symbol 'CCTSYM'
*     defined below. Each word in STR can be shortened individually.
*
*     So Axxx_Bxxx_Cxxx could be matched by A_B_C, as well as
*     Axxx being matched by A.

*  Arguments:
*     FULSTR = CHARACTER * ( * ) (Given)
*        The comparision string.
*     STR = CHARACTER * ( * ) (Given)
*        The string to be compared.

*  Returned Value:
*     COMSTR = LOGICAL
*        True if a matched is found.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     26-MAY-1983 (?OLAF?):
*        Original version.
*     6-JUN-1994 (PDRAPER):
*        Added prologue and tidied code.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) FULSTR
      CHARACTER * ( * ) STR

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used length of string

*  Local Constants:
      CHARACTER * ( 1 ) CCTSYM   ! Inter-word separator
      PARAMETER ( CCTSYM = '_' )

*  Local Variables:
      INTEGER IF                 ! Current position in FULSTR
      INTEGER IS                 ! Current position in STR
      INTEGER LENF               ! Length of complete input string
      INTEGER LENS               ! Length of compared string
      INTEGER LWBF               ! Next position of CCTSYM in FULSTR
      INTEGER LWBS               ! Next position of CCTSYM in STR
*.

*  Obtain the effective lengths of the two strings.
      LENF = CHR_LEN( FULSTR )
      LENS = CHR_LEN( STR )

*  Check if it is valid to compare them.
      IF ( LENS .LE. LENF  .AND.  LENS .GT. 0  .AND.  LENF .GT. 0 ) THEN

*  It is, initially set the character pointers and COMSTR.
         IF = 1
         IS = 1
         COMSTR = .TRUE.

*  Now compare each word in turn.
 1000    CONTINUE                ! Start of DO WHILE loop.
         IF ( COMSTR  .AND.  IS .LE. LENS ) THEN

*  Locate the next word boundary in both strings.
            LWBF = INDEX( FULSTR( IF: ), CCTSYM ) + IF - 2
            LWBS = INDEX( STR( IS: ), CCTSYM) + IS - 2

*  Test if there is a word boundary in STR which is not present
*  in FULSTR.
            IF ( LWBF .LT. IF  .AND. LWBS .GE. IS ) THEN

*  There is, the strings do not match.
               COMSTR = .FALSE.
            ELSE

*  There is not, set the word boundaries to the end of the strings if
*  there are no following words.
               IF ( LWBF .LT. IF ) LWBF = LENF
               IF ( LWBS .LT. IS ) LWBS = LENS

*  Compare the current words if the remaining characters in STR are
*  fewer than or the same number as those left in FULSTR, and the word
*  in STR is shorter than or the same length as the word in FULSTR.
               IF ( LENS - IS .LE. LENF - IF  .AND.
     :              LWBS - IS .LE. LWBF - IF ) THEN

*  They are, compare the words up to the length of that in STR.
                  COMSTR = FULSTR( IF : IF + LWBS - IS) .EQ.
     :                     STR( IS : LWBS )

*  Set the character pointers to the start of the next word.
                  IF = LWBF + 2
                  IS = LWBS + 2
               ELSE

*  There are more characters left in STR than in FULSTR, or the word in
*  STR is longer than that in FULSTR: set COMSTR to .FALSE..
                  COMSTR = .FALSE.
               END IF
            END IF

*  Compare the next word.
            GO TO 1000
         END IF
      ELSE

*  STR is longer than FULSTR, or STR or FULSTR are blank: set COMSTR if
*  both are blank.
         COMSTR = LENF .EQ. LENS
      END IF
      END
* $Id$
