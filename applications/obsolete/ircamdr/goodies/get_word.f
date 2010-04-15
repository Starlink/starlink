*+  GET_WORD - get next word from command line

      SUBROUTINE GET_WORD ( LINE, WORD, WORDLEN, PARMS, STATUS )

*    Description :
*     Obtains the first contiguous block of non-space characters from
*     the line given to it.  Returns this and the rest of the line
*     as separate parameters.

*    Invocation :
*     CALL GET_WORD ( LINE, WORD, WORDLEN, PARMS, STATUS )

*    Parameters :
*     LINE=CHARACTER*(*) (given)
*           input string of characters
*     WORD=CHARACTER*(*) (returned)
*           the first 'word' in LINE
*     WORDLEN=INTEGER (returned)
*           the number of characters in the word
*     PARMS=CHARACTER*(*) (returned)
*           the remainder of LINE
*     STATUS=INTEGER

*    Method :
*     Calls library function lib$skpc to skip preceding blanks.  Then calls
*     lib$locc to look for the next blank.  The resulting pointers delimit
*     the first word on the line.

*    Deficiencies :
*     Accepts very limited syntax, eg, is likely to break up quoted
*     strings if they constitute the first 'word'.

*    Bugs :
*     <description of any "bugs" which have not been fixed>

*    Authors :
*     John Cooke (REVS::JAC) 01May84

*    History :
*     4-MAY-1984  first insertion (REVAD::JAC)
*     20.04.1985: UFACE version (REVAD::BDK)
*     07.05.1985: handle case where PARMS missing (REVAD::BDK)
*     07.05.1985: return PARMS without leading space (REVAD::BDK)
*     12.05.1986: created this version from UFACE_GETWORD (REVA::CAA)
*     14.05.1986: added a word length parameter to call (REVA::CAA)
*     15.05.1986: added temporary buffer for parms to allow skip for leading
*                 spaces in output parms (REVA::CAA)
*     25-May-1994 Changed lib$ calls to chr_ (SKL@JACH)
*    endhistory

*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'CHR_ERR'
      INCLUDE 'SAE_PAR'

*    Import :
      CHARACTER*(*) LINE               ! the input line

*    Export :
      CHARACTER*(*) WORD               ! the first word from LINE

      INTEGER WORDLEN                  ! number of characters in word

      CHARACTER*(*) PARMS              ! the rest of LINE

*    Status :
      INTEGER STATUS

*    External references :
      INTEGER CHR_LEN

*    Local variables :
      INTEGER LENGTH               ! length of line

      INTEGER START                ! start of word index in LINE

      INTEGER END                  ! end of word index in LINE

*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      LENGTH = CHR_LEN( LINE )


*   find indices for start and end of first word

      START = 1
      CALL CHR_FIWS( LINE, START, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN

         END = START
         CALL CHR_FIWE( LINE, END, STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN

            WORDLEN = END - START + 1
            WORD = LINE(START:END)
            PARMS = LINE(END+1:)
            CALL CHR_LDBLK( PARMS )

         ELSE IF ( STATUS .EQ. CHR__ENDOFSENT) THEN

            WORDLEN = LENGTH - START + 1
            WORD = LINE(START:)
            PARMS = ' '

         ELSE
*
*       blank the output parameters
*
            WORDLEN = 1
            WORD = ' '
            PARMS = ' '

         ENDIF

      ELSE
*
*      blank the output parameters
*
         WORDLEN = 1
         WORD = ' '
         PARMS = ' '

      ENDIF

      END
