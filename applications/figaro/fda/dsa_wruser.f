      SUBROUTINE DSA_WRUSER( STRING )
*+
*  Name:
*     DSA_WRUSER
*     DSA_WRFLUSH

*  Purpose:
*     Put out a message string to the user.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_WRUSER( STRING )
*     CALL DSA_WRFLUSH( )

*  Description:
*     DSA_WRUSER puts out a message string to the user, buffering it
*     until either a line's worth of output has been received or until a
*     string containing a newline code (`backslash n', as in C's printf)
*     is received. So a call to this routine does not necessarily result
*     in a single line being output immediately to the user.
*
*     A call to DSA_WRFLUSH puts out any buffered messages that have
*     been written using DSA_WRUSER but not yet sent to the user, and is
*     exactly the same as making a call to DSA_WRUSER with a string
*     consisting solely of a 'backslash n' sequence. Using this routine
*     allows a program to avoid the need for backslash characters in its
*     code - something that some UNIX compilers treat as escape
*     characters in a true emulation of C.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The (part of the) message.

*  Implementation Status:
*     This code is taken straight from DSA 5.0-1 with only cosmetic
*     changes, the replacement of calls to PAR_WRUSER, the conversion of
*     common variables into 'save' local variables, and the
*     initialisation of such variables. There is
*     troublesome code where buffer remains are copied to is beginning,
*     copying overlapping substrings within the same string is not
*     standard Fortran.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     10 Jun 1987 (ks):
*        Original version.  KS / AAO.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     23 Aug 1992 (ks):
*        Modified to remove the backslash character from the code for
*        portablility reasons. Also introduction of DSA_WRFLUSH.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     15 Feb 1996 (hme):
*        FDA library. The common variables are made local, save,
*        variables.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) STRING

*  Local Constants:
      INTEGER BUFF_LEN
      PARAMETER ( BUFF_LEN = 80 )

*  Local Variables:
      CHARACTER BACKSLASH * 1    ! Holds the backslash character
      CHARACTER CHR * 1          ! Character from buffer
      LOGICAL ESCAPE             ! Flags in middle of escape sequence
      INTEGER I                  ! Loop index through chars in STRING
      INTEGER STATUS             ! PAR_WRUSER status return - ignored
      INTEGER BUFF_PTR, LAST_BLANK
      CHARACTER * ( BUFF_LEN ) MESS_BUFF
      SAVE BUFF_PTR, LAST_BLANK, MESS_BUFF

*  Data Statements:
      DATA BUFF_PTR, LAST_BLANK, MESS_BUFF / 0, 0, ' ' /

*.

*  Set the backslash character - some UNIX compilers don't like to see
*  this is Fortran code, so this works around it - at the expense of
*  assuming ASCII characters, I'm afraid.
      BACKSLASH = CHAR(92)

*  Loop through all the characters in the buffer.
      ESCAPE = .FALSE.
      DO 1 I = 1, LEN(STRING)
         IF ( BUFF_PTR .GE. BUFF_LEN ) THEN

*        Buffer full, output up to and including last blank.  If
*        there are no blanks at all in the buffer, output the lot.
            IF ( LAST_BLANK .EQ. 0 ) THEN
*              CALL PAR_WRUSER( MESS_BUFF, STATUS )
               CALL ERR_MARK
                  STATUS = SAI__OK
                  CALL MSG_OUT( 'DSA_WRUSER', MESS_BUFF, STATUS )
                  CALL ERR_ANNUL( STATUS )
               CALL ERR_RLSE
               BUFF_PTR = 0
            ELSE
*              CALL PAR_WRUSER( MESS_BUFF(:LAST_BLANK), STATUS )
               CALL ERR_MARK
                  STATUS = SAI__OK
                  CALL MSG_OUT( 'DSA_WRUSER', MESS_BUFF(:LAST_BLANK),
     :               STATUS )
                  CALL ERR_ANNUL( STATUS )
               CALL ERR_RLSE
               MESS_BUFF(1:) = MESS_BUFF(LAST_BLANK+1:)
               BUFF_PTR = BUFF_LEN - LAST_BLANK
            END IF
            LAST_BLANK = 0
         END IF

*     Get next character
         CHR = STRING(I:I)
         IF ( ESCAPE ) THEN

*        Previous character was a backslash, so we are in the middle of
*        an escape sequence. If this is a newline, flush the buffer.
*        Otherwise, just ignore the sequence.
            IF ( CHR .EQ. 'N' .OR. CHR .EQ. 'n' ) THEN
*              CALL PAR_WRUSER(MESS_BUFF(:BUFF_PTR),STATUS)
               CALL ERR_MARK
                  STATUS = SAI__OK
                  CALL MSG_OUT( 'DSA_WRUSER', MESS_BUFF(:BUFF_PTR),
     :               STATUS )
                  CALL ERR_ANNUL( STATUS )
               CALL ERR_RLSE
               BUFF_PTR = 0
               LAST_BLANK = 0
            END IF
            ESCAPE=.FALSE.
         ELSE

*        If character is a backslash then set escape flag.  Otherwise,
*        stuff it into the buffer.
            IF ( CHR .EQ. BACKSLASH ) THEN
               ESCAPE = .TRUE.
            ELSE
               BUFF_PTR = BUFF_PTR + 1
               MESS_BUFF(BUFF_PTR:BUFF_PTR) = CHR
               IF (CHR.EQ.' ') LAST_BLANK = BUFF_PTR
            END IF
         END IF
 1    CONTINUE

*  Return.
      END



      SUBROUTINE DSA_WRFLUSH( )

      IMPLICIT NONE

      CHARACTER BACKSLASH * 1

      BACKSLASH = CHAR(92)
      CALL DSA_WRUSER( BACKSLASH // 'N' )

      END
