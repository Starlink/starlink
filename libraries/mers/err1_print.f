      SUBROUTINE ERR1_PRINT( TEXT, STATUS )
*+
*  Name:
*     ERR1_PRINT

*  Purpose:
*     Split the text of an error message up for delivery to the user.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERR1_PRINT( TEXT, STATUS )

*  Description:
*     The text of the given error message is split into lines of length
*     ERR__SZOUT. Each line in than delivered to the user by a call to
*     ERR1_PRERR.

*  Arguments:
*     TEXT = CHARACTER * ( * ) (Given)
*        Text to be output.
*     STATUS = INTEGER (Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     SLW: Sid Wright (UCL)
*     BDK: Dennis Kelly (ROE)
*     RFWS: R.F. Warren-Smith (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A.J.Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     10-MAR-1983 (SLW):
*        Modified to use SAI I/O.
*     10-APR-1985 (BDK):
*        ADAM version.
*     4-NOV-1987 (BDK):
*        Use ERR_SZMSG.
*     7-AUG-1989 (RFWS):
*        Converted to new prologue layout and added comments.
*     12-SEP-1989 (PCTR):
*        Completed code tidy-up.
*     9-APR-1990 (PCTR):
*        Removed unreferenced include file.
*     8-AUG-1991 (PCTR):
*        Re-written to wrap error messages and call ERR1_PRERR.
*     19-AUG-1992 (PCTR):
*        Changed argument list in EMS1_PFORM call.
*     3-SEP-1992 (PCTR):
*        Remove the LSTAT check in the delivery loop.
*     4-OCT-1993 (PCTR):
*        Added bell character behaviour.
*     12-MAY-1994 (AJC):
*        EMS1_PFORM renamed EMS1_RFORM
*     15-AUG-1997 (AJC):
*        Use NEQV to compare ERRBEL
*     21-JUL-1999 (AJC):
*        Add tuning parameters ERRWSZ and ERRSTM
*     26-FEB-2001 (AJC):
*        Change EMS1_RFORM to MSG1_RFORM
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! Standard SAE constants
      INCLUDE 'ERR_PAR'                 ! ERR_ public constants
      INCLUDE 'ERR_SYS'                 ! ERR_ private constants

*  Global Variables:
      INCLUDE 'ERR_CMN'                 ! ERR_ global variables

*  Arguments Given:
      CHARACTER * ( * ) TEXT

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN                   ! String length 
      EXTERNAL ERR1_BLK                 ! Force inclusion of block data

*  Local Constants:
      INTEGER ASCBEL                    ! ASCII BEL code
      PARAMETER ( ASCBEL = 7 )

      INTEGER CONTAB                    ! Continuation indentation
      PARAMETER ( CONTAB = 7 )

*  Local Variables:
      INTEGER IPOSN                     ! Character position for text
      INTEGER LENG                      ! String length
      INTEGER LSTAT                     ! Local status
      INTEGER LSTART                    ! Start index of LINE
      INTEGER OPLEN                     ! Output string length

      CHARACTER * 1 BELCHR              ! The bell character
      CHARACTER * ( ERR__SZMSG ) LINE   ! Output line of text

*.

*  Get length of text to send.
      LENG = CHR_LEN( TEXT )

*  Initialise the local status.
      LSTAT = SAI__OK

*  Check whether a bell character is to be delivered and initialise the 
*  output line.
      IF ( ERRBEL .NEQV. ERR__NOBEL ) THEN
         BELCHR = CHAR( ASCBEL )
         LINE = BELCHR
         LSTART = 2
         ERRBEL = ERR__NOBEL
      ELSE
         LINE = ' '
         LSTART = 1
      END IF

*  If the text is not blank, then continue.
      IF ( LENG .GT. 0 ) THEN

*     Loop to split the line of text into sensible lengths for output, 
*     then write them to the error stream. First, initialise the 
*     character pointer.
         IPOSN = 1

*     Now output the message in a way determined by the relevant tuning
*     parameters.
         IF ( ERRSTM ) THEN
            IF ( LSTART .GT. 1 ) CALL ERR1_PRERR( LINE, LSTAT )
            CALL ERR1_PRERR( TEXT, LSTAT )

         ELSE
*     Call MSG1_RFORM to load the first output line and deliver
*     the result.
            CALL MSG1_RFORM
     :         ( TEXT, IPOSN, LINE( LSTART : ERRWSZ ), OPLEN )
            CALL ERR1_PRERR( LINE( : OPLEN+LSTART-1 ), LSTAT )

*     Loop to continue the remainder of the message.
*     DO WHILE loop.
 10         CONTINUE
            IF ( IPOSN .NE. 0 ) THEN

*        Re-initialise the output line for a continuation.
               LINE = '!'

*        Call MSG1_RFORM to load the continuation line and write the
*        result.
               CALL MSG1_RFORM( TEXT, IPOSN, LINE( CONTAB : ERRWSZ ),
     :                       OPLEN )
               CALL ERR1_PRERR( LINE( : OPLEN+CONTAB-1 ), LSTAT )
            GO TO 10
            END IF
         END IF
      ELSE

*     If there is no text, then send a blank message.
         LINE( LSTART : ) = '!!'
         CALL ERR1_PRERR( LINE( : LSTART+1 ), LSTAT )
      END IF

*  Check I/O status and set STATUS if necessary.
      IF ( LSTAT .NE. SAI__OK ) THEN
         STATUS = LSTAT
      END IF

      END
