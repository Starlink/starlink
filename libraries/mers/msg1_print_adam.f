      SUBROUTINE MSG1_PRINT( TEXT, STATUS )
*+
*  Name:
*     MSG1_PRINT

*  Purpose:
*     Deliver the text of a message to the user. 

*  Language:
*    Starlink Fortran 77

*  Invocation:
*     CALL MSG1_PRINT( TEXT, STATUS )

*  Description:
*     This uses the parameter system to deliver the message text to the 
*     user. Trailing blanks are removed. If the delivery fails, the 
*     message is given as an error report and a subsequent explanatory 
*     error report made. This should ensure that the message is seen by 
*     the user.

*  Arguments:
*     TEXT = CHARACTER * ( * ) (Given)
*        The message text.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Implementation Notes:
*     -  This subroutine is the ADAM version of MSG1_PRINT.

*  Algorithm:
*     -  Use the parameter system to send a print message to the user
*     interface.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     SLW: Sid  Wright (UCL)
*     BDK: Dennis Kelly (ROE)
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A.J. Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1982 (JRG):
*        Original version.
*     10-Mar-1983 (SLW):
*        Modified to use SAE I/O.
*     13-NOV-1984 (BDK):
*        ADAM version.
*     3-NOV-1987 (BDK):
*        Remove trap on message length.
*     19-SEP-1989 (PCTR):
*        Converted to new prologue and layout.
*     13-MAR-1990 (PCTR):
*        Changed the name of the subroutine and included error
*        reporting.
*        and EMS_ calls.
*     26-JUN-1991 (PCTR):
*        Added mark and release to prevent message tokens being annulled
*        on error.
*     8-AUG-1991 (PCTR):
*        Added use of EMS1_PFORM to reformat the output messages.
*     19-AUG-1992 (PCTR):
*        Changed argument list in EMS1_PFORM call.
*     26-AUG-1992 (PCTR):
*        Changed call to SUBPAR_WRITE to SUBPAR_WRMSG.
*     3-SEP-1992 (PCTR):
*        Deliver the message text via the error reporting system on
*        failure.
*     12-MAY-1994 (AJC):
*        EMS1_PFORM renamed EMS1_RFORM
*     14-DEC-1994 (AJC):
*        Avoid concatenation of argument text
*     21-JUL-1999 (AJC):
*        Add tuning parameters MSGWSZ and MSGSTM
*     26-FEB-2001 (AJC):
*        Use MSG1_RFORM nor EMS1_RFORM
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Declarations:
      IMPLICIT NONE                     ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! Standard SAE constants
      INCLUDE 'MSG_ERR'                 ! MSG_ error codes
      INCLUDE 'MSG_PAR'                 ! MSG_ public constants
      INCLUDE 'MSG_SYS'                 ! MSG_ private constants

*  Global Variables:
      INCLUDE 'MSG_CMN'                 ! Line length parameters

*  Arguments Given:
      CHARACTER * ( * ) TEXT

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN                   ! String length

*  Local Variables:
      INTEGER IPOSN                     ! Character pointer
      INTEGER ISTAT                     ! Local status
      INTEGER LENG                      ! Given string length
      INTEGER OPLEN                     ! Output string length

      CHARACTER * ( MSG__SZMSG ) LINE   ! Output line of text

*.

*  Check the inherited global status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a new error context and initialise the local status.
      CALL EMS_MARK
      ISTAT = SAI__OK

*  Find the filled length of the message text string and write it 
*  to the standard output stream.
      LENG = CHR_LEN( TEXT )

      IF ( LENG .GT. 0 ) THEN
         LENG = MIN( LENG, MSG__SZMSG )

         IF ( MSGSTM ) THEN
*     Output with no messing
            CALL SUBPAR_WRMSG( TEXT, ISTAT )

         ELSE
*     Call MSG1_RFORM to load the output line and deliver it.
            IPOSN = 1

*     Loop to deliver the message in line-sized chunks.
*     DO WHILE loop.
 10         CONTINUE
            IF ( IPOSN .NE. 0 .AND. ISTAT .EQ. SAI__OK ) THEN
               CALL MSG1_RFORM( TEXT, IPOSN, LINE( : MSGWSZ), OPLEN )
               CALL SUBPAR_WRMSG( LINE( : OPLEN ), ISTAT )
            GO TO 10
            END IF
         END IF

      ELSE
*     If there is no text, then send a blank message.
         LINE = " "
         OPLEN = 1
         CALL SUBPAR_WRMSG( ' ', ISTAT )
      END IF

*  If the message cannot be delivered, then annul the current error
*  context and report the error.
      IF ( ISTAT .NE. SAI__OK ) THEN
         STATUS = ISTAT
         CALL EMS_ANNUL( ISTAT )
         CALL EMS_MARK
         CALL EMS_REP( 'MSG_PRINT_MESS',
     :                 'MSG1_PRINT: ' // LINE( : OPLEN ), STATUS )
         STATUS = MSG__OPTER
         CALL EMS_REP( 'MSG_PRINT_OPTER', 
     :   'Error encountered during message output', STATUS )
         CALL EMS_RLSE
      END IF

*  Release the current error context.
      CALL EMS_RLSE

      END
