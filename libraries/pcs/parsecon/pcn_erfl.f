      SUBROUTINE PARSECON_ERFL( STATUS )
*+
*  Name:
*     PARSECON_EROUT

*  Purpose:
*     To flush out any error messages for PARSECON

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PARSECON_ERFL( STATUS )

*  Description:
*     The routine checks if there are any error messages stacked at the
*     current context level and outputs them using PRINT if there are.
*     It does this regardless of the given STATUS value and returns
*     STATUS = SAI__OK.
*
*     This routine is provided because it is required that error
*     messages reported when parsing the interface file are output as
*     the errors are detected so that the message table does not fill
*     up and messages are output before the task continues.
*     It can safely use PRINT because there will be no return path at
*     this stage.

*  Deficiencies:
*     1.The format has to be changed to just ( A ) for the Unix version
*     2.One could consider also writing to the error output channel.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The global status.

*  External Routines Used:
*     EMS_ELOAD
*     PARSECON_WRITE

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JUN-1991 (AJC):
*        Original version.
*      1-OCT-1991 (AJC)
*        Prefix messages with "!! " etc.
*     21-JAN-1992 (AJC):
*        Remove ref to ISTAT
*      6-MAR-1992 (AJC):
*        Use WRITE with FORMAT
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'EMS_PAR'          ! EMS constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER*(EMS__SZPAR) PARSTR ! The message name
      CHARACTER*(EMS__SZMSG) MSSTR ! The message string
      CHARACTER*3 MSPRE          ! Message prefix
      INTEGER PARLEN             ! The message name length
      INTEGER MSLEN              ! The message length
*.

*   Set STATUS to the last reported value (ie if any reported).
      CALL EMS_STAT( STATUS )

*   If there are any messages pending, unpack and output them

*   Set the initial prefix
      MSPRE = '!! '

      DOWHILE ( STATUS .NE. SAI__OK )
         CALL EMS_ELOAD( PARSTR, PARLEN, MSSTR, MSLEN, STATUS )
         WRITE(*,10) MSPRE//MSSTR(1:MSLEN)
10       FORMAT ( A )
*     Set the subsequent prefix
         MSPRE = '!  '
      ENDDO

      END
