      SUBROUTINE ERR_STOP( STATUS )
*+
*  Name:
*     ERR_STOP

*  Purpose:
*     Close the Error Reporting System.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERR_STOP( STATUS )

*  Description:
*     Flush any messages pending output and return the Error Reporting 
*     System to its initial state.

*  Arguments:
*     STATUS = INTEGER (Given)
*        The global status.

*  Implementation Notes:
*     This subroutine is for use only with the ADAM implementation of
*     the Error Reporting System.

*  Algorithm:
*     -  If there are pending messages, then flush them.
*     -  If there are no pending messages, but STATUS is set, then
*     report a warning message and then flush it.
*     -  Clear the message token table.

*  Authors:
*     SLW: Sid Wright (UCL)
*     BDK: Dennis Kelly (ROE)
*     RFWS: R.F. Warren-Smith (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-FEB-1983 (SLW):
*        Original version.
*     10-JUN-1987 (BDK):
*        Ensure return to startup state.
*     7-AUG-1989 (RFWS):
*        Converted to new prologue layout and added comments.
*     11-SEP-1989 (PCTR):
*        Completed code tidy-up.
*     12-JAN-1990 (PCTR):
*        Converted to use EMS_ calls.
*     31-JAN-1991 (PCTR):
*        Removed restart of EMS.
*     20-FEB-2001 (AJC):
*        Remove report on status set but no report (can't happen)
*         and avoids use of EMS internal EMS1_IEPND.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! Standard SAE constants

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER ISTAT                     ! Local status

*. 

*  Set the local status.
      ISTAT = STATUS

*     Call ERR_CLEAR to clear the error message table.
      CALL ERR_CLEAR( ISTAT )

*  Call EMS1_ESTOP to close EMS_ error reporting.
!      CALL EMS1_ESTOP

*  Clear the message token table.
!      CALL EMS1_MSTOP

      END
