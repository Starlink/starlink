      SUBROUTINE MSG_BELL( STATUS )
*+
*  Name:
*     MSG_BELL

*  Purpose:
*     Deliver an ASCII BEL character.

*  Language:
*    Starlink Fortran 77

*  Invocation:
*     CALL MSG_BELL( STATUS )

*  Description:
*     A bell character and a new line is delivered to the user. If the 
*     user interface in use supports the ASCII BEL character, this routine 
*     will ring a bell and print a new line on the terminal.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Implementation Notes:
*     -  This subroutine is the ADAM version of MSG1_PRINT.

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-OCT-1993 (PCTR):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! Standard SAE constants
      INCLUDE 'MSG_ERR'                 ! MSG_ error codes
 
*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER ASCBEL                    ! ASCII BEL code
      PARAMETER ( ASCBEL = 7 )

*  Local Variables:
      CHARACTER BELCHR * 1              ! The bell character

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Use SUBPAR_WRMSG to deliver the bell character and MSG_SYNC to ensure
*  the output buffer is delivered.
      BELCHR = CHAR( ASCBEL )
      CALL SUBPAR_WRMSG( BELCHR, STATUS )
      CALL MSG_SYNC( STATUS )

*  Check the returned status and report an error message if necessary.
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = MSG__OPTER
         CALL EMS_MARK
         CALL EMS_REP( 'MSG_BELL_OPTER', 
     :   'Error encountered during BELL output.', STATUS )
         CALL EMS_RLSE
      END IF

      END
