      SUBROUTINE IUEDR3( NAME, STATUS )
*+
*  Name:
*     PROGRAM IUEDR3

*  Purpose:
*     IUE data reduction program.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IUEDR3( NAME, STATUS )

*  Arguments:
*     NAME = CHARACTER* ( * ) (Given)
*        Name of task to be executed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     This is the main program for a Monolithic set of routines for
*     reducting IUE Data.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     05-MAY-82 (JRG):
*       AT4 version.
*     05-JAN-88 (PCTR):
*       IUEDR Vn. 1.4.
*     07-SEP-88 (PCTR):
*       IUEDR Vn. 2.0.  Conversion to FORTRAN.
*     16-DEC-94 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER STATUS         ! Global status.

*  Arguments Given:
      CHARACTER*( * ) NAME   ! Name of task to do.

*  Global Variables:
      INCLUDE 'CMDYN'

      LOGICAL ACTIVE
      COMMON / REMSTATE / ACTIVE

*  Local Variables:
      INTEGER I
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Initialise.
      IF ( .NOT. ACTIVE ) THEN
         CALL INITDR( STATUS )
         ACTIVE = .TRUE.
         DO I = 1, 32
            DFREE( I ) = .TRUE.
         END DO
      END IF

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL LINE_WCONT( '%p Problems encountered during \\' )
         CALL LINE_WCONT( '%p initialisation, IUEDR aborting.\\' )
         CALL PRTBUF( STATUS )

      ELSE
         CALL INVOKE( NAME, STATUS )
         IF ( STATUS .EQ. -999 ) THEN
	    CALL CLOSDR

         ELSE IF ( STATUS.NE.SAI__OK .AND. STATUS.NE.SAI__WARN ) THEN
            CALL ERR_FLUSH( STATUS )

         ELSE IF ( STATUS .EQ. SAI__WARN ) THEN
            STATUS = SAI__OK
 	 END IF
      END IF

      END
