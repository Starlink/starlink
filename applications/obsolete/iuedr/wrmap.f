      SUBROUTINE WRMAP( NAME, STATUS )
*+
*  Name:
*     SUBROUTINE WRMAP

*  Description:
*     This writes the mapped spectrum to the file NAME.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL WRMAP( NAME, STATUS )

*  Arguments:
*     NAME = CHARACTER*( * ) (Given)
*        Name of the file to be written.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     22-SEP-88 (PCTR):
*       IUEDR Vn. 2.0
*     15-JUL-94 (MJC):
*       IUEDR Vn. 3.1-1
*       Corrected write_ndf call, added status check.
*     27-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMCOMB'

*  Arguments Given:
      CHARACTER*( * ) NAME

*  Status:
      INTEGER STATUS        ! Global status.

*  Local Variables:
      REAL*8 RSEDMY( 100 )  ! Dummy for REAL*8 arrays.
      CHARACTER*80 DTITLE   ! Dummy for ndf title.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Check for a spectrum.
      IF ( NOCOMB ) THEN
         GOTO 999

      ELSE IF ( NCOMB .LE. 0 ) THEN
         GO TO 999
      END IF

*   Write the file.
      DTITLE = ' '
      CALL WRITE_NDF( 'MSPECTRUM', NAME, NCOMB, RSEDMY, RSEDMY,
     :                ' ', ' ', DTITLE, 1, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: writing mapped spectrum\\', STATUS )
         GO TO 999
      END IF

 999  CONTINUE

      END
