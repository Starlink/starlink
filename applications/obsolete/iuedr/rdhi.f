      SUBROUTINE RDHI( STATUS )
*+
*  Name:
*     SUBROUTINE RDHI

*  Purpose:
*     Read an order spectrum.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RDHI( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The ORDER parameter is read, and if this differs from the current
*     internal echelle order, then the new order is read.
*     In the event of the new order not existing, no harm is done.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     07-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     07-FEB-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Problems:
*     It should check that there are ANY orders before looking for
*     any one in particular.

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER STATUS     ! Global status.

*  External References:
      EXTERNAL CAHI      ! HIRES calibration.

*  Local Variables:
      INTEGER ACTVAL     ! Parameter value count.
      INTEGER NEWORD     ! New echelle order.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      DO WHILE ( .TRUE. )
         CALL RDPARI( 'ORDER\\', .FALSE., 1, NEWORD, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'ORDER\\', STATUS )
            GO TO 999

         ELSE
            CALL RDORD( NEWORD, CAHI, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               GO TO 999
            END IF
            CALL CNPAR( 'ORDER\\', STATUS )
         END IF
      END DO

 999  CONTINUE

      END
