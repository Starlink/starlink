      SUBROUTINE RECAL( STATUS )
*+
*  Name:
*     SUBROUTINE RECAL

*  Description:
*     If there is a current order with defined NET, then this is
*     recalibrated to produce FLX and proper wavelengths.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RECAL( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     25-JUN-81 (JRG):
*       IUEDR Vn. 1.0
*     15-DEC-87 (PCTR):
*       IUEDR Vn. 1.4
*     13-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     03-FEB-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMNET'

*  Status:
      INTEGER STATUS     ! Global status.

*  Local Variables:
      INTEGER IRES       ! Resolution index.
*.

*   Proceed only if NET is defined.
      IF ( .NOT. NONET ) THEN
         CALL IUE_RESN( RESOL, IRES, STATUS )
         IF ( STATUS .NE. 0 ) THEN
            CALL ERROUT( 'Error: resolution invalid\\', STATUS )
            GO TO 999

         ELSE IF ( IRES .EQ. 1 ) THEN
            CALL LINE_WCONT( '%p Recalibrating Spectrum.\\' )
            CALL PRTBUF( STATUS )
            CALL CALO( STATUS )

         ELSE IF ( IRES .EQ. 2 ) THEN
            CALL LINE_WCONT( '%p Recalibrating Order.\\' )
            CALL PRTBUF( STATUS )
            CALL CAHI( STATUS )
         END IF

      END IF

 999  CONTINUE

      END
