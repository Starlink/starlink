      SUBROUTINE RDLO( STATUS )

*+
*
*   Name:
*      SUBROUTINE RDLO
*
*   Description:
*      The APERTURE parameter is read, and if this differs from the current
*      internal "order", then the new spectrum is read.
*      In the event of the new aperture not existing, no harm is done.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          07-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*
*   Deficiencies:
*      It should check that there are ANY orders before looking for
*      any one in particular.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Export:
      INTEGER STATUS     ! status return

*   External references:
      EXTERNAL CALO      ! LORES calibration

*   Local variables:
      INTEGER IAPER      ! aperture index

*   Aperture/resolution
      CALL DEFAPR(1, IAPER, STATUS)
      IF (STATUS.NE.0) THEN
         CALL ERROUT('Error: aperture/resolution invalid\\', STATUS)
         RETURN
      END IF

*   Get order
      CALL RDORD(IAPER, CALO, STATUS)
      IF (STATUS.NE.0) THEN
         CALL ERROUT('Error: accessing spectrum\\', STATUS)
         RETURN
      END IF

      END
