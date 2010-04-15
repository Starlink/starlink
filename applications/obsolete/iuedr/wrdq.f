      SUBROUTINE WRDQ(STATUS)

*+
*
*   Name:
*      SUBROUTINE WRDQ
*
*   Description:
*      The current data quality is written back to the spectrum store.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          13-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Export:
      INTEGER STATUS     ! status return

*   CMFILE:
      INCLUDE 'CMFILE'

*   CMSPEC:
      INCLUDE 'CMSPEC'

*   CMSAVE:
      INCLUDE 'CMSAVE'

*   CMWAV:
      INCLUDE 'CMWAV'

*   CMNET:
      INCLUDE 'CMNET'

*   Local variables:
      INTEGER IORDER     ! order index

      INTEGER I          ! wavelength index

*   Find existing order, or allocate a new one
      CALL FNORD(ORDER, IORDER)

      IF (IORDER.LE.0) THEN

         CALL ERROUT('Error: updating non-existent order\\', STATUS)
         RETURN

      END IF

*   Copy QNET to QNETS
      DO 100 I = 1, NWAV

         CALL DQ_ITOU(QNET(I), QNETS(I, IORDER))

 100  CONTINUE

      SPCHAN = .TRUE.
      STATUS = 0

      END
