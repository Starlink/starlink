      SUBROUTINE RDCOMB(STATUS)

*+
*
*   Name:
*      SUBROUTINE RDCOMB
*
*   Description:
*      An attempt is made to access the mapped spectrum associated with
*      the current dataset.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          13-OCT-88     IUEDR Vn. 2.0
*      Martin Clayton     01-JUL-94     IUEDR Vn. 3.1-1
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Export:
      INTEGER STATUS     ! status return

*   Global includes:
      INCLUDE 'CMCOMB'

*   Check inherited global status
      IF (STATUS .NE. SAI__OK) RETURN

*   Get Calibration and Mean Spectrum
      CALL DASSOC('M\\', 'T\\', STATUS)
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERROUT('Error: could not access dataset\\', STATUS)
         RETURN
      ELSE IF (NOCOMB) THEN
         CALL ERROUT('Error: no combined spectrum\\', STATUS)
      END IF
      END
