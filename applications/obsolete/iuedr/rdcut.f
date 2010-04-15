      SUBROUTINE RDCUT(FD, STATUS)

*+
*
*   Name:
*      SUBROUTINE RDCUT
*
*   Description:
*      The CMCUT contents are read.
*
*   History:
*      Jack Giddings      01-MAY-82     AT4 version
*      Paul Rees          06-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Constants:
      LOGICAL FALSE      ! .FALSE.
      LOGICAL TRUE       ! .TRUE.

      PARAMETER (FALSE = .FALSE., TRUE = .TRUE.)

      INTEGER MAXNAME    ! maximum name length
      INTEGER MAXORD     ! maximum number of orders
      INTEGER OK         ! OK status

      PARAMETER (MAXNAME = 16, MAXORD = 100, OK = 0)

*   Import:
      INTEGER FD         ! file descriptor

*   Export:
      INTEGER STATUS     ! status return

*   CMCUT:
      INCLUDE 'CMCUT'

*   Local variables:
      INTEGER I          ! loop index

      NOCUT = TRUE
      CALL STR_MOVE('IUE_CUT\\', MAXNAME, CUTTP)
      READ (FD, *, IOSTAT = STATUS) NCUT,
     :                              (CUTORD(I), CUTW1(I), CUTW2(I),
     :                              I = 1, MIN(NCUT, MAXORD))

      IF (STATUS.NE.OK) THEN
         CALL ERROUT('Error: reading wavelength cut data\\',
     :               STATUS)
         RETURN
      ELSE IF (NCUT.GT.MAXORD) THEN
         CALL ERROUT('Error: too many wavelength cut-offs\\',
     :               STATUS)
         RETURN
      ELSE IF (NCUT.GT.0) THEN
         NOCUT = FALSE
      END IF

      STATUS = OK

      END
