      SUBROUTINE RDFACE(FD, STATUS)

*+
*
*   Name:
*      SUBROUTINE RDFACE
*
*   Description:
*      The CMFACE contents related to the face-plate circle are read.
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
      LOGICAL FALSE               ! .FALSE.
      LOGICAL TRUE                ! .TRUE.
      PARAMETER (FALSE = .FALSE., TRUE = .TRUE.)

      INTEGER MAXNAME             ! maximum name string length
      INTEGER OK                  ! OK status
      PARAMETER (MAXNAME = 16, OK = 0)

*   Import:
      INTEGER FD                  ! file descriptor

*   Export:
      INTEGER STATUS              ! status return

*   CMFACE:
      INCLUDE 'CMFACE'

*   Local variables:
      CHARACTER*(MAXNAME) CTYPE   ! F77 type string

      INTEGER NCHAR               ! character count

      NOFACE = TRUE
      NOROT = TRUE
      READ ( FD, *, IOSTAT=STATUS ) CTYPE, RADIUS, CENTRE, ANGLE, RLIM

      IF (STATUS.NE.OK) THEN
         CALL ERROUT('Error: reading faceplate data\\', STATUS)
         RETURN
      ELSE IF (RADIUS.LE.0) THEN
         CALL ERROUT('Error: faceplate radius <= 0\\', STATUS)
         RETURN
      ELSE IF (ABS(ANGLE).GT.70.0) THEN
         CALL ERROUT('Error: angle > 70 degrees\\', STATUS)
         RETURN
      END IF

      CALL GEN_CTOS(CTYPE, MAXNAME, FACETP, NCHAR)
      CALL STR_RMBLK(FACETP)
      PLIM(1) = -RADIUS
      PLIM(2) = RADIUS
      NOROT = FALSE
      NOFACE = FALSE
      STATUS = OK

      END
