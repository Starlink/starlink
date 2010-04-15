*  History:
*     07 Dec 1993 (hme):
*        Use zero-filled field (I2.2) for first integer as well.
*        Otherwise there is a blank between the sign and number.
C-----------------------------------------------------------------------

      SUBROUTINE DEG_TO_STRING (DEG, STRING)

C  Routine to take (signed) real*8 angle in degrees and convert to
C  standard string format for R.A. or Dec.

      IMPLICIT NONE

C     Formal parameters
      DOUBLE PRECISION  DEG
      CHARACTER         STRING*(*)

C     Local variables
      INTEGER*4 I
      INTEGER*4 IERR
      INTEGER*4 IDMS(4)
      CHARACTER SIGN_DMS*1

C     Functions
      INTEGER*4 ABS

      CALL DEG_TO_DMS (DEG, IDMS)

      SIGN_DMS = ' '
      DO I = 1,4
        IF (IDMS(I).LT.0) SIGN_DMS = '-'
      END DO

      WRITE (STRING,'(A1,I2.2,2I3.2,''.'',I2.2)', IOSTAT=IERR)
     &                   SIGN_DMS,
     &                   ABS(IDMS(1)), ABS(IDMS(2)),
     &                   ABS(IDMS(3)), ABS(IDMS(4))

      RETURN
      END

C-----------------------------------------------------------------------
