C-----------------------------------------------------------------------

      SUBROUTINE DMS_TO_STRING (IDMS, STRING)

C  Routine to take angle or time array and produce an output string
C  including sign (if negative) assuming that sign is given with first
C  non-zero array element.

      IMPLICIT NONE

C  Formal parameters
      INTEGER*4 IDMS(4)
      CHARACTER STRING*(*)

C  Local variables
      INTEGER*4 I
      INTEGER*4 IERR
      CHARACTER SIGN_DMS*1

C  Functions
      INTEGER*4 ABS

      SIGN_DMS = ' '
      DO I = 1,4
        IF (IDMS(I).LT.0) SIGN_DMS = '-'
      END DO

      WRITE (STRING,'(A1,I2.1,2I3.2,''.'',I2.2)', IOSTAT=IERR)
     &                   SIGN_DMS,
     &                   ABS(IDMS(1)), ABS(IDMS(2)),
     &                   ABS(IDMS(3)), ABS(IDMS(4))

      RETURN
      END

C-----------------------------------------------------------------------
