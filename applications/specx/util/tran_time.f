C-----------------------------------------------------------------------

      SUBROUTINE TRAN_TIME (DTIME, TIME)

C  Routine to translate time in format hh.fffff to SPECX standard
C  time format of hh:mm:ss

      IMPLICIT  NONE

      REAL*8    DTIME
      CHARACTER TIME*8

      INTEGER   IHOUR
      INTEGER   MINS
      INTEGER   ISEC
      INTEGER   IERR
      REAL      FHOUR

      IHOUR = DTIME
      FHOUR = DTIME - IHOUR
      MINS  = 60.*FHOUR
      ISEC  = 3600.*(FHOUR-MINS/60.)

      WRITE (TIME, '(I2.2,'':'',I2.2,'':'',I2.2)', IOSTAT=IERR)
     &       IHOUR, MINS, ISEC

      RETURN
      END

C-----------------------------------------------------------------------
