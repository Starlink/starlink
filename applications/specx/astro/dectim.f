C-----------------------------------------------------------------------

      SUBROUTINE DECTIM (ITIMEC, JTIME)

C  Routine to decode the time from character variable

      INTEGER   JTIME(3)
      CHARACTER ITIMEC*8

      READ (ITIMEC,10, IOSTAT=IERR) JTIME
   10 FORMAT(I2,1X,I2,1X,I2)

      RETURN
      END


