C-------------------------------------------------------------------------

      SUBROUTINE TRAN_DATE (DDATE, DATE)

C   Routine to translate date in format yyyy.mmdd into SPECX
C   character format date dd-mon-yy

      IMPLICIT  NONE

      REAL*8    DDATE
      CHARACTER DATE*9

      INTEGER   IERR
      INTEGER   IDAY
      INTEGER   MONTH
      INTEGER   IYEAR
      CHARACTER MONTHS(12)*3

      DATA MONTHS /'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
     &             'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'/

      IYEAR = INT (DDATE)
      MONTH = 100.*(DDATE-FLOAT(IYEAR))
      IDAY  = NINT(10000.*(DDATE-(IYEAR+.01*MONTH)))

      WRITE (DATE, '(I2.2,''-'',A3,''-'',I2.2)', IOSTAT=IERR)
     &                IDAY, MONTHS(MONTH), IYEAR-1900

      RETURN
      END

C-----------------------------------------------------------------------
