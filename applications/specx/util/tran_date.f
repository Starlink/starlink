      SUBROUTINE TRAN_DATE (DDATE, DATE)
*+
*  Name:
*     TRAN_DATE

*  Purpose:
*     Translates date from yyyy.mmdd to SPECX string format

*  Invocation:
*     CALL TRAN_DATE( DDATE, DATE)

*  Description:
*     Translates a double precision date stored in YYYY.MMDD
*     format (commonly found in GSD headers) into the SPECX
*     intenral character format date of dd-mon-yy.

*  Arguments:
*     DDATE = DOUBLE PRECISION [REAL*8] (Given)
*        Date as number of form YYY.MMDD
*     DATE = CHARACTER*9 (Returned)
*        Date as string in SPECX internal format (DD-MON-YY)

*  Notes:
*     This routine discards century information. The reverse
*     routine (DECDAT) uses windowing to regenerate the full
*     four digit year from the SPECX character string.

*  Authors:
*     Rachael Padman (MRAO)
*     Remo Tilanus (JAC, Hawaii)
*     Tim Jenness (JAC, Hawaii)

*  History:
*     Pre-history (rp):
*        Original version
*     27 Oct 1999 (rpt):
*        Fix Y2K problem by using MOD on year i.s.o. subtracting 1900
*     30 Dec 1999 (timj):
*        Use informative header
*     17 May 2000 (timj):
*        Fix precision error that caused 1999.0722 to be translated
*        to 23-JUL-99. Forced the conversion to be all in DBLE rather
*        than first going to REAL.

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE

*  Arguments Given:
      REAL*8    DDATE

*  Arguments Returned:
      CHARACTER DATE*9

*  Local Variables:
      INTEGER     IDAY          ! Integer day
      INTEGER     IERR          ! Error from Write (not used)
      INTEGER     IYEAR         ! Integer year
      INTEGER     MONTH         ! integer month
      CHARACTER*3 MONTHS(12)    ! List of all 12 months

*  Local Data:
      DATA MONTHS /'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
     &             'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'/
*-

      IYEAR = INT (DDATE)
      MONTH = 100.*(DDATE-FLOAT(IYEAR))
      IDAY  = NINT(10000.*(DDATE-DBLE(IYEAR+.01D0*MONTH)))

      WRITE (DATE, '(I2.2,''-'',A3,''-'',I2.2)', IOSTAT=IERR)
     &                IDAY, MONTHS(MONTH), MOD(IYEAR,100)

      RETURN
      END
