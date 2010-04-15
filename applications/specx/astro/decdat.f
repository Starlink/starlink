      SUBROUTINE DECDAT (IDATEC, JDATE)
*+
*  Name:
*     DECDAT

*  Purpose:
*     Routine to decode the date from a character expression

*  Invocation:
*     CALL DECDAT( IDATEC, JDATE )

*  Description:
*     Converts a string of form dd-mon-yy (eg 30-Dec-99)
*     to an array of day, month and year.

*  Arguments:
*     IDATEC = CHARACTER*9 (Given)
*        Input string of form dd-mon-yy
*     JDATE( 3 ) = INTEGER (Returned)
*        Array of integer day, month and year. Year includes the
*        century.

*  Notes:
*     Since the year is 2-digits this routine uses windowing to convert
*     to a four digit year. The windowing is identical to that used
*     by SLALIB (SUN/67):
*        00-49, interpreted as 2000-2049
*        50-99, interpreted as 1950-1999

*  Authors:
*     Rachael Padman (MRAO)
*     Remo Tilanus (JAC, Hawaii)
*     Tim Jenness (JAC, Hawaii)
*     Horst Meyerdierks (UoE, Starlink)

*  History:
*     Pre-history (rp):
*        Original version
*     19 Nov 1993 (hme):
*        Remove TABs.
*     27 Oct 1999 (rpt):
*        Y2K fix replace adding 1900 with window.
*     30 Dec 1999 (timj):
*        Change windowing to match slalib (<50 is 2000)
*        Use more informative header
*        For g77 compatibility:
*           Fix declarations of AMONTH and AMON to be CHAR
*           Change formats to 1X rather than just X

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER     JDATE(3)

*  Arguments Returned:
      CHARACTER   IDATEC*9

*  Local Variables:
      CHARACTER*3 AMON          ! Month from input string
      CHARACTER*3 AMONTH(12)    ! List of all 12 months
      INTEGER     I             ! Loop counter over months
      INTEGER     IYEAR         ! Year from string

*  Local Data:
      DATA AMONTH/'JAN','FEB','MAR','APR','MAY','JUN',
     &           'JUL','AUG','SEP','OCT','NOV','DEC'/
*.

      READ(IDATEC,10,ERR=99) JDATE(1),AMON,IYEAR
   10 FORMAT(I2,1X,A3,1X,I2)
      DO I=1,12
        IF(AMON.EQ.AMONTH(I)) GO TO 30
      END DO
   30 JDATE(2)=I
      IF (IYEAR .LT. 87) THEN
        JDATE(3)=2000+IYEAR
      ELSE
        JDATE(3)=1900+IYEAR
      ENDIF

   99 RETURN
      END


