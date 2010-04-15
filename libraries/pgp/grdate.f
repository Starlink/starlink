      SUBROUTINE GRDATE(STRING, L)
*+
*     - - - - - - - -
*       G R D A T E     (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Get date and time as character string in format 'dd-Mmm-yyyy hh:mm'.
*
*   Returned
*      VALUE    c   receives date and time, truncated or extended with
*                   blanks as necessary.
*      L        i   receives the number of characters in STRING, excluding
*                   trailing blanks. This will always be 17, unless the length
*                   of the string supplied is shorter.
*
*   This routine only works correctly between 1970 and 2070 because the posix
*   time routine does not return a century number.
*
*   D.L.Terrett  Starlink  Apr 1991
*+
      IMPLICIT NONE

      CHARACTER*(*) STRING
      INTEGER L

      INCLUDE 'SAE_PAR'


      INTEGER ISTAT, TICKS
      INTEGER HOUR, MINS, SEC, DAY, MONTH, YEAR, WDAY, YDAY, IDST
      INTEGER ISTRCT
      CHARACTER*17 LOCAL

      CHARACTER*3 MON(0:11)
      DATA MON/ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     :          'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'/

      STRING = ' '
      L = MIN(17,LEN(STRING))

*   Get current time in 'ticks'
      ISTAT = SAI__OK
      CALL PSX_TIME(TICKS, ISTAT)

*   Convert to calander date
      CALL PSX_LOCALTIME(  TICKS, SEC, MINS, HOUR, DAY, MONTH, YEAR,
     :                     WDAY, YDAY, IDST, ISTRCT, ISTAT)

*   Add the appropriate century to the year
      YEAR = YEAR + 1900
      IF (YEAR.LT.1970) YEAR = YEAR + 100

*   Construct character string
      WRITE( LOCAL, '(I2,''-'',A3,''-'',I4,1X,I2,'':'',I2)' )
     :  DAY, MON(MONTH), YEAR, HOUR, MINS

*   Copy to output string
      STRING = LOCAL

      END
