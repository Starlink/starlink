      SUBROUTINE VIC_MEZ2( RECLEN, DATA, STATUS )
*+
*   Name:
*      SUBROUTINE VIC_MEZ2
*
*   Description:
*      Translate the new part of IUE Record Zero.
*
*   History:
*      Jack Giddings      02-DEC-81     AT4 version
*      Paul Rees          07-NOV-88     IUEDR Vn. 2.0
*
*   Method:
*      The newly added contents of IUE Record Zero are read into the
*      common block CMUEZ2.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER RECLEN             ! size of record in short words

      INTEGER*2 DATA(RECLEN)     ! record zero data

*   Export:
      INTEGER STATUS             ! status return

*   External references:
      LOGICAL STR_SIMLR          ! caseless string equality

*   CMHEAD:
      INCLUDE 'CMHEAD'

*   Local variables:
      REAL*8 DEC                   ! Dec
      REAL*8 GMT                   ! GMT
      REAL*8 RA                    ! RA
      REAL*8 S                     ! seconds

      INTEGER D                  ! degrees
      INTEGER DAYN               ! day number in year
      INTEGER DAYS(12)           ! days in each month
      INTEGER H                  ! hours
      INTEGER M                  ! minutes
      INTEGER MON                ! month index

*   Initialisations :
      DATA DAYS/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
*.

*   Year
      YEAR = DATA( 9 ) + 1900

*   Give extra day to Feb in leap year
      IF ( ( ( YEAR + 3 ) / 4 ) .EQ. ( YEAR / 4 ) ) THEN
         DAYS( 2 ) = 29
      END IF

*   Day Number in Year
      DAYN = DATA( 10 )
      DO MON = 1, 12
         DAY = DAYN
         MONTH = MON
         DAYN = DAYN - DAYS( MON )
         IF ( DAYN .LT. 1 ) THEN
            GO TO 100
         END IF
      END DO
 100  CONTINUE

*   GMT for mid-point of observation
*     h = data(11)
*     m = data(12)
*     s = 0.0
*     call timrad(h, m, s, Gmt)

*   Right Ascension
*     h = data(45)
*     m = data(46)
*     s = DBLE(data(47))*0.1
*     call timrad(h, m, s, Ra)

*   Declination
*     d = data(48)
*     m = data(49)
*     s = data(50)
*     call degrad(d, m, s, Dec)

*   THDA - camera temperature
      THDA = DATA( 20 ) * 0.1

*   Satellite radial velocity
*     if (str_simlr(Resol, "HIRES\")) then
*        Vearth = -data(57)
*     endif

      END
