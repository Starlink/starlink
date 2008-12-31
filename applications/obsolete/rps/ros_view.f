*+ROS_VIEW         Gets the date range when a Target is visible during the AO
      SUBROUTINE ROS_VIEW(DRA, DDEC, NVIS, DMJD, DSTRING)
      IMPLICIT NONE
 
*  Calling Arguments
      DOUBLE PRECISION DRA, DDEC		! In	Target position, equatorial, J2000.0, rad
      INTEGER NVIS		!	 no. of periods (0 indicates error?)
      DOUBLE PRECISION DMJD(2,2)		! Out	One or two periods when source is visible
      CHARACTER*9 DSTRING(2,2)	!	Date string, 'DD-MMM-YY' for each date
 
*  History
*     1989 Feb		M Ricketts	1st version, based on SUN_DATES
*     1991 Jan		::		reduce dates ranges that overlap AO

*     The Ao period and sun - axis offset max. are got from symbols, 1st call
 
*  Global Variables
      INCLUDE 'zpidata.inc'

      DOUBLE PRECISION DECLONG, DECLAT
      COMMON / ECL_COORD/ DECLONG, DECLAT	! Make available

      DOUBLE PRECISION AO_MJD0 
      DOUBLE PRECISION AO_PERIOD 
      DOUBLE PRECISION AO_DELAY 
      DOUBLE PRECISION SUN_MAX_OFF_DEG 	
      COMMON /AO_SPECS/ AO_MJD0,AO_PERIOD,AO_DELAY,SUN_MAX_OFF_DEG

*-
*  Functions
      REAL RO_SUNELONG
 
*  Local Parameters
      DOUBLE PRECISION YEAR_DAYS/365.242D0/
      CHARACTER*3 MONTH(12)/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'/
 
*  Local Variables
      DOUBLE PRECISION MJD_START, MJD_END
      DOUBLE PRECISION SUN_MAX_OFF_RAD, SUN_LONG0
      LOGICAL GOT_SYMBOLS/.FALSE./
      SAVE GOT_SYMBOLS, MJD_START, MJD_END
      SAVE SUN_MAX_OFF_RAD, SUN_LONG0
 
      DOUBLE PRECISION DSIGNI, DEL_ECLONG, DEL_SDAYS, MJD_SUN_AT_TLONG
      INTEGER I, J, IVIS, IY, IM, ID, STATUS
      DOUBLE PRECISION DELONG(2), DEL_DAYS(2), OFF_DEL_DAYS, MID_DEL_DAYS
      DOUBLE PRECISION ACOSELONG
      DOUBLE PRECISION MJD_MID, MJD_START_VIS, MJD_END_VIS, CMJD, FD

*  __________________________ Executable Code  _________________________________
 
*  Get symbols if not used before
      MJD_START = AO_MJD0
 
      SUN_MAX_OFF_RAD = SUN_MAX_OFF_DEG * DDTOR
      SUN_LONG0 = DBLE( RO_SUNELONG(MJD_START) )			! Sun ecl. long. at start date
      MJD_END = MJD_START + AO_PERIOD
 
      CALL SLA_EQECL(DRA, DDEC, MJD_START, DECLONG, DECLAT)		! Get ecliptic coords.
      DEL_ECLONG = MOD (DECLONG - SUN_LONG0 + DTWOPI, DPI)		! +ve ecl long, sun path} from mjd_start
      DEL_SDAYS   = YEAR_DAYS * DEL_ECLONG / DTWOPI			! days                  } till sun at target long.
									! mod(...,pi) 'cos twice per year ...
      MJD_SUN_AT_TLONG = MJD_START + DEL_SDAYS				! Date within 6 months of AO start (ie or antisun)
 
      DSIGNI = -1.0D0
      DO I=1,2											! Get long offset for min, max
         
         ACOSELONG =  COS( DPIBY2 + SIGN( SUN_MAX_OFF_RAD, DSIGNI) ) / COS (DECLAT) 		! sun - target angles
         IF (ABS (ACOSELONG) .GT.1.D0 ) THEN						! Visible all the time
            NVIS = 1
            DMJD(1,1) = MJD_START
            DMJD(2,1) = MJD_END + AO_DELAY
            GOTO 10
         END IF
         DELONG(I) = ACOS( ACOSELONG )								! sun - target angles
         DEL_DAYS(I) = YEAR_DAYS * DELONG(I) / DTWOPI			
         DSIGNI = 1.D0
      END DO
      OFF_DEL_DAYS = ( DEL_DAYS(2) - DEL_DAYS(1) ) / 2.
      MID_DEL_DAYS =   DEL_DAYS(1) + OFF_DEL_DAYS

*  See which periods overlap AO, allowing for error
      MJD_MID = MJD_SUN_AT_TLONG - MID_DEL_DAYS			! Check first possible period
      MJD_START_VIS = MJD_MID - OFF_DEL_DAYS
      MJD_END_VIS   = MJD_MID + OFF_DEL_DAYS
      IF (MJD_END_VIS .GT. MJD_START ) THEN				! Put in list
         NVIS = 1
         DMJD(1,NVIS) = MAX ( MJD_START_VIS, MJD_START )
         DMJD(2,NVIS) = MJD_END_VIS
      ELSE
         NVIS = 0
      END IF
 
      MJD_MID = MJD_SUN_AT_TLONG + MID_DEL_DAYS
      MJD_START_VIS = MJD_MID - OFF_DEL_DAYS
      MJD_END_VIS   = MJD_MID + OFF_DEL_DAYS
      IF (MJD_START_VIS .LT. MJD_END+AO_DELAY ) THEN				! Put in list
         NVIS = NVIS + 1
         DMJD(1,NVIS) = MJD_START_VIS
         DMJD(2,NVIS) = MIN ( MJD_END_VIS, MJD_END + AO_DELAY )
      END IF

10    CONTINUE								! Jump here if visible all the time
      DO IVIS = 1, NVIS							! Loop for each period
         DO J=1,2							! 	  start, end
            CMJD = DINT( DMJD(J,IVIS) ) + DBLE(2-J)			! round date up / down
            CALL SLA_DJCL(CMJD, IY, IM, ID, FD, STATUS)
            WRITE(DSTRING(J,IVIS), '(I2,A5,I2)') ID, '-'//MONTH(IM)//'-', MOD(IY,100)
         END DO
      END DO
      
 
90    CONTINUE
      END
