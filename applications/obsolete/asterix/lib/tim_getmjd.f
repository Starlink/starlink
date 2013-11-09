*+  TIM_GETMJD - Retrieve current system date in MJD
      SUBROUTINE TIM_GETMJD( MJD, STATUS )
*
*    Description :
*
*     Returns the INTEGER MJD for today.
*
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (ROSAT,BHVAD::DJA)
*
*    History :
*
*     29 Nov 93 : Original (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Export :
*
      INTEGER		     MJD		! Today's date
*
*    Local variables :
*
      CHARACTER*3	     CMONTH		! Character month
      CHARACTER*20           CTIME            	! Formatted time string

      INTEGER                DAY, MONTH, YEAR   ! Integer date components
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get time in string form
      CALL TIM_GETDATE( CTIME, STATUS )

*    Extract year, month, and day
      READ( CTIME, '(I2,1X,A3,1X,I4)' ) DAY, CMONTH, YEAR

*    Convert character month to integer
      CALL CONV_MONTH( CMONTH, MONTH, STATUS )

*    Convert to MJD
      CALL CONV_YMDMJD( YEAR, MONTH, DAY, MJD )

      END
