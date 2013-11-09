*+  TIM_GETDATE - Retrieve current system time in a sensible format
      SUBROUTINE TIM_GETDATE( DATE, STATUS )
*
*    Description :
*
*     Returns a string of the form,
*
*      "dd-Mmm-yyyy hh:mm:ss"
*
*     without the quotes. This is the same is the old VMS LIB$DATE_TIME call
*     expect the fractional seconds are dropped. Those values are not easily
*     available on UNIX.
*
*    Method :
*
*     Extracts info out of PSX_ time string. No check is made to see if
*     the output is truncated in the exported string.
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
      CHARACTER*(*)          DATE		! Output date & time
*
*    Local variables :
*
      CHARACTER*26           PT              	! PSX time string
      CHARACTER*20           TEMP            	! Formatted time string

      INTEGER                NTICKS          	! System time
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get time
      CALL PSX_TIME( NTICKS, STATUS )
      CALL PSX_CTIME( NTICKS, PT, STATUS )

*    Format sensibly
 10   FORMAT( A2, '-', A3, '-', A4, ' ', A8 )
      WRITE( TEMP, 10 ) PT(9:10), PT(5:7), PT(21:24), PT(12:19)

*    Set output
      DATE = TEMP

      END
