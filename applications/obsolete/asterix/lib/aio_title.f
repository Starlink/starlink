*+  AIO_TITLE - Writes title (with date/time) to output channel
      SUBROUTINE AIO_TITLE( OCH, NAME, STATUS )
*
*    Description :
*
*     Outputs a title (normally programme name & version), date and time
*     to the output channel OCH.
*
*    Parameters :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*      5 May 94 : Adapted from UTIL_TITLE (DJA)
*     12 Apr 95 : Removed use of intrinsic functions DATE and TIME in
*                 favour of the PSX alternatives. (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE			'SAE_PAR'
*
*    Import :
*
      CHARACTER*(*)		NAME			! Program name (inc version)
      INTEGER 			OCH			! Output channel id
*
*    Status :
*
      INTEGER			STATUS			! Local status code
*
*    External references :
*
      INTEGER 			CHR_LEN

*  Local Variables:
      CHARACTER*50              B50			! A line of blanks
      CHARACTER*9 		DAT			! Date string
      CHARACTER*24              PT                      ! PSX time string
      CHARACTER*8 		TIM			! Time string

      INTEGER                   FSTAT                   ! i/o status
      INTEGER                   NTICKS                  ! System time

      INTEGER 			LENGTH			! Length of prog name
      INTEGER 			NX			! Spacing to centre title
*-

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get time
      CALL PSX_TIME( NTICKS, STATUS )
      CALL PSX_CTIME( NTICKS, PT, STATUS )

*    Format sensibly
 10   FORMAT( A2, '-', A3, '-', A2 )
      WRITE( DAT, 10, IOSTAT=FSTAT ) PT(9:10), PT(5:7), PT(23:24)
      TIM = PT(12:19)

*    Centre title for 80 char page width
      LENGTH = CHR_LEN(NAME)
      NX = (80-LENGTH)/2
      CALL AIO_IWRITE( OCH, NX, NAME(:LENGTH), STATUS )
      CALL CHR_FILL( ' ', B50 )
      CALL AIO_IWRITE( OCH, 4, DAT//B50//TIM, STATUS )

      END
