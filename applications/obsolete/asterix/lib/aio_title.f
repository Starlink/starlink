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
*
*    Local variables :
*
      CHARACTER*50              B50			! A line of blanks
      CHARACTER*9 		DAT			! Date string
      CHARACTER*8 		TIM			! Time string

      INTEGER 			LENGTH			! Length of prog name
      INTEGER 			NX			! Spacing to centre title
*-

*    Get date and time
      CALL DATE(DAT)
      CALL TIME(TIM)

*    Centre title for 80 char page width
      LENGTH=CHR_LEN(NAME)
      NX=(80-LENGTH)/2
      STATUS = SAI__OK
      CALL AIO_IWRITE( OCH, NX, NAME(:LENGTH), STATUS )
      CALL CHR_FILL( ' ', B50 )
      CALL AIO_IWRITE( OCH, 4, DAT//B50//TIM, STATUS )

      END
