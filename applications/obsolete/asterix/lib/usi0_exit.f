*+  USI0_EXIT - USI exit handler
      SUBROUTINE USI0_EXIT( )
*    Description :
*     <description of what the subroutine does - for user info>
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*     21 May 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USI0_PAR'
*
*    Global variables :
*
      INCLUDE 'USI_CMN'
*
*    Status :
*
      INTEGER 			STATUS
*-

*  Initialise status
      STATUS = SAI__OK

*  USI initialised?
      IF ( USI_SYINIT ) THEN

*      Unwind contexts
        DO WHILE ( USI_ICTX .GT. 0 )
          CALL USI0_RLSE( STATUS )
        END DO

*      Close log file if open
        IF ( USI_LOGGING ) THEN
          CALL FIO_CLOSE( USI_LOGFID, STATUS )
        END IF

      END IF

      END
