*+  USI0_MARK - Start a new parameter context
      SUBROUTINE USI0_MARK( FORM, STATUS )
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
*    Import :
*
      CHARACTER*(*)		FORM			! Parameter system
*
*    Status :
*
      INTEGER 			STATUS
*
*    External references :
*
      EXTERNAL			USI_BLK
*
*    Functions :
*
      LOGICAL			CHR_SIMLR
*
*    Local Variables :
*
      INTEGER			PSYS
      LOGICAL			FOUND
*-

*  Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start USI of not done already
      IF ( .NOT. USI_SYINIT ) THEN
        CALL USI_INIT( STATUS )
      END IF

*  Identify parameter system by name
      PSYS = 1
      FOUND = .FALSE.
      DO WHILE ( (PSYS.LE.USI_NPSYS) .AND. .NOT. FOUND )
        IF ( CHR_SIMLR( FORM, USI_PSYS(PSYS).NAME
     :                (:USI_PSYS(PSYS).NLEN) ) ) THEN
          FOUND = .TRUE.
        ELSE
          PSYS = PSYS + 1
        END IF
      END DO

*  Start new context if found
      IF ( FOUND ) THEN
        USI_ICTX = USI_ICTX + 1
        USI_CTX(USI_ICTX).TYPE = PSYS
        USI_CTX(USI_ICTX).PLEN = 0

*  Warn if not found
      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'NAME', FORM )
        CALL ERR_REP( ' ', 'Parameter system ^NAME not known', STATUS )
      END IF

      END
