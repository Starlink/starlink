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
      EXTERNAL			USI0_BLK
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
      DO WHILE ( (PSYS.LE.USI_NPS) .AND. .NOT. FOUND )
        IF ( CHR_SIMLR( FORM, PS_NAME(PSYS)(:PS_NLEN(PSYS)) ) ) THEN
          FOUND = .TRUE.
        ELSE
          PSYS = PSYS + 1
        END IF
      END DO

*  Start new context if found
      IF ( FOUND ) THEN

*    Increment context
        USI_ICTX = USI_ICTX + 1

*    Store type
        CTX_TYPE(USI_ICTX) = PSYS

*    Create parameter store
        CALL ADI_NEW0( 'STRUC', CTX_PST(USI_ICTX), STATUS )

*  Warn if not found
      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'NAME', FORM )
        CALL ERR_REP( ' ', 'Parameter system ^NAME not known', STATUS )
      END IF

      END
