*+  FIT_STATTOK - Sets TOKEN to the named of the statistic
      SUBROUTINE FIT_STATTOK( FSTAT, TOKEN, STATUS )
*
*    Description :
*
*     Sets a character string token describing the fit statistic whose
*     code is FSTAT.
*
*    Method :
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     11 Nov 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'FIT_PAR'
*
*    Import :
*
      INTEGER             FSTAT                 ! Statistic to use
      CHARACTER*(*)       TOKEN                 ! Token to set
*
*    Status :
*
      INTEGER             STATUS
*-

*    Status check
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Set the token
      IF ( FSTAT .EQ. FIT__CHISQ ) THEN
        CALL MSG_SETC( TOKEN, 'chi-squared' )
      ELSE IF ( FSTAT .EQ. FIT__LOGL ) THEN
        CALL MSG_SETC( TOKEN, 'Cash statistic' )
      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', '...from FIT_STATTOK', STATUS )
      END IF

      END
