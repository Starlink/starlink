*+  FIT1_COPDEP -  Copy dependent parameter values blindly
      SUBROUTINE FIT1_COPDEP( MODEL, PARAM, STATUS )
*
*    Description :
*
*     Applies constraints defined in MODEL to parameters in PARAM. Depending
*     on the value of FORCE FIT_COPDEP will or will not give an error if a
*     dependent parameter lies outside its error bounds. If it does not then
*     the offending bound is adjusted to that of the parameter on which it
*     depends.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*     19 May 94 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
*
*    Structure definitions :
*
      INCLUDE 'FIT_STRUC'
*
*    Import :
*
      RECORD /MODEL_SPEC/ 	MODEL			! Model specification
*
*    Import / Export
*
      REAL 			PARAM(*)		! Complete set of cmodel parameters
*
*    Status :
*
      INTEGER 			STATUS
*
*    Local variables :
*
      INTEGER 			IPAR			! Loop over parameters
*-

*    Status check
      IF ( STATUS.NE.SAI__OK ) RETURN

*    If there are any ties
      IF ( MODEL.NTIE .GT. 0 ) THEN

*      Loop over all parameters
        DO IPAR = 1, MODEL.NPAR

*        Part of tie?
          IF ( MODEL.TGROUP(IPAR) .GT. 0 ) THEN

*          Is this other than the first parameter in a tie?
            IF ( IPAR .NE. MODEL.TSTART(MODEL.TGROUP(IPAR)) ) THEN

*            Copy dependent value
              PARAM(IPAR) = PARAM(MODEL.TSTART(MODEL.TGROUP(IPAR)))

*          End of dependant parameter test
            END IF

*        End of test on tie membership
          END IF

*      End loop over parameters
        END DO

*    End of tie presence check
      END IF

*    Exit
      IF ( STATUS.NE.SAI__OK ) THEN
        CALL AST_REXIT( 'FIT1_COPDEP', STATUS )
      END IF

      END


*+  FIT1_COPDEPL -  Copy logical dependent parameter values blindly
      SUBROUTINE FIT1_COPDEPL( MODEL, LPARAM, STATUS )
*
*    Description :
*
*     Applies constraints defined in MODEL to parameters in PARAM. Depending
*     on the value of FORCE FIT_COPDEP will or will not give an error if a
*     dependent parameter lies outside its error bounds. If it does not then
*     the offending bound is adjusted to that of the parameter on which it
*     depends.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*     19 May 94 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
*
*    Structure definitions :
*
      INCLUDE 'FIT_STRUC'
*
*    Import :
*
      RECORD /MODEL_SPEC/ 	MODEL			! Model specification
*
*    Import / Export
*
      LOGICAL			LPARAM(*)		! Complete set of cmodel parameters
*
*    Status :
*
      INTEGER 			STATUS
*
*    Local variables :
*
      INTEGER 			IPAR			! Loop over parameters
*-

*    Status check
      IF ( STATUS.NE.SAI__OK ) RETURN

*    If there are any ties
      IF ( MODEL.NTIE .GT. 0 ) THEN

*      Loop over all parameters
        DO IPAR = 1, MODEL.NPAR

*        Part of tie?
          IF ( MODEL.TGROUP(IPAR) .GT. 0 ) THEN

*          Is this other than the first parameter in a tie?
            IF ( IPAR .NE. MODEL.TSTART(MODEL.TGROUP(IPAR)) ) THEN

*            Copy dependent value
              LPARAM(IPAR) = LPARAM(MODEL.TSTART(MODEL.TGROUP(IPAR)))

*          End of dependant parameter test
            END IF

*        End of test on tie membership
          END IF

*      End loop over parameters
        END DO

*    End of tie presence check
      END IF

*    Exit
      IF ( STATUS.NE.SAI__OK ) THEN
        CALL AST_REXIT( 'FIT1_COPDEPL', STATUS )
      END IF

      END
