*+  FIT_APPTIE -  Apply parameter constraints to parameter set
      SUBROUTINE FIT_APPTIE( IMOD, FORCE, PARAM, LB, UB, STATUS )
*
*    Description :
*
*     Applies constraints defined in MODEL to parameters in PARAM. Depending
*     on the value of FORCE FIT_APPTIE will or will not give an error if a
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
c     RECORD /MODEL_SPEC/ 	MODEL			! Model specification
      INTEGER			IMOD
      LOGICAL			FORCE			! Force dependant bounds
*
*    Import / Export
*
      REAL 			PARAM(*)		! Complete set of cmodel parameters
      REAL 			LB(*)			! Parameter lower bounds
      REAL 			UB(*)			! Parameter upper bounds
*
*    Status :
*
      INTEGER 			STATUS
*
*    Local variables :
*
      REAL			VALUE			! New parameter value

      INTEGER 			IPAR			! Loop over parameters
*-

*    Status check
      IF ( STATUS.NE.SAI__OK ) RETURN

*    If there are any ties
      IF ( MODEL_SPEC_NTIE(IMOD) .GT. 0 ) THEN

*      Loop over all parameters
        DO IPAR = 1, MODEL_SPEC_NPAR(IMOD)

*        Part of tie?
          IF ( MODEL_SPEC_TGROUP(IMOD,IPAR) .GT. 0 ) THEN

*          Is this other than the first parameter in a tie?
            IF ( IPAR .NE.
     :           MODEL_SPEC_TSTART(IMOD,
     :                             MODEL_SPEC_TGROUP(IMOD,IPAR)) ) THEN

*            Get new value
              VALUE = PARAM(MODEL_SPEC_TSTART(IMOD,
     :                                        MODEL_SPEC_TGROUP(IMOD,
     :                                                          IPAR)))

*            Is it out of range?
              IF ( (VALUE.LT.LB(IPAR)) .OR. (VALUE.GT.UB(IPAR)) ) THEN

*              Coerce bounds?
                IF ( FORCE ) THEN

                  IF ( VALUE .LT. LB(IPAR) ) THEN
                    LB(IPAR) =
     :                LB(MODEL_SPEC_TSTART(IMOD,
     :                                     MODEL_SPEC_TGROUP(IMOD,
     :                                                       IPAR)))
                  ELSE
                    UB(IPAR) =
     :                UB(MODEL_SPEC_TSTART(IMOD,
     :                                     MODEL_SPEC_TGROUP(IMOD,
     :                                                       IPAR)))
                  END IF
                  PARAM(IPAR) = VALUE

*              Report error
                ELSE
                  IF ( VALUE .LT. LB(IPAR) ) THEN
                    CALL MSG_SETC( 'BOUND', 'lower' )
                  ELSE
                    CALL MSG_SETC( 'BOUND', 'upper' )
                  END IF
                  CALL MSG_SETI( 'PAR', IPAR )
                  CALL MSG_SETI( 'TIE', MODEL_SPEC_TGROUP(IMOD,IPAR) )
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'Parameter ^PAR ^BOUND bound '/
     :                 /'violation due to constraint ^TIE', STATUS )

                END IF

*            Otherwise value is in range
              ELSE
                PARAM(IPAR) = VALUE

*            End of range check
              END IF

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
        CALL AST_REXIT( 'FIT_APPTIE', STATUS )
      END IF

      END
