*+  FIT_STATDERIV - Calculates deriv of statistic w.r.t. a parameter
      SUBROUTINE FIT_STATDERIV(NDS,OBDAT,INSTR,MODEL,NPAR,PARAM,LB,UB,
     :	PARNO,DELTAP,FSTAT,PREDICTOR,PREDDAT,DERIV,STATUS)
*
*    Description :
*
*     Calls FIT_STAT to compute a difference approximation to the derivative
*     of the fit statistic with respect to one model parameter at a given
*     point in parameter space. The derivative is centred
*     (ie. (f(x+d)-f(x-d))/2d ) unless parameter bounds prevent this.
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Trevor Ponman (BHVAD::TJP)
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     21 May 87 : Original (adapted from SPEC_CHIDERIV)
*      3 May 88 : New structures, global eliminated (TJP)
*     25 Mar 92 : Renamed, FIT_PREDDAT made external PREDICTOR (RJV)
*     28 May 92 : Documentation updated for max-l fitting. Tidied and
*                 derivative returned in D.P. (DJA)
*     15 Jun 92 : Added FSTAT parameter (DJA)
*     18 Aug 92 : Statistic now double precision (DJA)
*     19 May 94 : Handles constrained fitting (DJA)
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
      INTEGER             NDS			! Number of observed datasets
      RECORD /DATASET/    OBDAT(NDS)		! Observed datasets
      RECORD /INSTR_RESP/ INSTR(NDS)		! Instrument responses
      RECORD /MODEL_SPEC/ MODEL			! Model specification
      INTEGER             NPAR			! No of parameters
      REAL                PARAM(NPAMAX)		! Model params at which deriv is centred
      REAL                LB(NPAMAX)		! Parameter lower bounds
      REAL                UB(NPAMAX)		! Parameter upper bounds
      INTEGER             PARNO			! Parameter along which deriv is wanted
      REAL                DELTAP		! Diff. increment (scaled) for param
      INTEGER             FSTAT                 ! Statistic to use
      EXTERNAL            PREDICTOR		! Predicted data routine
*
*    Import/export :
*
      RECORD /PREDICTION/ PREDDAT(NDS)		! Data predicted by model
                                                ! (actually only the data
*						! pointed to are updated)
*    Export :
*
      DOUBLE PRECISION    DERIV			! Derivative
*
*    Status :
*
      INTEGER             STATUS
*
*    Local variables :
*
      DOUBLE PRECISION    STATUP		! Statistic at upper param set
      DOUBLE PRECISION    STATDOWN		! Statistic at lower param set

      REAL                DPDOWN		! Lower param increment
      REAL                DPUP			! Upper param increment
      REAL                PARCAL(NPAMAX)	! Param values for stat eval'n

      INTEGER             J			! Parameter index
*-

*    Status check
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialisation
      DO J = 1, NPAR
	PARCAL(J) = PARAM(J)
      END DO

*    Check that derivative can be calculated - else error condition
      IF ( (DELTAP.LE.0.0) .OR. (LB(PARNO).GE.UB(PARNO)) )THEN
	STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Unable to calculate statistic derivative',
     :                                                        STATUS )
	GOTO 9000
      END IF

*    Ensure param increments don't cross bounds
      IF ( PARAM(PARNO)+DELTAP .LE. UB(PARNO) ) THEN
	DPUP = DELTAP
      ELSE
	DPUP = UB(PARNO)-PARAM(PARNO)
      END IF
      IF ( PARAM(PARNO)-DELTAP .GE. LB(PARNO) ) THEN
	DPDOWN = DELTAP
      ELSE
	DPDOWN = PARAM(PARNO)-LB(PARNO)
      END IF

*    Calculate statistic at +ve and -ve deviates in parameter PARNO
      PARCAL(PARNO) = PARAM(PARNO) + DPUP
      IF ( MODEL.NTIE .GT. 0 ) THEN
        CALL FIT_APPTIE( PARCAL, .FALSE., PARCAL, LB, UB, STATUS )
      END IF
      CALL FIT_STAT( NDS, OBDAT, INSTR, MODEL, PARCAL, FSTAT, PREDICTOR,
     :                                         PREDDAT, STATUP, STATUS )
      PARCAL(PARNO) = PARAM(PARNO) - DPDOWN
      IF ( MODEL.NTIE .GT. 0 ) THEN
        CALL FIT_APPTIE( PARCAL, .FALSE., PARCAL, LB, UB, STATUS )
      END IF
      CALL FIT_STAT( NDS, OBDAT, INSTR, MODEL, PARCAL, FSTAT, PREDICTOR,
     :                                       PREDDAT, STATDOWN, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 9000

*    Form difference approximation to derivative
      DERIV = (STATUP-STATDOWN)/(DBLE(DPUP)+DBLE(DPDOWN))

*    Exit
 9000 IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_STATDERIV', STATUS )
      END IF

      END
