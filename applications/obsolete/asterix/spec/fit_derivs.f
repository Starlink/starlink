*+  FIT_DERIVS - Evaluates 1st and 2nd derivs of fit statistic wrt. model params
      SUBROUTINE FIT_DERIVS(NDS,OBDAT,INSTR,MODEL,NPAR,PARAM,LB,UB,
     :    DPAR,FROZEN,FSTAT,PREDICTOR,PREDDAT,DERIV1,DERIV2,STATUS)
*
*    Description :
*
*     First and second derivatives of chi-squared or maximum likelihood fit
*     between observed data and the data predicted by the model are evaluated
*     with respect to the model parameters (apart from those which are frozen).
*
*    Method :
*
*     First derivatives are evaluated from differences, using parameter
*     increments DPAR. The evaluation is centred provided that this would not
*     involve crossing one of the parameter bounds; in this case the increment
*     is truncated at the bound.
*     The second derivatives are evaluated using the first order approximation
*     of Bevington. This is valid provided that the gradients of the data
*     values w.r.t. the parameters do not undergo large fractional changes
*     over the data interval separating observed and predicted data.
*      i.e. ABS[ (Fobs-Fpred)*(d/dp1(dF/dp2)) ] << (dF/dp1)*(dF/dp2)
*     where p1 and p2 are any two model parameters and F represents the data
*     values. This approximation saves a lot of model evaluations, especially
*     if the number of parameters is fairly large (>4).
*     NOTE: Care must be taken to set up the work arrays (whose pointers are
*     stored in PREDDAT) correctly before this routine is called.
*     If N is the length of the dataset being fitted and NPAR the number of
*     model parameters, then the two arrays pointed to by PREDPTR must each
*     be at least of length N, and the DFDP array must be at least N*NPAR.
*     NOTE: It is assumed that the current predicted data values are already
*     available in the PREDDAT structure (from previous call to FIT_STAT).
*
*    Deficiencies :
*
*     Second derivatives are only approximate, they are therefore unsuitable
*     for some purposes, e.g. evaluating errors from the curvature of the
*     chi-squared surface.
*
*    Bugs :
*    Authors :
*
*     Trevor Ponman (BHVAD::TJP)
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      3 Feb 87 : Original (BHVAD::TJP)
*      8 Apr 87 : Double precision accumulation of derivs (TJP)
*     29 Apr 87 : Parameter increments passed in (TJP)
*     14 Apr 88 : Changed structures, global eliminated (TJP)
*      8 Jul 88 : Bug causing _DERIVS_ACCUM crash fixed (TJP)
*     25 Mar 92 : FIT_PREDDAT made external PREDICTOR (RJV)
*     27 May 92 : Changes to do maximum likelihood fitting. Error handling
*                 corrected. Use D.P. so no REAL accumulator needed (DJA)
*     12 Jun 92 : Pass observed data quality to FIT_DERIVS_ACCUM (DJA)
*     15 Jun 92 : Added FSTAT argument (DJA)
*     15 Mar 94 : Allocate DFDPPTR in routine if set to VAL__BADI (DJA)
*     19 May 94 : Added constraint handling (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
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
      REAL                PARAM(NPAMAX)		! Model parameters
      REAL                LB(NPAMAX)		! Parameter lower bounds
      REAL                UB(NPAMAX)		! Parameter upper bounds
      REAL                DPAR(NPAMAX)		! Desired parameter increments
      LOGICAL             FROZEN(NPAMAX)	! Frozen parameter flag
      INTEGER             FSTAT                 ! Statistic to use
      EXTERNAL            PREDICTOR		! Prediction routine
*
*    Import-Export :
*
      RECORD /PREDICTION/ PREDDAT(NDS)	        ! Data predicted by model
                                                ! (actually only the data
*						! pointed to are updated)
*    Export :
*
      DOUBLE PRECISION    DERIV1(NPAMAX)	! Statistic derivs wrt. params
      DOUBLE PRECISION    DERIV2(NPAMAX,NPAMAX)	! Matrix of 2nd derivs (approx.)
*
*    Status :
*
      INTEGER             STATUS
*
*    Local variables :
*
      REAL                DPUP(NPAMAX)		! Actual upper increment
      REAL                DPDOWN(NPAMAX)	! Actual lower increment

      INTEGER             J,K			! Parameter indices
      INTEGER             N			! Dataset index
*-

*    Status check
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Set up upper and lower increments for parameters, taking bounds into account
      DO J=1,NPAR
	IF ( .NOT. FROZEN(J) ) THEN
	  IF ( PARAM(J)+DPAR(J) .LE. UB(J) )THEN
	    DPUP(J) = DPAR(J)
	  ELSE
	    DPUP(J) = UB(J)-PARAM(J)
	  END IF
	  IF ( PARAM(J)-DPAR(J) .GE. LB(J) ) THEN
	    DPDOWN(J) = DPAR(J)
	  ELSE
	    DPDOWN(J) = PARAM(J)-LB(J)
	  END IF
	END IF
      END DO

*    Zero derivative accumulators
      DO J = 1, NPAR
	DERIV1(J) = 0.0D0
	DO K = 1, NPAR
	  DERIV2(J,K) = 0.0D0
	END DO
      END DO

*    Call subroutine to accumulate contributions to the derivs from each dataset
      DO N = 1, NDS

*      Derivatives array wasn't pre-allocated
        IF ( PREDDAT(N).DFDPPTR .EQ. VAL__BADI ) THEN
          CALL DYN_MAPR( 1, PREDDAT(N).NMDAT*NPAR, PREDDAT(N).DFDPPTR,
     :                   STATUS )
        END IF

*      Check workspace allocated
	IF((PREDDAT(N).PREDPTR(1).EQ.0).OR.(PREDDAT(N).PREDPTR(2).EQ.0)
     :    .OR.(PREDDAT(N).DFDPPTR.EQ.0))THEN
	  STATUS = SAI__ERROR
          CALL ERR_REP('NOWKSP','No workspace available for statistic'//
     :      ' gradient calculation',STATUS)
	  GOTO 99
	END IF

D	print *,'iteration',n,'   - entering fit_derivs_accum'
	CALL FIT_DERIVS_ACCUM(NDS,OBDAT,INSTR,MODEL,FSTAT,PREDICTOR,
     :    PREDDAT,NPAR,PARAM,LB,UB,FROZEN,DPUP,DPDOWN,N,OBDAT(N).NDAT,
     :    %VAL(OBDAT(N).DPTR),%VAL(OBDAT(N).WPTR),
     :    OBDAT(N).QFLAG, %VAL(OBDAT(N).QPTR),
     :    %VAL(PREDDAT(N).DPTR),DERIV1,DERIV2,%VAL(PREDDAT(N).DFDPPTR),
     :    %VAL(PREDDAT(N).PREDPTR(1)),%VAL(PREDDAT(N).PREDPTR(2)),
     :    STATUS)

      END DO

*    Double to obtain the derivatives, and fill in upper triangle of DERIV2
      DO K = 1, NPAR
        DERIV1(K) = 2*DERIV1(K)
	DO J = 1, NPAR
	  IF ( K .LE. J ) THEN
	    DERIV2(J,K) = 2*DERIV2(J,K)
	  ELSE
	    DERIV2(J,K) = DERIV2(K,J)
	  END IF
	END DO
      END DO

*    Exit
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_DERIVS', STATUS )
      END IF

      END
