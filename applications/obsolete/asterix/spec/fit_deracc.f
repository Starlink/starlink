*+  FIT_DERIVS_ACCUM - Accumulate derivatives in statistic
      SUBROUTINE FIT_DERIVS_ACCUM( NDS, OBDAT, INSTR, MODEL, FSTAT,
     :             PREDICTOR, PREDDAT, NPAR, PARAM, LB, UB, FROZEN,
     :             DPUP, DPDOWN, N, NDAT, OBS, WT, QOK, OBSQ, PRED,
     :             DDERIV1, DDERIV2, DFDP, PREDUP, PREDDOWN, STATUS )
*
*    Description :
*
*     Accumulates derivatives of chi-squared or Cash's C-statistic w.r.t.
*     the model parameters. Note - only the lower triangle of DDERIV2
*     (i.e. DDERIV2(x,y) for x>=y) is filled in. Bevington approx for
*     DDERIV2 is used.
*
*    Method :
*
*     The expressions for the first and second derivatives of the fit
*     statistic with respect to the fit parameters are summed over all
*     data/model values, taking quality into account. In the likelihood
*     case, zero or negative model values are also trapped.
*
*    History :
*
*      3 Feb 87 : Original (BHVAD::TJP)
*      8 Apr 87 : Double precision accumulation of derivs (TJP)
*     14 Apr 88 : New structures, interfaces change (TJP)
*      8 Jul 88 : Bug fix (TJP)
*     25 Mar 92 : FIT_PREDDAT made external PREDICTOR (RJV)
*     27 May 92 : Added maximum likelihood option (DJA)
*     12 Jun 92 : Added quality (DJA)
*     15 Jun 92 : Added FSTAT argument (DJA)
*     24 Jun 92 : Proofed against PRED<=0 (DJA)
*     19 May 94 : Added handling of constraints (DJA)
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
      RECORD /PREDICTION/ PREDDAT(NDS)	        ! Data predicted by model
      INTEGER             NPAR			! No of parameters
      REAL                PARAM(NPAMAX)		! Model parameters
      REAL                LB(NPAMAX)		! Model lower bounds
      REAL                UB(NPAMAX)		! Model upper bounds
      LOGICAL             FROZEN(NPAMAX)	! Frozen parameter flag
      REAL                DPUP(NPAMAX)		! Actual upper increment
      REAL                DPDOWN(NPAMAX)	! Actual lower increment
      INTEGER             N			! Dataset no
      INTEGER             NDAT			! No of values in dataset
      REAL                OBS(NDAT)		! Observed data
      REAL                WT(NDAT)		! Data weights (1/variances)
      LOGICAL             QOK                   ! Use data quality?
      LOGICAL             OBSQ(NDAT)            ! Data quality
      REAL                PRED(NDAT)		! Array of predicted data
      INTEGER             FSTAT                 ! Statistic to use
      EXTERNAL            PREDICTOR             ! Model data predictor
*
*    Import-Export :
*
      DOUBLE PRECISION    DDERIV1(NPAMAX)       ! Array of stat derivs w.r.t. params
      DOUBLE PRECISION    DDERIV2(NPAMAX,NPAMAX)! Statistic 2nd derivs (approx)
*
*    Export :
*
      REAL                DFDP(NDAT,NPAR)	! Data derivs w.r.t. parameters
      REAL                PREDUP(NDAT)		! Upper & lower predicted datasets
      REAL                PREDDOWN(NDAT)	! for derivative evaluation
*
*    Status :
*
      INTEGER             STATUS
*
*    Local constants :
*
      REAL                BADREP                ! 0 or -ve model replacement
        PARAMETER         ( BADREP = 1.0E-6 )
*
*    Local variables :
*
      REAL                UPPAR(NPAMAX)		! Upper parameter set (for derivs)
      REAL 		  LOPAR(NPAMAX)		! Lower parameter set (for derivs)
      REAL 	          DP			! Param upper-lower

      INTEGER             I			! Data index
      INTEGER             J,K			! Parameter indices

      LOGICAL             MAXL                  ! Maximum likelihood?
      LOGICAL             POK                   ! Model value is ok?
*-

*    Status check
      IF ( STATUS.NE.SAI__OK ) RETURN

*    Using maximum likelihood
      MAXL = (FSTAT .EQ. FIT__LOGL)

*    Initialise parameter arrays and accumulators
      DO J = 1, NPAR
	UPPAR(J) = PARAM(J)
	LOPAR(J) = PARAM(J)
      END DO

*    Evaluate predicted data for upper and lower increments
*    in each unfrozen param
      DO J = 1, NPAR
	IF ( .NOT. FROZEN(J) ) THEN

*        Create parameter sets for lower and upper deviates of parameter J
	  UPPAR(J) = PARAM(J) + DPUP(J)
	  LOPAR(J) = PARAM(J) - DPDOWN(J)

*        Keep constrained parameters up to date
          IF ( MODEL.NTIE .GT. 0 ) THEN
            CALL FIT_APPTIE( MODEL, .FALSE., LOPAR, LB, UB, STATUS )
            CALL FIT_APPTIE( MODEL, .FALSE., UPPAR, LB, UB, STATUS )
          END IF

	  DP = DPUP(J) + DPDOWN(J)
	  CALL PREDICTOR(FSTAT,NDS,OBDAT,INSTR,PREDDAT,MODEL,UPPAR,N,
     :                                                 PREDUP,STATUS)
	  CALL PREDICTOR(FSTAT,NDS,OBDAT,INSTR,PREDDAT,MODEL,LOPAR,N,
     :                                               PREDDOWN,STATUS)

*        Find derivatives of Cash statistic?
          IF ( MAXL ) THEN

*          Quality present?
            IF ( QOK ) THEN

	      DO I = 1, NDAT
                IF ( OBSQ(I) ) THEN
	          DFDP(I,J)=(PREDUP(I)-PREDDOWN(I))/DP
                  POK = (PRED(I).GT.0.0)

*                Replace zero or negative model with BADREP if observed
*                data is positive. If observed data is zero then there
*                is no contribution to the first derivative.
                  IF ( POK ) THEN
	            DDERIV1(J) = DDERIV1(J) +
     :                 DBLE(1.0D0-OBS(I)/PRED(I))*DFDP(I,J)
                  ELSE IF ( OBS(I) .GT. 0.0 ) THEN
	            DDERIV1(J) = DDERIV1(J) +
     :                 DBLE(1.0D0-OBS(I)/BADREP)*DFDP(I,J)
                  END IF

*                Second Cash derivs
	          DO K = 1, J
	            IF ( .NOT. FROZEN(K) ) THEN
                      IF ( POK ) THEN
	                DDERIV2(J,K) = DDERIV2(J,K) +
     :                    DBLE(DFDP(I,J)*DFDP(I,K)*OBS(I)/PRED(I)**2)
                      ELSE IF ( OBS(I) .GT. 0.0 ) THEN
	                DDERIV2(J,K) = DDERIV2(J,K) +
     :                    DBLE(DFDP(I,J)*DFDP(I,K)*OBS(I)/BADREP**2)
                      END IF
	            END IF
	          END DO
                END IF
	      END DO

            ELSE

	      DO I = 1, NDAT
	        DFDP(I,J)=(PREDUP(I)-PREDDOWN(I))/DP
                POK = (PRED(I).GT.0.0)

*              Replace zero or negative model with BADREP if observed
*              data is positive. If observed data is zero then there
*              is no contribution to the first derivative.
                IF ( POK ) THEN
	          DDERIV1(J) = DDERIV1(J) +
     :               DBLE(1.0D0-OBS(I)/PRED(I))*DFDP(I,J)
                ELSE IF ( OBS(I) .GT. 0.0 ) THEN
	          DDERIV1(J) = DDERIV1(J) +
     :               DBLE(1.0D0-OBS(I)/BADREP)*DFDP(I,J)
                END IF

*              Second Cash derivs
	        DO K = 1, J
	          IF ( .NOT. FROZEN(K) ) THEN
                    IF ( POK ) THEN
	              DDERIV2(J,K) = DDERIV2(J,K) +
     :                  DBLE(DFDP(I,J)*DFDP(I,K)*OBS(I)/PRED(I)**2)
                    ELSE IF ( OBS(I) .GT. 0.0 ) THEN
	              DDERIV2(J,K) = DDERIV2(J,K) +
     :                  DBLE(DFDP(I,J)*DFDP(I,K)*OBS(I)/BADREP**2)
                    END IF
	          END IF
	        END DO

	      END DO

            END IF

*        Chi-squared statistic
          ELSE

*          Data and chisq derivs
	    DO I = 1, NDAT
	      DFDP(I,J)=(PREDUP(I)-PREDDOWN(I))/DP
	      DDERIV1(J)=DDERIV1(J)+DBLE(WT(I)*(PRED(I)-OBS(I))*
     :        DFDP(I,J))

*            Second chisq derivs
	      DO K=1,J
	        IF(.NOT.FROZEN(K))THEN
	          DDERIV2(J,K)=DDERIV2(J,K)+DBLE(WT(I)*DFDP(I,J)*
     :            DFDP(I,K))
	        END IF
	      END DO
	    END DO
          END IF

*        Reset parameter J
	  UPPAR(J)=PARAM(J)
	  LOPAR(J)=PARAM(J)

	END IF
      END DO

*    Exit
 99   IF ( STATUS.NE.SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_DERIVS_ACCUM', STATUS )
      END IF

      END
