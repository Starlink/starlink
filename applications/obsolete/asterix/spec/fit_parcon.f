*+  FIT_PARCON - Evaluates confidence intervals for some model parameters
      SUBROUTINE FIT_PARCON( NDS, OBDAT, INSTR, MODEL, SOFF, OPCHAN,
     :                       NEPAR, EPAR, NITMAX, MINSLO, NPAR, LB,
     :                       UB, FROZEN, SSCALE, DPAR, FSTAT, PREDICTOR,
     :                       PREDDAT, STATMIN, PARAM, PEGGED, LE, UE,
     :                       PEGCODE, STATUS )
*
*    Description :
*
*     Derives a confidence interval corresponding to a chi_squared increase
*     of SOFF (with optimisation of all other parameters) for each free
*     parameter of a fit_model. If the number of free parameters (apart from
*     that whose interval is being evaluated) changes as a result of parameter
*     bounds being encountered, then a warning is returned by setting PEGCODE
*     as follows:
*               PEGCODE(i) = 0	no problems
*               PEGCODE(i) = 1	parameter pegged in evaluation of lower bound
*               PEGCODE(i) = 2   ......  unpegged ...........................
*               PEGCODE(i) = 3  parameter pegged in evaluation of upper bound
*               PEGCODE(i) = 4   ......  unpegged ...........................
*		PEGCODE(i) = 5  parameters pegged for both bounds
*		PEGCODE(i) = 6  parameters unpegged for both bounds
*		PEGCODE(i) = 7  parameter pegged on lower & unpegged on upper
*		PEGCODE(i) = 8  parameter unpegged on lower & pegged on upper
*
*     For parameters on bounds the confidence interval across the bound is
*     returned as the negative of the trial value entered (so that this can
*     be recovered later if required).
*     Progress is reported to logical unit OPCHAN, if set >0.
*     If a lower value of chi-squared than the supposed minimum is found, then
*     STATUS=USER__001 is returned. If a fitting error ocurrs then
*     STATUS=SAI__ERROR is returned.
*
*    Method :
*
*     Each parameter in turn is adjusted (according to the initial guesses
*     LE and UE passed into the program) and frozen. FIT_MIN is then called
*     to evaluate the optimum fit obtainable by allowing the other parameters
*     to vary. From the actual chi-squared values attained, the parameter
*     offset giving chi-sq. min increase of SOFF is estimated on the assumption
*     that the minimum chi-squared surface is a parabolic function of the
*     parameter whose error is being evaluated. The parabola may have a
*     different shape on each side of the minimum, the curve on each side
*     being defined by the assumption of zero slope at the minimum, and the
*     requirement that it should pass through the single offset point
*     evaluated.  e.g.
*
*           .                             !
*            *                            !            .
*             .                           !
*               .<----------------------->!<--------->.      -
*                  .                      !          *       ^
*                       .                 !        .        SOFF
*                              .          !     .            ^
*                                         M                  ^
*
*      where * are the evaluated points, and the resulting confidence limits
*      are shown by <---->.
*
*      From Lampton, Margon & Bowyer (Ap.J. 208, p177, 1976) a value SOFF=1
*      gives a 1 sigma (i.e. 68% confidence) interval and SOFF=2.71 a 90%
*      confidence interval for the case where the model is linear (i.e. the
*      predicted values are linear functions of the parameters). Where this
*      is not the case (i.e. always!) the results may still be approximately
*      right (see LMB and also Avni Ap.J. 210, p642, 1976).
*      A safe limit in all cases is obtained by projecting the full
*      NPAR-dimensional confidence interval onto the subspace desired (a single
*      parameter axis in this case). This corresponds to taking
*         SOFF = chi-squared with NPAR d.o.f.( n% conf)
*      for a safe upper bound on the n% confidence limit. For large NPAR this
*      will be way above the linear approximation result.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Trevor Ponman (BHVAD::TJP)
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     24 Jul 87 : Original (BHVAD::TJP)
*      6 Aug 87 : Sign flip for unavailable errors (TJP)
*      9 Nov 87 : Bug in pegcode setting fixed (TJP)
*     20 Jun 88 : New structures, global eliminated (TJP)
*     26 Mar 92 : FIT_PREDDAT passed as external PREDICTOR (RJV)
*     28 May 92 : Updated for maximum likelihood fitting. Error handling
*                 corrected (DJA)
*     15 Jun 92 : Added FSTAT and changed NDOF to SSCALE (DJA)
*     25 Jun 92 : Error handling fixed (TJP)
*     18 Aug 92 : Statistic now double precision (DJA)
*     13 Jan 93 : NEPAR and EPAR arrays allows error calculation to be
*                 restricted to specific parameters (DJA)
*     19 May 94 : Updated to handle constrained fitting (DJA)
*     12 Jan 95 : Fixed deleted line causing FFROZEN to be uninitialised (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'
      INCLUDE 'FIT_PAR'
*
*    Structure declarations :
*
      INCLUDE 'FIT_STRUC'
*
*    Import :
*
      INTEGER             NDS			! Number of observed datasets
      RECORD /DATASET/    OBDAT(NDS)		! Observed datasets
      RECORD /INSTR_RESP/ INSTR(NDS)		! Instrument responses
      RECORD /MODEL_SPEC/ MODEL			! Model specification
      REAL                SOFF			! Stat offset for conf. limit
      INTEGER             OPCHAN		! Output channel for diagnostic
	                                        ! messages ( <1 for no messages)
      INTEGER             NEPAR                 ! No of parameters for errors
      INTEGER             EPAR(NEPAR)           ! Parameter nos for errors
      INTEGER             NITMAX		! Max. no of iterations for each fit
      REAL                MINSLO		! Statistic slope threshold for
					        ! termination in FIT_MIN
      INTEGER             NPAR			! No of parameters
      REAL                LB(NPAR)		! Parameter lower bounds
      REAL                UB(NPAR)		! Parameter upper bounds
      LOGICAL             FROZEN(NPAR)		! Frozen parameter flag
      INTEGER             SSCALE		! Statistic scale factor
      REAL                DPAR(NPAR)		! Param increments for differencing
      INTEGER             FSTAT                 ! Statistic to use
      EXTERNAL            PREDICTOR             ! Model predictor
*
*    Import-Export  :
*
      RECORD /PREDICTION/ PREDDAT(NDS)	        ! Data predicted by model
                                                ! (actually only the data
                                                ! pointed to are updated)
      DOUBLE PRECISION    STATMIN		! Statistic at minimum     { only updated
      REAL                PARAM(NPAR)		! Model params at min.  { if lower min
      LOGICAL             PEGGED(NPAR)		! Parameter pegged      { is found
      REAL                LE(NPAR)		! Lower error estimate
      REAL                UE(NPAR)		! Upper error estimate
      INTEGER             PEGCODE(NPAR)		! Pegging code (flags pegging changes)
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      DOUBLE PRECISION    STAT			! Statistic at offset position

      REAL                FPAR(NPAMAX)		! Model parameters from fitting

      INTEGER             FITERR		! Fitting error encountered
      INTEGER             EJ,J,K		! Parameter indices
      INTEGER             LEPAR(NPAMAX)         ! Parameter to have errors done
      INTEGER             NIT			! Iteration number (for FIT_MIN)
      INTEGER             NUNFROZEN		! No of unfrozen parameters

      LOGICAL             FPEGGED(NPAMAX)	! Parameters pegged in fitting
      LOGICAL             FFROZEN(NPAMAX)	! Frozen flags for fitting
      LOGICAL             FINISHED		! Minimum found
      LOGICAL             INITIALISE		! Initialise FIT_MIN?
      LOGICAL             LFROZEN(NPAMAX)	! Frozen flags with constraints
*
*    Local data :
*
      DATA                INITIALISE /.FALSE./
*-

*    Status check
      IF ( STATUS.NE.SAI__OK ) RETURN

*    Local frozen array
      CALL FIT1_LOCFRO( MODEL, NPAR, FROZEN, LFROZEN, STATUS )

*    Initialise
      NUNFROZEN=0
      DO J = 1, NPAR
	FPEGGED(J) = PEGGED(J)
	FROZEN(J) = LFROZEN(J)
	FFROZEN(J) = FROZEN(J)
	IF ( .NOT. LFROZEN(J) ) NUNFROZEN = NUNFROZEN + 1
	FPAR(J) = PARAM(J)
      END DO

*    Load the LEPAR array of parameters whose error are to be calculated
      IF ( NEPAR .EQ. NPAR ) THEN
        DO J = 1, NPAR
          LEPAR(J) = J
        END DO
      ELSE
        DO J = 1, NEPAR
          LEPAR(J) = EPAR(J)
        END DO
      END IF

*    Loop through parameters
      DO EJ = 1, NEPAR

*      Get parameter number for error
        J = LEPAR(EJ)

*      Only errors for free and unconstrained parameters
	IF ( .NOT. LFROZEN(J) ) THEN

*        Freeze parameter to test statistic at offsets
	  FFROZEN(J) = .TRUE.

*        Lower error
	  IF ( OPCHAN .GT. 0 ) THEN
	    WRITE(OPCHAN,*) '- Lower bound for parameter ',J
	  END IF

*        Check LE
	  IF ( LE(J) .EQ. 0.0 ) THEN
	    STATUS = SAI__ERROR
	    CALL ERR_REP( ' ', 'Lower error of zero entered', STATUS )
	    GOTO 9000
	  ELSE IF ( LE(J) .LT. 0.0 ) THEN
	    LE(J) = -LE(J)		! Flip sign back to get +ve offset
	  ENDIF
 100	  FPAR(J) = PARAM(J) - LE(J)

*        Check against bound
	  IF ( PARAM(J) .EQ. LB(J) ) THEN
	    LE(J) = -LE(J)		! No error available - flip sign to flag
	    IF ( OPCHAN .GT. 0 ) THEN
	      WRITE(OPCHAN,*) '  Parameter on bound'
	    END IF

	  ELSE

*          Check if on lower bound - if it is adjuts both the value and error
	    IF ( FPAR(J) .LT. LB(J) ) THEN
	      FPAR(J) = LB(J)
	      LE(J) = PARAM(J) - LB(J)
	    END IF

*          Ensure dependencies are up to date
            CALL FIT1_COPDEP( MODEL, FPAR, STATUS )
            CALL FIT1_COPDEPL( MODEL, FFROZEN, STATUS )

*          Get initial chi-squared value
	    CALL FIT_STAT( NDS, OBDAT, INSTR, MODEL, FPAR, FSTAT,
     :                         PREDICTOR, PREDDAT, STAT, STATUS )

*          Catch case where there is only one parameter (no minimisation needed)
	    IF ( NUNFROZEN .EQ. 1 ) THEN
	        IF(OPCHAN.GT.0)THEN
	          WRITE(OPCHAN,*) '  No parameters to optimise'
	        ENDIF
	        GO TO 1000
	      ENDIF
*        Find minimum
	      INITIALISE=.TRUE.
	      CALL FIT_MIN(NDS,OBDAT,INSTR,MODEL,0,NITMAX,NPAR,LB,UB,
     :         FFROZEN,SSCALE,INITIALISE,MINSLO,FSTAT,PREDICTOR,PREDDAT,
     :         FPAR,DPAR,FPEGGED,STAT,NIT,FINISHED,FITERR,STATUS)
	      IF(FITERR.NE.0)THEN
	        CALL MSG_SETI('FERR',FITERR)
	        CALL MSG_SETI('NPAR',J)
	        STATUS=SAI__ERROR
	        CALL ERR_REP('LB_ERR','Fitting error ^FERR in evaluating '//
     :          'lower bound for parameter ^NPAR',STATUS)
*        Warning if minimum not found
	      ELSE IF(OPCHAN.GT.0.AND.(.NOT.FINISHED))THEN
	        WRITE(OPCHAN,*) '! Minimum not achieved after max. no'//
     :          ' of iterations'
	      ENDIF
	      IF(STATUS.NE.SAI__OK) CALL ERR_FLUSH(STATUS)

*        If new minimum is <STATMIN then update params etc & return bad STATUS
 1000	      IF(STAT.LT.STATMIN)THEN
	        STATMIN=STAT
	        DO K=1,NPAR
	          PARAM(K)=FPAR(K)
	          PEGGED(K)=FPEGGED(K)
	        ENDDO
	        STATUS=USER__001
	        GO TO 9000
*        If it's no higher then try again with larger parameter offset
	      ELSE IF(STAT.EQ.STATMIN)THEN
	        LE(J)=3*LE(J)
	        IF(OPCHAN.GT.0)THEN
	          WRITE(OPCHAN,*) '  Chi-squared not increased - trying'//
     :            ' with increased parameter offset'
	        ENDIF
	        GO TO 100
	      ENDIF
*        Evaluate parameter 'error' for confidence SOFF (parabolic assumption)
D	      print *,'le,soff,stat,statmin: ',le(j),soff,stat,statmin
	      LE(J)=LE(J)*SQRT(SOFF/(STAT-STATMIN))
D	      print *,'new le: ',le(j)
*        Check for change in parameter pegging and set flag if necessary
	      PEGCODE(J)=0
	      DO K=1,NPAR
	        IF((.NOT.LFROZEN(K)).AND.(K.NE.J))THEN
	          IF((.NOT.PEGGED(K)).AND.FPEGGED(K))THEN
	            PEGCODE(J)=1
	          ELSE IF(PEGGED(K).AND.(.NOT.FPEGGED(K)))THEN
	            PEGCODE(J)=2
	          ENDIF
	        ENDIF
	      ENDDO
	    ENDIF

*        Upper error
	  IF ( OPCHAN .GT. 0 ) THEN
	    WRITE(OPCHAN,*) '- Upper bound for parameter ', J
	  END IF

*        Reset FPAR
          CALL ARR_COP1R( NPAR, PARAM, FPAR, STATUS )

*        Check UE
	    IF(UE(J).EQ.0.0)THEN
	      STATUS = SAI__ERROR
	      CALL ERR_REP(' ', 'Upper error of zero entered', STATUS )
	      GO TO 9000
	    ELSE IF(UE(J).LT.0.0)THEN
	      UE(J)=-UE(J)		! Flip sign back to get +ve offset
	    ENDIF
 1100	    FPAR(J)=PARAM(J)+UE(J)
*        Check against bound
	    IF(PARAM(J).EQ.UB(J))THEN
	      UE(J)=-UE(J)		! No error available - flip sign to flag
	      IF(OPCHAN.GT.0)THEN
	        WRITE(OPCHAN,*) '  Parameter on bound'
	      ENDIF
	    ELSE
	      IF(FPAR(J).GT.UB(J))THEN
	        FPAR(J)=UB(J)		! Set at upper boundary
	        UE(J)=UB(J)-PARAM(J)
	      ENDIF

*            Ensure dependencies are up to date
              CALL FIT1_COPDEP( MODEL, FPAR, STATUS )
              CALL FIT1_COPDEPL( MODEL, FFROZEN, STATUS )

*        Get initial chi-squared value
	      CALL FIT_STAT(NDS,OBDAT,INSTR,MODEL,FPAR,FSTAT,
     :                          PREDICTOR,PREDDAT,STAT,STATUS)

*        Catch case where there is only one parameter (no minimisation needed)
	      IF(NUNFROZEN.EQ.1)THEN
	        IF(OPCHAN.GT.0)THEN
	          WRITE(OPCHAN,*) '  No parameters to optimise'
	        ENDIF
	        GO TO 2000
	      ENDIF
*        Find minimum
	      INITIALISE=.TRUE.
	      CALL FIT_MIN(NDS,OBDAT,INSTR,MODEL,0,NITMAX,NPAR,LB,UB,
     :         FFROZEN,SSCALE,INITIALISE,MINSLO,FSTAT,PREDICTOR,PREDDAT,
     :         FPAR,DPAR,FPEGGED,STAT,NIT,FINISHED,FITERR,STATUS)
	      IF(FITERR.NE.0)THEN
	        CALL MSG_SETI('FERR',FITERR)
	        CALL MSG_SETI('NPAR',J)
	        STATUS=SAI__ERROR
	        CALL ERR_REP('UB_ERR','Fitting error ^FERR in evaluating '//
     :          'upper bound for parameter ^NPAR',STATUS)
*        Warning if minimum not found
	      ELSE IF(OPCHAN.GT.0.AND.(.NOT.FINISHED))THEN
	        WRITE(OPCHAN,*) '! Minimum not achieved after max. no'//
     :          ' of iterations'
	      ENDIF
	      IF(STATUS.NE.SAI__OK) CALL ERR_FLUSH(STATUS)
*        If new minimum is <STATMIN then update params etc & return bad STATUS
 2000	      IF(STAT.LT.STATMIN)THEN
	        STATMIN=STAT
	        DO K=1,NPAR
	          PARAM(K)=FPAR(K)
	          PEGGED(K)=FPEGGED(K)
	        ENDDO
	        STATUS=USER__001
	        GO TO 9000
*        If it's no higher then try again with larger parameter offset
	      ELSE IF(STAT.EQ.STATMIN)THEN
	        UE(J)=3*UE(J)
	        IF(OPCHAN.GT.0)THEN
	          WRITE(OPCHAN,*) '  Chi-squared not increased - trying'//
     :            ' with increased parameter offset'
	        ENDIF
	        GO TO 1100
	      ENDIF

*        Evaluate parameter 'error' for confidence SOFF (parabolic assumption)
	      UE(J)=UE(J)*SQRT(SOFF/(STAT-STATMIN))

*        Check for change in parameter pegging and set flag if necessary
	      DO K=1,NPAR
	        IF((.NOT.LFROZEN(K)).AND.(K.NE.J))THEN
	          IF(PEGCODE(K).EQ.0)THEN
	            IF((.NOT.PEGGED(K)).AND.FPEGGED(K))THEN
	              PEGCODE(K)=3
	            ELSE IF(PEGGED(K).AND.(.NOT.FPEGGED(K)))THEN
	              PEGCODE(K)=4
	            ENDIF
	          ELSE IF(PEGCODE(K).EQ.1)THEN
	            IF((.NOT.PEGGED(K)).AND.FPEGGED(K))THEN
	              PEGCODE(K)=5
	            ELSE IF(PEGGED(K).AND.(.NOT.FPEGGED(K)))THEN
	              PEGCODE(K)=7
	            ENDIF
	          ELSE IF(PEGCODE(K).EQ.2)THEN
	            IF((.NOT.PEGGED(K)).AND.FPEGGED(K))THEN
	              PEGCODE(K)=6
	            ELSE IF(PEGGED(K).AND.(.NOT.FPEGGED(K)))THEN
	              PEGCODE(K)=8
	            ENDIF
	          ENDIF
	        ENDIF
	      ENDDO
	    ENDIF

*        Reset for next pass through loop
	  FFROZEN(J) = FROZEN(J)
	  FPAR(J) = PARAM(J)

*      End of frozen check
	END IF

*    Next parameter whose error to be found
      END DO

*    Ensure dependent parameters are set up
      CALL FIT1_COPDEP( MODEL, LE, STATUS )
      CALL FIT1_COPDEP( MODEL, UE, STATUS )

*    Exit
 9000 IF ( (STATUS.NE.SAI__OK) .AND. (STATUS.NE.USER__001) ) THEN
        CALL AST_REXIT( 'FIT_PARCON', STATUS )
      END IF

      END
