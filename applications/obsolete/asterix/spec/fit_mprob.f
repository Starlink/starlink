*+  FIT_MPROB - Finds probability that observed data is drawn from model
      SUBROUTINE FIT_MPROB( NDS, OBDAT, FSTAT, SSCALE, PREDDAT,
     :                      STATMIN, FPROB, STATUS )
*
*    Description :
*
*
*    Method :
*
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Trevor Ponman (BHVAD::TJP)
*     David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*     30 Aug 94 : Original (DJA)
*     14 Jun 95 : Iteration scheme changed to stabilise expectation value
*                 calculation, and Gaussian limit simplification added (DJA)
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
*    Structure declarations :
*
      INCLUDE 'FIT_STRUC'
*
*    Import :
*
      INTEGER             	NDS			! Number of observed datasets
      RECORD /DATASET/    	OBDAT(NDS)		! Observed datasets
      INTEGER             	SSCALE			! Statistic scale factor
      INTEGER             	FSTAT                 	! Statistic to use
      RECORD /PREDICTION/ 	PREDDAT(NDS)	        ! Data predicted by
							! model
      DOUBLE PRECISION    	STATMIN			! Statistic at minimum
*
*    Export :
*
      DOUBLE PRECISION 		FPROB			! Fit probability
*
*    Function declarations :
*
      DOUBLE PRECISION 		G01BCF
      DOUBLE PRECISION 		G01EAF
*
*
*    Status :
*
      INTEGER 			STATUS
*
*    Local variables :
*
      DOUBLE PRECISION		LFPD, LFPDV		! Fit probability and
							! variance for data
      DOUBLE PRECISION		LFPT			! log P~
      DOUBLE PRECISION		LFPM			! Fit probability for
							! model data
      INTEGER			N			! Loop over datasets
      INTEGER			NAGSTAT			! NAG status
*-

*    Status check
      IF ( STATUS.NE.SAI__OK ) RETURN

*    Chi-squared case
      IF ( FSTAT .EQ. FIT__CHISQ ) THEN

*      Get probability from NAG routine
        NAGSTAT = 0
        FPROB = G01BCF( STATMIN, SSCALE, NAGSTAT )

*    Likelihood
      ELSE IF ( FSTAT .EQ. FIT__LOGL ) THEN

*      Loop over datasets
        LFPM = 0.0D0
        LFPD = 0.0D0
        LFPT = 0.0D0
        LFPDV = 0.0D0

        DO N = 1, NDS

*        Accumulate likelihood probability
          CALL FIT_MPROB1( OBDAT(N).NDAT, %VAL(OBDAT(N).DPTR),
     :                     OBDAT(N).QFLAG, %VAL(OBDAT(N).QPTR),
     :                     %VAL(PREDDAT(N).DPTR), LFPM, LFPD,
     :                     LFPDV, LFPT, STATUS )

        END DO
	print *,lfpm, lfpd,sqrt(lfpdv),lfpt

*      Fit probability given
        NAGSTAT = 0
        FPROB = 1.0D0 - G01EAF( 'L', (LFPM-LFPD)/SQRT(LFPDV),
     :                          NAGSTAT )

*    Programmer error
      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETI( 'FC', FSTAT )
        CALL ERR_REP( 'FIT_MPROB_1', 'Unknown fit statistic code /^FC/',
     :                STATUS )

      END IF

*    Exit
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_MPROB', STATUS )
      END IF

      END


*+  FIT_MPROB1 - Calculate log fit probabilityand its error
      SUBROUTINE FIT_MPROB1( NDAT, DATA, QOK, QUAL, PRED,
     :                       LFPM, LFPD, LFPDV, LFPT, STATUS )
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
*     DJA: David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*     30 Aug 94 : Original (DJA)
*
*    Type definitions :
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER			NDAT			! Number of data values
      REAL			DATA(*)			! Observed data
      LOGICAL			QOK			! Quality ok?
      LOGICAL			QUAL(*)			! Observed quality
      REAL			PRED(*)			! Predicted data
*
*    Import / Export :
*
      DOUBLE PRECISION		LFPM			! log p_fit
      DOUBLE PRECISION		LFPD			! < log p >
      DOUBLE PRECISION		LFPDV			! var{ log p}
      DOUBLE PRECISION		LFPT			! log p_max
*
*    Status :
*
      INTEGER 			STATUS
*
*    Function declarations :
*
      DOUBLE PRECISION		FIT_MPROB_LDF		! < log d! >
*
*    Local constants :
*
      REAL			GLIMIT
        PARAMETER		( GLIMIT = 200.0 )
*
*    Local variables :
*
      DOUBLE PRECISION		ELPF			! < log d! >
      DOUBLE PRECISION		ELPF2			! < log d! ^ 2 >
      DOUBLE PRECISION		EDLPF			! < d log d! >
      DOUBLE PRECISION		LP			! Log predicted data

      INTEGER			I			! Loop over data

      LOGICAL			GOOD			! Is this point good?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK  )RETURN

*    Initialise for case of no quality
      GOOD = .TRUE.

*    Loop over data values
      DO I = 1, NDAT

*      Good point?
        IF ( QOK ) GOOD = QUAL(I)
	if ( i.le.10 ) then
	print *,'Point ',i,' good = ',good,
     :               ' data = ',data(i), ', pred = ',pred(i)
	end if

        IF ( GOOD .AND. (PRED(I).GT.0.0) .AND. (DATA(I).GT.0.0) ) THEN

*        Get log of predicted data, and the 3 expectation values. In the
*        Gaussian limit we need only the
          LP = LOG(PRED(I))
          IF ( PRED(I) .GT. GLIMIT ) THEN

*          Expectation values <log d!>
            CALL FIT_MPROB_EX3( DBLE(PRED(I)), .FALSE., ELPF,
     :                          ELPF2, EPLPF )

            LFPDV = 0.5D0

          ELSE

*          Expectation values <log d!>, <(log d!)^2> and <d log d!>
            CALL FIT_MPROB_EX3( DBLE(PRED(I)), .TRUE., ELPF,
     :                          ELPF2, EPLPF )

*          and its variance
            LFPDV = LFPDV + PRED(I)*LP*LP + ELPF2
     :                - ELPF*ELPF - 2.0*LP*(EPLPF-PRED(I)*ELPF)

          END IF

*        Accumulate log probability for data
          LFPD = LFPD + DATA(I)*LP - PRED(I) -
     :                           FIT_MPROB_LDF(NINT(DATA(I)))

*        Accumulate log probability for model
          LFPT = LFPT + PRED(I)*LP - PRED(I) -
     :                           FIT_MPROB_LDF(NINT(PRED(I)))

*        Accumulate log probability for model
          LFPM = LFPM + PRED(I)*LP - PRED(I) - ELPF

        END IF

      END DO

      END


*+  FIT_MPROB_EX3AC - Accumulate <log(d!)>, <log(d!) ^ 2> and <d log(d!)>
      SUBROUTINE FIT_MPROB_EX3AC( MEAN, LOB, HIB, DO2, EXLDF, EXLDF2,
     :                            EXDLDF )
*
*    Description :
*
*     Accumulates the expectation values <log(d!)>, <log(d!) ^ 2> and
*     <d log(d!)> for d=MEAN, over the interval LOB to HIB.
*
*    Method :
*
*     The expectation of a function like log(d!) must be calculated by
*     summing the contributions for the the 'd' for which the product
*
*        log(d!) * prob(d)
*
*     is significant. The 'd' are drawn from a Poisson distribution of mean
*     MEAN so we sum over all terms for d=0 until the sum converges. The
*     trick used by this routine is to acccumulate the upwards from LOB and
*     downwards from HIB. Where the interval encloses the peak in the
*     Poisson distribution this ensures maximum retention of precision in
*     the result.
*
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     DJA: David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*     30 Aug 94 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      DOUBLE PRECISION		MEAN			! Mean of Poisson
							! distribution
      INTEGER			LOB, HIB		! Interval to accumulate
      LOGICAL			DO2
*
*    Export :
*
      DOUBLE PRECISION		EXLDF
      DOUBLE PRECISION		EXLDF2
      DOUBLE PRECISION		EXDLDF
*
*    Function declarations :
*
      DOUBLE PRECISION		FIT_MPROB_LDF
*
*    Local variables :
*
      DOUBLE PRECISION		LFAC			! log(d!)
      DOUBLE PRECISION		LMEAN			! log(MEAN)
      DOUBLE PRECISION		PPROB			! Probability of J
							! given MEAN
      DOUBLE PRECISION		BIT1,BIT2,BIT3
      DOUBLE PRECISION		SUM1,SUM2,SUM3

      INTEGER			L, H
*-

      L = LOB
      H = HIB
      SUM1 = 0.0D0
      SUM2 = 0.0D0
      SUM3 = 0.0D0
      LMEAN = LOG(MEAN)

 10   CONTINUE

        LFAC = FIT_MPROB_LDF( L )
        PPROB = EXP( LMEAN*DBLE(L) - MEAN - LFAC )
        BIT1 = LFAC * PPROB
        IF ( DO2 ) THEN
          BIT2 = LFAC * LFAC * PPROB
          BIT3 = LFAC * DBLE(L) * PPROB
        END IF

        IF ( L .LT. H ) THEN
          LFAC = FIT_MPROB_LDF( H )
          PPROB = EXP( LMEAN*DBLE(H) - MEAN - LFAC )
          BIT1 = BIT1 + LFAC * PPROB
          IF ( DO2 ) THEN
            BIT2 = BIT2 + LFAC * LFAC * PPROB
            BIT3 = BIT3 + LFAC * DBLE(H) * PPROB
          END IF
        END IF

        SUM1 = SUM1 + BIT1
        IF ( DO2 ) THEN
          SUM2 = SUM2 + BIT2
          SUM3 = SUM3 + BIT3
        END IF
        L = L + 1
        H = H - 1

      IF ( L .LE. H ) GOTO 10

      EXLDF = EXLDF + SUM1
      IF ( DO2 ) THEN
        EXLDF2 = EXLDF2 + SUM2
        EXDLDF = EXDLDF + SUM3
      END IF

      END



*+  FIT_MPROB_EX3 - Find <log(d!)> and optionally <log(d!) ^ 2>,<d log(d!)>
      SUBROUTINE FIT_MPROB_EX3( MEAN, EXLDF, DO2, EXLDF2, EXDLDF )
*
*    Description :
*
*     Calculates the expectation value of log(d!) for a Poisson distribution
*     of mean MEAN.
*
*    Method :
*
*     The expectation of a function like log(d!) must be calculated by
*     summing the contributions for the the 'd' for which the product
*
*        log(d!) * prob(d)
*
*     is significant. The 'd' are drawn from a Poisson distribution of mean
*     MEAN so we sum over all terms for d=0 until the sum converges.
*
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     DJA: David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*     30 Aug 94 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      DOUBLE PRECISION		MEAN			! Mean of Poisson
							! distribution
      LOGICAL			DO2
*
*    Export :
*
      DOUBLE PRECISION		EXLDF
      DOUBLE PRECISION		EXLDF2
      DOUBLE PRECISION		EXDLDF
*
*    Local constants :
*
      REAL			TOLNCE			! Convergence tolerance
	PARAMETER		( TOLNCE = 1.0E-9 )
      INTEGER			PTHRESH
	PARAMETER 		( PTHRESH = 512 )
*
*    Local variables :
*
      DOUBLE PRECISION		LASTMEAN		! Last value of MEAN
        SAVE			LASTMEAN
      DOUBLE PRECISION          L_LDF, L_LDF2, L_DLDF
        SAVE                    L_LDF, L_LDF2, L_DLDF
      INTEGER			I6			! 6*sqrt(MEAN) roughly
      INTEGER			ISTEP			! Accumulation increment
      INTEGER			LOB, HIB		! Founds for eval'n
      INTEGER			NEWLO, NEWHI

      LOGICAL			C_LDF
      LOGICAL			C_LDF2
      LOGICAL			C_DLDF
*
*    Local data :
*
      DATA			LASTMEAN/-1.0/
*-

*    Not same as last time?
      IF ( MEAN .NE. LASTMEAN ) THEN

*      Initialise outputs
        EXLDF = 0.0D0
        IF ( DO2 ) THEN
          EXLDF2 = 0.0D0
          EXDLDF = 0.0D0
        END IF

*      Initial bounds for accumulation
        IF ( MEAN .GT. PTHRESH ) THEN
          I6 = INT( 6.0*SQRT(MEAN) )
          LOB = MEAN - I6
          IF ( LOB .LT. 0 ) LOB = 0
          HIB = MEAN + I6
        ELSE
          LOB = 0
          HIB = MEAN + 10
        END IF

*      Step size for extra accumulations
        ISTEP = MAX( 5, INT(SQRT(MEAN)) )

*      First accumulation
        CALL FIT_MPROB_EX3AC( MEAN, LOB, HIB, DO2,
     :                        EXLDF, EXLDF2, EXDLDF )

*      Loop until convergence achieved
 10     CONTINUE

*        Store old values
          L_LDF = EXLDF
          IF ( DO2 ) THEN
            L_LDF2 = EXLDF2
            L_DLDF = EXDLDF
          END IF

*        More on lower bound
          IF ( LOB .GT. 0 ) THEN
            NEWLO = MAX( 0, LOB - ISTEP )
            CALL FIT_MPROB_EX3AC( MEAN, NEWLO, LOB-1, DO2, EXLDF,
     :                            EXLDF2, EXDLDF )
            LOB = NEWLO
          END IF

*        More on upper bound
          NEWHI = HIB + ISTEP
          CALL FIT_MPROB_EX3AC( MEAN, HIB+1, NEWHI, DO2, EXLDF,
     :                          EXLDF2, EXDLDF )
          HIB = NEWHI

*        Convergence tests
          C_LDF = ( (EXLDF-L_LDF) .LT. TOLNCE*EXLDF )
          IF ( DO2 ) THEN
            C_LDF2 = ( (EXLDF2-L_LDF2) .LT. TOLNCE*EXLDF2 )
            C_DLDF = ( (EXDLDF-L_DLDF) .LT. TOLNCE*EXDLDF )
          END IF

*      Convergence
        IF ( DO2 ) THEN
          IF ( .NOT. (C_LDF.AND.C_LDF2.AND.C_DLDF) ) GOTO 10
        ELSE
          IF ( .NOT. C_LDF ) GOTO 10
        END IF

*      Update old values
        LASTMEAN = MEAN
        L_LDF = EXLDF
        IF ( DO2 ) THEN
          L_LDF2 = EXLDF2
          L_DLDF = EXDLDF
        END IF

      END IF

*    Set return value
      EXLDF = L_LDF
      IF ( DO2 ) THEN
        EXLDF2 = L_LDF2
        EXDLDF = L_DLDF
      END IF

      END



*+  FIT_MPROB_LDF - Calculate log(D!)
      DOUBLE PRECISION FUNCTION FIT_MPROB_LDF( D )
*
*    Description :
*
*     Calculates the value of log(d!)
*
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     DJA: David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*     30 Aug 94 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'MATH_PAR'
*
*    Import :
*
      INTEGER			D			! The argument
*
*    Local constants :
*
      INTEGER			LOFAC          		! Max D to calculate
	PARAMETER		( LOFAC = 35 )		! directly
      INTEGER			MAXFAC          	! Max D to calculate
	PARAMETER		( MAXFAC = 2048 )
*
*    Local variables :
*
      DOUBLE PRECISION		DI
      DOUBLE PRECISION		FAC			! Cumulative factorial
      DOUBLE PRECISION		LDF(0:MAXFAC)		! Store log(d!) values
        SAVE			LDF

      INTEGER			I			! Loop over integers

      LOGICAL			FIRST			! First call?
        SAVE			FIRST
*
*    Local data :
*
      DATA			FIRST/.TRUE./
*-

*    First call?
      IF ( FIRST ) THEN

*      Initialise for loop
        FAC = 1.0D0
        LDF(0) = 0.0D0

*      Find log(D!) by accurate method up to LOFAC
        DO I = 1, LOFAC
          FAC = FAC * I
          LDF(I) = LOG(FAC)
        END DO

*      Do the rest from LOFAC+1 up to MAXFAC using recurrence relation
        DO I = LOFAC+1, MAXFAC
          LDF(I) = LDF(I-1) + LOG(DBLE(I))
        END DO

*      Now initialised
        FIRST = .FALSE.

      END IF

*    Use value from array if supplied data is less than MAXFAC
      IF ( D .LE. MAXFAC ) THEN
        FIT_MPROB_LDF = LDF(D)

*    Otherwise calculate it
      ELSE

        DI = DBLE(I)
        FIT_MPROB_LDF = DI * LOG(DI) - DI +
     :                  0.5D0*LOG(2.0D0*MATH__DPI*DI)

      END IF

      END
