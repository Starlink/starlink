*+  FIT_MPROB - Finds probability that observed data is drawn from model
      SUBROUTINE FIT_MPROB( NDS, FSTAT, SSCALE, STATMIN, FPROB, STATUS )
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
*     Richard Beard (ROSAT, UNiversity of Birminghma)
*
*    History :
*
*     30 Aug 94 : Original (DJA)
*     14 Jun 95 : Iteration scheme changed to stabilise expectation value
*                 calculation, and Gaussian limit simplification added (DJA)
*     13 Mar 97 : Correct NAG arguments (RB)
*     22 Jun 87 : Replace NAG with ASTPDA (RB)
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
c     RECORD /DATASET/    	OBDAT(NDS)		! Observed datasets
      INTEGER             	SSCALE			! Statistic scale factor
      INTEGER             	FSTAT                 	! Statistic to use
c     RECORD /PREDICTION/ 	PREDDAT(NDS)	        ! Data predicted by
							! model
      DOUBLE PRECISION    	STATMIN			! Statistic at minimum
*
*    Export :
*
      DOUBLE PRECISION 		FPROB			! Fit probability
*
*    Function declarations :
*
      DOUBLE PRECISION 		PDA_CHISQD
      DOUBLE PRECISION 		PDA_NORMAL
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
      INTEGER			PDASTAT			! PDA status
*-

*    Status check
      IF ( STATUS.NE.SAI__OK ) RETURN

*    Chi-squared case
      IF ( FSTAT .EQ. FIT__CHISQ ) THEN

*      Get probability from PDA routine
        PDASTAT = 0
        FPROB = PDA_CHISQD( 'L', STATMIN, DBLE(SSCALE), PDASTAT )

*    Likelihood
      ELSE IF ( FSTAT .EQ. FIT__LOGL ) THEN

*      Loop over datasets
        LFPM = 0.0D0
        LFPD = 0.0D0
        LFPT = 0.0D0
        LFPDV = 0.0D0

        DO N = 1, NDS

*        Accumulate likelihood probability
          CALL FIT_MPROB1( DATASET_NDAT(N), %VAL(DATASET_DPTR(N)),
     :                     DATASET_QFLAG(N), %VAL(DATASET_QPTR(N)),
     :                     %VAL(DATASET_DPTR(N)), LFPM, LFPD,
     :                     LFPDV, LFPT, STATUS )

        END DO

*      Fit probability given
        PDASTAT = 0
        FPROB = 1.0D0 - PDA_NORMAL( 'L', (LFPM-LFPD)/SQRT(LFPDV),
     :                          PDASTAT )

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
      INCLUDE 'MATH_PAR'
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
      REAL			GTOL
        PARAMETER		( GTOL = 1.0E-5 )
      REAL			PTOL
        PARAMETER		( PTOL = 1.0E-9 )
*
*    Local variables :
*
      DOUBLE PRECISION		DPRED
      DOUBLE PRECISION		ELPF			! < log d! >
      DOUBLE PRECISION		ELPF2			! < log d! ^ 2 >
      DOUBLE PRECISION		EPLPF			! < d log d! >
      DOUBLE PRECISION		LP			! Log predicted data
	REAL*8 LFPM2
      INTEGER			I			! Loop over data
      INTEGER			NGOOD

      LOGICAL			GOOD			! Is this point good?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise for case of no quality
      GOOD = .TRUE.
      LFPM2 = 0.0D0
      NGOOD = 0

*    Loop over data values
      DO I = 1, NDAT

*      Good point?
        IF ( QOK ) GOOD = QUAL(I)

        IF ( GOOD .AND. (PRED(I).GT.0.0) .AND. (DATA(I).GT.0.0) ) THEN

*        Get log of predicted data, and the 3 expectation values. In the
*        Gaussian limit we need only the
          DPRED = DBLE(PRED(I))
          LP = LOG(DPRED)
          IF ( PRED(I) .GT. GLIMIT ) THEN

*          Use Gaussian approximation
	    LFPM2 = LFPM2 + LOG(MATH__DPI*2.0D0*DPRED)
            NGOOD = NGOOD + 1

*          Variance is easy...
            LFPDV = LFPDV + 0.5D0

          ELSE

*          Expectation values <log d!>, <(log d!)^2> and <d log d!>
            CALL FIT_MPROB_EX3( DPRED, PTOL, ELPF, ELPF2, EPLPF )

*        Accumulate log probability for model
            LFPM = LFPM + PRED(I)*LP - DPRED - ELPF

*          and its variance
            LFPDV = LFPDV + DPRED*LP*LP + ELPF2
     :                - ELPF*ELPF - 2.0D0*LP*(EPLPF-DPRED*ELPF)

          END IF

*        Accumulate log probability for data
          LFPD = LFPD + DATA(I)*LP - DPRED -
     :                           FIT_MPROB_LDF(NINT(DATA(I)))

*        Accumulate log probability for model
          LFPT = LFPT + PRED(I)*LP - DPRED -
     :                           FIT_MPROB_LDF(NINT(PRED(I)))


        END IF

      END DO

*  Adjust expected log probability if any Gaussian points
      IF ( NGOOD .GT. 0 ) THEN
	LFPM2 = - 0.5D0 * LFPM2 - DBLE(NGOOD)/2.0D0
        LFPM = LFPM + LFPM2
      END IF

      END


*+  FIT_MPROB_EX3AC - Accumulate <log(d!)>, <log(d!) ^ 2> and <d log(d!)>
      SUBROUTINE FIT_MPROB_EX3AC( MEAN, LOB, HIB, EXLDF, EXLDF2,
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
        BIT2 = LFAC * LFAC * PPROB
        BIT3 = LFAC * DBLE(L) * PPROB

        IF ( L .LT. H ) THEN
          LFAC = FIT_MPROB_LDF( H )
          PPROB = EXP( LMEAN*DBLE(H) - MEAN - LFAC )
          BIT1 = BIT1 + LFAC * PPROB
          BIT2 = BIT2 + LFAC * LFAC * PPROB
          BIT3 = BIT3 + LFAC * DBLE(H) * PPROB
        END IF

        SUM1 = SUM1 + BIT1
        SUM2 = SUM2 + BIT2
        SUM3 = SUM3 + BIT3
        L = L + 1
        H = H - 1

      IF ( L .LE. H ) GOTO 10

      EXLDF = EXLDF + SUM1
      EXLDF2 = EXLDF2 + SUM2
      EXDLDF = EXDLDF + SUM3

      END



*+  FIT_MPROB_EX3 - Find <log(d!)>, <log(d!) ^ 2> and <d log(d!)>
      SUBROUTINE FIT_MPROB_EX3( MEAN, TOLNCE, EXLDF, EXLDF2, EXDLDF )
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
      REAL			TOLNCE			! Convergence tolerance
*
*    Export :
*
      DOUBLE PRECISION		EXLDF
      DOUBLE PRECISION		EXLDF2
      DOUBLE PRECISION		EXDLDF
*
*    Local constants :
*
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
        EXLDF2 = 0.0D0
        EXDLDF = 0.0D0

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
        CALL FIT_MPROB_EX3AC( MEAN, LOB, HIB, EXLDF, EXLDF2, EXDLDF )

*      Loop until convergence achieved
 10     CONTINUE

*        Store old values
          L_LDF = EXLDF
          L_LDF2 = EXLDF2
          L_DLDF = EXDLDF

*        More on lower bound
          IF ( LOB .GT. 0 ) THEN
            NEWLO = MAX( 0, LOB - ISTEP )
            CALL FIT_MPROB_EX3AC( MEAN, NEWLO, LOB-1, EXLDF,
     :                            EXLDF2, EXDLDF )
            LOB = NEWLO
          END IF

*        More on upper bound
          NEWHI = HIB + ISTEP
          CALL FIT_MPROB_EX3AC( MEAN, HIB+1, NEWHI, EXLDF,
     :                          EXLDF2, EXDLDF )
          HIB = NEWHI

*        Convergence tests
          C_LDF = ( (EXLDF-L_LDF) .LT. TOLNCE*EXLDF )
          C_LDF2 = ( (EXLDF2-L_LDF2) .LT. TOLNCE*EXLDF2 )
          C_DLDF = ( (EXDLDF-L_DLDF) .LT. TOLNCE*EXDLDF )

*      Convergence
          IF ( .NOT. (C_LDF.AND.C_LDF2.AND.C_DLDF) ) GOTO 10

*      Update old values
        LASTMEAN = MEAN
        L_LDF = EXLDF
        L_LDF2 = EXLDF2
        L_DLDF = EXDLDF

      END IF

*    Set return value
      EXLDF = L_LDF
      EXLDF2 = L_LDF2
      EXDLDF = L_DLDF

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
	PARAMETER		( MAXFAC = 4096 )
*
*    Local variables :
*
      DOUBLE PRECISION		LFAC,FAC		! Cumulative factorial
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

        LFAC = LDF(MAXFAC)
        DO I = MAXFAC + 1, D
          FAC = LFAC + LOG(DBLE(I))
          LFAC = FAC
        END DO
        FIT_MPROB_LDF = FAC

      END IF

      END
