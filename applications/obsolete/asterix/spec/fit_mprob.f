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
      DOUBLE PRECISION		FIT_MPROB_EXDLDF	! < d log(d!) >
      DOUBLE PRECISION		FIT_MPROB_LDF		! < d >
      DOUBLE PRECISION		FIT_MPROB_EXLDF		! < log(d!) >
      DOUBLE PRECISION		FIT_MPROB_EXLDF2	! < log(d!)^2 >
*
*    Local variables :
*
      DOUBLE PRECISION		ELPF			! Expectation of Log
							! prediected data!
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
        IF ( GOOD .AND. (PRED(I).GT.0.0) .AND. (DATA(I).GT.0.0) ) THEN

*        Get log of predicted data, and the expectation value of the predicted
*        data factorial
          LP = LOG(PRED(I))
          ELPF = FIT_MPROB_EXLDF( PRED(I) )

*        Accumulate log probability for data
          LFPD = LFPD + DATA(I)*LP - PRED(I) -
     :                           FIT_MPROB_LDF(NINT(DATA(I)))

*        Accumulate log probability for model
          LFPT = LFPT + PRED(I)*LP - PRED(I) -
     :                           FIT_MPROB_LDF(NINT(PRED(I)))

*        Accumulate log probability for model
          LFPM = LFPM + PRED(I)*LP - PRED(I) -
     :                           FIT_MPROB_EXLDF(PRED(I))

*        and its variance
          LFPDV = LFPDV + PRED(I)*LP*LP + FIT_MPROB_EXLDF2( PRED(I) )
     :                - ELPF*ELPF
     :                - 2.0*LP*(FIT_MPROB_EXDLDF(PRED(I))-PRED(I)*ELPF)

        END IF

      END DO

      END



*+  FIT_MPROB_EXLDF2 - Calculates < log(d !)^2 >
      DOUBLE PRECISION FUNCTION FIT_MPROB_EXLDF2( MEAN )
*
*    Description :
*
*     Calculates the expectation value of log(d!)^2 for a Poisson distribution
*     of mean MEAN.
*
*    Method :
*
*     The expectation of a function like log(d!)^2 must be calculated by
*     summing the contributions for the the 'd' for which the product
*
*        log(d!)^2 * prob(d)
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
      REAL			MEAN			! Mean of Poisson
							! distribution
*
*    Function declarations :
*
      DOUBLE PRECISION		FIT_MPROB_LDF
*
*    Local constants :
*
      REAL			TOLNCE			! Convergence tolerance
	PARAMETER		( TOLNCE = 1.0E-5 )
      INTEGER			PTHRESH
	PARAMETER 		( PTHRESH = 512 )
*
*    Local variables :
*
      DOUBLE PRECISION		LASTMEAN		! Last value of MEAN
        SAVE			LASTMEAN
      DOUBLE PRECISION		LASTSUM			! SUM on last iteration
      DOUBLE PRECISION		LASTVAL			! Last value of function
        SAVE			LASTVAL

      DOUBLE PRECISION		LJFAC			! log(J!)
      DOUBLE PRECISION		LMEAN			! log(MEAN)
      DOUBLE PRECISION		PPROB			! Probability of J
							! given MEAN
      DOUBLE PRECISION		SUM			! Cumulative result

      INTEGER			I3SIG			! 3*sqrt(MEAN) roughly
      INTEGER			J			! Loop over Poisson
							! distribution
      INTEGER			LOB, HIB		! Founds for eval'n
*
*    Local data :
*
      DATA			LASTMEAN/-1.0/
*-

*    Not same as last time?
      IF ( MEAN .NE. LASTMEAN ) THEN

*      Above Poisson threshold?
        IF ( MEAN .GT. PTHRESH ) THEN
          I3SIG = INT(6.0*SQRT(MEAN))
          LOB = NINT(MEAN) - I3SIG
          HIB = NINT(MEAN) + I3SIG
        ELSE
          LOB = 0
          HIB = MEAN + 10
        END IF

*      Initialise for loop
        SUM = 0.0D0
        J = LOB
        LMEAN = LOG(MEAN)
        LJFAC = FIT_MPROB_LDF( J )

*      Top of loop
 10     CONTINUE

*        Store last sum
          LASTSUM = SUM

*        Find Poisson probability of J given MEAN
          PPROB = EXP(LMEAN*J - MEAN - LJFAC)

*        Accumulate expectation value
          SUM = SUM + (LJFAC*LJFAC) * PPROB

*        Next value of J
          J = J + 1
          LJFAC = LJFAC + LOG(DBLE(J))

*      Repeat if minimum number of iterations not performed, or convergence
*      not achieved
        IF ( (J.LT.HIB) .OR.
     :       (ABS((SUM-LASTSUM)/SUM).GT.TOLNCE) ) GOTO 10

*      Update old values
        LASTMEAN = MEAN
        LASTVAL = SUM

      END IF

*    Set return value
      FIT_MPROB_EXLDF2 = LASTVAL

      END



*+  FIT_MPROB_EXDLDF - Calculates < d log(d !) >
      DOUBLE PRECISION FUNCTION FIT_MPROB_EXDLDF( MEAN )
*
*    Description :
*
*     Calculates the expectation value of d.log(d!) for a Poisson distribution
*     of mean MEAN.
*
*    Method :
*
*     The expectation of a function like d.log(d!) must be calculated by
*     summing the contributions for the the 'd' for which the product
*
*        d.log(d!) * prob(d)
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
      REAL			MEAN			! Mean of Poisson
							! distribution
*
*    Function declarations :
*
      DOUBLE PRECISION		FIT_MPROB_LDF
*
*    Local constants :
*
      REAL			TOLNCE			! Convergence tolerance
	PARAMETER		( TOLNCE = 1.0E-5 )
      INTEGER			PTHRESH
	PARAMETER 		( PTHRESH = 512 )
*
*    Local variables :
*
      DOUBLE PRECISION		LASTMEAN		! Last value of MEAN
        SAVE			LASTMEAN
      DOUBLE PRECISION		LASTSUM			! SUM on last iteration
      DOUBLE PRECISION		LASTVAL			! Last value of function
        SAVE			LASTVAL

      DOUBLE PRECISION		LJFAC			! log(J!)
      DOUBLE PRECISION		LMEAN			! log(MEAN)
      DOUBLE PRECISION		PPROB			! Probability of J
							! given MEAN
      DOUBLE PRECISION		SUM			! Cumulative result

      INTEGER			I3SIG			! 3*sqrt(MEAN) roughly
      INTEGER			J			! Loop over Poisson
							! distribution
      INTEGER			LOB, HIB		! Founds for eval'n
*
*    Local data :
*
      DATA			LASTMEAN/-1.0/
*-

*    Not same as last time?
      IF ( MEAN .NE. LASTMEAN ) THEN

*      Above Poisson threshold?
        IF ( MEAN .GT. PTHRESH ) THEN
          I3SIG = INT(6.0*SQRT(MEAN))
          LOB = NINT(MEAN) - I3SIG
          HIB = NINT(MEAN) + I3SIG
        ELSE
          LOB = 0
          HIB = MEAN + 10
        END IF

*      Initialise for loop
        SUM = 0.0D0
        J = LOB
        LMEAN = LOG(MEAN)
        LJFAC = FIT_MPROB_LDF( J )

*      Top of loop
 10     CONTINUE

*        Store last sum
          LASTSUM = SUM

*        Find Poisson probability of J given MEAN
          PPROB = EXP(LMEAN*J - MEAN - LJFAC)

*        Accumulate expectation value
          SUM = SUM + LJFAC * J * PPROB

*        Next value of J
          J = J + 1
          LJFAC = LJFAC + LOG(DBLE(J))

*      Repeat if minimum number of iterations not performed, or convergence
*      not achieved
        IF ( (J.LT.HIB) .OR.
     :       (ABS((SUM-LASTSUM)/SUM).GT.TOLNCE) ) GOTO 10

*      Update old values
        LASTMEAN = MEAN
        LASTVAL = SUM

      END IF

*    Set return value
      FIT_MPROB_EXDLDF = LASTVAL

      END



*+  FIT_MPROB_EXLDF - Calculates < log(d !) >
      DOUBLE PRECISION FUNCTION FIT_MPROB_EXLDF( MEAN )
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
      REAL			MEAN			! Mean of Poisson
							! distribution
*
*    Function declarations :
*
      DOUBLE PRECISION		FIT_MPROB_LDF
*
*    Local constants :
*
      REAL			TOLNCE			! Convergence tolerance
	PARAMETER		( TOLNCE = 1.0E-5 )
      INTEGER			PTHRESH
	PARAMETER 		( PTHRESH = 512 )
*
*    Local variables :
*
      DOUBLE PRECISION		LASTMEAN		! Last value of MEAN
        SAVE			LASTMEAN
      DOUBLE PRECISION		LASTSUM			! SUM on last iteration
      DOUBLE PRECISION		LASTVAL			! Last value of function
        SAVE			LASTVAL

      DOUBLE PRECISION		LJFAC			! log(J!)
      DOUBLE PRECISION		LMEAN			! log(MEAN)
      DOUBLE PRECISION		PPROB			! Probability of J
							! given MEAN
      DOUBLE PRECISION		SUM			! Cumulative result

      INTEGER			I3SIG			! 3*sqrt(MEAN) roughly
      INTEGER			J			! Loop over Poisson
							! distribution
      INTEGER			LOB, HIB		! Founds for eval'n
*
*    Local data :
*
      DATA			LASTMEAN/-1.0/
*-

*    Not same as last time?
      IF ( MEAN .NE. LASTMEAN ) THEN

*      Above Poisson threshold?
        IF ( MEAN .GT. PTHRESH ) THEN
          I3SIG = INT(6.0*SQRT(MEAN))
          LOB = NINT(MEAN) - I3SIG
          HIB = NINT(MEAN) + I3SIG
        ELSE
          LOB = 0
          HIB = MEAN + 10
        END IF

*      Initialise for loop
        SUM = 0.0D0
        J = LOB
        LMEAN = LOG(MEAN)
        LJFAC = FIT_MPROB_LDF( J )

*      Top of loop
 10     CONTINUE

*        Store last sum
          LASTSUM = SUM

*        Find Poisson probability of J given MEAN
          PPROB = EXP(LMEAN*J - MEAN - LJFAC)

*        Accumulate expectation value
          SUM = SUM + LJFAC * PPROB

*        Next value of J
          J = J + 1
          LJFAC = LJFAC + LOG(DBLE(J))

*      Repeat if minimum number of iterations not performed, or convergence
*      not achieved
        IF ( (J.LT.HIB) .OR.
     :       (ABS((SUM-LASTSUM)/SUM).GT.TOLNCE) ) GOTO 10

*      Update old values
        LASTMEAN = MEAN
        LASTVAL = SUM

      END IF

*    Set return value
      FIT_MPROB_EXLDF = LASTVAL

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
*
*    Import :
*
      INTEGER			D			! The argument
*
*    Local constants :
*
      INTEGER			LOFAC          		! Max D to calculate
	PARAMETER		( LOFAC = 29 )		! directly
      INTEGER			MAXFAC          	! Max D to calculate
	PARAMETER		( MAXFAC = 1024 )
*
*    Local variables :
*
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
        FAC = LDF(MAXFAC)
        DO I = MAXFAC + 1, D
          FAC = FAC + LOG(DBLE(I))
        END DO
        FIT_MPROB_LDF = FAC

      END IF

      END
