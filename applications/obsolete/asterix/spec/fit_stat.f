*+  FIT_STAT - Evaluates statistical fit between predicted and observed data
      SUBROUTINE FIT_STAT( NDS, OBDAT, INSTR, MODEL, PARAM, FSTAT,
     :                          PREDICTOR, PREDDAT, STAT, STATUS )
*    Description :
*
*     Evaluates current predicted data (storing the result in PREDDAT) and
*     performs a comparison with observed data using either the chi-squared
*     statistic or Cash's maximum likelihood C statistic.
*
*    Method :
*
*     Depending on the value of the FSTAT, the statistic used is
*
*      FSTAT = FIT__CHISQ (chi-squared)
*
*        STAT = Sum over all data [ WT*(OBDAT-PREDDAT)**2 ]
*
*        where the weights are assumed to have been set to be inverse data
*        variances.
*
*      FSTAT = FIT__LOGL (maximum likelihood)
*
*        STAT = Sum over all data [ 2 * (PREDDAT-OBDAT*LN(PREDDAT)) ]
*
*     The sum is accumulated in double precision.
*
*    Deficiencies :
*
*     Second derivatives are only approximate, they are therefore unsuitable
*     for some purposes, e.g. evaluating errors from the curvature of the
*     chi-squared surface.
*
*    Bugs :
*
*    Authors :
*
*     Trevor Ponman (BHVAD::TJP)
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      4 Feb 87 : Original (BHVAD::TJP)
*     14 Apr 88 : New structures - global eliminated (TJP)
*     25 Mar 92 : Renamed, FIT_PREDDAT made external (RJV)
*     27 May 92 : Added max-l statistic, error handling corrected (DJA)
*     12 Jun 92 : Passes quality to FIT_LOGL_ACCUM (DJA)
*     18 Aug 92 : Statistic now double precision (DJA)
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
      REAL                PARAM(NPAMAX)		! Model parameters
      INTEGER             FSTAT                 ! Statistic to use
      EXTERNAL            PREDICTOR             ! Model data predictor
*
*    Import-Export :
*
      RECORD /PREDICTION/ PREDDAT(NDS)	        ! Data predicted by model
                                                ! (actually only the data
*                                               ! pointed to are updated)
*    Export :
*
      DOUBLE PRECISION    STAT			! Statistic. Predicted data in
                                                ! common is updated
*
*    Status :
*
      INTEGER             STATUS
*
*    Local variables :
*
      DOUBLE PRECISION    DSTAT			! Double prec accumulator
D     INTEGER             I
      INTEGER             N			! Dataset index
      LOGICAL             MAXL                  ! Use maximum likelihood?
*-

*    Status check
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Maximum likelihood?
      MAXL = (FSTAT.EQ.FIT__LOGL)

*    Loop through datasets calculating predicted data and accumulating statistic
      DSTAT = 0.0D0
      DO N=1,NDS

*      Check dataset
D	PRINT *,'fit_stat;param,ndat: ',(PARAM(I),I=1,6),OBDAT(N).NDAT
	IF ( (OBDAT(N).WPTR.EQ.0) .AND. .NOT. MAXL ) THEN
	  STATUS = SAI__ERROR
	  CALL ERR_REP( 'NOWTS','No data weights available', STATUS )
	  GOTO 99
	ELSE IF ( PREDDAT(N).DPTR .EQ. 0 ) THEN
	  STATUS = SAI__ERROR
	  CALL ERR_REP( ' ','No space set up for predicted data',STATUS)
	  GOTO 99
	END IF

*      Evaluate model
	CALL PREDICTOR( FSTAT, NDS, OBDAT, INSTR, PREDDAT, MODEL,
     :                  PARAM, N, %VAL(PREDDAT(N).DPTR), STATUS )

*      Accumulate statistic
        IF ( MAXL ) THEN
	  CALL FIT_LOGL_ACCUM( OBDAT(N).NDAT, %VAL(OBDAT(N).DPTR),
     :                      OBDAT(N).QFLAG, %VAL(OBDAT(N).QPTR),
     :                      %VAL(PREDDAT(N).DPTR), DSTAT, STATUS )
        ELSE
          IF ( STATUS .NE. SAI__OK ) GOTO 99
	  CALL FIT_CHISQ_ACCUM( OBDAT(N).NDAT, %VAL(OBDAT(N).DPTR),
     :          %VAL(OBDAT(N).WPTR), %VAL(PREDDAT(N).DPTR), DSTAT )
        END IF

      END DO

***** temporary bodge to get round problem
      IF ( DSTAT .GT. 1.0D20 ) THEN
        STAT = 1.0E6
      ELSE
        STAT = DSTAT
      END IF

D     PRINT *,'fit_stat;stat: ',STAT

*    Exit
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', '...from FIT_STAT', STATUS )
      END IF

      END
