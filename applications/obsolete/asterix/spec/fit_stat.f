*+  FIT_STAT - Evaluates statistical fit between predicted and observed data
      SUBROUTINE FIT_STAT( NDS, IMOD, PARAM, FSTAT,
     :                          PREDICTOR, STAT, STATUS )
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
*      4 Feb 1987 (TJP):
*        Original version
*     14 Apr 1988 (TJP):
*        New structures - global eliminated
*     25 Mar 1992 (RJV):
*        Renamed, FIT_PREDDAT made external
*     27 May 1992 (DJA):
*        Added max-l statistic, error handling corrected
*     12 Jun 1992 (DJA):
*        Passes quality to FIT_LOGL_ACCUM
*     18 Aug 1992 (DJA):
*        Statistic now double precision
*      4 Mar 1996 (DJA):
*        Now use grouped data pointers. If no grouping is defined these
*        have the same values as the primary dataset pointers
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'FIT_PAR'
*
*    Structure definitions :
*
      INCLUDE 'FIT_STRUC'
*
*    Import :
*
      INTEGER             NDS			! Number of observed datasets
c     RECORD /DATASET/    OBDAT(NDS)		! Observed datasets
c     RECORD /INSTR_RESP/ INSTR(NDS)		! Instrument responses
c     RECORD /MODEL_SPEC/ MODEL			! Model specification
      INTEGER		  IMOD
      REAL                PARAM(NPAMAX)		! Model parameters
      INTEGER             FSTAT                 ! Statistic to use
      EXTERNAL            PREDICTOR             ! Model data predictor
*
*    Import-Export :
*
c     RECORD /PREDICTION/ PREDDAT(NDS)	        ! Data predicted by model
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
      INTEGER             N			! Dataset index
      LOGICAL             MAXL                  ! Use maximum likelihood?
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Maximum likelihood?
      MAXL = (FSTAT.EQ.FIT__LOGL)

*  Loop through datasets calculating predicted data and accumulating statistic
      DSTAT = 0.0D0
      DO N = 1, NDS

*    Check dataset
	IF ( (DATASET_WPTR(N).EQ.0) .AND. .NOT. MAXL ) THEN
	  STATUS = SAI__ERROR
	  CALL ERR_REP( 'NOWTS','No data weights available', STATUS )
	  GOTO 99
	ELSE IF ( PREDICTION_DPTR(N) .EQ. 0 ) THEN
	  STATUS = SAI__ERROR
	  CALL ERR_REP( ' ','No space set up for predicted data',STATUS)
	  GOTO 99
	END IF

*    Evaluate model
	CALL PREDICTOR( FSTAT, NDS, IMOD, PARAM, N,
     :                  %VAL(PREDICTION_DPTR(N)), STATUS )

*    Group the predicted data?
        IF ( DATASET_GFLAG(N) ) THEN
          CALL UTIL_GRPWR( DATASET_NDAT(N), %VAL(PREDICTION_DPTR(N)),
     :                    .FALSE., 0.0, DATASET_QFLAG(N),
     :                    %VAL(DATASET_QPTR(N)), %VAL(DATASET_GPTR(N)),
     :                    DATASET_NGDAT(N), %VAL(PREDICTION_GDPTR(N)),
     :                    0.0, %VAL(DATASET_GQPTR(N)), STATUS )
        END IF

*    Accumulate statistic
        IF ( MAXL ) THEN
	  CALL FIT_LOGL_ACCUM( DATASET_NGDAT(N), %VAL(DATASET_GDPTR(N)),
     :                         DATASET_QFLAG(N), %VAL(DATASET_GQPTR(N)),
     :                         %VAL(PREDICTION_GDPTR(N)), DSTAT,
     :                         STATUS )
        ELSE
	  CALL FIT_CHISQ_ACCUM( DATASET_NGDAT(N),
     :                          %VAL(DATASET_GDPTR(N)),
     :                          DATASET_QFLAG(N),
     :                          %VAL(DATASET_GQPTR(N)),
     :                          %VAL(DATASET_GWPTR(N)),
     :                          %VAL(PREDICTION_GDPTR(N)), DSTAT,
     :                          STATUS )
        END IF

      END DO

***** temporary bodge to get round problem
      IF ( DSTAT .GT. 1.0D20 ) THEN
        STAT = 1.0E6
      ELSE
        STAT = DSTAT
      END IF

*  Exit
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_STAT', STATUS )
      END IF

      END
