*+  FIT_PREDDAT - Computes a single predicted dataset from current model
      SUBROUTINE FIT_PREDDAT( FSTAT, NDS, IMOD, PARAM, N, PRED, STATUS )
*
*    Description :
*
*     Calls FIT_MCALC to compute the model space data corresponding to a given
*     observed dataset, and if necessary folds it through an instrument
*     response matrix to transform to the data space.
*     The model may be a composite (cmodel) built up from several component
*     primitive models (pmodels), the names of the model components are stored
*     in keyword form in array KEY and the prescription for combining them
*     in the polish representation POLISH.
*     For the case of likelihood fitting, the model is scaled to give
*     raw counts, and background is added (if COUNTMODEL is set true).
*
*    Method :
*
*     All done in subroutines.
*     Note that PREDDAT itself is not updated. Though the array of predicted
*     data pointed to by PREDDAT.DPTR can be updated by passing this in to be
*     picked up as the PRED array.
*     NOTE: The working space needed for the model stack must be set up to be
*     of size at least NMAX*MAXSTACK , where NMAX is the length of the
*     model dataset (not necessarily the same as the length of the longest
*     observed dataset, since the two can be in different spaces).
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Trevor Ponman (BHVAD::TJP)
*
*    History :
*
*     31 Mar 87 : Original (BHVAD::TJP)
*     19 Apr 88 : New structures, global eliminated (TJP)
*     23 Feb 89 : Change to FIT_MCALC interface,dataset number passed down (TJP)
*     15 Jun 92 : COUNTMODEL option to cater for likelihood fitting (TJP)
*     10 Jan 94 : Change to FIT_MCALC interface,number of parameters used
*                 passed down (DJA)
*      9 Feb 94 : Now pass MODEL spec directly rather than splitting it up (DJA)
*      1 Aug 95 : Apply vignetting correction if available (DJA)
*     11 Mar 1996 (DJA):
*        Removed old style convolution.
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'FIT_PAR'
*
*    Structure definitions :
*
      INCLUDE 'FIT_STRUC'
*
*    Import :
*
      INTEGER FSTAT			! Fit statistic flag (1=chisq, 2=l'hood)
      INTEGER NDS			! Number of observed datasets
c     RECORD/DATASET/OBDAT(NDS)		! Observed datasets
c     RECORD/INSTR_RESP/INSTR(NDS)	! Instrument responses
c     RECORD/MODEL_SPEC/MODEL		! Model specification
      INTEGER IMOD
c     RECORD/PREDICTION/PREDDAT(NDS)	! Data predicted by model
      REAL PARAM(NPAMAX)		! Model parameters
      INTEGER N				! Dataset no
*
*    Export :
*
      REAL PRED(*)			! Array of predicted data
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      LOGICAL COUNTMODEL		! Predicted data required in counts
      INTEGER IERR,NERR			! Error arguments for VEC_* routines
      INTEGER IDIMM(FIT__MXDIM),I
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check if raw count model is required
      COUNTMODEL = ( FSTAT .EQ. FIT__LOGL )

*  Copy out the axis pointers
      DO I = 1, FIT__MXDIM
        IDIMM(I) = PREDICTION_IDIMM(N, I)
      END DO

*  Convolution with instrument response required
      IF ( PREDICTION_CONVOLVE(N) ) THEN

*    Generate model space data
	CALL FIT_MCALC( IMOD, PARAM, N, PREDICTION_NMDIM(N),
     :                  IDIMM, PREDICTION_NMDAT(N),
     :                  PREDICTION_NMBOUND(N),
     :                  %VAL(PREDICTION_MLBNDPTR(N)),
     :                  %VAL(PREDICTION_MUBNDPTR(N)),
     :                  %VAL(MODEL_SPEC_STACKPTR(IMOD)),
     :                  %VAL(PREDICTION_MPTR(N)),STATUS)

*    Fold through instrument response. Test for new style response
        CALL ERI_FOLD( PREDICTION_NMDAT(N), %VAL(PREDICTION_MPTR(N)),
     :                 DATASET_NDAT(N), INSTR_RESP_R_ID(N),
     :                 INSTR_RESP_A_ID(N), PRED, STATUS )

      ELSE

*    No convolution required (i.e. model space = data space )
	CALL FIT_MCALC( IMOD, PARAM, N, PREDICTION_NMDIM(N),
     :                  IDIMM, PREDICTION_NMDAT(N),
     :                  PREDICTION_NMBOUND(N),
     :                  %VAL(PREDICTION_MLBNDPTR(N)),
     :                  %VAL(PREDICTION_MUBNDPTR(N)),
     :                  %VAL(MODEL_SPEC_STACKPTR(N)), PRED, STATUS )

      END IF

*  Scale by vignetting array if present
c      IF ( OBDAT(N).V_ID .NE. ADI__NULLID ) THEN
c       CALL VEC_MULR( .FALSE., DATASET_NDAT(N), %VAL(DATASET_VIGPTR(N)),
c     :                 PRED, PRED, IERR, NERR, STATUS )
c      END IF

*  Scale up by TEFF and add background if count model is required
      IF ( COUNTMODEL ) THEN
	CALL ARR_MULTR( DATASET_TEFF(N), DATASET_NDAT(N), PRED, STATUS )
	CALL VEC_ADDR( .FALSE., DATASET_NDAT(N), %VAL(DATASET_BPTR(N)),
     :                 PRED, PRED, IERR, NERR, STATUS )
      END IF

*  Exit
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_PREDDAT', STATUS )
      END IF

      END
