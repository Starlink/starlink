*+  FIT_PREDDAT - Computes a single predicted dataset from current model
      SUBROUTINE FIT_PREDDAT( FSTAT, NDS, OBDAT, INSTR, PREDDAT, MODEL,
     :                        PARAM, N, PRED, STATUS )
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
	INTEGER FSTAT			! Fit statistic flag (1=chisq, 2=l'hood)
	INTEGER NDS			! Number of observed datasets
	RECORD/DATASET/OBDAT(NDS)	! Observed datasets
	RECORD/INSTR_RESP/INSTR(NDS)	! Instrument responses
	RECORD/MODEL_SPEC/MODEL		! Model specification
	RECORD/PREDICTION/PREDDAT(NDS)	! Data predicted by model
	REAL PARAM(NPAMAX)		! Model parameters
	INTEGER N			! Dataset no
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
	INTEGER IERR,NERR		! Error arguments for VEC_* routines
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check if raw count model is required
      COUNTMODEL = ( FSTAT .EQ. FIT__LOGL )

*    Convolution with instrument response required
      IF ( PREDDAT(N).CONVOLVE ) THEN

*      Generate model space data
	CALL FIT_MCALC( MODEL, PARAM, N, PREDDAT(N).NMDIM,
     :                  PREDDAT(N).IDIMM, PREDDAT(N).NMDAT,
     :                  PREDDAT(N).NMBOUND,%VAL(PREDDAT(N).MLBNDPTR),
     :                  %VAL(PREDDAT(N).MUBNDPTR),%VAL(MODEL.STACKPTR),
     :                  %VAL(PREDDAT(N).MPTR),STATUS)

*      Fold through instrument response
	CALL FIT_FOLD(PREDDAT(N).NMDAT,OBDAT(N).NDAT,INSTR(N).NRESP,
     :    %VAL(PREDDAT(N).MPTR),%VAL(INSTR(N).MIPTR),
     :    %VAL(INSTR(N).DIPTR),%VAL(INSTR(N).RESPTR),PRED,STATUS)

      ELSE

*      No convolution required (i.e. model space = data space )
	CALL FIT_MCALC( MODEL, PARAM, N, PREDDAT(N).NMDIM,
     :                  PREDDAT(N).IDIMM, PREDDAT(N).NMDAT,
     :                  PREDDAT(N).NMBOUND, %VAL(PREDDAT(N).MLBNDPTR),
     :                  %VAL(PREDDAT(N).MUBNDPTR),
     :                  %VAL(MODEL.STACKPTR), PRED, STATUS )

      END IF

*    Scale up by TEFF and add background if count model is required
      IF ( COUNTMODEL ) THEN
	CALL ARR_MULTR( OBDAT(N).TEFF, OBDAT(N).NDAT, PRED )
	CALL VEC_ADDR( .FALSE., OBDAT(N).NDAT, %VAL(OBDAT(N).BPTR),
     :                 PRED, PRED, IERR, NERR, STATUS )
      END IF

*    Exit
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_PREDDAT', STATUS )
      END IF

      END
