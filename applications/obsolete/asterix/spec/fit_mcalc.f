*+  FIT_MCALC - Computes model-space dataset from composite model
      SUBROUTINE FIT_MCALC( MODEL, PARAM, ND, NMDIM, IDIMM, NMDAT,
     :                      NMBOUND,
     :                      MLBOUND, MUBOUND, STACK, PRED, STATUS )
*
*    Description :
*
*     Model space data from a composite model (cmodel) are computed over the
*     model space bins delimited by MLBOUND:MUBOUND (hence bins need not be
*     contiguous).
*     The keywords of the primitive components (pmodels) of class GENUS are
*     stored in the array MODEL.KEY and their parameter values in PARAM, and they are
*     combined according to the prescription given in the polish string.
*     (NMDIM and IDIMM are not used at present, but may be required in future
*     for data with dimension >1.)
*
*    Method :
*
*     Two stacks are maintained: The actual calculated models are stored in
*     STACK - which is really a storage area rather than a true stack, since
*     models may be pulled out from it in any order (the name is historical).
*     Initially models are calculated if they differ from the last call of
*     the subroutine (due to either changed parameter values or a new dataset
*     (hence new bounds)) and are stacked in order in STACK(NMDAT,1),
*     STACK(NMDAT,2) etc. These models are retained for possible use next time
*     the routine is called. Further vectors in STACK are used for arrays
*     generated during the combination of the pmodels.
*     MSTACK is a stack of numbers, which point to the corresponding rows in
*     the STACK array. Numbers less than STOFF correspond to pmodels, larger
*     numbers to the results of combining them. This is a true stack.
*     As the polish string is scanned, each 'M' entry causes a new model number
*     (one larger than the previous) to be entered onto MSTACK. The stack
*     is popped twice (since two models will be combined) each time an operator
*     is encountered in the polish string, and at the end of an operation a
*     number indicating the position of the result in the computed model stack
*     (see below) is entered on MSTACK.
*
*			 LOGIC
*			=======
*       LOOP through all pmodels
*           IF dataset or parameter values differ from last call
*               Compute pmodel and insert into STACK
*	END LOOP
*	LOOP through MODEL.POLISH string elements
*	    IF element='M' then stack on MSTACK
*           ELSE symbol is an operator
*               Check it's legal
*	        Pop model no. N1 off MSTACK
*	        IF N1>STOFF then reduce STACK by one (i.e. pop)
*               ELSE just extract N1 from STACK
*	        Pop model no. N2 off MSTACK
*	        IF N2>STOFF then reduce STACK by one (i.e. pop)
*               ELSE just extract N1 from STACK
*	        Combine models -> result = model(N2) <oper> model(N1)
*               Put result on top of STACK and pointer in MSTACK
*           ENDIF
*	END LOOP
*	Exit with final model in PRED
*
*     A further complication is added by the slice handling. When FIT_MCALL
*     calculates a pmodel, it may also optionally define a data slice. This
*     slice is a single section of the model data which contains non-zero
*     data. FIT_MCALC can handle such slices more efficiently when NMDAT
*     becomes large.
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
*      4 Feb 85 : Original (MCALC)
*     17 Oct 85 : Redundant NCHAN argument removed (BHVAD::TJP)
*      3 Oct 86 : Renamed to SPEC_MCALC (BHVAD::TJP)
*     18 Feb 87 : FIT_MCALC original (BHVAD::TJP)
*      2 Jun 87 : STATUS check after FIT_MCALL (TJP)
*     20 Apr 88 : Lower and upper bound arrays introduced (TJP)
*     23 Feb 89 : Dataset number passed through to FIT_MCALL (TJP)
*     28 Feb 89 : ASTERIX88 version, STOFF made a parameter (TJP)
*     24 Nov 89 : Reuse of models where params are unchanged - new logic (TJP)
*     29 Nov 90 : Bug in new logic for multiple spectra & pmodels corrected (TJP)
*      5 Aug 91 : Use of FIRST flag to detect new run under ICL (TJP)
*     12 Jan 93 : Corrected error reporting (DJA)
*     18 Aug 93 : Handle pmodel keys of any length (DJA)
*      7 Jan 94 : Added SLICE_DEF and SLICE variables. Added number of
*                 parameters argument to FIT_MCALL. (DJA)
*      8 Feb 94 : Changed arguments again. Pass MODEL specification directly
*                 rather than splitting it up in caller (DJA)
*     11 Mar 94 : Make local copy of IDIMM array before padding (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'FIT_PAR'
*
*    Structure definitions :
*
      INCLUDE 'FIT_STRUC'
*
*    Global variables :
*
      INCLUDE 'SPEC_CMN_RZ'
*
*    Import :
*
      RECORD /MODEL_SPEC/       MODEL			! Model specification
      REAL			PARAM(*)		! Model parameters
      INTEGER 			ND			! Current dataset number
      INTEGER 			NMDIM			! No of model dimensions
      INTEGER 			IDIMM(NMDIM)		! Size of each dimension
      INTEGER 			NMDAT			! # model data values
      INTEGER 			NMBOUND			! # model bin bounds
      REAL 			MLBOUND(NMBOUND)	! Model bin lower boundaries
      REAL 			MUBOUND(NMBOUND)	! Model bin upper boundaries
*
*    Export :
*
      REAL 			STACK(NMDAT,MAXSTACK)	! Stack of computed models
      REAL 			PRED(NMDAT)		! Predicted model-space data
*
*    Status :
*
      INTEGER 			STATUS
*
*    Function declarations :
*
      INTEGER 			CHR_LEN
*
*    Local variables :
*
      CHARACTER*80 		CLEAN			! CLEANed version of MODEL.POLISH
      CHARACTER*(MAXKEYLEN)	LASTMOD(MAXCOMP)	! Last model keys
        SAVE 			LASTMOD
      CHARACTER*1 		SYM			! Current symbol

      REAL 		PARSAVE(NPAMAX)			! Previous parameter values
	SAVE 		PARSAVE

      INTEGER 		I,J
      INTEGER 		ISTOP				! Upper lim. to index for pmodel
      INTEGER		LDIMS(DAT__MXDIM)
      INTEGER 		LSTRING				! Length of polish string
      INTEGER 		MSTACK(MAXCOMP)			! Model number stack
      INTEGER 		MSTACKPT			! Pointer to top of MSTACK
      INTEGER 		NM				! Total no. of pmodels in STACK
      INTEGER 		N1,N2				! 1st & 2nd current pmodels
      INTEGER 		NMST				! Last pmodel stacked on MSTACK
      INTEGER 		NDCURR				! Current dataset no (for
	SAVE 		NDCURR				! model calculations)
      INTEGER 		STACKPT				! Pointer to top of STACK
      INTEGER 		STOFF				! Stack offset for
							! Compound models
      INTEGER           SLICE(2,DAT__MXDIM,MAXSTACK)	! Slice indices

      LOGICAL		SAME_MODEL			! Same model as last time
      LOGICAL 		SLICE_DEF(MAXSTACK)		! Slice defined?
      LOGICAL 		UNCHANGED			! Pmodel same as last time?
*-

*    Status check
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Make copy of IDIMM
      DO I = 1, NMDIM
        LDIMS(I) = IDIMM(I)
      END DO

*    Pad SLICE and LDIMS array
      IF ( NMDIM .LT. 3 ) THEN
        DO I = NMDIM+1, 3
          LDIMS(I) = 1
          DO J = 1, MAXSTACK
            SLICE(1,I,J) = 1
            SLICE(2,I,J) = 1
          END DO
        END DO
      END IF

*    Initialise counters etc.
      NMST=0
      MSTACKPT=0
      STACKPT=0
      CLEAN=MODEL.POLISH
      CALL CHR_CLEAN(CLEAN)		! Zero extend since CHR_LEN counts
      LSTRING=CHR_LEN(CLEAN)		! ASCII nulls but not blanks
      IF(LSTRING.EQ.0) THEN
	STATUS=SAI__ERROR
	CALL ERR_REP('EMPTY_STRING','Polish string is empty',STATUS)
      ELSE IF ( MODEL.NCOMP .GT. MAXSTACK ) THEN
	STATUS=SAI__ERROR
	CALL ERR_REP('TOOBIG','Too many pmodel for model stack',STATUS)
      ENDIF
      IF ( STATUS .NE. SAI__OK ) GOTO 9000

*    Is the model spec the same as the last one?
      SAME_MODEL = .TRUE.
      DO NM = 1, MODEL.NCOMP
        IF ( MODEL.KEY(NM) .NE. LASTMOD(NM) ) THEN
          SAME_MODEL = .TRUE.
          LASTMOD(NM) = MODEL.KEY(NM)
        END IF
      END DO

*    Compute and stack all pmodels where they differ from previous call
      DO NM = 1, MODEL.NCOMP

*      Index of last parameter for this component
	IF ( NM .LT. MODEL.NCOMP ) THEN
	  ISTOP = MODEL.ISTART(NM+1)-1
	ELSE
	  ISTOP = MODEL.NPAR
	END IF

*      Check that dataset is same as for last call (and it's not the first call)
        IF((.NOT.S_FIRST).AND.SAME_MODEL.AND.(ND.EQ.NDCURR))THEN

*        Check whether parameters are unchanged for this pmodel
	  UNCHANGED=.TRUE.
	  DO J = MODEL.ISTART(NM), ISTOP
	    IF(PARAM(J).NE.PARSAVE(J))THEN
	      UNCHANGED=.FALSE.
	      PARSAVE(J)=PARAM(J)		! Reset for next time
	    END IF
	  END DO
	ELSE
	  UNCHANGED=.FALSE.			! New dataset => can't reuse
	END IF

*      If necessary, compute new model and stack in position NM
	IF ( .NOT. UNCHANGED ) THEN
	  CALL FIT_MCALL( MODEL.GENUS, MODEL.KEY(NM),
     :                    ISTOP - MODEL.ISTART(NM) + 1,
     :                    PARAM(MODEL.ISTART(NM)), ND,
     :                    NMDIM, LDIMS, NMDAT, NMBOUND, MLBOUND,
     :                    MUBOUND, STACK(1,NM), SLICE_DEF(NM),
     :                    SLICE(1,1,NM), STATUS )
	END IF
	IF(STATUS.NE.SAI__OK) GO TO 9000

      END DO
      NDCURR = ND
      STOFF = MODEL.NCOMP
      STACKPT = MODEL.NCOMP

*    Loop through string elements
      DO I=1,LSTRING
	SYM=MODEL.POLISH(I:I)
	IF(SYM.EQ.'M')THEN
	  NMST=NMST+1
	  MSTACKPT=MSTACKPT+1
	  MSTACK(MSTACKPT)=NMST
	  GO TO 100				! Go on to next symbol
	END IF

*      Check for illegal symbol
	IF(SYM.NE.'+'.AND.SYM.NE.'-'.AND.SYM.NE.'*')THEN
	  STATUS=SAI__ERROR
	  CALL ERR_REP('BAD_SYM','Bad symbol in polish string',STATUS)
	  GOTO 9000
	END IF

*     Operator encountered

*      Pop top model
	N1=MSTACK(MSTACKPT)
	MSTACKPT=MSTACKPT-1
	IF(N1.GT.STOFF)THEN			! Model to be removed from
	  STACKPT=STACKPT-1			! top portion of STACK
	END IF

*      Pop second model
	N2=MSTACK(MSTACKPT)
	MSTACKPT=MSTACKPT-1
	IF(N2.GT.STOFF)THEN			! Model to be removed from
	  STACKPT=STACKPT-1			! top portion of STACK
	END IF

*      Combine the two models and enter result at top of STACK
	STACKPT=STACKPT+1

*      Check that max stack size has not been exceeded
	IF(STACKPT.GT.MAXSTACK)THEN
	  STATUS=SAI__ERROR
	  CALL ERR_REP('OVERSTACKED','Max. size of computed model'//
     :      ' stack exceeded',STATUS)
	  GOTO 9000
	END IF

*      Apply operator
	IF(SYM.EQ.'+')THEN

*        When adding models together the output is never itself a slice
*        (this would only be true if both inputs were conformal or adjacent
*        slices, and it would add a lot of complexity to do that test here).
*        We can still make the saving performing the calculation however.

*        Both slices?
          IF ( SLICE_DEF(N1) .AND. SLICE_DEF(N2) ) THEN

            CALL ARR_INIT1R( 0.0, NMDAT, STACK(1,STACKPT), STATUS )
            CALL FIT_MCALC_COP( LDIMS(1), LDIMS(2), LDIMS(3),
     :                          SLICE(1,1,N1), STACK(1,N1),
     :                          STACK(1,STACKPT) )
            CALL FIT_MCALC_ADD( LDIMS(1), LDIMS(2), LDIMS(3),
     :                          SLICE(1,1,N2), STACK(1,N2),
     :                          STACK(1,STACKPT) )

*        Only N1 is a slice
          ELSE IF ( SLICE_DEF(N1) ) THEN

            CALL ARR_COP1R( NMDAT, STACK(1,N2), STACK(1,STACKPT),
     :                      STATUS )
            CALL FIT_MCALC_ADD( LDIMS(1), LDIMS(2), LDIMS(3),
     :                          SLICE(1,1,N1), STACK(1,N1),
     :                          STACK(1,STACKPT) )

*        Only N2 is a slice
          ELSE IF ( SLICE_DEF(N2) ) THEN

            CALL ARR_COP1R( NMDAT, STACK(1,N1), STACK(1,STACKPT),
     :                      STATUS )
            CALL FIT_MCALC_ADD( LDIMS(1), LDIMS(2), LDIMS(3),
     :                          SLICE(1,1,N2), STACK(1,N2),
     :                          STACK(1,STACKPT) )

*        Neither input is a slice
          ELSE
	    DO J=1,NMDAT
	      STACK(J,STACKPT)=STACK(J,N2)+STACK(J,N1)
	    ENDDO

          END IF
          SLICE_DEF(STACKPT) = .FALSE.

	ELSE IF(SYM.EQ.'-')THEN

*        Both slices?
          IF ( SLICE_DEF(N1) .AND. SLICE_DEF(N2) ) THEN

            CALL ARR_INIT1R( 0.0, NMDAT, STACK(1,STACKPT), STATUS )
            CALL FIT_MCALC_COP( LDIMS(1), LDIMS(2), LDIMS(3),
     :                          SLICE(1,1,N2), STACK(1,N2),
     :                          STACK(1,STACKPT) )
            CALL FIT_MCALC_SUB( LDIMS(1), LDIMS(2), LDIMS(3),
     :                          SLICE(1,1,N1), STACK(1,N1),
     :                          STACK(1,STACKPT) )

*        Only N1 is a slice
          ELSE IF ( SLICE_DEF(N1) ) THEN

            CALL ARR_COP1R( NMDAT, STACK(1,N2), STACK(1,STACKPT),
     :                      STATUS )
            CALL FIT_MCALC_SUB( LDIMS(1), LDIMS(2), LDIMS(3),
     :                          SLICE(1,1,N1), STACK(1,N1),
     :                          STACK(1,STACKPT) )

*        Only N2 is a slice
          ELSE IF ( SLICE_DEF(N2) ) THEN

            CALL ARR_INIT1R( 0.0, NMDAT, STACK(1,STACKPT), STATUS )
            CALL FIT_MCALC_ADD( LDIMS(1), LDIMS(2), LDIMS(3),
     :                          SLICE(1,1,N2), STACK(1,N2),
     :                          STACK(1,STACKPT) )
	    DO J=1,NMDAT
	      STACK(J,STACKPT)=STACK(J,STACKPT)-STACK(J,N1)
	    ENDDO

*        Neither input is a slice
          ELSE
	    DO J=1,NMDAT
	      STACK(J,STACKPT)=STACK(J,N2)-STACK(J,N1)
	    ENDDO

          END IF

          SLICE_DEF(STACKPT) = .FALSE.

	ELSE IF(SYM.EQ.'*')THEN

*        Model multiplication is slightly different in that if only one of
*        inputs is a slice, then the output is a slice as well. This is
*        because we insist that the data elements outside a slice are set
*        to zero. Again we could find the intersection of two multipled
*        slices but it is hard to see where such a thing would be used so
*        we ignore this case.

*        Both slices?
          IF ( SLICE_DEF(N1) .AND. SLICE_DEF(N2) ) THEN

            CALL ARR_INIT1R( 0.0, NMDAT, STACK(1,STACKPT), STATUS )
            CALL FIT_MCALC_COP( LDIMS(1), LDIMS(2), LDIMS(3),
     :                          SLICE(1,1,N1), STACK(1,N1),
     :                          STACK(1,STACKPT) )
            CALL FIT_MCALC_MUL( LDIMS(1), LDIMS(2), LDIMS(3),
     :                          SLICE(1,1,N2), STACK(1,N2),
     :                          STACK(1,STACKPT) )

*        One input is a slice?
          ELSE IF ( SLICE_DEF(N1) .XOR. SLICE_DEF(N2) ) THEN

*          Get here if only N2 is a slice
            IF ( SLICE_DEF(N2) ) THEN
              CALL ARR_COP1R( NMDAT, STACK(1,N1), STACK(1,STACKPT),
     :                        STATUS )
              CALL FIT_MCALC_MUL( LDIMS(1), LDIMS(2), LDIMS(3),
     :                            SLICE(1,1,N2), STACK(1,N2),
     :                            STACK(1,STACKPT) )
              CALL ARR_COP1I( 2*DAT__MXDIM, SLICE(1,1,N2),
     :                        SLICE(1,1,STACKPT), STATUS )

*          Get here if only N1 is a slice
            ELSE
              CALL ARR_COP1R( NMDAT, STACK(1,N2), STACK(1,STACKPT),
     :                        STATUS )
              CALL FIT_MCALC_MUL( LDIMS(1), LDIMS(2), LDIMS(3),
     :                            SLICE(1,1,N1), STACK(1,N1),
     :                            STACK(1,STACKPT) )
              CALL ARR_COP1I( 2*DAT__MXDIM, SLICE(1,1,N1),
     :                        SLICE(1,1,STACKPT), STATUS )
            END IF

*        Neither input is a slice
          ELSE
	    DO J=1,NMDAT
	      STACK(J,STACKPT)=STACK(J,N2)*STACK(J,N1)
	    ENDDO

          END IF

*        Output is a slice only if inputs' slice states are opposite
          SLICE_DEF(STACKPT) = ( SLICE_DEF(N1) .XOR. SLICE_DEF(N2) )

	END IF

*      Enter pointer to result in MSTACK
	MSTACKPT=MSTACKPT+1
	MSTACK(MSTACKPT)=STACKPT
 100	CONTINUE

      END DO

*    Exit sequence - return with cmodel predicted data in PRED
*    Test whether we have either a compound model or a signle pmodel
      IF ( (STACKPT .EQ. (STOFF+1)) .OR. (STACKPT.EQ.1) ) THEN

*      Last computed model was a slice?
        IF ( SLICE_DEF(STACKPT) ) THEN
          CALL ARR_INIT1R( 0.0, NMDAT, PRED, STATUS )
          CALL FIT_MCALC_COP( LDIMS(1), LDIMS(2), LDIMS(3),
     :                        SLICE(1,1,STACKPT), STACK(1,STACKPT),
     :                        PRED )
        ELSE
          CALL ARR_COP1R( NMDAT, STACK(1,STACKPT), PRED, STATUS )
        END IF

      ELSE IF ( STACKPT .EQ. 0 ) THEN

*      No model entered
	STATUS = SAI__ERROR
	CALL ERR_REP('NO_MOD','No model entered',STATUS)

      ELSE

*      Error
	STATUS=SAI__ERROR
	CALL ERR_REP('EX_ERR','Logic error in FIT_MCALC',STATUS)

      END IF

*    Exit
 9000 IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_MCALC', STATUS )
      END IF

      END


*+  FIT_MCALC_ADD - Add IN to a slice of OUT
*-
      SUBROUTINE FIT_MCALC_ADD( N1, N2, N3, SLICE, IN, OUT )
      IMPLICIT NONE
      INTEGER N1,N2,N3,SLICE(2,*),I,J,K
      REAL IN(N1,N2,N3), OUT(N1,N2,N3)
      DO K = SLICE(1,3), SLICE(2,3)
        DO J = SLICE(1,2), SLICE(2,2)
          DO I = SLICE(1,1), SLICE(2,1)
            OUT(I,J,K) = OUT(I,J,K) + IN(I,J,K)
          END DO
        END DO
      END DO

      END


*+  FIT_MCALC_COP - Copy IN into a slice of OUT
*-
      SUBROUTINE FIT_MCALC_COP( N1, N2, N3, SLICE, IN, OUT )
      IMPLICIT NONE
      INTEGER N1,N2,N3,SLICE(2,*),I,J,K
      REAL IN(N1,N2,N3), OUT(N1,N2,N3)
      DO K = SLICE(1,3), SLICE(2,3)
        DO J = SLICE(1,2), SLICE(2,2)
          DO I = SLICE(1,1), SLICE(2,1)
            OUT(I,J,K) = IN(I,J,K)
          END DO
        END DO
      END DO

      END



*+  FIT_MCALC_SUB - Subtract IN from a slice of OUT
*-
      SUBROUTINE FIT_MCALC_SUB( N1, N2, N3, SLICE, IN, OUT )
      IMPLICIT NONE
      INTEGER N1,N2,N3,SLICE(2,*),I,J,K
      REAL IN(N1,N2,N3), OUT(N1,N2,N3)
      DO K = SLICE(1,3), SLICE(2,3)
        DO J = SLICE(1,2), SLICE(2,2)
          DO I = SLICE(1,1), SLICE(2,1)
            OUT(I,J,K) = OUT(I,J,K) - IN(I,J,K)
          END DO
        END DO
      END DO

      END



*+  FIT_MCALC_MUL - Multiply a slice of OUT by IN
*-
      SUBROUTINE FIT_MCALC_MUL( N1, N2, N3, SLICE, IN, OUT )
      IMPLICIT NONE
      INTEGER N1,N2,N3,SLICE(2,*),I,J,K
      REAL IN(N1,N2,N3), OUT(N1,N2,N3)
      DO K = SLICE(1,3), SLICE(2,3)
        DO J = SLICE(1,2), SLICE(2,2)
          DO I = SLICE(1,1), SLICE(2,1)
            OUT(I,J,K) = OUT(I,J,K) * IN(I,J,K)
          END DO
        END DO
      END DO

      END
