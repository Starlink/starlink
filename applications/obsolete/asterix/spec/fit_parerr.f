*+  FIT_PARERR - Estimates 1 sigma errors in best fit parameters
      SUBROUTINE FIT_PARERR(NDS,OBDAT,INSTR,MODEL,NPAR,PARAM,LB,UB,DPAR,
     :       FROZEN,PEGGED,SSCALE,FSTAT,PREDICTOR,PREDDAT,PARSIG,STATUS)
*
*    Description :
*
*     Generates difference estimate of the alpha matrix (2nd derivs/2) and
*     inverts it to give an estimate of the parameter errors. This is accurate
*     if the chi-squared surface is a quadratic function of the parameters
*     near its minimum. (Note that within this approximation, the effects of
*     elongation of the chi-squared contours in directions between parameter
*     axes ARE taken into account - via the off diagonal elements in the
*     matrix of second derivatives.)
*     Only active parameters (i.e. not frozen or on bounds) are included;
*     errors are returned as zero for frozen or pegged parameters.
*
*    Method :
*
*     The matrix of second derivatives is set up (in terms of derivatives of
*     reduced chi-squared with respect to scaled parameter values, to improve
*     the chances of a well-conditioned matrix) and inverted. The diagonal
*     elements of the resulting matrix are simply related to the variances of
*     the corresponding parameters.
*     See Bevington pp 242- for justification.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Trevor Ponman (BHVAD::TJP)
*     David J. Allan (BHVAD::DJA)
*     Richard Beard (RB)
*
*    History :
*
*     21 May 87 : Original (adapted from SPEC_PARERR)
*      3 May 88 : New structures, global eliminated (TJP)
*      1 Jul 88 : Amended to handle NDOF=0 (TJP)
*     23 Sep 88 : Small mod to cope with case where all params peg (TJP)
*     26 Mar 92 : FIT_PREDDAT passed as external PREDICTOR (RJV)
*     27 May 92 : Allow maximum likelihood fitting. Error handling fixed.
*                 Derivatives now D.P. (DJA)
*     15 Jun 92 : FSTAT added and NDOF changed to SSCALE (DJA)
*     18 Aug 92 : Statistic now double precision (DJA)
*     16 Dec 92 : Changed IFAIL value to allow trap to work (DJA)
*     19 May 94 : Updated to handle parameter constraints (DJA)
*     10 Mar 97 : Update NAG call F01AAF to multiple calls (RB)
*      6 Jun 97 : Convert to PDA (RB)
*      3 Jul 97 : Add FIT_PARERR_INT for exact matrix size (RB)
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
      INTEGER             NPAR			! No of parameters
      REAL                PARAM(NPAMAX)		! Model parameters
      REAL                LB(NPAR)		! Parameter lower bounds
      REAL                UB(NPAR)		! Parameter upper bounds
      REAL                DPAR(NPAR)		! Desired differential increments
      LOGICAL             FROZEN(NPAR)		! Parameter frozen?
      LOGICAL             PEGGED(NPAR)		! Parameter pegged on bound?
      INTEGER             SSCALE		! Statistic scale factor
      INTEGER             FSTAT                 ! Statistic to use
      EXTERNAL            PREDICTOR             ! Model data predictor
*
*    Import/export :
*
      RECORD /PREDICTION/ PREDDAT(NDS)	        ! Data predicted by model
                                                ! (actually only the data
*                                               ! pointed to are updated)
*    Export :
*
      REAL                PARSIG(NPAR)		! Sigma values for parameters
*
*    Status :
*
      INTEGER             STATUS
*
*    Local variables :
*
      DOUBLE PRECISION    ALPHA(NPAMAX,NPAMAX)	! Matrix of 2nd derivs (*1/2)
      INTEGER             ALPHAINV              ! Inverse of alpha matrix
      DOUBLE PRECISION    LSTATDERIV		! Lower reduced statistic deriv.
      DOUBLE PRECISION    USTATDERIV		! Upper reduced statistic deriv.

      REAL                DPUP(NPAMAX)		! Actual upper param increments
      REAL                DPDOWN(NPAMAX)	! Actual lower param increments
      REAL                LPAR(NPAMAX)		! Lower parameter arrays
      REAL                PSCALE(NPAMAX)	! Parameter scale factors
      REAL                UPAR(NPAMAX)		! Upper parameter arrays

      INTEGER             IFAIL			! PDA failure flag
      INTEGER             IPFREE(NPAMAX)	! Free parameter indices
      INTEGER             J,K    		! Parameter indices
      INTEGER             JP1,JP2		! Parameter indices
      INTEGER             NPFREE		! No of free (unfrozen+unpegged params
      INTEGER             LSSCALE		! SSCALE proofed against zero value
      INTEGER		  DIMS(2)		! Dimensions of inverse matrix

      LOGICAL		  LFROZEN(NPAMAX)	! Local frozen array
*-

*    Status check
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Extend FROZEN array to handle constrained parameters
      CALL FIT1_LOCFRO( MODEL, NPAR, FROZEN, LFROZEN, STATUS )

*    Set up upper and lower increments for parameters (taking bounds into
*    account) and parameter scale factors
      DO J=1,NPAR
	IF ( .NOT. LFROZEN(J) ) THEN
	  PSCALE(J)=(UB(J)-LB(J))/PARRANGE
	  IF ( PARAM(J)+DPAR(J) .LE. UB(J) ) THEN
	    DPUP(J) = DPAR(J)
	  ELSE
	    DPUP(J) = UB(J)-PARAM(J)
	  END IF
	  IF ( PARAM(J)-DPAR(J) .GE. LB(J) ) THEN
	    DPDOWN(J) = DPAR(J)
	  ELSE
	    DPDOWN(J) = PARAM(J)-LB(J)
	  END IF
	END IF
      END DO

*    Initialisation
      DO J = 1, NPAR
	LPAR(J)=PARAM(J)
	UPAR(J)=PARAM(J)
      END DO
      NPFREE = 0
      IF ( SSCALE .GT. 0 ) THEN
	LSSCALE = SSCALE
      ELSE IF ( SSCALE .EQ. 0 ) THEN
	LSSCALE = 1			! To avoid zero divide
      ELSE
	STATUS = SAI__ERROR
	CALL ERR_REP( ' ', 'More free parameters than data values!',
     :                                                      STATUS )
	GOTO 9000
      END IF

*    Look for free parameters
      DO J = 1, NPAR
	IF ( (.NOT.LFROZEN(J)) .AND. (.NOT.PEGGED(J)) ) THEN
	  NPFREE = NPFREE + 1
	  IPFREE(NPFREE) = J
	END IF
      END DO

*    Calculate derivative in scaled statistic at incremented positions
*    Loop over free parameters
      DO J = 1, NPFREE

*      Loop over all other free parameters
	DO K = 1, J

*        Get original parameter indices
	  JP1 = IPFREE(J)
	  JP2 = IPFREE(K)

*        Construct parameter sets for -ve and +ve deviation in parameter JP1
	  LPAR(JP1)=PARAM(JP1)-DPDOWN(JP1)
	  UPAR(JP1)=PARAM(JP1)+DPUP(JP1)

*        Find derivative at -+ve deviations
	  CALL FIT_STATDERIV(NDS,OBDAT,INSTR,MODEL,NPAR,LPAR,LB,UB,JP2,
     :             DPAR(JP2),FSTAT,PREDICTOR,PREDDAT,LSTATDERIV,STATUS)
	  CALL FIT_STATDERIV(NDS,OBDAT,INSTR,MODEL,NPAR,UPAR,LB,UB,JP2,
     :             DPAR(JP2),FSTAT,PREDICTOR,PREDDAT,USTATDERIV,STATUS)
          IF(STATUS.NE.SAI__OK) GO TO 9000

*        Form alpha matrix (derivs of reduced chisq in terms of scaled params)
	  ALPHA(J,K) = 0.5D0*(USTATDERIV-LSTATDERIV)*PSCALE(JP1)
     :                  *PSCALE(JP2)/(LSSCALE*(DPDOWN(JP1)+DPUP(JP1)))
	  IF ( K.NE.J ) ALPHA(K,J) = ALPHA(J,K)

*        Reset parameters
	  LPAR(JP1) = PARAM(JP1)
	  UPAR(JP1) = PARAM(JP1)

	END DO
      END DO

*    Invert alpha matrix to get parameter errors
      IFAIL = 0
      IF ( NPFREE .GT. 0 ) THEN
        DIMS(1) = NPFREE
        DIMS(2) = NPFREE
        CALL DYN_MAPD( 2, DIMS, ALPHAINV, STATUS )
        CALL FIT_PARERR_INT( ALPHA, %VAL(ALPHAINV), NPFREE, IFAIL )
      END IF
      IF ( IFAIL .EQ. 0 ) THEN
	J = 1
	DO K = 1, NPAR
	  IF ( K .EQ. IPFREE(J) ) THEN
	    PARSIG(K) = PSCALE(K)*SQRT(ABS(ALPHA(J,J))/LSSCALE)
	    J = J + 1
	  ELSE
	    PARSIG(K) = 0.0
	  END IF
	END DO

*      Copy errors to dependent parameters
        IF ( MODEL.NTIE .GT. 0 ) THEN
          CALL FIT1_COPDEP( MODEL, PARSIG, STATUS )
        END IF

      ELSE
	CALL MSG_SETI('IFAIL',IFAIL)
        STATUS = SAI__ERROR
	CALL ERR_REP( ' ', 'Error in PDA_DGEDI: U(^IFAIL,^IFAIL) = 0',
     :                STATUS )
      END IF

*    Exit
 9000 IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_PARERR', STATUS )
      END IF

      END


*+ FIT_PARERR_INT - Calculate inverse of matrix
      SUBROUTINE FIT_PARERR_INT( ALPHA, INV, N, STATUS )
*
*  Description:
*    PDA routines for matrix inversion required variables of the exact
*    size to be passed, therfore an internal subroutine and the use of
*    pointers is necessary.
*    Returns the inverse back in ALPHA to reduce the fuss above.
*
*  Authors:
*    RB: Richard Beard (ROSAT, University of Birmingham)
*
*  History:
*    03 Jul 1997 : (RB)
*      Original version
*-

* Type definitions:
      IMPLICIT NONE

* Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'FIT_PAR'

*  Import/export:
      DOUBLE PRECISION		ALPHA(NPAMAX,NPAMAX)
      DOUBLE PRECISION		INV(N,N)
      INTEGER			N
      INTEGER			STATUS

*  Local variables:
      DOUBLE PRECISION		DET(2)
      DOUBLE PRECISION		WORK(NPAMAX)
      INTEGER			IWORK(NPAMAX)
      INTEGER			I, J
*.
      IF ( STATUS .NE. SAI__OK ) RETURN

      DO I = 1, N
        DO J = 1, N
          INV(I,J) = ALPHA(I,J)
        END DO
      END DO

      CALL PDA_DGEFA( INV, N, N, IWORK, STATUS )
      IF ( STATUS .EQ. 0 ) THEN
        CALL PDA_DGEDI( INV, N, N, IWORK, DET, WORK, 1 )
      END IF

      DO I = 1, N
        DO J = 1, N
          ALPHA(I,J) = INV(I,J)
        END DO
      END DO

      END
