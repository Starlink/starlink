*+  FIT_MCALL - calls appropriate subroutine to calculate data from a pmodel
      SUBROUTINE FIT_MCALL(GENUS,KEY,NPAR,PARAM,ND,NMDIM,IDIMM,NMDAT,
     :         NMBOUND,MLBOUND,MUBOUND,PRED,SLICE_DEF,SLICE,STATUS)
*
*    Description :
*
*     Calls the appropriate model subroutine to calculate a pmodel.
*
*     NOTE: THIS ROUTINE MUST BE EXTENDED WHEN A NEW MODEL IS ADDED TO THE MENU
*     AVAILABLE.
*
*    Method :
*
*     The model genus and keyword are inspected and the appropriate subroutine
*     called to calculate the model space data in the NMDAT model bins
*     delineated by the bin bounds MLBOUND:MUBOUND, from the model with
*     parameters passed in PARAM (starting at element N1).
*     If the key is U1,U2 or U3 then a usermodel routine is called irrespective
*     of the genus. The interface to this routine is simplified for the 1D
*     case, but requires full specification of data dimensions etc for higher
*     dimensionality.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Trevor Ponman  (BHVAD::TJP)
*
*    History :
*
*      4 Feb 85 : Original (MCALL)
*      3 Oct 86 : Renamed to SPEC_MCALL (BHVAD::TJP)
*     18 Feb 87 : FIT_MCALL original (BHVAD::TJP)
*      2 Jun 87 : SPEC_LL added (TJP)
*     20 Apr 88 : Lower and upper model bounds included (TJP)
*     27 Apr 88 : Usermodels 1,2 and 3 incorporated (TJP)
*     22 Aug 88 : Sunyaev-Titarchuk Comptonization model incorporated (CGH)
*     24 Jan 89 : Chapline-Stevens model. Invalid parameter report. (TJP)
*     23 Feb 89 : Dataset number passed in, may be needed by some models (TJP)
*      2 Mar 89 : Raymond-Smith plasma model (TJP)
*     22 Nov 89 : AB and AG models distinguished (TJP)
*      6 Dec 90 : AC, EC, ED & AP models added (TJP)
*     10 May 91 : DM and CF models added (TJP)
*      9 Nov 92 : AV model (TJP)
*     12 Jan 93 : Error reporting done properly (DJA)
*     18 Aug 93 : Handles keys of any length (DJA)
*      7 Jan 94 : SLICE_DEF and SLICE arguments added. No uses NPAR and only
*                 has access to its own parameters. (DJA)
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
*
*    Import :
*
	CHARACTER*(*) GENUS			! Model genus - SPEC, STAT etc.
	CHARACTER*(*) KEY			! Keyword of pmodel
	INTEGER NPAR				! Position of 1st parameter
	REAL PARAM(NPAR)			! Array of model parameters
	INTEGER ND				! Current dataset number
	INTEGER NMDIM				! No of model dimensions
	INTEGER IDIMM(NMDIM)			! Size of each dimension
	INTEGER NMDAT				! No of model data values
	INTEGER NMBOUND				! No of model bin bounds
	REAL MLBOUND(NMBOUND)			! Model bin lower boundaries
	REAL MUBOUND(NMBOUND)			! Model bin upper boundaries
*
*    Export :
*
      REAL 			PRED(NMDAT)	! Predicted model-space data
      LOGICAL			SLICE_DEF	! Slice defined by SPEC_xx?
      INTEGER			SLICE(2,*)	! Slice indices
*
*    Status :
*
      INTEGER 			STATUS
*-

*    Status check
      IF ( STATUS .NE. SAI__OK ) RETURN

*    All the SPEC_xx routines change all predicted data
*    elements so we don't define a slice.
      SLICE_DEF = .FALSE.

* Call appropriate pmodel subroutine

*   User models - subroutine interface depends upon data dimensionality
	IF(KEY.EQ.'U1')THEN
	  IF(NMDIM.EQ.1)THEN
	    CALL FIT_USER1(NMDAT,MLBOUND,MUBOUND,PARAM,PRED,STATUS)
	  ELSE
	    CALL FIT_USER1(NMDAT,NMDIM,IDIMM,NMBOUND,MLBOUND,MUBOUND,
     :                     PARAM,PRED,STATUS)
	  ENDIF
	ELSE IF(KEY.EQ.'U2')THEN
	  IF(NMDIM.EQ.1)THEN
	    CALL FIT_USER2(NMDAT,MLBOUND,MUBOUND,PARAM,PRED,STATUS)
	  ELSE
	    CALL FIT_USER2(NMDAT,NMDIM,IDIMM,NMBOUND,MLBOUND,MUBOUND,
     :                     PARAM,PRED,STATUS)
	  ENDIF
	ELSE IF(KEY.EQ.'U3')THEN
	  IF(NMDIM.EQ.1)THEN
	    CALL FIT_USER3(NMDAT,MLBOUND,MUBOUND,PARAM,PRED,STATUS)
	  ELSE
	    CALL FIT_USER3(NMDAT,NMDIM,IDIMM,NMBOUND,MLBOUND,MUBOUND,
     :      PARAM,PRED,STATUS)
	  ENDIF

*   Spectral fitting	[ N.B. Spectral models all return predicted photon ]
*			[      flux in  photons/(cm**2 sec) in each bin    ]
	ELSE IF(GENUS.EQ.'SPEC')THEN
	  IF(KEY.EQ.'PL')THEN
	    CALL SPEC_PL(NMDAT,MLBOUND,MUBOUND,PARAM,PRED,STATUS)
	  ELSE IF(KEY.EQ.'BR')THEN
	    CALL SPEC_BR(NMDAT,MLBOUND,MUBOUND,PARAM,PRED,STATUS)
	  ELSE IF(KEY.EQ.'AB')THEN
	    CALL SPEC_AB(NMDAT,MLBOUND,MUBOUND,PARAM,PRED,STATUS)
	  ELSE IF(KEY.EQ.'AG')THEN
	    CALL SPEC_AG(NMDAT,MLBOUND,MUBOUND,PARAM,PRED,STATUS)
	  ELSE IF(KEY.EQ.'AV')THEN
	    CALL SPEC_AV(NMDAT,MLBOUND,MUBOUND,PARAM,PRED,STATUS)
	  ELSE IF(KEY.EQ.'AC')THEN
	    CALL SPEC_AC(NMDAT,MLBOUND,MUBOUND,PARAM,PRED,STATUS)
	  ELSE IF(KEY.EQ.'AP')THEN
	    CALL SPEC_AP(NMDAT,MLBOUND,MUBOUND,PARAM,PRED,STATUS)
	  ELSE IF(KEY.EQ.'EC')THEN
	    CALL SPEC_EC(NMDAT,MLBOUND,MUBOUND,PARAM,PRED,STATUS)
	  ELSE IF(KEY.EQ.'ED')THEN
	    CALL SPEC_ED(NMDAT,MLBOUND,MUBOUND,PARAM,PRED,STATUS)
	  ELSE IF(KEY.EQ.'BP')THEN
	    CALL SPEC_BP(NMDAT,MLBOUND,MUBOUND,PARAM,PRED,STATUS)
	  ELSE IF(KEY.EQ.'PC')THEN
	    CALL SPEC_PC(NMDAT,MLBOUND,MUBOUND,PARAM,PRED,STATUS)
	  ELSE IF(KEY.EQ.'WN')THEN
	    CALL SPEC_WN(NMDAT,MLBOUND,MUBOUND,PARAM,PRED,STATUS)
	  ELSE IF(KEY.EQ.'BB')THEN
	    CALL SPEC_BB(NMDAT,MLBOUND,MUBOUND,PARAM,PRED,STATUS)
	  ELSE IF(KEY.EQ.'BH')THEN
	    CALL SPEC_BH(NMDAT,MLBOUND,MUBOUND,PARAM,PRED,STATUS)
	  ELSE IF(KEY.EQ.'ST')THEN
	    CALL SPEC_ST(NMDAT,MLBOUND,MUBOUND,PARAM,PRED,STATUS)
	  ELSE IF(KEY.EQ.'CS')THEN
	    CALL SPEC_CS(NMDAT,MLBOUND,MUBOUND,PARAM,PRED,STATUS)
	  ELSE IF(KEY.EQ.'RZ')THEN
	    CALL SPEC_RZ(ND,NMDAT,MLBOUND,MUBOUND,PARAM,PRED,STATUS)
	  ELSE IF(KEY.EQ.'DM')THEN
	    CALL SPEC_DM(ND,NMDAT,MLBOUND,MUBOUND,PARAM,PRED,STATUS)
	  ELSE IF(KEY.EQ.'CF')THEN
	    CALL SPEC_CF(ND,NMDAT,MLBOUND,MUBOUND,PARAM,PRED,STATUS)
	  ELSE IF(KEY.EQ.'LG')THEN
	    CALL SPEC_LG(NMDAT,MLBOUND,MUBOUND,PARAM,PRED,STATUS)
	  ELSE IF(KEY.EQ.'LL')THEN
	    CALL SPEC_LL(NMDAT,MLBOUND,MUBOUND,PARAM,PRED,STATUS)
	  ELSE
	    CALL MSG_SETC('MODL',GENUS//'_'//KEY)
	    STATUS=SAI__ERROR
	    CALL ERR_REP('BAD_MODL','Model ^MODL unrecognised in FIT_MCALL',
     :      STATUS)
	  ENDIF

* Unrecognised genus
	ELSE
	  CALL MSG_SETC('GEN',GENUS)
	  STATUS=SAI__ERROR
	  CALL ERR_REP('BAD_GENUS','Model genus ^GEN not recognised',STATUS)
	ENDIF

*    Report if parameters have transgressed allowed bounds for routine, but
*    annul STATUS to allow fitting to proceed if possible
      IF ( STATUS .EQ. USER__001 ) THEN
	CALL ERR_ANNUL(STATUS)
	CALL MSG_SETC('MODEL',KEY)
	CALL MSG_PRNT( 'Invalid region of parameter space for'//
     :                                           ' model ^MODEL' )
      END IF

*    Exit
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_MCALL', STATUS )
      END IF

      END
