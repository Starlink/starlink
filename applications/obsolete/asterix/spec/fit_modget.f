*+ FIT_MODGET - Extracts model specification from fit_model object
      SUBROUTINE FIT_MODGET(FID,IMOD,NPAR,PARAM,LB,UB,LE,UE,FROZEN,
     :             STATUS)
*
*    Description :
*
*     Gets model details from fit_model object.
*     If LB>=UB for a parameter then the FROZEN flag is set for it.
*     Any parameter errors not available are set to -1.
*     Note that model stack is NOT set up, since it must be of size
*     NMAX*MAXSTACK, where NMAX is the length of the longest dataset to be
*     modelled. The pointer to the stack array is explicitly initialised to
*     zero as a flag for further software.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Trevor Ponman (BHVAD::TJP)
*
*    History :
*
*     31 Mar 87 : Original - adapted from MODPAR (TJP)
*     29 Jun 87 : Changes to structure of fit_model object (TJP)
*     22 Jul 87 : Parameter error estimates included (TJP)
*     14 Apr 88 : MODEL_SPEC structure extended to include PARNAME & UNITS (TJP)
*     23 Sep 88 : Input file name displayed (TJP)
*     20 Oct 88 : Model locator included in MODEL_SPEC structure (TJP)
*     22 Feb 89 : ASTERIX88 version - no significant change (TJP)
*     24 Jun 92 : Error handling corrected. Efficiency improvements by
*                 checking existance of objects before CMP_GETing (DJA)
*     10 Jan 94 : Added NPAR object to model structure (DJA)
*     24 Jan 94 : Added SPEC and ADDITIVE objects to model structure (DJA)
*     21 Apr 95 : Adapted from pure HDS version (DJA)
*      2 May 95 : Added format field (DJA)
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
      INTEGER			FID			! Model dataset id
*
*    Export
*
c     RECORD /MODEL_SPEC/ 	MODEL			! Model specification
      INTEGER			IMOD
      INTEGER 			NPAR			! Total no of cmodel parameters
      REAL 			PARAM(NPAMAX)		! Complete set of cmodel parameters
      REAL 			LB(NPAMAX)		! Parameter lower bounds
      REAL 			UB(NPAMAX)		! Parameter upper bounds
      REAL 			LE(NPAMAX)		! Lower parameter error (1 sigma)
      REAL 			UE(NPAMAX)		! Upper parameter error (1 sigma)
      LOGICAL 			FROZEN(NPAMAX)		! Parameter frozen?
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
      CHARACTER*80              CBUF                    ! Character buffer

      CHARACTER*(DAT__SZLOC) 	FLOC			! FIT_MODEL object
      CHARACTER*(DAT__SZLOC) 	PMLOC	 		! locator to pmodels
      CHARACTER*(DAT__SZLOC) 	MILOC     		! locator to pmodel I
      CHARACTER*(DAT__SZLOC) 	MIPLOC    		! locator to parameters of pmodel I
      CHARACTER*(DAT__SZLOC) 	MIPJLOC   		! locator to parameter J of pmodel I
      CHARACTER*(DAT__SZTYP) 	TYP	 		! object type

      CHARACTER*14		TYPE			! pmodel type

      INTEGER           	TSTART(MAXTIE)
      INTEGER           	TGROUP(NPAMAX)

      INTEGER 			JPAR			! No. pmodel parameters
      INTEGER 			I,J

      LOGICAL 			THERE                   ! HDS component exists?
*-

*    Status check
      IF ( STATUS.NE.SAI__OK ) RETURN

*    Store identifier
      MODEL_SPEC_M_ID(IMOD) = FID

*    Extract locator from identifier
      CALL ADI1_GETLOC( FID, FLOC, STATUS )

*    Check the fit_model object and pull out the model information
      CALL DAT_TYPE( FLOC, TYP, STATUS )
      IF ( TYP .NE. 'FIT_MODEL' ) THEN
	STATUS = SAI__ERROR
	CALL ERR_REP( 'WRONG_OBJ', 'Not a fit_model object', STATUS )
	GOTO 99
      ELSE
	CALL DISP_FILENAM( FID,'Input model',STATUS)
      END IF

      CALL CMP_GET0C(FLOC,'SPEC',MODEL_SPEC_SPEC(IMOD),STATUS)
      IF(STATUS.NE.SAI__OK.OR.CHR_LEN(MODEL_SPEC_SPEC(IMOD)).EQ.0)THEN
	STATUS = SAI__ERROR
	CALL ERR_REP('NOMOD','Existing fit_model object contains no'
     :    //' model',STATUS)
	GOTO 99
      ENDIF
      CALL CMP_GET0C(FLOC,'POLISH',MODEL_SPEC_POLISH(IMOD),STATUS)
      CALL CMP_GET0I(FLOC,'NCOMP',MODEL_SPEC_NCOMP(IMOD),STATUS)
      IF ( MODEL_SPEC_NCOMP(IMOD) .GT. MAXCOMP ) THEN
	STATUS = SAI__ERROR
	CALL ERR_REP('MANYCOMP','Maximum number of model components'//
     :    ' exceeded',STATUS)
	GOTO 99
      END IF
      CALL DAT_FIND(FLOC,'PMODEL',PMLOC,STATUS) ! Locate array of pmodels
      IF(STATUS.NE.SAI__OK) GOTO 99

*    Step through model components finding their keys,counting the parameters,
*    setting initial values and lower and upper bounds
      NPAR = 0
      DO I=1,MODEL_SPEC_NCOMP(IMOD)

*      Get locator to ith pmodel
	CALL DAT_CELL(PMLOC,1,I,MILOC,STATUS)
	CALL CMP_GET0C(MILOC,'KEY',MODEL_SPEC_KEY(IMOD,I),STATUS)
	CALL CMP_GET0I(MILOC,'NPAR',JPAR,STATUS)
	CALL CMP_GET0C(MILOC,'TYPE',TYPE,STATUS)
	CALL DAT_FIND(MILOC,'PAR',MIPLOC,STATUS)
	MODEL_SPEC_ISTART(IMOD,I)=NPAR+1

*      Model is additive?
        MODEL_SPEC_ADDITIVE(IMOD,I) = ( TYPE(1:8) .EQ. 'additive' )

*        Loop through parameters of ith pmodel
	  DO J=1,JPAR
	    NPAR=NPAR+1

*          Get locator to jth parameter of pmodel i
	    CALL DAT_CELL(MIPLOC,1,J,MIPJLOC,STATUS)
	    CALL CMP_GET0C(MIPJLOC,'NAME',CBUF,STATUS)
            MODEL_SPEC_PARNAME(IMOD,NPAR) = CBUF
	    CALL CMP_GET0C(MIPJLOC,'UNITS',CBUF,STATUS)
            MODEL_SPEC_UNITS(IMOD,NPAR) = CBUF
            CALL DAT_THERE(MIPJLOC,'FORMAT',THERE,STATUS)
            IF ( THERE ) THEN
	      CALL CMP_GET0C(MIPJLOC,'FORMAT',CBUF,STATUS)
            ELSE
              CBUF = ' '
            END IF
            MODEL_SPEC_FORMAT(IMOD,NPAR) = CBUF
	    CALL CMP_GET0R(MIPJLOC,'VAL',PARAM(NPAR),STATUS)
	    CALL CMP_GET0R(MIPJLOC,'LOW',LB(NPAR),STATUS)
	    CALL CMP_GET0R(MIPJLOC,'HI',UB(NPAR),STATUS)
	    CALL CMP_GET0L(MIPJLOC,'FROZEN',FROZEN(NPAR),STATUS)
	    IF(STATUS.NE.SAI__OK) GOTO 99

*          Lower error exists?
            CALL DAT_THERE( MIPJLOC, 'LERROR', THERE, STATUS )
            IF ( THERE ) THEN
	      CALL CMP_GET0R( MIPJLOC, 'LERROR', LE(NPAR), STATUS )
	      IF ( STATUS .NE. SAI__OK ) THEN
	        CALL ERR_ANNUL( STATUS )
	        LE(NPAR)=-1.0		! Flag as 'not available'
	      END IF
            ELSE
	      LE(NPAR)=-1.0		! Flag as 'not available'
            END IF

*          Upper error exists?
            CALL DAT_THERE( MIPJLOC, 'UERROR', THERE, STATUS )
            IF ( THERE ) THEN
	      CALL CMP_GET0R(MIPJLOC,'UERROR',UE(NPAR),STATUS)
	      IF(STATUS.NE.SAI__OK)THEN
	        CALL ERR_ANNUL(STATUS)
	        UE(NPAR)=-1.0		! Flag as 'not available'
	      END IF
            ELSE
	      UE(NPAR)=-1.0		! Flag as 'not available'
            END IF

*       Freeze parameter if LB>=UB
	    IF(LB(NPAR).GE.UB(NPAR)) FROZEN(NPAR)=.TRUE.

* Tidy up
	    CALL DAT_ANNUL(MIPJLOC,STATUS)
	  END DO
	  CALL DAT_ANNUL(MIPLOC,STATUS)
	  CALL DAT_ANNUL(MILOC,STATUS)
	END DO
	CALL DAT_ANNUL(PMLOC,STATUS)

*    Check that total number of parameters is within allowed range
      IF ( NPAR .GT. NPAMAX ) THEN
	STATUS = SAI__ERROR
	CALL ERR_REP('MANYPAR','Maximum number of model parameters'//
     :    ' exceeded',STATUS)
	GOTO 99
      ELSE
        MODEL_SPEC_NPAR(IMOD) = NPAR
      END IF

*    Copy over local copies of tie
      DO I = 1, MAXTIE
        TSTART(I) = MODEL_SPEC_TSTART(IMOD, I)
      END DO
      DO I = 1, NPAMAX
        TGROUP(I) = MODEL_SPEC_TGROUP(IMOD, I)
      END DO

*    Get constraints
      CALL FIT_MODGET_TIES( FLOC, MODEL_SPEC_NTIE(IMOD),
     :                      TSTART, TGROUP, STATUS )

*    Now copy the full ties back
      DO I = 1, MAXTIE
        MODEL_SPEC_TSTART(IMOD, I) = TSTART(I)
      END DO
      DO I = 1, NPAMAX
        MODEL_SPEC_TGROUP(IMOD, I) = TGROUP(I)
      END DO

*    Initialise stack pointer to 0 (flag for pointer not set)
      MODEL_SPEC_STACKPTR(IMOD) = 0

*    Exit
 99   IF ( STATUS.NE.SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_MODGET', STATUS )
      END IF

      END
