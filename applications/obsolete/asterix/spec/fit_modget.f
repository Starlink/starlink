*+ FIT_MODGET - Extracts model specification from fit_model object
      SUBROUTINE FIT_MODGET(FLOC,MODEL,NPAR,PARAM,LB,UB,LE,UE,FROZEN,
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
      CHARACTER*(DAT__SZLOC) 	FLOC			! FIT_MODEL object
*
*    Export
*
      RECORD /MODEL_SPEC/ 	MODEL			! Model specification
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

      CHARACTER*(DAT__SZLOC) 	PMLOC	 		! locator to pmodels
      CHARACTER*(DAT__SZLOC) 	MILOC     		! locator to pmodel I
      CHARACTER*(DAT__SZLOC) 	MIPLOC    		! locator to parameters of pmodel I
      CHARACTER*(DAT__SZLOC) 	MIPJLOC   		! locator to parameter J of pmodel I
      CHARACTER*(DAT__SZTYP) 	TYP	 		! object type

      CHARACTER*14		TYPE			! pmodel type

      INTEGER 			JPAR			! No. pmodel parameters
      INTEGER 			I,J
      INTEGER			NDIM			! Dummy from DAT_SHAPE

      LOGICAL 			THERE                   ! HDS component exists?
*-

*    Status check
      IF ( STATUS.NE.SAI__OK ) RETURN

*    Insert locator into model structure
      MODEL.MLOC = FLOC
      IF ( STATUS.NE.SAI__OK ) GOTO 99

*    Check the fit_model object and pull out the model information
      CALL DAT_TYPE( FLOC, TYP, STATUS )
      IF ( TYP .NE. 'FIT_MODEL' ) THEN
	STATUS = SAI__ERROR
	CALL ERR_REP( 'WRONG_OBJ', 'Not a fit_model object', STATUS )
	GOTO 99
      ELSE
	CALL DISP_FILENAM(FLOC,'Input model',STATUS)
      END IF

      CALL CMP_GET0C(FLOC,'SPEC',MODEL.SPEC,STATUS)
      IF(STATUS.NE.SAI__OK.OR.CHR_LEN(MODEL.SPEC).EQ.0)THEN
	STATUS = SAI__ERROR
	CALL ERR_REP('NOMOD','Existing fit_model object contains no'
     :    //' model',STATUS)
	GOTO 99
      ENDIF
      CALL CMP_GET0C(FLOC,'POLISH',MODEL.POLISH,STATUS)
      CALL CMP_GET0I(FLOC,'NCOMP',MODEL.NCOMP,STATUS)
      IF ( MODEL.NCOMP .GT. MAXCOMP ) THEN
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
      DO I=1,MODEL.NCOMP

*      Get locator to ith pmodel
	CALL DAT_CELL(PMLOC,1,I,MILOC,STATUS)
	CALL CMP_GET0C(MILOC,'KEY',MODEL.KEY(I),STATUS)
	CALL CMP_GET0I(MILOC,'NPAR',JPAR,STATUS)
	CALL CMP_GET0C(MILOC,'TYPE',TYPE,STATUS)
	CALL DAT_FIND(MILOC,'PAR',MIPLOC,STATUS)
	MODEL.ISTART(I)=NPAR+1

*      Model is additive?
        MODEL.ADDITIVE(I) = ( TYPE(1:8) .EQ. 'additive' )

*        Loop through parameters of ith pmodel
	  DO J=1,JPAR
	    NPAR=NPAR+1

*          Get locator to jth parameter of pmodel i
	    CALL DAT_CELL(MIPLOC,1,J,MIPJLOC,STATUS)
	    CALL CMP_GET0C(MIPJLOC,'NAME',CBUF,STATUS)
            MODEL.PARNAME(NPAR) = CBUF
	    CALL CMP_GET0C(MIPJLOC,'UNITS',CBUF,STATUS)
            MODEL.UNITS(NPAR) = CBUF
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
        MODEL.NPAR = NPAR
      END IF

*    Get constraints
      CALL FIT_MODGET_TIES( FLOC, MODEL.NTIE, MODEL.TSTART,
     :                      MODEL.TGROUP, STATUS )

*    Initialise stack pointer to 0 (flag for pointer not set)
      MODEL.STACKPTR = 0

*    Exit
 99   IF ( STATUS.NE.SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_MODGET', STATUS )
      END IF

      END




*+  FIT_MODGET_TIES - Get constraints from model file
      SUBROUTINE FIT_MODGET_TIES( FLOC, NTIE, TSTART, TGROUP, STATUS )
*
*    Description :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*     19 May 94 : Original (DJA)
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
*    Import :
*
      CHARACTER*(DAT__SZLOC) 	FLOC			! FIT_MODEL object
*
*    Export
*
      INTEGER 			NTIE      		! Number of ties
      INTEGER			TSTART(*)		! First tie parameter
      INTEGER			TGROUP(*)		! Tie group
*
*    Status :
*
      INTEGER 			STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)	TLOC			! TIES object
      CHARACTER*(DAT__SZLOC)	TCLOC			! Particular TIES object

      INTEGER			ITIE			! Loop over ties
      INTEGER			ITPAR			! Loop over tied pars
      INTEGER			NDIM			! Dummy from DAT_SHAPE
      INTEGER			NTIESTR			! Dimensions of TIES
      INTEGER			NTPAR			! No. tied parameters
      INTEGER			TPARS(NPAMAX)		! Tied parameters

      LOGICAL 			THERE                   ! HDS component exists?
*-

*    Status check
      IF ( STATUS.NE.SAI__OK ) RETURN

*    Does a TIEs structure extist
      CALL DAT_THERE( FLOC, 'TIES', THERE, STATUS )
      NTIE = 0
      IF ( THERE ) THEN

*      Initialise
        CALL ARR_INIT1I( 0, NPAMAX, TGROUP, STATUS )

*      Locate structure
        CALL DAT_FIND( FLOC, 'TIES', TLOC, STATUS )

*      Get dimensions
        CALL DAT_SHAPE( TLOC, 1, NTIESTR, NDIM, STATUS )

*      Loop over interesting tie structures
        DO ITIE = 1, NTIESTR

*        Locate the ITIE'th tie
          CALL DAT_CELL( TLOC, 1, ITIE, TCLOC, STATUS )
          CALL DAT_THERE( TCLOC, 'PARS', THERE, STATUS )

*        Does this object contain a valid tie structure
          IF ( THERE ) THEN

*          Get tie data
            CALL CMP_GET1I( TCLOC, 'PARS', NPAMAX, TPARS,
     :                                    NTPAR, STATUS )

*          Increment counter
            NTIE = NTIE + 1
            IF ( NTIE .GT. MAXTIE ) THEN
              STATUS = SAI__ERROR
              CALL ERR_REP( ' ', 'Maximum number of ties exceeded',
     :                      STATUS )
              GOTO 99
            END IF

*          Store base parameter
            TSTART(NTIE) = TPARS(1)
            TGROUP(TPARS(1)) = ITIE

*          Mark other parameters as dependent, check for multiple dependency
            DO ITPAR = 2, NTPAR
              IF ( TGROUP(TPARS(ITPAR)) .NE. 0 ) THEN
                STATUS = SAI__ERROR
                CALL MSG_SETI( 'ONE', TGROUP(TPARS(ITPAR)) )
                CALL MSG_SETI( 'TWO', ITIE )
                CALL MSG_SETI( 'PAR', TPARS(ITPAR) )
                CALL ERR_REP( ' ', 'Model ties ^ONE and ^TWO have a'//
     :                       ' multiple dependency on parameter ^PAR',
     :                       STATUS )
                GOTO 99
              ELSE
                TGROUP(TPARS(ITPAR)) = ITIE
              END IF
            END DO

          END IF

*        Release this tie
          CALL DAT_ANNUL( TCLOC, STATUS )

*      Next tie
        END DO

*      Release TIES
        CALL DAT_ANNUL( TLOC, STATUS )

      END IF

*    Exit
 99   IF ( STATUS.NE.SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_MODGET_TIES', STATUS )
      END IF

      END
