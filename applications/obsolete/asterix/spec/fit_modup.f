*+  FIT_MODUP - Updates parameter values in model_spec object
	SUBROUTINE FIT_MODUP(FID,NCOMP,NPAR,PARAM,LE,UE,CONF,STATUS)
*    Description :
*     Updates values of model parameters in a model_spec object.
*     Parameter errors entered are 1 sigma estimates. If the errors
*     passed in are n-sigma confidence limits (given by CONF) then the
*     values entered in the fit_model are LE/SQRT(CONF) and UE/SQRT(CONF).
*     This corresponds to assuming a parabolic shape to the chi-squared
*     surface near the minimum.
*     If CONF<=0.0 then the errors are not updated.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Martin Watt (BHVAD::MPW)
*     Trevor Ponman (BHVAD::TJP)
*
*    History :
*
*     31 Mar 1987 (TJP):
*        Original - adapted from UPDATE
*     29 Jun 1987 (TJP):
*        Change to fit_model object structure
*     17 Jul 1987 (TJP):
*        Parameter error estimates included
*      6 Aug 1987 (TJP):
*        HISTORY entry (& VERSION argument) dispensed with
*     12 May 1994 (DJA):
*        HDS_FREE to force update of model spec file on UNIX
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
      INTEGER			FID			! ADI identifier
      INTEGER NCOMP			! No of model components
      INTEGER NPAR			! Total no of model parameters
      REAL PARAM(NPAR)			! All the component model parameters
      REAL LE(NPAR)			! Parameter lower errors
      REAL UE(NPAR)			! Parameter upper errors
      REAL CONF				! Confidence limit (in sigma) for errors
*    Status :
      INTEGER STATUS
*    Local variables :
      CHARACTER*(DAT__SZLOC) FLOC	! Locator to fit_model object
      CHARACTER*(DAT__SZLOC) PMLOC      ! locator to pmodels
      CHARACTER*(DAT__SZLOC) MILOC      ! locator to pmodel I
      CHARACTER*(DAT__SZLOC) MIPLOC     ! locator to parameters of pmodel I
      CHARACTER*(DAT__SZLOC) MIPJLOC    ! locator to param J of pmod I
      LOGICAL THERE			! component exists?
      INTEGER NUMPAR			! parameter number
      INTEGER JPAR			! no of parameters in each pmodel
      INTEGER I				! loop counter
      INTEGER J				! loop counter
*-

* Status check
	IF(STATUS.NE.SAI__OK) RETURN

* Extract locator
        CALL ADI1_GETLOC( FID, FLOC, STATUS )

* Get locator to pmodels
	CALL DAT_FIND(FLOC,'PMODEL',PMLOC,STATUS)
	IF(STATUS.NE.SAI__OK) GO TO 9000

* Loop through model components
	NUMPAR=0
	DO I=1,NCOMP
	  CALL DAT_CELL(PMLOC,1,I,MILOC,STATUS)
	  CALL CMP_GET0I(MILOC,'NPAR',JPAR,STATUS)
	  CALL DAT_FIND(MILOC,'PAR',MIPLOC,STATUS)
          IF(STATUS.NE.SAI__OK) GO TO 9000

*    Loop through parameters in each component, overwriting them
	  DO J=1,JPAR
	    NUMPAR=NUMPAR+1
	    CALL DAT_CELL(MIPLOC,1,J,MIPJLOC,STATUS)
	    CALL CMP_PUT0R(MIPJLOC,'VAL',PARAM(NUMPAR),STATUS)
*        Update errors
	    IF(CONF.GT.0.0)THEN
	      CALL DAT_THERE(MIPJLOC,'LERROR',THERE,STATUS)
	      IF(.NOT.THERE)THEN
	        CALL DAT_NEW0R(MIPJLOC,'LERROR',STATUS)
	      ENDIF
	      CALL CMP_PUT0R(MIPJLOC,'LERROR',LE(NUMPAR)/SQRT(CONF),
     :        STATUS)
	      CALL DAT_THERE(MIPJLOC,'UERROR',THERE,STATUS)
	      IF(.NOT.THERE)THEN
	        CALL DAT_NEW0R(MIPJLOC,'UERROR',STATUS)
	      ENDIF
	      CALL CMP_PUT0R(MIPJLOC,'UERROR',UE(NUMPAR)/SQRT(CONF),
     :        STATUS)
	    ENDIF
	    CALL DAT_ANNUL(MIPJLOC,STATUS)
	    IF(STATUS.NE.SAI__OK) GO TO 9000
	  END DO

* Tidy up
	  CALL DAT_ANNUL(MIPLOC,STATUS)
	  CALL DAT_ANNUL(MILOC,STATUS)
	  IF(STATUS.NE.SAI__OK)CALL ERR_FLUSH(STATUS)
	END DO
	CALL DAT_ANNUL(PMLOC,STATUS)

*  Force updates to disk
      CALL HDS_FREE( FLOC, STATUS )

*  Exit
 9000 IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_MODUP', STATUS )
      END IF

      END
