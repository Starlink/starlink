*+ FIT_USER3 - Dummy user model for fitting software
      SUBROUTINE FIT_USER3(NMDAT,LBOUND,UBOUND,PARAM,PRED,STATUS)
*    Description :
*     Dummy user model - to be replaced with user defined module.
*    Method :
*    Deficiencies :
*     The arguments of this dummy are correct only for 1D models.
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*     3 May 88:  Original
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
	INTEGER NMDAT			! No of model bins
	REAL LBOUND(NMDAT)		! Lower bin bounds
	REAL UBOUND(NMDAT)		! Upper bin bounds
	REAL PARAM(*)			! Model parameters
*    Import-Export :
*    Export :
	REAL PRED(NMDAT)		! Predicted data
*    Status :
	INTEGER STATUS
*    Local variables :

*-----------------------------------------------------------------------------

* Status check
	IF(STATUS.NE.SAI__OK) RETURN

* Output message and exit with bad status
	CALL ERR_REP('NO_MOD','User model 3 not linked',STATUS)
	STATUS=SAI__ERROR

* Exit
 9000	IF(STATUS.NE.SAI__OK) CALL ERR_REP('EXERR','from FIT_USER3',
     :  STATUS)
	END
