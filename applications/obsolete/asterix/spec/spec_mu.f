*+ SPEC_MU - Multiplicative constant
      SUBROUTINE SPEC_MU(NEN,ELBOUND,EUBOUND,PARAM,FLUX,STATUS)
*    Description :
*     Multiplicative constant
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*      8 Jan 1996 (DJA):
*        Original version.
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
	INTEGER NEN			! No of energy channels
	REAL ELBOUND(NEN)		! Lower bin bounds (keV)
	REAL EUBOUND(NEN)		! Upper bin bounds (keV)
	REAL PARAM(1)			! Model parameters
*    Import-Export :
*    Export :
	REAL FLUX(NEN)			! Flux multiplication factor
*    Status :
	INTEGER STATUS
*    Local variables :
	INTEGER I
*-

* Status check
	IF(STATUS.NE.SAI__OK) RETURN

* Check for spot value
	IF(ELBOUND(1).EQ.EUBOUND(1))THEN
	  STATUS=SAI__ERROR
	  CALL ERR_REP('SPOT','Spot values not supported by SPEC_ routines',
     :    STATUS)
	  GO TO 9000
	ENDIF

* A very simple model!
        DO I=1,NEN
	  FLUX(I)=PARAM(1)
        END DO

*    Exit
 9000 IF ( STATUS .NE. SAI__OK) THEN
        CALL AST_REXIT( 'SPEC_MU', STATUS )
      END IF

      END
