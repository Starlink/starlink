*+  HDX_CCOPY - Component copy from one dataset to another
      SUBROUTINE HDX_CCOPY(ILOC,OLOC,NAME,STATUS)
*    Description :
*     Copies the component NAME from the structure located by ILOC to
*     the structure located by OLOC.
*    Invocation :
*     CALL HDX_CCOPY(ILOC,OLOC,NAME,STATUS)
*    Parameters :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Jim Peden (BHVAD::JCMP)
*    History :
*     28 Sep 84: Original (UTIL_CCOPY) (BHVAD::JCMP)
*      3 Jul 87: Drastically simplified (BHVAD::TJP)
*      3 Oct 88: Renamed to HDX_CCOPY (TJP)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
	CHARACTER*(DAT__SZLOC) ILOC	! input structure locator
	CHARACTER*(DAT__SZLOC) OLOC	! output structure locator
	CHARACTER*(*) NAME		! component name
*    Import-Export :
*    Export :
*    Status :
	INTEGER STATUS
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
	CHARACTER*(DAT__SZLOC) CLOC	! component locator
	LOGICAL THERE			! component exists?
*    Internal References :
*    Local data :
*-

* Status check
	IF(STATUS.NE.SAI__OK) RETURN

* If component already exists in output structure then delete it
	CALL DAT_THERE(OLOC,NAME,THERE,STATUS)
	IF(THERE)THEN
	  CALL DAT_ERASE(OLOC,NAME,STATUS)
	ENDIF

* Copy
	CALL DAT_FIND(ILOC,NAME,CLOC,STATUS)
	CALL DAT_COPY(CLOC,OLOC,NAME,STATUS)
	CALL DAT_ANNUL(CLOC,STATUS)

* Exit
	IF(STATUS.NE.SAI__OK) CALL ERR_REP('EXERR','from HDX_CCOPY',
     :  STATUS)
	END
