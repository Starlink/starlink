*+  UTIL_TYPLEN - Gets the length in bytes of a primitive type
      SUBROUTINE UTIL_TYPLEN(TYPE,LENGTH,STATUS)
*    Description :
*     The length in bytes of the numeric type in TYPE is returned in LENGTH.
*    Invocation :
*     CALL UTIL_TYPLEN(TYPE,LENGTH,STATUS)
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Jim Peden (BHVAD::JCMP)
*    History :
*     8 June 83: original (BHVAD::JCMP)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Status :
	INTEGER STATUS
*    External references :
*    Global variables :
*    Local Constants :
*    Import :
	CHARACTER*(*) TYPE			! primitive type
*    Export :
	INTEGER LENGTH				! its length in bytes
*    Local variables :
*    Internal References :
*    Local data :
*-

* Exit if STATUS is bad
	IF(STATUS.NE.SAI__OK) RETURN

* Look through primitive types for a match
	IF(TYPE.EQ.'_INTEGER') THEN
	   LENGTH=4
	ELSEIF(TYPE.EQ.'_REAL') THEN
	   LENGTH=4
	ELSEIF(TYPE.EQ.'_DOUBLE') THEN
	   LENGTH=8
	ELSEIF(TYPE.EQ.'_LOGICAL') THEN
	   LENGTH=4
	ELSEIF(TYPE.EQ.'_BYTE') THEN
	   LENGTH=1
	ELSEIF(TYPE.EQ.'_UBYTE') THEN
	   LENGTH=1
	ELSEIF(TYPE.EQ.'_WORD') THEN
	   LENGTH=2
	ELSEIF(TYPE.EQ.'_UWORD') THEN
	   LENGTH=2
	ELSE
	   STATUS=SAI__ERROR
	   CALL ERR_REP('NON_NUM','Non-numeric type',STATUS)
	ENDIF

	END
