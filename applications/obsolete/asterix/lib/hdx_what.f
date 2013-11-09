*+  HDX_WHAT - Finds details of HDS component.
	SUBROUTINE HDX_WHAT(LOC, NAME, THERE, TYPE, NELS, STATUS)
* Description :
*     <description of what the subroutine does - for user info>
* Method :
* Deficiencies :
*     <description of any deficiencies>
* Bugs :
*     <description of any "bugs" which have not been fixed>
* Authors :
*	Clive Page	1986 Sept 16.
* History :
*       Modified	Dick Willingale 1986-Sep-18	structured names
*       May 10 1988     Asterix88 version   (LTVAD::RDS)
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
* Import :
	CHARACTER*(DAT__SZLOC) LOC	!input: locator to HDS structure.
	CHARACTER*(*) NAME	!input: name of item.
* Export :
	LOGICAL THERE		!output: .true. if it exists.
	CHARACTER*(*) TYPE	!output: HDS data type e.g. '_CHAR*80' or
				!	'does not exist' if not there.
	INTEGER NELS		!output: number of elements if array,
				! 	=1 if scalar, =0 if does not exist.
* Status :
      INTEGER STATUS
* Local variables :
	CHARACTER*(DAT__SZLOC) LOCA
*-
	IF (STATUS .NE. SAI__OK) RETURN
*
	CALL HDX_THERE(LOC, NAME, THERE, status)
*
	IF(STATUS .NE. SAI__OK) THEN
		THERE = .FALSE.
		TYPE  = 'HDS error'
		NELS  = 0
	ELSE IF(THERE) THEN
		CALL HDX_FIND(LOC,NAME,LOCA,status)
		CALL DAT_TYPE(LOCA,TYPE,status)
		CALL DAT_SIZE(LOCA,NELS,status)
		CALL DAT_ANNUL(LOCA,status)
	ELSE
		TYPE = 'does not exist'
		NELS = 0
	END IF
*
        IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP(' ','from HDX_WHAT',STATUS)
        ENDIF
*
	END
