*+  SBG - Writes reference to background dataset into source dataset
      SUBROUTINE SBG(STATUS)
*
*    Description :
*
*     HDS reference to background dataset is written into .MORE.ASTERIX.BGREF
*     component of dataset.
*     If null (!) background is specified then any existing reference is
*     deleted.
*
*    Environment parameters :
*     INP=UNIV(U)
*            source dataset
*     BGFILE=UNIV(R)
*            background dataset
*    Method :
*     Uses the Starlink REFERENCE system.
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Trevor Ponman (BHVAD::TJP)
*
*    History :
*
*      1 Jul 92 : V1.6-1 Original (BHVAD::TJP)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*
*    Type definitions :
*
	IMPLICIT NONE
*
*    Global constants :
*
	INCLUDE 'SAE_PAR'
	INCLUDE 'DAT_PAR'
	INCLUDE 'PAR_ERR'
	INCLUDE 'FIT_PAR'
*    Global variables :
*    Structure definitions :
*    Status :
	INTEGER STATUS
*    Local constants :
*    Local variables :
	CHARACTER*(DAT__SZLOC) ILOC	! Locator to source dataset
	CHARACTER*(DAT__SZLOC) ALOC	! Locator to ASTERIX box in source data
	CHARACTER*(DAT__SZLOC) BLOC	! Locator to b/g dataset
	LOGICAL PRIM			! Input data primitive?
	LOGICAL THERE			! Component present?
	LOGICAL NULL			! No background
*    Local data :
*    Version :
	CHARACTER*30 VERSION
	PARAMETER		(VERSION='SBG Version 1.8-0')
*-

* Version
	CALL MSG_PRNT(VERSION)

* Initialise BDA_*
	CALL AST_INIT

* Get names of source and background datasets
	CALL USI_ASSOCI('INP','UPDATE',ILOC,PRIM,STATUS)
	IF(STATUS.NE.SAI__OK) GO TO 9000
	CALL USI_DASSOC('BGFILE','READ',BLOC,STATUS)
	IF(STATUS.EQ.PAR__NULL)THEN
	  CALL ERR_ANNUL(STATUS)
	  NULL=.TRUE.
	ELSE
	  NULL=.FALSE.
	ENDIF

* Find ASTERIX box
	CALL BDA_LOCAST(ILOC,ALOC,STATUS)
	IF(STATUS.NE.SAI__OK) GO TO 9000

* If BGREF already exists then delete it
	CALL DAT_THERE(ALOC,'BGREF',THERE,STATUS)
	IF(THERE)THEN
	  IF(NULL)THEN
	    CALL MSG_PRNT('Deleting existing background reference')
	  ELSE
	    CALL MSG_PRNT('Replacing existing background reference')
	  ENDIF
	  CALL DAT_ERASE(ALOC,'BGREF',STATUS)
	ENDIF

* Write new reference if necessary
	IF(.NOT.NULL)THEN
	  CALL REF_CRPUT(ALOC,'BGREF',BLOC,.FALSE.,STATUS)
	ENDIF

* Exit
 9000  	CALL AST_CLOSE()
        CALL AST_ERR( STATUS )

	END
