*+  ADI1_FIND - Find locator for structured name
      SUBROUTINE ADI1_FIND(LOCIN,NAME,LOCOUT,STATUS)
* Description :
* Deficiencies :
* Bugs :
* Authors :
*     Dick Willingale 1986-Sep-18
* History :
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
* Import :
      CHARACTER*(DAT__SZLOC) LOCIN	  !start locator
      CHARACTER*(*) NAME	          !structured name
* Import-Export :
* Export :
      CHARACTER*(DAT__SZLOC) LOCOUT	  !locator found
* Status :
      INTEGER STATUS
* Function declarations :
* Local constants :
      INTEGER MAXL
      PARAMETER (MAXL=10)
* Local variables :
      CHARACTER*(DAT__SZLOC) OBJECT(MAXL),LOC,LOCA
      INTEGER KDIM(MAXL),KELS(DAT__MXDIM,MAXL),KUPS(DAT__MXDIM,MAXL)
      INTEGER K,NOB
      LOGICAL STRUC
*-
	IF(STATUS .NE. SAI__OK) RETURN
*
* Parse name into objects and dimensions
	CALL STR_PARSE(NAME,MAXL,DAT__MXDIM,OBJECT,KDIM,KELS,KUPS,NOB)
*
	IF(NOB.EQ.0) THEN
          STATUS=SAI__ERROR
	ENDIF
C Now find locator
	CALL DAT_CLONE(LOCIN,LOCOUT,STATUS)
*
        K=1
	DO WHILE (K.LE.NOB.AND.STATUS.EQ.SAI__OK)
	  CALL DAT_FIND(LOCOUT,OBJECT(K),LOC,STATUS)

*  locate slice or index
	  IF(KDIM(K).NE.0.AND.STATUS.EQ.SAI__OK) THEN
            CALL DAT_STRUC(LOC,STRUC,STATUS)
            IF (STRUC) THEN
              CALL DAT_CELL(LOC,KDIM(K),KELS(1,K),LOCA,STATUS)
            ELSE
              CALL DAT_SLICE(LOC,KDIM(K),KELS(1,K),KUPS(1,K),LOCA,
     :                                                      STATUS)
            ENDIF
	    CALL DAT_ANNUL(LOC,STATUS)
	    LOC=LOCA
	  ENDIF

	  CALL DAT_ANNUL(LOCOUT,STATUS)
	  LOCOUT=LOC
          K=K+1
	ENDDO
*
	IF(STATUS.NE.SAI__OK) THEN
           CALL ERR_REP(' ','from HDX_FIND',STATUS)
        ENDIF
*
	END
