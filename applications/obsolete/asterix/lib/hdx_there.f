*+  HDX_THERE - See if structured name exists
	SUBROUTINE HDX_THERE(LOCIN,NAME,THERE,STATUS)
* Description :
*     <description of what the subroutine does - for user info>
* Environment parameters :
* Method :
* Deficiencies :
* Bugs :
* Authors :
*     Dick Willingale 1986-Sep-18
* History :
*     MAY 10 1988     Asterix88 version  (LTVAD::RDS)
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
* Import :
	CHARACTER*(DAT__SZLOC) LOCIN     !start locator
	CHARACTER*(*) NAME               !structured name
* Import-Export :
* Export :
	LOGICAL THERE                    ! .TRUE. if exists, .FALSE. if not
* Status :
      INTEGER STATUS
* Function declarations :
*     <declarations for function references>
* Local constants :
        INTEGER MAXL
	PARAMETER (MAXL=10)
* Local variables :
	CHARACTER OBJECT(MAXL)*(DAT__SZLOC),LOC*(DAT__SZLOC),
     :        LOCA*(DAT__SZLOC),LOCOUT*(DAT__SZLOC)
	INTEGER NELS(DAT__MXDIM),KDIM(MAXL),
     :                KELS(DAT__MXDIM,MAXL),KUPS(DAT__MXDIM,MAXL)
        INTEGER NOB,K,J
        INTEGER NDIM
* Global variables :
*     <global variables held in named COMMON>
* Local data :
*     <any DATA initialisations for local variables>
*-
	IF(STATUS.NE.SAI__OK) RETURN
*
C Parse name into objects and dimensions
	CALL STR_PARSE(NAME,MAXL,DAT__MXDIM,OBJECT,KDIM,KELS,KUPS,NOB)
*
	IF(NOB.EQ.0) THEN
		STATUS=SAI__ERROR
		GOTO 999
	ENDIF
C Now find if there
	CALL DAT_CLONE(LOCIN,LOCOUT,STATUS)
*
	DO K=1,NOB
	   CALL DAT_THERE(LOCOUT,OBJECT(K),THERE,STATUS)
*
	   IF(THERE) THEN
		CALL DAT_FIND(LOCOUT,OBJECT(K),LOC,STATUS)
	        IF(KDIM(K).NE.0) THEN
		    CALL DAT_SHAPE(LOC,DAT__MXDIM,NELS,NDIM,STATUS)
		    IF(NDIM.NE.KDIM(K)) THEN
		         THERE=.FALSE.
		    ELSE
			 DO J=1,NDIM
			     IF(KELS(J,K).GT.NELS(J)) THEN
                                   THERE=.FALSE.
			     ENDIF
			 ENDDO
*
                         DO J=1,NDIM
			    IF(KUPS(J,K).GT.NELS(J)) THEN
			        THERE=.FALSE.
		            ENDIF
			 ENDDO
                    ENDIF
*
		    IF(.NOT.THERE) THEN
		        CALL DAT_ANNUL(LOC,STATUS)
			CALL DAT_ANNUL(LOCOUT,STATUS)
			GOTO 999
		    ENDIF
*
                    CALL DAT_SLICE(LOC,KDIM(K),
     +		           KELS(1,K),KUPS(1,K),LOCA,STATUS)
	            CALL DAT_ANNUL(LOC,STATUS)
		    LOC=LOCA
*
		ENDIF
           ELSE
	        CALL DAT_ANNUL(LOCOUT,STATUS)
	        GOTO 999
	   ENDIF
	   CALL DAT_ANNUL(LOCOUT,STATUS)
	   LOCOUT=LOC
*
	ENDDO
	CALL DAT_ANNUL(LOCOUT,STATUS)
*
999     CONTINUE
	IF(STATUS.NE.SAI__OK) THEN
		CALL ERR_REP(' ',' from HDX_THERE',STATUS)
	ENDIF
*
	END
