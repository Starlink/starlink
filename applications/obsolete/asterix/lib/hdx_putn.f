*+  HDX_PUTN - Puts non-character variable/array on HDS file.
      SUBROUTINE HDX_PUTN(LOC, NAME, ITYPE, NELS, ARRAY, STATUS)
* Description :
*    This routine puts a non-character array onto an HDS object.
*   It is the generic routine and is called by HDX_PUTD,HDX_PUTI etc..
* Environment parameters :
* Method :
* Deficiencies :
*     <description of any deficiencies>
* Bugs :
*     <description of any "bugs" which have not been fixed>
* Authors :
*	Clive Page	1986 July 9
* History :
*       Modified	Dick Willingale 1986-Sep-18	structured names
*       Asterix88 version   May 10 1988  (LTVAD::RDS)
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
* Structure definitions :
*     <specification of FORTRAN structures>
* Import :
	CHARACTER*(DAT__SZLOC) LOC	!input: locator to structure.
	CHARACTER*(*) NAME	!input: name of variable or array.
	CHARACTER*(*) ITYPE	!input: HDS data-type, e.g. '_DOUBLE'.
	INTEGER NELS		!input: no of elements, =1 for scalar.
	REAL ARRAY(NELS)	!input: values to be written to HDS file.
* Import-Export :
*     <declarations and descriptions for imported/exported arguments>
* Export :
*     <declarations and descriptions for exported arguments>
* Status :
      INTEGER STATUS
* Function declarations :
*     <declarations for function references>
* Local constants :
*     <local constants defined by PARAMETER>
* Local variables :
	LOGICAL THERE
	CHARACTER*(DAT__SZLOC) LOCNAM,LOCV
        CHARACTER*(DAT__SZTYP) TYPE
        INTEGER NDIMS,NTOT
* Global variables :
*     <global variables held in named COMMON>
* Local data :
*     <any DATA initialisations for local variables>
*-
        IF (STATUS .NE. SAI__OK) RETURN
*
	NDIMS = MIN(1, NELS-1)
	CALL HDX_WHAT(LOC,NAME,THERE,TYPE,NTOT,STATUS)
*
	IF(.NOT.THERE.OR.NTOT.NE.NELS) THEN
	   CALL HDX_CREATE(LOC,NAME,NDIMS,NELS,ITYPE,LOCNAM,STATUS)
	   IF(STATUS.NE.SAI__OK) GOTO 999
	ELSE
	   CALL HDX_FIND(LOC,NAME,LOCNAM,STATUS)
	ENDIF
C Vectorise
	IF(NDIMS .EQ. 0) THEN
		CALL DAT_PUT(LOCNAM, ITYPE, 0, 0, ARRAY, STATUS)
	ELSE
		CALL DAT_VEC(LOCNAM,LOCV,STATUS)
		CALL DAT_PUT(LOCV, ITYPE, NDIMS, NELS, ARRAY, STATUS)
		CALL DAT_ANNUL(LOCV,STATUS)
	END IF
*
999     CONTINUE
	CALL DAT_ANNUL(LOCNAM, STATUS)
*
	IF(STATUS .NE. SAI__OK) THEN
           CALL ERR_REP(' ',' from HDX_PUTN',STATUS)
        ENDIF
*
	END
