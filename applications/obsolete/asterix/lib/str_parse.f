*+  STR_PARSE - Split structured name into object names and dimensions
      SUBROUTINE STR_PARSE(NAME,MAXL,MAXD,OBJECT,NDIM,NELS,NUP,NOB)
*
* Description :
*	Split structured name into object names and dimensions
*             If just single cell then NPU(K,J)=NELS(K,J)
* History :
*     Author Dick Willingale 1986-Sep-18
*     May 11 1988   Asterix88 version    (LTVAD::RDS)
* Type Definitions :
      IMPLICIT NONE
* Import :
	CHARACTER NAME*(*)          !structured name of form
*                                   !     'THIS.IS.A(5,6).STRUCTURE'
        INTEGER MAXL	            !maximum number of objects
        INTEGER MAXD	            !maximum number of dimensions
*     <declarations and descriptions for imported arguments>
* Import-Export :
*     <declarations and descriptions for imported/exported arguments>
* Export :
        CHARACTER*(*) OBJECT(MAXL)  !array of parsed object names
        INTEGER NDIM(MAXL)	    !number of dimensions
        INTEGER NELS(MAXD,MAXL)	    !size of dimensions
        INTEGER NUP(MAXD,MAXL)	    !upper limits if slice specified
*                                   !     e.g. (1:3,4)
        INTEGER NOB	            !number of parsed object names (0 if failed)

*     <declarations and descriptions for exported arguments>
* Local constants :
*     <local constants defined by PARAMETER>
* Local variables :
	CHARACTER ITEM*80
        INTEGER NDOT,IB,IN
*-
*
	NOB=1
	NDOT=1
	IN=LEN(NAME)
100	CALL STR_ITEM(NAME,'.',NDOT,ITEM)
	IB=INDEX(ITEM,'(')
	IF(IB.GT.0) THEN
		CALL STR_CTODIM(ITEM(IB:),MAXD,NELS(1,NOB),NUP(1,NOB),
     +		NDIM(NOB))
		IF(NDIM(NOB).EQ.0) THEN
			NOB=0
			GOTO 999
		ENDIF
		OBJECT(NOB)=ITEM(1:IB-1)
	ELSE
		OBJECT(NOB)=ITEM
		NDIM(NOB)=0
	ENDIF
	IF(NDOT.LT.IN) THEN
		NOB=NOB+1
		GOTO 100
	ENDIF
*
999     CONTINUE
	END
