*+  STR_CTODIM - Decode dimensions from a character string
      SUBROUTINE STR_CTODIM(STRING,MAXD,NELS,NUP,NDIM)
* Description :
*	Decode dimensions from a character string
* Method :
*	If no upper bounds then NUP(k)=NELS(K)
*	If NUP(k)=0 then assume upper limit
* History :
*     Author Dick Willingale 1986-Sep-18
*     MAY 10 ASTERIX88 version         (LTVAD::RDS)
*     10-JAN-2002 (AJC):
*        Use a variable format string - not I<NN> form for reading
*        as didn't work on Linux.
* Type Definitions :
      IMPLICIT NONE
* Import :
      CHARACTER STRING*(*)     !String: encoded form '(10,3,5)'
      INTEGER MAXD	       !Maximum number of dimensions allowed

* Export :
      INTEGER NELS(MAXD)       !Dimensions decoded
      INTEGER NUP(MAXD)        !Upper bound decoded if set e.g. (10:14,2,3:3)
      INTEGER NDIM             !Number of dimensions decoded (0 if failed)
* Functions :
        INTEGER CHR_LEN
* Local variables :
	CHARACTER*80 ITEM
        CHARACTER*20 FORM
        INTEGER II,IL,NC
        INTEGER NN
        INTEGER NCOL
        INTEGER N1,N2
        INTEGER IERR          !Error variable
*-
*
	NDIM=0

	II=INDEX(STRING,'(')
	IL=INDEX(STRING,')')
	IF(II.EQ.0.OR.IL.EQ.0) RETURN
	NC=II+1
*
100     CONTINUE
	CALL STR_ITEM(STRING,',',NC,ITEM)
	NN = CHR_LEN(ITEM)
	IF(NC.GT.IL) NN=NN-1
	NDIM=NDIM+1
*
	IF(NDIM.GT.MAXD) THEN
		NDIM=0
		GOTO 999
	ENDIF
*
	NCOL=INDEX(ITEM,':')
	IF(NCOL.EQ.0) THEN
                WRITE( FORM, '(''(I'',I1,'')'')'),NN
		READ(ITEM,FORM,IOSTAT=IERR) NELS(NDIM)
		NUP(NDIM)=NELS(NDIM)
	ELSE
		N1=NCOL-1
		IF(N1.GT.0) THEN
                        WRITE( FORM, '(''(I'',I1,'')'')'),N1
			READ(ITEM(:NCOL-1),FORM,IOSTAT=IERR) NELS(NDIM)
		ELSE
			NELS(NDIM)=1
		ENDIF
		N2=NN-NCOL
		IF(N2.GT.0) THEN
                        WRITE( FORM, '(''(I'',I1,'')'')'),N2
			READ(ITEM(NCOL+1:),FORM,IOSTAT=IERR) NUP(NDIM)
		ELSE
			NUP(NDIM)=0
		ENDIF
	ENDIF
	IF(IERR.NE.0) THEN
		NDIM=0
		GOTO 999
	ENDIF
*
	IF(NC.LT.IL) GOTO 100
*
999     CONTINUE
*
	END
