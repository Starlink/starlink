*+ THAW - Unfreezes fit_model parameters
      SUBROUTINE THAW(STATUS)
*    Description :
*     Resets FROZEN flag (and optionally values) for parameters in a fit_model
*     data object.
*    Environment parameters :
*     FIT_MOD=UNIV(U)
*		Object containing fit model
*     PARAMS=INTEGER()(R)
*		Numbers of parameters to be thawed
*     VALS=REAL()(R)
*               Values for thawed parameters
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*      2 Jul 87 : V0.6-1 Original (TJP)
*     14 Dec 88 : V0.6-2 Thaw multiple parameters, allow param resetting (TJP)
*     20 Jun 89 : V1.0-1 ASTERIX88 release (TJP)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'FIT_PAR'
*    Status :
      INTEGER STATUS
*    Function declarations :
	INTEGER CHR_LEN
*    Local constants :
	INTEGER MAXTHAW			! Max no of params which can
	PARAMETER (MAXTHAW=20)		! be thawed in one pass
*    Local variables :
	CHARACTER*(DAT__SZLOC) FLOC		! Locator to fit_model object
	CHARACTER*(DAT__SZLOC) MLOC		! Locator to pmodels
	CHARACTER*(DAT__SZLOC) MILOC		! Locator to pmodel I
	CHARACTER*(DAT__SZLOC) MIPLOC		! Locator to parameters of
						! pmodel I
	CHARACTER*(DAT__SZLOC) MIPJLOC		! Locator to one param of pmod I
	CHARACTER*(DAT__SZTYP) TYP		! Object type
	CHARACTER*25 PARNAME			! Parameter name
	LOGICAL INPRIM				! Input data primitive?
	LOGICAL FROZEN				! Parameter frozen?
	LOGICAL PVALS				! Param values entered?
	LOGICAL GETPAR				! Parameter component accessed?
	LOGICAL VALID				! Locator valid?
	INTEGER NCOMP				! No of pmodels in cmodel
	INTEGER NPAR				! No of pmodel parameters
	INTEGER NTHAW				! No of parameters to be thawed
	INTEGER NVAL				! No of param values entered
	INTEGER NPARSUM				! Running sum of param number
	INTEGER NPMIN,NPMAX			! Min/max param nos for pmodel
	INTEGER PARNO(MAXTHAW)			! Nos of parameters to be thawed
	INTEGER I,J				! Loop indices
	REAL VAL(MAXTHAW)			! Values for thawed params
*    Local data :
*    Version :
	CHARACTER*30 VERSION
	PARAMETER		(VERSION = 'THAW Version 1.0-1')

*-

* Version not announced (neater)

        CALL AST_INIT()

* Access and check fit_model object
	CALL USI_ASSOCI('FIT_MOD','UPDATE',FLOC,INPRIM,STATUS)
	IF(STATUS.NE.SAI__OK) GO TO 9000
	CALL DAT_TYPE(FLOC,TYP,STATUS)
	IF(TYP.NE.'FIT_MODEL')THEN
	  CALL ERR_REP('WRONG_OBJ','Not a fit_model data object',STATUS)
	  STATUS=SAI__ERROR
	  GO TO 9000
	ENDIF
	IF(STATUS.NE.SAI__OK) GO TO 9000

* Get parameter numbers
	CALL USI_GET1I('PARAMS',MAXTHAW,PARNO,NTHAW,STATUS)
	IF(STATUS.NE.SAI__OK) GO TO 9000

* Check for invalid parameter numbers
	DO J=1,NTHAW
	  IF(PARNO(J).LT.1.OR.PARNO(J).GT.NPAMAX)THEN
	    CALL ERR_REP('BAD_NUM','Bad parameter number',STATUS)
	    STATUS=SAI__ERROR
	    GO TO 9000
	  ENDIF
	ENDDO

* Get values at which thawed parameters are to be set
	CALL USI_GET1R('VALS',MAXTHAW,VAL,NVAL,STATUS)
	IF(STATUS.EQ.PAR__NULL)THEN
	  CALL ERR_ANNUL(STATUS)
	  PVALS=.FALSE.			! Null indicates no resetting of params
	ELSE IF(STATUS.EQ.SAI__OK)THEN
	  PVALS=.TRUE.
	ELSE
	  CALL ERR_FLUSH(STATUS)
	  PVALS=.FALSE.
	ENDIF

* Access primitive models
	CALL CMP_GET0I(FLOC,'NCOMP',NCOMP,STATUS)
	CALL DAT_FIND(FLOC,'PMODEL',MLOC,STATUS)
	IF(STATUS.NE.SAI__OK)THEN
	  CALL ERR_REP('NOPRIMS','Primitive models not found in fit_model '
     :    //'object',STATUS)
	  GO TO 9000
	ENDIF

* Step through model components
	NPARSUM=0
	DO I=1,NCOMP
	  CALL DAT_CELL(MLOC,1,I,MILOC,STATUS)
	  CALL CMP_GET0I(MILOC,'NPAR',NPAR,STATUS)
	  IF(STATUS.NE.SAI__OK) GO TO 9000
	  NPMIN=NPARSUM+1
	  NPMAX=NPARSUM+NPAR
D	  print *,'comp,npmin,npmax,nthaw: ',i,npmin,npmax,nthaw

* Find required parameter(s)
	  GETPAR=.TRUE.
	  DO J=1,NTHAW
D	    print *,'j: ',j
	    IF(PARNO(J).GE.NPMIN.AND.PARNO(J).LE.NPMAX)THEN
	      IF(GETPAR)THEN
	        CALL DAT_FIND(MILOC,'PAR',MIPLOC,STATUS)
	        GETPAR=.FALSE.
	      ENDIF
	      CALL DAT_CELL(MIPLOC,1,PARNO(J)-NPARSUM,MIPJLOC,STATUS)
	      CALL CMP_GET0C(MIPJLOC,'NAME',PARNAME,STATUS)
	      IF(STATUS.NE.SAI__OK) CALL ERR_FLUSH(STATUS)
	      CALL CMP_GET0L(MIPJLOC,'FROZEN',FROZEN,STATUS)
	      IF(STATUS.NE.SAI__OK) GO TO 9000

*    Set parameter value if required
	      IF(PVALS)THEN
	        CALL CMP_PUT0R(MIPJLOC,'VAL',VAL(J),STATUS)
	      ENDIF
	      IF(STATUS.NE.SAI__OK) CALL ERR_FLUSH(STATUS)

*    Thaw parameter
	      IF(.NOT.FROZEN)THEN
	        CALL MSG_SETC('PAR',PARNAME)
	        CALL MSG_OUT('PAR_FROZ','^PAR not frozen',STATUS)
	        IF(PVALS)THEN
	          CALL MSG_OUT('RESET','parameter value amended',STATUS)
	        ENDIF
	      ELSE
	        CALL CMP_PUT0L(MIPJLOC,'FROZEN',.FALSE.,STATUS)
	        IF(STATUS.NE.SAI__OK) GO TO 9000
	        WRITE(6,500)PARNAME
 500	        FORMAT(16X,'* ',A<CHR_LEN(PARNAME)>,1X,'thawed')
	      ENDIF
	      CALL DAT_ANNUL(MIPJLOC,STATUS)
	    ENDIF
	  ENDDO
	  CALL DAT_VALID(MIPLOC,VALID,STATUS)
	  IF(VALID)THEN
	    CALL DAT_ANNUL(MIPLOC,STATUS)
	  ENDIF
	  CALL DAT_ANNUL(MILOC,STATUS)
	  NPARSUM=NPARSUM+NPAR
	ENDDO

* Exit
 9000   CALL AST_INIT()
        CALL AST_ERR( STATUS )

	END
