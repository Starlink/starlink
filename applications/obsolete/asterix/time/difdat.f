*+   DIFDAT - Difference data set
      SUBROUTINE DIFDAT( STATUS )
*
*    Description :
*
*     DIFDAT differences a time series dataset of intensity values,differen-
*     cing successive values in order to remove any low frequency variation.
*     It can deal with primitive datasets,assuming a constant timescale,as
*     well as structures containing data values at irregular intervals and
*     quality information.
*
*    Environment parameters :
*
*
*
*
*
*    Method :
*     The program outputs a difference dataset and a corresponding timescale
*     the time for each point being the average of the twopoints that were
*     differenced to get the difference value.
*
*    Deficiences :
*
*    Authors :
*
*     S.R.Duck   (BHVAD::SRD)
*     G.R.Mellor (LTVAD::GRM)
*
*    History :
*
*     09 Mar 90 : V1.2-0  Original (BHVAD::SRD)
*     28 Jun 90 : V1.2-1  Corrected (BHVAD::SRD)
*      8 Nov 91 : V1.2-2  Corrected (LTVAD::GRM)
*     20 Apr 95 : V1.8-0  Updated data interfaces (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*
*    Status :
*
      INTEGER STATUS ! status return variable.
*
*    Local variables :
*
	INTEGER DIMS(ADI__MXDIM)
	INTEGER NDIM
	INTEGER DPTR
	INTEGER ODPTR
	INTEGER NVAL
	INTEGER QNDIM
	INTEGER QDIMS(ADI__MXDIM)
	INTEGER NOP
	INTEGER AXPTR
	INTEGER QPTR
	INTEGER NBAD
	INTEGER OAXPTR
	INTEGER VPTR
	INTEGER OVPTR
	INTEGER VNDIM
	INTEGER VDIMS(ADI__MXDIM)
	INTEGER ONDAT
	INTEGER NDAT
	INTEGER ONDIM
	INTEGER ODIMS(ADI__MXDIM)
	INTEGER ACTVAL

	LOGICAL INPRIM
	LOGICAL REG
	LOGICAL VFLAG
	LOGICAL QFLAG
	LOGICAL VOK
	LOGICAL OK
	LOGICAL QUALITY
	LOGICAL BAD

        INTEGER			IFID,OFID

*    Version :
	CHARACTER*30 VERSION
	PARAMETER  (VERSION='DIFDAT Version 1.8-0')
*-

*  Version
      CALL MSG_PRNT( VERSION )

*  Initialise
      CALL AST_INIT()

*  Dataset access
      CALL USI_TASSOC2('INP','OUT','READ',IFID,OFID,STATUS)
      CALL BDI_PRIM( IFID, INPRIM, STATUS )

*  Check 1-D
      CALL BDI_CHKDATA(IFID,OK,NDIM,DIMS,STATUS)
      IF ( NDIM .NE. 1 ) THEN
	STATUS=SAI__ERROR
	CALL ERR_REP( ' ', 'Dataset is not 1-D', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Input and output dimensions
      NDAT=DIMS(1)
      ONDAT=NDAT-1

*  Map data
      CALL BDI_MAPDATA (IFID,'READ',DPTR,STATUS)


      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Primitive dataset
	IF(INPRIM)THEN

*    Set flags
	   REG=.TRUE.
	   VFLAG=.FALSE.
	   QFLAG=.FALSE.
	   CALL BDI_CREDATA(OFID,NDIM,ONDAT,STATUS)
	   CALL BDI_MAPDATA(OFID,'WRITE',ODPTR,STATUS)
	   CALL BDI_CREAXES(OFID,1,STATUS)
	   CALL BDI_PUTAXVAL(OFID,1,0,1,NDAT,STATUS)
	   CALL BDI_CREVAR(OFID,1,NDAT,STATUS)
	   CALL BDI_MAPVAR(OFID,'WRITE',OVPTR,STATUS)
	   IF(STATUS.NE.SAI__OK)GOTO 9000
	   CALL DIFDAT_CALCP (NDAT,%VAL(DPTR),%VAL(ODPTR))
	   CALL MATH_POISSVAR(%VAL(ODPTR),NDAT,1,%VAL(OVPTR),STATUS)

*    Non-primitive

	ELSE

	   CALL BDI_CHKAXVAL(IFID,1,OK,REG,NVAL,STATUS)
*    If ok=false...
	   IF(.NOT.OK)THEN
	      STATUS=SAI__ERROR
	      CALL ERR_REP('AX_BAD','Bad axis',STATUS)
	   ENDIF
	   IF(.NOT.REG) THEN
             CALL MSG_PRNT('WARNING, IRREGULAR AXIS')
           END IF
	   IF(STATUS.NE.SAI__OK)GOTO 9000
*	CHECK FOR NVAL=NDAT
	   NDAT=DIMS(1)
	   IF(NVAL.NE.NDAT)THEN
	   STATUS=SAI__ERROR
	   CALL ERR_REP('NUM_AX',
     :     'Number axis points.ne.number data points',
     :     STATUS)
	   ENDIF
	   IF(STATUS.NE.SAI__OK)GOTO 9000
	   CALL BDI_MAPAXVAL(IFID,'READ',1,AXPTR,STATUS)
	   CALL BDI_CHKQUAL(IFID,OK,QNDIM,QDIMS,STATUS)
*    If ok=true...
	   IF( OK )THEN
	      CALL BDI_MAPLQUAL(IFID,'READ',BAD,QPTR,STATUS)
	      IF ( BAD ) THEN
                 CALL ARR_NBAD( NDAT, %VAL(QPTR), NBAD, STATUS )
                 CALL MSG_SETI( 'NBAD', NBAD )
                 CALL MSG_PRNT( '^NBAD bad quality points present' )
	      END IF
	   ELSE
	      NBAD=0
	   ENDIF
	   NOP=NDAT-1-NBAD
*    Variance
	   CALL BDI_CHKVAR(IFID,VOK,VNDIM,VDIMS,STATUS)
	   IF(VOK)THEN
	      CALL BDI_MAPVAR(IFID,'READ',VPTR,STATUS)
	   ELSE
	      CALL DYN_MAPR(1,NDAT,VPTR,STATUS)
	      CALL MATH_POISSVAR(%VAL(DPTR),NDAT,1,%VAL(VPTR),STATUS)
	   ENDIF
*    Set flags
	   REG=.FALSE.
	   VFLAG=.TRUE.
	   QFLAG=.FALSE.
	   DIMS(1)=NOP
	   CALL BDI_CREDATA(OFID,1,DIMS(1),STATUS)
	   CALL BDI_MAPDATA(OFID,'WRITE',ODPTR,STATUS)
	   CALL BDI_COPTEXT(IFID,OFID,STATUS)
           CALL BDI_COPMORE(IFID,OFID,STATUS)
	   CALL BDI_CREAXVAL(OFID,1,REG,DIMS(1),STATUS)
	   CALL BDI_MAPAXVAL(OFID,'WRITE',1,OAXPTR,STATUS)
	   CALL BDI_COPAXTEXT(IFID,OFID,1,1,STATUS)
	   CALL BDI_CREVAR(OFID,1,DIMS(1),STATUS)
	   CALL BDI_MAPVAR(OFID,'WRITE',OVPTR,STATUS)
	   IF(STATUS.NE.SAI__OK)GOTO 9000
           IF(OK)THEN
	   CALL DIFDAT_CALCS(NDAT,NOP,%VAL(DPTR),%VAL(QPTR),%VAL(AXPTR),
     :     %VAL(VPTR),%VAL(ODPTR),%VAL(OAXPTR),%VAL(OVPTR))
	   ELSE
	   CALL DIFDAT_CALNQ(NDAT,NOP,%VAL(DPTR),%VAL(AXPTR),%VAL(VPTR),
     :     %VAL(ODPTR),%VAL(OAXPTR),%VAL(OVPTR))
	   ENDIF
	ENDIF

*  History :
      CALL HSI_OK(IFID,OK,STATUS)
      IF ( OK ) THEN
        CALL HSI_COPY(IFID,OFID,STATUS)
        CALL HSI_ADD(OFID,VERSION,STATUS)
      ELSE
	CALL HSI_NEW(OFID,STATUS)
      END IF
      CALL HSI_ADD(OFID,VERSION,STATUS)

*  Tidy up
  99  CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END


*+   DIFDAT_CALCP-difference calculation for primitive dataset
	SUBROUTINE DIFDAT_CALCP(NDAT,DATA,DIFF)
*    Description :
*     This subroutine outputs an array of differences between neighbouring
*     points in the input array.
*    History :
*     22-NOV-88:  original (BHVAD::SRD)
*    Type definitions :
	IMPLICIT NONE
*    Import :

	INTEGER NDAT
	REAL DATA(NDAT)
*    Import-Export :
*
*    Export :
	REAL DIFF(NDAT-1)
*    Local constants :
*
*    Local variables :
	INTEGER I
*-
	DO I=1,NDAT-1
	   DIFF(I)=DATA(I+1)-DATA(I)
	ENDDO
	END


*+   DIFDAT_CALCS-Difference calculation for structured dataset.
	SUBROUTINE DIFDAT_CALCS(NDAT,NOP,DAT,QUAL,AX,VARI,DIFF,DIFFAX,VARO)
*    Description :
*     This subroutine outputs an array of differences between neighbouring good
*     data points. It also outputs the corresponding axis value for each
*     difference value.
*    History :
*     22-NOV-88: original (BHVAD::SRD)
*    Type definitions :
	IMPLICIT NONE
*    Import :
	INTEGER NDAT
	INTEGER NOP
	REAL DAT(NDAT)
	REAL AX(NDAT)
	REAL VARI(NDAT)
	LOGICAL QUAL(NDAT)
*    Import-Export :
*    Export :
	REAL DIFF(NOP)
	REAL DIFFAX(NOP)
	REAL VARO(NOP)
*    Local constants :
*    Local variables :
	REAL LDAT
	REAL LAX
	INTEGER NIP
	INTEGER NOPT
	INTEGER I
	INTEGER ILAST
*
*
	NOPT=1
	NIP=1
	DO WHILE(.NOT.QUAL(NIP))
	   NIP=NIP+1
	ENDDO
	   ILAST=NIP
	   DO I=NIP+1,NDAT
	      IF (QUAL(I))THEN
	         DIFF(NOPT)=(DAT(I)-DAT(ILAST)/(((AX(NDAT)-AX(1))/NDAT)/
     :	(AX(I)-AX(ILAST))))
	         VARO(NOPT)=VARI(ILAST)+VARI(I)
		 ILAST=I
		 DIFFAX(NOPT)=(AX(ILAST)+AX(I))/2
		 NOPT=NOPT+1
	      ENDIF
	   ENDDO
	END




*+   DIFDAT_CALNQ-Difference calculations for axis but no quality.
	SUBROUTINE DIFDAT_CALNQ(NDAT,NOP,DAT,AX,VARI,DIFF,DIFFAX,VARO)
*    Description :
*     This subroutine outputs an array of differences between neighbouring data
*     points.It also outputs the corresponding axis value for each difference
*     value.
*    History :
*     20-DEC-88: origina; (BHVAD::SRD)
*    Type definitions :
	IMPLICIT NONE
*    Import :
	INTEGER NDAT
	INTEGER NOP
	REAL DAT(NDAT)
	REAL AX(NDAT)
	REAL VARI(NDAT)
*    Import-Export :
*    Export :
	REAL DIFF(NOP)
	REAL DIFFAX(NOP)
	REAL VARO(NOP)
*    Local constants :
*    Local variables :
	INTEGER I
*
*
	DO I=1,NOP
	   DIFF(I)=DAT(I+1)-DAT(I)
	   DIFFAX(I)=(AX(I)+AX(I+1))/2
	   VARO(I)=VARI(I)+VARI(I+1)
	ENDDO

	END
