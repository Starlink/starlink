*+  SBIN - Rebin spectrum
      SUBROUTINE SBIN( STATUS )
*    Description :
*     rebins a spectrum to give approximately equal population
*     in each bin
*    Authors :
*     BHVAD::RJV
*    History :
*
*      6 Nov 92 : V1.5-1 Max bin content changed to min (RJV)
*     18 Nov 93 : V1.7-0 Added missing AST_INIT call (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*     27 Mar 95 : V1.8-1 BIT_ used (RJV)
*     21 Apr 95 : V1.8-2 Updated data interface (DJA)
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*    Global variables :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*80 TEXT
      REAL SUM
      REAL MINVAL
      INTEGER NDIM,DIMS(ADI__MXDIM)
      INTEGER INVAL,ONVAL
      INTEGER IDPTR,IAPTR,IWPTR,IVPTR,IQPTR
      INTEGER ODPTR,OAPTR,OWPTR,OVPTR
      INTEGER TDPTR,TAPTR,TWPTR,TVPTR
      INTEGER OPT
      INTEGER			IFID			! Input dataset id
      INTEGER			OFID			! Output dataset id

      BYTE 			MASK

      LOGICAL DOK,VOK,QOK

*  Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'SBIN Version 1.8-2')
*-

*    Version id
      CALL MSG_PRNT( VERSION )

*    Initialise Asterix
      CALL AST_INIT()

*  Get input & output files
      CALL USI_TASSOC2( 'INP', 'OUT', 'READ', IFID, OFID, STATUS )

      CALL BDI_CHKDATA(IFID,DOK,NDIM,DIMS,STATUS)
      IF (DOK.AND.NDIM.EQ.1) THEN

*  get pointers to input components
        INVAL=DIMS(1)
        CALL BDI_MAPDATA(IFID,'READ',IDPTR,STATUS)
        CALL BDI_CHKVAR(IFID,VOK,NDIM,DIMS,STATUS)
        IF (VOK) THEN
          CALL BDI_MAPVAR(IFID,'READ',IVPTR,STATUS)
        ENDIF
        CALL BDI_MAPAXVAL(IFID,'READ',1,IAPTR,STATUS)
        CALL BDI_MAPAXWID(IFID,'READ',1,IWPTR,STATUS)

*  get total counts taking account of any QUALITY
        CALL BDI_CHKQUAL(IFID,QOK,NDIM,DIMS,STATUS)
        IF (QOK) THEN
          CALL BDI_MAPQUAL(IFID,'READ',IQPTR,STATUS)
          CALL BDI_GETMASK(IFID,MASK,STATUS)
          CALL ARR_SUM1RQ(INVAL,%VAL(IDPTR),%VAL(IQPTR),MASK,SUM,STATUS)
        ELSE
          CALL ARR_SUM1R(INVAL,%VAL(IDPTR),SUM,STATUS)
        ENDIF

*  get some temporary storage for rebined data
        CALL DYN_MAPR(1,INVAL,TDPTR,STATUS)
        CALL DYN_MAPR(1,INVAL,TAPTR,STATUS)
        CALL DYN_MAPR(1,INVAL,TWPTR,STATUS)
        IF (VOK) THEN
          CALL DYN_MAPR(1,INVAL,TVPTR,STATUS)
        ENDIF

*  which mode of rebinning
        CALL USI_GET0I('OPT',OPT,STATUS)
        IF (OPT.EQ.1) THEN
          CALL USI_GET0I('NBIN',ONVAL,STATUS)
          MINVAL=SUM/REAL(ONVAL)
        ELSEIF (OPT.EQ.2) THEN
          CALL USI_GET0R('MIN',MINVAL,STATUS)
        ELSE
          CALL MSG_PRNT('AST_ERR: invalid mode option')
          STATUS=SAI__ERROR
        ENDIF

*  rebin
        CALL SBIN_DOIT(INVAL,%VAL(IDPTR),%VAL(IAPTR),%VAL(IWPTR),
     :                   VOK,%VAL(IVPTR),QOK,%VAL(IQPTR),MASK,
     :                   MINVAL,ONVAL,%VAL(TDPTR),%VAL(TAPTR),
     :                   %VAL(TWPTR),%VAL(TVPTR),STATUS)

*  copy data from temporary storage to output file
        CALL BDI_CREDATA(OFID,1,ONVAL,STATUS)
        CALL BDI_MAPDATA(OFID,'WRITE',ODPTR,STATUS)
        CALL ARR_COP1R(ONVAL,%VAL(TDPTR),%VAL(ODPTR),STATUS)
        CALL BDI_UNMAPDATA(OFID,STATUS)
        IF (VOK) THEN
          CALL BDI_CREVAR(OFID,1,ONVAL,STATUS)
          CALL BDI_MAPVAR(OFID,'WRITE',OVPTR,STATUS)
          CALL ARR_COP1R(ONVAL,%VAL(TVPTR),%VAL(OVPTR),STATUS)
          CALL BDI_UNMAPVAR(OFID,STATUS)
        ENDIF
        CALL BDI_CREAXVAL(OFID,1,.FALSE.,ONVAL,STATUS)
        CALL BDI_MAPAXVAL(OFID,'WRITE',1,OAPTR,STATUS)
        CALL ARR_COP1R(ONVAL,%VAL(TAPTR),%VAL(OAPTR),STATUS)
        CALL BDI_UNMAPAXVAL(OFID,1,STATUS)
        CALL BDI_CREAXWID(OFID,1,.FALSE.,ONVAL,STATUS)
        CALL BDI_MAPAXWID(OFID,'WRITE',1,OWPTR,STATUS)
        CALL ARR_COP1R(ONVAL,%VAL(TWPTR),%VAL(OWPTR),STATUS)
        CALL BDI_UNMAPAXWID(OFID,1,STATUS)
        CALL BDI_GETAXLABEL(IFID,1,TEXT,STATUS)
        CALL BDI_PUTAXLABEL(OFID,1,TEXT,STATUS)
        CALL BDI_GETAXUNITS(IFID,1,TEXT,STATUS)
        CALL BDI_PUTAXUNITS(OFID,1,TEXT,STATUS)

*  update history
        CALL HSI_COPY(IFID,OFID,STATUS)
        CALL HSI_ADD(OFID,VERSION,STATUS)

*  copy ancilliary stuff
        CALL BDI_COPTEXT(IFID,OFID,STATUS)
        CALL BDI_COPMORE(IFID,OFID,STATUS)

*  release temporary storage
        CALL DYN_UNMAP(TDPTR,STATUS)
        CALL DYN_UNMAP(TAPTR,STATUS)
        CALL DYN_UNMAP(TWPTR,STATUS)
        IF (VOK) THEN
          CALL DYN_UNMAP(TVPTR,STATUS)
        ENDIF

      ENDIF

      CALL BDI_RELEASE(IFID,STATUS)
      CALL BDI_RELEASE(OFID,STATUS)
      CALL USI_TANNUL(IFID,STATUS)
      CALL USI_TANNUL(OFID,STATUS)
      CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END


*+  SBIN_DOIT
      SUBROUTINE SBIN_DOIT(IN,ID,IA,IW,VOK,IV,QOK,IQ,MASK,MINVAL,
     :                                      ON,OD,OA,OW,OV,STATUS)
*    Description :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*    Import :
      INTEGER IN
      REAL ID(*),IA(*),IW(*),IV(*)
      REAL MINVAL
      BYTE IQ(*),MASK
      LOGICAL VOK,QOK
*    Import-Export :
*    Export :
      INTEGER ON
      REAL OD(*),OA(*),OW(*),OV(*)
*    Status :
      INTEGER STATUS
*    Functions :
      BYTE BIT_ANDUB
*    Local variables :
      REAL TOT,VARTOT
      REAL LO,HI
      INTEGER I
      LOGICAL GOOD
*-

      IF (STATUS.EQ.SAI__OK) THEN

        ON=0
        I=1

        LO=IA(I)-IW(I)/2.0
        TOT=0.0
        IF (VOK) THEN
          VARTOT=0.0
        ENDIF
        DO WHILE (I.LE.IN)
          IF (QOK) THEN
            GOOD=(BIT_ANDUB(IQ(I),MASK).EQ.QUAL__GOOD)
          ELSE
            GOOD=.TRUE.
          ENDIF
          IF (GOOD) THEN
            TOT=TOT+ID(I)
            IF (VOK) THEN
              VARTOT=VARTOT+IV(I)
            ENDIF

            IF (TOT.GE.MINVAL.OR.I.EQ.IN) THEN
*  output bin
              ON=ON+1
              OD(ON)=TOT
              IF (VOK) THEN
                OV(ON)=VARTOT
              ENDIF
              HI=IA(I)+IW(I)/2.0
              OA(ON)=(HI+LO)/2.0
              OW(ON)=ABS(HI-LO)
              LO=HI
              TOT=0.0
              VARTOT=0.0
            ENDIF

          ENDIF

          I=I+1

        ENDDO

      ENDIF

      END
