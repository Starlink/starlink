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
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) ILOC,OLOC
      CHARACTER*(DAT__SZTYP) TYPE
      CHARACTER*80 TEXT
      REAL SUM
      REAL MINVAL
      INTEGER NDIM,DIMS(DAT__MXDIM)
      INTEGER INVAL,ONVAL
      INTEGER IDPTR,IAPTR,IWPTR,IVPTR,IQPTR
      INTEGER ODPTR,OAPTR,OWPTR,OVPTR
      INTEGER TDPTR,TAPTR,TWPTR,TVPTR
      INTEGER OPT
      BYTE MASK
      LOGICAL DOK,VOK,QOK
      LOGICAL PRIM
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'SBIN Version 1.8-0')
*-

*    Version id
      CALL MSG_PRNT( VERSION )

*    Initialise Asterix
      CALL AST_INIT()

*  get input file
      CALL USI_ASSOCI('INP','READ',ILOC,PRIM,STATUS)

      CALL BDA_CHKDATA(ILOC,DOK,NDIM,DIMS,STATUS)
      IF (DOK.AND.NDIM.EQ.1) THEN

* create output dataset
        CALL DAT_TYPE(ILOC,TYPE,STATUS)
        CALL USI_ASSOCO('OUT',TYPE,OLOC,STATUS)

*  get pointers to input components
        INVAL=DIMS(1)
        CALL BDA_MAPDATA(ILOC,'R',IDPTR,STATUS)
        CALL BDA_CHKVAR(ILOC,VOK,NDIM,DIMS,STATUS)
        IF (VOK) THEN
          CALL BDA_MAPVAR(ILOC,'R',IVPTR,STATUS)
        ENDIF
        CALL BDA_MAPAXVAL(ILOC,'R',1,IAPTR,STATUS)
        CALL BDA_MAPAXWID(ILOC,'R',1,IWPTR,STATUS)

*  get total counts taking account of any QUALITY
        CALL BDA_CHKQUAL(ILOC,QOK,NDIM,DIMS,STATUS)
        IF (QOK) THEN
          CALL BDA_MAPQUAL(ILOC,'R',IQPTR,STATUS)
          CALL BDA_GETMASK(ILOC,MASK,STATUS)
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
        CALL BDA_CREDATA(OLOC,1,ONVAL,STATUS)
        CALL BDA_MAPDATA(OLOC,'W',ODPTR,STATUS)
        CALL ARR_COP1R(ONVAL,%VAL(TDPTR),%VAL(ODPTR),STATUS)
        CALL BDA_UNMAPDATA(OLOC,STATUS)
        IF (VOK) THEN
          CALL BDA_CREVAR(OLOC,1,ONVAL,STATUS)
          CALL BDA_MAPVAR(OLOC,'W',OVPTR,STATUS)
          CALL ARR_COP1R(ONVAL,%VAL(TVPTR),%VAL(OVPTR),STATUS)
          CALL BDA_UNMAPVAR(OLOC,STATUS)
        ENDIF
        CALL BDA_CREAXVAL(OLOC,1,.FALSE.,ONVAL,STATUS)
        CALL BDA_MAPAXVAL(OLOC,'W',1,OAPTR,STATUS)
        CALL ARR_COP1R(ONVAL,%VAL(TAPTR),%VAL(OAPTR),STATUS)
        CALL BDA_UNMAPAXVAL(OLOC,1,STATUS)
        CALL BDA_CREAXWID(OLOC,1,.FALSE.,ONVAL,STATUS)
        CALL BDA_MAPAXWID(OLOC,'W',1,OWPTR,STATUS)
        CALL ARR_COP1R(ONVAL,%VAL(TWPTR),%VAL(OWPTR),STATUS)
        CALL BDA_UNMAPAXWID(OLOC,1,STATUS)
        CALL BDA_GETAXLABEL(ILOC,1,TEXT,STATUS)
        CALL BDA_PUTAXLABEL(OLOC,1,TEXT,STATUS)
        CALL BDA_GETAXUNITS(ILOC,1,TEXT,STATUS)
        CALL BDA_PUTAXUNITS(OLOC,1,TEXT,STATUS)

*  update history
        CALL HIST_COPY(ILOC,OLOC,STATUS)
        CALL HIST_ADD(OLOC,VERSION,STATUS)

*  copy ancilliary stuff
        CALL BDA_COPTEXT(ILOC,OLOC,STATUS)
        CALL BDA_COPMORE(ILOC,OLOC,STATUS)


*  release temporary storage
        CALL DYN_UNMAP(TDPTR,STATUS)
        CALL DYN_UNMAP(TAPTR,STATUS)
        CALL DYN_UNMAP(TWPTR,STATUS)
        IF (VOK) THEN
          CALL DYN_UNMAP(TVPTR,STATUS)
        ENDIF

      ENDIF

      CALL BDA_RELEASE(ILOC,STATUS)
      CALL BDA_RELEASE(OLOC,STATUS)
      CALL USI_ANNUL(ILOC,STATUS)
      CALL USI_ANNUL(OLOC,STATUS)
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
      INCLUDE 'DAT_PAR'
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
            GOOD=((IQ(I).AND.MASK).EQ.QUAL__GOOD)
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
