*+  ARX_MASK - convert ARD text to logical mask
      SUBROUTINE ARX_MASK(ID,DIMS,BASE,SCALE,UNITS,MASK,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'GRP_PAR'
      INCLUDE 'PRM_PAR'
*    Global Variables :
*    Import :
      INTEGER ID
      INTEGER DIMS(2)
      REAL BASE(2)
      REAL SCALE(2)
      CHARACTER*(*) UNITS(2)
*    Import-Export :
*    Export :
      INTEGER MASK(*)
*    Status :
      INTEGER STATUS
*    External references :
*    Local Constants :
*    Local variables :
      REAL TR(6)
      INTEGER LBND(2),LBNDI(2),LBNDE(2)
      INTEGER UBND(2),UBNDI(2),UBNDE(2)
      INTEGER REGVAL
      INTEGER INDEX
*-
      IF (STATUS.EQ.SAI__OK) THEN


*  set image dimensions
        LBND(1)=1
        LBND(2)=1
        UBND(1)=DIMS(1)
        UBND(2)=DIMS(2)

*  pixel coords.
        IF (UNITS(1)(:3).EQ.'PIX') THEN
          TR(1)=VAL__BADR
*  otherwise set pixel to world transformation - see Sec.4 of ARD manual
*  note that ARD assumes the pixel coord of first pixel to range
*  from 0..1 whereas ASTERIX assumes a range 0.5 to 1.5 with 1
*  at centre
        ELSE
          TR(1)=0.5-BASE(1)/SCALE(1)
          TR(2)=1.0/SCALE(1)
          TR(3)=0.0
          TR(4)=0.5-BASE(2)/SCALE(2)
          TR(5)=0.0
          TR(6)=1.0/SCALE(2)
        ENDIF

        REGVAL=2

*  interpret ARD
        CALL ARD_WORK(ID,2,LBND,UBND,TR,.FALSE.,REGVAL,MASK,
     :                         LBNDI,UBNDI,LBNDE,UBNDE,STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from ARX_MASK',STATUS)
        ENDIF

      ENDIF

      END
