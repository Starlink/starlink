

      SUBROUTINE GK1TSS
      INCLUDE '../../include/check.inc'
*       Set character size and rotation

      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     ICHAHI Offset in QWKDAT for hardware character height
*     ICHAWI Offset in QWKDAT for hardware character width
*     ICHROT Offset in QWKDAT for hardware character rotation
*
      INTEGER    ICHAHI,   ICHAWI,   ICHROT
      PARAMETER (ICHAHI=1, ICHAWI=2, ICHROT=3)
      INTEGER ISIZE(12),IROT(10),I,J,NLEFT,MANT,EXP
     :        ,ICW,ICH
*
      REAL PTORX,PTORY
      PARAMETER (PTORX=6.0, PTORY=6.0)
      DATA ISIZE /27,77,67,9*0/ ,IROT /27,77,82,0,0,0,0,0,0,0/
*
      ICW=INT(QWKDAT(ICHAWI,KWKIX)*PTORX+0.5)
      ICH=INT(QWKDAT(ICHAHI,KWKIX)*PTORY+0.5)
      I=4
      CALL GK1TTI(ICW,ISIZE,I)
      CALL GK1TTI(ICH,ISIZE,I)
      CALL GK1TTI(1,ISIZE,I)
      CALL GKIOBO(KIOPB,I-1,ISIZE,NLEFT)
*
      CALL GK1TTR(QWKDAT(ICHROT,KWKIX),MANT,EXP)
      I=4
      CALL GK1TTI(MANT,IROT,I)
      CALL GK1TTI(EXP,IROT,I)
      CALL GKIOBO(KIOPB,I-1,IROT,NLEFT)
*
      RETURN
      END
