C# IL>=a, OL>=0
      SUBROUTINE GQWKT (IWKID,IER,ITUS,RWINDO,CWINDO,RVIEWP,CVIEWP)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE WORKSTATION TRANSFORMATION
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns workstation transformations
*
*  MAINTENANCE LOG
*  ---------------
*     04/10/83  AS    Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IWKID  - workstation identifier
*     OUT IER    - error indicator
*     OUT ITUS   - workstation transformation update state
*     OUT RWINDO - requested workstation window
*     OUR CWINDO - current workstation window
*     OUT RVIEWP - requested workstation viewport
*     OUR CVIEWP - current workstation viewport
*
      INTEGER IWKID, IER, ITUS
      REAL RWINDO(4), CWINDO(4), RVIEWP(4), CVIEWP(4)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*
      REAL RX(8),RY(8)
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GWSOP,GSGOP)

      IF (KERROR.EQ.0) THEN
        CALL GKSONW(IWKID,KQWKT,1,KDAT,8,RX,RY,1,CH)
        IF (KERROR.EQ.0) THEN
          ITUS = KWI1
          RWINDO(1) = RX(1)
          RWINDO(2) = RX(2)
          RWINDO(3) = RY(1)
          RWINDO(4) = RY(2)
          CWINDO(1) = RX(3)
          CWINDO(2) = RX(4)
          CWINDO(3) = RY(3)
          CWINDO(4) = RY(4)
          RVIEWP(1) = RX(5)
          RVIEWP(2) = RX(6)
          RVIEWP(3) = RY(5)
          RVIEWP(4) = RY(6)
          CVIEWP(1) = RX(7)
          CVIEWP(2) = RX(8)
          CVIEWP(3) = RY(7)
          CVIEWP(4) = RY(8)
        ENDIF
      ENDIF
      IER = KERROR

      END
