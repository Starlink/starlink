      SUBROUTINE GK0ERF(RX,RY,DONE)
*
*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Part of workstation driver
*  Author:             DLT
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*      Test for and possibly draw a filed rectangle
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine.
*
*  ARGUMENTS
*  ---------
*     INP RX     X coordinates
*     INP RY     Y coordinates
*     OUT DONE   Whether fill was done

      REAL RX(4) ,RY(4)
      LOGICAL DONE
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     IESC   ASCII code for ESC
*     NLEFT  Returned by GKIOBO (not used here)
*     IPERON Orders to switch Pericom to rectangular draw
*     IPEROF Orders to switch Pericom to normal drawing
*     XDC,YDC Points in device coordinates
*     IXDC,IYDC Points in device coordinates
*     IXSIZE, IYSIZE Size of rectangle
*
      INTEGER IESC, STX, ETX
      PARAMETER (IESC=27, STX=2, ETX=3)
      INTEGER NLEFT, I, IXDC(4), IYDC(4), IXSIZE, IYSIZE
      REAL XDC(4), YDC(4)
      INTEGER IPERON(2),IPEROF(2)

*
*  Pericoms & GraphOn 235
      DATA IPERON/IESC,STX/
      DATA IPEROF/IESC,ETX/
*
*-----------------------------------------------------------------------

*   Assume we can't do the fill
      DONE = .FALSE.

*   Convert coordinates to device coordinates
      CALL GKTWD(4,RX,RY,XDC,YDC)
*   Convert to integers
      DO 10 I = 1,4
        IXDC(I) = NINT(XDC(I))
        IYDC(I) = NINT(YDC(I))
   10 CONTINUE

*   Is first side parallel to x axis
      IF (IYDC(1).EQ.IYDC(2)) THEN
        IXSIZE = IXDC(2) - IXDC(1)
*   check that last side is parallel to y
        IF (IXDC(4).EQ.IXDC(1)) THEN
          IYSIZE = IYDC(4) - IYDC(1)
        ELSE
          GOTO 100
        ENDIF
*   try first side parallel to y
      ELSEIF (IXDC(1).EQ.IXDC(2)) THEN
        IYSIZE = IYDC(2) - IYDC(1)
*   check that last side is parallel to x
        IF (IYDC(4).EQ.IYDC(1)) THEN
          IXSIZE = IXDC(4) - IXDC(1)
        ELSE
          GOTO 100
        ENDIF
      ELSE
        GOTO 100
      ENDIF
*   Check that 3rd point forms a rectangle
      IF (IXDC(1)+IXSIZE.NE.IXDC(3) .OR. IYDC(1)+IYSIZE.NE.IYDC(3))
     :                                                       GOTO 100
*   Trim rectangle to clipping window
      IF (XDC(1).LT.QWCLXL(KWKIX)) XDC(1) = QWCLXL(KWKIX)
      IF (XDC(1).GT.QWCLXR(KWKIX)) XDC(1) = QWCLXR(KWKIX)
      IF (XDC(3).LT.QWCLXL(KWKIX)) XDC(3) = QWCLXL(KWKIX)
      IF (XDC(3).GT.QWCLXR(KWKIX)) XDC(3) = QWCLXR(KWKIX)
      IF (YDC(1).LT.QWCLYB(KWKIX)) YDC(1) = QWCLYB(KWKIX)
      IF (YDC(1).GT.QWCLYT(KWKIX)) YDC(1) = QWCLYT(KWKIX)
      IF (YDC(3).LT.QWCLYB(KWKIX)) YDC(3) = QWCLYB(KWKIX)
      IF (YDC(3).GT.QWCLYT(KWKIX)) YDC(3) = QWCLYT(KWKIX)
*   Check that rectangle still exists
      IF (XDC(1).EQ.XDC(3) .AND. YDC(1).EQ.YDC(3)) GOTO 100
*   All OK copy 3rd point to second element of array
      XDC(2) = XDC(3)
      YDC(2) = YDC(3)

*   Pericom Monterey/Graphpack & GraphOn 235
      IF( KWKTYP.EQ.820 .OR. KWKTYP.EQ.821 .OR. KWKTYP.EQ.845) THEN
        CALL GKIOBO(KIOPB,2,IPERON,NLEFT)
        CALL GK0TLN(2,XDC,YDC)
        CALL GKIOBO(KIOPB,2,IPEROF,NLEFT)
        DONE = .TRUE.
      ENDIF

  100 CONTINUE
      END
