      SUBROUTINE GRSETS(IDENT,XSIZE,YSIZE)
*+
*     - - - - - - - -
*       G R S E T S   (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Changes the size of the workstation view surface
*
*   Given
*      IDENT    i     Device id (IGNORED)
*      XSIZE    r     x size in device coordinates
*      YSIZE    r     y size in device coordinates
*
*   Read from COMMON
*      GRCIDE   i     Current device
*      GRTYP    i()   Device type
*      GRWSOP   l()   W/S opened by GRPCKG
*
*   Written to COMMON
*      GRVIE2   r()   Full workstation viewport
*      GRWIN2   r()   Full workstation window
*      GRXMAX   r()   Maximum extent (x)
*      GRYMAX   r()   Maximum extent (y)
*
*   Constants from GRECOM
*      TRN      i     Transformation used by GRPCKG
*      TRN2     i     Transformation used by GRPCKG
*
*   Constants from GKS_PAR
*      GPERFO   i     Perform implicit regeneration
*      GMO      i     Catagory - metafile output
*
*   D.L.Terrett  Starlink  Aug 1987
*+
      IMPLICIT NONE

      INTEGER IDENT
      REAL XSIZE, YSIZE

      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'

      INCLUDE 'GKS_PAR'


      INTEGER IERR, LX, LY, IUNIT
      REAL AR, XM, YM, XDC, YDC

      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRSETS - No PGPLOT device open',
     :   GRNODO)
      ELSE IF (.NOT.GRWSOP(GRCIDE)) THEN
         CALL ERR_REP('GRNOPG',
     :   'GRSETS - workstation was not opened by PGPLOT', GRNOPG)
      ELSE

*   Find the size and resolution of the workstation
        CALL GQDSP(GRTYP(GRCIDE),IERR,IUNIT,XM,YM,LX,LY)
        IF (IERR.NE.0) THEN
            CALL GRQREP('GRSETS', 'GQMDS', IERR)
            GO TO 9999
        END IF

*   Calculate new DC limits
        AR = (XSIZE/GRXPIN(GRCIDE))/(YSIZE/GRYPIN(GRCIDE))
        IF (XSIZE.GT.REAL(LX-1) .OR. YSIZE.GT.REAL(LY-1)) THEN
           IF (AR.LT.XM/YM) THEN
              YDC = YM
              XDC = YM*AR
           ELSE
              XDC = XM
              YDC = XM/AR
           END IF
        ELSE
           XDC = XM * XSIZE/REAL(LX-1)
           YDC = YM * YSIZE/REAL(LY-1)
        END IF


*   Set the workstation viewport to the new size and force regeneration
        CALL GSWKVP(GRWKID(GRCIDE),0.0,XDC,0.0,YDC)
        CALL GUWK(GRWKID(GRCIDE),GPERFO)

*   Set the workstation window to the maximum possible area of the same
*   aspect ratio
        CALL GSWKWN(GRWKID(GRCIDE),0.0,MIN(1.0,AR),0.0,MIN(1.0,1.0/AR))

*   Set the viewports to the same
        CALL GSVP(TRN,0.0,MIN(1.0,AR),0.0,MIN(1.0,1.0/AR))
        CALL GSVP(TRN2,0.0,MIN(1.0,AR),0.0,MIN(1.0,1.0/AR))

*   Set the windows so that world coordinates match device pixels
        CALL GSWN(TRN,0.0,XDC*REAL(LX-1)/XM,0.0,YDC*REAL(LY-1)/YM)
        CALL GSWN(TRN2,0.0,XDC*REAL(LX-1)/XM,0.0,YDC*REAL(LY-1)/YM)

*   Save values of full workstation viewport and window
         GRVIE2(1,GRCIDE) = 0.0
         GRVIE2(2,GRCIDE) = MIN(1.0,AR)
         GRVIE2(3,GRCIDE) = 0.0
         GRVIE2(4,GRCIDE) = MIN(1.0,1.0/AR)
         GRWIN2(1,GRCIDE) = 0.0
         GRWIN2(2,GRCIDE) = XDC*REAL(LX-1)/XM
         GRWIN2(3,GRCIDE) = 0.0
         GRWIN2(4,GRCIDE) = YDC*REAL(LY-1)/YM

*   Save workstation size
        GRXMAX(GRCIDE) = XDC*REAL(LX-1)/XM
        GRYMAX(GRCIDE) = YDC*REAL(LY-1)/YM
      END IF

 9999 CONTINUE
      END
