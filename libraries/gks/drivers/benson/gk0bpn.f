

*----------------------------------------------------------------------
      SUBROUTINE GK0BPN(RED,GREEN,BLUE,IPEN,RPEN,GPEN,BPEN)
*
* (C) COPYRIGHT ICL & SERC  1985
*

*----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Part of Workstation Driver
*  Author:             DRJF
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To return the PEN number and its corresponding RGB value
*     which is the nearest available to the specified RGB value
*
*  MAINTENANCE LOG
*  ---------------
*     01/12/85  DRJF  Original version stabilized
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwdt.cmn'
*
*  ARGUMENTS
*  ---------
*     INP RED   - RED intensity (set value)
*     INP GREEN - GREEN intensity (set value)
*     INP BLUE  - BLUE intensity (set value)
*     OUT IPEN  - BENSON plotter pen number (realizing set value)
*     OUT RPEN  - RED intensity of pen IPEN
*     OUT GPEN  - GREEN intensity of pen IPEN
*     OUT BPEN  - BLUE intensity of pen IPEN
*
      REAL RED,GREEN,BLUE
      INTEGER IPEN
      REAL RPEN,GPEN,BPEN
*
*  LOCALS
*  ------
*
      INTEGER I,NCOLS,IHIGH,IMED,ILOW
      PARAMETER  (NCOLS=3)
      REAL TNSTYS(NCOLS),RMIN,RMAX
*
*----------------------------------------------------------------------


      TNSTYS(1)=RED
      TNSTYS(2)=GREEN
      TNSTYS(3)=BLUE
*
*     Set pointers to the highest, lowest and medium intensities
*
      IHIGH=1
      IMED=2
      ILOW=3
      RMIN=TNSTYS(3)
      RMAX=TNSTYS(1)
      DO 10 I=1,NCOLS
        IF (TNSTYS(I).GT.RMAX) THEN
          IHIGH=I
          RMAX=TNSTYS(I)
        END IF
        IF (TNSTYS(I).LT.RMIN) THEN
          ILOW=I
          RMIN=TNSTYS(I)
        END IF
   10 CONTINUE
      IMED=6-IHIGH-ILOW
*
*     Establish plotter pen. Note plotter pens are:
*     0 - Black, 1 - Red, 2 - Blue, 3 - Green
*
      IF (TNSTYS(IHIGH).LE.0.1) THEN
*
*       BLACK
*
        IPEN=0
      ELSE IF (TNSTYS(ILOW).GE.(0.6*TNSTYS(IHIGH))) THEN
*
*       WHITE represented as BLACK
*
        IPEN=0
      ELSE IF (TNSTYS(IMED).GE.(0.8*TNSTYS(IHIGH))) THEN
*
*       Secondary colours
*
        IF ((IHIGH+IMED).EQ.5) THEN
*
*         CYAN represented as BLUE
*
          IPEN=2
        ELSE IF ((IHIGH+IMED).EQ.4) THEN
*
*         MAGENTA represented as RED
*
          IPEN=1
        ELSE IF ((IHIGH+IMED).EQ.3) THEN
*
*         YELLOW represented as GREEN
*
          IPEN=3
        END IF
      ELSE IF (TNSTYS(IHIGH).GE.(TNSTYS(IMED)+TNSTYS(ILOW)).OR.
     :         TNSTYS(IMED).LE.(0.6*TNSTYS(IHIGH))) THEN
        IF (IHIGH.EQ.1) THEN
*
*         Red
*
          IPEN=1
        ELSE
*
*         Swap BLUE and GREEN for Benson
*
          IPEN=5-IHIGH
        END IF
      ELSE
*
*       BLACK
*
        IPEN=0
      END IF
*
*     Set colour intensities according to plotter pen
*
      RPEN=0.0
      GPEN=0.0
      BPEN=0.0
      IF (IPEN.EQ.0) GOTO 9999
      GOTO (100,200,300) IPEN
  100 CONTINUE
*
*     Set RED intensity of IPEN
*
      RPEN=1.0
      GOTO 9999
  200 CONTINUE
*
*     Set BLUE intensity of IPEN
*
      BPEN=1.0
      GOTO 9999
  300 CONTINUE
*
*     Set GREEN intensity of IPEN
*
      GPEN=1.0
      IF (KPCI(KWKIX)-1.EQ.3) THEN
*
*       3 Pen plotter
*
        IF (IPEN.EQ.3) THEN
*
*         GREEN is represented as BLACK
*
          IPEN=0
        END IF
      END IF
 9999 CONTINUE
      RETURN
*
      END
