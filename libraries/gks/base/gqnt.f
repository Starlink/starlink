C# IL>=a, OL>=0
      SUBROUTINE GQNT(NTNR,IER,WINDOW,VIEWPT)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE NORMALIZATION TRANSFORMATION
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns the normalization transformation
*
*  MAINTENANCE LOG
*  ---------------
*     10/10/83  AS   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP   NTNR   Normalization transformation number
*     OUT   IER    Error indicator
*     OUT   WINDOW Window limits
*     OUT   VIEWPT Viewport limits
*
      INTEGER NTNR, IER
      REAL WINDOW(4), VIEWPT(4)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gksl.cmn'
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GGKOP,GSGOP)
      IER = KERROR

      IF (KERROR .EQ. 0) THEN

        IF (NTNR.GE.0 .AND. NTNR.LE.KT) THEN

          WINDOW(1) = QLWXL(NTNR)
          WINDOW(2) = QLWXR(NTNR)
          WINDOW(3) = QLWYB(NTNR)
          WINDOW(4) = QLWYT(NTNR)

          VIEWPT(1) = QLVPXL(NTNR)
          VIEWPT(2) = QLVPXR(NTNR)
          VIEWPT(3) = QLVPYB(NTNR)
          VIEWPT(4) = QLVPYT(NTNR)

        ELSE

          IER = 50

        ENDIF

      ENDIF

      END
