C# IL>=a, OL>=0
      SUBROUTINE GSELNT(ITNR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SELECT NORMALISATION TRANSFORMATION
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Set the current normalisation number in the GKS State List
*
*  MAINTENANCE LOG
*  ---------------
*    16/3/83   First version
*   30/3/83     CHECK.INC included
*   23/6/83     Changes to use KERROR for error reporting
*     20/01/87  DCS   IS conversion. Always set current normalization
*                     transformation and set flags out of date; also set
*                     clipping rectangle (all necessary in case
*                     interpreting a metafile has altered the clipping
*                     rectangle).
*
*
*  ARGUMENTS
*  ---------
*     INP   ITNR   Normalisation Transformation Number
*
      INTEGER ITNR
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYSL/  Viewport
*     Modify /GKYSL/  Normalization Transformation Number, Clipping
*                     Rectangle
*     Modify /GKYERR/ KERROR
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*       50   N.T number invalid (<0 or >max)
*
*---------------------------------------------------------------------


*   GKS Prologue
      CALL GKPRLG (ESELNT, GGKOP,GSGOP)
      IF( KERROR.GT.0 ) GOTO 990

*   Check transformation number (error 50)
      IF( ITNR.LT.0 .OR. ITNR.GT.KT ) THEN
        KERROR=50
        GOTO 990
      ENDIF
*   End of error checking
*---------------------------


*   Set current N.T., set clipping rectangle, and set flags to be out of
*   date.
      KCNTN=ITNR
      QCCLXL=QLVPXL(ITNR)
      QCCLXR=QLVPXR(ITNR)
      QCCLYB=QLVPYB(ITNR)
      QCCLYT=QLVPYT(ITNR)
      CALL GKTOLD
      GOTO 999

*   Report error
  990 CALL GKERR(KERROR)

*   Finish
  999 CONTINUE
      END
