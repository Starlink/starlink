C# IL>=a, OL>=0
      SUBROUTINE GSVP(ITNR, XMIN,XMAX, YMIN,YMAX)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET VIEWPORT
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Set viewport of specified transformation.
*
*  MAINTENANCE LOG
*  ---------------
*     22/06/83  JRG   Stabilised
*     23/06/83  JRG   Bug correction
*     23/06/83  JRG   Changes to use KERROR for error reporting
*     08/07/83  JRG   Calls GKRECT to check rectangle limits
*     28/09/83  AS    Change subroutine name
*     19/01/87  ARG   IS conversion. Report negative GKPRLG errors.
*     20/01/87  DCS   IS conversion. Set clipping rectangle in GKS
*                     State List (new entry) if viewport of current
*                     NT is specified.
*
*  ARGUMENTS
*  ---------
*     INP   ITNR       Normalisation transformation number
*     INP   XMIN, XMAX Limits in X direction of viewport
*     INP   YMIN, YMAX Limits in Y direction of viewport
*
      INTEGER ITNR
      REAL XMIN,XMAX, YMIN,YMAX
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/ Viewport, Clipping Rectangle
*     Modify /GKYERR/ KERROR
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*        8   Wrong GKS state
*       50   Transformation number invalid (<1 or >max)
*       51   Viewport limits in wrong order or too close
*       52   Viewport not within unit square
*
*---------------------------------------------------------------------

*   GKS Prologue
      CALL GKPRLG (ESVP,GGKOP,GSGOP)
      IF (KERROR.NE.0) GOTO 990

*   Check Transformation Number (error 50): remember that 0 cannot
*   be altered
      IF( ITNR.LE.0 .OR. ITNR.GT.KT ) THEN
        KERROR=50
        GOTO 990
      ENDIF

*   Check that limits are right way round (error 51)
      CALL GKRECT(XMIN,XMAX, YMIN,YMAX)
      IF( KERROR.NE.0 ) GOTO 990

*   Check that limits are within NDC unit square
      IF( XMIN.LT.0.0 .OR. XMAX.GT.1.0 .OR.
     :    YMIN.LT.0.0 .OR. YMAX.GT.1.0 ) THEN
            KERROR=52
            GOTO 990
      ENDIF
*   End of error checking
*----------------------------


*   In GKS State List, update viewport for this transformation
      QLVPXL(ITNR)=XMIN
      QLVPXR(ITNR)=XMAX
      QLVPYB(ITNR)=YMIN
      QLVPYT(ITNR)=YMAX

*   If ITNR is the currently selected N.T., then set clipping rectangle
*   to these limits and set flags to be out of date.
      IF (ITNR.EQ.KCNTN) THEN
        QCCLXL=XMIN
        QCCLXR=XMAX
        QCCLYB=YMIN
        QCCLYT=YMAX
        CALL GKTOLD
      ENDIF
      GOTO 999

*   Report error
  990 CALL GKERR(KERROR)

*   Finish
  999 CONTINUE
      END
