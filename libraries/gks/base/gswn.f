C# IL>=a, OL>=0
      SUBROUTINE GSWN(ITNR, XMIN,XMAX, YMIN,YMAX)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET WINDOW
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Set window of specified transformation.
*
*  MAINTENANCE LOG
*  ---------------
*     16/03/83  JRG   First version
*     030/3/83  JRG   CHECK.INC included
*     23/06/83  JRG   Changes to use KERROR for error reporting
*     08/07/83  JRG   Use GKRECT to check rectangle limits
*     28/09/83  AS    Change subroutine name
*     19/01/87  ARG   IS conversion. Report negative errors.
*
*  ARGUMENTS
*  ---------
*     INP   ITNR       Normalisation transformation number
*     INP   XMIN, XMAX Limits in X direction of window
*     INP   YMIN, YMAX Limits in Y direction of window
*
      INTEGER ITNR
      REAL XMIN,XMAX, YMIN,YMAX
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/ Window
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
*       51   Window limits in wrong order or too close
*
*---------------------------------------------------------------------

*   GKS Prologue
      CALL GKPRLG (ESWN,GGKOP,GSGOP)
      IF (KERROR .NE. 0) GOTO 990

*   Check Transformation Number (error 50): remember that 0 cannot
*   be altered
      IF( ITNR.LE.0 .OR. ITNR.GT.KT ) THEN
        KERROR=50
        GOTO 990
      ENDIF

*   Check that limits are correct (error 51)
      CALL GKRECT(XMIN,XMAX, YMIN,YMAX)
      IF( KERROR.NE.0 ) GOTO 990
*   End of error checking
*----------------------------


*   In GKS State List, update window for this transformation
      QLWXL(ITNR)=XMIN
      QLWXR(ITNR)=XMAX
      QLWYB(ITNR)=YMIN
      QLWYT(ITNR)=YMAX

*   If ITNR is the currently selected N.T., then set flags to
*   be out of date.
      IF( ITNR.EQ.KCNTN )  CALL GKTOLD
      GOTO 999

*   Report error
  990 CALL GKERR(KERROR)

*   Finish
  999 CONTINUE
      END
