C# IL>=a, OL>=0
      SUBROUTINE GSCLIP(ICLIN)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET CLIPPING INDICATOR
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Set Clipping Indicator
*
*  MAINTENANCE LOG
*  ---------------
*     08/07/83  JRG   First version
*     28/09/83  AS    Change subroutine name
*     20/01/87  ARG   IS conversion. Error number changed.
*
*  ARGUMENTS
*  ---------
*     Inp   ICLIN   Clipping indicator
*
      INTEGER ICLIN
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
*     2000  Enumeration type out of range
*
*---------------------------------------------------------------------

*   GKS Prologue
      CALL GKPRLG (ESCLIP,GGKOP,GSGOP)
      IF( KERROR.GT.0 ) GOTO 990

      IF( ICLIN.NE.GCLIP .AND. ICLIN.NE.GNCLIP ) THEN
          KERROR = 2000
          GOTO 990
      ENDIF
*   End of error checking
*----------------------------


*   If new clipping indicator is different from old one, make change
      IF( ICLIN.NE.KCLIN ) THEN
        KCLIN=ICLIN
        CALL GKTOLD
      ENDIF
      GOTO 999

*   Report error
  990 CALL GKERR(KERROR)

*   Finish
  999 CONTINUE
      END
