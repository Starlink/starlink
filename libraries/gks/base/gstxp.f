C# IL>=a, OL>=0
      SUBROUTINE GSTXP(ITXP)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET TEXT PATH
*  Author:             DCS/FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     set text path
*
*  MAINTENANCE LOG
*  ---------------
*     02/03/83    FY  Original version stabilized
*     19/01/87  ARG   IS conversion. Error number changed.
*     20/01/87  DCS   IS conversion. Remove test on value of KSTXWK
*                     since now only two possible values.
*
*  ARGUMENTS
*  ---------
*     INP   ITXP   text path
*
      INTEGER ITXP
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/   KCCHP,KSTXWK
*     Read   /gkysl/   khange,kgksfn..(par)
*     Read   /gks/    right,down ..(par)
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*     2000 Text path invalid
*
*---------------------------------------------------------------------



      CALL GKPRLG (ESTXP,GGKOP,GSGOP)
      IF (KERROR .EQ. 0) THEN
        IF ((ITXP .LT. GRIGHT) .OR. (ITXP .GT. GDOWN)) THEN
          CALL GKERR(2000)
        ELSE
          KCCHP = ITXP
          KSTXWK = KHANGE
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF
      END
