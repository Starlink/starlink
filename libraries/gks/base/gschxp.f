C# IL>=a, OL>=0
      SUBROUTINE GSCHXP(RCHXP)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET CHARACTER EXPANSION FACTOR
*  Author:             DCS/FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     set character expansion factor
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
*     INP   RCHXP  character expansion factor
*
      REAL RCHXP
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/   QCCHXP,KSTXWK
*     Read   /GKYSL/   KCTXAF
*     Read   /gkysl/   khange,kgksfn..(par)
*     Read   /aspct/  kchxpa..(par)
*     Read   /gks/     indivi..(par)
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gaspct.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*     77     ch. exp. factor less than or equal to zero
*
*---------------------------------------------------------------------



      CALL GKPRLG (ESCHXP,GGKOP,GSGOP)
      IF (KERROR .EQ. 0) THEN
        IF (RCHXP .LE. 0.0) THEN
          CALL GKERR(77)
        ELSE
          QCCHXP = RCHXP
          IF (KCTXAF(KCHXPA) .EQ. GINDIV) KSTXWK = KHANGE
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF
      END
