C# IL>=a, OL>=0
      SUBROUTINE GSCHSP(RCHSP)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET CHARACTER SPACING
*  Author:             DCS/FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     set character spacing
*
*  MAINTENANCE LOG
*  ---------------
*     02/03/83    FY  Original version stabilized
*     20/01/87  DCS   IS conversion. Remove test on value of KSTXWK
*                     since now only two possible values.
*
*  ARGUMENTS
*  ---------
*     INP   RCHSP  character spacing
*
      REAL RCHSP
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/   QCCHSP,KSTXWK
*     Read   /GKYSL/   KCTXAF
*     Read   /gkysl/   khange,kgksfn..(par)
*     Read   /aspct/  kchspa..(par)
*     Read   /gks/     indivi..(par)
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gaspct.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*---------------------------------------------------------------------



      CALL GKPRLG (ESCHSP,GGKOP,GSGOP)
      IF (KERROR .EQ. 0) THEN
        QCCHSP = RCHSP
        IF (KCTXAF(KCHSPA) .EQ. GINDIV) KSTXWK = KHANGE
      ELSE
        CALL GKERR(KERROR)
      ENDIF
      END
