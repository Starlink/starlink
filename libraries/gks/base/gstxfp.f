C# IL>=a, OL>=0
      SUBROUTINE GSTXFP(IFONT,IPREC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET TEXT FONT AND PRECISION
*  Author:             DCS/FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     set text font and precision
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
*     INP   IFONT  text font
*     INP   IPREC  text precision
*
      INTEGER IFONT,IPREC
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/   KCTXFN,KCTXPR,KSTXWK
*     Read   /GKYSL/   KCTXAF
*     Read   /gkysl/   khange,kgksfn..(par)
*     Read   /aspct/  ktxfna..(par)
*     Read   /gks/     indivi,strngp,strokp..(par)
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gaspct.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*     75   Text font is equal to 0
*     2000 Invalid precision
*
*---------------------------------------------------------------------



      CALL GKPRLG (ESTXFP,GGKOP,GSGOP)
      IF (KERROR .EQ. 0) THEN
        IF (IFONT .EQ. 0) THEN
          CALL GKERR(75)
        ELSEIF ((IPREC .LT. GSTRP) .OR. (IPREC .GT. GSTRKP)) THEN
          CALL GKERR(2000)
        ELSE
          KCTXFN = IFONT
          KCTXPR = IPREC
          IF (KCTXAF(KTXFNA) .EQ. GINDIV) KSTXWK = KHANGE
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF
      END
