C# IL>=a, OL>=0
      SUBROUTINE GSTXAL(ITXAH,ITXAV)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET TEXT ALIGNMENT
*  Author:             DCS/FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     set text alignment
*
*  MAINTENANCE LOG
*  ---------------
*     02/03/83  FY    Original version stabilized
*     19/01/87  ARG   IS conversion. Error number changed.
*     20/01/87  DCS   IS conversion. Remove test on value of KSTXWK
*                     since now only two possible values.
*
*  ARGUMENTS
*  ---------
*     INP   ITXAH  horizontal alignment
*     INP   ITXAV  vertical alignment
*
      INTEGER ITXAH,ITXAV
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/   KCHZCH,KCVTCH,KSTXWK
*     Read   /gkysl/   khange,kgksfn..(par)
*     Read   /gkks/    anorml,aright,abottm..(par)
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*     2000 Invalid alignment
*
*---------------------------------------------------------------------



      CALL GKPRLG (ESTXAL,GGKOP,GSGOP)
      IF (KERROR .EQ. 0) THEN
        IF((ITXAH .LT. GAHNOR) .OR. (ITXAH .GT. GARITE) .OR.
     :     (ITXAV .LT. GAVNOR) .OR. (ITXAV .GT. GABOTT)) THEN
          CALL GKERR(2000)
        ELSE
          KCHZCH = ITXAH
          KCVTCH = ITXAV
          KSTXWK = KHANGE
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF
      END
