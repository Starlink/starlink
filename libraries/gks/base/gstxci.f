C# IL>=a, OL>=0
      SUBROUTINE GSTXCI(ITXCOL)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET TEXT COLOUR INDEX
*  Author:             DCS/FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     set text colour index
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
*     INP   ITXCOL text colour index
*
      INTEGER ITXCOL
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/   KCTXCI,KSTXWK
*     Read   /GKYSL/   KCTXAF
*     Read   /gkysl/   khange,kgksfn..(par)
*     Read   /aspct/  ktxcia..(par)
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
*  ERRORS
*  ------
*     92   Colour index is less than 0
*
*---------------------------------------------------------------------



      CALL GKPRLG (ESTXCI,GGKOP,GSGOP)
      IF (KERROR .EQ. 0) THEN
        IF (ITXCOL .LT. 0) THEN
          CALL GKERR(92)
        ELSE
          KCTXCI = ITXCOL
          IF (KCTXAF(KTXCIA) .EQ. GINDIV) KSTXWK = KHANGE
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF
      END
