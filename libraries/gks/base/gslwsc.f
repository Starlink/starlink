C# IL>=a, OL>=0
      SUBROUTINE GSLWSC(RLWSC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET LINEWIDTH SCALE FACTOR
*  Author:             DCS/FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     set linewidth scale factor
*
*  MAINTENANCE LOG
*  ---------------
*     02/03/83  FY    Original version stabilized
*     19/01/87  ARG   IS conversion. New error detected.
*     20/01/87  DCS   IS conversion. Remove test on value of KSPLWK
*                     since now only two possible values.
*
*  ARGUMENTS
*  ---------
*     INP   RLWSC  linewidth scale factor
*
      REAL RLWSC
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/   QCLNWD,KSPLWK
*     Read   /GKYSL/   KCPLAF
*     Read   /gkysl/   khange,kgksfn..(par)
*     Read   /aspct/  klnwda
*     Read   /gks/     indivi
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
*     65     linewidth scale factor is less than zero
*
*---------------------------------------------------------------------



      CALL GKPRLG (ESLWSC,GGKOP,GSGOP)
      IF (KERROR .EQ. 0) THEN
        IF (RLWSC .LT. 0.0) THEN
          CALL GKERR(65)
        ELSE
          QCLNWD = RLWSC
          IF (KCPLAF(KLNWDA) .EQ. GINDIV) KSPLWK = KHANGE
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF
      END
