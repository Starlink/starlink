C# IL>=a, OL>=0
      SUBROUTINE GSPLCI(IPLCOL)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET POLYLINE COLOUR INDEX
*  Author:             DCS/FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     set polyline colour index
*
*  MAINTENANCE LOG
*  ---------------
*     02/03/83  FY    Original version stabilized
*     19/01/87  ARG   IS conversion. Error number changed.
*     20/01/87  DCS   IS conversion. Remove test on value KSPLWK
*                     since now only two possible values.
*
*  ARGUMENTS
*  ---------
*     INP   IPLCOL polyline colour index
*
      INTEGER IPLCOL
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/   KCPLCI,KSPLWK
*     Read   /GKYSL/   KCPLAF
*     Read   /gkysl/   khange,kgksfn..(par)
*     Read   /aspct/  kplcia
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
*     92   Colour index less than zero
*
*---------------------------------------------------------------------



      CALL GKPRLG (ESPLCI,GGKOP,GSGOP)
      IF (KERROR .EQ. 0) THEN
        IF (IPLCOL .LT. 0) THEN
          CALL GKERR(92)
        ELSE
          KCPLCI = IPLCOL
          IF (KCPLAF(KPLCIA) .EQ. GINDIV) KSPLWK = KHANGE
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF
      END
