C# IL>=a, OL>=0
      SUBROUTINE GSPMCI(IPMCOL)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET POLYMARKER COLOUR INDEX
*  Author:             DCS/FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     set polymarker colour index
*
*  MAINTENANCE LOG
*  ---------------
*     02/03/83  FY    Original version stabilized
*     19/01/87  ARG   IS conversion. Error number changed.
*     20/01/87  DCS   IS conversion. Remove test on value of KSPMWK
*                     since now only two possible values.
*
*  ARGUMENTS
*  ---------
*     INP   IPMCOL polymarker colour index
*
      INTEGER IPMCOL
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/   KCPMCI,KSPMWK
*     Read   /GKYSL/   KCPMAF
*     Read   /gkysl/   khange,kgksfn..(par)
*     Read   /aspct/  kpmcia..(par)
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
*     92   Colour index is less than zero
*
*---------------------------------------------------------------------



      CALL GKPRLG (ESPMCI,GGKOP,GSGOP)
      IF (KERROR .EQ. 0) THEN
        IF (IPMCOL .LT. 0) THEN
          CALL GKERR(92)
        ELSE
          KCPMCI = IPMCOL
          IF (KCPMAF(KPMCIA) .EQ. GINDIV) KSPMWK = KHANGE
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF
      END
