C# IL>=a, OL>=0
      SUBROUTINE GSFASI(ISTYLI)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET FILL AREA STYLE INDEX
*  Author:             DCS/FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     set fill area style index
*
*  MAINTENANCE LOG
*  ---------------
*     02/03/83  FY    Original version stabilized
*     19/01/87  ARG   IS conversion. Error number changed.
*     20/01/87  DCS   IS conversion. Remove test on value of KSFAWK
*                     since now only two possible values.
*
*  ARGUMENTS
*  ---------
*     INP   ISTYLI fill area style index
*
      INTEGER ISTYLI
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/   KCFASI,KSFAWK
*     Read   /GKYSL/   KCFAAF
*     Read   /gkysl/   khange,kgksfn..(par)
*     Read   /aspct/  kfasia..(par)
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
*     84     style index equal to zero
*
*---------------------------------------------------------------------



      CALL GKPRLG (ESFASI,GGKOP,GSGOP)
      IF (KERROR .EQ. 0) THEN
        IF (ISTYLI .EQ. 0) THEN
          CALL GKERR(84)
        ELSE
          KCFASI = ISTYLI
          IF (KCFAAF(KFASIA) .EQ. GINDIV) KSFAWK = KHANGE
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF
      END
