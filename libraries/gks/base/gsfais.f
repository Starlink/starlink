C# IL>=a, OL>=0
      SUBROUTINE GSFAIS(INTS)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET FILL AREA INTERIOR STYLE
*  Author:             DCS/FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     set fill area interior style
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
*     INP   INTS   fill area interior style
*
      INTEGER INTS
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/   KCFAIS,KSFAWK
*     Read   /GKYSL/   KCFAAF
*     Read   /gkysl/   khange,kgksfn..(par)
*     Read   /aspct/  kfaisa..(par)
*     Read   /gks/     indivi,hollow,hatch..(par)
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
*     2000   invalid argument
*
*---------------------------------------------------------------------



      CALL GKPRLG (ESFAIS,GGKOP,GSGOP)
      IF (KERROR .EQ. 0) THEN
        IF ((INTS .LT. GHOLLO) .OR. (INTS .GT. GHATCH)) THEN
          CALL GKERR(2000)
        ELSE
          KCFAIS = INTS
          IF (KCFAAF(KFAISA) .EQ. GINDIV) KSFAWK = KHANGE
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF
      END
