C# IL>=a, OL>=0
      SUBROUTINE GSFACI(IFACOL)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET FILL AREA COLOUR INDEX
*  Author:             DCS/FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     set fill area colour index
*
*  MAINTENANCE LOG
*  ---------------
*     02/03/83    FY  Original version stabilized
*     19/01/87  ARG   IS conversion. Error number changed.
*     20/01/87  DCS   IS conversion. Remove test on value of KSFAWK
*                     since now only two possible values.
*
*  ARGUMENTS
*  ---------
*     INP   IFACOL fill area colour index
*
      INTEGER IFACOL
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/   KCFACI,KSFAWK
*     Read   /GKYSL/   KCFAAF
*     Read   /gkysl/   khange,kgksfn..(par)
*     Read   /aspct/  kfacia..(par)
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
*     92     colour index less than zero
*
*---------------------------------------------------------------------



      CALL GKPRLG (ESFACI,GGKOP,GSGOP)
      IF (KERROR .EQ. 0) THEN
        IF (IFACOL .LT. 0) THEN
          CALL GKERR(92)
        ELSE
          KCFACI = IFACOL
          IF (KCFAAF(KFACIA) .EQ. GINDIV) KSFAWK = KHANGE
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF
      END
