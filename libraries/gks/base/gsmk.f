C# IL>=a, OL>=0
      SUBROUTINE GSMK(MTYPE)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET MARKER TYPE
*  Author:             DCS/FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     set marker type
*
*  MAINTENANCE LOG
*  ---------------
*     02/03/83    FY  Original version stabilized
*     19/01/87  ARG   IS conversion. Error number changed.
*     20/01/87  DCS   IS conversion. Remove test on value of KSPMWK
*                     since now only two possible values.
*
*  ARGUMENTS
*  ---------
*     INP   MTYPE  marker type
*
      INTEGER MTYPE
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/   KCMKTY,KSPMWK
*     Read   /GKYSL/   KCPMAF
*     Read   /gkysl/   khange,kgksfn..(par)
*     Read   /aspct/  kmktya..(par)
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
*     69     marker type is equal to zero
*
*---------------------------------------------------------------------



      CALL GKPRLG (ESMK,GGKOP,GSGOP)
      IF (KERROR .EQ. 0) THEN
        IF (MTYPE .EQ. 0) THEN
          CALL GKERR(69)
        ELSE
          KCMKTY = MTYPE
          IF (KCPMAF(KMKTYA) .EQ. GINDIV) KSPMWK = KHANGE
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF
      END
