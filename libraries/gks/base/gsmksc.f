C# IL>=a, OL>=0
      SUBROUTINE GSMKSC(RMSZSF)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET MARKER SIZE SCALE FACTOR
*  Author:             DCS/FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     set marker size scale factor
*
*  MAINTENANCE LOG
*  ---------------
*     02/03/83    FY  Original version stabilized
*     19/01/87  ARG   IS conversion. New error detected.
*     20/01/87  DCS   IS conversion. Remove test on value of KSPMWK
*                     since now only two possible values.
*
*  ARGUMENTS
*  ---------
*     INP   RMSZSF marker size scale factor
*
      REAL RMSZSF
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/   QCMKSZ,KSPMWK
*     Read   /GKYSL/   KCPMAF
*     Read   /gkysl/   khange,kgksfn..(par)
*     Read   /aspct/  kmksza..(par)
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
*     71     marker size scale factor is less than zero
*
*---------------------------------------------------------------------



      CALL GKPRLG (ESMKSC,GGKOP,GSGOP)
      IF (KERROR .EQ. 0) THEN
      IF (RMSZSF .LT. 0.0) THEN
          CALL GKERR(71)
        ELSE
          QCMKSZ = RMSZSF
          IF (KCPMAF(KMKSZA) .EQ. GINDIV) KSPMWK = KHANGE
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF
      END
