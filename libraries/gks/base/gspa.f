C# IL>=a, OL>=0
      SUBROUTINE GSPA(RSZX,RSZY)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET PATTERN SIZE
*  Author:             DCS/FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     set pattern size
*
*  MAINTENANCE LOG
*  ---------------
*     02/03/83  FY    Original version stabilized
*     19/01/87  ARG   IS conversion. Error number changed.
*     20/01/87  DCS   IS conversion. Remove test on value of KSFAWK
*                     since now only two possible values.
*                     Set pattern vectors (changed GKS State List
*                     entries).
*     30/05/90  KEVP  Changed occurrences of '0' in the comparison
*                     with RSZX and RSZY to '0.0' (S236).
*
*  ARGUMENTS
*  ---------
*     INP   RSZX   x component of pattern size
*     INP   RSZY   y component of pattern size
*
      REAL RSZX,RSZY
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/   QCPAWX,QCPAWY,QCPAHX,QCPAHY,KSFAWK
*     Read   /GKYSL/   khange,kgksfn..(par)
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
*     87     pattern size is not positive
*
*---------------------------------------------------------------------



      CALL GKPRLG (ESPA,GGKOP,GSGOP)
      IF (KERROR .EQ. 0) THEN
        IF ((RSZX .LE. 0.0) .OR. (RSZY .LE. 0.0)) THEN
          CALL GKERR(87)
        ELSE
          QCPAWX = RSZX
          QCPAWY = 0.0
          QCPAHX = 0.0
          QCPAHY = RSZY
          KSFAWK = KHANGE
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF
      END
