C# IL>=a, OL>=0
      SUBROUTINE GSFAI(IFAI)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET FILL AREA INDEX
*  Author:             DCS/FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     set fill area index
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
*     INP   IFAI   fill area index
*
      INTEGER IFAI
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/   KCFAI,KSFAWK
*     Read   /gkysl/   khange,kgksfn...(par)
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
*     80     fill area index invalid
*
*---------------------------------------------------------------------



      CALL GKPRLG (ESFAI,GGKOP,GSGOP)
      IF (KERROR .EQ. 0) THEN
        IF (IFAI .LE. 0) THEN
          CALL GKERR(80)
        ELSE
          KCFAI = IFAI
          KSFAWK = KHANGE
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF
      END
