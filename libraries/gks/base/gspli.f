C# IL>=a, OL>=0
      SUBROUTINE GSPLI(IPLI)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET POLYLINE INDEX
*  Author:             DCS/FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     set polyline index
*
*  MAINTENANCE LOG
*  ---------------
*     02/03/83    FY  Original version stabilized
*     20/01/87  DCS   IS conversion. Remove test on value of KSPLWK
*                     since now only two possible values.
*
*  ARGUMENTS
*  ---------
*     INP   IPLI   polyline index
*
      INTEGER IPLI
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/   KCPLI,KSPLWK
*     Read   /gkysl/   khange,kgksfn..(par)
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*     60     polyline index invalid
*
*---------------------------------------------------------------------



      CALL GKPRLG (ESPLI,GGKOP,GSGOP)
      IF (KERROR .EQ. 0) THEN
        IF (IPLI .LE. 0) THEN
          CALL GKERR(60)
        ELSE
          KCPLI = IPLI
          KSPLWK = KHANGE
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF
      END
