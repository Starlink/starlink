C# IL>=a, OL>=0
      SUBROUTINE GSPMI(IPMI)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET POLYMARKER INDEX
*  Author:             DCS/FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     set polymarker index
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
*     INP   IPMI   polymarker index
*
      INTEGER IPMI
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/   KCPMI,KSPMWK
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
*     66   Polymarker index invalid
*
*---------------------------------------------------------------------



      CALL GKPRLG (ESPMI,GGKOP,GSGOP)
      IF (KERROR .EQ. 0) THEN
        IF (IPMI .LE. 0) THEN
          CALL GKERR(66)
        ELSE
          KCPMI = IPMI
          KSPMWK = KHANGE
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF
      END
