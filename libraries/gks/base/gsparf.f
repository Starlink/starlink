C# IL>=a, OL>=0
      SUBROUTINE GSPARF(RFX,RFY)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET PATTERN REFERENCE POINT
*  Author:             DCS/FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     set pattern reference point
*
*  MAINTENANCE LOG
*  ---------------
*     02/03/83    FY  Original version stabilized
*     20/01/87  DCS   IS conversion. Remove test on value of KSFAWK
*                     since now only two possible values.
*
*  ARGUMENTS
*  ---------
*     INP   RFX    x component of reference point
*     INP   RFY    y component of reference point
*
      REAL RFX,RFY
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/   QCPAX,QCPAY,KSFAWK
*     Read   /gkysl/   khange,kgksfn..(par)
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*---------------------------------------------------------------------



      CALL GKPRLG (ESPARF,GGKOP,GSGOP)
      IF (KERROR .EQ. 0) THEN
        QCPAX = RFX
        QCPAY = RFY
        KSFAWK = KHANGE
      ELSE
        CALL GKERR(KERROR)
      ENDIF
      END
