C# IL>=a, OL>=0
      SUBROUTINE GQPARF (IER,REFX,REFY)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE CURRENT PRIMITIVE ATTRIBUTE VALUES
*                      PATTERN REFERENCE POINT
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns the current primitive attribute value for pattern reference
*     point.
*
*  MAINTENANCE LOG
*  ---------------
*     29/09/83  AS   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     OUT  IER    Error indicator
*     OUT  REFX   Pattern reference point
*     OUT  REFY   Pattern reference point
*
      INTEGER IER
      REAL REFX, REFY
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gksl.cmn'
*
*---------------------------------------------------------------------

      CALL GKPRLG (KNIL, GGKOP, GSGOP)
      IER = KERROR
      IF (IER.EQ.0) THEN
        REFX = QCPAX
        REFY = QCPAY
      ENDIF

      END
