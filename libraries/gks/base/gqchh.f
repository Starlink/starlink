C# IL>=a, OL>=0
      SUBROUTINE GQCHH(IER,RCHH)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE CURRENT PRIMITIVE ATTRIBUTE VALUES
*                      CHARACTER HEIGHT
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns the current primitive attribute value for character height
*
*  MAINTENANCE LOG
*  ---------------
*     28/09/83  AS   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     OUT  IER    Error indicator
*     OUT  RCHH   Current character height
*
      INTEGER IER
      REAL RCHH
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

      CALL GKPRLG(KNIL, GGKOP, GSGOP)
      IER = KERROR
      IF (IER.EQ.0) RCHH = QCCHH

      END
