C# IL>=a, OL>=0
      SUBROUTINE GQCHUP(IER,RCHUX,RCHUY)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE CURRENT PRIMITIVE ATTRIBUTE VALUES
*                      CHARACTER UP VECTOR
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns the current primitive attribute value for character up vector
*
*  MAINTENANCE LOG
*  ---------------
*     29/09/83  AS   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     OUT  IER    Error indicator
*     OUT  RCHUX  Character up vector
*     OUT  RCHUY  Character up vector
*
      INTEGER IER
      REAL RCHUX, RCHUY
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
      IF (IER.EQ.0) THEN
        RCHUX = QCCHUX
        RCHUY = QCCHUY
      ENDIF

      END
