C# IL>=a, OL>=0
      SUBROUTINE GQPA (IER, PWX, PWY, PHX, PHY )
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE CURRENT PRIMITIVE ATTRIBUTE VALUES
*                      PATTERN SIZE
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns the current primitive attribute value for pattern size.
*
*  MAINTENANCE LOG
*  ---------------
*     29/09/83  AS    Original version stabilized
*     20/01/87  AA    IS conversion. Extra arguments - width and height
*                     changed to vectors.
*
*  ARGUMENTS
*  ---------
*     OUT  IER    Error indicator
*     OUT   PWX,PWY Pattern width vector
*     OUT   PHX,PHY Pattern height vector
*
      INTEGER IER
      REAL PWX,PWY,PHX,PHY
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
        PWX = QCPAWX
        PWY = QCPAWY
        PHX = QCPAHX
        PHY = QCPAHY
      ENDIF

      END
