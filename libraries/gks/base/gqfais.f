C# IL>=a, OL>=0
      SUBROUTINE GQFAIS (IER,INTSTY)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE CURRENT INDIVIDUAL ATTRIBUTE VALUES
*                      FILL AREA INTERIOR STYLE
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns the Current Individual Attribute Value for fill area interior
*     style.
*
*  MAINTENANCE LOG
*  ---------------
*     29/09/83  AS   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     OUT   IER    Error indicator
*     OUT   INTSTY Fill area interior style
*
      INTEGER IER, INTSTY
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
      IF (IER.EQ.0) INTSTY = KCFAIS

      END
