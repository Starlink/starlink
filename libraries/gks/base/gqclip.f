C# IL>=a, OL>=0
      SUBROUTINE GQCLIP ( IER, ICLIN, CLRECT )
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  Inquire Clipping Indicator
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns the Clipping Indicator
*
*  MAINTENANCE LOG
*  ---------------
*     08/03/83  CJW   Original version stabilized
*     27/06/83  CJW   Implement revised error handling precedure
*     28/09/83  AS    Change subroutine name
*     20/01/87  AA    IS conversion. Additional argument - CLRECT.
*
*  ARGUMENTS
*  ---------
*     OUT   IER    Error indicator
*     OUT   ICLIN  Clipping Indicator
*     OUT   CLRECT Clipping rectangle
*
      INTEGER IER, ICLIN
*
      REAL  CLRECT(4)
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYERR/   Access error status
*     Read   /GKYSL/    KCLIN,QCCLXL,QCCLYB,QCCLXR,QCCLYT
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*---------------------------------------------------------------------


      CALL GKPRLG(KNIL,GGKOP,GSGOP)
      IER = KERROR

      IF (KERROR .EQ. 0) THEN
         ICLIN = KCLIN
         CLRECT(1) = QCCLXL
         CLRECT(2) = QCCLXR
         CLRECT(3) = QCCLYB
         CLRECT(4) = QCCLYT
      ENDIF
      IF (KERROR .EQ. 0) ICLIN = KCLIN

      END
