C# IL>=a, OL>=0
      SUBROUTINE GQCNTN ( IER, INTN )
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  Inquire Current Transformation Number
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns the Current Transformation Number
*
*  MAINTENANCE LOG
*  ---------------
*     08/03/83  CJW  Original version stabilized
*     27/06/83  CJW  Implement revised error handling precedure
*
*  ARGUMENTS
*  ---------
*     OUT   IER    Error indicator
*     OUT   INTN   Current Transformation Number
*
      INTEGER IER, INTN
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYERR/   Access error status
*     Read   /GKYSL/    KCNTN
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*---------------------------------------------------------------------


      CALL GKPRLG(KNIL, GGKOP, GSGOP)
      IER = KERROR

      IF (KERROR .EQ. 0) INTN = KCNTN

      END
