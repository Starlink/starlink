C# IL>=a, OL>=0
      SUBROUTINE GQLVKS ( IER, LEVEL )
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  Inquire Level of GKS
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  Returns the level of GKS
*
*  MAINTENANCE LOG
*  ---------------
*     01/11/82  CJW  Original version stabilized
*     10/02/83  CJW  Insert call to prologue
*     27/06/83  CJW  Implement revised error handling precedure
*     22/12/83  AS   Change second argument to PRLG
*
*  ARGUMENTS
*  ---------
*     OUT   IER    Error indicator
*     OUT   LEVEL  Operating State Value
*
      INTEGER IER, LEVEL
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYERR/   Access error status
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkerr.cmn'
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GGKOP,GSGOP)
      IER = KERROR

      IF (KERROR .EQ. 0) LEVEL = KLVKS

      END
