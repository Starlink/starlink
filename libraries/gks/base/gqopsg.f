C# IL>=a, OL>=1
      SUBROUTINE GQOPSG (IER,ISGNAM)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE NAME OF OPEN SEGMENT
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns the name of the open segment
*
*  MAINTENANCE LOG
*  ---------------
*     17/11/83  AS   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     OUT IER     Error indicator
*     OUT ISGNAM  Clipping Indicator
*
      INTEGER IER, ISGNAM
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*---------------------------------------------------------------------


      CALL GKPRLG(KNIL,GSGOP,GSGOP)
      IER = KERROR

      IF (KERROR .EQ. 0) ISGNAM = KOPSG

      END
