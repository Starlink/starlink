C# IL>=a, OL>=0
      SUBROUTINE GQTXP (IER,IPATH)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE CURRENT PRIMITIVE ATTRIBUTE VALUES
*                      TEXT PATH
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns the current primitive attribute value for text path.
*
*  MAINTENANCE LOG
*  ---------------
*     29/09/83  AS   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     OUT  IER    Error indicator
*     OUT  IPATH  Text path
*
      INTEGER IER, IPATH
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
      IF (IER.EQ.0) IPATH = KCCHP

      END
