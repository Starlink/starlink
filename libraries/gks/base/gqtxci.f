C# IL>=a, OL>=0
      SUBROUTINE GQTXCI (IER,INDEX)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE CURRENT INDIVIDUAL ATTRIBUTE VALUES
*                      TEXT COLOUR INDEX
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns the Current Individual Attribute Value for text colour
*     index.
*
*  MAINTENANCE LOG
*  ---------------
*     29/09/83  AS   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     OUT   IER    Error indicator
*     OUT   INDEX  Text colour index
*
      INTEGER IER, INDEX
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
      IF (IER.EQ.0) INDEX = KCTXCI

      END
