C# IL>=a, OL>=0
      SUBROUTINE GQFACI (IER,INDEX)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE CURRENT INDIVIDUAL ATTRIBUTE VALUES
*                      FILL AREA COLOUR INDEX
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns the Current Individual Attribute Value for fill area
*     colour index.
*
*  MAINTENANCE LOG
*  ---------------
*     29/09/83  AS   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     OUT   IER    Error indicator
*     OUT   INDEX  Fill area colour index
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

      CALL GKPRLG(KNIL, GGKOP, GSGOP)
      IER = KERROR
      IF (IER.EQ.0) INDEX = KCFACI

      END
