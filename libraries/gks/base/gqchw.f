
      SUBROUTINE GQCHW ( IER, RCHW )
*
* (C) COPYRIGHT ICL # SERC  1987
*
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE CURRENT INDIVIDUAL ATTRIBUTE VALUES
*                      CHARACTER WIDTH
*  Author:             CJC,AA
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns the Current Individual Attribute Value for character width
*     - a new state list entry in IS GKS
*
* MAINTENANCE LOG
*  ---------------
*     20/01/87  CJC   IS conversion. New routine
*
*  ARGUMENTS
*  ---------
*     OUT   IER    Error indicator
*     OUT   RCHW   Current character width
*
      INTEGER IER
      REAL RCHW
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
      CALL GKPRLG (KNIL,GGKOP,GSGOP)
      IER = KERROR
      IF (IER.EQ.0) RCHW = QCCHW
      END
