      SUBROUTINE GQCHB ( IER, RCHBX, RCHBY )
*
* (C) COPYRIGHT ICL & SERC  1987
*
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE CURRENT INDIVIDUAL ATTRIBUTE VALUES
*                      CHARACTER BASE VECTOR
*  Author:             CJC,AA
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF ROUTINE
*  ------------------
*     Returns the Current Individual Attribute value for character base
*     vector - a new state list entry is IS GKS
*
*  MAINTENANCE LOG
*  ---------------
*     20/01/87  CJC   IS conversion. New routine
*
*  ARGUMENTS
*  ---------
*     OUT   IER    ERROR indicator
*     OUT   RCHBX,RCHBY Character base rector
*
      INTEGER IER
      REAL RCHBX, RCHBY
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
      IF (IER.EQ.0) THEN
        RCHBX = QCCHBX
        RCHBY = QCCHBY
      ENDIF
      END
