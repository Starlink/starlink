C# IL>=a, OL>=0
      SUBROUTINE GQMNTN ( IER, MAXTNR )
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  Inquire Maximum Normalization Transformation Number
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  Returns the Maximum Normalization Transformation Number
*
*  MAINTENANCE LOG
*  ---------------
*     08/03/83  CJW   Original version stabilized
*     27/06/83  CJW   Implement revised error handling precedure
*     18/05/87  DCS   Correct valid states passed to GKPRLG (S266).
*
*  ARGUMENTS
*  ---------
*     OUT   IER    Error indicator
*     OUT   MAXTNR Maximum Normalization Transformation Number
*
      INTEGER IER, MAXTNR
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkerr.cmn'
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GGKOP,GSGOP)
      IER = KERROR
      IF (KERROR .EQ. 0) MAXTNR = KT

      END
