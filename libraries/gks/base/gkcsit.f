C# IL>=a, OL>=0
      SUBROUTINE GKCSIT(IENT, NID, NRD, NCD, NI, NR)
*
* (C) COPYRIGHT ICL & SERC  1984
*
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Make a new item in the current open CSS segment
*
*  MAINTENANCE LOG
*  ---------------
*     17/11/83  CJW   Original version stabilized
*     12/03/85  MGC   Entry contents now supplied in KCSWWK,QCSWWK
*
*  ARGUMENTS
*  ---------
*     INP IENT   Entrypoint code
*     INP NID    Number of integer arguments
*     INP NRD    Number of coordinate pairs
*     INP NCD    Length of string argument
*     INP NI     Number of WCA integers
*     INP NR     Number of WCA reals
*
      INTEGER IENT, NID, NRD, NCD, NI, NR
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkcss.par'
      INCLUDE '../include/gkcss.cmn'
*
*  LOCALS
*  ------
*
*
*  ERRORS
*  ------
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------


*     put entry and counts into segment
      KCSBUF(1) = IENT
      KCSBUF(2) = NID
      KCSBUF(3) = NRD
      KCSBUF(4) = NCD
      KCSBUF(5) = NI
      KCSBUF(6) = NR
      CALL GKCSAI(6, KCSBUF)
      CALL GKCSAI(NI, KCSWWK)
      CALL GKCSAR(NR, QCSWWK)
      END
