C# IL>=a, OL>=0
      SUBROUTINE GKPKI(NUMBER, IVAL, IAT, CPACK)
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
*     Pack integers into a data record
*
*  MAINTENANCE LOG
*  ---------------
*     16/08/83  CJW   Original version stabilized
*     21/04/86  RMK   Included contents of file GKPK.INC. Renamed
*                     array INTS as INTARR to avoid confusion.
*                     Changed use of Fortran CHAR to GKS GKAN1 (S103).
*     10/12/91  KEVP  Removed unused statement function (C91).
*
*  ARGUMENTS
*  ---------
*     INP   NUMBER Pack this NUMBER of integers
*     INP   IVAL   Array containing data to be packed
*     INP   IAT    Position within data record to pack integers
*     INOUT CPACK  Data Record
*
      INTEGER IVAL(1:*), NUMBER, IAT
      CHARACTER * (*) CPACK(1:*)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkpid.par'
*
* EXTERNAL FUNCTION DEFINITION
* ----------------------------
*
      CHARACTER*1 GKAN1
*
*  LOCALS
*  ------
*     INTARR  Array to hold each part of the integer
*     I       Loop index
*     J       Loop index
*
      INTEGER INTARR(1:KPDISZ), I, J
      INTEGER IOFF, IND, IOFFST, INDX, IPOS, IDRSS
*
*------------------------------------------------------------------------
*
      INDX(IPOS)   = (IPOS-1) / IDRSS + 1
      IOFFST(IPOS) = MOD(IPOS-1,IDRSS) + 1
      IPOS = IAT
      IDRSS = LEN(CPACK(1))

      DO 3 J = 1, NUMBER

         INTARR(1) = ABS(IVAL(J))

         DO 1 I = 2, KPDISZ
            INTARR(1) = INTARR(1) / KPDMXI
            INTARR(I) = MOD(INTARR(1), KPDMXI)
    1    CONTINUE

         INTARR(1) = MOD(ABS(IVAL(J)),KPDMXI)
         IF (IVAL(J) .LT. 0) INTARR(KPDISZ) = INTARR(KPDISZ) + 100
         DO 2 I = 1, KPDISZ
            IND  = INDX(IPOS+I-1)
            IOFF = IOFFST(IPOS+I-1)
            CPACK(IND)(IOFF:IOFF) = GKAN1(INTARR(I))
    2    CONTINUE

         IPOS = IPOS + KPDISZ

    3 CONTINUE
      END
