C# IL>=a, OL>=0
      SUBROUTINE GKPKR(NUMBER, RVAL, IAT, CPACK)
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
*     Pack reals into a data record
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
*     INP   NUMBER Pack this NUMBER of reals
*     INP   RVAL   Array containing data to be packed
*     INP   IAT    Position within data record to pack reals
*     INOUT CPACK  Data Record
*
      REAL RVAL(1:*)
      INTEGER NUMBER, IAT
      CHARACTER * (*) CPACK(1:*)
*
* COMMON BLOCK USAGE
* ------------------
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
*     INTARR  Array to hold each part of the real
*     INTV    User to convert real to integer
*     REALV   User to convert real to integer
*     I       Loop index
*     J       Loop index
*
      REAL REALV
      INTEGER INTARR(1:KPDRSZ), I, J, INTV
*
      EQUIVALENCE (REALV, INTV)
*
      INTEGER IOFF, IND, IOFFST, INDX, IPOS, IDRSS
*
*------------------------------------------------------------------------
*
      INDX(IPOS)   = (IPOS-1) / IDRSS + 1
      IOFFST(IPOS) = MOD(IPOS-1,IDRSS) + 1
      IPOS = IAT
      IDRSS = LEN(CPACK(1))

      DO 3 J = 1, NUMBER

         REALV = RVAL(J)
         INTARR(1) = ABS(INTV)

         DO 1 I = 2, KPDRSZ
            INTARR(1) = INTARR(1) / KPDMXI
            INTARR(I) = MOD(INTARR(1), KPDMXI)
    1    CONTINUE

         INTARR(1) = MOD(ABS(INTV),KPDMXI)
         IF (INTV .LT. 0) INTARR(KPDRSZ) = INTARR(KPDRSZ) + 100
         DO 2 I = 1, KPDRSZ
            IND  = INDX(IPOS+I-1)
            IOFF = IOFFST(IPOS+I-1)
            CPACK(IND)(IOFF:IOFF) = GKAN1(INTARR(I))
    2    CONTINUE

         IPOS = IPOS + KPDRSZ

    3 CONTINUE
      END
