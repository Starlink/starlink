C# IL>=a, OL>=0
      SUBROUTINE GKUPR(IAT, CPACK, NUMBER, RVAL)
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
*     Unpack reals from a data record
*
*  MAINTENANCE LOG
*  ---------------
*     16/08/83  CJW   Original version stabilized
*     21/04/86  RMK   Included contents of file GKPK.INC.
*                     Changed use of Fortran ICHAR to GKS GKNA1 (S103).
*     10/12/91  KEVP  Removed unused variable & statement function (C91).
*
*  ARGUMENTS
*  ---------
*     INP   IAT    Position within data record to unpack reals
*     INP   CPACK  Data Record
*     INP   NUMBER Unpack this NUMBER of reals
*     OUT   RVAL   Array to receive the unpacked reals
*
      REAL RVAL(1:*)
      INTEGER NUMBER, IAT
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
      INTEGER GKNA1
*
*  LOCALS
*  ------
*     NEG     TRUE if the integer is negative
*     IFROM   Current Data Record Position
*     INTV    User to convert real to integer
*     REALV   User to convert real to integer
*     CVAL    Copy of KPDRSZ characters from the data record
*     I       Loop index
*     J       Loop index
*
      REAL REALV
      LOGICAL NEG
      INTEGER IFROM, I, J, INTV
      CHARACTER * (KPDRSZ) CVAL
*
      EQUIVALENCE (REALV, INTV)
*
      INTEGER IOFF, IND, IOFFST, INDX, IPOS, IDRSS
*
*------------------------------------------------------------------------
*
      INDX(IPOS)   = (IPOS-1) / IDRSS + 1
      IOFFST(IPOS) = MOD(IPOS-1,IDRSS) + 1
      IFROM = IAT
      IDRSS = LEN(CPACK(1))

      DO 3 J = 1, NUMBER

         DO 1 I = 1, KPDRSZ
            IND  = INDX(IFROM+I-1)
            IOFF = IOFFST(IFROM+I-1)
            CVAL(I:I) = CPACK(IND)(IOFF:IOFF)
    1    CONTINUE

         INTV = GKNA1(CVAL(KPDRSZ:KPDRSZ))
         NEG = (INTV .GE. 100)
         IF (NEG) INTV = INTV - 100
         DO 2 I = KPDRSZ - 1, 1, -1
            INTV = INTV * KPDMXI + GKNA1(CVAL(I:I))
    2    CONTINUE

         IF (NEG) INTV = -INTV
         RVAL(J) =  REALV

      IFROM = IFROM + KPDRSZ

    3 CONTINUE

      END
