C# IL>=a, OL>=0
      SUBROUTINE GKUPI(IAT, CPACK, NUMBER, IVAL)
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
*     Unpack integers from a data record
*
*  MAINTENANCE LOG
*  ---------------
*     16/08/83  CJW   Original version stabilized
*     21/04/86  RMK   Included contents of file GKPK.INC.
*                     Changed use of Fortran ICHAR to GKS GKNA1 (S103).
*     10/12/91  KEVP  Removed unused statement function (C91).
*
*  ARGUMENTS
*  ---------
*     INP   IAT    Position within data record to unpack integers
*     INP   CPACK  Data Record
*     INP   NUMBER Unpack this NUMBER of integers
*     OUT   IVAL   Array to receive the unpacked integers
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
      INTEGER GKNA1
*
*  LOCALS
*  ------
*     NEG     TRUE if the integer is negative
*     IFROM   Current Data Record Position
*     CVAL    Copy of KPDISZ characters from the data record
*     I       Loop index
*     J       Loop index
*
      LOGICAL NEG
      INTEGER IFROM, I, J, IRES
      CHARACTER * (KPDISZ) CVAL
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

         DO 1 I = 1, KPDISZ
            IND  = INDX(IFROM+I-1)
            IOFF = IOFFST(IFROM+I-1)
            CVAL(I:I) = CPACK(IND)(IOFF:IOFF)
    1    CONTINUE

         IRES = GKNA1(CVAL(KPDISZ:KPDISZ))
         NEG = (IRES .GE. 100)
         IF (NEG) IRES = IRES - 100
         DO 2 I = KPDISZ - 1, 1, -1
            IRES = IRES * KPDMXI + GKNA1(CVAL(I:I))
    2    CONTINUE

         IF (NEG) IRES = -IRES
         IVAL(J) =  IRES

      IFROM = IFROM + KPDISZ

    3 CONTINUE

      END
