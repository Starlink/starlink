C# IL>=a, OL>=0
      SUBROUTINE GKUPSI(IAT, CPACK, NUMBER, IVAL)
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
*     Unpack short integers (0-16383) from a data record
*
*  MAINTENANCE LOG
*  ---------------
*     16/08/83  CJW   Original version stabilized
*     21/04/86  RMK   Included contents of file GKPK.INC.
*                     Changed use of Fortran ICHAR to GKS GKNA1 (S103).
*     26/06/86  RMK   Added temporary variable to get round problem when using
*                     an expression as the start point of a DO-loop with the
*                     Perkin-Elmer compiler (requested by PKY@Leicester).
*     10/12/91  KEVP  Removed unused statement function (C91).
*
*  ARGUMENTS
*  ---------
*     INP   IAT    Position within data record to unpack integers
*     INOUT CPACK  Data Record
*     INP   NUMBER Unpack this NUMBER of integers
*     INP   IVAL   Array to receive the unpacked integers
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
*     IFROM   Current Data Record Position
*     CVAL    Copy of KPDSSZ characters from the data record
*     I       Loop index
*     J       Loop index
*     ISTART  Holds KPDSSZ-1, needed by Perkin-Elmer compiler
*
      INTEGER IFROM, I, J, IRES, ISTART
      CHARACTER * (KPDSSZ) CVAL
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

         DO 1 I = 1, KPDSSZ
            IND  = INDX(IFROM+I-1)
            IOFF = IOFFST(IFROM+I-1)
            CVAL(I:I) = CPACK(IND)(IOFF:IOFF)
    1    CONTINUE

         IRES = GKNA1(CVAL(KPDSSZ:KPDSSZ))
         ISTART = KPDSSZ - 1
         DO 2 I = ISTART, 1, -1
            IRES = IRES * KPDMXI + GKNA1(CVAL(I:I))
    2    CONTINUE

         IVAL(J) =  IRES

      IFROM = IFROM + KPDSSZ

    3 CONTINUE

      END
