C# IL>=a, OL>=0
      SUBROUTINE GKPKC(NUMBER, LENGTH, CVAL, IAT, CPACK)
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
*     Pack characters into a data record
*
*  MAINTENANCE LOG
*  ---------------
*     16/08/83  CJW   Original version stabilized
*     21/04/86  RMK   Included contents of file GKPK.INC.
*     10/12/91  KEVP  Removed unused statement function (C91).
*
*  ARGUMENTS
*  ---------
*     INP   NUMBER Pack this NUMBER of characters
*     INP   LENGTH Length of longest string
*     INP   CVAL   Array containing data to be packed
*     INP   IAT    Position within data record to pack characters
*     INOUT CPACK  Data Record
*
      INTEGER NUMBER, IAT, LENGTH
      CHARACTER * (*) CPACK(1:*)
      CHARACTER * (*) CVAL(1:*)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkpid.par'
*
*  LOCALS
*  ------
*     ISTR    Loop index
*     ICH     Loop index
*
      INTEGER ISTR, ICH
      INTEGER IOFF, IND, IOFFST, INDX, IPOS, IDRSS
*
*-------------------------------------------------------------
*
      INDX(IPOS)   = (IPOS-1) / IDRSS + 1
      IOFFST(IPOS) = MOD(IPOS-1,IDRSS) + 1
      IPOS = IAT
      IDRSS = LEN(CPACK(1))


*     For each string do

      DO 2 ISTR = 1, NUMBER

*        Pack characters

         DO 1 ICH = 1, LENGTH
            IND  = INDX(IPOS)
            IOFF = IOFFST(IPOS)
            CPACK(IND)(IOFF:IOFF) = CVAL(ISTR)(ICH:ICH)
            IPOS = IPOS + KPDCSZ
    1    CONTINUE

    2 CONTINUE

      END
