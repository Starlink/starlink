C# IL>=a, OL>=0
      SUBROUTINE GKUPC(IAT, CPACK, NUMBER, LENGTH, CVAL)
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
*     Unpack characters from a data record
*
*  MAINTENANCE LOG
*  ---------------
*     16/08/83  CJW   Original version stabilized
*     21/04/86  RMK   Included contents of file GKPK.INC.
*
*  ARGUMENTS
*  ---------
*     INP   IAT    Position within data record to unpack reals
*     INP   CPACK  Data Record
*     INP   NUMBER Unpack this NUMBER of reals
*     OUT   LENGTH Actual Length of longest string found
*     OUT   CVAL   Array to receive the unpacked characters
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
*     ICH     Loop index
*     ISTR    Loop index
*     ILEN    Length of longest string returned (might be truncated)
*
      INTEGER ICH, ISTR, ILEN
      INTEGER IOFF, IND, IOFFST, INDX, IPOSN, IPOS, IDRSS
*
*-------------------------------------------------------------
*
      INDX(IPOS)   = (IPOS-1) / IDRSS + 1
      IOFFST(IPOS) = MOD(IPOS-1,IDRSS) + 1
      IPOSN(IND, IOFF) = IND * IDRSS + IOFF
      IPOS = IAT
      IDRSS = LEN(CPACK(1))
      ILEN = MIN (LENGTH, LEN(CVAL(1)))

*     For each string do

      DO 2 ISTR = 1, NUMBER

         CVAL(ISTR) = ' '


*        Unpack characters

         DO 1 ICH = 1, ILEN
            IND  = INDX(IPOS)
            IOFF = IOFFST(IPOS)
            CVAL(ISTR)(ICH:ICH) = CPACK(IND)(IOFF:IOFF)
            IPOS = IPOS + KPDCSZ
    1    CONTINUE

*        Skip over any remaining characters

         IPOS = IPOS + (LENGTH - ILEN) * KPDCSZ

    2 CONTINUE

      END
