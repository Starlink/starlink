C# IL>=a, OL>=0
      SUBROUTINE GKPDMV(IFROM, ITO, NUMBER, CPACK)
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
*     Internal routine to "PD" Package - Move substring in data record
*
*  MAINTENANCE LOG
*  ---------------
*     16/08/83  CJW   Original version stabilized
*     21/04/86  RMK   Included contents of file GKPK.INC.
*     10/12/91  KEVP  Removed unused statement function (C91).
*
*  ARGUMENTS
*  ---------
*     INP   IFROM  Move FROM here
*     INP   ITO    TO here
*     INP   NUMBER move this NUMBER of characters
*     INOUT CPACK  Data Record
*
      INTEGER IFROM, ITO, NUMBER
      CHARACTER * (*) CPACK(1:*)
*
*  LOCALS
*  ------
*     JTO     Next character position to move TO
*     JFROM   Next character position to move FROM
*     ISTR    Loop index
*     INDT    Index (TO)
*     IOFT    Offset (TO)
*     INC     Increment to add to JFROM and JTO
*
      INTEGER JFROM, JTO, ISTR, INDT, IOFT, INC
      INTEGER IOFF, IND, IOFFST, INDX, IPOS, IDRSS
*
*-------------------------------------------------------------
*
      INDX(IPOS)   = (IPOS-1) / IDRSS + 1
      IOFFST(IPOS) = MOD(IPOS-1,IDRSS) + 1
      IDRSS = LEN(CPACK(1))
      JFROM = IFROM
      JTO = ITO

      IF (IFROM .GT. ITO) THEN
         INC = 1
      ELSE
*        Must go backwards!
         INC = -1
         JFROM = JFROM + NUMBER - 1
         JTO = JTO + NUMBER - 1
      END IF

*     Move each character (Heavy going 'cos its an array of strings)

      DO 1 ISTR = 1, NUMBER
         IND  = INDX(JFROM)
         IOFF = IOFFST(JFROM)
         INDT = INDX(JTO)
         IOFT = IOFFST(JTO)
         CPACK(INDT)(IOFT:IOFT) = CPACK(IND)(IOFF:IOFF)
         JFROM = JFROM + INC
         JTO = JTO + INC
    1 CONTINUE

      END
