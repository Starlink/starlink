      SUBROUTINE GUREC(LDR, DATREC, IDL, IDRL, IDSL, IER, IL, IA, IRL,
     :                  RA, ISL, LSTR, STR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Front end
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Unpacks a data record.
*
*  MAINTENANCE LOG
*  ---------------
*     10/05/83  CJW  Original version stabilized
*     27/06/83  CJW  Implement revised error handling precedure
*                    (No change)
*     28/09/83  AS   Change subroutine name
*     21/04/86  RMK  Changed use of Fortran CHAR to GKS GKAN1 (S103).
*     23/01/87  CJC  IS conversion. Complete rewrite to IS spec.
*     23/03/87  RMK  Re-ordered declarations of IA and IDL (S247).
*     29/05/90  KEVP Changed error 2004 to -2004 - IS conversion (C2).
*
*  ARGUMENTS
*  ---------
*     INP   LDR    Number of array elements in DATREC
*     INP   DATREC Data record - declared internally *1 for convenience
*     INP   IDL    Dimension of integer array
*     INP   IDRL   Dimension of real array
*     INP   IDSL   Dimension of character array
*     OUT   IER    Error indicator
*     OUT   IL     Number of integer entries
*     OUT   IA     Array containing integer entries
*     OUT   IRL    Number of real entries
*     OUT   RA     Array containing real entries
*     OUT   ISL    Number of character string entries
*     OUT   LSTR   Length of each character string entry
*     OUT   STR    Array containing character string entries
*
      INTEGER IDL, IA(IDL), IDRL, IDSL, IER, IL, IRL, ISL, J,
     : LDR, LSTR(IDSL)
      REAL RA(IDRL)
      CHARACTER DATREC(80*LDR)*1, STR(IDSL)*(*)
*
*  LOCALS
*  ------
*     I, J   Loop indices
*     ICH    Character index
*     NCH    Number of integers required to store character string
*     REA    A real - equivalenced to INTG
*     INTG   An integer used for packing of reals
*
      REAL REA
      INTEGER I, ICH, INTG, NCH
      EQUIVALENCE (INTG, REA)
*
*  COMMENTS
*  --------
*     The routine makes assumptions about the sizes of variables. This
*     version converts a real into an integer by EQUIVALENCE. It also
*     assumes that integers 0-127 can be put in a character (see GKUREI)
*
*                       SYSTEM DEPENDENT
*
*---------------------------------------------------------------------



*     Calculate number of characters that may be unpacked
      NCH = LDR*80
      ICH = 1

*     Unpack the number of integers - check room - then unpack integers
      IF(NCH.LT.ICH+4)GOTO 93
      CALL GKUREI(DATREC(ICH),IL)
      ICH = ICH+5

      IF(IL.GT.IDL)GOTO 91
      IF(NCH.LT.ICH+MAX(0,IL)*5-1)GOTO 93
      DO 10 I = 1, IL
        CALL GKUREI(DATREC(ICH),IA(I))
        ICH = ICH+5
  10  CONTINUE
*
*     Unpack the number of reals - check room - unpack the reals
*     themselves using equivalence to transfer the real bit pattern from
*     an integer to a real (INTG to REA)
      IF(NCH.LT.ICH+4)GOTO 93
      CALL GKUREI(DATREC(ICH),IRL)
      ICH = ICH+5

      IF(IRL.GT.IDRL)GOTO 91
      IF(NCH.LT.ICH+MAX(0,IL)*5-1)GOTO 93
      DO 20 I = 1, IRL
        CALL GKUREI(DATREC(ICH),INTG)
        ICH = ICH+5
        RA(I) = REA
   20 CONTINUE
*
*     Unpack the number of strings followed by each string length and
*     the string
      IF(NCH.LT.ICH+4)GOTO 93
      CALL GKUREI(DATREC(ICH),ISL)
      ICH = ICH+5
      IF(ISL.GT.IDSL)GOTO 91
      DO 40 I = 1, ISL
        IF(NCH.LT.ICH+4)GOTO 93
        CALL GKUREI(DATREC(ICH),LSTR(I))
        ICH = ICH+5
        IF(LSTR(I).GT.LEN(STR(1)))GOTO 91
        IF(NCH.LT.ICH+LSTR(I)-1)GOTO 93
*       Copy the string - one character at a time so that there is
*       no problem on machines that only allow short strings.
        DO 30 J = 1, LSTR(I)
          STR(I)(J:J) = DATREC(ICH)
          ICH = ICH+1
   30   CONTINUE

   40 CONTINUE

      IER = 0
      GOTO 99

*     Output array too small
   91 IER = 2001
      GOTO 99

*     Invalid packed string
   93 IER = -2004

   99 RETURN
      END
