      SUBROUTINE GPREC(IL, IA, IRL, RA, ISL, LSTR, STR, MLDR, IER, LDR,
     :                DATREC)
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
*   Packs a data record.
*
*  MAINTENANCE LOG
*  ---------------
*     10/05/83  CJW  Original version stabilized
*     27/06/83  CJW  Implement revised error handling precedure
*                    (No change required)
*     28/09/83  AS   Change subroutine name
*     21/04/86  RMK  Changed use of Fortran ICHAR to GKS GKNA1 (S103).
*     23/01/87  CJC  IS conversion. Complete rewrite to IS spec.
*     10/03/87  CJC  Stabilisation of new version.
*     23/03/87  RMK  Re-ordered declarations of IA and IL (S247).
*     07/05/92  NMH  Changed declarations of input arrays to have
*                    adjustable dimensions rather than the fixed lengths
*                    passed in (S411/C99).
*
*  ARGUMENTS
*  ---------
*     INP   IL     Number of integer entries
*     INP   IA     Array of integer entries
*     INP   IRL    Number of real entries
*     INP   RA     Array of real entries
*     INP   ISL    Number of character string entries
*     INP   LSTR   Array of lengths - one for each character string
*                  entry
*     INP   STR    Array of character string entries
*     INP   MLDR   Dimension of data record array
*     OUT   IER    Error indicator
*     OUT   LDR    Number of array elements used in DATREC
*     OUT   DATREC Data record - declared internally *1 for convenience
*
      INTEGER IL, IA(*), IRL, ISL, LSTR(*), MLDR, IER, LDR
      REAL RA(*)
      CHARACTER STR(*)*(*), DATREC(80*MLDR)*1
*
*  LOCALS
*  ------
*     I, J   Loop indices
*     ICH    Character index
*     NCH    Number of characters available
*     REA    A real - equivalenced to INTG
*     INTG   An integer used for packing of reals
*
      REAL REA
      INTEGER I, ICH, INTG, J, NCH
      EQUIVALENCE (INTG, REA)
*
*  COMMENTS
*  --------
*     The routine makes assumptions about the sizes of variables. This
*     version converts a real into an integer by EQUIVALENCE. It also
*     assumes that integers 0-127 can be put in a character (see GKPREI)
*
*                       SYSTEM DEPENDENT
*
*---------------------------------------------------------------------


*     Number of characters available
      NCH = MLDR*80
      ICH = 1
*
*     Count of integers followed by the integers themselves

      IF(NCH.LT.ICH+4+MAX(0,IL)*5)GOTO 90
      CALL GKPREI(IL,DATREC(ICH))
      ICH = ICH+5
      DO 10 I = 1, IL
        CALL GKPREI(IA(I),DATREC(ICH))
        ICH = ICH+5
   10 CONTINUE
*
*     Count of reals followed by reals themselves.
*     NOTE: use of equivalence between integers and reals - messy

      IF(NCH.LT.ICH+4+MAX(0,IRL)*5)GOTO 90
      CALL GKPREI(IRL,DATREC(ICH))
      ICH = ICH+5
      DO 20 I = 1, IRL
        REA = RA(I)
        CALL GKPREI(INTG,DATREC(ICH))
        ICH = ICH+5
   20 CONTINUE
*
*     Count of character strings followed by length of each string and
*     the string itself

      IF(NCH.LT.ICH+4)GOTO 90
      CALL GKPREI(ISL,DATREC(ICH))
      ICH = ICH+5

      DO 40 I = 1, ISL
        IF(NCH.LT.ICH+4+MAX(0,LSTR(I)))GOTO 90
        CALL GKPREI(LSTR(I),DATREC(ICH))
        ICH = ICH+5

        DO 30 J = 1, LSTR(I)
          DATREC(ICH) = STR(I)(J:J)
          ICH = ICH+1
   30   CONTINUE

   40 CONTINUE
*
*     Calculate the number of 80 character array elements used - ICH
*     points to an unused element so ICH=81 maps onto LDR=1

      LDR = (ICH+78)/80
      IER = 0
      GOTO 99

*     Too little room in the data record for the data

   90 IER = 2001

   99 RETURN
      END
