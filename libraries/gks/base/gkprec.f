      SUBROUTINE GKPREC(IL, IA, IRL, RA, ISL, IST, MLDR, LDR, DATREC)
*
*  COPYRIGHT (C) SERC 1987
*
*-----------------------------------------------------------------------
*
*  RAL GKS SYSTEM
*
*  Type of routine:  DATA RECORD UTILITY
*  Author:           PJWR
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Packs a data record from internal data.
*
*  MAINTENANCE LOG
*  ---------------
*     10/08/87  PJWR  Created,  based on GPREC by CJW.
*     12/08/87  PJWR  Corrected declaration error for IST and modified
*                     references to IST to reflect correction.
*     14/08/87  PJWR  Corrected usage of heap.
*     30/09/87  PJWR  Removed direct heap access to obviate problems
*                     like those suffered by GKUREC.
*     17/12/87  RMK   Changed declarations of input arrays to have
*                     adjustable dimensions rather than the fixed lengths
*                     passed in (S300).
*
*  ARGUMENTS
*  ---------
*     INP   IL     Number of integer data.
*     INP   IA     Array of integer data.
*     INP   IRL    Number of real data.
*     INP   RA     Array of real data.
*     INP   ISL    Number of character string data.
*     INP   IST    String table (see COMMENTS).
*     INP   MLDR   Number of CHARACTER*(80) elements in DATREC.
*     OUT   LDR    Number of CHARACTER*(80) elements used in DATREC.
*     OUT   DATREC Data record.  This is a CHARACTER*(80)(MLDR),  but is
*                  redeclared as CHARACTER*(1)(80*MLDR) for ease of use.
*
      INTEGER IL, IA(*), IRL, ISL, IST(*), MLDR, LDR
      REAL RA(*)
      CHARACTER DATREC(80*MLDR)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYERR/ KERROR
*            /GKYSTK/ KSTACK
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkstk.cmn'
*
*  EXTERNAL FUNCTIONS
*  ------------------
*     GKAN1   Converts an ASCII integer into a native character.
*
      CHARACTER GKAN1
*
*  LOCALS
*  ------
*     I      Loop index
*     J      Loop index
*     ICH    Character index
*     NCH    Number of characters available
*     RTEMP  A temporary real, equivalenced to ITEMP
*     ITEMP  A temporary integer,  used for packing reals.
*     IOFFI  Stack offset for workspace to extract strings from heap.
*
      INTEGER I, J, NCH, ICH, ITEMP, IOFFI
      REAL RTEMP
      EQUIVALENCE (ITEMP, RTEMP)
*
*  ERRORS
*  ------
*     2001   Insufficient room in data record
*
*  COMMENTS
*  --------
*     The main difference between this routine and GPREC is that strings
*     are stored  internally on the integer heap as ASCII  integers  and
*     passed down as  a  table  of  [string length, integer heap offset]
*     pairs.  As this routine uses GKPREI,  which makes some assumptions
*     about how an integer or real can be packed into characters,  it's:
*
*                           SYSTEM DEPENDENT
*
*-----------------------------------------------------------------------

*     Calculate the number of characters available and initialise the
*     next character to use.
      NCH = MLDR*80
      ICH = 1

*     Count of integers followed by the integers themselves
      IF(NCH.LT.ICH+4+MAX(0,IL)*5)GOTO 99
      CALL GKPREI(IL,DATREC(ICH))
      ICH = ICH+5
      DO 10 I = 1, IL
        CALL GKPREI(IA(I),DATREC(ICH))
        ICH = ICH+5
   10 CONTINUE

*     Count of reals followed by reals themselves.
      IF(NCH.LT.ICH+4+MAX(0,IRL)*5)GOTO 99
      CALL GKPREI(IRL,DATREC(ICH))
      ICH = ICH+5
      DO 20 I = 1, IRL
        RTEMP = RA(I)
        CALL GKPREI(ITEMP,DATREC(ICH))
        ICH = ICH+5
   20 CONTINUE

*     Count of character strings followed by length of each string and
*     the string itself
      IF(NCH.LT.ICH+4)GOTO 99
      CALL GKPREI(ISL,DATREC(ICH))
      ICH = ICH+5
      DO 40 I = 1, ISL*2, 2
        IF(NCH.LT.ICH+4+MAX(0,IST(I)))GOTO 99
        CALL GKPREI(IST(I),DATREC(ICH))
        ICH = ICH+5
*       Allocate stack space for heap entry and copy string to stack.
      CALL GKSTAL(KINTGS,IST(I),IOFFI)
      IF (KERROR.NE.0) GO TO 90
      CALL GKHPGI(IST(I+1),0,IST(I),KSTACK(IOFFI))
      IF (KERROR.NE.0) GO TO 90
*       Pack string from stack
      DO 30 J = 0, IST(I) - 1
        DATREC(ICH) = GKAN1(KSTACK(IOFFI+J))
        ICH = ICH+1
   30   CONTINUE
*       Deallocate stack space.
      CALL GKSTDA(KINTGS,IOFFI)
      IF (KERROR.NE.0) GO TO 90
   40 CONTINUE

*     Calculate the number of 80 character array elements used - ICH
*     points to an unused element so ICH=81 maps onto LDR=1
      LDR = (ICH+78)/80

*     Return here for normal exit or if a utility returned an error.
   90 CONTINUE

      RETURN

*     Too little room in the data record for the data
   99 CONTINUE
      KERROR = 2001

      RETURN

      END
