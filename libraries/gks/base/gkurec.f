      SUBROUTINE GKUREC(LDR, DATREC, NID, IID, NRD, IRD, NSD, IST)
*
* COPYRIGHT (C) SERC 1987
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
*   Unpacks a data record,  returning the data in an internal format.
*
*  MAINTENANCE LOG
*  ---------------
*     12/08/87  PJWR  Created from GUREC by CJW.
*     14/08/87  PJWR  Corrected heap usage.
*     30/09/87  PJWR  Changed to unpack onto stack then copy to heap,
*                     as heap corruption was occurring unexpectedly.
*
*  ARGUMENTS
*  ---------
*     INP   LDR    Number of array elements in DATREC.
*     INP   DATREC Data record.  This is a CHARACTER*(80)(LDR), but is
*                  redeclared as CHARACTER*(1)(80*LDR) for convienience.
*     OUT   NID    Number of integer data
*     OUT   IID    Integer heap offset of integer data
*     OUT   NRD    Number of real data
*     OUT   IRD    Real heap offset of real data
*     OUT   NSD    Number of character string data
*     OUT   IST    Integer heap offset of string table
*
      INTEGER LDR, NID, IID, NRD, IRD, NSD, IST
      CHARACTER DATREC(80*LDR)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify  /GKYERR/  KERROR
*             /GKYSTK/  KSTACK, QSTACK
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  EXTERNAL FUNCTIONS
*  ------------------
*     GKNA1   Converts a native character to an ASCII integer.
*
      INTEGER GKNA1
*
*  LOCALS
*  ------
*     I, J    Loop indices
*     ICH     Character index
*     NCH     The size of the data record in characters
*     ITEMP   An integer temporary used for unpacking reals
*     RTEMP   A real temporary equivalenced to ITEMP
*     LENGTH  The length of a string item
*     IWHERE  Where a string is on the heap
*     IOFFI   Stack offset integer extraction workspace
*     IOFFR   Stack offset real extraction workspace
*     IOFFS   Stack offset for native -> ASCII conversion workspace
*
      INTEGER I, J, ICH, NCH, ITEMP, LENGTH, IWHERE, IOFFI, IOFFR, IOFFS
      REAL RTEMP
      EQUIVALENCE (ITEMP, RTEMP)
*
*  ERRORS
*  ------
*      2003  Invalid data record
*
*  COMMENTS
*  --------
*     Heap allocation errors are detected,  but no attempt at cleanup is
*     made.
*
*     This routine differs from GUREC in that the data in the record is
*     placed on the heap and the heap offsets are returned.  This is
*     necessary because the calling routine may not know what data is
*     in the record if it has been supplied by an application and useful
*     because input device data is stored in this way.  Because this
*     routine uses GKUREI,  which assumes that a five character string
*     created by GKPREI can be converted into an integer or real,  it is
*     unfortunately
*
*                            SYSTEM DEPENDENT
*
*-----------------------------------------------------------------------

*     Calculate the size of the data record and initialise the current
*     character cursor.
      NCH = LDR*80
      ICH = 1

*     Initialise heap and stack offsets.
      IID = KNIL
      IRD = KNIL
      IST = KNIL
      IOFFI = KNIL
      IOFFR = KNIL
      IOFFS = KNIL

*     Unpack the number of integers followed by any integer data.
      IF (NCH.LT.ICH+4) GO TO 99
      CALL GKUREI(DATREC(ICH),NID)
      ICH = ICH+5
      IF (NID.GT.0) THEN
*       Check that the data record is at least big enough to contain the
*       integers we're trying to unpack!
        IF (NCH.LT.ICH+MAX(0,NID)*5-1) GO TO 99
*       Allocate heap and stack space and unpack the integers.
      CALL GKSTAL(KINTGS,NID,IOFFI)
      IF (KERROR.NE.0) GO TO 90
      CALL GKHPAL(NID,KINTGS,IID)
      IF (KERROR.NE.0) GO TO 90
      DO 10 I = 0, NID - 1
        CALL GKUREI(DATREC(ICH),KSTACK(IOFFI+I))
        ICH = ICH+5
  10    CONTINUE
*       Copy the integers onto the heap and deallocate stack.
      CALL GKHPPI(IID,0,NID,KSTACK(IOFFI))
      IF (KERROR.NE.0) GO TO 90
      CALL GKSTDA(KINTGS,IOFFI)
      IF (KERROR.NE.0) GO TO 90
      END IF

*     Unpack the number of reals,  then the real data.  Done in the same
*     fashion as integers.
      IF (NCH.LT.ICH+4) GO TO 99
      CALL GKUREI(DATREC(ICH),NRD)
      ICH = ICH+5
      IF (NCH.LT.ICH+MAX(0,NRD)*5-1) GO TO 99
      IF (NRD.GT.0) THEN
      CALL GKSTAL(KREALS,NRD,IOFFR)
      IF (KERROR.NE.0) GO TO 90
      CALL GKHPAL(NRD,KREALS,IRD)
      IF (KERROR.NE.0) GO TO 90
      DO 20 I = 0, NRD - 1
        CALL GKUREI(DATREC(ICH),ITEMP)
        QSTACK(IOFFR+I) = RTEMP
        ICH = ICH+5
   20   CONTINUE
      CALL GKHPPR(IRD,0,NRD,QSTACK(IOFFR))
      IF (KERROR.NE.0) GO TO 90
      CALL GKSTDA(KREALS,IOFFR)
      IF (KERROR.NE.0) GO TO 90
      END IF

*     Unpack the number of strings followed by each string length and
*     each string.  Checks are made along the same lines as for integers
*     and reals.
      IF(NCH.LT.ICH+4)GOTO 99
      CALL GKUREI(DATREC(ICH),NSD)
      ICH = ICH+5
      IF (NSD.GT.0) THEN
*       Allocate stack and heap space for the string table.
      CALL GKSTAL(KINTGS,NSD*2,IOFFI)
      IF (KERROR.NE.0) GO TO 90
      CALL GKHPAL(NSD*2,KINTGS,IST)
      IF (KERROR.NE.0) GO TO 90
*       For each string,  get the length of the string and put it in the
*       string table,  then allocate heap and stack space for the string
*       convert the string into,ASCII on the stack and copy the stack
*       frame onto the heap.
      DO 40 I = 1, NSD*2, 2
        IF(NCH.LT.ICH+4)GOTO 99
        CALL GKUREI(DATREC(ICH),LENGTH)
        KSTACK(IOFFI+I-1) = LENGTH
        ICH = ICH+5
        IF(NCH.LT.ICH+LENGTH-1)GOTO 99
        CALL GKHPAL(LENGTH,KINTGS,IWHERE)
        IF(KERROR.NE.0) GO TO 90
        KSTACK(IOFFI+I) = IWHERE
*         Allocate some stack space for string conversion.
        CALL GKSTAL(KINTGS,LENGTH,IOFFS)
        DO 30 J = 0, LENGTH - 1
          KSTACK(IOFFS+J) = GKNA1(DATREC(ICH))
          ICH = ICH + 1
   30     CONTINUE
        CALL GKHPPI(IWHERE,0,LENGTH,KSTACK(IOFFS))
*         Deallocate stack space.
        CALL GKSTDA(KINTGS,IOFFS)
   40   CONTINUE
*       Now copy the string table onto the heap and deallocate its
*       stack.
      CALL GKHPPI(IST,0,NSD*2,KSTACK(IOFFI))
      IF (KERROR.NE.0) GO TO 90
      CALL GKSTDA(KINTGS,IOFFI)
      END IF

*     Return from here for normal exit,  or if a heap or stack
*     allocation error was detected.
   90 RETURN

*     Return from here if the data record was invalid.
   99 KERROR = 2003
      RETURN

      END
