C# IL>=a, OL>=0
      SUBROUTINE GKCSFR(IREC)
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
*     CSS : Add record chain to free chain
*
*  MAINTENANCE LOG
*  ---------------
*     17/11/83  CJW   Original version stabilized
*      7/02/84  CJW   Free chain now left in state UNLOCK, not FREE.
*     19/04/84  CJW   Use local copy of next pointer. Lower routines
*                     might swap original out. (I182)
*
*  ARGUMENTS
*  ---------
*     INP IREC   Record that is the Head of the chain
*
      INTEGER IREC
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkcss.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkcss.cmn'
*
*  LOCALS
*  ------
*     INDEX  Page index
*     I      Next record
*
      INTEGER INDEX,I
*
*  COMMENTS
*  --------
*     Adds the whole chain of records starting at record to
*     the free chain
*
*---------------------------------------------------------------------




*     remember the start of the chain - will become the start of
*     the free chain

      CALL GKCSGT(IREC,INDEX)
      CALL GKCSUL(INDEX)

*     Find the end of the chain

*     while KCSNXT(INDEX) .NE. KNIL do
    1 CONTINUE
      IF (KCSNXT(INDEX) .EQ. KNIL) GO TO 2

         I = KCSNXT(INDEX)
         CALL GKCSGT(I, INDEX)
         CALL GKCSUL(INDEX)

      GO TO 1
    2 CONTINUE
*     end while

*     Connect it to the existing free chain
      KCSNXT(INDEX) = KCSFCH

*     Free chain now starts at begining of chain

      KCSFCH = IREC

      END
