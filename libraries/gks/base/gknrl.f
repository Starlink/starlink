C# IL>=a, OL>=0
      SUBROUTINE GKNRL(RIDEAL,LLIS,RLIS,IXRLIS)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     NeaRest in List:  Find member of list RLIS (length LLIS)
*     that is nearest to RIDEAL and return its index in IXRLIS.
*
*  MAINTENANCE LOG
*  ---------------
*     17/10/85  JRG   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     Inp  RIDEAL  Value that is to be matched
*     Inp  LLIS    Number of items in list RLIS
*     Inp  RLIS    Array containing list of candidates
*     Out  IXRLIS  Index of candidate that is nearest to RIDEAL
*
      INTEGER LLIS,IXRLIS
      REAL RIDEAL,RLIS(LLIS)
*
*  LOCALS
*  ------
*     LP    Loop counter
*     DIST  Distance between RIDEAL and candidate currently being tested
*     DISTMN Nearest distance so far
*
      INTEGER LP
      REAL DIST,DISTMN
*
*-----------------------------------------------------------------------

*   Initialise the variables that we alter while scanning the list
      IXRLIS=1
      DISTMN=ABS(RLIS(1)-RIDEAL)

*   Now find the nearest by looking at the remainder
      DO 200 LP=2,LLIS
        DIST=ABS(RLIS(LP)-RIDEAL)
        IF( DIST.LT.DISTMN ) THEN
          IXRLIS=LP
          DISTMN=DIST
        ENDIF
  200 CONTINUE

      END
