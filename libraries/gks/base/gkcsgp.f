C# IL>=a, OL>=0
      SUBROUTINE GKCSGP(NUM, RX, RY)
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
*     Get real points from CSS segment
*
*  MAINTENANCE LOG
*  ---------------
*     17/11/83  CJW   Original version stabilized
*     29/12/83  CJW   Correct call to GKCSGT
*                     Correct locking of pages
*
*  ARGUMENTS
*  ---------
*     INP NUM    Number of coordinate pairs
*     INP RX     X coordinates
*     INP RY     Y coordinates
*
      INTEGER NUM
      REAL RX(*), RY(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkcss.par'
      INCLUDE '../include/gkcss.cmn'
*
*  LOCALS
*  ------
*
      INTEGER IFROM, NCOPY, NSPACE, IMOVE, I, INEXT
*
*  ERRORS
*  ------
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------


*     put data into segment
      IFROM =1
      NCOPY = NUM
      NSPACE = (KCSMXD - KCSRDO + 1) / 2

*     while IFROM <= NUM do
    1 CONTINUE
      IF (IFROM .GT. NUM) GO TO 3

         IF (NSPACE .EQ. 0) THEN
            CALL GKCSUL(KCSRDX)
            INEXT = KCSNXT(KCSRDX)
            CALL GKCSGT(INEXT, KCSRDX)

            KCSRDO = 1
            NSPACE = KCSMXD / 2
         END IF
         IMOVE = MIN(NCOPY, NSPACE)
         DO 2 I = 0, IMOVE - 1
            RX(I+IFROM) = QCSDAT((2*I)+KCSRDO, KCSRDX)
            RY(I+IFROM) = QCSDAT((2*I)+KCSRDO+1, KCSRDX)
    2    CONTINUE
         NCOPY = NCOPY - IMOVE
         NSPACE = NSPACE - IMOVE
         IFROM = IFROM + IMOVE
         KCSRDO = KCSRDO + (2 * IMOVE)

      GO TO 1
    3 CONTINUE
*     end while

      END
