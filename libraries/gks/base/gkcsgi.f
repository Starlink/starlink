C# IL>=a, OL>=0
      SUBROUTINE GKCSGI(NUM, IDAT)
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
*     Get integers from CSS segment
*
*  MAINTENANCE LOG
*  ---------------
*     17/11/83  CJW   Original version stabilized
*     29/12/83  CJW   Correct call to GKCSGT
*                     Correct locking of pages
*
*  ARGUMENTS
*  ---------
*     INP NUM    Number of integers
*     INP IDAT   Integer array

      INTEGER NUM, IDAT(*)
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
      NSPACE = KCSMXD - KCSRDO + 1

*     while IFROM <= NUM do
    1 CONTINUE
      IF (IFROM .GT. NUM) GO TO 3

         IF (NSPACE .EQ. 0) THEN
            CALL GKCSUL(KCSRDX)
            INEXT = KCSNXT(KCSRDX)
            CALL GKCSGT(INEXT,KCSRDX)
            KCSRDO = 1
            NSPACE = KCSMXD
         END IF
         IMOVE = MIN(NCOPY, NSPACE)
         DO 2 I = 0, IMOVE - 1
            IDAT(I+IFROM) = KCSDAT(I+KCSRDO, KCSRDX)
    2    CONTINUE
         NCOPY = NCOPY - IMOVE
         NSPACE = NSPACE - IMOVE
         IFROM = IFROM + IMOVE
         KCSRDO = KCSRDO + IMOVE

      GO TO 1
    3 CONTINUE
*     end while

      END
