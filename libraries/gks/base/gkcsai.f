C# IL>=a, OL>=0
      SUBROUTINE GKCSAI(NUM, IDAT)
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
*     Add integers to CSS segment
*
*  MAINTENANCE LOG
*  ---------------
*     17/11/83  CJW   Original version stabilized
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
      INTEGER IFROM, NCOPY, NSPACE, IMOVE, I
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
      NSPACE = KCSMXD - KCSOPO + 1

*     while IFROM <= NUM do
    1 CONTINUE
      IF (IFROM .GT. NUM) GO TO 3

         IF (NSPACE .EQ. 0) THEN
            CALL GKCSNX(KCSOPX)
            KCSOPO = 1
            NSPACE = KCSMXD
         END IF
         IMOVE = MIN(NCOPY, NSPACE)
         DO 2 I = 0, IMOVE - 1
            KCSDAT(I+KCSOPO, KCSOPX) = IDAT(I+IFROM)
    2    CONTINUE
         NCOPY = NCOPY - IMOVE
         NSPACE = NSPACE - IMOVE
         IFROM = IFROM + IMOVE
         KCSOPO = KCSOPO + IMOVE

      GO TO 1
    3 CONTINUE
*     end while

      END
