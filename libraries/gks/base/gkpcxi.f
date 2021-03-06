C# IL>=a, OL>=0
      SUBROUTINE GKPCXI(NUM)
*
* (C) COPYRIGHT ICL & SERC  1985
*
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             MGC
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Skip integers in CSS segment
*
*  MAINTENANCE LOG
*  ---------------
*     01/08/85  MGC   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP NUM    Number of integers

      INTEGER NUM
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
      INTEGER IFROM, NCOPY, NSPACE, ISKIP, INEXT
*
*  ERRORS
*  ------
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------


*     initial size
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
         ISKIP = MIN(NCOPY, NSPACE)
         NCOPY = NCOPY - ISKIP
         NSPACE = NSPACE - ISKIP
         IFROM = IFROM + ISKIP
         KCSRDO = KCSRDO + ISKIP

      GO TO 1
    3 CONTINUE
*     end while

      END
