C# IL>=a, OL>=0
      SUBROUTINE GKMTIV (XFM,XFOUT)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             JGW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     The subroutine is designed to return the inverse of matrix XFM
*
*  MAINTENANCE LOG
*  ---------------
*     19/12/83  JGW  Original version stabilized
*     20/12/83  AS   Tidy up to coding standards
*     10/04/84  JGW  Setup shift component!
*     08/09/88  KEVP Corrected column selection from XFIN for XFOUT (S340)
*
*  ARGUMENTS
*  ---------
*     INP   XFM          transform matrix
*     OUT   XFOUT        inverse transform matrix
*
      REAL    XFM(3,2), XFOUT(3,2)
*
*  LOCALS
*  ------
*     XFIN     Local copy of XFM for use/corruption
*     XFINV    The inverse matrix without swapped rows
*     ICOL     Loop variable (column)
*     IROW     Loop variable (row)
*     IROWA    Row to consider as row 1
*     IROWB    Row to consider as row 2
*     FACTOR   Pivot operation row factor
*
      INTEGER ICOL, IROW, IROWA, IROWB
      REAL    XFIN(3,2), XFINV(3,2), FACTOR
*
*  ALGORITHM
*  ---------
*     Uses Guass/Jordan type row pivot operations.
*
*---------------------------------------------------------------------

      DO 10 ICOL=1,3
        DO 5 IROW=1,2
          XFINV(ICOL,IROW) = 0.0
    5   CONTINUE
   10 CONTINUE
      XFINV(1,1) = 1.0
      XFINV(2,2) = 1.0

      DO 20 ICOL=1,3
        DO 15 IROW=1,2
          XFIN(ICOL,IROW) = XFM(ICOL,IROW)
   15   CONTINUE
   20 CONTINUE

      IF (ABS(XFIN(1,1)) .LT. ABS(XFIN(1,2))) THEN
        IROWA = 2
        IROWB = 1
      ELSE
        IROWA = 1
        IROWB = 2
      ENDIF

      IF (ABS(XFIN(2,IROWB)) .LT.1E-12) THEN
        FACTOR = 0.0
      ELSE
        FACTOR = XFIN(2,IROWA)/XFIN(2,IROWB)
      ENDIF
      DO 100 ICOL=1,3
        XFINV(ICOL,IROWA)= XFINV(ICOL,IROWA) - FACTOR*XFINV(ICOL,IROWB)
        XFIN(ICOL,IROWA) = XFIN(ICOL,IROWA)  - FACTOR*XFIN(ICOL,IROWB)
  100 CONTINUE

      IF (ABS(XFIN(1,IROWA)) .LT.1E-12) THEN
        FACTOR = 0.0
      ELSE
        FACTOR = XFIN(1,IROWB)/XFIN(1,IROWA)
      ENDIF
      DO 110 ICOL=1,3
        XFINV(ICOL,IROWB)= XFINV(ICOL,IROWB) - FACTOR*XFINV(ICOL,IROWA)
        XFIN(ICOL,IROWB) = XFIN(ICOL,IROWB)  - FACTOR*XFIN(ICOL,IROWA)
  110 CONTINUE

      XFINV(3,1) = -XFIN(3,1)
      XFINV(3,2) = -XFIN(3,2)
      DO 120 ICOL=1,3
        XFOUT(ICOL,1) = XFINV(ICOL,IROWA)/XFIN(1,IROWA)
        XFOUT(ICOL,2) = XFINV(ICOL,IROWB)/XFIN(2,IROWB)
  120 CONTINUE


      END
