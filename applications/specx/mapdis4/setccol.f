*-----------------------------------------------------------------------

      SUBROUTINE SETCCOL (Z, ZMIN, ZMAX, ICOL)

*  Routine to choose a plotting colour that will contrast with
*  the greyscale background (with standard greyscales anyway)

      IMPLICIT  NONE

*     Formal parameters:

      REAL      Z
      REAL      ZMIN, ZMAX
      INTEGER   ICOL

*  Ok, go...

      IF (Z.GT.(ZMIN+ZMAX)*0.5) THEN
        ICOL = 0
      ELSE
        ICOL = 1
      END IF

      IF (ZMIN.GT.ZMAX) ICOL = 1-ICOL

      RETURN
      END
*-----------------------------------------------------------------------
