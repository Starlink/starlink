      SUBROUTINE NDP_REPLACE_MAGIC_R(ARRAY,NELM,MAGICVAL,VALUE)
C+
C
C   ---------------------------------
C   N D P _ R E P L A C E _ M A G I C
C   ---------------------------------
C
C   Description
C   -----------
C   Replaces magic values in an array with a supplied value.
C
C
C   Parameters
C   ----------
C   ARRAY      (! TYPE array). Data array.
C   NELM       (> integer). Number of array elements.
C   MAGICVAL   (> TYPE) Magic value
C   VALUE      (> TYPE). Replacement value.
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   None.
C
C
C   INCLUDE statements
C   ------------------
C   None.
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters
C
C
C   VAX-specific statements
C   -----------------------
C   None.
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Julian Gold RGO   (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C
C   History
C   -------
C   01-FEB-1989   - Original program
C   14-OCT-1991   - Re-written in GENERIC form (GOLDJIL)
C
C+-----------------------------------------------------------------------------
C
      IMPLICIT NONE
C
C     Parameters.
C
      INTEGER   NELM
      REAL
     &          ARRAY(NELM),
     &          VALUE,MAGICVAL
C
C     Local variables.
C
      INTEGER   I
C
C     Loop through data, replacing magic values.
C
      DO I=1,NELM
        IF(ARRAY(I).EQ.MAGICVAL)ARRAY(I)=VALUE
      END DO
C
      END
