      SUBROUTINE NDP_REPLACE_QUAL_R(ARRAY,QARRAY,NELM,VALUE)
C+
C
C   -------------------------------
C   N D P _ R E P L A C E _ Q U A L
C   -------------------------------
C
C   Description
C   -----------
C   Replaces bad quality pixels in an array with a supplied value.
C
C
C   Parameters
C   ----------
C   ARRAY      (! TYPE array). Data array.
C   QARRAY     (> BYTE array). Quality array
C   NELM       (> integer). Number of array elements.
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
C   Julian Gold RGO   (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C
C   History
C   -------
C   14-OCT-1991   - Original version
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
     &          VALUE
      BYTE      QARRAY(NELM)
C
C     Local variables.
C
      INTEGER   I
C
C     Loop through data, replacing bad values.
C
      DO I=1,NELM
        IF(QARRAY(I).NE.0)ARRAY(I)=VALUE
      END DO
C
      END
