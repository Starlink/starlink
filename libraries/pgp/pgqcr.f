C*PGQCR  -- inquire color representation
C%void cpgqcr(int ci, float *cr, float *cg, float *cb);
C+
      SUBROUTINE PGQCR (CI, CR, CG, CB)
      INTEGER CI
      REAL    CR, CG, CB
C
C Query the RGB colors associated with a color index.
C
C Arguments:
C  CI  (input)  : color index
C  CR  (output) : red, green and blue intensities
C  CG  (output)   in the range 0.0 to 1.0
C  CB  (output)
C--
C 7-Apr-1992 - new routine [DLT]
C-----------------------------------------------------------------------
      CALL GRQCR(CI, CR, CG, CB)
      END
