C
      SUBROUTINE RMSER(GX,GZVALS,GAUSUM,ERRUSE,NX,ERRORS,IGST,
     :                      GZRESID,RMS,MERR,HIGHR,LOWR,ERASE)
C
C     R M S E R
C
C     Calculates the residuals, rms and mean fractional error ( in terms of
C     error bars, if errors available ) over the extent of the fitted line.
C     If the residuals are greater in extent than those previously set,
C     then ERASE is set true so that the plot can be replotted to encompass
C     the new residuals
C
C     Parameters - (">" input, "<" output, "=" both )
C
C     (>) GX      (Integer) Length of array GXVALS
C     (>) GZVALS  (Real array) Y values of line points
C     (>) GAUSUM  (Real array) Y values of Gaussian fit to line
C     (>) ERRUSE  (Logical) True if erros are available
C     (>) NX      (Integer) Length of array ERRORS
C     (>) ERRORS  (Real array) Z values of errors on data points
C     (>) IGST    (Integer) First channel of line extent
C     (<) GZRESID (Real array) Residuals on observed - Gaussian fitted
C     (<) RMS     (Real) R.m.s. on Gaussian fit over line
C     (<) MERR    (Real) Mean error in terms of error bars on line fit
C     (=) LOWR    (Real) Largest negative residual
C     (=) HIGHR   (Real) Largest positive residual
C     (<) ERASE   (Logical) True if residuals are larger in extent than
C                 previously, necessitating plot to be replotted
C
C                                                JRW / AAO  February 1987
C
C     Modified:
C      Original
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER GX,NX,IGST
      REAL GZVALS(GX),GZRESID(GX),GAUSUM(GX),ERRORS(NX),
     :RMS,MERR,LOWR,HIGHR
      LOGICAL ERRUSE,ERASE
C
C     Local variables
C
      INTEGER I
      REAL RMST,MAXR,MINR

      IF (ERRUSE) THEN
        RMST=0.0
        MERR=0.0
        DO I=1,GX
          GZRESID(I)=GZVALS(I)-GAUSUM(I)
          RMST=RMST + (GZRESID(I)**2.)
          MERR=MERR + ABS(GZRESID(I)/ERRORS(IGST+I-1))
        END DO
        RMS=SQRT(RMST/REAL(GX-1))
        MERR=MERR/REAL(GX)
      ELSE
        RMST=0.0
        DO I=1,GX
          GZRESID(I)=GZVALS(I)-GAUSUM(I)
          RMST=RMST + (GZRESID(I)**2.)
        END DO
        RMS=SQRT(RMST/REAL(GX-1))
      END IF
C
C     Find the maximum and minimum values of the resids to
C     determine of the upper resids box needs to be redrawn
C
      MAXR=-1.E36
      MINR=1.E36
      DO I=1,GX
        IF (GZRESID(I).GT.MAXR) THEN
          MAXR=GZRESID(I)
        END IF
        IF (GZRESID(I).LT.MINR) THEN
          MINR=GZRESID(I)
        END IF
      END DO
      IF (MAXR.NE.HIGHR) THEN
        ERASE=.TRUE.
        HIGHR=MAXR
      END IF
      IF (MINR.NE.LOWR) THEN
        ERASE=.TRUE.
        LOWR=MINR
      END IF
      IF (ERASE) THEN
        HIGHR=HIGHR + ABS(HIGHR-LOWR)/12.
        LOWR=LOWR - ABS(HIGHR-LOWR)/12.
      END IF

      END
