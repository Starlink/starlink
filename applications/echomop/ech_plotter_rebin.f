      SUBROUTINE ECH_PLOTTER_REBIN( REBIN_FACTOR, XDATA, YDATA, N )
*+
*-
      IMPLICIT NONE

      INTEGER N
      REAL XDATA( N )
      REAL YDATA( N )
      INTEGER REBIN_FACTOR
      INTEGER BINO
      INTEGER BINS
      INTEGER I
*.

      BINS = 1
      BINO = 0
      DO WHILE ( BINS .LT. N )
         BINO = BINO + 1
         XDATA( BINO ) = XDATA( MIN( BINS + REBIN_FACTOR, N ) )
         YDATA( BINO ) = YDATA( BINS )
         DO I = BINS, MIN( BINS + REBIN_FACTOR - 1, N )
            YDATA( BINO ) = YDATA( BINO ) + YDATA( I )
         END DO
         YDATA( BINO ) = YDATA( BINO ) / REBIN_FACTOR
         BINS = BINS + REBIN_FACTOR
      END DO
      N = N / REBIN_FACTOR

      END
