      SUBROUTINE ECH_PLOTTER_SMOOTH( SMOOTH_FACTOR, YDATA, N )
*+
*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments:
      INTEGER SMOOTH_FACTOR
      INTEGER N
      REAL YDATA( N )

*  Local Constants:
      INTEGER MAX_PTS
      PARAMETER ( MAX_PTS = 8192 )

*  Local Variables:
      REAL TEMP( MAX_PTS )
      INTEGER I
      INTEGER IMAX
      INTEGER J
      INTEGER STARTJ
      INTEGER ENDJ
*.
      IMAX = MIN( N, MAX_PTS )
      DO I = 1, IMAX
         TEMP( I ) = 0.0
         STARTJ = MIN( MAX( 1, I - SMOOTH_FACTOR / 2 ),
     :         IMAX - SMOOTH_FACTOR + 1 )
         ENDJ = MIN( STARTJ + SMOOTH_FACTOR - 1, IMAX )
         DO J = STARTJ, ENDJ
            TEMP( I ) = TEMP( I ) + YDATA( J )
         END DO
      END DO

      DO I = 1, IMAX
         YDATA( I ) = TEMP( I ) / FLOAT( SMOOTH_FACTOR )
      END DO

      END
