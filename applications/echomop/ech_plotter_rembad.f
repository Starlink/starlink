      SUBROUTINE ECH_PLOTTER_REMBAD( XDATA, YDATA, N )
*+
*-
      IMPLICIT NONE

      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'

      INTEGER N
      REAL XDATA( N )
      REAL YDATA( N )

      INTEGER I
      INTEGER OUT
*.
      OUT = 1
      DO I = 1, N
         IF ( XDATA( I ) .EQ. ECH__BAD_REAL .OR.
     :        YDATA( I ) .EQ. ECH__BAD_REAL ) THEN
            GO TO 100
         END IF
         OUT = OUT + 1
      END DO
  100 CONTINUE
      DO I = OUT, N
         IF ( XDATA( I ) .NE. ECH__BAD_REAL .AND.
     :        YDATA( I ) .NE. ECH__BAD_REAL ) THEN
            XDATA( OUT ) = XDATA( I )
            YDATA( OUT ) = YDATA( I )
            OUT = OUT + 1
         END IF
      END DO
      N = OUT - 1

      END
