C------------------------------------------------------------------------------

      DOUBLE PRECISION FUNCTION RED(ANGLE)

C     REDUCES RADIAN ANGLES TO THE RANGE (0.,2*PI)

      REAL*8 ANGLE, PI, ALPHA

      DATA PI/3.141592654D0/

      ALPHA=ANGLE
      N=IDINT(ALPHA/(2.0D0*PI))
      ALPHA=ALPHA-N*2.D0*PI
      IF(ALPHA.GE.0.0D0)   GO TO 20
      ALPHA=ALPHA+2.D0*PI
      N=N-1
   20 RED=ALPHA

      RETURN
      END

C------------------------------------------------------------------------------
