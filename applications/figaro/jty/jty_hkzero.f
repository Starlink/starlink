      SUBROUTINE JTY_HKZERO(N,WAVE,DATA,PERCENT)
* Program to apodize the ends of the spectrum with a cosine bell,
* making sure to take out all of H+K by starting at 3994A
*
* The parameter statement had no brackets. The Sun compiler doesn't like
* that.
*                                HME/UoE, Starlink. 01-SEP-1992.
      REAL*4 WAVE(1), DATA(1)
      PARAMETER ( PI = 3.14159265 )
      REAL*4 CUT1, CUT2
      DATA CUT1,CUT2 /3910.,3994./
      I1 = 0
      I2 = 0
      DO I = 1,N
          IF(WAVE(I).GT.CUT1.AND.I1.EQ.0) THEN
            I1 = I - 1
          ENDIF
          IF(WAVE(I).GT.CUT2.AND.I2.EQ.0) THEN
            I2 = I
            GOTO 11
          ENDIF
      END DO
11    CONTINUE
      NSQUASH = N * .01 * PERCENT
      DO I = 1,I2
          DATA(I) = 0
      END DO
      DO I = 1,NSQUASH
          ARG = PI * (I-1)/(NSQUASH-1)
          FACTOR = .5 * (1 - COS(ARG))
          DATA(I+I2) = FACTOR * DATA(I+I2)
          DATA(N-I+1) = FACTOR * DATA(N-I+1)
      END DO
      RETURN
      END
