      SUBROUTINE JTY_SHIFTIT(N,SHIFT,X)
* Shift a complex buffer by -SHIFT
*
* The parameter statement had no brackets. The Sun compiler doesn't like
* that.
*                                HME/UoE, Starlink. 01-SEP-1992.
      COMPLEX*8 X(N), PHASE
      PARAMETER ( PI = 3.14159265 )
      DO I = 1,N
          IF(I-1.LE.N/2) THEN
              ANGLE = -2*PI*(I-1)*SHIFT/N
          ELSE
              ANGLE = -2*PI*(I-1-N)*SHIFT/N
          ENDIF
          PHASE = CMPLX(COS(ANGLE),SIN(ANGLE))
          X(I) = X(I) * PHASE
      ENDDO
      RETURN
      END
