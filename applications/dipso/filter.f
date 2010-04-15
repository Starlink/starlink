      SUBROUTINE FILTER (FREQ, RE, IM, NPTS, FC, FHUN, WT)
C
C  Program evaluates "exponent squared edge" weight function (WT) for
C Fourier filtering purposes.
C  The real and imaginary parts of a transform are then multiplied by the
C weighting function to return filtered transforms.
C
C         WT(FREQ) = 1.0                          (FREQ <= FC)
C         WT(FREQ) = EXP[-{FAC(FREQ-FC)}^2]    (FREQ > FC)
C
      IMPLICIT NONE
C  Imports:-
      REAL FHUN                 ! WT(FREQ=FHUN) = 0.01
      REAL FC                   ! filtering start frequency
      REAL FREQ(*)              ! array of frequency abscissae
      INTEGER NPTS              ! number of values in each array
C  Imports/exports:-
      REAL RE(*)                ! real part of transform
      REAL IM(*)                ! imaginary part of transform
C  Exports:-
      REAL WT(*)                ! array of weights
C  Local:-
      REAL LN0                            ! LOGe(10)
      PARAMETER (LN0 = 2.302585093)
      REAL FAC
      INTEGER I                             ! DO-loop incrementer
C
      FAC = SQRT(2.0*LN0) / (FHUN-FC)
      DO 100,I=1,NPTS
        IF (FREQ(I).LE.FC) THEN
          WT(I) = 1.0
        ELSE
C
C  Set WT = zero if underflow might result.
C
          IF ((FAC*(FREQ(I)-FC)).LT.8.0) THEN
            WT(I) = EXP(-(FAC*(FREQ(I)-FC))**2)
          ELSE
            WT(I) = 0.0
          ENDIF
        ENDIF
        RE(I) = RE(I) * WT(I)
        IM(I) = IM(I) * WT(I)
  100 CONTINUE
      RETURN
      END
