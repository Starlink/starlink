*
       SUBROUTINE CHEATSM(VARRAY,WAVE,FLUX,NPOINT,OK)
       INTEGER NPOINT
       REAL WAVE(NPOINT), FLUX(NPOINT)
       REAL VARRAY(10)
       LOGICAL OK

       X1 = VARRAY(1)
       TOL = VARRAY(2)
       DO 100 I = 2, NPOINT - 1
          IF (WAVE(I-1).GT.X1) THEN
             IF (WAVE(I+1).GT.0.0) THEN
                GOTO 200
             ELSE
                DIFF1 = ABS(FLUX(I-1)-FLUX(I))
                DIFF2 = ABS(FLUX(I)-FLUX(I+1))
                IF (DIFF1.LT.TOL .AND. DIFF2.LT.TOL) THEN
                   FLUX(I) = (FLUX(I-1)+FLUX(I)+FLUX(I+1))/3.
                ENDIF
             ENDIF
          ENDIF
  100  CONTINUE

  200  CONTINUE

       END
