**==RLINT.FOR
       FUNCTION RLINT(CUTIE,EEXT,WAVEXT,M,N)
       DIMENSION EEXT(M), WAVEXT(M)
       IF (CUTIE.GT.WAVEXT(1)) THEN
          DO 50 K = 2, N
             IF (CUTIE.LE.WAVEXT(K)) GOTO 100
   50     CONTINUE
       ENDIF
       RLINT = 1.0D0
       RETURN
  100  CONTINUE
       KK = K - 1
       RLINT = EEXT(KK) + (EEXT(K)-EEXT(KK))*(CUTIE-WAVEXT(KK))
     :         /(WAVEXT(K)-WAVEXT(KK))
       RETURN
       END
