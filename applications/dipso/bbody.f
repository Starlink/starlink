       SUBROUTINE BBODY(WAVE,FLUX,ASIZE1,NPOINT,TITLE,TEMP,OK)

       IMPLICIT NONE

       INTEGER ASIZE1, NPOINT
       INTEGER I, J, K, NERR

       REAL WAVE(ASIZE1), FLUX(ASIZE1)
       REAL TEMP
       REAL X, Y

       REAL C1, C2
       REAL LOGC1, LOGC2, LOGE, LOGT, LOGW

       CHARACTER*(*) TITLE

       LOGICAL OK


       PARAMETER (C1=3.74185E+27,C2=1.43883E+08)


       IF (TEMP.LE.0.0) THEN
          OK = .FALSE.
          WRITE (*,'(''   BBODY:  temperature must be positive'')')
          GOTO 300
       ENDIF


       DO 100 I = 1, NPOINT
          IF (WAVE(I).LE.0.0) THEN
             OK = .FALSE.
             WRITE (*,
     :       '(''   BBODY:  negative wavelength encountered'')')
             GOTO 300
          ENDIF
  100  CONTINUE


       LOGE = LOG10(2.7182818)
       LOGC1 = LOG10(C1)
       LOGC2 = LOG10(C2)
       NERR = 0
       LOGT = LOG10(TEMP)

       DO 200 I = 1, NPOINT

          LOGW = LOG10(WAVE(I))
          X = LOGC2 - LOGW - LOGT

          IF (X.GT.1.9) THEN
             IF (X.GT.35.0) THEN
                Y = 100.0
             ELSE
                Y = LOGC1 - 5.0*LOGW - LOGE*(10.0**X)
             ENDIF
          ELSEIF (X.LT.-1.9) THEN
             Y = LOGC1 - LOGC2 + LOGT - 4.0*LOGW
          ELSE
             Y = EXP(10.0**X) - 1.0
             Y = LOGC1 - 5.0*LOGW - LOG10(Y)
          ENDIF

          IF (ABS(Y).GT.35.0) THEN
             FLUX(I) = 0.0
             NERR = NERR + 1
          ELSE
             FLUX(I) = 10.0**Y
          ENDIF

  200  CONTINUE

       TITLE = ' '
       IF (TEMP.LT.1000.0) THEN
          WRITE (TITLE(23:),'(F7.2)') TEMP
       ELSEIF (TEMP.LT.10000.0) THEN
          WRITE (TITLE(23:),'(F8.2)') TEMP
       ELSEIF (TEMP.LT.1.0E+5) THEN
          WRITE (TITLE(23:),'(F9.2)') TEMP
       ELSEIF (TEMP.LT.1.0E+6) THEN
          WRITE (TITLE(23:),'(F10.2)') TEMP
       ELSE
          WRITE (TITLE(23:),'(1PE12.5)') TEMP
       ENDIF
       TITLE(1:22) = ' Black-body curve, T ='

       IF (NERR.NE.0) THEN
          WRITE (*,
     :    '(''   BBODY:'',I4,
     :    '' points afflicted by numerical errors'')') NERR
          OK = .FALSE.
       ENDIF

  300  CONTINUE

       END
