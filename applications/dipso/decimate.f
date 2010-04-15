
       SUBROUTINE DECIMATE(IFONT,X,APPEND,NAPPEND,IEXP)
       CHARACTER CEXP*3
       CHARACTER S2*3
       CHARACTER APPEND*(*)
       LOGICAL NEGATIVE
       DATA NEGATIVE/.FALSE./
       S2 = '''S'''
       XMANT = X
       IF (XMANT.LT.0.0) THEN
          XMANT = ABS(XMANT)
          NEGATIVE = .TRUE.
       ENDIF

       IEXP = 0
       DO 100 WHILE ((LOG10(XMANT).GT.1.0) .OR. (LOG10(XMANT).LT.0.0))
          IF (XMANT.GE.1.0) THEN
             XMANT = XMANT/10.0
             IEXP = IEXP + 1
          ELSE
             XMANT = XMANT*10.0
             IEXP = IEXP - 1
          ENDIF
  100  CONTINUE

       IF (NEGATIVE) THEN
          XMANT = -XMANT
       ENDIF
       WRITE (CEXP,'(I3)') IEXP
       J = 3
       IF (ABS(IEXP).GE.10) J = J - 1
       IF (IEXP.LT.0) J = J - 1
       NAPPEND = 8 - J
       IF (IFONT.EQ.2) THEN
          APPEND = ' / 10'//S2//CEXP(J:3)
          NAPPEND = NAPPEND + 4
       ELSE
          APPEND = ' / 10**'//CEXP(J:3)
          NAPPEND = NAPPEND + 3
       ENDIF
       END
