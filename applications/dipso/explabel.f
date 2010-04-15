       SUBROUTINE EXPLABEL(IFONT,ARRAY,NPTS,LIMITS,LABEL,LABELN,
     :                     NEWLABELN,IEXP,IXYEXP,LOGXY)

       PARAMETER (ZEROTOL=1.0E-35)

       REAL ARRAY(*)
       REAL LIMITS(2)

       CHARACTER APPEND*16
       CHARACTER CTEMP*16
       CHARACTER LABEL*60
       CHARACTER*3 USER

       LOGICAL ZEROMAX, ZEROMIN, STFORM
       LOGICAL LOGXY

       DATA XLARGENEG/ -35.0/

       IF (LOGXY) THEN
          STFORM = .FALSE.
          GOTO 100
       ENDIF

       AMIN = LIMITS(1)
       AMAX = LIMITS(2)
       ZEROMAX = .FALSE.
       ZEROMIN = .FALSE.
*   Avoid taking log of zero
       IF (ABS(AMAX).LT.ZEROTOL) ZEROMAX = .TRUE.
       IF (ABS(AMIN).LT.ZEROTOL) ZEROMIN = .TRUE.
*   If exponent too long change to standard fom

       IF (ZEROMAX) THEN
          AMAXEXP = XLARGENEG
       ELSE
          AMAXEXP = LOG10(ABS(AMAX))
       ENDIF

       IF (ZEROMIN) THEN
          AMINEXP = XLARGENEG
       ELSE
          AMINEXP = LOG10(ABS(AMIN))
       ENDIF

       IF(ZEROMIN.AND.ZEROMAX)THEN
*        Both max and min are zero
          STFORM = .FALSE.
       ELSEIF (AMIN.GT.0.0) THEN
*        Max and min both greater than zero
          IF (INT(ABS(AMAXEXP)).GE.IXYEXP) THEN
             STFORM = .TRUE.
             AUSE = AMAX
          ELSE
             STFORM = .FALSE.
          ENDIF
       ELSEIF (AMAX.LT.0.0) THEN
*       Max and Min both less than zero
          IF (INT(ABS(AMINEXP)).GE.IXYEXP) THEN
             STFORM = .TRUE.
             AUSE = AMIN
          ELSE
             STFORM = .FALSE.
          ENDIF
       ELSEIF (AMAXEXP.GT.AMINEXP) THEN
*       Straddling zero
          IF (INT(ABS(AMAXEXP)).GE.IXYEXP) THEN
             STFORM = .TRUE.
             AUSE = AMAX
          ELSE
             STFORM = .FALSE.
          ENDIF
       ELSEIF (INT(ABS(AMINEXP)).GE.IXYEXP) THEN
          STFORM = .TRUE.
          AUSE = AMIN
       ELSE
          STFORM = .FALSE.
       ENDIF


  100  CONTINUE
       IF (STFORM) THEN
          CALL DECIMATE(IFONT,AUSE,APPEND,NAPPEND,IEXP)
          DIVFACTOR = 10.0**IEXP
          DO 150 I = 1, NPTS
             ARRAY(I) = ARRAY(I)/DIVFACTOR
  150     CONTINUE
*
          IF (IFONT.EQ.2) THEN
             CTEMP = '''PRU'''//APPEND
             APPEND = CTEMP
             NAPPEND = NAPPEND + 5
          ENDIF
*
          NEWLABELN = LABELN + NAPPEND
          LABEL(1:NEWLABELN) = LABEL(1:LABELN)//APPEND
       ELSE
          NEWLABELN = LABELN
          IEXP = 0
       ENDIF

       END
