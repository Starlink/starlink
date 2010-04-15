       SUBROUTINE ARITH(FN,WAVE,FLUX,NPOINT,BREAK,NBREAK,XSTACK,YSTACK,
     : STKNPT,BSTACK,NBSTCK,WORK,IWORK,ASIZE1,MAXBRK)
       CHARACTER*(*) FN
       LOGICAL OK
       REAL WAVE(1), FLUX(1), XSTACK(1), YSTACK(1), WORK(1)
       INTEGER NBSTCK
       INTEGER IWORK(1), BREAK(1), BSTACK(1)
       INTEGER ASIZE1, MAXBRK
       INTEGER NPOINT, NBREAK, STKNPT
       INTEGER ISHIFT, I, J, I1, I2
       TSTVAL = -16.1054

       SW1 = MAX(WAVE(1),WAVE(NPOINT))
       SW2 = MIN(WAVE(1),WAVE(NPOINT))
       CW1 = MAX(XSTACK(1),XSTACK(STKNPT))
       CW2 = MIN(XSTACK(1),XSTACK(STKNPT))
       GW1 = MAX(SW2,CW2)
       GW2 = MIN(SW1,CW1)

       CALL REMAP(WAVE,FLUX,NPOINT,XSTACK,WORK,STKNPT,0)
       NPOINT = STKNPT
       IF (FN.EQ.'/') THEN
          DO 50 I = 1, NPOINT
             IF (WORK(I).EQ.0.0) THEN
                FLUX(I) = TSTVAL
             ELSE
                FLUX(I) = YSTACK(I)/WORK(I)
             ENDIF
   50     CONTINUE
       ELSEIF (FN.EQ.'*') THEN
          DO 100 I = 1, NPOINT
             FLUX(I) = YSTACK(I)*WORK(I)
  100     CONTINUE
       ELSEIF (FN.EQ.'-') THEN
          DO 150 I = 1, NPOINT
             FLUX(I) = YSTACK(I) - WORK(I)
  150     CONTINUE
       ELSEIF (FN.EQ.'+') THEN
          DO 200 I = 1, NPOINT
             FLUX(I) = YSTACK(I) + WORK(I)
  200     CONTINUE
       ELSEIF (FN.EQ.'MAX') THEN
          DO 250 I = 1, NPOINT
             FLUX(I) = MAX(YSTACK(I),WORK(I))
  250     CONTINUE
       ELSEIF (FN.EQ.'MIN') THEN
          DO 300 I = 1, NPOINT
             FLUX(I) = MIN(YSTACK(I),WORK(I))
  300     CONTINUE
       ENDIF

       DO 400 JJ = 1, NBREAK - 1
          X1 = WAVE(BREAK(JJ))
          X2 = WAVE(BREAK(JJ)+1)
          TMP = MIN(X1,X2)
          X2 = MAX(X1,X2)
          X1 = TMP


          DO 350 II = 1, NPOINT
             WV = XSTACK(II)
             IF (WV.GT.X1 .AND. WV.LT.X2) THEN
                FLUX(II) = TSTVAL
             ENDIF
  350     CONTINUE
  400  CONTINUE
       DO 500 I = 1, NPOINT
          WAVE(I) = XSTACK(I)
          IF (WAVE(I).LT.GW1 .OR. WAVE(I).GT.GW2) THEN
             FLUX(I) = TSTVAL
          ENDIF
  500  CONTINUE


       DO 600 I = 1, NBSTCK
          BREAK(I) = BSTACK(I)
  600  CONTINUE
       NBREAK = NBSTCK

       OK = .TRUE.
       IF (GW2.LT.GW1) THEN
          NPOINT = 0
          NBREAK = 0
          BREAK(1) = 0
          WAVE(1) = 0.0
          FLUX(1) = 0.0
          GOTO 700
       ENDIF

       CALL SRTBRK
     : (ASIZE1,WAVE,FLUX,NPOINT,MAXBRK,BREAK,NBREAK,TSTVAL, OK)

       IF (.NOT.OK) THEN
          WRITE (*,'(''      WARNING:  error resorting breaks!'')')
       ENDIF

  700  CONTINUE

       END
