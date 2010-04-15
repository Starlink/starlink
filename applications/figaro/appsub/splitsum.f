C+
      FUNCTION SPLITSUM(SUM,FRAC,YVALS,CVALS,XVALS,NSEG,NL,NH,SIGN,
     :  STATUS)
C
C     Computes X value for which area from NL up to this X
C     value is FRAC times total area (SUM).
C     So for FRAC = 0.5 this gives the median.
C     Linear interpolation to get fractional "channel".
C     SUM = total sum of 1 - YVALS/CVALS over NL to NH
C     NML = highest channel s.t.sum from NL to NML is .LE. FRAC*SUM.
C     EXES = (sum from NL to NML+1) - FRAC*SUM
C     EXCH = fraction of channel NML+1 required to be added to sum to
C            to NML to get total exactly = FRAC*SUM
C     SIGN is set to -1.0 for emission lines; +1.0 for normal absorption
C            line case
C     STATUS .NE. 0 after return indicates failure (probably can't happen)
C
C     22nd Jul 1993 HME / UoE, Starlink.  Disuse GKD.
C+
      INTEGER STATUS
      DIMENSION XVALS(NSEG),YVALS(NSEG),CVALS(NSEG)
C
      STATUS=0
      SPLITSUM=0.
      A=0.
      DO I=NL,NH
        IF (CVALS(I).NE.0.) A=A + (1.-YVALS(I)/CVALS(I))*SIGN
        IF(A.GT.FRAC*SUM)THEN
          NML=I-1
          GO TO 38
        END IF
      END DO
      CALL PAR_WRUSER('Unexpected error bombout #1 (SPLITSUM)',STATUS)
      STATUS=1
      RETURN
   38 EXES=A-FRAC*SUM
      IF (CVALS(NML+1).NE.0.) THEN
         EXCH=1. - EXES/ ((1.-YVALS(NML+1)/CVALS(NML+1))*SIGN)
      ELSE
         EXCH=0.
      END IF
      ACH=(XVALS(NSEG)-XVALS(1))/(NSEG-1.)
      XMED=XVALS(NML) + ACH*(EXCH+0.5)
      IF(XMED.LT.(XVALS(NML)+0.5*ACH).OR.XMED.GT.(XVALS(NML+1)+0.5*ACH))
     :                                                              THEN
        STATUS=1
      END IF
      SPLITSUM=XMED
      RETURN
      END
