      REAL FUNCTION CHANCE(OBS, PRED, SIGMA, THRLO, THRHI)
*
*
* Written by T.R. Marsh
*
* Designed to reject according to Poisson statistics (Gaussian
* in the large signal limit). For small numbers, the Poisson
* distribution is significantly different from a Gaussian
* and Gaussian rejection is too severe.
*
* Warning: CHANCE distinguishes between Gaussian and Poisson on the
*          basis of the size of SIGMA**2. This is not scale independent
*          i.e. if OBS, PRED and SIGMA were all multiplied by some factor
*          the result could change. You should scale the inputs correctly.
*          In these cases it uses an observed count of
*          NDATA = MAX(0,NINT(OBS - PRED + SIGMA*SIGMA))
*          tested against a Poisson distribution with parameter SIGMA*SIGMA
*          If OBS=PRED this gives the integer closest to SIGMA*SIGMA which
*          has a high probability. This also allows for constant offsets
*          in OBS and PRED. If PRED = SIGMA*SIGMA as it would for an exact
*          Poisson setup then NDATA = NINT(OBS) as it should.
*
*           If OBS + SIGMA*SIGMA << PRED NDATA floors at 0 and the test
*          becomes insensitive to OBS. This can lead to acceptance of
*          almost any deviation. This is because this is an unrealistic
*          situation. A true Poisson deviate could never produce an observed
*          value with this property and thus CHANCE is being misused. A
*          warning to this effect is given.
*
* Given a variable with predicted value PRED, observed OBS
* and uncertainty SIGMA this routine determines whether
* OBS lies further than THRLO below or THRHI above PRED
* and if it does, it comes back with -LOG(of the ratio of the
* probability of observing OBS/divided by the probability
* of observing the relevant threshold) (that is THRLO if
* OBS .lt. PRED, or else THRHI). If not rejected it does not
* compute the ratio (for speed) and it returns with -1.
* If SIGMA .LT.0. it returns with -2.
*
* 19/07/90 TRM @STSCI fixed bug that led to incorrect rejection when
*          observed data value greater than expected in Poisson case
* 19/07/90 trm @STSCI now always returns LOG rather than old style way
* 16/08/96 fixed bug that led to certain deviates being accepted and
*          changed to using LOG throughout so that large THRLO and THRHI
*          values will not cause problems
* 23/01/98 Swapped S15ACF to PDA_DERFC
*
      IMPLICIT NONE
      INTEGER I, NDATA
      REAL OBS, PRED, SIGMA, THRLO, THRHI, PROBLO, PROBHI
      REAL OLDTHRLO, OLDTHRHI, POISS_PARAM, PLIM, P, Q
      REAL SUM, QFACTOR
*
* PDA_DERFC replaces NAG routine S15ACF used to compute the integral
* from X to infinity of EXP(-X**2/2)/SQRT(2*PI)
*
* In fact: S15ACF(X,IFAIL) = PDA_DERFC(X/SQRT(2.))/2
*
      DOUBLE PRECISION PDA_DERFC, DEVIATE
      DATA OLDTHRLO, OLDTHRHI, PROBLO, PROBHI/1., -1., 0., 0./
      DATA PLIM/5./
*
* Automatic rejection if sigma is < 0
*
      IF(SIGMA.LE.0.) THEN
        CHANCE = -2.
        GOTO 999
      END IF
*
* Compute LOG(rejection probability) every time THRLO or THRHI
* are altered. Use an approximation for large THRLO or THRHI
* (see later for more about this)
*
      IF(THRLO.NE.OLDTHRLO) THEN
         IF(ABS(THRLO).LT.10.) THEN
            PROBLO = SNGL(LOG(PDA_DERFC(DBLE(ABS(THRLO))/
     &               SQRT(2.))/2.))
         ELSE
            PROBLO = - THRLO**2/2. - 0.9189 - LOG(ABS(THRLO))
         END IF
         OLDTHRLO = THRLO
      END IF
      IF(THRHI.NE.OLDTHRHI) THEN
         IF(ABS(THRHI).LT.10.) THEN
            PROBHI = SNGL(LOG(PDA_DERFC(DBLE(ABS(THRHI))/
     &               SQRT(2.))/2.))
         ELSE
            PROBHI = - THRHI**2/2. - 0.9189 - LOG(ABS(THRHI))
         END IF
         OLDTHRHI = THRHI
      END IF
*-------------------------------------------------------
*
* Gaussian rejection section
*
* If gaussian does not reject the point, then Poisson
* won't, so jump out of program if its OK for Gaussian.
*
      IF(OBS-PRED.GT.THRLO*SIGMA .AND. OBS-PRED.LT.THRHI*SIGMA) THEN
        CHANCE = -1.
        GOTO 999
      END IF
      POISS_PARAM = SIGMA*SIGMA
*
* Assume Gaussian if parameter of Poisson
* distribution is greater than PLIM. Since the
* variable has been passed by previous test, it
* must be rejected if it passes the test below.
*
      IF(POISS_PARAM.GT.PLIM) THEN
         DEVIATE = (OBS-PRED)/SIGMA
         IF(ABS(DEVIATE).LT.10) THEN
            CHANCE = SNGL(LOG(PDA_DERFC(ABS(DEVIATE)/SQRT(2.))/2.))
            IF(DEVIATE.LT.0.D0) THEN
               CHANCE = PROBLO - CHANCE
            ELSE
               CHANCE = PROBHI - CHANCE
            END IF
         ELSE
*
* For large deviation (>10), approximate by
* CHANCE = exp(-X**2/2)/sqrt(2*pi)/X. At worst
* this overestimates CHANCE by 1%.
*
*
            CHANCE = REAL(DEVIATE**2/2.+0.9189 + LOG(ABS(DEVIATE)))
            IF(DEVIATE.LT.0.D0) THEN
               CHANCE = CHANCE + PROBLO
            ELSE
               CHANCE = CHANCE + PROBHI
            END IF
         END IF
         GOTO 999
      END IF
*
* End of Gaussian rejection section
*---------------------------------------------------------
*
* Poisson section
*
* Scale to give a Poisson variate with
* Poisson parameter = SIGMA*SIGMA
*
c      IF(OBS + POISS_PARAM + 1 .LT. PRED) THEN
c         WRITE(*,*) '** You are making dubious use of CHANCE. **'
c         WRITE(*,*) '** Input numbers are not Poisson **'
c      END IF
      NDATA = MAX(0,NINT(OBS - PRED + POISS_PARAM))
      CHANCE = -1.
*
* Case 1: Observed is less than Poisson parameter
*
      IF(REAL(NDATA).LT.POISS_PARAM) THEN
        IF(NDATA.EQ.0 .AND. POISS_PARAM.GT.-PROBLO) THEN
          CHANCE = PROBLO+POISS_PARAM
          GOTO 999
        END IF
        Q = EXP(-POISS_PARAM)
        P = Q
        I = 1
        DO WHILE(LOG(P).LE.PROBLO .AND. I.LE.NDATA)
          Q = Q*POISS_PARAM/REAL(I)
          P = P + Q
          I = I + 1
        END DO
        IF(LOG(P).LE.PROBLO) CHANCE = PROBLO - LOG(P)
*
* Case 2: More than Poisson parameter observed
*
      ELSE
*
        P = -POISS_PARAM+NDATA*LOG(POISS_PARAM)-QFACTOR(NDATA)
        CHANCE = EXP(P-PROBHI)
        I = NDATA + 1
        Q = 1.
        SUM = 1.
        DO WHILE(SUM*CHANCE.LE.1. .AND. I.LE.NDATA+NINT(PLIM))
          Q = Q*POISS_PARAM/REAL(I)
          SUM = SUM + Q
          I = I + 1
        END DO
*
* Next line bug fix 19/7/90
*
        IF(SUM*CHANCE.GT.1.) THEN
          CHANCE = -1.
        ELSE
          CHANCE = PROBHI - P - LOG(SUM)
        END IF
      END IF
999   RETURN
      END


