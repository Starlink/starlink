*+TIM_AOV  -  Period folding routine
      SUBROUTINE TIM_AOV(NDATA, TIME, DATA, NFREQ, F0, DF, NPBIN,
     &                                       PH, XPER, AOV, BESTPH)
*    Description :
*     Performs period folding over a range of frequencies on a data
*     array supplied to the routine by the method of one-way analysis
*     of variance. Works on unevenly spaced data.
*    Method :
*     Uses: the method of A. SCHWARZENBERG-CZERNY, MNRAS 1989, V241, 153.
*    History :
*     FEB 23 90   Author: D. HOLMGREN,
*     Jan 10 91   converted to Asterix   (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
*    Import :
      INTEGER NDATA               !Number of data points
      REAL TIME(NDATA)            !Time stamp of each point
      REAL DATA(NDATA)            !Data value at each point
      INTEGER NFREQ               !Number of frequencies to try
      REAL F0                     !Initial frequency
      REAL DF                     !Frequency increment
      INTEGER NPBIN               !Number of phase bins to use
      REAL PH(NDATA)              !Workspace used by this routine to store
*                                 !the phase binvalues at a given frequency
*    Import-Export :
*    Export :
      REAL XPER(NFREQ)            !Periods searched
      REAL AOV(NFREQ)             !Significance at each frequency
      REAL BESTPH(NPBIN)          !Phase curve at best fit period
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
*     <declarations for local variables>
*-
      REAL NG(50),GMEAN(50),BIN(50)
      REAL AV                     !Average data value
      REAL TREF                   !Average time or reference epoch
      REAL VAR,TVAR               !Variance of above averages
      REAL DPH                    !Period increment
      REAL P                      !Period
      REAL F                      !Frequency
      REAL PHASE                  !Phase value
      REAL S1,S2,SUM
      REAL THAOV                  !Fit statistic
      REAL AMIN,AMAX              !Min and max fit statistics
      REAL PMAX                   !Best fit period
      REAL DIFF
      INTEGER IDF1,IDF2
      INTEGER I,J,H,II,JJ,N
      INTEGER IPH,NMAX
      CHARACTER CH
*
* Find appropriate degrees of freedom for F DISTRIBUTION.
* also find DATA MEAN and VARIANCE (AVEVAR IS FROM NUMERICAL
* RECIPES). TREF is a reference epoch.
      IDF1=NPBIN-1
      IDF2=NDATA-NPBIN
*
      CALL TIM_AVEVAR(DATA,NDATA,AV,VAR)
      CALL TIM_AVEVAR(TIME,NDATA,TREF,TVAR)
*
      DPH=1./REAL(NPBIN)
*
* Set up phase bins.
      DO I=1,NPBIN+1
         BIN(I)=(I-1)*DPH
      ENDDO
*
      AMIN=100.
      AMAX=0.
*
* Now do period folding and AOV test for each period.
* See A. SCHWARZENBERG-CZERNY, MNRAS 1989, V241, 153.
      DO N=1,NFREQ
*
*   Set frequency - such that periods increase with N.
         F = F0 + (NFREQ-N) * DF
*
*   Calculate period
         P=1./F
*
*   Initialise arrays
         DO I=1,NPBIN
            NG(I)=0
            GMEAN(I)=0.
         ENDDO
*
         DO J=1,NDATA
            PHASE=(TIME(J)-TREF)/P
            IPH=PHASE
            PHASE=PHASE-REAL(IPH)
            IF(PHASE.LT.0.)PHASE=PHASE+1.
            PH(J)=PHASE
         ENDDO
*
         DO I=1,NPBIN
            DO J=1,NDATA
*
               IF ((PH(J).GE.BIN(I)).AND.(PH(J).LT.BIN(I+1))) THEN
*
                  NG(I)=NG(I)+1
                  GMEAN(I)=GMEAN(I)+DATA(J)
*
               ENDIF
            ENDDO
         ENDDO
*
         DO I=1,NPBIN
            IF ( NG(I) .NE. 0 ) THEN
               GMEAN(I) = GMEAN(I)/REAL(NG(I))
            ENDIF
         ENDDO
*
         S1=0.
*
         DO I=1,NPBIN
            S1=S1+REAL(NG(I))*(GMEAN(I)-AV)*(GMEAN(I)-AV)
         ENDDO
*
         S1=S1/REAL(IDF1)
         S2=0.
*
         DO I=1,NPBIN
            SUM=0.
            DO II=1,NPBIN
               DO JJ=1,NDATA
                  IF ((PH(JJ).GE.BIN(II)).AND.(PH(JJ).LT.BIN(II+1)))THEN
                     DIFF = DATA(JJ) - GMEAN(I)
                     SUM = SUM + DIFF * DIFF
                  ENDIF
               ENDDO
            ENDDO
*
            S2=S2+SUM
*
         ENDDO
*
         S2=S2/REAL(IDF2)
*
* THAOV is the test statistic, which is directly related
* to the F-DISTRIBUTION.
         THAOV=S1/S2
*
         XPER(N)=P
*
         AOV(N)=THAOV
*
* Keep track of maximum and minimum
         IF (THAOV .GT. AMAX) THEN
            NMAX=N
            AMAX=THAOV
*
*    Store best phase curve
            DO I=1,NPBIN
               BESTPH(I) = GMEAN(I)
            ENDDO
*
         ENDIF
*
         IF (THAOV .LT. AMIN) AMIN=AOV(N)
*
      ENDDO                          ! End of frequency loop
*
* Set max frequency
      PMAX=XPER(NMAX)
*
      CALL MSG_SETR('PMAX', PMAX)
      CALL MSG_SETR('AMAX', AMAX)
      CALL MSG_PRNT('Best fit period ^PMAX with amplitude ^AMAX')
*
* Size and plot THAOV(P)=AOV(N).
      CALL PGBEGIN(0,'?',1,1,1)
      CALL PGENV(XPER(1),XPER(NFREQ),AMIN,AMAX,0,0)
      CALL PGLABEL('Period','\gH\dAOV\u','AOV Periodogram')
      CALL PGLINE(NFREQ,XPER,AOV)
      CALL PGEND
*
      END
