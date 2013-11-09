*+TIM_FASPER   -   Calculates power spectra with error estimates
      SUBROUTINE TIM_FASPER(NDATA, TIME, DATA, OFAC, HIFAC, F1, W,
     &                      NWORK, NWIND, XW, WINDOW, FREQ, POWER,
     &                                               DF, NOUT, PROB)
*    Description :
*       Calculates power spectra with error estimates. Doesn't require any
*       zero padding of data to stretch the frequency range
*    Method :
*       Uses PRESS and RYBICKI'S FASPER routine
*                       (ASTROPHYS. J. V338, 277, 1989)
*    History :
*       Feb 18 1990  Author: D. HOLMGREN  (QUVAD::DNH)
*       June 8 1990  Window function added (QUVAD::DHN)
*       Jan 15 1991  Converted to Asterix  (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
*    Import :
      INTEGER NDATA                 !No of input data points
      REAL TIME(NDATA)              !Times of data points
      REAL DATA(NDATA)              !Data values
      REAL OFAC                     !Factor to oversample the data (typically 4)
      REAL HIFAC                    !No of Nyquist frequencies to go out to
      REAL F1                       !Frequency of window function
     &                              !  ( =0.0 if not wanted )
      REAL W(NDATA)                 !Workspace for window function
      INTEGER NWORK                 !Size of workspace areas
      INTEGER NWIND                 !Size of window arrays (=1 if not wanted)
*    Import-Export :
      REAL XW(NWIND)                !Workspace for window fn.
      REAL WINDOW(NWIND)            !Window output function
      REAL FREQ(NWORK)              !Frequencies used as workspace
      REAL POWER(NWORK)             !Power spectrum. Used as workspace; only
*                                   !the first NOUT values contain the spectrum
*    Export :
      REAL DF                       !Frequency increment
      INTEGER NOUT                  !No of output frequencies
      REAL PROB                     !Signif. estimate of peak frequency. A small
*                                   !value indicates a good periodic signal.
*    Local constants :
      REAL PI,TWOPI
           PARAMETER (PI=3.1415927, TWOPI=PI*2.0)
*    Local variables :
      INTEGER J
      REAL TREF                     !Reference time
      REAL EN
      INTEGER JMAX
      REAL PMAX,FMAX                !Peak frequency index,amplitude and freq.
      REAL WMAX                     !Peak window value
*-
*  Find reference epoch for data
      TREF=0.0D0
      EN = DFLOAT(NDATA)
*
      DO J=1,NDATA
         TREF = TREF + TIME(J)
      ENDDO
*
      TREF=TREF/EN
*
* Set up synthetic data for window function if required.
* This is a Sine of unit amplitude and zero phase
      IF (F1.GT.0.0) THEN
*
         DO J=1,NDATA
            W(J) = SIN( TWOPI * F1 * TIME(J) )
         ENDDO
*
      ENDIF
*
* Compute power spectrum
      CALL TIM_FASPER_DOIT(TIME, DATA, NDATA, OFAC, HIFAC, FREQ,
     &                             POWER, NWORK, NOUT, JMAX, PROB)
*
      PMAX=POWER(JMAX)
      FMAX=FREQ(JMAX)
*
* Tell user peak period
      CALL MSG_SETR('FMAX', FMAX)
      CALL MSG_SETR('PMAX', PMAX)
      CALL MSG_SETR('PROB', PROB)
      CALL MSG_PRNT('Peak found at ^FMAX of amplitude ^PMAX '/
     &             /'and significance ^PROB')
*
* Find power spectrum of window function if required. i.e. if F1 > 0
      IF (F1 .GT. 0.0) THEN
*
         CALL TIM_FASPER_DOIT(TIME, W, NDATA, OFAC, HIFAC, XW,
     &                          WINDOW, NWORK, NOUT, JMAX, PROB)
*
         WMAX=WINDOW(JMAX)
      ENDIF
*
* Do plots
      IF (F1 .GT. 0.0) THEN
         CALL PGBEGIN(0,'?',1,2)
      ELSE
         CALL PGBEGIN(0,'?',1,1)
      ENDIF
*
      CALL PGENV(FREQ(1),FREQ(NOUT),0.,PMAX,0,0)
      CALL PGLABEL('Frequency','Spectral Power','Power Spectrum')
      CALL PGLINE(NOUT,FREQ,POWER)
*
      IF (F1 .GT. 0.0) THEN
         CALL PGENV(XW(1),XW(NOUT),0.,WMAX,0,0)
         CALL PGLABEL('Frequency','Spectral Power','Window Function')
         CALL PGLINE(NOUT,XW,WINDOW)
      ENDIF
*
      CALL PGEND
*
* Calculate the frequency increment
      DF = FREQ(2) - FREQ(1)
*
      END
C
C SUBROUTINE FASPER
C SEE PRESS AND RYBICKI FOR DOCUMENTATION.
C
      SUBROUTINE TIM_FASPER_DOIT(X, Y, N, OFAC, HIFAC, WK1, WK2,
     &                  NWK, NOUT, JMAX, PROB)
      PARAMETER (MACC=4)
      REAL X(N),Y(N),WK1(NWK),WK2(NWK)
      NOUT=0.5*OFAC*HIFAC*N
C
C SIZE FFT AS NEXT POWER OF 2 ABOVE NFREQT
C
      NFREQT=OFAC*HIFAC*N*MACC
      NFREQ=64
1     IF(NFREQ.LT.NFREQT)THEN
       NFREQ=NFREQ*2
      GO TO 1
      ENDIF
      NDIM=2*NFREQ
      IF(NDIM.GT.NWK) PAUSE 'Workspaces too small in FASPER !'
      CALL TIM_AVEVAR(Y,N,AVE,VAR)
      XMIN=X(1)
      XMAX=XMIN
      DO 11 J=2,N
        IF(X(J).LT.XMIN)XMIN=X(J)
        IF(X(J).GT.XMAX)XMAX=X(J)
11    CONTINUE
      XDIF=XMAX-XMIN
      DO 12 J=1,NDIM
       WK1(J)=0.
       WK2(J)=0.
12    CONTINUE
      FAC=NDIM/(XDIF*OFAC)
      FNDIM=NDIM
C
C EXTIRPOLATE THE DATA INTO THE WORKSPACES
C
      DO 13 J=1,N
        CK=1.+AMOD((X(J)-XMIN)*FAC,FNDIM)
        CKK=1.+AMOD(2.*(CK-1.),FNDIM)
        CALL TIM_SPREAD(Y(J)-AVE,WK1,NDIM,CK,MACC)
        CALL TIM_SPREAD(1.,WK2,NDIM,CKK,MACC)
13    CONTINUE
C
C TAKE THE FAST FOURIER TRANSFORMS
C
      CALL TIM_REALFT(WK1,NFREQ,1)
      CALL TIM_REALFT(WK2,NFREQ,1)
      DF=1./(XDIF*OFAC)
      K=3
      PMAX=-1.
C
C COMPUTE LOMB-SCARGLE VALUE FOR EACH FREQUENCY
C
      DO 14 J=1,NOUT
      HYPO=SQRT(WK2(K)**2+WK2(K+1)**2)
      HC2WT=0.5*WK2(K)/HYPO
      HS2WT=0.5*WK2(K+1)/HYPO
      CWT=SQRT(0.5+HC2WT)
      SWT=SIGN(SQRT(0.5-HC2WT),HS2WT)
      DEN=0.5*N+HC2WT*WK2(K)+HS2WT*WK2(K+1)
      CTERM=(CWT*WK1(K)+SWT*WK1(K+1))**2/DEN
      STERM=(CWT*WK1(K+1)-SWT*WK1(K))**2/(N-DEN)
      WK1(J)=J*DF
      WK2(J)=(CTERM+STERM)/(2.*VAR)
      IF(WK2(J).GT.PMAX)THEN
        PMAX=WK2(J)
        JMAX=J
      ENDIF
      K=K+2
14    CONTINUE
C
C ESTIMATE SIGNIFICANCE OF LARGEST PEAK - A SMALL VALUE
C OF PROB INDICATES THAT A PERIODIC SIGNAL IS PRESENT.
C
      EXPY=EXP(-PMAX)
      EFFM=2.*NOUT/OFAC
*
      PROB=EFFM*EXPY
      IF(PROB.GT.0.01)PROB=1.-(1.-EXPY)**EFFM
*
      RETURN
      END
      SUBROUTINE TIM_SPREAD(Y,YY,N,X,M)
C
C EXTIRPOLATION SUBROUTINE
C
      REAL YY(N), NFAC(10) /1,1,2,6,24,120,720,5040,40320,
     -362880/
      IF(M.GT.10)PAUSE 'factorial table too small in SPREAD'
      IX=X
      IF(X.EQ.FLOAT(IX))THEN
       YY(IX)=YY(IX)+Y
      ELSE
       ILO=MIN(MAX(INT(X-0.5*M+1.0),1),N-M+1)
       IHI=ILO+M-1
       NDEN=NFAC(M)
       FAC=X-ILO
       DO 15 J=ILO+1,IHI
         FAC=FAC*(X-J)
15     CONTINUE
       YY(IHI)=YY(IHI)+Y*FAC/(NDEN*(X-IHI))
       DO 16 J=IHI-1,ILO,-1
         NDEN=(NDEN/(J+1-ILO))*(J-IHI)
         YY(J)=YY(J)+Y*FAC/(NDEN*(X-J))
16     CONTINUE
      ENDIF
      RETURN
      END
