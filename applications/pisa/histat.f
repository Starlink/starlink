      SUBROUTINE HISTAT(IHIST,MODE,MAXH,XMEAN,XPEAK,FSIGMA,GSIGMA,N)
C     *** computes statistics of histogram

C  Changes:
C     17-JUN-1995: P.W.Draper
C        Code transformed by TOOLPACK.

C     .. Scalar Arguments ..
      REAL FSIGMA,GSIGMA,XMEAN,XPEAK
      INTEGER MAXH,MODE,N
C     ..
C     .. Array Arguments ..
      INTEGER IHIST(N)
C     ..
C     .. Local Scalars ..
      REAL COG,COGD,COGN,CONV,RTTPI,SD,SIGMA,SIGMSQ,TEMP,XCOUNT,XMED,
     +     XNUMB,XX
      INTEGER I,ICOUNT,IHIH,IHSUM,IHTOT,II,ILIM,ILOW,ISIGM,IWID,IXPEAK,
     +     J,JJ,K,M,NFILT,NN,NOFF
C     ..
C     .. Local Arrays ..
      REAL BUF(1024),XPAR(1024)

      RTTPI = SQRT(8.0*ATAN(1.0))
      XCOUNT = 0.0
C     *** first get no. of data values in histogram
      DO 10 I = 1,N
         II = N + 1 - I
         IF (IHIST(II).NE.0) GOTO 20
 10   CONTINUE

 20   NN = II
      IHTOT = 0
      MAXH = 0
      MODE = NN
      DO 30 I = 1,NN
         IHTOT = IHTOT + IHIST(I)
 30   CONTINUE
C     *** trap for saturation or holes
C     *** only look for mode in 10% to 90% range if possible
      IHSUM = 0
      DO 40 I = 1,NN
         IHSUM = IHSUM + IHIST(I)
         IF (IHSUM.GE.0.1*IHTOT .AND. IHSUM.LE.0.9*IHTOT) THEN
            IF (IHIST(I).GT.MAXH) THEN
               MAXH = IHIST(I)
               MODE = I
            ENDIF
         ENDIF
 40   CONTINUE
C     *** in case all points in 1 bin
      IF (MAXH.EQ.0) MAXH = IHIST(NN)
      XMEAN = 0.0
      SIGMSQ = 0.0
      DO 50 I = 1,NN
         TEMP = FLOAT(IHIST(I))
         XCOUNT = XCOUNT + TEMP
         XMEAN = XMEAN + FLOAT(I)*TEMP
         SIGMSQ = SIGMSQ + FLOAT(I)**2*TEMP
 50   CONTINUE
      IF (MAXH.EQ.0) THEN
         MODE = -9999
         XMEAN = XMEAN/XCOUNT
         XPEAK = -9999.0
         SIGMA = 0.0
         GSIGMA = 0.0

      ELSE
C     *** median
         XMED = 0.0
         DO 60 I = 1,NN
            XMED = XMED + IHIST(I)
            IF (XMED.GT.0.5*XCOUNT) GOTO 70
 60      CONTINUE

         GOTO 80

 70      XMED = I
 80      XMEAN = XMEAN/XCOUNT
         SIGMA = SQRT(SIGMSQ/XCOUNT-XMEAN**2)
C     *** parabolic interpolation to find maximum
         GSIGMA = XCOUNT/ (FLOAT(MAXH)*RTTPI)
         IF (GSIGMA.GT.0.1*FLOAT(NN)) GSIGMA = 0.1*FLOAT(NN)
         ISIGM = MAX0(3,INT(GSIGMA+0.5))
         ISIGM = MIN0(50,ISIGM)
         NFILT = ISIGM*4 + 1
         NOFF = (NFILT+1)/2
C     *** form filter array
         CONV = 0.5/FLOAT(ISIGM**2)
         DO 90 I = 1,NOFF
            XX = I - NOFF
            BUF(I) = EXP(-CONV*XX**2)
            BUF(NFILT+1-I) = BUF(I)
 90      CONTINUE
         XPEAK = FLOAT(MODE)
         DO 100 K = 1,2
C     *** make sure ixpeak bounded by data range
            IXPEAK = MIN(NN,INT(XPEAK+0.5))
            IXPEAK = MAX(1,IXPEAK)
            ILOW = MAX(1,IXPEAK-ISIGM)
            IHIH = MIN(NN,IXPEAK+ISIGM)
            M = IHIH - ILOW + 1
            II = 0
            COGD = 0.0
            COGN = 0.0
            DO 110 I = ILOW,IHIH
               COGD = COGD + IHIST(I)
               COGN = COGN + IHIST(I)*FLOAT(I)
               TEMP = 0.0
               XNUMB = 0.0
               DO 120 J = 1,NFILT
                  JJ = J - NOFF
                  IF (I+JJ.GE.1 .AND. I+JJ.LE.NN) THEN
                     TEMP = TEMP + IHIST(I+JJ)*BUF(J)
                     XNUMB = XNUMB + BUF(J)
                  ENDIF

 120           CONTINUE
               IF (XNUMB.GT.0.0) THEN
                  TEMP = TEMP/XNUMB

               ELSE

                  TEMP = 0.0
               ENDIF

               II = II + 1
               XPAR(II) = ALOG(AMAX1(1.0,TEMP))
 110        CONTINUE
            CALL PARBOL(XPAR,M,XPEAK,SD)
            FSIGMA = SQRT(AMAX1(0.0,SD**2-ISIGM**2))
            XPEAK = XPEAK + FLOAT(ILOW-1)
            IF (COGD.GT.0.5) THEN
               COG = COGN/COGD

            ELSE

               COG = XPEAK
            ENDIF

            IF (ABS(XPEAK-MODE).GT.GSIGMA) XPEAK = COG
 100     CONTINUE
         MODE = MODE - 1
         XPEAK = XPEAK - 1.0
         XMEAN = XMEAN - 1.0
         XMED = XMED - 1.0
C     *** another trap for saturation or holes
         IF (XPEAK.GT.NN-GSIGMA) XPEAK = XMED
         IF (XPEAK.LT.GSIGMA) XPEAK = XMED
C     *** find sigma by 68% cutoff level
         IXPEAK = INT(XPEAK+1.5)
         IWID = 0
         ICOUNT = IHIST(IXPEAK)
         ILIM = INT(0.68*XCOUNT+0.5)
         DO 130 I = 1,NN
            II = IXPEAK + I
            IF (II.LE.NN) THEN
               IWID = IWID + 1
               ICOUNT = ICOUNT + IHIST(II)
               IF (ICOUNT.GT.ILIM) GOTO 140
            ENDIF

            II = IXPEAK - I
            IF (II.GE.1) THEN
               IWID = IWID + 1
               ICOUNT = ICOUNT + IHIST(II)
            ENDIF

            IF (ICOUNT.GT.ILIM) GOTO 140
 130     CONTINUE
 140     GSIGMA = 0.5*IWID
      ENDIF

      END

