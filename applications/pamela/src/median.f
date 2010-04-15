      SUBROUTINE MEDIAN(WDATA,NPIX,IPEAK,FWHM,RBIN,SIGBIN,IFLIP)
C
C   This subroutine calculates accurate line positions given a
C   line position 'IPEAK' and a line full-width-half-maximum 'FWHM'.
C   A mean position is calculated by choosing a point which equates the
C   number of counts in the line above and below the given position. This
C   is done with a gaussian derivative kernal function rather than a
C   step function. This gives high stability for confused lines or low
C   signal-to-noise ratio data.
C
C   WDATA  =  R*4 Array containing the data.
C   NPIX   =  I*4. Number of pixels in data array.
C   IPEAK  =  Bin number of peak of line. Need only be within about 'FWHM'
C             of true position.
C   FWHM   =  Full-width half maximum of line to be fed in.
C             If FWHM.LT.2. then a value of 2. is used.
C   RBIN   =  R*4 Returns line position.
C   SIGBIN =  R*4 Returns the ratio of the error of the line
C             position in bins to the error in the data number
C             for a pixel in the continuum.
C             Returns a negative number if failure occurs
C   IFLIP  =  I*4 1 for emission lines, -1 for absorption.
C
C TRM 09/03/98
C
C Modified so it actually uses the FWHM value entered rather than
C 0.8 times it as it used to do.
C
      IMPLICIT NONE
      INTEGER NPIX, IPEAK, IFLIP, ROUND
      REAL RBIN, SIGBIN, WDATA(NPIX), CNT(11), X, YW
      REAL EPS, CARE, FSIG, FWHM, RSIG, DEV, FACT, RMU
      REAL RAT, EXPF, CHAMP, FWFAC
      INTEGER IWID, ICHAMP, JPEAK, ISEARCH, IP, II, IQ
      INTEGER IT, IB1, IB2, I
      DOUBLE PRECISION SUM, RX1, RX2
      SAVE FWFAC, EPS
      DATA FWFAC/2.3548/
      DATA EPS/1.E-5/
C
      CARE = 1.
      FSIG = MAX(2., FWHM)
      RSIG = FSIG/FWFAC
      IWID = NINT((FSIG/FWFAC)*3.)
C
C     Hunt for local maximum in cross-correlation
C
      ICHAMP  = IPEAK
      ROUND   = 0
 13   JPEAK   = ICHAMP
      ROUND   = ROUND + 1
      ISEARCH = MAX(2, MIN(5, IWID))
      DO IP=JPEAK-ISEARCH,JPEAK+ISEARCH
         II=IP-JPEAK+ISEARCH+1
         SUM = 0.D0
         DO IQ = MAX(1,IP-IWID), MIN(NPIX,IP+IWID)
            FACT = EXP(-(REAL(IQ-IP)/RSIG)**2/2.)
            SUM  = SUM + WDATA(IQ)*FACT
         END DO
         CNT(II) = REAL(SUM)
      END DO
      CHAMP  = CNT(1)
      ICHAMP = JPEAK-ISEARCH
      DO II = 2, 2*ISEARCH+1
         IF (CNT(II)*IFLIP.GE.CHAMP*IFLIP) THEN
            CHAMP = CNT(II)
            ICHAMP= JPEAK+II-ISEARCH-1
         END IF
      END DO
C
C     If maximum occurs at end of search range, then have
C     another go. If it gets to end of array give up, and if
C     the loop has been traversed too many times, give up.
C
      IF (ABS(ICHAMP-JPEAK).EQ.ISEARCH) THEN
         IF(ICHAMP.EQ.1 .OR. ICHAMP.EQ.NPIX .OR.
     &        ROUND.GT.10) THEN
            SIGBIN = -1.E10
            RETURN
         ELSE
            GOTO 13
         END IF
      END IF
C
      JPEAK = ICHAMP
      RMU   = REAL(JPEAK)
C
C   Solve line-position equation by Newton's method.
C
      IT  = 0
      DEV = EPS + 1.
      DO WHILE(ABS(DEV).GT.EPS .AND. IT.LT.100)
         IT = IT + 1
         IF(MOD(IT,10).EQ.0) CARE=0.95*CARE
         JPEAK = NINT(RMU)
         IB1   = MAX(1,JPEAK-IWID)
         IB2   = MIN(NPIX,JPEAK+IWID)
         RX1   = 0.D0
         RX2   = 0.D0
         DO I=IB1,IB2
            X   = REAL(I)-RMU
            RAT = (X/RSIG)**2
            YW  = WDATA(I)*EXP(-0.5*RAT)
            RX1 = RX1 + DBLE(YW*X)
            RX2 = RX2 + DBLE(YW*(RAT-1.))
         END DO
         IF(RX2.NE.0.D0) THEN
            DEV = REAL(- CARE*MAX(-3.,MIN(RX1/RX2,3.)))
         ELSE
            DEV = - 1.
         END IF
         RMU = RMU + DEV
      END DO
      RBIN = RMU
C
C     Calculate error in bin position (assuming WDATA measured
C     in counts)
C
      RX1=0.D0
      RX2=0.D0
      DO I=IB1,IB2
         X   = REAL(I)-RBIN
         RAT = (X/RSIG)**2
         EXPF= EXP(-RAT*0.5)
         RX1 = RX1+ABS(WDATA(I))*(X*EXPF)**2
         RX2 = RX2+WDATA(I)*(RAT-1.)*EXPF
      END DO
      SIGBIN=0.
      IF(RX2*RX2.NE.0.D0) SIGBIN=REAL(SQRT(RX1/(RX2*RX2)))
      RETURN
      END
