
      SUBROUTINE PERIOD_PERFORMPDM(XDATA, YDATA, NDATA, NBIN, WBIN,
     :                             VARI, PDM, MAXPTS, IFAIL, NPTS,
     :                             SAMPLE, SVAR)

C===========================================================================
C This routine calculates the PDM statistic of XDATA(NDATA), YDATA(NDATA),
C following the method described by Stellingwerf (1978, APJ, 224, 953).
C Basically, the data is folded on a trial frequency and split up into
C NBIN samples, each of width WBIN. The PDM statistic is then calculated
C by estimating the mean and variance of each sample, calculating the
C overall variance for all of the samples and then dividing this by the
C variance of the whole dataset. If there is an error IFAIL = 1, otherwise
C IFAIL = 0.
C
C Essentially written by Vikram Singh Dhillon @LPO 3-March-1993.
C
C Converted to Double Precision (KPD), August 2001
C Modified to incorporate dynamic memory allocation for major
C  data/work array(s) and/or use of such arrays (KPD), October 2001
C===========================================================================

      IMPLICIT NONE

C-----------------------------------------------------------------------------
C PERIOD_PDM declarations.
C-----------------------------------------------------------------------------

      INTEGER NDATA, NBIN, IFAIL, MAXPTS, I, J, K
      INTEGER NPTS(NBIN)
      DOUBLE PRECISION XDATA(NDATA), YDATA(NDATA)
      DOUBLE PRECISION WBIN, VARI, PDM
      DOUBLE PRECISION BINWID, CENBIN, MINBIN, MAXBIN
      DOUBLE PRECISION NOM, DENOM
      DOUBLE PRECISION AVE, ADEV, SDEV
      DOUBLE PRECISION SAMPLE(NDATA*3), SVAR(NBIN)


C-----------------------------------------------------------------------------
C Loop through the NBIN samples.
C-----------------------------------------------------------------------------

      DO 30 I = 1, NBIN

C-----------------------------------------------------------------------------
C Determine the limits of the bin.
C-----------------------------------------------------------------------------

         BINWID = 1.0D0/DFLOAT(NBIN)
         CENBIN = (DFLOAT(I)*BINWID) - (0.5D0*BINWID)
         MINBIN = CENBIN - (0.5D0*WBIN)
         MAXBIN = CENBIN + (0.5D0*WBIN)

C-----------------------------------------------------------------------------
C Loop through the phases and store those that fall in the bin.
C-----------------------------------------------------------------------------

         NPTS(I) = 0
         DO 20 K = 1, 3
            DO 10 J = 1, NDATA
               IF ( K.EQ.1 ) THEN
                  IF ( XDATA(J)-1.0D0.LE.MAXBIN ) THEN
                     IF ( XDATA(J)-1.0D0.GT.MINBIN ) THEN
                        NPTS(I) = NPTS(I) + 1
                        SAMPLE(NPTS(I)) = YDATA(J)
                     END IF
                  END IF
               ELSE IF ( K.EQ.2 ) THEN
                  IF ( XDATA(J).LE.MAXBIN ) THEN
                     IF ( XDATA(J).GT.MINBIN ) THEN
                        NPTS(I) = NPTS(I) + 1
                        SAMPLE(NPTS(I)) = YDATA(J)
                     END IF
                  END IF
               ELSE IF ( K.EQ.3 ) THEN
                  IF ( XDATA(J)+1.0D0.LE.MAXBIN ) THEN
                     IF ( XDATA(J)+1.0D0.GT.MINBIN ) THEN
                        NPTS(I) = NPTS(I) + 1
                        SAMPLE(NPTS(I)) = YDATA(J)
                     END IF
                  END IF
               END IF
  10        CONTINUE
  20     CONTINUE

C-----------------------------------------------------------------------------
C Calculate the mean and variance of the sample and store the variance.
C-----------------------------------------------------------------------------

         IF ( NPTS(I).GT.1 ) THEN

            CALL PERIOD_MOMENT(SAMPLE, NPTS(I), AVE, ADEV, SDEV,
     :                         SVAR(I))

         ELSE
            SVAR(I) = 0.0D0
         END IF
  30  CONTINUE

C-----------------------------------------------------------------------------
C Calculate the overall variance for all of the samples and divide it by the
C variance of the whole dataset. The result is the PDM statistic.
C-----------------------------------------------------------------------------

      NOM = 0.0D0
      DENOM = 0.0D0
      DO 40 I = 1, NBIN
         NOM = NOM + (DFLOAT(NPTS(I)-1)*SVAR(I))
         DENOM = DENOM + DFLOAT(NPTS(I))
  40  CONTINUE

      IF ( DENOM.EQ.0.0D0 ) THEN
         CALL PERIOD_WRITEBELL()
         WRITE (*, *) '** ERROR:  No points in bin in PERIOD_PDM.'
         IFAIL = 1
         GO TO 300
      ELSE IF ( VARI.EQ.0.0D0 ) THEN
         CALL PERIOD_WRITEBELL()
         WRITE (*, *) '** ERROR:  Zero variance in PERIOD_PDM.'
         IFAIL = 1
         GO TO 300
      END IF
      DENOM = DENOM - DFLOAT(NBIN)
      PDM = (NOM/DENOM)/VARI

C-----------------------------------------------------------------------------
C And return.
C-----------------------------------------------------------------------------

      IFAIL = 0

 300  CONTINUE
      RETURN
      END
