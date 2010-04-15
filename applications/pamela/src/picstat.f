*PICSTAT
*
* PICSTAT  -- Closely related to istat but allows evaluation of statistics
*             over a set of rectangular regions rather than just one. It
*             also computes these with reject cycles. picstat is meant for
*             computation of constant values for bias subtraction. The
*             revised mean value in particular is useful for this.
*             The revised and robust estimates of standard deviation
*             can be useful for measuring readout noise. The revised
*             mean and median are stored as PIC_MEAN and PIC_MEDIAN
*             in ~/adam/GLOBAL.sdf. If you apply this to integer data,
*             the plot can look a little peculiar as the data appear 0.5
*             pixels to the right of the model. This is OK.
*
* Parameters:
*
*   IMAGE    -- Data frame to analyse.
*
*   STREG    -- File defining regions with format x1 x2 y1 y2 on each line.
*
*               e.g.
*               1 34 101 500
*               21 50 601 700
*
*               Pixels outside the bounds of the NDF section loaded are
*               ignored.
*
*   CLIP      -- Sigma rejection threshold. 2.5 seems to work reasonably
*                but plot one or two to check. Very small values will lead
*                to spuriously small values of RMS.
*
*   PLOT      -- TRUE to plot histogram of values. A gaussian with
*                mean and rms set to the revised mean and robust
*                sigma will be plotted over with approximately the
*                right vertical scaling.
*
*   DEVICE    -- Plot device
*
*   RANGE     -- Sigma range for histogram plot
*
*   NBIN      -- Number of bins
*
*PICSTAT
      SUBROUTINE PICSTAT(STATUS)
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'CNF_PAR'
      INTEGER STATUS, IMAGE, LBND(2), UBND(2), N
     :DIM
      INTEGER NXS, NYS, XLO, YLO, IFAIL, IPTR, EL
      INTEGER MAXREG, NREG, I, J, NBIN, PLACE, WORK
      INTEGER WPTR
      PARAMETER (MAXREG=100)
      INTEGER REGION(2,2,MAXREG)
      REAL CLIP, RANGE, MEAN, MEDVAL
      LOGICAL PLOT
      CHARACTER*64 STREG, DEVICE
C
      IF(STATUS.NE.SAI__OK) RETURN
      CALL NDF_BEGIN
C
C     Open data file
C
      CALL NDF_ASSOC('IMAGE', 'READ',IMAGE, STATUS)
      CALL NDF_BOUND(IMAGE,2,LBND,UBND,NDIM,STATUS)
      IF(NDIM.EQ.1) THEN
        NXS = UBND(1)-LBND(1)+1
        NYS = 1
        XLO = LBND(1)
        YLO = 1
      ELSE IF(NDIM.EQ.2) THEN
        NXS = UBND(1)-LBND(1)+1
        NYS = UBND(2)-LBND(2)+1
        XLO = LBND(1)
        YLO = LBND(2)
      END IF
C
C     Get statistics region file
C
      CALL PAR_GET0C('STREG',STREG,STATUS)
      OPEN(UNIT=25,FILE=STREG,STATUS='OLD',IOSTAT=IFAIL)
      IF(IFAIL.NE.0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Unable to open region file',STATUS)
      ELSE
         NREG = 0
         IFAIL = 0
         DO WHILE(IFAIL.EQ.0 .AND. NREG.LT.MAXREG)
            NREG = NREG + 1
            READ(25,*,IOSTAT=IFAIL) ((REGION(I,J,NREG),I=1,2),J=1,2)
         END DO
         CLOSE(UNIT=25)
         IF(IFAIL.GT.0) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP(' ','Error during read',STATUS)
         ELSE IF(NREG.EQ.MAXREG) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP(' ','Region buffer full.', STATUS)
         ELSE
            NREG = NREG - 1
         END IF
      END IF
      IF(STATUS.EQ.SAI__OK) THEN
         IF(NREG.LE.0) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP(' ','No regions loaded',STATUS)
         ELSE
            CALL MSG_SETI('NREG',NREG)
            IF(NREG.EQ.1) THEN
               CALL MSG_OUT(' ','^NREG region loaded.',STATUS)
            ELSE
               CALL MSG_OUT(' ','^NREG regions loaded.',STATUS)
            END IF
         END IF
      END IF
      CALL PAR_GDR0R('CLIP',2.5,0.,1.E10,.FALSE.,CLIP,STATUS)
      CALL PAR_GET0L('PLOT',PLOT,STATUS)
      IF(PLOT) THEN
         CALL PAR_GET0C('DEVICE',DEVICE,STATUS)
         CALL PAR_GDR0R('RANGE',5.,0.01,1.E4,.FALSE.,RANGE,STATUS)
         CALL PAR_GDR0I('NBIN', 100, 1, 400,.FALSE.,NBIN,STATUS)
      END IF
C
C     Map
C
      CALL NDF_MAP(IMAGE,'Data','_REAL','READ',IPTR,EL,STATUS)
      CALL NDF_TEMP(PLACE, STATUS)
      CALL NDF_NEW('_INTEGER',2,LBND,UBND,PLACE,WORK,STATUS)
      CALL NDF_MAP(WORK,'Data','_INTEGER','WRITE',WPTR,EL,STATUS)
C
      CALL PIC_STAT(%VAL(CNF_PVAL(IPTR)), %VAL(CNF_PVAL(WPTR)),
     :              NXS, NYS, XLO, YLO,
     &     REGION, NREG, CLIP, PLOT, RANGE, NBIN, DEVICE, MEAN,
     &     MEDVAL, STATUS)
C
C     Store revised mean and median
C
      CALL PAR_PUT0R('PIC_MEAN',MEAN,STATUS)
      CALL PAR_PUT0R('PIC_MEDIAN',MEDVAL,STATUS)
      CALL NDF_END(STATUS)
      RETURN
      END

      SUBROUTINE PIC_STAT(DATA, MASK, NXS, NYS, XLO, YLO,
     &     REGION, NREG, CLIP, PLOT, RANGE, NBIN, DEVICE,
     &     MEAN, MEDVAL, STATUS)
C
C     Computes simple statistics on pixels in a data section
C     DATA(NXS,NYS) with lower left corner XLO, YLO.
C
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INTEGER NXS, NYS, XLO, YLO, NREG, NPIX, MASK(NXS,NYS)
      INTEGER IR, IY, IX, IXMIN, IYMIN, IXMAX, IYMAX, NOK
      INTEGER REGION(2,2,NREG), STATUS, PGOPEN, NBIN, N
      INTEGER IX1, IX2, IY1, IY2, NREJTOT, NREJ, ICYCLE
      INTEGER L, NLO, NSUM, I, ID, IBIN, IADD
      REAL DATA(NXS,NYS), CLIP, RMS, MEDVAL, RANGE, DMAX
      REAL X1, X2, Y1, Y2, EFAC, PMAX, PMIN, D1, D2
      REAL MEAN, VAL, DMUL, NEWD1, NEWD2, ACC
      DOUBLE PRECISION SUM1, SUM2
      LOGICAL PLOT
      INTEGER MAXPLOT
      PARAMETER (MAXPLOT = 1000)
      REAL XPLOT(MAXPLOT), YPLOT(MAXPLOT)
      INTEGER MAXSORT
      PARAMETER (MAXSORT = 10000)
      REAL RANK(MAXSORT)
      INTEGER IRANK(MAXSORT)
      INTEGER MAXHIST
      PARAMETER (MAXHIST = 100)
      REAL HIST(MAXPLOT)
      CHARACTER*(*) DEVICE
C
      IF(STATUS.NE.SAI__OK) RETURN
C
C     Compute fundamentals
C
      PMIN =  1.E30
      PMAX = -1.E30
      NOK  = 0
      SUM1 = 0.D0
      DO IR = 1, NREG
         IX1 = MAX(1,REGION(1,1,IR)-(XLO-1))
         IX2 = MIN(NXS,REGION(2,1,IR)-(XLO-1))
         IY1 = MAX(1,REGION(1,2,IR)-(YLO-1))
         IY2 = MIN(NYS,REGION(2,2,IR)-(YLO-1))
         DO IY = IY1, IY2
            DO IX = IX1, IX2
               VAL = DATA(IX,IY)
               IF(VAL.NE.VAL__BADR) THEN
                  NOK  = NOK + 1
                  SUM1 = SUM1 + VAL
                  IF(VAL.LT.PMIN) THEN
                     IXMIN = IX+XLO-1
                     IYMIN = IY+YLO-1
                     PMIN  = VAL
                  END IF
                  IF(VAL.GT.PMAX) THEN
                     IXMAX = IX+XLO-1
                     IYMAX = IY+YLO-1
                     PMAX  = VAL
                  END IF
               END IF
            END DO
         END DO
      END DO
      IF(NOK.EQ.0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','There were no valid pixels.',STATUS)
         RETURN
      END IF
      CALL MSG_BLANK(STATUS)
      CALL MSG_SETI('NPIX',NOK)
      CALL MSG_OUT(' ','Number of pixels = ^NPIX',STATUS)
      MEAN = REAL(SUM1)/REAL(NOK)
      CALL MSG_SETI('XMIN',IXMIN)
      CALL MSG_SETI('YMIN',IYMIN)
      CALL MSG_SETR('PMIN',PMIN)
      CALL MSG_OUT(' ',
     &     'Minimum = ^PMIN at X,Y: ^XMIN, ^YMIN', STATUS)
      CALL MSG_SETI('XMAX',IXMAX)
      CALL MSG_SETI('YMAX',IYMAX)
      CALL MSG_SETR('PMAX',PMAX)
      CALL MSG_OUT(' ',
     &     'Maximum = ^PMAX at X,Y: ^XMAX, ^YMAX', STATUS)
      CALL MSG_SETR('MEAN',MEAN)
      CALL MSG_OUT(' ','        Straight mean value = ^MEAN',
     &STATUS)
      IF(NOK.EQ.1) THEN
         CALL MSG_OUT(' ','Only one pixel. No more stats.',STATUS)
         RETURN
      END IF
C
C     Now compute standard deviation
C
      SUM2 = 0.D0
      DO IR = 1, NREG
         IX1 = MAX(1,REGION(1,1,IR)-(XLO-1))
         IX2 = MIN(NXS,REGION(2,1,IR)-(XLO-1))
         IY1 = MAX(1,REGION(1,2,IR)-(YLO-1))
         IY2 = MIN(NYS,REGION(2,2,IR)-(YLO-1))
         DO IY = IY1, IY2
            DO IX = IX1, IX2
               VAL = DATA(IX,IY)
               IF(VAL.NE.VAL__BADR) SUM2 = SUM2 + (VAL-MEAN)**2
            END DO
         END DO
      END DO
      RMS = SQRT(REAL(SUM2)/REAL(NOK-1))
      CALL MSG_SETR('RMS',RMS)
      CALL MSG_OUT(' ','Straight standard deviation = ^RMS',STATUS)
C
C     Initialise mask
C
      DO IY = 1, NYS
         DO IX = 1, NXS
            MASK(IX,IY) = 1
         END DO
      END DO
C
C     Re-compute mean etc with rejection.
C
      NREJTOT  = 0
      NREJ     = 1
      ICYCLE   = 0
      DO WHILE(NREJ.GT.0 .OR. ICYCLE.LT.5)
         IF(ICYCLE.LT.5) THEN
            DMAX = CLIP*RMS*(1.+1./2.**ICYCLE)
         ELSE
            DMAX = CLIP*RMS
         END IF
         ICYCLE = ICYCLE + 1
         SUM1   = 0.D0
         SUM2   = 0.D0
         NPIX   = 0
         NREJ   = 0
         DO IR = 1, NREG
            IX1 = MAX(1,REGION(1,1,IR)-(XLO-1))
            IX2 = MIN(NXS,REGION(2,1,IR)-(XLO-1))
            IY1 = MAX(1,REGION(1,2,IR)-(YLO-1))
            IY2 = MIN(NYS,REGION(2,2,IR)-(YLO-1))
            DO IY = IY1, IY2
               DO IX = IX1, IX2
                  VAL = DATA(IX,IY)
                  IF(VAL.NE.VAL__BADR .AND. MASK(IX,IY).EQ.1) THEN
                     IF(ABS(VAL-MEAN).GT.DMAX) THEN
                        NREJ = NREJ + 1
                        MASK(IX,IY) = 0
                     ELSE
                        NPIX = NPIX + 1
                        SUM1 = SUM1 + (VAL-MEAN)
                        SUM2 = SUM2 + (VAL-MEAN)**2
                     END IF
                  END IF
               END DO
            END DO
         END DO
         RMS  = REAL(SQRT((SUM2-SUM1*SUM1/REAL(NPIX))/REAL(NPIX-1)))
         MEAN = MEAN + REAL(SUM1/REAL(NPIX))
         NREJTOT = NREJTOT + NREJ
      END DO
      CALL MSG_SETR('CLIP',CLIP)
      CALL MSG_SETI('NREJ',NREJTOT)
      CALL MSG_OUT(' ','^NREJ pixels rejected at ^CLIP sigma.',
     &     STATUS)
      CALL MSG_SETR('MEAN',MEAN)
      CALL MSG_OUT(' ','         Revised mean value = ^MEAN',
     &     STATUS)
      CALL MSG_SETR('RMS',RMS)
      CALL MSG_OUT(' ',' Revised standard deviation = ^RMS',
     &     STATUS)
C
C     Robust estimate of standard deviation
C
      SUM1 = 0.D0
      DO IR = 1, NREG
         IX1 = MAX(1,REGION(1,1,IR)-(XLO-1))
         IX2 = MIN(NXS,REGION(2,1,IR)-(XLO-1))
         IY1 = MAX(1,REGION(1,2,IR)-(YLO-1))
         IY2 = MIN(NYS,REGION(2,2,IR)-(YLO-1))
         DO IY = IY1, IY2
            DO IX = IX1, IX2
               VAL = DATA(IX,IY)
               IF(VAL.NE.VAL__BADR .AND. MASK(IX,IY).EQ.1) THEN
                  SUM1 = SUM1 + ABS(VAL-MEAN)
               END IF
            END DO
         END DO
      END DO
C
C     Correction of bias assuming gaussian statistics.
C
      RMS = REAL(SQRT(2.*ATAN(1.))*SUM1/REAL(NPIX-1))
      CALL MSG_SETR('RMS',RMS)
      CALL MSG_OUT(' ','  Robust standard deviation = ^RMS',STATUS)
C
C     Now estimate median. For less than MAXSORT pixels
C     we do this by sorting; for larger values we estimate
C     it approximately by binning.
C
      NPIX = NOK
      IF(NPIX.LE.MAXSORT) THEN
         NPIX = 0
         DO IR = 1, NREG
            IX1 = MAX(1,REGION(1,1,IR)-(XLO-1))
            IX2 = MIN(NXS,REGION(2,1,IR)-(XLO-1))
            IY1 = MAX(1,REGION(1,2,IR)-(YLO-1))
            IY2 = MIN(NYS,REGION(2,2,IR)-(YLO-1))
            DO IY = IY1, IY2
               DO IX = IX1, IX2
                  VAL = DATA(IX,IY)
                  IF(VAL.NE.VAL__BADR) THEN
                     NPIX = NPIX + 1
                     RANK(NPIX) = VAL
                  END IF
               END DO
            END DO
         END DO
         CALL HEAPSORT(NPIX, RANK, IRANK)
         MEDVAL = RANK(IRANK(NPIX/2+1))
         IF(MOD(NPIX,2).EQ.0) THEN
            MEDVAL = (MEDVAL + RANK(IRANK(NPIX/2)))/2.
         END IF
      ELSE
C
C     Compute CDF over ever narrower ranges bracketing the median
C
         D1   = PMIN
         D2   = PMAX
         N    = 1
         ACC  = RMS/SQRT(REAL(NPIX))/10.
         DO WHILE(N.LT.10 .AND. ABS(D2-D1).GT.ACC)
            N = N + 1
            DMUL = REAL(MAXHIST)/(D2-D1)
            NLO  = 0
C
C     Accumulate histogram
C
            DO L = 1, MAXHIST
               HIST(L) = 0.
            END DO
            DO IR = 1, NREG
               IX1 = MAX(1,REGION(1,1,IR)-(XLO-1))
               IX2 = MIN(NXS,REGION(2,1,IR)-(XLO-1))
               IY1 = MAX(1,REGION(1,2,IR)-(YLO-1))
               IY2 = MIN(NYS,REGION(2,2,IR)-(YLO-1))
               DO IY = IY1, IY2
                  DO IX = IX1, IX2
                     VAL = DATA(IX,IY)
                     IF(VAL.NE.VAL__BADR) THEN
                        L = NINT(DMUL*(VAL-D1)+0.5)
                        IF(L.LT.1) THEN
                           NLO = NLO + 1
                        ELSE IF(L.LE.MAXHIST) THEN
                           HIST(L) = HIST(L) + 1.
                        END IF
                     END IF
                  END DO
               END DO
            END DO
C
C     Make into CDF
C
            NSUM = NLO
            DO I = 1, MAXHIST
               NSUM    = NSUM + NINT(HIST(I))
               HIST(I) = REAL(NSUM)/REAL(NPIX)
            END DO
            L = 1
            DO WHILE(HIST(L).LE.0.5)
               L = L + 1
            END DO
            NEWD1 = D1 + (D2-D1)*REAL(L-1)/REAL(MAXHIST)
            NEWD2 = D1 + (D2-D1)*REAL(L  )/REAL(MAXHIST)
            D1    = NEWD1
            D2    = NEWD2
         END DO
         MEDVAL = (D1+D2)/2.
      END IF
      CALL MSG_SETR('MEDIAN',MEDVAL)
      CALL MSG_OUT(' ','               Median value = ^MEDIAN',
     &     STATUS)
      CALL MSG_BLANK(STATUS)
C
C     Plot histogram
C
      IF(PLOT) THEN
         ID = PGOPEN(DEVICE)
         IF(ID.GT.0) THEN
            X1 = MEDVAL - RANGE*RMS
            X2 = MEDVAL + RANGE*RMS

C
C     Load up histogram
C
            IBIN = MIN(NBIN, MAXPLOT)
            IX   = NINT((X2-X1)/REAL(IBIN))
            IX   = MAX(1, IX)
            X1   = MEDVAL - REAL(IBIN)*REAL(IX)/2.
            X2   = MEDVAL + REAL(IBIN)*REAL(IX)/2.
            DO I = 1, IBIN
               XPLOT(I) = X1+(X2-X1)*(REAL(I)-1)/REAL(IBIN)
               YPLOT(I) = 0.
            END DO
            DO IR = 1, NREG
               IX1 = MAX(1,REGION(1,1,IR)-(XLO-1))
               IX2 = MIN(NXS,REGION(2,1,IR)-(XLO-1))
               IY1 = MAX(1,REGION(1,2,IR)-(YLO-1))
               IY2 = MIN(NYS,REGION(2,2,IR)-(YLO-1))
               DO IY = IY1, IY2
                  DO IX = IX1, IX2
                     VAL = DATA(IX,IY)
                     IF(VAL.NE.VAL__BADR) THEN
                        IADD = INT(REAL(IBIN)*(VAL-X1)/(X2-X1))+1
                        IADD = MAX(1, MIN(IADD, IBIN))
                        YPLOT(IADD) = YPLOT(IADD) + 1.
                     END IF
                  END DO
               END DO
            END DO
            Y1 = 0.
            DO I = 1, IBIN
               Y2 = MAX(YPLOT(I), Y2)
            END DO
            Y2 = 1.2*Y2
            CALL PGSCI(5)
            CALL PGENV(X1, X2, Y1, Y2, 0, 1)
            CALL PGSCI(7)
            CALL PGLAB('Counts', 'Number of pixels', 'Pixel histogram')
            CALL PGSCI(1)
            CALL PGBIN(IBIN, XPLOT, YPLOT, .TRUE.)
            EFAC = REAL(NPIX-NREJTOT)*(X2-X1)/
     &           REAL(IBIN)/RMS/SQRT(8.*ATAN(1.))
            DO I = 1, MAXPLOT
               XPLOT(I) = X1 + (X2-X1)*REAL(I-1)/REAL(MAXPLOT-1)
               YPLOT(I) = EFAC*EXP(-((XPLOT(I)-MEAN)/RMS)**2/2.)
            END DO
            CALL PGSCI(2)
            CALL PGLINE(MAXPLOT, XPLOT, YPLOT)
            CALL PGEND
         END IF
      END IF
      RETURN
      END
