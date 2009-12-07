*NOISE
*
* NOISE -- plots variance versus signal and noise models over the top.
*
* noise is used to calibrate the parameters READOUT and PHOTON used by 
* most of the pamela routines. It needs a series of fairly smooth
* frames (e.g. biases and flat fields) with different count levels. 
* The frames should have been debiassed and flat fielded. You should
* apply a flat field in which large scale variations have been removed
* in order to preserve the numbers of counts while correcting for pixel
* to pixel variations. Typically this is NOT the same as the one you might
* want to apply during reduction.
*
* noise works by taking blocks of pixels for each of which it computes the
* absolute value of the pixel value minus the average of its 8 neighbours.
* The average of these values for a block is then corrected to give a
* sigma assuming gaussian statistics. The mean square is not used as it
* is sensitive to bad pixels. A correction is made for the noise of the
* mean of 8 pixels.
*
* The models that you can plot are 
*
* Noise = SQRT(READOUT**2 + COUNTS/PHOTON + (COUNTS*GRAIN)**2))
*
* where READOUT is the RMS readout noise in ADU and provides a flat
* base to the noise at low count levels. PHOTON is the number of
* electron or detected photons/ADU and is also called the gain.
* GRAIN in the fractional flat field noise due to an imperfect flat-field
* (and is really only sensitive to very short scale flat field errors
* due to the measurement process described above). If you flat field your
* data GRAIN should be very small or zero, but it is often visible even after
* flat fielding as a steepening of the gradient of the noise versus counts
* at high counts as flat field noise dominates over photon noise.
*
* noise recognizes bad pixels.
* 
* Parameters:
*
* FILES  -- An ASCII list of the calibration files. Any number is possible.
*           Ideally they should extend from zero counts to the maximum
*           possible. You may want to add a small constant to the zero
*           level frames since the axes are logarithmic and you otherwise
*           miss many points.
*
* XSTART, XEND -- The region of the frames to use. Allow a 1 pixel boundary
* YSTART, YEND    at its edge in which pixels are OK. i.e. make sure that
*                 you are more than 1 pixel away from duff regions.
*
* NXBOX, NYBOX  -- Size of boxes to average over. Can make quite large because
*                  the variance is not calculated globally over the boxes.
*
* DEVICE        -- Plot device
*
*NOISE
      SUBROUTINE NOISE(STATUS)
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CNF_PAR'
      INTEGER STATUS, IFAIL, MXFILE, NFILE, COL
      INTEGER IMAGE, PLACE, IPTR, LBND(2), UBND(2), IKEEP
      INTEGER NDIM, XLO, XHI, YLO, YHI, IFILE, EL, I
      INTEGER ID, TEMP, NXS, NYS, NXBOX, NYBOX, PGOPEN
      REAL PMIN, PMAX, READOUT, PHOTON, GRAIN, TEST, DN
      PARAMETER (MXFILE=500)
      CHARACTER*64 FILENAME, FILE(MXFILE)
      CHARACTER*64 REPLY, DEVICE
      INTEGER MAXPLOT
      PARAMETER (MAXPLOT=400)
      REAL PLOT1(MAXPLOT), PLOT2(MAXPLOT)
      LOGICAL MOREFILES
      INTEGER INPUT, EOF
      PARAMETER (INPUT=41,EOF=-1)
C
      IF(STATUS.NE.SAI__OK) RETURN
      CALL NDF_BEGIN
C
C     Find the name of the input file and open it
C
      CALL PAR_GET0C('FILES',FILENAME,STATUS)
      OPEN(UNIT=INPUT,FILE=FILENAME,STATUS='OLD',IOSTAT=IFAIL)
      IF (IFAIL.NE.0) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC('FILE',FILENAME)
         CALL ERR_REP(' ','Error opening ^FILE for input',STATUS)
      END IF
C
C     Read in file names
C
      MOREFILES=.TRUE.
      NFILE = 1
      DO WHILE(MOREFILES .AND. STATUS.EQ.SAI__OK .AND. 
     &     NFILE.LE.MXFILE)
         READ (INPUT,'(A64)',IOSTAT=IFAIL) FILE(NFILE)
         IF((FILE(NFILE)(:1).NE.'#')
     &        .AND.(FILE(NFILE).NE.' ').AND.
     &        (IFAIL.EQ.0)) THEN
            NFILE = NFILE + 1
         ELSE IF(IFAIL.EQ.EOF) THEN
            NFILE = NFILE - 1
            MOREFILES=.FALSE.
         ELSE IF(IFAIL.NE.0) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP(' ','I/O error reading from data file',
     &           STATUS)
         END IF
      END DO
      CLOSE(UNIT=INPUT)
      IF(NFILE.EQ.0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','No files found', STATUS)
      ELSE IF(STATUS.EQ.SAI__OK) THEN
         CALL MSG_SETI('NFILE',NFILE)
         CALL MSG_OUT(' ','Read ^NFILE file names',STATUS)
      END IF
C
C     Open first one to get size. Close it so that later
C     loop can start at beginning.
C
      CALL NDF_OPEN(DAT__ROOT,FILE(1),'READ','OLD',IMAGE,
     &     PLACE,STATUS)
      CALL NDF_BOUND(IMAGE,2,LBND,UBND,NDIM,STATUS)
      IF(NDIM.NE.2) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Images must be 2D',STATUS)
      END IF
      CALL NDF_ANNUL(IMAGE,STATUS)
C
C     Get region of frame of interest
C
      CALL PAR_GDR0I('XSTART',LBND(1),LBND(1),UBND(1),
     &     .FALSE.,XLO,STATUS)
      CALL PAR_GDR0I('XEND',UBND(1),XLO,UBND(1),
     &     .FALSE.,XHI,STATUS)
      CALL PAR_GDR0I('YSTART',LBND(2),LBND(2),UBND(2),
     &     .FALSE.,YLO,STATUS)
      CALL PAR_GDR0I('YEND',UBND(2),YLO,UBND(2),
     &     .FALSE.,YHI,STATUS)
      NXS = XHI-XLO+1
      NYS = YHI-YLO+1
C
C     Get box size
C
      CALL PAR_GDR0I('NXBOX',MIN(5,NXS),1,NXS,.FALSE.,NXBOX,STATUS)
      CALL PAR_GDR0I('NYBOX',MIN(5,NYS),1,NYS,.FALSE.,NYBOX,STATUS)
C
C     Plot device
C
      CALL PAR_GET0C('DEVICE',DEVICE,STATUS) 
      IF(STATUS.EQ.SAI__OK) THEN
         ID = PGOPEN(DEVICE)
         IF(ID.LE.0) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('DEVICE',DEVICE)
            CALL ERR_REP(' ',
     &           'Failed to open plot device ^DEVICE', STATUS)
         END IF
         CALL PGSCI(5)
         PMIN = -1.
         PMAX = 5.
         CALL PGENV(PMIN, PMAX, 0., 3., 0, 30)
         CALL PGSCI(7)
         CALL PGLAB('Mean value', 'RMS fluctuation', 'Noise analysis')
      END IF
C
C     Now get on with it
C
      CALL MSG_BLANK(STATUS)
      DO IFILE=1,NFILE
         CALL MSG_SETC('FILE',FILE(IFILE))
         CALL MSG_OUT(' ','File = ^FILE',STATUS)
C
C     Open file, get dimensions
C
         CALL NDF_OPEN(DAT__ROOT,FILE(IFILE),'READ',
     &        'OLD',IMAGE,PLACE,STATUS)
         CALL NDF_BOUND(IMAGE,2,LBND,UBND,NDIM,STATUS)
         IF(NDIM.NE.2) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP(' ','Images must be 2D',STATUS)
         END IF
C
C     Check file covers requested region, get NDF section if
C     necessary
C
         IF(XLO.LT.LBND(1) .OR. XHI.GT.UBND(1) .OR.
     &        YLO.LT.LBND(2) .OR. YHI.GT.UBND(2)) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP(' ',
     &           'Requested region outside range of file',STATUS)
         ELSE IF(XLO.NE.LBND(1) .OR. XHI.NE.UBND(1) .OR.
     &        YLO.NE.LBND(2) .OR. YHI.NE.UBND(2)) THEN
            LBND(1) = XLO
            UBND(1) = XHI
            LBND(2) = YLO
            UBND(2) = YHI
            CALL NDF_SECT(IMAGE, 2, LBND, UBND, TEMP, STATUS)
            CALL NDF_ANNUL(IMAGE, STATUS)
            CALL NDF_CLONE(TEMP, IMAGE, STATUS)
            CALL NDF_ANNUL(TEMP, STATUS)
         END IF
C
C     Map data
C     
         CALL NDF_MAP(IMAGE,'Data','_REAL','READ',IPTR,EL,STATUS)
C
C     Noise analysis 
C
         CALL P_NOISE(%VAL(CNF_PVAL(IPTR)), 
     :                NXS, NYS, NXBOX, NYBOX, STATUS)
C
C     Tidy up
C
         CALL NDF_ANNUL(IMAGE, STATUS)

      END DO
C
C     Chance to plot models
C
      IF(STATUS.EQ.SAI__OK) THEN
         WRITE(*,*) ' '
         WRITE(*,*) 'You can now plot noise models over the data'
         IFAIL = 1
         COL   = 2
         DO WHILE(IFAIL.NE.0)
            WRITE(*,*) ' '
            WRITE(*,*) 'ENTER (1) Readout noise'//
     &           ' sigma (in data numbers)'
            WRITE(*,*) '      (2) Number of detected photons'//
     &           ' per data number.'
            WRITE(*,*) '      (3) Grain noise (percent)'
            WRITE(*,'(A,$)') 'or <CR> to exit: '
            READ(*,'(A)') REPLY
            IF(REPLY.EQ.' ') THEN
               IFAIL = 0
            ELSE
               READ(REPLY,*,IOSTAT=IKEEP) READOUT, PHOTON, GRAIN
               IF(IKEEP.EQ.0 .AND. READOUT.GE.0. .AND. PHOTON.GT.0. 
     &              .AND. GRAIN.GE. 0.) THEN
                  GRAIN = GRAIN/100.
C     
C     Evaluate and plot noise model 
C     
                  DO I = 1, MAXPLOT
                     TEST = PMIN+(PMAX-PMIN)*REAL(I-1)/REAL(MAXPLOT-1)
                     DN = 10.**TEST
                     PLOT1(I) = TEST
                     PLOT2(I) = 0.5*ALOG10(READOUT**2+
     &                    DN/PHOTON+(DN*GRAIN)**2)
                  END DO
                  CALL PGSCI(COL)
                  COL =  COL + 1
                  CALL PGLINE(MAXPLOT, PLOT1, PLOT2)
               END IF
            END IF
         END DO
      END IF
      CALL PGCLOS
      CALL NDF_END(STATUS)
      RETURN
      END

      SUBROUTINE P_NOISE(DATA, NX, NY, IXSTEP, IYSTEP, STATUS)
C
C     Adapted from NOISEMAP in PAMELA
C     This routine is used to determine the noise characteristics of 
C     a detector by examining the way short-scale fluctuations in a 
C     flat field (or other) frame vary with data number.
C     the empirical results may be compared with various noise models.
C     
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INTEGER NX, NY, IXSTEP, IYSTEP, IXHI, IYHI, NPLOT
      INTEGER NDUMP, NXBOX, NYBOX, STATUS, IXBOX
      INTEGER IXLO, IYLO, IX1, IX2, IY1, IY2, IX, IY
      INTEGER IXM, IXC, IXP, IYM, IYC, IYP, NPIX, IYBOX
      REAL DATA(NX, NY), F1, F2, F3, F4, F5, F6
      REAL F7, F8, F9, AVG, SUM, SUM2, BFAC
C
      PARAMETER (NDUMP=100)
      REAL PLOT1(NDUMP), PLOT2(NDUMP)
C
      IF(STATUS.NE.SAI__OK) RETURN
      NXBOX = NX/IXSTEP
      NYBOX = NY/IYSTEP
      NPLOT = 0
C
C     Dimensions of large box which fits into specified area
C     and is integral multiple of desired box
C
      IXLO = 1
      IYLO = 1
      IXHI = NXBOX*IXSTEP
      IYHI = NYBOX*IXSTEP
      IF(IXHI.LT.NX-1) THEN
        IXLO = IXLO + 1
        IXHI = IXHI + 1
      END IF
      IF(IYHI.LT.NY-1) THEN
        IYLO = IYLO + 1
        IYHI = IYHI + 1
      END IF
C
      BFAC = SQRT(16.*ATAN(1.)/9.)      
      DO IYBOX = 1, NYBOX
         IY1 = IYLO + IYSTEP*(IYBOX-1)
         IY1 = MAX(2, MIN(NY-1, IY1))
         IY2 = IY1 + IYSTEP - 1
         IY2 = MAX(2, MIN(NY-1, IY2))
         DO IXBOX = 1, NXBOX
            IX1 = IXLO + IXSTEP*(IXBOX-1)
            IX1 = MAX(2, MIN(NX-1, IX1))
            IX2 = IX1 + IXSTEP - 1
            IX2 = MAX(2, MIN(NX-1, IX2))
C
C     Compute average deviation within chosen box
C
            SUM  = 0.
            SUM2 = 0.
            NPIX = 0
C
C     Take a local average for each pixel in the large box
C     by averaging surrounding 8 pixels together
C
            DO IY = IY1, IY2
               IYM = IY - 1
               IYC = IY
               IYP = IY + 1
               DO IX = IX1, IX2
                  IXM = IX - 1
                  IXC = IX
                  IXP = IX + 1
                  F1 = DATA(IXM,IYM)
                  F2 = DATA(IXC,IYM)
                  F3 = DATA(IXP,IYM)
                  F4 = DATA(IXM,IYC)
                  F5 = DATA(IXC,IYC)
                  F6 = DATA(IXP,IYC)
                  F7 = DATA(IXM,IYP)
                  F8 = DATA(IXC,IYP)
                  F9 = DATA(IXP,IYP)
                  IF(F1.NE.VAL__BADR .AND. F2.NE.VAL__BADR
     &                 .AND. F3.NE.VAL__BADR .AND. 
     &                 F4.NE.VAL__BADR .AND. F5.NE.VAL__BADR
     &                 .AND. F6.NE.VAL__BADR .AND.
     &                 F7.NE.VAL__BADR .AND. F8.NE.VAL__BADR
     &                 .AND. F9.NE.VAL__BADR) THEN
                     AVG = (F1+F2+F3+F4+F6+F7+F8+F9)/8.
                     SUM = SUM + AVG
C     
C     The absolute value is more robust than
C     the squared deviation
C     
                     SUM2 = SUM2 + ABS(F5-AVG)
                     NPIX = NPIX + 1
                  END IF
               END DO
            END DO
C     
C     Average sums sums for the large box
C     also include a correction for the noise in
C     the 8 pixel average and for taking the mean of the
C     absolute deviation
C     
            IF(NPIX.GT.1) THEN
               SUM  = SUM /MAX(1.,REAL(NPIX))
               SUM2 = BFAC*SUM2/MAX(1.,REAL(NPIX))
C     
               NPLOT = NPLOT + 1
               PLOT1(NPLOT) = ALOG10( MAX(SUM, 0.11) )
               PLOT2(NPLOT) = ALOG10( MAX(SUM2, 1.) )
C     
C     Dump plot buffer if full
C     
               IF(NPLOT.EQ.NDUMP) THEN
                  CALL PGSCI(1)
                  CALL PGPT(NPLOT, PLOT1, PLOT2, 1)
                  NPLOT = 0
               END IF
            END IF
         END DO      
      END DO
C     
C     Dump final buffer (if any)
C     
      IF(NPLOT.GT.0) THEN
         CALL PGSCI(1)
         CALL PGPT(NPLOT, PLOT1, PLOT2, 1)
         NPLOT = 0
      END IF
      RETURN
      END



