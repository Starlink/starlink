*OPTEXT
*
* OPTEXT  -- Uses the fitted profile from PROFIT to optimally extract
*            spectrum from the frame.
*
*
* OPTEXT just applies the usual optimal weights. All the hard part has been
* done by PROFIT
*
* Parameters:
*
*  IMAGE   -- The data file for extraction.
*
*  FLAT    -- balance frame
*
*  DLOAD   -- TRUE if dark frame is required.
*
*  DARK    -- Dark frame representing counts unaffected by slit
*             profile. This will be subtracted off data before applying
*             the balance factors.
*
*  REGION  -- Contains the sky/object region limits.
*
*  SKY     -- Contains fits to the background sky in the region of
*             the object.
*
*  TRACK   -- Contains trace of the position of the spectrum from TRACE
*
*  FRACT   -- Contains fit to profile from PROFIT
*
*  SPECT   -- The output file for both the spectrum and the uncertainty
*             estimates.
*
*  YSTART,YEND -- The Y pixel limit of the extraction.
*
*  READOUT -- The readout noise of the detector in RMS data units/pixel
*
*  PHOTON  -- The number of photons per data number.
*
*  ZAPRATS -- .TRUE. then outlying points are ignored. Should not be used
*             if profile does not represent frame to be extracted (e.g.
*             arc or sky frame)
*
*  If ZAPRATS
*
*      RATLO  -- Lower outlier zap threshold if ZAPRATS = .TRUE.
*
*      RATHI  -- Upper outlier zap threshold if ZAPRATS = .TRUE.
*
*  EPS    -- Fudge factor to avoid silly rejections. On high signal-to-noise
*            data you can get very close fits but still be several sigma out.
*            EPS imposes a lower limit to the fractional deviation required
*            for a point to be rejected.
*
*  IAVE   -- Number of sky pixels to average over in low count cases
*
*  PLOT   -- .TRUE. if you want to plot
*
*  DEVICE -- Plot device
*
*OPTEXT
      SUBROUTINE OPTEXT(STATUS)
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CNF_PAR'
      INTEGER STATUS, IMAGE, FLAT, REGION, SLO, SHI, NDIM
      INTEGER NS, SKY, NPOLY, FRACT, SMALL, SPECT, TRIM(5)
      INTEGER LBND(2), UBND(2), YLO, XLO, XHI, TEMP, NXS
      INTEGER NYS, IAVE, ODPTR, EL, OVPTR, PLACE, NVARM
      INTEGER WORK, MPTR, IPTR, YHI, FPTR, SPTR, RPTR
      INTEGER FRPTR, DPTR, DARK
      REAL LEFT, RIGHT, READOUT, PHOTON, RATLO, RATHI, EPS
      REAL CPOS
      DOUBLE PRECISION YPOS, TOFF, YREF
      LOGICAL IBASE, SBASE, FRBASE, FBASE, BASE, ZAPRATS
      LOGICAL PLOT, THERE, DBASE, DLOAD
      CHARACTER*64 DEVICE
      CHARACTER*(DAT__SZLOC) LOC, LOCS, LOCS1, LOCR, LOCR1
C
      IF(STATUS.NE.SAI__OK) RETURN
      CALL NDF_BEGIN
C
C     Open data file
C
      CALL NDF_ASSOC('IMAGE', 'READ',IMAGE, STATUS)
C
C     Open balance frame, identifier FLAT
C
      CALL NDF_ASSOC('FLAT','READ',FLAT,STATUS)
C
C     Open dark frame
C
      CALL PAR_GET0L('DLOAD', DLOAD, STATUS)
      IF(DLOAD) THEN
         CALL NDF_ASSOC('DARK','READ',DARK,STATUS)
      ELSE
         CALL NDF_BOUND(FLAT, 2, LBND, UBND, NDIM, STATUS)
         CALL NDF_TEMP(PLACE, STATUS)
         CALL NDF_NEW('_REAL', NDIM, LBND, UBND, PLACE, DARK, STATUS)
      END IF
C
C     Get sky region input file, check its OK
C
      CALL NDF_ASSOC('REGION','READ',REGION,STATUS)
      CALL NDF_ISBAS(REGION, BASE, STATUS)
      IF(STATUS.EQ.SAI__OK .AND. .NOT.BASE) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Sky region file must be a base NDF'
     &        //' to allow computation of uncertainties.',STATUS)
      END IF
C
      CALL NDF_XGT0R(REGION,'PAMELA','REGPIC.LEFT',LEFT,STATUS)
      CALL NDF_XGT0R(REGION,'PAMELA','REGPIC.RIGHT',RIGHT,STATUS)
      IF(STATUS.EQ.SAI__OK .AND. (LEFT.GT.RIGHT
     &     .OR. LEFT.LE.0.)) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Bad object limits', STATUS)
      END IF
      CALL NDF_BOUND(REGION, 1, SLO, SHI, NDIM, STATUS)
      NS  = SHI - SLO + 1
C
C     Open sky file. Number of poly coefficients is used in
C     uncertainty estimation.
C
      CALL NDF_ASSOC('SKY','READ',SKY,STATUS)
      CALL NDF_XSTAT(SKY, 'PAMELA', THERE, STATUS)
      NPOLY = 0
      IF(THERE) THEN
         CALL NDF_XLOC(SKY,'PAMELA', 'READ', LOCS, STATUS)
         CALL DAT_THERE(LOCS, 'SKYFIT', THERE, STATUS)
         CALL DAT_ANNUL(LOCS, STATUS)
         IF(THERE)
     &        CALL NDF_XGT0I(SKY,'PAMELA','SKYFIT.NPOLY',NPOLY,STATUS)
      END IF
C
C     Get track file
C
      CALL GET_TRACK(YPOS, TOFF, STATUS)
C
C     There must be a reference Y position in the sky region file
C
      CALL NDF_XGT0D(REGION,'PAMELA','REGPIC.YREF',YREF,STATUS)
C
C     Open fraction file
C
      CALL NDF_ASSOC('FRACT','READ',FRACT,STATUS)
C
C     Open spectrum output file
C
      CALL NDF_SECT(IMAGE,1,1,1,SMALL,STATUS)
      CALL NDF_PROP(SMALL,' ','SPECT',SPECT,STATUS)
      CALL NDF_RESET(SPECT,'Title',STATUS)
      CALL NDF_CPUT('OPTEXT output',SPECT,'Title',STATUS)
C
C     What region of frame
C
      CALL NDF_ISBAS(IMAGE, IBASE, STATUS)
      CALL NDF_ISBAS(FLAT,  FBASE, STATUS)
      CALL NDF_ISBAS(DARK,  DBASE, STATUS)
      CALL NDF_ISBAS(SKY,   SBASE, STATUS)
      CALL NDF_ISBAS(FRACT, FRBASE, STATUS)
      BASE = IBASE .AND. FBASE .AND. SBASE .AND. FRBASE .AND. DBASE
      TRIM(1) = IMAGE
      TRIM(2) = FLAT
      TRIM(3) = DARK
      TRIM(4) = SKY
      TRIM(5) = FRACT
      CALL NDF_MBNDN('TRIM',5,TRIM,STATUS)
      IMAGE = TRIM(1)
      FLAT  = TRIM(2)
      DARK  = TRIM(3)
      SKY   = TRIM(4)
      FRACT = TRIM(5)

      CALL NDF_BOUND(IMAGE, 2, LBND, UBND, NDIM, STATUS)
      IF(BASE .AND. NDIM.EQ.2) THEN
         CALL PAR_GDR0I('YSTART',LBND(2),LBND(2),UBND(2),
     &        .FALSE.,YLO,STATUS)
         CALL PAR_GDR0I('YEND',UBND(2),YLO,UBND(2),
     &        .FALSE.,YHI,STATUS)
      ELSE
         YLO = LBND(2)
         YHI = UBND(2)
      END IF
C
C     compute minimal range in X to use
C
      CALL XLIMS(.TRUE.,YREF,LEFT,RIGHT,YLO,YHI,XLO,XHI,STATUS)
      XLO = MAX(LBND(1), XLO)
      XHI = MIN(UBND(1), XHI)
C
C     Generate appropriate section NDFs
C
      IF(XLO.NE.LBND(1) .OR. XHI.NE.UBND(1) .OR.
     &     YLO.NE.LBND(2) .OR. YHI.NE.UBND(2)) THEN
         LBND(1) = XLO
         UBND(1) = XHI
         LBND(2) = YLO
         UBND(2) = YHI
         CALL NDF_SECT(IMAGE, 2, LBND, UBND, TEMP, STATUS)
         CALL NDF_ANNUL(IMAGE, STATUS)
         CALL NDF_CLONE(TEMP, IMAGE, STATUS)
         CALL NDF_ANNUL(TEMP, STATUS)

         CALL NDF_SECT(FLAT, 2, LBND, UBND, TEMP, STATUS)
         CALL NDF_ANNUL(FLAT, STATUS)
         CALL NDF_CLONE(TEMP, FLAT, STATUS)
         CALL NDF_ANNUL(TEMP, STATUS)

         CALL NDF_SECT(DARK, 2, LBND, UBND, TEMP, STATUS)
         CALL NDF_ANNUL(DARK, STATUS)
         CALL NDF_CLONE(TEMP, DARK, STATUS)
         CALL NDF_ANNUL(TEMP, STATUS)

         CALL NDF_SECT(SKY, 2, LBND, UBND, TEMP, STATUS)
         CALL NDF_ANNUL(SKY, STATUS)
         CALL NDF_CLONE(TEMP, SKY, STATUS)
         CALL NDF_ANNUL(TEMP, STATUS)

         CALL NDF_SECT(FRACT, 2, LBND, UBND, TEMP, STATUS)
         CALL NDF_ANNUL(FRACT, STATUS)
         CALL NDF_CLONE(TEMP, FRACT, STATUS)
         CALL NDF_ANNUL(TEMP, STATUS)
      END IF
C
C     Section dimensions
C
      NXS = UBND(1)-LBND(1)+1
      NYS = UBND(2)-LBND(2)+1
C
C     Noise characteristics
C
      CALL PAR_GDR0R('READOUT',1.E-2,0.,1.E10,.FALSE.,READOUT,STATUS)
      CALL PAR_GDR0R('PHOTON',1.,1.E-10,1.E10,.FALSE.,PHOTON,STATUS)
C
C     Identify cosmic rays ?
C
      CALL PAR_GET0L('ZAPRATS',ZAPRATS,STATUS)
      IF(ZAPRATS) THEN
        CALL PAR_GDR0R('RATLO',-4.,-1.E5,0.,.FALSE.,RATLO,STATUS)
        CALL PAR_GDR0R('RATHI',+4.,0.,+1.E5,.FALSE.,RATHI,STATUS)
        CALL PAR_GDR0R('EPS',0.01,0.,1.E4,.FALSE.,EPS,STATUS)
      END IF
      CALL PAR_GET0L('PLOT',PLOT,STATUS)
      IF(PLOT) CALL PAR_GET0C('DEVICE',DEVICE,STATUS)
      CALL PAR_GODD('IAVE',1,1,2*(NYS/2)+1,.FALSE.,IAVE,STATUS)
C
C     Create and map output spectrum file
C
      CALL NDF_RESET(SPECT,'Data,Variance',STATUS)
      CALL NDF_SBND(1,YLO,YHI,SPECT,STATUS)
      CALL NDF_STYPE('_REAL',SPECT,'Data,Variance',STATUS)
      CALL NDF_MAP(SPECT,'Data', '_REAL','WRITE',ODPTR,EL,STATUS)
      CALL NDF_MAP(SPECT,'Variance','_REAL','WRITE',OVPTR,EL,STATUS)
      CALL NDF_CPUT('Counts',SPECT,'Units',STATUS)
C
C     Get space for variance matrix
C
      CALL NDF_TEMP(PLACE, STATUS)
      NVARM   = NINT(RIGHT)-NINT(LEFT)+3
      LBND(1) = 1
      LBND(2) = 1
      UBND(1) = NVARM
      UBND(2) = NVARM
      CALL NDF_NEW('_REAL',2,LBND,UBND,PLACE,WORK,STATUS)
      CALL NDF_MAP(WORK,'Data','_REAL','WRITE',MPTR,EL,STATUS)
C
C     Map files
C
      CALL NDF_MAP(IMAGE,'Data','_REAL','READ',IPTR,EL,STATUS)
      CALL NDF_MAP(FLAT,'Data','_REAL','READ',FPTR,EL,STATUS)
      CALL NDF_MAP(DARK,'Data','_REAL','READ/ZERO',DPTR,EL,STATUS)
      CALL NDF_MAP(SKY,'Data','_REAL','READ',SPTR,EL,STATUS)
      CALL NDF_MAP(REGION,'Data','_INTEGER','READ',RPTR,EL,STATUS)
      CALL NDF_MAP(FRACT,'Data','_REAL','READ',FRPTR,EL,STATUS)
C
C     Extract
C
      CALL OPT_EXT(%VAL(CNF_PVAL(IPTR)), %VAL(CNF_PVAL(FPTR)),
     &     %VAL(CNF_PVAL(DPTR)), %VAL(CNF_PVAL(SPTR)),
     &     %VAL(CNF_PVAL(FRPTR)), NXS, NYS, XLO, YLO, LEFT, RIGHT,
     &     YREF, READOUT, PHOTON, ZAPRATS, %VAL(CNF_PVAL(ODPTR)),
     &     %VAL(CNF_PVAL(OVPTR)), RATLO, RATHI,PLOT, SLO, NS,
     &     %VAL(CNF_PVAL(RPTR)), NPOLY, %VAL(CNF_PVAL(MPTR)), NVARM,
     &     IAVE, CPOS, EPS, DEVICE, STATUS)
C
C     Create 'pamela' extension if not already one. Write useful
C     info to it.
C
      CALL NDF_XSTAT(SPECT, 'PAMELA', THERE, STATUS)
      IF(.NOT.THERE) THEN
         CALL NDF_XNEW(SPECT,'PAMELA', 'Ext', 0, 0, LOC, STATUS)
      ELSE
         CALL NDF_XLOC(SPECT,'PAMELA', 'UPDATE', LOC, STATUS)
      END IF
C
C     Store position of centre of spectrum and extraction type
C
      CALL NDF_XPT0R(CPOS,SPECT,'PAMELA','CPOS',STATUS)
      CALL NDF_XPT0C('Tilted optimal',SPECT,'PAMELA','EXTRACTION',
     &     STATUS)
C
C     Copy components from sky and region files into output file
C
      CALL NDF_XSTAT(SKY, 'PAMELA', THERE, STATUS)
      IF(THERE) THEN
         CALL NDF_XLOC(SKY,'PAMELA', 'READ', LOCS, STATUS)
         CALL DAT_THERE(LOCS, 'SKYFIT', THERE, STATUS)
         IF(THERE) THEN
            CALL DAT_FIND(LOCS, 'SKYFIT', LOCS1, STATUS)
            CALL DAT_COPY(LOCS1, LOC, 'SKYFIT',STATUS)
            CALL DAT_ANNUL(LOCS1, STATUS)
         END IF
         CALL DAT_ANNUL(LOCS, STATUS)
      END IF
      CALL NDF_XLOC(REGION,'PAMELA', 'READ', LOCR, STATUS)
      CALL DAT_FIND(LOCR, 'REGPIC', LOCR1, STATUS)
      CALL DAT_COPY(LOCR1,LOC,'REGPIC',STATUS)
      CALL DAT_ANNUL(LOCR1, STATUS)
      CALL DAT_ANNUL(LOCR, STATUS)
      CALL DAT_ANNUL(LOC, STATUS)
      CALL NDF_END(STATUS)
      RETURN
      END
C
      SUBROUTINE OPT_EXT(DATA, FLAT, DARK, SKY, FRACT, NXS, NYS,
     &     XLO, YLO, LEFT, RIGHT, YREF, READOUT, PHOTON, ZAPRATS,
     &     SPEC, SPECVAR, RATLO, RATHI, PLOT, SLO, NS, MASK,
     &     NPSKY, VARMAT, NVARM, ISAVE, POS, EPS, DEVICE, STATUS)
*
* Written by T.R. Marsh, Feb 1989
* This routine uses the profile contained in FRACT, (fitted by
* PROFIT for example) to extract a spectrum with optimal weights.
* The spectrum is extracted parallel to the rows of pixels and should
* be modified if lines of constant wavelength are not almost parallel
* to the rows.
*
* Modified May 1989 to include change to CHANCE
*
* 05/03/90   Subtle bug fixed which in some circumstances could cause too
*            many pixels to be rejected. (Weak continua with most flux in
*            one pixel).
*
* 25/05/90   Improved cosmic ray rejection by rejecting positive outliers
*            before negative.
*
* 19/07/90   Modified to include change to CHANCE
*
* 31/10/90   Fixed bug spotted by CRJ which prevented early escape from
*            reject cycle loop. Bug caused by 19/07/90 changes.
*
* 01/01/91   Added code to allow averaging in wavelength of sky background
*            which is useful in low count data where the fractional
*            uncertainty on the sky estimate is large. To be avoided with
*            CCD data especially where there are lots of night sky lines.
*
* 19/06/96   Added EPS fudge factor for high signal-to-noise cases
*
* Input:
*
* R*4 DATA(NXS,NYS)  -- Raw data; constant wavelength paralle to rows
*
* R*4 FLAT(NXS,NYS)  -- Multiplicative flat field correction frame so that
*                     FLAT(I,J)*DATA(I,J) corrects for sensitivity variations.
*                     Linear data.
*
* R*4 SKY(NXS,NYS)   -- picture containing estimate of sky background for every
*                     pixel to be included in the extraction.
*
* R*4 FRACT(NXS,NYS) -- picture containing estimated fraction of light falling
*                     into every pixel that will be used in the fit.
*
* I*4 NXS, NYS      -- Size of section under consideration
*
* I*4 XLO, YLO      -- Lower left corner of section
*
* R*4 LEFT, RIGHT   -- X-limits of extraction region as computed at Y = (1+NY)/2
*                      at all other Y these will be shifted by an amount
*                      according to the fitted position of the spectrum given by
*                      a track file
*
*
* Noise model is readout noise plus photon noise with a scale factor
* to account for the number of photons per count. i.e.
* Variance = READOUT**2 + DATA/PHOTON
*
*  R*4 READOUT      -- RMS readout noise (in data numbers)
*
*  R*4 PHOTON       -- Equivalent number of photons per data number
*
*  L*4 ZAPRATS      -- Program will attempt to remove cosmic rats if ZAPRATS
*                      is .TRUE.
*
*  R*4 RATLO, RATHI -- Cosmic rat upper and lower rejection thresholds.
*
*  L*4 PLOT         -- .TRUE. then plots can be made
*
* The following variables take account of the noise added by a specific
* form of sky background estimate whereby low order polynomials are fitted
* along rows. If this is not how SKY(NXS,NYS) was estimated, the program will
* estimate the uncertainties incorrectly and should be altered.
*
*  I*4 SLO, NS      -- Start of sky mask and number of pixels
*
*  I*4 MASK(NS)     -- Sky mask 1 for sky pixels, 0 otherwise. An array to
*                      tell the program which pixels were used to estimate
*                      the sky background.
*
*  I*4 NPSKY        -- Number of polynomial terms used for sky fit.
*
*  R*4 VARMAT(NVARM, NVARM) where NVARM = INT(RIGHT)-NINT(LEFT)+3
*                   -- Another workspace array.
*
*  I*4 ISAVE        -- The sky background will be averaged with a running
*                      mean to lower noise in variance estimates (Note
*                      the actual subtraction of the sky is still carried
*                      out with the raw estimates passed through SKY).
*                      ISAVE pixels are used (an odd number)
*
* Output:
*
*  R*4 SPEC(NYS) The extracted 1-D spectrum
*  R*4 SPECVAR(NYS) variance estimates
*  R*4 POS X position of trace at centre in dispersion direction
*
* The optimal extraction algorithm for CCD data is described
* in Horne(1986) PASP 98, 609.
*
* The optimal 1-D spectrum estimate is obtained by a weighted
* least squares fit of the seeing profile model to 2-D data
* at each wavelength. For this routine, such a fit must already
* have been done (e.g. see routine FRACFIT).
*
* Balance corrections are applied to the data on a pixel-by-pixel basis.
* Data should not be balanced before running this routine as then the
* estimates of variance may be incorrect.
*
* Cosmic ray hits and other detector defects near the spectrum
* are eliminated by a sigma-clipping algorithm, or by approximate
* poisson probability in case of low count rate.
*
*
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INTEGER NXS, NYS, STATUS, XLO, YLO, SLO, NS, MASK(NS), NPSKY
      INTEGER NVARM, ISAVE, IX, IY, IXLO, IXHI, NVAR, NRATS, NPLOT
      INTEGER IY2, NOK, IXMIN, IXMAX, ISUB, NLOOP, NREJT, ICYCLE
      INTEGER I, J, K, I2, IREJL, IREJH, IX1, ID, IREJ
      REAL RIGHT, LEFT, PFAC, CMIN, XSHIFT, VAL
      REAL SPEC(NYS), SPECVAR(NYS), SKAVE, X1, X2
      REAL DATA(NXS,NYS), SKY(NXS,NYS), FLAT(NXS,NYS)
      REAL FRACT(NXS,NYS), POS, EPS, VT, READOUT, PHOTON
      REAL VARMAT(NVARM,NVARM), SK, BAL, DAT, A1, A2
      REAL VAR0, STAR, VAR, VNORM, RMAXL, RMAXH
      REAL VOLD, RATIO, VMINL, VMINH, OUT, QF, YMIN, YMAX
      REAL RANGE, XMN, XMX, RSCALE, VMIN, DARK(NXS,NYS)
      DOUBLE PRECISION SUMD, SUMD2, SUM, WSUM, WVSUM
      DOUBLE PRECISION YREF, XREF, XD, YD
C
      INTEGER MAXX, MAXY
      PARAMETER (MAXX=1000, MAXY=5000)
      REAL PROF(MAXX), WEIGHT(MAXX), XPLOT(MAXX)
      REAL YPLOT(MAXX), FPLOT(MAXX), EPLOT(MAXX)
      REAL BPLOT(MAXX), SSAVE(MAXY), SAVE(MAXY)
      REAL RATLO, RATHI, CHANCE, ALPHA, BETA
      LOGICAL PLOT, MORE, ZAPRATS
      CHARACTER*8 WHAT
      CHARACTER*40 TITLE
      CHARACTER*(*) DEVICE
      INTEGER PGOPEN
      SAVE ALPHA, BETA
      DATA ALPHA,BETA/3.,2./
C
      IF(STATUS.NE.SAI__OK) RETURN
C
C     Check input arguments
C
      IF(NYS .GT. MAXY) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI('MAX',MAXY)
         CALL ERR_REP(' ',
     &        'Maximum y-dimension = ^MAX',STATUS)
         RETURN
      ELSE IF( NXS .GT. MAXX ) THEN
         CALL MSG_SETI('MAX',MAXX)
         CALL ERR_REP(' ',
     &        'Maximum x-dimension = ^MAX',STATUS)
         RETURN
      ELSE IF(ISAVE.EQ.2*(ISAVE/2)) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','ISAVE should be odd', STATUS)
         RETURN
      END IF
C
      DO IY=1, NYS
         SPEC(IY) = 0.
         SPECVAR(IY) = 0.
      END DO
C
C     Factor for weighting low count cases. This is
C     derived by requiring that pixels cannot be
C     incorrectly weighted by more than a factor BETA
C     for an ALPHA sigma fluctuation. For CCDs this should
C     be unimportant (likely to be zero) whereas for
C     photon counting detectors, it is important.
C
      VAR0 = READOUT*READOUT
      PFAC = 1./PHOTON
      CMIN = PFAC*(ALPHA*BETA/(BETA-1.))**2. - VAR0*PHOTON
      CMIN = MAX(0., CMIN)
C
C     Compute sky fit covariance matrix
C
      IXLO = NINT(LEFT)  - 1
      IXHI = NINT(RIGHT) + 1
      NVAR = IXHI - IXLO + 1
      IF(NPSKY.GT.0) THEN
         CALL VARSKY(MASK,SLO,NS,NPSKY,IXLO,NVAR,VARMAT,STATUS)
         IF(STATUS.NE.SAI__OK) RETURN
      END IF
C
      NRATS = 0
      CALL GET_TRACK(YREF, XREF, STATUS)
      IF(STATUS.NE.SAI__OK) RETURN
      POS  = REAL(XREF)
C
C     Spectrum extraction loop
C     First compute average of sky background
C
      DO IY = 1, NYS
         CALL GET_TRACK(DBLE(IY+YLO-1), XD, STATUS)
         IF(STATUS.NE.SAI__OK) RETURN
         XSHIFT = REAL(XD - XREF)
         IXMIN  = NINT(LEFT+XSHIFT)-(XLO-1)
         IXMAX  = NINT(RIGHT+XSHIFT)-(XLO-1)
         IXMIN  = MAX(1, MIN(NXS, IXMIN))
         IXMAX  = MAX(1, MIN(NXS, IXMAX))
         SKAVE = 0.
         NOK   = 0
         DO IX = IXMIN, IXMAX
            IF(SKY(IX,IY).NE.VAL__BADR .AND.
     &           DARK(IX,IY).NE.VAL__BADR .AND.
     &           FLAT(IX,IY).NE.VAL__BADR) THEN
               SKAVE = SKAVE+SKY(IX,IY)+FLAT(IX,IY)*DARK(IX,IY)
               NOK = NOK + 1
            END IF
         END DO
         IF(NOK.GT.0) THEN
            SSAVE(IY) = SKAVE/REAL(NOK)
         ELSE
            SSAVE(IY) = VAL__BADR
         END IF
      END DO
C
C     Take running average if needed
C
      IF(ISAVE.GT.1) THEN
         SUM = 0.
         IY2 = MIN(NYS,1+ISAVE/2)
         NOK = 0
         DO IY = 1, IY2
            IF(SSAVE(IY).NE.VAL__BADR) THEN
               SUM = SUM + SSAVE(IY)
               NOK = NOK + 1
            END IF
         END DO
         IF(NOK.GT.0) THEN
            SAVE(1) = REAL(SUM/REAL(NOK))
         ELSE
            SAVE(1) = VAL__BADR
         END IF
         DO IY = 2, NYS
            IF(IY.GT.1+ISAVE/2) THEN
               VAL = SSAVE(IY-ISAVE/2-1)
               IF(VAL.NE.VAL__BADR) THEN
                  SUM = SUM - VAL
                  NOK = NOK - 1
               END IF
            END IF
            IF(IY.LE.NYS-ISAVE/2) THEN
               VAL = SSAVE(IY+ISAVE/2)
               IF(VAL.NE.VAL__BADR) THEN
                  SUM = SUM + SSAVE(IY+ISAVE/2)
                  NOK  = NOK + 1
               END IF
            END IF
            IF(NOK.GT.0) THEN
               SAVE(IY) = REAL(SUM/REAL(NOK))
            ELSE
               SAVE(IY) = VAL__BADR
            END IF
         END DO
      ELSE
         DO IY = 1, NYS
            SAVE(IY) = SSAVE(IY)
         END DO
      END IF
C
      DO IY = 1, NYS
         IF(SAVE(IY).NE.VAL__BADR) THEN
            CALL GET_TRACK(DBLE(IY+YLO-1), XD, STATUS)
            IF(STATUS.NE.SAI__OK) RETURN
            XSHIFT = REAL(XD - XREF)
            X1     = LEFT +XSHIFT-REAL(XLO-1)
            X2     = RIGHT+XSHIFT-REAL(XLO-1)
            IXMIN  = NINT(X1)
            IXMAX  = NINT(X2)
            IXMIN  = MAX(1, MIN(NXS, IXMIN))
            IXMAX  = MAX(1, MIN(NXS, IXMAX))
            NPLOT  = IXMAX-IXMIN + 1
C
            SUM    = 0.
            WSUM   = 0.
            SKAVE  = SAVE(IY)
            NOK    = 0
            DO IX = IXMIN, IXMAX
               XPLOT(IX)  = REAL(IX+XLO-1)
               IF(DATA(IX,IY).NE.VAL__BADR .AND.
     &              FLAT(IX,IY).NE.VAL__BADR .AND.
     &              DARK(IX,IY).NE.VAL__BADR .AND.
     &              SKY(IX,IY).NE.VAL__BADR) THEN
                  BAL  = FLAT(IX,IY)
                  SK   = SKY(IX,IY)+BAL*DARK(IX,IY)
                  DAT  = BAL*DATA(IX,IY)
                  STAR = DAT-SK
                  A1   = REAL(IX)+0.5-X1
                  A2   = X2-REAL(IX)+0.5
                  VAR  = BAL*(BAL*VAR0
     &                 + PFAC*MAX(CMIN,ABS(DAT),ABS(SKAVE)))
                  NOK = NOK + 1
                  YPLOT(IX)  = STAR
                  EPLOT(IX)  = SQRT(VAR)
                  BPLOT(IX)  = BAL
                  PROF(IX)   = FRACT(IX,IY)
                  WEIGHT(IX) = MAX(0.,MIN(1.,A1,A2))
                  SUM        = SUM + WEIGHT(IX)*STAR
                  WSUM       = WSUM + PROF(IX)*WEIGHT(IX)
               ELSE
                  YPLOT(IX)  = VAL__BADR
                  EPLOT(IX)  = -1.
               END IF
            END DO
            VNORM = VAR0+PFAC*MAX(0.,SKAVE)
C
C     initial estimate of the spectrum is a normal
C     sum across the profile.
C
            SPEC(IY) = REAL(SUM/WSUM)
C
C     loop to reject cosmic rays and saturated pixels
C     Up to 8 pixels can be rejected, one at a time.
C
            ISUB = NINT(REAL(IXLO-XLO+1)+XSHIFT) - 1
            IF(ZAPRATS) THEN
               NLOOP  = 8
               NREJT  = 0
               ICYCLE = 0
C
C     Initialise variables which will be used to correct
C     for the correlation between the spectrum point and the
C     particular data point (01/07/88)
C
               WVSUM = 0.D0
               WSUM  = 0.D0
               SUMD2 = 0.D0
               DO IX = IXMIN, IXMAX
                  IF(YPLOT(IX).NE.VAL__BADR) THEN
                     WSUM = WSUM + WEIGHT(IX)*PROF(IX)
                     STAR = SPEC(IY)*PROF(IX)
                     BAL  = BPLOT(IX)
                     WVSUM = WVSUM + WEIGHT(IX)**2*
     &                    BAL*(BAL*VAR0+PFAC*ABS(STAR+SKAVE))
                     SUMD = 0.
                     IF(NPSKY.GT.0) THEN
                        J = IX - ISUB
                        DO I2 = IXMIN, IXMAX
                           IF(YPLOT(I2).NE.VAL__BADR) THEN
                              SUMD = SUMD + WEIGHT(I2)*
     &                             VARMAT(I2-ISUB,J)
                           END IF
                        END DO
                        SUMD2 = SUMD2 + WEIGHT(IX)*SUMD
                     END IF
                  END IF
               END DO
C
               MORE = .TRUE.
               DO WHILE(ICYCLE.LE.1 .OR.
     &              (NREJT.LT.NLOOP.AND.MORE) )
                  ICYCLE = ICYCLE + 1
                  IREJL  = 0
                  RMAXL  = -1.
                  IREJH  = 0
                  RMAXH  = -1.
                  DO IX = IXMIN, IXMAX
C
C     Compute revised variance estimate using profile model
C     Skip rejected pixels, find worst pixel. At least one
C     extra run is forced to take advantage of the improved
C     estimate of the spectrum, even if no pixel is rejected.
C
                     IF(EPLOT(IX).GT.0.) THEN
                        STAR = SPEC(IY)*PROF(IX)
                        FPLOT(IX) = STAR
                        STAR = MAX(PFAC*PROF(IX), STAR)
                        BAL = BPLOT(IX)
                        VAR = BAL*(BAL*VAR0
     &                       +MAX(ABS(SKAVE),ABS(STAR+SKAVE))/
     &                       PHOTON)
C
                        VOLD = EPLOT(IX)**2
                        EPLOT(IX) = SQRT(VAR)
C
C     A correction is made before calling CHANCE to account for
C     correlation between the estimated spectrum and the
C     data value. This increases the chance of rejection on
C     data points with high weights which was too low before.
C     Approximate decrease in VAR by (1-f*f/Sum of f*f) where f
C     is the profile factor.
C     (01/07/88 TRM)
C
                        VAR = REAL(VOLD*(1.-2.*WEIGHT(IX)*PROF(IX)/
     &                       WSUM)+(WVSUM+SUMD2*VNORM)*
     &                       (PROF(IX)/WSUM)**2)
C
C     Avoid crashes in zero photon case by pretending that
C     there is 0.2 of a photon. This will be a bad approximation
C     if there are very many such instances since then the
C     chance of even 0.1 photons may be small.
C
                        VAR = SQRT(MAX(0.2, VAR))
C
C     A floor to var is added for the test on the assumption that
C     the fractional flux is at best accurate to EPS
C     This reduces the chances of silly rejections when the smooth
C     model is imperfect as can happen due to imperfection of CCD
C     Purely empirical
C
                        VT = MAX(VAR, EPS*FPLOT(IX))
C
C     Now test point
C
                        RATIO = CHANCE(YPLOT(IX),FPLOT(IX),VT,
     &                       RATLO, RATHI)
C
C     RATIO = -1 shows that the point is OK. >0 shows that
C     it has -LOG(Prob) of being so bad.
C
                        IF(RATIO.GT.-0.5) THEN
                           IF(YPLOT(IX).GT.FPLOT(IX)) THEN
                              IF(RATIO.GT.RMAXH) THEN
                                 RMAXH = RATIO
                                 VMINH = VAR
                                 IREJH = IX
                              END IF
                           ELSE
                              IF(RATIO.GT.RMAXL) THEN
                                 RMAXL = RATIO
                                 VMINL = VAR
                                 IREJL = IX
                              END IF
                           END IF
                        END IF
                     ELSE
C
C     Recompute values for plot purposes
C
                        STAR = SPEC(IY)*PROF(IX)
                        FPLOT(IX) = STAR
                        STAR = MAX(PFAC*PROF(IX),STAR)
                        IF(YPLOT(IX).NE.VAL__BADR) THEN
                           BAL  = BPLOT(IX)
                           VAR = BAL*(BAL*VAR0
     &                          +MAX(ABS(SKAVE),ABS(STAR+SKAVE))
     &                          /PHOTON)
                           EPLOT(IX) = - SQRT(VAR)
                        END IF
                     END IF
                  END DO
C
C     Reject worst outlier
C
                  IF(IREJH.GT.0 .OR. IREJL.GT.0) THEN
                     NREJT = NREJT + 1
                     IF(IREJH.GT.0) THEN
                        IREJ = IREJH
                        VMIN = VMINH
                     ELSE
                        IREJ = IREJL
                        VMIN = VMINL
                     END IF
                     EPLOT(IREJ) = - ABS(EPLOT(IREJ))
                     OUT = (YPLOT(IREJ)-FPLOT(IREJ))/VMIN
C
C     Report reject
C
                     CALL MSG_SETI('IREJ',IREJ+XLO-1)
                     CALL MSG_SETI('IY',IY+YLO-1)
                     CALL MSG_SETR('OUT',OUT)
                     CALL MSG_OUT(' ',
     &                    'Rejected ^OUT sigma outlier'//
     &                    ' at X = ^IREJ, Y = ^IY',STATUS)
C
                     MORE = .TRUE.
                  ELSE
                     MORE = .FALSE.
                  END IF
C
C     Load optimal weights, with appropriate reduced weight for
C     partial pixels to ensure smooth behaviour with wavelength
C
                  DO IX = IXMIN, IXMAX
                     IF(EPLOT(IX).GT.0.) THEN
                        A1 = REAL(IX)+0.5-X1
                        A2 = X2-REAL(IX)+0.5
                        QF = MAX(0., MIN(1., A1, A2))
                        WEIGHT(IX) = QF*PROF(IX)/EPLOT(IX)**2
                     END IF
                  END DO
C
                  SUM   = 0.D0
                  WSUM  = 0.D0
                  SUMD2 = 0.D0
                  WVSUM = 0.D0
                  DO IX = IXMIN, IXMAX
                     IF(EPLOT(IX).GT.0.) THEN
                        SUM   = SUM  + WEIGHT(IX)*YPLOT(IX)
                        WSUM  = WSUM + WEIGHT(IX)*PROF(IX)
                        WVSUM = WVSUM + (WEIGHT(IX)*EPLOT(IX))**2
                        SUMD = 0.
                        IF(NPSKY.GT.0) THEN
                           J = IX-ISUB
                           DO IX1 = IXMIN, IXMAX
                              IF(EPLOT(IX1).GT.0.) THEN
                                 SUMD = SUMD + WEIGHT(IX1)*
     &                                VARMAT(IX1-ISUB,J)
                              END IF
                           END DO
                           SUMD2 = SUMD2 + WEIGHT(IX)*SUMD
                        END IF
                     END IF
                  END DO
C
C     stow optimal spectrum and its standard deviation
C
                  IF(WSUM.GT.0.) THEN
                     SPEC(IY)    = REAL(SUM/WSUM)
                     SPECVAR(IY) = REAL((WVSUM+SUMD2*VNORM)/WSUM**2)
                  ELSE
                     SPEC(IY)    = VAL__BADR
                     SPECVAR(IY) = VAL__BADR
                     CALL MSG_SETI('IY',IY+YLO-1)
                     CALL MSG_OUT(' ',
     &                    'Rejected whole of row ^IY',STATUS)
                  END IF
               END DO
               NRATS = NRATS + NREJT
            ELSE
C
C     If not cosmic ray zapping, just give
C     normal weighted estimate, with an
C     extra loop to refine the variances.
C
               DO J = 1, 2
C
C     Load optimal weights
C
                  DO IX = IXMIN, IXMAX
                     IF(YPLOT(IX).NE.VAL__BADR) THEN
                        A1 = REAL(IX)+0.5-X1
                        A2 = X2-REAL(IX)+0.5
                        QF = MAX(0., MIN(1., A1, A2))
                        WEIGHT(IX) = QF*PROF(IX)/EPLOT(IX)**2
                     END IF
                  END DO
                  VNORM = VAR0+PFAC*MAX(0.,SKAVE)
C
                  SUM   = 0.D0
                  WSUM  = 0.D0
                  SUMD2 = 0.D0
                  WVSUM = 0.D0
                  DO IX = IXMIN, IXMAX
                     IF(YPLOT(IY).NE.VAL__BADR) THEN
                        SUM   = SUM  + WEIGHT(IX)*YPLOT(IX)
                        WSUM  = WSUM + WEIGHT(IX)*PROF(IX)
                        WVSUM = WVSUM + (WEIGHT(IX)*EPLOT(IX))**2
                        SUMD = 0.
                        IF(NPSKY.GT.0) THEN
                           K = IX-ISUB
                           DO IX1 = IXMIN, IXMAX
                              IF(YPLOT(IX1).NE.VAL__BADR) THEN
                                 SUMD = SUMD + WEIGHT(IX1)*
     &                                VARMAT(IX1-ISUB,K)
                              END IF
                           END DO
                           SUMD2 = SUMD2 + WEIGHT(IX)*SUMD
                        END IF
                     END IF
                  END DO
C
C     stow optimal spectrum and its standard deviation
C
                  IF(WSUM.GT.0.) THEN
                     SPEC(IY)    = REAL(SUM/WSUM)
                     SPECVAR(IY) = REAL((WVSUM+SUMD2*VNORM)/WSUM**2)
                  ELSE
                     SPEC(IY)    = VAL__BADR
                     SPECVAR(IY) = VAL__BADR
                     CALL MSG_SETI('IY',IY+YLO-1)
                     CALL MSG_OUT(' ',
     &                    'Rejected whole of row ^IY',STATUS)
                  END IF
C
                  IF(J.LT.2) THEN
                     DO IX = IXMIN, IXMAX
                        IF(YPLOT(IX).NE.VAL__BADR) THEN
                           STAR = MAX(1., SPEC(IY))*PROF(IX)
                           BAL = BPLOT(IX)
                           VAR = BAL*(BAL*VAR0
     &                          +MAX(ABS(SKAVE),PFAC*
     &                          ABS(STAR+SKAVE)))
                           EPLOT(IX) = SQRT(VAR)
                        END IF
                     END DO
                  END IF
               END DO
            END IF
C
C     Code to plot seeing profile with cosmic rat
C
            IF(SPEC(IY).NE.VAL__BADR .AND. PLOT .AND.
     &           NREJT.GT.0) THEN
 100           WRITE(*,'(A,$)') 'Plot rejected point profile ? [N] '
               READ(*,'(A)') WHAT
               CALL UPPER_CASE(WHAT)
               IF(WHAT.NE.' '.AND. WHAT.NE.'N'.AND.WHAT.NE.'Y') GOTO 100
               IF(WHAT.EQ.'Y') THEN
C
                  WRITE(TITLE,'(''ROW'',I4,3X)') IY+YLO-1
                  ID = PGOPEN(DEVICE)
                  IF(ID.GT.0) THEN
C
                     YMIN =  1.E20
                     YMAX = -1.E20
                     DO I = 1, NXS
                        IF(YPLOT(I).NE.VAL__BADR) THEN
                           YMIN = MIN(YMIN, YPLOT(I))
                           YMAX = MAX(YMAX, YPLOT(I))
                        END IF
                     END DO
                     RANGE = MAX(10.,YMAX-YMIN)
                     YMIN  = MIN(YMIN - RANGE/10., 0.)
                     YMAX  = YMAX + RANGE/10.
                     XMN   = REAL(IXMIN+XLO-1)-1.
                     XMX   = REAL(IXMAX+XLO-1)+1.
                     CALL PGSCI(5)
                     CALL PGENV(XMN, XMX, YMIN, YMAX, 0, 1)
                     CALL PGSCI(7)
                     CALL PGLAB('X-column', 'Profile', TITLE)
C
                     DO IX = IXMIN, IXMAX
                        IF(EPLOT(IX).GT.0.) THEN
                           CALL PGSCI(3)
                           CALL PGMOVE(XPLOT(IX),YPLOT(IX)-EPLOT(IX))
                           CALL PGDRAW(XPLOT(IX),YPLOT(IX)+EPLOT(IX))
                           CALL PGSCI(1)
                           CALL PGPT(1,XPLOT(IX), YPLOT(IX), 5 )
                        ELSE IF(YPLOT(I).EQ.VAL__BADR) THEN
                           CALL PGSCI(2)
                           CALL PGPT(1,XPLOT(I),0.,5)
                        ELSE
                           CALL PGSCI(2)
                           CALL PGMOVE(XPLOT(IX),YPLOT(IX)-EPLOT(IX))
                           CALL PGDRAW(XPLOT(IX),YPLOT(IX)+EPLOT(IX))
                           CALL PGPT( 1, XPLOT(IX), YPLOT(IX), 4 )
                        END IF
                     END DO
C
                     RSCALE = (SPEC(IY)+SQRT(SPECVAR(IY)))/SPEC(IY)
                     DO IX=IXMIN, IXMAX
                        FPLOT(IX) = RSCALE*FPLOT(IX)
                     END DO
                     CALL PGSCI(7)
                     CALL PGLINE(NPLOT,XPLOT(IXMIN),FPLOT(IXMIN))
C
                     RSCALE = RSCALE*RSCALE
                     DO IX=IXMIN,IXMAX
                        FPLOT(IX) = FPLOT(IX)/RSCALE
                     END DO
                     CALL PGLINE(NPLOT,XPLOT(IXMIN),FPLOT(IXMIN))
                     CALL PGCLOS
                  END IF
               END IF
            END IF
         ELSE
            SPEC(IY)    = VAL__BADR
            SPECVAR(IY) = VAL__BADR
            CALL MSG_SETI('IY',IY+YLO-1)
            CALL MSG_OUT(' ','No sky values at Y = ^IY',STATUS)
         END IF
      END DO
      IF(ZAPRATS) THEN
         CALL MSG_SETI('NRATS',NRATS)
         CALL MSG_OUT(' ','Total pixels rejected = ^NRATS',
     &        STATUS)
      END IF
      RETURN
      END

