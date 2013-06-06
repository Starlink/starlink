*PROFIT
*
* PROFIT  -- Implements the fitting method described in
*            1989, PASP, 101, 1032 for fitting the fractions needed
*            for optimal extraction when the spectrum is tilted or curved.
*
* There are circumstances when extopt behaves poorly because the spectra
* are too strongly tilted. This is when 'profit' followed by 'optext'
* should be used.
*
* In principle PROFIT should collapse with a dead-straight spectrum as the
* linear equations become ill-conditioned. However I have yet to have it do
* this and even on spectra tilted by less than a pixel it has been fine.
* However you should keep an eye out if you are trying to apply it very
* straight spectra. Use extopt if it does fail in such cases.
*
*
* Parameters are:
*
*  IMAGE   -- The data file under analysis.
*
*  FLAT    -- balance frame.
*
*  DLOAD   -- TRUE if dark frame is required.
*
*  DARK    -- Dark frame representing counts unaffected by slit
*             profile. This will be subtracted off data before applying
*             the balance factors.
*
*  REGION  -- Contains the sky/object region limits (only the object
*             limits matter in this case).
*
*  SKY     -- Contains fits to the background sky in the region of
*             the object.
*
*  TRACK   -- Contains fit to position of spectrum.
*
*  FRACT   -- Output file containing fit evaluated over region of
*             extraction.
*
*  YSTART, YEND -- The Y pixel limits of the extraction.
*
*  READOUT -- The readout noise of the detector in RMS data units/pixel
*
*  PHOTON  -- The number of photons per data number.
*
*  NPOLY   -- Number of coefficients for profile polynomial fits.
*             Should not be more than 3 or 4 normally.
*
*  SIZEX   -- The separation in pixels between the polynomials.
*
*  THRESH  -- Sigma threshold for poly fits.
*
*  NSLOW   -- Number of bad pixels to reject one by one. Standardly profit
*             rejects pixels in whole swathes. This parameter allows you to
*             force the safe-but-slow reject the single worst pixel at each
*             point. The larger you make it the safer it should be. For subtle
*             reasons, the safest values of this parameter are either 0 (which
*             gives standard old-style behaviour) or a large number like 100
*
*  BADVAL  -- Value above which a pixel will automatically be ignored.
*             Use as a way of avoiding horrendous cosmics which can
*             upset 'profit'. Should obviously be way above any feasible
*             data values.
*
*  NBLOCK  -- Data will be taken in groups of NBLOCK rows.
*
*  NMED   -- Low signals with large numbers of cosmic rays can go completely wrong
*            owing to the profiles with cosmic rays in them being given EXTRA weight. e.g.
*            if the total signal is typically 100 counts but a cosmic rays comes
*            along with 10000 counts, that profile looks particularly good. To reduce
*            this, this option median filters the profile sums before they are divided
*            in to estimate fractions. Should not be too wide if target has lots of
*            narrow line features. Must be odd. thsi number can be traded against NBLOCK
*            to some extent.
*
*  PLOT    -- TRUE to make plots.
*
*  DEVICE  -- Plot device
*
*PROFIT
      SUBROUTINE PROFIT(STATUS)
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CNF_PAR'
      INTEGER STATUS, NDIM, LBND(2), UBND(2), IMAGE, FLAT
      INTEGER REGION, SLO, SHI, SKY, NSLOW, FRACT, XLO, XHI
      INTEGER YLO, YHI, TEMP, NXS, NYS, NPOLYT, NBLOCK, NWORK, MWORK
      INTEGER EL, MPTR, RWORK, RPTR, PPOLY, IPTR, FPTR
      INTEGER SPTR, FRPTR, PPTR, TRIM(4), NPOLS, PLACE, SMALL
      INTEGER DARK, DPTR, NMED
      LOGICAL BASE, IBASE, FBASE, SBASE, THERE, PLOT, DBASE
      LOGICAL DLOAD
      REAL LEFT, RIGHT, READOUT, PHOTON, SIZEX, THRESH
      REAL BADVAL
      DOUBLE PRECISION YPOS, TOFF, YREF, XREF
      CHARACTER*64 DEVICE
      CHARACTER*(DAT__SZLOC) LOC
C
      IF(STATUS.NE.SAI__OK) RETURN
      CALL NDF_BEGIN
C
C     Open data file
C
      CALL NDF_ASSOC('IMAGE', 'READ', IMAGE, STATUS)
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
C
      CALL NDF_XGT0R(REGION,'PAMELA','REGPIC.LEFT',LEFT,STATUS)
      CALL NDF_XGT0R(REGION,'PAMELA','REGPIC.RIGHT',RIGHT,STATUS)
      IF(LEFT.GT.RIGHT .OR. LEFT.LE.0.) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Bad object limits', STATUS)
      END IF
      CALL NDF_BOUND(REGION, 1, SLO, SHI, NDIM, STATUS)
C
C     Open sky file. Number of poly coefficients is used in
C     uncertainty estimation.
C
      CALL NDF_ASSOC('SKY','READ',SKY,STATUS)
C
C     Get track file
C
      CALL GET_TRACK(YPOS, TOFF, STATUS)
C
C     There must be a reference X position in the sky region file
C
      CALL NDF_XGT0D(REGION,'PAMELA','REGPIC.YREF',YREF,STATUS)
C
C     Open fraction file
C
      CALL NDF_SECT(IMAGE,1,1,1,SMALL,STATUS)
      CALL NDF_PROP(SMALL,' ','FRACT',FRACT,STATUS)
      CALL NDF_RESET(FRACT,'Title',STATUS)
      CALL NDF_CPUT('PROFIT output',FRACT,'Title',STATUS)
C
C     What region of frame
C
      CALL NDF_ISBAS(IMAGE, IBASE, STATUS)
      CALL NDF_ISBAS(FLAT,  FBASE, STATUS)
      CALL NDF_ISBAS(DARK,  DBASE, STATUS)
      CALL NDF_ISBAS(SKY,   SBASE, STATUS)
      BASE = IBASE .AND. FBASE .AND. SBASE .AND. DBASE
      TRIM(1) = IMAGE
      TRIM(2) = FLAT
      TRIM(3) = DARK
      TRIM(4) = SKY
      CALL NDF_MBNDN('TRIM',4,TRIM,STATUS)
      IMAGE = TRIM(1)
      FLAT  = TRIM(2)
      DARK  = TRIM(3)
      SKY   = TRIM(4)

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
      END IF
C
C     Section dimensions
C
      NXS = UBND(1)-LBND(1)+1
      NYS = UBND(2)-LBND(2)+1
C
C     Enforce same X dimensions on sky region file
C
      CALL NDF_BOUND(REGION, 1, SLO, SHI, NDIM, STATUS)
      IF(SLO.NE.LBND(1) .OR. SHI.NE.UBND(1)) THEN
         CALL NDF_SECT(REGION, 1, LBND, UBND, TEMP, STATUS)
         CALL NDF_ANNUL(REGION, STATUS)
         CALL NDF_CLONE(TEMP, REGION, STATUS)
         CALL NDF_ANNUL(TEMP, STATUS)
      END IF
C
C     Fiddle with fraction output file so that only relevant
C     region is stored.
C
      CALL NDF_RESET(FRACT,'Data',STATUS)
      CALL NDF_SBND(2,LBND,UBND,FRACT,STATUS)
      CALL NDF_STYPE('_REAL',FRACT,'Data',STATUS)
C
C     Parameters
C
      CALL PAR_GDR0R('READOUT',1.E-2,0.,1.E10,.FALSE.,READOUT,STATUS)
      CALL PAR_GDR0R('PHOTON',1.,1.E-10,1.E10,.FALSE.,PHOTON,STATUS)
      CALL PAR_GDR0I('NPOLY',3,1,10,.FALSE.,NPOLYT,STATUS)
      CALL PAR_GDR0R('SIZEX',0.5,0.,1.,.FALSE.,SIZEX,STATUS)
      NPOLS = INT((RIGHT-LEFT+2.)/SIZEX)+1
      CALL PAR_GDR0R('THRESH',3.,0.,1000.,.FALSE.,THRESH,STATUS)
      CALL PAR_GDR0I('NSLOW',10,0,1000,.FALSE.,NSLOW,STATUS)
      CALL PAR_GDR0R('BADVAL',1.e8,0.,1.e20,.FALSE.,BADVAL,STATUS)
      CALL PAR_GDR0I('NBLOCK',1,1,NYS,.FALSE.,NBLOCK,STATUS)
      CALL PAR_GODD( 'NMED',  5,1,   101,.FALSE.,NMED,STATUS)
      CALL PAR_GET0L('PLOT', PLOT, STATUS)
      IF(PLOT) CALL PAR_GET0C('DEVICE', DEVICE, STATUS)
C
C     Get workspace for matrix operations
C
      NWORK = NPOLYT*NPOLS*(NPOLYT*NPOLS+3)
      CALL NDF_TEMP(PLACE, STATUS)
      CALL NDF_NEW('_DOUBLE',1,1,NWORK,PLACE,MWORK,STATUS)
      CALL NDF_MAP(MWORK,'Data','_DOUBLE','WRITE',MPTR,EL,STATUS)
C
C     Get workspace for rejection array
C
      CALL NDF_TEMP(PLACE, STATUS)
      CALL NDF_NEW('_INTEGER',1,1,NXS*NYS/NBLOCK,PLACE,RWORK,STATUS)
      CALL NDF_MAP(RWORK,'Data','_INTEGER','WRITE',RPTR,EL,STATUS)
C
C     Create 'pamela' extension if not already one present.
C
      CALL NDF_XSTAT(FRACT, 'PAMELA', THERE, STATUS)
      IF(.NOT.THERE) THEN
         CALL NDF_XNEW(FRACT,'PAMELA', 'Ext', 0, 0, LOC, STATUS)
         CALL DAT_NEW(LOC, 'PROFIT', 'Struct', 0, 0, STATUS)
      ELSE
         CALL NDF_XLOC(FRACT,'PAMELA','UPDATE',LOC,STATUS)
         CALL DAT_THERE(LOC, 'PROFIT', THERE, STATUS)
         IF(.NOT.THERE)
     &        CALL DAT_NEW(LOC, 'PROFIT', 'Struct', 0, 0, STATUS)
      END IF
C
C     Write values needed by 'RECOMP'
C
      CALL NDF_XPT0R(LEFT,FRACT,'PAMELA','PROFIT.LEFT',STATUS)
      CALL NDF_XPT0R(RIGHT,FRACT,'PAMELA','PROFIT.RIGHT',STATUS)
C
C     Create NDF for the polys
C
      CALL NDF_PLACE(LOC, 'PROFIT.PPOLY', PLACE, STATUS)
      LBND(1) = 1
      LBND(2) = 1
      UBND(1) = NPOLYT
      UBND(2) = NPOLS
      CALL NDF_NEW('_DOUBLE',2,LBND,UBND,PLACE,PPOLY,STATUS)
      CALL DAT_ANNUL(LOC, STATUS)
C
C     Map files
C
      CALL NDF_MAP(IMAGE,'Data','_REAL','READ',IPTR,EL,STATUS)
      CALL NDF_MAP(FLAT,'Data','_REAL','READ',FPTR,EL,STATUS)
      CALL NDF_MAP(DARK,'Data','_REAL','READ/ZERO',DPTR,EL,STATUS)
      CALL NDF_MAP(SKY,'Data','_REAL','READ',SPTR,EL,STATUS)
      CALL NDF_MAP(FRACT,'Data','_REAL','WRITE',FRPTR,EL,STATUS)
      CALL NDF_MAP(PPOLY,'Data','_DOUBLE','WRITE',PPTR,EL,STATUS)
C
C     Fit
C
      CALL FRACFIT(%VAL(CNF_PVAL(IPTR)),%VAL(CNF_PVAL(FPTR)),
     &     %VAL(CNF_PVAL(DPTR)),%VAL(CNF_PVAL(SPTR)),
     &     NXS,NYS,XLO,YLO,YREF,LEFT,RIGHT,NPOLS,NPOLYT,
     &     READOUT,PHOTON,NBLOCK,THRESH,NSLOW,BADVAL,
     &     %VAL(CNF_PVAL(MPTR)),%VAL(CNF_PVAL(RPTR)),PLOT,
     &     %VAL(CNF_PVAL(FRPTR)),%VAL(CNF_PVAL(PPTR)),
     :     XREF,DEVICE,NMED,STATUS)
C
C     Write final value need by RECOMP and also the YLO and YHI
C     values which should not change
C
      CALL NDF_XPT0D(XREF,FRACT,'PAMELA','PROFIT.XREF',STATUS)
      CALL NDF_XPT0I(YLO,FRACT,'PAMELA','PROFIT.YLO',STATUS)
      CALL NDF_XPT0I(YHI,FRACT,'PAMELA','PROFIT.YHI',STATUS)
      CALL NDF_END(STATUS)
      RETURN
      END
*
      SUBROUTINE FRACFIT(DATA,FLAT,DARK,SKY,NXS, NYS,XLO,YLO,
     &     YREF, LEFT, RIGHT, NPOLS, NPOLY, READOUT, PHOTON,
     &     NBLOCK, THRESH, NSLOW, BADVAL, WORK, REJECT, PLOT,
     &     FRACTION, PROFILE, XREF, DEVICE, NMED, STATUS)
*
* F R A C F I T 2
*
* Written by T.R. Marsh February 1989
*
* Modified 31/5/89 to return the polynomial coefficients.
*
* Optimal extraction needs to know what fraction of the spectrum has fallen
* into each pixel. For small tilts on a spectrum this is easily handled with
* the standard method of Horne (fitting low order polynomials to each column).
* However, for large tilts, this loses its power since higher order and more
* polynomials are needed. Removing the tilt does not work because, apart from
* introducing correlations during the inevitable rebinning process, the spectrum
* may be undersampled.
*
* This routine uses the fact that we can trace the position of the spectrum
* to reduce the number of polynomial terms needed. This routine assumes that
* lines of constant wavelength are roughly parallel to the rows. If this is not
* the case, it may still work, but it would be better to alter the initial
* estimate stage to cope with a tilted slit.
*
* Inputs:
*
* R*4 DATA(NXS,NYS)  --  The raw data frame with no correction of any sort
*                        applied.
*
* R*4 FLAT(NXS,NYS)  --  Multiplicative flat field correction frame. Typically
*                        this should contain numbers of order unity such that
*                        FLAT(I,J)*DATA(I,J) is the corrected number.
*
* R*4 DARK(NXS,NYS)  --  Frame that does not participarte in flat field, so
*                        that true correction is FLAT*(DATA-DARK)+DARK
*
* R*4 SKY(NXS,NYS)    --  Sky frame evaluated for all pixels relevant to fit.
*                         That is all pixels from LEFT to RIGHT plus 2 at either
*                         end, and taking the spectrum position into account.
*                         The sky is assumed to have been flat fielded and had
*                         the dark frame removed so that the sky subtracted
*                         data = FLAT*(DATA-DARK)-SKY
*
* I*4 NXS, NYS        --  X, Y dimensions of frame section
*
* I*4 XLO, YLO        -- Lower left corner of region under consideration
*
* R*4 LEFT, RIGHT   -- Left and right extent of object extraction. The profile
*                      is fitted over a slight larger region than this, but is
*                      normalised to 1 only in this region.
*
* I*4 NPOLS         -- Number of polynomials to fit to profile. Should be
*                      greater than NINT(RIGHT-LEFT)+2. Each polynomial is
*                      evaluated at a position which shifts with the spectrum.
*
* I*4 NPOLY         -- Order of polynomials. Typically of order 3 or 4. Takes
*                      any change of profile width with Y into account, or any
*                      long term mis-fit to the spectrum position.
*
* Noise model of detector, readout noise plus photon noise with a scale
* factor to account for any conversion between photons and counts.
*
* Variance on a pixels value = READOUT**2 + DATA/PHOTON
*
* R*4 READOUT       -- Readout noise of detector in RMS data numbers.
*                      Used for estimating noise.
*
* R*4 PHOTON        -- Number of photons per data number. Used for estimating
*                      noise.
*
* I*4 NBLOCK        -- Number of rows per fit point. Only use greater than 1
*                      if signal weak. Rows are then combined in groups of
*                      NBLOCK
*
* R*4 THRESH        -- Rejection threshold, sigma. Points further than THRESH
*                      sigma from the fit are rejected and the fit is re-done.
*
* I*4 NSLOW         -- For speed, fracfit reject pixels wholesale, but this can
*                      cause difficulties. NSLOW forces it to reject the worst
*                      pixels one by one, for the first NSLOW at least
*
* R*4 BADVAL        -- Value above which rejection is automatic
*
* R*8 WORK(NPOLS*NPOLY*(NPOLS*NPOLY+3)) -- Work space array for the least
*                      squares solution of the fit.
*
* L*4 REJECT(NX, NYS/NBLOCK) - Work space array. Used to store which
*                      pixels have been rejected during the fit.
*
* L*4 PLOT          -- .TRUE. if you want to be able to plot the fit. The
*                      program then gives you the option of plotting the
*                      estimated versus fitted fractions for each column.
*
* I*4 NMED         -- Median filter width to reduce effect of cosmic rats
*                     on initial profile sums
*
* Outputs:
*
* R*4 FRACTION(NXS,NYS)  -- Contains the fit to the fraction of flux in every
*                          pixel. This can now be used to extract the spectrum.
*
* R*8 PROFILE(NPOLY,NPOLS) -- Array containing the NPOLY coefficients of the
*                          NPOLY polynomials. This is returned so that it can
*                          be used to compute the fractions for a spectrum at
*                          a different position on the detector.
*
* R*8 XREF -- Position of spectrum at Y = YREF
*
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INTEGER NXS, NYS, XLO, YLO, NPOLS, NPOLY, NBLOCK
      INTEGER STATUS, IFAIL, NSLOW, NSOFAR
      REAL DATA(NXS,NYS), FLAT(NXS,NYS), SKY(NXS,NYS)
      REAL FRACTION(NXS,NYS), DARK(NXS,NYS)
      LOGICAL REJECT(NXS, NYS/NBLOCK), PLOT, OK
      REAL LEFT, RIGHT, READOUT, PHOTON, BADVAL
      DOUBLE PRECISION XD, YD, XREF, YREF, WORK(*)
      DOUBLE PRECISION PROFILE(NPOLY,NPOLS)
      CHARACTER*(*) DEVICE
C
C     Local variables and arrays
C
      INTEGER MAXCOFF
      PARAMETER (MAXCOFF=1000)
      INTEGER INDX(MAXCOFF), PGOPEN
      CHARACTER*10 REPLY
      DOUBLE PRECISION SUM1, SUM2, SUM3, CHISQ, D
      DOUBLE PRECISION OFFSET, HRANGE
      INTEGER MAXX, MAXY
      PARAMETER (MAXY=10000, MAXX=5000)
      LOGICAL POSIT(MAXY), MASK(MAXY), TYPE
      REAL SPROF(MAXY), YPOS(MAXY), YSUM(MAXY)
      REAL PLOTY(MAXY), XS(MAXY), SPOSIT(MAXY)
      REAL GOOD(MAXY), MFGOOD(MAXY)
      REAL PREDICT(MAXX), OBSERVE(MAXX), VARIANC(MAXX)
      REAL PROF(MAXX), VAR0, PFAC, XSHIFT, ADD, PART
      REAL RLEFT, RRIGHT, SPIX, SAVE, SAVE1, SAVE2, SAVE3
      REAL SAVE4, CHIOLD, X1, X2, SUM, DAT, FR, SK, BAL
      REAL XJL, XJL1, XJL2, XJ, XZ, XT, QFAC, XKL
      REAL XKL2, XK, QJ, QK, XADD, WEIGHT, RMAX, TFAC
      REAL PRED, RESID, THRESH, PROB, FMAX, Y1, Y2, SNORM
      INTEGER MXCYCLE, NPOINT, NSIDE, IY, IX, NNEG
      INTEGER IY1, IY2, IX1, IX2, ILO, IHI, JJ, ICYCLE, NTOTAL
      INTEGER I, JL, IL, NADD, J, IP, NTEM1, NTEM2, KL, NMED
      INTEGER NL, INDEX1, NEXT, INDEX2, INDEX3, N1, N2
      INTEGER IND1, IXL, IXR, IP1, IP2, NSUM, ID, ITL, ITH
      INTEGER IXMAX, IYMAX, NREJ, IXMIN, NPLOT, NCOUNTER
      PARAMETER (MXCYCLE=1020)
*
      IF(STATUS.NE.SAI__OK) RETURN
      IF(NPOLS.LT.1 .OR. NPOLY.LT.1 .OR. MXCYCLE.LT.0 .OR.
     &     NXS.LT.1 .OR. NYS.LT.1) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Invalid inputs to FRACFIT',STATUS)
         RETURN
      END IF
      NPOINT = NYS/NBLOCK
      IF(NYS.GT.MAXY) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','MAXY too small in FRACFIT',STATUS)
         RETURN
      END IF
      IF(NXS.GT.MAXX) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','MAXX too small in FRACFIT',STATUS)
         RETURN
      END IF
      IF(NPOLS.LE.INT(RIGHT-LEFT+2.)) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Too few polynomials in FRACFIT',STATUS)
         RETURN
      END IF
      IF(NPOLS*NPOLY .GT.MAXCOFF) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Too many polys for MAXCOFF',STATUS)
         RETURN
      END IF
      IF(NPOLS*NPOLY .GT. 400) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Too many polys for LU decomposition',STATUS)
         RETURN
      END IF
      CALL MSG_BLANK(STATUS)
      CALL MSG_SETI('XLO',XLO)
      CALL MSG_SETI('XHI',XLO+NXS-1)
      CALL MSG_SETI('YLO',YLO)
      CALL MSG_SETI('YHI',YLO+NYS-1)
      CALL MSG_OUT(' ',
     &     'Section, X: ^XLO to ^XHI, Y: ^YLO to ^YHI',STATUS)
      CALL MSG_SETI('NPOLS',NPOLS)
      CALL MSG_SETI('NPOLY',NPOLY)
      CALL MSG_OUT(' ','^NPOLS polynomials, ^NPOLY coeffs each',
     &     STATUS)
      CALL MSG_SETR('LEFT',LEFT)
      CALL MSG_SETR('RIGHT',RIGHT)
      CALL MSG_OUT(' ','Object region: ^LEFT to ^RIGHT',STATUS)
      CALL MSG_SETI('NPOINT',NPOINT)
      CALL MSG_SETI('NBLOCK',NBLOCK)
      CALL MSG_OUT(' ','^NPOINT blocks of ^NBLOCK rows each.',
     &     STATUS)
      CALL MSG_SETR('BADVAL',BADVAL)
      CALL MSG_OUT(' ',
     &     'Pixels > ^BADVAL will be automatically rejected',
     &     STATUS)
C
      VAR0   = READOUT*READOUT
      PFAC   = 1./PHOTON
C
C     These are used to scale the Y values appropriately
C
      HRANGE = DBLE(NYS-1)/2.
      OFFSET = DBLE(YLO)+HRANGE
C
      CALL GET_TRACK(YREF, XREF, STATUS)
      IF(STATUS.NE.SAI__OK) RETURN
C
C     Initialise distortion computations.
C
      DO IY = 1, NPOINT
         DO IX = 1, NXS
            REJECT(IX,IY) = .TRUE.
         END DO
      END DO
C
C     First estimate of fractions based upon simple division of pixel value
C     by sum across profile. Cannot be used directly. Later fitting process
C     will modify this array. Negative blocks are flagged as are blocks with
C     any bad pixels.
C
      DO IY = 1, NYS
         DO IX = 1, NXS
            FRACTION(IX,IY) = 0.
         END DO
      END DO
      NNEG = 0
      NREJ = 0
      DO IY = 1, NPOINT
         IY1 = NBLOCK*(IY-1) + 1
         IY2 = IY1 + NBLOCK - 1
         IF(IY.EQ.NPOINT) IY2 = NYS
         SUM1 = 0.D0
C
C     Store position for speed later
C
         YD   = DBLE(YLO-1)+DBLE(IY1+IY2)/2.
         CALL GET_TRACK(YD, XD, STATUS)
         IF(STATUS.NE.SAI__OK) RETURN
C
C     Scale Y value which is used repeatedly later from -1 to 1
C
         YPOS(IY) = REAL((YD-OFFSET)/HRANGE)
         XSHIFT   = REAL(XD - XREF)
         XS(IY)   = XSHIFT
         IX1      = NINT(LEFT+XSHIFT)-XLO+1
         IX2      = NINT(RIGHT+XSHIFT)-XLO+1
C
C     Add data into an array in X. Pixels at
C     the ends are added in partially to avoid
C     discontinuities in the sum
C
         ILO = MAX(1, IX1-1)
         IHI = MIN(NXS, IX2+1)
         ITL = MAX(1,   ILO-1)
         ITH = MIN(NXS, IHI+1)
         DO IX = ILO, IHI
            PROF(IX) = 0.
         END DO
         JJ = IY1
         OK = .TRUE.
         DO WHILE(JJ.LE.IY2 .AND. OK)

C
C     ITL and ITH extend the region over which bad pixels
C     are looked for but do not contribute to the initial
C     sum over flux. They are to cope with the matrix/vector
C     accumulation stage which runs over the edges of the
C     extraction region.
C

            OK = FLAT(ITL,JJ).NE.VAL__BADR .AND.
     &           DATA(ITL,JJ).NE.VAL__BADR .AND.
     &           DATA(ITL,JJ).LT.BADVAL    .AND.
     &           DARK(ITL,JJ).NE.VAL__BADR .AND.
     &           SKY(ITL,JJ).NE.VAL__BADR
            IF(OK) THEN
               OK = FLAT(ITH,JJ).NE.VAL__BADR .AND.
     &              DATA(ITH,JJ).NE.VAL__BADR .AND.
     &              DATA(ITH,JJ).LT.BADVAL    .AND.
     &              DATA(ITH,JJ).NE.VAL__BADR .AND.
     &              SKY(ITH,JJ).NE.VAL__BADR
            END IF
            IF(OK .AND. ILO.LT.IX1) THEN
               OK = FLAT(ILO,JJ).NE.VAL__BADR .AND.
     &              DATA(ILO,JJ).NE.VAL__BADR .AND.
     &              DATA(ILO,JJ).LT.BADVAL    .AND.
     &              DARK(ILO,JJ).NE.VAL__BADR .AND.
     &              SKY(ILO,JJ).NE.VAL__BADR
               IF(OK) THEN
                  ADD = FLAT(ILO,JJ)*(DATA(ILO,JJ)-DARK(ILO,JJ))
     &                 -SKY(ILO,JJ)
                  PROF(ILO) = PROF(ILO) + ADD
               END IF
            END IF
            IF(OK) THEN
               OK = FLAT(IX1,JJ).NE.VAL__BADR .AND.
     &              DATA(IX1,JJ).NE.VAL__BADR .AND.
     &              DATA(IX1,JJ).LT.BADVAL    .AND.
     &              DARK(IX1,JJ).NE.VAL__BADR .AND.
     &              SKY(IX1,JJ).NE.VAL__BADR
               IF(OK) THEN
                  ADD = FLAT(IX1,JJ)*(DATA(IX1,JJ)-DARK(IX1,JJ))
     &                 -SKY(IX1,JJ)
                  PROF(IX1) = PROF(IX1) + ADD
                  PART = REAL(IX1+XLO-1)+0.5-LEFT-XSHIFT
                  SUM1 = SUM1 + PART*ADD
                  OK = FLAT(IX2,JJ).NE.VAL__BADR .AND.
     &                 DATA(IX2,JJ).NE.VAL__BADR .AND.
     &                 DATA(IX2,JJ).LT.BADVAL    .AND.
     &                 DARK(IX2,JJ).NE.VAL__BADR .AND.
     &                 SKY(IX2,JJ).NE.VAL__BADR
                  IF(OK) THEN
                     ADD = FLAT(IX2,JJ)*(DATA(IX2,JJ)-DARK(IX2,JJ))
     &                    -SKY(IX2,JJ)
                     PROF(IX2) = PROF(IX2) + ADD
                     PART = RIGHT+XSHIFT+0.5-REAL(IX2+XLO-1)
                     SUM1 = SUM1 + PART*ADD
                     IF(IHI.GT.IX2) THEN
                        OK = FLAT(IHI,JJ).NE.VAL__BADR .AND.
     &                       DATA(IHI,JJ).NE.VAL__BADR .AND.
     &                       DATA(IHI,JJ).LT.BADVAL    .AND.
     &                       DARK(IHI,JJ).NE.VAL__BADR .AND.
     &                       SKY(IHI,JJ).NE.VAL__BADR
                        IF(OK) THEN
                           ADD = FLAT(IHI,JJ)*(DATA(IHI,JJ)-
     &                          DARK(IHI,JJ))-SKY(IHI,JJ)
                           PROF(IHI) = PROF(IHI) + ADD
                        END IF
                     END IF
                     IF(OK) THEN
                        IX = IX1
                        DO WHILE(OK .AND. IX.LT.IX2-1)
                           IX = IX + 1
                           OK = FLAT(IX,JJ).NE.VAL__BADR .AND.
     &                          DATA(IX,JJ).NE.VAL__BADR .AND.
     &                          DATA(IX,JJ).LT.BADVAL    .AND.
     &                          DARK(IX,JJ).NE.VAL__BADR .AND.
     &                          SKY(IX,JJ).NE.VAL__BADR
                           IF(OK) THEN
                              ADD = FLAT(IX,JJ)*(DATA(IX,JJ)-
     &                             DARK(IX,JJ))-SKY(IX,JJ)
                              SUM1 = SUM1 + ADD
                              PROF(IX) = PROF(IX) + ADD
                           END IF
                        END DO
                     END IF
                  END IF
               END IF
            END IF
            JJ = JJ  + 1
         END DO
         SPROF(IY) = REAL(SUM1)
         IF(OK .AND. SUM1.GT.0.D0) THEN
            POSIT(IY) = .TRUE.
            DO JJ = IY1, IY2
               DO IX = ILO, IHI
                  FRACTION(IX,JJ) = REAL(PROF(IX)/SUM1)
               END DO
            END DO
         ELSE IF(.NOT.OK) THEN
            POSIT(IY) = .FALSE.
            NREJ = NREJ + 1
         ELSE
            POSIT(IY) = .FALSE.
            NNEG = NNEG + 1
         END IF
      END DO
      CALL MSG_SETI('NREJ',NREJ)
      CALL MSG_SETI('NNEG',NNEG)
      CALL MSG_SETI('NPOINT',NPOINT)
      CALL MSG_OUT(' ',
     &     'There were ^NNEG negative and ^NREJ bad blocks'//
     &     ' out of ^NPOINT', STATUS)
C
C     Median filter the sums. the idea is to kick out cosmic rays
C     which can otherwise dominate the profile. Ignore bad points.
C
      IF(NMED.GT.1) THEN
         NCOUNTER = 0
         DO IY = 1, NPOINT
            IF(POSIT(IY)) THEN
               NCOUNTER = NCOUNTER + 1
               GOOD(NCOUNTER) = SPROF(IY)
            END IF
         END DO
         CALL MEDFILT(GOOD,MFGOOD,NCOUNTER,NMED,IFAIL)
         NCOUNTER = 0
         DO IY = 1, NPOINT
            IF(POSIT(IY)) THEN
               NCOUNTER = NCOUNTER + 1
               SPROF(IY) = MFGOOD(NCOUNTER)
            END IF
         END DO
      END IF
C
      RLEFT  = LEFT  - 1. - REAL(XLO-1)
      RRIGHT = RIGHT + 1. - REAL(XLO-1)
      SPIX   = (RRIGHT-RLEFT)/REAL(NPOLS)
      SAVE   = 0.5-SPIX
      SAVE1  = ABS(SAVE)
      SAVE2  = 1.-(SAVE/SPIX)**2
      SAVE3  = 0.5+SPIX
      SAVE4  = SQRT(2.)*SPIX
C
      NSIDE = NPOLS*NPOLY
      NADD  = NSIDE*NSIDE
C
C     Start fitting cycle.
C
      ICYCLE = -1
      CHISQ = 1.
      NTOTAL = 0
      NSOFAR = 0
 200  CONTINUE
      ICYCLE = ICYCLE + 1
      IF(ICYCLE.LE.4) CHIOLD = REAL(CHISQ)
      CALL MSG_BLANK(STATUS)
      CALL MSG_SETI('CYCLE',ICYCLE)
      CALL MSG_OUT(' ','Cycle number ^CYCLE',STATUS)
C
C     Intialise and evaluate least squares vector.
C
      DO J = 1, NPOLS
         DO I = 1, NPOLY
            PROFILE(I,J) = 0.D0
         END DO
      END DO
      DO JL = 1, NPOLS
         XJL  = RLEFT + SPIX*(REAL(JL)-0.5)
         XJL1 = XJL - SPIX
         XJL2 = XJL + SPIX
         SUM1 = 0.D0
         DO IY = 1, NPOINT
            IF(POSIT(IY)) THEN
               IY1 = NBLOCK*(IY-1) + 1
               IY2 = NBLOCK*IY
               IF(IY.EQ.NPOINT) IY2 = NYS
               YD = DBLE(YPOS(IY))

               XSHIFT = XS(IY)
               X1  = XJL1+XSHIFT
               X2  = XJL2+XSHIFT
               XJ  = XJL +XSHIFT
               IX1 = NINT(X1)
               IX2 = NINT(X2)
               DO IX = IX1, IX2
                  PROF(IX) = 0.
                  VARIANC(IX) = 0.
               END DO
C
C     Compute data point and variance for block.
C
               SUM   = SPROF(IY)
               SNORM = SUM/REAL(IY2-IY1+1)
               DO JJ = IY1, IY2
                  DO IX = IX1, IX2
                     IF(REJECT(IX,IY)) THEN
                        DAT = DATA(IX,JJ)
                        FR  = FRACTION(IX,JJ)
                        BAL = FLAT(IX,JJ)
                        SK  = SKY(IX,JJ)+BAL*DARK(IX,JJ)
                        VARIANC(IX) = VARIANC(IX) +
     &                       BAL*(BAL*VAR0+PFAC*(FR*SNORM+SK))
                        PROF(IX) = PROF(IX)+BAL*DAT-SK
                     END IF
                  END DO
               END DO
               SUM2 = 0.D0
               DO IX = IX1, IX2
                  IF(REJECT(IX,IY)) THEN
C
C     Evaluate QFAC, the contribution of polynomial number JL
C     for the pixel IX1,JJ. Four cases are considered. The first
C     two account for the triangular interpolation function partially
C     overlapping a pixel, on one side only. The third is for the
C     function wholly inside a pixel, and finally for the pixel wholly
C     covered by the interpolation function.
C
                     XZ = XJ-REAL(IX)
                     XT = ABS(XZ)
                     IF(XT.GE.SAVE1) THEN
                        IF(XT.GE.0.5) THEN
                           QFAC = ((XT-SAVE3)/SAVE4)**2
                        ELSE
                           QFAC = 1.- ((XT-SAVE)/SAVE4)**2
                        END IF
                     ELSE IF(XT.LE.SAVE) THEN
                        QFAC = 1.
                     ELSE
                        QFAC = SAVE2-(XZ/SPIX)**2
                     END IF
                     SUM2 = SUM2 + QFAC*SUM*PROF(IX)/VARIANC(IX)
                  END IF
               END DO
               YSUM(IY) = REAL(SUM2)
            END IF
         END DO
         DO IL = 1, NPOLY
            SUM1 = 0.D0
            IP = IL - 1
            DO IY = 1, NPOINT
               IF(POSIT(IY)) SUM1 = SUM1 + YSUM(IY)*(YPOS(IY)**IP)
            END DO
            PROFILE(IL,JL) = SUM1
         END DO
      END DO
C
C     Evaluate least squares matrix
C
      DO J = 1, NADD
         WORK(J) = 0.D0
      END DO
      NTEM1 = NSIDE - 1
      NTEM2 = NPOLY*NTEM1
      DO JL = 1, NPOLS
         XJL  = RLEFT + SPIX*(REAL(JL)-0.5)
         XJL1 = XJL - SPIX
C
C     Symmetric matrix; only evaluate half of it.
C
         DO KL = 1, JL
            XKL  = RLEFT + SPIX*(REAL(KL)-0.5)
            XKL2 = XKL + SPIX
C
C     Contribution to matrix only evaluated if two polynomial
C     terms can be affected by the same pixel.
C
            SUM1 = 0.D0
            IF(XJL1-XKL2.LT.1.) THEN
               DO IY = 1, NPOINT
                  IF(POSIT(IY)) THEN
                     IY1 = NBLOCK*(IY-1) + 1
                     IY2 = NBLOCK*IY
                     IF(IY.EQ.NPOINT) IY2 = NYS
                     XSHIFT = XS(IY)
C
C     Compute left and right limits of polynomials JL and KL
C     for this value of Y
C
                     IX1 = NINT(XJL1+XSHIFT)
                     IX2 = NINT(XKL2+XSHIFT)
                     IF(IX2.GE.IX1) THEN
                        XJ = XJL + XSHIFT
                        XK = XKL + XSHIFT
                        DO IX = IX1, IX2
                           VARIANC(IX) = 0.
                        END DO
                        SUM   = SPROF(IY)
                        SNORM = SUM/REAL(IY2-IY1+1)
                        SUM = SUM*SUM
                        DO JJ = IY1, IY2
                           DO IX = IX1, IX2
                              IF(REJECT(IX,IY)) THEN
                                 FR  = FRACTION(IX,JJ)
                                 BAL = FLAT(IX,JJ)
                                 SK  = SKY(IX,JJ)+BAL*DARK(IX,JJ)
                                 VARIANC(IX) = VARIANC(IX) +
     &                                BAL*(BAL*VAR0+PFAC*
     &                                (FR*SNORM+SK))
                              END IF
                           END DO
                        END DO
C
C     Evaluate sum over row of QFAC(JL) times QFAC(KL) where
C     QFAC(I) is fraction of polynomial I which contributes to
C     to pixel IX,JJ.
C
                        SUM2 = 0.D0
                        DO IX = IX1, IX2
                           IF(REJECT(IX,IY)) THEN
                              XZ = XJ-REAL(IX)
                              XT = ABS(XZ)
                              IF(XT.GE.SAVE1) THEN
                                 IF(XT.GE.0.5) THEN
                                    QJ = ((XT-SAVE3)/SAVE4)**2
                                 ELSE
                                    QJ = 1.- ((XT-SAVE)/SAVE4)**2
                                 END IF
                              ELSE IF(XT.LE.SAVE) THEN
                                 QJ = 1.
                              ELSE
                                 QJ = SAVE2-(XZ/SPIX)**2
                              END IF
                              IF(KL.NE.JL) THEN
                                 XZ = XK-REAL(IX)
                                 XT = ABS(XZ)
                                 IF(XT.GE.SAVE1) THEN
                                    IF(XT.GE.0.5) THEN
                                       QK = ((XT-SAVE3)/SAVE4)**2
                                    ELSE
                                       QK = 1.- ((XT-SAVE)/SAVE4)**2
                                    END IF
                                 ELSE IF(XT.LE.SAVE) THEN
                                    QK = 1.
                                 ELSE
                                    QK = SAVE2-(XZ/SPIX)**2
                                 END IF
                              ELSE
                                 QK = QJ
                              END IF
                              SUM2 = SUM2 + QJ*QK*SUM/VARIANC(IX)
                           END IF
                        END DO
                        YSUM(IY) = REAL(SUM2)
                     ELSE
                        YSUM(IY) = 0.
                     END IF
                  END IF
               END DO

               DO IL = 1, NPOLY
                  DO NL = 1, IL
                     SUM1 = 0.
                     IP = IL + NL - 2
                     DO IY = 1, NPOINT
                        IF(POSIT(IY)) THEN
                           SUM1 = SUM1 + YSUM(IY)*(YPOS(IY)**IP)
                        END IF
                     END DO
                     INDEX1 = NSIDE*(NPOLY*(JL-1)+IL-1)+
     &                    NPOLY*(KL-1)+NL
                     WORK(INDEX1) = SUM1
                     IF(NL.NE.IL) THEN
                        NEXT = NTEM1*(NL-IL)
                        INDEX2 = INDEX1 + NEXT
                        WORK(INDEX2) = SUM1
                     ELSE
                        INDEX2 = INDEX1
                     END IF
                     IF(KL.NE.JL) THEN
                        INDEX3 = INDEX2 + NTEM2*(KL-JL)
                        WORK(INDEX3) = SUM1
                        IF(NL.NE.IL) THEN
                           WORK(INDEX3-NEXT) = SUM1
                        END IF
                     END IF
                  END DO
               END DO
            END IF
         END DO
      END DO
C
C     Solve matrix equation AX = B for X. A is a real symmetric,
C     positive definite matrix, dimension (NPOLY*NPOLS)**2. X is
C     the vector representing the coefficients fitted to the
C     normalised profile.
C
      CALL LUDCMP(WORK,NSIDE,NSIDE,INDX,D,IFAIL)
      IF(IFAIL.NE.0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','LU decomposition failed',STATUS)
         RETURN
      END IF
      CALL LUBKSB(WORK,NSIDE,NSIDE,INDX,PROFILE)
C
C     Fit coefficients contained in PROFILE. Now evaluate fit.
C     First rearrange for computation speed in next section.
C
      DO J = 1, NPOLY
         DO I = 1, NPOLS
            WORK(NPOLS*(J-1)+I) = PROFILE(J,I)
         END DO
      END DO
C
C     Evaluate fit individually rather than in blocks
C     to give smooth change with Y. Evaluate over larger
C     region but renormalise over requested region.
C
      DO IY = 1, NYS
         YD = (DBLE(IY+YLO-1)-OFFSET)/HRANGE
         IF(ICYCLE.GT.0) THEN
            XSHIFT = SPOSIT(IY)
         ELSE
            CALL GET_TRACK(DBLE(IY+YLO-1), XD, STATUS)
            IF(STATUS.NE.SAI__OK) RETURN
            XSHIFT     = REAL(XD - XREF)
            SPOSIT(IY) = XSHIFT
         END IF
         IXL  = NINT(LEFT+XSHIFT)-(XLO-1)
         IXR  = NINT(RIGHT+XSHIFT)-(XLO-1)
         IX1  = IXL - 2
         IX1  = MAX(1, IX1)
         IX2  = IXR + 2
         IX2  = MIN(NXS, IX2)
         XADD = RLEFT+XSHIFT
         SUM3 = 0.
         DO IX = IX1, IX2
            X1  = REAL(IX)-XADD-0.5
            X2  = REAL(IX)-XADD+0.5
            IP1 = INT(X1/SPIX+0.5)
            IP2 = INT(X2/SPIX+1.5)
            IP1 = MAX(1, MIN(IP1, NPOLS))
            IP2 = MAX(1, MIN(IP2, NPOLS))
            SUM1 = 0.D0
            DO J = 0, NPOLY-1
               IND1 = NPOLS*J
               SUM2 = 0.D0
               DO I = IP1, IP2
                  XZ = XADD+SPIX*(REAL(I-1)+0.5)-REAL(IX)
                  XT = ABS(XZ)
                  IF(XT.GE.SAVE1) THEN
                     IF(XT.GE.0.5) THEN
                        QFAC = ((XT-SAVE3)/SAVE4)**2
                     ELSE
                        QFAC = 1.- ((XT-SAVE)/SAVE4)**2
                     END IF
                  ELSE IF(XT.LE.SAVE) THEN
                     QFAC = 1.
                  ELSE
                     QFAC = SAVE2-(XZ/SPIX)**2
                  END IF
                  SUM2 = SUM2 + QFAC*WORK(IND1+I)
               END DO
               SUM1 = SUM1 + SUM2*(YD**J)
            END DO
            FRACTION(IX,IY) = REAL(MAX(SUM1, 0.D0))
            IF(IX.GT.IXL .AND. IX.LT.IXR) THEN
               WEIGHT = 1.
            ELSE IF(IX.EQ.IXL) THEN
               WEIGHT = REAL(IXL)+0.5-LEFT-XSHIFT+REAL(XLO-1)
            ELSE IF(IX.EQ.IXR) THEN
               WEIGHT = RIGHT+XSHIFT-REAL(IXR)+0.5-REAL(XLO-1)
            ELSE
               WEIGHT = 0.
            END IF
c            write(*,*) ix, iy, weight, fraction(ix,iy)
            SUM3 = SUM3 + WEIGHT*FRACTION(IX,IY)
         END DO
         DO IX = IX1, IX2
            FRACTION(IX,IY) = REAL(FRACTION(IX,IY)/SUM3)
         END DO
      END DO
C
C     Evaluation of Chi-squared of fit, rejection of bad pixels
C
      CHISQ = 0.D0
      NSUM  = 0
      RMAX  = 0.
      IXMAX = 0
      IYMAX = 0
      NREJ  = 0
      IF(ICYCLE.LT.NSLOW) THEN
         TFAC = 2.
      ELSE IF(ICYCLE.LT.NSLOW+3) THEN
         TFAC  = 1. + 1.5*(0.5**(ICYCLE-NSLOW+1))
      ELSE
         TFAC = 1.
      END IF
      TFAC = TFAC*SQRT(CHIOLD)
      DO IY = 1, NPOINT
         IF(POSIT(IY)) THEN
            IY1 = NBLOCK*(IY-1) + 1
            IY2 = NBLOCK*IY
            IF(IY.EQ.NPOINT) IY2 = NYS
            SUM1 = 0.D0
            XSHIFT = XS(IY)
            IX1 = NINT(LEFT+XSHIFT)-(XLO-1)
            IX2 = NINT(RIGHT+XSHIFT)-(XLO-1)
            SNORM = SPROF(IY)/REAL(IY2-IY1+1)
            DO IX = IX1, IX2
               OBSERVE(IX) = 0.D0
               PREDICT(IX) = 0.D0
               VARIANC(IX) = 0.D0
            END DO
            DO JJ = IY1, IY2
               DO IX = IX1, IX2
                  BAL = FLAT(IX,JJ)
                  PRED = SKY(IX,JJ) + BAL*DARK(IX,JJ)
     &                 + MAX(0.,FRACTION(IX,JJ))*SNORM
                  PREDICT(IX) = PREDICT(IX) + PRED
                  OBSERVE(IX) = OBSERVE(IX) + BAL*DATA(IX,JJ)
                  VARIANC(IX) = VARIANC(IX) + BAL*(BAL*VAR0+PFAC*
     &                 MAX(0.,PRED))
               END DO
            END DO
            DO IX = IX1, IX2
               IF(REJECT(IX,IY)) THEN
                  NSUM = NSUM + 1
                  RESID = (OBSERVE(IX)-PREDICT(IX))/SQRT(VARIANC(IX))
                  CHISQ = CHISQ + RESID**2
C
C     NSLOW used here to prevent wholesale rejcetion
C
                  IF(ABS(RESID).GT.TFAC*THRESH .AND.
     &                 NSOFAR.GE.NSLOW) THEN
                     REJECT(IX,IY) = .FALSE.
                     RESID = RESID/SQRT(CHIOLD)
                     CALL MSG_SETI('IX',IX+XLO-1)
                     CALL MSG_SETI('IY',IY)
                     CALL MSG_SETR('SIG',RESID)
                     CALL MSG_OUT(' ',
     &               '^SIG sigma reject at X: ^IX, Y block: ^IY',
     &                    STATUS)
                     NREJ = NREJ + 1
                  ELSE IF(ABS(RESID).GT.ABS(RMAX)) THEN
                     RMAX  = RESID
                     IXMAX = IX
                     IYMAX = IY
                  END IF

               ELSE IF(NSLOW.EQ.0 .AND. ICYCLE.EQ.4) THEN
C
C     One restore cycle for pathological cases
C
                  RESID = (OBSERVE(IX)-PREDICT(IX))/SQRT(VARIANC(IX))
                  IF(ABS(RESID).LT.TFAC*THRESH) THEN
                     REJECT(IX,IY) = .TRUE.
                     RESID = RESID/SQRT(CHIOLD)
                     CALL MSG_SETI('IX',IX+XLO-1)
                     CALL MSG_SETI('IY',IY)
                     CALL MSG_SETR('SIG',RESID)
                     CALL MSG_OUT(' ',
     &                    '^SIG sigma restored at X: ^IX, Y block: ^IY',
     &                    STATUS)
                     NREJ = NREJ - 1
                  END IF
               END IF
            END DO
         END IF
      END DO
      CHISQ = CHISQ/REAL(MAX(NSUM-NPOLS*NPOLY,1))
      PROB = EXP(-MIN(80.,REAL(NSUM-NPOLS*NPOLY)*
     &     REAL(CHISQ-1.D0)**2/4.))
      CHISQ = PROB + (1.-PROB)*CHISQ
      IF(ICYCLE.LE.4) THEN
         RMAX = REAL(RMAX/SQRT(CHISQ))
      ELSE
         RMAX = REAL(RMAX/SQRT(CHIOLD))
      END IF
C
C     Rejection of points one by one
C
      IF(ABS(RMAX).GT.THRESH) THEN
         NSOFAR = NSOFAR + 1
         NREJ = NREJ + 1
         REJECT(IXMAX, IYMAX) = .FALSE.
         CALL MSG_SETI('IX',IXMAX+XLO-1)
         CALL MSG_SETI('IY',IYMAX)
         CALL MSG_SETR('SIG',RMAX)
         CALL MSG_OUT(' ',
     &        '^SIG sigma reject at X: ^IX, Y block: ^IY',
     &                    STATUS)
         WRITE(*,'(1X,F8.3,A,I4,A,I4)')
     &        RMAX,' sigma reject at X: ',IXMAX,', Y block: ',IYMAX
      END IF
      CALL MSG_SETR('CHISQ',REAL(CHISQ))
      CALL MSG_OUT(' ',
     &     'Reduced Chi-squared of fit = ^CHISQ',
     &     STATUS)
      NTOTAL = NTOTAL + NREJ
C
C     Try another fit?
C
      IF(ICYCLE.LE.3 .OR. (NREJ.GT.0 .AND. ICYCLE.LE.MXCYCLE)) GOTO 200
C
C     Fit over
C
      CALL MSG_BLANK(STATUS)
      CALL MSG_OUT(' ','Profile fit finished.',STATUS)
      WRITE(*,'(A,I5,A,I5)')
     &     ' Total number of rejected points = ',NTOTAL,
     &     ' out of ',NSUM+NTOTAL
      CALL MSG_BLANK(STATUS)
C
      IF(PLOT) THEN
C
C     Compute plot ranges
C
         IXMIN = NXS
         IXMAX = 1
         DO IY = 1, NYS
            CALL GET_TRACK(DBLE(IY+YLO-1),XD,STATUS)
            IF(STATUS.NE.SAI__OK) RETURN
            XSHIFT = REAL(XD - XREF)
            IXMIN  = MIN(IXMIN, NINT(LEFT+XSHIFT)-(XLO-1))
            IXMAX  = MAX(IXMAX, NINT(RIGHT+XSHIFT)-(XLO-1))
         END DO
         FMAX = -1.
         DO IY = 1, NYS
            DO IX = IXMIN, IXMAX
               FMAX = MAX(FRACTION(IX,IY), FMAX)
            END DO
         END DO
         X1 = REAL(YLO-1)
         X2 = REAL(NYS+YLO)
         Y1 = -0.2*FMAX
         Y2 =  1.2*FMAX
         ID = PGOPEN(DEVICE)
         IF(ID.GT.0) THEN
            CALL PGENV(X1-10, X2+10, Y1, Y2, 0, 1)
            CALL PGLAB('Y position','Fraction',' ')
            WRITE(*,'(A,I4,A,I4)')
     &           ' Plotting columns ',IXMIN+XLO-1,
     &           ' to ',IXMAX+XLO-1
            WRITE(*,*) ' '
            DO IX = IXMIN, IXMAX
 250           WRITE(*,'(A,I4)') ' Column number ',IX+XLO-1
               WRITE(*,'(A,$)') 'A(bort), S(kip), <CR> continue: '
               READ(*,'(A)') REPLY
               CALL UPPER_CASE(REPLY)
               IF(REPLY.EQ.'A') THEN
                  GOTO 400
               ELSE IF(REPLY.NE.'S' .AND. REPLY.NE.' ') THEN
                  GOTO 250
               ELSE IF(REPLY.EQ.' ') THEN
C
C     Load up new plot
C
                  NPLOT = 0
                  DO IY = 1, NPOINT
                     IF(POSIT(IY)) THEN
                        IY1 = NBLOCK*(IY-1) + 1
                        IY2 = NBLOCK*IY
                        IF(IY.EQ.NPOINT) IY2 = NYS
                        XSHIFT = XS(IY)
                        IX1 = NINT(LEFT+XSHIFT)-(XLO-1)
                        IX2 = NINT(RIGHT+XSHIFT)-(XLO-1)
                        IF(IX1.LE.IX .AND. IX2.GE.IX) THEN
                           SUM1 = 0.D0
                           SUM2 = 0.D0
                           DO JJ = IY1, IY2
                              SUM1 = SUM1 + MAX(0., FRACTION(IX,JJ))
                              SUM2 = SUM2 +
     &                             FLAT(IX,JJ)*DATA(IX,JJ)-SKY(IX,JJ)
                           END DO
                           NPLOT = NPLOT + 1
                           YPOS(NPLOT)  = REAL(YLO-1)+
     &                          REAL(IY1+IY2)/2.
                           PLOTY(NPLOT) = REAL(SUM2/SPROF(IY))
                           YSUM(NPLOT)  = REAL(SUM1/REAL(IY2-IY1+1))
                           MASK(NPLOT)  = REJECT(IX,IY)
                        END IF
                     END IF
                  END DO
C
C     Now plot
C
                  IF(NPLOT.GT.1) THEN
                     N1 = 1
                     N2 = 1
                     TYPE = MASK(1)
 300                 CONTINUE
                     N2 = N2 + 1
                     IF(TYPE.NEQV.MASK(N2)) THEN
                        IF(TYPE) THEN
                           CALL PGSCI(1)
                           CALL PGPT(N2-N1,YPOS(N1),PLOTY(N1),1)
                        ELSE
                           CALL PGSCI(3)
                           CALL PGPT(N2-N1,YPOS(N1),PLOTY(N1),3)
                        END IF
                        TYPE = .NOT.TYPE
                        N1 = N2
                     END IF
                     IF(N2.LT.NPLOT) THEN
                        GOTO 300
                     ELSE
                        IF(TYPE) THEN
                           CALL PGSCI(1)
                           CALL PGPT(N2-N1+1,YPOS(N1),PLOTY(N1),1)
                        ELSE
                           CALL PGSCI(3)
                           CALL PGPT(N2-N1+1,YPOS(N1),PLOTY(N1),3)
                        END IF
                     END IF
                     CALL PGSCI(2)
                     CALL PGLINE(NPLOT, YPOS, YSUM)
                  ELSE
                     WRITE(*,*) 'Too few points to plot'
                  END IF
               END IF
            END DO
 400        CONTINUE
            CALL PGCLOS
         END IF
      END IF
      RETURN
      END

