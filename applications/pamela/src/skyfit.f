*SKYFIT
*
* SKYFIT -- fits polynomials to sky and evaluates them in the object region. 
*
* 'skyfit' is pamela's only method for sky estimation. It assumes that
* the slit is near-parallel to horizontal rows of pixels. If it is not 
* quite parallel then a sky line will gradually move across the row and 
* the resulting variation is taken out using higher order poly fits. An 
* important benefit of this method of background estimation is that cosmic 
* rays can be fairly well removed so long as there are more than a few 
* pixels. This is done with a cautious one at a time reject cycle.
*
*
* Bad pixels: skyfit checks for bad pixels in both the flat field and
*             object frames. This allows the user to eliminate regions 
*             once and for all (by setting appropriate region of the
*             flat field) or on a frame by frame basis. If insufficient
*             pixels on a particular row are found to fit, then in the
*             fitted output file, the entire row will be set to the bad
*             value.
*
* Dark frame: if a dark frame is used, the skyfit result is returned
*             applying to true sky only, so you must use the same dark
*             frame during extraction.
* 
* Parameters:
*
*  IMAGE  -- The data under analysis
*
*  FLAT   -- Balance frame
*
*  DLOAD     -- TRUE if dark frame is required.
*
*  DARK      -- Dark frame representing counts unaffected by slit
*               profile. This will be subtracted off data before applying
*               the balance factors. 
*
*  REGION -- file defining sky and object regions.  
*
*  TRACE  -- Is a poly fit to be used to define the spectrum position?
*
*  If TRACE
*
*     TRACK -- The file containing the poly fit.
*
*  SKY    -- Output file containing the polyfit to the sky evaluated over
*            the object region only. This file will only cover the specified
*            region of the file to save space.
*
*  XSTART, XEND -- Valid range in X. Anything outside will be ignored.
*
*  YSTART, YEND -- Range over which to fit polys.
*
*  NPOLY  -- Number of coefficients to fit (1=constant, 2=linear etc).
*            The correct number to be used here is best estimated by trial
*            and error. If you end up with marked P-Cyg type residuals on
*            sky lines then maybe you have NPOLY too small, however too
*            high a value can cause excess noise especially if you are 
*            effectively attempting to interpolate across a large gap.
*            You should have a very good reason for anything above 3.
*            Remember that often there may only be a few sky lines and you
*            may be able to accept imperfect subtraction on them if it 
*            improves the rest of the spectrum.
*
* THRESH  -- Threshold in sigma for rejection of cosmic rays, bad pixels etc.
*
* READOUT -- Readout noise, RMS ADU
*
* PHOTON  -- electrons/ADU.
*
* Converted to NDF by TRM 23/01/98
*
*SKYFIT
      SUBROUTINE SKYFIT(STATUS)
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CNF_PAR'
      INTEGER STATUS, NDIM, LBND(2), UBND(2), SMALL
      INTEGER NXS, NYS, XLO, XHI, YLO, YHI
      INTEGER IMAGE, FLAT, REGION, SKY, PLACE
      INTEGER IPTR, FPTR, RPTR, SPTR, EL, TEMP
      INTEGER NPOLY, SLO, SHI, NS, DPTR, DARK, TRIM(3)
      LOGICAL TRACE, BASE, THERE, IBASE, FBASE, DBASE, DLOAD
      REAL READOUT, THRESH, PHOTON
      DOUBLE PRECISION YREF, YPOS, TOFF
      REAL LEFT, RIGHT
      CHARACTER*(DAT__SZLOC) LOC
C
C     Start
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
C     Get sky region input file
C
      CALL NDF_ASSOC('REGION','READ',REGION,STATUS)
C
      CALL NDF_XGT0R(REGION,'PAMELA','REGPIC.LEFT',LEFT,STATUS)      
      CALL NDF_XGT0R(REGION,'PAMELA','REGPIC.RIGHT',RIGHT,STATUS)    
      IF(STATUS.EQ.SAI__OK .AND. LEFT.GT.RIGHT .OR. LEFT.LE.0.) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Bad object limits', STATUS)
      END IF
C
      CALL PAR_GET0L('TRACE', TRACE, STATUS)
      IF(TRACE) THEN
         CALL GET_TRACK(YPOS, TOFF, STATUS)
         CALL NDF_XGT0D(REGION,'PAMELA','REGPIC.YREF',YREF,STATUS)    
      END IF
C
C     Get sky output file name
C
      CALL NDF_SECT(IMAGE,1,1,1,SMALL,STATUS)
      CALL NDF_PROP(SMALL,' ','SKY',SKY,STATUS)
      CALL NDF_RESET(SKY,'Title',STATUS)
      CALL NDF_CPUT('SKYFIT output',SKY,'Title',STATUS)
C
C     What region of frame
C
      CALL NDF_ISBAS(IMAGE, IBASE, STATUS)
      CALL NDF_ISBAS(FLAT,  FBASE, STATUS)
      CALL NDF_ISBAS(DARK,  DBASE, STATUS)
      BASE = IBASE .AND. FBASE .AND. DBASE
C
C     Force to same size
C
      TRIM(1) = IMAGE
      TRIM(2) = FLAT
      TRIM(3) = DARK
      CALL NDF_MBNDN('TRIM', 3, TRIM, STATUS)
      IMAGE = TRIM(1)
      FLAT  = TRIM(2)
      DARK  = TRIM(3)
C
      CALL NDF_BOUND(IMAGE, 2, LBND, UBND, NDIM, STATUS)
      IF(BASE .AND. NDIM.EQ.2) THEN
         CALL PAR_GDR0I('XSTART',LBND(1),LBND(1),UBND(1),
     &        .FALSE.,XLO,STATUS)
         CALL PAR_GDR0I('XEND',UBND(1),XLO,UBND(1),
     &        .FALSE.,XHI,STATUS)
         CALL PAR_GDR0I('YSTART',LBND(2),LBND(2),UBND(2),
     &        .FALSE.,YLO,STATUS)
         CALL PAR_GDR0I('YEND',UBND(2),YLO,UBND(2),
     &        .FALSE.,YHI,STATUS)
C
C     Replace IMAGE, FLAT, DARK with section ndfs
C 
         IF(XLO.NE.LBND(1) .OR. XHI.NE.UBND(1) .OR.
     &        YLO.NE.LBND(2) .OR. YHI.NE.UBND(2)) THEN
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
         END IF
      END IF
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
C     compute minimal range in X to use for sky output
C     
      CALL XLIMS(TRACE,YREF,LEFT,RIGHT,YLO,YHI,SLO,SHI,STATUS)
      NS = SHI-SLO+1
      LBND(1) = SLO
      UBND(1) = SHI
      CALL NDF_RESET(SKY,'Data',STATUS)
      CALL NDF_SBND(2,LBND,UBND,SKY,STATUS)
      CALL NDF_STYPE('_REAL',SKY,'Data',STATUS)
C
C     How many polyfit terms for sky ?
C     Store in sky file for use in computing uncertainty on sky fit
C
      CALL PAR_GDR0I('NPOLY',2,1,10,.FALSE.,NPOLY,STATUS)
C
C     Reject threshold in sigma units
C     
      CALL PAR_GDR0R('THRESH',3.,0.,1.E6,.FALSE.,THRESH,STATUS)
C     
C     Noise characteristics
C     
      CALL PAR_GDR0R('READOUT',1.E-2,0.,1.E10,.FALSE.,READOUT,STATUS)
      CALL PAR_GDR0R('PHOTON',1.,1.E-10,1.E10,.FALSE.,PHOTON,STATUS)
C     
C     Map 
C     
      CALL NDF_MAP(IMAGE,'Data','_REAL','READ',IPTR,EL,STATUS)
      CALL NDF_MAP(FLAT,'Data','_REAL','READ',FPTR,EL,STATUS)
      CALL NDF_MAP(DARK,'Data','_REAL','READ/ZERO',DPTR,EL,STATUS)
      CALL NDF_MAP(REGION,'Data','_INTEGER','READ',RPTR,EL,STATUS)
      CALL NDF_MAP(SKY,'Data','_REAL','WRITE',SPTR,EL,STATUS)
C     
C     Fit sky
C     
      CALL SKY_FIT(%VAL(CNF_PVAL(IPTR)), %VAL(CNF_PVAL(FPTR)), 
     &     %VAL(CNF_PVAL(DPTR)), %VAL(CNF_PVAL(RPTR)),
     &     NXS, NYS, LEFT, RIGHT, XLO, YLO, SLO, NS, 
     &     %VAL(CNF_PVAL(SPTR)), NPOLY, THRESH, READOUT, 
     &     PHOTON, TRACE, YREF, STATUS)
C     
C     Create 'pamela' extension if not already one present.
C     Store NPOLY
C     
      CALL NDF_XSTAT(SKY, 'PAMELA', THERE, STATUS)
      IF(.NOT.THERE) THEN
         CALL NDF_XNEW(SKY,'PAMELA', 'Ext', 0, 0, LOC, STATUS)
         CALL DAT_NEW(LOC, 'SKYFIT', 'Struct', 0, 0, STATUS)
      ELSE
         CALL NDF_XLOC(SKY,'PAMELA','UPDATE',LOC,STATUS)
         CALL DAT_THERE(LOC, 'SKYFIT', THERE, STATUS)
         IF(.NOT.THERE) 
     &        CALL DAT_NEW(LOC, 'SKYFIT', 'Struct', 0, 0, STATUS)
      END IF
      CALL DAT_ANNUL(LOC, STATUS)
      CALL NDF_XPT0I(NPOLY,SKY,'PAMELA','SKYFIT.NPOLY',STATUS)      
C     
C     Tidy up
C     
      CALL NDF_END(STATUS)
      RETURN
      END
C      
      SUBROUTINE SKY_FIT(DATA, FLAT, DARK, MASK, NXS, NYS, LEFT, 
     &RIGHT, XLO, YLO, SLO, NS, SKY, NPOLY, THRESH, READOUT, 
     &PHOTON, TRACE, YREF, STATUS)
C
C     SKY_FIT -- Adapted from the PAMELA routine FITSKY
C     
C     This routine fits low order (NPOLY terms) polynomials with
C     reject threshold THRESH to every Y position from YLO to YLO+NYS-1
C     It fits to the pixels specified by MASK (MASK(I) = 1 indicates 
C     that pixels I is sky). The polynomials are then evaluated in the 
C     region of the object determined by LEFT, RIGHT
C     
C     Lines of constant wavelength should be approximately parallel to 
C     the rows.
C     
C     History:
C
C     Created  June    1988 by TRM @RGO
C     Modified August  1988 to include distortion trace of spectrum
C     Modified August  1988 to include partial pixels when distorted spectrum
C     Modified May     1989 to include modification to CHANCE
C     Modified July    1990 to include modification to CHANCE
C     Modified January 1997 to include NDF changes
C     
C     Arguments:
C     
C     Inputs:
C     
C     R*4 DATA(NXS,NYS)    -- Raw data frame
C     
C     R*4 FLAT(NXS,NYS)    -- Multiplicative balance frame such that 
C                             FLAT(I,J)*DATA(I,J) is the corrected data value.
C
C     R*4 DARK(NXS,NYS)    -- Dark frame of counts not affected by slit
C     
C     I*4 MASK(NXS)       -- Sky pixel mask 1 for sky pixel, 0 otherwise
C     
C     I*4 NXS, NYS
C     
C     R*4 LEFT, RIGHT    -- X range over which polynomial estimate of sky will
C     be evaluated. These limits are shifted parallel to
C     the spectrum.
C     
C     I*4 XLO, YLO       -- Lower left corner of section.
C     
C     I*4 SLO, NS        -- Left limit of sky output, number in X 
C     
C     I*4 SKY(NS,NYS)    -- Sky output
C     
C     I*4 NPOLY          -- Number of terms for sky polynomials
C     
C     R*4 THRESH         -- Threshold for rejection of bad sky pixel
C     
C     R*4 READOUT        -- Readout noise in RMS data numbers
C     
C     R*4 PHOTON         -- Photons/data number
C     
C     L   TRACE          -- Is a track file being used
C
C     D   YREF           -- Reference Y value
C
C     I   STATUS         -- Integer error flag
C 
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      LOGICAL TRACE
      INTEGER NXS, NYS, XLO, YLO, IFAIL, SLO, NS
      INTEGER NPOLY, NPIX, NPIXTOT, NREJTOT, NFITS, IM
      INTEGER IREJ, K, NDF, ILEFT, IRIGHT, STATUS
      REAL LEFT, RIGHT, VAR0, CMIN, THRLO, THRHI
      REAL RMSAVG, XSHIFT, XP, PART, BALFAC, TOTAL, PIX
      REAL VMIN, RMS, SKYMEAN, VARI, ETA, PROB, RESCALE
      REAL RMAX, SIG, RATIO, ALPHA, BETA
      REAL DATA(NXS,NYS), FLAT(NXS,NYS), SKY(NS,NYS)
      REAL DARK(NXS,NYS), THRESH, READOUT, PHOTON
      INTEGER MASK(NXS), NSOME
      INTEGER IX, IY, NFIT, ICYCLE
      DOUBLE PRECISION XREF, YREF, YD, XD
      CHARACTER*80 DISASTER

      PARAMETER (ALPHA=3.,BETA=2.)
C
C     Work arrays for sky/object regions
C
      INTEGER MAXPOLY, MAXX
      PARAMETER (MAXPOLY = 10, MAXX = 3000)
      REAL XDATA(MAXX), SKYDATA(MAXX), SKYSIGMA(MAXX)
      REAL BALANCE(MAXX), SKYFIT(MAXX), PARTIAL(MAXX)
C
C     Functions
C
      REAL CHANCE
      DOUBLE PRECISION POLY
C
      IF(STATUS.NE.SAI__OK) RETURN
C
C     Factor for weighting low count cases. This is
C     derived by requiring that pixels cannot be
C     incorrectly weighted by more than a factor BETA
C     for an ALPHA sigma fluctuation. For CCDs this should
C     be unimportant (likely to be zero) whereas for
C     photon counting detectors, it is important.
C     
      VAR0 = READOUT*READOUT
      CMIN = (ALPHA*BETA/(BETA-1.))**2./PHOTON - VAR0*PHOTON
      CMIN = MAX(0., CMIN)
      IF(TRACE) THEN
         CALL GET_TRACK(YREF, XREF, STATUS)
         IF(STATUS.NE.SAI__OK) RETURN
      END IF
C     
C     Zero sky frame
C     
      DO IY = 1, NYS
         DO IX = 1, NS
            SKY(IX,IY) = 0.
         END DO
      END DO
C
      THRLO = - ABS(THRESH)
      THRHI =   ABS(THRESH)
C
C     Compute number of sky pixels
C
      NPIX = 0
      DO IX = 1, NXS
         IF(MASK(IX).EQ.1) NPIX = NPIX + 1
      END DO
      IF( NPIX.LT.NPOLY ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Not enough sky pixels for fit', 
     &        STATUS)
         RETURN
      END IF
C
C     Wavelength loop
C     
      NPIXTOT = 0
      NREJTOT = 0
      NFITS   = 0
      CALL MSG_BLANK(STATUS)
      RMSAVG = 0.
      NSOME = 0
C     
      DO IY = 1, NYS
C     
C     Load sky data into arrays for poly-fit
C     
         NPIX   = 0
         XSHIFT = 0.
         IF(TRACE) THEN
            CALL GET_TRACK(DBLE(IY+YLO-1), XD, STATUS)
            XSHIFT = REAL(XREF - XD)
         END IF
         DO IX = 1, NXS
C     
C     Use mask to select sky pixels. On first cycle, weights are corrected
C     by CMIN factor from above to prevent excessive weight being given to 
C     low count pixels. For low signal photon counting data this gives 
C     almost uniform weights, but it allows lower weights on CCD data.
C     
C     Partial pixels are fitted with a lower weight but are still
C     included for continuity between rows.
C     
            XP = REAL(IX) + XSHIFT
            IM = INT(XP)
C     
C     Compute amount of pixel covered by sky region
C     
            IF(IM.EQ.0 .AND. MASK(1).EQ.1) THEN

C     At left-hand end
               
               PART = XP
            ELSE IF(IM.EQ.NXS .AND. MASK(NXS).EQ.1) THEN

C     At right-hand end
               
               PART = REAL(NXS+1) - XP
            ELSE IF((IM.LT.1 .OR. IM.GE.NXS) .OR.
     &              (MASK(IM).NE.1 .AND. MASK(IM+1).NE.1)) THEN

C     Beyond ends or not sky
               
               PART = -1.
            ELSE IF(MASK(IM).EQ.1 .AND. MASK(IM+1).EQ.1) THEN

C     Totally covered by sky
               
               PART = 1.
            ELSE IF(MASK(IM).EQ.1) THEN
               PART = REAL(IM+1) - XP
            ELSE IF(MASK(IM+1).EQ.1) THEN
               PART = XP - REAL(IM)
            END IF
            IF(PART.GT.0. .AND. FLAT(IX,IY).NE.VAL__BADR
     &           .AND. DATA(IX,IY).NE.VAL__BADR .AND.
     &           DARK(IX,IY).NE.VAL__BADR) THEN
               NPIX = NPIX + 1
               PARTIAL(NPIX)  = 1./SQRT(PART)
               XDATA(NPIX)    = REAL(IX)/REAL(NXS)
               BALANCE(NPIX)  = FLAT(IX,IY)
               BALFAC         = BALANCE(NPIX)
               SKYDATA(NPIX)  = BALFAC*(DATA(IX,IY)-DARK(IX,IY))
               SKYSIGMA(NPIX) = SQRT(BALFAC*BALFAC*
     &              (VAR0+MAX(DATA(IX,IY),CMIN)/PHOTON))
            END IF
         END DO
         NPIXTOT = NPIXTOT + NPIX
         NFIT = NPIX
         IF(NFIT.GE.NPOLY) THEN
C
C     pixel reject cycle and refining the variances
C
            ICYCLE = 0
            IREJ   = 1
            DO WHILE(NFIT.GE.NPOLY .AND. (ICYCLE.LE.1 .OR. 
     &           IREJ.GT.0))
               ICYCLE = ICYCLE + 1
C     
C     fit polynomial by least squares
C     First scale variances for degree of
C     covering of each pixel
C     
               DO K = 1, NPIX
                  IF(SKYSIGMA(K).GT.0.) THEN
                     SKYSIGMA(K) = PARTIAL(K)*SKYSIGMA(K)
                  END IF
               END DO
               CALL POLYFIT1( NPIX, XDATA, SKYDATA, SKYSIGMA, 
     #              SKYFIT, NPOLY, IFAIL )
               NFITS = NFITS + 1
C     
C     revise sigma estimates using the polynomial sky model
C     First estimate total counts and therefore a floor to
C     the variance. This prevents overweighting taking into
C     account the uncertainty in the polynomial fit.
C     Only do this if a pixel has been rejected from previous
C     cycle.
C     
               IF(IREJ.GT.0) THEN
                  TOTAL = 0.
                  PIX = 0.
                  DO K = 1, NPIX
                     IF(SKYSIGMA(K).GT.0.) THEN
                        TOTAL = TOTAL + SKYDATA(K)
                        PIX = PIX + 1.
                     END IF
                  END DO
                  VMIN = ALPHA*BETA/(BETA-1)*SQRT(REAL(NPOLY)/PIX)
     &                 *SQRT(VAR0+MAX(0.,TOTAL)/(PIX*PHOTON)) - 
     &                 VAR0*PHOTON
C     
                  RMS = 0.
                  SKYMEAN = 0.
                  NFIT = 0
                  DO K = 1, NPIX
                     IF( SKYSIGMA(K).GT.0. ) THEN
                        NFIT = NFIT + 1
                        VARI = BALANCE(K) * ( BALANCE(K) * VAR0 +
     #                       MAX(VMIN, ABS(SKYFIT(K))) / PHOTON )
                        SKYSIGMA(K) = SQRT( VARI )
                        SKYMEAN = SKYMEAN + SKYFIT(K)
                        ETA = (SKYDATA(K) - SKYFIT(K)) /SKYSIGMA(K)
                        RMS = RMS + ETA*ETA
                     END IF
                  END DO
                  NDF = NFIT - NPOLY
                  IF( NDF.GE.0 ) THEN
                     SKYMEAN = SKYMEAN / REAL(NFIT)
                     IF(NDF.LE.1) THEN
                        RMS = 1.
                        NDF = 1
                     ELSE
                        RMS = SQRT(RMS/REAL(NDF))
                     END IF
C     
C     Apply KDH scaling
C     
                     PROB = EXP(-MIN(80.,REAL(NDF)*(RMS-1)**2/4.))
                     RMS  = PROB + (1.-PROB)*RMS
C     
C     apply the pixel rejection criterion (Poisson)
C     Only one pixel at a time is rejected.
C     
                     RESCALE = MAX(0.01, RMS)
                     RMAX = -1.
                     IREJ = 0
                     DO K=1,NPIX
                        SIG = RESCALE*SKYSIGMA(K)
                        IF(SIG.GT.0) THEN
                           RATIO = CHANCE( SKYDATA(K), SKYFIT(K), 
     &                          SIG, THRLO, THRHI )
C
C     CHANCE returns the - LOG(Probability of getting as bad as the observed
C     value divided by the probabilty of getting as bad as the threshold).
C
                           IF( RATIO.GT.-0.5) THEN
                              IF(RATIO.GT.RMAX) THEN
                                 RMAX = RATIO
                                 IREJ = K
                              END IF
                           END IF
                        END IF
                     END DO
                     IF(IREJ.GT.0) THEN
                        SKYSIGMA(IREJ) = -ABS(SKYSIGMA(IREJ))
                        NREJTOT = NREJTOT + 1
                     END IF
                  ELSE
                     CALL MSG_SETI('IY',IY+YLO-1)
                     CALL MSG_OUT(' ',
     &          'Too few pixels left to fit at wavelength ^IY',
     &                    STATUS)
                  END IF
               END IF
            END DO
         ELSE
            CALL MSG_SETI('IY',IY+YLO-1)
            CALL MSG_OUT(' ',
     &           'Too few pixels to fit at wavelength ^IY',
     &           STATUS)
         END IF
C
C Evaluate and stow the sky polynomial over the object region
C Left and right are correct in middle of frame. Shift applied
C here to compute sky fit in correct spatial position
C
         ILEFT  = NINT(LEFT - XSHIFT)  - SLO + 1
         IRIGHT = NINT(RIGHT - XSHIFT) - SLO + 1
         IF(TRACE) THEN
            ILEFT  = ILEFT  - 2
            IRIGHT = IRIGHT + 2
         END IF
         ILEFT   = MAX(1, MIN(ILEFT, NS))
         IRIGHT  = MAX(1, MIN(IRIGHT, NS))
         IF(NFIT.GE.NPOLY) THEN
            SKYMEAN = 0.
            DO IX = ILEFT, IRIGHT
               CALL POLYCALC( 1, REAL(IX+SLO-XLO)/REAL(NXS), 
     &              SKY(IX,IY), IFAIL )
               SKYMEAN = SKYMEAN + SKY(IX,IY)
            END DO
            SKYMEAN = SKYMEAN /MAX(1,IRIGHT-ILEFT+1)
C
C report pixel rejects
C
            IF( MOD(IY,100) .EQ. 0 ) THEN
               WRITE(DISASTER,
     &              '(A,I4,A,1PG10.3,A,1PG9.3,A,I5,A,0PF6.2,A)')
     &    ' Row ',IY+YLO-1,', sky = ',SKYMEAN,
     &              ', RMS = ',RMS,',',NREJTOT,
     &    ' rejects = ',100.*REAL(NREJTOT)/REAL(NPIXTOT),'%.'
               CALL MSG_OUT(' ',DISASTER, STATUS)
            END IF
            RMSAVG = RMSAVG + RMS
            NSOME  = NSOME + 1
         ELSE
            DO IX = ILEFT, IRIGHT
               SKY(IX,IY) = VAL__BADR
            END DO
         END IF
      END DO
      RMSAVG = RMSAVG/REAL(NSOME)
      WRITE(DISASTER,'(A,F6.2)') 'Mean fits/row = ',
     &                           REAL(NFITS)/REAL(NYS)
      CALL MSG_OUT(' ',DISASTER, STATUS)
      WRITE(DISASTER,'(A,1PG9.3,A,I5,A,0PF6.2,A)')
     &'Average RMS = ',RMSAVG,',',NREJTOT,
     &' rejects = ',100.*REAL(NREJTOT)/REAL(NPIXTOT),'%.'
      CALL MSG_OUT(' ',DISASTER, STATUS)
      RETURN
      END

