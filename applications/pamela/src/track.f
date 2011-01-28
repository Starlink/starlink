*TRACK
*
* TRACK   -- measures and fits the X position of the spectrum as a 
* function of Y. The result is needed when you wish to cope with 
* significant tilt on the spectrum. It is assumed that the spectrum
* runs more-or-less vertically.
*
* TRACK works by first locating the spectrum in a block of rows. It then steps
* up and down from this point looking for the spectrum in a window centred on
* the previous position. The positions are measured by cross-correlation with 
* a gaussian. The positions are fitted with B-splines and optionally the
* whole process is repeated for stability with the windows centred on 
* positions defined by the previous fit. Plots of the fit can be overlaid a 
* greyscale image or over the measured positions.
*
* If an old track file is used, then the spline is kept fixed, and the residuals relative
* to the spline are fitted with a polynomial whose order is stored in the PAMELA.TRACK
* extension as NPOLY. This is set to zero for a new spline-only fit. If non zero then the
* data values are the polynomial coefficients. If NPOLY=0, the data value (only 1 of them) is
* meaningless. The polynomial is scaled to run from -1 to 1 between the end spline knots which 
* mark the edge of the data
*
* TRACK checks for bad pixels and eliminates points too badly affected by them. This allows
* the user to specifically remove points if he/she thinks TRACK is failing because of them.
*

* Parameters:
*
*  IMAGE  -- Data frame to track
*
*  FLAT   -- Balance frame
*
*  OLD    -- TRUE to update an old fit. In this case the spline from the old fit
*            will be read in and preserved and you will be prompted for the number 
*            coefficients of a polynomial correction to this.
*
*  TRACK  -- Name of file in which fit will be stored. Should already exist
*            if OLD=TRUE.
*
* If OLD
*
*    NPOLY  -- Order of polynomial fit. This will only be prompted for if OLD=TRUE.
*              It is the number of poly coefficients after the existing spline fit 
*              has first been subtracted. 
* else
*
*    NSPLINE - number of splines to use if OLD=FALSE. 
*
*    ORDER   - order of the spline (4=cubic)
*

*
* NFPOLY  -- Order of plot polynomial. A poly of order NFPOLY will be
*            removed before plotting in order to show the residuals more
*            clearly.
*
* XSTART, XEND -- valid X region. Useful for isolating an order.
*
* YSTART, YEND -- Valid Y region, sometimes should crop down if track is
*                 having trouble finding the spectrum if it fades near ends.
*
* If OLD
*
*   TWEAK  -- TRUE to tweak spectrum as opposed to completely relocating it.
*           This means that the old spectrum will be used to find the new one
*           as opposed to finding the entire spectrum from scratch. The current 
*           spectrum should be close to the old one (within a FWHM or two).
*
* PICK   -- TRUE to pick spectrum automatically. Normally this works, but
*           every so often you have to do it by hand.
*
* If PICK
*
*    NOBJ  -- Number of objects. The routine will identify the NOBJ highest
*             peaks. If your target spectrum is the third highest then you
*             should either define XSTART and XEND to isolate it or put
*             NOBJ = 3. Beware variability which may demote your target even
*             lower. If so increase NOBJ.
*
*
*    IOBJ -- Which object counting from the left of the frame.
*
*    IBLOCK -- Number of rows to average for start position.
*
*    YPOS   -- Initial Y position to start at.
*
* If TWEAK
*
*    OFFSET  --  Offset to add to old fit to locate new spectrum. Most
*               useful if there are two stars on the slit.
*
*    FCHANGE --  If a position shifts by more than FCHANGE on the first position
*                check that point is rejected. This is similar to CHANGE but allows
*                more slop for TWEAK cases where one cannot be too accurate to start
*                with. CHANGE (see below) applies on later cycles.
*
* PLOT   -- TRUE if you want plots.
*
* HARDCOPY   -- TRUE if you want the plots to be hardcopy
*
* AUTO   -- TRUE for automatic scaling of greyscale plot
*
* If .NOT.AUTO
*
*    LOW, HIGH -- Limits for greyscale plot
*
* WIDTH -- The width of the window which should cover the object and some more.
*          (Preferably should be several times both ESIG and FWHM).
*
* ESIG  -- FWHM of gaussian used to measure position by cross-correlation.
*          Should be similar to FWHM of spectrum.
*
* FWHM  -- FWHM of profile. This need only be an estimate and is used to 
*          attempt to correct for undersampling. This can be seen if the
*          positions tend to stick to the middle of pixels. Beware of making
*          it too large because it can actually add discontinuities to the 
*          measured positions. Set = 0 to ignore.
*
* READOUT -- RMS readout noise, ADU
*
* PHOTON  -- electrons/ADU
*
* CLIP  -- Sigma rejection threshold during poly fit.
*
* NBLOCK -- The spectrum can be taken in blocks of NBLOCK rows at a time to
*           improve signal-to-noise.
*
* If NBLOCK.GT.2
*
*    PSIG   -- The blocks can be improved by kicking out discrepant rows.
*              PSIG is a threshold for doing just this. 
*
* TCYCLE  -- The number of additional cycles following the first fit to
*            clean it up and make sure that it is stable.
*
* CHANGE  -- If a position shifts by more than CHANGE during a tweak cycle
*            that point is rejected. Typically the positions should shift
*            by much less than a pixel.
*
*
*
*TRACK
      SUBROUTINE TRACK(STATUS)
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CNF_PAR'
      INTEGER STATUS, NDIM
      INTEGER LBND(2), UBND(2)
      INTEGER TLBND(1), TUBND(1)
      INTEGER MAXPOLY
      PARAMETER (MAXPOLY=20)
      DOUBLE PRECISION OLDPOLY(20)
      INTEGER MAXSPLINE, MAXORD
      PARAMETER (MAXORD=20)
      PARAMETER (MAXSPLINE=200)
C      
C NDF identifiers and pointers
C
      INTEGER IMAGE, FLAT, TRCK, TEMP, IPTR
      INTEGER FPTR, TPTR, SMALL, XPTR, CPTR
C
C Parameter values
C
      INTEGER NPOLY, NPOLD, NFPOLY, NOBJ, IOBJ, IBLOCK
      INTEGER NSET, NBLOCK, TCYCLE, NSPLINE, NPCHECK, NORD
      REAL YPOS, OFFSET, WIDTH, READOUT, PHOTON, ESIG
      REAL CLIP, PSIG, CHANGE, FCHANGE, FWHM, LIMITS(2)
      LOGICAL TWEAK, PICK, OLD, PLOT, AUTO, THERE
      CHARACTER*128 DEVICE, FILE
C
C Other variables
C
      INTEGER XLO, XHI, YLO, YHI, EL
      INTEGER FLEN, NXS, NYS
      CHARACTER*(DAT__SZLOC) LOC, LOC1
C
C Flags
C
      LOGICAL BASE
C
C     Start
C
      IF(STATUS.NE.SAI__OK) RETURN
      CALL NDF_BEGIN
C
C     Open data file, identifier IMAGE.
C
      CALL NDF_ASSOC('IMAGE','READ',IMAGE,STATUS)
C
C     Open balance frame, identifier FLAT
C     
      CALL NDF_ASSOC('FLAT','READ',FLAT,STATUS)
C     
C     Force to same size
C     
      CALL NDF_MBND('TRIM', IMAGE, FLAT, STATUS)
C     
C     Get distortion output file. First check to see if it is an old
C     file or not. If it is, we will fit a low order poly, after subtractoin
C     of the spline.
C     
      CALL PAR_GET0L('OLD', OLD, STATUS)
      IF(OLD) THEN
         CALL NDF_ASSOC('TRACK','UPDATE',TRCK,STATUS)
C
C     Recover and store any old poly fit.
C
         CALL NDF_XGT0I(TRCK, 'PAMELA', 'TRACK.NPOLY', NPOLD, STATUS)
         IF(NPOLD.GT.0) THEN
            CALL NDF_DIM(TRCK, 1, NPCHECK, NDIM, STATUS)
            IF(NPCHECK.NE.NPOLD) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP('TRACK',
     &              'Conflict between PAMELA.TRACK.NPOLY' //
     &              ' and actual number of coefficients', STATUS)
            END IF
            IF(NPOLD.GT.MAXPOLY) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP('TRACK',
     &              'Too many poly coefficients in old file', STATUS)
            END IF
            CALL NDF_MAP(TRCK,'Data','_DOUBLE','READ',TPTR,EL,STATUS)
            CALL TDATD(%VAL(CNF_PVAL(TPTR)), OLDPOLY, NPOLD)
            CALL NDF_UNMAP(TRCK,'Data',STATUS)
         END IF
C
C     Prompt for new number of poly terms that need not be related
C     in any way to the old number. Then modify the data array
C     appropriately. Finally recover spline info.
C
         CALL PAR_GDR0I('NPOLY',2,1,MAXPOLY,.FALSE.,NPOLY,STATUS)
         TLBND(1) = 1
         TUBND(1) = NPOLY
         CALL NDF_SBND(1,TLBND,TUBND,TRCK,STATUS)
         CALL NDF_MAP(TRCK,'Data','_DOUBLE','WRITE',TPTR,EL,STATUS)
         CALL NDF_XPT0I(NPOLY, TRCK,'PAMELA','TRACK.NPOLY',STATUS)

         CALL NDF_XGT0I(TRCK, 'PAMELA', 'TRACK.NSPLINE',NSPLINE,STATUS)
         CALL NDF_XGT0I(TRCK, 'PAMELA', 'TRACK.NORD', NORD, STATUS)
         CALL NDF_XLOC(TRCK,'PAMELA','READ',LOC,STATUS)
         CALL DAT_FIND(LOC, 'TRACK', LOC1, STATUS)
         CALL CMP_MAPV(LOC1,'XKNOT','_DOUBLE','READ',XPTR,EL,STATUS)
         IF(NSPLINE+2*NORD-1.NE.EL) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP('TRACK',
     &           'Incorrect number of knots given NSPLINE & NORD', 
     &           STATUS)
         END IF
         CALL CMP_MAPV(LOC1,'CSPLINE','_DOUBLE','READ',CPTR,EL,STATUS )
         IF(NSPLINE+NORD-1.NE.EL) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP('TRACK',
     &           'Incorrect number of coeffs given NSPLINE & NORD', 
     &           STATUS)
         END IF
         CALL DAT_ANNUL(LOC1, STATUS)
         CALL DAT_ANNUL(LOC, STATUS)      
      ELSE  
         CALL NDF_SECT(IMAGE,1,1,1,SMALL,STATUS)         
         CALL NDF_PROP(SMALL,' ','TRACK',TRCK,STATUS)
         CALL NDF_RESET(TRCK,'Title,Data',STATUS)
         CALL NDF_CPUT('TRACK output',TRCK,'Title',STATUS)
C     
C     data array is used to store the polynomial. This only means something
C     in the 'old' case, so here we just create a 1 element array set to zero
C
         CALL NDF_STYPE('_DOUBLE',TRCK,'Data',STATUS)
         TLBND(1) = 1
         TUBND(1) = 1
         CALL NDF_SBND(1,TLBND,TUBND,TRCK,STATUS)
         CALL NDF_MAP(TRCK,'Data','_DOUBLE','WRITE/ZERO',TPTR,EL,STATUS)

         CALL PAR_GDR0I('NSPLINE',3,1,MAXSPLINE,.FALSE.,NSPLINE,STATUS)
         CALL PAR_GDR0I('ORDER',4,1,MAXORD,.FALSE.,NORD,STATUS)
C
C     Create PAMELA extension with TRACK subcomponent
C
         CALL NDF_XSTAT(TRCK, 'PAMELA', THERE, STATUS)
         IF(.NOT.THERE) THEN
            CALL NDF_XNEW(TRCK,'PAMELA', 'Ext', 0, 0, LOC, STATUS)
            CALL DAT_NEW(LOC, 'TRACK', 'Struct', 0, 0, STATUS)
         ELSE
            CALL NDF_XLOC(TRCK,'PAMELA','UPDATE',LOC,STATUS)
            CALL DAT_THERE(LOC, 'TRACK', THERE, STATUS)
            IF(.NOT.THERE) 
     &           CALL DAT_NEW(LOC, 'TRACK', 'Struct', 0, 0, STATUS)
         END IF
C
C     Create and map arrays for  knot positions and spline coefficients
C
         CALL DAT_FIND(LOC, 'TRACK', LOC1, STATUS)
         CALL DAT_NEW1D(LOC1, 'XKNOT', NSPLINE+2*NORD-1, STATUS) 
         CALL CMP_MAPV(LOC1,'XKNOT','_DOUBLE','WRITE',XPTR,EL,STATUS )
         CALL DAT_NEW1D(LOC1, 'CSPLINE', NSPLINE+3, STATUS)
         CALL CMP_MAPV(LOC1,'CSPLINE','_DOUBLE','WRITE',CPTR,EL,STATUS )
         CALL DAT_ANNUL(LOC1, STATUS)
         CALL DAT_ANNUL(LOC, STATUS)

         NPOLY = 0
         NPOLD = 0
         CALL NDF_XPT0I(NSPLINE, TRCK,'PAMELA','TRACK.NSPLINE',STATUS)
         CALL NDF_XPT0I(NORD, TRCK,'PAMELA','TRACK.NORD',STATUS)      
         CALL NDF_XPT0I(NPOLY, TRCK,'PAMELA','TRACK.NPOLY',STATUS)      

      END IF
C
      CALL PAR_GDR0I('NFPOLY',1,0,10,.FALSE.,NFPOLY,STATUS)
C
C     What region of frame
C
      CALL NDF_BOUND(IMAGE, 2, LBND, UBND, NDIM, STATUS)
      CALL NDF_ISBAS(IMAGE, BASE, STATUS)
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
C     Replace IMAGE and FLAT with section ndfs
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
         END IF
      END IF
      NXS = UBND(1)-LBND(1)+1
      NYS = UBND(2)-LBND(2)+1
      IF(OLD) THEN
         CALL PAR_GET0L('TWEAK', TWEAK, STATUS)
      ELSE
         TWEAK = .FALSE.
      END IF
      IF(.NOT.TWEAK) THEN
         CALL PAR_GET0L('PICK', PICK, STATUS)
         IF(PICK) THEN
            CALL PAR_GDR0I('NOBJ',1,1,100,.FALSE.,NOBJ,STATUS)
            IF(NOBJ.EQ.1) THEN
               IOBJ = 1
            ELSE
               CALL PAR_GDR0I('IOBJ',1,1,NOBJ,.FALSE.,IOBJ,STATUS)
            END IF
            CALL PAR_GDR0I('IBLOCK',1,1,NYS,.FALSE.,IBLOCK,STATUS)
            YPOS  = REAL(LBND(2)+UBND(2))/2.
            CALL PAR_DEF0R('YPOS', YPOS, STATUS)
            CALL PAR_GDR0R('YPOS',YPOS,REAL(LBND(2)),REAL(UBND(2)),
     &           .FALSE.,YPOS,STATUS)
         END IF
      ELSE
         CALL PAR_GDR0R('OFFSET',0.,-REAL(NXS),REAL(NXS),
     &        .FALSE.,OFFSET,STATUS)
         CALL PAR_GDR0R('FCHANGE',1.,0.,1.E6,.FALSE.,FCHANGE,STATUS)
      END IF

      CALL PAR_GET0L('PLOT', PLOT, STATUS)
      IF(PLOT) THEN      
         CALL PAR_GET0C('DEVICE',DEVICE,STATUS) 
         CALL PAR_GET0L('AUTO', AUTO, STATUS)
         IF(.NOT.AUTO) CALL PAR_GET1R('LIMITS',2,LIMITS,NSET,STATUS)
      END IF
C
C     Get special variables 
C
      CALL PAR_GDR0R('WIDTH',15.,3.,REAL(NXS),.FALSE.,WIDTH,STATUS)
      CALL PAR_GDR0R('ESIG',3.,2.,REAL(NXS),.FALSE.,ESIG,STATUS)
      CALL PAR_GDR0R('FWHM',3.,0.,REAL(NXS),.FALSE.,FWHM,STATUS)
      CALL PAR_GDR0R('READOUT',0.,0.,1.E10,.FALSE.,READOUT,STATUS)
      CALL PAR_GDR0R('PHOTON',1.,1.E-10,1.E10,.FALSE.,PHOTON,STATUS)
      CALL PAR_GDR0R('CLIP',4.,0.,1.E10,.FALSE.,CLIP,STATUS)
      CALL PAR_GDR0I('NBLOCK',1,1,NYS,.FALSE.,NBLOCK,STATUS)
      IF(NBLOCK.GT.2) THEN
         CALL PAR_GDR0R('PSIG',4.,0.,1.E30,.FALSE.,PSIG,STATUS)
      END IF
      CALL PAR_GDR0I('TCYCLE',2,0,100,.FALSE.,TCYCLE,STATUS)
      IF(TCYCLE.GT.0. .OR. TWEAK) THEN
         CALL PAR_GDR0R('CHANGE',1.,0.,1.E6,.FALSE.,CHANGE,STATUS)
      END IF
C     
C     Map arrays
C
      CALL NDF_MAP(IMAGE,'Data','_REAL','READ',IPTR,EL,STATUS)
      CALL NDF_MAP(FLAT,'Data','_REAL','READ',FPTR,EL,STATUS)
C     
C     Generate plot title
C
      CALL NDF_MSG('NAME', IMAGE)
      CALL MSG_LOAD('BLA','^NAME',FILE,FLEN,STATUS)
C
C     TRACK spectrum
C
      CALL TRACK_SPEC(%VAL(CNF_PVAL(IPTR)), %VAL(CNF_PVAL(FPTR)), 
     &     %VAL(CNF_PVAL(XPTR)), %VAL(CNF_PVAL(CPTR)), NSPLINE, 
     &     NORD, %VAL(CNF_PVAL(TPTR)), NPOLY, OLDPOLY, NPOLD, NXS, NYS, 
     &     LBND(1), LBND(2), NBLOCK, WIDTH, ESIG, CLIP, PSIG, AUTO, 
     &     PICK, NOBJ, IOBJ, IBLOCK, YPOS, TCYCLE, PLOT, FILE(:FLEN),
     &     LIMITS(1), LIMITS(2), READOUT, PHOTON, NFPOLY, TWEAK, CHANGE,
     &     FCHANGE, FWHM, OFFSET, DEVICE, STATUS)
C
C     Tidy up
C
      CALL NDF_END(STATUS)
      RETURN
      END	

C Subroutine to do the real work

      SUBROUTINE TRACK_SPEC(DATA, BALANCE, XKNOT, CSPLINE, NSPLINE,
     &     NORD, CPOLY, NPOLY, OLDPOLY, NPOLD, NXS, NYS, XLO, YLO, 
     &     NBLOCK, WIDTH, ESIG, CLIP, PSIG, AUTO, PICK, NOBJ, IOBJ, 
     &     IBLOCK, YPOS, TCYCLE, PLOT, TITLE, LOW, HIGH, READOUT, 
     &     PHOTON, NFPOLY, TWEAK, CHANGE, FCHANGE, FWHM, OFFSET, 
     &     DEVICE, STATUS)
*     
* TRACK_SPEC measures the spatial position of a spectrum and fits a 
* polynomial to it. It works by locating a point on the spectrum either 
* interactively or autonatically and then moving upwards and downwards 
* from that point measuring the X position of the spectrum at each value
* of Y. The position is measured by cross-correlation with the derivative 
* of a gaussian and finding the point where this goes through zero. This 
* is accomplished with the routine MEDIAN. The positions are then fitted 
* with a polynomial, which is then returned to the calling program.
*
* A correction is made for undersampling effects as follows. After
* first poly fit, the fitted position is used to generate a fake
* profile from a gaussian of appropriate FWHM. The position of this
* is measured and the difference between true and measured position
* is applied as a correction to the observed data for that point.
*
* Arguments: (> = input, < = output)
*
* >   REAL DATA(NXS,NYS)       -- The image 
* >   REAL BALANCE(NXS,NYS)    -- Multiplicative balance frame
* ><  REAL*8 XKNOT(NSPLINE+2*NORD-1)  -- Positions of knots. Input if NPOLY > 0, output if NPOLY=0
* ><  REAL*8 CSPLINE(NSPLINE+3)-- Spline coefficients
* >   INTEGER NSPLINE          -- Number of splines
* >   INTEGER NORD             -- Order of spline (4=cubic)
* <   REAL*8 CPOLY(NPOLY)      -- Poly coefficients if NPOLY>0
* >   INTEGER NPOLY            -- Order of polynomial fit
* >   REAL*8 OLDPOLY(NPOLD)    -- Old poly coefficients if NPOLD >0
* >   INTEGER NPOLD            -- Old order of polynomial fit
* >   INTEGER NXS, NYS         -- Section dimensions
* >   INTEGER XLO,YLO          -- Lower corner of frame
* >   INTEGER NBLOCK           -- Y rows can be treated in blocks of NBLOCK
*                                 to improve S/N on each postion measurement.
* >   REAL WIDTH               -- The width of the profile to compute each
*                                 point. Should be enough to cover the width
*                                 of the spectrum.
* >   REAL ESIG                -- FWHM of gaussian for X-corr. Should be about
*                                 the same as FWHM of spectrum along slit
* >   REAL CLIP                -- Rejection threshold during poly fit to
*                                 measured positions.
* >   REAL PSIG                -- Sigma threshold for cleaning blocks
* >   LOGICAL AUTO             -- .TRUE. for autoscaled plots.
* >   LOGICAL PICK             -- .TRUE. for automatic location of spectra,
*                                 .FALSE. for interactive.
* >   INTEGER NOBJ             -- Number of objects, only needed if PICK
*                                 the program selects the NOBJ highest peaks so
*                                 NOBJ can be one if you want the strongest.
* >   INTEGER IOBJ             -- Which object of the NOBJ to use only if PICK.
*                                 IOBJ is measured from the left.
* >   INTEGER IBLOCK           -- If PICK, IBLOCK is the number of rows to be
*                                 added to get the initial position estimate.
* >   REAL YPOS                -- If PICK, YPOS is the initial Y position to
*                                 get profile from.
* >   INTEGER TCYCLE           -- Number of tweak cycles. A tweak cycles uses
*                                 a fit from the previous cycle as the initial
*                                 spectrum position estimate.
* >   LOGICAL PLOT             -- .TRUE. for plots.
* >   CHARACTER*(*) TITLE      -- Title of plots.
* >   REAL LOW                 -- Lowest value for frame plot
* >   REAL HIGH                -- Highest value for frame plot
* >   REAL READOUT             -- Readout noise, RMS data numbers
* >   REAL PHOTON              -- Photons/data number
* >   INTEGER NFPOLY           -- Order of fit to subtract for display of fit.
* >   LOGICAL TWEAK            -- .TRUE. if you want to tweak an old fit, which
*                                 must be contained in DISTORT. In this case,
*                                 no point has to be selected as it is assumed
*                                 that the spectrum is close to the current fit
*                                 plus an OFFSET.
* >   REAL CHANGE              -- The maximum shift allowed for a point during a
*                                 tweak cycle. The point is ignored for larger
*                                 shifts.
* >   REAL FCHANGE            --  The maximum shift allowed for a point during a
*                                 the first tweak cycle if TWEAK.
*
* >   REAL FWHM                -- FWHM of spectrum profile to be used to make
*                                 undersampling corrections.
* >   REAL OFFSET              -- If TWEAK is true, OFFSET is added to the old
*                                 position to find the new one.
* >   CHARACTER*(*) DEVICE     -- Plot device
* >   INTEGER STATUS           -- Error status

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INTEGER XLO, XHI, YLO, YHI, NBLOCK, IBLOCK
      INTEGER NPOLY, IFAIL, IX, IY, LENGTH, STATUS, IOBJ
      INTEGER NOBJ, TCYCLE, PGOPEN, NUMBLOCK, I1, I2, J1
      INTEGER J2, I, J, ISTART, IB, IGUESS, ICYCLE, NREJ
      INTEGER NFIT, IREJ, NREJTOT, NFOUND, NXS, NYS
      INTEGER NSPLINE, NPOLD, NORD, NFPOLY
      REAL DATA(NXS,NYS), BALANCE(NXS,NYS), TR(6), HIGH, LOW
      REAL PSIG, X1, X2, Y1, Y2, XV1, XV2, YV1, YV2, X, Y, YPOS
      REAL VAR0, EFAC, XSTART, XSAVE, XPOS, SIGBIN, FAC, SFAC
      REAL CHIMAX, RMS, PRMS, DEV, CHI, DEVHI, SUB, WIDTH, ESIG
      REAL CLIP, READOUT, PHOTON, CHANGE, FWHM, OFFSET, DEVLO
      REAL RANGE, FCHANGE
      DOUBLE PRECISION CPOLY(*), OLDPOLY(*), XKNOT(NSPLINE+2*NORD-1)
      DOUBLE PRECISION CSPLINE(*), XMID, XHRANGE
      DOUBLE PRECISION PDA_DERF, DX1, DX2, POLY, SUM1, SUM2
      CHARACTER*40 BACK*5, REPLY*5, YLABEL
      CHARACTER*(*) TITLE, OPTION*2, DEVICE
      LOGICAL CURSOR, AUTO, PICK, OK, PLOT
      LOGICAL FPLOT, TWEAK
C     
C     Internal work arrays
C
      INTEGER MAXPROF, NPLOT, MAXDAT, MAXPOLY, MAXOBJ
      INTEGER NOK, MAXBUFF
      PARAMETER (NPLOT=500)
      PARAMETER (MAXDAT=20000)
      REAL PLOTX(MAXDAT), PLOTY(MAXDAT), RBUF(MAXDAT)
      REAL SBUF(MAXDAT), UNCERT(MAXDAT), MEDVAL
      DOUBLE PRECISION WORK(3,MAXDAT), YFIT(MAXDAT)
      DOUBLE PRECISION YSTORE(MAXDAT)
      LOGICAL ACC(MAXDAT)

      PARAMETER (MAXBUFF=100000)
      DOUBLE PRECISION DBUFFER(MAXBUFF)

      PARAMETER (MAXPROF=2000)
      REAL  PROFILE(MAXPROF), VAR(MAXPROF)

      PARAMETER (MAXPOLY=50)
      DOUBLE PRECISION XM(MAXPOLY*(2*MAXPOLY+3)), CHISQ
      DOUBLE PRECISION SUBFIT(MAXPOLY)
C
C     Object detection arrays
C
      PARAMETER (MAXOBJ=100)
      INTEGER IPEAK(MAXOBJ)
      REAL PEAK(MAXOBJ)
      INTEGER NHIST 
      PARAMETER (NHIST=100)
      REAL HIST(NHIST), DATMIN, DATMAX
C
      IF(STATUS .NE. SAI__OK) RETURN

      IF(NPOLY.GT.MAXPOLY .OR. NFPOLY.GT.MAXPOLY) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP('TRACK_SPEC',
     &        'MAXPOLY must be increased; NPOLY OR NFPOLY too large', 
     &        STATUS)
         RETURN
      END IF

      IF(NPOLY.GT.0) THEN
         XMID    = (XKNOT(1)+XKNOT(NSPLINE+2*NORD-1))/2.
         XHRANGE = (XKNOT(NSPLINE+2*NORD-1)-XKNOT(1))/2.
      END IF
C     
C     Select device for greyscale plot (if not picking
C     point automatically).
C     
      XHI = XLO + NXS - 1
      YHI = YLO + NYS - 1
      IF(NXS.GT.MAXPROF .OR. NYS.GT.MAXDAT) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP('TRACK_SPEC',
     &        'Frame too large for TRACK_SPEC', STATUS)
         RETURN
      END IF
      FPLOT = .FALSE.
      IF(.NOT.PICK .AND. .NOT.TWEAK) THEN
         FPLOT = .TRUE.
      ELSE IF(PLOT) THEN
         WRITE(*,'(A,$)') 'Do you want frame plot ? [Y] '
         READ(*,'(A)') REPLY
         CALL UPPER_CASE(REPLY)
         FPLOT = .TRUE.
         IF(REPLY.EQ.'N') FPLOT = .FALSE.
      END IF
      IF(AUTO) THEN
         DATMIN = DATA(1, 1)
         DATMAX = LOW
         DO IY = 1, NYS
            DO IX = 1, NXS
               DATMIN = MIN(DATMIN, DATA(IX,IY))
               DATMAX = MAX(DATMAX, DATA(IX,IY))
            END DO
         END DO
         CALL CDF_LOC(DATA, NXS, NYS, HIST, NHIST, 0.1, 
     &        2, DATMIN, DATMAX, LOW)
         CALL CDF_LOC(DATA, NXS, NYS, HIST, NHIST, 0.98, 
     &        2, DATMIN, DATMAX, HIGH)      
      END IF

      IF(FPLOT) THEN
         IFAIL = PGOPEN(DEVICE)
         IF(IFAIL.LE.0) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP('TRACK_SPEC',
     &           'Failed to open plot device', STATUS)
            RETURN
         END IF
         TR(1) = REAL(XLO-1)
         TR(2) = 1.
         TR(3) = 0.
         TR(4) = REAL(YLO-1)
         TR(5) = 0.
         TR(6) = 1.
         X1 = REAL(XLO) - 0.5
         X2 = REAL(XHI) + 0.5
         Y1 = REAL(YLO) - 0.5
         Y2 = REAL(YHI) + 0.5
         XV1 = 0.2
         XV2 = 0.8
         YV1 = 0.2
         YV2 = 0.8
         CALL MSG_SYNC(STATUS)
         CALL PGSVP(XV1,XV2,YV1,YV2)
         CALL PGSWIN(X1,X2,Y1,Y2)
         CALL PGGRAY(DATA,NXS,NYS,1,NXS,1,NYS,HIGH,LOW,TR)
         CALL PGSCI(5)
         CALL PGBOX('BCNST', 0., 0, 'BCNST', 0., 0)
         CALL PGSCI(7)
         CALL PGLAB('Column number, \\fi i',
     &        'Row number, \\fi j',TITLE)              
         CALL PGIDEN
C     
C     Now pick a point on the spectrum
C
         IF(.NOT.TWEAK .AND. .NOT.PICK) THEN
            CALL PGQINF('CURSOR', BACK, LENGTH)
            IF(BACK(1:3).EQ.'YES') THEN
               CURSOR = .TRUE.
            ELSE
               CURSOR = .FALSE.
            END IF
C     
C     Find what mode of input (if any)
C     
            WRITE(*,*) ' '
            WRITE(*,*) 'Pick a point on the spectrum'//
     &           ' to be TRACKed'
            IFAIL = 1
            DO WHILE(IFAIL.NE.0)
               IF(CURSOR) THEN
                  WRITE(*,'(A,$)') 'C(ursor), T(erminal) ? '
                  READ(*,'(A)') OPTION
                  CALL UPPER_CASE(OPTION)
                  IF(OPTION.EQ.'C' .OR. OPTION.EQ.'T') IFAIL = 0
               ELSE
                  OPTION = 'T'
                  IFAIL = 0
               END IF     
            END DO
            OPTION(2:2) = 'B'
            CALL GET_POINT(OPTION, X, REAL(XLO), REAL(XHI), Y, 
     &           REAL(YLO), REAL(YHI))
            X = X -  REAL(XLO-1)
            Y = Y -  REAL(YLO-1)
         END IF
      END IF
      NUMBLOCK = NYS/NBLOCK
      VAR0     = READOUT*READOUT
      EFAC     = 1./2./(ESIG/2.3548)**2
      IF(TWEAK) GOTO 1000
C     
      IF(PICK) THEN
C
C     Select point automatically
C     First median collapse in Y to get a profile in X
C
         J1 = NINT(YPOS) - YLO + 1 - IBLOCK/2
         J1 = MAX(1, MIN(NYS, J1))
         J2 = J1 + IBLOCK - 1
         J2 = MAX(1, MIN(NYS, J2))
         DO IX = 1, NXS
            NOK = 0
            DO IY = J1, J2
               IF(BALANCE(IX,IY).NE.VAL__BADR .AND. 
     &              DATA(IX,IY).NE.VAL__BADR) THEN
                  NOK = NOK + 1
                  RBUF(NOK) = BALANCE(IX,IY)*DATA(IX,IY)
               END IF
            END DO
            IF(NOK.EQ.0) THEN
               PROFILE(IX) = VAL__BADR
            ELSE
               PROFILE(IX) = MEDVAL( RBUF, NOK ) 
            END IF
         END DO
C     
C     Locate top NOBJ peaks, arrange in descending order of height.
C
         DO I = 1, NOBJ
            PEAK(I) = 0.
         END DO
         NFOUND = 0
         DO IX = 2, NXS-1
            IF(PROFILE(IX).NE.VAL__BADR       .AND. 
     &           PROFILE(IX-1).NE.VAL__BADR   .AND. 
     &           PROFILE(IX+1).NE.VAL__BADR   .AND. 
     &           PROFILE(IX-1).LE.PROFILE(IX) .AND. 
     &           PROFILE(IX+1).LE.PROFILE(IX)) THEN
               NFOUND = NFOUND + 1
               I = 1
               OK = .TRUE.
               DO WHILE(I.LE.NOBJ .AND. OK)
                  IF(PROFILE(IX).GT.PEAK(I)) THEN
                     IF(NOBJ-1.GE.I) THEN
                        DO J = NOBJ-1,I,-1
                           PEAK(J+1)  = PEAK(J)
                           IPEAK(J+1) = IPEAK(J)
                        END DO
                     END IF
                     PEAK(I)  = PROFILE(IX)
                     IPEAK(I) = IX
                     OK = .FALSE.
                  END IF
                  I = I + 1
               END DO
            END IF   
         END DO
         IF(NFOUND.LT.NOBJ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP('TRACK_SPEC',
     &           'Not enough objects located',STATUS)
            RETURN
         END IF
C
C     Sort and choose the IOBJ'th peak
C     
         DO I = 1, NOBJ
            PEAK(I) = REAL(IPEAK(I))
         END DO
         CALL SHELLSORT(NOBJ, PEAK, IPEAK)
         X = PEAK(IPEAK(IOBJ))
         Y = REAL(J1+J2)/2.
      END IF

C     
C     Measure position of spectrum.
C     
      ISTART = NINT((Y-0.5)/REAL(NBLOCK)+1.)
C     
C     First move upwards
C     
      XSTART = X
      XSAVE  = X
      DO IB = ISTART, NUMBLOCK
         J1 = 1 + (IB-1)*NBLOCK
         J2 = J1 + NBLOCK - 1
         IF(IB.EQ.NUMBLOCK) J2 = NYS
C     
C     Offset and scale to reduce under/overflow chance
C     
         IF(NPOLY.GT.0) THEN
            WORK(1,IB) = (DBLE(YLO-1)+DBLE(J1+J2)/2.-XMID)/XHRANGE
         ELSE
            WORK(1,IB) = DBLE(YLO-1)+DBLE(J1+J2)/2.
         END IF
C     
C     Compute profile.
C     
         I1 = MAX(1, MIN(NXS, NINT(XSTART-WIDTH/2.)))
         I2 = MAX(1, MIN(NXS, NINT(XSTART+WIDTH/2.)))
C     
C     Collapse with cosmic ray rejection
C
         CALL RCOLLP(PROFILE, VAR, MAXPROF, DATA, BALANCE, 
     &        NXS, NYS, I1, I2, J1, J2, VAR0, PHOTON, PSIG, 
     &        RBUF, SBUF, ACC, MAXDAT, OK)
C     
C     Measure peak position
C
         IF(OK) THEN
            IGUESS = NINT(XSTART) - I1 + 1
            CALL MEDIAN(PROFILE(I1),I2-I1+1,IGUESS,ESIG,XPOS,SIGBIN,1)
            WORK(2,IB) = DBLE(XPOS) + DBLE(XLO+I1-2)
C     
C     Compute error in position
C     
            IF(SIGBIN.GT.0. .AND. XPOS.GT.1. .AND. 
     &           XPOS.LT.REAL(I2-I1+1)) THEN
               SUM1 = 0.
               SUM2 = 0.
               DO I = I1, I2
                  X    = REAL(I-I1+1) - XPOS
                  FAC  = EXP(-EFAC*X*X)   
                  SUM1 = SUM1 + VAR(I)*(X*FAC)**2
                  SUM2 = SUM2 + PROFILE(I)*FAC*(1.-2.*EFAC*X*X)
               END DO
               SIGBIN     = REAL(SQRT(SUM1)/ABS(SUM2))
C
C     First time through set to unit weights
C
               UNCERT(IB) = MAX(0.001, SIGBIN)
               WORK(3,IB) = 1.D0
               IF(ABS(REAL(IGUESS)-XPOS).LT.ESIG/2.) 
     &              XSTART = REAL(WORK(2,IB) - DBLE(XLO-1))
            ELSE
               WORK(3,IB) = -1.D10
            END IF
         ELSE
            WORK(3,IB) = -1.D10
         END IF
      END DO

C     
C     Now downwards
C
      XSTART = XSAVE
      DO IB = ISTART, 1, -1
         J1 = 1 + (IB-1)*NBLOCK
         J2 = J1 + NBLOCK - 1
         IF(IB.EQ.NUMBLOCK) J2 = NYS
         IF(NPOLY.GT.0) THEN
            WORK(1,IB) = (DBLE(YLO-1)+DBLE(J1+J2)/2.-XMID)/XHRANGE
         ELSE
            WORK(1,IB) = DBLE(YLO-1)+DBLE(J1+J2)/2.
         END IF
C     
C     Compute profile.
C
         I1 = MAX(1, MIN(NXS, NINT(XSTART-WIDTH/2.)))
         I2 = MAX(1, MIN(NXS, NINT(XSTART+WIDTH/2.)))
         CALL RCOLLP(PROFILE, VAR, MAXPROF, DATA, BALANCE,
     &        NXS, NYS, I1, I2, J1, J2, VAR0, PHOTON, PSIG, RBUF, 
     &        SBUF, ACC, MAXDAT, OK)
C     
C     Measure peak position
C
         IF(OK) THEN
            IGUESS = NINT(XSTART) - I1 + 1
            CALL MEDIAN(PROFILE(I1),I2-I1+1,IGUESS,ESIG,XPOS,SIGBIN,1)
C     
            WORK(2,IB) = DBLE(XPOS) + DBLE(XLO+I1-2)
C
C     Compute error in position
C
            IF(SIGBIN.GT.0. .AND. XPOS.GT.1. .AND. 
     &           XPOS.LT.REAL(I2-I1+1)) THEN
               SUM1 = 0.
               SUM2 = 0.
               DO I = I1, I2
                  X    = REAL(I-I1+1) - XPOS
                  FAC  = EXP(-EFAC*X*X)   
                  SUM1 = SUM1 + VAR(I)*(X*FAC)**2
                  SUM2 = SUM2 + PROFILE(I)*FAC*(1.-2.*EFAC*X*X)
               END DO
               SIGBIN     = REAL(SQRT(SUM1)/ABS(SUM2))
               UNCERT(IB) = MAX(0.001, SIGBIN)
               WORK(3,IB) = 1.D0
               IF(ABS(REAL(IGUESS)-XPOS).LT.ESIG/2.)
     &              XSTART = REAL(WORK(2,IB) -DBLE(XLO-1))
            ELSE
               WORK(3,IB) = -1.D10
            END IF
         ELSE
            WORK(3,IB) = -1.D10
         END IF
      END DO
C
C     If NPOLY>0, we remove the current spline fit, so that the poly
C     just corrects for deviations from the spline.
C
      IF(NPOLY.GT.0) THEN
         DO IB = 1, NUMBLOCK
            DBUFFER(IB) = XHRANGE*WORK(1,IB)+XMID
         END DO
         CALL SPLCALCB(NUMBLOCK,DBUFFER,XKNOT,CSPLINE,NSPLINE,
     &        NORD,YFIT,IFAIL)
         IF(IFAIL.GT.0) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP('TRACK_SPEC',
     &           'Failed to evaluate old spline fit', STATUS)
            RETURN
         END IF
         DO IB = 1, NUMBLOCK
            WORK(2,IB) = WORK(2,IB)-YFIT(IB)
         END DO
      END IF
C
C     We finally have measured the positions. Now for the fits.
C

 1000 CONTINUE
      IF(TWEAK) THEN
         DO IB = 1, NUMBLOCK
            J1 = 1 + (IB-1)*NBLOCK
            J2 = J1 + NBLOCK - 1
            IF(IB.EQ.NUMBLOCK) J2 = NYS
            WORK(1,IB) = (DBLE(YLO-1)+DBLE(J1+J2)/2.-XMID)/XHRANGE
         END DO
      END IF
C     
C     Now fit.
C
      ICYCLE = 0
      DO WHILE(ICYCLE.LE.TCYCLE)
         ICYCLE = ICYCLE + 1
         IF(TWEAK .OR. ICYCLE.GT.1) THEN
C     
C     Tweak positions using fit for initial estimate
C     This part only carried out if at least one
C     fit has been made and TCYCLE .GT. 0 or a pure TWEAK
C     is wanted.
C     
            DO IB = 1, NUMBLOCK
               J1 = 1 + (IB-1)*NBLOCK
               J2 = J1 + NBLOCK - 1
               IF(IB.EQ.NUMBLOCK) J2 = NYS
C     
C     Compute profile. We need a centre position to extract
C     the profile which is determined in different ways according 
C     to the fit.
C
               IF(ICYCLE.EQ.1) THEN
                  IF(NPOLD.GT.1) THEN
                     XSTART = REAL(YFIT(IB)+
     &                    POLY(OLDPOLY,NPOLD,WORK(1,IB))+
     &                    OFFSET- DBLE(XLO-1))
                  ELSE
                     XSTART = REAL(YFIT(IB)+OFFSET- DBLE(XLO-1))
                  END IF
               ELSE IF(ICYCLE.GT.1 .AND. NPOLY.GT.1) THEN
                  XSTART = REAL(YFIT(IB)+POLY(CPOLY,NPOLY,WORK(1,IB))
     &                 - DBLE(XLO-1))
               ELSE
                  XSTART = REAL(YFIT(IB)- DBLE(XLO-1))
               END IF
               I1 = MAX(1, MIN(NXS, NINT(XSTART-WIDTH/2.)))
               I2 = MAX(1, MIN(NXS, NINT(XSTART+WIDTH/2.)))

               CALL RCOLLP(PROFILE, VAR, MAXPROF, DATA, BALANCE,
     &              NXS, NYS, I1, I2, J1, J2, VAR0, PHOTON, PSIG, 
     &              RBUF, SBUF, ACC, MAXDAT, OK)
C     
C     Measure peak position
C
               IF(OK) THEN
                  IGUESS = NINT(XSTART) - I1 + 1
                  CALL MEDIAN(PROFILE(I1),I2-I1+1,IGUESS,ESIG,
     &                 XPOS,SIGBIN,1)
C     
                  WORK(2,IB) = DBLE(XPOS) + DBLE(XLO+I1-2)
C
C     Compute error in position. Points which have shifted
C     too far are eliminated, unless it is the first cycle
C     of a TWEAK (02/08/2001)
C
                  IF(SIGBIN.LT.0. .OR. 
     &                 ((TWEAK .AND. ICYCLE.EQ.1 .AND.
     &                 ABS(WORK(2,IB)-REAL(XLO-1)-XSTART) .GT. FCHANGE) 
     &                 .OR. ((.NOT.TWEAK .OR. ICYCLE.GT.1) .AND.
     &                 ABS(WORK(2,IB)-REAL(XLO-1)-XSTART) .GT. 
     &                 CHANGE))) THEN
                     WORK(3,IB) = -1.E10
                  ELSE
                     SUM1 = 0.
                     SUM2 = 0.
                     DO I = I1, I2
                        X    = REAL(I-I1+1) - XPOS
                        FAC  = EXP(-EFAC*X*X)   
                        SUM1 = SUM1 + VAR(I)*(X*FAC)**2
                        SUM2 = SUM2 + PROFILE(I)*FAC*(1.-2.*EFAC*X*X)
                     END DO
                     SIGBIN     = REAL(SQRT(SUM1)/ABS(SUM2))
                     WORK(3,IB) = MAX(0.001, SIGBIN)
                     IF(XPOS.LT.1. .OR. XPOS.GT.REAL(I2-I1+1)) THEN
                        WORK(3,IB) = -1.D10
                     END IF
                  END IF
               ELSE
                  WORK(3,IB) = -1.D10
               END IF
            END DO
C
C     Make undersampling corrections
C     TRM 16/5/89. Override option added 02/01/2005
C
            IF(ICYCLE.GT.1 .AND. FWHM.GT.0.) THEN
               SFAC = SQRT(2.)*FWHM/2.3548
               DO IB = 1, NUMBLOCK
C     
C     Compute profile from a pixel integrated gaussian.
C
                  IF(NPOLY.GT.1) THEN
                     XSTART = REAL(YFIT(IB)
     &                    + POLY(CPOLY,NPOLY,WORK(1,IB))
     &                    - DBLE(XLO-1))
                  ELSE
                     XSTART = REAL(YFIT(IB)- DBLE(XLO-1))
                  END IF
                  I1 = MAX(1, MIN(NXS, NINT(XSTART-WIDTH/2.)))
                  I2 = MAX(1, MIN(NXS, NINT(XSTART+WIDTH/2.)))
                  DO I = I1, I2
                     DX1 = (DBLE(I)-0.5-XSTART)/SFAC
                     DX2 = (DBLE(I)+0.5-XSTART)/SFAC
                     PROFILE(I) = REAL(5.*(PDA_DERF(DX2)-PDA_DERF(DX1)))
                  END DO
C     
C     Measure peak position
C
                  IGUESS = NINT(XSTART) - (I1 - 1)
                  CALL MEDIAN(PROFILE(I1),I2-I1+1,IGUESS,ESIG,
     &                 XPOS,SIGBIN,1)
C     
C     Compute undersampling correction and add to measurements
C
                  IF(SIGBIN.GT.0.) WORK(2,IB) = 
     &                 WORK(2,IB)+XSTART-XPOS-REAL(I1-1)
               END DO
            END IF
C     
C     Correct positions for the spline
C     
            IF(NPOLY.GT.0) THEN
               DO IB = 1, NUMBLOCK
                  WORK(2,IB) = WORK(2,IB) - YFIT(IB)
               END DO
            END IF
         END IF

         CHIMAX = 2.*CLIP
         NREJ = -1
         DO WHILE(CHIMAX.GT.CLIP)
            NREJ = NREJ + 1
            IF(NPOLY.GT.0) THEN
               CALL LSQUAR(WORK,NUMBLOCK,NPOLY,CPOLY,CHISQ,XM,0)
            ELSE
               CALL SPLFITB(WORK, NUMBLOCK, NSPLINE, NORD,
     &              DBUFFER, MAXBUFF, XKNOT, YFIT, CSPLINE, IFAIL)
               IF(IFAIL.GT.0) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP('TRACK_SPEC',
     &                 'Failed to fit spline', STATUS)
                  RETURN
               END IF
            END IF
C     
C     Compute RMS, evaluate largest deviate.
C     
            RMS = 0.
            PRMS = 0.
            NFIT = 0
            CHIMAX = -100.
            IREJ = 0
            DO I = 1, NUMBLOCK
               IF(WORK(3,I).GT.0.) THEN
                  IF(NPOLY.GT.0) THEN
                     DEV  = REAL(WORK(2,I)-POLY(CPOLY,NPOLY,WORK(1,I)))
                  ELSE
                     DEV  = REAL(WORK(2,I)-YFIT(I))
                  END IF
                  PRMS = PRMS + DEV*DEV
                  CHI  = REAL(ABS(DEV/WORK(3,I)))
                  IF(CHI.GT.CHIMAX) THEN
                     IREJ   = I
                     CHIMAX = CHI
                  END IF
                  RMS  = RMS + CHI**2
                  NFIT = NFIT + 1
               END IF
            END DO
            IF(NPOLY.GT.0) THEN
               RMS    = SQRT(RMS/REAL(NFIT-NPOLY))
            ELSE
               RMS    = SQRT(RMS/REAL(NFIT-NSPLINE))
            END IF
            
            CHIMAX = CHIMAX/RMS
C     
C     Reject if greater than CLIP*RMS from prediction.
C     
            IF(CHIMAX.GT.CLIP) WORK(3,IREJ) = -ABS(WORK(3,IREJ))
         END DO
C
C     Recover uncertainties now. First fits are with unit weights to prevent
C     being affected by single points too much.
C     
         IF(.NOT.TWEAK .AND. ICYCLE.EQ.1) THEN
            DO IB = 1, NUMBLOCK
               IF(WORK(3,IB).GT.0.D0) WORK(3,IB) = UNCERT(IB)
            END DO
         END IF

      END DO
C     
C     Restore positions
C     
      IF(NPOLY.GT.0) THEN
         DO IB = 1, NUMBLOCK
            WORK(2,IB) = WORK(2,IB) + YFIT(IB)
         END DO
      END IF
C     
C     Fit for residual plot
C     
      IF(NFPOLY.GT.0) 
     &     CALL LSQUAR(WORK,NUMBLOCK,NFPOLY,SUBFIT,CHISQ,XM,0)
      NREJTOT = 0
      DO I = 1, NUMBLOCK
         IF(WORK(3,I).LT.0.) NREJTOT = NREJTOT + 1
      END DO
      CALL MSG_SETR('CHI',RMS*RMS)
      CALL MSG_OUT(' ',
     &     'Reduced Chi-squared of fit = ^CHI',STATUS)
      CALL MSG_SETR('DEV',SQRT(PRMS/REAL(NFIT)))
      CALL MSG_OUT(' ','RMS deviation = ^DEV pixels',STATUS)
      CALL MSG_SETI('REJ',NREJTOT)
      CALL MSG_OUT(' ','Rejected ^REJ points during fit.',
     &     STATUS)
C     
C     Plot TRACK
C     
      IF(FPLOT) THEN

         DO I = 1, NPLOT
            PLOTY(I)  = Y1 + (Y2-Y1)*REAL(I-1)/REAL(NPLOT-1)
            DBUFFER(I) = DBLE(PLOTY(I))
         END DO
         CALL SPLCALCB(NPLOT, DBUFFER, XKNOT, CSPLINE, NSPLINE,
     &        NORD, YSTORE, IFAIL)
         IF(IFAIL.GT.0) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP('TRACK_SPEC',
     &           'Failed to evaluate spline for plot', STATUS)
            RETURN
         END IF
         IF(NPOLY.GT.0) THEN
            DO I = 1, NPLOT
               YSTORE(I) = YSTORE(I) + 
     &              POLY(CPOLY,NPOLY,(DBUFFER(I)-XMID)/XHRANGE)
            END DO
         END IF
         DO I = 1, NPLOT
            PLOTX(I) = REAL(YSTORE(I))
         END DO

         CALL MSG_SYNC(STATUS)
         CALL PGSCI(2)
         CALL PGSLW(2)
         CALL PGLINE(NPLOT, PLOTX, PLOTY)
         CALL PGCLOS
      END IF

C
C     Some information to the user
C
      DBUFFER(1) = DBLE(YLO)
      DBUFFER(2) = DBLE(YLO+YHI)/2.
      DBUFFER(3) = DBLE(YHI)
      CALL SPLCALCB(3, DBUFFER, XKNOT, CSPLINE, NSPLINE,
     &        NORD, YSTORE, IFAIL)

      DO I = 1, 3
         IF(NPOLY.GT.0) THEN
            YSTORE(I) = YSTORE(I) + 
     &           POLY(CPOLY,NPOLY,(DBUFFER(I)-XMID)/XHRANGE)
         END IF
         CALL MSG_SETR('X',REAL(YSTORE(I)))
         CALL MSG_SETR('Y',REAL(DBUFFER(I)))
         CALL MSG_OUT(' ','Y = ^Y, X = ^X', STATUS)
      END DO

      IF(PLOT) THEN
         WRITE(*,'(A,$)') 'Do you want to plot fit ? [Y] '
         READ(*,'(A)') REPLY
         CALL UPPER_CASE(REPLY)
      ELSE
         REPLY = 'N'
      END IF
      IF(REPLY.NE.'N') THEN
         DEVHI = -1000.
         DEVLO =  1000.
         DO I = 1, NUMBLOCK
            IF(WORK(3,I).GT.0.) THEN
               IF(NFPOLY.GT.0) THEN
                  SUB = REAL(WORK(2,I) - POLY(SUBFIT,NFPOLY,WORK(1,I)))
               ELSE
                  SUB = REAL(WORK(2,I))
               END IF
               DEVLO = MIN(DEVLO, SUB)
               DEVHI = MAX(DEVHI, SUB)
            END IF
         END DO
         CALL MSG_SYNC(STATUS)
         RANGE = MAX(1.,DEVHI - DEVLO)
         DEVLO = DEVLO - 0.1*RANGE
         DEVHI = DEVHI + 0.1*RANGE
         IFAIL = PGOPEN(DEVICE)
         IF(IFAIL.LE.0) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP('TRACK_SPEC',
     &           'Failed to open plot device', STATUS)
            RETURN
         ELSE
            CALL PGSCI(5)
            CALL PGENV(REAL(YLO-1),REAL(YLO+NYS),DEVLO,DEVHI, 0, 1)
            CALL PGSCI(7)
            YLABEL = 'X position - order    fit'
            WRITE(YLABEL(20:21),'(I2)') NFPOLY
            CALL PGLAB('Y position',YLABEL,TITLE)
            DO I = 1, NUMBLOCK

               IF(NPOLY.GT.0) THEN
                  PLOTX(I) = REAL(XHRANGE*WORK(1,I)+XMID)
                  PLOTY(I) = REAL(YFIT(I) + POLY(CPOLY,NPOLY,WORK(1,I)))
               ELSE
                  PLOTX(I) = REAL(WORK(1,I))
                  PLOTY(I) = REAL(YFIT(I))
               END IF
               
               IF(NFPOLY.GT.0) THEN
                  PLOTY(I) = REAL(PLOTY(I) - 
     &                 POLY(SUBFIT,NFPOLY,WORK(1,I)))
                  Y  = REAL(WORK(2,I) - POLY(SUBFIT,NFPOLY,WORK(1,I)))
               ELSE
                  Y  = REAL(WORK(2,I))
               END IF

               IF(WORK(3,I).GT.-1.E9) THEN
                  Y1 = REAL(Y - WORK(3,I))
                  Y2 = Y + REAL(WORK(3,I))
                  CALL PGMOVE(PLOTX(I),Y1)
                  CALL PGDRAW(PLOTX(I),Y2)
               END IF
               IF(WORK(3,I).GT.0.) THEN
                  CALL PGSCI(1)
                  CALL PGPOINT(1,PLOTX(I),Y,20)
               ELSE
                  CALL PGSCI(3)
                  CALL PGPOINT(1,PLOTX(I),Y,3)
               END IF
            END DO

            CALL PGSCI(2)
            CALL PGLINE(NUMBLOCK, PLOTX, PLOTY)
            CALL PGIDEN
            CALL PGCLOS
         END IF
      END IF
      IFAIL = 0
      RETURN
      END
      
      SUBROUTINE RCOLLP(PROFILE, VAR, MXPROF, DATA, BALANCE,
     &     NX, NY, I1, I2, J1, J2, VAR0, PHOTON, PSIG, RBUF, SBUF,
     &     ACC, MXBUF, OK)
C
C     Subroutine for collapsing profile with rejection
C
      IMPLICIT NONE
      INCLUDE 'PRM_PAR'
      INTEGER MXPROF, I, J, I1, I2, J1, J2, MXBUF
      INTEGER NOK, JMAX, NX, NY
      REAL DATA(NX,NY), BALANCE(NX,NY), BAL, DAT
      REAL PROFILE(MXPROF), VAR(MXPROF), PSIG, VAR0
      REAL PHOTON, RBUF(MXBUF), SBUF(MXBUF), ERR, DMAX
      LOGICAL ACC(MXBUF), OK
      DOUBLE PRECISION SUM, SUM1, SUM2
*
      IF(J2-J1.GT.1) THEN
         OK = .TRUE.
         DO I = I1, I2
            DO J = J1, J2
               IF(BALANCE(I,J).EQ.VAL__BADR .OR. 
     &              DATA(I,J).EQ.VAL__BADR) THEN
                  ACC(J) = .FALSE.
               ELSE
                  SBUF(J) = BALANCE(I,J)
                  RBUF(J) = SBUF(J)*DATA(I,J)
                  ACC(J)  = .TRUE.
               END IF
            END DO
C
C     Rejection scheme based upon variance estimated from scatter
C     in data only. This in conservative and should not go bananas
C     around sky lines I hope.
C
 1000       SUM  = 0.D0
            SUM1 = 0.D0
            NOK  = 0
            DO J = J1, J2
               IF(ACC(J)) THEN
                  SUM  = SUM  + RBUF(J)
                  SUM1 = SUM1 + RBUF(J)**2
                  NOK  = NOK + 1
               END IF
            END DO
            IF(NOK.GT.1) THEN
               SUM  = SUM/REAL(NOK)
               ERR  = REAL(SQRT((SUM1-SUM*SUM*DBLE(NOK))/REAL(NOK-1)))
               DMAX = -1.E30
               DO J = J1, J2
                  IF(ACC(J) .AND. ABS(RBUF(J)-SUM).GT.DMAX) THEN
                     DMAX = REAL(ABS(RBUF(J)-SUM))
                     JMAX = J
                  END IF
               END DO
               IF(DMAX.GT.PSIG*ERR) THEN
                  ACC(JMAX) = .FALSE.
                  GOTO 1000
               END IF
            END IF
C
C     Final variances on points depend on noise model
C
            SUM1 = 0.D0
            SUM2 = 0.D0
            NOK  = 0
            DO J = J1, J2
               IF(ACC(J)) THEN
                  NOK  = NOK + 1
                  SUM1 = SUM1 + RBUF(J)
                  SUM2 = SUM2 + SBUF(J)*(SBUF(J)*VAR0+ 
     &                   MAX(0.,RBUF(J))/PHOTON)
               END IF
            END DO
            IF(NOK.GT.0) THEN
               PROFILE(I) = REAL(SUM1*REAL(J2-J1+1)/REAL(NOK))
               VAR(I)     = REAL(SUM2*(REAL(J2-J1+1)/REAL(NOK))**2)
            ELSE
               OK = .FALSE.
            END IF
         END DO
      ELSE
C
C     Too few to apply rejection. Any bad pixels
C     and whole profile goes
C
         OK = .TRUE.
         DO I = I1, I2
            VAR(I)     = 0.
            PROFILE(I) = 0.
         END DO
         DO J = J1, J2
            DO I = I1, I2
               IF(OK .AND. (BALANCE(I,J).EQ.VAL__BADR
     &              .OR. DATA(I,J).EQ.VAL__BADR)) OK = .FALSE.
               BAL        = BALANCE(I,J)
               DAT        = BAL*DATA(I,J)
               PROFILE(I) = PROFILE(I) + DAT
               VAR(I)     = VAR(I) + BAL*(BAL*VAR0 + 
     &              MAX(0.,DAT)/PHOTON)
            END DO
         END DO
      END IF
C     
C     Test for zero counts
C     
      IF(OK) THEN
         OK = .FALSE.
         DO I = I1, I2
            OK = OK .OR. ABS(PROFILE(I)).GT.0.1
         END DO
      END IF
      RETURN
      END

C
C     Copy data over
C
      SUBROUTINE TDATD(INPUT, OUTPUT, N)
      INTEGER N, I
      DOUBLE PRECISION INPUT(N), OUTPUT(N)
      DO I = 1, N
         OUTPUT(I) = INPUT(I)
      END DO
      RETURN
      END
