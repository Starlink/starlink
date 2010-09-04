*SKYMOV
*
* SKYMOV  -- moves sky region files according to movement along slit.
*
* In time series work the basic regions are unlikely to change much except
* that the star may wander along the slit. skymov is designed to re-jig a
* file dumped by 'regpic' to account for such movement. It works by starting
* from a template set-up with regpic and looking at the change in position of
* a reference object.
*
* Beyond the edges of the valid X range skymov takes whatever value (sky or
* not) that the edge pixels have. Thus the edge pixels should be sky if you
* want more sky to be brought in following a large shift.
*
* If you there are particular bad pixels fixed in position then these are best applied
* using KAPPA'S ardmask to mask them.
*
* Parameters:
*
*  IMAGE   -- Data file which will define the modified sky regions.
*
*  FLAT    -- Balance frame.
*
*  REGION  -- Name of input sky region file
*
*  TRACE   -- TRUE if you will use a poly fit to define the spectrum position
*
*  If TRACE
*
*     TRACK  -- The poly fit file
*
*
* OUTSKY -- Name of modified region file (not same as INSKY)
*
* XSTART, XEND -- valid region of data in X.
*
* YSTART, YEND -- valid region of data in Y
*
* SLO, SHI -- the reference object will only be searched for between these
*             limits (allows one to choose a fainter star if that is what
*             is wanted).
*
* FWHM  -- The reference object is located and its position measured by
*          cross-correlation with a gaussian of width FWHM
*
* XPOS  -- position of object used to determine REGION. This is best found
*          by running skymov once on REGION, putting any old value for XPOS,
*          then recording whatever value it returns.
*
* NPOLY -- The frame is collapsed and a poly fitted to the background. This
*          fit can be used to reject bad pixels.
*
* THRLO, THRHI -- low, high sigma reject thresholds. You might set THRHI
*          more tightly than THRLO because it is easy to get stars on the
*          slit which would produce an excess. With time series data where
*          you can be sure that stars will not appear on the slit, you should
*          set these to be very wide.
*
* NMIN  -- Pixels only rejected if there is a group of NMIN or more next
*          to each other. This is a safeguard against rejecting good sky
*          pixels because of cosmic rays.
*
* MEDIAN - TRUE to median filter to remove cosmic rays before collapsing
*          profile
*
* if MEDIAN
*
*    NWIDTH  -- Half width of median fileter. Will use 2*NWIDTH+1
*SKYMOV
      SUBROUTINE SKYMOV(STATUS)
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'CNF_PAR'
      INTEGER STATUS, NXS, NYS, LBND(2), UBND(2)
      INTEGER IMAGE, FLAT, REGION, OUTSKY, NDIM, NMIN
      INTEGER XLO, XHI, YLO, YHI, TEMP, SLO, SHI
      INTEGER ISPTR, OSPTR, DPTR, FPTR, SKLO, WORK
      INTEGER SKHI, EL, WPTR, PLACE, NPOLY, NWIDTH
      REAL LEFT, RIGHT, XPOS, THRLO, THRHI, FWHM
      DOUBLE PRECISION YREF, YPOS, TOFF
      LOGICAL BASE, IBASE, FBASE, TRACE, MEDIAN
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
C     Get sky region input file
C
      CALL NDF_ASSOC('REGION','READ',REGION,STATUS)
C
      CALL NDF_XGT0R(REGION,'PAMELA','REGPIC.LEFT',LEFT,STATUS)
      CALL NDF_XGT0R(REGION,'PAMELA','REGPIC.RIGHT',RIGHT,STATUS)
      IF(STATUS.EQ.SAI__OK .AND. (LEFT.GT.RIGHT .OR.
     &     LEFT.LE.0.)) THEN
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
C Get sky output file name
C
      CALL NDF_PROP(REGION,' ','OUTSKY',OUTSKY,STATUS)
C
C What region of frame
C
      CALL NDF_ISBAS(IMAGE, IBASE, STATUS)
      CALL NDF_ISBAS(FLAT, FBASE, STATUS)
      BASE = IBASE .AND. FBASE
      CALL NDF_MBND('TRIM',IMAGE,FLAT,STATUS)
      CALL NDF_BOUND(IMAGE, 2, LBND, UBND, NDIM, STATUS)
      IF(BASE) THEN
         CALL PAR_GDR0I('XSTART',LBND(1),LBND(1),UBND(1),
     &        .FALSE.,XLO,STATUS)
         CALL PAR_GDR0I('XEND',UBND(1),XLO,UBND(1),
     &        .FALSE.,XHI,STATUS)
         CALL PAR_GDR0I('YSTART',LBND(2),LBND(2),UBND(2),
     &        .FALSE.,YLO,STATUS)
         CALL PAR_GDR0I('YEND',UBND(2),YLO,UBND(2),
     &        .FALSE.,YHI,STATUS)
C
C Replace IMAGE and FLAT with section ndfs
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
C
C Enforce same X dimensions on sky region files
C
      CALL NDF_BOUND(REGION, 1, SKLO, SKHI, NDIM, STATUS)
      IF(SKLO.NE.LBND(1) .OR. SKHI.NE.UBND(1)) THEN
         CALL NDF_SECT(REGION, 1, LBND, UBND, TEMP, STATUS)
         CALL NDF_ANNUL(REGION, STATUS)
         CALL NDF_CLONE(TEMP, REGION, STATUS)
         CALL NDF_ANNUL(TEMP, STATUS)
         CALL NDF_SECT(OUTSKY, 1, LBND, UBND, TEMP, STATUS)
         CALL NDF_ANNUL(OUTSKY, STATUS)
         CALL NDF_CLONE(TEMP, OUTSKY, STATUS)
         CALL NDF_ANNUL(TEMP, STATUS)
      END IF
C
C     More parameters
C
      CALL PAR_GDR0I('SLO',1,LBND(1),UBND(1),.FALSE.,SLO,STATUS)
      CALL PAR_GDR0I('SHI',UBND(1),SLO,UBND(1),.FALSE.,SHI,STATUS)
      CALL PAR_GDR0R('FWHM',2.,2.,1.E4,.FALSE.,FWHM,STATUS)
      CALL PAR_GDR0R('XPOS',REAL(LBND(1)+UBND(1))/2.,
     &     REAL(LBND(1)),REAL(UBND(1)),.FALSE.,XPOS,STATUS)
      CALL PAR_GDR0I('NPOLY',2,1,20,.FALSE.,NPOLY,STATUS)
      CALL PAR_GDR0R('THRLO',-4.,-1.E10,1.E10,.FALSE.,THRLO,STATUS)
      CALL PAR_GDR0R('THRHI',3.,THRLO,1.E10,.FALSE.,THRHI,STATUS)
      CALL PAR_GDR0I('NMIN',3,1,NXS,.FALSE.,NMIN,STATUS)
      CALL PAR_GET0L('MEDIAN',MEDIAN,STATUS)
      IF(MEDIAN)
     &     CALL PAR_GODD('NWIDTH',7,3,101,.FALSE.,NWIDTH,STATUS)
C
C     Map files
C
      CALL NDF_MAP(REGION,'Data','_INTEGER','READ',ISPTR,EL,STATUS)
      CALL NDF_MAP(OUTSKY,'Data','_INTEGER','WRITE',OSPTR,EL,STATUS)
      CALL NDF_MAP(IMAGE,'Data','_REAL','READ',DPTR,EL,STATUS)
      CALL NDF_MAP(FLAT,'Data','_REAL','READ',FPTR,EL,STATUS)
C
C     Get workspace
C
      CALL NDF_TEMP(PLACE, STATUS)
      CALL NDF_NEW('_REAL',2,LBND,UBND,PLACE,WORK,STATUS)
      CALL NDF_MAP(WORK,'Data','_REAL','WRITE',WPTR,EL,STATUS)
C
C     Shift sky regions
C
      CALL SKY_MOV(%VAL(CNF_PVAL(DPTR)), %VAL(CNF_PVAL(FPTR)), NXS, NYS,
     &     %VAL(CNF_PVAL(ISPTR)), %VAL(CNF_PVAL(OSPTR)),
     &     %VAL(CNF_PVAL(WPTR)), SLO, SHI, XLO, YLO, XPOS, FWHM,
     &     MEDIAN, THRLO, THRHI, NPOLY, NMIN, LEFT, RIGHT, NWIDTH,
     &     TRACE, YREF, STATUS)
C
C     Write Left and right to the output file.
C
      CALL NDF_XPT0R(LEFT,OUTSKY,'PAMELA','REGPIC.LEFT',STATUS)
      CALL NDF_XPT0R(RIGHT,OUTSKY,'PAMELA','REGPIC.RIGHT',STATUS)
      CALL NDF_END(STATUS)
      RETURN
      END

      SUBROUTINE SKY_MOV(DATA, BAL, NXS, NYS, SKYIN, SKYOUT,
     &WORK, SLO, SHI, XLO, YLO, XPOS, FWHM, MEDFLT,
     &THRLO, THRHI, NPOLY, NMIN, LEFT, RIGHT, NWIDTH,
     &TRACE, YREF, STATUS)
*
* SKY_MOV shifts the sky regions input in SKYIN according to the
* position of an object searched for in the range XLO to XHI
* If the sky regions go up to the edge of the valid X range,
* then it is assumed that regions beyond the edge are OK.
*
* Inputs:
*
* R*4 DATA(NXS,NYS)  -- The data frame
* R*4 BAL(NXS,NYS)  -- Multiplicative balance factor frame
* I*4 NXS, NYS       -- Dimensions of frame section
* I*4 SKYIN(NXS)     -- 1 for sky regions, 0 otherwise. This is the
*                       initial sky region that will be shifted
* I*4 SKYOUT(NXS)    -- Output region
* R*4 WORK(NXS,NYS)  -- Work space
* I*4 SLO, SHI     -- Range in X to search for object
* I*4 XLO, YLO     -- Lower left corner of frame
* R*4 XPOS         -- Reference position of object. Shift to be applied
*                     is measured relative to this value.
* R*4 FWHM         -- Object position is measured by X-correlation with
*                     a gaussian of FWHM = FWHM
* L   MEDFLT       -- .TRUE., then a median filter is applied in Y before
*                     taking the average.
*
* R*4 THRLO, THRHI -- reject thresholds for bad sky
* I*4 NPOLY        -- Order of poly fit
* I*4 NMIN         -- Minimum contiguous size for bad sky
* L   INITIAL      -- see entry for SKYOUT
* R*4 LEFT, RIGHT  -- Left and right limits of object region.
* I*4 NWIDTH       -- The pixel width (odd) of the median filter
* L   TRACE        -- Is a track file being used.
* D   YREF         -- reference Y value
*
* Output:
*
* I*4 SKYOUT(NX)  -- Shifted sky regions.
* R*4 LEFT, RIGHT -- Shifted object limits.
*
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INTEGER NXS, NYS, XLO, SLO, SHI, YLO, NWIDTH
      INTEGER IX, N, IFAIL, IY, IPEAK, NPIX, J, NR, NMIN
      INTEGER NPOLY, NFIT, ICYCLE, IREJ, NDF, STATUS, NOK
      INTEGER I1, I2, JLIM
      REAL DATA(NXS,NYS), BAL(NXS,NYS), WORK(NXS,NYS), LEFT, RIGHT
      REAL XPOS, FWHM, R1, R2, D1, D2, XSHIFT, PEAK, RPEAK
      REAL SIGBIN, SHIFT, THRLO, THRHI, DEV, DMAX
      DOUBLE PRECISION SUM, RMS, XREF, YREF, XD, POLY
      INTEGER SKYIN(NXS), SKYOUT(NXS), NB
      LOGICAL MEDFLT, TRACE
      INTEGER MAXY, MAXX
      PARAMETER (MAXY=10000, MAXX=2000)
      REAL YDATA(MAXY), YSTORE(MAXY), PROF(MAXX), MEDVAL
      REAL XDATA(MAXX), TPROF(MAXX), SKYFIT(MAXX)
*
      IF(STATUS.NE.SAI__OK) RETURN

      IF(NYS.GT.MAXY) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Too many Y pixels for skmov',STATUS)
         RETURN
      END IF
      IF(NXS.GT.MAXX) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Too many X pixels for skmov',STATUS)
         RETURN
      END IF
C
C Compute average or median profile of frame
C No distortion map coefficients.
C
      IF(.NOT.TRACE) THEN
         DO IX = 1, NXS
            NOK = 0
            DO IY = 1, NYS
               IF(BAL(IX,IY).NE.VAL__BADR .AND.
     &              DATA(IX,IY).NE.VAL__BADR .AND.
     &              SKYIN(IX).NE.VAL__BADI) THEN
                  NOK = NOK + 1
                  YDATA(NOK) = BAL(IX,IY)*DATA(IX,IY)
               END IF
            END DO
            IF(NOK.GT.0) THEN
               IF(MEDFLT) THEN
                  CALL MEDFILT(YDATA,YSTORE,NOK,NWIDTH,IFAIL)
                  SUM = 0.
                  DO IY = 1, NOK
                     SUM = SUM + YSTORE(IY)
                  END DO
               ELSE
                  SUM = 0.
                  DO IY = 1, NOK
                     SUM = SUM + YDATA(IY)
                  END DO
               END IF
               PROF(IX) = REAL(SUM/REAL(NOK))
            ELSE
               PROF(IX) = VAL__BADR
            END IF
         END DO

      ELSE
C
C     Distortion coefficient case
C
         CALL GET_TRACK(YREF, XREF, STATUS)
         IF(STATUS.NE.SAI__OK) RETURN

         DO IY = 1, NYS
            CALL GET_TRACK(DBLE(IY+YLO-1),XD,STATUS)
            IF(STATUS.NE.SAI__OK) RETURN
            XSHIFT = REAL(XD-XREF)
            DO IX = 1, NXS
               IF(BAL(IX,IY).NE.VAL__BADR .AND.
     &              DATA(IX,IY).NE.VAL__BADR) THEN
                  XDATA(IX) = BAL(IX,IY)*DATA(IX,IY)
               ELSE
                  XDATA(IX)  = 0.
               END IF
            END DO
            CALL REBIN(0, 0, XDATA, NXS, TPROF, 1, XSHIFT,
     &           0, D1, 0, D2, R1, R2)
C
C     Mask pixels affected by bad data
C
            DO IX = 1, NXS
               I1 = INT(REAL(IX)-XSHIFT)
               I2 = I1+1
               IF((I1.GE.1 .AND. I1.LE.NXS .AND.
     &              (BAL(I1,IY).EQ.VAL__BADR .OR.
     &              DATA(I1,IY).EQ.VAL__BADR)) .OR.
     &              (I2.GE.1 .AND. I2.LE.NXS .AND.
     &              (BAL(I2,IY).EQ.VAL__BADR .OR.
     &              DATA(I2,IY).EQ.VAL__BADR .OR.
     &              SKYIN(IX).EQ.VAL__BADR))) THEN
                  WORK(IX,IY) = VAL__BADR
               ELSE
                  WORK(IX,IY) = TPROF(IX)
               END IF
            END DO
         END DO
         DO IX = 1, NXS
            NOK = 0
            DO IY = 1, NYS
               IF(WORK(IX,IY).NE.VAL__BADR) THEN
                  NOK = NOK + 1
                  YDATA(NOK) = WORK(IX,IY)
               END IF
            END DO
            IF(NOK.GT.0) THEN
               IF(MEDFLT) THEN
                  CALL MEDFILT(YDATA,YSTORE,NOK,NWIDTH,IFAIL)
                  SUM = 0.
                  DO IY = 1, NOK
                     SUM = SUM + YSTORE(IY)
                  END DO
               ELSE
                  SUM = 0.
                  DO IY = 1, NOK
                     SUM = SUM + YDATA(IY)
                  END DO
               END IF
               PROF(IX) = REAL(SUM/REAL(NOK))
            ELSE
               PROF(IX) = VAL__BADR
            END IF
         END DO
      END IF
C
C     We now have profile over the object search region
C     with bad pixels set to the bad pixekl value. Locate highest pixel in region
C
      NOK   = 0
      SUM   = 0.
      PEAK  = -1.E30
      DO IX = SLO-XLO+1, SHI-XLO+1
         IF(PROF(IX).NE.VAL__BADR) THEN
            NOK = NOK + 1
            TPROF(NOK) = PROF(IX)
            IF(PROF(IX).GT.PEAK) THEN
               PEAK  = PROF(IX)
               IPEAK = IX
            END IF
         END IF
      END DO
      IF(NOK.EQ.0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Failed to find any good data',STATUS)
         RETURN
      END IF
      SUM = MEDVAL(TPROF, NOK)
      DO IX = SLO-XLO+1, SHI-XLO+1
         IF(PROF(IX).EQ.VAL__BADR) THEN
            TPROF(IX) = REAL(SUM)
         ELSE
            TPROF(IX) = PROF(IX)
         END IF
      END DO
C
C     Refine peak position with MEDIAN
C
      NPIX  = SHI-SLO+1
      IPEAK = IPEAK - (SLO-XLO)
      CALL MEDIAN(TPROF(SLO-XLO+1),NPIX,IPEAK,FWHM,RPEAK,SIGBIN,1)
C
      RPEAK = RPEAK + REAL(SLO-1)
      SHIFT = RPEAK-XPOS
      CALL MSG_SETR('RPEAK',RPEAK)
      CALL MSG_SETR('SHIFT',SHIFT)
      CALL MSG_OUT(' ','Object position = ^RPEAK, shift = ^SHIFT',
     &     STATUS)
C
C     Now shift object and sky regions, preserving number of columns
C     Eliminate bad regions found earlier. Input sky pixels marked bad
C     represent bad columns and are eliminated without a shift
C
      LEFT  = LEFT  + SHIFT
      RIGHT = RIGHT + SHIFT
      IF(.NOT.TRACE) THEN
         LEFT  = NINT(LEFT)
         RIGHT = NINT(RIGHT)
      END IF

      DO IX = 1, NXS
         IF(SKYIN(IX).EQ.VAL__BADI .OR. PROF(IX).EQ.VAL__BADR) THEN
            SKYOUT(IX) = 0
         ELSE
            JLIM = MAX(1,MIN(NINT(REAL(IX)-SHIFT),NXS))
            SKYOUT(IX) = SKYIN(JLIM)
         END IF
      END DO
C
C     Now search for bad sky regions in shifted sky on the basis
C     that they are too discrepant. Set non-sky pixels to have
C     negative sigma to reject them from start.
C
      NPIX = 0
      DO IX = 1, NXS
         XDATA(IX) = REAL(IX)/REAL(NXS)
         IF(SKYOUT(IX).EQ.1) THEN
            NPIX = NPIX + 1
            TPROF(IX) = 1.
         ELSE
            TPROF(IX) = -1.
         END IF
      END DO
      IF(NPIX.LT.NPOLY) THEN
         CALL MSG_OUT(' ','Not enough sky pixels for fit'//
     &        ' and so no test will be applied',STATUS)
      ELSE
C
C     pixel reject cycle and refining the variances
C
         ICYCLE = 0
         IREJ = 1
         DO WHILE(ICYCLE.LE.1 .OR.
     &        (IREJ.GT.0.AND.ICYCLE.LE.NPIX))
            ICYCLE = ICYCLE + 1
C
C     fit polynomial by least squares
C
            CALL POLYFIT1(NXS,XDATA,PROF,TPROF,SKYFIT,NPOLY,IFAIL)
            IF(IREJ.GT.0) THEN
               RMS = 0.D0
               NFIT = 0
               SUM = 0.D0
               DO IX= 1, NXS
                  IF(TPROF(IX).GT.0.) THEN
                     NFIT = NFIT + 1
                     RMS = RMS + (PROF(IX)-SKYFIT(IX))**2
                     SUM = SUM + SKYFIT(IX)
                  END IF
               END DO
               NDF = NFIT-NPOLY
               SUM = SUM / REAL(NFIT)
               RMS = SQRT( RMS / DBLE(MAX(1,NDF)) )
C
C     Look for worst outlier
C
               IREJ = 0
               DMAX = -1.E10
               DO IX= 1, NXS
                  IF( TPROF(IX).GT.0. ) THEN
                     DEV = REAL((PROF(IX)-SKYFIT(IX))/RMS)
                     IF((DEV.GT.THRHI .OR. DEV.LT.THRLO) .AND.
     &                    ABS(DEV).GT.DMAX) THEN
                        DMAX = ABS(DEV)
                        IREJ = IX
                     END IF
                  END IF
               END DO
C
C     Mark so that can be identified later
C
               IF(IREJ.GT.0) TPROF(IREJ) = -100.
            END IF
         END DO
         CALL MSG_SETR('AVE',REAL(SUM))
         CALL MSG_OUT(' ','Mean sky = ^AVE',STATUS)
C
C     Search through for blocks of NMIN or more pixels together;
C     eliminate any smaller than this.
C
         NB = 0
         DO IX = 1, NXS
            IF(TPROF(IX).LT.-10.) THEN
               NB = NB + 1
            ELSE IF(TPROF(IX).GT.-10. .AND. NB.LT.NMIN) THEN
               DO J = IX-NB,IX-1
                  TPROF(J) = 1.
               END DO
               NB = 0
            END IF
         END DO
C
C     Now eliminate bad pixels
C
         NR = 0
         N  = 0
         DO IX = 1, NXS
            IF(TPROF(IX).LT.-10.) THEN
               SKYOUT(IX) = 0.
               NR = NR + 1
               CALL MSG_SETI('IX',IX+XLO-1)
               CALL MSG_OUT(' ','Rejected pixel number ^IX',
     &              STATUS)
            ELSE IF(TPROF(IX).GT.0.5) THEN
               N = N + 1
            END IF
         END DO
         CALL MSG_SETI('NR',NR)
         CALL MSG_SETI('N',N)
         CALL MSG_OUT(' ',
     &        '^NR sky pixels were eliminated; ^N remain.',
     &        STATUS)
      END IF
      RETURN
      END


