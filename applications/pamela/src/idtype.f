*IDTYPE
* IDTYPE -- identify type of data, ARC, FLAT or DATA. This is useful
*           for automated processing of data.
*
* IDTYPE collapses a frame in X and Y and then searches for peaks.
* Depending upon what it finds it classifies the frame as an ARC,
* FLAT or DATA. The dispersion should run along the Y direction and
* the frame should have been debiassed. Thus if it finds no object
* but some lines it will identify the type as ARC. If it finds an
* object it will always return DATA. If it finds neither lines nor
* object it will return FLAT. There are both mulitplicative and
* additive thresholds that the peak height must pass to count.
*
* To be any use, IDTYPE must be carried out before any other operation.
* However, iuts operation can be affected by severely tilted data, therefore
* it can operate with a trace file which need not be perfect but can be used
* to remove gross distortions.
*
* IDTYPE recognises bad pixels.
*
* Parameters:
*
*  IMAGE     -- Image name
*
*  FLAT      -- Flat field name
*
*  TRACE     -- Load a spectrum track?
*
*  TRACK     -- If TRACE, name of track.
*
*  XSTART, XEND, YSTART, YEND -- Region of frame to consider.
*
*  NXWIDTH   -- Width of median filter to apply in X direction; must be
*               odd = 1 will have no effect.
*
*  NYWIDTH   -- Width of median filter to apply in Y direction; must be
*               odd = 1 will have no effect.
*
*  TOBJM -- For a peak to be detected in the spatial direction it
*           must be TOBJM times the median.
*
*  TOBJA -- For a peak to be detected in the spatial direction it
*           must also be TOBJA above the median.
*
*  TLINM -- For a peak to be detected in the dispersion direction it
*           must be TLINM times the median.
*
*  TLINA -- For a peak to be detected in the dispersion direction it
*           must also be TLINA above the median.
*
*  FLIM  -- If a frame fails the object and arc tests then it is classified
*           as a FLAT or JUNK. FLIM is the dividing limit in terms of
*           mean counts. i.e. above FLIM, it counts as FLAT.
*
*IDTYPE
      SUBROUTINE IDTYPE(STATUS)
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'CNF_PAR'
      INTEGER STATUS, LBND(2), UBND(2), NDIM, IMAGE, FLAT, TRIM(2)
      INTEGER XLO, XHI, YLO, YHI, NXS, NYS, TEMP, IPTR, FPTR
      INTEGER EL, NOBJ, ILINE, IOBJ, NLINE, NPIX, NXWIDTH, NYWIDTH
      INTEGER PLACE, WORK, WPTR
      REAL TOBJM, TOBJA, TLINM, TLINA, TOTAL, SOBJ, SLINE, SKY, CONT
      REAL MEAN, FLIM
      LOGICAL BASE, TRACE
      DOUBLE PRECISION YPOS, TOFF
      CHARACTER*8 ID

      IF(STATUS.NE.SAI__OK) RETURN
      CALL NDF_BEGIN
C
C     Open data file
C
      CALL NDF_ASSOC('IMAGE', 'READ',IMAGE, STATUS)
C
C     Open balance frame
C
      CALL NDF_ASSOC('FLAT','READ',FLAT,STATUS)
C
C     Force to same size
C
      TRIM(1) = IMAGE
      TRIM(2) = FLAT
      CALL NDF_MBNDN('TRIM', 2, TRIM, STATUS)
      IMAGE = TRIM(1)
      FLAT  = TRIM(2)
C
C     Trace
C
      CALL PAR_GET0L('TRACE', TRACE, STATUS)
      IF(TRACE) CALL GET_TRACK(YPOS, TOFF, STATUS)
C
C     What region of frame?
C
      CALL NDF_BOUND(IMAGE, 2, LBND, UBND, NDIM, STATUS)
      CALL NDF_ISBAS(IMAGE, BASE, STATUS)
      IF(NDIM.NE.2) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','IDTYPE requires 2D data', STATUS)
      END IF
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
C
      CALL PAR_GODD('NXWIDTH',11,1,1001,.FALSE.,NXWIDTH,STATUS)
      CALL PAR_GODD('NYWIDTH',11,1,1001,.FALSE.,NYWIDTH,STATUS)
      CALL PAR_GDR0R('TOBJM',2.,1.,1.E30,.FALSE.,TOBJM,STATUS)
      CALL PAR_GDR0R('TOBJA',50.,0.,1.E30,.FALSE.,TOBJA,STATUS)
      CALL PAR_GDR0R('TLINM',2.,1.,1.E30,.FALSE.,TLINM,STATUS)
      CALL PAR_GDR0R('TLINA',50.,0.,1.E30,.FALSE.,TLINA,STATUS)
      CALL PAR_GDR0R('FLIM',10000.,0.,1.E30,.FALSE.,FLIM,STATUS)
C
C     Map data files
C
      IF(TRACE) THEN
         CALL NDF_TEMP(PLACE, STATUS)
         CALL NDF_NEW('_REAL',2,LBND,UBND,PLACE,WORK,STATUS)
         CALL NDF_MAP(WORK,'Data','_REAL','WRITE',WPTR,EL,STATUS)
      END IF

      CALL NDF_MAP(IMAGE,'Data','_REAL','READ',IPTR,EL,STATUS)
      CALL NDF_MAP( FLAT,'Data','_REAL','READ',FPTR,EL,STATUS)
C
      CALL RECOG(%VAL(CNF_PVAL(IPTR)), %VAL(CNF_PVAL(FPTR)),
     &     %VAL(CNF_PVAL(WPTR)), NXS, NYS, XLO, YLO, NXWIDTH,
     &     NYWIDTH, TOBJM, TOBJA, TLINM, TLINA, FLIM, TRACE,
     &     TOTAL, NPIX, MEAN, NOBJ, SOBJ, IOBJ, SKY,
     &     NLINE, SLINE, ILINE, CONT, ID, STATUS)
C
      IF(STATUS.EQ.SAI__OK) THEN
C
C     Output information
C
         CALL MSG_BLANK(STATUS)
         CALL NDF_MSG('FILE',IMAGE)
         CALL MSG_OUT(' ',
     &        'Frame = ^FILE',STATUS)
         CALL MSG_SETI('XLO',XLO)
         CALL MSG_SETI('XHI',XHI)
         CALL MSG_SETI('YLO',YLO)
         CALL MSG_SETI('YHI',YHI)
         CALL MSG_OUT(' ','X: ^XLO to ^XHI, Y: ^YLO to ^YHI',
     &        STATUS)
         CALL MSG_SETC('ID',ID)
         CALL MSG_SETR('TOTAL',TOTAL)
         CALL MSG_OUT(' ','Type = ^ID, total count = ^TOTAL',
     &        STATUS)
         CALL MSG_SETI('NPIX',NPIX)
         CALL MSG_SETR('MEAN',MEAN)
         CALL MSG_OUT(' ','Number of valid pixels= ^NPIX, mean = ^MEAN',
     &        STATUS)
         CALL MSG_SETR('SKY',SKY)
         CALL MSG_SETI('NOBJ',NOBJ)
         CALL MSG_SETR('SOBJ',SOBJ)
         CALL MSG_SETI('IOBJ',IOBJ+XLO-1)
         CALL MSG_OUT(' ',
     &        'Sky = ^SKY, no. of obj = ^NOBJ'//
     &        ', max = ^SOBJ, at pixel ^IOBJ',STATUS)
         CALL MSG_SETR('CONT',CONT)
         CALL MSG_SETI('NLINE',NLINE)
         CALL MSG_SETR('SLINE',SLINE)
         CALL MSG_SETI('ILINE',YLO-1+ILINE)
         CALL MSG_OUT(' ',
     &        'Cont = ^CONT, no. of lines = ^NLINE'//
     &        ', max = ^SLINE, at pixel ^ILINE',STATUS)
      END IF
      CALL NDF_END(STATUS)
      RETURN
      END

      SUBROUTINE RECOG(DATA, BAL, WORK, NX, NY, XLO, YLO,
     &     NXWIDTH, NYWIDTH, TOBJM, TOBJA, TLINM, TLINA,
     &     FLIM, TRACE, TOTAL, NPIX, MEAN, NOBJ, SOBJ, IOBJ,
     &     SKY, NLINE, SLINE, ILINE, CONT, ID, STATUS)
C
C     Subroutine to compute and return useful information to
C     help identify the type of a data frame (e.g. FLAT, DATA, ARC)
C     and to return other numbers to describe quality thereof.
C
C     Data assumed to be dispersed in Y direction.
C     Only frame from IXLO to IXHI, IYLO to IYHI will be treated.
C     Crude at the moment. Will not cope with strong slopes in either
C     spatial or spectral direction.
C
C     R*4 DATA(NX,NY) -- Data frame
C     R*4 BAL(NX,NY)  -- Balance frame
C     R*4 WORK(NX,NY) -- Workspace for distorted spectrum case
C     I*4 NX, NY      -- Dimensions
C     I*4 XLO, YLO    -- Original corner of frame
C     I*4 NXWIDTH, NYWIDTH -- Median filter widths
C     R*4 TOBJM       -- Number of times a maximum must be above the sky
C                        to count as an object
C     R*4 TOBJA       -- Number of counts a maximum must be above the sky
C                        to count as an object
C     R*4 TLINM       -- Number of times a maximum must be above the continuum
C                        to count as a line
C     R*4 TLINA       -- Number of counts a maximum must be above the continuum
C                        to count as a line
C     R*4 FLIM        -- Dividing limit between FLAT and JUNK
C     L   TRACE       -- TRUE to trace track file
C     R*4 TOTAL       -- Total counts in region of interest
C     I*4 NPIX        -- Total number of pixels
C     R*4 MEAN        -- Mean value
C     I*4 NOBJ        -- Number of objects identified
C     R*4 SOBJ        -- Value of highest peak (with sky subtracted) if any
C                        object identified.
C     I*4 IOBJ        -- X value of strongest object
C     R*4 SKY         -- Sky value from median of collapse in Y
C     I*4 NLINE       -- Number of lines found
C     R*4 SLINE       -- Peak value of strongest line found (continuum removed)
C     I*4 ILINE       -- Position of strongest line
C     R*4 CONT        -- Median of collapse in X
C     C*(*) ID        -- Tentative identification of frame type
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      LOGICAL TRACE
      INTEGER NX, NY, IOBJ, NLINE, ILINE, IX, IY, NXWIDTH, NYWIDTH
      INTEGER NOBJ, STATUS, NOK, NPIX, I1, I2, IFAIL, XLO, YLO
      REAL DATA(NX, NY), BAL(NX, NY), TOBJM, TOBJA, TLINM, TLINA
      REAL TOTAL, SOBJ, SLINE, SKY, CONT, THRESHM, WORK(NX, NY)
      REAL THRESHA, VAL, MEAN, FLIM, XSHIFT, R1, R2
      DOUBLE PRECISION POLY, XREF, D1, D2, YD, XD
      CHARACTER*(*) ID
C
C     Fixed size buffers for image profile and median. IRANK
C     must match the largest of MAX or MAXY
C
      INTEGER MAXX, MAXY
      PARAMETER (MAXX = 3000, MAXY = 5000)
      INTEGER IRANK(MAXY)
      REAL XDATA(MAXX), XFILT(MAXX), XPROF(MAXX)
      REAL YDATA(MAXY), YFILT(MAXY), YSPEC(MAXY)
      DOUBLE PRECISION SUM
C
      IF(STATUS.NE.SAI__OK) RETURN

      IF(NX.GT.MAXX .OR. NY.GT.MAXY) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Buffer in IDTYPE too small.',STATUS)
         RETURN
      END IF
C
C     First compute total number of counts.
C
      TOTAL = 0.
      NPIX  = 0
      DO IY = 1, NY
         DO IX = 1, NX
            VAL = DATA(IX,IY)
            IF(VAL.NE.VAL__BADR) THEN
               TOTAL = TOTAL + VAL
               NPIX  = NPIX + 1
            END IF
         END DO
      END DO
      IF(NPIX.EQ.0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Found no good data at all',STATUS)
         RETURN
      END IF
      MEAN = TOTAL/REAL(NPIX)
C
C     Compute average profile by collapsing in Y
C
      IF(.NOT.TRACE) THEN
C
C     No distortion map case
C
         DO IX = 1, NX
            NOK = 0
            DO IY = 1, NY
               IF(BAL(IX,IY).NE.VAL__BADR .AND.
     &              DATA(IX,IY).NE.VAL__BADR) THEN
                  NOK = NOK + 1
                  YDATA(NOK) = BAL(IX,IY)*DATA(IX,IY)
               END IF
            END DO
            IF(NOK.GT.0) THEN
               CALL MEDFILT(YDATA,YFILT,NOK,NYWIDTH,IFAIL)
               SUM = 0.D0
               DO IY = 1, NOK
                  SUM = SUM + YFILT(IY)
               END DO
               XPROF(IX) = REAL(SUM/REAL(NOK))
            ELSE
               XPROF(IX) = VAL__BADR
            END IF
         END DO
      ELSE
C
C     X values referred relative to values calculated at mid point
C     The mid-point value is returned as it will be needed by any
C     routines that use the sky regions chosen
C
         CALL GET_TRACK(DBLE(2*YLO+NY-1)/2., XREF, STATUS)
C
C     Distortion coefficient case
C
         DO IY = 1, NY
C
C     Compute shift, load into buffer, shift to the right and then
C     add into profile
C
            CALL GET_TRACK(DBLE(IY+YLO-1), XD, STATUS)
            XSHIFT = REAL(XD - XREF)
            DO IX = 1, NX
               IF(BAL(IX,IY).NE.VAL__BADR .AND.
     &              DATA(IX,IY).NE.VAL__BADR) THEN
                  XDATA(IX) = BAL(IX,IY)*DATA(IX,IY)
               ELSE
                  XDATA(IX)  = 0.
               END IF
            END DO
            CALL REBIN(0, 0, XDATA, NX, XFILT, 1, XSHIFT,
     &           0, D1, 0, D2, R1, R2)
C
C     Mask pixels affected by bad data
C
            DO IX = 1, NX
               I1 = INT(REAL(IX)-XSHIFT)
               I2 = I1+1
               IF((I1.GE.1 .AND. I1.LE.NX .AND.
     &              (BAL(I1,IY).EQ.VAL__BADR .OR.
     &              DATA(I1,IY).EQ.VAL__BADR)) .OR.
     &              (I2.GE.1 .AND. I2.LE.NX .AND.
     &              (BAL(I2,IY).EQ.VAL__BADR .OR.
     &              DATA(I2,IY).EQ.VAL__BADR))) THEN
                  WORK(IX,IY) = VAL__BADR
               ELSE
                  WORK(IX,IY) = XFILT(IX)
               END IF
            END DO
         END DO
         DO IX = 1, NX
            NOK = 0
            DO IY = 1, NY
               IF(WORK(IX,IY).NE.VAL__BADR) THEN
                  NOK = NOK + 1
                  YDATA(NOK) = WORK(IX,IY)
               END IF
            END DO
            IF(NOK.GT.0) THEN
               CALL MEDFILT(YDATA,YFILT,NOK,NYWIDTH,IFAIL)
               SUM = 0.
               DO IY = 1, NOK
                  SUM = SUM + YFILT(IY)
               END DO
               XPROF(IX) = REAL(SUM/REAL(NOK))
            ELSE
               XPROF(IX) = VAL__BADR
            END IF
         END DO
      END IF
C
C     Now compute median of profile (XFILT just used as a temporary)
C
      NOK = 0
      DO IX = 1, NX
         IF(XPROF(IX).NE.VAL__BADR) THEN
            NOK = NOK + 1
            XFILT(NOK) = XPROF(IX)
         END IF
      END DO
      IF(NOK.GT.0) THEN
         CALL HEAPSORT(NOK,XFILT,IRANK)
C
C     Set 'SKY' equal to median value
C
         SKY = XFILT(IRANK(NOK/2+1))
         IF(MOD(NOK,2).EQ.0) THEN
            SKY = (SKY+XFILT(IRANK(NOK/2)))/2.
         END IF
      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Found no good data in X',STATUS)
         RETURN
      END IF
C
C     Objects are identified as local maxima above TOBJM times AND
C     TOBJA above SKY with SKY set to minimum of 1 for the
C     multiplicative part. Bad pixels are now set very negative.
C
      THRESHM = TOBJM*MAX(1., SKY)
      THRESHA = SKY + TOBJA
      NOBJ = 0
      SOBJ = -1.E30
      DO IX = 1, NX
         IF(XPROF(IX).EQ.VAL__BADR) XPROF(IX) = 1.1*SOBJ
      END DO
      IOBJ = 0
      DO IX=2,NX-1
         IF(XPROF(IX).GE.XPROF(IX-1) .AND.
     &        XPROF(IX).GT.XPROF(IX+1) .AND.
     &        XPROF(IX).GT.THRESHM .AND. XPROF(IX).GT.THRESHA) THEN
            NOBJ = NOBJ + 1
            IF(XPROF(IX).GT.SOBJ) THEN
               SOBJ = XPROF(IX)
               IOBJ = IX
            END IF
         END IF
      END DO
      SOBJ = SOBJ - SKY
      IF(NOBJ.EQ.0) SOBJ = 0.
C
C     Compute average spectrum by collapsing in X
C     (no distortion allowed for)
C
      DO IY = 1, NY
         NOK = 0
         DO IX = 1, NX
            IF(BAL(IX,IY).NE.VAL__BADR .AND.
     &           DATA(IX,IY).NE.VAL__BADR) THEN
               NOK = NOK + 1
               XDATA(NOK) = BAL(IX,IY)*DATA(IX,IY)
            END IF
         END DO
         IF(NOK.GT.0) THEN
            CALL MEDFILT(XDATA,XFILT,NOK,NXWIDTH,IFAIL)
            SUM = 0.D0
            DO IX = 1, NOK
               SUM = SUM + XFILT(IX)
            END DO
            YSPEC(IY) = REAL(SUM/REAL(NOK))
         ELSE
            YSPEC(IX) = VAL__BADR
         END IF
      END DO
C
C     Now compute median of profile (YFILT just used as a temporary)
C
      NOK = 0
      DO IY = 1, NY
         IF(YSPEC(IY).NE.VAL__BADR) THEN
            NOK = NOK + 1
            YFILT(NOK) = YSPEC(IY)
         END IF
      END DO
      IF(NOK.GT.0) THEN
         CALL HEAPSORT(NOK,YFILT,IRANK)
C
C     Set 'CONT' equal to median value
C
         CONT = YFILT(IRANK(NOK/2+1))
         IF(MOD(NOK,2).EQ.0) THEN
            CONT = (CONT+YFILT(IRANK(NOK/2)))/2.
         END IF
      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Found no good data in Y',STATUS)
         RETURN
      END IF
C
C     Lines are identified as local maxima above TLINM times CONT
C     with minimum of 1, AND TLINA above CONT
C
      THRESHM = TLINM*MAX(1., CONT)
      THRESHA = CONT + TLINA
      NLINE = 0
      ILINE = 0
      SLINE = -1.E30
      DO IY = 1, NY
         IF(YSPEC(IY).EQ.VAL__BADR) YSPEC(IY) = 1.1*SLINE
      END DO
      DO IY=2,NY-1
         IF(YSPEC(IY).GE.YSPEC(IY-1) .AND.
     &        YSPEC(IY).GT.YSPEC(IY+1) .AND.
     &        YSPEC(IY).GT.THRESHM .AND. YSPEC(IY).GT.THRESHA) THEN
            NLINE = NLINE + 1
            IF(YSPEC(IY).GT.SLINE) THEN
               SLINE = YSPEC(IY)
               ILINE = IY
            END IF
         END IF
      END DO
      SLINE = SLINE - CONT
      IF(NLINE.EQ.0) SLINE = 0.
C
C     Finally try to identify the data type.
C
      IF(NOBJ.GT.0) THEN
         ID = 'DATA'
      ELSE IF(NLINE.GT.1) THEN
         ID = 'ARC'
      ELSE IF(MEAN.GT.FLIM) THEN
         ID = 'FLAT'
      ELSE
         ID = 'JUNK'
      END IF
      RETURN
      END
