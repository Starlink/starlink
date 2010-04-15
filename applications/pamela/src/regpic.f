*REGPIC
*
* REGPIC -- selects regions which define sky and object in a frame.
*
* This routine allows the user to interactively select regions defining
* the sky and object. The selection can either be done from a greyscale image
* or better, since it is easier to pick up faint stars, by collapsing the
* frame along the disperion direction.
*
* Regions which are significantly different from the average should be avoided
* since they may be being distorted by other objects on the slit. Try to
* choose regions symmetrically about an object and choose the same regions for
* standard as well as target stars, even though it may be obvious that the
* wings of the standard star spill light into sky regions that looked ok for
* your target.
*
* Parameters:
*
*  IMAGE     -- The frame under analysis
*
*  FLAT      -- Balance factor frame
*
*  DLOAD     -- TRUE if dark frame is required.
*
*  DARK      -- Dark frame representing counts unaffected by slit
*               profile. This will be subtracted off data before applying
*               the balance factors.
*
*  TRACE     -- True if a distortion file is wanted
*
*  If TRACE
*
*      TRACK     -- Name of distortion file. Inside this file will
*                   have been stored dimensions of frame from which
*                   it was generated. These must match those of
*                   IMAGE and FLAT.
*
*  REGION      -- The output file containing the selected regions.
*
*  XSTART,XEND -- X pixel limits of region to be treated.
*
*  YSTART,YEND -- Y pixel limits of region to be treated.
*
*  GREY      -- .TRUE. if greyscale image wanted, .FALSE. for a profile
*               plot. FALSE is normally recommended because profile plots
*               are better for spotting contaminating stars etc.
*
*  AUTO      -- Yes to find scales automatically. Based on going from first
*               to third quartiles, with some extra knobs and twiddles.
*
*  If .NOT.AUTO then set LIMITS
*
*  NWIDTH -- If not GREY, a median filter is applied during the collapse;
*            NWIDTH is its width; NWIDTH = 1 has no effect, but it is very
*            useful in getting rid of cosmic rays. Must be odd.
*
*  DEVICE -- Plot device
*
*  METHOD -- Method for selecting regions. C=cursor,T=terminal.
*
*REGPIC
      SUBROUTINE REGPIC(STATUS)
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CNF_PAR'
      INTEGER STATUS, IMAGE, FLAT, REGION, TEMP
      INTEGER IPTR, FPTR, RPTR, LBND(2), UBND(2)
      INTEGER DIM(2), NDIM, XLO, XHI, YLO, YHI, NSET
      INTEGER WORK, WPTR, EL, SMALL, DARK, TRIM(3), DPTR
      INTEGER NWIDTH, PLACE, FLEN
      REAL LIMITS(2), LEFT, RIGHT
      LOGICAL BASE, THERE, TRACE, AUTO, GREY, DLOAD
      CHARACTER*(DAT__SZLOC) LOC
      CHARACTER*128 DEVICE, FILE
      CHARACTER*1 METHOD
      DOUBLE PRECISION YREF, YPOS, TOFF
C
      IF(STATUS.NE.SAI__OK) RETURN
      CALL NDF_BEGIN
C
C     Open data file
C
      CALL NDF_ASSOC('IMAGE','READ',IMAGE,STATUS)
C
C     Open balance frame
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
C     Trace
C
      CALL PAR_GET0L('TRACE', TRACE, STATUS)
      IF(TRACE) CALL GET_TRACK(YPOS, TOFF, STATUS)
C
C     Open sky region file, set new title. Propagate from
C     special small NDF section
C
      CALL NDF_SECT(IMAGE,1,1,1,SMALL,STATUS)
      CALL NDF_PROP(SMALL,' ','REGION',REGION,STATUS)
      CALL NDF_RESET(REGION,'Title',STATUS)
      CALL NDF_CPUT('REGPIC output',REGION,'Title',STATUS)
C
C     Want greyscale or profile ?
C
      CALL PAR_GET0L('GREY',GREY,STATUS)
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
C     Replace IMAGE, FLAT and DARK with ndf sections
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
      DIM(1) = UBND(1)-LBND(1)+1
      DIM(2) = UBND(2)-LBND(2)+1
C
C     Beat output file into shape
C
      CALL NDF_RESET(REGION,'Data',STATUS)
      CALL NDF_SBND(1,LBND(1),UBND(1),REGION,STATUS)
      CALL NDF_STYPE('_INTEGER',REGION,'Data',STATUS)
C
C     Set plotting parameters
C
      CALL PAR_GET0L('GREY', GREY, STATUS)
      CALL PAR_GET0L('AUTO', AUTO, STATUS)
      IF(.NOT.AUTO) CALL PAR_GET1R('LIMITS',2,LIMITS,NSET,STATUS)
      IF(GREY) THEN
         CALL PAR_GET0C('DEVICE',DEVICE,STATUS)
      ELSE
         CALL PAR_GODD('NWIDTH',11,1,1001,.FALSE.,NWIDTH,STATUS)
         CALL PAR_GET0C('DEVICE',DEVICE,STATUS)
      END IF
      CALL PAR_CHOIC('METHOD','C','C,T',.FALSE.,METHOD,STATUS)
C
C     Get work space
C
      IF(GREY .OR. TRACE) THEN
         CALL NDF_TEMP(PLACE, STATUS)
         CALL NDF_NEW('_REAL',2,LBND,UBND,PLACE,WORK,STATUS)
         CALL NDF_MAP(WORK,'Data','_REAL','WRITE',WPTR,EL,STATUS)
      END IF
C
C     Map files
C
      CALL NDF_MAP(IMAGE,'Data','_REAL','READ',IPTR,EL,STATUS)
      CALL NDF_MAP(FLAT,'Data','_REAL','READ',FPTR,EL,STATUS)
      CALL NDF_MAP(DARK,'Data','_REAL','READ/ZERO',DPTR,EL,STATUS)
      CALL NDF_MAP(REGION,'Data','_INTEGER','WRITE',RPTR,EL,STATUS)
C
C     Generate plot title
C
      CALL NDF_MSG('NAME', IMAGE)
      CALL MSG_LOAD('BLA','^NAME',FILE,FLEN,STATUS)
C
C     Pick sky regions
C
      CALL PICK_SKY(%VAL(CNF_PVAL(IPTR)), %VAL(CNF_PVAL(FPTR)),
     &     %VAL(CNF_PVAL(DPTR)), %VAL(CNF_PVAL(RPTR)),
     &     LEFT, RIGHT, DIM(1), DIM(2), LBND(1), LBND(2),
     &     GREY, AUTO, TRACE, LIMITS(1), LIMITS(2),
     &     %VAL(CNF_PVAL(WPTR)), NWIDTH, DEVICE, FILE, YREF,
     &     METHOD, STATUS)
C
C     Store limits for next time
C
      IF(AUTO) THEN
         CALL PAR_DEF1R('LIMITS',2,LIMITS,STATUS)
         CALL PAR_GET1R('LIMITS',2,LIMITS,NSET,STATUS)
      END IF
C
C     Create 'pamela' extension if not already one present.
C     Write full frame size and left and right limits to it.
C
      CALL NDF_XSTAT(REGION, 'PAMELA', THERE, STATUS)
      IF(.NOT.THERE) THEN
         CALL NDF_XNEW(REGION,'PAMELA', 'Ext', 0, 0, LOC, STATUS)
         CALL DAT_NEW(LOC, 'REGPIC', 'Struct', 0, 0, STATUS)
      ELSE
         CALL NDF_XLOC(REGION,'PAMELA','UPDATE',LOC,STATUS)
         CALL DAT_THERE(LOC, 'REGPIC', THERE, STATUS)
         IF(.NOT.THERE)
     &        CALL DAT_NEW(LOC, 'REGPIC', 'Struct', 0, 0, STATUS)
      END IF
      CALL DAT_ANNUL(LOC, STATUS)
      CALL NDF_XPT0R(LEFT,REGION,'PAMELA','REGPIC.LEFT',STATUS)
      CALL NDF_XPT0R(RIGHT,REGION,'PAMELA','REGPIC.RIGHT',STATUS)
      IF(TRACE)
     &     CALL NDF_XPT0D(YREF,REGION,'PAMELA','REGPIC.YREF',STATUS)
C
C     Tidy up
C
      CALL NDF_END(STATUS)
      RETURN
      END

      SUBROUTINE PICK_SKY(DATA, BAL, DARK, MASK, LEFT, RIGHT, NXS,
     &     NYS, XLO, YLO, GREY, AUTO, TRACE, LOW, HIGH, WORK, NWIDTH,
     &     DEVICE, TOPLABEL, YREF, METHOD, STATUS)
C
C     PICK_SKY
C
C     Written by T.R. Marsh, June 1988.
C     Modified by TRM @STSCI June 1989 to have a median filter option.
C     modified 13/01/98 for NDF sections
C
C     PICK_SKY allows the display of a data frame either as
C     a greyscale image or a profile in X. The user must then
C     pick sky and object regions for a number of objects.
C     This can be done with a cursor if one is available or
C     through the terminal.
C
C Arguments
C
C     On input:
C
C     REAL DATA(NXS,NYS)   -- The data frame to be displayed.
C
C     REAL BAL(NXS,NYS)    -- Balance frame
C
C     REAL DARK(NXS,NYS)   -- Dark frame
C
C     INTEGER NXS, NYS     -- The size of the data frame section
C
C     INTEGER XLO, YLO   -- lower corner in terms of original frame
C     coords
C
C     LOGICAL GREY       -- .TRUE. for greyscale, else profile display.
C
C     LOGICAL AUTO       -- .TRUE. for automatic scaling.
C
C     LOGICAL TRACE      -- Use a tracing of the spectrum or not
C
C     REAL LOW, HIGH     -- The lower and upper limits for plot.
C
C     LOGICAL INIT       -- If .TRUE. the original values of MASK are used to
C     ignore certain regions. This is useful if there is
C     very weak structure that cannot be detected on a
C     single frame but will show up on their sum.
C
C     INTEGER NWIDTH     -- Width of median filter. .LE. 1 to ignore. This
C     is applied in Y (after any rebinning) to remove
C     cosmic rays.
C
C     CHARACTER*(*) DEVICE  Plot device
C
C     CHARACTER*(*) TOPLABEL  Plot label
C
C     REAL WORK(NXS*NYS)  -- Work array required if GREY used to store
C     balanced image
C
C     DOUBLE PRECISION YREF -- Returned Y reference position in case of
C     use of TRACK poly
C
C     On output:
C
C     INTEGER MASK(NXS)  -- 0 for pixels not in the sky, 1 for a sky pixel.
C
C     REAL LEFT, RIGHT -- The X pixel limits of the object.
C
C     DOUBLE YREF -- To be kept if need be.
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INTEGER NXS, NYS, IX, IY, IFAIL, STATUS
      REAL DATA(NXS,NYS), TR(6), HIGH, LOW
      REAL BAL(NXS,NYS), WORK(NXS,NYS), DARK(NXS,NYS)
      INTEGER MASK(NXS), NWIDTH, IX1, IX2, N1, N2, N3
      INTEGER LENGTH, NOK, I1, I2, XLO, YLO, PGOPEN, ID
      REAL LEFT, RIGHT, R1, R2, XX1, XX2, YY1, YY2, Q1, Q2, Q3
      REAL X1, X2, Y1, Y2, SPREAD, XSHIFT
      CHARACTER*40 BACK, REPLY*5
      CHARACTER*(*) DEVICE, TOPLABEL, METHOD
      LOGICAL GREY, AUTO, TRACE
      DOUBLE PRECISION XREF, YREF, D1, D2, YD, XD
C
C     Fixed size buffers for image profile and median
C
      INTEGER MAXX, MAXY
      PARAMETER (MAXX = 3000, MAXY = 5000)
      REAL PROF(MAXX), XDATA(MAXX), TPROF(MAXX)
      INTEGER IRANK(MAXX)
      REAL YDATA(MAXY), YFILT(MAXY)
      DOUBLE PRECISION SUM
      INTEGER NHIST
      PARAMETER (NHIST=100)
      REAL HIST(NHIST), DATMIN, DATMAX
C
      IF(STATUS.NE.SAI__OK) RETURN
      IF(MAXX.LT.NXS .OR. MAXY.LT.NYS) THEN
         CALL MSG_SETI('MAXX',MAXX)
         CALL MSG_SETI('MAXY',MAXY)
         CALL MSG_OUT(' ','Frame large than maximum ^MAXX, ^MAXY',
     &        STATUS)
         CALL MSG_OUT(' ','Fixed buffers need increasing',
     &        STATUS)
         STATUS = SAI__ERROR
         RETURN
      END IF
C
      DO IX = 1, NXS
         MASK(IX) = 0
      END DO
      LEFT = 0.
      RIGHT= 0.
C
C     X values referred relative to values calculated at mid point
C     The mid-point value is returned as it will be needed by any
C     routines that use the sky regions chosen
C

      YREF = DBLE(2*YLO+NYS-1)/2.
      IF(TRACE)
     &     CALL GET_TRACK(YREF, XREF, STATUS)
C
C     Open device
C
      ID = PGOPEN(DEVICE)
      IF(ID.LT.1) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC('DEVICE',DEVICE)
         CALL ERR_REP(' ','Failed to open plot device ^DEVICE',
     &        STATUS)
         RETURN
      END IF
C
C     Check that cursor is possible
C
      CALL PGQINF('CURSOR', BACK, LENGTH)
      IF(BACK(1:3).EQ.'YES' .AND. METHOD.EQ.'C') THEN
         REPLY = 'C'
      ELSE IF(BACK(1:3).NE.'YES' .AND. METHOD.EQ.'C') THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ',
     &        'Cursor not possible on device ^DEVICE',STATUS)
         RETURN
      ELSE
         REPLY = 'T'
      END IF
C
      IF(GREY) THEN
         TR(1) = REAL(XLO-1)
         TR(2) = 1.
         TR(3) = 0.
         TR(4) = REAL(YLO-1)
         TR(5) = 0.
         TR(6) = 1.
         X1 = REAL(XLO) - 0.5
         X2 = REAL(XLO+NXS-1) + 0.5
         Y1 = REAL(YLO) - 0.5
         Y2 = REAL(YLO+NYS-1) + 0.5
         DO IY = 1, NYS
            DO IX = 1, NXS
               IF(BAL(IX,IY).NE.VAL__BADR .AND.
     &              DATA(IX,IY).NE.VAL__BADR .AND.
     &              DARK(IX,IY).NE.VAL__BADR) THEN
                  WORK(IX,IY) = BAL(IX,IY)*(DATA(IX,IY)-DARK(IX,IY))
               ELSE
                  WORK(IX,IY) = VAL__BADR
               END IF
            END DO
         END DO
         IF(AUTO) THEN
            DATMIN =  1.E30
            DATMAX = -1.E30
            DO IY = 1, NYS
               DO IX = 1, NXS
                  IF(WORK(IX,IY).NE.VAL__BADR) THEN
                     DATMIN  = MIN(DATMIN, WORK(IX,IY))
                     DATMAX  = MAX(DATMAX, WORK(IX,IY))
                  END IF
               END DO
            END DO
            CALL CDF_LOC(WORK, NXS, NYS, HIST, NHIST, 0.1, 2,
     &           DATMIN, DATMAX, LOW)
            CALL CDF_LOC(WORK, NXS, NYS, HIST, NHIST, 0.9, 2,
     &           DATMIN, DATMAX, HIGH)
         END IF
         CALL PGENV(X1,X2,Y1,Y2,0,-2)
         CALL PGGRAY(WORK,NXS,NYS,1,NXS,1,NYS,HIGH,LOW,TR)
         CALL PGBOX('BCNST', 0., 0, 'BCNST', 0., 0)
         CALL PGLAB('X pixels','Y pixels',TOPLABEL)
      ELSE
C
C     Compute average profile by collapsing in Y
C
         IF(.NOT.TRACE) THEN
C
C     No distortion map case
C
            DO IX = 1, NXS
               NOK = 0
               DO IY = 1, NYS
                  IF(BAL(IX,IY).NE.VAL__BADR .AND.
     &                 DATA(IX,IY).NE.VAL__BADR .AND.
     &                 DARK(IX,IY).NE.VAL__BADR) THEN
                     NOK = NOK + 1
                     YDATA(NOK) = BAL(IX,IY)*(DATA(IX,IY)-DARK(IX,IY))
                  END IF
               END DO
               IF(NOK.GT.0) THEN
                  CALL MEDFILT(YDATA,YFILT,NOK,NWIDTH,IFAIL)
                  SUM = 0.D0
                  DO IY = 1, NOK
                     SUM = SUM + YFILT(IY)
                  END DO
                  PROF(IX) = REAL(SUM/REAL(NOK))
               ELSE
                  PROF(IX) = VAL__BADR
               END IF
            END DO

         ELSE
C
C     Distortion coefficient case
C
            DO IY = 1, NYS
C
C     Compute shift, load into buffer, shift to the right and then
C     add into profile
C
               CALL GET_TRACK(DBLE(IY+YLO-1), XD, STATUS)
               XSHIFT = REAL(XD-XREF)
               DO IX = 1, NXS
                  IF(BAL(IX,IY).NE.VAL__BADR .AND.
     &                 DATA(IX,IY).NE.VAL__BADR .AND.
     &                 DARK(IX,IY).NE.VAL__BADR) THEN
                     XDATA(IX) = BAL(IX,IY)*(DATA(IX,IY)-DARK(IX,IY))
                  ELSE
                     XDATA(IX)  = 0.
                  END IF
               END DO
               CALL REBIN(0, 0, XDATA, NXS, TPROF, 1, XSHIFT,
     &              0, D1, 0, D2, R1, R2)
C
C     Mask pixels affected by bad data
C
               DO IX = 1, NXS
                  I1 = INT(REAL(IX)-XSHIFT)
                  I2 = I1+1
                  IF((I1.GE.1 .AND. I1.LE.NXS .AND.
     &                 (BAL(I1,IY).EQ.VAL__BADR .OR.
     &                 DATA(I1,IY).EQ.VAL__BADR .OR.
     &                 DARK(I1,IY).EQ.VAL__BADR)) .OR.
     &                 (I2.GE.1 .AND. I2.LE.NXS .AND.
     &                 (BAL(I2,IY).EQ.VAL__BADR .OR.
     &                 DATA(I2,IY).EQ.VAL__BADR .OR.
     &                 DARK(I2,IY).EQ.VAL__BADR))) THEN
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
                  CALL MEDFILT(YDATA,YFILT,NOK,NWIDTH,IFAIL)
                  SUM = 0.
                  DO IY = 1, NOK
                     SUM = SUM + YFILT(IY)
                  END DO
                  PROF(IX) = REAL(SUM/REAL(NOK))
               ELSE
                  PROF(IX) = VAL__BADR
               END IF
            END DO
         END IF
C
C     Now have the profile in PROF. Get ready for plot.
C
         DO IX = 1, NXS
            XDATA(IX) = REAL(XLO-1+IX)
         END DO
         X1 = REAL(XLO) - 0.5
         X2 = REAL(XLO+NXS-1) + 0.5
         IF(AUTO) THEN
C
C     Find quartiles for automatic scaling
C
            NOK = 0
            DO IX = 1, NXS
               IF(PROF(IX).NE.VAL__BADR) THEN
                  NOK = NOK + 1
                  TPROF(NOK) = PROF(IX)
               END IF
            END DO
            CALL HEAPSORT(NOK, TPROF, IRANK)
            N1 = MAX(1,   NOK/4)
            N2 = MAX(1, 2*NOK/4)
            N3 = MAX(1, 3*NOK/4)
            Q1 = TPROF(IRANK(N1))
            Q2 = TPROF(IRANK(N2))
            Q3 = TPROF(IRANK(N3))
            SPREAD = MAX(Q3-Q1, 1.E-20)
            LOW  = Q2 - 4.*SPREAD
            HIGH = Q2 + 4.*SPREAD
            IF(LOW.GT.0. .AND. LOW/SPREAD.LT.10) LOW = 0.
         END IF
         Y1 = LOW
         Y2 = HIGH
         CALL PGENV(X1,X2,Y1,Y2,0,0)
         CALL PGLAB('X pixels', 'Average profile', TOPLABEL)
C
C     Plot good parts of profile
C
         I2 = 0
         DO WHILE(I2.LT.NXS)
            I1 = I2 + 1
            DO WHILE(I1.LE.NXS .AND.
     &           PROF(I1).EQ.VAL__BADR)
               I1 = I1 + 1
            END DO
            IF(I1.LT.NXS) THEN
               I2 = I1 + 1
               DO WHILE(I2.LE.NXS .AND.
     &              PROF(I2).NE.VAL__BADR)
                  I2 = I2 + 1
               END DO
               I2 = I2 - 1
            ELSE IF(I1.EQ.NXS) THEN
               I2 = I1
            END IF
            IF(I1.LE.NXS)
     &           CALL PGBIN(I2-I1+1, XDATA(I1),
     &           PROF(I1),.TRUE.)
         END DO
      END IF
C
C     Now select regions
C
      WRITE(*,*) ' '
      WRITE(*,*) 'Define sky/object mask by specifying'//
     &     ' a series of regions'
      WRITE(*,*) ' '
      WRITE(*,*) ' '
      IF(GREY .AND. TRACE) THEN
         REPLY(2:2) = 'B'
      ELSE
         REPLY(2:2) = 'X'
      END IF
      IFAIL = 0
      DO WHILE(IFAIL.EQ.0)
         CALL GET_LIM(REPLY, XX1, XX2, REAL(XLO),
     &        REAL(XLO+NXS-1), YY1, YY2, REAL(YLO),
     &        REAL(YLO+NYS-1), IFAIL)
         IF(IFAIL.EQ.0) THEN
            IF(GREY .AND. TRACE) THEN
C
C     Refer chosen region back to centre
C
               CALL GET_TRACK(DBLE(YY1), XD, STATUS)
               XX1 = REAL(XX1 + XREF - XD)
               CALL GET_TRACK(DBLE(YY2), XD, STATUS)
               XX2 = REAL(XX2 + XREF - XD)
            END IF
            IX1 = NINT(XX1)
            IX2 = NINT(XX2)
            XX1 = REAL(IX1)
            XX2 = REAL(IX2)
            IX1 = IX1 - XLO + 1
            IX2 = IX2 - XLO + 1
            DO IX = IX1, IX2
               MASK(IX) = 1
            END DO
C
C     Plot region as red dashed lines
C
            CALL PGSCI(2)
            CALL PGSLS(2)
            CALL PLOTREG(XX1,XX2,Y1,Y2,XREF,TRACE,GREY,STATUS)
            IF(STATUS.NE.SAI__OK) THEN
               CALL PGCLOS
               RETURN
            END IF
         END IF
      END DO
C
      WRITE(*,*) ' '
      WRITE(*,*) 'Sky region selection finished. '
      WRITE(*,*) 'Now pick object limits in the same way.'
      WRITE(*,*) ' '
      IFAIL = 1
      DO WHILE(IFAIL.NE.0)
         CALL GET_LIM(REPLY, LEFT, RIGHT, REAL(XLO),
     &        REAL(XLO+NXS-1), YY1, YY2, REAL(YLO),
     &        REAL(YLO+NYS-1), IFAIL)
         IF(IFAIL.NE.0) WRITE(*,*) 'Try again'
      END DO
      IF(GREY .AND. TRACE) THEN
         CALL GET_TRACK(DBLE(YY1),XD,STATUS)
         LEFT  = REAL(LEFT + XREF - XD)
         CALL GET_TRACK(DBLE(YY2),XD,STATUS)
         RIGHT = REAL(RIGHT + XREF - XD)
      ELSE
         LEFT  = REAL(NINT(LEFT))
         RIGHT = REAL(NINT(RIGHT))
      END IF
C
C     Plot object in green solid lines
C
      CALL PGSLS(1)
      CALL PGSCI(3)
      XX1 = MAX(X1, LEFT)
      XX2 = MIN(X2, RIGHT)
      CALL PLOTREG(XX1,XX2,Y1,Y2,XREF,TRACE,GREY,STATUS)
      CALL PGCLOS
      RETURN
      END

      SUBROUTINE PLOTREG(X1,X2,Y1,Y2,XREF,TRACE,GREY,STATUS)
C
C     Plots lines at X1 and X2 extending from Y1 to Y2
C     and a horizontal line at (Y1+Y2)/2 from X1 to X2
C     If TRACE then distortion is included.
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INTEGER MAXPLOT, STATUS
      REAL X1, X2, Y1, Y2
      PARAMETER (MAXPLOT=400)
      REAL PLOTX(MAXPLOT), PLOTY(MAXPLOT)
      REAL LINEX(2), LINEY(2)
      DOUBLE PRECISION XREF, XD, YD
      INTEGER IY
      LOGICAL GREY, TRACE
C
      IF(STATUS.NE.SAI__OK) RETURN
      IF(.NOT.TRACE .OR. .NOT.GREY) THEN
         LINEX(1) = X1
         LINEX(2) = X1
         LINEY(1) = Y1
         LINEY(2) = Y2
         CALL PGLINE(2, LINEX, LINEY)
         LINEX(1) = X2
         LINEX(2) = X2
         LINEY(1) = Y1
         LINEY(2) = Y2
         CALL PGLINE(2, LINEX, LINEY)
         LINEX(1) = X1
         LINEX(2) = X2
         LINEY(1) = (Y1+Y2)/2.
         LINEY(2) = (Y1+Y2)/2.
         CALL PGLINE(2, LINEX, LINEY)
      ELSE
         DO IY = 1, MAXPLOT
            PLOTY(IY) = Y1 + (Y2-Y1)*REAL(IY-1)/REAL(MAXPLOT-1)
            CALL GET_TRACK(DBLE(PLOTY(IY)), XD, STATUS)
            IF(STATUS.NE.SAI__OK) RETURN
            PLOTX(IY) = REAL(X1+XD-XREF)
         END DO
         CALL PGLINE(MAXPLOT, PLOTX, PLOTY)
         DO IY = 1, MAXPLOT
            PLOTY(IY) = Y1 + (Y2-Y1)*REAL(IY-1)/REAL(MAXPLOT-1)
            CALL GET_TRACK(DBLE(PLOTY(IY)), XD, STATUS)
            PLOTX(IY) = REAL(X2+XD-XREF)
         END DO
         CALL PGLINE(MAXPLOT, PLOTX, PLOTY)
         CALL GET_TRACK(DBLE(Y1+Y2)/2., XD, STATUS)
         LINEX(1) = REAL(X1 + XD - XREF)
         LINEX(2) = REAL(X2 + XD - XREF)
         LINEY(1) = (Y1+Y2)/2.
         LINEY(2) = (Y1+Y2)/2.
         CALL PGLINE(2, LINEX, LINEY)
      END IF
      RETURN
      END


