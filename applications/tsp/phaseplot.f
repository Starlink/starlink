C+
      SUBROUTINE PHASEPLOT(STATUS)
C
C            P H A S E P L O T
C
C     Command name:
C        PHASEPLOT
C
C     Function:
C        Plot time series data against phase.
C
C     Description:
C        PHASEPLOT plots time series data against the phase of a periodic
C        variation. Up to six items may be plotted against the same phase
C        axis. Each item may be a different channel or Stokes parameter etc.
C        The data may be binned (all points in a given phase bin averaged)
C        or simply folded (each indivdual time point plotted). The
C        plotted phase may range from -1.0 to +2.0 allowing more than
C        one cycle. Plotting is done with the NCAR/SGS/GKS graphics system.
C
C        Specifying the FILE parameter as TRUE causes the data points
C        to be output as a text file, which can then be used in other
C        plotting packages such as MONGO to provide greater control over
C        the final plot.
C
C     Parameters:
C    (1) INPUT      (TSP, 2D)  The input time series dataset.
C    (2) NPLOTS     (Integer)  The number of items to plot (max 6).
C    (3) DEVICE     (Device)   The Graphics device (any valid GKS device).
C        WHOLE      (Logical)  If TRUE, All time points are used.
C    (C) XSTART     (Double)   First time value (MJD) to use.
C    (C) XEND       (Double)   Last time value (MJD) to use.
C        EPOCH      (Double)   The Epoch of phase zero (MJD).
C        PERIOD     (Double)   The Period (days).
C        PHSTART    (Double)   Starting Phase to plot.
C        PHEND      (Double)   End Phase to plot.
C        CHANn      (Integer)  Channel for nth plot. This and the
C                               following parameters repeat for
C                               n = 1 to NPLOTS.
C        ITEMn      (Char)     Item for nth plot (I,FLUX,MAG,Q,U,V,P,THETA)
C        AUTOn      (Logical)  If True nth plot is autoscaled.
C        BINn       (Double)   Bin size (negative for no binning).
C        PLABELn    (Char)     Label for plot n.
C    (C) MINn       (Real)     Minimum scaling level for plot n.
C    (C) MAXn       (Real)     Maximum scaling level for plot n.
C        LABEL      (Char)     Label for Diagram.
C    (H) ERRORS     (Logical)  If True (default), Error bars are plotted.
C    (H) LINE       (Logical)  If True, the points are joined by a
C                               continuous line. (Default False).
C    (H) PEN        (Integer)  SGS Pen number to plot in. (Default 1).
C    (H) SIZE       (Real)     Scaling Factor for character sizes (Default 1).
C    (H) PTOP       (Real)     Position of top of diagram. (Default 0.9).
C    (H) PBOTTOM    (Real)     Position of bottom of diagram. (Default 0.1).
C    (H) FILE       (Logical)  If true generate a text file of data.
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 1/3/1990
C
C-
C
C  History:
C    Nov/1987   Original Version.   JAB/AAO
C    28/2/1988   TSP Monolith version.  JAB/AAO
C    1/3/1990    Handle bad pixels.  JAB/AAO
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS
      INTEGER IPTR,QPTR,UPTR,VPTR          ! Pointers to Stokes arrays
      INTEGER XPTR                         ! Pointer to X (time) array
      INTEGER WPTR                         ! Pointer to wavelength axis
      INTEGER STRT,FIN                     ! Start and finish channels
      INTEGER SIZE                         ! Original size of data in X
      INTEGER FSIZE                        ! Size after X limits selection
      INTEGER NEWSIZE                      ! Size of binned data
      INTEGER NPLOTS                       ! Number of plots
      INTEGER PLOT                         ! Current plot
      LOGICAL AUTO                         ! Autoscaling flag
      REAL IMIN,IMAX                       ! Y scaling limits
      CHARACTER*(DAT__SZLOC) LOC           ! Top level locator
      CHARACTER*1 C                        ! Character to append to
                                           ! parameter names for current plot
      CHARACTER*5 ITEM                     ! Current item to be plotted
      INTEGER CHAN                         ! Current channel to be plotted
      REAL TOP,BOT                         ! Grid positions for Top and
                                           ! Bottom of current plot
      CHARACTER*20 YLABEL                  ! Y label for current plot
      CHARACTER*20 PLABEL                  ! Label for current plot
      CHARACTER*40 XLABEL                  ! X axis label
      LOGICAL FIRST,LAST                   ! First,Last plot flags
      INTEGER NCHANS                       ! Number of channels in data
      INTEGER ZONE                         ! SGS zone for plot
      DOUBLE PRECISION BINSIZE             ! Binsize for current plot
      REAL PTOP, PBOTTOM                   ! Plot positioning
      INTEGER DIMS(3)                      ! Dimensions of data
      INTEGER ACTDIM                       ! Actual number of dimensions
      CHARACTER*(DAT__SZLOC) XLOC,WLOC     ! Axis locators
      LOGICAL FILE
      INTEGER STAT

*  HDS locators

      CHARACTER*(DAT__SZLOC) YRLOC,YELOC,SRLOC,SELOC,S2RLOC,S2ELOC
      CHARACTER*(DAT__SZLOC) XXLOC,XDLOC,TILOC,TDLOC,T1LOC,T2LOC

*   Temporary array pointers

      INTEGER XXPTR,XDPTR,TIPTR,TDPTR,T1PTR,T2PTR
      INTEGER YRPTR,YEPTR,SRPTR,SEPTR
      INTEGER S2RPTR,S2EPTR,STPTR

*  Phase range parameters

      DOUBLE PRECISION PHSTART,PHEND

*  Get the data

      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)

*  Get the number of plots

      CALL PAR_GET0I('NPLOTS',NPLOTS,STATUS)

      IF (STATUS .EQ. SAI__OK) THEN

*  Initialize for plotting

        CALL TSP_PHSINITAG(ZONE,STATUS)

*  Get file parameter

        CALL PAR_GET0L('FILE',FILE,STATUS)

*  Get X Axis data

        CALL TSP_MAP_TIME(LOC,'READ',XPTR,XLOC,STATUS)
        CALL TSP_MAP_LAMBDA(LOC,'READ',WPTR,WLOC,STATUS)

*  Get size of data

        CALL TSP_SIZE(LOC,3,DIMS,ACTDIM,STATUS)
        STRT=1
        FIN=DIMS(2)
        NCHANS=DIMS(1)
        SIZE = FIN-STRT+1

*  Get temporary arrays

        CALL TSP_TEMP(3*SIZE,'_REAL',YRPTR,YRLOC,STATUS)
        CALL TSP_TEMP(3*SIZE,'_REAL',YEPTR,YELOC,STATUS)
        CALL TSP_TEMP(3*SIZE,'_REAL',SRPTR,SRLOC,STATUS)
        CALL TSP_TEMP(3*SIZE,'_REAL',SEPTR,SELOC,STATUS)
        CALL TSP_TEMP(3*SIZE,'_REAL',S2RPTR,S2RLOC,STATUS)
        CALL TSP_TEMP(3*SIZE,'_REAL',S2EPTR,S2ELOC,STATUS)
        CALL TSP_TEMP(3*SIZE,'_REAL',T1PTR,T1LOC,STATUS)
        CALL TSP_TEMP(3*SIZE,'_REAL',T2PTR,T2LOC,STATUS)
        CALL TSP_TEMP(3*SIZE,'_INTEGER',TIPTR,TILOC,STATUS)
        CALL TSP_TEMP(3*SIZE,'_DOUBLE',TDPTR,TDLOC,STATUS)
        CALL TSP_TEMP(3*SIZE,'_DOUBLE',XXPTR,XXLOC,STATUS)
        CALL TSP_TEMP(3*SIZE,'_DOUBLE',XDPTR,XDLOC,STATUS)

*   Find X plotting limits

        CALL TSP_PHSXLIMITS(%VAL(XPTR),STRT,FIN,%VAL(XXPTR),STATUS)
        SIZE = FIN-STRT+1
        FSIZE = SIZE

*  Get phase range parameters

        CALL PAR_GET0D('PHSTART',PHSTART,STATUS)
        CALL PAR_GET0D('PHEND',PHEND,STATUS)

*  Plot Positioning

        CALL PAR_GET0R('PTOP',PTOP,STATUS)
        CALL PAR_GET0R('PBOTTOM',PBOTTOM,STATUS)

*  Loop over multiple plots

        PLOT = 1
        DO WHILE (PLOT .LE. NPLOTS)
          SIZE = FSIZE

*  Get channel number for plot

          C = CHAR(PLOT+ICHAR('0'))
          CALL PAR_GET0I('CHAN'//C,CHAN,STATUS)
          IF ((CHAN .LE. 0) .OR. (CHAN .GT. NCHANS)) THEN
             CALL MSG_OUT('MSG','Invalid Channel',STATUS)
             CHAN=1
          ENDIF

*   Get Item for plot

          CALL PAR_GET0C('ITEM'//C,ITEM,STATUS)
          CALL CHR_UCASE(ITEM)

*   Map required data arrays

          CALL TSP_PHSGETITEM(LOC,CHAN,STRT,FIN,IPTR,QPTR,UPTR,
     :         VPTR,STATUS)
          IF (STATUS .NE. SAI__OK) GOTO 100

*   Check that all the necessary components are available

          IF (ITEM .EQ. 'I' .OR. ITEM .EQ. 'MAG'
     :        .OR. ITEM .EQ. 'FLUX') THEN

*  We need intensity if item is I, MAG or FLUX

              IF (IPTR .EQ. 0) THEN
                  CALL MSG_OUT('MSG','Item Not Available',STATUS)
                  STATUS = USER__001
              ENDIF
          ELSE IF (ITEM .EQ. 'V') THEN

*  We need V and I if item is V

              IF (IPTR .EQ. 0 .OR. VPTR .EQ. 0) THEN
                  CALL MSG_OUT('MSG','Item Not Available',STATUS)
                  STATUS = USER__001
              ENDIF
          ELSE IF (ITEM .EQ. 'Q') THEN

*  We need Q and I if item is Q

              IF (IPTR .EQ. 0 .OR. QPTR .EQ. 0) THEN
                  CALL MSG_OUT('MSG','Item Not Available',STATUS)
                  STATUS = USER__001
              ENDIF
          ELSE IF (ITEM .EQ. 'U') THEN

*  We need U and I if item is U

              IF (IPTR .EQ. 0 .OR. UPTR .EQ. 0) THEN
                  CALL MSG_OUT('MSG','Item Not Available',STATUS)
                  STATUS = USER__001
              ENDIF
          ELSE IF (ITEM .EQ. 'P' .OR. ITEM .EQ. 'THETA') THEN

*  We need I, Q and U if item is P or THETA

              IF (IPTR .EQ. 0 .OR. QPTR .EQ. 0
     :               .OR. UPTR .EQ. 0) THEN
                  CALL MSG_OUT('MSG','Item Not Available',STATUS)
                  STATUS = USER__001
              ENDIF
          ENDIF

*  Bin Size and Autoscaling?

          CALL PAR_GET0D('BIN'//C,BINSIZE,STATUS)
          CALL PAR_GET0L('AUTO'//C,AUTO,STATUS)

*  Bin data, convert the item to required form for plots, and get scaling limits

*  I, Flux or Magnitude

          IF (ITEM.EQ.'I'.OR.ITEM.EQ.'FLUX'.OR.ITEM.EQ.'MAG') THEN
              CALL TSP_PHSBIN(SIZE,%VAL(XXPTR),%VAL(XDPTR),
     :          %VAL(IPTR),%VAL(YRPTR),%VAL(YEPTR),BINSIZE,
     :          %VAL(TIPTR),%VAL(TDPTR),PHSTART,PHEND,NEWSIZE,STATUS)
              SIZE = NEWSIZE
              IF (ITEM .EQ. 'FLUX') THEN
                CALL TSP_PHSFLUX(SIZE,%VAL(YRPTR),%VAL(YEPTR),STATUS)
              ELSE IF (ITEM .EQ. 'MAG') THEN
                CALL TSP_PHSMAG(SIZE,%VAL(YRPTR),%VAL(YEPTR),
     :             %VAL(WPTR),CHAN,STATUS)
              ENDIF
              CALL TSP_PHSSCALE(SIZE,%VAL(XDPTR),%VAL(YRPTR),
     :             %VAL(YEPTR),IMIN,IMAX,STATUS)

*  Stokes Q, U OR V (%)

          ELSE IF (ITEM.EQ.'Q'.OR.ITEM.EQ.'U'.OR.ITEM.EQ.'V') THEN
              IF (ITEM .EQ. 'Q') STPTR=QPTR
              IF (ITEM .EQ. 'U') STPTR=UPTR
              IF (ITEM .EQ. 'V') STPTR=VPTR
              CALL TSP_PHSBIN(SIZE,%VAL(XXPTR),%VAL(XDPTR),
     :          %VAL(IPTR),%VAL(YRPTR),%VAL(YEPTR),BINSIZE,
     :          %VAL(TIPTR),%VAL(TDPTR),PHSTART,PHEND,NEWSIZE,STATUS)
              CALL TSP_PHSBIN(SIZE,%VAL(XXPTR),%VAL(XDPTR),
     :          %VAL(STPTR),%VAL(SRPTR),%VAL(SEPTR),BINSIZE,
     :          %VAL(TIPTR),%VAL(TDPTR),PHSTART,PHEND,NEWSIZE,STATUS)
              SIZE = NEWSIZE
              CALL TSP_PHSSTOKES(SIZE,%VAL(YRPTR),%VAL(YEPTR),
     :             %VAL(SRPTR),%VAL(SEPTR),STATUS)
              CALL TSP_PHSSCALE(SIZE,%VAL(XDPTR),%VAL(YRPTR),
     :             %VAL(YEPTR),IMIN,IMAX,STATUS)

*  Polarization (%)

          ELSE IF (ITEM .EQ. 'P') THEN
              CALL TSP_PHSBIN(SIZE,%VAL(XXPTR),%VAL(XDPTR),
     :          %VAL(IPTR),%VAL(YRPTR),%VAL(YEPTR),BINSIZE,
     :          %VAL(TIPTR),%VAL(TDPTR),PHSTART,PHEND,NEWSIZE,STATUS)
              CALL TSP_PHSBIN(SIZE,%VAL(XXPTR),%VAL(XDPTR),
     :          %VAL(QPTR),%VAL(SRPTR),%VAL(SEPTR),BINSIZE,
     :          %VAL(TIPTR),%VAL(TDPTR),PHSTART,PHEND,NEWSIZE,STATUS)
              CALL TSP_PHSBIN(SIZE,%VAL(XXPTR),%VAL(XDPTR),
     :          %VAL(UPTR),%VAL(S2RPTR),%VAL(S2EPTR),BINSIZE,
     :          %VAL(TIPTR),%VAL(TDPTR),PHSTART,PHEND,NEWSIZE,STATUS)
              SIZE = NEWSIZE
              CALL TSP_PHSPOL(SIZE,%VAL(YRPTR),%VAL(YEPTR),%VAL(SRPTR),
     :             %VAL(SEPTR),%VAL(S2RPTR),%VAL(S2EPTR),STATUS)
              CALL TSP_PHSSCALE(SIZE,%VAL(XDPTR),%VAL(YRPTR),
     :             %VAL(YEPTR),IMIN,IMAX,STATUS)

*   Position Angle (degrees)

          ELSE IF (ITEM .EQ. 'THETA') THEN
              CALL TSP_PHSBIN(SIZE,%VAL(XXPTR),%VAL(XDPTR),
     :          %VAL(IPTR),%VAL(YRPTR),%VAL(YEPTR),BINSIZE,
     :          %VAL(TIPTR),%VAL(TDPTR),PHSTART,PHEND,NEWSIZE,STATUS)
              CALL TSP_PHSBIN(SIZE,%VAL(XXPTR),%VAL(XDPTR),
     :          %VAL(QPTR),%VAL(SRPTR),%VAL(SEPTR),BINSIZE,
     :          %VAL(TIPTR),%VAL(TDPTR),PHSTART,PHEND,NEWSIZE,STATUS)
              CALL TSP_PHSBIN(SIZE,%VAL(XXPTR),%VAL(XDPTR),
     :          %VAL(UPTR),%VAL(S2RPTR),%VAL(S2EPTR),BINSIZE,
     :          %VAL(TIPTR),%VAL(TDPTR),PHSTART,PHEND,NEWSIZE,STATUS)
              SIZE = NEWSIZE
              CALL TSP_PHSTHETA(SIZE,%VAL(YRPTR),%VAL(YEPTR),
     :             %VAL(SRPTR),%VAL(SEPTR),%VAL(S2RPTR),
     :             %VAL(S2EPTR),STATUS)
              CALL TSP_PHSSCALE(SIZE,%VAL(XDPTR),%VAL(YRPTR),
     :             %VAL(YEPTR),IMIN,IMAX,STATUS)
          ENDIF

*  Set scaling limits

          IF (.NOT. AUTO) THEN
             CALL PAR_DEF0R('MIN'//C,IMIN,STATUS)
             CALL PAR_DEF0R('MAX'//C,IMAX,STATUS)
             CALL PAR_GET0R('MIN'//C,IMIN,STATUS)
             CALL PAR_GET0R('MAX'//C,IMAX,STATUS)
          ENDIF

*  Set first,last flags

          LAST = (PLOT .EQ. NPLOTS)
          FIRST = (PLOT .EQ. 1)

*  Determine plot position

          TOP = REAL(NPLOTS-PLOT+1)/REAL(NPLOTS)
     :       * (PTOP-PBOTTOM) + PBOTTOM
          BOT = REAL(NPLOTS-PLOT)/REAL(NPLOTS)
     :       * (PTOP-PBOTTOM) + PBOTTOM

*  Set Y axis label

          IF (ITEM .EQ. 'I') THEN
              YLABEL = 'Counts/sec$'
          ELSE IF (ITEM .EQ. 'FLUX') THEN
              YLABEL = 'Flux mJy$'
          ELSE IF (ITEM .EQ. 'MAG') THEN
              YLABEL = 'Magnitude$'
          ELSE IF (ITEM .EQ. 'V') THEN
              YLABEL = 'V/I (%)$'
          ELSE IF (ITEM .EQ. 'Q') THEN
              YLABEL = 'Q/I (%)$'
          ELSE IF (ITEM .EQ. 'U') THEN
              YLABEL = 'U/I (%)$'
          ELSE IF (ITEM .EQ. 'P') THEN
              YLABEL = 'P (%)$'
          ELSE IF (ITEM .EQ. 'THETA') THEN
              YLABEL = 'Theta (degrees)$'
          ENDIF

*  Get plot label

          CALL PAR_GET0C('PLABEL'//C,PLABEL,STATUS)

*  Do the plot

          XLABEL = 'Phase'
          IF (STATUS .EQ. SAI__OK) THEN
            CALL TSP_PHSLOT(SIZE,FSIZE,%VAL(XXPTR),%VAL(XDPTR),
     :        %VAL(YRPTR),%VAL(YEPTR),%VAL(T1PTR),%VAL(T2PTR),
     :        XLABEL,YLABEL,TOP,BOT,FIRST,PHSTART,PHEND,
     :        LAST,ITEM,IMIN,IMAX,BINSIZE,PLABEL,FILE,STATUS)
          ENDIF
          CALL TSP_PHSUNMAPITEM(LOC,STATUS)
          PLOT = PLOT+1
        ENDDO
      ENDIF

100   CONTINUE

*  Tidy up

      STAT = SAI__OK
      CALL TSP_UNMAP(XLOC,STAT)
      CALL TSP_UNMAP(WLOC,STAT)
      CALL TSP_UNMAP(YRLOC,STAT)
      CALL TSP_UNMAP(YELOC,STAT)
      CALL TSP_UNMAP(SRLOC,STAT)
      CALL TSP_UNMAP(SELOC,STAT)
      CALL TSP_UNMAP(S2RLOC,STAT)
      CALL TSP_UNMAP(S2ELOC,STAT)
      CALL TSP_UNMAP(XXLOC,STAT)
      CALL TSP_UNMAP(XDLOC,STAT)
      CALL TSP_UNMAP(TILOC,STAT)
      CALL TSP_UNMAP(TDLOC,STAT)
      CALL TSP_UNMAP(T1LOC,STAT)
      CALL TSP_UNMAP(T2LOC,STAT)
      CALL DAT_ANNUL(LOC,STAT)
      CALL SGS_ANNUL(ZONE,STAT)
      END








      SUBROUTINE TSP_PHSBIN(SIZE,X,XB,Y,YB,YE,BINSIZE,NP,DS,
     :    PHSTART,PHEND,NEWSIZE,STATUS)
*+
*
*  T S P _ P H S B I N
*
*  PHASEPLOT command - Binning
*
*  Bin data with a specified binsize in the X axis. The X data contains
*  phases normalized to the range 0 to 1. The binned data can cover a phase
*  range of -1 to 2, so any original point could appear up to 3 times in the
*  binned dataset.
*
*  (>)  SIZE     (Integer)   The number of data points before binning
*  (>)  X        (Double)    The X array before binning
*  (<)  XB       (Double)    The X array after binning
*  (>)  Y        (Real)      The Y array before binning
*  (<)  YB       (Real)      The Y array after binning
*  (<)  YE       (Real)      The array of Y errors (from bin statistics)
*  (>)  BINSIZE  (Double)    The size of the bins in X
*  (W)  NP       (Integer)   Worksapce integer array
*  (W)  DS       (Double)    Workspace Double precision array
*  (>)  PHSTART  (Double)    Starting phase for plot
*  (>)  PHEND    (Double)    End phase for plot
*  (<)  NEWSIZE  (Integer)   Number of data points after binning
*  (!)  STATUS   (Integer)   Status value
*
*  Jeremy Bailey    28/2/1988
*
*  Modified:
*      11/12/1991
*
*+


      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'USER_ERR'

*  Parameters
      INTEGER SIZE,NEWSIZE
      REAL Y(SIZE), YB(*), YE(*)
      INTEGER STATUS
      DOUBLE PRECISION BINSIZE
      DOUBLE PRECISION X(*),XB(*)
      INTEGER NP(*)
      DOUBLE PRECISION DS(*)
      DOUBLE PRECISION PHSTART,PHEND

*  Local variables
      INTEGER I
      DOUBLE PRECISION D,DD
      INTEGER BIN1,BIN2,BIN3,BIN
      REAL PHASE
      REAL YX


      IF (STATUS .EQ. SAI__OK) THEN

*  determine size of binned array and check that it is not too large

         IF (BINSIZE .GT. 0.0) THEN
             NEWSIZE = (PHEND-PHSTART)/BINSIZE + 1
         ELSE
             NEWSIZE = SIZE
             BIN = 1
         ENDIF

*  Complain if there are too many bins for the data array sizes

         IF (NEWSIZE .GT. SIZE) THEN
             CALL MSG_OUT('MSG','Too Many Bins',STATUS)
             STATUS = USER__001
             RETURN
         ENDIF

*  Zero each bin

         DO I=1,NEWSIZE
             YB(I) = 0.0
             DS(I) = 0.0
             NP(I) = 0
             XB(I) = 0.0
         ENDDO

*  Loop over points of unbinned data, adding into appropriate bin
*  We have to take into account the fact that the same data point could
*  appear in up to three bins, since the phase range can go from -1 to
*  2.

         DO I=1,SIZE
             IF (BINSIZE .GT. 0.0 .AND. Y(I) .NE. VAL__BADR) THEN

*  Determine three possible bin numbers

                 BIN1 = (X(I)-PHSTART)/BINSIZE + 1
                 BIN2 = (X(I)-PHSTART-1.0)/BINSIZE + 1
                 BIN3 = (X(I)-PHSTART+1.0)/BINSIZE + 1

*  If first bin number is in range to be plotted add data into bin

                 IF (BIN1 .GT. 0 .AND. BIN1 .LE. NEWSIZE) THEN
                     YB(BIN1) = YB(BIN1) + Y(I)
                     DS(BIN1) = DS(BIN1) + Y(I)*Y(I)
                     NP(BIN1) = NP(BIN1) + 1
                     XB(BIN1) = XB(BIN1) + X(I)
                 ENDIF

*  If second bin number is in range to be plotted add data into bin

                 IF (BIN2 .GT. 0 .AND. BIN2 .LE. NEWSIZE) THEN
                     YB(BIN2) = YB(BIN2) + Y(I)
                     DS(BIN2) = DS(BIN2) + Y(I)*Y(I)
                     NP(BIN2) = NP(BIN2) + 1
                     XB(BIN2) = XB(BIN2) + X(I)-1.0
                 ENDIF

*  If third bin number is in range to be plotted add data into bin

                 IF (BIN3 .GT. 0 .AND. BIN3 .LE. NEWSIZE) THEN
                     YB(BIN3) = YB(BIN3) + Y(I)
                     DS(BIN3) = DS(BIN3) + Y(I)*Y(I)
                     NP(BIN3) = NP(BIN3) + 1
                     XB(BIN3) = XB(BIN3) + X(I)+1.0
                 ENDIF
             ELSE IF (Y(I) .NE. VAL__BADR) THEN

*  Handle the case where we are not binning, but plotting every point
*  Again we may have to plot each point up to three times.

*  Phase range 0 to 1

                 PHASE = X(I)
                 IF (PHASE .GT. PHSTART .AND. PHASE .LT. PHEND) THEN
                     YB(BIN) = Y(I)
                     NP(BIN) = 1
                     XB(BIN) = PHASE
                     BIN=BIN+1
                 ENDIF

*  Phase range 1 to 2

                 PHASE = PHASE+1.0
                 IF (PHASE .GT. PHSTART .AND. PHASE .LT. PHEND) THEN
                     YB(BIN) = Y(I)
                     NP(BIN) = 1
                     XB(BIN) = PHASE
                     BIN=BIN+1
                 ENDIF

*  Phase range -1 to 0

                 PHASE = PHASE-2.0
                 IF (PHASE .GT. PHSTART .AND. PHASE .LT. PHEND) THEN
                     YB(BIN) = Y(I)
                     NP(BIN) = 1
                     XB(BIN) = PHASE
                     BIN=BIN+1
                 ENDIF
             ENDIF
         ENDDO
         IF (BINSIZE .LT. 0.0) THEN
             NEWSIZE = BIN-1
         ENDIF

*  Calculate X,Y and errors for bin

         DO I=1,NEWSIZE
             IF (NP(I) .EQ. 0) THEN

*  If no data in bin set value to bad

                 YB(I)=VAL__BADR
                 YE(I)=0.0
                 XB(I)=0.0
             ELSE IF (NP(I) .EQ. 1) THEN

*  If only one point in bin set error to zero

                 YE(I)=0.0
             ELSE IF (NP(I) .GE. 2) THEN

*  Otherwise calculate mean and standard error of data in bin

                 YB(I) = YB(I)/NP(I)
                 XB(I) = XB(I)/NP(I)
                 D = YB(I)
                 DD = DS(I)-D*D*NP(I)
                 IF (DD .GT. 0.0) THEN
                     YE(I) = REAL(DSQRT(DD/(NP(I)*(NP(I)-1))))
                 ELSE
                     YE(I) = 0
                 ENDIF
             ENDIF
         ENDDO

      ENDIF
      END





