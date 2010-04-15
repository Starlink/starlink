C+
      SUBROUTINE TSPLOT(STATUS)
C
C            T S P L O T
C
C     Command name:
C        TSPLOT
C
C     Function:
C        Plot time series data.
C
C     Description:
C        TSPLOT plots time series data against time. Up to six items may be
C        plotted. Each item may be a different channel or Stokes parameter etc.
C        The data may be binned (all points in a given time bin averaged).
C        or points plotted individually. Plotting is done with the
C        NCAR/SGS/GKS graphics system.
C
C     Parameters:
C    (1) INPUT      (TSP, 2D)  The input time series dataset.
C    (2) NPLOTS     (Integer)  The number of items to plot (max 6).
C    (3) DEVICE     (Device)   The Graphics device (any valid GKS device).
C        WHOLE      (Logical)  If TRUE, All time points are used.
C    (C) XSTART     (Double)   First time value (MJD) to use.
C    (C) XEND       (Double)   Last time value (MJD) to use.
C        CHANn      (Integer)  Channel for nth plot. This and the following
C                               parameters repeat for
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
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         28/2/1988
C
C-
C
C  History:
C    Nov/1987   Original Version.   JAB/AAO
C    28/2/1988   TSP Monolith version.  JAB/AAO
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'USER_ERR'
      INTEGER STATUS
      INTEGER IPTR,QPTR,UPTR,VPTR          ! Pointers to Stokes arrays
      INTEGER IEPTR,QEPTR,UEPTR,VEPTR      ! Pointers to variances
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
      CHARACTER*64 XLABEL                  ! X axis label
      CHARACTER*64 XUNITS
      CHARACTER*20 PLABEL                  ! Label for current plot
      LOGICAL FIRST,LAST                   ! First,Last plot flags
      INTEGER NCHANS                       ! Number of channels in data
      INTEGER ZONE                         ! SGS zone for plot
      INTEGER DIMS(3)                      ! Dimensions of data
      INTEGER ACTDIM                       ! Actual number of dimensions
      DOUBLE PRECISION BINSIZE             ! Binsize for current plot
      DOUBLE PRECISION PHSTART,PHEND
      LOGICAL FILE
      INTEGER STAT

*   Temporary array pointers and locators

      CHARACTER*(DAT__SZLOC) YRLOC,YELOC,SRLOC,SELOC,S2RLOC,S2ELOC
      CHARACTER*(DAT__SZLOC) XXLOC,XDLOC,TILOC,TDLOC,T1LOC,T2LOC
      CHARACTER*(DAT__SZLOC) XLOC,WLOC,ZLOC
      INTEGER XXPTR,XDPTR,TIPTR,TDPTR,T1PTR,T2PTR
      INTEGER YRPTR,YEPTR,SRPTR,SEPTR,S2RPTR,S2EPTR,ZPTR

*  Get the data

      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)

*  Get the number of plots

      CALL PAR_GET0I('NPLOTS',NPLOTS,STATUS)

      IF (STATUS .EQ. SAI__OK) THEN

*  Initialize for plotting

        CALL TSP_PHSINITAG(ZONE,STATUS)
        IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP(' ','Error initializing graphics',STATUS)
            GOTO 999
        ENDIF

*  Get file parameter

        CALL PAR_GET0L('FILE',FILE,STATUS)

*  Map the time axis

        CALL TSP_MAP_TIME(LOC,'READ',XPTR,XLOC,STATUS)

*  Map the wavelength axis

        CALL TSP_MAP_LAMBDA(LOC,'READ',WPTR,WLOC,STATUS)

*  Read label and units of time axis

        CALL TSP_RLU_TIME(LOC,XLABEL,XUNITS,STATUS)

*  Get size of data

        CALL TSP_SIZE(LOC,3,DIMS,ACTDIM,STATUS)
        STRT=1
        FIN=DIMS(2)
        NCHANS=DIMS(1)
        SIZE = FIN-STRT+1

*  Get temporary arrays

        CALL TSP_TEMP(SIZE,'_REAL',YRPTR,YRLOC,STATUS)
        CALL TSP_TEMP(SIZE,'_REAL',YEPTR,YELOC,STATUS)
        CALL TSP_TEMP(SIZE,'_REAL',SRPTR,SRLOC,STATUS)
        CALL TSP_TEMP(SIZE,'_REAL',SEPTR,SELOC,STATUS)
        CALL TSP_TEMP(SIZE,'_REAL',S2RPTR,S2RLOC,STATUS)
        CALL TSP_TEMP(SIZE,'_REAL',S2EPTR,S2ELOC,STATUS)
        CALL TSP_TEMP(SIZE,'_REAL',T1PTR,T1LOC,STATUS)
        CALL TSP_TEMP(SIZE,'_REAL',T2PTR,T2LOC,STATUS)
        CALL TSP_TEMP(SIZE,'_INTEGER',TIPTR,TILOC,STATUS)
        CALL TSP_TEMP(SIZE,'_DOUBLE',TDPTR,TDLOC,STATUS)
        CALL TSP_TEMP(SIZE,'_DOUBLE',XXPTR,XXLOC,STATUS)
        CALL TSP_TEMP(SIZE,'_DOUBLE',XDPTR,XDLOC,STATUS)
        CALL TSP_TEMP(SIZE,'_REAL',ZPTR,ZLOC,STATUS)
        IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP(' ','Error getting workspace ^STATUS',STATUS)
            GOTO 999
        ENDIF
        CALL TSP_EPZERO(SIZE,%VAL(ZPTR))

*   Find X plotting limits

        CALL TSP_QPLTXLIMITS(%VAL(XPTR),STRT,FIN,%VAL(XXPTR),STATUS)
        SIZE = FIN-STRT+1
        FSIZE = SIZE

*  Loop over multiple plots

        PLOT = 1
        DO WHILE (PLOT .LE. NPLOTS)
          SIZE = FSIZE

*  Get channel number for plot

          C = CHAR(PLOT+ICHAR('0'))
          CALL PAR_GET0I('CHAN'//C,CHAN,STATUS)

*  Check that it is valid

          IF (STATUS .NE. SAI__OK) GOTO 999
          IF ((CHAN .LE. 0) .OR. (CHAN .GT. NCHANS)) THEN
             CALL MSG_OUT('MSG','Invalid Channel',STATUS)
             CHAN=1
             GOTO 999
          ENDIF

*   Get Item for plot

          CALL PAR_GET0C('ITEM'//C,ITEM,STATUS)
          CALL CHR_UCASE(ITEM)

*   Map required data arrays

          CALL TSP_QPLTGETITEM(LOC,CHAN,STRT,FIN,IPTR,QPTR,UPTR,
     :           VPTR,IEPTR,QEPTR,UEPTR,VEPTR,STATUS)
          IF (STATUS .NE. SAI__OK) GOTO 999

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

*  We need I, Q and U if item is P or THETA

          ELSE IF (ITEM .EQ. 'P' .OR. ITEM .EQ. 'THETA') THEN
              IF (IPTR .EQ. 0 .OR. QPTR .EQ. 0
     :               .OR. UPTR .EQ. 0) THEN
                  CALL MSG_OUT('MSG','Item Not Available',STATUS)
                  STATUS = USER__001
              ENDIF
          ENDIF

          IF (IEPTR .EQ. 0) IEPTR = ZPTR
          IF (QEPTR .EQ. 0) QEPTR = ZPTR
          IF (UEPTR .EQ. 0) UEPTR = ZPTR
          IF (VEPTR .EQ. 0) VEPTR = ZPTR


*  Bin Size and Autoscaling?

          CALL PAR_GET0D('BIN'//C,BINSIZE,STATUS)
          CALL PAR_GET0L('AUTO'//C,AUTO,STATUS)
          IF (STATUS .EQ. SAI__OK) THEN

*  Bin data, convert the item to required form for plots, and get scaling limits

*  I, Flux or Magnitude


            IF (ITEM .EQ. 'I' .OR. ITEM .EQ. 'FLUX'
     :          .OR. ITEM .EQ. 'MAG') THEN
              CALL TSP_TSPBIN(SIZE,%VAL(XXPTR),%VAL(XDPTR),
     :             %VAL(IPTR),%VAL(IEPTR),%VAL(YRPTR),%VAL(YEPTR),
     :             BINSIZE,%VAL(TIPTR),%VAL(TDPTR),NEWSIZE,STATUS)
              SIZE = NEWSIZE
              IF (ITEM .EQ. 'FLUX') THEN
                CALL TSP_PHSFLUX(SIZE,%VAL(YRPTR),%VAL(YEPTR),STATUS)
              ELSE IF (ITEM .EQ. 'MAG') THEN
                CALL TSP_PHSMAG(SIZE,%VAL(YRPTR),%VAL(YEPTR),
     :             %VAL(WPTR),CHAN,STATUS)
              ENDIF

*  Stokes Q, U or V (%)

            ELSE IF (ITEM .EQ. 'Q' .OR. ITEM .EQ. 'U'
     :          .OR. ITEM .EQ. 'V') THEN
              IF (ITEM .EQ. 'U') THEN
                 QPTR=UPTR
                 QEPTR=UEPTR
              ELSE IF (ITEM .EQ. 'V') THEN
                 QPTR=VPTR
                 QEPTR=VEPTR
              ENDIF
              CALL TSP_TSPBIN(SIZE,%VAL(XXPTR),%VAL(XDPTR),
     :             %VAL(IPTR),%VAL(IEPTR),%VAL(YRPTR),%VAL(YEPTR),
     :             BINSIZE,%VAL(TIPTR),%VAL(TDPTR),NEWSIZE,STATUS)
              CALL TSP_TSPBIN(SIZE,%VAL(XXPTR),%VAL(XDPTR),
     :             %VAL(QPTR),%VAL(QEPTR),%VAL(SRPTR),%VAL(SEPTR),
     :             BINSIZE,%VAL(TIPTR),%VAL(TDPTR),NEWSIZE,STATUS)
              SIZE = NEWSIZE
              CALL TSP_PHSSTOKES(SIZE,%VAL(YRPTR),%VAL(YEPTR),
     :             %VAL(SRPTR),%VAL(SEPTR),STATUS)

*  Polarization (%) or position angle (degrees)

            ELSE IF (ITEM .EQ. 'P' .OR. ITEM .EQ. 'THETA') THEN

*  Bin the I, Q and U data

              CALL TSP_TSPBIN(SIZE,%VAL(XXPTR),%VAL(XDPTR),
     :             %VAL(IPTR),%VAL(IEPTR),%VAL(YRPTR),%VAL(YEPTR),
     :             BINSIZE,%VAL(TIPTR),%VAL(TDPTR),NEWSIZE,STATUS)
              CALL TSP_TSPBIN(SIZE,%VAL(XXPTR),%VAL(XDPTR),
     :             %VAL(QPTR),%VAL(QEPTR),%VAL(SRPTR),%VAL(SEPTR),
     :             BINSIZE,%VAL(TIPTR),%VAL(TDPTR),NEWSIZE,STATUS)
              CALL TSP_TSPBIN(SIZE,%VAL(XXPTR),%VAL(XDPTR),
     :             %VAL(UPTR),%VAL(UEPTR),%VAL(S2RPTR),%VAL(S2EPTR),
     :             BINSIZE,%VAL(TIPTR),%VAL(TDPTR),NEWSIZE,STATUS)
              SIZE = NEWSIZE
              IF (ITEM .EQ. 'P') THEN

*  Calculate P

                 CALL TSP_PHSPOL(SIZE,%VAL(YRPTR),%VAL(YEPTR),
     :             %VAL(SRPTR),%VAL(SEPTR),%VAL(S2RPTR),
     :             %VAL(S2EPTR),STATUS)
              ELSE

*  Calculate Theta

                 CALL TSP_PHSTHETA(SIZE,%VAL(YRPTR),%VAL(YEPTR),
     :             %VAL(SRPTR),%VAL(SEPTR),%VAL(S2RPTR),
     :             %VAL(S2EPTR),STATUS)
              ENDIF
            ENDIF
            CALL TSP_PHSSCALE(SIZE,%VAL(XDPTR),%VAL(YRPTR),
     :             %VAL(YEPTR),IMIN,IMAX,STATUS)
          ELSE
            GOTO 999
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

          TOP = REAL(NPLOTS-PLOT+1)/REAL(NPLOTS) * 0.80 + 0.10
          BOT = REAL(NPLOTS-PLOT)/REAL(NPLOTS) * 0.80 + 0.10

*  Set Y label

          IF (ITEM .EQ. 'I') THEN
              YLABEL = 'Counts/sec'
          ELSE IF (ITEM .EQ. 'FLUX') THEN
              YLABEL = 'Flux mJy'
          ELSE IF (ITEM .EQ. 'MAG') THEN
              YLABEL = 'Magnitude$'
          ELSE IF (ITEM .EQ. 'V') THEN
              YLABEL = 'V/I (%)'
          ELSE IF (ITEM .EQ. 'Q') THEN
              YLABEL = 'Q/I (%)'
          ELSE IF (ITEM .EQ. 'U') THEN
              YLABEL = 'U/I (%)'
          ELSE IF (ITEM .EQ. 'P') THEN
              YLABEL = 'P (%)'
          ELSE IF (ITEM .EQ. 'THETA') THEN
              YLABEL = 'Theta (degrees)'
          ENDIF

*  Get plot label

          CALL PAR_GET0C('PLABEL'//C,PLABEL,STATUS)

*  Do the plot

          IF (STATUS .EQ. SAI__OK) THEN
            CALL TSP_PHSLOT(SIZE,FSIZE,%VAL(XXPTR),%VAL(XDPTR),
     :        %VAL(YRPTR),%VAL(YEPTR),%VAL(T1PTR),%VAL(T2PTR),
     :        XLABEL,YLABEL,TOP,BOT,FIRST,PHSTART,PHEND,
     :        LAST,ITEM,IMIN,IMAX,BINSIZE,PLABEL,FILE,STATUS)
          ENDIF

*  Unmap the data items

          CALL TSP_QPLTUNMAPITEM(LOC,STATUS)
          PLOT = PLOT+1
        ENDDO
      ENDIF

*  Tidy up

999   CONTINUE
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
      CALL TSP_UNMAP(ZLOC,STAT)
      CALL DAT_ANNUL(LOC,STAT)
      CALL SGS_ANNUL(ZONE,STAT)
      END

















      SUBROUTINE TSP_TSPBIN(SIZE,X,XB,Y,YV,YB,YE,BINSIZE,NP,DS,
     :    NEWSIZE,STATUS)
*+
*
*  T S P _ T S P B I N
*
*  TSPLOT command
*
*  Bin data with a specified binsize in the X axis.
*  New X,Y and error arrays are produced from the data by
*  forming the mean of all the values within the bin. The error
*  is the standard deviation of values within the bin.
*  If BINSIZE is set to -1 no binning is performed and
*  the original data is returned
*
*  (>)  SIZE     (Integer)   The number of data points before binning
*  (>)  X        (Double)    The X array before binning
*  (<)  XB       (Double)    The X array after binning
*  (>)  Y        (Real)      The Y array before binning
*  (>)  YV       (Real)      The Y variance before binning
*  (<)  YB       (Real)      The Y array after binning
*  (<)  YE       (Real)      The array of Y errors (from bin statistics)
*  (>)  BINSIZE  (Double)    The size of the bins in X
*  (W)  NP       (Integer)   Worksapce integer array
*  (W)  DS       (Double)    Workspace Double precision array
*  (<)  NEWSIZE  (Integer)   Number of data points after binning
*  (!)  STATUS   (Integer)   Status value
*
*  Jeremy Bailey  28/2/1988
*+

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'USER_ERR'

*  Parameters
      INTEGER SIZE,NEWSIZE
      REAL Y(SIZE), YV(SIZE), YB(SIZE), YE(SIZE)
      INTEGER STATUS
      DOUBLE PRECISION BINSIZE
      DOUBLE PRECISION X(*),XB(*)
      INTEGER NP(*)
      DOUBLE PRECISION DS(*)

*  Local variables
      INTEGER I
      DOUBLE PRECISION D
      INTEGER BIN
      REAL YX


      IF (STATUS .EQ. SAI__OK) THEN

*  determine size of binned array and check that it is not too large

         IF (BINSIZE .GT. 0.0) THEN
             NEWSIZE = (X(SIZE)-X(1))/BINSIZE + 1
         ELSE
             NEWSIZE = SIZE
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

         DO I=1,SIZE

*  Determine the bin number for this point

             IF (BINSIZE .GT. 0.0) THEN
                 BIN = (X(I)-X(1))/BINSIZE + 1
             ELSE
                 BIN = I
                 YE(BIN) = SQRT(YV(I))
             ENDIF

*  Add data into bin (if it is not bad)

             IF (Y(I) .NE. VAL__BADR) THEN
                 YB(BIN) = YB(BIN) + Y(I)
                 DS(BIN) = DS(BIN) + Y(I)*Y(I)
                 NP(BIN) = NP(BIN) + 1
                 XB(BIN) = XB(BIN) + X(I)
             ENDIF
         ENDDO

*  Calculate X,Y and errors for bin

         DO I=1,NEWSIZE
             IF (NP(I) .EQ. 0) THEN

*  No data in bin

                 YB(I)=VAL__BADR
                 YE(I)=0.0
                 XB(I)=VAL__BADD             ! Signals no data in bin
             ELSE IF (NP(I) .EQ. 1) THEN

*  Only one point in bin

                 IF (BINSIZE .GE. 0.0) THEN
                    YE(I)=0.0
                 ENDIF
             ELSE IF (NP(I) .GE. 2) THEN

*  More than one point in bin - calculate mean and standard deviation

                 YB(I) = YB(I)/NP(I)
                 XB(I) = XB(I)/NP(I)
                 D = YB(I)
                 YE(I) = REAL(DSQRT((DS(I)-D*D*NP(I))/
     :                      (NP(I)*(NP(I)-1))))
             ENDIF
         ENDDO

      ENDIF
      END




