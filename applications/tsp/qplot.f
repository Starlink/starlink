C+
      SUBROUTINE QPLOT(STATUS)
C
C            Q P L O T
C
C     Command name:
C        QPLOT
C
C     Function:
C        Quick plot of time series data.
C
C     Description:
C        QPLOT provides a quick means of plotting one item from a time
C        series data set, but without the many options provided by TSPLOT.
C        Plotting is done with the NCAR/SGS/GKS graphics system.
C
C     Parameters:
C    (1) INPUT      (TSP, 2D)  The input time series dataset.
C    (2) DEVICE     (Device)   The Graphics device (any valid GKS device).
C    (3) CHAN       (Integer)  Channel to plot.
C    (4) ITEM       (Char)     Item to plot (I,Q,U,V)
C        LABEL      (Char)     Label for Diagram.
C        AUTOn      (Logical)  If True plot is autoscaled.
C    (C) MIN        (Real)     Minimum level for scaling.
C    (C) MAX        (Real)     Maximum level for scaling.
C        WHOLE      (Logical)  If TRUE, All time points are used.
C    (C) XSTART     (Double)   First time value (MJD) to use.
C    (C) XEND       (Double)   Last time value (MJD) to use.
C    (H) ERRORS     (Logical)  If True (default), Error bars are plotted.
C    (H) LINE       (Logical)  If True, the points are joined by a
C                               continuous line. (Default False).
C    (H) PEN        (Integer)  SGS Pen number to plot in. (Default 1).
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 28/2/1988
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
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS
      INTEGER IPTR,QPTR,UPTR,VPTR          ! Pointers to Stokes arrays
      INTEGER IEPTR,QEPTR,UEPTR,VEPTR      ! Pointers to variances
      INTEGER XPTR                         ! Pointer to X (time) array
      INTEGER WPTR                         ! Pointer to wavelength axis
      INTEGER STRT,FIN                     ! Start and finish channels
      INTEGER SIZE                         ! Original size of data in X
      INTEGER FSIZE                        ! Size after X limits selection
      LOGICAL AUTO                         ! Autoscaling flag
      REAL IMIN,IMAX                       ! Y scaling limits
      CHARACTER*(DAT__SZLOC) LOC           ! Top level locator
      CHARACTER*5 ITEM                     ! Current item to be plotted
      INTEGER CHAN                         ! Current channel to be plotted
      CHARACTER*20 YLABEL                  ! Y label for current plot
      CHARACTER*40 XLABEL                  ! X axis label
      INTEGER NCHANS                       ! Number of channels in data
      INTEGER ZONE                         ! SGS zone for plot
      CHARACTER*(DAT__SZLOC) XLOC,WLOC     ! Axis locators
      INTEGER ACTDIM,DIMS(7)

*   Temporary array pointers and locators

      CHARACTER*(DAT__SZLOC) XXLOC,T1LOC,T2LOC
      INTEGER XXPTR,T1PTR,T2PTR,YRPTR

*  Get the data

      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)

      IF (STATUS .EQ. SAI__OK) THEN

*  Initialize for plotting

        CALL TSP_PHSINITAG(ZONE,STATUS)

*  Get X Axis data

        CALL TSP_MAP_TIME(LOC,'READ',XPTR,XLOC,STATUS)

*  Get wavelength axis data

        CALL TSP_MAP_LAMBDA(LOC,'READ',WPTR,WLOC,STATUS)

*  Get size of data

        CALL TSP_SIZE(LOC,3,DIMS,ACTDIM,STATUS)
        STRT=1
        FIN=DIMS(2)
        NCHANS=DIMS(1)
        SIZE = FIN-STRT+1

*  Get temporary arrays

        CALL TSP_TEMP(SIZE,'_REAL',T1PTR,T1LOC,STATUS)
        CALL TSP_TEMP(SIZE,'_REAL',T2PTR,T2LOC,STATUS)
        CALL TSP_TEMP(SIZE,'_DOUBLE',XXPTR,XXLOC,STATUS)

*   Find X plotting limits

        CALL TSP_QPLTXLIMITS(%VAL(XPTR),STRT,FIN,%VAL(XXPTR),STATUS)
        SIZE = FIN-STRT+1
        FSIZE = SIZE

*  Get channel number for plot

          CALL PAR_GET0I('CHAN',CHAN,STATUS)

*  Check it is valid

          IF ((CHAN .LE. 0) .OR. (CHAN .GT. NCHANS)) THEN
             CALL MSG_OUT('MSG','Invalid Channel',STATUS)
             CHAN=1
          ENDIF

*   Get Item for plot

          CALL PAR_GET0C('ITEM',ITEM,STATUS)
          CALL CHR_UCASE(ITEM)

*   Map required data arrays

          CALL TSP_QPLTGETITEM(LOC,CHAN,STRT,FIN,IPTR,QPTR,UPTR,VPTR,
     :       IEPTR,QEPTR,UEPTR,VEPTR,STATUS)

*   Check that all the necessary components are available

          IF (ITEM .EQ. 'I') THEN

*  Intensity

              IF (IPTR .EQ. 0) THEN
                  CALL MSG_OUT('MSG','Item Not Available',STATUS)
                  STATUS = USER__001
              ENDIF
          ELSE IF (ITEM .EQ. 'V') THEN

*  Stokes V

              IF (VPTR .EQ. 0) THEN
                  CALL MSG_OUT('MSG','Item Not Available',STATUS)
                  STATUS = USER__001
              ENDIF
          ELSE IF (ITEM .EQ. 'Q') THEN

*  Stokes Q

              IF (QPTR .EQ. 0) THEN
                  CALL MSG_OUT('MSG','Item Not Available',STATUS)
                  STATUS = USER__001
              ENDIF
          ELSE IF (ITEM .EQ. 'U') THEN

*  Stokes U

              IF (UPTR .EQ. 0) THEN
                  CALL MSG_OUT('MSG','Item Not Available',STATUS)
                  STATUS = USER__001
              ENDIF
          ENDIF

*  Bin Size and Autoscaling?

          CALL PAR_GET0L('AUTO',AUTO,STATUS)

*  Bin data, convert the item to required form for plots, and get scaling limits

          IF (ITEM .EQ. 'I') THEN

*  Take square root of variance to get sigma

              IF (IEPTR .NE. 0) THEN
                  CALL TSP_QPLTSQ(SIZE,%VAL(IEPTR),%VAL(T2PTR))
              ELSE

*  If there is no error array set the temp array to zero

                  CALL TSP_QPLTZERO(SIZE,%VAL(T2PTR))
              ENDIF

*  Find the scaling limits

              CALL TSP_PHSSCALE(SIZE,%VAL(XXPTR),%VAL(IPTR),
     :             %VAL(T2PTR),IMIN,IMAX,STATUS)
              YRPTR = IPTR

*  Stokes Q

          ELSE IF (ITEM .EQ. 'Q') THEN

*  Take square root of variance to get sigma

              IF (QEPTR .NE. 0) THEN
                  CALL TSP_QPLTSQ(SIZE,%VAL(QEPTR),%VAL(T2PTR))
              ELSE

*  If there is no error array set the temp array to zero

                  CALL TSP_QPLTZERO(SIZE,%VAL(T2PTR))
              ENDIF

*  Find the scaling limits

              CALL TSP_PHSSCALE(SIZE,%VAL(XXPTR),%VAL(QPTR),
     :             %VAL(T2PTR),IMIN,IMAX,STATUS)
              YRPTR = QPTR

*  Stokes U

          ELSE IF (ITEM .EQ. 'U') THEN
              IF (UEPTR .NE. 0) THEN

*  Take square root of variance to get sigma

                  CALL TSP_QPLTSQ(SIZE,%VAL(UEPTR),%VAL(T2PTR))
              ELSE

*  If there is no error array set the temp array to zero

                  CALL TSP_QPLTZERO(SIZE,%VAL(T2PTR))
              ENDIF

*  Find the scaling limits

              CALL TSP_PHSSCALE(SIZE,%VAL(XXPTR),%VAL(UPTR),
     :             %VAL(T2PTR),IMIN,IMAX,STATUS)
              YRPTR = UPTR

*  Stokes V

          ELSE IF (ITEM .EQ. 'V') THEN
              IF (VEPTR .NE. 0) THEN

*  Take square root of variance to get sigma

                  CALL TSP_QPLTSQ(SIZE,%VAL(VEPTR),%VAL(T2PTR))
              ELSE

*  If there is no error array set the temp array to zero

                  CALL TSP_QPLTZERO(SIZE,%VAL(T2PTR))
              ENDIF

*  Find the scaling limits

              CALL TSP_PHSSCALE(SIZE,%VAL(XXPTR),%VAL(VPTR),
     :             %VAL(T2PTR),IMIN,IMAX,STATUS)
              YRPTR = VPTR

          ENDIF

*  Set scaling limits using max and min determined above as defaults

          IF (.NOT. AUTO) THEN
             CALL PAR_DEF0R('MIN',IMIN,STATUS)
             CALL PAR_DEF0R('MAX',IMAX,STATUS)
             CALL PAR_GET0R('MIN',IMIN,STATUS)
             CALL PAR_GET0R('MAX',IMAX,STATUS)
          ENDIF

*  Set Y label

          IF (ITEM .EQ. 'I') THEN
              YLABEL = 'Counts/sec'
          ELSE IF (ITEM .EQ. 'V') THEN
              YLABEL = 'V'
          ELSE IF (ITEM .EQ. 'Q') THEN
              YLABEL = 'Q'
          ELSE IF (ITEM .EQ. 'U') THEN
              YLABEL = 'U'
          ENDIF

*  Do the plot

          IF (STATUS .EQ. SAI__OK) THEN
            CALL TSP_QPLTPLOT(SIZE,%VAL(XXPTR),
     :        %VAL(YRPTR),%VAL(T2PTR),%VAL(T1PTR),
     :        XLABEL,YLABEL,IMIN,IMAX,STATUS)
          ENDIF
          CALL TSP_QPLTUNMAPITEM(LOC,STATUS)
      ENDIF

*  Tidy up

      CALL TSP_UNMAP(XXLOC,STATUS)
      CALL TSP_UNMAP(T1LOC,STATUS)
      CALL TSP_UNMAP(T2LOC,STATUS)
      CALL TSP_UNMAP(XLOC,STATUS)
      CALL TSP_UNMAP(WLOC,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)
      CALL SGS_ANNUL(ZONE,STATUS)
      END


      SUBROUTINE TSP_QPLTSQ(SIZE,X,Y)
*+
*
*  T S P _ Q P L T S Q
*
*   QPLOT command
*
*  Take the square root of an input array - This is used to convert
*  variances to standard deviations for plotting as error bars
*
*   (>)  SIZE    (Integer)           Size of the arrays
*   (>)  X       (Real array(SIZE))  Input array containing variance
*   (<)  Y       (Real array(SIZE))  Output array containg standard
*                                      deviation
*
*  Jeremy Bailey    28/2/1988
*
*  Modified:
*      11/12/1991  -  Handle bad values
*
*+
      IMPLICIT NONE
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE
      REAL X(SIZE),Y(SIZE)

*  Local variables
      INTEGER I

*  Loop over data points replacing all good values with their square root
      DO I=1,SIZE
          IF (X(I) .GE. 0. .AND. X(I) .NE. VAL__BADR) THEN
              Y(I)=SQRT(X(I))
          ELSE
              Y(I)=VAL__BADR
          ENDIF
      ENDDO
      END


      SUBROUTINE TSP_QPLTZERO(SIZE,Y)
*+
*
*  T S P _ Q P L T Z E R O
*
*   QPLOT command
*
*  Fill an input array with zeros - This is used to set up a
*  zero error array when there are no errors in the data.
*
*   (>)  SIZE    (Integer)           Size of the arrays
*   (<)  Y       (Real array(SIZE))  Output array to be filled with zeros
*
*  Jeremy Bailey    28/2/1988
*
*  Modified:
*      11/12/1991
*
*+
      IMPLICIT NONE

*  Parameters
      INTEGER SIZE
      REAL Y(SIZE)

*  Local variables
      INTEGER I

      DO I=1,SIZE
          Y(I)=0.0
      ENDDO
      END







      SUBROUTINE TSP_QPLTPLOT(SIZE,X,Y,YE,XR,
     :  XLABEL,YLABEL,IMIN,IMAX,STATUS)
*+
*
*   T S P _ Q P L T P L O T
*
*   QPLOT command
*
*   Subroutine to do the plot  -  The Y data array is plotted against
*   the X array (an array of MJD times). Error bars are plotted if the
*   ADAM parameter ERRORS is TRUE.
*
*   Plotting should already have been initialized by a call
*   to TSP_PHSINITAG (in the PHASEPLOT program)
*
*  (>)  SIZE    (Integer) -  The number of data points to plot
*  (>)  X       (Double)  -  The X data array
*  (>)  Y       (Real)    -  The Y data array
*  (>)  YE      (Real)    -  The Y error array
*  (W)  XR      (Real)    -  Workspace real array
*  (>)  XLABEL  (Char)    -  X axis label
*  (>)  YLABEL  (Char)    -  Y axis label
*  (>)  IMIN    (Real)    -  Minimum Y value for plot scaling
*  (>)  IMAX    (Real)    -  Maximum Y value for plot scaling
*  (!)  STATUS  (Integer) -  Status value
*
*  Jeremy Bailey     28/2/1988
*
*  Modified:
*      11/12/1991  -  Handle bad values
*
*+

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE
      REAL Y(SIZE),YE(SIZE)
      DOUBLE PRECISION X(SIZE)
      REAL XR(SIZE)
      REAL IMIN,IMAX
      INTEGER STATUS
      CHARACTER*(*) YLABEL
      CHARACTER*40 XLABEL

*  LOCAL VARIABLES
      INTEGER I
      CHARACTER*80 TSTRING

*  MJD zero point
      INTEGER JDZERO

*  Data in autograph coordinates
      REAL GX,GY,GY1,GY2

*  Coordinate conversion functions
      REAL SNX_AGUGX,SNX_AGUGY
      REAL ZEROPT,ZEROT
      CHARACTER*40 LABEL
      CHARACTER*20 BUFF

*  Line mode flag
      LOGICAL LINE

*  Error bar flag
      LOGICAL ERRORS

*  SGS pen number
      INTEGER PEN
      INTEGER IZ,J,IZA
      REAL AX(2),AY(2)
      INTEGER L
      INTEGER CHR_LEN


      IF (STATUS .EQ. SAI__OK) THEN


*  Get the plot label

             CALL PAR_GET0C('LABEL',LABEL,STATUS)

*  Get LINE parameter

             CALL PAR_GET0L('LINE',LINE,STATUS)

*  Get ERRORS parameter

             CALL PAR_GET0L('ERRORS',ERRORS,STATUS)

*  Get PEN parameter

             CALL PAR_GET0I('PEN',PEN,STATUS)

*  Set up X array for plot
             DO I=1,SIZE
                 XR(I) = REAL(X(I) - DBLE(INT(X(1))))
             ENDDO
             JDZERO = INT(X(1))

*  make X label including JD zero point
             WRITE(BUFF,'('' - '',I5,''$'')') JDZERO
             L = CHR_LEN(XLABEL)
             XLABEL(L+1:L+10) = BUFF
             ZEROT = XR(1)

*  Set up plot position

         CALL AGSETF('GRID/TOP.',0.90)
         CALL AGSETF('GRID/BOTTOM.',0.15)

*  X-axis label

         CALL AGSETC('LABEL/NAME.','B')
         CALL AGSETI('LINE/NUMBER.',-100)
         CALL AGSETC('LINE/TEXT.',XLABEL)
         CALL AGSETF('LINE/CHAR.',0.030)
         CALL AGSETF('B/WI.',0.030)
         CALL AGSETF('B/MA/CO.',2.0)
         CALL AGSETF('AXIS/BOTTOM/NUMERIC/TYPE.',3.0)

*  Top label

         CALL AGSETC('LABEL/NAME.','T')
         CALL AGSETF('LABEL/BASEPOINT/X.',0.05)
         CALL AGSETF('LABEL/CENTERING.',-1.0)
         CALL AGSETI('LINE/NUMBER.',100)
         TSTRING = LABEL//'$'
         CALL AGSETC('LINE/TEXT.',TSTRING)
         CALL AGSETF('LINE/CHAR.',0.040)
         CALL AGSETF('TOP/MAJOR/CO.',2.0)
         CALL AGSETF('AXIS/TOP/NUMERIC/TYPE.',0.0)

*  Y-axis label

         CALL AGSETC('LABEL/NAME.','L')
         CALL AGSETI('LINE/NUMBER.',100)
         CALL AGSETF('LINE/CHAR.',0.030)
         TSTRING = YLABEL//'$'
         CALL AGSETC('LINE/TEXT.',TSTRING)

*  Character size and spacing

         CALL AGSETF('L/WI.',0.040)
         CALL AGSETF('L/MA/CO.',2.0)
         CALL AGSETF('RIGHT/MA/CO.',2.0)
         CALL AGSETF('X/NICE.',0.0)
         CALL AGSETF('Y/NICE.',0.0)

*  Plot scaling levels

         CALL AGSETF('Y/MIN.',IMIN)
         CALL AGSETF('Y/MAX.',IMAX)

*  Plot the data


*  Draw axes

         CALL AGSTUP(XR,1,1,SIZE,1,Y,1,1,SIZE,1)
         CALL AGBACK
         CALL SGS_ICURZ(IZA)

*  Set SGS Zone to correspond to Autograph User Coordinates

         CALL SNX_AGCS

*  Select Window

         J = SAI__OK
         CALL SGS_ZONE(0.0,1.0,0.0,1.0,IZ,J)
         CALL SGS_SW(0.0,1.0,0.0,1.0,J)

*  Set pen number

         CALL SGS_SPEN(PEN)

*  Loop over points

         DO I=1,SIZE
          IF (Y(I) .NE. VAL__BADR) THEN

*  Calculate poition opf point

            GX=SNX_AGUGX(XR(I))
            GY=SNX_AGUGY(Y(I))
            IF (ERRORS) THEN

*  Calculate ends of error bars

                GY1=SNX_AGUGY(Y(I)+YE(I))
                GY2=SNX_AGUGY(Y(I)-YE(I))

*  Draw error bar

                CALL SGS_LINE(GX-.0030,GY,GX+0.0030,GY)
                CALL SGS_LINE(GX,GY1,GX,GY2)
            ENDIF

*  Draw polygon to represent point

            CALL SGS_BPOLY(GX+.002,GY)
            CALL SGS_APOLY(GX+.0014,GY+.0020)
            CALL SGS_APOLY(GX,GY+.003)
            CALL SGS_APOLY(GX-.0014,GY+.0020)
            CALL SGS_APOLY(GX-.002,GY)
            CALL SGS_APOLY(GX-.0014,GY-.0020)
            CALL SGS_APOLY(GX,GY-.003)
            CALL SGS_APOLY(GX+.0014,GY-.0020)
            CALL SGS_APOLY(GX+.002,GY)

*  Output the polygon

            CALL SGS_OPOLY
          ENDIF
         ENDDO

*  Draw line joining points if the LINE parameter is set

         IF (LINE) THEN
            GX=SNX_AGUGX(XR(1))
            GY=SNX_AGUGY(Y(1))

*  Set start of line

            CALL SGS_BPOLY(GX,GY)

*  Loop over points adding them to the polyline

            DO I=2,SIZE
              IF (Y(I) .NE. VAL__BADR) THEN
                GX=SNX_AGUGX(XR(I))
                GY=SNX_AGUGY(Y(I))
                CALL SGS_APOLY(GX,GY)
              ENDIF
            ENDDO

*  Output the line

            CALL SGS_OPOLY
         ENDIF
         CALL SGS_SPEN(1)

*  Flush any remaining output

         CALL SGS_FLUSH
         CALL SGS_SELZ(IZA,J)

      ENDIF
      END

