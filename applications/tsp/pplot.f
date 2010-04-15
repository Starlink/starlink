C+
      SUBROUTINE PPLOT(STATUS)
C
C            P P L O T
C
C     Command name:
C        PPLOT
C
C     Function:
C        Plot a polarization spectrum as P, Theta
C
C     Description:
C        PPLOT produces a plot of a polarization spectrum. The plot is
C        divided into three panels. The lower panel is the total intensity,
C        the center panel is the percentage polarization, the top panel
C        is the position angle in degrees. The polarization data is binned
C        into variable size bins to give a constant polarization error per
C        bin. Plotting is done with the NCAR/SGS/GKS graphics system.
C
C     Parameters:
C    (1) INPUT      (TSP, 1D)  The input dataset, a spectrum which must
C                               have Q and U Stokes parameters present.
C    (2) BINERR     (Real)     The percentage error for each polarization
C                               bin.
C    (3) DEVICE     (Device)   The Graphics device (any valid GKS device).
C    (4) LABEL      (Char)     A label for the plot.
C        AUTO       (Logical)  True if plot is to be autoscaled.
C    (C) IMIN       (Real)     Minimum Intensity level to plot.
C    (C) IMAX       (Real)     Maximum Intensity level to plot.
C    (C) PMIN       (Real)     Minimum Polarization level to plot.
C    (C) PMAX       (Real)     Maximum Polarization level to plot.
C    (H) THETA      (Real)     Shift in angle to apply to theta plot.
C                               Plot range is THETA to 180+THETA.
C    (H) TMIN       (Real)     Minimum position angle to plot
C    (H) TMAX       (Real)     Maximum position angle to plot
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         15/8/1990
C
C-
C
C  History:
C    Aug/1987   Original Version.   JAB/AAO
C    26/2/1988   TSP Monolith version.  JAB/AAO
C    29/4/1988   Use X-label from data. JAB/AAO
C    18/8/1988   Use Y-label from data. JAB/AAO
C    19/8/1988   Add THETA parameter. JAB/AAO
C    19/8/1988   Prevent crash on bad input file.  JAB/AAO
C    19/8/1988   Handle abort on LABEL or DEVICE.  JAB/AAO
C    15/8/1990   Add TMIN, TMAX.      JAB/AAO
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Status argument
      INTEGER STATUS

*  Data pointers
      INTEGER IPTR,QPTR,UPTR,QEPTR,UEPTR,T1PTR,T2PTR,LPTR

*  Array sizes
      INTEGER SIZE,DIMS(3),ACTDIM

*  Error per bin
      REAL BINERR

*  Autoscale flag
      LOGICAL AUTO

*  Scaling levels
      REAL IMIN,IMAX
      REAL PMIN,PMAX

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC,IDLOC,QDLOC,UDLOC,QELOC,UELOC
      CHARACTER*(DAT__SZLOC) QLOC,ULOC,T1LOC,T2LOC,LLOC
      CHARACTER*64 LABEL,UNITS,XLABEL,YLABEL
      INTEGER L1,L2

*  Position angle scaling
      REAL THETA
      REAL TMIN,TMAX

      INTEGER CHR_LEN

*  Get the data

      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)
      CALL TSP_SIZE(LOC,3,DIMS,ACTDIM,STATUS)
      SIZE = DIMS(1)

*  Get the Stokes parameter objects

      CALL TSP_GET_STOKES(LOC,'Q',QLOC,STATUS)
      CALL TSP_GET_STOKES(LOC,'U',ULOC,STATUS)

*  Map the data

      CALL TSP_MAP_DATA(LOC,'READ',IPTR,IDLOC,STATUS)

*  Map the Q stokes parameter and its variance

      CALL TSP_MAP_DATA(QLOC,'READ',QPTR,QDLOC,STATUS)
      CALL TSP_MAP_VAR(QLOC,'READ',QEPTR,QELOC,STATUS)

*  Map the U stokes parameter and its variance

      CALL TSP_MAP_DATA(ULOC,'READ',UPTR,UDLOC,STATUS)
      CALL TSP_MAP_VAR(ULOC,'READ',UEPTR,UELOC,STATUS)

*  Map the wavelength array

      CALL TSP_MAP_LAMBDA(LOC,'READ',LPTR,LLOC,STATUS)

*  Get the label and units and use as X label for the plot

      CALL TSP_RLU_LAMBDA(LOC,LABEL,UNITS,STATUS)
      L1 = CHR_LEN(LABEL)
      L2 = CHR_LEN(UNITS)
      XLABEL = LABEL(1:L1)//' '//UNITS(1:L2)//'$'

*  Get the data label and units and use as Y label for the plot

      CALL TSP_RLU(LOC,LABEL,UNITS,STATUS)
      L1 = CHR_LEN(LABEL)
      L2 = CHR_LEN(UNITS)
      IF (LABEL .EQ. ' ') THEN
          YLABEL = 'Intensity$'
      ELSE
          YLABEL = LABEL(1:L1)//' '//UNITS(1:L2)//'$'
      ENDIF

*  Get temporary arrays for binned data

      CALL TSP_TEMP(SIZE,'_REAL',T1PTR,T1LOC,STATUS)
      CALL TSP_TEMP(SIZE,'_REAL',T2PTR,T2LOC,STATUS)

*  Get the Binning error

      CALL PAR_GET0R('BINERR',BINERR,STATUS)

*  Get Theta scaling values

      CALL PAR_GET0R('TMIN',TMIN,STATUS)
      CALL PAR_GET0R('TMAX',TMAX,STATUS)

*  Autoscaling?

      CALL PAR_GET0L('AUTO',AUTO,STATUS)
      IF (STATUS .EQ. SAI__OK) THEN

*  Calculate Binned P and Theta arrays and errors

         CALL TSP_PPBIN(SIZE,%VAL(IPTR),%VAL(QPTR),%VAL(UPTR),
     :     %VAL(QEPTR),%VAL(UEPTR),%VAL(T1PTR),%VAL(T2PTR),BINERR,
     :     TMIN,TMAX,STATUS)

*  Find maximum and minimum values

         CALL TSP_EPSCALE(SIZE,%VAL(IPTR),%VAL(T1PTR),IMIN,IMAX,
     :     PMIN,PMAX,STATUS)
      ENDIF

*  If we are not autoscaling use the min and max values as defaults and
*  prompt the user for new values

      IF (.NOT. AUTO) THEN
          CALL PAR_DEF0R('IMIN',IMIN,STATUS)
          CALL PAR_DEF0R('IMAX',IMAX,STATUS)
          CALL PAR_GET0R('IMIN',IMIN,STATUS)
          CALL PAR_GET0R('IMAX',IMAX,STATUS)
          CALL PAR_DEF0R('PMIN',PMIN,STATUS)
          CALL PAR_DEF0R('PMAX',PMAX,STATUS)
          CALL PAR_GET0R('PMIN',PMIN,STATUS)
          CALL PAR_GET0R('PMAX',PMAX,STATUS)
      ENDIF

*  Do the plot

      IF (STATUS .EQ. SAI__OK) THEN
        CALL TSP_PPLOT(SIZE,%VAL(IPTR),%VAL(T1PTR),%VAL(T2PTR),
     :   %VAL(LPTR),PMIN,PMAX,IMIN,IMAX,XLABEL,YLABEL,TMIN,TMAX,
     :   STATUS)
      ENDIF

*  Tidy up

      CALL TSP_UNMAP(IDLOC,STATUS)
      CALL TSP_UNMAP(QDLOC,STATUS)
      CALL TSP_UNMAP(UDLOC,STATUS)
      CALL TSP_UNMAP(QELOC,STATUS)
      CALL TSP_UNMAP(UELOC,STATUS)
      CALL TSP_UNMAP(LLOC,STATUS)
      CALL TSP_UNMAP(T1LOC,STATUS)
      CALL TSP_UNMAP(T2LOC,STATUS)
      CALL DAT_ANNUL(QLOC,STATUS)
      CALL DAT_ANNUL(ULOC,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)
      END




       SUBROUTINE TSP_PPBIN(SIZE,INT,Q,U,QERROR,UERROR,TEMP1,TEMP2,
     :   BINERR,TMIN,TMAX,STATUS)
*+
*
*   T S P _ P P B I N
*
*   PPLOT command
*
*   Subroutine to calculate Binned P and Theta. The binned output array
*   has the same size as the input arrays, but is filled with a value which
*   is constant over the wavelength range of each bin. The bin sizes are
*   variable, and are adjusted so that the error for each bin is constant
*   in percentage polarization.
*
*   Bins with no data are filled with the bad value  (VAL__BADR)
*
*    (>)  SIZE   (Integer)            The number of spectral points
*    (>)  INT    (Real array(SIZE))   The intensity array
*    (>)  Q      (Real array(SIZE))   The Q stokes parameter array
*    (>)  U      (Real array(SIZE))   The U stokes parameter array
*    (>)  QERROR (Real array(SIZE))   The Q error array (variance of data)
*    (>)  UERROR (Real array(SIZE))   The U error array (variance of data)
*    (<)  TEMP1  (Real array(SIZE))   Binned Polarization array
*    (<)  TEMP2  (Real array(SIZE))   Binned Theta array
*    (>)  BINERR (Real)               Error per bin (percent)
*    (>)  TMIN   (Real)               Minimum theta value
*    (>)  TMAX   (Real)               Maximum theta value
*    (!)  STATUS (Integer)            Status value
*
*    Jeremy Bailey   12/7/1990
*
*    Modified:
*        11/12/1991  -  Handle bad values
*
*+


      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'

*  Parameter
      INTEGER SIZE
      REAL INT(SIZE),Q(SIZE),U(SIZE),QERROR(SIZE),TEMP1(SIZE)
      REAL UERROR(SIZE),TEMP2(SIZE)
      REAL BINERR
      REAL TMIN,TMAX
      INTEGER STATUS

*  Local variables
      LOGICAL BIG_ENOUGH, MORE_DATA
      INTEGER BIN_START, BIN_END
      REAL BIN_Q, BIN_U, BIN_INT, BIN_VAR
      REAL QQ,UU,P,THETA
      INTEGER I
      REAL DEGRAD

      DEGRAD = 45.0/ATAN(1.0)

      IF (STATUS .EQ. SAI__OK) THEN

*  Bin the data

         MORE_DATA = .TRUE.

*  Initial values for BIN start and end

         BIN_START = 1
         BIN_END = 1

*  Loop over bins

         DO WHILE (MORE_DATA)

*  Initialize accumlated values for bin

            BIN_START = BIN_END
            BIN_INT = 0.0
            BIN_Q = 0.0
            BIN_U = 0.0
            BIN_VAR = 0.0
            BIG_ENOUGH = .FALSE.

*  loop over data points within bin

            DO WHILE (.NOT. BIG_ENOUGH)
              IF (INT(BIN_END) .NE. VAL__BADR
     :        .AND. Q(BIN_END) .NE. VAL__BADR
     :        .AND. U(BIN_END) .NE. VAL__BADR) THEN

*   Add data into bin accumulated values

               BIN_INT = BIN_INT+INT(BIN_END)
               BIN_Q = BIN_Q+Q(BIN_END)
               BIN_U = BIN_U+U(BIN_END)
               BIN_VAR = BIN_VAR+(QERROR(BIN_END)+UERROR(BIN_END))*0.5

*  Have we finished bin?

               BIN_END = BIN_END+1
               IF (BIN_END .GT. SIZE) THEN
                  BIG_ENOUGH = .TRUE.

*  Don't allow any bin to be bigger than 200

               ELSE IF (BIN_END-BIN_START .GT. 200) THEN
                  BIG_ENOUGH = .TRUE.
               ELSE

*  Bin is complete when error is small enough

                  BIG_ENOUGH = (SQRT(BIN_VAR) .LT. BINERR*BIN_INT/100.0)
               ENDIF
              ENDIF
            ENDDO

*  Calculate value for bin.

            IF (BIN_INT .GT. 0.0) THEN

*  Percentage stokes parameters

               QQ = BIN_Q/BIN_INT * 100.0
               UU = BIN_U/BIN_INT * 100.0

*  Percentage polarization

               P = SQRT(QQ*QQ+UU*UU)

*  Position angle

               THETA = DEGRAD* ATAN2(UU,QQ)
               THETA = THETA/2.0
               IF (THETA .LT. 0.0) THETA = THETA+180
            ELSE
               P = 0.0
               THETA = 0.0
            ENDIF

*  Fill bin with data values

            DO I=BIN_START, BIN_END-1
               TEMP1(I) = P
               TEMP2(I) = THETA
               IF (TEMP2(I) .LT. TMIN) TEMP2(I) = TEMP2(I)+180
               IF (TEMP2(I) .GT. TMAX) TEMP2(I) = TEMP2(I)-180
            ENDDO
            MORE_DATA = BIN_END .LE. SIZE
         ENDDO

      ENDIF
      END

       SUBROUTINE TSP_PPLOT(SIZE,INT,TEMP1,TEMP2,LAMBDA,
     :   PMIN,PMAX,IMIN,IMAX,XLABEL,YLABEL,TMIN,TMAX,STATUS)
*+
*
*   T S P _ P P L O T
*
*   Subroutine to do the polarization plot. This routine plots
*   the intensity, polarization and position angle arrays as a function of
*   wavelength. It includes the PAR_ calls to get the plot device
*   and plot label.
*
*   (>)  SIZE   (Integer)           The number of spectral points
*   (>)  INT    (Real array(SIZE))  The intensity array
*   (>)  TEMP1  (Real array(SIZE))  Temporary array for the binned
*                                     polarization data
*   (>)  TEMP2  (Real array(SIZE))  Temporary array for the binned
*                                     position angle data
*   (>)  LAMBDA (Real array(SIZE))  Wavelength array
*   (>)  PMIN   (Real)              Minimum polarization value
*   (>)  PMAX   (Real)              Maximum polarization value
*   (>)  IMIN   (Real)              Minimum Intensity for scaling
*   (>)  IMAX   (Real)              Maximum Intensity for scaling
*   (>)  XLABEL (Real)              X axis label
*   (>)  YLABEL (Real)              Y axis label
*   (>)  TMIN   (Real)              Minimum theta value
*   (>)  TMAX   (Real)              Maximum theta value
*   (!)  STATUS (Integer)           Status value
*
*    Jeremy Bailey    16/8/1988
*
*    Modified:
*        11/12/1991     Handle bad values
*
*+

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE
      REAL INT(SIZE),TEMP1(SIZE)
      REAL TEMP2(SIZE),LAMBDA(SIZE)
      REAL IMIN,IMAX
      REAL PMIN,PMAX
      CHARACTER*(*) XLABEL,YLABEL
      REAL TMIN,TMAX
      INTEGER STATUS

*  Local variables

*  SGS zone
      INTEGER ZONE
      INTEGER I

*  GKS error flag
      INTEGER IERR

*  FIRST time through flag
      LOGICAL FIRST
      SAVE FIRST

*  Window and viewport data
      REAL WIND(4),VIEWP(4)

*  HEAP for saving autograph context
      REAL HEAP(3000)

*  plot label
      CHARACTER*80 LABEL

      DATA FIRST /.TRUE./

      IF (STATUS .EQ. SAI__OK) THEN

*  Get the plot label

         CALL PAR_GET0C('LABEL',LABEL,STATUS)

*  Get the Graphics device

         CALL SGS_ASSOC('DEVICE','WRITE',ZONE,STATUS)
         IF (STATUS .NE. SAI__OK) RETURN
         CALL GQNT(1,IERR,WIND,VIEWP)

*  Initialize Autograph

         IF (FIRST) THEN
*             CALL SNX_AGSAV(HEAP)
             FIRST = .FALSE.
         ELSE
*             CALL SNX_AGRES(HEAP)
         ENDIF

*  Set up for NCAR plot

         CALL AGSETP('GRAPH.',VIEWP,4)

*  Set position of polarization plot

         CALL AGSETF('GRID/TOP.',0.70)
         CALL AGSETF('GRID/BOTTOM.',0.5)

*  No numbering on X-axis

         CALL AGSETF('AXIS/BOTTOM/NUMERIC/TYPE.',0.0)

*  Y-axis label

         CALL AGSETC('LABEL/NAME.','L')
         CALL AGSETI('LINE/NUMBER.',100)
         CALL AGSETF('LINE/CHAR.',0.070)
         CALL AGSETC('LINE/TEXT.','Polarization (%)$')

*  Character size and spacing

         CALL AGSETF('L/WI.',0.070)
         CALL AGSETF('L/MA/CO.',2.0)
         CALL AGSETF('RIGHT/MA/CO.',2.0)
         CALL AGSETF('X/NICE.',0.0)

*  Plot scaling

         CALL AGSETF('Y/MIN.',PMIN)
         CALL AGSETF('Y/MAX.',PMAX)
         CALL AGSETF('Y/NICE.',0.0)

*  Blank X-axis label

         CALL AGSETC('LABEL/NAME.','B')
         CALL AGSETI('LINE/NUMBER.',-100)
         CALL AGSETC('LINE/TEXT.',' $')
         CALL AGSETF('B/MA/CO.',2.0)
         CALL AGSETF('T/MA/CO.',2.0)

*  Plot the polarization data

         CALL EZXY(LAMBDA,TEMP1,SIZE,' $')

*  Bottom part of screen for intensity plot

         CALL AGSETF('GRID/TOP.',0.5)
         CALL AGSETF('GRID/BOTTOM.',0.15)

*  Y axis label

         CALL AGSETC('LABEL/NAME.','L')
         CALL AGSETI('LINE/NUMBER.',100)
         CALL AGSETF('LINE/CHAR.',0.040)
         CALL AGSETC('LINE/TEXT.',YLABEL)
         CALL AGSETF('L/WI.',0.040)
         CALL AGSETF('L/MA/CO.',2.0)

*  X-axis label

         CALL AGSETC('LABEL/NAME.','B')
         CALL AGSETI('LINE/NUMBER.',-100)
         CALL AGSETC('LINE/TEXT.',XLABEL)
         CALL AGSETF('LINE/CHAR.',0.040)
         CALL AGSETF('B/WI.',0.040)
         CALL AGSETF('X/NICE.',0.0)

*  Plot scaling

         CALL AGSETF('Y/MIN.',IMIN)
         CALL AGSETF('Y/MAX.',IMAX)
         CALL AGSETF('AXIS/BOTTOM/NUMERIC/TYPE.',3.0)
         CALL EZXY(LAMBDA,INT,SIZE,' $')

*  Top plot for position angle

         CALL AGSETF('GRID/TOP.',0.90)
         CALL AGSETF('GRID/BOTTOM.',0.70)

*  No numbering on X-axis

         CALL AGSETF('AXIS/BOTTOM/NUMERIC/TYPE.',0.0)

*  Y-axis label

         CALL AGSETC('LABEL/NAME.','L')
         CALL AGSETI('LINE/NUMBER.',100)
         CALL AGSETF('LINE/CHAR.',0.070)
         CALL AGSETC('LINE/TEXT.','Theta (degrees)$')

*  Title Size

         CALL AGSETC('LABEL/NAME.','T')
         CALL AGSETF('LABEL/BASEPOINT/X.',0.05)
         CALL AGSETF('LABEL/CENTERING.',-1.0)
         CALL AGSETI('LINE/NUMBER.',100)
         CALL AGSETF('LINE/CHAR.',0.080)

*  Character size and spacing

         CALL AGSETF('L/WI.',0.070)
         CALL AGSETF('L/MA/CO.',2.0)
         CALL AGSETF('X/NICE.',0.0)

*  Plot scaling

         CALL AGSETF('Y/MAX.',TMAX)
         CALL AGSETF('Y/MIN.',TMIN)
         CALL AGSETF('Y/NICE.',0.0)

*  Blank X-axis label

         CALL AGSETC('LABEL/NAME.','B')
         CALL AGSETI('LINE/NUMBER.',-100)
         CALL AGSETC('LINE/TEXT.',' $')
         CALL AGSETF('B/MA/CO.',2.0)

*  Plot theta data

         CALL EZXY(LAMBDA,TEMP2,SIZE,LABEL//'$')

*  Annul SGS zone

         CALL SGS_ANNUL(ZONE,STATUS)
      ENDIF
      END

