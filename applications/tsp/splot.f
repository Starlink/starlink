C+
      SUBROUTINE SPLOT(STATUS)
C
C            S P L O T
C
C     Command name:
C        SPLOT
C
C     Function:
C        Plot a polarization spectrum with a single Stokes parameter
C
C     Description:
C        SPLOT produces a plot of a polarization spectrum. The plot is
C        divided into two panels. The lower panel is the total intensity,
C        the top panel is the percentage polarization for a single Stokes
C        parameter. If the dataset contains only one Stokes parameter that
C        Stokes parameter is plotted. If the spectrum contains more than
C        one Stokes parameter any one of them may be chosen for plotting.
C        The polarization data is binned into variable size bins to give
C        a constant polarization error per bin. Plotting is done with the
C        NCAR/SGS/GKS graphics system.
C
C     Parameters:
C    (1) INPUT      (TSP, 1D)  The input dataset, a spectrum which must
C                               have at least one Stokes parameter.
C    (2) BINERR     (Real)     The percentage error for each polarization
C                               bin.
C    (3) DEVICE     (Device)   The Graphics device (any valid GKS device).
C    (4) LABEL      (Char)     A label for the plot.
C        STOKESPAR  (Char)     The Stokes parameter to be plotted (Q,U,V).
C        AUTO       (Logical)  True if plot is to be autoscaled.
C    (C) IMIN       (Real)     Minimum Intensity level to plot.
C    (C) IMAX       (Real)     Maximum Intensity level to plot.
C    (C) PMIN       (Real)     Minimum Polarization level to plot.
C    (C) PMAX       (Real)     Maximum Polarization level to plot.
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         19/8/1988
C
C-
C
C  History:
C    1/9/1987   Original Version.   JAB/AAO
C    26/2/1988   TSP Monolith version.  JAB/AAO
C    29/4/1988   Use X-label from data. JAB/AAO
C    19/8/1988   Handle abort on LABEL or DEVICE.  JAB/AAO
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  Data pointers
      INTEGER IPTR,EPTR,SPTR,TPTR,LPTR

*  Array sizes
      INTEGER SIZE,DIMS(3),ACTDIM

*  Error per bin
      REAL BINERR

*  Autoscale flag
      LOGICAL AUTO

*  Input OK flag
      LOGICAL OK

*  Stokes parameter to plot
      CHARACTER*64 STOKES

*  Scaling levels
      REAL IMIN,IMAX
      REAL PMIN,PMAX
      INTEGER NUM
      LOGICAL Q,U,V

*  HDS locators
      CHARACTER*(DAT__SZLOC) TLOC,LOC,ILOC,STLOC,SLOC,ELOC,LLOC
      CHARACTER*64 LABEL,UNITS,XLABEL,YLABEL
      INTEGER L1,L2

*  ICH function
      INTEGER CHR_LEN

*  Get the data

      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)
      CALL TSP_SIZE(LOC,3,DIMS,ACTDIM,STATUS)
      SIZE = DIMS(1)

*  Get the Stokes parameter objects

      CALL TSP_STOKES(LOC,NUM,Q,U,V,STATUS)

*  If more than one Stokes parameter ask which one to use

      IF (NUM .GE. 2) THEN
          OK = .FALSE.
          DO WHILE ((.NOT. OK) .AND. (STATUS .EQ. SAI__OK))

*  Ask for a Stokes parameter
              CALL PAR_GET0C('STOKESPAR',STOKES,STATUS)

*  Check input is valid
              OK = (STOKES .EQ. 'Q' .OR. STOKES .EQ. 'U' .OR.
     :             STOKES .EQ. 'V')

*  If not cancel parameter and prompt again
              IF (.NOT. OK) CALL PAR_CANCL('STOKESPAR',STATUS)
          ENDDO
      ELSE

*  If only one stokes parameter use that

          IF (Q) THEN
             STOKES = 'Q'
          ELSE IF (U) THEN
             STOKES = 'U'
          ELSE IF (V) THEN
             STOKES = 'V'
          ELSE
             CALL MSG_OUT('MSG','No Stokes Parameter in Dataset',
     :            STATUS)
             STATUS = USER__001
          ENDIF
      ENDIF

*  Get the Stokes parameter

      CALL TSP_GET_STOKES(LOC,STOKES,STLOC,STATUS)

*  Map the intensity data

      CALL TSP_MAP_DATA(LOC,'READ',IPTR,ILOC,STATUS)

*  Map the Stokes parameter data

      CALL TSP_MAP_DATA(STLOC,'READ',SPTR,SLOC,STATUS)

*  Map the Stokes variance

      CALL TSP_MAP_VAR(STLOC,'READ',EPTR,ELOC,STATUS)

*  Map the wavelength array

      CALL TSP_MAP_LAMBDA(LOC,'READ',LPTR,LLOC,STATUS)

*  Get label and units of wavelength axis and use to construct X label

      CALL TSP_RLU_LAMBDA(LOC,LABEL,UNITS,STATUS)
      L1 = CHR_LEN(LABEL)
      L2 = CHR_LEN(UNITS)
      XLABEL = LABEL(1:L1)//' '//UNITS(1:L2)//'$'

*  Get label and units of data and use to construct Y label

      CALL TSP_RLU(LOC,LABEL,UNITS,STATUS)
      L1 = CHR_LEN(LABEL)
      L2 = CHR_LEN(UNITS)
      IF (LABEL .EQ. ' ') THEN
          YLABEL = 'Intensity$'
      ELSE
          YLABEL = LABEL(1:L1)//' '//UNITS(1:L2)//'$'
      ENDIF

*  Get temporary array for binned data

      CALL TSP_TEMP(SIZE,'_REAL',TPTR,TLOC,STATUS)

*  Get the Binning error

      CALL PAR_GET0R('BINERR',BINERR,STATUS)

*  Autoscaling?

      CALL PAR_GET0L('AUTO',AUTO,STATUS)
      IF (STATUS .EQ. SAI__OK) THEN

*   Bin the data
         CALL TSP_SPBIN(SIZE,%VAL(IPTR),%VAL(SPTR),%VAL(EPTR),
     :      BINERR,%VAL(TPTR),STATUS)

*   Find maximum and minimum values
         CALL TSP_QUPLTSCALE(SIZE,%VAL(IPTR),%VAL(TPTR),IMIN,IMAX,PMIN,
     :      PMAX,STATUS)
      ENDIF

*  If not autoscaling prompt for scaling levels with maximum and minimum as
*  defaults
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
        CALL TSP_SPLOT(SIZE,%VAL(IPTR),%VAL(TPTR),
     :   %VAL(LPTR),PMIN,PMAX,IMIN,IMAX,XLABEL,YLABEL,STATUS)
      ENDIF

      print *,'finished'

*  Tidy up

      CALL TSP_UNMAP(ILOC,STATUS)
      CALL TSP_UNMAP(SLOC,STATUS)
      CALL TSP_UNMAP(ELOC,STATUS)
      CALL TSP_UNMAP(TLOC,STATUS)
      CALL DAT_ANNUL(STLOC,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)
      print *,'exiting'
      END




      SUBROUTINE TSP_SPBIN(SIZE,INT,STOKES,ERROR,BINERR,BINNED,STATUS)
*+
*
*   T S P _ S P B I N
*
*   SPLOT command
*
*   Subroutine to calculate Binned Stokes parameters. The binned output array
*   has the same size as the input array, but is filled with a value which
*   is constant over the wavelength range of each bin. The bin sizes are
*   variable, and are adjusted so that the error for each bin is constant
*   in percentage polarization.
*
*   Bins with no data are filled with the bad value  (VAL__BADR)
*
*    (>)  SIZE   (Integer)            The number of spectral points
*    (>)  INT    (Real array(SIZE))   The intensity array
*    (>)  STOKES (Real array(SIZE))   The stokes parameter array
*    (>)  ERROR  (Real array(SIZE))   The stokes error array (variance of data)
*    (>)  BINERR (Real)               Error per bin (percent)
*    (<)  BINNED (Real array(SIZE))   Binned stokes array
*    (!)  STATUS (Integer)            Status value
*
*    Jeremy Bailey   12/7/1990
*
*    Modified:
*        16/12/1991  -  Handle bad values
*
*+


      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE
      REAL BINERR
      REAL INT(SIZE),STOKES(SIZE),ERROR(SIZE),BINNED(SIZE)
      INTEGER STATUS

*  Local variables
      LOGICAL BIG_ENOUGH, MORE_DATA
      INTEGER BIN_START, BIN_END
      REAL BIN_STOKES, BIN_INT, BIN_VAR
      INTEGER I

*  Bin the data

      IF (STATUS .EQ. SAI__OK) THEN
         MORE_DATA = .TRUE.

*   Initial values for bin start and end

         BIN_START = 1
         BIN_END = 1

*  Loop over bins

         DO WHILE (MORE_DATA)

*  initialize accumulated values for bin

            BIN_START = BIN_END
            BIN_INT = 0.0
            BIN_STOKES = 0.0
            BIN_VAR = 0.0
            BIG_ENOUGH = .FALSE.

*  add data into the bin until there is enough data to give
*  required polarization accuracy

            DO WHILE (.NOT. BIG_ENOUGH)
              IF (INT(BIN_END) .NE. VAL__BADR
     :        .AND. STOKES(BIN_END) .NE. VAL__BADR) THEN

*  Add data into bin accumulated values

               BIN_INT = BIN_INT+INT(BIN_END)
               BIN_STOKES = BIN_STOKES+STOKES(BIN_END)
               BIN_VAR = BIN_VAR+ERROR(BIN_END)

*  Have we finished bin?

               BIN_END = BIN_END+1
               IF (BIN_END .GT. SIZE) THEN
                  BIG_ENOUGH = .TRUE.
               ELSE

*  Bin is complete when error is small enough

                  BIG_ENOUGH = (SQRT(BIN_VAR) .LT. BINERR*BIN_INT/100.0)
               ENDIF
              ENDIF
            ENDDO

*  Fill the bin with the polarization value

            DO I=BIN_START, BIN_END-1
               IF (BIN_INT .GT. 0.0) THEN
                   BINNED(I) = BIN_STOKES/BIN_INT * 100.0
               ELSE
                   BINNED(I) = VAL__BADR
               ENDIF
            ENDDO
            MORE_DATA = BIN_END .LE. SIZE
         ENDDO
      ENDIF
      END





       SUBROUTINE TSP_SPLOT(SIZE,INT,TEMP,LAMBDA,PMIN,PMAX,
     :  IMIN,IMAX,XLABEL,YLABEL,STATUS)
*+
*
*   T S P _ S P L O T
*
*   Subroutine to do the stokes plot. This routine plots
*   the intensity and stokes parameter arrays as a function of
*   wavelength. It includes the PAR_ calls to get the plot device
*   and plot label.
*
*   Parameters:
*
*  (>)   SIZE    (Integer)           The number of spectral points
*  (>)   INT     (Real array(SIZE))  The intensity array
*  (>)   TEMP    (Real array(SIZE))  Temporary array for the binned data
*  (>)   LAMBDA  (Real array(SIZE))  Wavelength array
*  (>)   PMIN    (Real)              Minimum Polarization value
*  (>)   PMAX    (Real)              Maximum Polarization value
*  (>)   IMIN    (Real)              Minimum Intensity for scaling
*  (>)   IMAX    (Real)              Maximum Intensity for scaling
*  (>)   XLABEL  (Char)              Label for X-axis
*  (>)   YLABEL  (Char)              Label for Y-axis
*  (!)   STATUS  (Integer)           Status value
*
*   Jeremy Bailey   19/8/1988
*
*   Modified:
*      16/12/1991  -  Handle bad values
*
*+
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Parameters
      INTEGER SIZE

*  Data arrays
      REAL INT(SIZE),TEMP(SIZE),LAMBDA(SIZE)
      REAL IMIN,IMAX
      REAL PMIN,PMAX
      CHARACTER*(*) XLABEL,YLABEL
      INTEGER STATUS

*  Local variables

*  SGS zone
      INTEGER ZONE

*  HEAP for saving autograph context
      REAL HEAP(5000)
      INTEGER I

*  GKS error flag
      INTEGER IERR

*  Window and viewport data
      REAL WIND(4),VIEWP(4)
      CHARACTER*80 LABEL

*  First time through flag
      LOGICAL FIRST
      SAVE FIRST

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

*  Top plot  (Stokes parameter plot)

         CALL AGSETF('GRID/TOP.',0.95)
         CALL AGSETF('GRID/BOTTOM.',0.55)

*  No numbering on X-axis

         CALL AGSETF('AXIS/BOTTOM/NUMERIC/TYPE.',0.0)

*  Y-axis label

         CALL AGSETC('LABEL/NAME.','L')
         CALL AGSETI('LINE/NUMBER.',100)
         CALL AGSETF('LINE/CHAR.',0.040)
         CALL AGSETC('LINE/TEXT.','Polarization (%)$')

*  Title Size

         CALL AGSETC('LABEL/NAME.','T')
         CALL AGSETF('LABEL/BASEPOINT/X.',0.05)
         CALL AGSETF('LABEL/CENTERING.',-1.0)
         CALL AGSETI('LINE/NUMBER.',100)
         CALL AGSETF('LINE/CHAR.',0.045)

*  Character size and spacing

         CALL AGSETF('L/WI.',0.040)
         CALL AGSETF('L/MA/CO.',2.0)
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

*  Plot the Stokes data

         CALL EZXY(LAMBDA,TEMP,SIZE,LABEL//'$')

*  Bottom half of screen for intensity plot

         CALL AGSETF('GRID/TOP.',0.55)
         CALL AGSETF('GRID/BOTTOM.',0.15)

*  Y-axis label

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
         CALL AGSETF('Y/NICE.',0.0)

*  Numbering on X-axis

         CALL AGSETF('AXIS/BOTTOM/NUMERIC/TYPE.',3.0)

*  Plot the intensity

         CALL EZXY(LAMBDA,INT,SIZE,' $')

*  Annul SGS Zone

         CALL SGS_ANNUL(ZONE,STATUS)
      ENDIF
      END

