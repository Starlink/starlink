C+
      SUBROUTINE FPLOT(STATUS)
C
C            F P L O T
C
C     Command name:
C        FPLOT
C
C     Function:
C        Plot a polarization spectrum as Polarized Intensity
C
C     Description:
C        FPLOT produces a plot of a polarization spectrum. The plot is
C        divided into two panels. The lower panel is the total intensity,
C        the top panel is the polarized intensity (or polarized flux).
C
C        The polarized intensity data is binned into fixed size bins of
C        size specified by the BINSIZE parameter. Plotting is done with the
C        NCAR/SGS/GKS graphics system.
C
C     Parameters:
C    (1) INPUT      (TSP, 1D)  The input dataset, a spectrum which must
C                               have both Q and U Stokes parameters or
C                               the V Stokes parameter present.
C    (2) BINSIZE    (Integer)  The number of spectral channels per bin
C    (3) DEVICE     (Device)   The Graphics device (any valid GKS device).
C    (4) LABEL      (Char)     A label for the plot.
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
C         9/12/1991
C
C-
C
C  History:
C    Jan/1988   Original Version.   JAB/AAO
C    26/2/1988   TSP Monolith version.  JAB/AAO
C    29/4/1988   Use X-label from data. JAB/AAO
C    16/8/1988   Use fixed rather than variable size bins.  JAB/AAO
C    19/8/1988   Prevent Crash on bad input file.  JAB/AAO
C    19/8/1988   Handle abort on LABEL or DEVICE.  JAB/AAO
C    2/12/1988   Allow Circular Polarization data.  JAB/JACH
C    9/12/1991   Handle bad values.   JAB/AAO
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Status argument
      INTEGER STATUS

*  Data pointers
      INTEGER IPTR,QPTR,UPTR,QEPTR,UEPTR,T1PTR,T2PTR,LPTR

*  Array size
      INTEGER SIZE,DIMS(3),ACTDIM

*  bin size
      INTEGER BINSIZE

*  Scaling parameters
      LOGICAL AUTO
      REAL IMIN,IMAX
      REAL PMIN,PMAX

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC,IDLOC,QDLOC,UDLOC,QELOC,UELOC
      CHARACTER*(DAT__SZLOC) QLOC,ULOC,T1LOC,T2LOC,LLOC

*  Label and units
      CHARACTER*64 LABEL,UNITS,XLABEL,YLABEL
      INTEGER L1,L2
      LOGICAL CIRC

      INTEGER CHR_LEN

*  Get the data

      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)

*  Find its size
      CALL TSP_SIZE(LOC,3,DIMS,ACTDIM,STATUS)
      SIZE = DIMS(1)

*  Get the Q and U Stokes parameter objects - if these can't be found look
*  for a V Stokes parameter object, and if that is present set the CIRC flag

      CALL TSP_GET_STOKES(LOC,'Q',QLOC,STATUS)
      CALL TSP_GET_STOKES(LOC,'U',ULOC,STATUS)
      IF (STATUS .NE. SAI__OK) THEN
          STATUS = SAI__OK

*  Get the V stokes parameter as locator QLOC

          CALL TSP_GET_STOKES(LOC,'V',QLOC,STATUS)
          CIRC = .TRUE.
      ELSE
          CIRC = .FALSE.
      ENDIF

*  Map the data

      CALL TSP_MAP_DATA(LOC,'READ',IPTR,IDLOC,STATUS)

*  Map the Q (or V) data

      CALL TSP_MAP_DATA(QLOC,'READ',QPTR,QDLOC,STATUS)
      CALL TSP_MAP_VAR(QLOC,'READ',QEPTR,QELOC,STATUS)
      IF (.NOT. CIRC) THEN

*  Map the U data

          CALL TSP_MAP_DATA(ULOC,'READ',UPTR,UDLOC,STATUS)
          CALL TSP_MAP_VAR(ULOC,'READ',UEPTR,UELOC,STATUS)
      ELSE
          UPTR = QPTR
          UEPTR = QEPTR
      ENDIF

*  Map the wavelength array

      CALL TSP_MAP_LAMBDA(LOC,'READ',LPTR,LLOC,STATUS)

*  Get the label and units of the wavelength and build the plot X label

      CALL TSP_RLU_LAMBDA(LOC,LABEL,UNITS,STATUS)
      L1 = CHR_LEN(LABEL)
      L2 = CHR_LEN(UNITS)
      XLABEL = LABEL(1:L1)//' '//UNITS(1:L2)//'$'

*  Get the label and units of the data and build the plot Y label

      CALL TSP_RLU(LOC,LABEL,UNITS,STATUS)
      L1 = CHR_LEN(LABEL)
      L2 = CHR_LEN(UNITS)
      IF (LABEL .EQ. ' ') THEN
          YLABEL = 'Intensity$'
      ELSE
          YLABEL = LABEL(1:L1)//' '//UNITS(1:L2)//'$'
      ENDIF

*  Get temporary array for binned data

      CALL TSP_TEMP(SIZE,'_REAL',T1PTR,T1LOC,STATUS)

*  Get the Bin size

      CALL PAR_GET0I('BINSIZE',BINSIZE,STATUS)

*  Autoscaling?

      CALL PAR_GET0L('AUTO',AUTO,STATUS)

*  Do the binning and get the maximum and minimum values

      IF (STATUS .EQ. SAI__OK) THEN
         CALL TSP_FPBIN(SIZE,%VAL(IPTR),%VAL(QPTR),%VAL(UPTR),
     :     %VAL(QEPTR),%VAL(UEPTR),%VAL(T1PTR),CIRC,BINSIZE,STATUS)
         CALL TSP_FPSCALE(SIZE,%VAL(IPTR),%VAL(T1PTR),IMIN,IMAX,
     :     PMIN,PMAX,STATUS)
      ENDIF

*  If not autoscaling get the scaling levels using the maximum and minimum as
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
        CALL TSP_FPLOT(SIZE,%VAL(IPTR),%VAL(T1PTR),
     :   %VAL(LPTR),PMIN,PMAX,IMIN,IMAX,XLABEL,YLABEL,STATUS)
      ENDIF

*  Tidy up

      CALL TSP_UNMAP(IDLOC,STATUS)
      CALL TSP_UNMAP(QDLOC,STATUS)
      IF (.NOT. CIRC) CALL TSP_UNMAP(UDLOC,STATUS)
      CALL TSP_UNMAP(QELOC,STATUS)
      IF (.NOT. CIRC) CALL TSP_UNMAP(UELOC,STATUS)
      CALL TSP_UNMAP(LLOC,STATUS)
      CALL TSP_UNMAP(T1LOC,STATUS)
      CALL DAT_ANNUL(QLOC,STATUS)
      IF (.NOT. CIRC) CALL DAT_ANNUL(ULOC,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)
      END



       SUBROUTINE TSP_FPSCALE(SIZE,INT,P,IMIN,IMAX,PMIN,PMAX,STATUS)
*+
*
*  T S P _ F P S C A L E
*
*   Determine maximum and minimum values of intensity and polarized intensity
*   for use in the plot scaling
*
*   (>)  SIZE   (Integer)            Size of the arrays
*   (>)  INT    (Real array(SIZE))   Intensity array
*   (>)  P      (Real array(SIZE))   Polarized intensity array
*   (<)  IMIN   (Real)               Minimum intensity value
*   (<)  IMAX   (Real)               Maximum intensity value
*   (<)  PMIN   (Real)               Mininum polarization value
*   (<)  PMAX   (Real)               Maximum polarization value
*   (!)  STATUS (Integer)            Status value
*
*    Jeremy Bailey    12/7/1990
*
*    Modified:
*       6/12/1991    Handle bad values
*
*+
       IMPLICIT NONE

      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'

*  Parameters
       INTEGER SIZE
       REAL INT(SIZE),P(SIZE)
       REAL IMIN,IMAX,PMIN,PMAX
       INTEGER STATUS

*  Local variables
       REAL RANGE
       INTEGER I

       IF (STATUS .EQ. SAI__OK) THEN

*  Set initial values

          IMIN = VAL__MAXR
          IMAX = VAL__MINR

*  Loop over good data values data replacing current value of IMAX with
*  any data value larger than IMAX, similarly for IMIN
          DO I = 1,SIZE
            IF (INT(I) .NE. VAL__BADR) THEN
              IF (INT(I) .GT. IMAX) THEN
                  IMAX=INT(I)
              ENDIF
              IF (INT(I) .LT. IMIN) THEN
                  IMIN=INT(I)
              ENDIF
            ENDIF
          ENDDO

*  Expand range slightly for tidier plot

          RANGE=IMAX-IMIN
          IMAX=IMAX+0.05*RANGE
          IMIN=IMIN-0.05*RANGE


*  Set initial values

*  Force PMIN to zero
          PMIN = 0.0
*  Set initial value for PMAX
          PMAX = VAL__MINR

*  Loop over good data values data replacing current value of PMAX with
*  any data value larger than PMAX,
          DO I = 1,SIZE
            IF (P(I) .NE. VAL__BADR) THEN
              IF (P(I) .GT. PMAX) THEN
                  PMAX=P(I)
              ENDIF
            ENDIF
          ENDDO

*  Expand range slightly for tidier plot

          RANGE=PMAX-PMIN
          PMAX=PMAX+0.05*RANGE
       ENDIF
       END




       SUBROUTINE TSP_FPBIN(SIZE,INT,Q,U,QERROR,UERROR,TEMP1,
     :   CIRC,BINSIZE,STATUS)
*+
*
*   T S P _ F P B I N
*
*   Subroutine to calculate the polarized intensity and output the values
*   binned into fixed size bins. The binned output array has the same size
*   as the input arrays, but is filled with a value which is constant over
*   the wavelength range of each bin, thus giving the effect of a histogram
*   type plot when it is plotted as a continuous line.
*
*   Bins with no data are filled with the bad value (VAL__BADR)
*
*    (>)  SIZE   (Integer)           The number of spectral points
*    (>)  INT    (Real array(SIZE))  The intensity array
*    (>)  Q      (Real array(SIZE))  The Q stokes parameter array
*    (>)  U      (Real array(SIZE))  The U stokes parameter array
*    (>)  QERROR (Real array(SIZE))  The Q error array (variance of data)
*    (>)  UERROR (Real array(SIZE))  The U error array (variance of data)
*    (<)  TEMP1  (Real array(SIZE))  Temporary array for the binned data
*    (>)  CIRC   (Logical)           TRUE if circular polarization data
*    (>)  BINSIZE(Integer)           The size of bin for plotting
*    (!)  STATUS (Integer)           Status value
*
*   Jeremy Bailey   16/8/1988
*
*   Modified:
*      9/12/1991    Handle bad values
*
*+

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE
      REAL INT(SIZE),Q(SIZE),U(SIZE),QERROR(SIZE),TEMP1(SIZE)
      REAL UERROR(SIZE)
      INTEGER BINSIZE
      INTEGER STATUS

*  Local variables
      LOGICAL BIG_ENOUGH, MORE_DATA
      INTEGER BIN_START, BIN_END
      REAL BIN_Q, BIN_U, BIN_INT, BIN_VAR
      REAL QQ,UU,P,THETA
      INTEGER I
      LOGICAL CIRC
      INTEGER BIN_N

      IF (STATUS .EQ. SAI__OK) THEN

*  Bin the data

         MORE_DATA = .TRUE.

*  Intial values for BIN start and end

         BIN_START = 1
         BIN_END = 1

*  Loop over bins

         DO WHILE (MORE_DATA)

*  Initialize accumulated values for bin

            BIN_START = BIN_END
            BIN_INT = 0.0
            BIN_Q = 0.0
            BIN_U = 0.0
            BIN_N = 0
            BIG_ENOUGH = .FALSE.

*  Loop over data points within bin

            DO WHILE (.NOT. BIG_ENOUGH)
              IF (INT(BIN_END) .NE. VAL__BADR
     :        .AND. Q(BIN_END) .NE. VAL__BADR
     :        .AND. U(BIN_END) .NE. VAL__BADR) THEN

*   Add data into bin accumulated values

                BIN_INT = BIN_INT+INT(BIN_END)
                BIN_Q = BIN_Q+Q(BIN_END)

*   Only accumulate U if not circular polarization

                IF (.NOT. CIRC) THEN
                   BIN_U = BIN_U+U(BIN_END)
                ENDIF
                BIN_N = BIN_N+1

*  Have we finished bin?

                BIN_END = BIN_END+1
                IF (BIN_END .GT. SIZE) THEN
                  BIG_ENOUGH = .TRUE.
                ELSE
                  BIG_ENOUGH = BIN_END-BIN_START .GE. BINSIZE
                ENDIF
              ENDIF
            ENDDO

*  Calculate value for bin. If we are doing circular polarization this
*  is the fractional V stokes parameter times the intensity divided by
*  the number of good points. For linear polarization it is the fractional
*  polarization times the intensity divided by the number of good points

            IF (BIN_N .NE. 0) THEN
               QQ = BIN_Q/BIN_INT
               UU = BIN_U/BIN_INT
               IF (CIRC) THEN
                  P = QQ*BIN_INT/REAL(BIN_N)
               ELSE
                  P = SQRT(QQ*QQ+UU*UU)*BIN_INT/REAL(BIN_N)
               ENDIF
            ELSE

*  If there is no good data in bin set to bad value

               P = VAL__BADR
            ENDIF

*  Fill output data with value for bin

            DO I=BIN_START, BIN_END-1
               TEMP1(I) = P
            ENDDO
            MORE_DATA = BIN_END .LT. SIZE
         ENDDO

      ENDIF
      END

       SUBROUTINE TSP_FPLOT(SIZE,INT,TEMP1,
     :   LAMBDA,PMIN,PMAX,IMIN,IMAX,XLABEL,YLABEL,STATUS)
*+
*
*   T S P _ F P L O T
*
*   Subroutine to do the polarized intensity plot. This routine plots
*   the intensity and polarized intensity arrays as a function of
*   wavelength. It includes the PAR_ calls to get the plot device
*   and plot label.
*
*   (>)  SIZE   (Integer)           The number of spectral points
*   (>)  INT    (Real array(SIZE))  The intensity array
*   (>)  TEMP1  (Real array(SIZE))  Temporary array for the binned
*                                     polarized intensity data
*   (>)  LAMBDA (Real array(SIZE))  Wavelength array
*   (>)  PMIN   (Real)              Minimum polarization value
*   (>)  PMAX   (Real)              Maximum polarization value
*   (>)  IMIN   (Real)              Minimum Intensity for scaling
*   (>)  IMAX   (Real)              Maximum Intensity for scaling
*   (>)  XLABEL (Real)              X axis label
*   (>)  YLABEL (Real)              Y axis label
*   (!)  STATUS (Integer)           Status value
*
*    Jeremy Bailey    16/8/1988
*
*    Modified:
*        9/12/1991     Handle bad values
*
*+

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE
      REAL INT(SIZE),TEMP1(SIZE)
      REAL LAMBDA(SIZE)
      REAL IMIN,IMAX
      REAL PMIN,PMAX
      CHARACTER*(*) XLABEL,YLABEL
      INTEGER STATUS

*  Local variables
      INTEGER ZONE
      INTEGER I
      INTEGER IERR
      LOGICAL FIRST
      SAVE FIRST
      REAL WIND(4),VIEWP(4)
      CHARACTER*80 TSTRING

*  HEAP for saving autograph context

      REAL HEAP(3000)
      CHARACTER*80 LABEL

*  First time through flag

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

*  Set position of top plot (polarized intensity)

         CALL AGSETF('GRID/TOP.',0.90)
         CALL AGSETF('GRID/BOTTOM.',0.5)

*  No numbering on X-axis

         CALL AGSETF('AXIS/BOTTOM/NUMERIC/TYPE.',0.0)

*  Y-axis label (Put 'Polarized' in front of the intensity label

         CALL AGSETC('LABEL/NAME.','L')
         CALL AGSETI('LINE/NUMBER.',100)
         CALL AGSETF('LINE/CHAR.',0.040)
         TSTRING = 'Polarized '//YLABEL
         CALL AGSETC('LINE/TEXT.',TSTRING)

*  Character size and spacing

         CALL AGSETF('L/WI.',0.040)
         CALL AGSETF('L/MA/CO.',2.0)
         CALL AGSETF('RIGHT/MA/CO.',2.0)
         CALL AGSETF('X/NICE.',0.0)

*  Set plot scaling for polarization

         CALL AGSETF('Y/MIN.',PMIN)
         CALL AGSETF('Y/MAX.',PMAX)
         CALL AGSETF('Y/NICE.',0.0)

*  Blank X-axis label

         CALL AGSETC('LABEL/NAME.','B')
         CALL AGSETI('LINE/NUMBER.',-100)
         CALL AGSETC('LINE/TEXT.',' $')
         CALL AGSETF('B/MA/CO.',2.0)
         CALL AGSETF('T/MA/CO.',2.0)

*  Title Size

         CALL AGSETC('LABEL/NAME.','T')
         CALL AGSETF('LABEL/BASEPOINT/X.',0.05)
         CALL AGSETF('LABEL/CENTERING.',-1.0)
         CALL AGSETI('LINE/NUMBER.',100)
         CALL AGSETF('LINE/CHAR.',0.040)

*  Plot the polarization data

         CALL EZXY(LAMBDA,TEMP1,SIZE,LABEL)

*  Set bottom part of screen for intensity plot

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

*  Setr plot scaling for intensity

         CALL AGSETF('Y/MIN.',IMIN)
         CALL AGSETF('Y/MAX.',IMAX)
         CALL AGSETF('AXIS/BOTTOM/NUMERIC/TYPE.',3.0)

*  Plot the intensity data

         CALL EZXY(LAMBDA,INT,SIZE,' $')

*  Annul the SGS zone

         CALL SGS_ANNUL(ZONE,STATUS)
      ENDIF
      END

