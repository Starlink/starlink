C+
      SUBROUTINE QUPLOT(STATUS)
C
C            Q U P L O T
C
C     Command name:
C        QUPLOT
C
C     Function:
C        Plot a polarization spectrum in the Q,U plane.
C
C     Description:
C        QUPLOT produces a plot of the polarization spectrum in the
C        Q,U plane. The polarization data is first binned to a constant
C        percentage polarization error per bin, and the resulting points
C        are plotted. Plotting is done with the NCAR/SGS/GKS graphics system.
C
C     Parameters:
C    (1) INPUT      (TSP, 1D)  The input dataset, a spectrum which must
C                               have Q and U Stokes parameters present.
C    (2) BINERR     (Real)     The percentage error for each polarization
C                               bin.
C    (3) DEVICE     (Device)   The Graphics device (any valid GKS device).
C    (4) LABEL      (Char)     A label for the plot.
C        AUTO       (Logical)  True if plot is to be autoscaled.
C    (C) QMIN       (Real)     Minimum Q level to plot.
C    (C) QMAX       (Real)     Maximum Q level to plot.
C    (C) UMIN       (Real)     Minimum U level to plot.
C    (C) UMAX       (Real)     Maximum U level to plot.
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         26/2/1988
C
C-
C
C  History:
C    Jan/1988   Original Version.   JAB/AAO
C    26/2/1988   TSP Monolith version.  JAB/AAO
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

*  Scaling limits
      REAL QMIN,QMAX
      REAL UMIN,UMAX

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC,IDLOC,QDLOC,UDLOC,QELOC,UELOC
      CHARACTER*(DAT__SZLOC) QLOC,ULOC,T1LOC,T2LOC,LLOC

*  Get the data

      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)

*  Get the size of the data

      CALL TSP_SIZE(LOC,3,DIMS,ACTDIM,STATUS)
      SIZE = DIMS(1)

*  Get the Stokes parameter objects

      CALL TSP_GET_STOKES(LOC,'Q',QLOC,STATUS)
      CALL TSP_GET_STOKES(LOC,'U',ULOC,STATUS)

*  Map the intensity data

      CALL TSP_MAP_DATA(LOC,'READ',IPTR,IDLOC,STATUS)

*  Map the Stokes parameters and their variances

      CALL TSP_MAP_DATA(QLOC,'READ',QPTR,QDLOC,STATUS)
      CALL TSP_MAP_VAR(QLOC,'READ',QEPTR,QELOC,STATUS)
      CALL TSP_MAP_DATA(ULOC,'READ',UPTR,UDLOC,STATUS)
      CALL TSP_MAP_VAR(ULOC,'READ',UEPTR,UELOC,STATUS)

*  Map the wavelength array

      CALL TSP_MAP_LAMBDA(LOC,'READ',LPTR,LLOC,STATUS)

*  Get temporary arrays for binned data

      CALL TSP_TEMP(SIZE,'_REAL',T1PTR,T1LOC,STATUS)
      CALL TSP_TEMP(SIZE,'_REAL',T2PTR,T2LOC,STATUS)

*  Get the Binning error

      CALL PAR_GET0R('BINERR',BINERR,STATUS)

*  Autoscaling?

      CALL PAR_GET0L('AUTO',AUTO,STATUS)

*  Convert to Q and U and bin the data

      CALL TSP_QUPLTBIN(SIZE,%VAL(IPTR),%VAL(QPTR),%VAL(UPTR),
     :   %VAL(QEPTR),%VAL(UEPTR),%VAL(T1PTR),%VAL(T2PTR),BINERR,STATUS)

*  Get scaling limits

      CALL TSP_QUPLTSCALE(SIZE,%VAL(T1PTR),%VAL(T2PTR),QMIN,QMAX,
     :   UMIN,UMAX,STATUS)

*  If we are not autoscaling prompt for the scaling levels using
*  the maximum and minimum as the defaults

      IF (.NOT. AUTO) THEN
          CALL PAR_DEF0R('QMIN',QMIN,STATUS)
          CALL PAR_DEF0R('QMAX',QMAX,STATUS)
          CALL PAR_GET0R('QMIN',QMIN,STATUS)
          CALL PAR_GET0R('QMAX',QMAX,STATUS)
          CALL PAR_DEF0R('UMIN',UMIN,STATUS)
          CALL PAR_DEF0R('UMAX',UMAX,STATUS)
          CALL PAR_GET0R('UMIN',UMIN,STATUS)
          CALL PAR_GET0R('UMAX',UMAX,STATUS)
      ENDIF

*  Do the plot

      IF (STATUS .EQ. SAI__OK) THEN
        CALL TSP_QUPLOT(SIZE,%VAL(T1PTR),%VAL(T2PTR),
     :  UMIN,UMAX,QMIN,QMAX,STATUS)
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






       SUBROUTINE TSP_QUPLTBIN(SIZE,INT,Q,U,QERROR,UERROR,TEMP1,TEMP2,
     :   BINERR,STATUS)
*+
*
*   T S P _ Q U P L T B I N
*
*   QUPLOT command
*
*   Subroutine to calculate Binned Q and U. The binned output arrays
*   have the same size as the input arrays, but are filled with a value which
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
*    (<)  TEMP1  (Real array(SIZE))   Binned Q array
*    (<)  TEMP2  (Real array(SIZE))   Binned U array
*    (>)  BINERR (Real)               Error per bin (percent)
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

*  Parameters
      INTEGER SIZE
      REAL INT(SIZE),Q(SIZE),U(SIZE),QERROR(SIZE),TEMP1(SIZE)
      REAL UERROR(SIZE),TEMP2(SIZE)
      REAL BINERR
      INTEGER STATUS

*  Local variables
      LOGICAL BIG_ENOUGH, MORE_DATA
      INTEGER BIN_START, BIN_END
      REAL BIN_Q, BIN_U, BIN_INT, BIN_VAR
      REAL QQ,UU,P,THETA
      INTEGER I

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

*  Loop over data points within bin

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
            ELSE
               QQ = 0.0
               UU = 0.0
            ENDIF

*  Fill bin with data values

            DO I=BIN_START, BIN_END-1
               TEMP1(I) = QQ
               TEMP2(I) = UU
            ENDDO
            MORE_DATA = BIN_END .LE. SIZE
         ENDDO

      ENDIF
      END

       SUBROUTINE TSP_QUPLOT(SIZE,TEMP1,TEMP2,
     :   UMIN,UMAX,QMIN,QMAX,STATUS)
*+
*
*   T S P _ Q U P L O T
*
*   QUPLOT command
*
*   Subroutine to do the polarization plot. Q and U are plotted against
*   each other as a QU diagram.
*
*   The plot device and plot label are obtained by ADAM PAR_ calls within
*   this routine
*
*   Parameters
*
*    (>) SIZE   (Integer)           The number of spectral points
*    (>) INT    (Real array(SIZE))  The intensity array
*    (>) TEMP1  (Real array(SIZE))  Temporary array for the binned Q data
*    (>) TEMP2  (Real array(SIZE))  Temporary array for the binned U data
*    (>) UMIN   (Real)              Minimum U value
*    (>) UMAX   (Real)              Maximum U value
*    (>) QMIN   (Real)              Minimum Q value for scaling
*    (>) QMAX   (Real)              Maximum Q value for scaling
*    (!) STATUS (Integer)           Status value
*
*   Jeremy Bailey   28/2/1988
*
*   Modified:
*       3/12/1991   -  Reduce size of plot area slightly to avoid losing the
*                      axes occasionally
*      11/12/1991   -  Handle bad values
*
*+

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE
      REAL TEMP1(SIZE),TEMP2(SIZE)
      REAL QMIN,QMAX
      REAL UMIN,UMAX
      INTEGER STATUS

*  Local variables

*  SGS zone
      INTEGER ZONE
      INTEGER ZONE1
      INTEGER I

*  GKS error flag
      INTEGER IERR
      INTEGER STAT

*  First time through flag
      LOGICAL FIRST
      SAVE FIRST

*  Window and viewport data
      REAL WIND(4),VIEWP(4)

*  HEAP for saving autograph context
      REAL HEAP(3000)

*  Plot label
      CHARACTER*80 LABEL

      DATA FIRST /.TRUE./

      IF (STATUS .EQ. SAI__OK) THEN

*  Get the plot label

         CALL PAR_GET0C('LABEL',LABEL,STATUS)

*  Get the Graphics device

         CALL SGS_ASSOC('DEVICE','WRITE',ZONE,STATUS)

*  Set square zone
         CALL SGS_ZSHAP(1.0,'CC',ZONE1,STATUS)
         CALL GQNT(1,IERR,WIND,VIEWP)

*  Initialize Autograph

         IF (FIRST) THEN
*             CALL SNX_AGSAV(HEAP)
             FIRST = .FALSE.
         ELSE
*            CALL SNX_AGRES(HEAP)
         ENDIF

*  Set up for NCAR plot

         CALL AGSETP('GRAPH.',VIEWP,4)

*  Set position of plot

         CALL AGSETF('GRID/TOP.',0.86)
         CALL AGSETF('GRID/BOTTOM.',0.14)

*  Y-axis label

         CALL AGSETC('LABEL/NAME.','L')
         CALL AGSETI('LINE/NUMBER.',100)
         CALL AGSETF('LINE/CHAR.',0.030)
         CALL AGSETC('LINE/TEXT.','U/I (%)$')

         CALL AGSETC('LABEL/NAME.','B')
         CALL AGSETI('LINE/NUMBER.',-100)
         CALL AGSETF('LINE/CHAR.',0.030)
         CALL AGSETC('LINE/TEXT.','Q/I (%)$')

*  Character size and spacing

         CALL AGSETF('LEFT/WI.',0.030)
         CALL AGSETF('AXIS/BOTTOM/WI.',0.030)
         CALL AGSETF('AXIS/BOTTOM/MA/CO.',2.0)
         CALL AGSETF('AXIS/TOP/MA/CO.',2.0)
         CALL AGSETF('LEFT/MA/CO.',2.0)
         CALL AGSETF('RIGHT/MA/CO.',2.0)

*  Plot scaling

         CALL AGSETF('Y/MIN.',UMIN)
         CALL AGSETF('Y/MAX.',UMAX)
         CALL AGSETF('Y/NICE.',0.0)
         CALL AGSETF('X/MIN.',QMIN)
         CALL AGSETF('X/MAX.',QMAX)
         CALL AGSETF('X/NICE.',0.0)


         CALL AGSETC('LABEL/NAME.','T')
         CALL AGSETF('LABEL/BASEPOINT/X.',0.05)
         CALL AGSETF('LABEL/CENTERING.',-1.0)
         CALL AGSETI('LINE/NUMBER.',100)
         CALL AGSETF('LINE/CHAR.',0.040)

*  Plot the polarization data

         CALL EZXY(TEMP1,TEMP2,SIZE,LABEL)

*  Annul the SGS zone

         CALL SGS_ANNUL(ZONE,STATUS)
      ENDIF
      END

