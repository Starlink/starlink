C+
      SUBROUTINE EPLOT(STATUS)
C
C            E P L O T
C
C     Command name:
C        EPLOT
C
C     Function:
C        Plot a polarization spectrum as P, Theta with error bars
C
C     Description:
C        EPLOT produces a plot of a polarization spectrum. The plot is
C        divided into three panels. The lower panel is the total intensity,
C        the center panel is the percentage polarization, the top panel
C        is the position angle in degrees.
C        Plotting is done with the NCAR/SGS/GKS graphics system.
C
C     Parameters:
C    (1) INPUT      (TSP, 1D)  The input dataset, a spectrum which must
C                               have Q and U Stokes parameters present.
C    (2) DEVICE     (Device)   The Graphics device (any valid GKS device).
C    (3) LABEL      (Char)     A label for the plot.
C        AUTO       (Logical)  True if plot is to be autoscaled.
C    (C) IMIN       (Real)     Minimum Intensity level to plot.
C    (C) IMAX       (Real)     Maximum Intensity level to plot.
C    (C) PMIN       (Real)     Minimum Polarization level to plot.
C    (C) PMAX       (Real)     Maximum Polarization level to plot.
C    (H) THETA      (Real)     Shift in angle to apply to theta plot.
C                               Plot range is THETA to 180+THETA.
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         6/12/1991
C
C-
C
C  History:
C    12/7/1990   Original Version.   JAB/JAC
C    6/12/1991   Handle bad values.  JAB/JAC
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Status argument
      INTEGER STATUS

*  Data pointers
      INTEGER IPTR,QPTR,UPTR,IEPTR,QEPTR,UEPTR,PPTR,TPTR,PEPTR,TEPTR,
     :   LPTR

*  Array sizes
      INTEGER SIZE,DIMS(3),ACTDIM

*  Autoscale flag
      LOGICAL AUTO

*  Scaling levels
      REAL IMIN,IMAX
      REAL PMIN,PMAX

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC,IDLOC,IELOC,QDLOC,UDLOC,QELOC,UELOC
      CHARACTER*(DAT__SZLOC) QLOC,ULOC,PLOC,TLOC,PELOC,TELOC,LLOC
      CHARACTER*64 LABEL,UNITS,XLABEL,YLABEL
      INTEGER L1,L2
      REAL THETA

      INTEGER CHR_LEN

*  Get the data

      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)
      CALL TSP_SIZE(LOC,3,DIMS,ACTDIM,STATUS)
      SIZE = DIMS(1)

*  Get the Stokes parameter objects

      CALL TSP_GET_STOKES(LOC,'Q',QLOC,STATUS)
      CALL TSP_GET_STOKES(LOC,'U',ULOC,STATUS)

*  Map the data and variance

      CALL TSP_MAP_DATA(LOC,'READ',IPTR,IDLOC,STATUS)
      CALL ERR_MARK
      CALL TSP_MAP_VAR(LOC,'READ',IEPTR,IELOC,STATUS)

*  If the variance is not present create a dummy array and set it to zero

      IF (STATUS .NE. SAI__OK) THEN
          CALL ERR_ANNUL(STATUS)
          CALL TSP_TEMP(SIZE,'_REAL',IEPTR,IELOC,STATUS)
          CALL TSP_EPZERO(SIZE,%VAL(IEPTR))
      ENDIF
      CALL ERR_RLSE

*  Map the Q stokes parameter data and variance

      CALL TSP_MAP_DATA(QLOC,'READ',QPTR,QDLOC,STATUS)
      CALL ERR_MARK
      CALL TSP_MAP_VAR(QLOC,'READ',QEPTR,QELOC,STATUS)

*  If the variance is not present create a dummy array and set it to zero

      IF (STATUS .NE. SAI__OK) THEN
          CALL ERR_ANNUL(STATUS)
          CALL TSP_TEMP(SIZE,'_REAL',QEPTR,QELOC,STATUS)
          CALL TSP_EPZERO(SIZE,%VAL(QEPTR))
      ENDIF
      CALL ERR_RLSE

*  Map the U stokes parameter data and variance

      CALL TSP_MAP_DATA(ULOC,'READ',UPTR,UDLOC,STATUS)
      CALL ERR_MARK
      CALL TSP_MAP_VAR(ULOC,'READ',UEPTR,UELOC,STATUS)

*  If the variance is not present create a dummy array and set it to zero

      IF (STATUS .NE. SAI__OK) THEN
          CALL ERR_ANNUL(STATUS)
          CALL TSP_TEMP(SIZE,'_REAL',UEPTR,UELOC,STATUS)
          CALL TSP_EPZERO(SIZE,%VAL(UEPTR))
      ENDIF
      CALL ERR_RLSE

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

*  Get temporary arrays for P, Theta

      CALL TSP_TEMP(SIZE,'_REAL',PPTR,PLOC,STATUS)
      CALL TSP_TEMP(SIZE,'_REAL',TPTR,TLOC,STATUS)
      CALL TSP_TEMP(SIZE,'_REAL',PEPTR,PELOC,STATUS)
      CALL TSP_TEMP(SIZE,'_REAL',TEPTR,TELOC,STATUS)

*  Get Theta Offset

      CALL PAR_GET0R('THETA',THETA,STATUS)

*  Autoscaling?

      CALL PAR_GET0L('AUTO',AUTO,STATUS)
      IF (STATUS .EQ. SAI__OK) THEN

*  Calculate P and Theta arrays and errors

         CALL TSP_EP(SIZE,%VAL(IPTR),%VAL(IEPTR),%VAL(QPTR),%VAL(UPTR),
     :     %VAL(QEPTR),%VAL(UEPTR),%VAL(PPTR),%VAL(PEPTR),%VAL(TPTR),
     :     %VAL(TEPTR),THETA,STATUS)

*  Find maximum and minimum values

         CALL TSP_EPSCALE(SIZE,%VAL(IPTR),%VAL(PPTR),IMIN,IMAX,
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
        CALL TSP_EPLOT(SIZE,%VAL(IPTR),%VAL(IEPTR),%VAL(PPTR),
     :   %VAL(PEPTR),%VAL(TPTR),%VAL(TEPTR),
     :   %VAL(LPTR),PMIN,PMAX,IMIN,IMAX,XLABEL,YLABEL,THETA,STATUS)
      ENDIF

*  Tidy up

      CALL TSP_UNMAP(IDLOC,STATUS)
      CALL TSP_UNMAP(QDLOC,STATUS)
      CALL TSP_UNMAP(UDLOC,STATUS)
      CALL TSP_UNMAP(IELOC,STATUS)
      CALL TSP_UNMAP(QELOC,STATUS)
      CALL TSP_UNMAP(UELOC,STATUS)
      CALL TSP_UNMAP(LLOC,STATUS)
      CALL TSP_UNMAP(PLOC,STATUS)
      CALL TSP_UNMAP(TLOC,STATUS)
      CALL TSP_UNMAP(PELOC,STATUS)
      CALL TSP_UNMAP(TELOC,STATUS)
      CALL DAT_ANNUL(QLOC,STATUS)
      CALL DAT_ANNUL(ULOC,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)
      END



       SUBROUTINE TSP_EP(SIZE,INT,INTE,Q,U,QERROR,UERROR,P,PE,TH,THE,
     :   THOF,STATUS)
*+
*
*   T S P _ E P
*
*   Subroutine to calculate P and Theta
*
*    (>)  SIZE   (Integer)            The number of spectral points
*    (>)  INT    (Real array(SIZE))   The intensity array
*    (>)  INTE   (Real array(SIZE))   The intensity errors
*    (>)  Q      (Real array(SIZE))   The Q stokes parameter array
*    (>)  U      (Real array(SIZE))   The U stokes parameter array
*    (>)  QERROR (Real array(SIZE))   The Q error array (variance of data)
*    (>)  UERROR (Real array(SIZE))   The U error array (variance of data)
*    (<)  P      (Real array(SIZE))   Polarization array
*    (<)  PE     (Real array(SIZE))   Polarization errors
*    (<)  TH     (Real array(SIZE))   Theta array
*    (<)  THE    (Real array(SIZE))   Theta errors
*    (<)  THOF   (Real array(SIZE))   P.A. plot offset
*
*    Jeremy Bailey   12/7/1990
*
*    Modified:
*        5/12/1991  -  Handle bad values
*
*+

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE
      REAL INT(SIZE),INTE(SIZE),Q(SIZE),U(SIZE),QERROR(SIZE),P(SIZE)
      REAL UERROR(SIZE),PE(SIZE),TH(SIZE),THE(SIZE)
      REAL THOF
      INTEGER STATUS

*  Local variables
      REAL QQ,UU,QE,UE,X
      INTEGER I

      IF (STATUS .EQ. SAI__OK) THEN


*  loop over data points
          DO I=1,SIZE

*  Set default value to use if data is bad
            P(I) = 0.0
            TH(I) = 0.0
            PE(I) = 0.0
            THE(I) = 0.0
            IF (INT(I) .NE. VAL__BADR) THEN
              IF (INT(I) .GT. 0.0) THEN

*  Calculate percentage stokes parameters ...
                  QQ = Q(I)/INT(I) * 100.0
                  UU = U(I)/INT(I) * 100.0

*  ... and errors on percentage stokes parameters
                  QE = SQRT(QERROR(I))/INT(I) * 100.0
                  UE = SQRT(UERROR(I))/INT(I) * 100.0

*  Calculate polarization
                  P(I) = SQRT(QQ*QQ+UU*UU)

*  Calculate position angle
                  IF (QQ .NE. 0.0) THEN
                      TH(I) = ATAN2(UU,QQ)/2.0 * (180.0/3.14159265)
                  ELSE
                      TH(I) = 0.0
                  ENDIF

*  Calculate error on percentage polarization ...
                  IF (P(I) .GT. 1E-4) THEN
                      PE (I) = (SQRT(QQ*QQ*QE*QE+UU*UU*UE*UE))/P(I)

*  ... and on position angle
                      X = QQ*QQ*UE*UE + UU*UU*QE*QE
                      X = 0.5*SQRT(X)
                      X = X/(P(I)*P(I))
                      THE(I) = ABS(X*57.2958)
                  ELSE
                      PE(I) = 0.0
                      THE(I) = 0.0
                  ENDIF

*  Normalize THETA to range 0 to 180
                  IF (TH(I) .LT. 0.0) TH(I) = TH(I) + 180.0
              ENDIF
            ENDIF
          ENDDO

      ENDIF
      END

       SUBROUTINE TSP_EPLOT(SIZE,INT,INTE,P,PE,TH,THE,LAMBDA,
     :   PMIN,PMAX,IMIN,IMAX,XLABEL,YLABEL,THETA,STATUS)
*+
*
*   T S P _ E P L O T
*
*   Subroutine to do the polarization plot
*
*    (>)  SIZE    (Integer)           The number of spectral points
*    (>)  INT     (Real array(SIZE))  The intensity array
*    (>)  INTE    (Real array(SIZE))  Intensity error array
*    (>)  P       (Real array(SIZE))  Polarization array
*    (>)  PE      (Real array(SIZE))  Polarization error array
*    (>)  TH      (Real array(SIZE))  Position angle array
*    (>)  THE     (Real array(SIZE))  Position angle error array
*    (>)  LAMBDA  (Real array(SIZE))  Wavelength array
*    (>)  PMIN    (Real)              Minimum polarization value
*    (>)  PMAX    (Real)              Maximum polarization value
*    (>)  IMIN    (Real)              Minimum Intensity for scaling
*    (>)  IMAX    (Real)              Maximum Intensity for scaling
*    (>)  XLABEL  (Char)              Label for Wavelength Axis
*    (>)  YLABEL  (Char)              Label for Y axis
*    (>)  THETA   (Real)              P.A. plot Offset
*    (!)  STATUS  (Integer)           Status value
*
*    Jeremy Bailey   12/7/1990
*
*    Modified:
*       5/12/1991 - Handle bad values
*
*+

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE
      REAL INT(SIZE),INTE(SIZE),P(SIZE),PE(SIZE),TH(SIZE),THE(SIZE)
      REAL LAMBDA(SIZE)
      REAL IMIN,IMAX
      REAL PMIN,PMAX
      CHARACTER*(*) XLABEL,YLABEL
      REAL THETA
      INTEGER STATUS

*  Local variables
      INTEGER ZONE
      INTEGER I
      INTEGER IERR
      LOGICAL FIRST
      SAVE FIRST
      REAL WIND(4),VIEWP(4)
      REAL HEAP(3000)
      CHARACTER*80 LABEL
      REAL GX,GY,GY1,GY2,GS,GX1,GX2
      INTEGER IZ,J

*  SNX functions
      REAL SNX_AGUGX,SNX_AGUGY

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

*  Set up position for first plot (Polarization)

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

*  Plot the polarization data (using SGS)

*  Plot axes
         CALL AGSTUP(LAMBDA,1,1,SIZE,1,P,1,1,SIZE,1)
         CALL AGBACK

*  Set SGS Zone
         CALL SNX_AGCS
         J = SAI__OK
         CALL SGS_ZONE(0.0,1.0,0.0,1.0,IZ,J)
         CALL SGS_SW(0.0,1.0,0.0,1.0,J)

*  Calculate size of horizontal error bar
         GS = (LAMBDA(SIZE)-LAMBDA(1))/(2.0*SIZE)

*  Loop over points
         DO I=1,SIZE

           IF (INT(I) .NE. VAL__BADR) THEN
*  Calculate coordinates of ends of horizontal bar ...
             GX = SNX_AGUGX(LAMBDA(I))
             GX1 = SNX_AGUGX(LAMBDA(I)+GS)
             GX2 = SNX_AGUGX(LAMBDA(I)-GS)

*  ... and of vertical bar
             GY = SNX_AGUGY(P(I))
             GY1 = SNX_AGUGY(P(I)+PE(I))
             GY2 = SNX_AGUGY(P(I)-PE(I))

*  Draw the two error bars
             CALL SGS_LINE(GX1,GY,GX2,GY)
             CALL SGS_LINE(GX,GY1,GX,GY2)
           ENDIF
         ENDDO

*  Restore original SGS zone
         CALL SGS_SELZ(ZONE,J)

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

*  Plot intensity data

*  Draw axes
         CALL AGSTUP(LAMBDA,1,1,SIZE,1,INT,1,1,SIZE,1)
         CALL AGBACK

*  Set up SGS zone
         CALL SNX_AGCS
         CALL SGS_ZONE(0.0,1.0,0.0,1.0,IZ,J)
         CALL SGS_SW(0.0,1.0,0.0,1.0,J)

*  determine size of horizontal error bar
         GS = (LAMBDA(SIZE)-LAMBDA(1))/(2.0*SIZE)

*  Loop over points
         DO I=1,SIZE

           IF (INT(I) .NE. VAL__BADR) THEN

*  Calculate end points of horizontal bar
             GX = SNX_AGUGX(LAMBDA(I))
             GX1 = SNX_AGUGX(LAMBDA(I)+GS)
             GX2 = SNX_AGUGX(LAMBDA(I)-GS)

*  Calculate end points of vertical bar
             GY = SNX_AGUGY(INT(I))
             GY1 = SNX_AGUGY(INT(I)+SQRT(INTE(I)))
             GY2 = SNX_AGUGY(INT(I)-SQRT(INTE(I)))

*  Draw error bars
             CALL SGS_LINE(GX1,GY,GX2,GY)
             CALL SGS_LINE(GX,GY1,GX,GY2)
           ENDIF
         ENDDO

*  Restore original zone
         CALL SGS_SELZ(ZONE,J)


*  Top plot (position angle)

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

         CALL AGSETF('Y/MAX.',THETA+180.0)
         CALL AGSETF('Y/MIN.',THETA+0.01)
         CALL AGSETF('Y/NICE.',0.0)

*  Blank X-axis label

         CALL AGSETC('LABEL/NAME.','B')
         CALL AGSETI('LINE/NUMBER.',-100)
         CALL AGSETC('LINE/TEXT.',' $')
         CALL AGSETF('B/MA/CO.',2.0)

*  Top label

         CALL AGSETC('LABEL/NAME.','T')
         CALL AGSETF('LABEL/BASEPOINT/X.',0.05)
         CALL AGSETF('LABEL/CENTERING.',-1.0)
         CALL AGSETI('LINE/NUMBER.',100)
         CALL AGSETC('LINE/TEXT.',LABEL//'$')

*  Plot theta data


*  Draw axes
         CALL AGSTUP(LAMBDA,1,1,SIZE,1,TH,1,1,SIZE,1)
         CALL AGBACK

*  Set up SGS zone
         CALL SNX_AGCS
         CALL SGS_ZONE(0.0,1.0,0.0,1.0,IZ,J)
         CALL SGS_SW(0.0,1.0,0.0,1.0,J)

*  Calculate size of horizontal bar
         GS = (LAMBDA(SIZE)-LAMBDA(1))/(2.0*SIZE)

*  Loop over points
         DO I=1,SIZE

           IF (INT(I) .NE. VAL__BADR) THEN
*  Calculate end points of horizontal bar
             GX = SNX_AGUGX(LAMBDA(I))
             GX1 = SNX_AGUGX(LAMBDA(I)+GS)
             GX2 = SNX_AGUGX(LAMBDA(I)-GS)

*  Calculate end points of vertical bar
             GY = SNX_AGUGY(TH(I))
             GY1 = SNX_AGUGY(TH(I)+THE(I))
             GY2 = SNX_AGUGY(TH(I)-THE(I))

*  Draw error bars
             CALL SGS_LINE(GX1,GY,GX2,GY)
             CALL SGS_LINE(GX,GY1,GX,GY2)
           ENDIF
         ENDDO

*  Restore original zone
         CALL SGS_SELZ(ZONE,J)

*  Annul zone
         CALL SGS_ANNUL(ZONE,STATUS)
      ENDIF
      END




