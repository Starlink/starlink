


      SUBROUTINE ELF1_GRAPH(PSIZE,ZEROP,RESULT,VALIDP,
     :                      STATUS)
*+
*  Name:
*     ELF1_GRAPH

*  Purpose:
*     Displays the graphs on the requested graphics device.
*     Graphical display will show the equivalent radius (r*)
*     rather than the semi-major axis length r(a).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL ELF1_GRAPH(PSIZE,ZEROP,RESULT,VALIDP,STATUS)

*  Description:
*      Displays the graphical output from the program. This consists of
*      a graph showing the radius versus brightness.

*  Arguments:
*     PSIZE = REAL (Given)
*        The pixel size in arcsecs.
*     ZEROP = REAL (Given)
*        Magnitude scale zero point.
*     RESULT(17,Ell__MXPOI) = REAL (Given)
*        The profiling results array.
*     VALIDP = INTEGER (Given)
*        Number of radii for which a 'fit' was obtained.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     02-Dec-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'ELF_PAR'               ! ELLFOU constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'NDF_PAR'               ! NDF constants

*  Arguments Given:
      INTEGER VALIDP                  ! Number of radii for which
                                      ! a profile was determined
      REAL PSIZE                      ! Size of the pixels
      REAL RESULT(17,ELF__MXPOI)      ! Profiling results file
      REAL ZEROP                      ! Magnitude scale zero point

*  Arguments Given and Returned:

*  Arguments Returned:

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      CHARACTER *(256) HEAD           ! Graph heading
      CHARACTER *(256) LABELX         ! Graph X axis heading
      CHARACTER *(256) LABELY         ! Graph Y axis heading
      INTEGER I                       ! Loop variable
      REAL LOW(2)                     ! The lowest parameter value
      REAL HIGH(2)                    ! The highest parameter value
      REAL RAD(1)                     ! X axis value to display
      REAL TEMP                       ! Temporary value
      REAL VAL1(1)                    ! Y axis value to display
      REAL VAL2(1)                    ! Y axis value to display
      REAL X1                         ! Viewport limit
      REAL X2                         ! Viewport limit
      REAL Y1                         ! Viewport limit
      REAL Y2                         ! Viewport limit
      REAL ZERO                       ! Zero
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set the minimum and maximum initial values.
      LOW(1)=VAL__MAXR
      HIGH(1)=VAL__MINR
      LOW(2)=VAL__MAXR
      HIGH(2)=VAL__MINR

*   Loop through all the data points.
      LOW(1)=0.0
      DO 30 I=1,VALIDP

*      Find the low and high values for the equivalent radius and value.

*      Equivalent radius. Maximum value only required.
         TEMP=PSIZE*SQRT(RESULT(4,I)*RESULT(4,I)*RESULT(3,I))
         IF (TEMP.GT.HIGH(1)) HIGH(1)=TEMP

*      Value. Check to ensure that the value obtained can
*      be converted to a logarithm.
         TEMP=RESULT(6,I)
         IF (TEMP.GT.0.0) THEN
            TEMP=ZEROP-2.5*LOG10(TEMP)
            IF (TEMP.LT.LOW(2)) LOW(2)=TEMP
            IF (TEMP.GT.HIGH(2)) HIGH(2)=TEMP
         END IF

 30   CONTINUE

*   Adjust for a more pleasing display.
      HIGH(2)=HIGH(2)+.2
      LOW(2)=LOW(2)-.2

*   Set up the display using the limits calculated.
      CALL PGWINDOW(LOW(1),HIGH(1),HIGH(2),LOW(2))

*   Inquire what the viewport size is.
      CALL PGQVP(1,X1,X2,Y1,Y2)

*   Reset the lettering size if necessary.
      IF (((Y2-Y1).LT.2.5).OR.((X2-X1).LT.2.5)) THEN
         IF ((Y2-Y1).LT.(X2-X1)) THEN
            TEMP=(Y2-Y1)/2.
         ELSE
            TEMP=(X2-X1)/2.
         END IF
         CALL PGSCH(TEMP)
      END IF

*   Set up the labelling marks on the axes.
      ZERO=0.0
      CALL PGBOX('ABCGNST',ZERO,0,'ABCGNST',ZERO,0)

*   Set up values for and display the labels for the graph.

*   Set up the labels.
      IF (((Y2-Y1).LT.2.5).OR.((X2-X1).LT.2.5)) THEN
         LABELX='Radius * (arc secs)'
         LABELY='SB Zp-2.5Log(I-Back)'
      ELSE
         LABELX='Radius * (arc seconds)'
         LABELY='Surface Brightness Zp-2.5Log(I-Back)'
      END IF

*   Main heading.
      HEAD='ELLFOU PROFILE'

*   Display the labels of the graphs.
      CALL PGLABEL(LABELX,LABELY,HEAD)

*   Display the data points.
      DO 40 I=1,VALIDP

*      Get the equivalent radius and count values.
         RAD(1)=PSIZE*SQRT(RESULT(4,I)*RESULT(4,I)*RESULT(3,I))
         TEMP=RESULT(6,I)

*      Only display on a log graph if the subtracted count is
*      greater than zero.
         IF (TEMP.GT.0.0) THEN

*         Display the data point.
            VAL1(1)=ZEROP-2.5*LOG10(TEMP)
            CALL PGPOINT(1,RAD,VAL1,23)

*         Display its error bar. Must check that the count minus the
*         error does not go below zero and if so, correct accordingly.
            VAL1(1)=ZEROP-2.5*LOG10(TEMP+RESULT(7,I))
            IF (TEMP-RESULT(7,I).GT.0.0) THEN
               VAL2(1)=ZEROP-2.5*LOG10(TEMP-RESULT(7,I))
            ELSE
               VAL2(1)=HIGH(2)
            END IF

            CALL PGERRY(1,RAD,VAL2,VAL1,1.)

         END IF

 40   CONTINUE

 9999 CONTINUE

      END


      SUBROUTINE ELP1_GRAPH(PSIZE,ZEROP,RESULT,VALIDP,
     :                      STATUS)
*+
*  Name:
*     ELP1_GRAPH

*  Purpose:
*     Displays the graphs on the requested graphics device.
*     radius values are displayed converted to equivalent radius.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL ELP1_GRAPH(PSIZE,ZEROP,RESULT,VALIDP,STATUS)

*  Description:
*      Displays the graphical output from the program. This consists of
*      a graph showing the radius versus brightness.

*  Arguments:
*     PSIZE = REAL (Given)
*        The pixel size in arcsecs.
*     ZEROP = REAL (Given)
*        Magnitude scale zero point.
*     RESULT(ELP__NRES,Ell__MXPOI) = REAL (Given)
*        The profiling results array.
*     VALIDP = INTEGER (Given)
*        Number of radii for which a 'fit' was obtained.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     02-Dec-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'ELP_PAR'               ! ELLPRO constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'NDF_PAR'               ! NDF constants

*  Arguments Given:
      INTEGER VALIDP                  ! Number of radii for which
                                      ! a profile was determined
      REAL PSIZE                      ! Size of the pixels
      REAL RESULT(ELP__NRES,ELP__MXPOI)      ! Profiling results file
      REAL ZEROP                      ! Magnitude scale zero point

*  Arguments Given and Returned:

*  Arguments Returned:

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      CHARACTER *(256) HEAD           ! Graph heading
      CHARACTER *(256) LABELX         ! Graph X axis heading
      CHARACTER *(256) LABELY         ! Graph Y axis heading
      INTEGER I                       ! Loop variable
      REAL LOW(2)                     ! The lowest parameter value
      REAL HIGH(2)                    ! The highest parameter value
      REAL RAD(1)                     ! X axis value to display
      REAL TEMP                       ! Temporary value
      REAL VAL1(1)                    ! Y axis value to display
      REAL VAL2(1)                    ! Y axis value to display
      REAL X1                         ! Viewport limit
      REAL X2                         ! Viewport limit
      REAL Y1                         ! Viewport limit
      REAL Y2                         ! Viewport limit
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set the minimum and maximum initial values.
      LOW(1)=VAL__MAXR
      HIGH(1)=VAL__MINR
      LOW(2)=VAL__MAXR
      HIGH(2)=VAL__MINR

*   Loop through all the data points.
      LOW(1)=0.0
      DO 30 I=1,VALIDP

*      Find the low and high values for the equivalent radius and value.

*      Equivalent radius. Maximum value only required.
         TEMP=PSIZE*SQRT(RESULT(4,I)*RESULT(4,I)*RESULT(3,I))
         IF (TEMP.GT.HIGH(1)) HIGH(1)=TEMP

*      Value. Check to ensure that the value obtained can
*      be converted to a logarithm.
         TEMP=RESULT(6,I)
         IF (TEMP.GT.0.0) THEN
            TEMP=ZEROP-2.5*LOG10(TEMP)
            IF (TEMP.LT.LOW(2)) LOW(2)=TEMP
            IF (TEMP.GT.HIGH(2)) HIGH(2)=TEMP
         END IF

 30   CONTINUE

*   Adjust for a more pleasing display.
      HIGH(2)=HIGH(2)+.2
      LOW(2)=LOW(2)-.2

*   Set up the display using the limits calculated.
      CALL PGWINDOW(LOW(1),HIGH(1),HIGH(2),LOW(2))

*   Inquire what the viewport size is.
      CALL PGQVP(1,X1,X2,Y1,Y2)

*   Reset the lettering size if necessary.
      IF (((Y2-Y1).LT.2.5).OR.((X2-X1).LT.2.5)) THEN
         IF ((Y2-Y1).LT.(X2-X1)) THEN
            TEMP=(Y2-Y1)/2.
         ELSE
            TEMP=(X2-X1)/2.
         END IF
         CALL PGSCH(TEMP)
      END IF

*   Set up the labelling marks on the axes.
      CALL PGBOX('ABCGNST',0.0,0,'ABCGNST',0.0,0)

*   Set up values for and display the labels for the graph.

*   Set up the labels.
      IF (((Y2-Y1).LT.2.5).OR.((X2-X1).LT.2.5)) THEN
         LABELX='Radius * (arc secs)'
         LABELY='SB Zp-2.5Log(I-Back)'
      ELSE
         LABELX='Radius * (arc seconds)'
         LABELY='Surface Brightness Zp-2.5Log(I-Back)'
      END IF

*   Main heading.
      HEAD='ELLPRO PROFILE'

*   Display the labels of the graphs.
      CALL PGLABEL(LABELX,LABELY,HEAD)

*   Display the data points.
      DO 40 I=1,VALIDP

*      Get the radius and count values.
         RAD(1)=PSIZE*SQRT(RESULT(4,I)*RESULT(4,I)*RESULT(3,I))
         TEMP=RESULT(6,I)

*      Only display on a log graph if the subtracted count is
*      greater than zero.
         IF (TEMP.GT.0.0) THEN

*         Display the data point.
            VAL1(1)=ZEROP-2.5*LOG10(TEMP)
            CALL PGPOINT(1,RAD,VAL1,23)

*         Display its error bar. Must check that the count minus the
*         error does not go below zero and if so, correct accordingly.
            VAL1(1)=ZEROP-2.5*LOG10(TEMP+RESULT(7,I))
            IF (TEMP-RESULT(7,I).GT.0.0) THEN
               VAL2(1)=ZEROP-2.5*LOG10(TEMP-RESULT(7,I))
            ELSE
               VAL2(1)=HIGH(2)
            END IF

            CALL PGERRY(1,RAD,VAL2,VAL1,1.)

         END IF

 40   CONTINUE

 9999 CONTINUE

      END


      SUBROUTINE GRA1_GRAPH(GMODE,ANGCON,ANGOFF,FTYPE,ZEROP,RADISP,
     :                     WHATD,POINTS,PSIZE,SIGMA,RESULT,CONS,
     :                     GRAD,STATUS)
*+
*  Name:
*     GRA1_GRAPH

*  Purpose:
*     Displays the graphs on the requested graphics device.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL GRA1_GRAPH(GMODE,ANGCON,ANGOFF,FTYPE,ZEROP,RADISP,
*                      WHATD,POINTS,PSIZE,SIGMA,RESULT,CONS,GRAD,STATUS)

*  Description:
*      Displays the graphical output from the program. This consists of
*      a graph showing the radius (or some transformation thereof) versus
*      the mean pixel brightness/intensity in terms of Log10(I)
*      or relative to sky I/Sigma.
*
*      It is also possible, depending on the input file type, to show
*      ellipticity, position angle, Fourier descriptors etc versus
*      transformed radius.

*  Arguments:
*     GMODE = INTEGER (Given)
*        Defines whether or not raw data or profile fits are to be
*        displayed at the moment. 1=Raw 2=Fit
*     ANGCON = LOGICAL (Given)
*        Position angle rotation convention. TRUE=clockwise positive.
*     ANGOFF = REAL (Given)
*        Position angle offset. Units degrees.
*     FTYPE *(3) = CHARACTER (Given)
*        The record type found in the results file. ELF=ELLFOU,
*        SEC=SECTOR and ELP=ELLPRO.
*     ZEROP = REAL (Given)
*        Zero point of the brightness scale.
*     RADISP = CHARACTER (Given)
*        Defines the format in whcih the radius will be displayed on
*        the graph. R=linear Q=Quarter power L=Logarithmic S=Square.
*     WHATD *(256) = CHARACTER (Given)
*        Which parameter is to be displayed versus radius.
*     POINTS = INTEGER (Given)
*        Number of raw data points available for the graph.
*     PSIZE = REAL (Given)
*        Pixel size. Units arc seconds.
*     SIGMA = REAL (Given)
*        The standard deviation of the background count value. Units counts.
*     RESULT(GRA__RESUL) = REAL (Given)
*        The summation count for all the data points found at a given
*        distance from the required origin.
*     CONS(2) = REAL (Given)
*        The constant term of the linear equation fit to the radius/brightness
*        results obtained in the most recent profile analysis.
*     GRAD(2) = REAL (Given)
*        The gradient term of the linear equation fit to the radius/brightness
*        results obtained in the most recent profile analysis.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     02-Dec-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'GRA_PAR'               ! GRAPHS constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Arguments Given:
      LOGICAL ANGCON                  ! Position angle convention
      CHARACTER *(3) FTYPE            ! File header type
      CHARACTER *(256) RADISP         ! Denotes the form in which radius
                                      ! values are displayed
      CHARACTER *(256) WHATD          ! What parameter is to be displayed
                                      ! against radius
      INTEGER GMODE                   ! Determines which parts of the
                                      ! graph should be displayed
      INTEGER POINTS                  ! The number of data points
      REAL ANGOFF                     ! Position angle offset
      REAL PSIZE                      ! The size of each pixel in
                                      ! arc seconds
      REAL SIGMA                      ! Standard deviation of the background count
      REAL RESULT(GRA__RESUL,17)      ! Summation of the count value of
                                      ! all the pixels found at a given
                                      ! radius
      REAL CONS(2)                    ! Constant term of the linear regression
                                      ! fit for the data
      REAL GRAD(2)                    ! Gradient term of the linear regression
                                      ! fit for the data
      REAL ZEROP                      ! The zero point of the magnitude
                                      ! scale

*  Arguments Given and Returned:

*  Arguments Returned:

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      CHARACTER *(256) LABELX         ! Graph X axis heading
      CHARACTER *(256) LABELY         ! Graph Y axis heading
      INTEGER FLAG                    ! Indicates whether the transformation
                                      ! radius and brightness to be displayed
                                      ! was possible for the current data
                                      ! point
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Loop variable
      REAL BIGR                       ! Biggest semi-major axis
      REAL BRIGHT                     ! Brightness/count etc at a given
                                      ! distance from the origin
      REAL LOWP                       ! The lowest count/brightness value
      REAL LOWP2                      ! Temporary stoarge of LOWP
      REAL LOWR                       ! The lowest radius value
      REAL HIGHP                      ! The highest count/brightness value
      REAL HIGHP2                     ! Temporary storage of HIGHP
      REAL HIGHR                      ! The highest radius value
      REAL MEAN(1)                    ! Y axis value to display
      REAL RAD(1)                     ! X axis value to display
      REAL RADIUS                     ! Distance from the origin in arc sec
      REAL TEMP                       ! Temporary variable
      REAL X                          ! The transformed radius value
      REAL X1                         ! Viewport limit
      REAL X2                         ! Viewport limit
      REAL Y1                         ! Viewport limit
      REAL Y2                         ! Viewport limit
      REAL Y(2)                       ! Transformed brightness value
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Raw data display.
      IF (GMODE.EQ.1) THEN

*      Set the minimum and maximum initial values.
         LOWR=VAL__MAXR
         HIGHR=VAL__MINR
         LOWP=VAL__MAXR
         HIGHP=VAL__MINR

*      Loop through all the data points.
         DO 30 I=1,POINTS

*            Convert the radius data to the right form.
               IF ((FTYPE.EQ.'ELF').OR.(FTYPE.EQ.'ELP')) THEN
                  RADIUS=PSIZE*SQRT(RESULT(I,4)*RESULT(I,4)*RESULT(I,7))
               ELSE
                  RADIUS=RESULT(I,4)*PSIZE
               END IF

*            Set the parameter value required for the current
*            value of WHATD.
               IF (WHATD.EQ.'S') BRIGHT=RESULT(I,5)
               IF (WHATD.EQ.'B') BRIGHT=RESULT(I,5)
               IF (WHATD.EQ.'C') BRIGHT=RESULT(I,5)
               IF (WHATD.EQ.'E') BRIGHT=RESULT(I,7)
               IF (WHATD.EQ.'P') THEN
                  IF (ANGCON) THEN
                     BRIGHT=RESULT(I,6)+ANGOFF
                  ELSE
                     BRIGHT=-RESULT(I,6)+ANGOFF
                  END IF
               END IF
               IF (WHATD.EQ.'X') BRIGHT=RESULT(I,1)
               IF (WHATD.EQ.'Y') BRIGHT=RESULT(I,2)

*            Do the same in the case of Fourier descriptors.
               IF (WHATD.EQ.'FS1') BRIGHT=RESULT(I,10)
               IF (WHATD.EQ.'FC1') BRIGHT=RESULT(I,11)
               IF (WHATD.EQ.'FS2') BRIGHT=RESULT(I,12)
               IF (WHATD.EQ.'FC2') BRIGHT=RESULT(I,13)
               IF (WHATD.EQ.'FS3') BRIGHT=RESULT(I,14)
               IF (WHATD.EQ.'FC3') BRIGHT=RESULT(I,15)
               IF (WHATD.EQ.'FS4') BRIGHT=RESULT(I,16)
               IF (WHATD.EQ.'FC4') BRIGHT=RESULT(I,17)

*            Transfer the required values to the conversion subroutine.
               CALL GRA1_CONV(RADISP,WHATD,SIGMA,ZEROP,FLAG,
     :                        RADIUS,BRIGHT,STATUS)

*            Use the results if they were legal.
               IF (FLAG.EQ.0) THEN
                  IF (RADIUS.LT.LOWR) LOWR=RADIUS
                  IF (RADIUS.GT.HIGHR) HIGHR=RADIUS
                  IF (BRIGHT.LT.LOWP) LOWP=BRIGHT
                  IF (BRIGHT.GT.HIGHP) HIGHP=BRIGHT
               END IF

 30      CONTINUE

*      Set up the display using the limits calculated. Limits are
*      defined by the values of WHATD and RADISP.
         LOWP2=LOWP
         HIGHP2=HIGHP
         IF (WHATD.EQ.'S') THEN
            HIGHP=HIGHP+.25
            LOWP=LOWP-.25
            TEMP=HIGHP
            HIGHP=LOWP
            LOWP=TEMP
         ELSE
            IF (HIGHP.GT.0.0) THEN
               HIGHP=HIGHP*1.10
            ELSE
               HIGHP=HIGHP*0.9
            END IF
            IF (LOWP.GT.0.0) THEN
               LOWP=LOWP*0.9
            ELSE
               LOWP=LOWP*1.1
            END IF
         END IF
         HIGHR=HIGHR*1.05
         CALL PGWINDOW(LOWR,HIGHR,LOWP,HIGHP)

*      Inquire what the viewport size is.
         CALL PGQVP(1,X1,X2,Y1,Y2)

*      Reset the lettering size if necessary.
         IF (((Y2-Y1).LT.1.0).OR.((X2-X1).LT.1.0)) THEN
            IF ((Y2-Y1).LT.(X2-X1)) THEN
               TEMP=(Y2-Y1)/2.
            ELSE
               TEMP=(X2-X1)/2.
            END IF
            CALL PGSCH(TEMP)
         END IF

*      Set up the labelling marks on the axes.
         CALL PGBOX('ABCGNST',0.0,0,'ABCGNST',0.0,0)

*      Set up values for and display the labels for the graph.

*      Set up the Y axis labels.

*      Normal parameters.
         IF (WHATD.EQ.'B') LABELY='(I-BACK)/Sigma'
         IF (WHATD.EQ.'C') LABELY='Counts'
         IF (WHATD.EQ.'E') LABELY='Ellipticity'
         IF (WHATD.EQ.'P') LABELY='Position angle'
         IF (WHATD.EQ.'X') LABELY='X co-ordinate'
         IF (WHATD.EQ.'Y') LABELY='Y co-ordinate'
         IF (WHATD.EQ.'S') LABELY='Surface Brightness Zp-2.5Log(I)'

*      Fourier descriptors.
         IF (WHATD.EQ.'FS1') LABELY='Sine(theta)'
         IF (WHATD.EQ.'FC1') LABELY='Cos(theta)'
         IF (WHATD.EQ.'FS2') LABELY='Sine(2xtheta)'
         IF (WHATD.EQ.'FC2') LABELY='Cos(2xtheta)'
         IF (WHATD.EQ.'FS3') LABELY='Sine(3xtheta)'
         IF (WHATD.EQ.'FC3') LABELY='Cos(3xtheta)'
         IF (WHATD.EQ.'FS4') LABELY='Sine(4xtheta)'
         IF (WHATD.EQ.'FC4') LABELY='Cos(4xtheta)'

*      Setup the X axis labels.
         IF ((FTYPE.EQ.'ELF').OR.(FTYPE.EQ.'ELP')) THEN
            IF (RADISP.EQ.'R') LABELX='Radius(*) (arc seconds)'
            IF (RADISP.EQ.'Q') LABELX='Power Radius(*) (**0.25)'
            IF (RADISP.EQ.'L') LABELX='Logarithmic Radius(*)'
            IF (RADISP.EQ.'S') LABELX='Radius(*) Squared'
         ELSE
            IF (RADISP.EQ.'R') LABELX='Radius (arc seconds)'
            IF (RADISP.EQ.'Q') LABELX='Power Radius (**0.25)'
            IF (RADISP.EQ.'L') LABELX='Logarithmic Radius'
            IF (RADISP.EQ.'S') LABELX='Radius Squared'
         END IF

*      Display the labels of the graph.
         CALL PGLABEL(LABELX,LABELY,'GALAXY PROFILE')

*      Display the data points in the RADISP mode required.
         DO 40 I=1,POINTS

*            Set up a flag. If greater than 0 then an error was found.
*            Display is then stopped for the current radius value.
               FLAG=0

*            Convert the radius data to the right form.
               IF ((FTYPE.EQ.'ELF').OR.(FTYPE.EQ.'ELP')) THEN
                  RADIUS=PSIZE*SQRT(RESULT(I,4)*RESULT(I,4)*RESULT(I,7))
               ELSE
                  RADIUS=RESULT(I,4)*PSIZE
               END IF

*            Set the parameter value required for the current
*            value of WHATD.
               IF (WHATD.EQ.'S') BRIGHT=RESULT(I,5)
               IF (WHATD.EQ.'B') BRIGHT=RESULT(I,5)
               IF (WHATD.EQ.'C') BRIGHT=RESULT(I,5)
               IF (WHATD.EQ.'E') BRIGHT=RESULT(I,7)
               IF (WHATD.EQ.'P') THEN
                  IF (ANGCON) THEN
                     BRIGHT=RESULT(I,6)+ANGOFF
                  ELSE
                     BRIGHT=-RESULT(I,6)+ANGOFF
                  END IF
               END IF
               IF (WHATD.EQ.'X') BRIGHT=RESULT(I,1)
               IF (WHATD.EQ.'Y') BRIGHT=RESULT(I,2)

*            Do the same in the case of Fourier descriptors.
               IF (WHATD.EQ.'FS1') BRIGHT=RESULT(I,10)
               IF (WHATD.EQ.'FC1') BRIGHT=RESULT(I,11)
               IF (WHATD.EQ.'FS2') BRIGHT=RESULT(I,12)
               IF (WHATD.EQ.'FC2') BRIGHT=RESULT(I,13)
               IF (WHATD.EQ.'FS3') BRIGHT=RESULT(I,14)
               IF (WHATD.EQ.'FC3') BRIGHT=RESULT(I,15)
               IF (WHATD.EQ.'FS4') BRIGHT=RESULT(I,16)
               IF (WHATD.EQ.'FC4') BRIGHT=RESULT(I,17)

*            Transfer the required values to the conversion subroutine.
               CALL GRA1_CONV(RADISP,WHATD,SIGMA,ZEROP,FLAG,
     :                        RADIUS,BRIGHT,STATUS)

*            Display the result if it is possible. An arrow is displayed
*            indicating the radius at which a value has occured for which
*            the Y co-ordinate cannot be calculated.
*            If FLAG=0 then both x and y values may be displayed.
*            If FLAG=1 then the Y axis value cannot be calculated.
*            IF FLAG=-1 the X axis value could not be calculated so
*            no display is possible.
               IF (FLAG.NE.-1) THEN

                  IF ((RADIUS.GE.LOWR).AND.(RADIUS.LE.HIGHR)) THEN
                     IF (FLAG.EQ.0) THEN
*                     Display radius and brightness value.
                        IF ((BRIGHT.GE.LOWP2).AND.
     :                       (BRIGHT.LE.HIGHP2)) THEN
                           RAD(1)=RADIUS
                           MEAN(1)=BRIGHT
                           CALL PGPOINT(1,RAD,MEAN,4)
                        END IF
                     ELSE
*                     Display the radius value only.
                        IF ((BRIGHT.GE.LOWP2).AND.
     :                       (BRIGHT.LE.HIGHP2)) THEN
                           RAD(1)=RADIUS
                           MEAN(1)=LOWP2+(HIGHP2-LOWP2)*.05
                           CALL PGPOINT(1,RAD,MEAN,31)
                        END IF
                     END IF

                  END IF

               END IF

 40      CONTINUE

      END IF

*  show both spiral and elliptical fits.
      IF (GMODE.EQ.2) THEN

*      Find biggest radius to be considered (use semi major axis).
         BIGR=0.0
         DO 100 J=1,POINTS
            IF (RESULT(J,4).GT.BIGR) BIGR=RESULT(J,4)
 100     CONTINUE
         BIGR=BIGR*1.10

*      Display the fits at reasonable resolution.
         DO 50 I=1,500

*         Calculate the fit values and display.

*         Find the fit value at each radius value.
            X=BIGR/500.*REAL(I)

            DO 60 J=1,2

*            Calculate brightness value at this radius.
               IF (J.EQ.1) THEN
                  Y(J)=GRAD(J)*X+CONS(J)
               ELSE
                  Y(J)=GRAD(J)*(X**(0.25))+CONS(J)
               END IF

 60         CONTINUE

*         Calculate radius value on current display.
            IF (RADISP.EQ.'R') X=X
            IF (RADISP.EQ.'Q') X=X**(0.25)
            IF (RADISP.EQ.'L') X=LOG10(X)
            IF (RADISP.EQ.'S') X=X*X

*         Convert the intensity value to sigma or surface brightness.
            DO 65 J=1,2

*            Convert to brightness the current display.
               IF (WHATD.EQ.'B') Y(J)=(10.**Y(J))/SIGMA
               IF (WHATD.EQ.'S') Y(J)=ZEROP-2.5*Y(J)
               IF (WHATD.EQ.'C') Y(J)=10.**Y(J)

*            Display.
               RAD(1)=X
               MEAN(1)=Y(J)
               CALL PGPOINT(1,RAD,MEAN,1+(J-1)*19)

 65         CONTINUE

 50      CONTINUE

      END IF


 9999 CONTINUE

      END


      SUBROUTINE HIS1_GRAPH(ADEV,BARRAY,HIGHB,LOW,MODE,MEDIAN,
     :                     SDEV,SMOBAR,BARSIZ,HEIG,X1,Y1,NUMDAT,
     :                     BINWID,STATUS)
*+
*  Name:
*     HIS1_GRAPH

*  Purpose:
*     Create the screen graphics of the histogram.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HIS1_GRAPH(ADEV,BARRAY,HIGHB,LOW,MODE,MEDIAN,
*                    SDEV,SMOBAR,BARSIZ,HEIG,X1,Y1,NUMDAT,
*                    BINWID,STATUS)

*  Description:
*     Draws the histogram, displays axes and plots the
*     raw and smoothed data.

*  Arguments:
*     ADEV = DOUBLE PRECISION (Given)
*        Absolute deviation of the image pixels count distribution.
*        Units counts.
*     BARRAY(BARSIZ) = DOUBLE PRECISION (Given)
*        Array used to bin pixel values.
*     HIGHB = DOUBLE PRECISION (Given)
*        Highest value found in BARRAY.
*     LOW = REAL (Given)
*        Lowest count value in the image. Also used as an array
*        offset index for the arrays SMOBAR and BARRAY. Units counts.
*     MODE(4) = DOUBLE PRECISION (Given)
*        Estimated values for the mode value of the image pixel
*        count distribution. Units counts.
*     MEDIAN = DOUBLE PRECISION (Given)
*        The estimated median value for the image pixel counts.
*        Units counts.
*     SDEV(2) = DOUBLE PRECISION (Given)
*        Standard deviation of the image pixel count distribution
*        and the background count standard deviation value. Units counts.
*     SMOBAR(BARTSIZ) = DOUBLE PRECISION (Given)
*        Smoothed version of BARRAY.
*     BARSIZ = INTEGER (Given)
*        Size (no. of elements) of the binning arrays used.
*     HEIG(HIS__CHORM) = REAL (Given)
*        The height at which the chord through the histogram occurs.
*     X1(HIS__CHORM) = REAL (Given)
*        Histogram chord information.
*     Y1(HIS__CHORM) = REAL (Given)
*        Histogram chord information.
*     NUMDAT = INTEGER (Given)
*        Number of histogram chords to display.
*     BINWID = REAL (Given)
*        Width of each element of the binning array. Units counts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     8-June-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'HIS_PAR'               ! HISTPEAK system variables

*  Arguments Given:
      INTEGER BARSIZ                  ! Size of the binning arrays used
      INTEGER NUMDAT                  ! Number of histogram chords to
                                      ! be displayed
      REAL BINWID                     ! Width of each element of the
                                      ! binning array
      REAL HEIG(HIS__CHORM)           ! The values at which chords
                                      ! (slices) were taken through the
                                      ! histogram
      REAL LOW                        ! Binning arrays origin offset and
                                      ! lowest value found in image
      REAL X1(HIS__CHORM)             ! Chord length information
      REAL Y1(HIS__CHORM)             ! Chord centre information
      DOUBLE PRECISION ADEV           ! Absolute deviation of the pixels
      DOUBLE PRECISION BARRAY(BARSIZ) ! Binning array for the pixel
                                      ! values
      DOUBLE PRECISION HIGHB          ! Highest element used in the
                                      ! binning array
      DOUBLE PRECISION MODE(4)        ! Mode value for the data
      DOUBLE PRECISION MEDIAN         ! Median value for the data
      DOUBLE PRECISION SDEV(2)        ! Background standard deviation
      DOUBLE PRECISION SMOBAR(BARSIZ) ! Smoothed version of BARRAY

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Temporary loop variable
      INTEGER J                       ! Lowest bin array element
                                      ! to display
      INTEGER K                       ! Highest bin array element
                                      ! to display

      REAL AV1                        ! Lowest pixel value to be
                                      ! displayed
      REAL AV2                        ! Highest pixel value to be
                                      ! displayed
      REAL AV3                        ! Temporary value
      REAL PGX(2)                     ! X indices of points to be
                                      ! plotted
      REAL PGY(2)                     ! Y indices of points to be
                                      ! plotted
      REAL VALUE                      ! Temporary variable

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Calculate sensible bounds for displaying the histogram.

*   Derive mean mode value.
      VALUE=0.0
      I=0
      DO 27 J=1,4

*      Ignore the projected value.
         IF (J.NE.3) THEN
            IF (ABS(MODE(J)).GT.HIS__VSMAL) THEN
               I=I+1
               VALUE=VALUE+MODE(J)
            END IF
         END IF

 27   CONTINUE

*   Abort if histogram was empty.
      IF (I.EQ.0) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','No mode value has been calculated.',STATUS)
         GOTO 9999
      END IF
      VALUE=VALUE/REAL(I)

*   Estimate the approx. window of values to display.
      IF (SDEV(2).LT.HIS__VSMAL) THEN
         AV1=VALUE-3.*ADEV
         AV2=VALUE+3.*ADEV
      ELSE
         AV1=VALUE-3.*(SDEV(2)+ADEV)/2.
         AV2=VALUE+4.*(SDEV(2)+ADEV)/2.
      END IF
      AV3=REAL(HIGHB)

*   Display the histogram axes.
      CALL PGWINDOW(AV1,AV2,0.0,AV3)
      CALL PGBOX('ABCGNST',0.0,0,
     :           'ABCGNST',0.0,0)

*   Label the histogram axes.
      CALL PGLABEL('Pixel count value','Number of pixels',
     :             'Histogram of pixel count value versus frequency.')

*   Display the raw histogram data if reasonably near the mode.
      J=NINT((AV1-LOW)/BINWID)+1
      K=NINT((AV2-LOW)/BINWID)+1
      IF (J.LT.1) J=1
      IF (K.GT.BARSIZ) K=BARSIZ
      DO 29 I=J,K
         IF (BARRAY(I).GT.0.0) THEN
            PGX(1)=LOW+(I-1)*BINWID
            PGY(1)=BARRAY(I)
            CALL PGPOINT(1,PGX,PGY,1)
         END IF
 29   CONTINUE

*   Display the smoothed histogram data if reasonably near the mode.
      IF (ABS(MODE(2)).GT.HIS__VSMAL) THEN
         VALUE=MODE(2)
      ELSE
         VALUE=MODE(1)
      END IF
      J=NINT((VALUE-ADEV-LOW)/BINWID)+1
      K=NINT((VALUE+ADEV-LOW)/BINWID)+1
      IF (J.LT.1) J=1
      IF (K.GT.BARSIZ) K=BARSIZ
      DO 292 I=J,K
         IF (SMOBAR(I).GT.0.0) THEN
            PGX(1)=LOW+(I-1)*BINWID
            PGY(1)=SMOBAR(I)
            CALL PGPOINT(1,PGX,PGY,20)
         END IF
 292  CONTINUE

*   Display the sections through the histogram used to
*   extrapolate the zero width of chords through the histogram
*   and hence determine a modal estimate.
      DO 430 I=1,NUMDAT

*      The chord itself.
         PGX(1)=Y1(I)-X1(I)*X1(I)
         PGY(1)=HEIG(I)
         PGX(2)=Y1(I)+X1(I)*X1(I)
         PGY(2)=HEIG(I)
         CALL PGLINE(2,PGX,PGY)

*      Centre of the chord.
         PGX(1)=Y1(I)
         PGY(1)=HEIG(I)-.01*HIGHB
         PGX(2)=Y1(I)
         PGY(2)=HEIG(I)+.01*HIGHB
         CALL PGLINE(2,PGX,PGY)

 430  CONTINUE

 9999 CONTINUE

      END


      SUBROUTINE SEC1_GRAPH(GMODE,ZEROP,RADISP,SURF,RLIM,BACK,
     :                     NUMBER,PSIZE,SIGMA,SUMMAT,CONS,
     :                     GRAD,STATUS)
*+
*  Name:
*     SEC1_GRAPH

*  Purpose:
*     Displays the graphs on the requested graphics device.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL SEC1_GRAPH(GMODE,ZEROP,RADISP,SURF,RLIM,BACK,NUMBER,
*                      PSIZE,SIGMA,SUMMAT,CONS,GRAD,STATUS)

*  Description:
*      Displays the graphical output from the program. This consists of
*      a graph showing the radius (or some transformation thereof) versus
*      the mean pixel brightness/intensity in terms of Log10(I-BACK)
*      or relative to sky.

*  Arguments:
*     GMODE = INTEGER (Given)
*        Defines whether or not raw data or profile fits are to be
*        displayed at the moment. 1=Raw 2=Fit
*     ZEROP = REAL (Given)
*        Zero point of the brightness scale.
*     RADISP = CHARACTER (Given)
*        Defines the format in whcih the radius will be displayed on
*        the graph. R=linear Q=Quarter power L=Logarithmic S=Square.
*     SURF = LOGICAL (Given)
*        Defines the format in which the brightness is displayed.
*        FALSE=Sigma TRUE=Surface brightness
*     RLIM = INTEGER (Given)
*        Number of raw data points available for the graph.
*     BACK = REAL (Given)
*        The background sky count value. Units counts.
*     NUMBER(SEC__RESUL) = REAL (Given)
*        The array containing the number of pixels found at a given
*        distance from the required origin.
*     PSIZE = REAL (Given)
*        Pixel size. Units arc seconds.
*     SIGMA = REAL (Given)
*        The standard deviation of the background count value. Units counts.
*     SUMMAT(SEC__RESUL) = REAL (Given)
*        The summation count for all the data points found at a given
*        distance from the required origin (see NUMBER).
*     CONS(2) = REAL (Given)
*        The constant term of the linear equation fit to the radius/brightness
*        results obtained in the most recent profile analysis.
*     GRAD(2) = REAL (Given)
*        The gradient term of the linear equation fit to the radius/brightness
*        results obtained in the most recent profile analysis.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     02-Dec-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'SEC_PAR'               ! SECTOR constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Arguments Given:
      CHARACTER *(256) RADISP         ! Denotes the form in which radius
                                      ! values are displayed
      LOGICAL SURF                    ! Defines the form in which brightness
                                      ! values are displayed
      INTEGER GMODE                   ! Determines which parts of the
                                      ! graph should be displayed
      INTEGER RLIM                    ! The number of data points ie
                                      ! radii at which brightness
                                      ! was determined
      REAL BACK                       ! Background count value
      REAL NUMBER(SEC__RESUL)         ! The number of pixels found at
                                      ! a given radius
      REAL PSIZE                      ! The size of each pixel in
                                      ! arc seconds
      REAL SIGMA                      ! Standard deviation of the background
      REAL SUMMAT(SEC__RESUL)         ! Summation of the count value of
                                      ! all the pixels found at a given
                                      ! radius
      REAL CONS(2)                    ! Constant term of the linear regression
                                      ! fit for the data
      REAL GRAD(2)                    ! Gradient term of the linear regression
                                      ! fit for the data
      REAL ZEROP                      ! The zero point of the magnitude
                                      ! scale

*  Arguments Given and Returned:

*  Arguments Returned:

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      CHARACTER *(256) LABELX         ! Graph X axis heading
      CHARACTER *(256) LABELY         ! Graph Y axis heading
      INTEGER FLAG                    ! Indicates whether the transformation
                                      ! radius and brightness to be displayed
                                      ! was possible for the current data
                                      ! point
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Loop variable
      REAL BRIGHT                     ! Brightness/count etc at a given
                                      ! distance from the origin
      REAL LOWP                       ! The lowest count/brightness value
      REAL LOWR                       ! The lowest radius value
      REAL HIGHP                      ! The highest count/brightness value
      REAL HIGHR                      ! The highest radius value
      REAL MEAN(1)                    ! Y axis value to display
      REAL RAD(1)                     ! X axis value to display
      REAL RADIUS                     ! Distance from the origin in arc sec
      REAL TEMP                       ! Temporary variable
      REAL X                          ! The transformed radius value
      REAL X1                         ! Viewport limit
      REAL X2                         ! Viewport limit
      REAL Y1                         ! Viewport limit
      REAL Y2                         ! Viewport limit
      REAL Y(2)                       ! Transformed brightness value
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

      IF (GMODE.EQ.1) THEN

*      Set the minimum and maximum initial values.
         LOWR=VAL__MAXR
         HIGHR=VAL__MINR
         LOWP=VAL__MAXR
         HIGHP=VAL__MINR

*      Loop through all the data points.
         DO 30 I=1,RLIM

*         Only look at radii for which data was found.
            IF (NUMBER(I).GT.0.0) THEN

*            Convert the radius data to the right form.
               RADIUS=(REAL(I)-1.)*PSIZE

*            Convert the count data to the mean value.
               BRIGHT=SUMMAT(I)/NUMBER(I)

*            Transfer the required values to the conversion subroutine.
               CALL SEC1_CONV(RADISP,SURF,BACK,SIGMA,ZEROP,FLAG,
     :                        RADIUS,BRIGHT,STATUS)

*            Use the results if they were legal.
               IF (FLAG.EQ.0) THEN
                  IF (RADIUS.LT.LOWR) LOWR=RADIUS
                  IF (RADIUS.GT.HIGHR) HIGHR=RADIUS
                  IF (BRIGHT.LT.LOWP) LOWP=BRIGHT
                  IF (BRIGHT.GT.HIGHP) HIGHP=BRIGHT
               END IF

            END IF

 30      CONTINUE

*      Set up the display using the limits calculated. Limits are
*      defined by the values of SURF and RADISP.
         IF (SURF) THEN
            CALL PGWINDOW(LOWR,HIGHR,HIGHP,LOWP)
         ELSE
            CALL PGWINDOW(LOWR,HIGHR,LOWP,HIGHP)
         END IF

*      Inquire what the viewport size is.
         CALL PGQVP(1,X1,X2,Y1,Y2)

*      Reset the lettering size if necessary.
         IF (((Y2-Y1).LT.2.5).OR.((X2-X1).LT.2.5)) THEN
            IF ((Y2-Y1).LT.(X2-X1)) THEN
               TEMP=(Y2-Y1)/2.
            ELSE
               TEMP=(X2-X1)/2.
            END IF
            CALL PGSCH(TEMP)
         END IF

*      Set up the labelling marks on the axes.
         CALL PGBOX('ABCGNST',0.0,0,'ABCGNST',0.0,0)

*      Set up values for and display the labels for the graph.

*      Set up the Y axis labels.
         IF (.NOT.SURF) THEN
            LABELY='(I-Back)/Sigma'
         ELSE
            IF (((Y2-Y1).LT.2.5).OR.((X2-X1).LT.2.5)) THEN
               LABELY='SB Zp-2.5Log(I-Back)'
            ELSE
               LABELY='Surface Brightness Zp-2.5Log(I-Back)'
            END IF
         END IF

*      Setup the X axis labels.
         IF (RADISP.EQ.'R') LABELX='Radius (arc seconds)'
         IF (RADISP.EQ.'Q') LABELX='Power Radius (R**0.25)'
         IF (RADISP.EQ.'L') LABELX='Logarithmic Radius'
         IF (RADISP.EQ.'S') LABELX='Radius Squared'

*      Display the labels of the graph.
         CALL PGLABEL(LABELX,LABELY,'GALAXY PROFILE')

*      Display the data points in the RADISP mode required.
         DO 40 I=1,RLIM

*         Do not display if there is no pixel at the current radius.
            IF (NUMBER(I).GT.0) THEN

*            Set up a flag. If greater than 0 then an error was found.
*            Display is then stopped for the current radius value.
               FLAG=0

*            Convert the radius data to the right form.
               RADIUS=(REAL(I)-1.)*PSIZE

*            Convert the count data to the mean value.
               BRIGHT=SUMMAT(I)/NUMBER(I)

*            Transfer the required values to the conversion subroutine.
               CALL SEC1_CONV(RADISP,SURF,BACK,SIGMA,ZEROP,FLAG,
     :                        RADIUS,BRIGHT,STATUS)

*            Display the result if it is possible. An arrow is displayed
*            indicating the radius at which a value has occured for which
*            the Y co-ordinate cannot be calculated.
*            If FLAG=0 then both x and y values may be displayed.
*            If FLAG=1 then the Y axis value cannot be calculated.
*            IF FLAG=-1 the X axis value could not be calculated so
*            no display is possible.
               IF (FLAG.NE.-1) THEN
                  IF ((RADIUS.GE.LOWR).AND.(RADIUS.LE.HIGHR)) THEN
                     IF (FLAG.EQ.0) THEN
*                     Display radius and brightness value.
                        IF ((BRIGHT.GE.LOWP).AND.(BRIGHT.LE.HIGHP)) THEN
                           RAD(1)=RADIUS
                           MEAN(1)=BRIGHT
                           CALL PGPOINT(1,RAD,MEAN,4)
                        END IF
                     ELSE
*                     Display the radius value only.
                        IF ((BRIGHT.GE.LOWP).AND.(BRIGHT.LE.HIGHP)) THEN
                           RAD(1)=RADIUS
                           MEAN(1)=LOWP+(HIGHP-LOWP)*.05
                           CALL PGPOINT(1,RAD,MEAN,31)
                        END IF
                     END IF

                  END IF

               END IF
            END IF

 40      CONTINUE

      END IF


*  Display the 'fits' to the data points.

      IF (GMODE.EQ.2) THEN

*      Display the fits at .2*PSIZE resolution.
         DO 50 I=2,RLIM*5

*         Calculate the fit values and display.

*         Perform the radius transform.
            DO 60 J=1,2
               X=(REAL(I)-1.)*PSIZE/5.
               IF (J.EQ.1) THEN
                  Y(J)=GRAD(J)*X+CONS(J)
               ELSE
                  Y(J)=GRAD(J)*(X**(0.25))+CONS(J)
               END IF
 60         CONTINUE

*         Calculate their equivalent position on the current display.
            IF (RADISP.EQ.'R') RAD(1)=X
            IF (RADISP.EQ.'Q') RAD(1)=X**(0.25)
            IF (RADISP.EQ.'L') RAD(1)=LOG10(X)
            IF (RADISP.EQ.'S') RAD(1)=X*X

*         Convert the intensity value to sigma or surface brightness.
            DO 65 J=1,2

               IF (.NOT.SURF) THEN
*               Sigma.
                  MEAN(1)=(10.**Y(J))/SIGMA
               ELSE
*               Surface brightness.
                  MEAN(1)=ZEROP-2.5*Y(J)
               END IF

*           Check that the value is inside the window for display.
               CALL PGPOINT(1,RAD,MEAN,1+(J-1)*19)

 65         CONTINUE

 50      CONTINUE

      END IF


 9999 CONTINUE

      END
