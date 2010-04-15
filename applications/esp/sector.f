      SUBROUTINE SECTOR(STATUS)
*+
*  Name:
*     SECTOR

*  Purpose:
*     May be used to display the average pixel values within a wedge
*     shaped sector/slice of the image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SECTOR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     May be used to display the average pixel values within a wedge
*     shaped sector/slice of the image. The sector is in the form of a
*     wedge (of user defined size) drawn outward from the galaxy
*     origin point.
*
*     The results are displayed as mean pixel value (in terms of level
*     relative to sky or surface brightness) versus distance from the
*     galaxy origin. Pixel count values are summed over all the pixels
*     at a given distance from the origin.
*
*     Options include:
*       - summing pixels taken  from two equal sized, but
*         diametrically opposite, sectors.
*       - displaying data using a number of possible radius
*         transformations.
*       - the use of a graphics cursor to select the image object
*         to be examined.
*       - refinement of approximate galaxy centre positions
*         if required.
*       - automatic selection of the maximum radius out
*         from the origin to be considered.
*
*     The application is not intended to replace ELLPRO or ELLFOU
*     profiling application, but merely to allow the user to obtain
*     quickly a first approximation to the brightness cross-section of
*     an interactively selected galaxy.

*  Usage:
*     SECTOR CURSOR ARDFIL BACK SIGMA PSIZE SURF RADISP MIRROR AUTOL
*            ZEROP OUT [IN] [DEVICE] [IMGDEV] [FITLIM] [POSANG]
*            [ANGWID] [RLIM] [SAME] [AGAIN] [ORIGIN] (COLOUR)

*  ADAM Parameters:
*     ABOBEL = _REAL (Write)
*        Central brightness of the object relative to sky.
*     AGAIN = _LOGICAL(Read)
*        Should another profile be attempted?
*     ANGWID = _REAL (Read)
*        The angular width of the slice/wedge/sector to be considered.
*        Units degrees.
*     ARDFIL = _CHAR (Read)
*        The name of an ARD file to be used to mask out regions of the
*        image that are not to be used.
*     AUTOL = _LOGICAL
*        Is a simple method to be applied to get a better
*        estimate of the galaxy centre position?
*        The accuracy of the method used is no better than 1 pixel.
*     BACK = _REAL (Read)
*        The background value for the image. Units counts.
*     COLOUR = _INTEGER (Read)
*        Colour used when showing the galaxy centre and profiling radius.
*     CURSOR = _LOGICAL (Read)
*        Whether the galaxy location is to be identified using the
*        graphics cursor or the keyboard.
*     DEVICE = _DEVICE (Read)
*        The name of the display device on which the results graphs
*        should be displayed.
*     FITLIM = _REAL (Read)
*        The range of radius values over which the scale length 'fits'
*        are to be calculated.  Units arc seconds.
*     IMGDEV = _DEVICE (Read)
*        Name of the graphics device displaying the image.
*     IN = _NDF (Read)
*        The name of the source NDF data structure/file.
*     MIRROR = _LOGICAL (Read)
*        Whether the summation is to be taken from two
*        sectors/wedges/slices of the same size, but on
*        diametrically opposite sides of the galaxy origin.
*     ORIGIN = _CHAR (Read)
*        Image indices for the origin point to be used. Given in the
*        Current coordinate system of the WCS component of IN.
*     PORIGIN = _CHAR (Read)
*        Image indices for the origin point to be used, in pixel units.
*        This parameter is present to aid the interface with the GAIA system.
*        It should be regarded as an `internal' parameter, and may disappear
*        or change without notice.  If present, the value of this parameter
*        overrides any value specified by the ORIGIN parameter.
*     OCOUNT = _REAL (Write)
*        Count value for the object centre chosen.
*     OUT = _CHAR (Read)
*        File name for the output text file containing the
*        profile data.
*     POSANG = _REAL (Read)
*        The position angle of the sector relative to the top of the
*        image. Convention is clockwise increases angle and the image
*        Y axis represents 0 degrees. Units degrees.
*     PSIZE = _REAL (Read)
*        The size of each pixel in arc seconds.  If the image contains
*        a SKY co-ordinate frame this value will be determined
*        automatically.
*     RADISP = _CHAR (Read)
*        The display mode used for the radius axis of the graphs.
*         - Q=quarter power
*         - L=logarithmic
*         - S=square root
*         - R=linear
*     RLIM = _INTEGER (Read)
*        Distance out from the origin at which the sector stops. Values
*        are input as arc seconds, but the program works in pixels.
*        A value of 0 causes the application to automatically select
*        the distance at which to stop.
*     SAME = _LOGICAL (Read)
*        Should the graphs be displayed on the same device as the
*        original image?
*     SIGMA = _REAL (Read)
*        The standard deviation of the background value. Units counts.
*     SLENE = _REAL (Write)
*        The scale length of the object (elliptical model).
*     SLENS = _REAL (Write)
*        The scale length of the object (spiral model).
*     SURF = _LOGICAL (Read)
*        Are the pixel values to be expressed as surface brightness.
*        If true then the output is surface brightness, otherwise the
*        display shows brightness in terms of sigma above sky.
*        i.e. (I-Back)/SIGMA
*     XCO = _REAL (Write)
*        The X co-ordinate of the object the user chose.
*     YCO = _REAL (Write)
*        The Y co-ordinate of the object the user chose.
*     ZEROP = _REAL (Read)
*        Zero point of the scale for surface brightness plots. Units
*        magnitudes per square arc second.

*  Examples:
*     sector cursor=true ardfil=^ardup.dat back=6200 sigma=390
*            psize=0.96 surf=true radisp=r mirror=true autol=true
*            zerop=27.5 out=x2windows device=x2windows
*            imgdev=xwindows same=false
*
*        Profiles an object identified on the currently displayed
*        image using a cursor/mouse. The resulting profile is displayed
*        as linear radius versus surface brightness. ARD file ARDUP is
*        used to identify parts of the image that may not be used.
*        The source image is currently displayed on device XWINDOW and
*        the graphs will appear on device X2WINDOW. The galaxy centre
*        co-ordinate identified is refined automatically. The radius
*        limits to be employed when calculating scale length are
*        defined using the cursor/mouse.
*
*     sector cursor=false ardfil=^ardfile.dat back=760 sigma=23
*            surf=true radisp=q mirror=false autol=false
*            zerop=26.4 in=ic3374 out=ic3374.pro device=xwindows
*            fitlim=0,20 posang=25 angwid=5
*            rlim=25 origin="12:36:53.42 62:12:21.8"
*
*        An object located at the co-ordinates indicated on image IC3374
*        is profiled in the 25 degree direction out to a distance of 25
*        arc seconds. The Current co-ordinate frame of IC3374 is in
*        the SKY domain. The pixel size in arcseconds is determined
*        automatically from the SKY coordinate frame. The wedge/sector
*        used will be 5 degrees wide and the scale length will be
*        calculated using data obtained in the radius range 0-20 arc
*        seconds. The user supplied estimate of the galaxy centre will
*        not be refined. Output is to the text file ic3374.pro. The
*        graphs generated will be quarter power radius versus surface
*        brightness.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-JUL-1994 (GJP)
*     (Original version)
*     8-NOV-1999 (MBT)
*     Modified to use World Coordinate Systems.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      LOGICAL CURSOR                  ! Keyboard or cursor origin selction

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Show that the application is running.
      CALL MSG_BLANK(STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      CALL MSG_OUT(' ','ESP SECTOR running.',STATUS)

*   Get the user selection of using the cursor or a keyboard?
      CALL PAR_GET0L('CURSOR',CURSOR,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Begin an AST context.
      CALL AST_BEGIN(STATUS)

*   Pass control to an appropriate routine.
      IF (.NOT.CURSOR) THEN
*      Keyboard user input.
         CALL SEC1_KMODE(STATUS)
      ELSE
*      Keyboard and mouse input.
         CALL SEC1_CMODE(STATUS)
      END IF

*   Abort the program.
 9999 CONTINUE

*   Exit AST context.
      CALL AST_END(STATUS)

      END


      SUBROUTINE SEC1_PIE(CLEAR,BACK,ELEMS,XCO,YCO,PRANGE,POSANG,
     :                    ANGWID,NVP,NUMBER,SUMMAT,RLIM,ARRAY,
     :                    STATUS)
*+
*  Name:
*     SEC1_PIE

*  Purpose:
*     Looks at the image array stored and take the values it needs to
*     create an array containing the sum of all the pixels within
*     the required slice that are not bad valued. The index of the
*     array indicates the radial distance from the chosen origin.
*     Each array element may be considered to be PSIZE in width and
*     the radius corresponding to each element is (I-1)*PSIZE since
*     FORTRAN does not allow a zeroth element for an array as default.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SEC1_PIE(CLEAR,BACK,ELEMS,XCO,YCO,PRANGE,POSANG,ANGWID,NVP,
*                   NUMBER,SUMMAT,RLIM,ARRAY,STATUS)

*  Description:
*     The subroutine used to sum the values of all pixels at a given
*     radius in the results array (SUMMAT) and also keep a count
*     of how many data points contribute to that summation in
*     array NUMBER.
*
*     The method used to locate all the points is given below.
*
*     Concentric arc of pixels about the origin are considered
*     in turn. Each arc differs from the previous arc in its radius
*     by one pixel. To ensure that all pixels within an arc are found
*     the arc is scanned from an angle equal to the position angle
*     minus half the angular width to an angle equal to the
*     position angle plus half the angular width of the section. The
*     angular step between points on the arc is adjusted at each new
*     radius value to ensure all pixels are found. The arbitrary steps
*     used have been tested for radii up to 8000.
*
*     When all the pixels within a given arc have been examined the
*     radius used to generate the arc is increased. This increase
*     continues until the requested arc radius has been exceeded or the
*     application detects that the edge of the object has been reached.
*
*     Automatic selection of the maximum required radius out from the
*     origin pixel may be selected by entering a length value of 0.

*  Arguments:
*     CLEAR = INTEGER (Given)
*        Defines whether or not the SUMMAT and NUMBER arrays should
*        be cleared. 0=No 1=Yes.
*     BACK = REAL (Given)
*        The background count value. Units counts.
*     ELEMS = INTEGER (Given)
*        Number of elements/pixels in the image array. Units pixels.
*     XCO = REAL (Given)
*        X index of the galaxy centre/origin supplied by the user.
*        Units pixels.
*     YCO = REAL (Given)
*        Y index of the galaxy centre/origin supplied by the user.
*        Units pixels.
*     PRANGE(2) = INTEGER (Given)
*        Length of the X and Y axes of the image. Units pixels.
*     POSANG = REAL (Given)
*        Position angle of the slice outward from the galaxy origin/centre.
*        Units degrees.
*     ANGWID = REAL (Given)
*        Angular width of the slice. The width is distributed equally either
*        side of the position angle (POSANG). Units degrees.
*     NVP = INTEGER (Returned)
*        The number of valid data points found.
*     NUMBER(SEC__RESUL) = REAL (Returned)
*        The number of pixels that were found at a given radius outward
*        form the galaxy origin/centre. Index is the distance in pixels.
*     SUMMAT(SEC__RESUL) = REAL (Returned)
*        The sum of all the pixels found at a given distance from the
*        galaxy centre/origin. Units counts.
*     ARRAY(ELEMS) = REAL (Given and Returned)
*        The image array. Contains the count values for all the image pixels.
*        Units counts.
*     RLIM = INTEGER (Given and Returned)
*        The radius out from the centre/origin of the galaxy at which
*        the profile slice was stopped. Units pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     1-Dec-1992 (GJP)
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

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER CLEAR                   ! Clear the SUMMAT/NUMBER arrays?
      INTEGER ELEMS                   ! Number of elements/pixels in the
                                      ! image array
      INTEGER PRANGE(2)               ! Length of the X and Y axes of the
                                      ! image
      REAL ANGWID                     ! Angular width of the slice
      REAL BACK                       ! The background count value
      REAL XCO                        ! X index of the galaxy centre/origin
                                      ! supplied by the user
      REAL YCO                        ! Y index of the galaxy centre/origin
                                      ! supplied by the user
      REAL POSANG                     ! Position angle of the slice outward
                                      ! from the galaxy origin/centre

*  Arguments Returned:
      INTEGER NVP                     ! Number of valid points found
      REAL NUMBER(SEC__RESUL)         ! The number of pixels that were found
                                      ! at a given radius outward from the
                                      ! galaxy origin/centre
      REAL SUMMAT(SEC__RESUL)         ! The sum of all the pixels found at
                                      ! a given distance from the
                                      ! galaxy centre/origin

*  Arguments Given and Returned:
      INTEGER RLIM                    ! The radius out from the centre/origin
                                      ! of the galaxy at which
                                      ! the profile slice was stopped
      REAL ARRAY(ELEMS)               ! The image array contains the count
                                      ! values for all the image pixels

*  Local variables:
      LOGICAL FINISH                  ! The highest radius value permitted
                                      ! has been exceeded or some other
                                      ! criteria for stopping the examination
                                      ! of pixels further from the galaxy
                                      ! origin has become true flag
      LOGICAL LTEST                   ! Is the length to be determined
                                      ! automatically?
      INTEGER ADDRES                  ! Array address of the element
                                      ! corresponding to pixel indices X,Y
      INTEGER COUNT                   ! Current positional step (see NUMSEC)
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Temporary counter
      INTEGER MAXRAD                  ! The maximum radius reasonable for
                                      ! the current image
      INTEGER NUMSEC                  ! The total number of positional steps
                                      ! around an arc (of radius=RADIUS) that
                                      ! will be examined
      INTEGER RADIUS                  ! Radius from the galaxy origin (in
                                      ! pixels) at which pixels are being
                                      ! examined
      INTEGER X                       ! Current X index
      INTEGER XMAX                    ! Highest image pixel X index value
      INTEGER XMIN                    ! Lowest image pixel X index value
      INTEGER YMAX                    ! Highest image pixel Y index value
      INTEGER YMIN                    ! Lowest image pixel Y index value
      INTEGER Y                       ! Current Y index
      REAL ANGINC                     ! Angular increment with which the pixels
                                      ! along a given arc (POSANG-ANGWID/2. to
                                      ! POSANG+ANGWID/2.) will be examined
      REAL ANGLE                      ! Angular position currently being
                                      ! examined in the arc from
                                      ! POSANG-ANGWID/2. to POSANG+ANGWID/2.
                                      ! at the current radius value
      REAL ANGWO2                     ! Half the angular width of the slice
                                      ! to be looked at
      REAL CONST                      ! 100./2/pi Defined outside of the
                                      ! loops to improve performance
      REAL MEAN1                      ! Mean count value at a smaller radius
      REAL MEAN2                      ! Mean count value at a larger radius
      REAL PI2                        ! 2 x pi defined outside a loop to
                                      ! improve performance
      REAL PI2360                     ! 2 x pi / 360. defined outside a loop
                                      ! to improve performance
      REAL STAANG                     ! Starting angle for the routine to
                                      ! look for unexamined pixels at a given
                                      ! distance from the galaxy origin
      REAL TOTAL                      ! Temporary summing variable
      REAL VALUE                      ! Current pixel count value
      REAL V1                         ! Temporary storage
      REAL V2                         ! Temporary storage
      REAL V3                         ! Temporary storage

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Clear the number and summation arrays if required.
      IF (CLEAR.EQ.1) THEN
         DO 10 I=1,SEC__RESUL
            SUMMAT(I)=0.0
            NUMBER(I)=0.0
 10      CONTINUE
      END IF

*   Setup some useful constants.
      PI2=2.*SEC__PIVAL
      PI2360=PI2/360.
      CONST=100./PI2
      ANGWO2=ANGWID/2.
      STAANG=POSANG-ANGWO2

*   Set the X and Y limits.
      XMIN=1
      XMAX=PRANGE(1)
      YMIN=1
      YMAX=PRANGE(2)

*   Set up the value for the largest sensible radius.
      V1=REAL(XMAX)
      V2=REAL(YMAX)
      MAXRAD=SQRT(V1*V1+V2*V2)+1

*   Set up a dummy value for the distance from the origin at
*   which the slice should stop. Needed when the program is suppoed to
*   stop automatically.
      IF (RLIM.EQ.0) THEN
         RLIM=SEC__RESUL
         LTEST=.TRUE.
      ELSE
         LTEST=.FALSE.
      END IF

*   Set flag and initial radius value.
      FINISH=.FALSE.
      RADIUS=1

*   Loop until the required radius is found or until (in automatic mode)
*   the mean value at a given radius is either noisy, below sky or increasing.
      DO WHILE ((RADIUS.LE.RLIM).AND.(.NOT.FINISH))

*      Calculate the amount by which the angular position in the wedge
*      must change to ensure that all pixels are accounted for. Also
*      calculate how many angular steps are required then to go from
*      the initial angle STAANG (i.e. POSANG-ANGWID/2) to the other end of
*      the arc STAANG+ANGWID (i.e. POSANG+ANGWID/2).
         ANGINC=CONST/REAL(RADIUS)
         NUMSEC=NINT(ANGWID/ANGINC)
         COUNT=0

*      Consider angles at a given distance from the object centre.
         DO WHILE (COUNT.LE.NUMSEC-1)

*         Calculate the current angle.
            ANGLE=STAANG+COUNT*ANGINC

*         Generate the image co-ordinates of the next image pixel given
*         the current radius and angle.
            X=NINT(XCO+SIN(ANGLE*PI2360)*(RADIUS-1))
            Y=NINT(YCO+COS(ANGLE*PI2360)*(RADIUS-1))

*         Check that the co-ordinates are within the image bounds.
            IF ((X.GE.XMIN).AND.(X.LE.XMAX)) THEN
               IF ((Y.GE.YMIN).AND.(Y.LE.YMAX)) THEN

*               Generate the number of the array element that contains
*               the image pixel required and read its value.
                  ADDRES=(Y-1)*XMAX+X
                  VALUE=ARRAY(ADDRES)

*               Check that the pixel is not bad and ignore it if it is.
                  IF (VALUE.NE.VAL__BADR) THEN

*                  Add the pixel count value to the summation array
*                  and increment the array containing how many data
*                  points have contributed to summation.
                     SUMMAT(RADIUS)=SUMMAT(RADIUS)+VALUE
                     NUMBER(RADIUS)=NUMBER(RADIUS)+1

*                  Set the image pixel bad so that it will not be
*                  used again.
                     ARRAY(ADDRES)=VAL__BADR

                  END IF

               END IF

            END IF

*         Increment the counter used to determine the current angle
*         being considered.
            COUNT=COUNT+1

         END DO

*      Check to see if the search should be stopped using a variety
*      criteria to achieve the option of stopping the calculations
*      automatically when the results are too near to sky.

*      Is the current radius value at the maximum permitted value?
         IF ((RADIUS.EQ.SEC__RESUL-1).OR.(RADIUS.GT.MAXRAD)) THEN
            FINISH=.TRUE.

*         Ensure that the whole array is not displayed if the object
*         was near the image edge and did not drop to threshold
*         within the image. i.e. object near edge and position angle
*         facing off the image.
            RADIUS=1
            DO 20 I=1,SEC__RESUL
               IF (NUMBER(I).GT.0) RADIUS=I
 20         CONTINUE

         END IF

*      Check those tests that are only tested in automatic length mode.
         IF (LTEST) THEN

*         Is the latest value below the background level?
            IF (NUMBER(RADIUS).GT.0.0) THEN
               IF (SUMMAT(RADIUS)/NUMBER(RADIUS).LT.BACK-1.)
     :             FINISH=.TRUE.
            END IF

*         Has the value increased twice as much in the latest radius
*         than it dropped in the last but one radius?
            IF (RADIUS.GT.3) THEN

*            Only carry out check if the last three radii values
*            all had significant values.
               IF ((NUMBER(RADIUS-2).GT.0.0).AND.
     :                       (NUMBER(RADIUS-1).GT.0.0).AND.
     :                       (NUMBER(RADIUS).GT.0.0)) THEN
                  V1=SUMMAT(RADIUS-2)/NUMBER(RADIUS-2)
                  V2=SUMMAT(RADIUS-1)/NUMBER(RADIUS-1)
                  V3=SUMMAT(RADIUS)/NUMBER(RADIUS)
                  IF ((V3-V2).GT.2.*(V1-V2)) FINISH=.TRUE.

               END IF
            END IF

*         Does it look like the local mean is now higher than it was at
*         a smaller radius.
            IF (RADIUS.GT.10) THEN

*            Setup the counter and summation variable.
               J=0
               TOTAL=0.0

*            Average over the four points taken at radii 7 to 10*PSIZE
*            smaller than the current value.
               DO 200 I=RADIUS-10,RADIUS-7

*               Only use radii value where a pixel was found.
                  IF (NUMBER(I).GT.0.0) THEN

*                  Increment the counter and add the pixel count value
*                  to the summation.
                     J=J+1
                     TOTAL=TOTAL+SUMMAT(I)/NUMBER(I)

                  END IF

 200           CONTINUE

*            Go on to consider a mean value at a larger radius if the
*            previous averageing attempt was finished okay.
               IF (J.GT.0) THEN

*               Keep mean value for the earlier summation.
                  MEAN1=TOTAL/REAL(J)

*               Clear the summation value and counter.
                  J=0
                  TOTAL=0.0

*               Look at count values from the current radius back
*               to the current radius-3*PSIZE.
                  DO 210 I=RADIUS-3,RADIUS

*                  Only use radii values where some pixels were found.
                     IF (NUMBER(I).GT.0.0) THEN

*                     Increment the counter and add the count value
*                     to the summation.
                        J=J+1
                        TOTAL=TOTAL+SUMMAT(I)/NUMBER(I)

                     END IF

 210              CONTINUE

*               Calculate the mean value at the larger radius only
*               if some valid data points were found.
                  IF (J.GT.0) THEN

*                  Calculate the mean value.
                     MEAN2=TOTAL/REAL(J)

*                  If the current mean is smaller than that found
*                  at a smaller radius then stop the study of larger
*                  radii.
                     IF (MEAN2.GT.MEAN1) FINISH=.TRUE.

                  END IF

               END IF

            END IF

         END IF

*      Increment the radius currently being considered.
         RADIUS=RADIUS+1

      END DO

*   Returns the value of the last radius for which data was taken.
      RLIM=RADIUS
      IF (RADIUS.GT.SEC__RESUL) RLIM=SEC__RESUL

*   Check to see how many valid results were obtained. Also, determine
*   the length.
      NVP=0
      I=0
      DO 400 I=1,RLIM
*      Any pixels at this radius?
         IF (NUMBER(I).GT.0.0) THEN
*         On average, were they below sky?
            IF ( SUMMAT(I)/NUMBER(I) - BACK.GT.0.0) THEN
               NVP=NVP+1
               J=I
            END IF
         END IF
 400  CONTINUE
      RLIM=J

*   Display messages for miscellaneous errors.

*   No pixels found.
      IF (NVP.EQ.0) THEN
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT( ' ','No valid pixels were found.',STATUS)
         CALL MSG_OUT(' ','So. No results will be shown.',STATUS)
         CALL MSG_BLANK(STATUS)
         GOTO 9999
      END IF

*   Too few pixels to draw a graph.
      IF (NVP.LT.2) THEN
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','Too few radius data points for'/
     :                /' a graph to be plotted or',STATUS)
         CALL MSG_OUT(' ','for the scale lengths to be '/
     :                /'calculated.',STATUS)
         CALL MSG_BLANK(STATUS)
         GOTO 9999
      END IF

*   Too few pixels for scale length calculation.
      IF (NVP.LT.3) THEN
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','Too few radius data points for'/
     :                /' the scale lengths to be calculated.',STATUS)
         CALL MSG_BLANK(STATUS)
         GOTO 9999
      END IF

 9999 CONTINUE

      END


      SUBROUTINE SEC1_POSAN(X1,Y1,X2,Y2,POSANG,RADIUS,FLAG,STATUS)
*+
*  Name:
*     SEC1_POSAN

*  Purpose:
*     Calculates the position angle and length of two points from the image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SEC1_POSAN(X1,Y1,X2,Y2,POSANG,RADIUS,FLAG,STATUS)

*  Description:
*     The co-ordinates of two points on the image are used to calculate
*     the distance between the two points (same units as those supplied).
*     That is then used to determine the arc sine value for the angle of
*     a line between the two points and the vertical image axis. The units
*     for the angle are radians, vertically upward has a value 0 and the
*     angle increases in a clockwise direction.
*
*     FLAG is returned with a non-zero value if the angle between the
*     points could not be calculated due to the start and end locations
*     being the same.

*  Arguments:
*     X1 = REAL (Given)
*        Image X co-ordinate for the centre of the sector.
*        Units of input must be consistent.
*     Y1 = REAL (Given)
*        Image Y co-ordinate for the centre of the sector.
*        Units of input must be consistent.
*     X2 = REAL (Given)
*        Image X co-ordinate for a point away from the centre.
*        Units of input must be consistent.
*     Y2 = REAL (Given)
*        Image Y co-ordinate for a point away from the centre.
*        Units of input must be consistent.
*     POSANG = REAL (Returned)
*        Angle between the central point of the sector and a
*        point defined X2,Y2. Units radians.
*     RADIUS = REAL (Returned)
*        Distance between the central point of the sector and a
*        point defined X2,Y2. Units as supplied.
*     FLAG = INTEGER (Returned)
*        Used to indicate if it was impossible to measure the angle.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     06-Jan-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'SEC_PAR'               ! SECTOR constants

*  Arguments Given:
      REAL X1                         ! X co-ord of central point
      REAL Y1                         ! Y co-ord of central point
      REAL X2                         ! X co-ord of another point
      REAL Y2                         ! Y co-ord of another point

*  Arguments Given and Returned:

*  Arguments Returned:
      INTEGER FLAG                    ! Was it possible to determine the
                                      ! angle? FLAG <>0 is fail
      REAL POSANG                     ! Angle between X1,Y1 and X2,Y2
      REAL RADIUS                     ! Distance between X1,Y1 and X2,Y2

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Calculate the distance between the points.
      RADIUS=(X2-X1)*(X2-X1)+(Y2-Y1)*(Y2-Y1)

*   Jump out of the subroutine if it was not possible to calculate an
*   angle (due to zero radius).
      IF (RADIUS.LT.SEC__VSMAL) THEN
         FLAG=1
         CALL MSG_OUT(' ','The radius is too small.',STATUS)
         GOTO 9999
      END IF

*   Complete the radius calculation.
      RADIUS=SQRT(RADIUS)

*   Calculate the angle in radians (vertical=0  and clockwise positive).
*   Range of function is only -pi to +pi.
      POSANG=ASIN((X2-X1)/RADIUS)

*   Determine in which hemisphere the angle is and correct to
*   appropriate quadrant.
      IF (Y2-Y1.LE.0.0) THEN
         IF (X2-X1.GE.0.0) THEN
            POSANG=SEC__PIVAL-POSANG
         ELSE
            POSANG=-SEC__PIVAL-POSANG
         END IF
      END IF

 9999 CONTINUE

      END
