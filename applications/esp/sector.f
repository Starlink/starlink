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
*            [ANGWID] [RLIM] [COSYS] [SAME] [AGAIN] [ORIGIN] (COLOUR)
 
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
*            psize=0.3 surf=true radisp=q mirror=false autol=false 
*            zerop=26.4 out in=ic3374 device=xwindows fitlim=0,20 
*            posang=25 angwid=5 
*            rlim=25 cosys=d origin=47,123
* 
*        An object located at co-ordinates 47,123 or image IC3374 is
*        profiled in the 25 degree direction out to a distance of 25
*        arc seconds. The wedge/sector used will be 5 degrees wide
*        and the scale length will be calculated using data obtained
*        in the radius range 0-20 arc seconds. The user supplied 
*        estimate of the galaxy centre will not be refined. The graphs 
*        generated will be quarter power radius versus surface 
*        brightness.
 
*  ADAM Parameters:
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
*     COSYS = _CHAR (Read)
*        Use a data or world co-ordinate system? (D=data W=world).
*     CURSOR = _LOGICAL (Read)
*        Whether the galaxy location is to be identified using the 
*        graphics cursor or the keyboard.
*     DEVICE = _DEVICE (Read) 
*        The name of the display device on which the results graphs
*        should be displayed.
*     FITLIM = _REAL (Read)
*        The range of radius values over which the scale length 'fits' 
*        are to be calculated.
*     IMGDEV = _DEVICE (Read)
*        Name of the graphics device displaying the image.
*     IN = _NDF (Read)
*        The name of the source NDF data structure/file.
*     MIRROR = _LOGICAL (Read)
*        Whether the summation is to be taken from two 
*        sectors/wedges/slices of the same size, but on 
*        diametrically opposite sides of the galaxy origin.
*     ORIGIN = _REAL (Read)
*        Image indices for the origin point to be used. Units pixels.
*     OUT = _CHAR (Read)
*        File name for the output text file containing the 
*        profile data.
*     POSANG = _REAL (Read)
*        The position angle of the sector relative to the top of the 
*        image. Convention is clockwise increases angle and the image
*        Y axis represents 0 degrees. Units degrees.
*     PSIZE = _REAL (Read)
*        Size of the image pixels in arc seconds. Units arc secs.
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
*     SURF = _LOGICAL (Read)
*        Are the pixel values to be expressed as surface brightness. 
*        If true then the output is surface brightness, otherwise the
*        display shows brightness in terms of sigma above sky. 
*        i.e. (I-Back)/SIGMA
*     ZEROP = _REAL (Read)
*        Zero point of the scale for surface brightness plots. Units 
*        magnitudes per square arc second.
 
*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-JUL-1994 (GJP)
*     (Original version)

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

      END 


      SUBROUTINE SEC1_CANCL(MODE,STATUS)    
*+
*  Name:
*     SEC1_CANCL

*  Purpose:
*     Cancels a number of input parameters so that they are then in a
*     state where the user is again prompted for an input.
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL SEC1_CANCL(MODE,STATUS)    

*  Description:
*      Cancels the values of a number of input parameters so that they are
*      changed from active state to Ground state. This means that the next 
*      time values for them are required the user will be reprompted.
*
*      The MODE variable defines which paramters must be cancelled.
*

*  Arguments:               
*     MODE = INTEGER (Given)
*        Defines which parameters must be cancelled. MODE=0 those required
*        for the cursor input or (MODE=1) those for keyboard input. 
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     23-FEB-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:                              
      INTEGER MODE                    ! Defines which parameters are to be
                                      ! cancelled

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Cancel those parameters required for cursor input.
      IF (MODE.EQ.0) THEN
*      Another profile.
         CALL PAR_CANCL('AGAIN',STATUS)
*      Symmetrical summation.
         CALL PAR_CANCL('MIRROR',STATUS)
*      Backgound count.
         CALL PAR_CANCL('BACK',STATUS)
*      Display brightness mode.
         CALL PAR_CANCL('SURF',STATUS)
*      Display radius mode.
         CALL PAR_CANCL('RADISP',STATUS)
*      Auto-locate better galaxy centre.
         CALL PAR_CANCL('AUTOL',STATUS)
*      Output text file name.
         CALL PAR_CANCL('OUT',STATUS)
*      The graphics display device used for the graphs.
         CALL AGI_CANCL('DEVICE',STATUS)
      END IF

*   Cancel those parameters required for keyboard input.
      IF (MODE.EQ.1) THEN
*      Another profile.
         CALL PAR_CANCL('AGAIN',STATUS)
*      Galaxy origin co-ordinates.
         CALL PAR_CANCL('ORIGIN',STATUS)
*      Position angle of sector.
         CALL PAR_CANCL('POSANG',STATUS)
*      Angular width of sector.
         CALL PAR_CANCL('ANGWID',STATUS)
*      Length of sector.
         CALL PAR_CANCL('RLIM',STATUS)
*      Symmetrical summation.
         CALL PAR_CANCL('MIRROR',STATUS)
*      Background  count.
         CALL PAR_CANCL('BACK',STATUS)
*      Brightness display mode.
         CALL PAR_CANCL('SURF',STATUS)
*      Radius display mode.
         CALL PAR_CANCL('RADISP',STATUS)
*      Auto-locate better galaxy origin.
         CALL PAR_CANCL('AUTOL',STATUS)
*      Range of radius over which the scale length calculations 
*      are performed.
         CALL PAR_CANCL('FITLIM',STATUS)
*      Output text file name.
         CALL PAR_CANCL('OUT',STATUS)
      END IF

 9999 CONTINUE

      END


      SUBROUTINE SEC1_CMODE(STATUS)
*+
*  Name:
*     SEC1_CMODE

*  Purpose:
*     May be used to display the average pixel values within a wedge shaped 
*     sector of the image. The sector is in the form of a wedge (of user 
*     defined size) drawn out from an origin point.
*
*     The results are displayed as pixel value (in terms of level relative
*     to sky or surface brightness) versus origin distance. Pixel values 
*     are summed over all the points at a given distance from the origin.
*
*     Options include summing results from two equal sized diametrically
*     opposite sectors, displaying data using a number of possible radius
*     transformations, the use of an approximate local maximum location as
*     origin if required, automatic selection of the maximum radius out 
*     from the origin to be considered and the use of a graphics cursor 
*     to select the image object to be examined.
*    
*     This routine operates using a combination of keyboard and cursor
*     inputs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SEC1_CMODE(STATUS)

*  Arguments:   
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description: 
*     Allows the pixels count values within a wedge/sector drawn from a
*     user specified point(s) to be plotted as a function of distance
*     from the origin point. 
*
*     Also, allows the results from two diametrically opposite sectors
*     to be plotted as an option. The results are displayed with
*     radii as arc seconds and mean pixel values expressed in terms of
*     sigma or surface brightness.
*
*     The length of the slice, it's angular width and position angle are
*     all user defined. The length may also be selected automatically by
*     the software if desired. 
*
*     The user is allowed to define how the radius values will be displayed
*     i.e. as R, R**0.25, Log R or R x R. 
*
*     Contaminating parts of the image may be defined using an ARD file.
*
*     An option is present allowing the input object location to be
*     adjusted by the software to employ an approximate (1 pixel 
*     accuracy) estimate of the location of the weighted intensity maximum. 

*  Implementation Status:
*     Under development

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-Nov-1992 (GJP)
*     (Original version)
*     20-FEB-1997 (GJP)
*     Modified to avoid cursor selection of points when 
*     GRAPH=FALSE.

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'sec_par'               ! SECTOR constants
      INCLUDE 'SUBPAR_PAR'            ! SUBPAR constants   

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local Variables:      
      CHARACTER *(256) RADISP         ! Option choice defining how the
                                      ! radius data is to be displayed
      LOGICAL AGAIN                   ! Determine another profile?
      LOGICAL AUTOL                   ! Is an estimate of the galaxy centre
                                      ! position to be made?
      LOGICAL EXCLAIM                 ! File name was '!'?
      LOGICAL GRAPH                   ! Is a graph to be plotted
      LOGICAL INOKAY                  ! Was the most recent input value okay
      LOGICAL INOK2                   ! Same as above
      LOGICAL MIRROR                  ! Sum the pixels over a single slice
                                      ! or two diametrically opposite slices
      LOGICAL SAME                    ! Use the display device the image is
                                      ! on to show the result graphs?
      LOGICAL SURF                    ! Pixel values expressed as sigma 
                                      ! or surface brightness
      INTEGER AGIID                   ! AGI identifier
      INTEGER COUNT(2)                ! The number of data points used in the
                                      ! scale length regression
      INTEGER COLOUR                  ! Colour of the galaxy centre point
      INTEGER ELEMS                   ! Total number of pixels in the NDF
      INTEGER FIOD2                   ! Output file identifier
      INTEGER FIRST                   ! First time the NDF identifier has been
                                      ! determined
      INTEGER FLAG                    ! Can the central pixel value be found?
      INTEGER I                       ! Loop variable
      INTEGER LBND(NDF__MXDIM)        ! Lower limit for image index
      INTEGER RLIM                    ! The length of the slice to be taken
      INTEGER LEN2                    ! Temporary storage of RLIM
      INTEGER NDF1                    ! Identifier for the source NDF  
      INTEGER NDIM                    ! Number of dimensions in the 
                                      ! image
      INTEGER NVP                     ! The number of valid data points found
      INTEGER PFLAG                   ! can the position angle be found?
      INTEGER POINT0(1)               ! Pointer to the data component of
                                      ! the source NDF
      INTEGER POINT1(1)               ! Pointer to the data component of 
                                      ! for the output NDF
      INTEGER POINT3(1)               ! Pointer to the mapped ARD mask
      INTEGER PRANGE(2)               ! Length of the x and y axes
      INTEGER UBND(NDF__MXDIM)        ! Upper limit for image index
      REAL ANGLE                      ! Angle between a line between two
                                      ! points
      REAL ANGWID                     ! Angular width of the sector
      REAL BACK                       ! Background count value
      REAL CONS(2)                    ! Constant terms of the curves used 
                                      ! to find the scale length
      REAL DISTAN                     ! Distance between two points on
                                      ! the image
      REAL GRAD(2)                    ! Gradients of the curves used
                                      ! to find out the scale length
      REAL HIR                        ! Highest radius value used in the fit
                                      ! calculated
      REAL LOR                        ! Lowest radius value employed in
                                      ! the fit calculated
      REAL NUMBER(SEC__RESUL)         ! The number of pixels found at a given
                                      ! distance from the origin.
      REAL OCOUNT                     ! Pixel count for the origin pixel
      REAL POSANG                     ! Position angle of the slice
      REAL PSIZE                      ! Size of the image pixels in arc sec
      REAL RADIUS                     ! radius of the arc to be used when
                                      ! drawing the sector
      REAL SIGMA                      ! Standard deviation of the background value
      REAL SLEN(2)                    ! Scale length of the galaxy
      REAL SUMMAT(SEC__RESUL)         ! Sum of the pixel counts for all pixels
                                      ! at a given distance from the origin
      REAL TEMP                       ! Temporary storage
      REAL X(10)                      ! Array indices of the sector to be used
      REAL XCO                        ! X index of the sector origin
      REAL Y(10)                      ! Array indices of the sector to be used
      REAL YCO                        ! Y index of the sector origin
      REAL ZEROP                      ! Zero point of the surface 
                                      ! brightness graphs
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Get the pen colour.
      CALL PAR_STATE('COLOUR',I,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN    
         CALL PAR_GET0I('COLOUR',COLOUR,STATUS)
         CALL MSG_OUT(' ','Command line COLOUR value used.',STATUS)
      ELSE
         COLOUR=1
      END IF
 
*   Begin an NDF context.                               
      CALL NDF_BEGIN
      IF (STATUS.NE.SAI__OK) GOTO 9999

*     Loop around looking at different parts for the same image.
      AGAIN=.TRUE.
      FIRST=0
      DO WHILE (AGAIN.AND.(STATUS.EQ.SAI__OK))  

*      Get the cursor positon for the galaxy origin. At the same time
*      obtain the NDF identifier for the most recent 'DATA' picture.
         INOKAY=.FALSE.
         DO WHILE ((.NOT.INOKAY).AND.(STATUS.EQ.SAI__OK))  

*         Get the value from the image. Taking care to ensure that
*         the NDF is obtained by SEC1_CURSO only the first time 
*         a co-ordinate is provided.
            CALL SEC1_CURSO('IMGDEV',FIRST,0,POSANG,COLOUR,NDF1,
     :                      X,Y,RADIUS,ANGWID,STATUS)
            XCO=X(10)
            YCO=Y(10)

*         Get the image bounds and also the size of the axes in pixels.
            CALL NDF_BOUND(NDF1,2,LBND,UBND,NDIM,STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 9999
            PRANGE(1)=UBND(1)-LBND(1)+1
            PRANGE(2)=UBND(2)-LBND(2)+1

*         Check that the co-ordinate values input are legal and
*         annul the parameter if not.
            IF ((XCO.LT.1.0).OR.(XCO.GT.PRANGE(1)).OR.(YCO.LT.0.0).
     :           OR.(YCO.GT.PRANGE(2))) THEN
               CALL MSG_OUT(' ','The position supplied, is not '//
     :                      'within the image.',STATUS)
               CALL PAR_CANCL('ORIGIN',STATUS)
            ELSE
               INOKAY=.TRUE.
            END IF
            FIRST=1

         END DO            
         IF (STATUS.NE.SAI__OK) GOTO 9999 

*      Get slice position angle.
        
*      Get the position indicating the sector direction and extent.     
         CALL SEC1_CURSO('IMGDEV',2,0,POSANG,COLOUR,NDF1,
     :                   X,Y,RADIUS,ANGWID,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999 
        
*      Calculate the position angle in radians.
         CALL SEC1_POSAN(X(1),Y(1),X(2),Y(2),ANGLE,DISTAN,PFLAG,STATUS)
         POSANG=ANGLE
         RADIUS=DISTAN

*      Get the pixel position defining the angular width of the sector.
         CALL SEC1_CURSO('IMGDEV',3,0,POSANG,COLOUR,NDF1,
     :                   X,Y,RADIUS,ANGWID,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999 

*      Calculate the angular width of the sector required.
         CALL SEC1_POSAN(X(1),Y(1),X(3),Y(3),ANGLE,DISTAN,PFLAG,STATUS)
         IF (POSANG-ANGLE.GT.SEC__PIVAL) ANGLE=ANGLE+SEC__PIVAL
         IF (ANGLE-POSANG.GT.SEC__PIVAL) ANGLE=ANGLE-SEC__PIVAL
         ANGWID=2.*ABS(POSANG-ANGLE)

*      Display the sector.
         CALL SEC1_CURSO('IMGDEV',4,0,POSANG,COLOUR,NDF1,
     :                   X,Y,RADIUS,ANGWID,STATUS)
         CALL MSG_BLANK(STATUS)
 
*      Are pixels on both sides of the origin to be used.
         CALL PAR_GET0L('MIRROR',MIRROR,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Display the second half of the sector.
         IF (MIRROR) CALL SEC1_CURSO('IMGDEV',5,0,POSANG,COLOUR,NDF1,
     :                               X,Y,RADIUS,ANGWID,STATUS)
     
*      Convert position angle and angular width from radians
*      to degrees format.
         POSANG=POSANG*SEC__RADS
         ANGWID=ANGWID*SEC__RADS
         RLIM=NINT(RADIUS)

*      Get the background count value.
         CALL PAR_GET0R('BACK',BACK,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the background count standard deviation value.
         INOKAY=.FALSE.
         DO WHILE ((.NOT.INOKAY).AND.(STATUS.EQ.SAI__OK))
            CALL PAR_GET0R('SIGMA',SIGMA,STATUS)
            IF (SIGMA.LE.0.0) THEN
*            Display message and annul the parameter.
               CALL MSG_OUT(' ','Sigma supplied, is not '//
     :                      'feasible.',STATUS)
               CALL PAR_CANCL('SIGMA',STATUS)
            ELSE
               INOKAY=.TRUE.  
            END IF
         END DO
         IF (STATUS.NE.SAI__OK) GOTO 9999 

*      Get the pixel size value.
         INOKAY=.FALSE.
         DO WHILE ((.NOT.INOKAY).AND.(STATUS.EQ.SAI__OK))
            CALL PAR_GET0R('PSIZE',PSIZE,STATUS)
            IF (PSIZE.LE.0.0) THEN
*            Display message and annul the parameter.
               CALL MSG_OUT(' ','The pixel size supplied, is not '//
     :                      'feasible.',STATUS)
               CALL PAR_CANCL('PSIZE',STATUS)
            ELSE
               INOKAY=.TRUE.
            END IF
         END DO
         IF (STATUS.NE.SAI__OK) GOTO 9999 
        
*      Convert the length in arc secs to length in pixels.
         RLIM=NINT(REAL(RLIM)/PSIZE)
         IF (RLIM.GT.SEC__RESUL) THEN
            STATUS=SAI__ERROR
            CALL ERR_REP(' ','The size of the sector is '//
     :                   'too big.',STATUS)
            GOTO 9999
         END IF 

*      Get the user selection of values shown in sigma or surface brightness.
         CALL PAR_GET0L('SURF',SURF,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the radius display type mode and convert to upper case.
         CALL PAR_GET0C('RADISP',RADISP,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999
         CALL CHR_UCASE(RADISP)

*      Get the zero point for the surface brightness scale/graphs.
         CALL PAR_GET0R('ZEROP',ZEROP,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the user selection of automatic location of the galaxy centre
*      or not.
         CALL PAR_GET0L('AUTOL',AUTOL,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Map the input NDF data array as _REAL values for reading.
         CALL NDF_MAP(NDF1,'DATA','_REAL','READ',POINT0(1),ELEMS,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Allocate dynamic memory on which to map the NDF.
         CALL PSX_CALLOC(ELEMS,'_REAL',POINT1(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Transfer values from the mapped NDF to the allocated memory.
         CALL SEC1_TRANS(ELEMS,%VAL(POINT0(1)),%VAL(POINT1(1)),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Un-map the source NDF. Helps to reduce the resources being used.
         CALL NDF_UNMAP(NDF1,'DATA',STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999
            
*      Allocate the memory needed for the logical mask array.
         CALL PSX_CALLOC(ELEMS,'_INTEGER',POINT3(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999
 
*      Transfer to the ARD driver control routine.
         CALL ESP_ARD_DRIVER(NDIM,ELEMS,LBND,UBND,POINT1,POINT3,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Free the dynamic array space of the logical mask.
         CALL PSX_FREE(POINT3(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999 
         
*      Look for a better (though crude) estimate of the galaxy core position.
         CALL SEC1_AUTOL(AUTOL,ELEMS,PRANGE,OCOUNT,FLAG,
     :                   %VAL(POINT1(1)),XCO,YCO,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Call the routine that fills the arrays with the summation of all
*      the data points within the required slice.
         CALL SEC1_PIE(1,BACK,ELEMS,XCO,YCO,PRANGE,POSANG,ANGWID,NVP,
     :                 NUMBER,SUMMAT,RLIM,%VAL(POINT1(1)),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999
         LEN2=RLIM


*      Continue the radius/brightness summation using pixels on the other
*      side of the origin.
         IF (MIRROR) THEN 
   
*          Pass the position angle for the other side of the origin and
*          then repeat the summation.
            TEMP=POSANG+180.0

*         Perform the count summation for the opposite side of the object.
            CALL SEC1_PIE(0,BACK,ELEMS,XCO,YCO,PRANGE,TEMP,ANGWID,NVP,
     :                    NUMBER,SUMMAT,RLIM,%VAL(POINT1(1)),STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 9999

         END IF

*      Ensure that the length value used reflects the highest value found
*      even when MIRROR is true.
         IF (RLIM.LT.LEN2) RLIM=LEN2

*      Only bother with the graphs if there is more than one data point.
         IF (NVP.GT.1) THEN

*         Get the user selection of using the same display as the image
*         to display the results.
            CALL PAR_GET0L('SAME',SAME,STATUS)

*         Display the results graphs on the device used to show the image.
            IF (SAME) THEN

*            Find limits for the window to be used to display the graph.
               CALL SEC1_CURSO('IMGDEV',6,0,POSANG,COLOUR,NDF1,
     :                         X,Y,RADIUS,ANGWID,STATUS)
           
*            Set up the new window.
               CALL SEC1_CURSO('IMGDEV',7,0,POSANG,COLOUR,NDF1,
     :                         X,Y,RADIUS,ANGWID,STATUS)

*            Set up the AGI/PGPLOT interface.
               CALL SEC1_AGICO(0,1,0,AGIID,STATUS)
               
*            Display the raw results graph.
               CALL SEC1_GRAPH(1,ZEROP,RADISP,SURF,RLIM,BACK,
     :                         NUMBER,PSIZE,SIGMA,SUMMAT,CONS,
     :                         GRAD,STATUS)  
  
*            Turn off the AGI/PGPLOT interface.
               CALL SEC1_AGICO(1,1,0,AGIID,STATUS)
  
*            Only get back the radius limits if there are more
*            than two data points.
               IF (NVP.GT.2) THEN

*               Loop around until two reasonable value have been selected
*               for the minimum and maximum radii.
                  INOK2=.FALSE.
                  DO WHILE ((.NOT.INOK2).AND.(STATUS.EQ.SAI__OK))

*                  Display a cursor in the new window and get back the 
*                  range of radius values the user is interested in.
                     CALL SEC1_CURSO('IMGDEV',8,1,POSANG,COLOUR,NDF1,
     :                               X,Y,RADIUS,ANGWID,STATUS)
                     CALL SEC1_CURSO('IMGDEV',9,1,POSANG,COLOUR,NDF1,
     :                               X,Y,RADIUS,ANGWID,STATUS)
                     LOR=MIN(X(8),X(9))
                     HIR=MAX(X(8),X(9))

*                 Transfer the radius values for conversion.
                     CALL SEC1_UNCON(RADISP,LOR,HIR,STATUS)         
 
*                  Check to see if it is possible for there to be two 
*                  data points in the radius range required.
                     IF ((HIR-LOR)/PSIZE.LT.2.0) THEN
                        CALL MSG_BLANK(STATUS)
                        CALL MSG_OUT(' ','The radius range supplied,'//
     :                              ' is too narrow to be used.',STATUS)
                     ELSE
                        INOK2=.TRUE.             
                     END IF

                  END DO

*               Calculate the scale length assuming spiral or elliptical.

*               Obtain the 'fit' parameters for linear fits to the
*               brightness versus radius data (suitably transformed). 
                  CALL SEC1_LINRE(LOR,HIR,NUMBER,SUMMAT,PSIZE,BACK,
     :                            RLIM,GRAD,CONS,COUNT,SLEN,STATUS)

*               Open up the AGI/PGPLOT interface again.
                  CALL SEC1_AGICO(0,1,1,AGIID,STATUS)

*               Display the fit plots if a graphics device was selected.
                  CALL SEC1_GRAPH(2,ZEROP,RADISP,SURF,RLIM,BACK,
     :                            NUMBER,PSIZE,SIGMA,SUMMAT,CONS,
     :                            GRAD,STATUS) 
                   
*               Turn off the AGI/PGPLOT interface.
                  CALL SEC1_AGICO(1,1,1,AGIID,STATUS)

               END IF

            END IF
                                
         END IF
                

*      Display the results graphs on a device specified by the user.
         IF (.NOT.SAME) THEN

*      Only display the graphs if there is more than 1 data point.
             IF (NVP.GT.1) THEN

*            Display the graph of the data points i.e. radius versus brightness.
*            Determine if graphical histogram output is required. Set the
*            value for GRAPH accordingly.
               AGIID=0
               GRAPH=.TRUE.
               CALL ERR_MARK
               CALL SEC1_AGICO(0,0,0,AGIID,STATUS)
               IF (STATUS.NE.SAI__OK) THEN
                  GRAPH=.FALSE.
                  CALL ERR_ANNUL(STATUS)
               END IF
               CALL ERR_RLSE

*            Display the un-analysed data as a graphical plot of radius 
*            (in some form) versus intensity (in some form).
               IF (GRAPH) THEN
                  CALL SEC1_GRAPH(1,ZEROP,RADISP,SURF,RLIM,BACK,
     :                            NUMBER,PSIZE,SIGMA,SUMMAT,
     :                         CONS,GRAD,STATUS)    
*               Turn off the AGI/PGPLOT interface.
                  CALL SEC1_AGICO(1,0,0,AGIID,STATUS)
               END IF

*            Only determine the scale lengths if there are more 
*            than 2 data points.
               IF (NVP.GT.2) THEN

                  IF(GRAPH) THEN
*                  Display a cursor in the new window and get back the range of 
*                  radius values the user is interested in.
                     CALL SEC1_CURSO('DEVICE',8,1,POSANG,COLOUR,NDF1,
     :                               X,Y,RADIUS,ANGWID,STATUS)
                     CALL SEC1_CURSO('DEVICE',9,1,POSANG,COLOUR,NDF1,
     :                               X,Y,RADIUS,ANGWID,STATUS)
                     LOR=MIN(X(8),X(9))
                     HIR=MAX(X(8),X(9))
                     
*                 Transfer the radius values for conversion.
                     CALL SEC1_UNCON(RADISP,LOR,HIR,STATUS)         

                  ELSE

*                  Define range when no device selected.
                     LOR=0.0
                     HIR=RADIUS   

                  END IF
                     
*               Check to see if it is possible for there to be two 
*               data points in the radius range required.
                  IF ((HIR-LOR)/PSIZE.LT.2.0) THEN
                     STATUS=SAI__ERROR
                     CALL ERR_REP(' ','The radius range supplied,'//
     :                            ' is too narrow to be used.',STATUS)
                     GOTO 9999
                  END IF

*               Calculate the scale length assuming spiral or elliptical.

*               Obtain the 'fit' parameters for linear fits to the 
*               brightness versus radius data (suitably transformed). 
                  CALL SEC1_LINRE(LOR,HIR,NUMBER,SUMMAT,PSIZE,BACK,
     :                            RLIM,GRAD,CONS,COUNT,SLEN,STATUS)
            

*               Display the fit plots if a graphics device was selected.
                  IF (GRAPH) THEN
                    
*                  Open up the AGI/PGPLOT interface again.
                     CALL SEC1_AGICO(0,0,1,AGIID,STATUS)

*                  Show the fit.
                     CALL SEC1_GRAPH(2,ZEROP,RADISP,SURF,RLIM,BACK,
     :                               NUMBER,PSIZE,SIGMA,SUMMAT,
     :                               CONS,GRAD,STATUS) 

*                  Turn off the AGI/PGPLOT interface.
                     CALL SEC1_AGICO(1,0,0,AGIID,STATUS)
                                 
                  END IF

               END IF

            END IF

         END IF     
      

*      Display the results on the default display.
*      Output a text file containing results if required.
         IF (NVP.GT.2) THEN
            CALL SEC1_TEXTD(FLAG,NDF1,XCO,YCO,OCOUNT,BACK,SIGMA,CONS,
     :                      GRAD,RLIM,PSIZE,COUNT,ZEROP,SLEN,
     :                      LOR,HIR,LBND,STATUS)    

            CALL SEC1_TEXTO(NDF1,SUMMAT,NUMBER,XCO,YCO,BACK,
     :                      SIGMA,CONS,RLIM,PSIZE,ZEROP,SLEN,LBND,
     :                      FIOD2,EXCLAIM,STATUS)
         END IF

*      De-allocate the dynamic memory used.
         CALL PSX_FREE(POINT1(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the user selection of preparing another galaxy profile 
*      or not.
         CALL PAR_GET0L('AGAIN',AGAIN,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

         IF (AGAIN) THEN

*         Cancel the parameters so that they must be reinput when
*         looping round.
            CALL SEC1_CANCL(0,STATUS)

         END IF

      END DO

      IF (STATUS.NE.SAI__OK) GOTO 9999

 9999 CONTINUE

*   End the NDF context.
      CALL NDF_END(STATUS)                              

      END
       
      
      SUBROUTINE SEC1_KMODE(STATUS)
*+
*  Name:
*     SEC1_KMODE

*  Purpose:
*     May be used to display the average pixel values within a wedge shaped 
*     sector of the image. The sector is in the form of a wedge (of user 
*     defined size) drawn out from an origin point.
*
*     The results are displayed as pixel value (in terms of level relative
*     to sky or surface brightness) versus origin distance. Pixel values 
*     are summed over all the points at a given distance from the origin.
*
*     Options include summing results from two equal sized diametrically
*     opposite sectors, displaying data using a number of possible radius
*     transformations, the use of an approximate local maximum location as
*     origin if required, automatic selection of the maximum radius out 
*     from the origin to be considered and the use of a graphics cursor 
*     to select the image object to be examined.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SEC1_KMODE(STATUS)

*  Arguments:   
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description: 
*     Allows the pixels count values within a wedge/sector drawn from a
*     user specified point(s) to be plotted as a function of distance
*     from the origin point. 
*
*     Also, allows the results from two diametrically opposite sectors
*     to be plotted as an option. The results are displayed with
*     radii as arc seconds and mean pixel values expressed in terms of
*     sigma or surface brightness.
*
*     The length of the slice, it's angular width and position angle are
*     all user defined. The length may also be selected automatically by
*     the software if desired. 
*
*     The user is allowed to define how the radius values will be displayed
*     i.e. as R, R**0.25, Log R or R x R. 
*
*     Contaminating parts of the image may be defined using an ARD file.
*
*     An option is present allowing the input object location to be
*     adjusted by the software to employ an approximate (1 pixel 
*     accuracy) estimate of the location of the weighted intensity maximum. 
*
*     This routine operates by keyboard inputs exclusively.

*  Implementation Status:
*     Under development

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-Nov-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'sec_par'               ! SECTOR constants
      INCLUDE 'SUBPAR_PAR'            ! SUBPAR constant
   
*  Status:     
      INTEGER STATUS                  ! Global status

*  Local Variables:      
      CHARACTER *(256) COSYS          ! Option choice defining how the
                                      ! pixel data format to be input
      CHARACTER *(256) RADISP         ! Option choice defining how the
                                      ! radius data is to be displayed
      LOGICAL AGAIN                   ! Look at another part of the image?
      LOGICAL AUTOL                   ! Is an estimate of the galaxy centre
                                      ! position to be made?
      LOGICAL EXCLAIM                 ! File name was '!'?
      LOGICAL GRAPH                   ! Is a graph to be plotted
      LOGICAL INOKAY                  ! Was the most recent input value okay
      LOGICAL MIRROR                  ! Sum the pixels over a single slice
                                      ! or two diametrically opposite slices
      LOGICAL SURF                    ! Pixel values expressed as sigma 
                                      ! or surface brightness
      INTEGER AGIID                   ! AGI identifier
      INTEGER COUNT(2)                ! The number of data points used in the
                                      ! scale length regression
      INTEGER ELEMS                   ! Total number of pixels in the NDF
      INTEGER FIOD2                   ! Output file identifier
      INTEGER FLAG                    ! Can the central pixel value be found?
      INTEGER IND                     ! The number of origin indices to
                                      ! be input at one go i.e. 2
      INTEGER IND2                    ! Number of indices returned
      INTEGER LBND(NDF__MXDIM)        ! Lower limit for image index
      INTEGER RLIM                    ! The length of the slice to be taken
      INTEGER LEN2                    ! Temporary storage of RLIM
      INTEGER NDF1                    ! Identifier for the source NDF  
      INTEGER NDIM                    ! Number of dimensions in the 
                                      ! image
      INTEGER NVP                     ! The number of valid data points found
      INTEGER POINT0(1)               ! Pointer to the data component of
                                      ! the source NDF
      INTEGER POINT1(1)               ! Pointer to the data component after
                                      ! its been mapped to dynamic memory
      INTEGER POINT3(1)               ! Pointer to the ARD mask
      INTEGER PRANGE(2)               ! Length of the x and y axes
      INTEGER UBND(NDF__MXDIM)        ! Upper limit for image index
      REAL ANGWID                     ! Angular width of the slice
      REAL BACK                       ! Background count value
      REAL CONS(2)                    ! Constant terms of the curves used 
                                      ! to find the scale length
      REAL GRAD(2)                    ! Gradients of the curves used
                                      ! to find out the scale length
      REAL HIR                        ! Highest radius value used in the fit
                                      ! calculated
      REAL INP(2)                     ! Value input by the user
      REAL LOR                        ! Lowest radius value employed in
                                      ! the fit calculated
      REAL NUMBER(SEC__RESUL)         ! The number of pixels found at a given
                                      ! distance from the origin.
      REAL OCOUNT                     ! Pixel count for the origin pixel
      REAL POSANG                     ! Position angle of the slice
      REAL PSIZE                      ! Size of the image pixels in arc sec
      REAL SIGMA                      ! Standard deviation of the background value
      REAL SLEN(2)                    ! Scale length of the galaxy
      REAL SUMMAT(SEC__RESUL)         ! Sum of the pixel counts for all pixels
                                      ! at a given distance from the origin
      REAL TEMP                       ! Temporary storage
      REAL XCO                        ! X index of the sector origin
      REAL YCO                        ! Y index of the sector origin
      REAL ZEROP                      ! Zero point of the surface 
                                      ! brightness graphs
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Begin an NDF context.                               
      CALL NDF_BEGIN
      IF (STATUS.NE.SAI__OK) GOTO 9999
      
*   Obtain an identifier for the NDF structure to be examined.       
      CALL NDF_ASSOC('IN','READ',NDF1,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Get the image bounds and also the size of the axes in pixels.
      CALL NDF_BOUND(NDF1,2,LBND,UBND,NDIM,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999 
      PRANGE(1)=UBND(1)-LBND(1)+1
      PRANGE(2)=UBND(2)-LBND(2)+1
                                       
*   Get the co-ordinate system mode and convert to upper case.
      CALL PAR_GET0C('COSYS',COSYS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      CALL CHR_UCASE(COSYS)

*   Look at another location on the image.
      AGAIN=.TRUE.
      DO WHILE (AGAIN.AND.(STATUS.EQ.SAI__OK))

*      Get the pixel to be used as the galaxy centre.
         IND=2
         IND2=2
         INOKAY=.FALSE.
         DO WHILE ((.NOT.INOKAY).AND.(STATUS.EQ.SAI__OK))

*         Get the input.
            CALL PAR_GET1R('ORIGIN',IND,INP,IND2,STATUS)
            XCO=INP(1)
            YCO=INP(2)

*         Check that the co-ordinate values input are legal.
            IF (COSYS.EQ.'W') THEN
               IF ((XCO.GE.LBND(1)).AND.(XCO.LE.UBND(1))
     :            .AND.(YCO.GE.LBND(2)).AND.(YCO.LE.UBND(2))) 
     :            INOKAY=.TRUE.
               XCO=XCO-LBND(1)+1
               YCO=YCO-LBND(2)+1
            ELSE
               IF ((XCO.GE.1.0).AND.(XCO.LE.PRANGE(1))
     :            .AND.(YCO.GE.1.0).AND.(YCO.LE.PRANGE(2))) 
     :            INOKAY=.TRUE.
            END IF

            IF (.NOT.INOKAY) THEN
               CALL MSG_OUT(' ','The position supplied, is not '//
     :                      'within the image.',STATUS)
               CALL PAR_CANCL('ORIGIN',STATUS)
            END IF

         END DO
         IF (STATUS.NE.SAI__OK) GOTO 9999 

*      Get the position angle for the sector.
         CALL PAR_GET0R('POSANG',POSANG,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get angular width of the slice.
         CALL PAR_GET0R('ANGWID',ANGWID,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get a value for the length of the slice. 0=automatic.
         CALL PAR_GET0I('RLIM',RLIM,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Are pixels on both sides of the origin to be used.
         CALL PAR_GET0L('MIRROR',MIRROR,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the background count value.
         CALL PAR_GET0R('BACK',BACK,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the background count standard deviation value.
         INOKAY=.FALSE.
         DO WHILE ((.NOT.INOKAY).AND.(STATUS.EQ.SAI__OK))
            CALL PAR_GET0R('SIGMA',SIGMA,STATUS)
            IF (SIGMA.LE.0.0) THEN
*            Display message and annul the parameter.
               CALL MSG_OUT(' ','Sigma supplied, is not '//
     :                      'feasible.',STATUS)
               CALL PAR_CANCL('SIGMA',STATUS)
            ELSE
               INOKAY=.TRUE.
            END IF
         END DO
         IF (STATUS.NE.SAI__OK) GOTO 9999 

*      Get the pixel size value.
         INOKAY=.FALSE.
         DO WHILE ((.NOT.INOKAY).AND.(STATUS.EQ.SAI__OK))
            CALL PAR_GET0R('PSIZE',PSIZE,STATUS)
            IF (PSIZE.LE.0.0) THEN
*            Display message and annul the parameter.
               CALL MSG_OUT(' ','The pixel size supplied, is not '//
     :                      'feasible.',STATUS)
               CALL PAR_CANCL('PSIZE',STATUS)
            ELSE
               INOKAY=.TRUE.
            END IF
         END DO
         IF (STATUS.NE.SAI__OK) GOTO 9999 

*      Convert the length in arc secs to length in pixels.
         RLIM=NINT(REAL(RLIM)/PSIZE)
         IF (RLIM.GT.SEC__RESUL) THEN
            STATUS=SAI__ERROR
            CALL ERR_REP(' ','The size of the sector is '//
     :                   'too big.',STATUS)
            GOTO 9999
         END IF

*      Get the user selection of values shown in sigma or surface brightness.
         CALL PAR_GET0L('SURF',SURF,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the radius display type mode and convert to upper case.
         CALL PAR_GET0C('RADISP',RADISP,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999
         CALL CHR_UCASE(RADISP)

*      Get the zero point for the surface brightness scale/graphs.
         CALL PAR_GET0R('ZEROP',ZEROP,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the user selection of automatic location of the galaxy centre
*      or not.
         CALL PAR_GET0L('AUTOL',AUTOL,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Map the output NDF data array as _REAL values for reading.
         CALL NDF_MAP(NDF1,'DATA','_REAL','READ',POINT0(1),
     :                ELEMS,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999
            
*      Allocate dynamic memory on which to map the NDF.
         CALL PSX_CALLOC(ELEMS,'_REAL',POINT1(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Transfer values from the mapped NDF to the allocated memory.
         CALL SEC1_TRANS(ELEMS,%VAL(POINT0(1)),%VAL(POINT1(1)),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Un-map the source NDF. Helps to reduce the resourcse being used.
         CALL NDF_UNMAP(NDF1,'DATA',STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998
                         
*      Allocate the memory needed for the logical mask array.
         CALL PSX_CALLOC(ELEMS,'_INTEGER',POINT3(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998
 
*      Transfer to the ARD driver control routine.
         CALL ESP_ARD_DRIVER(NDIM,ELEMS,LBND,UBND,POINT1,POINT3,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998
  
*      Free the dynamic array space of the logical mask.
         CALL PSX_FREE(POINT3(1),STATUS)
        IF (STATUS.NE.SAI__OK) GOTO 9998
 
*      Look for a better (though crude) estimate of the galaxy core position.
         CALL SEC1_AUTOL(AUTOL,ELEMS,PRANGE,OCOUNT,FLAG,
     :                   %VAL(POINT1(1)),XCO,YCO,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Call the routine that fills the arrays with the summation of all
*      the data points within the required slice.
         CALL SEC1_PIE(1,BACK,ELEMS,XCO,YCO,PRANGE,POSANG,ANGWID,NVP,
     :                 NUMBER,SUMMAT,RLIM,%VAL(POINT1(1)),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998
         LEN2=RLIM

*      Continue the summation of radius/brightness points using data on
*      the other side of the origin.
         IF (MIRROR) THEN 
   
*          Pass the position angle for the other side of the origin and
*          then repeat the summation.
            TEMP=POSANG+180.

*         Perform the count summation for the opposite side of the object.
            CALL SEC1_PIE(0,BACK,ELEMS,XCO,YCO,PRANGE,TEMP,ANGWID,NVP,
     :                    NUMBER,SUMMAT,RLIM,%VAL(POINT1(1)),STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 9998

         END IF

*      Ensure that the length value used reflects the highest value found
*      even when MIRROR is true.
         IF (RLIM.LT.LEN2) RLIM=LEN2


*      Only allow graphical output if NVP is greater than 1. Since
*      otherwise there are too few points.
         IF (NVP.GT.1) THEN

*         Ask user for device name.
            AGIID=0
            GRAPH=.TRUE.
            CALL ERR_MARK
            CALL SEC1_AGICO(0,0,0,AGIID,STATUS)
            IF (STATUS.NE.SAI__OK) THEN
               GRAPH=.FALSE.
               CALL ERR_ANNUL(STATUS)
            END IF
            CALL ERR_RLSE

*         Display the un-analysed data as a graphical plot of radius (in 
*         some form) versus intensity (in some form).
            IF (GRAPH) THEN              
               CALL SEC1_GRAPH(1,ZEROP,RADISP,SURF,RLIM,BACK,NUMBER,
     :                         PSIZE,SIGMA,SUMMAT,CONS,GRAD,STATUS)  
               IF (STATUS.NE.SAI__OK) GOTO 9998
            END IF

*         Only allow fitting when more than two data points are present.
            IF (NVP.GT.2) THEN

*            Loop round until two sensible radius values are input.
               INOKAY=.FALSE.
               DO WHILE ((.NOT.INOKAY).AND.(STATUS.EQ.SAI__OK))

*               Get the two values from the keyboard
                  CALL PAR_GET1R('FITLIM',IND,INP,IND2,STATUS)

*               Check that the values were not inadvertantly reversed 
*               and swop them round so that the result can be sensible.
                  IF (INP(1).LT.INP(2)) THEN
                     LOR=INP(1)
                     HIR=INP(2)
                  ELSE
                     CALL MSG_BLANK(STATUS)
                     CALL MSG_OUT(' ','WARNING!!!',STATUS)
                     CALL MSG_OUT(' ','The low and high values were'/
     :                            /' swapped.',STATUS)
                     CALL MSG_BLANK(STATUS)
                     LOR=INP(2)
                     HIR=INP(1)
                  END IF

*               Check to see if it is possible for there to be two 
*               data points in the radius range required.
                  IF ((HIR-LOR)/PSIZE.LT.2.0) THEN
                     CALL MSG_OUT(' ','The radius range supplied, is '/
     :                           /' too narrow to be used.',STATUS)
                  ELSE
                     INOKAY=.TRUE.
                  END IF

               END DO
               IF (STATUS.NE.SAI__OK) GOTO 9999 

*            Calculate the scale length assuming spiral or elliptical.

*            Obtain the 'fit' parameters for linear fits to the brightness 
*            versus radius data (suitably transformed). 
                CALL SEC1_LINRE(LOR,HIR,NUMBER,SUMMAT,PSIZE,BACK,
     :                          RLIM,GRAD,CONS,COUNT,SLEN,STATUS)
                IF (STATUS.NE.SAI__OK) GOTO 9998

*            Display the fit plots if a graphics device was selected.
               IF (GRAPH) THEN

                  CALL SEC1_GRAPH(2,ZEROP,RADISP,SURF,RLIM,BACK,
     :                            NUMBER,PSIZE,SIGMA,SUMMAT,CONS,
     :                            GRAD,STATUS) 
                  IF (STATUS.NE.SAI__OK) GOTO 9998

*               Turn off the AGI/PGPLOT interface.
                  CALL SEC1_AGICO(1,0,0,AGIID,STATUS)

               END IF

            END IF

         END IF

*      Display the results on the default display.
*      Output a text file containing results if required.
         IF (NVP.GT.2) THEN
            CALL SEC1_TEXTD(FLAG,NDF1,XCO,YCO,OCOUNT,BACK,SIGMA,CONS,
     :                      GRAD,RLIM,PSIZE,COUNT,ZEROP,SLEN,
     :                      LOR,HIR,LBND,STATUS)    

            CALL SEC1_TEXTO(NDF1,SUMMAT,NUMBER,XCO,YCO,BACK,
     :                      SIGMA,CONS,RLIM,PSIZE,ZEROP,SLEN,LBND,
     :                      FIOD2,EXCLAIM,STATUS)
         END IF

 9998    CONTINUE

*      De-allocate the dynamic memory used.
         CALL PSX_FREE(POINT1(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the user selection of preparing a galaxy profile 
*      or not.
         CALL PAR_GET0L('AGAIN',AGAIN,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

         IF (AGAIN) THEN

*         Spacing to make things look more tidy.
            CALL MSG_BLANK(STATUS)

*         Cancel the parameters so that they must be reinput when
*         looping round.
            CALL SEC1_CANCL(1,STATUS)

         END IF

      END DO

 9999 CONTINUE

*   End the NDF context.
      CALL NDF_END(STATUS)                              

      END   


      SUBROUTINE SEC1_AGICO(ONOFF,MODE,NEW,AGIID,STATUS)
*+
*  Name:
*     SEC1_AGICO

*  Purpose:
*     Turns on/off the AGI/PGPLOT interface used for plotting the graphs.
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SEC1_AGICO(ONOFF,MODE,NEW,AGIID,STATUS)    

*  Description:
*     Depending on the value of ONOFF the subroutine either:-
*     sets up the AGI/PGPLOT interface and enters new information into
*     the AGI database (ONOFF=0) or closes down the database and
*     interface (ONOFF=1). The routine may be called to generate a display
*     on the device currently used or on a new device to be specified.

*  Arguments:                                     
*     ONOFF = INTEGER (Given)
*        Defines whether the routines controlling AGI/PGPLOT should
*        be turned on or off. 0=on 1=off 
*     MODE = INTEGER (Given)
*        Defines whether or not the ADAM parameter is the current
*        graphics device or a new one to be specified. 0=current 1=new.
*     NEW = INTEGER (Given)
*        Defines whether or not a new viewport should be created
*        and to determine if UPDATE or WRITE mode is required in
*        AGI_ASSOC. This influences the tranformation required.
*     AGIID = INTEGER (Given and Returned)
*        An AGI picture identifier used by AGI.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     8-July-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:                              
      INTEGER MODE                    ! Defines whether the display will
                                      ! be on a new device or the current
                                      ! one 0=current 1=new
      INTEGER NEW                     ! Defines the WRITE/UPDATE status
                                      ! and whether or not a new
                                      ! viewport transformation is required
      INTEGER ONOFF                   ! Defines whether AGI/PGPLOT
                                      ! must be turned on or off
                                      ! 0=on 1=off

*  Arguments Given and Returned:           
      INTEGER AGIID                   ! An AGI picture identifier
                                           
*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:
      REAL CR                         ! RGB red index of current 
                                      ! background colour
      REAL CG                         ! RGB green index of current
                                      ! background colour
      REAL CB                         ! RGB blue index of current
                                      ! background colour
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Start an AGI context.
      CALL AGI_BEGIN

*   Setup the AGI/PGPLOT interface.
      IF (ONOFF.EQ.0) THEN

*      Open AGI on a device obtained from the parameter system.
         IF (MODE.EQ.0) THEN 

*         Use a device to be specified by the user.
            IF (NEW.EQ.0) THEN

*            Get the name of the new device.
               CALL AGI_ASSOC('DEVICE','WRITE',AGIID,STATUS) 

*            Ensure that the whole screen is used.
               CALL AGI_IBASE(AGIID,STATUS)
               CALL AGI_SELP(AGIID,STATUS)

            ELSE

*            Update the graph display.
               CALL AGI_ASSOC('DEVICE','UPDATE',AGIID,STATUS)

            END IF
    
         ELSE

*         Associate the window in the correct mode.
            IF (NEW.EQ.0) THEN
               CALL AGI_ASSOC('IMGDEV','WRITE',AGIID,STATUS)
            ELSE
               CALL AGI_ASSOC('IMGDEV','UPDATE',AGIID,STATUS)
            END IF

         END IF
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Activate the PGPLOT interface to AGI.
         CALL AGP_ACTIV(STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Create a new viewport if required.
         IF (NEW.EQ.0) THEN
*         Create the new viewport.
            CALL AGP_NVIEW(.TRUE.,STATUS)
         ELSE
*         Use the old viewport information. No new border and transformations.
            CALL AGP_NVIEW(.FALSE.,STATUS)
         END IF
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Enquire details of the current background colour.
         CALL PGQCR(0,CR,CG,CB)

*      Set the pen colours (otherwise the output does not show on the IKON).
*      User colour index of 1 since it is accepted by all monochrome
*      devices.
         CALL PGSCR(1,1.0-CR,1.0-CG,1.0-CB)
         CALL PGSCI(1)

      END IF

 9999 CONTINUE

*   Closedown the AGI/PGPLOT interface.
      IF ((ONOFF.EQ.1).OR.(STATUS.NE.SAI__OK)) THEN

*      Save the current viewport in the AGI database.  
         CALL AGP_SVIEW('SECTOR','Galaxy Profile',AGIID,STATUS)

*      Close down PGPLOT. 
         CALL AGP_DEACT(STATUS)

*      Cancel the picture identifier or annul the parameter association 
*      depending on the value of STATUS.
         IF (STATUS.NE.SAI__OK) THEN

*         Cancel the AGI parameter association.       
            IF (MODE.EQ.0) THEN
               CALL AGI_CANCL('DEVICE',STATUS)
            ELSE
               CALL AGI_CANCL('IMGDEV',STATUS)
            END IF

         ELSE

*         Annul the AGI parameter association.       
            CALL AGI_ANNUL(AGIID,STATUS)

         END IF

      END IF

      END


      SUBROUTINE SEC1_AGIC2(GRADEV,ONOFF,NDFS,NAME,NDF1,DEVCAN,
     :                      PICID,STATUS)
*+
*  Name:
*     SEC1_AGIC2

*  Purpose:
*     Turns on/off the AGI/SGS/PGPLOT interface allowing line drawing
*     and a cursor using SGS, displaying graphs using PGPLOT and returning
*     the NDF identifier as required.
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SEC1_AGIC2(GRADEV,ONOFF,NDFS,NAME,NDF1,DEVCAN,PICID,STATUS)    

*  Description:
*     Depending on the value of ONOFF the subroutine either:-
*
*     Sets up the AGI interface, obtaining the most recent 'DATA' or 
*     'SECTOR' picture. Activates SGS so that PGPLOT and normal SGS 
*     routines can be used. PGPLOT is turned on/off to set up its colour
*     tables.
*
*     Also (if required) obtains the NDF identifier for the current picture
*     (if available).
*    
*     Closes down the above in an orderly fashion. (ONOFF=1).

*  Arguments:                
*     GRADEV *(6) = CHARACTER (Given)
*        The name of the graphiccs device used.                     
*     ONOFF = INTEGER (Given)
*        Defines whether the routines controlling AGI/PGPLOT should
*        be turned on or off. 0=on 1=off 
*     NDFS = INTEGER (Given)
*        Defines whether the routines obtaining the NDF used to generate
*        the current picture should be used. 0=No 1=Yes.
*     NAME = INTEGER (Given)
*        Defines whether DATA or SECTOR pictures are to looked at.
*        0=DATA 1=SECTOR
*     NDF1 = INTEGER (Returned)
*        NDF identifier for the picture required.
*     DEVCAN = LOGICAL (Given and Returned)
*        The device parameter is to be annuled when ONOFF=1.
*     PICID = INTEGER (Given and Returned)
*        An AGI picture identifier used by AGI.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     18-Jan-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PAR_ERR'               ! Parameter-system errors
      INCLUDE 'NDF_PAR'               ! NDF constants
      INCLUDE 'DAT_PAR'               ! DAT constants

*  Arguments Given:                            
      CHARACTER *(6) GRADEV           ! Graphics device name  
      INTEGER NAME                    ! Defines whether pictures of name
                                      ! DATA or SECTOR are to used
      INTEGER ONOFF                   ! Defines whether AGI/PGPLOT
                                      ! must be turned on or off
                                      ! 0=on 1=off

*  Arguments Returned.                                           
      INTEGER NDFS                    ! Should the NDF identifier be 
                                      ! returned?

*  Arguments Given and Returned:           
      LOGICAL DEVCAN                  ! Defines whether the current
                                      ! picture is to be retained at
                                      ! database closedown
      INTEGER NDF1                    ! An NDF identifier
      INTEGER PICID                   ! An AGI picture identifier

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local Variables:
      LOGICAL GOTLOC                  ! What type of identifer?
      CHARACTER *255 IDENT1           ! HDS identifier for the image
      CHARACTER *(DAT__SZLOC) IDENT   ! HDS identifier for the image
      INTEGER ZONID                   ! SGS zone identifier of the initial
                                      ! picture
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN
               
*   Set default value.
      DEVCAN=.FALSE.   

*   Setup the AGI/SGS/PGPLOT interface.
      IF (ONOFF.EQ.0) THEN

*      Start a new AGI context.
         CALL AGI_BEGIN

*      Get the graphics device, and open SGS.
         CALL AGI_ASSOC(GRADEV,'UPDATE',PICID,STATUS)
         IF (STATUS.NE.SAI__OK) THEN 
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Activate SGS.
         CALL AGS_ACTIV(STATUS)

*      If the graphics device was not available, report the error and
*      leave the programme.
         IF (STATUS.NE.SAI__OK) THEN 
            IF (STATUS.NE.PAR__ABORT) DEVCAN=.TRUE.
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Select the base picture as current so that the search for DATA 
*      or SECTOR later will look through all the pictures.
         CALL AGI_IBASE(PICID,STATUS)
         CALL AGI_SELP(PICID,STATUS)
         IF (STATUS.NE.SAI__OK) THEN 
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Create a new SGS_ZONE from current picture of correct name.
         IF (NAME.EQ.0) THEN
*         Find most recent image.
            CALL AGI_RCL('DATA',PICID,STATUS)
         ELSE
*         Find most recent SECTOR results display.
            CALL AGI_RCL('SECTOR',PICID,STATUS)
         END IF

*      Abort if it was impossible to find a suitable entry in the AGI database.
         IF (STATUS.NE.SAI__OK) THEN 
            DEVCAN=.TRUE.
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Select the AGI database entry selected as the current picture and
*      create the new zone.
         CALL AGI_SELP(PICID,STATUS)
         CALL AGS_NZONE(ZONID,STATUS)

*      Set up PGPLOT so that its colours are used.
         CALL AGP_ACTIV(STATUS)
         IF (STATUS.NE.SAI__OK) THEN 
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Try to get the value for the NDF identifier of the selected picture.
         IF (NDFS.EQ.1) THEN

*         Get a locator to the NDF associated with the DATA picture.
            CALL AGI_GTREF(PICID,'READ',IDENT1,STATUS)
            IF (STATUS.NE.SAI__OK) THEN
               CALL ERR_REP(' ',
     :         'Could not get the reference to an HDS.',STATUS)
               CALL ERR_FLUSH(STATUS)
               GOTO 9999
            END IF

*         Check to see if the identifier has been supplied in the old
*         DAT__SZLOC length format.
            CALL DAT_VALID(IDENT1(1:DAT__SZLOC),GOTLOC,STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 9999
 
*         Use NDF_FIND in a manner suiatble for the type of
*         identifier found.
            IF (GOTLOC) THEN 
               IDENT=IDENT1(1:DAT__SZLOC)
               CALL NDF_FIND(IDENT,' ',NDF1,STATUS)
            ELSE
               CALL NDF_FIND(DAT__ROOT,IDENT1,NDF1,STATUS)
            END IF
            IF (STATUS.NE.SAI__OK) THEN
               CALL ERR_REP(' ',
     :                      'Could not get the image NDF identifier.',
     :                      STATUS)
               GOTO 9999
            END IF
 
*         Display the name of the file in question.
            CALL NDF_MSG('IN2',NDF1)
            CALL MSG_OUT(' ','Using ^IN2 as the input NDF.',STATUS)

         END IF

      END IF

 9999 CONTINUE
 

*   Closedown the AGI/SGS interface.
      IF ((ONOFF.EQ.1).OR.(STATUS.NE.SAI__OK)) THEN

*      Deactivate PGPLOT.
         CALL AGP_DEACT(STATUS)

*      Deactivate SGS and close the workstation.
         CALL AGS_DEACT(STATUS)

*      Close the AGI context.
         CALL AGI_END(PICID,STATUS)

*      Close the AGI database. Record the name of the workstation only
*      if it was used successfully.
         IF (DEVCAN) THEN
            CALL AGI_CANCL(GRADEV,STATUS)
         ELSE
            CALL AGI_ANNUL(PICID,STATUS)
         END IF

      END IF

      END


      SUBROUTINE SEC1_AUTOL(AUTOL,ELEMS,PRANGE,OCOUNT,FLAG,
     :                      ARRAY,XCO,YCO,STATUS)
*+
*  Name:
*     SEC1_AUTOL

*  Purpose:
*     Looks at the pixels immediately surrounding the user defined galaxy
*     centre and then chooses a location that provides the biggest weighted
*     mean value. 
*
*     Does not generate a value greater than 1 pixel accuracy. For better
*     accuracy PISA, FOCAS or the ESP profiling application is recommended.

*  Language:
*     Starlink Fortran 77

*  Invocation:   
*     CALL SEC1_AUTOL(AUTOL,ELEMS,PRANGE,OCOUNT,FLAG,
*                     ARRAY,XCO,YCO,STATUS)   

*  Description:
*     Searches for the highest weighted mean pixel value in the region of the
*     image immediately surrounding the user input value. The region examined
*     is 11 pixels by 11 pixels and is searched using a 5 by 5 square.

*  Arguments:               
*     AUTOL = LOGICAL (Given)
*        Defines whether autolocate is to be used.                     
*     ELEMS = INTEGER (Given)               
*        Number of elements/pixels in the image array. Units pixels.
*     PRANGE(2) = INTEGER (Given)
*        Length of the X and Y axes of the image. Units pixels.
*     OCOUNT = REAL (Returned)
*        The count value found at the galaxy origin. Units counts.          
*     FLAG = INTEGER (Returned)
*        It was not possible to find the central pixel value flag.
*     ARRAY(ELEMS) = REAL (Given and Returned)
*        The image array. Contains the count values for all the image pixels.
*        Units counts.
*     XCO = REAL (Given and Returned)
*        X index of the galaxy centre/origin supplied by the user.
*        Units pixels.
*     YCO = REAL (Given and Returned)
*        Y index of the galaxy centre/origin supplied by the user.
*        Units pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status value.

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
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Status:     
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      LOGICAL AUTOL                   ! Defines whether autolocation of the
                                      ! object centre is to be used.
      INTEGER ELEMS                   ! Number of elements/pixels in the 
                                      ! image array
      INTEGER PRANGE(2)               ! Length of the X and Y axes of the 
                                      ! image

*  Arguments Returned:
      INTEGER FLAG                    ! It was not possible to find the 
                                      ! central pixel value flag
      REAL OCOUNT                     ! The count value found at the pixel
                                      ! used as the galaxy origin

*  Arguments Given and Returned:
      REAL ARRAY(ELEMS)               ! The image array contains the count 
                                      ! values for all the image pixels
      REAL XCO                        ! X index of the galaxy centre/origin 
                                      ! supplied by the user
      REAL YCO                        ! Y index of the galaxy centre/origin
                                      ! supplied by the user

*  Local variables:
      INTEGER ADDRES                  ! Array address of the element
                                      ! corresponding to pixel indices X,Y 
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Temporary counter
      INTEGER X                       ! Current X index
      INTEGER XMAX                    ! Highest image pixel X index examined
      INTEGER XMIN                    ! Lowest image pixel X index examined
      INTEGER YMAX                    ! Highest image pixel Y index examined
      INTEGER YMIN                    ! Lowest image pixel Y index examined
      INTEGER Y                       ! Current Y index
      REAL MAX                        ! Maximum weighted average pixel value
      REAL NEWX                       ! X value of pixel with highest
                                      ! weighted surrounding values
      REAL NEWY                       ! Y value of the pixel with the highest
                                      ! weighted surrounding values
      REAL TOTAL                      ! Weighted pixel count total
      REAL VALUE                      ! Current pixel count value
      REAL WTOTAL                     ! Weighting total
      REAL WEIGHT                     ! Weighting value used when summing the 
                                      ! pixel count values about a given point
      
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN
               
*   Set a flag to indicate if the pixel count value could be determined.
      FLAG=0
 
*   Use autolocation if selected.
      IF (AUTOL) THEN

*      Set up the minimum and maximum image limits.
         XMIN=1
         XMAX=PRANGE(1)
         YMIN=1
         YMAX=PRANGE(2)

*      Set up the initial indices for the pixel with the highest weighted value. 
         NEWX=XCO
         NEWY=YCO
         MAX=VAL__MINR

*      Loop through all pixels nearby to the chosen origin.
         DO 10 X=XCO-5,XCO+5

            DO 15 Y=YCO-5,YCO+5

*            Avoid choosing an off image origin.
               IF ((X.GE.XMIN).AND.(X.LE.XMAX).AND.
     :             (Y.GE.YMIN).AND.(Y.LE.YMAX)) THEN
    
*               Initialise the pixel total and its weighting sum.
                  TOTAL=0.0
                  WTOTAL=0.0

*               Look at the pixels immediately adjacent to the current pixel.
*               Also check that they are within the image bounds.
                  DO 20 I=X-2,X+2

                     DO 25 J=Y-2,Y+2

*                     Avoid using points that are outside the image.
                        IF ((I.GE.XMIN).AND.(I.LE.XMAX).AND.
     :                      (J.GE.YMIN).AND.(J.LE.YMAX)) THEN

*                        Find the address of one of the surrounding pixels.
                           ADDRES=(J-1)*XMAX+I

*                        Find the pixel value.
                           VALUE=ARRAY(ADDRES)
       
*                        Check that the pixel is not bad.
                           IF (VALUE.NE.VAL__BADR) THEN

*                           Calculate the weighting value.
*                           An arbitrary method.
                              WEIGHT=1./(1.+SQRT(REAL((I-X)*(I-X)
     :                               +(J-Y)*(J-Y))))

*                           Add the weighted pixel value to the summation 
*                           and then add the current weighting value to 
*                           the sum of all the weights for the current 
*                           X/Y location.
                              TOTAL=TOTAL+VALUE*WEIGHT
                              WTOTAL=WTOTAL+WEIGHT

                           END IF

                        END IF

 25                  CONTINUE

 20               CONTINUE

*               Check to see if any legal points were found.
                  IF (WTOTAL.GT.0.0) THEN

*                  Calculate the weighted mean pixel value surrounding the 
*                  current X/Y value. Keep it and its co-ords if it is bigger 
*                  than the biggest found so far.
                     IF (TOTAL/WTOTAL.GT.MAX) THEN
                        MAX=TOTAL/WTOTAL
                        NEWX=X
                        NEWY=Y
                     END IF
 
                  END IF

               END IF

 15         CONTINUE

 10      CONTINUE

*      Transfer the new centre location to the XCO YCO variables. Also,
*      pass back the value of the pixel chosen.
         XCO=NEWX
         YCO=NEWY
         OCOUNT=ARRAY((YCO-1)*XMAX+XCO)

      ELSE

*      Obtain the value of the pixel at XCO,YCO if autolocate 
*      is not selected.
         OCOUNT=ARRAY((YCO-1)*XMAX+XCO)

*      Correct and issue warning if the value is bad.
         IF (OCOUNT.EQ.VAL__BADR) THEN
            CALL MSG_BLANK(STATUS)
            CALL MSG_OUT(' ','WARNING!!!',STATUS)
            CALL MSG_OUT(' ','The central pixel was bad.',STATUS)
            CALL MSG_BLANK(STATUS)
            OCOUNT=0.0
            FLAG=1
         END IF

      END IF

 9999 CONTINUE
      
      END


      SUBROUTINE SEC1_CONV(RADISP,SURF,BACK,SIGMA,ZEROP,FLAG,RADIUS,
     :                     BRIGHT,STATUS)
*+
*  Name:
*     SEC1_CONV

*  Purpose:
*     Transforms the radius and pixel count value into the currently 
*     required format. Returns an error flag if one of the values is 
*     unusable. Is employed when displaying data points or data
*     fits.
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SEC1_CONV(RADISP,SURF,BACK,SIGMA,ZEROP,FLAG,RADIUS,BRIGHT,STATUS)    

*  Description:
*     Depending on the value of RADISP the subroutine converts the values
*     for radius and pixel count into the appropriate form. These may be
*     R, R**2, R**.5 or Log10(R) (in the case of R) and Log10(I-BACK)
*     (I-BACK)/SIGMA in the case of pixel count value.

*  Arguments:                                     
*     RADISP = CHAR (Given)
*        Character variable denoting the format to be used for the radius
*        value. R=Linear Q=Quarter power, L=Logarithmic and S=Squared.
*     SURF = LOGICAL (Given)
*        Logical variable denoting the format to be used for the brightness.
*        FALSE=Sigma and TRUE=Suyrface brightness.
*     BACK = REAL (Given)
*        The background count value for the image. Units counts.
*     SIGMA = REAL (Given)
*        The standard deviation of the image background. Units counts.
*     ZEROP = REAL (Given)
*        Zero point of the brightness scale. Units magnitudes.
*     FLAG = INTEGER (Returned)
*        Set to non-zero if the radius or brightness value could not
*        be transformed. eg log of a negative number.
*     RADIUS = REAL (Given and Returned)
*        The radius value to be transformed. Units arc seconds.
*     BRIGHT = REAL (Given and Returned)
*        The pixel count value to be transformed. Units counts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     30-Nov-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:                              
      CHARACTER RADISP *(256)         ! Defines the type of transformation
                                      ! to be applied to the radius value
      LOGICAL SURF                    ! Denotes how the pixel count is 
                                      ! to be transformed i.e. surface brightness
                                      ! or in terms of sky
      REAL BACK                       ! Pixel count value for the background
      REAL SIGMA                       ! Standard deviation of the background
                                      ! count value
      REAL ZEROP                      ! The zero point of the magnitude scale

*  Arguments Returned:
      INTEGER FLAG                    ! Indicates whether or not the 
                                      ! transformations were successful
*  Arguments Given and Returned:           
      REAL BRIGHT                     ! The pixel count value to be
                                      ! transformed
      REAL RADIUS                     ! The radius value to be transformed    

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set the transformation successful? flag.
      FLAG=0

*   Perform pixel count transformations.

*   Convert the pixel values to sigma or surface brightness.
      IF (.NOT.SURF) THEN        
         BRIGHT=(BRIGHT-BACK)/SIGMA
      ELSE                       
         BRIGHT=(BRIGHT-BACK)
         IF (BRIGHT.GT.0.0) THEN
            BRIGHT=ZEROP-2.5*LOG10(BRIGHT)
         ELSE
*         Avoid calculating Log of zero or a negative number.
            FLAG=1
         END IF
      END IF

*   Perform radius transformations.

*   Linear radius transformation.
      IF (RADISP.EQ.'R') RADIUS=RADIUS

*   Quarter power radius transformation.
      IF (RADISP.EQ.'Q') RADIUS=RADIUS**(0.25)

*   Logarithmic radius transform.
      IF (RADISP.EQ.'L') THEN
         IF (RADIUS.GT.0.0) THEN
            RADIUS=LOG10(RADIUS)
         ELSE
*         Avoid calculation Log of zero or a negative number.
            FLAG=-1
         END IF
      END IF

*   Squared radius transformation.
      IF (RADISP.EQ.'S') RADIUS=RADIUS*RADIUS

 9999 CONTINUE

      END


      SUBROUTINE SEC1_CURSO(GRADEV,POINT,NAME,POSANG,COLOUR,NDF1,
     :                      X,Y,RADIUS,ANGWID,STATUS)
*+
*  Name:
*     SEC1_CURSO

*  Purpose:
*     Multi-purpose routine that allows use of the SGS cursor for returning 
*     the co-ordinate for a given type of image and also controls all SGS 
*     graphics displays (such as that displaying the shape/position of the 
*     user defined sector). The routine is used for more than one purpose to 
*     avoid unecessary duplication of code.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SEC1_CURSO(GRADEV,POINT,NAME,POSANG,COLOUR,NDF1,X,Y,
*                     RADIUS,ANGWID,STATUS)

*  Arguments:
*     GRADEV *(6) = CHARACTER (Given)
*        Name of the graphics device to be used.
*     POINT = INTEGER (Given)
*        Specifies what action is to be taken by the subroutine.
*     NAME = INTEGER (Given)
*        Defines whether or not pictures of name DATA or SECTOR will
*        be located.
*     POSANG = REAL (Given)
*        Position angle of the sector. Units radians.
*     COLOUR = INTEGER (Given)
*        Colour to be used when drawing the galaxy centre.
*     NDF1 = INTEGER (Given and Returned)
*        NDF identifier for the current picture.
*     X(10) = REAL (Given and Returned)
*        Co-ordinate information obtained via the cursor or to be
*        displayed on the workstation.
*     Y(10) = REAL (Given and Returned)
*        Co-ordinate information obtained via the cursor or to be
*        displayed on the workstation.
*     RADIUS = REAL (Given and Returned)
*        Radius of the sector to be used.
*     ANGWID = REAL (Given and Returned)
*        Angular width of the sector. Units radians.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The routine undertakes several different tasks. These have been
*     placed together in one routine to avoid unnecessary duplication 
*     of code. The tasks undertaken are:
*
*     - allowing the user to use the SGS cursor to specify the location of 
*     the sector to be used and the quadrant in which the graphical results 
*     display are to be shown. The routines include text messages to be 
*     shown to instruct the user. 
*    
*     - return a locator/identifier value from the AGI database, that allows 
*     the NDF that was used to generate the most recently displayed image 
*     named DATA, to be accessed.
*
*     - allow simple SGS routines to be used to display lines etc on top
*     of the image represented in the AGI database by the entry most
*     recently named DATA.
*
*     - inspecting the AGI database to ensure that (as required) the co-ordinate
*     values are being returned for the file most recently stored with 
*     the database name DATA (as with an image) or from a database entry named
*     SECTOR (as with a results graph).
*
*     - sets up a new AGI databse entry (SECTOR) to define part of the screen
*     so that PGPLOT routines may be used to update the display and show 
*     the results graphically in a form more sophisticated than SGS would 
*     normally allow.
*
*     - close down the AGI resources and SGS at the end of each call so
*     that confusion may be avoided at the calling routines.

*  Notes:
*     This program is a massively disembowelled version of KAPPA program
*     CURSOR with a few bits of ZAPLIN used here and there. 
*
*     The application only acts on the most recent picture in the 
*     graphics database named 'DATA' and also an entry called 'SECTOR' which
*     contains a graphical display of the profile results.
*
*    Within ESP the scale lengths are calculated by assuming an
*     exponential brightness profile for spiral galaxies and a
*     quarter power law for elliptical galaxies. The scale length
*     value given is derived from the decay constant of the 
*     exponential functions. 
 
*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     See KAPPA CURSOR and ZAPLIN for their history.
*     Original Version: 05/01/93
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'AGI_ERR'          ! AGI error constants

*  Arguments given:
      CHARACTER *(6) GRADEV      ! Name of the graphics device
      INTEGER COLOUR             ! Colour of the galaxy centre marker
      INTEGER NAME               ! Whether pictures of name DATA or SECTOR
                                 ! are to be used 0=DATA 1=SECTOR
      INTEGER POINT              ! Which of the describing points is being
                                 ! selected
      REAL POSANG                ! Position angle of the sector

*  Arguments Given and Returned.
      INTEGER NDF1               ! NDF identifier for the current picture
      REAL ANGWID                ! Angular width of the sector
      REAL X(10)                 ! Position information from the cursor 
                                 ! or to be displayed on the workstation
      REAL Y(10)                 ! Position information from the cursor 
                                 ! or to be displayed on the workstatio
      REAL RADIUS                ! Radius of the sector to be used in
                                 ! world units

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER *80 IMGMES(4)    ! Informational messages if device is
                                 ! an image display
      CHARACTER *80 TERMES(4)    ! Informational messages if device is
                                 ! a terminal

      INTEGER HITVAL             ! The selected choice of the cursor
      INTEGER NIMGMS             ! Number of lines of image-display
                                 ! messages
      INTEGER NTERMS             ! Number of lines of terminal messages
      INTEGER PICID              ! Current (input) picture identifier
      REAL CURSIZ                ! Size of the graphics cursor
      REAL TEMPX                 ! Temporary storage
      REAL TEMPY                 ! Temporary storage
      REAL X1,Y1                 ! Lower-left corner of the initial
                                 ! picture
      REAL X2,Y2                 ! Upper-right corner of the initial
                                 ! picture
      REAL XIN                   ! x co-ordinate as measured by the
                                 ! cursor
      REAL XM,YM                 ! Size of the initial picture
      REAL YIN                   ! y co-ordinate as measured by the
                                 ! cursor

      LOGICAL CURCHO             ! Cursor is available with suitable
                                 ! number of choices
      LOGICAL DEVCAN             ! The device parameter is to be
                                 ! cancelled
      LOGICAL IMGDIS             ! Device is nominally an image display

*.

*   Check inherited global status.

      IF (STATUS.NE.SAI__OK) RETURN

*   Create informational messages for use with the cursor.
      CALL SEC1_MESSG(POINT,TERMES,IMGMES,NTERMS,NIMGMS,STATUS)
      
*   Start the graphics system. If this is the first time the routine has 
*   been used then an identifier/locator to the NDF for the displayed 
*   image is returned as NDF1.
      IF (POINT.EQ.0) THEN 
         CALL SEC1_AGIC2(GRADEV,0,1,NAME,NDF1,DEVCAN,
     :                   PICID,STATUS)
         POINT=1
      ELSE
         CALL SEC1_AGIC2(GRADEV,0,0,NAME,NDF1,DEVCAN,PICID,STATUS)
      END IF
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Set initial cursor position of the current picture. When defining the
*   sector to be used, the last location selected is supplied as the initial
*   position. Also re-establishes the screen limits.
      CALL SGS_IZONE(X1,X2,Y1,Y2,XM,YM)
      IF ((POINT.NE.2).AND.(POINT.NE.3).AND.(POINT.NE.9)) THEN
         XIN=0.5*(X1+X2)
         YIN=0.5*(Y1+Y2)
      ELSE 
         XIN=X(POINT-1)
         YIN=Y(POINT-1)
      END IF  
      
*   Actually sets the position (code above calculated it).
      CALL SGS_SETCU(XIN,YIN)
      CURSIZ=0.004*MIN(X2-X1,Y2-Y1)

*   Draw the sector(s) that will be used and then exit. Is done here so that
*   the value for X1 and Y1 need not be retained between calls to this 
*   routine.
      IF ((POINT.EQ.4).OR.(POINT.EQ.5)) THEN
         CALL SEC1_GRBIT(POINT,CURSIZ,X,Y,POSANG,
     :                   ANGWID,RADIUS,COLOUR,STATUS)
         GOTO 980
      END IF
   

*   Set up the screen sector that will be used to display the graph results.
      IF (POINT.EQ.7) THEN

*      Set up the x and y range divided by 2.
         TEMPX=(X2-X1)/2.
         TEMPY=(Y2-Y1)/2.

*      Sort out the x co-ordinates for the quadrant position required.
         IF (X(6)-X1.LT.TEMPX) THEN
            X(6)=X1
            X(7)=X1+TEMPX
         ELSE
            X(6)=X1+TEMPX
            X(7)=X2
         END IF

*      Sort out the y co-ordinates for the quadrant required. 
         IF (Y(6)-Y1.LT.TEMPY) THEN
            Y(6)=Y1
            Y(7)=Y1+TEMPY
         ELSE           
            Y(6)=Y1+TEMPY
            Y(7)=Y2
         END IF

*      Draw the box showing the quadrant being used.
         CALL SEC1_GRBIT(POINT,CURSIZ,X,Y,POSANG,
     :                   ANGWID,RADIUS,COLOUR,STATUS)

*      Sort out the x co-ordinates for within the quadrant required.
         IF (X(6)-X1.LT.TEMPX) THEN
            X(6)=X1+TEMPX*.15
            X(7)=X1+TEMPX*.9
         ELSE
            X(6)=X1+TEMPX*1.15
            X(7)=X1+TEMPX*1.9
         END IF

*      Sort out the y co-ordinates for within the quadrant required. 
         IF (Y(6)-Y1.LT.TEMPY) THEN
            Y(6)=Y1+TEMPY*.15
            Y(7)=Y1+TEMPY*.85
         ELSE           
            Y(6)=Y1+TEMPY*1.15
            Y(7)=Y1+TEMPY*1.85
         END IF

*      Setup the new entry in the database.
         CALL AGI_NUPIC(X(6),X(7),Y(6),Y(7),'SECTOR','Galaxy Profile',
     :                 0.0,1.0,0.0,1.0,PICID,STATUS)
         IF (STATUS.NE.SAI__OK) THEN
            STATUS=SAI__ERROR
            CALL ERR_REP(' ','Could not create a new picture in the'/
     :                   /' AGI database.',STATUS)
         END IF
         GOTO 980

      END IF

*   Put out a blank line to ensure the commentary appears on the alpha
*   plane of the terminal.
      CALL MSG_BLANK(STATUS)

*   Prepare the cursor for use.
      CALL SEC1_PRPCUR(1,3,TERMES,NTERMS,IMGMES,NIMGMS,'12 .',
     :            CURCHO,IMGDIS,STATUS)
      IF ((.NOT.CURCHO).OR.(STATUS.NE.SAI__OK)) GOTO 980

*   Initialise HITVAL before the main loop is entered.
      HITVAL=0

*   Loop until the point is selected.
*   Values 4 taken as the select.
*   Value 2 as an emergency exit.
*   Values 1 and 3 used to show the current position.
      DO WHILE ((HITVAL.NE.4).AND.(STATUS.EQ.SAI__OK))

*      Start a new error context.
         CALL ERR_MARK

*      If a message has already been displayed, and then the cursor
*      is used, the next message is no longer in synchronisation
*      with the cursor. So synchronise the message system.
         CALL MSG_SYNC(STATUS)

*      Read the cursor position and button value.
         CALL SGS_REQCU(XIN,YIN,HITVAL)

*      Emergency exit.
         IF (HITVAL.EQ.2) THEN
            CALL MSG_BLANK(STATUS)
            STATUS=SAI__ERROR
            CALL ERR_REP(' ','You have opted to leave the'/
     :                   /' program.',STATUS)
            GOTO 980
         END IF
 
*      Convert the world co-ordinates to data system.
         IF ((HITVAL.EQ.1).OR.(HITVAL.EQ.3).OR.(HITVAL.EQ.4)) THEN
            X(POINT)=XIN
            Y(POINT)=YIN
*         Display the cursor results if necessary.
            IF (POINT.LT.5.AND.(HITVAL.EQ.1.OR.HITVAL.EQ.3)) THEN
               CALL SEC1_CURVD(POINT,X1,Y1,XIN,YIN,STATUS)
            END IF
         END IF
 
*      Release the new error context.
         CALL ERR_RLSE

      END DO

*   Draw the currently required part of the sector.
*   1 = cross at centre, 2 = line in correct direction.
      IF ((POINT.EQ.1).OR.(POINT.EQ.2)) THEN
         CALL SEC1_GRBIT(POINT,CURSIZ,X,Y,POSANG,
     :                   ANGWID,RADIUS,COLOUR,STATUS)
      END IF
  
*   Convert the world co-ordinate to data co-ordinates so that it can be
*   transfered on return to SEC1_CMODE.
      X(10)=REAL(INT(X(1)-X1+1.))
      Y(10)=REAL(INT(Y(1)-Y1+1.))

 980  CONTINUE

*   Closedown the AGI/SGS/PGPLOT interface.
      CALL SEC1_AGIC2(GRADEV,1,0,NAME,NDF1,DEVCAN,PICID,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*    Exit point for errors that occurred before the graphics device
*    was opened.

 9999 CONTINUE

      END


      SUBROUTINE SEC1_CURVD(POINT,X1,Y1,XW,YW,STATUS)
*+
*  Name:
*     SEC1_CURVD

*  Purpose:
*     Displays information informing the user what the latest value 
*     is for the cursor position. 

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SEC1_CURVD(POINT,X1,Y1,XW,YW,STATUS)

*  Description:
*     Depending on the value of POINT, the routine displays the latest
*     value for the cursor position. When an image is being displayed 
*     output is in the form of world and data co-ordinates.
*
*     In some cases no messages at all are displayed.
*

*  Arguments:
*     POINT = INTEGER (Given)
*        Defines which values will be displayed is required.
*     X1 = REAL (Given)
*        X world co-ordinate of the left-hand edge of the image.
*     Y1 = REAL (Given)
*        Y world co-ordinate of the bottom edge of the image.
*     XW = REAL (Given)
*        X world co-ordinate.
*     YW = REAL (Given)
*        Y world co-ordinate.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     15-Feb-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
                     
*  Arguments Given:
      INTEGER POINT                   ! Defines how the cursor position
                                      ! is displayed
      REAL XW                         ! X world co-ordinate
      REAL X1                         ! X world co-ordinate of the image
                                      ! edge
      REAL YW                         ! Y world co-ordinate
      REAL Y1                         ! Y world co-ordinate of the image
                                      ! bottom

*  Arguments Returned:

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local Variables:                                               
      REAL TEMPX                      ! Temporary X value
      REAL TEMPY                      ! Temporary Y value
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Sector cursor position when on the image.
      IF ((POINT.NE.6).AND.(POINT.NE.8).AND.(POINT.NE.9)) THEN

*      Put the data co-ordinates into message token.
         TEMPX=REAL(INT(XW-X1+1.))
         TEMPY=REAL(INT(YW-Y1+1.))
         CALL MSG_SETR('XVALD',TEMPX)
         CALL MSG_SETR('YVALD',TEMPY)

*      Put the world co-ordinates into message tokens.
         CALL MSG_SETR('XVALW',XW)
         CALL MSG_SETR('YVALW',YW)

*      Display the current X and Y values.
         CALL MSG_OUT(' ','Cursor position (x/y)'/  
     :                /' ^XVALW, ^YVALW (world), '/
     :                /'^XVALD, ^YVALD (data)',STATUS)
  
*      The following call achieves graphics/text synchronisation.
         CALL MSG_SYNC(STATUS)

      END IF

      END


      SUBROUTINE SEC1_GRBIT(MODE,CURSIZ,X,Y,POSANG,
     :                      ANGWID,RADIUS,COLOUR,STATUS)
*+
*  Name:
*     SEC1_GRBIT

*  Purpose:
*     Generates the graphics required when the sector is specified by 
*     the user and when the quadrant to be used fro the results graph is
*     cleared. 
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SEC1_GRBIT(MODE,CURSIZ,X,Y,POSANG,ANGWID,RADIUS,COLOUR,STATUS)

*  Description:
*     Generates the graphics showing the sector specified by the user. 
*     Employs SGS to do so.
*
*     The variable MODE defines whcih part of the sector is to be
*     drawn: ie
*        
*        MODE = 1  Draw the cross at the centre of the chosen sector
*        MODE = 2  Draw a line outward from the sector centre to its
*                  edge in the direction specified
*        MODE = 3  Not implemented (no drawing required)
*        MODE = 4  Draw the sector to be used and highlight its centre
*                  and the ends of the arc
*        MODE = 5  As for MODE=4 but for the equivalent sector placed
*                  at an angle increased by 180 degrees
*        MODE = 6  Not implemented
*        MODE = 7  Draw the window within which the results graph will be
*                  displayed


*  Arguments:         
*     MODE = INTEGER (Given)
*        Indicates which part of the sector drawing is to take place.
*     CURSIZ = REAL (Given)
*        The size of the cross to be drawn at the first point specified.
*     X(10) = REAL (Given)
*        X co-ordinates for various parts of the sector to be drawn.
*     Y(10) = REAL (Given)
*        Y co-ordinates for various parts of the sector to be drawn.
*     POSANG = REAL (Given)
*        Position angle of the sector. Units radians.
*     ANGWID = REAL (Given) 
*        Angular width of the sector. Units radians.
*     RADIUS = REAL (Given)
*        Radius out from the chosen sector origin that the sector
*        should extend.
*     COLOUR = REAL (Given)
*        Colour to be used when drawing the galaxy centre marker.
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
      INCLUDE 'sec_par'               ! SECTOR constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Arguments Given:                                   
      INTEGER COLOUR                  ! The colour of the galaxy centre marker
      INTEGER MODE                    ! Which part of the sector drawing 
                                      ! is to take place.
      REAL ANGWID                     ! Angular width of the sector 
      REAL CURSIZ                     ! Size of the cross to be drawn at 
                                      ! the first point specified.
      REAL POSANG                     ! Position angle of the sector
      REAL RADIUS                     ! Radius out from the chosen sector 
                                      ! origin that the sector should extend.
      REAL X(10)                      ! X co-ordinates for various parts of 
                                      ! the sector to be drawn.
      REAL Y(10)                      ! Y co-ordinates for various parts of 
                                      ! the sector to be drawn.

*  Arguments Given and Returned:        

*  Arguments Returned:

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:
      REAL TEMP1                      ! Temporary value
      REAL TEMP2                      ! Temporary value
      REAL X4                         ! X Co-ordinate of one end of the arc
      REAL X5                         ! X Co-ordinate of the other 
                                      ! end of the arc
      REAL Y4                         ! Y Co-ordinate of one end of the arc
      REAL Y5                         ! Y Co-ordinate of the other 
                                      ! end of the arc

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN


*   Draw the cross at the centre of the proposed sector.
      IF (MODE.EQ.1) THEN         
         CALL SGS_SPEN(COLOUR)
         CALL SGS_LINE(X(1)-CURSIZ,Y(1),X(1)+CURSIZ,Y(1))
         CALL SGS_LINE(X(1),Y(1)-CURSIZ,X(1),Y(1)+CURSIZ)
      END IF

*   Draw a line from the centre of the sector to its proposed end.
      IF (MODE.EQ.2) THEN
         CALL SGS_SPEN(COLOUR)
         CALL SGS_LINE(X(1),Y(1),X(2),Y(2))
      END IF

*   Draw lines showing the edge of the sector required and then join
*   them together with an arc.
      IF ((MODE.EQ.4).OR.(MODE.EQ.5)) THEN

*      Determine the endpoints of each side of the arc.
         TEMP1=POSANG-ANGWID/2.
         TEMP2=POSANG+ANGWID/2.
         CALL SGS_SPEN(COLOUR)
         IF (MODE.EQ.4) THEN
            X4=X(1)+SIN(TEMP1)*RADIUS 
            Y4=Y(1)+COS(TEMP1)*RADIUS   
            X5=X(1)+SIN(TEMP2)*RADIUS
            Y5=Y(1)+COS(TEMP2)*RADIUS   
         ELSE
            X4=X(1)-SIN(TEMP1)*RADIUS 
            Y4=Y(1)-COS(TEMP1)*RADIUS   
            X5=X(1)-SIN(TEMP2)*RADIUS
            Y5=Y(1)-COS(TEMP2)*RADIUS   
         END IF
                       
*      Draw the sides of the sector requested.
         CALL SGS_LINE(X(1),Y(1),X4,Y4)
         CALL SGS_LINE(X(1),Y(1),X5,Y5)

*      Calculate the angles of the sides in the angular convention
*      required for SGS_ARC and then draw the arc.
         IF (MODE.EQ.4) THEN 
            TEMP1=-TEMP1+SEC__PIVAL/2.
            TEMP2=-TEMP2+SEC__PIVAL/2.
         ELSE
            TEMP1=-TEMP1+SEC__PIVAL*3./2.
            TEMP2=-TEMP2+SEC__PIVAL*3./2.
         END IF
         CALL SGS_ARC(X(1),Y(1),RADIUS,TEMP1,TEMP2)

*      Draw dots at the end of the arcs.
         CALL SGS_SPEN(COLOUR)
         CALL SGS_LINE(X4,Y4,X4,Y4)
         CALL SGS_LINE(X5,Y5,X5,Y5)

      END IF

*   Clear the quadrant of the window where the results will be displayed 
*   and then draw a border around it.
      IF (MODE.EQ.7) THEN
         CALL SGS_SPEN(1)
         CALL SGS_CLRBL(X(6),X(7),Y(6),Y(7))
         CALL SGS_BOX(X(6),X(7),Y(6),Y(7))
      END IF


*   Flush any SGS errors.
 9999 CALL SGS_FLUSH

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
      INCLUDE 'sec_par'               ! SECTOR constants
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


      SUBROUTINE SEC1_LINRE(LOR,HIR,NUMBER,SUMMAT,PSIZE,BACK,
     :                      NUMDAT,GRAD,CONS,COUNT,SLEN,STATUS)
*+
*  Name:
*     SEC1_LINRE

*  Purpose:
*     Determines a least squares linear fit for data in arrays X and Y. 
*     This fit is used to calculate value for the scale length of the
*     object if it was a spiral galaxy or elliptical galaxy.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SEC1_LINRE(LOR,HIR,NUMBER,SUMMAT,PSIZE,BACK,
*                     NUMDAT,GRAD,CONS,COUNT,SLEN,STATUS)

*  Description:
*     Uses a normal least squares method to determine the coefficients
*     of a linear fit to the contents of arrays X and Y.

*  Arguments:
*     LOR = REAL (Given)
*        The radius below which a data point should not be included
*        in the linear regression. Units arc seconds.
*     HIR = REAL (Given)
*        The radius above which a data point should not be included
*        in the linear regression. Units arc seconds.
*     NUMBER(SEC__RESUL) = REAL (Given)
*        The number of pixels averaged at a given distance.
*     SUMMAT(SEC__RESUL) = REAL (Given)
*        The summation of pixels counts at a given radius. Units counts.
*     PSIZE = REAL (Given)
*        Image pixel size in arc secs.
*     BACK = REAL (Given)
*        Image background count value. Units counts.
*     NUMDAT = INTEGER (Given)
*        The number of data (X/Y) pairs to be fitted.
*     GRAD(2) = REAL (Returned)
*        The gradient values for the linear fit.
*     CONS(2) = REAL (Returned)
*        The constant values for the linear fit.
*     COUNT(2) = INTEGER (Returned)
*        The number of data points used during the linear regression.
*     SLEN(2) = REAL (Returned)
*        The scale lengths of the two fits i.e. spiral and elliptical.
*        Units arc seconds.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     15-Nov-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'sec_par'               ! SECTOR constants
                     
*  Arguments Given:
      INTEGER NUMDAT                  ! Number of data points
      REAL BACK                       ! The background count value
      REAL HIR                        ! The upper limit of radius requested  
      REAL LOR                        ! The lower limit of radius requested
      REAL NUMBER(SEC__RESUL)         ! Number of pixels found at a given 
                                      ! distance from the galaxy centre
      REAL PSIZE                      ! Size of the pixels in arc secs
      REAL SUMMAT(SEC__RESUL)         ! Summed counts for all pixels at a
                                      ! given radius

*  Arguments Returned:
      INTEGER COUNT(2)                ! Number of data points used
      REAL CONS(2)                    ! Constant of linear equation
      REAL GRAD(2)                    ! Gradient of linear equation     
      REAL SLEN(2)                    ! Scale length of the galaxy

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local Variables:                                               
      INTEGER FLAG                    ! Problems with the regresssion flag
      INTEGER I                       ! Loop variable
      INTEGER TYPE                    ! Defines the data transform type
      REAL MNVX                       ! Mean value of X1 array
      REAL NUMB                       ! Number of data points
      REAL SUMX                       ! Sum of X1 array
      REAL SUMY                       ! Sum of Y1 array
      REAL TOT1                       ! Absolute X1 deviation from
                                      ! the mean
      REAL TOT2                       ! Absolute X1 deviation squared
                                      ! sum
      REAL TOT3                       ! Absolute X1 deviation 
                                      ! times Y1
      REAL X                          ! Transformed raw data radius value
      REAL Y                          ! Transformed raw data pixel count

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN
   
*   Find the mean value of x and the sums of the x and y arrays.

*   Set an error flag.
      FLAG=0

*   Calculate the linear regression for each of the gallaxy types.
      DO 200 TYPE=1,2

*      Setup the initial values for the sums and counter.
         SUMX=0.0
         SUMY=0.0
         MNVX=0.0
         COUNT(TYPE)=0

*      Loop through all the data points used.
         DO 500 I=1,NUMDAT

*         Ignore all values of radius where no data was collected. 
           IF (NUMBER(I).GT.0.0) THEN
           
*            Ignore all radius values for which the mean pixel
*            value was below background.
               IF ((SUMMAT(I)/NUMBER(I)-BACK).GT.0.0) THEN

*               Calculate the radius.
                  X=(REAL(I)-1.)*PSIZE

*               Only use the value if it is within the requested limits.
                  IF ((X.GE.LOR).AND.(X.LE.HIR)) THEN

*                  Increment the points used counter.
                     COUNT(TYPE)=COUNT(TYPE)+1

*                 Convert radius to form required for spiral galaxy or
*                 for an elliptical galaxy.
                     IF (TYPE.EQ.1) THEN
                        X=(REAL(I)-1.)*PSIZE
                     ELSE
                        X=((REAL(I)-1.)*PSIZE)**(0.25)
                     END IF
                     Y=LOG10(SUMMAT(I)/NUMBER(I)-BACK)
  
*                  Sum the X and Y values found (radius and brightness).
                      SUMX=SUMX+X
                      SUMY=SUMY+Y

                  END IF
               
               END IF

            END IF 

 500     CONTINUE

*      Raise an error flag since the number of data points was too low.
         IF (COUNT(TYPE).LT.2) THEN

            FLAG=1
            GOTO 200

         ELSE

*         Produce the mean value of X and a real representation of the number
*         of data points that contributed to the current mean.
            NUMB=REAL(COUNT(TYPE))
            MNVX=SUMX/NUMB

         END IF

*      Calculate the squared sum of (x-xmean) 
*      and thereby the gradient and constant terms in the equation.
     
*      Set up the initial values for the sums.
         TOT1=0.0
         TOT2=0.0
         TOT3=0.0
      
*      Loop through all the data points used.
         DO 510 I=1,NUMDAT

*         Use only those radii where data was collected.
            IF (NUMBER(I).GT.0.0) THEN

*            Ignore a data point if the mean value was below sky.
               IF (SUMMAT(I)/NUMBER(I)-BACK.GT.0.0) THEN

*               Calculate the radius.
                  X=(REAL(I)-1.)*PSIZE

*               Check that the radius is within the range requested.
                  IF ((X.GE.LOR).AND.(X.LE.HIR)) THEN
              
*                  Generate the radius and brightness values.              
                     IF (TYPE.EQ.1) THEN
                        X=(REAL(I)-1.)*PSIZE
                     ELSE
                        X=((REAL(I)-1.)*PSIZE)**(0.25)
                     END IF
                     Y=LOG10(SUMMAT(I)/NUMBER(I)-BACK)

*                  Calculate deviations from the mean.
                     TOT1=X-MNVX
                     TOT2=TOT2+TOT1*TOT1
                     TOT3=TOT3+TOT1*Y

                  END IF

               END IF

            END IF

 510     CONTINUE

*      Check that more than one value of pixel count was found.
         IF (ABS(TOT1).GT.SEC__VSMAL) THEN

*         Calculate the linear regression co-efficients.
            GRAD(TYPE)=TOT3/TOT2
            CONS(TYPE)=(SUMY-SUMX*GRAD(TYPE))/NUMB

*         Check that the gradient is physically reasonable.
            IF (GRAD(TYPE).LT.0.0) THEN

*            Calculate the scale length.
               IF ((TYPE.EQ.1).AND.(GRAD(TYPE).NE.0.0)) THEN
                  SLEN(TYPE)=-LOG10(EXP(1.0))/GRAD(TYPE)
               ELSE
                  SLEN(TYPE)=LOG10(EXP(1.0))/((-GRAD(TYPE))**(4.0))  
               END IF
            ELSE

*            Set the error flag since the gradient was zero or greater.
               FLAG=3

            END IF
     
         ELSE

*      Raise an error flag if all the pixels had the same value.
            FLAG=2

         END IF
 
 200  CONTINUE

*   Display the reason that the linear regression failed.
      IF (FLAG.GT.0) THEN

         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','WARNING!!!',STATUS)

*      Too few data points found for a result to be calculated.
         IF (FLAG.EQ.1) THEN
            CALL MSG_OUT(' ','Not enough data points '//
     :                       'were selected.',STATUS) 
         END IF

*      Not possible to calculate a meaningful slope.
         IF (FLAG.EQ.2) THEN
            CALL MSG_OUT(' ','The points selected had a '//
     :                       'single value.',STATUS) 
         END IF

*      Negative or zero scale length.
         IF (FLAG.EQ.3) THEN
            CALL MSG_OUT(' ','The scale length calculated '//
     :                       'was not physically sensible.',STATUS) 
         END IF

         CALL MSG_BLANK(STATUS)

      END IF

 9999 CONTINUE
     
      END       
       

      SUBROUTINE SEC1_MESSG(POINT,TERMES,IMGMES,NTERMS,NIMGMS,STATUS)
*+
*  Name:
*     SEC1_MESSG

*  Purpose:
*     Sets up the messages that are to be displayed with the cursor to
*     tell the user how to operate it and what input is currenlty being 
*     requested.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SEC1_MESSG(POINT,TERMES,IMGMES,NTERMS,NIMGMS,STATUS)


*  Description:
*     Depending on the value of POINT the routine assigns values to two 
*     character arrays. These are then used by subroutine SEC1_PRPCUR to
*     inform the user what is required. Also assigns value to NITERMS and
*     NIMGMS to define how many lines of text there are in each message.

*  Arguments:
*     POINT = INTEGER (Given)
*        Defines which of the messages is required.
*     TERMES(4) = CHARACTER*80 (Returned)
*        Messages if device is a terminal.
*     IMGMES(4) = CHARACTER*80 (Returned)
*        Messages if device is an image display. 
*     NTERMS = INTEGER (Returned)
*        Number of lines of terminal text.
*     NIMGMS = INTEGER (Returned)
*        Number of lines of image-display text.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     15-Feb-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
                     
*  Arguments Given:
      INTEGER POINT                   ! Defines which message is required

*  Arguments Returned:
      CHARACTER *80 IMGMES(4)         ! Informational messages if device is
                                      ! an image display
      CHARACTER *80 TERMES(4)         ! Informational messages if device is
                                      ! a terminal

      INTEGER NIMGMS                  ! Number of lines of image-display
                                      ! messages
      INTEGER NTERMS                  ! Number of lines of terminal messages

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local Variables:                                               

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set default to no values.
         TERMES(1)=' '
         TERMES(2)=' '
         TERMES(3)=' '
         TERMES(4)=' '
         IMGMES(1)=' '
         IMGMES(2)=' '
         IMGMES(3)=' '
         IMGMES(4)=' '

*   Selecting the sector centre.
      IF ((POINT.EQ.1).OR.(POINT.EQ.0)) THEN
         TERMES(1)='Select the centre of the galaxy.'
         IMGMES(1)=TERMES(1)
         TERMES(2)='Keyboard "2" key:   Quit the program.'
         IMGMES(2)=TERMES(2)
         TERMES(3)='Keyboard "." key:   Select the galaxy.'
         IMGMES(3)=TERMES(3)
         TERMES(4)='Keyboard "1" key:   Show the cursor co-ordinates.'
         IMGMES(4)='Mouse left button:  Show the cursor co-ordinates.'
      END IF

*   Select a point defining the direction and extent of the proposed sector.
      IF (POINT.EQ.2) THEN
         TERMES(1)='Indicate centre of the outer limit of the sector.'
         IMGMES(1)=TERMES(1)
         TERMES(2)='Keyboard "2" key:   Quit the program.'
         IMGMES(2)=TERMES(2)
         TERMES(3)='Keyboard "." key:   Select centre of the outer '/
     :             /' limit of the sector.'
         IMGMES(3)=TERMES(3)
         TERMES(4)='Keyboard "1" key:   Show the cursor co-ordinates.'
         IMGMES(4)='Mouse left button:  Show the cursor co-ordinates.'
      END IF

*   Select a point defining the angular width of the sector.
      IF (POINT.EQ.3) THEN
         TERMES(1)='Select the sector angular width.'
         IMGMES(1)=TERMES(1)
         TERMES(2)='Keyboard "2" key:   Quit the program.'
         IMGMES(2)=TERMES(2)
         TERMES(3)='Keyboard "." key:   Select width of the sector.'
         IMGMES(3)=TERMES(3)
         TERMES(4)='Keyboard "1" key:   Show the cursor co-ordinates.'
         IMGMES(4)='Mouse left button:  Show the cursor co-ordinates.'
      END IF

*   Select a point defining the quadrant in which a graph should be 
*   displayed.
      IF (POINT.EQ.6) THEN
         TERMES(1)='Select a point defining the quadrant of the window'/
     :             /' in which to plot.'
         IMGMES(1)=TERMES(1)
         TERMES(2)='Keyboard "2" key:   Quit the program.'
         IMGMES(2)=TERMES(2)
         TERMES(3)='Keyboard "." key:   Select the quadrant.'
         IMGMES(3)=TERMES(3)
      END IF

*   Select a point defining the lower limit for the radius of data
*   points used in the analysis of the scale length.
      IF (POINT.EQ.8) THEN
         TERMES(1)='Select a point defining the lower radius limit.'
         IMGMES(1)=TERMES(1)
         TERMES(2)='Keyboard "2" key:   Quit the program.'
         IMGMES(2)=TERMES(2)
         TERMES(3)='Keyboard "." key:   Select lower radius limit.'
         IMGMES(3)=TERMES(3)
       END IF

*   Select a point defining the upper limit for the radius of data
*   points used in the analysis of the scale length.
      IF (POINT.EQ.9) THEN
         TERMES(1)='Select a point defining the upper radius limit.'
         IMGMES(1)=TERMES(1)
         TERMES(2)='Keyboard "2" key:   Quit the program.'
         IMGMES(2)=TERMES(2)
         TERMES(3)='Keyboard "." key:   Select upper radius limit.'
         IMGMES(3)=TERMES(3)
      END IF

      NTERMS=4
      NIMGMS=4

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
      INCLUDE 'sec_par'               ! SECTOR constants  
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
      INCLUDE 'sec_par'               ! SECTOR constants

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


      SUBROUTINE SEC1_TEXTD(FLAG,NDF1,XCO,YCO,OCOUNT,BACK,SIGMA,CONS,
     :                      GRAD,RLIM,PSIZE,COUNT,ZEROP,SLEN,
     :                      LOR,HIR,LBND,STATUS)
*+
*  Name:
*     SEC1_TEXTD

*  Purpose:
*     Displays the galaxy 'fit' results in text format.
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL SEC1_TEXTD(FLAG,NDF1,XCO,YCO,OCOUNT,BACK,SIGMA,CONS,
*                      GRAD,RLIM,PSIZE,COUNT,ZEROP,SLEN,
*                      LOR,HIR,LBND,STATUS)    

*  Description:
*      Displays the text output from the program. This consists of
*      values for the scale lengths, brightness at the origin, extrapolated
*      central brightnesses, the number of data points used, the range
*      over which data points were used in the 'fit' and also the origin
*      image indices used.

*  Arguments:               
*     FLAG = INTEGER (Given)
*        Was a value found for the central pixel flag.                    
*     NDF1 = INTEGER (Given)
*        NDF identifier for the image.
*     XCO = REAL (Given)
*        The X index of the origin used. Units pixels.
*     YCO = REAL (Given)
*        The Y index of the origin used. Units pixels.
*     OCOUNT = REAL (Given)  
*        Count value fo the origin pixel. Units counts.
*     BACK = REAL (Given)
*        Background value found. Units counts.
*     SIGMA = REAL (Given)
*        Standard deviation of the background value. Units counts.
*     CONS = REAL (Given)
*        Constant terms of the linear fits made on the data selected.
*     GRAD = REAL (Given)
*        Gradinet of the fits made on the data selected.
*     RLIM = INTEGER (Given)
*        The number of radii value for which data was obtained.
*     PSIZE = REAL (Given)
*        The image pixels size. Units arc secs.
*     COUNT(2) = INTEGER (Given)
*        The number of data points used when calculating the scale
*        length values.
*     ZEROP = REAL (Given)
*        Zero point of the maginitude scale. Units magnitudes.
*     SLEN(2) = REAL (Given)
*        Scale lengths of the galaxy/object in the case of a spiral
*        or an elliptical. Units arc secs.
*     LOR = REAL (Given)
*        Low limit of the radius range from which data was taken.
*        Units arc seconds.
*     HIR = REAL (Given)
*        High limit of the radius range from which data was taken.
*        Units arc seconds.
*     LBND(10) = INTEGER (Given)
*        Lower limits of the image world co-ordinate system.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     02-FEB-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF public constants

*  Arguments Given:                              
      INTEGER COUNT(2)                ! Number of data points used for the
                                      ! radius/brightness fits
      INTEGER FLAG                    ! Was the central pixel value found?
      INTEGER LBND(NDF__MXDIM)        ! Lower limits of image world
                                      ! co-ordinate system
      INTEGER RLIM                    ! The number of data points ie
                                      ! radii at which brightness
                                      ! was determined
      INTEGER NDF1                    ! NDF indentifier
      REAL BACK                       ! Background count value
      REAL CONS(2)                    ! The constant term of the fits
                                      ! to radius versus brightness
      REAL GRAD(2)                    ! The gradient term of the fits
                                      ! to radius versus brightness
      REAL HIR                        ! Upper limit of radius values used
      REAL LOR                        ! Lower limit of radius values used 
      REAL OCOUNT                     ! Count value for the origin pixel
      REAL PSIZE                      ! The size of each pixel in
                                      ! arc seconds
      REAL SIGMA                      ! Standard deviation of the background
      REAL SLEN(2)                    ! Scale length values from the two  
                                      ! fits i.e. spiral and elliptical
      REAL XCO                        ! X index of the origin
      REAL YCO                        ! Y index of the origin
      REAL ZEROP                      ! Zero point of the magnitude scale
      
*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:
      REAL VALUE                      ! Temporary variable
      REAL VALUE1                     ! Temporary variable
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Leave a clear line.
      CALL MSG_BLANK(STATUS)

*   Show the user which file was used.
      IF (STATUS.NE.SAI__OK) GOTO 9999
      CALL NDF_MSG('FOUT',NDF1)
      CALL MSG_OUT('FOUT','SECTOR Results for file: ^FOUT',STATUS)
      CALL MSG_BLANK(STATUS)

*   Show the co-ordinates used.
      CALL MSG_FMTR('VALUE','F7.2',XCO)
      CALL MSG_FMTR('VALUE1','F7.2',YCO)
      CALL MSG_OUT(' ','Origin (data):  ^VALUE,^VALUE1',STATUS)
      CALL MSG_FMTR('VALUE','F7.2',XCO+LBND(1)-1)
      CALL MSG_FMTR('VALUE1','F7.2',YCO+LBND(2)-1)
      CALL MSG_OUT(' ','Origin (world): ^VALUE,^VALUE1',STATUS)

*   Store data value in the parameters system.
      CALL PAR_PUT0R('XCO',XCO+LBND(1)-1,STATUS)
      CALL PAR_PUT0R('YCO',YCO+LBND(2)-1,STATUS)
      CALL MSG_BLANK(STATUS)

*   Show the count values found.

*   Raw pixel count.
      VALUE=OCOUNT
      CALL MSG_FMTR('VALUE','F12.2',VALUE)
      IF (FLAG.EQ.0) THEN   
         CALL MSG_OUT(' ','Pixel count (raw):        ^VALUE',STATUS)
         CALL PAR_PUT0R('OCOUNT',OCOUNT,STATUS)
      ELSE
         CALL MSG_OUT(' ','Pixel count (raw):        ----',STATUS)
      END IF

*   Background count subtracted.
      VALUE=OCOUNT-BACK
      CALL MSG_FMTR('VALUE','F12.2',VALUE)
      IF (FLAG.EQ.0) THEN   
         CALL MSG_OUT(' ','Pixel count (subtracted): ^VALUE',STATUS)
      ELSE
         CALL MSG_OUT(' ','Pixel count (subtracted): ----',STATUS)
      END IF

*   In terms of standard deviation (sigma).
      IF (SIGMA.GT.0.0) THEN
         VALUE=(OCOUNT-BACK)/SIGMA
         CALL MSG_FMTR('VALUE','F8.2',VALUE) 
         IF (FLAG.EQ.0) THEN   
            CALL MSG_OUT(' ','Pixel count (sigma):          '//
     :                   '^VALUE',STATUS)
         ELSE
            CALL MSG_OUT(' ','Pixel count (sigma):          '//
     :                   '----',STATUS)
         END IF
      END IF

*   Relative to the background.
      IF (OCOUNT-BACK.GT.0.0) THEN
         VALUE=LOG10(OCOUNT-BACK)
         CALL MSG_FMTR('VALUE','F8.2',VALUE)
         IF (FLAG.EQ.0) THEN
            CALL MSG_OUT(' ','Pixel count (Log(I-BACK)):    '//
     :                        '^VALUE',STATUS)
         ELSE
            CALL MSG_OUT(' ','Pixel count (Log(I-BACK)):    '//
     :                        '----',STATUS)
         END IF

*      In terms of magnitude relative to image zero point.
         VALUE=ZEROP-2.5*LOG10(OCOUNT-BACK)
         CALL MSG_FMTR('VALUE','F8.4',VALUE)
         IF (FLAG.EQ.0) THEN
            CALL MSG_OUT(' ','Mag. rel. zero point:         '//
     :                   '^VALUE',STATUS)
         ELSE
            CALL MSG_OUT(' ','Mag. rel. zero point:         '//
     :                   '----',STATUS)
         END IF

*     Extrapolated central brightnesses based on the 'fits' obtained.

*     Spiral galaxy.
         CALL MSG_BLANK(STATUS)
         IF (ABS(CONS(1)).GT.0.0) THEN
            VALUE=ZEROP-2.5*CONS(1)
            CALL MSG_FMTR('VALUE','F8.4',VALUE)
            CALL MSG_OUT(' ','Central mag. spiral: ^VALUE',STATUS)
         END IF

*     Elliptical galaxy.
         IF (ABS(CONS(2)).GT.0.0) THEN
            VALUE=ZEROP-2.5*CONS(2)
            CALL MSG_FMTR('VALUE','F8.4',VALUE)
            CALL MSG_OUT(' ','Central mag. ellipt: ^VALUE',STATUS)
         END IF
           
*      Peak surface brightness relative to the sky level.
         IF ((FLAG.EQ.0).AND.(BACK.NE.0.0)) THEN
            VALUE=LOG10(ABS((OCOUNT-BACK)/BACK))
            CALL MSG_FMTR('VALUE','F8.4',VALUE)
            CALL MSG_OUT(' ','Above or below sky:  ^VALUE',STATUS)
            CALL PAR_PUT0R('ABOBEL',VALUE,STATUS)
         ELSE
            CALL MSG_OUT(' ','Above or below sky:  ----',STATUS)
         END IF  

      END IF
 
*  Display the extent of the data set available, the size
*  in arc seconds this represents.     
      CALL MSG_BLANK(STATUS)
      VALUE=(RLIM-1)*PSIZE
      CALL MSG_FMTI('VALUE','I5',RLIM)
      CALL MSG_FMTR('VALUE1','F7.2',VALUE)
      CALL MSG_OUT(' ','Number of data points:   ^VALUE '/
     :             /'ie.^VALUE1"',STATUS)

*  Display the range in arc seconds of the data points used.
      VALUE=LOR       
      VALUE1=HIR
      CALL MSG_FMTR('VALUE','F7.2',VALUE)
      CALL MSG_FMTR('VALUE1','F7.2',VALUE1)
      CALL MSG_OUT(' ','Range used (arc sec):  ^VALUE,^VALUE1'
     :             ,STATUS)
      CALL MSG_BLANK(STATUS)

*  Display 'fit' information.

*  Spiral galaxy model.
      CALL MSG_FMTI('VALUE','I5',COUNT(1))
      CALL MSG_OUT(' ','Points used for spiral calculation:     '//
     :             '^VALUE',STATUS)

      CALL MSG_FMTR('VALUE','F8.4',SLEN(1))
      CALL MSG_OUT(' ','Scale length spiral:                 '//
     :             '^VALUE',STATUS)
      CALL PAR_PUT0R('SLENS',SLEN(1),STATUS)

*  Elliptical galaxy model.
      CALL MSG_FMTI('VALUE','I5',COUNT(2))
      CALL MSG_OUT(' ','Points used for elliptical calculation.:'//
     :               '^VALUE',STATUS)

      CALL MSG_FMTR('VALUE','F8.5',SLEN(2))
      CALL MSG_OUT(' ','Scale length elliptical:             '//
     :             '^VALUE',STATUS)
      CALL PAR_PUT0R('SLENE',SLEN(2),STATUS)

 9999 CONTINUE

      END 


      SUBROUTINE SEC1_TEXTO(NDF1,SUMMAT,NUMBER,XCO,YCO,BACK,
     :                      SIGMA,CONS,RLIM,PSIZE,ZEROP,SLEN,
     :                      LBND,FIOD2,EXCLAIM,STATUS)
*+
*  Name:
*     SEC1_TEXTO

*  Purpose:
*     Puts the most recent galaxy 'fit' results into a text format file.
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL SEC1_TEXTO(NDF1,SUMMAT,NUMBER,XCO,YCO,BACK,SIGMA,CONS,
*                      RLIM,PSIZE,ZEROP,SLEN,LBND,FIOD2,EXCLAIM,STATUS)    

*  Description:
*     Creates a text file (if required) and places in it data from the
*     most recent galaxy profile/fit generated. The output values for
*     radius are measured in pixels.

*  Arguments:               
*     NDF1 = INTEGER (Given)
*        NDF identifier for the image.
*     NUMBER(SEC__RESUL) = REAL (Given)
*        The array containing the number of pixels found at a given
*        distance from the required origin.
*     SUMMAT(SEC__RESUL) = REAL (Given)
*        The summation count for all the data points found at a given 
*        distance from the required origin (see NUMBER).
*     XCO = REAL (Given)
*        The X index of the origin used. Units pixels.
*     YCO = REAL (Given)
*        The Y index of the origin used. Units pixels.
*     BACK = REAL (Given)
*        Background value found. Units counts.
*     SIGMA = REAL (Given)
*        Standard deviation of the background value. Units counts.
*     CONS = REAL (Given)
*        Constant terms of the linear fits made on the data selected.
*     RLIM = INTEGER (Given)
*        The number of radii value for which data was obtained.
*     PSIZE = REAL (Given)
*        The image pixels size. Units arc secs.
*     ZEROP = REAL (Given)
*        Zero point of the maginitude scale. Units magnitudes.
*     SLEN(2) = REAL (Given)
*        Scale lengths of the galaxy/object in the case of a spiral
*        or an elliptical. Units arc secs.
*     LBND(10) = INTEGER (Given)
*        Lower limits of the image world co-ordinate system.
*     FIOD2 = INTEGER (Given and Returned)
*        Output file identifier.
*     EXCLAIM = LOGICAL (Given and Returned)
*        Was the file name a '!'?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-MAR-1993 (GJP)
*     (Original version)
*     20-FEB-1997 (GJP)
*     Output format modified.
 
*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'sec_par'               ! SECTOR variables
      INCLUDE 'MSG_PAR'               ! Parameter system constants
      INCLUDE 'NDF_PAR'               ! NDF public constants

*  Arguments Given:                              
      INTEGER LBND(NDF__MXDIM)        ! Lower limits of image world
                                      ! co-ordinate system
      INTEGER RLIM                    ! The number of data points ie
                                      ! radii at which brightness
                                      ! was determined
      INTEGER NDF1                    ! NDF indentifier
      REAL BACK                       ! Background count value
      REAL CONS(2)                    ! The constant term of the fits
                                      ! to radius versus brightness
      REAL NUMBER(SEC__RESUL)         ! The number of pixels found at a given
                                      ! distance from the origin.
      REAL PSIZE                      ! The size of each pixel in
                                      ! arc seconds
      REAL SIGMA                      ! Standard deviation of the background
      REAL SLEN(2)                    ! Scale length values from the two  
                                      ! fits i.e. spiral and elliptical
      REAL SUMMAT(SEC__RESUL)         ! Sum of the pixel counts for all pixels
                                      ! at a given distance from the origin
      REAL XCO                        ! X index of the origin
      REAL YCO                        ! Y index of the origin
      REAL ZEROP                      ! Zero point of the magnitude scale

*  Arguments Given and Returned:                              
      INTEGER FIOD2                   ! Output file output identifier
      LOGICAL EXCLAIM                 ! The file name was !?

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:
      CHARACTER *(100) LINE           ! FIO line output length
      CHARACTER *(MSG__SZMSG) NAME    ! NDF name
      CHARACTER *(100) TEXT           ! Temporary storage
      LOGICAL OPENF                   ! Was the output file opened?
      INTEGER I                       ! Temporary variable
      INTEGER NCHAR                   ! Length of output string
      REAL TEMP                       ! Temporary storage
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Determine the output text file name. If the file name chosen fails, 
*   the user is reprompted
      OPENF=.FALSE.             
      EXCLAIM=.FALSE.   
      CALL ERR_MARK
      DO WHILE((.NOT.OPENF).AND.(.NOT.EXCLAIM)
     :          .AND.(STATUS.EQ.SAI__OK))
         CALL SEC1_AIF_ASFIO('OUT','WRITE','LIST',100,FIOD2,OPENF,
     :                   EXCLAIM,STATUS)
         IF ((.NOT.OPENF).AND.(.NOT.EXCLAIM)) THEN
            CALL ERR_REP(' ','Bad file name.',STATUS)
            CALL ERR_REP(' ','For no file, type !',STATUS)
            CALL ERR_ANNUL(STATUS)
         END IF
      END DO
      CALL ERR_RLSE
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Inform the user if a difficulty was encountered and that an
*   an output file will not be used. 
      IF (EXCLAIM) THEN  
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','No output text file created.',STATUS)
         CALL MSG_BLANK(STATUS)
         GOTO 9999
      END IF
      
*   Inform the user if a difficulty was encountered and that an
*   an output file will not be used. Otherwise add values to the 
*   output file.
      IF ((OPENF).AND.(STATUS.EQ.SAI__OK)) THEN  

*      Output a heading.
         NCHAR=0
         CALL CHR_PUTC('## ESP SECTOR V1.0 OUTPUT FILE ',LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTC('##',LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)

*      Output the file name.
         NCHAR=0
         CALL CHR_PUTC('## Filename: ',LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL NDF_MSG('NAME',NDF1)
         CALL MSG_LOAD(' ','^NAME',NAME,I,STATUS)
         NAME=NAME(1:I)
         CALL CHR_RMBLK(NAME)
         CALL CHR_PUTC(NAME,LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)

*      Output the background count value.
         NCHAR=0
         CALL CHR_PUTC('## Background (counts):',
     :                 LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(1:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(BACK,LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)

*      Output the standard deviation value.
         NCHAR=0
         CALL CHR_PUTC('## Sigma (counts):',
     :                 LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(1:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(SIGMA,LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)

*      Output the image pixel size.
         NCHAR=0
         CALL CHR_PUTC('## Pixel size (arc secs):',
     :                 LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(1:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(PSIZE,LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)

*      Output X and Y data co-ordinates.
         NCHAR=0
         CALL CHR_PUTC('## X/Y co-ordinates (data):',
     :                 LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(1:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(XCO,LINE,NCHAR)
         CALL CHR_PUTC(' ',LINE,NCHAR)
         CALL CHR_PUTR(YCO,LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)

*      Output X and Y world co-ordinates.
         NCHAR=0
         CALL CHR_PUTC('## X/Y co-ordinates (world):',
     :                 LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(1:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(XCO+LBND(1)-1,LINE,NCHAR)
         CALL CHR_PUTC(' ',LINE,NCHAR)
         CALL CHR_PUTR(YCO+LBND(2)-1,LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)

*      Output the number of points determined.
         NCHAR=0
         CALL CHR_PUTC('## Number of points:',LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTI(RLIM,LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)

*      Output the magnitude zero point.
         NCHAR=0
         CALL CHR_PUTC('## Zero point for magnitude:',LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(ZEROP,LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)

*      Output the scale length values.
         NCHAR=0
         CALL CHR_PUTC('## Spiral and elliptical scale lengths ',
     :                 LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(1:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(SLEN(1),LINE,NCHAR)
         CALL CHR_PUTC(' ',LINE,NCHAR)
         CALL CHR_PUTR(SLEN(2),LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)

*      Output the galaxy central brightness.
         NCHAR=0
         CALL CHR_PUTC('## Spiral and elliptical central brightness:',
     :                 LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(1:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(ZEROP-2.5*CONS(1),LINE,NCHAR)
         CALL CHR_PUTC(' ',LINE,NCHAR)
         CALL CHR_PUTR(ZEROP-2.5*CONS(2),LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)

*      Output a data description.
         NCHAR=0
         CALL CHR_PUTC('## Profile data:',LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(1:NCHAR),STATUS)

         NCHAR=0
         CALL CHR_PUTC('!! Radius      Count     Above/Below Sky'//
     :                 '    Relative Magnitude ',LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(1:NCHAR),STATUS)

*      Output the actual values.
         DO 400 I=1,RLIM

*         Only give values for radii where some pixels were found.
            IF (ABS(NUMBER(I)).GT.SEC__VSMAL) THEN

*            Radius (pixels).
               NCHAR=0
               TEMP=(I-1)*1.0
               CALL MSG_FMTR('V1','F8.1',TEMP)

*            Raw intensity minus background.
               CALL CHR_PUTC(' ',LINE,NCHAR)
               TEMP=SUMMAT(I)/NUMBER(I)-BACK
               CALL MSG_FMTR('V2','F7.1',TEMP)

*            Background subtracted relative to sky.
               CALL CHR_PUTC(' ',LINE,NCHAR)
               TEMP=(SUMMAT(I)/NUMBER(I)-BACK)/SIGMA
               CALL MSG_FMTR('V3','F7.3',TEMP)

*            Magnitude relative to background.
               CALL CHR_PUTC(' ',LINE,NCHAR)
               IF (SUMMAT(I)/NUMBER(I)-BACK.GT.0.0) THEN 
                  TEMP=ZEROP-2.5*LOG10(SUMMAT(I)/NUMBER(I)-BACK)
               ELSE
                  TEMP=0.0
               END IF
               CALL MSG_FMTR('V4','F4.1',TEMP)

*            Create the output string.
               TEXT=' ^V1      ^V2       ^V3               ^V4'
               NCHAR=0
               CALL MSG_LOAD(' ',TEXT,NAME,NCHAR,STATUS)
               NAME=NAME(1:NCHAR)
               CALL CHR_CLEAN(NAME)
               CALL CHR_PUTC(NAME,LINE,NCHAR)
               CALL FIO_WRITE(FIOD2,NAME(:NCHAR),STATUS)

            END IF

 400     CONTINUE

*      Terminator.
         NCHAR=0
         CALL CHR_PUTC('## END ',LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(1:NCHAR),STATUS)

*      Close down the file output.
         CALL FIO_CLOSE(FIOD2,STATUS)
 
      ELSE
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','No output text file created.',STATUS)
         CALL MSG_BLANK(STATUS)
         GOTO 9999
      END IF


 9999 CONTINUE

      END 


      SUBROUTINE SEC1_TRANS(ELEMS,ARRAY0,ARRAY1,STATUS)    
*+
*  Name:
*     SEC1_TRANS

*  Purpose:
*     Transfer data from the mapped NDF to a dynamic memory array.
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL SEC1_TRANS(ELEMS,ARRAY0,ARRAY1,STATUS)    

*  Description:
*      Copies data from the currently mapped source NDF 'DATA' array and 
*      transfers it into the PSX dynamic memory allocated for temporary
*      storage. All manipulation is then carried out on that. The routine
*      allows it to be refreshed whenever a new profile is required. 
*      

*  Arguments:               
*     ELEMS = INTEGER (Given)
*        Number of elements in the data NDF.                  
*     ARRAY0(ELEMS) = REAL (Given)
*        Array containing the mapped NDF data region..
*     ARRAY1(ELEMS) = REAL (Returned)
*        Array into which the mapped NDF will be transfered.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     22-FEB-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:                              
      INTEGER ELEMS                   ! Number of pixels in the NDF array
      REAL ARRAY0(ELEMS)              ! The mapped NDF data array

*  Arguments Returned:
      REAL ARRAY1(ELEMS)              ! Dynamic array into which the 
                                      ! mapped NDF data region is copied 

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Temporary loop variable
*.
      
*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Transfer data from the mapped NDF array to the dynamic memory array.
      DO 10 I=1,ELEMS
         ARRAY1(I)=ARRAY0(I)
 10   CONTINUE

 9999 CONTINUE

      END


      SUBROUTINE SEC1_UNCON(RADISP,LOWR,HIGHR,STATUS)         
*+
*  Name:
*     SEC1_UNCON

*  Purpose:
*     Takes the value returned by the cursor for the radius required and
*     converts it to linear radius.
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SEC1_UNCON(RADISP,LOWR,HIGHR,STATUS)         

*  Description:
*     Take the values chosen by the user (using a cursor) from the graph
*     plot of raw data and converts it from the transform value i.e. log,
*     squared or quarter power to linear values.
*     
*     The value is modified to ignore values for radius beyond the left hand
*     edge of the plot.

*  Arguments:                                
*     RADISP = CHAR (Given)
*        Character variable denoting the format used for transforming 
*        the radius value on the plotted graph.
*        R=Linear Q=Quarter power, L=Logarithmic and S=Squared.
*     LOWR = REAL (Given and Returned)
*        The low value for transformed radius derived from the graph using
*        a cursor.
*     HIGHR = REAL (Given and Returned)
*        The high value for transformed radius derived from the graph using
*        a cursor.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     23-FEB-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:                              
      CHARACTER RADISP *(256)         ! Defines the type of transformation
                                      ! to be applied to the radius value

*  Arguments Given and Returned:
      REAL LOWR                       ! Low transfromed radius value to
                                      ! be untransformed
      REAL HIGHR                      ! High transformed radius value to
                                      ! be untransformed

*  Status:     
      INTEGER STATUS                  ! Global status

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Transformation not required.
      IF (RADISP.EQ.'R') THEN
         IF (LOWR.LT.0.0) LOWR=0.0
         IF (HIGHR.LT.0.0) HIGHR=0.0
      END IF

*   Untransform data from a squared plot.
      IF (RADISP.EQ.'S') THEN
         IF (LOWR.LT.0.0) LOWR=0.0
         IF (HIGHR.LT.0.0) HIGHR=0.0
         LOWR=LOWR**(0.5)
         HIGHR=HIGHR**(0.5)
      END IF

*   Untransform data from a log base 10 plot.
      IF (RADISP.EQ.'L') THEN
         LOWR=10.**(LOWR)
         HIGHR=10.**(HIGHR)
      END IF

*   Untransform data from a quarter power plot.
      IF (RADISP.EQ.'Q') THEN
         IF (LOWR.LT.0.0) LOWR=0.0
         IF (HIGHR.LT.0.0) HIGHR=0.0
         LOWR=LOWR**(4.0)
         HIGHR=HIGHR**(4.0)
      END IF

 9999 CONTINUE

      END



*********************************************
*** MODIFIED KAPPA/KAPGEN CODE ADDED HERE ***
*********************************************


      SUBROUTINE SEC1_AIF_ASFIO (PNFILE,ACMODE,FORM,RECSZ,FD,OPEN,
     :                           EXCLAIM,STATUS)
*+
*    Description :
*
*     This routine opens a sequential file via FIO_ASSOC.  Up to four
*     attempts may be made to open the file.  If a null response is
*     supplied the file is not opened, and the flag returned indicates
*     this fact.
*
*    Invocation :
*
*      CALL SEC1_AIF_ASFIO (PNFILE,ACMODE,FORM,RECSZ,FD,OPEN, 
*                           EXCLAIM,STATUS)

*
*    Arguments :
*
*     PNFILE=CHARACTER*(*)
*         Parameter name by which file is to be opened
*     ACMODE=CHARACTER*(*)
*         Expression giving the required access mode.
*           Valid modes are: 'READ', 'WRITE', 'UPDATE' and 'APPEND'.
*           For details, see FIO_OPEN.
*     FORM=CHARACTER*(*)( READ )
*         Expression giving the required formatting of the file.
*           Valid formats are: 'FORTRAN', 'LIST', 'NONE' and
*           'UNFORMATTED'. For details, see FIO_OPEN.
*     RECSZ=INTEGER( READ )
*         Expression giving the maximum record size in bytes.
*           Set it to zero if the Fortran default is required.
*     FD=INTEGER( WRITE )
*         Variable to contain the file descriptor.
*     OPEN=LOGICAL( WRITE )
*         If true the file has been opened.
*     EXCLAIM=LOGICAL( WRITE )
*         If true then the user input was '!'.
*     STATUS=INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Initialise looping flag
*     Do while no error obtaining the name and opening the output file
*       and maximum number of attempts not exceeded
*        Get file name and open file
*        If null returned then
*           Set flag so that a log file will not be created
*           Annul the error
*           Exit from the loop
*        Else if error occurred then
*           If abort requested, do so
*           Increment loop counter
*           If maximum number of attempts not exceeded then
*              Report error
*           Else
*              Set looping flag to exit
*           Endif
*             Cancel parameter used to get filename
*        Else
*           Set flag to indicate that the file has been opened
*           Set looping flag to false
*        Endif
*     Enddo
*     If error then
*        Report and abort
*     Endif
*     Return
*
*    Bugs :
*
*     None known.
*-
*    Authors :
*
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     1989 Jul 25: Original (RL.STAR::CUR).
*     1990 Feb 20: Renamed from AIF_OPFIO (RAL::CUR).
*     1994 Mar 1: Modified to return EXCLAIM (CARDIFF::GJP).
*     1997 Feb 24: Modified for Linux (GJP). 
*
*    Type definitions :

      IMPLICIT  NONE           ! no implicit typing allowed

*    Global constants :
      INCLUDE  'SAE_PAR'       ! SSE global definitions
      INCLUDE  'PAR_ERR'       ! parameter-system errors

*    Import :
      CHARACTER*(*) PNFILE     ! File Parameter Name
      CHARACTER*(*) ACMODE     ! File access mode
      CHARACTER*(*) FORM       ! Required form of carriagecontrol
      INTEGER RECSZ            ! File record size

*    Export :
      LOGICAL OPEN             ! File opened successfully
      LOGICAL EXCLAIM          ! File name was exclaimation
      INTEGER FD               ! File descriptor

*    Status :
      INTEGER STATUS

*    Local Constants :
      INTEGER MXLOOP           ! Maximum number of attempts at
                               ! opening a data file
      PARAMETER ( MXLOOP=4 )

      INTEGER LOOP             ! Number of attempts to open the file

      LOGICAL LOOPAG           ! Loop again to open output file

*.

*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

      LOOP=0
      LOOPAG=.TRUE.
      OPEN=.FALSE.
      EXCLAIM=.FALSE.
      DO WHILE ( LOOPAG )

*       attempt to obtain and open a file to output listing

         CALL FIO_ASSOC( PNFILE, ACMODE, FORM, RECSZ, FD, STATUS )

         IF ( STATUS .EQ. PAR__NULL ) THEN
            OPEN=.FALSE.
            LOOPAG=.FALSE.
            EXCLAIM=.TRUE.
            CALL ERR_ANNUL( STATUS )
         ELSE IF ( STATUS .NE. SAI__OK ) THEN

            IF ( STATUS .EQ. PAR__ABORT ) GOTO 999

*         Here if filename is not allowed or file is not opened
*         - try again
*         Need to flush error here, as not quitting routine

            LOOP=LOOP + 1
            IF ( LOOP .LE. MXLOOP ) THEN
               CALL MSG_SETC( 'FILNAM', PNFILE )
               CALL ERR_REP( 'ERR_AIF_ASFIO_NOFI',
     :           'AIF_ASFIO: Could not open file $^FILNAM - try again',
     :           STATUS )
               CALL ERR_FLUSH( STATUS )
            ELSE

*             end looping as user is having serious problems

               LOOPAG=.FALSE.
            END IF

            CALL PAR_CANCL( PNFILE, STATUS )

         ELSE

*          no problem, so exit loop

            LOOPAG=.FALSE.
            OPEN=.TRUE.

*       end of file-opened-successfully check

         END IF
      END DO

*    abort for repeated error

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ERR_AIF_ASFIO_NOOPEN',
     :     'AIF_ASFIO: Repeatedly unable to open a file.', STATUS )
      END IF

 999  CONTINUE

      END


      SUBROUTINE SEC1_PRPCUR ( MNCHOI, SWCHOI, TERMES, NTERMS, IMGMES,
     :                    NIMGMS, BUTTNS, CURSOR, IMGDIS, STATUS )
*+
*    Description :
*
*     This determines whether a cursor with a suitable number of choices
*     is available on the current graphics device.  Messages are given
*     describing which buttons to press if the device is a terminal or
*     an image display.  The messages has parameters CHOICETERMn or
*     CHOICEIDn, where n is number of the message starting from 1.
*
*    Invocation :
*
*     CALL SEC1_PRPCUR( MNCHOI, SWCHOI, TERMES, NTERMS, IMGMES,
*    :             NIMGMS, BUTTNS, CURSOR, IMGDIS, STATUS )
*
*    Parameters :
*
*     MNCHOI = INTEGER (Given)
*        The minimum number of choices required by the calling
*          application.  It must be positive.
*     SWCHOI = INTEGER (Given)
*        The maximum number of choices for the graphics-device to be an
*          image display. It must be at least %MNCHOI.
*     TERMES( NTERMS ) = CHARACTER (Given)
*        Description of which terminal buttons to press to obtain the
*          various choices, to be reported to the user if the device
*          is nominally a terminal, i.e. its number of choices exceeds
*          %SWCHOI.
*     NTERMS = INTEGER (Given)
*        Number of lines describing the action of the terminal choices.
*     IMGMES( NIMGMS ) = CHARACTER (Given)
*        Description of the action of the mouse or trackerball buttons
*          to be reported to the user if the device is nominally an
*          image display, i.e. its number of choices is less than or
*          equal to %SWCHOI.
*     NIMGMS = INTEGER (Given)
*        Number of lines describing the action of the image-display
*          choices.
*     BUTTNS = CHARACTER (Given)
*        The terminal buttons to be pressed to obtain the different
*          choices, e.g. '1A.' would mean '1' would give the first
*          choice, 'A' would the second and '.' to exit. A fullstop
*          is the recommended Starlink method for terminating such an
*          interaction.  The last character is assumed to be the exit
*          choice in cases where this string is longer than the number
*          of choices plus one (the exit). 
*          characters.  There must be at least %MNCHOI+1 characters.
*          This string is ignored if the device is an image display.
*     CURSOR = LOGICAL (Returned)
*        If true there is a suitable cursor and number of choices.
*     IMGDIS = LOGICAL (Returned)
*        If true the choice device is an image-display mouse or
*          trackerball
*     DEVICE = DEVICE (Given)
*        The graphics workstation.
*
*    Arguments :
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*    Method :
*
*     If status is bad then exit
*     Validate input data
*     Determine the number of options on the workstation's choice device
*     If the number of choices is less than specified minimum then
*       report error context and abort
*     Activate the cursor and specify the options depending on the
*       number of choices and set cursor-ready flag
*     End
*
*    Bugs :
*
*     None known. 
*
*    Authors :
*
*     Malcolm J. Currie  STARLINK  (RAL::CUR)
*
*    History :
*
*     1989 Nov 10: Original version (RAL::CUR).
*
*    Type definitions :
      IMPLICIT NONE              ! No implicit typing

*    Global Constants :
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*    Import :
      INTEGER MNCHOI             ! Minimum number of choices
      INTEGER SWCHOI             ! Maximum number fo choices if the
                                 ! device is to be classed as an image
                                 ! display
      INTEGER NTERMS             ! Number of lines of terminal messages
      INTEGER NIMGMS             ! Number of lines of image-display
                                 ! messages

      CHARACTER *(*) TERMES( NTERMS )
                                 ! Informational messages if device is
                                 ! a terminal
      CHARACTER *(*) IMGMES( NTERMS )
                                 ! Informational messages if device is
                                 ! an image display
      CHARACTER *(*) BUTTNS      ! Choices buttons for a terminal.

*    Export :
      LOGICAL CURSOR             ! Device has a sutiable cursor and
                                 ! choices
      LOGICAL IMGDIS             ! Device is an image-display for the
                                 ! purpose of using the cursor

*    Status :
      INTEGER STATUS             ! Global status

*    External references :
      INTEGER
     :  CHR_LEN

*    Local variables :
      CHARACTER*80 BUTLST        ! List of buttons which may be a
                                 ! trimmed version of the input list
      CHARACTER DATREC(10)*80    ! Data record return by GKS inquiry

      INTEGER CONID              ! Connection identifier
      INTEGER GSTAT              ! Graphics status
      INTEGER I                  ! Loop index
      CHARACTER*4 IC             ! Message counter
      CHARACTER*14 LABEL         ! Informational-message parameter
      INTEGER LDR                ! Length of data record returned by
                                 ! GKS inquiry
      INTEGER MALT               ! Number of alternatives for choice
                                 ! input on graphics device
      INTEGER NC                 ! Number of characters in a string
      INTEGER OL                 ! Number of available prompt/echo types
                                 ! for graphics device
      INTEGER PET                ! Element of prompt/echo types of
                                 ! device returned by GKS inquiry
      INTEGER WKID               ! GKS workstation identifier
      INTEGER WTYPE              ! Workstation type

      REAL EAREA( 4 )            ! Graphics device echo area

                                 ! True if:
      LOGICAL CURAVA             ! A cursor is available

*-

*    Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CURSOR = .FALSE.
      IMGDIS = .FALSE.
      
*    Validate input data.

      IF ( MNCHOI .LT. 1 .OR. SWCHOI .LT. MNCHOI ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PRPCUR__PROG',
     :     'PRPCUR: Programmer error.  Check calling arguments',
     :      STATUS )
         GOTO 999
      END IF
 
*    Put out a blank line to ensure the commentary appears on the alpha
*    plane of the terminal.

      CALL MSG_OUT( 'BLANK', ' ', STATUS )

*    Is there a cursor?

      CURAVA = .FALSE.
      CALL SGS_ICUAV( CURAVA )

      IF ( .NOT. CURAVA ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PRPCUR__NOCUR',
     :     'PRPCUR: Chosen workstation does not have a cursor.',
     :     STATUS )

         GOTO 999
      END IF

      CALL SGS_ICURW( WKID )

*    Find workstation type

      CALL GQWKC( WKID, GSTAT, CONID, WTYPE )

*    Find number of options on choice device

      CALL GQDCH( WTYPE, 1, 1, 10, GSTAT, MALT, OL, PET, EAREA, LDR,
     :            DATREC )

*    At least one choice required

      IF ( MALT .LT. MNCHOI ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PRPCUR__WDV',
     :     'PRPCUR: Graphics device chosen has unsuitable choice '/
     :     /'device (e.g. mouse or trackerball) for this application.',
     :     STATUS )

         GOTO 999

*       Tell the user what to do...

      ELSE IF ( MALT .LE. SWCHOI ) THEN

*       first for an image display with a few buttons, and...

         DO  I = 1, NIMGMS
            CALL MSG_SETC( 'IMGMSG', IMGMES( I ) )
            CALL CHR_ITOC( I, IC, NC )
            LABEL = 'CHOICEID'//IC( :NC )

            CALL MSG_OUT( LABEL, '^IMGMSG', STATUS )
         END DO

*       Set the flag to say the cursor is ready for use.

         CURSOR = .TRUE.

*       Nominally an image display.

         IMGDIS = .FALSE.
      ELSE

*       a terminal with many choices.

*       First validate list of buttons.

         NC = CHR_LEN( BUTTNS )
         IF ( NC .LT. MNCHOI ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'PRPCUR__PROG',
     :        'PRPCUR: Programmer error.  Check calling arguments',
     :         STATUS )
            GOTO 999
         END IF
 
*       Trim the button list if necessary.

         IF ( NC .GT. MALT + 1 ) THEN
            BUTLST = BUTTNS( :MNCHOI ) //BUTTNS( NC:NC )
         ELSE
            BUTLST = BUTTNS
         END IF

*       Ensure that the messages below appear before activating the
*       cursor, otherwise they may appear on the graphics plane instead
*       of the alpha plane. This is a two-part operation. First we
*       need to give time to switch to the alpha plane.
      
         CALL MSG_SYNC( STATUS )

         DO  I = 1, NTERMS
            CALL MSG_SETC( 'TERMSG', TERMES( I ) )
            CALL CHR_ITOC( I, IC, NC )
            LABEL = 'CHOICETERM'//IC( :NC )

            CALL MSG_OUT( LABEL, '^TERMSG', STATUS )
         END DO

*       The part is to wait for the messages to appear before returning
*       to graphics plane.
      
         CALL MSG_SYNC( STATUS )

*       Activate the cursor

         CALL SGS_CUVIS( .TRUE. )
         CALL SGS_SELCH( 0 )
         CALL SGS_DEFCH( BUTLST )

*       Set the flag to say the cursor is ready for use.

         CURSOR = .TRUE.
      END IF

 999  CONTINUE

      END
