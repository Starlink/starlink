      SUBROUTINE SKEW( STATUS )             
*+
*  Name:
*     SKEW

*  Purpose:
*     Generates a skewness representation of the image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SKEW( STATUS )

*  Arguments:
*     STATUS = INTEGER(Given and Returned)
*     The global status.

*  Description:
*     Performs skewness calculations on an input NDF image 
*     file. The resulting skewness image/plot is stored to disk.
*
*     Two actions have been taken to reduce the influence of bright 
*     objects or cosmic rays:
*     
*     - the user may elect to employ a cut out pixel count value 
*       where any pixel found to be above that value is ignored. The 
*       cutout value is determined by the user inputting a global mode 
*       value, the background count standard deviation (available via 
*       HISTPEAK) and the number of standard deviations above sky level 
*       at which the cutout should be.
*
*     - a local mean value may be used as the mode.
* 
*     The user is required to enter the size of the sampling area
*     and the pixel size (in arc secs). This is used to define the 
*     width of pixel template radius employed. It is assumed that 
*     pixels are the same size in the x and y directions.
*      
*     The skewness value assigned to each pixel of the output image
*     is calculated using the values of pixel count found for all the
*     non-bad pixels within the calculated radius. The value obtained 
*     is multiplied by 1000 (or a user defined value) to make display
*     easier.
*
*     The modal count value used during the calculation is either the
*     global value (defined by the user) or a local value calculated
*     as required.
*
*     The resultant value is some measure of the extent to which the
*     pixel count values surrounding a given pixel are not distributed
*     in a Gaussian manner.
*
*     A border is present in the final output image which is the same 
*     width as the radius of the template used. Pixels within the 
*     border have been assigned the value bad.

*  Usage:
*     SKEW IN OUT MODET WIDTH PSIZE MULT [BACK] [SIGMA] 
*          [NSIGMA] [USEALL]

*  ADAM Parameters:
*     BACK = _REAL (Read)
*        The background pixel count value found in the input NDF.
*        Units counts. Only used if MODET = TRUE.
*     IN = _NDF (Read)
*        The name of the NDF that is to be examined.
*     MODET = _LOGICAL (Read)
*        Used to indicate whether a global modal count value
*        is to be used when calculating the skewness values. 
*        The alternative is for the application to calculate and use the
*        local mode value. See BACK. Using a local background
*        calculation can be slow.
*     MULT = _REAL (Read)
*        A multiplying factor applied to each of the results.
*        Default value is 1000.
*     NSIGMA = _REAL(Read)
*        The number of standard deviations above the sky level
*        count value, where the pixel count cutoff occurs. 
*        Only employed if a global pixel count modal value is 
*        in use (MODET = TRUE).
*     OUT = _NDF (Write)
*        The name of the NDF that will be created. 
*     PSIZE = _REAL (Read)
*        The width of each pixel. Units arc seconds.
*     SIGMA = _REAL (Read)
*        The standard deviation of the back ground count within the 
*        input NDF. Should be determined using a routine such as
*        HISTPEAK which ignores outliers. Only employed if a global
*        pixel count modal value is in use (MODET = TRUE).
*        Units counts.
*     USEALL = _LOGICAL (Read)
*        Used to indicate whether a pixel count threshold is to
*        be a applied when calculating the skewness.
*        Only employed if MODET has been set to ensure that
*        a global modal value is in use.
*     WIDTH = _REAL (Read)
*        The width of the sampling area/filter to be passed over the
*        image. Units arc seconds.

*  Examples:
*     skew in=ic3374 out=skewed modet=false width=10. psize=0.5 
*          mult=1000
*        A skewness image named SKEWED is generated using IC3374 as 
*        the source image. The sampling area from which pixels are 
*        selected is 10 arc seconds across. The individual pixel size 
*        is .5 arc seconds so the area is 20 pixels across. All the 
*        skewness values generated for the output image are multiplied 
*        by a factor of 1000, and local background values are used 
*        throughout.
*
*     skew in=jet out=sjet modet=true width=5. psize=1. mult=1000. 
*          back=2010. useall=true
*        An output image SJET is generated using JET as the source 
*        image. The pixel size is 1 arc second, the background count 
*        2010. and all the pixels in the image can be used in the 
*        calculation. The sampling area width is 5 arc seconds. All 
*        the pixels in the image can be used in the calculation.
*
*     skew in=sgp27 out=result modet=true width=8. psize=1. mult=1000. 
*          back=4505. sigma=23.7 nsigma=10. useall=false
*        The output image generated is created by assuming a global
*        background count of 4505. with an associated standard deviation
*        of 23.7 counts. All pixels of a count value greater 
*        than 4505+23.7x10. are excluded from the calculations.

*  Implementation Status:
*     As the program stands it is useful for looking at an image to 
*     with a view to detecting faint objects and flat-fielding 
*     faults. It may be easily extended by the user to provide 
*     plots showing other statistical quantities such as kurtosis 
*     or S/N.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-OCT-1992 (GJP)
*     (Original version)

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'ske_par'               ! SKEW constants
                     
*  Status:     
      INTEGER STATUS                  ! Global status

*  Local Variables:                                                          
      INTEGER ELEMS                   ! Number of data items in the NDF
      INTEGER LBND(7)                 ! Lower bounds for each image axis
      INTEGER NDF1                    ! Identifier for the source NDF 
      INTEGER NDF2                    ! Identifier for the results NDF   
      INTEGER NDIM                    ! Number of dimensions in the image
      INTEGER POINT1(10)              ! Pointer to the data component of
                                      ! NDF1
      INTEGER POINT2(10)              ! Pointer to the data component of
                                      ! NDF2
      INTEGER PRANGE(2)               ! Number of pixels in the image x 
                                      ! and y axes 
      INTEGER RADIUS                  ! Radius of the circular area used
                                      ! for calculating skewness
      INTEGER UBND(7)                 ! Upper bounds for each image axis
      INTEGER XMAX                    ! Width in pixels of the image
      INTEGER YMAX                    ! Length in pixels of the image
      REAL HIEST                      ! Highest pixel count used
                                      ! in the correlation
      REAL MODE                       ! The global mode value for the
                                      ! NDF image
      REAL MULT                       ! Multiplying factor applied to
                                      ! all the output values
      REAL NSIGMA                     ! The number of standard deviations
                                      ! above the modal value that a 
                                      ! pixel must be, to be ignored
      REAL PSIZE                      ! The pixel size of the image
      REAL SIGMA                      ! Standard deviation of the background
      REAL WIDTH                      ! Width of the circular area used
                                      ! for calculating skewness
      LOGICAL MODET                   ! What type of mode is to be used
      LOGICAL USEALL                  ! Are all the pixels to be used or
                                      ! will the very bright be ignored
*.                              

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN
                                                                 
*   Begin an NDF context.                               
      CALL NDF_BEGIN

*   Indicate that the applicatiion is running.
      CALL MSG_BLANK(STATUS)
      CALL MSG_OUT(' ','ESP SKEW running.',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Obtain an identifier for the NDF structure to be examined.       
      CALL NDF_ASSOC('IN','READ',NDF1,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Display the name of the NDF.
      CALL NDF_MSG('IN',NDF1)           
      CALL MSG_OUT(' ','Filename:   ^IN',STATUS)

*   See if the title component is defined. If so, display its value.
      CALL NDF_CMSG('TITLE',NDF1,'Title',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      CALL MSG_OUT('TITLE','Title:      ^TITLE',STATUS)
    
*   Get the pixel-index bounds of an NDF and store in LBND and UBND.
      CALL NDF_BOUND(NDF1,2,LBND,UBND,NDIM,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Store the size (in pixels) of the image dimensions.
      PRANGE(1)=UBND(1)-LBND(1)+1
      PRANGE(2)=UBND(2)-LBND(2)+1

*   Display the image x and y axis sizes (pixels).
      CALL MSG_SETI('PR1',PRANGE(1))
      CALL MSG_SETI('PR2',PRANGE(2))
      CALL MSG_OUT(' ','Shape:      ^PR1 x ^PR2 pixels',STATUS)
     
*   Display the image x and y axis ranges (pixels).
      CALL MSG_SETI('L1',LBND(1))
      CALL MSG_SETI('L2',UBND(1))
      CALL MSG_SETI('L3',LBND(2))
      CALL MSG_SETI('L4',UBND(2))
      CALL MSG_OUT(' ','Bounds:     x= ^L1:^L2  y= ^L3:^L4'
     :             ,STATUS)  

*   Calculate the maximum number of pixels in the image.
      ELEMS=PRANGE(2)*PRANGE(1)
       
*   Display the image size.
      CALL MSG_SETI('ELEMS',ELEMS)
      CALL MSG_OUT(' ','Image size: ^ELEMS pixels',STATUS)

*   Map the NDF data array as _REAL values for reading.
      CALL NDF_MAP(NDF1,'Data','_REAL','READ',POINT1(1),ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Propogate an NDF to contain the results.
      CALL NDF_PROP(NDF1,'Data','OUT',NDF2,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Set the output NDF data type to real.
      CALL NDF_STYPE('_REAL',NDF2,'DATA',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Map the results NDF data array as _REAL values for updating.  
      CALL NDF_MAP(NDF2,'Data','_REAL','UPDATE',POINT2(1),ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Change the propogated title.
      CALL NDF_CPUT('ESP - Skewness Image',NDF2,'TITLE',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine the sampling width required. 
      CALL PAR_GET0R('WIDTH',WIDTH,STATUS) 
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine the pixel size.
      CALL PAR_GET0R('PSIZE',PSIZE,STATUS)

*   Check that the pixel size is not too small.
      IF (PSIZE.LT.SKE1__VSMAL) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The pixel size supplied is too small.',
     :                STATUS)
         GOTO 9999
      END IF

*   Check that the resultant radius isnt too small.
      RADIUS=NINT(WIDTH/PSIZE/2.)
      IF (RADIUS.LT.1) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The number of pixels involved is'//
     :                ' too small.',STATUS)
         GOTO 9999
      END IF

*   Determine whether the local mode or a global mode value is to
*   be used.
      CALL PAR_GET0L('MODET',MODET,STATUS)
         
*   Determine the global mode value required.
      IF (MODET) THEN 
         
         CALL PAR_GET0R('BACK',MODE,STATUS)

*      Determine whether any pixels are to be excluded if their value
*      is too high.
         CALL PAR_GET0L('USEALL',USEALL,STATUS)                           

*      Set up the cutout value.
         IF (.NOT.USEALL) THEN
     
*         Determine the background standard deviation to be used.
            CALL PAR_GET0R('SIGMA',SIGMA,STATUS)


*         Determine the number of standard deviations above sky to
*         apply the cutout. This is used to calculate at what value
*         of count, pixels are ignored. This is to allow very high
*         values to be ignored, which is useful for reducing the 
*         influence of very bright objects or gamma rays. 
            CALL PAR_GET0R('NSIGMA',NSIGMA,STATUS)

*         Calculate the pixel count value, above which pixels 
*         are to be ignored. 
            HIEST=MODE+NSIGMA*SIGMA

         END IF

      END IF

*   Determine the skewness multiplying factor to be used.
      CALL PAR_GET0R('MULT',MULT,STATUS)
  
      CALL MSG_BLANK(STATUS)

*   Inform the user of what will be done, since the routine takes a 
*   long while to run.
      CALL NDF_MSG('FOUT',NDF1)           
      CALL MSG_OUT(' ','Applying SKEW to file: ^FOUT',STATUS)
      CALL NDF_MSG('FOUT',NDF2)           
      CALL MSG_OUT(' ','Results file will be:  ^FOUT',STATUS)
      IF (MODET) THEN
         CALL MSG_OUT(' ','Global background value used.',STATUS)
         IF (USEALL) THEN
            CALL MSG_OUT(' ','High count cutoff was not used.',STATUS)
         ELSE
            CALL MSG_OUT(' ','High count cutoff was used.',STATUS)
         END IF
      ELSE
         CALL MSG_OUT(' ','Local background values used.',STATUS)
      END IF
      CALL MSG_BLANK(STATUS)

*   Prepare values that will be passed to the subroutine.
      XMAX=PRANGE(1)
      YMAX=PRANGE(2)

*   Create a skewness image.
      IF (MODET) THEN

*      Global mode version.
         CALL SKE1_GLOBAL(MULT,ELEMS,%VAL(POINT1(1)),RADIUS,MODE,
     :              XMAX,YMAX,USEALL,HIEST,STATUS,%VAL(POINT2(1)))  
         IF (STATUS.NE.SAI__OK) GOTO 9999
     
      ELSE

*      Local mode version.
         CALL SKE1_LOCAL(MULT,ELEMS,%VAL(POINT1(1)),RADIUS,
     :                   XMAX,YMAX,STATUS,%VAL(POINT2(1)))  
         IF (STATUS.NE.SAI__OK) GOTO 9999

      END IF 

 9999 CONTINUE

*   Un-map the NDF data arrays.
      CALL NDF_UNMAP(NDF1,'Data',STATUS)
      CALL NDF_UNMAP(NDF2,'Data',STATUS) 

*   End the NDF context.
      CALL NDF_END(STATUS)

      END      
                                                 

      SUBROUTINE SKE1_GLOBAL(MULT,ELEMS,ARRAY,RADIUS,MODE, 
     :                       XMAX,YMAX,USEALL,HIEST,STATUS,ARRAY2)    
*+
*  Name: 
*     SKE1_GLOBAL

*  Purpose:
*     Performs skewness calculations for a circular symmetrical
*     template of known width. It assumes a global mode value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SKE1_GLOBAL(MULT,ELEMS,%VAL(POINT1(1)),RADIUS,MODE,
*                      XMAX,YMAX,USEALL,HIEST,STATUS,%VAL(POINT2(1)))  
      
*  Description:
*     Given a source image, the routine generates an equivalent image of
*     similar size that shows, at each pixel, the degree of skewness
*     that was present in the circular area around the equivalent point
*     in the source image. Skewness should normally be near zero for
*     a Gaussian (Normal) distribution. Large deviations from this 
*     suggest regions either containing objects or flatfielding flaws.
*
*     The calculation is carried out as follows:
*
*     The value for each pixel of the output image is determined 
*     as follows. An imaginary circle is drawn about the pixel
*     and the count values for all pixels within that circle, 
*     are stored. 
*
*     The count values are then used to calculate the skewness using 
*     a global mode value.

*  Arguments:
*     MULT = REAL (Given)
*        The multiplying value applied to all the skewness values
*        generated.
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image.
*     ARRAY(ELEMS) = REAL (Given)
*        The array containing the source NDF image count values.
*     RADIUS = REAL (Given)
*        The radius of the circular template that is used to define
*        the region about a given pixel, that might contribute to the 
*        skewness value that will be inserted into
*        the equivalent pixel in ARRAY2.
*     MODE = REAL (Given)
*        The global mode value supplied by the user. Units counts.
*     XMAX = INTEGER (Given)
*        The length of the image x axis. Units pixels.
*     YMAX = INTEGER (Given)
*        The length of the y axis of the image. Units pixels.
*     USEALL = LOGICAL (Given)
*        Flag indicating whether or not a pixel cut out value is being
*        employed.
*     HIEST = REAL (Given)
*        The highest pixel count value that will be used in the analysis.
*        All pixels with count values above this will be ignored. Units 
*        counts.
*     STATUS = INTEGER (Given and Returned) 
*        The global status.     
*     ARRAY2(ELEMS) = REAL (Returned)
*        The array that eventually contains the skewness 'image'.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     14-Oct-1992
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'ske_par'               ! SKEW constants
                     
*  Arguments Given:
      LOGICAL USEALL                  ! Is a high count cut out being 
                                      ! used?
      INTEGER ELEMS                   ! Number of pixels in the data array
      INTEGER RADIUS                  ! Radius of the size of object
                                      ! being considered
      INTEGER XMAX                    ! Length of the image x axis
      INTEGER YMAX                    ! Length of the image y axis
      REAL ARRAY(ELEMS)               ! Array containing NDF data
      REAL HIEST                      ! The count value above which
                                      ! a pixel will be ignored if
                                      ! USEALL is true.
      REAL MODE                       ! Global image mode value
      REAL MULT                       ! Multipliying factor

*  Arguments Returned:
      REAL ARRAY2(ELEMS)              ! Image array containing the
                                      ! skewness results

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:                 
      INTEGER I                       ! Loop variable 
      INTEGER ADD2                    ! Array element address
      INTEGER J                       ! Loop variable
      INTEGER N                       ! The number of valid pixels
                                      ! that were present about a given
                                      ! origin pixel
      INTEGER NPIX                    ! The maximum number of pixels 
                                      ! about a given origin. Depends on the 
                                      ! pixel size and filter width. 
      INTEGER OFFSETS(SKE1__PIXN)     ! Address offsets for the
                                      ! pixels in the circular
                                      ! template used
      INTEGER PERC                    ! Percentage of the calculations 
                                      ! done so far                  
      INTEGER X                       ! Pixel x axis index
      INTEGER X1                      ! Pixel x axis index offset
      INTEGER Y                       ! Pixel y axis index
      INTEGER Y1                      ! Pixel y axis index

      REAL RN                         ! Number of pixels used
      REAL SKEW                       ! Temporary skewness sum
      REAL VALUE                      ! Temporary storage 
      REAL VALUES(SKE1__PIXN)         ! Values of pixel counts
      REAL VALUE1                     ! Temporary storage 
      REAL VARI                       ! Temporary variance sum
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set up the initial value for the output array.
      DO 10 I=1,ELEMS
         ARRAY2(I)=VAL__BADR
 10   CONTINUE

*   Construct an array containing the memory address offsets of all the
*   pixels in the template relative to the memory address of the circle 
*   centre. 
*
      I=RADIUS*RADIUS
      NPIX=0
      DO 30 X1=-RADIUS,RADIUS    
         DO 20 Y1=-RADIUS,RADIUS

*         Check that the pixel at pixel offsets X1,Y1 is
*         within a circle of the given radius and hence within the 
*         required circular area.
            IF (I.GT.(X1*X1+Y1*Y1)) THEN

*            Calculate the memory address offset. 
               VALUE=Y1*XMAX+X1
  
*            Increment the address counter and store the 
*            address offset.
               NPIX=NPIX+1

*            Check that there are not too many pixels.
               IF (NPIX.GT.SKE1__PIXN) THEN
                  STATUS=SAI__ERROR
                  CALL ERR_REP(' ','Too many pixels are being'//
     :                              'used.',STATUS)
                  GOTO 9999
               ELSE
                  OFFSETS(NPIX)=VALUE
               END IF

            END IF

 20      CONTINUE

 30   CONTINUE  

*   Consider all pixels within the template. Obviously, a point off the 
*   side of the image is not valid. To increase speed of operation only
*   the parts of the image where this is not the case are considered.
*   This leads to the generation of a border of bad-valued points around 
*   the image. The border width is the same as the radius of the circular 
*   filter in use.
*
*   Two nearly identical routines are used to maximise execution speed.
      IF (USEALL) THEN

*      Perform calculations for situation where there is no
*      high count cutoff.
         PERC=0
         DO 100 Y=RADIUS+1,YMAX-RADIUS-1
         
*         Indicate that something is happening.
            IF (Y.EQ.NINT(Y/50.)*50) THEN
               PERC=NINT((Y-RADIUS)*100./(YMAX-2.*RADIUS)+1.)
               CALL MSG_SETI('PERC',PERC)
               CALL MSG_OUT(' ','Percentage done so far: ^PERC',STATUS)
            END IF

*          Calculate the first component of the central pixel address.
            ADD2=(Y-1)*XMAX

            DO 90 X=RADIUS+1,XMAX-RADIUS-1

*            Calculate the full address of the central pixel address.
*            Also set the initial count for the number of usable pixel
*            found.
               N=0
               I=ADD2+X

*            Consider all points found earlier.
               DO 40 J=1,NPIX

*               Get the value of the point but only uses if
*               it is not defined as bad. 
                  VALUE1=ARRAY(I+OFFSETS(J))
                  IF (VALUE1.NE.VAL__BADR) THEN
               
*                  Increment the number of usable pixels 
*                  and retain latest value.
                     N=N+1
                     VALUES(N)=VALUE1

                  END IF

 40            CONTINUE  

*            Consider all the pixels that were found.
               IF (N.GT.1) THEN 

*               Calculate the skewness and variance totals.
                  SKEW=0.0
                  VARI=0.0
                  DO 50 J=1,N
                     VALUE1=VALUES(J)-MODE
                     VARI=VARI+VALUE1*VALUE1
                     SKEW=SKEW+VALUE1*VALUE1*VALUE1
 50               CONTINUE

*               Return the skewness value.
                  IF (VARI.GT.0.0) THEN
                     RN=REAL(N)
                     ARRAY2(I)=MULT*SKEW/(RN*SQRT(VARI/(RN-1))**3)
                  END IF

               END IF

 90         CONTINUE

 100     CONTINUE                    

      ELSE

*      Perform calculations for the situation where there is a high
*      count cutofff defined by HIEST
         PERC=0
         DO 1100 Y=RADIUS+1,YMAX-RADIUS-1
         
*         Indicate that something is happening.
            IF (Y.EQ.NINT(Y/50.)*50) THEN
               PERC=NINT((Y-RADIUS)*100./(YMAX-2.*RADIUS)+1.)
               CALL MSG_SETI('PERC',PERC)
               CALL MSG_OUT(' ','Percentage done so far: ^PERC',STATUS)
            END IF

*          Calculate the first component of the central pixel address.
            ADD2=(Y-1)*XMAX

            DO 1090 X=RADIUS+1,XMAX-RADIUS-1

*            Calculate the full address of the central pixel address.
*            Also set the initial count for the number of usable pixel
*            found.
               N=0
               I=ADD2+X

*            Consider all points found earlier.
               DO 1040 J=1,NPIX

*               Get the value of the point but only use if
*               it is not defined as bad and is not above the
*               cutoff value. 
                  VALUE1=ARRAY(I+OFFSETS(J))

                  IF ((VALUE1.NE.VAL__BADR).AND.(VALUE1.LT.HIEST)) THEN
               
*                  Increment the number of usable pixels 
*                  found and retain the latest value.
                     N=N+1
                     VALUES(N)=VALUE1

                  END IF

 1040          CONTINUE  

*            Consider all the pixels that were found.
               IF (N.GT.1) THEN 

*               Calculate the skewness and variance totals.
                  SKEW=0.0
                  VARI=0.0
                  DO 1050 J=1,N
                     VALUE1=VALUES(J)-MODE
                     VARI=VARI+VALUE1*VALUE1
                     SKEW=SKEW+VALUE1*VALUE1*VALUE1
 1050             CONTINUE

*               Return the skewness value.
                  IF (VARI.GT.0.0) THEN
                     RN=REAL(N)
                     ARRAY2(I)=MULT*SKEW/(RN*SQRT(VARI/(RN-1))**3)
                  END IF

               END IF

 1090       CONTINUE

 1100    CONTINUE                    
      
      END IF
        
 9999 CONTINUE

      END
 

      SUBROUTINE SKE1_LOCAL(MULT,ELEMS,ARRAY,RADIUS, 
     :                  XMAX,YMAX,STATUS,ARRAY2)    
*+
*  Name: 
*     SKE1_LOCAL

*  Purpose:
*     Performs skewness calculations for a circular symmetrical
*     template of known width. The mode value used is the average pixel
*     value found within the template.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SKE1_LOCAL(MULT,ELEMS,%VAL(POINT1(1)),RADIUS,MODE,
*                      XMAX,YMAX,STATUS,%VAL(POINT2(1)))  
      
*  Description:
*     Given a source image, the routine generates an equivalent image of
*     similar size that shows, at each pixel, the degree of skewness
*     that was present in the circular area around the equivalent point
*     in the source image. Skewness should normally be near zero for
*     a Gaussian (Normal) distribution. Large deviations from this 
*     suggest regions either containing objects or flatfielding flaws.
*
*     The calculation is carried out as follows:
*
*     The value for each pixel of the output image is determined 
*     as follows. An imaginary circle is drawn about the pixel
*     and the count values for all pixels within that circle, 
*     are stored. 
*
*     The count values are then used to calculate the skewness using 
*     a local mode value.
 
*  Arguments:
*     MULT = REAL (Given)
*        The multiplying value applied to all the skewness values
*        generated.
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image.
*     ARRAY(ELEMS) = REAL (Given)
*        The array containing the source NDF image count values.
*     RADIUS = REAL (Given)
*        The radius of the circular template that is used to define
*        the region about a given pixel, that might contribute to the 
*        skewness value that will be inserted into
*        the equivalent pixel in ARRAY2.
*     MODE = REAL (Given)
*        The global mode value supplied by the user. Units counts.
*     XMAX = INTEGER (Given)
*        The length of the image x axis. Units pixels.
*     YMAX = INTEGER (Given)
*        The length of the y axis of the image. Units pixels.
*     STATUS = INTEGER (Given and Returned) 
*        The global status.     
*     ARRAY2(ELEMS) = REAL (Returned)
*        The array that eventually contains the skewness 'image'.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     14-Oct-1992
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'ske_par'               ! SKEW constants
                     
*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the data array
      INTEGER RADIUS                  ! Radius of the size of object
                                      ! being considered
      INTEGER XMAX                    ! Length of the image x axis
      INTEGER YMAX                    ! Length of the image y axis
      REAL ARRAY(ELEMS)               ! Array containing NDF data
      REAL MODE                       ! Global image mode value
      REAL MULT                       ! Multiplying factor

*  Arguments Returned:
      REAL ARRAY2(ELEMS)              ! Image array containing the
                                      ! skewness results

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:                 
      INTEGER I                       ! Loop variable 
      INTEGER ADD2                    ! Array element address
      INTEGER J                       ! Loop variable
      INTEGER N                       ! The number of valid pixel 
                                      ! that were present about a given
                                      ! origin pixel
      INTEGER NPIX                    ! The maximum number of pixels
                                      ! about a given origin. Depends on the 
                                      ! pixel size and filter width. 
      INTEGER OFFSETS(SKE1__PIXN)     ! Address offsets for the
                                      ! pixels in the template
      INTEGER PERC                    ! Percentage of the calculations 
                                      ! done so far                  
      INTEGER X                       ! Pixel x axis index
      INTEGER X1                      ! Pixel x axis index offset
      INTEGER Y                       ! Pixel y axis index
      INTEGER Y1                      ! Pixel y axis index

      REAL RN                         ! Number of pixels used
      REAL SKEW                       ! Temporary skewness sum
      REAL VALUE                      ! Temporary storage 
      REAL VALUES(SKE1__PIXN)         ! Values of pixel counts
      REAL VALUE1                     ! Temporary storage 
      REAL VARI                       ! Temporary variance sum
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set up the initial value for the output array.
      DO 10 I=1,ELEMS
         ARRAY2(I)=VAL__BADR
 10   CONTINUE

*   Construct an array containing the memory address offsets of all the
*   pixels in the circular template relative to the memory address of 
*   the circle centre. 
*
      I=RADIUS*RADIUS
      NPIX=0                  
       DO 30 Y1=-RADIUS,RADIUS    
         DO 20 X1=-RADIUS,RADIUS

*         Check that the pixel at pixel offsets X1,Y1 is
*         within a circle of the given radius and hence within the 
*         required circular area.
            IF (I.GT.(X1*X1+Y1*Y1)) THEN

*            Calculate the memory address offset. 
               VALUE=Y1*XMAX+X1
  
*            Increment the address counter and store the 
*            address offset.
               NPIX=NPIX+1

*            Check that there are not too many pixels.
               IF (NPIX.GT.SKE1__PIXN) THEN
                  STATUS=SAI__ERROR
                  CALL ERR_REP(' ','Too many pixels are being'//
     :                              'used.',STATUS)
                  GOTO 9999
               ELSE
                  OFFSETS(NPIX)=VALUE
               END IF

            END IF

 20      CONTINUE

 30   CONTINUE  

*   Consider all pixels within the template. Obviously, a point off the side 
*   of the image is not valid. To increase the speed of operation only parts
*   of the image where this is not the case are used. This leads to the
*   generation of a border of bad-valued points around the image. 
*   The border width is the same as the radius of the circular filter in 
*   use.
      PERC=0
      DO 100 Y=RADIUS+1,YMAX-RADIUS-1
        
*      Indicate that something is happening.
         IF (Y.EQ.NINT(Y/50.)*50) THEN
            PERC=NINT((Y-RADIUS)*100./(YMAX-2.*RADIUS)+1.)
            CALL MSG_SETI('PERC',PERC)
            CALL MSG_OUT(' ','Percentage done so far: ^PERC',STATUS)
         END IF
  
*       Calculate the first component of the central pixel address.
         ADD2=(Y-1)*XMAX

         DO 90 X=RADIUS+1,XMAX-RADIUS-1

*         Calculate the full address of the central pixel address.
*         Also set the initial count for the number of usable pixel
*         found.
            N=0
            I=ADD2+X
      
*         Consider all points found earlier.
            DO 40 J=1,NPIX

*            Get the value of the point but only use if
*            it was not defined as bad. 
               VALUE1=ARRAY(I+OFFSETS(J))
               IF (VALUE1.NE.VAL__BADR) THEN
               
*               Increment the number of usable pixels 
*               found and retain the latest value.
                  N=N+1
                  VALUES(N)=VALUE1

               END IF

 40         CONTINUE  

*         Consider all the pixels that were found.
            IF (N.GT.1) THEN 

*            Calculate the local mode of the pixels that
*            were acceptable.
               MODE=0.0
               DO 41 J=1,N
                  MODE=MODE+VALUES(J)
 41            CONTINUE
               MODE=MODE/REAL(N)

*               Calculate the skewness and variance totals.
                  SKEW=0.0
                  VARI=0.0
                  DO 50 J=1,N
                     VALUE1=VALUES(J)-MODE
                     VARI=VARI+VALUE1*VALUE1
                     SKEW=SKEW+VALUE1*VALUE1*VALUE1
 50               CONTINUE

*               Return the skewness value.
                  IF (VARI.GT.0.0) THEN
                     RN=REAL(N)
                     ARRAY2(I)=MULT*SKEW/(RN*SQRT(VARI/(RN-1))**3)
                  END IF

            END IF

 90      CONTINUE

 100  CONTINUE                    
        
 9999 CONTINUE

      END
 
