      SUBROUTINE CORR( STATUS )             
*+
*  Name:
*     CORR

*  Purpose:
*     Performs cross-correlations on an image using a galaxy template.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CORR( STATUS )

*  Arguments:
*     STATUS = INTEGER(Given and Returned)
*     The global status.

*  Description:
*     Performs calculations to cross-correlate a circular shaped 
*     exponential template with an image. 
*     The exponential profile template chosen optimises the chances of 
*     identifying faint diffuse galaxies/galaxies of (and near) a 
*     scale length defined by the user.
*
*     Performs cross-correlation calculations on an input NDF image 
*     file. The resulting image/plot is stored in an output NDF.
*
*     For each image pixel in turn, all the pixels within a defined
*     radius are identified. The values for each of these in turn have 
*     their background values subtracted and the result (F1)
*     multiplied by a factor (F2) generated using an exponential function. 
*     The values obtained for all the surrounding image pixels are 
*     summed. The total generated is divided by using a normalisation
*     value created by taking the sums of square for F1 and F2, 
*     multiplying them together and then taking the square root.
*     This normalised sum is placed in the 
*     appropriate pixel of the output image and the program moves on 
*     to the next input image pixel to be considered.
*
*     The circular elliptical mask used is of a radius 1.8x the
*     scale length requested. Studies undertaken by Phillipps and 
*     Davies at Cardiff suggest that this value optimises the 
*     detection sensitivity.
*
*     The correlation value obtained is multiplied by 1000 (or a user 
*     defined value) to make display easier.
*
*     A border is present in the final output image which is the
*     same width as the radius of the template used. Pixels within the 
*     border have been assigned the value bad.

*  Usage:
*     CORR IN OUT SCALE PSIZE BACK USEALL MULT [SIGMA] [NSIGMA]

*  ADAM Parameters:
*     BACK = _REAL (Read)
*        The modal pixel count value found in the input NDF.
*        Units counts.
*     IN = _NDF (Read)
*        The name of the NDF image that is to be examined.
*     MULT = _REAL (Read)
*        A multiplying factor applied to each of the results.
*        Default value is 1000.
*     NSIGMA = _REAL(Read)
*        The number of standard deviations above the sky level
*        count value, where the pixel count cutoff occurs. 
*     OUT = _NDF (Write)
*        The name of the NDF data that will be created. 
*     PSIZE = _REAL (Read)
*        The size of each image pixel. Units arc seconds.
*     SCALE = _REAL (Read)
*        The scale length of the galaxies to be highlighted in the 
*        output image. Units arc seconds.
*     SIGMA = _REAL (Read)
*        The standard deviations of the background pixel count within the 
*        input NDF. Should be determined using a routine such as
*        HISTPEAK which ignores outliers. 
*     USEALL = _LOGICAL (Read)
*        Used to indicate whether a pixel count threshold is to
*        be a applied when calculating the correlation.

*  Examples:
*     corr in=hh1826 out=correl scale=8. psize=0.3 back=7437.
*          useall=true mult=1000.
*
*       Correlates image HH1826 with a mask/template optimised for 
*       galaxies of 8 arc seconds scale length. The pixel size on the 
*       image is .3 arc second, the background count value 7437 and 
*       all the pixels on the image can be used in the calculation. 
*       The output image is to be named CORREL.  
*
*     corr in=forn out=forn4 scale=4. psize=0.22 mult=1000. back=666
*          useall=false sigma=15 nsigma=3
*
*       Correlates image FORN with a mask/template optimised for
*       galaxies of 4 arc seconds scale length. The pixel size is .22
*       arc seconds and the background count value 666.
*
*       Pixels that are brighter than 666+15x3 counts are not 
*       included in the correlation calculations (USEALL=FALSE). 
*       The output image is to be named FORN4.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-MAY-1993 (GJP)
*     (Original version)

*  Notes:
*     It is assumed that the x and y axis pixels are of the same size.
* 
*     To establish the statistical significance of a detection, this 
*     application should be used in conjunction with MIXUP to allow noise 
*     equivalent images to be generated and correlated thereby 
*     establishing a 3 sigma limit.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'cor_par'               ! CORR constants
                     
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
                                      ! for calculating correlation
      INTEGER UBND(7)                 ! Upper bounds for each image axis
      INTEGER XMAX                    ! Width in pixels of the image
      INTEGER YMAX                    ! Length in pixels of the image
      REAL DIAM                       ! Template diameter.
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
      REAL SCALE                      ! Scale length of the galaxy
      REAL SIGMA                      ! Standard deviation of the background count
      LOGICAL USEALL                  ! Are all the pixels to be used or
                                      ! will the very bright be ignored
*.                              

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN
                                                                 
*   Begin an NDF context.                               
      CALL NDF_BEGIN

*   Indicate that the application is running.
      CALL MSG_BLANK(STATUS)
      CALL MSG_OUT(' ','ESP CORR running.',STATUS)
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

*   Set the data type in the output array to real.
      CALL NDF_STYPE('_REAL',NDF2,'DATA',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Map the results NDF data array as _REAL values for updating.  
      CALL NDF_MAP(NDF2,'Data','_REAL','UPDATE',POINT2(1),ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Change the propogated title.
      CALL NDF_CPUT('ESP - Cross-correlation Image',NDF2,'TITLE',
     :              STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine the sampling diameter required. 
      CALL PAR_GET0R('SCALE',SCALE,STATUS) 
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   1.8 factor optimises the filter size.
      DIAM=2.*SCALE*1.8

*   Determine the pixel size.
      CALL PAR_GET0R('PSIZE',PSIZE,STATUS)

*   Check that the pixel size is not too small.
      IF (PSIZE.LT.COR1__VSMAL) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The pixel size supplied is too small.',
     :                STATUS)
         GOTO 9999
      END IF

*   Check that the resultant radius isnt too small.
      RADIUS=NINT(DIAM/PSIZE/2.)
      IF (RADIUS.LT.1) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The number of pixels involved is'//
     :                ' too small.',STATUS)
         GOTO 9999
      END IF

*   Get the image background value.
      CALL PAR_GET0R('BACK',MODE,STATUS)

*   Determine whether any pixels are to be excluded if their value
*   is too high.
      CALL PAR_GET0L('USEALL',USEALL,STATUS)                           

*   Set up the cutout value.
      IF (.NOT.USEALL) THEN
     
*      Determine the background standard deviation to be used.
         CALL PAR_GET0R('SIGMA',SIGMA,STATUS)

*      Determine the number of standard deviations above sky at which to
*      apply the cutout. This is used to calculate at what value
*      of count, pixels are ignored. This is to allow very high
*      values to be ignored, which is useful for reducing the 
*      influence of very bright objects or gamma rays. 
         CALL PAR_GET0R('NSIGMA',NSIGMA,STATUS)

*      Calculate the pixel count value, above which pixels 
*      are to be ignored. 
            HIEST=MODE+NSIGMA*SIGMA

      END IF

*   Determine the correlation multiplying factor to be used.
      CALL PAR_GET0R('MULT',MULT,STATUS)
  
      CALL MSG_BLANK(STATUS)

*   Inform the user of what will be done, since the routine takes a 
*   long while to run.
      CALL NDF_MSG('FOUT',NDF1)           
      CALL MSG_OUT(' ','Applying CORR to file: ^FOUT',STATUS)
      CALL NDF_MSG('FOUT',NDF2)           
      CALL MSG_OUT(' ','Results file will be:  ^FOUT',STATUS)
      IF (USEALL) THEN
         CALL MSG_OUT(' ','High count cutoff was not used.',STATUS)
      ELSE
         CALL MSG_OUT(' ','High count cutoff was used.',STATUS)
      END IF
      CALL MSG_BLANK(STATUS)

*   Prepare values that will be passed to the subroutine.
      XMAX=PRANGE(1)
      YMAX=PRANGE(2)

*   Perform the calculations.
      CALL COR1_CORR(MULT,ELEMS,%VAL(POINT1(1)),RADIUS,MODE,
     :                 XMAX,YMAX,USEALL,HIEST,STATUS,%VAL(POINT2(1)))  
         IF (STATUS.NE.SAI__OK) GOTO 9999

 9999 CONTINUE

*   Un-map the NDF data arrays.
      CALL NDF_UNMAP(NDF1,'Data',STATUS)
      CALL NDF_UNMAP(NDF2,'Data',STATUS) 

*   End the NDF context.
      CALL NDF_END(STATUS)

      END      
                                                 

      SUBROUTINE COR1_CORR(MULT,ELEMS,ARRAY,RADIUS,MODE, 
     :                       XMAX,YMAX,USEALL,HIEST,STATUS,ARRAY2)    
*+
*  Name: 
*     COR1_CORR

*  Purpose:
*     Performs correlation calculations for a circular symmetrical
*     template of known diameter. It assumes a global mode value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COR1_CORR(MULT,ELEMS,%VAL(POINT1(1)),RADIUS,MODE,
*                    XMAX,YMAX,USEALL,HIEST,STATUS,%VAL(POINT2(1)))  
      
*  Description:
*     Given a source image, the routine generates an equivalent image of
*     similar size that shows, at each pixel, the degree of correlation
*     that was present in the circular area around the equivalent point
*     in the source image. Correlation should normally be near zero for
*     a Gaussian (Normal) distribution. Large deviations from this 
*     suggest regions either containing objects.
*
*     The calculation is carried out as follows:
*
*     The value for each pixel of the output image is determined 
*     as follows. An imaginary circle is drawn about the pixel
*     and the count values for all pixels within that circle, 
*     are stored. 
*
*     The values then have the background value subtracted and are
*     multiplied by an elliptical factor depending on the distance of
*     pixel from the resident pixel. 
*
*     The template size is set as 1.8x the size of the galaxy to be detected.
*     This is the optimal size.
*
*     The individual contributions from all the pixels within the user defined
*     radius are summed and then divided by the number found (non-bad) to
*     normalise the result. 

*  Arguments:
*     MULT = REAL (Given)
*        The multiplying value applied to all the correlation values.
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image.
*     ARRAY(ELEMS) = REAL (Given)
*        The array containing the source NDF image count values.
*     RADIUS = REAL (Given)
*        The radius of the circular template that is used to define
*        the region about a given pixel, that might contribute to the 
*        correlation value that will be inserted into
*        the equivalent pixel in ARRAY2.
*     MODE = REAL (Given)
*        The global mode value supplied by the user. Units counts.
*     XMAX = INTEGER (Given)
*        The length of the image x axis. Units pixels.
*     YMAX = INTEGER (Given)
*        The length of the y axis of the image. Units pixels.
*     USEALL = LOGICAL (Given)
*        Flag indicating whether or not pixels of too high a value will be
*        ignored.
*     HIEST = REAL (Given)
*        The highest pixel count value that will be used in the analysis.
*        All pixels with count values above this will be ignored. Units 
*        counts.
*     STATUS = INTEGER (Given and Returned) 
*        The global status.     
*     ARRAY2(ELEMS) = REAL (Returned)
*        The array that eventually contains the correlation 'image'.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     14-Oct-1992
*     (Original version)

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'cor_par'               ! CORR constants
                     
*  Arguments Given:
      LOGICAL USEALL                  ! Is a high count cut out being 
                                      ! used?
      INTEGER ELEMS                   ! Number of pixels in the data array
      INTEGER RADIUS                  ! Radius of the size of template
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
                                      ! correlation results

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
                                      ! pixel size and template size 
      INTEGER OFFSETS(COR1__PIXN)     ! Address offsets for the
                                      ! pixels in the circular
                                      ! template used
      INTEGER PERC                    ! Percentage of the calculations 
                                      ! done so far                  
      INTEGER X                       ! Pixel x axis index
      INTEGER X1                      ! Pixel x axis index offset
      INTEGER Y                       ! Pixel y axis index
      INTEGER Y1                      ! Pixel y axis index

      REAL CORR                       ! Temporary correlation sum
      REAL ELF(COR1__PIXN)            ! Elliptical multiplying factor
                                      ! for each of the pixels in the 
                                      ! pixels in the circular
                                      ! template used
      REAL ELFSQ(COR1__PIXN)          ! Elliptical multiplying factor squared
      REAL RRADIUS                    ! Radius value
      REAL SUMSQ1                     ! Sum of squares for data
      REAL SUMSQ2                     ! Sum of squares for mask
      REAL TEMP                       ! Temporary storage
      REAL TRAD                       ! Temporary radius value
      REAL VALUE                      ! Temporary storage 
      REAL VALUE1                     ! Temporary storage 
      REAL XR                         ! Temporary storage
      REAL YR                         ! Temporary storage
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set up the initial value for the output array.
      DO 10 I=1,ELEMS
         ARRAY2(I)=VAL__BADR
 10   CONTINUE

*   Real value for the radius.
      RRADIUS=REAL(RADIUS)

*   Construct an array containing the memory address offsets of all the
*   pixels in the template relative to the memory address of the circle 
*   centre. 
*
      NPIX=0
      DO 30 X1=-RADIUS,RADIUS    
         DO 20 Y1=-RADIUS,RADIUS

*         Calculate the distance to the origin pixel in the mask.
            XR=REAL(X1)
            YR=REAL(Y1)
            TRAD=SQRT(XR*XR+YR*YR)
            
*         Check that the pixel at pixel offsets X1,Y1 is
*         within a circle of the given radius and hence within the 
*         required circular area.
            IF (TRAD.LT.RADIUS) THEN

*            Calculate the memory address offset. 
               VALUE=Y1*XMAX+X1
  
*            Increment the address counter and store the 
*            address offset.
               NPIX=NPIX+1

*            Check that there are not too many pixels.
               IF (NPIX.GT.COR1__PIXN) THEN
                  STATUS=SAI__ERROR
                  CALL ERR_REP(' ','Too many pixels are being'//
     :                              ' used.',STATUS)
                  GOTO 9999
               ELSE
                  OFFSETS(NPIX)=VALUE
                  ELF(NPIX)=EXP(-TRAD/RRADIUS)
                  ELFSQ(NPIX)=ELF(NPIX)*ELF(NPIX)
               END IF

            END IF

 20      CONTINUE

 30   CONTINUE  

*   Consider all pixels within the template. Obviously, a point off the 
*   side of the image is not valid. To increase speed of operation only
*   the parts of the image where this is not the case are considered.
*   This leads to the generation of a border of bad-valued points around 
*   the image. The border width is the same as the radius of the circular 
*   template in use.
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


*            Consider all points found earlier. Clear counters first.
               SUMSQ1=0.0
               SUMSQ2=0.0
               CORR=0.0
               DO 40 J=1,NPIX

*               Get the value of the point but only uses if
*               it is not defined as bad. 
                  VALUE1=ARRAY(I+OFFSETS(J))
                  IF (VALUE1.NE.VAL__BADR) THEN
               
*                  Calculate sums of squares required.
                     TEMP=VALUE1-MODE
                     CORR=CORR+ELF(J)*TEMP
                     SUMSQ1=SUMSQ1+TEMP*TEMP
                     SUMSQ2=SUMSQ2+ELFSQ(J)

                  END IF

 40            CONTINUE  

*            Calculate correlation if some non-bad pixels were found. 
               IF (SUMSQ1.NE.0.0) THEN
                  ARRAY2(I)=MULT*CORR/SQRT(SUMSQ1*SUMSQ2)
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

*            Consider all points found earlier. Clear counters first.
               SUMSQ1=0.0
               SUMSQ2=0.0
               CORR=0.0
               DO 1040 J=1,NPIX

*               Get the value of the point but only use if
*               it is not defined as bad and is not above the
*               cutoff value. 
                  VALUE1=ARRAY(I+OFFSETS(J))

                  IF ((VALUE1.NE.VAL__BADR).AND.(VALUE1.LT.HIEST)) THEN
               
*                  Calculate sums of squares required.
                     TEMP=VALUE1-MODE
                     CORR=CORR+ELF(J)*TEMP
                     SUMSQ1=SUMSQ1+TEMP*TEMP
                     SUMSQ2=SUMSQ2+ELF(J)*ELF(J)

                  END IF

 1040          CONTINUE  

*            Calculate correlation if some non-bad pixels were found. 
               IF (SUMSQ1.NE.0.0) THEN
                  ARRAY2(I)=MULT*CORR/SQRT(SUMSQ1*SUMSQ2)
               END IF

 1090       CONTINUE

 1100    CONTINUE                    
      
      END IF
        
 9999 CONTINUE

      END
 

