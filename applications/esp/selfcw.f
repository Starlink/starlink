      SUBROUTINE SELFCW( STATUS )             
*+
*  Name:
*     SELFCW

*  Purpose:
*     To perform mixed cross-self-correlations on an NDF image file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SELFCW( STATUS )

*  Arguments:
*     STATUS = INTEGER(Given and Returned)
*     The global status.

*  Description:
*     Performs a mixed cross-self-correlation calculation on an input 
*     NDF image file. The resulting correlation image/plot is stored 
*     to disk.
*
*     The cross-self-correlated image may be used to find faint 
*     diffuse objects for a given scale length.
*
*     The circular exponential profile template used is of a size that 
*     optimises the search for galaxies of the scale length
*     requested by the user.
*
*     To reduce the influence of bright objects or cosmic rays;
*     the user may elect to employ a cut out pixel count value where 
*     any pixel found to be above that value is ignored. The cutout
*     value is determined by the user inputting a global background
*     count value (available via HISTPEAK), the background count 
*     standard deviation and the number of standard deviations above 
*     sky level at which the cutout should occur.
*
*     The user is required to enter a value for the scale length of 
*     of the object(s) of interest and also the image pixel size.
*
*     The method assumes some sort of symmetry is present in the 
*     objects detected but appears to work well on a wide range of 
*     image types.
*
*     A border is present in the output image which is of the same 
*     width as the radius of the template. All pixels within this 
*     border are assigned the value bad.
*
*     The correlation is optimised by making the template size 1.8x 
*     that of the galaxy scale length required. This factor was 
*     determined from simulations by Phillipps and Davies at Cardiff.

*  Usage:
*     SELFCW IN OUT SCALE PSIZE BACK USEALL MULT [SIGMA] [NSIGMA] 

*  ADAM Parameters:
*     BACK = _REAL (Read)
*        The modal pixel count value found in the input NDF.
*        Units counts.
*     IN = _NDF (Read)
*        The name of the NDF that is to be examined.
*     NSIGMA = _REAL(Read)
*        The number of standard deviations above the sky level
*        count value at which the pixel count cutoff occurs. 
*     MULT = _REAL (Read)
*        A multiplying factor used to modify the output range. 
*     OUT = _NDF (Write)
*        The name of the NDF that will be created. 
*     PSIZE = _REAL (Read)
*        The size of each pixel. Units arc seconds.
*     SCALE = _REAL (Read)
*        The scale length of the galaxies being searched for.
*        Units arc seconds.
*     SIGMA = _REAL (Read)
*        The standard deviation of the pixel background count within the 
*        input NDF. Should be determined using a routine such as
*        HISTPEAK which ignores outliers. 
*     USEALL = _LOGICAL (Read)
*        Used to indicate whether a pixel count threshold is to
*        be a applied when calculating the self-correlation.

*  Examples:
*     selfcw in=p2 out=scp2 scale=15. psize=0.96 back=1000.2 
*            useall=true
*        A self-correlation image, optimised for galaxies of a 15 arc
*        second scale length, is generated using image P2 as the input 
*        source image and SCP2 as the output image. The pixel size
*        on the image is .96 arc second and the background count value 
*        for the source image is 1000.2
*
*     selfcw in=lsbg1 out=lsbg2 scale=8. psize=1.5 back=444.
*            useall=false sigma=12. nsigma=4.
*        A self-correlation image, optimised for galaxies of a 8 arc
*        second scale length, is generated using image P2 as the input 
*        source image and SCP2 as the output image. The pixel size
*        on the image is 1.5 arc second and the background count value 
*        for the source image is 444.
*
*        All pixels with a count value greater than 444.+12.x4. are
*        excluded from the correlation calculations.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-MAY-1993 (GJP)
*     (Original version)

*  Notes:
*     It is assumed that the x and y axis pixels sizes are the same 
*     size.
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
      INCLUDE 'sel_par'               ! SELFC constants
                     
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
                                      ! for calculating self-correlation
      INTEGER UBND(7)                 ! Upper bounds for each image axis
      INTEGER XMAX                    ! Width in pixels of the image
      INTEGER YMAX                    ! Length in pixels of the image
      REAL DIAM                       ! Width of the template
      REAL HIEST                      ! Highest pixel count used
                                      ! in the correlation
      REAL MODE                       ! The global mode value for the
                                      ! NDF image
      REAL MULT                       ! Multiplying factor for output image
      REAL NSIGMA                     ! The number of standard deviations
                                      ! above the modal value that a 
                                      ! pixel must be, to be ignored
      REAL PSIZE                      ! The pixel size of the image
      REAL SCALE                      ! Scale length of the galaxies for
                                      ! which the template is optimised
      REAL SIGMA                      ! Standard deviation of the background
      LOGICAL USEALL                  ! Are all the pixels to be used or
                                      ! will the very bright be ignored
*.                              

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN
                                                                 
*   Begin an NDF context.                               
      CALL NDF_BEGIN

*   Indicate that the application is running.
      CALL MSG_BLANK(STATUS)
      CALL MSG_OUT(' ','ESP SELFCW running.',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Obtain an identifier for the NDF structure to be examined.       
      CALL NDF_ASSOC('IN','READ',NDF1,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Display the name of the NDF.
      CALL NDF_MSG('IN',NDF1)           
      CALL MSG_OUT(' ','Filename:   ^IN',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

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
      CALL NDF_STYPE('_REAL',NDF2,'Data',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Map the results NDF data array as _REAL values for updating.  
      CALL NDF_MAP(NDF2,'Data','_REAL','UPDATE',POINT2(1),ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Change the propogated title.
      CALL NDF_CPUT('ESP - Self/Cross-correlation Image',NDF2,'TITLE',
     :               STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine the galaxy size required. 
      CALL PAR_GET0R('SCALE',SCALE,STATUS) 
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Optimise the filter width.
      DIAM=1.8*SCALE*2.

*   Determine the pixel size.
      CALL PAR_GET0R('PSIZE',PSIZE,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Check that the pixel size is not too small.
      IF (PSIZE.LT.SEL1__VSMAL) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The pixel size supplied is too small.',
     :                STATUS)
         GOTO 9999
      END IF

*   Check that the resultant radius isnt too small.
      RADIUS=NINT(DIAM/PSIZE)
      IF (RADIUS.LT.1) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The number of pixels involved is'//
     :                ' too small.',STATUS)
         GOTO 9999
      END IF

*   Determine the global mode value required.
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

*   Determine the multiplying factor required. 
      CALL PAR_GET0R('MULT',MULT,STATUS) 
      IF (STATUS.NE.SAI__OK) GOTO 9999

      CALL MSG_BLANK(STATUS)

*   Inform the user of what will be done, since the routine takes a 
*   long while to run.
      CALL NDF_MSG('FOUT',NDF1)           
      CALL MSG_OUT(' ','Self-correlating file: ^FOUT',STATUS)
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

*   Create an self-correlation image.
         CALL SEL1_SELFCW(MULT,ELEMS,%VAL(POINT1(1)),RADIUS,MODE,
     :              XMAX,YMAX,USEALL,HIEST,STATUS,%VAL(POINT2(1)))  
         IF (STATUS.NE.SAI__OK) GOTO 9999
     
 9999 CONTINUE

*   Un-map the NDF data arrays.
      CALL NDF_UNMAP(NDF1,'Data',STATUS)
      CALL NDF_UNMAP(NDF2,'Data',STATUS) 

*   End the NDF context.
      CALL NDF_END(STATUS)

      END      
                                                 

      SUBROUTINE SEL1_SELFCW(MULT,ELEMS,ARRAY,RADIUS,MODE, 
     :                       XMAX,YMAX,USEALL,HIEST,STATUS,ARRAY2)    
*+
*  Name: 
*     SEL1_SELFCW

*  Purpose:
*     Performs mixed cross-self-correlation calculations for a circular 
*     symmetrical template of known diameter. 
*
*     It assumes a global mode value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SEL1_SELFCW(MULT,ELEMS,%VAL(POINT1(1)),RADIUS,MODE,
*                      XMAX,YMAX,USEALL,HIEST,STATUS,%VAL(POINT2(1)))  
      
*  Description:
*     Given a source image, the routine generates an equivalent image of
*     similar size that shows, at each pixel, the degree of self-correlation
*     that was present in the circular area around the equivalent point
*     in the source image. Correlation may be both negative, indicating
*     that the object is below sky - probably due to poor flat fielding, or
*     may be positive indicating that there may be an object present.
*
*     The calculation is carried out as follows:
*
*     The value for each pixel of the output image is determined 
*     as follows. An imaginary circle is drawn about the pixel
*     and all pixel pairs within that circle, that lie on opposite
*     sides of the centre from each other, are stored. 
*
*     For each image pixel in turn, all the pixels within a defined
*     radius are identified. These are then examiined to locate pairs of
*     diametrically opposed pixels. The rms value for each of the pairs of
*     pixels (background subtracted) is retained with sign maintained.
*     The value for each of these pairs are multiplied by an exponential
*     function and summed. The total is divided by the number of 
*     pixel pairs used (pairs with bad pixels ignored). This normalised
*     sum is then placed in the appropriate pixel of the output
*     image pixel. The procedure is then returned for the next pixel of
*     the image. 
 
*  Arguments:
*     MULT = REAL (Given)
*        Multiplying factor for the output.
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image.
*     ARRAY(ELEMS) = REAL (Given)
*        The array containing the source NDF image count values.
*     RADIUS = REAL (Given)
*        The radius of the circular template that is used to define
*        the region about a given pixel, that might contribute to the 
*        self-correlation function value that will be inserted into
*        the equivalent pixel in ARRAY2.
*     MODE = REAL (Given)
*        The global mode value supplied by the user. Units counts.
*     XMAX = INTEGER (Given)
*        The length of the image x axis. Units pixels.
*     YMAX = INTEGER (Given)
*        The length of the y axis of the image. Units pixels.
*     USEALL = LOGICAL (Given)
*        Flag indicating whether or not a pixel count cutoff threshold is
*        being used.
*     HIEST = REAL (Given)
*        The highest pixel count value that will be used in the analysis.
*        All pixels with count values above this will be ignored. Units 
*        counts.
*     STATUS = INTEGER (Given and Returned) 
*        The global status.     
*     ARRAY2(ELEMS) = REAL (Returned)
*        The array that eventually contains the self-correlation
*        'image'. 

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     14-MAY-1993
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'sel_par'               ! SELFC constants
                     
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
      REAL MULT                       ! Multiplying factor for the output

*  Arguments Returned:
      REAL ARRAY2(ELEMS)              ! Image array containing the
                                      ! self-corrlelation results

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:                 
      INTEGER I                       ! Loop variable 
      INTEGER ADD2                    ! Array element address
      INTEGER J                       ! Loop variable
      INTEGER N                       ! The number of valid pixel pairs
                                      ! that were present about a given
                                      ! origin pixel
      INTEGER NPIX                    ! The maximum number of pixel pairs 
                                      ! about a given origin. Depends on the 
                                      ! pixel size and template size 
      INTEGER OFFSETS(SEL1__PIXN)     ! Address offsets for the
                                      ! pixels in one hemisphere
                                      ! of the circular area used
      INTEGER PERC                    ! Percentage of the calculations 
                                      ! done so far   
      INTEGER S1                      ! Sign of one of the pixel
                                      ! pair
      INTEGER S2                      ! Sign of one of the pixel
                                      ! pair               
      INTEGER X                       ! Pixel x axis index
      INTEGER X1                      ! Pixel x axis index offset
      INTEGER Y                       ! Pixel y axis index
      INTEGER Y1                      ! Pixel y axis index

      REAL CORR                       ! Correlation value
      REAL MFACT(SEL1__PIXN)          ! Mask factor
      REAL MFACTSQ(SEL1__PIXN)        ! Mask factor squared
      REAL R                          ! Temporary radius storage
      REAL RRADIUS                    ! Real value fo radius
      REAL SUMSQ1                     ! Sum of squares
      REAL SUMSQ2                     ! Sum of squares
      REAL TEMP1                      ! Temporary value
      REAL TEMP2                      ! Temporary value
      REAL VALUE                      ! Temporary storage 
      REAL VALUES(SEL1__PIXN,4)       ! Values for pairs of pixels
                                      ! minus the mode value and 
                                      ! also their weighting factors
      REAL VALUE1                     ! Temporary storage 
      REAL VALUE2                     ! Temporary storage 
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set up the initial value for the output array.
      DO 10 I=1,ELEMS
         ARRAY2(I)=VAL__BADR
 10   CONTINUE

*   Create real equivalent of radius.
      RRADIUS=REAL(RADIUS)

*   Construct an array containing the memory address offsets of all the
*   pixels in one hemisphere of a circular area
*   relative to the memory address of the circle centre. 
*
*   The hemisphere is chosen so that each pixel has a matching pixel
*   on the other side of the centre of the circle at a memory address offset
*   that is the same in magnitude, but with an opposite sign.
* 
*   This allows the addresses of pairs of points, equidistantly 
*   placed on opposite sides of the origin, to be found using only
*   one array. 
      NPIX=0
      DO 30 X1=-RADIUS,RADIUS    
         DO 20 Y1=-RADIUS,0

*         Calculate the distance to tthe origin. 
            R=SQRT(REAL(X1*X1+Y1*Y1))

*         Check that the pixel at pixel offsets X1,Y1 is
*         within a circle of the given radius and hence within the 
*         required circular area.
            IF (R.LE.RADIUS) THEN

*            Calculate the memory address offset. 
               VALUE=Y1*XMAX+X1
  
*            Ensure that only points in the correct hemisphere
*            are used. The point at the origin itself is ignored
*            reduce the influence of cosmic rays.
               IF (VALUE.LT.0) THEN

*               Increment the address counter and store the 
*               address offset.
                  NPIX=NPIX+1

*               Check that there are not too many pixels.
                  IF (NPIX.GT.SEL1__PIXN) THEN
                     STATUS=SAI__ERROR
                     CALL ERR_REP(' ','Too many pixels are being'//
     :                              ' used.',STATUS)
                     GOTO 9999
                  ELSE

*                  Store array offset.
                     OFFSETS(NPIX)=VALUE

*                  Pixel mask factors.
                     MFACT(NPIX)=EXP(-R/RRADIUS)
                     MFACTSQ(NPIX)=MFACT(NPIX)*MFACT(NPIX)

                  END IF

               END IF

            END IF

 20      CONTINUE

 30   CONTINUE  

*   Consider all pixels where there is a legal pixel on the diametrically
*   opposite side of the circle centre. Obviously, a point off the side 
*   of the image is not valid. This leads to the generation of a border of
*   bad-valued points around the image. The border width is the same as
*   the radius of the circular filter in use.
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
*            pairs found.
               N=0
               I=ADD2+X

*            Consider all points in the hemisphere of pixels generated
*            earlier.
               DO 40 J=1,NPIX

*               Get the values of the points and only use if
*               they are not defined as bad. 
                  VALUE1=ARRAY(I+OFFSETS(J))
                  IF (VALUE1.NE.VAL__BADR) THEN

                     VALUE2=ARRAY(I-OFFSETS(J))
                     IF (VALUE2.NE.VAL__BADR) THEN
               
*                     Increment the number of usable pixels pairs 
*                     found and retain their values. Also keep the value 
*                     for the weighting factor.
                        N=N+1
                        VALUES(N,1)=VALUE1
                        VALUES(N,2)=VALUE2
                        VALUES(N,3)=MFACT(J)
                        VALUES(N,4)=MFACTSQ(J)

                     END IF

                 END IF

 40            CONTINUE  

*            Consider all the pixel pairs that were found.
               IF (N.GT.0) THEN 

*               Multiply together the values of the pixels (-mode) on
*               opposite sides of the circle origin and sum over all
*               the usable pixels. 
                  CORR=0.0
                  SUMSQ1=0.0
                  SUMSQ2=0.0
                  DO 50 J=1,N

*                  Find the RMS count relative to sky.
*                  Retaining sign information.
                     TEMP1=(VALUES(J,1)-MODE)
                     TEMP2=(VALUES(J,2)-MODE)
                     VALUE=SQRT(ABS(TEMP1*TEMP2))

*                  Check the final sign.
                     S1=SIGN(1.,TEMP1)
                     S2=SIGN(1.,TEMP2)  
                     IF (S1.EQ.S2) THEN 
                        VALUE=S1*VALUE
                     ELSE
                        VALUE=-VALUE
                     END IF

*                  Increase the summation counters.
                     SUMSQ1=SUMSQ1+VALUES(J,4)
                     SUMSQ2=SUMSQ2+VALUE*VALUE
                     CORR=CORR+VALUE*VALUES(J,3)

 50               CONTINUE

*               Return the self-correlation magnitude with the
*               appropriate sign.
                  IF (SUMSQ2.NE.0.0) THEN
                     ARRAY2(I)=MULT*CORR/SQRT(SUMSQ1*SUMSQ2)
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
*            pairs found.
               N=0
               I=ADD2+X

*            Consider all points in the hemisphere of pixels generated
*            earlier.
               DO 1040 J=1,NPIX

*               Get the values of the points and only use if
*               they are not defined as bad and are not above the
*               cutoff value. 
                  VALUE1=ARRAY(I+OFFSETS(J))

                  IF ((VALUE1.NE.VAL__BADR).AND.(VALUE1.LT.HIEST)) THEN

                     VALUE2=ARRAY(I-OFFSETS(J))

                     IF ((VALUE2.NE.VAL__BADR).AND.
     :                   (VALUE2.LT.HIEST)) THEN
               
*                     Increment the number of usable pixels pairs 
*                     found and retain their values. Aslo keep the
*                     appropraite weighting factor.
                       N=N+1
                       VALUES(N,1)=VALUE1
                       VALUES(N,2)=VALUE2
                       VALUES(N,3)=MFACT(J)

                     END IF

                 END IF

 1040          CONTINUE  

*            Consider all the pixel pairs that were found.
               IF (N.GT.0) THEN 

*               Multiply together the values of the pixels (-mode) on
*               opposite sides of the circle origin and sum over all
*               the usable pixels. 
                  CORR=0.0
                  SUMSQ1=0.0
                  SUMSQ2=0.0
                  DO 1050 J=1,N

*                  Find the RMS count relative to sky.
*                  Retaining sign information.
                     TEMP1=(VALUES(J,1)-MODE)
                     TEMP2=(VALUES(J,2)-MODE)
                     VALUE=SQRT(ABS(TEMP1*TEMP2))

*                  Check the final sign.
                     S1=SIGN(1.,TEMP1)
                     S2=SIGN(1.,TEMP2)  
                     IF (S1.EQ.S2) THEN 
                        VALUE=S1*VALUE
                     ELSE
                        VALUE=-VALUE
                     END IF

*                  Increase the summation counters.
                     SUMSQ1=SUMSQ1+VALUES(J,3)*VALUES(J,3)
                     SUMSQ2=SUMSQ2+VALUE*VALUE
                     CORR=CORR+VALUE*VALUES(J,3)

 1050             CONTINUE

*               Return the self-correlation magnitude with the
*               appropriate sign.
                  IF (SUMSQ2.NE.0.0) THEN
                     ARRAY2(I)=MULT*CORR/SQRT(SUMSQ1*SUMSQ2)
                  END IF

               END IF

 1090       CONTINUE

 1100    CONTINUE                    
      
      END IF
        
 9999 CONTINUE

      END 

