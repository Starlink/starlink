      SUBROUTINE TOPPED( STATUS )             
*+
*  Name:
*     TOPPED

*  Purpose:
*     Remove all pixel values above a certain limit from
*     an NDF image file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TOPPED( STATUS )

*  Arguments:
*     STATUS = INTEGER(Given and Returned)
*     The global status.

*  Description:
*     Sets to bad, all the pixels with a count above the threshold 
*     level. An option allows the close neighbours of the bright pixel 
*     to be set to bad as well. Close neighbours are considered to be 
*     those pixels within a user defined radius of the bright pixel.
*
*     A further option allows all bad pixels in the output image to be 
*     assigned a random value. The values chosen are taken from a 
*     Normal distribution defined by the user.

*  Usage:
*     TOPPED IN OUT WIDTH BACK SIGMA NSIGMA NOISE PSIZE

*  ADAM Parameters:
*     BACK = _REAL (Read)
*        The background count value. Units counts.
*     IN = _NDF (Read)
*        The name of the NDF that is to be examined.
*     NOISE = _LOGICAL (Read)
*        Defines whether or not bad pixels should eventually be
*        assigned a random value.
*     NSIGMA = _REAL (Read)
*        The number of standard deviations above sky at which the 
*        cutoff occurs.
*     OUT = _NDF (Write)
*        The name of the output NDF that will be created.  
*     PSIZE = _REAL (Read)
*        The size of each pixel. Units arc seconds.
*     SIGMA = _REAL (Read)                     
*        The background pixel count standard deviation value. Units 
*        counts.
*     WIDTH = _REAL (Read)
*        The width of the circle around a bright pixel within which 
*        pixels will be set to bad. Units arc seconds.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-Jan-1993 (GJP)
*     Original version.
*     19-Oct-1996 (GJP)
*     NAG routines removed.

*  Examples:
*     topped in=eggs out=scrambled width=2.5 psize=0.44 back=1000. 
*            sigma=23. nsigma=8. noise=false
*        Uses EGGS as the input image and finds all pixels within the 
*        image that have a count greater than 1000.+8.x23. These are 
*        all set to the bad value. In addition, all pixels within a 
*        radius of 1.25 arc seconds are also set to bad.
* 
*     topped in=objects out=cut width=4. psize=1. back=6200. sigma=390.
*            nsigma=10. noise=true
*        Uses OBJECTS as the input image and finds all pixels within
*        the image that have a count value greater than 6200.+10.x390..
*        These are all set to random values, as are all the pixels 
*        within a radius of 2. arc seconds. 
       
*  Notes:
*     The distribution of pixel values used when NOISE=TRUE comes 
*     from a Normal (Gaussian) distribution. In some circumstances,
*     particularly for low count values, this may not be appropriate.
 
*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'top_par'               ! TOPPED constants
                     
*  Status:     
      INTEGER STATUS                  ! Global status

*  Local Variables:
      LOGICAL NOISE                   ! Is noise to be put into bad parts 
                                      ! of the resultant image?          
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
      INTEGER RADIUS                  ! Radius of the circular area around
                                      ! bright pixels to be set bad
      INTEGER UBND(7)                 ! Upper bounds for each image axis
      INTEGER XMAX                    ! Width in pixels of the image
      INTEGER YMAX                    ! Length in pixels of the image
      REAL BACK                       ! background pixel count
      REAL NSIGMA                     ! The number of standard deviations
                                      ! above the modal value, above which  
                                      ! pixels, will be be ignored
      REAL PSIZE                      ! The pixel size of the image
      REAL SIGMA                      ! Standard deviation of the background
      REAL WIDTH                      ! Width of the circular area used
                                      ! for setting points to bad
*.                              

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN
                                                                 
*   Begin an NDF context.                               
      CALL NDF_BEGIN

*   Indicate that the application is running.
      CALL MSG_BLANK(STATUS)
      CALL MSG_OUT(' ','ESP TOPPED running.',STATUS)
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
      XMAX=PRANGE(1)
      YMAX=PRANGE(2)

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
      CALL NDF_CPUT('ESP - Topped Image',NDF2,'TITLE',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine the number of pixels to be eliminated on either side of 
*   pixels that are too bright.
      CALL PAR_GET0R('WIDTH',WIDTH,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF (WIDTH.LT.TOP1__VSMAL) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The width selected is too small.',STATUS)
         GOTO 9999
      END IF

*   Get the pixel size.
      CALL PAR_GET0R('PSIZE',PSIZE,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF (PSIZE.LT.TOP1__VSMAL) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The pixel size is too small.',STATUS)
         GOTO 9999
      END IF

*   Determine the radius of the circular area about a bright pixel 
*   within which all the pixels are to be set to bad.
      RADIUS=NINT(WIDTH/PSIZE/2.)
      IF (RADIUS.LT.1) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The radius calculated is too small.',STATUS)
         GOTO 9999
      END IF

*   Determine the background pixel count required. 
      CALL PAR_GET0R('BACK',BACK,STATUS) 
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine the background standard deviation required. 
      CALL PAR_GET0R('SIGMA',SIGMA,STATUS) 
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine the count threshold value required. 
      CALL PAR_GET0R('NSIGMA',NSIGMA,STATUS) 
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine whether bad data points are to be assigned a random value. 
      CALL PAR_GET0L('NOISE',NOISE,STATUS) 
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Inform the user of what will be done, since the routine takes a 
*   long while to run.
      CALL MSG_BLANK(STATUS)
      CALL NDF_MSG('FOUT',NDF1)           
      CALL MSG_OUT(' ','Applying TOPPED to file: ^FOUT',STATUS)
      CALL NDF_MSG('FOUT',NDF2)           
      CALL MSG_OUT(' ','Results file will be:    ^FOUT',STATUS)
      CALL MSG_BLANK(STATUS)

*   Carry out the operation to set the pixels immediately surrounding 
*   bright pixels to bad.
      CALL TOP1_REMOV(ELEMS,%VAL(POINT1(1)),NOISE,BACK,SIGMA,NSIGMA,
     :                RADIUS,XMAX,YMAX,%VAL(POINT2(1)),STATUS)  
      IF (STATUS.NE.SAI__OK) GOTO 9999
     
 9999 CONTINUE

*   Un-map the NDF data arrays.
      CALL NDF_UNMAP(NDF1,'Data',STATUS)
      CALL NDF_UNMAP(NDF2,'Data',STATUS) 

*   End the NDF context.
      CALL NDF_END(STATUS)

      END      
                                                 

      SUBROUTINE TOP1_REMOV(ELEMS,ARRAY,NOISE,BACK,SIGMA,NSIGMA,RADIUS,
     :                      XMAX,YMAX,ARRAY2,STATUS)  
*+
*  Name: 
*     TOP1_REMOV

*  Purpose:
*     Remove pixels immediately surrounding bright pixels.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TOP1_REMOV(ELEMS,ARRAY,NOISE,BACK,SIGMA,NSIGMA,RADIUS,
*                     XMAX,YMAX,ARRAY2,STATUS)  
      
*  Description:
*     Given a source image, the routine generates an equivalent image of
*     wherein all pixels within a circular radius (RADIUS) about bright
*     pixels (ie count exceeding COUNT) have been set to bad.

*  Arguments:
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image.
*     ARRAY(ELEMS) = REAL (Given)
*        The array containing the source NDF image count values.
*     NOISE = LOGICAL (Given)
*        Whether or not a random value is to be assigned to points set 
*        to the bad value.
*     BACK = REAL (Given)
*        The background count for the image. Units counts.
*     SIGMA = REAL (Given)
*        Standard deviation of the sky background count. Units counts.
*     NSIGMA = REAL (Given)
*        The number of standard deviations above sky at which the 
*        pixel cutoff occurs.
*     RADIUS = INTEGER (Given)
*        The radius of the circular template that is used to define
*        the bad region about bright pixels.
*     XMAX = INTEGER (Given)
*        The length of the image x axis. Units pixels.
*     YMAX = INTEGER (Given)
*        The length of the y axis of the image. Units pixels.
*     ARRAY2(ELEMS) = REAL (Returned)
*        The array that eventually contains the returned 'image'.
*     STATUS = INTEGER (Given and Returned) 
*        The global status.     

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     27-Jan-1993
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'top_par'               ! TOPPED constants
                     
*  Arguments Given:
      LOGICAL NOISE                   ! Random noise instead of bad 
                                      ! points flag
      INTEGER ELEMS                   ! Number of pixels in the data array
      INTEGER RADIUS                  ! Radius of the masking template
                                      ! being used
      INTEGER XMAX                    ! Length of the image x axis
      INTEGER YMAX                    ! Length of the image y axis
      REAL ARRAY(ELEMS)               ! Array containing NDF data
      REAL BACK                       ! Background sky count value
      REAL NSIGMA                     ! Pixel brightness cutoff value
      REAL SIGMA                      ! Standard deviation of the background count

*  Arguments Returned:
      REAL ARRAY2(ELEMS)              ! Image array containing the
                                      ! output image

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:                 
      INTEGER I                       ! Loop variable 
      INTEGER ADD2                    ! Array element address
      INTEGER J                       ! Loop variable
      INTEGER NPIX                    ! The maximum number of pixels 
                                      ! about a given origin. Depends on the 
                                      ! pixel size and filter width. 
      INTEGER OFFSETS(3,TOP1__PIXN)   ! Address offsets for the
                                      ! pixels in the circular
                                      ! template used
      INTEGER PERC                    ! Percentage of the calculations 
                                      ! done so far         
      INTEGER TEMP                    ! temporary variable         
      INTEGER X                       ! Pixel x axis index
      INTEGER X1                      ! Pixel x axis index offset
      INTEGER Y                       ! Pixel y axis index
      INTEGER Y1                      ! Pixel y axis index
      REAL LIMIT                      ! The count value above which
                                      ! a pixel will be erased.
      REAL VALUE                      ! Temporary storage 

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Calculate the threshold value to be used.
      LIMIT=BACK+SIGMA*NSIGMA

*   Set up the initial value for the output array.
      DO 10 I=1,ELEMS
         ARRAY2(I)=ARRAY(I)
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
               IF (NPIX.GT.TOP1__PIXN) THEN
                  STATUS=SAI__ERROR
                  CALL ERR_REP(' ','Too many pixels are being'//
     :                              'used.',STATUS)
                  GOTO 9999
               ELSE
                  OFFSETS(1,NPIX)=VALUE
                  OFFSETS(2,NPIX)=X1
                  OFFSETS(3,NPIX)=Y1
               END IF

            END IF

 20      CONTINUE

 30   CONTINUE  

*   Consider all pixels within the image. Obviously, a point off the 
*   side of the image is not valid. 

*   Perform calculations for the situation where there is a high
*   count cutofff defined by HIEST
      PERC=0
      DO 1100 Y=1,YMAX

*      Indicate that something is happening.
         IF (Y.EQ.NINT(Y/50.)*50) THEN
            PERC=NINT(Y*100./YMAX)
            CALL MSG_SETI('PERC',PERC)
            CALL MSG_OUT(' ','Percentage done so far: ^PERC',STATUS)
         END IF

*       Calculate the first component of the central pixel address.
         ADD2=(Y-1)*XMAX

         DO 1090 X=1,XMAX

*         Calculate the full address of the central pixel address.
            I=ADD2+X

*         Check to see if the pixel value is higher than LIMIT.
*         If it is, set the surrounding points (offsets calculated 
*         earlier) to bad.
            IF (ARRAY(I).GT.LIMIT) THEN

*            Consider all surrounding points.
               ARRAY2(I)=VAL__BADR
               DO 1040 J=1,NPIX

*               Check that the pixel is not off the edge of 
*               the picture.
                  TEMP=X+OFFSETS(2,J)

                  IF ((TEMP.GT.0).AND.(TEMP.LT.XMAX+1)) THEN

*                  Check that the pixel is not off the edge of 
*                  the picture.
                     TEMP=Y+OFFSETS(3,J)
                     IF ((TEMP.GT.0).AND.(TEMP.LT.YMAX+1)) THEN

*                     Set the pixel to bad.                  
                        ARRAY2(I+OFFSETS(1,J))=VAL__BADR

                     END IF

                  ENDIF

 1040          CONTINUE  

            END IF

 1090    CONTINUE

 1100 CONTINUE                    
      
*   Indicate how many pixels have had their values modified.
      TEMP=0
      DO 2000 I=1,ELEMS
         IF (ARRAY2(I).EQ.VAL__BADR) TEMP=TEMP+1
 2000 CONTINUE

      CALL MSG_BLANK(STATUS)
      VALUE=REAL(TEMP)/REAL(ELEMS)*100.
      CALL MSG_SETR('VALUE',VALUE)
      CALL MSG_BLANK(STATUS)

*   Look through image array. If a bad data point is found then it is assigned
*   a random value from the normal distribution defined by the background and
*   standard deviation values supplied. Only done if NOISE is true.
      IF (NOISE) THEN
      
*      Initialise the random number generator.
         CALL TOP1_RAND1(2001,VALUE,STATUS) 

*      Look through all the image points.
         DO 3000 I=1,ELEMS

*         Replace a bad point with noise.
            IF (ARRAY2(I).EQ.VAL__BADR) THEN
               CALL TOP1_RAND2(0,VALUE,STATUS)
               ARRAY2(I)=BACK+SIGMA*VALUE
            END IF

 3000    CONTINUE
   
         CALL MSG_OUT(' ','Percentage of pixels reset: ^VALUE',STATUS)
         CALL MSG_BLANK(STATUS)

      ELSE

         CALL MSG_OUT(' ','Percentage of pixels bad: ^VALUE',STATUS)
         CALL MSG_BLANK(STATUS)

      END IF

 9999 CONTINUE

      END


      SUBROUTINE TOP1_RAND1(SEED,VALUE,STATUS)
*+
*  Name:
*     TOP1_RAND1
 
*  Purpose:
*     Provide values from a Normal distribution of std dev 1.
*     Initialisation routine.
      
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL TOP1_RAND1(SEED,VALUE,STATUS)    
 
*  Description:
*     Crude and simple random number generator based
*     upon a NIST routine supplied by Malcolm Currie.
 
*  Arguments:               
*     SEED = INTEGER (Given)
*        Random number seed.
*     VALUE = REAL (Returned)
*        Random number created.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Authors:
*     GJP: Grant Privett (STARLINK)
 
*  History:
*     12-Oct-1996 (GJP)
*     (Original version)
 
*  Bugs:
*     None known.
 
*-
 
*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
 
*  Arguments Given:                              
      INTEGER SEED                    ! Seed value.
 
*  Arguments Returned:
      REAL VALUE                      ! Random number
 
*  Status:     
      INTEGER STATUS                  ! Global status
 
*  Local variables:
      DOUBLE PRECISION R
      DOUBLE PRECISION FACTOR
      DOUBLE PRECISION TWO28
 
      DATA FACTOR /41475557.0D0/, TWO28 /268435456.0D0/
      SAVE R
*.
  
*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   
 
*   Initialisation.
      IF (SEED.GT.0) R = DBLE(FLOAT(SEED))/TWO28
 
*   Normal use.
      R = DMOD(R*FACTOR, 1.0D0)
      VALUE = SNGL(R)
 
      END
 
  

      SUBROUTINE TOP1_RAND2(IR,VALUE,STATUS)
 
*+
*  Name:
*     TOP1_RAND2
 
*  Purpose:
*     Provide Gaussian number routine providing values
*     from a distribution of std dev 1.
      
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*      CALL TOP1_RAND2(IR,VALUE,STATUS)    
 
*  Description:
*      Crude and simple random number generator based
*      upon a NIST routine supplied by Malcolm Currie.
 
*  Arguments:               
*     IR = INTEGER (Given)
*        Random number seed.
*     VALUE = REAL (Returned)
*        Random number created.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Authors:
*     GJP: Grant Privett (STARLINK)
 
*  History:
*     12-Oct-1996 (GJP)
*     (Original version)
 
*  Bugs:
*     None known.
 
*-
 
*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
 
*  Arguments Given:                              
      INTEGER IR                      ! Seed value.
 
*  Arguments Returned:
      REAL VALUE                      ! RAndom number
 
*  Status:     
      INTEGER STATUS                  ! Global status
 
*  Local variables:
      DOUBLE PRECISION A(32),D(31),T(31),H(31)
      REAL             U,S,USTAR,W,TT,AA,Y
      INTEGER          I 
*.
 
*  Distribution data.
    
      DATA A/0.0, .3917609E-1, .7841241E-1, .1177699, .1573107, 
     :     .1970991, .2372021, .2776904, .3186394, .3601299, .4022501, 
     :     .4450965, .4887764, .5334097, .5791322, .6260990, .6744898, 
     :     .7245144, .7764218, .8305109, .8871466, .9467818, 1.009990, 
     :     1.077516, 1.150349, 1.229859, 1.318011, 1.417797, 1.534121, 
     :     1.675940, 1.862732, 2.153875/
 
      DATA D/5*0.0, .2636843, .2425085, .2255674, .2116342, .1999243, 
     :     .1899108, .1812252, .1736014, .1668419, .1607967, .1553497, 
     :     .1504094, .1459026, .1417700, .1379632, .1344418, .1311722, 
     :     .1281260, .1252791, .1226109, .1201036, .1177417, .1155119, 
     :     .1134023, .1114027, .1095039/
 
      DATA T/.7673828E-3, .2306870E-2, .3860618E-2, .5438454E-2, 
     :     .7050699E-2, .8708396E-2, .1042357E-1, .1220953E-1, 
     :     .1408125E-1, .1605579E-1, .1815290E-1, .2039573E-1, 
     :     .2281177E-1, .2543407E-1, .2830296E-1, .3146822E-1, 
     :     .3499233E-1, .3895483E-1, .4345878E-1, .4864035E-1, 
     :     .5468334E-1, .6184222E-1, .7047983E-1, .8113195E-1, 
     :     .9462444E-1, .1123001, .1364980, .1716886, .2276241, 
     :     .3304980, .5847031/
 
      DATA H/.3920617E-1, .3932705E-1, .3950999E-1, .3975703E-1, 
     :     .4007093E-1, .4045533E-1, .4091481E-1, .4145507E-1, 
     :     .4208311E-1, .4280748E-1, .4363863E-1, .4458932E-1, 
     :     .4567523E-1, .4691571E-1, .4833487E-1, .4996298E-1, 
     :     .5183859E-1, .5401138E-1, .5654656E-1, .5953130E-1, 
     :     .6308489E-1, .6737503E-1, .7264544E-1, .7926471E-1, 
     :     .8781922E-1, .9930398E-1, .1155599, .1404344, .1836142, 
     :     .2790016, .7010474/
 
 
*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   
 
      CALL TOP1_RAND1(IR,VALUE,STATUS)
      U = VALUE
      
      IF ( U.GE.0.5 ) THEN
         S = 1.0
      ELSE
         S = 0.0
      END IF

      U = U + U - S
 
      U = 32.0*U
      I = INT(U)
      IF ( I.EQ.0 ) THEN
 
C   START TAIL
 
         I = 6
         AA = A(32)
 
 50      CONTINUE
         U = U + U
         IF ( U.LT.1.0 ) THEN
            AA = AA + D(I)
            I = I + 1
            GO TO 50
         ELSE
            U = U - 1.0
            GO TO 300
         END IF
 
      ELSE
 
C   START CENTER
 
         USTAR = U - FLOAT(I)
         AA = A(I)
 100     CONTINUE
         IF ( USTAR.LE.T(I) ) THEN
 
C   CENTER CONTINUED
 
            CALL TOP1_RAND1(IR,VALUE,STATUS)
            U = VALUE
            W = U*(A(I+1)-AA)
            TT = (0.5*W+AA)*W
 120        CONTINUE
            IF ( USTAR.LE.TT ) THEN
               CALL TOP1_RAND1(IR,VALUE,STATUS)
               U = VALUE
               IF ( USTAR.GE.U ) THEN
                  TT = U
                  CALL TOP1_RAND1(IR,VALUE,STATUS)
                  USTAR = VALUE
                  GO TO 120
               ELSE
                  CALL TOP1_RAND1(IR,VALUE,STATUS)
                  USTAR = VALUE
                  GO TO 100
               END IF
            END IF
         ELSE
            W = (USTAR-T(I))*H(I)
         END IF
      END IF
 
C   EXIT   (BOTH CASES)
 
 
 200  CONTINUE

      Y = AA + W
      IF ( S.EQ.1.0 ) THEN
         VALUE = -Y
      ELSE
         VALUE = Y
      END IF
 
      RETURN
 
 
 300  CONTINUE
      W = U*D(I)
      TT = (0.5*W+AA)*W
 
 400  CONTINUE
      CALL TOP1_RAND1(IR,VALUE,STATUS)
      USTAR = VALUE
 
      IF ( USTAR.GT.TT ) GO TO 200
 
      CALL TOP1_RAND1(IR,VALUE,STATUS)
      U = VALUE
 
      IF ( USTAR.GE.U ) THEN
 
         TT = U
         GO TO 400
 
      ELSE
 
         CALL TOP1_RAND1(IR,VALUE,STATUS)
         U = VALUE
         GO TO 300

      END IF
 
      END
