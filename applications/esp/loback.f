      SUBROUTINE LOBACK( STATUS )
*+
*  Name:
*     LOBACK

*  Purpose:
*     Establishes the local mode values for parts of an image.
 
*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LOBACK( STATUS )

*  Arguments:   
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description: 
*     Establishes the local mode values for parts of an image
*     immediately surrounding a set of image co-ordinates supplied by 
*     the user. 
*
*     The user may also supply some indication of the number of pixels 
*     that must be used to create the pixel value histogram. This  
*     value may be supplied as the width of the square area around the 
*     co-ordinates that will be used, or alternatively the number of
*     contiguous data points believed to be present in the object at 
*     the image location specified. 
*
*     The latter method is intended specifically for use with RGASP's
*     IMAGES or IRAF's FOCAS output files.
*
*     All co-ordinates are read from a two or three columns
*     ASCII text file. If two columns are present then these are
*     taken as representing the image co-ordinates required for the 
*     regions of the image to be considered. If three columns are
*     present the third column respresents the width of the area 
*     to be sampled or the number of contiguous pixels detected there
*     by FOCAS or IMAGES.

*     The selection of the number of pixels to used in the histogram is 
*     defined by the user subject to a lower limit of 1024 pixels 
*     (32x32).
  
*  Usage:
*     LOBACK IN INFILE SFACT THIRD COSYS OUT WIDTH

*  ADAM Parameters:
*     COSYS = _CHAR (Read) 
*        Defines whether the co-ordinates are provided as data
*        (COSYS='D') or world (COSYS='W'). 
*     IN = _NDF (Read)
*        The name of the NDF data structure/file that is to be 
*        examined.
*     INFILE = _CHAR (Read)
*        The name of the ASCII text file containing the image 
*        co-ordinates and number of pixels to be used at each location 
*        or the number of contiguous pixels found there by FOCAS or 
*        IMAGES.
*     OUT = _CHAR (Read)
*        The file name in which the results are stored in text form.  
*     SFACT = _INTEGER (Read)
*        The Gaussian smoothing filter radius requested. This may be:
*        - -1 to indicate that the application should automatically
*          assign a filter radius to apply to the histogram.
*        - 0 to indicate that the histogram should not be smoothed.
*        - >0 to indicate the radius of the Gaussian filter to use. 
*        Values greater than LOB__SFLIM (see include file) are not 
*        allowed.
*        The value returned is that employed. Units counts.
*     THIRD = _LOGICAL (Read)
*        Determines whether or not the third column found in 
*        the text file specified using INFILE, contains the number
*        of contiguous image pixels believed to be at that image 
*        location, (THIRD = TRUE), or the number of screen pixels to be 
*        taken from the image around the required location 
*        (THIRD=FALSE).
*       
*         The value supplied via the third text column is used to 
*         determine how big an area of image must be used when
*         calculating the background. This is done by assuming
*         that all the pixels in the object are in a square
*         area and then determining the width of that area.
*         The width used in the end is 3 times that of the
*         object. 
* 
*        However, the WIDTH parameter 
*        overrides this if less than 32x32 pixels (as defined by the 
*        WIDTH parameter limits in the LOBACK.IFL file) are to be used. 
*        This ensures that the histogram employed is reasonably well 
*        filled under most circumstances.
*     WIDTH = _INTEGER (Read)
*        The width of the square area of the image from which pixel
*        values will be taken to construct the histogram. The minimum
*        permitted value is 32 (as defined in the LOBACK.IFL file). 
*        Units pixels.

*  Examples:
*     loback in=p2 infile=coords.dat sfact=0 third=true cosys=w 
*            out=backs.dat width=64
*
*        Reads the world co-ordinate data stored in text file COORDS
*        and determines the background count value within a 64x64 pixel 
*        area surrounding each of those locations. The histogram 
*        generated to do this will not be smoothed. The output will be 
*        into text file BACKS.DAT. Since THIRD is true, the third 
*        column represents the number of pixels thought to make up 
*        the object.
*
*     loback in=p2 infile=coords.dat sfact=4 third=false cosys=w 
*            out=output.dat width=35
*
*        Determines the background count value within a 35x35 pixel 
*        area surrounding each of the locations identified in 
*        COORDS.DAT. The histogram generated to do this will be 
*        smoothed using a Gaussian 4 counts wide. The output will be 
*        into text file OUTPUT.DAT. Since THIRD is false, the third 
*        column represent the lower limit of pixels to be taken from 
*        the image to make up the histogram. The co-ordinate system 
*        used is world. 

*  Notes:
*     The current version will not accept a pixel value range greater 
*     than the largest integer value possible. 
*
*     The user may easily abolish the 32x32 pixel filter lower size 
*     limit by modifying the WIDTH parameter entry in the LOBACK.IFL 
*     file. This action is only recommended for use with very flat 
*     images.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-Jun-1993 (GJP)
*     (Original version)

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'lob_par'               ! LOBACK system variables
                     
*  Status:     
      INTEGER STATUS                  ! Global status

*  Local Variables:
      CHARACTER *(256) COSYS          ! Character defining whether the 
                                      ! co-ordinates provided 
                                      ! are world or data.
      CHARACTER *(256) FTEXT          ! Formatting string
      CHARACTER *(256) TEXT           ! Output string
      LOGICAL EXCLAIM                 ! Was the file name used !?
      LOGICAL FILINP                  ! Was the input file name acceptable?
      LOGICAL THIRD                   ! Is the third column the number of 
                                      ! contiguous pixels?
      LOGICAL OPENF                   ! Was an output file opened?
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER FIOD                    ! Output file identifier
      INTEGER FIOID                   ! Input file identifier
      INTEGER I                       ! Temporary storage
      INTEGER J                       ! Temporary storage
      INTEGER LBND(7)                 ! Lower bounds for each image axis
      INTEGER NDF1                    ! NDF identifier
      INTEGER NGALS                   ! Number of regions of the image
                                      ! for which a background value must be
                                      ! calculated
      INTEGER NDIM                    ! Dimensionality of data
      INTEGER NPIX(LOB__NGALS)        ! Contents of the third column of the
                                      ! input text file defining either the
                                      ! desired number of pixels to be used
                                      ! (if possible) in the histogram or the
                                      ! number of contiguous pixels at that 
                                      ! location (see THIRD)
      INTEGER NUMP                    ! The size of the array in which the
                                      ! pixel values for the image subset 
                                      ! will be stored
      INTEGER NUMPS                   ! The number of pixel values taken from
                                      ! the image to be used. Differs from NUMP
                                      ! for locations near the image edge
      INTEGER NUMBER                  ! Number of points used in the 
                                      ! Calculation of mode
      INTEGER POINT0(10)              ! Pointer to NDF array to be used
      INTEGER POINT1(10)              ! Pointer to memory array to be used
      INTEGER PRANGE(2)               ! Number of pixels in the image x 
                                      ! and y axes 
      INTEGER SFACT                   ! Gaussian filter radius requested
      INTEGER UBND(7)                 ! Upper bounds for each image axis
      INTEGER WIDTH                   ! Width of the area of image 
                                      ! to be examined
      INTEGER WIDE                    ! Minimum width of the box from
                                      ! which pixels will be taken
      DOUBLE PRECISION MODE(4)        ! Estimates of the image mode value 
      DOUBLE PRECISION SDEV(2)        ! Estimate of the standard deviation
                                      ! of the modal value and its standard deviation
      REAL XC(LOB__NGALS)             ! X co-ordinate of galaxy/image location
      REAL YC(LOB__NGALS)             ! Y co-ordinate of galaxy/image location
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Show the user that the program is running.
      CALL MSG_BLANK(STATUS)
      CALL MSG_OUT(' ','ESP LOBACK running.',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Begin an NDF context.                               
      CALL NDF_BEGIN

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

*   Determine the input text file name. 
      CALL FIO_ASSOC('INFILE','READ','LIST',80,FIOID,STATUS)
      IF (STATUS.EQ.SAI__OK) FILINP=.TRUE.

*   Determine the smoothing filter radius required. SFACT=-1 is automatic,
*   SFACT=0 is none. Upper limit fixed by the LOB__SFLIM.
      CALL PAR_GET0I('SFACT',SFACT,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF (SFACT.GT.LOB__SFLIM) THEN
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','The value selected exceeded the maximum'/
     :                /' permitted.',STATUS)
         CALL MSG_OUT(' ','The maximum value has been employed.',
     :                STATUS)
         SFACT=LOB__SFLIM
      END IF

*   Determine what the third column of the ASCII text file is to be taken as
*   describing. True = number of contiguous pixels in an object. 
*   FALSE = the area of the image to be used.
      CALL PAR_GET0L('THIRD',THIRD,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999    

*   Minimum size for the width of the box to be used.
      CALL PAR_GET0I('WIDE',WIDE,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999    

*   Map the source NDF data array as _REAL values for reading.
      CALL NDF_MAP(NDF1,'Data','_REAL','READ',POINT0(1),ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
                      
*   Get the co-ordinate system mode and convert to upper case.
      CALL PAR_GET0C('COSYS',COSYS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      CALL CHR_UCASE(COSYS)

*   Obtain the co-ordinates of the image locations/galaxies required.
      CALL LOB1_FILER(FIOID,LBND,UBND,PRANGE,COSYS,
     :                NGALS,XC,YC,NPIX,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Loop round for all image locations provided.
      IF (NGALS.GT.0) THEN

*      Open a file and put in a heading.
         EXCLAIM=.FALSE.
         CALL LOB1_TEXTO(1,COSYS,NDF1,XC(1),YC(1),MODE,
     :                   SDEV,LBND,FIOD,OPENF,EXCLAIM,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999                         

*      Tell the user what is going on.
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','Considering co-ordinates.',STATUS)
         TEXT='    X         Y        Raw   Smoothed '//
     :        '  Proj.    Interp.    Sdev.    Sigma. '
         CALL MSG_OUT(' ',TEXT,STATUS)

         DO 10 I=1,NGALS

*         Calculate the width of the square part of the image about the 
*         the required location. Depends on the value of THIRD.
            IF (THIRD) THEN 

*            Number of contiguous pixels in the object mode.
*            Image area used is 9 times that of the known object.
               WIDTH=SQRT(REAL(NPIX(I)))*3.0

            ELSE

*            Number of pixels to be used mode.
               WIDTH=SQRT(REAL(NPIX(I)))

            END IF

*         Ensure that at least WIDExWIDE pixels are collected.
            IF (WIDTH.LT.WIDE) WIDTH=WIDE

*         Setup the memory necessary to store all the pixel values from the 
*         image.
            NUMP=WIDTH*WIDTH
            CALL PSX_CALLOC(NUMP,'_REAL',POINT1(1),STATUS)

*         Place the appropriate pixel values in the array from which the pixel
*         count histogram will be constructed.
         
            DO 1 J=1,4
               MODE(J)=0.0
 1          CONTINUE     
            SDEV(1)=0.0
            SDEV(2)=0.0
            CALL LOB1_FILL(ELEMS,%VAL(POINT0(1)),PRANGE,LBND,UBND,
     :                     XC(I),YC(I),NUMP,WIDTH,NUMPS,
     :                     %VAL(POINT1(1)),STATUS)

*         Call the modified version of HISTPEAK/HSUB. If it returns with
*         status set this is unset so that other data points can be
*         processed.
            CALL ERR_MARK    
            CALL LOB1_HIS(POINT1,NUMPS,MODE,SDEV,NUMBER,SFACT,STATUS)
            CALL ERR_RLSE
            IF (STATUS.NE.SAI__OK) CALL ERR_ANNUL(STATUS)
    
*         Place in the opened file background results  for these 
*         co-ordinates.
            IF (.NOT.EXCLAIM) THEN
               CALL LOB1_TEXTO(2,COSYS,NDF1,XC(I),YC(I),MODE,
     :                         SDEV,LBND,FIOD,OPENF,EXCLAIM,STATUS)
               IF (STATUS.NE.SAI__OK) GOTO 9999                         
            END IF

*         Create an appropriately formatted output string.
            IF (COSYS.EQ.'W') THEN

*            Original co-ordinates were in world form.
               CALL MSG_FMTR('X','F8.1',XC(I)-1+LBND(1))
               CALL MSG_FMTR('Y','F8.1',YC(I)-1+LBND(2))
 
            ELSE
 
*            Original co-ordinates were in pixel/data form.
               CALL MSG_FMTR('X','F8.1',XC(I))
               CALL MSG_FMTR('Y','F8.1',YC(I))

            END IF

*         Set up the formatting strings.

*         Mode values.
            FTEXT='F8.1'
            IF (ABS(MODE(1)).GT.9.9E8) FTEXT='E14.5'
            CALL MSG_FMTD('BACK1',FTEXT,MODE(1))

            FTEXT='F8.1'
            IF (ABS(MODE(2)).GT.9.9E8) FTEXT='E14.5'
            CALL MSG_FMTD('BACK2',FTEXT,MODE(2)) 

            FTEXT='F8.1'
            IF (ABS(MODE(3)).GT.9.9E8) FTEXT='E14.5'
            CALL MSG_FMTD('BACK3',FTEXT,MODE(3))

            FTEXT='F8.1'
            IF (ABS(MODE(4)).GT.9.9E8) FTEXT='E14.5'
            CALL MSG_FMTD('BACK4',FTEXT,MODE(4))

*         Standard deviation values.
            FTEXT='F8.1'
            IF (ABS(SDEV(1)).GT.9.9E6) FTEXT='E14.5' 
            CALL MSG_FMTD('SDEV1',FTEXT,SDEV(1))

            FTEXT='F8.1'
            IF (ABS(SDEV(2)).GT.9.9E6) FTEXT='E14.5'
            CALL MSG_FMTD('SDEV2',FTEXT,SDEV(2))

            TEXT='^X  ^Y  ^BACK1 ^BACK2 ^BACK3 ^BACK4 '//
     :           '^SDEV1 ^SDEV2'

*         Output the results in suitably formatted form.
            CALL MSG_OUT(' ',TEXT,STATUS)

*         Release the memory.
            CALL PSX_FREE(POINT1(1),STATUS)

 10      CONTINUE

*      Close the opened file.
         IF (.NOT.EXCLAIM) THEN
            CALL LOB1_TEXTO(3,COSYS,NDF1,XC(1),YC(1),MODE,
     :                      SDEV,LBND,FIOD,OPENF,EXCLAIM,STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 9999                         
         END IF

      END IF

 9999 CONTINUE

*   Close down the resources used.

*   Close the file input ASCII files.
      CALL FIO_ANNUL(FIOID,STATUS)

*   Un-map/annul the source NDF data array. 
      CALL NDF_UNMAP(NDF1,'Data',STATUS)
      CALL NDF_ANNUL(NDF1,STATUS)                          

*   End the NDF context.
      CALL NDF_END(STATUS)                              

      END 


      SUBROUTINE LOB1_FILL(ELEMS,ARRAY1,PRANGE,LBND,UBND,
     :                     X,Y,NUMP,WIDTH,NUMPS,ARRAY2,STATUS)
*+
*  Name:
*     LOB1_FILL

*  Purpose:
*     Given the co-ordinates of the region of the image in question, the
*     routine takes values from the pixels in a square of defined 
*     width about the chosen co-ordinates and places them in an array for 
*     subsequent use.      

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LOB1_FILL(ELEMS,ARRAY1,PRANGE,LBND,UBND,X,Y,NUMP,
*                    WIDTH,NUMPS,ARRAY2,STATUS)    

*  Description:
*     Transfers the required pixels to the memory array that will be used 
*     later to construct the pixel count histogram.

*  Arguments:              
*     ELEMS = INTEGER (Given)
*        Number of pixels in the source image.
*     ARRAY1(ELEMS) = REAL (Given)
*        The source image array. 
*     PRANGE(2) = INTEGER (Given)
*        Size of each image axis.
*     LBND(2) = INTEGER (Given)
*        Lower bound of the image.
*     UBND(2) = INTEGER (Given)
*        Upper bound of the image.
*     X = REAL (Given)
*        X co-ordinate to be used.
*     Y = REAL (Given)
*        Y co-ordinate to be used.
*     NUMP = INTEGER (Given)
*        Size of the array in which the pixel values may be stored.
*     WIDTH = INTEGER (Given)
*        Width of the square region of the image to be used.
*     NUMPS = INTEGER (Returned)
*        The number of pixels taken from the source image. Differs from NUMP
*        when the location requested is near the edge of the source image.
*     ARRAY2(NUMP) = REAL (Given and Returned)
*        The array into which values from the source image are placed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     29-JUN-1993 (GJP)
*     (Original version)

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:                              
      INTEGER ELEMS                   ! The number of pixels in the source
                                      ! image
      INTEGER LBND(2)                 ! Lower bounds of image axes 
      INTEGER NUMP                    ! Size of the array in which all the 
                                      ! pixel counts are to be placed
      INTEGER PRANGE(2)               ! Size of each image axis
      INTEGER UBND(2)                 ! Upper bounds of image axes
      INTEGER WIDTH                   ! The width of the square region of
      REAL ARRAY1(ELEMS)              ! The source image array
      REAL X                          ! X co-ordinate of the image position
      REAL Y                          ! Y co-ordinate of the image position

*  Arguments Returned:
      INTEGER NUMPS                   ! The number of pixel taken from the 
                                      ! source image 
      REAL ARRAY2(NUMP)               ! The pixel count values to be used to
                                      ! construct the histogram
*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER ADD                     ! Address of the image pixel required
      INTEGER ADDP                    ! Temporary storage
      INTEGER XLOW                    ! Lowest X value for the box
      INTEGER XHIGH                   ! Highest X value for the box
      INTEGER XV                      ! Loop variable
      INTEGER YLOW                    ! Lowest Y value for the box
      INTEGER YHIGH                   ! Highest Y value for the box
      INTEGER YV                      ! Loop variable
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Set up the values for the co-rdinates (in terms of data) of the
*   box surrounding the required image area.
      XLOW=INT(X)-WIDTH/2
      XHIGH=XLOW+WIDTH-1
      YLOW=INT(Y)-WIDTH/2
      YHIGH=YLOW+WIDTH-1

*   Adjust limits of the box to ensure the image edges are not exceeded.
      IF (XLOW.LT.1) XLOW=1  
      IF (XHIGH.GT.PRANGE(1)) XHIGH=PRANGE(1)
      IF (YLOW.LT.1) YLOW=1  
      IF (YHIGH.GT.PRANGE(2)) YHIGH=PRANGE(2)

*   Examine all pixels in that box and initialise the 
*   pixels stored counter.
      NUMPS=0
      DO 10 YV=YLOW,YHIGH

*      Calculate part of the pixel array address required.
         ADDP=(YV-1)*PRANGE(1)

         DO 20 XV=XLOW,XHIGH

*         Calculate the pixel address required.
            ADD=ADDP+XV

*         Increment the pixel counter and store the value found.        
            NUMPS=NUMPS+1
            ARRAY2(NUMPS)=ARRAY1(ADD) 
     
 20      CONTINUE

 10   CONTINUE

 9999 CONTINUE

      END


      SUBROUTINE LOB1_FILER(FIOID,LBND,UBND,PRANGE,COSYS,
     :                      NGALS,XC,YC,NPIX,STATUS)
*+
*  Name:
*     LOB1_FILER

*  Purpose:
*     Looks at each line of the required file in turn.
*     Ignores blank lines and those starting with # or ! since these
*     are assumed to be comments. Others it examines for the presence
*     of two numbers. 
*
*     These are taken as being x and y co-ordinates on an image and are
*     checked to ensure that they lie within the bounds of the image.
*     If it is found that the a co-ordinate pair is not within the 
*     bounds of the image, the values are not retained, otherwise the
*     counter is incremented and the values stored in arrays XC and YC.
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL LOB1_FILER(FIOID,LBND,UBND,PRANGE,COSYS,NGALS,XC,YC,NPIX,STATUS)    

*  Description:
*     Opens a user specified text file and reads from it a list of co-ordinates
*     indicating the locations where galaxies may exist in the image. Each of
*     these is to be profiled.
*
*     The co-ordinates obtained are returned in the arrays XC and YC. the
*     number of co-ordinate pairs defined is assigned to NGALS.

*  Arguments:               
*     FIOID = INTEGER (Given)
*        FIO identifier for the input file.
*     LBND(2) = INTEGER (Given)
*        Lower bound of the image.
*     UBND(2) = INTEGER (Given)
*        Upper bound of the image.
*     PRANGE(2) = INTEGER (Given)
*        Size of each image axis.
*     COSYS *(256) = CHARACTER (Given)
*        Character defining whether the co-ordinates provided 
*        are world or data format. 
*     NGALS = INTEGER (Returned)
*        Number of galaxies to be profiled.
*     XC(LOB__NGALS) = REAL (Returned)
*        X co-ordinates (for galaxies) obtained from the text file.
*     YC(LOB__NGALS) = REAL (Returned)
*        Y co-ordinates (for galaxies) obtained from the text file.
*     NPIX(LOB__NGALS) = INTEGER (Returned)
*        Number of contiguous pixels at the required location or the
*        number of pixels to be used in the histogram.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     29-APR-1993 (GJP)
*     (Original version)

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'FIO_ERR'               ! FIO error definitions
      INCLUDE 'lob_par'               ! LOBACK constants

*  Arguments Given:                              
      CHARACTER *(256) COSYS          ! Option choice defining how the
                                      ! pixel data format to be input
      INTEGER FIOID                   ! FIO identifier for the input file
      INTEGER LBND(2)                 ! Lower bounds of image axes 
      INTEGER PRANGE(2)               ! Size of each image axis
      INTEGER UBND(2)                 ! Upper bounds of image axes

*  Arguments returned:
      INTEGER NGALS                   ! The number of galaxies to be profiled
      INTEGER NPIX(LOB__NGALS)        ! The number of contiguous pixels at a 
                                      ! given location or the number of pixels
                                      ! to be used
      REAL XC(LOB__NGALS)             ! X co-ordinates of the galaxy positions
                                      ! found from the file
      REAL YC(LOB__NGALS)             ! Y co-ordinates of the galaxy positions
                                      ! found from the file

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:
      LOGICAL ABORT                   ! Has the maximum permitted number of
                                      ! galaxies been exceeded?
      LOGICAL FAIL
      CHARACTER *(80) BUFFER          ! Character string input from the file
      CHARACTER *(80) STRING          ! Part of the input string
      INTEGER I                       ! A loop counter
      INTEGER INDEXE                  ! End of a word in the buffer string
      INTEGER INDEXS                  ! Start of a word in the buffer string
      INTEGER LINE                    ! Line counter
      INTEGER NCHAR                   ! Number of characters
      REAL VALUE                      ! Temporary storage
      REAL XTEMP                      ! Temporary storage
      REAL YTEMP                      ! Temporary storage
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Initialise the galaxy counter
      NGALS=0

*   Start an error context.   
      CALL ERR_MARK

*   Read from a file. Stop if the end of file is reached or if the maximum
*   permitted number of galaxies is exceeded.

      ABORT=.FALSE.
      LINE=0
      DO WHILE ((STATUS.NE.FIO__EOF).AND.(.NOT.ABORT))
          
*       Read a line from the steering file.

         CALL FIO_READ(FIOID,BUFFER,NCHAR,STATUS)
         IF (STATUS.EQ.SAI__OK) THEN

*       Parse the buffer read from the file.

*         Check for comment (lines starting # or !) or blank line.
            LINE=LINE+1
            STRING=BUFFER(1:1)
            IF ((BUFFER.NE.' ').AND.(STRING.NE.'#').AND.
     :                 (STRING.NE.'!')) THEN

*             Find the x and y co-ordinates by looking for words in the BUFFER.
               FAIL=.FALSE.
               INDEXE=-1
               XTEMP=-1E10
               YTEMP=-1E10                             

               DO 10 I = 1,3

*               Identify the start and end indices of the words in the buffer.
*               If either fails there are not enough words in the buffer.

*               Start a new error context.
                  CALL ERR_MARK

*                  Look for the words.
                     INDEXS = INDEXE + 1
                     CALL CHR_FIWS(BUFFER,INDEXS,STATUS)
                     INDEXE = INDEXS
                     CALL CHR_FIWE(BUFFER,INDEXE,STATUS)

*                  Set the fail flag if the word extraction failed.
                     IF (STATUS.NE.SAI__OK) THEN
                        FAIL=.TRUE.
                        CALL ERR_ANNUL(STATUS)
                        CALL MSG_FMTI('LINE','I5',LINE)
                        CALL MSG_OUT(' ',
     :                   'A word was missing from line ^LINE ',
     :                   STATUS)
                     END IF

*               End error context.
                  CALL ERR_RLSE

*               Try to extract a number from the word if a word was
*               successfully obtained.
                  IF (.NOT.FAIL) THEN

*                  Start an new error context.
                     CALL ERR_MARK
                        
*                  Check each word to check that it is a number.
                     STRING=BUFFER(INDEXS:INDEXE)
                     CALL CHR_CTOR(STRING,VALUE,STATUS)

                     IF (STATUS.NE.SAI__OK) THEN
                        FAIL=.TRUE.
                        CALL ERR_ANNUL(STATUS)
                        CALL MSG_FMTI('LINE','I5',LINE)
                        CALL MSG_OUT(' ',
     :                   'A word was not a number in line ^LINE ',
     :                   STATUS)
                     ELSE

*                     Check that it is not the third value from the line.
                        IF (I.NE.3) THEN

*                        Check the value is within allowed range.
                           IF (COSYS.EQ.'W') THEN

*                           Check that the co-ordinate value input is legal.
                              IF ((VALUE.GE.LBND(I)).AND.
     :                               (VALUE.LE.UBND(I))) THEN

*                              Value within range so assign.
                                 VALUE=VALUE-LBND(I)+1
                                 IF (I.EQ.1) XTEMP=VALUE
                                 IF (I.EQ.2) YTEMP=VALUE

                              ELSE
                                 
*                              Set the fail flag since the point selected 
*                              is not on the image.
                                 CALL MSG_FMTI('LINE','I5',LINE)
                                 CALL MSG_OUT(' ',
     :                           'Co-ordinates not within the image'//
     :                           ' on line ^LINE ',STATUS)
                                 FAIL=.TRUE.
                    
                              END IF
                         
                           ELSE

*                           Check that the co-ordinate value input is legal.
                              IF ((VALUE.GE.1.0).AND.
     :                            (VALUE.LE.PRANGE(I))) THEN

*                              Value within range so assign.
                                 IF (I.EQ.1) XTEMP=VALUE
                                 IF (I.EQ.2) YTEMP=VALUE

                              ELSE

*                              Set the fail flag since the point selected 
*                              is not on the image.
                                 CALL MSG_FMTI('LINE','I5',LINE)
                                 CALL MSG_OUT(' ',
     :                           'Co-ordinates not within the image'//
     :                           ' on line ^LINE ',STATUS)
                                 FAIL=.TRUE.
                                 
                              END IF

                           END IF

                        END IF

*                     Assign the values to the arrays and increment the
*                     counter.
                        IF (I.EQ.3) THEN                          
                           NGALS=NGALS+1
                           XC(NGALS)=XTEMP
                           YC(NGALS)=YTEMP
                           IF (.NOT.FAIL) THEN
                              NPIX(NGALS)=INT(VALUE)
                           ELSE
                              NPIX(NGALS)=1
                              CALL MSG_OUT(' ',
     :                          'Dummy third column used.',STATUS)
                           END IF     
                        END IF

*                     Stop any further points being taken from the file.
                        IF (NGALS.EQ.LOB__NGALS) THEN
                           ABORT=.TRUE.  
                           FAIL=.TRUE.
                        END IF

                     END IF

*                  End the current error context.
                     CALL ERR_RLSE

                  ELSE

*                  Cope with a duff third column.
                     IF ((I.EQ.3).AND.
     :                  (XTEMP.GT.-1E9).AND.(YTEMP.GT.-1E9)) THEN  
                           NGALS=NGALS+1
                           XC(NGALS)=XTEMP
                           YC(NGALS)=YTEMP
                           NPIX(NGALS)=1
                           CALL MSG_OUT(' ',
     :                          'Dummy third column used.',STATUS)

*                     Stop any further points being taken from the file.
                        IF (NGALS.EQ.LOB__NGALS) THEN
                           ABORT=.TRUE.  
                           FAIL=.TRUE.
                        END IF
     
                     END IF

                  END IF
  
 10            CONTINUE

            END IF

         END IF

      END DO
     
*   Display the error message if necessary. Also, tidy up the error system.
      IF ((STATUS.NE.SAI__OK).AND.(STATUS.NE.FIO__EOF)) THEN
         CALL ERR_REP( ' ','Errors found when reading the data file.',
     :                STATUS)
         CALL ERR_FLUSH( STATUS )
      ELSE
         CALL ERR_ANNUL( STATUS )
      END IF
          
*   End the error context.
      CALL ERR_RLSE

*   Indicate that the file was flawed.
      IF (FAIL) THEN
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','Problems found reading the file.',
     :                STATUS)
         CALL MSG_BLANK(STATUS)
      END IF

*   Indicate if the maximum permitted number of galaxies was exceeded.
      IF (ABORT) THEN
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','Too many co-ordinate pairs were found.',
     :                STATUS)
         CALL MSG_OUT(' ','Proceeding with the maximum number'/
     :                /' allowed.',STATUS)
         CALL MSG_BLANK(STATUS)
      END IF

 9999 CONTINUE

      END 


      SUBROUTINE LOB1_HIS(POINT1,ELEMS,MODE,SDEV,
     :                    NUMBER,SFACT,STATUS)
*+                        
*  Name:
*     LOB1_HIS

*  Purpose:
*     Establish the mean, mode, median and other statistics 
*     for NDF image files.  

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL LOB1_HIS(POINT1,ELEMS,MODE,SDEV,NUMBER,SFACT,STATUS)
                    
*  Arguments:   
*     POINT1(10) = INTEGER (Given)
*        Pointer to the NDF data array.
*     ELEMS = INTEGER (Given)
*        Total number of image pixels.
*     MODE(4) = DOUBLE PRECISION (Returned)
*        The modal count value estimates. Units counts.
*     SDEV(2) = DOUBLE PRECISION (Returned)
*        The standard deviation and standard deviation values. Units counts.
*     NUMBER = INTEGER (Returned)
*        The number of non-bad pixels used. Units pixels.
*     SFACT = INTEGER (Given and Returned)
*        The radius of the Gaussian filter requested and used.
*        0 means no filtering. -1 is automatic. Otherwise units pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description: 
*     Allows the user to input the name of an NDF image file and 
*     then constructs an image count value versus occurence histogram.
*     This is used to allow count median, mode and standard 
*     deviation values to be estimated. 
*
*     It is assumed that all ARD file manipulation has already been 
*     carried out by the calling subroutine.
*
*     Bad valued points are excluded from the calculations.
*
*     Four estimates of the modal value are generated:
*     - unsmoothed mode.
*     - smoothed mode.
*     - projected mode. Calculated by extrapolating the lengths of a 
*       series of chords through the peak to zero length and determining
*       the count value at which this occurs.
*     - interpolated mode. Calculated by assuming a Normal form 
*       for the histogram peak and 'fitting' a function to it.
*       The function is then used to provide both a modal value and
*       the distribution standard deviation.  
*    
*     Two estimates of the standard deviation of pixel count values
*     are generated:
*     - simple standard deviation.
*     - the standard deviation of the region of the histogram near 
*       to the modal count value. This estimate reduces the influence of
*       outlier points produced by brighter sources.
*
*     All values of MODE are returned to the calling subroutine.
     

*  Implementation Status:
*     The current version will not accept a pixel value range greater 
*     than the largest integer value possible. This will be corrected 
*     in a later version.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JUN-1993 (GJP)
*     (Original version based on HISTPEAK)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'lob_par'               ! LOBACK system variables
                     
*  Status:     
      INTEGER STATUS                  ! Global status

*  Arguments Given:   
      INTEGER POINT1(10)              ! Pointer to the NDF data array.
      INTEGER ELEMS                   ! Total number of image pixels.

*  Arguments Given and Returned:
      INTEGER SFACT                   ! The radius of the Gaussian filter 
                                      ! requested and used.
    
*  Arguments Returned:
      INTEGER NUMBER                  ! Number of non-bad pixels used.
      DOUBLE PRECISION MODE(4)        ! Modal count value estimates. 
      DOUBLE PRECISION SDEV(2)        ! Standard deviation 
                                      ! and standard deviation values

*  Local Variables:      
      INTEGER BARSIZ                  ! Size of the of the bin arrays
                                      ! used
      INTEGER POINT2(10)              ! Pointer to the memory allocated 
                                      ! for the unsmoothed histogram
      INTEGER POINT3(10)              ! Pointer to the memory allocated
                                      ! for the smoothed histogram
      INTEGER SFACTA                  ! The actual filter radius used
      INTEGER UNUPIX                  ! Number of unused pixels in the
                                      ! source NDF
      DOUBLE PRECISION ADEV           ! Absolute deviation of the 
                                      ! NDF pixels used
      REAL BINWID                     ! Bin width used when finding the
                                      ! median and mode values (not
                                      ! 1 when count range > LOB__BINLI)
                                      ! Units counts
      REAL HIGH                       ! Highest pixel value found in the
                                      ! NDF data
      REAL LOW                        ! Binning arrays origin offset and
                                      ! lowest value found in image
      DOUBLE PRECISION MEAN           ! Mean pixel value in the NDF data
      DOUBLE PRECISION MEDIAN         ! Median value of the NDF pixels
      DOUBLE PRECISION PEAKV(3)       ! Estimates of the histogram
                                      ! array peak height
      DOUBLE PRECISION VARI           ! Variance of the NDF pixel values   
                                                          
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Set default values for absolute deviation etc (see above). 
      ADEV=0.0                      
      UNUPIX=0
      HIGH=0.0                      
      LOW=0.0                       
      MEAN=0.0                      
      MEDIAN=0.0                                 
      NUMBER=0 
      PEAKV(1)=0.0
      PEAKV(2)=0.0
      PEAKV(3)=0.0
      SFACTA=0
      VARI=0.0                 

*   Call routine to find the highest, lowest and mean
*   value of those in the data array.
      CALL LOB1_HILOA(ELEMS,%VAL(POINT1(1)),
     :                UNUPIX,HIGH,MEAN,LOW,NUMBER,STATUS)               
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine the size of the array to be used for binning the image 
*   pixel count values. The bin width is also set. The size of the 
*   binning array is not allowed to exceed LOB__BINLI.

      IF (HIGH-LOW+1.GT.LOB__BINLI) THEN
         BINWID=(HIGH-LOW+1.)/REAL(LOB__BINLI)
         BINWID=BINWID*1.01
         BARSIZ=LOB__BINLI
      ELSE
         BINWID=1.0
         BARSIZ=HIGH-LOW+1
         IF (BARSIZ.LT.1) BARSIZ=1
      END IF

*   Allocate the memory needed for the histogram and smoothed
*   histogram arrays.
      CALL PSX_CALLOC(BARSIZ,'_DOUBLE',POINT2(1),STATUS)
      CALL PSX_CALLOC(BARSIZ,'_DOUBLE',POINT3(1),STATUS)
      IF (STATUS.NE.SAI__OK) THEN  
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The dynamic arrays have not been allocated.'
     :                    ,STATUS)
         GOTO 9999
      END IF

*   Call routine to find moments of deviation from the mean for
*   the NDF data array.
      CALL LOB1_MOMDE(ELEMS,NUMBER,%VAL(POINT1(1)),
     :                MEAN,ADEV,VARI,SDEV,STATUS) 
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Call routine to find the median and mode of the values in
*   the NDF array data.
      CALL LOB1_MEDMO(ELEMS,%VAL(POINT1(1)),POINT2,POINT3,BARSIZ,
     :                BINWID,LOW,ADEV,SFACT,
     :                NUMBER,SDEV,%VAL(POINT2(1)),
     :                %VAL(POINT3(1)),MEDIAN,PEAKV,SFACTA,MODE,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999  
                                           
*   Return to the calling routine..
 9999 CONTINUE

*   Free the dynamic array space of the histogram arrays.
      CALL PSX_FREE(POINT2(1),STATUS)
      CALL PSX_FREE(POINT3(1),STATUS)                           

      END
      

      SUBROUTINE LOB1_CHORD(HIVAL,LOVAL,MODEC,SMOBAR,LOW,ADEV,
     :                      BARSIZ,BINWID,NUMDAT,HEIG,X1,Y1,STATUS)
*+                        
*  Name:
*     LOB1_CHORD

*  Purpose:
*     Estimate histogram mode by examining chords through peak. 
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LOB_CHORD(HIVAL,LOVAL,MODEC,SMOBAR,LOW,ADEV,
*                     BARSIZ,BINWID,NUMDAT,HEIG,X1,Y1,STATUS)
    
*  Description:
*     Determines the length of chords through the histogram 
*     peak at a variety of percentages of histogram heights.

*  Arguments:                                     
*     HIVAL = DOUBLE PRECISION (Given)
*        Highest value found in the smoothed bin array.
*     LOVAL = DOUBLE PRECISION (Given)
*        Lowest value found in the smoothed bin array.
*     MODEC = INTEGER (Given)
*        Index of highest value in smoothed bin array.
*     SMOBAR(BARSIZ) = DOUBLE PRECISION (Given)
*        Smoothed bin array of image pixel counts.
*     LOW = REAL (Given)
*        Lowest count value in image. Used as an array index offset
*        for the SMOBAR and BARRAY arrays. Units counts.
*     ADEV = DOUBLE PRECISION (Given)
*        Absolute deviation of pixel values in the image. Units counts.
*     BARSIZ = INTEGER (Given)
*        Size (no. elements) of the binning arrays used.     
*     BINWID = REAL (Given)
*        Width of each bin in the bin arrays. Units counts.
*     NUMDAT = INTEGER (Returned)
*        The number of legal histogram chords obtained.
*     HEIG(LOB__CHORM) = REAL (Returned)
*        The height at which the chord through the histogram occurs.
*     X1(LOB__CHORM) = REAL (Returned)
*        Length of chord through the histogram.      
*     Y1(LOB__CHORM) = REAL (Returned)
*        Midpoint x index of chords through the histogram.
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
      INCLUDE 'lob_par'               ! LOBACK system variables
                                                                      
*  Arguments Given:
      INTEGER BARSIZ                  ! Size of the binning arrays
      INTEGER MODEC                   ! Bin array index corresponding to
                                      ! the element containing HIVAL
      REAL BINWID                     ! Width of the bins used to find 
                                      ! median and mode (only differs 
                                      ! from 1 when the count range
                                      ! exceeds BINSIZ)
      REAL LOW                        ! Binning arrays origin offset and
                                      ! lowest value found in image
      DOUBLE PRECISION ADEV           ! Absolute deviation of image
                                      ! pixels
      DOUBLE PRECISION HIVAL          ! Highest value in smoothed bin
                                      ! array
      DOUBLE PRECISION LOVAL          ! Lowest value in smoothed bin
                                      ! array
      DOUBLE PRECISION SMOBAR(BARSIZ) ! Smoothed bin array      

*  Arguments Returned:
      INTEGER NUMDAT                  ! Number of sections through
                                      ! histogram found
      REAL HEIG(LOB__CHORM)           ! The histogram values at which 
                                      ! chords were taken through the
                                      ! histogram                  
      REAL X1(LOB__CHORM)             ! Length of the chord through
                                      ! the histogram
      REAL Y1(LOB__CHORM)             ! X index of midpoint of chord
                                      ! through the histogram

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:                                                   
      INTEGER I                       ! Temporary loop variable
      INTEGER J                       ! Temporary loop variable
      INTEGER N1                      ! The number of possible values
                                      ! that were found for the index of 
                                      ! one end of the chord through the
                                      ! histogram
      INTEGER N2                      ! Same as for N1 but the other end
      INTEGER S1                      ! The sign of the difference between 
                                      ! the value of the previous histogram
                                      ! element and the value at which
                                      ! the chord is being taken
      INTEGER S2                      ! The sign of the difference between 
                                      ! the value of the next histogram
                                      ! element and the value at which
                                      ! the chord is being taken
      INTEGER SLICE                   ! Temporary loop variable
      REAL AV1                        ! Average value for the chord 
                                      ! (slice) start index on the left
                                      ! hand side of the histogram
      REAL AV2                        ! Same as AV1 but right hand side
      REAL HEIGHT                     ! Value at which the current chord
                                      ! (slice) through the histogram
                                      ! is taken
      REAL VALUE                      ! Temporary value          

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Find the width of the smoothed histogram over a range of fractions 
*   of the histogram mode value. This eventually provides a further 
*   estimate for the location of the mode.
      NUMDAT=0
      DO 500 SLICE=2,38,2

*      Define the height at which the chord is taken as a decreasing  
*      value somewhere between the highest value found in the histogram 
*      and the lowest. The very top and bottom of the histogram are 
*      excluded.
         HEIGHT=NINT((1.-REAL(SLICE)/100.)*(HIVAL-LOVAL)+LOVAL)
                
*      Search for the points on the left hand side of the histogram
*      peak where two histogram values are either side of the required
*      slice height.
         AV1=0.0
         N1=0
         J=2
         IF (J.LT.MODEC-ADEV) J=MODEC-ADEV
         DO 410 I=J,MODEC-1
          
*         Establish whether the smoothed histogram elements at index 
*         I-1 and I+1 are bigger or smaller than the required 
*         slice value and exclude them if they are zero.

            VALUE=SMOBAR(I-1)
            S1=INT(SIGN(1.,VALUE-HEIGHT))  
            VALUE=SMOBAR(I+1)
            S2=INT(SIGN(1.,VALUE-HEIGHT))

*         If the elements at I-1 and I+1 are on either side of  
*         the required slice value then store the index I.
*         If the histogram is noisy the final value will be an
*         average value.            
            IF (S1*S2.EQ.-1) THEN
               N1=N1+1        
               AV1=AV1+REAL(I)
            END IF
 410     CONTINUE

*      Search for the points on the right hand side of the histogram
*      peak where two histogram values are either side of the required
*      slice height.
         AV2=0.0
         N2=0
         J=BARSIZ-1
         IF (J.GT.MODEC+ADEV) J=MODEC+ADEV
         DO 420 I=MODEC+1,J

*         Establish whether the smoothed histogram elements at index 
*         I-1 and I+1 are bigger or smaller than the required 
*         slice value and exclude them if they are zero.

            VALUE=SMOBAR(I-1)
            S1=INT(SIGN(1.,VALUE-HEIGHT))  
            VALUE=SMOBAR(I+1)
            S2=INT(SIGN(1.,VALUE-HEIGHT))

*         If the elements at I-1 and I+1 are on either side of  
*         the required slice value then store the index I.
*         If the histogram is noisy the final value will be an
*         average value.            
            IF (S1*S2.EQ.-1) THEN
               N2=N2+1
               AV2=AV2+REAL(I)
            END IF
 420     CONTINUE

*      Check to see if a legal (two ended) slice through the histogram
*      was found at the current height value.
      
         IF ((N1.GT.0).AND.(N2.GT.0).AND.
     :        (AV2/REAL(N2)-AV1/REAL(N1).GT.1.)) THEN

*         Use the current slice through the histogram if both ends 
*         were found and are not adjacent.
                                           
*         Modify useful data points counter and store the histogram
*         height at which the slice was taken.

            NUMDAT=NUMDAT+1
            HEIG(NUMDAT)=HEIGHT
                
*         Store values of histogram width at various fractions of the
*         histogram mode count and also the approximate histogram centre
*         point at each width.
            X1(NUMDAT)=SQRT((AV2/REAL(N2)-AV1/REAL(N1))/2.*BINWID)   
            Y1(NUMDAT)=LOW+((AV2/REAL(N2)+AV1/REAL(N1))/2.-1.)*BINWID

         END IF
          
 500  CONTINUE

*   Check that there are two or more data points.
      IF (NUMDAT.LT.2) THEN
        CALL MSG_OUT(' ','WARNING!!!',STATUS)
        CALL MSG_OUT(' ','There are less than two successful'//
     :                   ' slices through the histogram peak.'//
     :                   ' Consequently, the projected mean cannot'//
     :                   ' be found!',STATUS)
        GOTO 9999
      END IF

 9999 CONTINUE

      END


      SUBROUTINE LOB1_GAUJO(VECTOR,INPMAT,DETERM,STATUS)
*+
*  Name:
*     LOB1_GAUJO

*  Purpose:                                                          
*     Inverts a matrix containing preprocessed histogram values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LOB1_GAUJO(B,A,DETERM,STATUS)

*  Description:
*     Employs the very stable Gauss-Jordan with optimised 
*     array pivot elements method to invert a matrix. The matrix to be
*     inverted (INPMAT) is received in a form partially preprocessed.
*
*     On completion, the array INPMAT contains the inverted matrix and 
*     the vector array VECTOR contains the coefficients of the 
*     parabolic equation. 
* 
*     If the routine suceeds, the determinant (DETERM) of the array 
*     is significantly non-zero.

*  Arguments:
*     DETERM = REAL (Returned)
*        The determinant of the inverted array.
*     INPMAT(3,3) = REAL (Given and Returned)
*        The matrix to be inverted. The inverted matrix is returned.
*     VECTOR(3) = REAL ARRAY (Given and Returned)
*        Preprocessed count values are given. Values for the parabola
*        coefficients are returned.
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
               
*  Type Definitions:                   ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'lob_par'               ! LOBACK system variables

*  Arguments Given and Returned:
      REAL INPMAT(3,3)                ! Matrix to be inverted

*  Arguments Returned:
      REAL VECTOR(3)                  ! Results vector
      REAL DETERM                     ! The inverted matrix determinant      

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local Variables:                                                          
      INTEGER I                       ! Loop variable
      INTEGER COL                     ! Matrix column index
      INTEGER INDEX(2,3)              ! Row and column look-up table
      INTEGER ROW                     ! Matrix row index
      INTEGER J                       ! Loop variable
      INTEGER K                       ! Loop variable
      INTEGER L                       ! Number of coefficients required
      INTEGER N                       ! Size of matrix to be inverted    
      LOGICAL LPIVOT(3)               ! Has column been pivoted flag
      REAL PIVOT                      ! The pivot element
      REAL TEMP                       ! Temporary variable
    
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN
      
*   Set up the number of coefficients and size of the matrix to be
*   inverted. 3 given that a parabola is being considered.
      L=3
      N=3

*   Set up the initial determinant value and set the pivot flags to
*   their initial values.
      DETERM=1.0
      DO I=1,N
         LPIVOT(I)=.FALSE.
      END DO

      DO I=1,N
         PIVOT=0.0

*   Search for the pivot element.

         DO J=1,N
            IF (.NOT.LPIVOT(J)) THEN
               DO K=1,N
                  IF (.NOT.LPIVOT(K)) THEN
                     IF (ABS(PIVOT).LT.ABS(INPMAT(K,J))) THEN
                        PIVOT=INPMAT(K,J)
                        ROW=J
                        COL=K
                     END IF
                  END IF
               END DO
            END IF
         END DO

*      Calculate the determinant and exit if the value is zero ie
*      a singular matrix.
         DETERM=DETERM*PIVOT
         IF (DETERM.LT.LOB__VSMAL) THEN   
            DETERM=0.0
            CALL MSG_OUT(' ','WARNINGdir!!!',STATUS)
            CALL MSG_OUT(' ','Unable to complete the parabolic '//
     :                       'interpolation.',STATUS)
            GOTO 9999
         END IF

         LPIVOT(COL)=.TRUE.
     
         INDEX(1,I)=ROW
         INDEX(2,I)=COL

*   Interchange rows so that the pivot element is now on the diagonal.

         IF (ROW.NE.COL) THEN
            DETERM=-DETERM
            DO J=1,N
               TEMP=INPMAT(J,ROW)
               INPMAT(J,ROW)=INPMAT(J,COL)
               INPMAT(J,COL)=TEMP
            END DO  
            TEMP=VECTOR(ROW)
            VECTOR(ROW)=VECTOR(COL)
            VECTOR(COL)=TEMP
         END IF

*   Divide the pivot row by the pivot element.

         INPMAT(COL,COL)=1.0
         DO J=1,N
            INPMAT(J,COL)=INPMAT(J,COL)/PIVOT
         END DO
         VECTOR(COL)=VECTOR(COL)/PIVOT

*   Subtract the pivot row values from the other rows.

         DO J=1,N
            IF (J.NE.COL) THEN
               TEMP=INPMAT(COL,J)
               INPMAT(COL,J)=0.0
               DO K=1,N
                  INPMAT(K,J)=INPMAT(K,J)-INPMAT(K,COL)*TEMP
               END DO
               VECTOR(J)=VECTOR(J)-VECTOR(COL)*TEMP
            END IF
         END DO
      END DO

*   Interchange the columns to recover the solution coefficients.

      DO I=N,1,-1
         IF (INDEX(1,I).NE.INDEX(2,I)) THEN
            ROW=INDEX(1,I)
            COL=INDEX(2,I)
            DO J=1,N
               TEMP=INPMAT(ROW,J)
               INPMAT(ROW,J)=INPMAT(COL,J)
               INPMAT(COL,J)=TEMP
            END DO
         END IF
      END DO
      
*   Exit if the parabola is up the wrong way.
      IF (VECTOR(3).GE.0.0) THEN
         DETERM=0.0
         GOTO 9999
      END IF

 9999 CONTINUE
      
      END



      SUBROUTINE LOB1_HILOA(ELEMS,ARRAY,
     :                      UNUPIX,HIGH,MEAN,LOW,NUMBER,STATUS)               
*+
*  Name:
*     LOB1_HILOA

*  Purpose: 
*     Find the highest and lowest count values in an image array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LOB1_HILOA(ELEMS,ARRAY,
*                     UNUPIX,HIGH,MEAN,LOW,NUMBER,STATUS)               

*  Description:
*     Establish the highest and lowest count values found in the image
*     ARRAY. The mean value is found also as is the number of pixels 
*     that are bad.

*  Arguments:                                     
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image. Units pixels.
*     ARRAY(ELEMS) = INTEGER (Given)
*        An array containing the image pixel counts.
*     UNUPIX = INTEGER (Returned)
*        The number of unused pixels in the image. Units pixels.
*     HIGH = REAL (Returned) 
*        The highest count value found in the image pixels. Units counts.
*     MEAN = DOUBLE PRECISION (Returned)
*        Mean of the values found in the image pixels. Units counts.           
*     LOW = REAL (Returned)
*        Lowest count value in the image. Also used as an array
*        offset index for the arrays SMOBAR and BARRAY. Units counts.
*     NUMBER = INTEGER (Returned)
*        The number of pixels non-bad. Units pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     15-May-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
                                                                      
*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the image
      REAL ARRAY( ELEMS )             ! Image pixel counts  

*  Arguments Returned:
      INTEGER NUMBER                  ! The number of pixels non-bad
      INTEGER UNUPIX                  ! Number of unused pixels in the data
      REAL HIGH                       ! Highest value in the array
      REAL LOW                        ! Binning arrays origin offset and
                                      ! lowest value found in image
      DOUBLE PRECISION MEAN           ! Average value in the array 

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables: 
      INTEGER I                       ! Loop variable              
      REAL VALUE                      ! Value of current array element 
      REAL UNUPXR                     ! Number of bad pixels (Real)
      DOUBLE PRECISION SUM            ! Sum of all non-bad pixels in
                                      ! the data array
                      
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN
   
*   Look through the array in search of highest, lowest values and
*   also take the sum (which allows the mean to be calculated) and count
*   the number of unused pixels in the image. 
      MEAN=0.0
      HIGH=-VAL__MAXR
      LOW=VAL__MAXR
      UNUPIX=0
      UNUPXR=0.0
      SUM= 0.0

      DO 112 I=1,ELEMS
         VALUE=ARRAY(I)
         IF (VALUE.NE.VAL__BADR) THEN 
            SUM=SUM+VALUE
            IF (VALUE.GT.HIGH) HIGH=VALUE            
            IF (VALUE.LT.LOW) LOW=VALUE
         ELSE
            UNUPXR=UNUPXR+1.0
         END IF
 112  CONTINUE 

*   Check that the range of pixel values is too big for the
*   current software version.
      IF (REAL(HIGH)-REAL(LOW).GT.REAL(VAL__MAXI)-2.) THEN 
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','Pixel range too large to handle.',STATUS)
         STATUS=SAI__ERROR
         GOTO 9999
      END IF

*   Check that there are not too many bad pixels.
      IF (UNUPXR.GE.REAL(VAL__MAXI)-2.) THEN 
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','Too many data points are bad.',STATUS)
         STATUS=SAI__ERROR
         GOTO 9999
      END IF

      UNUPIX=NINT(UNUPXR)
      NUMBER=ELEMS-UNUPIX

*   Check that enough data points are available.
      IF (NUMBER.LT.3) THEN
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','Too few data points have been '//
     :                    'specified.',STATUS)
         STATUS=SAI__ERROR
         GOTO 9999
      END IF

*   Calculate the average data array value.
      IF (NUMBER.GT.0) MEAN=SUM/REAL(NUMBER)         

*   Check the number of unused data points present in the array and
*   set STATUS if less than 3 data points are present.
      IF (NUMBER.LT.3) THEN
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','Too few points are present for an accurate'//
     :                    ' estimate of the mode to be made.',STATUS)
         STATUS=SAI__ERROR
         GOTO 9999
      END IF

*   Check if the range of values found is less than 3 and
*   set STATUS if true. 
      IF (HIGH-LOW+1.0.LT.3.0) THEN
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','The range of values found is less than 3!'//
     :                    ' Consequently, accurate modes are not'//
     :                    ' available.',STATUS)
         STATUS=SAI__ERROR
         GOTO 9999
      END IF
  
 9999 CONTINUE

      END


      SUBROUTINE LOB1_MEDMO(ELEMS,ARRAY,POINT2,POINT3,BARSIZ,
     :                      BINWID,LOW,ADEV,SFACT,
     :                      NUMBER,SDEV,BARRAY,SMOBAR,
     :                      MEDIAN,PEAKV,SFACTA,MODE,STATUS)
*+
*  Name:
*     LOB1_MEDMO

*  Purpose:
*     Creates a histogram array from the image array and finds its
*     mode.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LOB1_MEDMO(ELEMS,ARRAY,POINT2,POINT3,BARSIZ,
*                     BINWID,LOW,ADEV,SFACT,
*                     NUMBER,SDEV,BARRAY,SMOBAR,
*                     MEDIAN,PEAKV,SFACTA,MODE,STATUS)
                                   
*  Description:
*     Places the values from the mapped image array into a binning
*     array. The array is used as a histogram. The routine then
*     determines values for the peak height, mode, median and standard
*     deviation of the histogram.

*  Arguments:              
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image. Units pixels.
*     ARRAY(ELEMS)= REAL (Given)
*        Array containing the image data.
*     POINT2(1) = INTEGER (Given)
*        Memory pointer to the binning array.
*     POINT3(1) = INTEGER (Given)
*        Memory pointer to the smoothed binning array.
*     BARSIZ = INTEGER (Given)
*        Size (no. of elements) of the binning arrays used.
*     BINWID = REAL (Given)
*        The width (range of values) of pixel count that are stored
*        within each element of the binning histogram array. Units counts.
*     LOW = REAL (Given)
*        Lowest count value in the image. Also used as an array
*        offset index for the arrays SMOBAR and BARRAY. Units counts.
*     ADEV = DOUBLE PRECISION (Given)
*        Absolute deviation of the image pixels count distribution.
*        Units counts.
*     SFACT = INTEGER (Given)
*        Requested Gaussian filter radius. -1 if the filter radius
*        is to be selected for you, 0 if no smoothing is to be done
*        otherwise any number less than LOB__SFLIM (see include file)
*        may be used. Units counts.
*     NUMBER = INTEGER (Given)
*        The number of pixels that are non-bad. Units pixels.
*     SDEV(2) = DOUBLE PRECISION (Given and Returned)
*        Standard deviation of the image pixel count distribution
*        and the background standard deviation. Units counts.
*     BARRAY(BARSIZ) = DOUBLE PRECISION (Returned)
*        The binning array.
*     SMOBAR(BARSIZ) = DOUBLE PRECISION (Returned)
*        The smoothed binning array.
*     MEDIAN = DOUBLE PRECISION (Returned)
*        The estimated median value for the image pixel counts.
*        Units counts.
*     PEAKV(3) = DOUBLE PRECISION (Returned)
*        Esimates of the peak number of pixels found with a given count
*        value in the count value versus occurence histogram.       
*        Units pixels.
*     SFACTA = INTEGER (Returned)
*        Gaussian filter radius actually employed when smoothing
*        the array (SMOBAR).
*     MODE(4) = DOUBLE PRECISION (Returned)
*        Estimated values for the mode value of the image pixel
*        count distribution. Units counts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     15-May-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'lob_par'               ! LOBACK system constants

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER BARSIZ                  ! Size of the binning arrays used 
      INTEGER NUMBER                  ! The number of pixels to be used
      INTEGER POINT2(1)               ! Pointer to the binning array
      INTEGER POINT3(1)               ! Pointer to the smoothed bin array
      INTEGER SFACT                   ! Requested radius for the Gaussian
                                      ! filter used to smooth the 
                                      ! histogram
      REAL ARRAY(ELEMS)               ! Array containing the image data
      REAL BINWID                     ! Width of the bins used to find 
                                      ! median and mode (only differs
                                      ! from 1 when count range exceeds
                                      ! LOB__BINLI)
      REAL LOW                        ! Binning arrays origin offset and
                                      ! lowest value found in image
      DOUBLE PRECISION ADEV           ! Absolute deviation of data 

*  Arguments Given and Returned:
      DOUBLE PRECISION SDEV(2)        ! Standard deviation and standard deviation
                                      ! of pixel count values

*  Arguments Returned:
      INTEGER SFACTA                  ! Radius of the Gaussian
                                      ! filter actually used to smooth
                                      ! the histogram
      DOUBLE PRECISION BARRAY(BARSIZ) ! Binning array for the pixel cnts
      DOUBLE PRECISION MEDIAN         ! Median value for the image 
      DOUBLE PRECISION MODE(4)        ! Mode values for the image
      DOUBLE PRECISION PEAKV(3)       ! Highest value found in the 
      DOUBLE PRECISION SMOBAR(BARSIZ) ! Smoothed binning array

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables: 
      INTEGER HIIND                   ! Temporary store
      INTEGER I                       ! Temporary loop variable
      INTEGER INDEX                   ! Array element index in which to 
                                      ! bin a given pixel count value
      INTEGER J                       ! Temporary loop variable
      INTEGER LOIND                   ! Temporary store
      INTEGER MODEC                   ! Bin array index corresponding
                                      ! to the bin array modal value
      INTEGER NUMDAT                  ! Number of data points in the
                                      ! array passed to
                                      ! subroutine LOB1_LINRE
      REAL CONS                       ! Constant term of linear
                                      ! relationship fitted by
                                      ! subroutine LOB1_LINRE
      REAL GRAD                       ! Gradient term of linear 
                                      ! relationship fitted by 
                                      ! subroutine LOB1_LINRE
      REAL HEIG(LOB__CHORM)           ! The values at which chords 
                                      ! (slices) were taken through the
                                      ! histogram
      REAL VALUE1                     ! Temporary storage variable
      REAL X1(LOB__CHORM)             ! X value array passed to
                                      ! subroutine LOB1_LINRE
      REAL Y1(LOB__CHORM)             ! Y value array passed to
                                      ! subroutine LOB1_LINRE
      DOUBLE PRECISION HALF           ! Half the number of non-bad
                                      ! in the binning arrays 
      DOUBLE PRECISION HIVAL          ! Highest value found in the bin
                                      ! array
      DOUBLE PRECISION LOVAL          ! Lowest value found in the bin
                                      ! array
      DOUBLE PRECISION SFTOT          ! Total of smoothing factors used
      DOUBLE PRECISION SMOFAC(-LOB__SFLIM:LOB__SFLIM) 
                                      ! Smoothing factors for a Gaussian
                                      ! filter to smooth array BARRAY  
      DOUBLE PRECISION SMOTOT         ! Total of values in the smoothed
                                      ! bin array SMOBAR
      DOUBLE PRECISION TOTAL          ! Sum of the bin array BARRAY
      DOUBLE PRECISION VALUE          ! Temporary storage variable

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN        

*   Clear the contents of the BARRAY and SMOBAR.
      DO 11 I=1,BARSIZ
         SMOBAR(I)=0.0
         BARRAY(I)=0.0
 11   CONTINUE

*   Assign all non-bad pixels of the image 
*   array to a binning array to allow the mode and median
*   to be calculated.
      DO 312 I=1,ELEMS
         VALUE1=ARRAY(I)
         IF (VALUE1.NE.VAL__BADR) THEN 

*         Calculate which bin element an image pixel count must be 
*         assigned to.
            INDEX=INT((VALUE1-LOW)/BINWID+1.)
           
*         Increment the count in the appropriate bin.
            BARRAY(INDEX)=BARRAY(INDEX)+1.
         END IF 
 312  CONTINUE 

*   Look through the bin array to find the highest value therein.
*   This is taken as a simple first estimate of the mode value.
      MODEC=0
      LOVAL=0
      HIVAL=0
      DO 320 I=1,BARSIZ
         
*      Set HIVAL and MODEC as a new highest value has been found.    
         IF (BARRAY(I).GT.HIVAL) THEN 
            MODEC=I
            HIVAL=BARRAY(I)
         END IF

*      Reset LOVAL as a new lowest value has been found.
         IF (BARRAY(I).LT.LOVAL) LOVAL=BARRAY(I)

 320  CONTINUE

*   Assigned unsmoothed mode and peak value. 
      MODE(1)=LOW+(REAL(MODEC)-1.)*BINWID
      PEAKV(1)=HIVAL

*   Sum the elements of the bin array and stop when the sum exceeds
*   half that of the number of non-bad pixels in the image.
      INDEX=0
      TOTAL=0.0
      HALF=REAL(NUMBER)/2.

*   Add bin array contents to the total until it exceeds HALF.
      DO WHILE ((TOTAL.LT.HALF).AND.(INDEX.LE.BARSIZ)) 
         INDEX=INDEX+1 
         TOTAL=TOTAL+BARRAY(INDEX)
      END DO

*   Remove the contents of the last bin (the one that took TOTAL above
*   the value of HALF) and decrement INDEX so that it now points to
*   the array element before HALF was exceeded.
      TOTAL=TOTAL-BARRAY(INDEX)
      INDEX=INDEX-1

*   Use approximate interpolation to estimate the median position
*   given the contents of the bin in which it occurs.
      MEDIAN=LOW+(INDEX-1)*BINWID
      
      SFACTA=SFACT
      IF (SFACTA.EQ.-1) THEN

*      Use the absolute deviation as an upper limit for the smoothing 
*      filter radius.
         SFACTA=NINT(ADEV/BINWID)

*      Look through the BARRAY to find if all the points within +-
*      SFACTA indices of the modal index have values greater than 
*      20% of the highest value. Retains the first values from either
*      side of the mode that are not.
         IF (SFACTA.LT.1) SFACTA=1

*      Calculate an average value for the region of the BARRAY around the 
*      largest value.
         VALUE=0.0
         J=0
         DO 329 I=MODEC-1,MODEC+1
            IF ((I.GE.1).AND.(I.LE.BARSIZ)) THEN
               VALUE=VALUE+BARRAY(I)
               J=J+1
            END IF
 329     CONTINUE
         VALUE=0.2*VALUE/REAL(J)

*      look for the lower limit.
         LOIND=MODEC-SFACTA
         IF (LOIND.LT.1) LOIND=1
         DO 330 I=MODEC-SFACTA,MODEC-1
            IF ((I.GE.1).AND.(I.LE.BARSIZ)) THEN
               IF (BARRAY(I).LT.VALUE) LOIND=I
            END IF
 330     CONTINUE

*      Look for the upper limit.          
         HIIND=MODEC+SFACTA
         IF (HIIND.GT.BARSIZ) HIIND=BARSIZ
         DO 331 I=MODEC+SFACTA,MODEC+1,-1
            IF ((I.GE.1).AND.(I.LE.BARSIZ)) THEN
               IF (BARRAY(I).LT.VALUE) HIIND=I
            END IF
 331     CONTINUE

*      Calculate the filter radius.
         SFACTA=NINT((HIIND-LOIND)/2.)

*      Impose an upper limit.
         IF (SFACTA.GT.LOB__SFLIM) SFACTA=LOB__SFLIM

      ELSE
     
*      Set the filter radius and impose an upper limit.
         SFACTA=SFACTA/BINWID
         IF (SFACTA.GT.LOB__SFLIM) SFACTA=LOB__SFLIM
         
      END IF


*   Calculate the weighting factors that should be applied to pixels
*   when using the Gaussian filter to smooth the histogram. 
      IF (SFACTA.EQ.0) THEN

*      Only one point is to be included in the smoothing routine. ie
*      no smoothing to take place so the weighting factor for that 
*      pixel is 1.0.
         SMOFAC(0)=1.0

      ELSE

*      Setup the weighting array.
         TOTAL=0.0
         DO 350 I=0,SFACTA
            SMOFAC(I)=1./SQRT(2.*LOB__PIVAL)/(SFACTA/3.)
            SMOFAC(I)=SMOFAC(I)*EXP(-.5*(REAL(I)/(SFACTA/3.))**2)
            SMOFAC(-I)=SMOFAC(I)
            IF (I.EQ.0) THEN
               TOTAL=TOTAL+SMOFAC(I)
            ELSE
               TOTAL=TOTAL+SMOFAC(I)*2.
            END IF 
 350     CONTINUE

*      Modify the weighting factors so that the sum of them is unity.
         DO 360 I=-SFACTA,SFACTA
            SMOFAC(I)=SMOFAC(I)/TOTAL
 360     CONTINUE  
 
      END IF

*   Smooth the BARRAY and put the new values into array SMOBAR.
*   Also determine the total of the SMOBAR array.
      SMOTOT=0.0
      DO 380 I=SFACTA+1,BARSIZ-SFACTA-1

*      Look at the histogram elements on either side of the 
*      element being considered and calculate the contribution
*      from each.
         DO 370 J=-SFACTA,SFACTA

*         Accumulate each contribution.
            SMOBAR(I)=SMOBAR(I)+SMOFAC(J)*BARRAY(I+J)
 370     CONTINUE

*      Determine the total for the smoothed array.
         SMOTOT=SMOTOT+SMOBAR(I)
 380  CONTINUE 

*   Smooth the data at the edges of the array.
*   Low value edge.
      DO 382 I=1,SFACTA

*      Look at the histogram elements on either side of the 
*      element being considered and calculate the contribution
*      from each.
         SFTOT=0.0
         DO 381 J=-SFACTA,SFACTA

*         Only use values that are defined. i.e. within the array bounds.
            IF ((I+J.GE.1).AND.(I+J.LE.BARSIZ)) THEN 
               SMOBAR(I)=SMOBAR(I)+SMOFAC(J)*BARRAY(I+J)
               SFTOT=SFTOT+SMOFAC(J)
            END IF
 381     CONTINUE

*      Modify smoothed value to approx. take into account
*      the missing data points.
         IF (SMOBAR(I).GT.0.0) SMOBAR(I)=SMOBAR(I)/SFTOT

*      Determine the total for the smoothed array.
         SMOTOT=SMOTOT+SMOBAR(I)
 382  CONTINUE 

*   Smooth the data at the edges of the array.
*   high value edge.
      DO 384 I=BARSIZ-SFACTA,BARSIZ

*      Set initial value of the smoothed array element.
         SMOBAR(I)=0.0

*      Look at the histogram elements on either side of the 
*      element being considered and calculate the contribution
*      from each.
         SFTOT=0.0
         DO 383 J=-SFACTA,SFACTA

*         Only use values that are defined. i.e. within the array bounds.
            IF ((I+J.GE.1).AND.(I+J.LE.BARSIZ)) THEN 
               SMOBAR(I)=SMOBAR(I)+SMOFAC(J)*BARRAY(I+J)
               SFTOT=SFTOT+SMOFAC(J)
            END IF
 383     CONTINUE

*      Modify smoothed value to approx. take into account
*      the missing data points.
         IF (SMOBAR(I).GT.0.0) SMOBAR(I)=SMOBAR(I)/SFTOT

*      Determine the total for the smoothed array.
         SMOTOT=SMOTOT+SMOBAR(I)
 384  CONTINUE        

*   Convert the SFACTA value used to show the radius of the smoothing
*   filter in terms of the actual array indices used (necessary only
*   when the difference between HIGH and LOW is greater 
*   than LOB__BINLI.
      SFACTA=NINT(SFACTA*BINWID)     

*   Search the array of smoothed values for its modal value and also
*   recalculate/estimate the value of the mode.
      MODEC=0
      LOVAL=VAL__MAXD
      HIVAL=VAL__MIND
      DO 390 I=1,BARSIZ

*      Reset HIVAL and MODEC as a new highest value has been found.    
         IF (SMOBAR(I).GT.HIVAL) THEN 
            MODEC=I
            HIVAL=SMOBAR(I)
         END IF                  

*      Reset LOVAL as a new lowest value has been found
         IF (SMOBAR(I).LT.LOVAL) LOVAL=SMOBAR(I)

 390  CONTINUE

*   Assigned smoothed mode value. 
      MODE(2)=LOW+(REAL(MODEC)-1.)*BINWID
      PEAKV(2)=HIVAL
      
*   Sum the elements of the smoothed bin array and stop when the sum 
*   exceeds half that of the number of non-bad pixels in the image.
      INDEX=0
      TOTAL=0.0
      HALF=SMOTOT/2.

*   Add bin array contents to the total until it exceeds HALF.
      DO WHILE ((TOTAL.LT.HALF).AND.(INDEX.LE.BARSIZ)) 
         INDEX=INDEX+1 
         TOTAL=TOTAL+SMOBAR(INDEX)
      END DO

*   Remove the contents of the last bin (the one that took TOTAL above
*   the value of HALF) and decrement INDEX so that it now points to
*   the array element before HALF was exceeded.
      TOTAL=TOTAL-SMOBAR(INDEX)
      INDEX=INDEX-1

*   Use approximate interpolation to estimate the median position
*   given the contents of the bin in which it occurs.
      MEDIAN=LOW+(INDEX-1+(HALF-TOTAL)/SMOBAR(INDEX+1))*BINWID

*   Take chords through the histogram peak and get values for 
*   histogram chord 
      CALL LOB1_CHORD(HIVAL,LOVAL,MODEC,%VAL(POINT3(1)),LOW,ADEV,
     :                BARSIZ,BINWID,NUMDAT,HEIG,X1,Y1,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
           
*   Determine the linear relationship between histogram width
*   and value at which the width was determined. Extrapolate to zero
*   width (peak) and thereby estimate a mode value.
      IF (NUMDAT.GT.2) THEN 
         CALL LOB1_LINRE(X1,Y1,NUMDAT,GRAD,CONS,STATUS)
         IF (NUMDAT.GT.2) MODE(3)=CONS
      END IF

*   Set up the data for matrix inversion to provide 
*   an interpolated value for the mode.
      CALL LOB1_PARA(ADEV,%VAL(POINT3(1)),BARSIZ,LOW,BINWID,
     :               MODE,SDEV,PEAKV,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

 9999 CONTINUE
                    
      END

 
      SUBROUTINE LOB1_LINRE(X1,Y1,NUMDAT,GRAD,CONS,STATUS)
*+
*  Name:
*     LOB1_LINRE

*  Purpose:
*     Determines a least squares linear fit for data in arrays X and Y. 

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LOB1_LINRE(X1,Y1,NUMDAT,GRAD,CONS,STATUS)

*  Description:
*     Uses a normal least squares method to determine the coefficients
*     of a linear fit to the contents of arrays X and Y.

*  Arguments:
*     X1(LOB__CHORM) = REAL (Given)
*        The x values to be used.
*     Y1(LOB__CHORM) = REAL (Given)
*        The y values to be used.
*     NUMDAT = INTEGER (Given)
*        The number of data (X/Y) pairs to be fitted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*     GRAD   = REAL (Returned)
*        The gradient value for the linear fit.
*     CONS   = REAL (Returned)
*        The constant value for the linear fit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     15-May-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'lob_par'               ! LOBACK system variables
                     
*  Arguments Given:
      INTEGER NUMDAT                  ! Number of data points
      REAL X1(LOB__CHORM)             ! Data points X1 value
      REAL Y1(LOB__CHORM)             ! Data points Y1 value

*  Arguments Returned:
      REAL CONS                       ! Constant of linear equation
      REAL GRAD                       ! Gradient of linear equation     

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local Variables:                                                          
      INTEGER I                       ! Loop variable
      REAL MNVX                       ! Mean value of X1 array
      REAL NUMB                       ! Number of data points
      REAL SUMX                       ! Sum of X1 array
      REAL SUMY                       ! Sum of Y1 array
      REAL TOT1                       ! Absolute X1 deviation from
                                      ! the mean
      REAL TOT2                       ! Absolute X1 deviation squared
                                      ! sum
      REAl TOT3                       ! Absolute X1 deviation 
                                      ! times Y1

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN
   
      NUMB=REAL(NUMDAT)

*   Find the mean value of x and the sums of the x and y arrays.
      SUMX=0.0
      SUMY=0.0
      MNVX=0.0
      DO 500 I=1,NUMDAT
         SUMX=SUMX+X1(I)
         SUMY=SUMY+Y1(I)
 500  CONTINUE
      MNVX=SUMX/NUMB

*   Calculate the squared sum of (x-xmean) 
*   and thereby the gradient and constant terms in the equation.
      TOT1=0.0
      TOT2=0.0
      TOT3=0.0
      DO 510 I=1,NUMDAT
         TOT1=X1(I)-MNVX
         TOT2=TOT2+TOT1*TOT1
         TOT3=TOT3+TOT1*Y1(I)
 510  CONTINUE

*   Check that more than one value of pixel count was found.
      IF (ABS(TOT1).GT.LOB__VSMAL) THEN
         GRAD=TOT3/TOT2
         CONS=(SUMY-SUMX*GRAD)/NUMB
      ELSE
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','All the histogram chords had the same'//
     :                    ' centre point. No projected'//
     :                    ' mode value will be generated.',STATUS)
         NUMDAT=0
         GOTO 9999
      END IF
 
 9999 CONTINUE
     
      END       
       

      SUBROUTINE LOB1_MOMDE(ELEMS,NUMBER,ARRAY,
     :                      MEAN,ADEV,VARI,SDEV,STATUS) 
*+
*  Name:
*     LOB1_MOMDE

*  Purpose:
*     Finds the absolute and standard deviations of pixels in an image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LOB1_MOMDE(ELEMS,NUMBER,ARRAY,
*                     MEAN,ADEV,VARI,SDEV,STATUS) 
                  
*  Description:
*     Finds values for the absolute deviation, standard deviation and 
*     variance of pixels in an image.

*  Arguments:                      
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image. Units pixels.
*     NUMBER = INTEGER (Given)
*        The number of image pixels to be used. Units pixels.
*     ARRAY(ELEMS) = INTEGER (Given)
*        Array containing the image data.
*     MEAN = DOUBLE PRECISION (Given)
*        Mean of the values found in the image pixels. Units counts.
*     ADEV = DOUBLE PRECISION (Returned)
*        Absolute deviation of the image pixels count distribution.
*        Units counts.
*     VARI = DOUBLE PRECISION (Returned)                             
*        Variance of the image pixel count distribution.
*     SDEV(2) = DOUBLE PRECISION (Returned)
*        Standard deviation of the image pixel count distribution
*        and the standard deviation of the background. Units counts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     15-May-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'lob_par'               ! LOBACK system variables

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the data 
      INTEGER NUMBER                  ! The number of pixels to be used
      REAL ARRAY(ELEMS)               ! Array containing image data
      DOUBLE PRECISION MEAN           ! Average value in the image 
                                      ! array       
             
*  Arguments Returned:
      DOUBLE PRECISION ADEV           ! Absolute deviation of array 
      DOUBLE PRECISION SDEV(2)        ! Standard deviation and the 
                                      ! standard deviation of background
      DOUBLE PRECISION VARI           ! Variance of array values

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables: 
      INTEGER I                       ! Loop variable
      REAL VALUE                      ! Temporary storage variable
      DOUBLE PRECISION P2             ! Temporary storage variable

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN
       
*   Look through the array to gather information for calculating the
*   standard deviation, absolute deviation and  variance 
*   of the distribution.
      DO 212 I=1,ELEMS
         VALUE=ARRAY(I)
         IF (VALUE.NE.VAL__BADR) THEN 
*         Absolute deviation (first moment of deviation). 
            VALUE=VALUE-MEAN         
            ADEV=ADEV+ABS(VALUE)
            P2=VALUE*VALUE
*         Variance. 
            VARI=VARI+P2
         END IF
 212  CONTINUE  
      
*   Derive values from the previous summations for absolute deviation,
*   variance and standard deviation.     
      IF (NUMBER.GE.2) THEN
         ADEV=ADEV/REAL(NUMBER)
         VARI=VARI/REAL(NUMBER-1)
         SDEV(1)=SQRT(VARI)
      ELSE
         CALL MSG_OUT(' ','WARNING',STATUS)
         CALL MSG_OUT(' ','Too few points for an accurate estimate'//
     :                    ' of the standard. deviation etc.',STATUS)
         STATUS=SAI__ERROR
         GOTO 9999
      END IF
      
 9999 CONTINUE

      END


      SUBROUTINE LOB1_PARA(ADEV,SMOBAR,BARSIZ,LOW,BINWID,
     :                     MODE,SDEV,PEAKV,STATUS)
*+                        
*  Name:
*     LOB1_PARA

*  Purpose:
*     Estimate histogram mode by parabolic fitting of the histogram peak. 
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LOB1_PARA(ADEV,SMOBAR,BARSIZ,LOW,BINWID,
*                    MODE,SDEV,PEAKV,STATUS)
    
*  Description:                                        
*     Logarithmically transforms the values of the smoothed
*     histogram array (SMOBAR) and then 'fits' a parabola to the
*     points near to the peak. The fitting is carried out by routine
*     LOB1_GAUJO but the data is passed in array VECTOR and 
*     preprocessed (to reduce the memory requirement) array INPMAT.
*     
*     The coefficients for the parabola are used to determine the
*     mode, standard deviation and height of the histogram peak.
*      

*  Arguments:                                     
*     ADEV = DOUBLE PRECISION (Given)
*        Absolute deviation of the image data. Units counts.
*     SMOBAR(BARSIZ) = DOUBLE PRECISION (Given)
*        The smoothed binning array.
*     BARSIZ = INTEGER (Given)
*        Size (no. of elements) of the binning array used.
*     LOW = REAL (Given)
*        Lowest count value in the image. Also used as an array
*        offset index for the arrays SMOBAR and BARRAY. Units counts.
*     BINWID = REAL (Given)
*        Width of each binning array element. Units counts.
*     MODE(4) = DOUBLE PRECISION (Given and Returned)
*        Estimated modal values for the image data. Units counts.
*     SDEV(2) = DOUBLE PRECISION (Given and Returned)
*        Standard deviations for the image pixel count distribution
*        and the background count standard deviation. Units counts.
*     PEAKV(3) = DOUBLE PRECISION (Given and Returned)
*        Esimates of the peak number of pixels found with a given count
*        value in the count value versus occurence histogram.          
*        Units pixels.
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
      INCLUDE 'lob_par'               ! LOBACK system variables
                                                                      
*  Arguments Given:
      INTEGER BARSIZ                  ! Size of the binning arrays used
      REAL BINWID                     ! Width of each binning array 
                                      ! elements
      REAL LOW                        ! Binning arrays origin offset and
                                      ! lowest value found in image
      DOUBLE PRECISION ADEV           ! Absolute deviation of image data
      DOUBLE PRECISION SMOBAR(BARSIZ) ! Smoothed bin array

*  Arguments Given and Returned:
      DOUBLE PRECISION MODE(4)        ! Mode values for the image data
      DOUBLE PRECISION SDEV(2)        ! Background standard deviation
      DOUBLE PRECISION PEAKV(3)       ! Highest value found in the
                                      ! histogram array 
*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:                                                
      INTEGER I                       ! Temporary loop variable
      INTEGER J                       ! Temporary loop variable
      INTEGER K                       ! Temporary loop variable
      INTEGER L                       ! Temporary loop variable
      REAL INPMAT(3,3)                ! Matrix array passed to
                                      ! subroutine LOB1_GAUJO 
      REAL VECTOR(3)                  ! Vector array in which parabola 
                                      ! coefficients are returned from 
                                      ! subroutine LOB1_GAUJO
      REAL DETERM                     ! Inverted matrix determinant
                                      ! (used to indicate failure)
      REAL RANGE                      ! Range of histogram elements over
                                      ! which the parabolic interpolation 
                                      ! will be applied
      REAL VALUE                      ! Temporary value
      REAL XX(3)                      ! Temporary array
          
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Model the part of the smoothed histogram within approx. +-1 absolute
*   deviations of the estimated mode value and use parabolic 
*   interpolation to obtain another value for mode. This is possible
*   since applying a logarithmic transform to a Gaussian distribution
*   makes it become a parabola.

*   Set the initial width for the part of the histogram to be used for
*   fitting a parabola.
      RANGE=ADEV/BINWID+1.0
      IF (RANGE.LT.3.0) RANGE=3.0

*   Perform the matrix inversion. If it fails and the number of data
*   points is not too big, increase the number of data points 
*   included and try again.
      DETERM=0.0
      DO WHILE (((ABS(DETERM).LT.LOB__VSMAL).
     :            OR.(VECTOR(3).GE.0.0)).AND.
     :                  (RANGE.LE.2.0*ADEV/BINWID))
       
*      Clear the arrays to be used.
         DO 450 I=1,3
            VECTOR(I)=0.0
            DO 440 J=1,3
               INPMAT(J,I)=0.0
 440        CONTINUE
 450     CONTINUE

*      Use only data points near the current mode estimate.
         L=(MODE(2)-LOW)/BINWID+1
         DO 480 I=NINT(-RANGE*1.5),NINT(RANGE)

*         Define the array index.
            J=L+I

*         Avoid looking at data points that are beyond the array bounds.
            IF ((J.GE.1).AND.(J.LE.BARSIZ)) THEN

*            Avoid taking the log. of zero and avoid using
*            distribution outliers.
               IF ((SMOBAR(J).GT.LOB__VSMAL).AND.
     :                       (SMOBAR(J).GT.PEAKV(2)*0.2)) THEN

*               Prepare matrix coefficients for inversion.
                  VALUE=SMOBAR(J)
                  VALUE=ALOG(VALUE)
                  XX(1)=1.0
                  XX(2)=REAL(I)
                  XX(3)=XX(2)*XX(2)

                  DO 470 J=1,3
                     VECTOR(J)=VECTOR(J)+XX(J)*VALUE
                     DO 460 K=1,3
                        INPMAT(K,J)=INPMAT(K,J)+XX(K)*XX(J)
 460                 CONTINUE
 470              CONTINUE
               END IF
            END IF
 480     CONTINUE

*      If sufficient data points are available, perform the matrix
*      inversion.
         IF (INPMAT(1,1).GT.2.0) 
     :       CALL LOB1_GAUJO(VECTOR,INPMAT,DETERM,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Increase the range of points to be used.
         RANGE=RANGE*1.05

      END DO
      
*   Assign interpolated mode, peak and standard deviation values.
      IF ((RANGE.LE.2.*ADEV/BINWID).AND.(ABS(DETERM).GT.LOB__VSMAL)
     :     .AND.(VECTOR(3).NE.0.0)) THEN
         MODE(4)=MODE(2)-VECTOR(2)/2./VECTOR(3)*BINWID
         SDEV(2)=SQRT(-1./VECTOR(3)/2.)*BINWID
         PEAKV(3)=EXP(VECTOR(1)-(VECTOR(2)/2.)**2/VECTOR(3))
         IF ((SDEV(2).LE.0.0).OR.(PEAKV(3).LE.0.0)) THEN 
            CALL MSG_OUT(' ','WARNING!!!',STATUS)
            CALL MSG_OUT(' ','Bad interpolation results.',STATUS)
         END IF
      ELSE
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','No sensible interpolated mode.',STATUS)
      END IF

 9999 CONTINUE
     
      END 
     

      SUBROUTINE LOB1_TEXTO(WHICH,COSYS,NDF1,XCO,YCO,MODE,SDEV,
     :                      LBND,FIOD,OPENF,EXCLAIM,STATUS)

*+
*  Name:
*     LOB1_TEXTO

*  Purpose:
*     Puts the background calculation results into a text format 
*     ASCII output file.
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL LOB1_TEXTO(WHICH,COSYS,NDF1,XCO,YCO,MODE,SDEV,
*                      LBND,FIOD,OPENF,EXCLAIM,STATUS)

*  Description:
*     Creates a text file (if required) and places in it data from the
*     background calculations.
*
*     The parameter WHICH is used as follows:
*         WHICH=1  Open file, save headings, save data  and close the file.
*         WHICH=2  Save the results for the current location.
*         WHICH=3  Close the file.

*  Arguments:               
*     WHICH = INTEGER (Given)
*        Used to show which part of the text file is to be created. 
*     COSYS *(256) = CHAR (Given)
*        Denotes whether world or pixel/data co-ordinates are
*        being used to define locations on the image.
*     NDF1 = INTEGER (Given)
*        NDF identifier for the image.
*     XCO = REAL (Given)
*        The X index of the origin used. Units pixels.
*     YCO = REAL (Given)
*        The Y index of the origin used. Units pixels.
*     MODE = REAL (Given)
*        Image background values. Units counts.
*     SDEV = REAL (Given)
*        Standard deviation of the background values
*        and the standard deviation value. Units counts.
*     LBND(10) = INTEGER (Given)
*        Lower limits of the image world co-ordinate system.
*     FIOD = INTEGER (Given and Returned)
*        Output file FIO identifier.
*     OPENF = LOGICAL (Given and Returned)
*        Was an output file created?
*     EXCLAIM = LOGICAL (Given and Returned)
*        Was the output file name !
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-JUN-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'lob_par'               ! LOBACK constants
      INCLUDE 'MSG_PAR'               ! Parameter system constants

*  Arguments Given:                              
      CHARACTER *256 COSYS            ! Defines if world or pixel/data
                                      ! co-ordinates are in use
      INTEGER LBND(10)                ! Lower limits of image world
                                      ! co-ordinate system
      INTEGER NDF1                    ! NDF indentifier
      INTEGER WHICH                   ! Defines which part of the file saving
                                      ! is to be performed.
      DOUBLE PRECISION MODE(4)        ! Background count values
      DOUBLE PRECISION SDEV(2)        ! Standard deviations
                                      ! of the background
      REAL XCO                        ! X index of the origin
      REAL YCO                        ! Y index of the origin

*  Arguments Given and Returned:                              
      LOGICAL EXCLAIM                 ! Was the file name a !
      LOGICAL OPENF                   ! Was a file created?
      INTEGER FIOD                    ! Output file identifier
      
*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:
      CHARACTER *(80) FTEXT           ! Temporary format storage
      CHARACTER *(80) LINE            ! FIO line output length
      CHARACTER *(MSG__SZMSG) NAME    ! NDF name
      CHARACTER *(80) TEXT            ! Temporary storage
      INTEGER I                       ! Temporary variable
      INTEGER J                       ! Temporary variable
      INTEGER NCHAR                   ! Length of output string

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Open the FIO file.
      IF (WHICH.EQ.1) THEN

*      Determine the output text file name. If the file name chosen fails, 
*      the user is reprompted
         OPENF=.FALSE.             
         EXCLAIM=.FALSE.   
         CALL ERR_MARK
         DO WHILE((.NOT.OPENF).AND.(.NOT.EXCLAIM)
     :             .AND.(STATUS.EQ.SAI__OK))
            CALL AIF_ASFIO('OUT','WRITE','LIST',80,FIOD,OPENF,
     :                      EXCLAIM,STATUS)
            IF ((.NOT.OPENF).AND.(.NOT.EXCLAIM)) THEN
               CALL ERR_REP(' ','Bad file name.',STATUS)
               CALL ERR_REP(' ','For no file, type !',STATUS)
               CALL ERR_ANNUL(STATUS)
            END IF
         END DO
         CALL ERR_RLSE
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Inform the user if a difficulty was encountered and that an
*      an output file will not be used. 
         IF (EXCLAIM) THEN  
            CALL MSG_BLANK(STATUS)
            CALL MSG_OUT(' ','WARNING!!!',STATUS)
            CALL MSG_OUT(' ','No output text file created.',STATUS)
            CALL MSG_BLANK(STATUS)
            GOTO 9999
         END IF

*      Inform the user if a difficulty was encountered and that an
*      an output file will not be used. Otherwise add values to the 
*      output file.
         IF (OPENF) THEN  

*         Output a heading.
            NCHAR=0
            CALL CHR_PUTC('## ESP LOBACK V1.0 OUTPUT FILE',LINE,NCHAR)
            CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
            NCHAR=0
            CALL CHR_PUTC('##',LINE,NCHAR)
            CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*         Output the file name.
            NCHAR=0
            CALL CHR_PUTC('## Filename: ',LINE,NCHAR)
            CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
            NCHAR=0
            CALL NDF_MSG('NAME',NDF1)
            CALL MSG_LOAD(' ','## ^NAME',NAME,I,STATUS)
            NAME=NAME(1:I)
            CALL CHR_CLEAN(NAME)
            CALL CHR_PUTC(NAME,LINE,NCHAR)
            CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*         Output a data description.
            NCHAR=0
            TEXT=' X         Y        Raw   Smoothed '//
     :       '  Proj.    Interp.    Sdev.     Sigma. '
            CALL CHR_PUTC('## '//TEXT,LINE,NCHAR)
            CALL FIO_WRITE(FIOD,LINE(1:NCHAR),STATUS)

         ELSE

            CALL MSG_BLANK(STATUS)
            CALL MSG_OUT(' ','WARNING!!!',STATUS)
            CALL MSG_OUT(' ','No output text file created.',STATUS)
            CALL MSG_BLANK(STATUS)
            GOTO 9999

         END IF

      END IF

*   Output the galaxy co-ordinates used and the profiling results.
      IF ((WHICH.EQ.2).AND.(OPENF)) THEN

*      Create an appropriately formatted output string.
         IF (COSYS.EQ.'W') THEN

*         Original co-ordinates were in world form.
            CALL MSG_FMTR('X','F8.1',XCO-1+LBND(1))
            CALL MSG_FMTR('Y','F8.1',YCO-1+LBND(2))
 
         ELSE
 
*         Original co-ordinates were in pixel/data form.
            CALL MSG_FMTR('X','F8.1',XCO)
            CALL MSG_FMTR('Y','F8.1',YCO)

         END IF

*      Set up the formatting strings.

*      Mode values.
         FTEXT='F8.1'
         IF (ABS(MODE(1)).GT.9.9E8) FTEXT='E14.5'
         CALL MSG_FMTD('BACK1',FTEXT,MODE(1))

         FTEXT='F8.1'
         IF (ABS(MODE(2)).GT.9.9E8) FTEXT='E14.5'
         CALL MSG_FMTD('BACK2',FTEXT,MODE(2)) 

         FTEXT='F8.1'
         IF (ABS(MODE(3)).GT.9.9E8) FTEXT='E14.5'
         CALL MSG_FMTD('BACK3',FTEXT,MODE(3))

         FTEXT='F8.1'
         IF (ABS(MODE(4)).GT.9.9E8) FTEXT='E14.5'
         CALL MSG_FMTD('BACK4',FTEXT,MODE(4))

*      Standard deviation value.
         FTEXT='F7.1'
         IF (ABS(SDEV(1)).GT.9.9E8) FTEXT='E14.5'
         CALL MSG_FMTD('SDEV1',FTEXT,SDEV(1))

*      Background standard deviation value.
         FTEXT='F7.1'
         IF (ABS(SDEV(2)).GT.9.9E8) FTEXT='E14.5'
         CALL MSG_FMTD('SDEV2',FTEXT,SDEV(2))

         TEXT='^X  ^Y  ^BACK1 ^BACK2 ^BACK3 ^BACK4'//
     :        '  ^SDEV1  ^SDEV2'

*      Output the results in suitably formatted form.
         NCHAR=0
         CALL MSG_LOAD(' ',TEXT,NAME,J,STATUS)
         NAME=NAME(1:J)
         CALL CHR_CLEAN(NAME)
         CALL CHR_PUTC(NAME,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

      END IF

*  Close down the file output.
      IF ((WHICH.EQ.3).AND.(OPENF)) CALL FIO_CLOSE(FIOD,STATUS)

 9999 CONTINUE

      END 


************************************
*** KAPPA/KAPGEN CODE ADDED HERE ***
************************************


      SUBROUTINE AIF_ASFIO (PNFILE,ACMODE,FORM,RECSZ,FD,OPEN,
     :                      EXCLAIM,STATUS)
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
*      CALL AIF_ASFIO (PNFILE,ACMODE,FORM,RECSZ,FD,OPEN, 
*                      EXCLAIM,STATUS)

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

