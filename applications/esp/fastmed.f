      SUBROUTINE FASTMED(STATUS)             
*+
*  Name:
*     FASTMED

*  Purpose:
*     Applies a square median filter of user defined size to an input 
*     image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FASTMED(STATUS)

*  Arguments:
*     STATUS = INTEGER(Given and Returned)
*     The global status.

*  Description:
*     The method used employs a rolling histogram and allows 
*     the whole image to be filtered. 
*
*     The median pixel value in the region surrounding each input 
*     image pixel is  calculated. This value is then subtracted from 
*     the value of the pixel being considered. Finally, the original 
*     background count is added to the result, which is placed in the 
*     corresponding pixel of the output image.

*  Usage:
*     FASTMED IN OUT BACK SIGMA WIDTH 

*  ADAM Parameters:
*     BACK = _REAL (Read)
*        Image background count value. Units counts.
*     IN = _NDF (Read)
*        The name of the NDF to which the filter will be applied.
*     OUT = _NDF (Write)
*        The name of the output NDF that will be created. 
*     SIGMA = _REAL (Read)
*        Standard deviation of the background count value. Units counts.
*     WIDTH = _INTEGER (Read)
*        The width of the filter to be employed. Units pixels.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-MAY-1993 (GJP)
*     (Original version)

*  Examples:
*     fastmed in=field out=flatgal back=760. sigma=27. width=72
*
*        In this example, a 72x72 pixel filter will be applied to
*        the input image FIELD. The resulting median filtered image 
*        will be placed in output image FLATGAL. The input image
*        (FIELD) had a global background count value of 760 with
*        an associated standard deviation of 27 counts.
 
*  Notes:
*     With small filters it may be found that the resulting output 
*     images are noisy. This is due to the small 
*     number of pixels contributing to the histogram. The problem will 
*     be most obvious at the image edges and corners. For this reason 
*     some users may find it necessary to clip the output image by 
*     WIDTH/2 pixels on each edge to generate better results.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
                     
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
      INTEGER RADIUS                  ! Radius of the median filter
      INTEGER UBND(7)                 ! Upper bounds for each image axis
      INTEGER WIDTH                   ! Width of the median filter
      INTEGER XMAX                    ! Width in pixels of the image
      INTEGER YMAX                    ! Length in pixels of the image
      REAL BACK                       ! Background count value
      REAL SIGMA                      ! Standard deviation of BACK
*.                              

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN
                                                                 
*   Begin an NDF context.                               
      CALL NDF_BEGIN

*   Indicate that the application is running.
      CALL MSG_BLANK(STATUS)
      CALL MSG_OUT(' ','ESP FASTMED running.',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Obtain an identifier for the NDF structure to be examined.       
      CALL NDF_ASSOC('IN','READ',NDF1,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Display the name of the NDF.
      CALL NDF_MSG('IN',NDF1)           
      CALL MSG_OUT(' ','Filename:   ^IN',STATUS)

*   See if the title component is defined. If so, display its value.
      CALL NDF_CMSG('TITLE',NDF1,'Title',STATUS)
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
      CALL NDF_STYPE('_REAL',NDF2,'Data',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Map the results NDF data array as _REAL values for updating.  
      CALL NDF_MAP(NDF2,'Data','_REAL','UPDATE',POINT2(1),ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Change the propogated title.
      CALL NDF_CPUT('ESP - FASTMED Image',NDF2,'TITLE',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine the background count value. 
      CALL PAR_GET0R('BACK',BACK,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine the standard deviation of the background count. 
      CALL PAR_GET0R('SIGMA',SIGMA,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF (SIGMA.LE.0.0) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The sigma value is too'/
     :                /' small.',STATUS)
         GOTO 9999
      END IF

*   Determine the width of the median filter. 
      CALL PAR_GET0I('WIDTH',WIDTH,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF (WIDTH.LT.2) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The width selected is too small.',STATUS)
         GOTO 9999
      END IF

*   Determine the radius of the filter to be used. If the size is too
*   large then display a message announcing the size used.
      RADIUS=WIDTH/2
      IF (RADIUS.GT.PRANGE(2)/2) THEN
          RADIUS=PRANGE(2)/2
          CALL MSG_SETI('SIZE',PRANGE(2))
          CALL MSG_OUT(' ','The size has been limited to ^SIZE'/
     :    /'x ^SIZE',STATUS)
          CALL MSG_BLANK(STATUS)
      END IF

*   Inform the user of what will be done, since the routine takes a 
*   long while to run.
      CALL MSG_BLANK(STATUS)
      CALL NDF_MSG('FOUT',NDF1)           
      CALL MSG_OUT(' ','Applying FASTMED to file: ^FOUT',STATUS)
      CALL NDF_MSG('FOUT',NDF2)           
      CALL MSG_OUT(' ','Results file will be:     ^FOUT',STATUS)
      CALL MSG_BLANK(STATUS)

*   Carry out the operation to median filter the image.
      CALL FAS1_FILT(ELEMS,%VAL(POINT1(1)),RADIUS,XMAX,YMAX,
     :               BACK,SIGMA,%VAL(POINT2(1)),STATUS)  
      IF (STATUS.NE.SAI__OK) GOTO 9999
     
 9999 CONTINUE

*   Un-map the NDF data arrays.
      CALL NDF_UNMAP(NDF1,'Data',STATUS)
      CALL NDF_UNMAP(NDF2,'Data',STATUS) 

*   End the NDF context.
      CALL NDF_END(STATUS)

      END      
                                                 

      SUBROUTINE FAS1_FILT(ELEMS,ARRAY,RADIUS,XMAX,YMAX,
     :                     BACK,SIGMA,ARRAY2,STATUS)  
*+
*  Name: 
*     FAS1_FILT

*  Purpose:
*     Create a median filtered equivalent of the source image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FAS1_FILT(ELEMS,ARRAY,RADIUS,XMAX,YMAX,BACK,SIGMA,ARRAY2,STATUS)  
      
*  Description:
*     Given a source image, the routine passes a median filter of user
*     defined size across the image. 
*
*     A histogram of the pixel values in the area of image surrounding
*     a pixel on the input image is constructed for each input image 
*     pixel in turn. From the histogram for a given pixel, the median count
*     value is determined. This value is subtracted from the value of the
*     pixel in the input image and the original background count added. 
*     The resultant count value is placed in the appropriate location of the 
*     output image.
*
*     The image is filtered all the way to the edge and corners of the 
*     image. This may lead to users needing to clip the size of the 
*     resultant image by WIDTH/2 pixels on each edge.

*  Arguments:
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image.
*     ARRAY(ELEMS) = REAL (Given)
*        The array containing the source NDF image count values.
*     RADIUS = INTEGER (Given)
*        Half the width of the square median filter.
*     XMAX = INTEGER (Given)
*        The length of the image x axis. Units pixels.
*     YMAX = INTEGER (Given)
*        The length of the y axis of the image. Units pixels.
*     BACK = REAL (Given)
*        Image background count values.
*     SIGMA = REAL (Given)
*        Standard deviation of the background pixel counts.
*     ARRAY2(ELEMS) = REAL (Returned)
*        The array that eventually contains the returned 'image'.
*     STATUS = INTEGER (Given and Returned) 
*        The global status.     

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     27-MAY-1993
*     (Original version)

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'fas_par'               ! FASTMED constants
                     
*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the data array
      INTEGER RADIUS                  ! Half the width of the filter
      INTEGER XMAX                    ! Length of the image x axis
      INTEGER YMAX                    ! Length of the image y axis
      REAL ARRAY(ELEMS)               ! Array containing input NDF data
      REAL BACK                       ! Background count value
      REAL SIGMA                      ! Standard deviation of BACK

*  Arguments Returned:
      REAL ARRAY2(ELEMS)              ! Image array containing the
                                      ! output image

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER ADD                     ! Image array address                 
      INTEGER HIST(FAS__HISZ)         ! Maximum number of elements in the  
                                      ! histogram used
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Index of most occupied histogram index
      INTEGER INDEX                   ! Histogram element into which a pixel
                                      ! count value should be binned
      INTEGER NPIX                    ! Number of pixels in the hitogram
      INTEGER PERC                    ! Percentage of the run over
      INTEGER RANGE                   ! Number of elements of the histogram in
                                      ! use
      INTEGER SUM                     ! Sum of the histogram bins
      INTEGER X                       ! X co-ordinate of the current filter
                                      ! centre position
      INTEGER XHIGH                   ! High X limit of filter for current
                                      ! filter centre
      INTEGER XLOW                    ! Low X limit of filter for current
                                      ! filter centre
      INTEGER XMIN                    ! Lowest x axis image co-ordinate
      INTEGER XV                      ! X value of part of the image being 
                                      ! added/subtracted from the histogram
      INTEGER Y                       ! Y co-ordinate of the current filter
                                      ! centre position
      INTEGER YHIGH                   ! High Y limit of filter for current 
                                      ! filter centre
      INTEGER YLOW                    ! Low Y limit of filter for current 
                                      ! filter centre
      INTEGER YMIN                    ! Lowest y axis image co-ordinate
      INTEGER YV                      ! Y value of part of the image being 
                                      ! added/subtracted from the histogram
      REAL OFFS                       ! Offset value used when working out
                                      ! which histogram element a pixel value 
                                      ! belongs in
      REAL V                          ! Temporary pixel count value
      REAL WIN1                       ! Lower limit of pixel values included 
                                      ! in the histogram
      REAL WIN2                       ! Upper limit of pixel values included 
                                      ! in the histogram

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set the co-ordinate of the lowest pixel in the x and y axes.
      XMIN=1
      YMIN=1

*   Set the upper and lower limits for a pixel count to be included 
*   in the histogram.
      WIN1=BACK-8.*SIGMA
      WIN2=BACK+8.*SIGMA

*   Check that the histogram size is not too big.
      IF ((WIN2-WIN1+1).GT.FAS__HISZ) WIN2=WIN1+REAL(FAS__HISZ)-1.
         

*   Set the number of elements of the histogram that will be used.
      RANGE=WIN2-WIN1+1

*   Offset value used when calculating the histogram element into which an
*   image pixel value should be assigned.
      OFFS=WIN1-1

*    Scan the filter across the image increasing X monotonically.
      X=XMIN
      DO WHILE (X.LE.XMAX)

*      Indicate that something is happening.
         IF (X.EQ.NINT(X/50.)*50) THEN
            PERC=NINT(X*100./XMAX)
            CALL MSG_SETI('PERC',PERC)
            CALL MSG_OUT(' ','Percentage done so far: ^PERC',STATUS)
        END IF

*      Define the x limits of the area being binned to the histogram.
         XLOW=X-RADIUS
         IF (XLOW.LT.XMIN) XLOW=XMIN
         XHIGH=X+RADIUS
         IF (XHIGH.GT.XMAX) XHIGH=XMAX
        
*      Scan the filter across the image increasing Y monotonically.
         Y=YMIN
         DO WHILE (Y.LE.YMAX)
    
*         Define the y limits of the area to be binned to the histogram.
            YLOW=Y-RADIUS
            IF (YLOW.LT.YMIN) YLOW=YMIN
            YHIGH=Y+RADIUS
            IF (YHIGH.GT.YMAX) YHIGH=YMAX
           
*         Adjust the histogram contents when away from the edge of the image.
            IF ((Y.GT.YMIN+RADIUS).AND.(Y.LT.YMAX-RADIUS)) THEN

*            Add a line of pixels to the histogram.
               ADD=(YHIGH-1)*XMAX
               DO 50 XV=XLOW,XHIGH    

*               Find pixel value and discard if the value is bad.
                  V=ARRAY(ADD+XV)
                  IF (V.NE.VAL__BADR) THEN
                     IF ((V.GE.WIN1).AND.(V.LE.WIN2)) THEN
                        INDEX=INT(V-OFFS)                    
                        HIST(INDEX)=HIST(INDEX)+1
                        NPIX=NPIX+1
                     END IF
                  END IF

 50            CONTINUE
   
*            Remove a line of pixels from the histogram.    
               ADD=(YLOW-1)*XMAX
               DO 60 XV=XLOW,XHIGH                      

*               Find pixel value and discard if the value is bad.
                  V=ARRAY(ADD+XV)
                  IF (V.NE.VAL__BADR) THEN
                     IF ((V.GE.WIN1).AND.(V.LE.WIN2)) THEN
                        INDEX=INT(V-OFFS)
                        HIST(INDEX)=HIST(INDEX)-1
                        NPIX=NPIX-1
                     END IF
                  END IF

 60            CONTINUE

               GOTO 666

            END IF
   
*         Adjust the histogram contents when near the bottom
*         edge of the image.
            IF ((Y.GT.YMIN).AND.(Y.LE.YMIN+RADIUS)) THEN

*            Add a line of pixels to the histogram.
               ADD=(YHIGH-1)*XMAX
               DO 70 XV=XLOW,XHIGH

*               Find pixel value and discard if the value is bad.
                  V=ARRAY(ADD+XV)
                  IF (V.NE.VAL__BADR) THEN
                     IF ((V.GE.WIN1).AND.(V.LE.WIN2)) THEN
                        INDEX=INT(V-OFFS)
                        HIST(INDEX)=HIST(INDEX)+1
                        NPIX=NPIX+1
                     END IF
                  END IF

 70            CONTINUE
  
               GOTO 666

            END IF
    
*         Adjust the histogram contents when near the bottom
*         edge of the image.
            IF (Y.GE.YMAX-RADIUS) THEN

*            Remove a line of pixels from the histogram.
               ADD=(YLOW-1)*XMAX
               DO 80 XV=XLOW,XHIGH                      

*               Find pixel value and discard if the value is bad.
                  V=ARRAY(ADD+XV)
                  IF (V.NE.VAL__BADR) THEN
                     IF ((V.GE.WIN1).AND.(V.LE.WIN2)) THEN
                        INDEX=INT(V-OFFS)
                        HIST(INDEX)=HIST(INDEX)-1
                        NPIX=NPIX-1
                     END IF
                  END IF

 80            CONTINUE

               GOTO 666

            END IF

*         Adjust the histogram when at the very bottom of the histogram.
            IF (Y.EQ.YMIN) THEN

*            Clear the histogram array.
               DO 90 I=1,RANGE
                  HIST(I)=0
 90            CONTINUE
                                   
*            Set number of pixels contributing to the histogram to zero.
               NPIX=0
                     
*            Calculate the initial histogram for this X value.
               DO 100 YV=YLOW,YHIGH
                  ADD=(YV-1)*XMAX
                  DO 110 XV=XLOW,XHIGH

*                  Find pixel value and discard if the value is bad.
                     V=ARRAY(ADD+XV)
                     IF (V.NE.VAL__BADR) THEN
                        IF ((V.GE.WIN1).AND.(V.LE.WIN2)) THEN
                           INDEX=INT(V-OFFS)
                           HIST(INDEX)=HIST(INDEX)+1
                           NPIX=NPIX+1
                        END IF
                     END IF

 110              CONTINUE
 100           CONTINUE
    
               GOTO 666
                    
            END IF

 666        CONTINUE

*         Sum the histogram bins (array elements) and stop
*         when the median element has been found.
*         Find the mean index of modal count.
            J=NPIX/2
            I=0
            SUM=0
            DO WHILE (SUM.LT.J)
               I=I+1
               SUM=SUM+HIST(I)
            END DO

*         Assign the value for the current pixel if the pixel was not bad.
            ADD=(Y-1)*XMAX+X
            IF (ARRAY(ADD).NE.VAL__BADR) THEN

*            Assign filtered value to pixel.
               ARRAY2(ADD)=ARRAY(ADD)-((I-1)+WIN1)+BACK
            
            ELSE

*            Set output image pixel to bad.
               ARRAY2(ADD)=VAL__BADR

            END IF
         
*         Move to the next pixel in the current column.
            Y=Y+1

         END DO

*      Move to the next row.
         X=X+1

      END DO

 9999 CONTINUE

      END

