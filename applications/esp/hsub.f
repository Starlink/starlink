      SUBROUTINE HSUB( STATUS )

*+
*  Name:
*     HSUB

*  Purpose:
*     A subroutine version of HISTPEAK for developers.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HSUB( STATUS )

*  Arguments:   
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description: 
*     A subroutine version of HISTPEAK that has been designed to be
*     easily transplanted into the users ADAM programs. It establishes 
*     the mode, median and other statistics for NDF image files. Calls a 
*     subroutine based upon a modified version of HISTPEAK to obtain 
*     values for the mode, skewness and kurtosis values for an NDF image.
*
*     The method employed to calculate the modal value from the 
*     count versus frequency histogram is user selected using
*     parameter TYPE.
*
*     The histogram may also be smoothed using a Normal distribution 
*     filter of integer radius SFACT. In general, values less than 3 
*     have very little effect. A value of 0 indicates no smoothing is to
*     be employed. 
 
*  Usage:
*     HSUB IN SFACT TYPE

*  ADAM Parameters:
*     IN = _NDF (Read)
*        The name of the NDF data structure/file that is to be 
*        examined.
*     SFACT = _INTEGER (Read)
*        The Gaussian smoothing filter radius requested. This may be:
*        - -1 to indicate that the application should automatically
*          assign a filter radius to apply to the histogram.
*        - 0 to indicate that the histogram should not be smoothed.
*        - >0 to indicate the radius of the Gaussian filter to use. 
*        Values greater than HSB__SFLIM (see include file HSUB_PAR)
*        are not allowed. The value returned is that actually  
*        employed. Units counts.
*     TYPE = _INTEGER (Read)
*        Allows the user to define which method is to be used to 
*        calculate the modal count value.
*        1 = raw histogram
*        2 = smoothed histogram
*        3 = extrapolate the length of chords through histogram peak
*            to zero length
*        4 = interpolation of data points near the histogram peak
*        0 = computer selection i.e. the highest method number that
*            didnt fail
*        A negative value is returned if the application cannot supply a
*        result using the method requested. The value returned for 
*        mode is the next best estimate.

*  Examples:
*     hsub in=ic3374 sfact=0 type=0
*
*        A histogram of the values in image IC3374 is constructed. 
*        The image is not smoothed (SFACT=0) and the results returned
*        correspond to the highest value (1-4) of TYPE that was 
*        obtainable.
*
*     hsub in=galaxy sfact=10 type=4
*
*        A histogram of the values in image GALAXY is used. The image 
*        is smoothed (SFACT=10) using a Gaussian filter of radius 10. 
*        The results required are those for the smoothed histogram 
*        only.
*
*     hsub in=forn4 sfact=6 type=3
*
*        A histogram of the values in image FORN4 is used. The image 
*        is smoothed using a gaussian filter of radius 6 and the 
*        results returned those for the projected mode value.

*  Implementation Status:
*     The current version will not accept a pixel value range greater 
*     than the largest integer value possible. 

*  Notes: 
*     HSUB should be viewed as a coding example for users wishing
*     to incorporate the functions of HISTPEAK into their own 
*     programs.
*
*     This application is intended to form the basis of a user 
*     program requiring image statistics. The user requiring other
*     data from the application will need to modify subroutines HSUB
*     and HISTPEA2 so that the desired parameters (say mean or median)
*     are passed between them.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-Nov-1992 (GJP)
*     (Original version)
*     29-Jan-1993 (GJP)
*     Bug in the interpolation method corrected.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'hsb_par'               ! HSUB system variables
                     
*  Status:     
      INTEGER STATUS                  ! Global status

*  Local Variables:
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER NDF1                    ! NDF identifier
      INTEGER NUPOI                   ! Number of points used in the 
                                      ! Calculation of mode
      INTEGER POINT1(10)              ! Pointer to NDF array to be used
      INTEGER SFACT                   ! Gaussian filter radius requested
      INTEGER TEMP                    ! Temporary storage
      INTEGER TYPE                    ! Value indicating whether the final
                                      ! estimate for mode given is to be from:
                                      ! 0 - get best available
                                      ! 1 - raw histogram
                                      ! 2 - smoothed histogram
                                      ! 3 - projecting peak chords
                                      ! 4 - interpolating smoothed histogram 
      DOUBLE PRECISION KURTO          ! Image pixel count kurtosis
      DOUBLE PRECISION MODEV          ! Estimate of the image mode value 
      DOUBLE PRECISION SKEWN          ! Image skewness value
      DOUBLE PRECISION STAND          ! Estimate of the standard deviation
                                      ! of the pixel values or the
                                      ! background count standard deviation
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*********************************************************************

*   Example usage. Obtain a whole image and work on that. Could be replaced
*   with anything that gets a pointer to a data array and supplies the array
*   size as POINT1(1) and ELEMS respectively. 

*   Begin an NDF context.                               
      CALL NDF_BEGIN

*   Show that the application is running.
      CALL MSG_BLANK(STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      CALL MSG_OUT(' ','ESP HSUB running.',STATUS)

*   Obtain an identifier for the NDF structure to be examined.       
      CALL NDF_ASSOC('IN','READ',NDF1,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine the smoothing filter radius required. SFACT=-1 is automatic,
*   SFACT=0 is none. Upper limit fixed by the HSB__SFLIM.
      CALL PAR_GET0I('SFACT',SFACT,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF (SFACT.GT.HSB__SFLIM) THEN
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','The value selected exceeded the maximum'/
     :                /' permitted.',STATUS)
         CALL MSG_OUT(' ','The maximum value has been employed.',
     :                STATUS)
         SFACT=HSB__SFLIM
      END IF

*   Determine the calculation method to be employed. TYPE=0 is automatic,
      CALL PAR_GET0I('TYPE',TYPE,STATUS)

*   Map the source NDF data array as _REAL values for reading.
      CALL NDF_MAP(NDF1,'Data','_REAL','READ',POINT1(1),ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Show whats going on.
      CALL MSG_BLANK(STATUS)
      CALL MSG_OUT(' ','HSUB calculating.',STATUS)

*******************************************************************
*   MAIN SUBROUTINE

*   Call the modified version of HISTPEAK.
      CALL HISTPEA2(POINT1,ELEMS,SFACT,TYPE,MODEV,STAND,KURTO,
     :              SKEWN,NUPOI,STATUS)
                    


 9999 CONTINUE

*   Display the results. Crude cos its only used  to test the example.

*   Modal value and standard deviation
      CALL MSG_FMTD('MODEV','F8.1',MODEV)
      CALL MSG_OUT(' ','Mode value:               ^MODEV',STATUS)
      CALL MSG_FMTD('STAND','F8.1',STAND)
      CALL MSG_OUT(' ','Std. dev.:                ^STAND',STATUS)
      CALL MSG_BLANK(STATUS)

*   Kurtosis and skewness.
      CALL MSG_FMTD('KURTO','F8.3',KURTO)
      CALL MSG_OUT(' ','Kurtosis:                 ^KURTO',STATUS)
      CALL MSG_FMTD('SKEWN','F8.3',SKEWN)
      CALL MSG_OUT(' ','Skewness:                 ^SKEWN',STATUS)
      CALL MSG_BLANK(STATUS)

*   Number of points used etc.
      CALL MSG_FMTI('ELEMS','I8',ELEMS)
      CALL MSG_OUT(' ','Number of points given:   ^ELEMS',STATUS)
      CALL MSG_FMTI('NUPOI','I8',NUPOI)
      CALL MSG_OUT(' ','Number of points used     ^NUPOI',STATUS)
      CALL MSG_FMTI('SFACT','I8',SFACT)
      CALL MSG_OUT(' ','Filter radius used:       ^SFACT',STATUS)
      CALL MSG_BLANK(STATUS)

*   Type of modal value found.
      CALL MSG_FMTI('TYPE','I8',TYPE)
      CALL MSG_OUT(' ','Mode type 1-4:            ^TYPE',STATUS)
      TEMP=STATUS
      CALL MSG_FMTI('STATS','I8',TEMP)
      CALL MSG_OUT(' ','Global status:            ^STATS',STATUS)

********************************************************************

*   Close down resources used for the example

*   Un-map/annul the source NDF data array. 
      CALL NDF_UNMAP(NDF1,'Data',STATUS)
      CALL NDF_ANNUL(NDF1,STATUS)                          

*   End the NDF context.
      CALL NDF_END(STATUS)                              

********************************************************************

      END 


      SUBROUTINE HISTPEA2(POINT1,ELEMS,SFACT,TYPE,MODEV,STAND,
     :                    KURTO,SKEWN,NUPOI,STATUS)
*+                        
*  Name:
*     HISTPEA2

*  Purpose:
*     Establish the mean, mode, median and other statistics 
*     for NDF image files.  

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL HISTPEA2(POINT1,ELEMS,SFACT,TYPE,MODEV,STAND,
*                    KURTO,SKEWN,NUPOI,STATUS)
                    
*  Arguments:   
*     POINT1(10) = INTEGER (Given)
*        Pointer to the NDF data array.
*     ELEMS = INTEGER (Given)
*        Total number of image pixels.
*     SFACT = INTEGER (Given and Returned)
*        The radius of the Gaussian filter requested and used.
*        0 means no filtering. -1 is automatic. Otherwise units pixels.
*     TYPE = INTEGER (Given and Returned)
*        Selects the method used to find the modal count value. If the
*        initial value of TYPE is set to 0 the subroutine provides the best
*        mode estimate available. If it is set in the range 1 - 4 the method
*        requested is provided. If the method requested is unavailable
*        the next best value is returned and TYPE returns with the value
*        set to -1. If all the methods (up to and including the method
*        required) fail then TYPE returns as -2.
*        0 = Use automatic get highest type value possible
*        1 = raw histogram used
*        2 = smoothed histogram used
*        3 = extrapolation to zero length of chords through histogram peak
*        4 = interpolation of smoothed histogram
*     MODEV = DOUBLE PRECISION (Returned)
*        The best modal count value estimate. Units counts.
*     STAND = DOUBLE PRECISION (Returned)
*        The standard deviation value or the standard deviation value
*        (if available). Units counts.
*     KURTO = DOUBLE PRECISION (Returned)
*        The pixel count distribution kurtosis.
*     SKEWN = DOUBLE PRECISION (Returned)
*        The pixel count distribution skewness.
*     NUPOI = INTEGER (Returned)
*        The number of non-bad pixels used. Units pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description: 
*     Allows the user to input the name of an NDF image file and 
*     then constructs an image count value versus occurence histogram.
*     This is used to allow count median, mode, kurtosis, standard 
*     deviation, background standard deviation and skewness values to be estimated. 
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
*       the background standard deviation.  
*    
*     The value of greatest probable accuracy is returned to the calling
*     subroutine.
*     
*     Both the standard deviation of pixel count values and the background
*     count standard deviation value are generated:
*
*     The value standard deviation value is returned if the standard deviation value
*     was not calculated.

*  Implementation Status:
*     The current version will not accept a pixel value range greater 
*     than the largest integer value possible. This will be corrected 
*     in a later version.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-Nov-1992 (GJP)
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
      INCLUDE 'hsb_par'               ! HSUB system variables
                     
*  Status:     
      INTEGER STATUS                  ! Global status

*  Arguments Given:   
      INTEGER POINT1(10)              ! Pointer to the NDF data array.
      INTEGER ELEMS                   ! Total number of image pixels.

*  Arguments Given and Returned:
      INTEGER SFACT                   ! The radius of the Gaussian filter 
                                      ! requested and used.
    
*  Arguments Returned:
      INTEGER TYPE                    ! Method used to find the modal 
                                      ! count value returned.
      INTEGER NUPOI                   ! Number of non-bad pixels used.
      DOUBLE PRECISION MODEV          ! Best modal count value estimate. 
      DOUBLE PRECISION STAND          ! Best standard deviation 
                                      ! value.
      DOUBLE PRECISION KURTO          ! Pixel count distribution kurtosis.
      DOUBLE PRECISION SKEWN          ! Pixel count distribution skewness.

*  Local Variables:      
      INTEGER BARSIZ                  ! Size of the of the bin arrays
                                      ! used
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Temporary variable
      INTEGER NUMBER                  ! The number of pixels used
      INTEGER POINT2(10)              ! Pointer to the memory allocated 
                                      ! for the unsmoothed histogram
      INTEGER POINT3(10)              ! Pointer to the memory allocated
                                      ! for the smoothed histogram
      INTEGER SFACTA                  ! The actual filter radius used
      INTEGER UNUPIX                  ! Number of unused pixels in the
                                      ! source NDF
      REAL BINWID                     ! Bin width used when finding the
                                      ! median and mode values (not
                                      ! 1 when count range > HSB__BINLI)
                                      ! Units counts
      REAL HIGH                       ! Highest pixel value found in the
                                      ! NDF data
      REAL LOW                        ! Binning arrays origin offset and
                                      ! lowest value found in image
      DOUBLE PRECISION ADEV           ! Absolute deviation of the 
                                      ! NDF pixels used
      DOUBLE PRECISION KURT           ! Kurtosis of NDF pixel values 
      DOUBLE PRECISION MEAN           ! Mean pixel value in the NDF data
      DOUBLE PRECISION MEDIAN         ! Median value of the NDF pixels
      DOUBLE PRECISION MODE(4)        ! Mode values for the NDF pixels
      DOUBLE PRECISION PEAKV(3)       ! Estimates of the histogram
                                      ! array peak height
      DOUBLE PRECISION SDEV(2)        ! Standard deviation
      DOUBLE PRECISION SKEW           ! Skewness of the NDF pixel values 
      DOUBLE PRECISION VARI           ! Variance of the NDF pixel values   
                                                          
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Check that the method type specified is within the range 1 - 4.
      IF ((TYPE.LT.0).OR.(TYPE.GT.4)) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Illegal method type selected.'
     :                    ,STATUS)
         GOTO 9999
      END IF 

*   Set default values for absolute deviation etc (see above). 
      ADEV=0.0                      
      UNUPIX=0
      HIGH=0.0                      
      KURT=0.0                      
      LOW=0.0                       
      MEAN=0.0                      
      MEDIAN=0.0                                 
      DO 1 I=1,4
         MODE(I)=0.0
 1    CONTINUE     
      NUMBER=0 
      PEAKV(1)=0.0
      PEAKV(2)=0.0
      PEAKV(3)=0.0
      SDEV(1)=0.0
      SDEV(2)=0.0
      SFACTA=0
      SKEW=0.0                       
      VARI=0.0         
         
*   Call routine to find the highest, lowest and mean
*   value of those in the data array.
      CALL HSB1_HILOA(ELEMS,%VAL(POINT1(1)),
     :                STATUS,UNUPIX,HIGH,MEAN,LOW,NUMBER)               
      IF (STATUS.NE.SAI__OK) GOTO 9999
            
*   Determine the size of the array to be used for binning the image 
*   pixel count values. The bin width is also set. The size of the 
*   binning array is not allowed to exceed HSB__BINLI.
      IF (HIGH-LOW+1.GT.HSB__BINLI) THEN
         BINWID=(HIGH-LOW+1.)/REAL(HSB__BINLI)
         BINWID=BINWID*1.01
         BARSIZ=HSB__BINLI
      ELSE
         BINWID=1.0
         BARSIZ=HIGH-LOW+1
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
      CALL HSB1_MOMDE(ELEMS,NUMBER,%VAL(POINT1(1)),
     :                MEAN,STATUS,ADEV,VARI,SDEV,SKEW,KURT) 
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Call routine to find the median and mode of the values in
*   the NDF array data.
      CALL HSB1_MEDMO(TYPE,ELEMS,%VAL(POINT1(1)),POINT2,POINT3,BARSIZ,
     :                BINWID,LOW,ADEV,SFACT,
     :                NUMBER,STATUS,SDEV,%VAL(POINT2(1)),
     :                %VAL(POINT3(1)),MEDIAN,PEAKV,SFACTA,MODE)
      IF (STATUS.NE.SAI__OK) GOTO 9999  
                                           
*   Return to the calling routine..
 9999 CONTINUE

*   Free the dynamic array space of the histogram arrays.
      CALL PSX_FREE(POINT2(1),STATUS)
      CALL PSX_FREE(POINT3(1),STATUS)                           

*   Setup the values to be returned to the calling subroutine.
      KURTO=KURT
      SKEWN=SKEW
      NUPOI=NUMBER
      MODEV=0.0
      SFACT=SFACTA
      IF (TYPE.EQ.0) THEN
*      Get the results generated by the best method available.
         DO 200 I=1,4
            IF (ABS(MODE(I)).GT.HSB__VSMAL) THEN 
               MODEV=MODE(I)
               TYPE=I
            END IF
 200     CONTINUE
         STAND=SDEV(1)
         IF (TYPE.EQ.4) STAND=SDEV(2)
      ELSE
*      Get the result for the method requested. Failing that, take the best 
*      available and set and set TYPE
*      to -1 or (if it all failed) -2.
         IF (ABS(MODE(TYPE)).GT.HSB__VSMAL) THEN
            MODEV=MODE(TYPE)
            STAND=SDEV(1)
            IF (TYPE.EQ.4) STAND=SDEV(2)
         ELSE 
*         Get the next best value available. Set TYPE value returned
*         to indicate total failure (-2) or failure of chosen
*         calculation method (-1).
            J=-2
            DO 201 I=1,TYPE
               IF (ABS(MODE(I)).GT.HSB__VSMAL) THEN 
                  MODEV=MODE(I)
                  J=I
               END IF
 201        CONTINUE
            TYPE=J
            IF (TYPE.GT.0) THEN 
               STAND=SDEV(1)
               IF (TYPE.EQ.4) STAND=SDEV(2)
               TYPE=-1
            END IF
         END IF 
      END IF

      END
      

      SUBROUTINE HSB1_CHORD(HIVAL,LOVAL,MODEC,SMOBAR,LOW,ADEV,
     :                      BARSIZ,BINWID,STATUS,NUMDAT,HEIG,X1,Y1)
*+                        
*  Name:
*     HSB1_CHORD

*  Purpose:
*     Estimate histogram mode by examining chords through peak. 
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HSB1_CHORD(HIVAL,LOVAL,MODEC,SMOBAR,LOW,ADEV,
*                     BARSIZ,BINWID,STATUS,NUMDAT,HEIG,X1,Y1)
    
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
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*     NUMDAT = INTEGER (Returned)
*        The number of legal histogram chords obtained.
*     HEIG(HSB__CHORM) = REAL (Returned)
*        The height at which the chord through the histogram occurs.
*     X1(HSB__CHORM) = REAL (Returned)
*        Length of chord through the histogram.      
*     Y1(HSB__CHORM) = REAL (Returned)
*        Midpoint x index of chords through the histogram.

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
      INCLUDE 'hsb_par'               ! HSUB system variables
                                                                      
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
      REAL HEIG(HSB__CHORM)           ! The histogram values at which 
                                      ! chords were taken through the
                                      ! histogram                  
      REAL X1(HSB__CHORM)             ! Length of the chord through
                                      ! the histogram
      REAL Y1(HSB__CHORM)             ! X index of midpoint of chord
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


      SUBROUTINE HSB1_GAUJO(INPMAT,STATUS,VECTOR,DETERM)
*+
*  Name:
*     HSB1_GAUJO

*  Purpose:                                                          
*     Inverts a matrix containing preprocessed histogram values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HSB1_GAUJO(A,STATUS,B,DETERM)

*  Description:
*     Employs the very stable Gauss-Jordan with optimised 
*     array pivot elements method to invert a matrix. The matrix to be
*     inverted (INPMAT) is received in a form preprocessed.
*
*     On completion, the array INPMAT contains
*     the inverted matrix and the vector array VECTOR contains the 
*     coefficients of the parabolic equation. 
* 
*     If the routine suceeds, the determinant (DETERM) of the array 
*     is significantly non-zero.

*  Arguments:
*     INPMAT(3,3) = REAL (Given and Returned)
*        The matrix to be inverted. The inverted matrix is returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*     VECTOR(3) = REAL ARRAY (Given and Returned)
*        Preprocessed count values are given. Values for the parabola
*        coefficients are returned.
*     DETERM = REAL (Returned)
*        The determinant of the inverted array.

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
      INCLUDE 'hsb_par'               ! HSUB system variables

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
         IF (DETERM.LT.HSB__VSMAL) THEN   
            DETERM=0.0
            CALL MSG_OUT(' ','WARNING!!!',STATUS)
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
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','Unable to complete the parabolic '//
     :                    'interpolation.',STATUS)
         GOTO 9999
      END IF

 9999 CONTINUE

      END



      SUBROUTINE HSB1_HILOA(ELEMS,ARRAY,
     :                      STATUS,UNUPIX,HIGH,MEAN,LOW,NUMBER)               
*+
*  Name:
*     HSB1_HILOA

*  Purpose: 
*     Find the highest and lowest count values in an image array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HSB1_HILOA(ELEMS,ARRAY,
*                     STATUS,UNUPIX,HIGH,MEAN,LOW,NUMBER)               

*  Description:
*     Establish the highest and lowest count values found in the image
*     ARRAY. The mean value is found also as is the number of pixels 
*     that are bad.

*  Arguments:                                     
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image. Units pixels.
*     ARRAY(ELEMS) = INTEGER (Given)
*        An array containing the image pixel counts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
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
      INCLUDE 'hsb_par'               ! HSUB system variables
                                                                      
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
      HIGH=VAL__MINR
      LOW= VAL__MAXR
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
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Pixel range too large to handle.',STATUS)
         GOTO 9999
      END IF

*   Check that there are not too many bad pixels.
      IF (UNUPXR.GE.REAL(VAL__MAXI)-2.) THEN 
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Too many data points are bad.',STATUS)
         GOTO 9999
      END IF

      UNUPIX=NINT(UNUPXR)
      NUMBER=ELEMS-UNUPIX

*   Check that enough data points are available.
      IF (NUMBER.LT.3) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Too few data points have been '//
     :                    'specified.',STATUS)
         GOTO 9999
      END IF

*   Calculate the average data array value.
      IF (NUMBER.GT.0) MEAN=SUM/REAL(NUMBER)         

*   Check the number of unused data points present in the array and
*   set STATUS if less than 3 data points are present.
      IF (NUMBER.LT.3) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Too few points are present for an accurate'//
     :                    ' estimate of the mode to be made.',STATUS)
         GOTO 9999
      END IF

*   Check if the range of values found is less than 3 and
*   set STATUS if true. 
      IF (HIGH-LOW+1.0.LT.3.0) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The range of values found is less than 3!'//
     :                    ' Consequently, accurate modes are not'//
     :                    ' available.',STATUS)
         GOTO 9999
      END IF
  
 9999 CONTINUE

      END


      SUBROUTINE HSB1_MEDMO(TYPE,ELEMS,ARRAY,POINT2,POINT3,BARSIZ,
     :                      BINWID,LOW,ADEV,SFACT,
     :                      NUMBER,STATUS,SDEV,BARRAY,SMOBAR,
     :                      MEDIAN,PEAKV,SFACTA,MODE)
*+
*  Name:
*     HSB1_MEDMO

*  Purpose:
*     Creates a histogram array from the image array and finds its
*     mode.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HSB1_MEDMO(TYPE,ELEMS,ARRAY,POINT2,POINT3,BARSIZ,
*                     BINWID,LOW,ADEV,SFACT,
*                     NUMBER,STATUS,SDEV,BARRAY,SMOBAR,
*                     MEDIAN,PEAKV,SFACTA,MODE)
                                   
*  Description:
*     Places the values from the mapped image array into a binning
*     array. The array is used as a histogram. The routine then
*     determines values for the peak height, mode, median and standard
*     deviation of the histogram.

*  Arguments:              
*     TYPE = INTEGER (Given)
*        Method to be used to calculate mode vale.
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
*        otherwise any number less than HSB__SFLIM (see include file)
*        may be used. Units counts.
*     NUMBER = INTEGER (Given)
*        The number of pixels that are non-bad. Units pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*     SDEV(2) = DOUBLE PRECISION (Given and Returned)
*        Standard deviation of the image pixel count distribution
*        and the background count standard deviation. Units counts.
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
      INCLUDE 'hsb_par'               ! HSUB system constants

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER BARSIZ                  ! Size of the binning arrays used 
      INTEGER NUMBER                  ! The number of pixels to be used
      INTEGER POINT2(1)               ! Pointer to the binning array
      INTEGER POINT3(1)               ! Pointer to the smoothed bin array
      INTEGER SFACT                   ! Requested radius for the Gaussian
                                      ! filter used to smooth the 
                                      ! histogram
      INTEGER TYPE                    ! Defines the method of calculation
                                      ! for mode to be chosen
      REAL ARRAY(ELEMS)               ! Array containing the image data
      REAL BINWID                     ! Width of the bins used to find 
                                      ! median and mode (only differs
                                      ! from 1 when count range exceeds
                                      ! HSB__BINLI)
      REAL LOW                        ! Binning arrays origin offset and
                                      ! lowest value found in image
      DOUBLE PRECISION ADEV           ! Absolute deviation of data 

*  Arguments Given and Returned:
      DOUBLE PRECISION SDEV(2)        ! Standard deviation

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
                                      ! subroutine HSB1_LINRE
      REAL CONS                       ! Constant term of linear
                                      ! relationship fitted by
                                      ! subroutine HSB1_LINRE
      REAL GRAD                       ! Gradient term of linear 
                                      ! relationship fitted by 
                                      ! subroutine HSB1_LINRE
      REAL HEIG(HSB__CHORM)           ! The values at which chords 
                                      ! (slices) were taken through the
                                      ! histogram
      REAL VALUE1                     ! Temporary storage variable
      REAL X1(HSB__CHORM)             ! X value array passed to
                                      ! subroutine HSB1_LINRE
      REAL Y1(HSB__CHORM)             ! Y value array passed to
                                      ! subroutine HSB1_LINRE
      DOUBLE PRECISION HALF           ! Half the number of non-bad
                                      ! in the binning arrays 
      DOUBLE PRECISION HIVAL          ! Highest value found in the bin
                                      ! array
      DOUBLE PRECISION LOVAL          ! Lowest value found in the bin
                                      ! array
      DOUBLE PRECISION SFTOT          ! Total of smoothing factors used
      DOUBLE PRECISION SMOFAC(-HSB__SFLIM:HSB__SFLIM) 
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

*     Bypass the rest of this subroutine since the required mode 
*     type has been derived.
      IF ((TYPE.LT.2).AND.(TYPE.NE.0)) GOTO 9999

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

*      Calculate the filter radius
         SFACTA=NINT((HIIND-LOIND)/2.)

*      Impose an upper limit.
         IF (SFACTA.GT.HSB__SFLIM) SFACTA=HSB__SFLIM

      ELSE
     
*      Set the filter radius and impose an upper limit.
         SFACTA=SFACTA/BINWID
         IF (SFACTA.GT.HSB__SFLIM) SFACTA=HSB__SFLIM
         
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
            SMOFAC(I)=1./SQRT(2.*HSB__PIVAL)/(SFACTA/3.)
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
*   than HSB__BINLI.
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

*     Bypass the rest of this subroutine since the required mode 
*     type has been derived.
      IF ((TYPE.LT.3).AND.(TYPE.NE.0)) GOTO 9999

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
      CALL HSB1_CHORD(HIVAL,LOVAL,MODEC,%VAL(POINT3(1)),LOW,ADEV,
     :                BARSIZ,BINWID,STATUS,NUMDAT,HEIG,X1,Y1)
      IF (STATUS.NE.SAI__OK) GOTO 9999
           
*   Determine the linear relationship between histogram width
*   and value at which the width was determined. Extrapolate to zero
*   width (peak) and thereby estimate a mode value.
      IF (NUMDAT.GT.2) THEN 
         CALL HSB1_LINRE(X1,Y1,NUMDAT,STATUS,GRAD,CONS)
         IF (NUMDAT.GT.2) MODE(3)=CONS
      END IF

*     Bypass the rest of this subroutine since the required mode 
*     type has been derived.
      IF ((TYPE.LT.4).AND.(TYPE.NE.0)) GOTO 9999

*   Set up the data for matrix inversion to provide 
*   an interpolated value for the mode.
      CALL HSB1_PARA(ADEV,%VAL(POINT3(1)),BARSIZ,LOW,BINWID,
     :               MODE,SDEV,PEAKV,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

 9999 CONTINUE
                    
      END

 
      SUBROUTINE HSB1_LINRE(X1,Y1,NUMDAT,STATUS,GRAD,CONS)
*+
*  Name:
*     HSB1_LINRE

*  Purpose:
*     Determines a least squares linear fit for data in arrays X and Y. 

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HSB1_LINRE(X1,Y1,NUMDAT,STATUS,GRAD,CONS)

*  Description:
*     Uses a normal least squares method to determine the coefficients
*     of a linear fit to the contents of arrays X and Y.

*  Arguments:
*     X1(HSB__CHORM) = REAL (Given)
*        The x values to be used.
*     Y1(HSB__CHORM) = REAL (Given)
*        The y values to be used.
*     NUMDAT = INTEGER (Given)
*        The number of data (X/Y) pairs to be fitted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*     GRAD   = REAL (Returned)
*        The gradient value for the linear fit.
*     CONS   = REAL (Returned)
*        The constant value for the linear fit.

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
      INCLUDE 'hsb_par'               ! HSUB system variables
                     
*  Arguments Given:
      INTEGER NUMDAT                  ! Number of data points
      REAL X1(HSB__CHORM)             ! Data points X1 value
      REAL Y1(HSB__CHORM)             ! Data points Y1 value

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
      IF (ABS(TOT1).GT.HSB__VSMAL) THEN
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
       

      SUBROUTINE HSB1_MOMDE(ELEMS,NUMBER,ARRAY,
     :                      MEAN,STATUS,ADEV,VARI,SDEV,SKEW,KURT) 
*+
*  Name:
*     HSB1_MOMDE

*  Purpose:
*     Finds the deviations, skewness and kurtosis of pixels in an image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HSB1_MOMDE(ELEMS,NUMBER,ARRAY,
*                     MEAN,STATUS,ADEV,VARI,SDEV,SKEW,KURT) 
                  
*  Description:
*     Finds values for the absolute deviation, standard deviation, 
*     variance, skewness and kurtosis of pixels in an image.

*  Arguments:                      
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image. Units pixels.
*     NUMBER = INTEGER (Given)
*        The number of image pixels to be used. Units pixels.
*     ARRAY(ELEMS) = INTEGER (Given)
*        Array containing the image data.
*     MEAN = DOUBLE PRECISION (Given)
*        Mean of the values found in the image pixels. Units counts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*     ADEV = DOUBLE PRECISION (Returned)
*        Absolute deviation of the image pixels count distribution.
*        Units counts.
*     VARI = DOUBLE PRECISION (Returned)                             
*        Variance of the image pixel count distribution.
*     SDEV(2) = DOUBLE PRECISION (Returned)
*        Standard deviation of the image pixel count distribution
*        and the background standard deviation. Units counts.
*     SKEW = DOUBLE PRECISION (Returned)
*        Skewness of the image pixel count distribution.
*     KURT = DOUBLE PRECISION (Returned)
*        Kurtosis of the image pixel count distribution.

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
      INCLUDE 'hsb_par'               ! HSUB system variables

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the data 
      INTEGER NUMBER                  ! The number of pixels to be used
      REAL ARRAY(ELEMS)               ! Array containing image data
      DOUBLE PRECISION MEAN           ! Average value in the image 
                                      ! array       
             
*  Arguments Returned:
      DOUBLE PRECISION ADEV           ! Absolute deviation of array 
      DOUBLE PRECISION KURT           ! Kurtosis of array values
      DOUBLE PRECISION SDEV(2)        ! Standard deviation
      DOUBLE PRECISION SKEW           ! Skewness of the array values 
      DOUBLE PRECISION VARI           ! Variance of array values

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables: 
      INTEGER I                       ! Loop variable
      REAL VALUE                      ! Temporary storage variable
      DOUBLE PRECISION P2             ! Temporary storage variable
      DOUBLE PRECISION P3             ! Temporary storage variable
      DOUBLE PRECISION P4             ! Temporary storage variable

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN
       
*   Look through the array to gather information for calculating the
*   standard deviation, absolute deviation, variance, skewness and
*   kurtosis of the distribution.
      DO 212 I=1,ELEMS
         VALUE=ARRAY(I)
         IF (VALUE.NE.VAL__BADR) THEN 
*         Absolute deviation (first moment of deviation). 
            VALUE=VALUE-MEAN         
            ADEV=ADEV+ABS(VALUE)
            P2=VALUE*VALUE
            P3=P2*VALUE
            P4=P3*VALUE
*         Variance. 
            VARI=VARI+P2
*         Skewness. 
            SKEW=SKEW+P3
*         Kurtosis.
            KURT=KURT+P4
         END IF
 212  CONTINUE  

*   Derive values from the previous summations for absolute deviation,
*   variance, standard deviation, skewness and kurtosis.     
      IF (NUMBER.GE.2) THEN
         ADEV=ADEV/REAL(NUMBER)
         VARI=VARI/REAL(NUMBER-1)
         SDEV(1)=SQRT(VARI)
         SKEW=SKEW/(REAL(NUMBER)*SDEV(1)**3)
         KURT=KURT/(REAL(NUMBER)*(VARI**2))-3
      ELSE
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Too few points for an accurate estimate'//
     :                    ' of the standard. deviation etc.',STATUS)
         GOTO 9999
      END IF
      
 9999 CONTINUE

      END


      SUBROUTINE HSB1_PARA(ADEV,SMOBAR,BARSIZ,LOW,BINWID,
     :                     MODE,SDEV,PEAKV,STATUS)
*+                        
*  Name:
*     HSB1_PARA

*  Purpose:
*     Estimate histogram mode by parabolic fitting of the histogram peak. 
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HSB1_PARA(ADEV,SMOBAR,BARSIZ,LOW,BINWID,
*                    MODE,SDEV,PEAKV,STATUS)
    
*  Description:                                        
*     Logarithmically transforms the values of the smoothed
*     histogram array (SMOBAR) and then 'fits' a parabola to the
*     points near to the peak. The fitting is carried out by routine
*     HSB1_GAUJO but the data is passed in array VECTOR and 
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
      INCLUDE 'hsb_par'               ! HSUB system variables
                                                                      
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
      DOUBLE PRECISION SDEV(2)        ! Standard deviation
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
                                      ! subroutine HSB1_GAUJO 
      REAL VECTOR(3)                  ! Vector array in which parabola 
                                      ! coefficients are returned from 
                                      ! subroutine HSB1_GAUJO
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
      DO WHILE (((ABS(DETERM).LT.HSB__VSMAL).
     :            OR.(VECTOR(3).GE.0.0)).AND.
     :                (RANGE.LE.2.0*ADEV/BINWID))

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
               IF ((SMOBAR(J).GT.HSB__VSMAL).AND.
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
     :       CALL HSB1_GAUJO(INPMAT,STATUS,VECTOR,DETERM)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Increase the range of points to be used.
         RANGE=RANGE*1.05

      END DO

*   Assign interpolated mode, peak and standard deviation values.
      IF ((RANGE.LE.2.*ADEV/BINWID).AND.(ABS(DETERM).GT.HSB__VSMAL)
     :     .AND.(VECTOR(3).NE.0.0)) THEN
         MODE(4)=MODE(2)-VECTOR(2)/2./VECTOR(3)*BINWID
         SDEV(2)=SQRT(-1./VECTOR(3)/2.)*BINWID
         PEAKV(3)=EXP(VECTOR(1)-(VECTOR(2)/2.)**2/VECTOR(3))
      ELSE
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','No sensible interpolated mode.',STATUS)
         GOTO 9999
      END IF

 9999 CONTINUE
     
      END 
     

