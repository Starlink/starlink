      SUBROUTINE HISTPEAK( STATUS )
*+
*  Name:
*     HISTPEAK

*  Purpose:
*     Establish the mean, mode, median and other statistics 
*     for NDF image files.  

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HISTPEAK( STATUS )

*  Arguments:   
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description: 
*     Allows the user to input the name of an NDF image file and 
*     then constructs an image count value versus occurence
*     histogram. This is used to allow count median, mode, kurtosis, 
*     standard deviation, background count standard deviation and skewness 
*     values to be estimated. 
*
*     The user may also select which parts of the image are to be 
*     used. The options implemented are:
*     - the whole image.
*     - areas defined using an ARD file.
*
*     Both options exclude bad valued points from the calculations.
*
*     Four estimates of the modal value are generated:
*     - unsmoothed histogram mode.
*     - smoothed histogram mode.
*     - projected mode. Calculated by extrapolating the lengths of a 
*       series of chords through the peak to zero length and 
*       determining the count value at which this occurs.
*     - interpolated mode. Calculated by assuming a Normal form 
*       for the histogram peak and 'fitting' a function to it.
*       The function is then used to provide both a modal value and
*       the background count standard deviation.  
*     
*     An estimate of the standard deviation of pixel count values
*     and the background count standard deviation are generated.

*  Usage:
*     HISTPEAK IN USE SFACT DEVICE [ARDFIL] 

*  ADAM Parameters:
*     ARDFIL = _CHAR (Read)
*        The name of the ARD file containing a description of
*        the parts of the image to be ignored.
*     DEVICE = _DEVICE (Read)
*        The name or number of the graphics display type to be
*        used when displaying the histogram. ! may be used if
*        graphics are not required.
*     IN = _NDF (Read)
*        The name of the NDF data structure/file that is to be 
*        examined.
*     LOW = _REAL (Write)
*        The lowest pixel count value found in the parts of the
*        image that were used. Units counts.
*     HIGH = _REAL (Write)
*        The highest pixel count value found in the parts of the 
*        image that were used. Units counts.
*     KURT = _DOUBLE (Write)
*        The value of pixel count kurtosis calculated for the good
*        pixels found in the parts of the image used.  
*     MEAN = _DOUBLE (Write)
*        The mean pixel count value calculated using the pixels found
*        in the parts of the image used. Units counts.
*     MEDIAN = _DOUBLE (Write)
*        The median pixel count value calculated using the pixels
*        found in the parts of the image used. Units counts.
*     MODE = DOUBLE (Write) 
*        The modal value of the unsmoothed histogram generated
*        when using only pixels from the parts of the image requested.
*     MODEI = _DOUBLE (Write)
*        The modal value of the histogram calculated by 
*        assuming that near the histogram peak a Normal distribution
*        is present and then 'fitting' it. The 'fit' obtained
*        supplies the value for the histogram peak and also an
*        accurate estimate of the background count standard deviation.
*     MODEP = _DOUBLE (Write)
*        The modal value of the smoothed histogram calculated
*        by taking a number of chords through the histogram and
*        by examining the length of chord versus height relationship
*        extrapolates to a zero chord length. Assumes that the 
*        histogram peak is probably a skewed distribution (SIGMA).
*     MODES = _DOUBLE (Write)
*        The modal value of the smoothed histogram calculated
*        when using only pixels from the parts of the image requested.
*     PEAKV = _DOUBLE (Write)
*        The peak number of pixels found with a given count value
*        in the unsmoothed count versus occurence histogram.
*     PEAKVS = _DOUBLE (Write)
*        The peak number of pixels found with a given count value
*        in the smoothed count versus occurence histogram.
*     PEAKVI = _DOUBLE (Write)
*        The peak number of pixels found with a given count value
*        as estimated by fitting a Normal distribution to the 
*        peak of the count versus occurence histogram.
*     SDEV = _DOUBLE (Write)
*        The standard deviation of the pixel count value calculated
*        using only the good pixels from the image areas requested.
*     SIGMA = _DOUBLE (Write)
*        Estimates for the background count standard deviation. Units counts.
*     SFACT = _INTEGER (Read)
*        The Gaussian smoothing filter radius requested. This may be:
*        - -1 to indicate that the application should automatically
*          assign a filter radius to apply to the histogram.
*        - 0 to indicate that the histogram should not be smoothed.
*        - >0 to indicate the radius of the Gaussian filter. 
*        Values greater than HIS__SFLIM (see include file) are not 
*        allowed. Units counts.
*     SFACTA = _INTEGER (Write)
*        The Gaussian filter radius actually employed by the application.
*        See SFACT. Units counts.
*     SKEW = _DOUBLE (Write)
*        The value of pixel count skewness calculated for the
*        pixels found in the parts of the image used.  
*     UNUPIX = _INTEGER (Write)
*        The number of unused pixels in the final image.
*     USE = _CHAR (Read)
*        Defines the method by which the areas of the image to be
*        used in building up the pixel count histogram are to be
*        selected. 
*           USE='W' All the image pixels are used.
*           USE='A' The image pixels are defined using an ARD file 

*  Examples:
*     histpeak in=galaxy sfact=3 use=w device=ikon1
*        
*        The statistics are calculated for the image GALAXY, using the
*        all the non-bad pixels on the image and the results displayed
*        on the default device as text and graphically on device IKON1.
*        The histogram used to calculate the interpolated mode and
*        background standard deviation is smoothed using a Gaussian filter of 
*        radius 3 counts.
*
*     histpeak in=galaxy2 sfact=-1 use=a device=xwindows ardfil=^okay.dat
*        
*        The statistics are calculated for the image GALAXY2, using
*        the image pixels defined by ARD file OKAY.DAT
*        and the results displayed
*        on the default device as text and graphically on device XWINDOWS.
*        A smoothed histogram is used to calculate the interpolated mode and
*        background standard deviation. The width of the smoothing filter is 
*        chosen by the application.

*  Implementation Status:
*
*     The current version will not accept a pixel value range greater 
*     than the largest integer value possible. 

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-JUL-1992 (GJP)
*     (Original version)
*     16-JAN-1995 (GJP)
*     Mended a bug that placed some values in illegal array elements.
*     30-AUG-1995 (GJP)
*     Corrected bug that meant the SFACT input did not respond properly to
*     the !! input.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'his_par'               ! HISTPEAK system variables
      INCLUDE 'PAR_ERR'               ! Parameter system error constants
                     
*  Status:     
      INTEGER STATUS                  ! Global status

*  Local Variables:      
      CHARACTER *( 256 ) TEMP         ! Temporary storage
      CHARACTER *( 256 ) USE          ! Use pixels from the whole image, 
                                      ! or an ARD file   
      INTEGER AGIID                   ! Device identifier used by AGI
      INTEGER BARSIZ                  ! Size of the of the bin arrays
                                      ! used
      INTEGER ELEMS                   ! Total number of pixels in the NDF
      INTEGER I                       ! Loop variable
      INTEGER LBND(NDF__MXDIM)        ! Lower bounds for each image axis
      INTEGER NDF1                    ! Identifier for the source NDF  
      INTEGER NDF2                    ! Identifier for the modified
                                      ! copy of the source NDF
      INTEGER NDIM                    ! Number of dimensions in the 
                                      ! image
      INTEGER NUMBER                  ! The number of pixels used
      INTEGER PLACE                   ! Placeholder of temporary NDF
      INTEGER POINT0(10)              ! Pointer to the data component of
                                      ! the source NDF
      INTEGER POINT1(10)              ! Pointer to the data component of 
                                      ! for the modified source NDF
      INTEGER POINT2                  ! Pointer to the memory allocated 
                                      ! for the unsmoothed histogram
      INTEGER POINT3                  ! Pointer to the memory allocated
                                      ! for the smoothed histogram
      INTEGER PRANGE(2)               ! Number of pixels in the image x 
                                      ! and y axes 
      INTEGER SFACT                   ! Radius of the Gaussian filter
                                      ! requested to smooth the
                                      ! histogram
      INTEGER SFACTA                  ! The actual filter radius used
      INTEGER UBND(NDF__MXDIM)        ! Upper bounds for each image axis
      INTEGER UNUPIX                  ! Number of unused pixels in the
                                      ! modified source NDF
      REAL BINWID                     ! Bin width used when finding the
                                      ! median and mode values (not
                                      ! 1 when count range > HIS__BINLI)
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
      DOUBLE PRECISION SDEV(2)        ! Background standard deviation
      DOUBLE PRECISION SKEW           ! Skewness of the NDF pixel values 
      DOUBLE PRECISION VARI           ! Variance of the NDF pixel values   
      LOGICAL SEEHIS                  ! Show histogram flag
                                                          
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

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
      SFACT=0
      SFACTA=0
      SKEW=0.0                       
      VARI=0.0                 

*   Begin an NDF context.                               
      CALL NDF_BEGIN
      CALL MSG_BLANK(STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      CALL MSG_OUT(' ','ESP HISTPEAK running.',STATUS)

*   Obtain an identifier for the NDF structure to be examined.       
      CALL NDF_ASSOC('IN','READ',NDF1,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Display the NDF's name.
      CALL NDF_MSG('IN',NDF1)           
      CALL MSG_OUT(' ','Filename:   ^IN',STATUS)
      
*   See if the label component is defined. If so, display its value.
      CALL NDF_CMSG('TITLE',NDF1,'Title',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      CALL MSG_OUT('TITLE','Title:      ^TITLE',STATUS)

*   Get the pixel-index bounds of an NDF and store in LBND and UBND.
      CALL NDF_BOUND(NDF1,2,LBND,UBND,NDIM,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Store the size (in pixels) of the image dimensions.
      DO 10 I=1,2
         PRANGE(I)=UBND(I)-LBND(I)+1
 10   CONTINUE
                                       
*   Display the x and y axis dimensions.    
      CALL MSG_SETI('PR1',PRANGE(1))
      CALL MSG_SETI('PR2',PRANGE(2))
      CALL MSG_OUT(' ','Shape:      ^PR1 x ^PR2  pixels',STATUS)
                              
*   Display the upper and lower bound of the image.
      CALL MSG_SETI('PR1',LBND(1))
      CALL MSG_SETI('PR2',UBND(1))
      CALL MSG_SETI('PR3',LBND(2))
      CALL MSG_SETI('PR4',UBND(2))
      CALL MSG_OUT(' ','Bounds:     x = ^PR1:^PR2  y = ^PR3:^PR4',
     :STATUS)

*   Calculate the nominal number of pixels in the image.
      ELEMS=PRANGE(2)*PRANGE(1)

*   Display the image size.
      CALL MSG_SETI('SIZE',ELEMS)
      CALL MSG_OUT(' ','Image size: ^SIZE pixels',STATUS)

*   Determine what sort of image area selection method is to be used.
*   Whole image or ARD file.
      CALL PAR_GET0C('USE',USE,STATUS)
      CALL CHR_UCASE(USE)
      TEMP=USE(1:1)
      USE=TEMP
      IF (STATUS.NE.SAI__OK) GOTO 9999 

*   Determine the smoothing filter radius required. SFACT=-1 is automatic,
*   SFACT=0 is none. Upper limit fixed by the HIS__SFLIM.
      CALL PAR_GET0I('SFACT',SFACT,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999 
      IF (SFACT.GT.HIS__SFLIM) THEN
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','The value selected exceeded the maximum'/
     :                /' permitted.',STATUS)
         CALL MSG_OUT(' ','The maximum value has been employed.',
     :                STATUS)
         SFACT=HIS__SFLIM
      END IF

*   Determine if graphical histogram output is required. Set the
*   value for SEEHIS accordingly.
      SEEHIS=.TRUE.
      CALL ERR_MARK
      AGIID=0
      CALL HIS1_AGICO(0,AGIID,STATUS)
      IF (STATUS.NE.SAI__OK) THEN
         SEEHIS=.FALSE.
         CALL ERR_ANNUL(STATUS)
      END IF
      CALL ERR_RLSE

*   Map the source NDF data array as _REAL values for reading.
      CALL NDF_MAP(NDF1,'Data','_REAL','READ/BAD',POINT0(1),
     :             ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Obtain a placeholder for a temporary NDF.
      CALL NDF_TEMP(PLACE,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Create a new simple NDF.
      CALL NDF_NEW('_REAL',2,LBND,UBND,PLACE,NDF2,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Map the propogated version of the source array as a real array.
*   This is then used as storage for a modified version of 
*   the source array.
      CALL NDF_MAP(NDF2,'Data','_REAL','WRITE/BAD',POINT1(1),
     :             ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Set up the image array to reflect which pixels are to
*   be used when the image is processed.
      CALL HIS1_SELAR(POINT1,PRANGE,USE,LBND,UBND,
     :                %VAL(POINT0(1)),ELEMS,%VAL(POINT1(1)),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Display the heading for the results display.
      CALL MSG_BLANK(STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      CALL NDF_MSG('FOUT',NDF1)
      CALL MSG_OUT(' ','HISTPEAK Results: ^FOUT',STATUS)  
      CALL MSG_BLANK(STATUS)

*   Un-map/annul the source NDF data array. 
      CALL NDF_UNMAP(NDF1,'Data',STATUS)
      CALL NDF_ANNUL(NDF1,STATUS)                          

*   Call routine to find the highest, lowest and mean
*   value of those in the data array.
      CALL HIS1_HILOA(ELEMS,%VAL(POINT1(1)),
     :                UNUPIX,HIGH,MEAN,LOW,NUMBER,STATUS)               
      IF (STATUS.NE.SAI__OK) GOTO 9999
    
*   Determine the size of the array to be used for binning the image 
*   pixel count values. The bin width is also set. The size of the 
*   binning array is not allowed to exceed HIS__BINLI.

      IF (HIGH-LOW+1.GT.HIS__BINLI) THEN
         BINWID=(HIGH-LOW+1.)/REAL(HIS__BINLI)
         BINWID=BINWID*1.01
         BARSIZ=HIS__BINLI
      ELSE
         BINWID=1.0
         BARSIZ=HIGH-LOW+1
         IF (BARSIZ.LT.1) BARSIZ=1
      END IF

*   Allocate the memory needed for the histogram and smoothed
*   histogram arrays.
      CALL PSX_CALLOC(BARSIZ,'_DOUBLE',POINT2,STATUS)
      CALL PSX_CALLOC(BARSIZ,'_DOUBLE',POINT3,STATUS)
      IF (STATUS.NE.SAI__OK) THEN  
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The dynamic arrays have not been allocated.'
     :                    ,STATUS)
         GOTO 9999
      END IF

*   Call routine to find moments of deviation from the mean for
*   the NDF data array.
      CALL HIS1_MOMDE(ELEMS,NUMBER,%VAL(POINT1(1)),
     :                MEAN,ADEV,VARI,SDEV,SKEW,KURT,STATUS) 
      IF (STATUS.NE.SAI__OK) GOTO 9999      

*   Call routine to find the median and mode of the values in
*   the smoothed NDF array data.
      CALL HIS1_MEDMO(ELEMS,%VAL(POINT1(1)),POINT2,POINT3,BARSIZ,
     :                BINWID,LOW,ADEV,SFACT,SEEHIS,
     :                NUMBER,SDEV,%VAL(POINT2),
     :                %VAL(POINT3),MEDIAN,PEAKV,SFACTA,MODE,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999  

*   Display the results.

*   Display the highest and lowest counts values.
      CALL MSG_FMTI('NUMBER','I14',NUMBER)
      CALL MSG_FMTI('UNUPIX','I14',UNUPIX)
      CALL MSG_OUT(' ','Pixels (used):     ^NUMBER     '//
     :'Pixels (bad):   ^UNUPIX ',STATUS)

      CALL MSG_FMTR('LOW','F14.3',LOW)
      CALL MSG_FMTR('HIGH','F14.3',HIGH)
      CALL MSG_OUT(' ','Lowest count:      ^LOW     '//
     :'Highest count:  ^HIGH ',STATUS)

*   Display the skewness/kurtosis values.
      CALL MSG_FMTD('SKEW','F14.3',SKEW)
      CALL MSG_FMTD('KURT','F14.3',KURT)
      CALL MSG_OUT(' ','Skewness:          ^SKEW     '//
     :'Kurtosis:       ^KURT ',STATUS)
      CALL MSG_BLANK(STATUS)           

*   Display the mean, mode and median values
*   for the image pixels.
      CALL MSG_FMTD('MEAN','F14.3',MEAN)
      CALL MSG_FMTD('MEDIAN','F14.3',MEDIAN)
      CALL MSG_OUT(' ','Mean:              ^MEAN     '// 
     :'Median:         ^MEDIAN ',STATUS)
      CALL MSG_BLANK(STATUS)

      CALL MSG_OUT(' ','Histogram modal values:',STATUS)
      CALL MSG_FMTD('MODE','F14.3',MODE(1))
      CALL MSG_FMTD('MODES','F14.3',MODE(2))
      CALL MSG_OUT(' ','Unsmoothed:        ^MODE     '//
     :'Smoothed:       ^MODES ',STATUS)
      CALL MSG_FMTD('MODEP','F14.3',MODE(3))
      CALL MSG_FMTD('MODEI','F14.3',MODE(4))
      CALL MSG_OUT(' ','Projected:         ^MODEP     '//
     :'Interpolated:   ^MODEI ',STATUS)
      CALL MSG_BLANK(STATUS)
   
*   Display the absolute deviation, standard deviations, variance, skewness,
*   kurtosis and background count std dev for the image pixels.
      CALL MSG_FMTD('ADEV','F14.3',ADEV)
      CALL MSG_FMTD('VARI','F14.0',VARI)
      CALL MSG_OUT(' ','Absolute dev.:     ^ADEV     '//
     :'Variance:       ^VARI',STATUS)
      CALL MSG_FMTD('SDEV','F14.3',SDEV(1))
      CALL MSG_FMTD('SIGMA','F14.3',SDEV(2))
      CALL MSG_OUT(' ','Standard. dev.:    ^SDEV     '//
     :'Back. st. dev.: ^SIGMA ',STATUS)
      CALL MSG_BLANK(STATUS)

*   Display the Gaussian filter size.
      CALL MSG_OUT(' ','Smoothing filter radius:',STATUS)
      CALL MSG_FMTI('SFACT','I14',SFACT)
      CALL MSG_FMTI('SFACTA','I14',SFACTA)
      CALL MSG_OUT(' ','Radius request:    ^SFACT     '//
     :'Radius actual:  ^SFACTA ',STATUS)
      CALL MSG_BLANK(STATUS)

*   Display highest values found in the histograms.
      CALL MSG_OUT(' ','Contents of the most occupied histogram bin:'
     :,STATUS)
      CALL MSG_FMTD('PEAKV','F14.3',PEAKV(1))
      CALL MSG_FMTD('PEAKVS','F14.3',PEAKV(2))
      CALL MSG_OUT(' ','Unsmoothed:        ^PEAKV     '//
     :'Smoothed:       ^PEAKVS ',STATUS)
      CALL MSG_FMTD('PEAKVI','F14.3',PEAKV(3))
      CALL MSG_OUT(' ','Interpolated:      ^PEAKVI ',STATUS)
      CALL MSG_BLANK(STATUS)
                             
*   Assign ADAM parameter values.
      CALL PAR_PUT0D('ADEV',ADEV,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      CALL PAR_PUT0I('UNUPIX',UNUPIX,STATUS)
      CALL PAR_PUT0R('LOW',LOW,STATUS)
      CALL PAR_PUT0R('HIGH',HIGH,STATUS)
      CALL PAR_PUT0D('KURT',KURT,STATUS)
      CALL PAR_PUT0D('MEAN',MEAN,STATUS)
      CALL PAR_PUT0D('MEDIAN',MEDIAN,STATUS)
      CALL PAR_PUT0D('MODE',MODE(1),STATUS)
      CALL PAR_PUT0D('MODEI',MODE(4),STATUS)
      CALL PAR_PUT0D('MODEP',MODE(3),STATUS)
      CALL PAR_PUT0D('MODES',MODE(2),STATUS)
      CALL PAR_PUT0I('NUMBER',NUMBER,STATUS)
      CALL PAR_PUT0D('SDEV',SDEV(1),STATUS)
      CALL PAR_PUT0D('SIGMA',SDEV(2),STATUS)
      CALL PAR_PUT0I('SFACT',SFACT,STATUS)
      CALL PAR_PUT0I('SFACTA',SFACTA,STATUS)
      CALL PAR_PUT0D('SKEW',SKEW,STATUS)
      CALL PAR_PUT0D('VARI',VARI,STATUS)                            
              
*   Close down all resources.
 9999 CONTINUE

*   Turn off the AGI/PGPLOT interface.
      IF (SEEHIS) THEN
         CALL HIS1_AGICO(1,AGIID,STATUS)
      END IF                             

*   Free the dynamic array space of the histogram arrays.
      CALL PSX_FREE(POINT2,STATUS)
      CALL PSX_FREE(POINT3,STATUS)                           

*   Un-map/annul the modified version of the source NDF data array. 
      CALL NDF_UNMAP(NDF2,'Data',STATUS)
      CALL NDF_ANNUL(NDF2,STATUS)                              

*   End the NDF context.
      CALL NDF_END(STATUS)                              

      END
      

      SUBROUTINE HIS1_AGICO(ONOFF,AGIID,STATUS)
*+
*  Name:
*     HIS1_AGICO

*  Purpose:
*     Turns on/off the AGI/PGPLOT interface.
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HIS1_AGICO(ONOFF,AGIID,STATUS)    

*  Description:
*     Depending on the value of ONOFF the subroutine either:-
*     sets up the AGI/PGPLOT interface and enters new information into
*     the AGI database (ONOFF=0) or closes down the database and
*     interface (ONOFF=1).

*  Arguments:                                     
*     ONOFF = INTEGER (Given)
*        Defines whether the routines controlling AGI/PGPLOT should
*        be turned on or off. 0=on 1=off 
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

*   Setup the AGI/PGPLOT interface.
      IF (ONOFF.EQ.0) THEN

*      Open AGI on a device obtained from the parameter system.
         CALL AGI_ASSOC('DEVICE','WRITE',AGIID,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Enure that the whole screen is used.
         CALL AGI_IBASE(AGIID,STATUS)
         CALL AGI_SELP(AGIID,STATUS)

*      Activate the PGPLOT interface to AGI.
         CALL AGP_ACTIV(STATUS)

*      Create a new viewport.
         CALL AGP_NVIEW(.TRUE.,STATUS)
         IF (STATUS.NE.SAI__OK) THEN
            CALL ERR_REP(' ','Using PGPLOT+AGI has failed.',STATUS)
            GOTO 9999 
         END IF

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
         CALL AGP_SVIEW('HISTPEAK','Histogram plot',AGIID,STATUS)

*      Close down PGPLOT. 
         CALL AGP_DEACT(STATUS)

*      Cancel the picture identifier or annul the parameter association 
*      depending on the value of STATUS.
         IF (STATUS.NE.SAI__OK) THEN

*         Cancel the AGI parameter association.       
            CALL AGI_CANCL('DEVICE',STATUS)
        
         ELSE

*         Annul the AGI parameter association.       
            CALL AGI_ANNUL(AGIID,STATUS)

         END IF

      END IF

      END
      

      SUBROUTINE HIS1_CHORD(HIVAL,LOVAL,MODEC,SMOBAR,LOW,ADEV,
     :                      BARSIZ,BINWID,NUMDAT,HEIG,X1,Y1,STATUS)
*+                        
*  Name:
*     HIS1_CHORD

*  Purpose:
*     Estimate histogram mode by examining chords through peak. 
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HIS1_CHORD(HIVAL,LOVAL,MODEC,SMOBAR,LOW,ADEV,
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
*     HEIG(HIS__CHORM) = REAL (Returned)
*        The height at which the chord through the histogram occurs.
*     X1(HIS__CHORM) = REAL (Returned)
*        Length of chord through the histogram.      
*     Y1(HIS__CHORM) = REAL (Returned)
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
      INCLUDE 'his_par'               ! HISTPEAK system variables
                                                                      
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
      REAL HEIG(HIS__CHORM)           ! The histogram values at which 
                                      ! chords were taken through the
                                      ! histogram                  
      REAL X1(HIS__CHORM)             ! Length of the chord through
                                      ! the histogram
      REAL Y1(HIS__CHORM)             ! X index of midpoint of chord
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


      SUBROUTINE HIS1_GAUJO(VECTOR,INPMAT,DETERM,STATUS)
*+
*  Name:
*     HIS1_GAUJO

*  Purpose:                                                          
*     Inverts a matrix containing preprocessed histogram values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HIS1_GAUJO(B,A,DETERM,STATUS)

*  Description:
*     Employs the very stable Gauss-Jordan with optimised 
*     array pivot elements method to invert a matrix. The matrix to be
*     inverted (INPMAT) is received in a form preprocessed by
*     subroutine ******. On completion, the array INPMAT contains
*     the inverted matrix and the vector array VECTOR contains the 
*     coefficients of the parabolic equation. 
* 
*     If the routine suceeds, the determinant (DETERM) of the array 
*     is significantly non-zero.

*  Arguments:
*     VECTOR(3) = REAL ARRAY (Given and Returned)
*        Preprocessed count values are given. Values for the parabola
*        coefficients are returned.
*     INPMAT(3,3) = REAL (Given and Returned)
*        The matrix to be inverted. The inverted matrix is returned.
*     DETERM = REAL (Returned)
*        The determinant of the inverted array.
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
      INCLUDE 'his_par'               ! HISTPEAK system variables

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
         IF (DETERM.LT.HIS__VSMAL) THEN   
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
      INCLUDE 'his_par'               ! HISTPEAK system variables

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
                                                                   

      SUBROUTINE HIS1_HILOA(ELEMS,ARRAY,
     :                      UNUPIX,HIGH,MEAN,LOW,NUMBER,STATUS)               
*+
*  Name:
*     HIS1_HILOA

*  Purpose: 
*     Find the highest and lowest count values in an image array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HIS1_HILOA(ELEMS,ARRAY,
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
*        The number of non-bad pixels. Units pixels.
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
      INCLUDE 'his_par'               ! HISTPEAK system variables
                                                                      
*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the image
      REAL ARRAY( ELEMS )             ! Image pixel counts  

*  Arguments Returned:
      INTEGER NUMBER                  ! The number of pixels to be used
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
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','Pixel range too large to handle.',STATUS)
         STATUS=SAI__ERROR
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

*   Check the likely width of the histogram.
      IF (HIGH-LOW.LT.1) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The histogram is very narrow.'//
     :                    ' Please rescale your data.',STATUS)
         GOTO 9999
      END IF

*   Check if the range of values found is less than 3 and
*   set STATUS if true. 
      IF (HIGH-LOW.LT.3.0) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The range of values found is less than 3!'//
     :                    ' Consequently, accurate modes are not'//
     :                    ' available.',STATUS)
         GOTO 9999
      END IF
  
 9999 CONTINUE

      END


      SUBROUTINE HIS1_MEDMO(ELEMS,ARRAY,POINT2,POINT3,BARSIZ,
     :                      BINWID,LOW,ADEV,SFACT,SEEHIS,
     :                      NUMBER,SDEV,BARRAY,SMOBAR,
     :                      MEDIAN,PEAKV,SFACTA,MODE,STATUS)
*+
*  Name:
*     HIS1_MEDMO

*  Purpose:
*     Creates a histogram array from the image array and finds its
*     mode.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HIS1_MEDMO(ELEMS,ARRAY,POINT2,POINT3,BARSIZ,
*                     BINWID,LOW,ADEV,SFACT,SEEHIS,
*                     NUMBER,SDEV,BARRAY,SMOBAR,
*                     MEDIAN,PEAKV,SFACTA,MODE,STATUS)
                                   
*  Description:
*     Places the values from the mapped image array into a binning
*     array. The array is used as a histogram. The routine then
*     determines values for the peak height, mode, median, and std dev
*     of the histogram.

*  Arguments:               
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image. Units pixels.
*     ARRAY(ELEMS)= REAL (Given)
*        Array containing the image data.
*     POINT2 = INTEGER (Given)
*        Memory pointer to the binning array.
*     POINT3 = INTEGER (Given)
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
*        otherwise any number less than HIS__SFLIM (see include file)
*        may be used. Units counts.
*     SEEHIS = LOGICAL (Given)
*        Graphics histogram to be displayed flag.
*     NUMBER = INTEGER (Given)
*        The number of of non-bad pixels.Units pixels.
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
      INCLUDE 'his_par'               ! HISTPEAK system constants

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER BARSIZ                  ! Size of the binning arrays used 
      INTEGER NUMBER                  ! The number of pixels to be used
      INTEGER POINT2                  ! Pointer to the binning array
      INTEGER POINT3                  ! Pointer to the smoothed bin array
      INTEGER SFACT                   ! Requested radius for the Gaussian
                                      ! filter used to smooth the 
                                      ! histogram
      REAL ARRAY(ELEMS)               ! Array containing the image data
      REAL BINWID                     ! Width of the bins used to find 
                                      ! median and mode (only differs
                                      ! from 1 when count range exceeds
                                      ! HIS__BINLI)
      REAL LOW                        ! Binning arrays origin offset and
                                      ! lowest value found in image
      DOUBLE PRECISION ADEV           ! Absolute deviation of data 
      LOGICAL SEEHIS                  ! User graphics or not choice

*  Arguments Given and Returned:
      DOUBLE PRECISION SDEV(2)        ! Standard deviation of the data
                                      ! and the background count std dev

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
                                      ! subroutine HIS1_LINRE
      REAL CONS                       ! Constant term of linear
                                      ! relationship fitted by
                                      ! subroutine HIS1_LINRE
      REAL GRAD                       ! Gradient term of linear 
                                      ! relationship fitted by 
                                      ! subroutine HIS1_LINRE
      REAL HEIG(HIS__CHORM)           ! The values at which chords 
                                      ! (slices) were taken through the
                                      ! histogram
      REAL VALUE1                     ! Temporary storage variable
      REAL X1(HIS__CHORM)             ! X value array passed to
                                      ! subroutine HIS1_LINRE
      REAL Y1(HIS__CHORM)             ! Y value array passed to
                                      ! subroutine HIS1_LINRE
      DOUBLE PRECISION HALF           ! Half the number of non-bad
                                      ! in the binning arrays 
      DOUBLE PRECISION HIVAL          ! Highest value found in the bin
                                      ! array
      DOUBLE PRECISION LOVAL          ! Lowest value found in the bin
                                      ! array
      DOUBLE PRECISION SFTOT          ! Total of smoothing factors used
      DOUBLE PRECISION SMOFAC(-HIS__SFLIM:HIS__SFLIM) 
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
            INDEX=INT((VALUE1-LOW)/BINWID+1)

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

*      Look for the lower limit.
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
         IF (SFACTA.GT.HIS__SFLIM) SFACTA=HIS__SFLIM

      ELSE
     
*      Set the filter radius and impose an upper limit.
         SFACTA=SFACTA/BINWID
         IF (SFACTA.GT.HIS__SFLIM) SFACTA=HIS__SFLIM
         
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
            SMOFAC(I)=1./SQRT(2.*HIS__PIVAL)/(SFACTA/3.)
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
*   than HIS__BINLI.
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
      CALL HIS1_CHORD(HIVAL,LOVAL,MODEC,%VAL(POINT3),LOW,ADEV,
     :                BARSIZ,BINWID,NUMDAT,HEIG,X1,Y1,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
           
*   Determine the linear relationship between histogram width
*   and value at which the width was determined. Extrapolate to zero
*   width (peak) and thereby estimate a mode value.
      IF (NUMDAT.GT.2) THEN 
         CALL HIS1_LINRE(X1,Y1,NUMDAT,GRAD,CONS,STATUS)
         IF (NUMDAT.GT.2) MODE(3)=CONS
      END IF

*   Set up the data for matrix inversion to provide 
*   an interpolated value for the mode.
      CALL HIS1_PARA(ADEV,%VAL(POINT3),BARSIZ,LOW,BINWID,
     :               MODE,SDEV,PEAKV,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Display histogram if user has opted for graphics.      
      IF (SEEHIS) THEN
         CALL HIS1_GRAPH(ADEV,%VAL(POINT2),PEAKV(1),LOW,MODE,MEDIAN,
     :                  SDEV,%VAL(POINT3),BARSIZ,HEIG,X1,Y1,NUMDAT,
     :                  BINWID,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999
      END IF

 9999 CONTINUE
                    
      END

 
      SUBROUTINE HIS1_LINRE(X1,Y1,NUMDAT,GRAD,CONS,STATUS)
*+
*  Name:
*     HIS1_LINRE

*  Purpose:
*     Determines a least squares linear fit for data in arrays X and Y. 

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HIS1_LINRE(X1,Y1,NUMDAT,GRAD,CONS,STATUS)

*  Description:
*     Uses a normal least squares method to determine the coefficients
*     of a linear fit to the contents of arrays X and Y.

*  Arguments:
*     X1(HIS__CHORM) = REAL (Given)
*        The x values to be used.
*     Y1(HIS__CHORM) = REAL (Given)
*        The y values to be used.
*     NUMDAT = INTEGER (Given)
*        The number of data (X/Y) pairs to be fitted.
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
      INCLUDE 'his_par'               ! HISTPEAK system variables
                     
*  Arguments Given:
      INTEGER NUMDAT                  ! Number of data points
      REAL X1(HIS__CHORM)             ! Data points X1 value
      REAL Y1(HIS__CHORM)             ! Data points Y1 value

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
      IF (ABS(TOT1).GT.HIS__VSMAL) THEN
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
       

      SUBROUTINE HIS1_MOMDE(ELEMS,NUMBER,ARRAY,
     :                      MEAN,ADEV,VARI,SDEV,SKEW,KURT,STATUS) 
*+
*  Name:
*     HIS1_MOMDE

*  Purpose:
*     Finds the deviations, skewness and kurtosis of pixels in an image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HIS1_MOMDE(ELEMS,NUMBER,ARRAY,
*                     MEAN,ADEV,VARI,SDEV,SKEW,KURT,STATUS) 
                  
*  Description:
*     Finds values for the absolute deviation, standard deviation, 
*     variance, skewness and kurtosis of pixels in an image.

*  Arguments:                      
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image. Units pixels.
*     NUMBER = INTEGER (Given)
*        The number of non-bad image pixels. Units pixels.
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
*        and the background count std dev. Units counts.
*     SKEW = DOUBLE PRECISION (Returned)
*        Skewness of the image pixel count distribution.
*     KURT = DOUBLE PRECISION (Returned)
*        Kurtosis of the image pixel count distribution.
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
      INCLUDE 'his_par'               ! HISTPEAK system variables

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the data 
      INTEGER NUMBER                  ! The number of pixels to be used
      REAL ARRAY(ELEMS)               ! Array containing image data
      DOUBLE PRECISION MEAN           ! Average value in the image 
                                      ! array       
             
*  Arguments Returned:
      DOUBLE PRECISION ADEV           ! Absolute deviation of array 
      DOUBLE PRECISION KURT           ! Kurtosis of array values
      DOUBLE PRECISION SDEV(2)        ! Background standard deviation 
      DOUBLE PRECISION SKEW           ! Skewness of the array values 
      DOUBLE PRECISION VARI           ! Variance of array values

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables: 
      INTEGER I                       ! Loop variable
      DOUBLE PRECISION VALUE          ! Temporary storage variable
      DOUBLE PRECISION P2             ! Temporary storage variable
      DOUBLE PRECISION P3             ! Temporary storage variable
      DOUBLE PRECISION P4             ! Temporary storage variable
      REAL TEMP                       ! Temporary storage
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN
       
*   Look through the array to gather information for calculating the
*   standard deviation, absolute deviation, variance, skewness and
*   kurtosis of the distribution.
      DO 212 I=1,ELEMS

*      Get the pixel value and check that it isnt bad.
         TEMP=ARRAY(I)
         IF (TEMP.NE.VAL__BADR) THEN 

*         Convert to double precision and then calculate the
*         absolute deviation (first moment of deviation). 
            VALUE=DBLE(TEMP)-MEAN         
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
         ADEV=ADEV/DBLE(NUMBER)
         VARI=VARI/DBLE(NUMBER-1)
         SDEV(1)=SQRT(VARI)
         SKEW=SKEW/(DBLE(NUMBER)*SDEV(1)**3)
         KURT=KURT/(DBLE(NUMBER)*(VARI**2))-3
      ELSE
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Too few points for an accurate estimate'//
     :                    ' of the standard. deviation etc.',STATUS)
         GOTO 9999
      END IF
      
 9999 CONTINUE

      END


      SUBROUTINE HIS1_PARA(ADEV,SMOBAR,BARSIZ,LOW,BINWID,
     :                     MODE,SDEV,PEAKV,STATUS)
*+                        
*  Name:
*     HIS1_PARA

*  Purpose:
*     Estimate histogram mode by parabolic fitting of the histogram peak. 
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HIS1_PARA(ADEV,SMOBAR,BARSIZ,LOW,BINWID,
*                    MODE,SDEV,PEAKV,STATUS)
    
*  Description:                                        
*     Logarithmically transforms the values of the smoothed
*     histogram array (SMOBAR) and then 'fits' a parabola to the
*     points near to the peak. The fitting is carried out by routine
*     HIS1_GAUJO but the data is passed in array VECTOR and 
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
*        and background count standard deviation. Units counts.
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
      INCLUDE 'his_par'               ! HISTPEAK system variables
                                                                      
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
                                      ! subroutine HIS1_GAUJO 
      REAL VECTOR(3)                  ! Vector array in which parabola 
                                      ! coefficients are returned from 
                                      ! subroutine HIS1_GAUJO
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
      DO WHILE (((ABS(DETERM).LT.HIS__VSMAL).
     :            OR.(VECTOR(3).GE.0.0)).AND.
     :               (RANGE.LE.2.0*ADEV/BINWID))

*      Clear the arrays to be used.
         DO 450 I=1,3
            VECTOR(I)=0.0
            DO 440 J=1,3
               INPMAT(J,I)=0.0
 440        CONTINUE
 450     CONTINUE

*      Use only data points near the current mode estimate.
*      Ensure that more points are taken from the low side of the
*      Gaussian curve.
         L=(MODE(2)-LOW)/BINWID+1
         DO 480 I=NINT(-RANGE*1.5),NINT(RANGE)

*         Define the array index.
            J=L+I

*         Avoid looking at data points that are beyond the array bounds.
            IF ((J.GE.1).AND.(J.LE.BARSIZ)) THEN

*            Avoid taking the log. of zero and avoid using
*            distribution outliers.
               IF ((SMOBAR(J).GT.HIS__VSMAL).AND.
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
         IF (INPMAT(1,1).GT.2.0) THEN
            CALL HIS1_GAUJO(VECTOR,INPMAT,DETERM,STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 9999
         END IF

*      Increase the range of points to be used.
         RANGE=RANGE*1.05

      END DO

*   Assign interpolated mode, peak and std dev values.
      IF ((RANGE.LE.2.*ADEV/BINWID).AND.(ABS(DETERM).GT.HIS__VSMAL)
     :     .AND.(VECTOR(3).NE.0.0)) THEN
         MODE(4)=MODE(2)-VECTOR(2)/2./VECTOR(3)*BINWID
         SDEV(2)=SQRT(-1./VECTOR(3)/2.)*BINWID
         PEAKV(3)=EXP(VECTOR(1)-(VECTOR(2)/2.)**2/VECTOR(3))
      ELSE
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','A sensible interpolated value was'//
     :                 ' not available.',STATUS)
        GOTO 9999
      END IF

 9999 CONTINUE
     
      END 
     

      SUBROUTINE HIS1_SELAR(POINT1,PRANGE,USE,LBND,UBND,
     :                      OARRAY,ELEMS,ARRAY,STATUS)
*+
*  Name:
*     HIS1_SELAR

*  Purpose:
*     Sets up image array to include only those pixels that have
*     been selected. 

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HIS1_SELAR(POINT1,PRANGE,USE,LBND,UBND,OARRAY,
*                     ELEMS,ARRAY,STATUS)

*  Description:
*     Allows the user to define which parts of the image are 
*     to be used by opting for the whole image or with parts selected
*     using an ARD file missing.
     
*  Arguments:
*     POINT1( 10 ) = INTEGER (Given)
*        Memory pointer to the modified image array.
*     PRANGE ( 2 ) = INTEGER (Given)
*        The length of each image axis in pixels.
*     USE = CHARACTER * ( * ) (Given)
*        Specifies how the useful parts of the image are defined.
*        'A' employs and ARD subset file, 'W' uses the whole image.
*     LBND ( 2 ) = INTEGER (Given)
*        Lower limits of indices of the image axes.
*     UBND ( 2 ) = INTEGER (Given)
*        Upper limits of the indices of the image axes.
*     OARRAY( ELEMS ) = REAL (Given)
*        The source NDF data component.
*     ELEMS = INTEGER (Given)
*        The number of pixel elements in the source NDF image.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*     ARRAY( ELEMS ) = REAL (Returned)
*        A copy of the data in the data component of the source NDF
*        with all the unwanted pixels replaced by the bad value.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     06-NOV-1992 (GJP):
*        Original version.
*     24-FEB-1997 (GJP)
*        Modified use of pointers.
*     {enter_further_changes_here}

*  Bugs:
*     None known.

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants
      INCLUDE 'his_par'          ! HISTPEAK system variables
      INCLUDE 'NDF_PAR'          ! NDF public constants

*  Arguments Given:
      CHARACTER * ( * ) USE      ! User input indicating if the whole
                                 ! image or parts defined by an ARD file
                                 ! are to be used
      INTEGER ELEMS              ! Number of pixels in the source image
      INTEGER LBND(NDF__MXDIM)   ! Low bounds of image axes
                                 ! May not be less than 1
      INTEGER POINT1( 10 )       ! Pointer to the modified image array
      INTEGER PRANGE ( 2 )       ! The length of the image axes in pixels
      INTEGER UBND(NDF__MXDIM)   ! High bounds of the image axes
                                 ! may not be greater than the image size
      REAL OARRAY( ELEMS )       ! The array containing the source NDF
                                 ! data component

*  Arguments Returned.
      REAL ARRAY( ELEMS )        ! An array containing a modified 
                                 ! version of the source array Only
                                 ! those points to be used are not
                                 ! assigned a bad value
   
*  Status:
      INTEGER STATUS             ! Global status
     
*  Local Variables:
      INTEGER POINT4             ! Pointer to the logical mask
      INTEGER P1(1)              ! Pointer
      INTEGER P4(1)              ! Pointer
      INTEGER I                  ! Temporary loop variable
      INTEGER NDIM               ! Number of dimensions
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
             
*   Copy the whole image array.
      DO 10 I=1,ELEMS
         ARRAY(I)=OARRAY(I)
 10   CONTINUE
 
*   Handle extra stages needed if USE = 'A' i.e. an ARD file is in use.
      IF (USE.EQ.'A') THEN     

*      Display the message saying the ARD file is being looked at.
         CALL MSG_OUT(' ','Examining the ARD file.',STATUS)
         
*      Allocate the memory needed for the logical mask array.         
         CALL PSX_CALLOC(ELEMS,'_INTEGER',POINT4,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Transfer to the ARD driver control routine.
         P1(1)=POINT1(1)
         P4(1)=POINT4
         NDIM=2
         CALL ESP_ARD_DRIVER(NDIM,ELEMS,LBND,UBND,P1,P4,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Free the dynamic array space of the logical mask.
         CALL PSX_FREE(POINT4,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

      END IF

 9999 CONTINUE

      END


