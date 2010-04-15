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
*     ADEV = _DOUBLE (Write)
*        The absolute deviation of the pixel values distribution
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
*     NUMBER = _DOUBLE (Write)
*        The number of pixels actually used.
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
*     VARI = _DOUBLE (Write)
*        Variance of the pixel values distribution.
*
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
      INCLUDE 'HIS_PAR'               ! HISTPEAK system variables
      INCLUDE 'PAR_ERR'               ! Parameter system error constants
      INCLUDE 'CNF_PAR'

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
     :                %VAL(CNF_PVAL(POINT0(1))),ELEMS,
     :                %VAL(CNF_PVAL(POINT1(1))),STATUS)
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
      CALL HIS1_HILOA(ELEMS,%VAL(CNF_PVAL(POINT1(1))),
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
      CALL HIS1_MOMDE(ELEMS,NUMBER,%VAL(CNF_PVAL(POINT1(1))),
     :                MEAN,ADEV,VARI,SDEV,SKEW,KURT,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Call routine to find the median and mode of the values in
*   the smoothed NDF array data.
      CALL HIS1_MEDMO(ELEMS,%VAL(CNF_PVAL(POINT1(1))),
     :                POINT2,POINT3,BARSIZ,
     :                BINWID,LOW,ADEV,SFACT,SEEHIS,
     :                NUMBER,SDEV,%VAL(CNF_PVAL(POINT2)),
     :                %VAL(CNF_PVAL(POINT3)),MEDIAN,PEAKV,
     :                SFACTA,MODE,STATUS)
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
