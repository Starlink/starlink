      SUBROUTINE HSUB( STATUS )

*+
*  Name:
*     HSUB
*
*  Purpose:
*     A subroutine version of HISTPEAK for developers.
*
*  Language:
*     Starlink Fortran 77
*
*  Invocation:
*     CALL HSUB( STATUS )
*
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
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
*
*  Usage:
*     HSUB IN SFACT TYPE OUT OUTCAT
*
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
*     OUT = _CHAR (Read)
*        The name of an output file which is to receive the results.  If
*        not present, then the results are printed on stdout.
*     OUTCAT = _CHAR (Read)
*        The name of a file which is to receive the results formatted as
*        an STL file, as defined in SUN/190.  Both OUT and OUTCAT may be
*        specified, in which case output is sent to both.  If neither
*        is specified, then output is sent to stdout, in the format
*        appropriate for the OUT parameter.  OUTCAT may only be
*        specified on the command line.
*
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
*     hsub in=forn4 sfact=6 type=3 outcat=hsub.txt
*
*        A histogram of the values in image FORN4 is used. The image
*        is smoothed using a gaussian filter of radius 6 and the
*        results returned those for the projected mode value.
*
*  Implementation Status:
*     The current version will not accept a pixel value range greater
*     than the largest integer value possible.
*
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
*
*     With the addition of the OUTCAT keyword, HSUB is now used by GAIA
*     to generate backgrounds, so it is not merely an example.  It is
*     still, of course, useful as an example of how to use the HISTPEAK
*     functions.  Specifically, you should not change the keywords in
*     the STL output.
*
*  Authors:
*     GJP: Grant Privett (STARLINK)
*     NG: Norman Gray (Starlink, Glasgow)
*
*  History:
*     13-Nov-1992 (GJP)
*       (Original version)
*     29-Jan-1993 (GJP)
*       Bug in the interpolation method corrected.
*     26-JAN-2000 (NG)
*       Added OUTCAT parameter, so it can be used by GAIA.
*
*  RCS ID:
*     $Id$
*-

*  Type Definitions:                  ! No implicit typing
      implicit none

*  Global Constants:
      include 'SAE_PAR'               ! Standard SAE constants
      include 'NDF_PAR'               ! NDF_ public constant
      include 'PRM_PAR'               ! PRIMDAT primitive data constants
      include 'PAR_ERR'               ! PAR constants
      include 'HSB_PAR'               ! HSUB system variables

*  Status:
      integer status                  ! Global status

*  Local Variables:
      integer elems                   ! Number of pixels in the image
      integer ndf1                    ! NDF identifier
      integer nupoi                   ! Number of points used in the
                                      ! calculation of mode
      integer point1(10)              ! Pointer to NDF array to be used
      integer sfact                   ! Gaussian filter radius requested
      integer type                    ! Value indicating whether the final
                                      ! estimate for mode given is to be from:
                                      ! 0 - get best available
                                      ! 1 - raw histogram
                                      ! 2 - smoothed histogram
                                      ! 3 - projecting peak chords
                                      ! 4 - interpolating smoothed histogram
      integer outfiod,outunit	      ! Output file descriptor and unit
      integer catfiod, catunit        ! OUTCAT file descriptor and unit
      double precision kurto          ! Image pixel count kurtosis
      double precision modev          ! Estimate of the image mode value
      double precision skewn          ! Image skewness value
      double precision stand          ! Estimate of the standard deviation
                                      ! of the pixel values or the
                                      ! background count standard deviation

*.

*   Check the inherited global status.
      if (status.ne.sai__ok) return

*********************************************************************

*   Example usage. Obtain a whole image and work on that. Could be replaced
*   with anything that gets a pointer to a data array and supplies the array
*   size as POINT1(1) and ELEMS respectively.

*   Begin an NDF context.
      call ndf_begin

*   Check inherited status
      if (status.ne.sai__ok) return

*   Show that the application is running.
      call msg_blank(status)
      call msg_out(' ','ESP HSUB running.',status)

*   Obtain parameters:
*
*   Obtain an identifier for the NDF structure to be examined.
      call ndf_assoc('in','read',ndf1,status)

*   Smoothing factor
      call par_get0i('sfact',sfact,status)

*   Determine the calculation method to be employed. TYPE=0 is automatic,
      call par_get0i('type',type,status)

*   Output file names, both OUT and OUTCAT
      call err_mark

      outfiod = 0
      call fio_assoc ('out', 'write', 'list', 0, outfiod, status)
      if (status .eq. par__null) then
*      That's OK -- no file specified.
         call err_annul (status)
      endif

      catfiod = 0
      call fio_assoc ('outcat', 'write', 'list', 0, catfiod, status)
      if (status .eq. par__null) then
         call err_annul (status)
      endif

      call err_rlse

*   JUMP OUT if the parameter-reading has not been successful (which
*   includes the case where a user enters ABORT (`!!')
      if (status .ne. sai__ok) goto 9999


*   Interpret parameters:
*
*   Determine the smoothing filter radius required. SFACT=-1 is automatic,
*   SFACT=0 is none. Upper limit fixed by the HSB__SFLIM.
      if (sfact.gt.hsb__sflim) then
         call msg_out(' ','WARNING!!!',status)
         call msg_out(' ','The value selected exceeded the maximum'/
     :                /' permitted.',status)
         call msg_out(' ','The maximum value has been employed.',
     :                status)
         sfact=hsb__sflim
      end if

*   Map the source NDF data array as _REAL values for reading.
      call ndf_map(ndf1,'data','_real','read',point1(1),elems,status)

*   Show whats going on.
      call msg_blank(status)
      call msg_out(' ','HSUB calculating.',status)

**********************************************************************
*   MAIN SUBROUTINE

*   Call the modified version of HISTPEAK.
      call histpea2(point1,elems,sfact,type,modev,stand,kurto,
     :              skewn,nupoi,status)

**********************************************************************

*   If something has gone wrong, then skip the output stage
      if (status.ne.sai__ok) goto 9999

*   Display the results.  Nothing sophisticated.

      outunit = 0
      catunit = 0
      if (outfiod .eq. 0 .and. catfiod .eq. 0) then
*      Just send the results to stdout
         outunit = 6            ! stdout
      else
*      Obtain the units corresponding to the IO descriptors
         if (outfiod .ne. 0) then
            call fio_unit (outfiod, outunit, status)
         endif
         if (catfiod .ne. 0) then
            call fio_unit (catfiod, catunit, status)
         endif
      endif

      if (outunit .ne. 0) then
         write (outunit,'("Mode value:               ",f8.1)'),modev
         write (outunit,'("Std. dev.:                ",f8.1/)'),stand

*      Kurtosis and skewness.
         write (outunit,'("Kurtosis:                 ",f8.3)'),kurto
         write (outunit,'("Skewness:                 ",f8.3/)'),skewn

*      Number of points used etc.
         write (outunit,'("Number of points given:   ",i8)'),elems
         write (outunit,'("Number of points used     ",i8)'),nupoi
         write (outunit,'("Filter radius used:       ",i8/)'),sfact

*      Type of modal value found.
         write (outunit,'("Mode type 1-4:            ",i8)'),type
         write (outunit,'("Global status:            ",i8)'),status
      endif

      if (catunit .ne. 0) then
*      Write out the results in an easily parsable format.  The
*      format here is the parameter section of an STL file, as
*      defined in SUN/190.  We could use the CAT library to write
*      this, but that's unnecessarily complicated for this
*      application.
*
*      Don't change the keywords here, as GAIA relies on them.  Feel
*      free to change the order, though, and to include comments
*      beginning with `!'.

         write (catunit,'("! HSUB output file")')
         write (catunit,'("P mode     REAL ",F8.1," EXFMT=E12.3")')modev
         write (catunit,'("P sd       REAL ",F8.1," EXFMT=E12.3")')stand
         write (catunit,'("P kurtosis REAL ",F8.3," EXFMT=E12.3")')kurto
         write (catunit,'("P skewness REAL ",F8.3," EXFMT=E12.3")')skewn
         write (catunit,'("P ngiven   INTEGER ",I8," EXFMT=I8")')  elems
         write (catunit,'("P nused    INTEGER ",I8," EXFMT=I8")')  nupoi
         write (catunit,'("P sfact    INTEGER ",I8," EXFMT=I8")')  sfact
         write (catunit,'("P modetype INTEGER ",I8," EXFMT=I8")')  type

      endif

********************************************************************

 9999 continue

*   Close down resources used

*   Un-map/annul the source NDF data array.
      call ndf_unmap(ndf1,'data',status)
      call ndf_annul(ndf1,status)

*   End the NDF context.
      call ndf_end(status)

      if (outfiod .ne. 0) then
         call fio_annul (outfiod, status)
         outfiod = 0
      endif
      if (catfiod .ne. 0) then
         call fio_annul (catfiod, status)
         catfiod = 0
      endif

********************************************************************

      end


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
      INCLUDE 'HSB_PAR'               ! HSUB system variables
      INCLUDE 'CNF_PAR'

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
      CALL HSB1_HILOA(ELEMS,%VAL(CNF_PVAL(POINT1(1))),
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
      CALL HSB1_MOMDE(ELEMS,NUMBER,%VAL(CNF_PVAL(POINT1(1))),
     :                MEAN,STATUS,ADEV,VARI,SDEV,SKEW,KURT)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Call routine to find the median and mode of the values in
*   the NDF array data.
      CALL HSB1_MEDMO(TYPE,ELEMS,%VAL(CNF_PVAL(POINT1(1))),
     :                POINT2,POINT3,BARSIZ,
     :                BINWID,LOW,ADEV,SFACT,
     :                NUMBER,STATUS,SDEV,%VAL(CNF_PVAL(POINT2(1))),
     :                %VAL(CNF_PVAL(POINT3(1))),
     :                MEDIAN,PEAKV,SFACTA,MODE)
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
