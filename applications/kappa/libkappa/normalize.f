      SUBROUTINE NORMALIZE( STATUS )
*+
*  Name:
*     NORMALIZE

*  Purpose:
*     Normalises one NDF to a similar NDF by calculating a scale factor
*     and zero-point difference.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL NORMALIZE( STATUS )

*  Description:
*     This application compares the data values in one NDF against the
*     corresponding values in the other NDF.  A least-squares
*     straight-line is then fitted to the relationship between the two
*     sets of data values in order to determine the relative scale
*     factor and any zero-level offset between the NDFs.  To reduce
*     computation time, the data points are binned according to the
*     data value in the first NDF.  The mean data value within each bin
*     is used to find the fit and weights are applied according to the
*     number of pixels which contribute to each bin.
*
*     To guard against erroneous data values, which can corrupt the fit
*     obtained, the application then performs a number of iterations.
*     It calculates a noise estimate for each bin according to the rms
*     deviation of data values in the bin from the straight-line fit
*     obtained previously.  It then re-bins the data, omitting values
*     which lie more than a specified number of standard deviations
*     from the expected value in each bin.  The straight-line fit is
*     then re-calculated.  You can specify the number of standard
*     deviations and the number of iterations used.
*
*     A plot is produced after the final iteration showing the bin
*     centres, with error bars representing the spread of values in each
*     bin.  This plot is produced within the current AGI picture and is 
*     of a size you specify.
*
*     Optionally, an output NDF can be created containing a normalised 
*     version of the data array from the first input NDF.

*  Usage:
*     normalize in1 in2 out

*  ADAM Parameters:
*     ABSLAB = LITERAL (Read)
*        A title for the plot's x axis.  Only the first 50 characters
*        are used.  The default is "Data value in ^NDF" where ^NDF is
*        replaced by the name of the NDF associated with IN2. []
*     CLEAR = _LOGICAL (Read)
*        Determines if the graphics workstation is to be cleared before
*        producing the plot. [TRUE]
*     DATARANGE( 2 ) = _REAL (Read)
*        This parameter may be used to override the plot auto-scaling
*        feature.  If given, two real numbers should be supplied
*        specifying the lower and upper data values in IN2, between
*        which data will be used.  If not given, the default is to use
*        the auto-scaled values, calculated according to the value of
*        PCRANGE. [,]
*     DEVICE = DEVICE (Read)
*        The graphics workstation on which to produce the plot.  If it
*        is null, !, there will be no plot made.
*        [Current graphics device]
*     FONT = LITERAL (Read)
*        The fount to be used for the line graphics.  It can be either
*        "NCAR" for the NCAR fancy characters and "GKS" for the standard
*        GKS san-serif fount.   The former is intended for hardcopy
*        publication-quality plots, since it is relatively slow; the
*        latter is intended for normal interactive graphics requiring
*        rapid plotting, and it is clearer on small plots.  The
*        suggested default is the current value. ["GKS"]
*     IN1 = NDF (Read)
*        The NDF to be normalised.
*     IN2 = NDF (Read)
*        The NDF to which IN1 will be normalised. 
*     MAJTIC( 2 ) = _REAL (Read)
*        The parameter controlling the numbers of major tick marks
*        for the x and y axes.  (Number used is between MAJTIC+2 and
*        5*MAJTIC/2+4.) [3.,3.]
*     MINPIX = _INTEGER (Read)
*        The minimum number of good pixels required in a bin before it
*        contributes to the fitted line.  It must be in the range 1 to
*        the number of pixels per bin. [2]
*     MINTIC( 2 ) = _REAL (Read)
*        The number of minor tick marks between each major tick mark
*        for the x and y axes.  A negative value forces the graphics
*        package to compute appropriate values. [-1.,-1.]
*     NBIN = _INTEGER (Read)
*        The number of bins to use when binning the scatter plot prior
*        to fitting a straight line, in the range 2 to 10000. [50]
*     NITER = _INTEGER (Read)
*        The number of iterations performed to reject bad data values
*        in the range 0 to 100. [2]
*     NSIGMA = _REAL (Read)
*        The number of standard deviations at which bad data is
*        rejected.  It must lie in the range 0.1 to 1.0E6. [3.0]
*     OFFSET = _REAL (Write)
*        An output parameter giving the offset in the linear
*        normalisation expression: IN1 = SLOPE * IN2 + OFFSET.
*     ORDLAB = LITERAL (Read)
*        A title for the plots y axis.  Only the first 50 characters
*        are used.  The default is "Data value in ^NDF" where ^NDF is
*        replaced by the name of the NDF associated with IN1.
*        []
*     OUT = NDF (Write)
*        An optional output NDF to hold a version of IN1 which is 
*        normalised to IN2.  A null (!) value indicates that an output
*        NDF is not required.
*     OUTTIC = _LOGICAL (Read)
*        TRUE if the axis tick marks are to appear on the outside of
*        the axes instead of inside. [FALSE]
*     PCRANGE( 2 ) = _REAL (Read)
*        This parameter takes two real values in the range 0 to 100 and
*        is used to modify the action of the auto-scaling algorithm
*        which scales the plots.  The two values correspond to the
*        percentage points in the histogram of IN2 at which the lower
*        and upper cuts on data value are placed.  With the default
*        value, the plots will omit those pixels which lie in the lower
*        and upper 2 percent intensity range of IN2. [2,98]
*     PTITLE = LITERAL (Read)
*        A title for the top of the plot.  Only the first 50 characters
*        are used. ["Normalization plot"]
*     PXSIZE = _REAL (Read)
*        The horizontal size of the plot in metres.  If a value less
*        than the default is requested, then the plot will appear at
*        the bottom left of the current picture. [The size of the
*        current picture]
*     PYSIZE = _REAL (Read)
*        The vertical size of the plot in metres.  If a value less than
*        the default is requested, then the plot will appear at the
*        bottom left of the current picture. [The size of the current
*        picture]
*     SLOPE = _REAL (Write)
*        An output parameter giving the slope of the linear
*        normalisation expression: IN1 = SLOPE * IN2 + OFFSET.
*     TITLE = LITERAL (Read)
*        Value for the title of the output NDF.  A null value will cause
*        the title of the NDF supplied for parameter IN1 to be used
*        instead. [!]

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     normalize cl123a cl123b cl123c
*        This normalises NDF cl123a to the NDF cl123b.  A plot of the
*        fit is made on the current graphics device, and the resulting
*        normalisation scale and offset are written only to the
*        normalize.sdf parameter file (as in the all the examples below
*        except where noted).  The NDF cl123c is the normalised version
*        of the input cl123a.
*     normalize cl123a cl123b cl123c title="Gain calibration"
*        This normalises NDF cl123a to the NDF cl123b.  A plot of the
*        fit is made on the current graphics device with the title
*        "Gain calibration".  The NDF cl123c is the normalised version
*        of the input cl123a.
*     normalize cl123a cl123b cl123c offset=(shift) slope=(scale)
*        This normalises NDF cl123a to the NDF cl123b.  A plot of the
*        fit is made on the current graphics device.  The resulting
*        normalisation scale and offset are written to the ICL
*        variables SCALE and SHIFT respectively, where they could be
*        passed to another application via an ICL procedure.  The NDF
*        cl123c is the normalised version of the input cl123a.
*     normalize in2=old in1=new out=! device=xwindows
*        This normalises NDF new to the NDF old.  A plot of the fit is
*        made on the xwindows device.  No output NDF is produced.
*     normalize in1=new in2=old out=young niter=5 pcrange=[3,98.5]
*        This normalises NDF new to the NDF old.  It has five iterations
*        to reject outliers from the linear regression, and forms the
*        regression using pixels in old whose data values lie between
*        the 3 and 98.5 percentiles, comparing with the corresponding
*        pixels in new.  A plot of the fit is made on the current
*        graphics device.  The NDF young is the normalised version of
*        the input new.

*  Notes:
*     -  Provided the application does not fail two pictures are stored
*     in the graphics database: a FRAME of the specified size containing
*     the title, annotated axes and the plot; and a DATA picture with
*     each world co-ordinate being the pixel value of each NDF.  Both
*     pictures have comment "KAPPA - Normalize".  The associated NDFs
*     are not stored in the database.

*  Implementation Status:
*     -  The routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS and HISTORY components of an NDF,
*     and propagates all extensions to the output NDF.  All propagated
*     components come from the NDF to be normalised.
*     -  At the moment, variance values are not used in the fitting
*     algorithm but are modified in the output NDF to take account of
*     the scaling introduced by the normalisation.  (A later version may
*     take account of variances in the fitting algorithm.)
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  Only _REAL data can be processed directly.  Other non-complex
*     numeric data types will undergo a type conversion before
*     processing occurs.  _DOUBLE data cannot be processed due to a
*     loss of precision.
*     -  The pixel bounds of the two input NDFs are matched by trimming
*     before calculating the normalisation constants, and are mapped as
*     vectors to allow processing of NDFs of any dimensionality.  An
*     output NDF may optionally be produced which is based on the
*     first input NDF (IN1) by applying the calculated normalisation
*     constants to IN1.

*  Related Applications:
*     CCDPACK: MAKEMOS.

*  Implementation Deficiencies:
*     -  Variances in the input data could be used in the fitting
*     algorithm but are not at the moment.
*     -  Bad pixels are nearly always assumed to be present.
*     -  NCAR does strange things if the plot is too small, so ensure
*     that the current picture covers at least 10 per cent of the
*     plotting area.

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-JUN-1990 (DSB):
*        Original version.
*     22-JUN-1990 (DSB):
*        Graphics added.
*     1990 October 4 (MJC):
*        Added standard KAPPA axis-style parameters, removed tabs, 
*        extended and reformatted the prologue, added some contextual
*        error messages, passed pointers as arrays, and added unmapping
*        calls to prevent the output NDF from being deleted.
*     1991 August 20 (MJC):
*        Added FONT parameter.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1995 May 2 (MJC):
*        Sets the bad-pixel flags.  Removed old histogram subroutine
*        calls.  Used PSX for workspace.  Made the Usage and Examples
*        lowercase.  Added Related Applications.  Title propagated.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'PAR_ERR'          ! Parameter-system error constants
      INCLUDE 'PRM_PAR'          ! Data-type constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER HISIZE             ! Size of histogram to use for
                                 ! calculating auto scaling range.
      PARAMETER ( HISIZE = 1000 )

*  Local Variables:
      LOGICAL  BAD               ! True if any bad pixels found
      LOGICAL  DEFIND            ! True if an NDF component is in a 
                                 ! defined state
      INTEGER  DEFMIN            ! Default for minimum number of pixels
      REAL     DRANGE( 2 )       ! Limits on IN2 data values to be used
      REAL     DRDEF( 2 )        ! Suggested default limits on IN2 data
                                 ! values
      CHARACTER * ( 4 ) FOUNT    ! Fount type
      INTEGER  ISTAT             ! Temporary status value
      INTEGER  LENGTH            ! Used length of a character string
      REAL     MAJTIC( 2 )       ! Parameters controlling the numbers of
                                 ! major tick marks along x and y axes
                                 ! respectively
      REAL     MAX2              ! Max. value in IN2 data array
      INTEGER  MAXPOS( 2 )       ! Position of maximum in IN2 data array
      REAL     MIN2              ! Min. value in IN2 data array
      INTEGER  MINPOS( 2 )       ! Position of minimum in IN2 data array
      INTEGER  MINPIX            ! Min. no. of good pixels required per
                                 ! bin in the fitting algorithm
      REAL     MINTIC( 2 )       ! Numbers of minor tick marks along x
                                 ! and y axes respectively
      INTEGER  NBAD2             ! No. of bad pixels in IN2 data array
      INTEGER  NBIN              ! No. of bins to use in the fitting
                                 ! algorithm
      INTEGER  NDF1S             ! NDF section identifier for input IN1
      INTEGER  NDF2S             ! NDF section identifier for input IN2
      INTEGER  NDF1B             ! Base NDF identifier for input IN1
      INTEGER  NDFOUT            ! NDF identifier for OUT
      INTEGER  NELS              ! No. of elements mapped from the NDF 
                                 ! sections
      INTEGER  NEL1B             ! No. of elements mapped from base
                                 ! NDF of IN1
      INTEGER  NEWPIC            ! The AGI identifier for the FRAME
                                 ! picture
      INTEGER  NITER             ! No. of rejection iterations to
                                 ! perform in the fitting algorithm
      REAL     NSIGMA            ! No. of standard deviations at which
                                 ! data is rejected in fitting algorithm
      REAL     OFFSET            ! Constant offset in final fit
      LOGICAL  OUTRQD            ! True if an output NDF is to be
                                 ! generated, false otherwise.
      LOGICAL  OUTTIC            ! True if axis tick marks are to be
                                 ! placed outside the box instead of
                                 ! inside
      REAL     PCDEF( 2 )        ! Suggested default percentage range
      REAL     PCRANG( 2 )       ! Percentage range for autoscaling
      INTEGER  PIC0              ! AGI identifier for current picture
      LOGICAL  PLOT              ! True if a plot is to be produced
      INTEGER  PNT1S( 1 )        ! Pointer to mapped IN1 section data
                                 ! array
      INTEGER  PNT2S( 1 )        ! Pointer to mapped IN2 section data
                                 ! array
      INTEGER  PNT1BD( 1 )       ! Pointer to mapped base IN1 data
                                 ! array
      INTEGER  PNTOD( 1 )        ! Pointer to mapped output data array
      INTEGER  PNT1BV( 1 )       ! Pointer to mapped base IN1 variance
                                 ! array
      INTEGER  PNTOV( 1 )        ! Pointer to mapped output variance
                                 ! array
      INTEGER  PNTW0             ! Pointer to temporary work space
      INTEGER  PNTW1             ! Pointer to temporary work space
      INTEGER  PNTW2             ! Pointer to temporary work space
      INTEGER  PNTW3             ! Pointer to temporary work space
      INTEGER  PNTW4             ! Pointer to temporary work space
      INTEGER  PNTW5             ! Pointer to temporary work space
      REAL     SLOPE             ! Gradient of final fit
      REAL     TICDEF( 2 )       ! Suggested default axis-tick values
      CHARACTER * 50    TTL      ! The title for the plot
      CHARACTER * 50    TTLX     ! The title for the X axis of the plot
      CHARACTER * 50    TTLY     ! The title for the Y axis of the plot
      LOGICAL  VAR1              ! True if IN1 has a defined variance
                                 ! component
      INTEGER        ZONE        ! The SGS zone corresponding to the 
                                 ! plot FRAME picture

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an NDF context.
      CALL NDF_BEGIN

*  Get NDF identifiers for the two input NDFs.
      CALL NDF_ASSOC( 'IN1', 'READ', NDF1S, STATUS )
      CALL NDF_ASSOC( 'IN2', 'READ', NDF2S, STATUS )

*  If an error occurred, abort.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Get an NDF identifier for the output NDF. The output is based on IN1
*  and inherits all the components of IN1 except the data and variance
*  arrays.
      CALL NDF_PROP( NDF1S, 'WCS,Axis,Quality,Units', 'OUT', NDFOUT,
     :               STATUS )

*  If an output NDF was obtained successfully...
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Clone the base NDF identifier for IN1 for use when calculating the
*  output values.
         CALL NDF_CLONE( NDF1S, NDF1B, STATUS )


*  Set the OUTRQD flag to indicate that the output values should be
*  calculated.
         OUTRQD = .TRUE.

*  If a null value for OUT was given, set OUTRQD flag appropriately
*  and clear the error condition.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN

         OUTRQD = .FALSE.
         CALL ERR_ANNUL( STATUS )

         CALL MSG_OUT( 'REPORT', ' ', STATUS )
         CALL MSG_OUT( 'REPORT', ' No output NDF will be created',
     :                  STATUS )
         CALL MSG_OUT( 'REPORT', ' ', STATUS )

      END IF

*  Create sections of the two input NDFs with matched pixel bounds
*  by trimming the input NDFs.
      CALL NDF_MBND( 'TRIM', NDF1S, NDF2S, STATUS )

*  Map the data arrays of the two NDF sections.
      CALL NDF_MAP( NDF1S, 'DATA', '_REAL', 'READ', PNT1S, NELS,
     :              STATUS )
      CALL NDF_MAP( NDF2S, 'DATA', '_REAL', 'READ', PNT2S, NELS,
     :              STATUS )

*  If an error has occured, abort.
      IF ( STATUS .NE. SAI__OK ) GOTO 999
       
*  Get percentage histogram range for scaling the binning of NDF2, using
*  2 to 98 per cent as the dynamic defaults.
      PCDEF( 1 ) = 2.0
      PCDEF( 2 ) = 98.0
      CALL PAR_GDR1R( 'PCRANGE', 2, PCDEF, 0.0, 100.0, .FALSE., PCRANG,
     :                STATUS )

*  Convert them to fractions.
      PCRANG( 1 ) = 0.01 * PCRANG( 1 )
      PCRANG( 2 ) = 0.01 * PCRANG( 2 )

*  Find if there are any bad values in the section of the 2nd input NDF.
      CALL NDF_BAD( NDF2S, 'Data', .FALSE., BAD, STATUS )

*  Get maximum and minimum values in the data array of the IN2 NDF 
*  section.
      CALL KPG1_MXMNR( BAD, NELS, %VAL( PNT2S( 1 ) ), NBAD2,
     :                 MAX2, MIN2, MAXPOS, MINPOS, STATUS )
      BAD = NBAD2 .NE. 0

*  Get temporary workspace to hold the histogram, and map it.
      CALL PSX_CALLOC( HISIZE, '_INTEGER', PNTW0, STATUS )

*  If an error has occurred, abort.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NORMALIZE_WSP1',
     :     'NORMALIZE: Unable to get workspace to form the histogram.',
     :     STATUS )
         CALL PSX_FREE( PNTW0, STATUS )
         GOTO 999
      END IF

*  Generate the histogram of the section of IN2.
      CALL KPG1_GHSTR( BAD, NELS, %VAL( PNT2S( 1 ) ), HISIZE,
     :                 MAX2, MIN2, %VAL( PNTW0 ), STATUS )

*  Find the data values in IN2 which correspond to the required
*  percentage histogram points.
      CALL KPG1_HSTFR( HISIZE, %VAL( PNTW0 ), MAX2, MIN2, 2, PCRANG,
     :                 DRDEF, STATUS )

*  Unmap and release the temporary work space used to hold the 
*  histogram.
      CALL PSX_FREE( PNTW0, STATUS )

*  Obtain a data range to limit the data to be fitted. Only pixels for
*  which the IN2 data value is inside the given range are used. The
*  auto-scaled range calculated above is used as the dynamic default.
      CALL PAR_GDR1R( 'DATARANGE', 2, DRDEF, VAL__MINR, VAL__MAXR,
     :                .FALSE., DRANGE, STATUS )

*  Get required scalar parameters.
      CALL PAR_GDR0I( 'NBIN', 50, 2, 100000, .TRUE., NBIN, STATUS )
      CALL PAR_GDR0I( 'NITER', 2, 0, 100, .TRUE., NITER, STATUS )
      CALL PAR_GDR0R( 'NSIGMA', 3.0, 0.1, 1.0E6, .TRUE., NSIGMA,
     :                STATUS )
      DEFMIN = MAX( 1, NELS / NBIN )
      CALL PAR_GDR0I( 'MINPIX', DEFMIN, 1, MIN( DEFMIN, NELS / NBIN ),
     :                .FALSE., MINPIX, STATUS )

*  Start a new error context.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL ERR_MARK

*  Open a graphics workstation for NCAR AUTOGRAPH output.
         CALL NCROPN( 'CLEAR', 'DEVICE', 'PXSIZE', 'PYSIZE', 
     :                'KAPPA - Normalize', PLOT, PIC0, NEWPIC, ZONE,
     :                STATUS )

*  Get titles for the plot, and labels for the X and Y axes.
         CALL PAR_DEF0C( 'PTITLE', 'normalisation plot', STATUS )
         CALL PAR_GET0C( 'PTITLE', TTL, STATUS )

         CALL NDF_MSG( 'XNAME', NDF2S )
         CALL MSG_LOAD( 'REPORT', 'Data value in ^XNAME', TTLX, LENGTH, 
     :                   STATUS )
         CALL PAR_DEF0C( 'ABSLAB', TTLX( :LENGTH ), STATUS )
         CALL PAR_GET0C( 'ABSLAB', TTLX, STATUS )
         
         CALL NDF_MSG( 'YNAME', NDF1S )
         CALL MSG_LOAD( 'REPORT', 'Data value in ^YNAME', TTLY, LENGTH, 
     :                   STATUS )
         CALL PAR_DEF0C( 'ORDLAB', TTLY( :LENGTH ), STATUS )
         CALL PAR_GET0C( 'ORDLAB', TTLY, STATUS )

*  Get the number of minor ticks, assigning the dynamic defaults.
         TICDEF( 1 ) = -1.
         TICDEF( 2 ) = -1.
         CALL PAR_GDR1R( 'MINTIC', 2, TICDEF, -1., VAL__MAXR, .FALSE.,
     :                   MINTIC, STATUS )

*  Get the parameter controlling the number of major ticks per axis,
*  assigning the dynamic defaults.
         TICDEF( 1 ) = 3.
         TICDEF( 2 ) = 3.
         CALL PAR_GDR1R( 'MAJTIC', 2, TICDEF, -1., VAL__MAXR, .FALSE.,
     :                   MAJTIC, STATUS )

*  Are the tick marks on the outside of the axes?
         CALL PAR_GTD0L( 'OUTTIC', .TRUE., .TRUE., OUTTIC, STATUS )

*  Get the fount.  Although NCAR is the default, either must be
*  selected to prevent persistence from earlier invocations.
         CALL PAR_CHOIC( 'FONT', 'GKS', 'GKS,NCAR', .TRUE., FOUNT,
     :                   STATUS )
         IF ( FOUNT .EQ. 'GKS ' ) THEN
            CALL AGPWRT( 0.0, 0.0, ' ', 0, 0, 0, -100 )
         ELSE IF ( FOUNT .EQ. 'NCAR' ) THEN
            CALL AGPWRT( 0.0, 0.0, ' ', 0, 0, 0, 100 )
         END IF

*  If an error occured in the current error context, then no graphics
*  will be produced, but the fit will still be calculated.  PLOT
*  will be true at this point if the device was opened.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP('NORMALIZE_NOPLOT',
     :                   'NORMALIZE: No plot will be produced', STATUS )
            CALL ERR_FLUSH( STATUS )
            PLOT = .FALSE.
         END IF

*  End the current error context.
         CALL ERR_RLSE
      END IF

*  Get temporary workspace for use in NMPLOT, and map it.
      CALL PSX_CALLOC( NBIN, '_INTEGER', PNTW1, STATUS )
      CALL PSX_CALLOC( NBIN, '_REAL', PNTW2, STATUS )
      CALL PSX_CALLOC( NBIN, '_REAL', PNTW3, STATUS )
      CALL PSX_CALLOC( NBIN, '_DOUBLE', PNTW4, STATUS )
      CALL PSX_CALLOC( NBIN, '_REAL', PNTW5, STATUS )

*  If an error has occurred, abort annulling any workspace already
*  obtained.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NORMALIZE_WSP2',
     :     'NORMALIZE: Unable to get workspace to calculate the '/
     :     /'mapping.', STATUS )
         CALL PSX_FREE( PNTW1, STATUS )
         CALL PSX_FREE( PNTW2, STATUS )
         CALL PSX_FREE( PNTW3, STATUS )
         CALL PSX_FREE( PNTW4, STATUS )
         CALL PSX_FREE( PNTW5, STATUS )
         GOTO 999
      END IF

*  Call NMPLOT to calculate the linear function which normalises IN1 to
*  IN2.
      CALL NMPLOT( %VAL( PNT2S( 1 ) ), %VAL( PNT1S( 1 ) ), NELS,
     :             DRANGE( 1 ), DRANGE( 2 ), NBIN, NITER, NSIGMA,
     :             MINPIX, NDF2S, NDF1S, PLOT, TTL, TTLX, TTLY, MINTIC,
     :             MAJTIC, OUTTIC, PIC0, %VAL( PNTW1 ), %VAL( PNTW2 ),
     :              %VAL( PNTW3 ), %VAL( PNTW4 ), %VAL( PNTW5 ), SLOPE,
     :             OFFSET, STATUS )

*  Unmap and release the temporary work space.
      CALL PSX_FREE( PNTW1, STATUS )
      CALL PSX_FREE( PNTW2, STATUS )
      CALL PSX_FREE( PNTW3, STATUS )
      CALL PSX_FREE( PNTW4, STATUS )
      CALL PSX_FREE( PNTW5, STATUS )

*  Close down AGI and SGS if plotting occurred.
      CALL AGS_DEASS( 'DEVICE', .NOT. PLOT, STATUS )

*  Unmap the NDF sections.
      CALL NDF_UNMAP( NDF1S, '*', STATUS )
      CALL NDF_UNMAP( NDF2S, '*', STATUS )

*  Write the values of the gradient and offset of the final fit to the
*  parameters SLOPE and OFFSET.
      CALL PAR_PUT0R( 'SLOPE', SLOPE, STATUS )
      CALL PAR_PUT0R( 'OFFSET', OFFSET, STATUS )

*  If an output NDF is to be made containing a normalised copy of IN1,
*  check that calculated slope was not zero.
      IF ( OUTRQD ) THEN

         IF ( ABS( SLOPE ) .LT. VAL__SMLR ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP('NORMALIZE_ZERO',
     :        'NORMALIZE: Zero slope.  Cannot normalize output.',
     :        STATUS )
            GOTO 999
         END IF

*  Map data arrays from base IN1 and output NDFs.
         CALL NDF_MAP( NDF1B, 'DATA', '_REAL', 'READ', PNT1BD, NEL1B,
     :                 STATUS )
         CALL NDF_MAP( NDFOUT, 'DATA', '_REAL', 'WRITE', PNTOD, NEL1B,
     :                 STATUS )

         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Produce the output data array by applying the inverse of the 
*  calculated normalisation function to the IN1 data array.
         CALL KPG1_SCLOF( %VAL( PNT1BD( 1 ) ), NEL1B,
     :                    DBLE( 1.0 / SLOPE ), DBLE( -OFFSET / SLOPE ),
     :                    %VAL( PNTOD( 1 ) ), BAD, STATUS )

*  Set the bad pixel flag for the output data array.
          CALL NDF_SBAD( BAD, NDFOUT, 'Data', STATUS )

*  Unmap the data arrays so they become defined, and save resources
*  whilst the variance array is being normalised.
          CALL NDF_UNMAP( NDF1B, 'Data', STATUS )
          CALL NDF_UNMAP( NDFOUT, 'Data', STATUS )

*  If IN1 contains a variance component, copy it to the output and
*  multiply by "1/SLOPE" squared to take account of the scaling of the 
*  data array.
         CALL NDF_STATE( NDF1B, 'VARIANCE', VAR1, STATUS)

         IF ( VAR1 ) THEN

            CALL NDF_MAP( NDF1B, 'VARIANCE', '_REAL', 'READ', PNT1BV,
     :                    NEL1B, STATUS )
            CALL NDF_MAP( NDFOUT, 'VARIANCE', '_REAL', 'WRITE', PNTOV,
     :                    NEL1B, STATUS )

            IF ( STATUS .NE. SAI__OK ) GOTO 999
       
            CALL KPG1_SCLOF( %VAL( PNT1BV( 1 ) ), NEL1B, 
     :                       DBLE( 1.0 / SLOPE**2 ), 0.0D0, 
     :                       %VAL( PNTOV( 1 ) ), BAD, STATUS )

*  Set the bad pixel flag for the output variance array.
            CALL NDF_SBAD( BAD, NDFOUT, 'Variance', STATUS )

         END IF

*  Get a value for the output title and store it.
         CALL NDF_CINP( 'TITLE', NDFOUT, 'TITLE', STATUS )

      END IF

*  Clear up.
 999  CONTINUE

*  If an output NDF was created, but its data array is still undefined,
*  then delete the output NDF. (N.B. including the ERR_MARK and ERR_RLSE
*  routines cause previously generated error reports to disappear for
*  some reason. PCTR has been informed).
C      CALL ERR_MARK
      ISTAT = SAI__OK

      CALL NDF_VALID( NDFOUT, OUTRQD, ISTAT )
      IF ( OUTRQD ) THEN
         CALL NDF_STATE( NDFOUT, 'Data', DEFIND, ISTAT )
         IF ( .NOT. DEFIND ) CALL NDF_DELET( NDFOUT, ISTAT )
      END IF

C      CALL ERR_ANNUL( ISTAT )
C      CALL ERR_RLSE

*  End the NDF context
      CALL NDF_END( STATUS )

      END
