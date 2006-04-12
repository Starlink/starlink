      SUBROUTINE COLLAPSE( STATUS )
*+
*  Name:
*     COLLAPSE

*  Purpose:
*     Reduce the number of axes in an N-dimensional NDF by compressing 
*     it along a nominated axis.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL COLLAPSE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application collapses a nominated pixel axis of an 
*     N-dimensional NDF, producing an output NDF with one fewer
*     pixels axes than the input NDF.  A specified range of axis values 
*     can be used instead of the whole axis (see parameters LOW and
*     HIGH).
*
*     For each output pixel, all corresponding input pixel values 
*     between the specified bounds of the the nominated axis to be
*     collapsed are combined together using one of a selection of
*     estimators, including a mean, mode, or median, to produce the
*     output pixel value.
*
*     Possible uses include such things as collapsing a range of
*     wavelength planes in a three-dimensional RA/DEC/Wavelength cube to
*     produce a single two-dimensional RA/DEC image, or collapsing a
*     range of slit positions in a two-dimensional slit 
*     position/wavelength image to produce a one-dimensional wavelength
*     array.

*  Usage:
*     collapse in out axis [low] [high] [estimator] [wlim]

*  ADAM Parameters:
*     AXIS = LITERAL (Read)
*        The axis along which to collapse the NDF.  This can be
*        specified by its integer index within the current Frame of the 
*        input NDF (in the range 1 to the number of axes in the current
*        Frame), or by its symbol string.  A list of acceptable values
*        is displayed if an illegal value is supplied.  If the axes of 
*        the current Frame are not parallel to the NDF pixel axes, then
*        the pixel axis which is most nearly parallel to the specified
*        current Frame axis will be used.
*     ESTIMATOR = LITERAL (Read)
*        The method to use for estimating the output pixel values.  It
*        can be one of the following options.  The first four are
*        more for general collapsing, and the remainder are for cube
*        analysis.
*          "Mean"   -- Mean value
*          "WMean"  -- Weighted mean in wich each data value is weighted
*                      by the reciprocal of the associated variance.  
*          "Mode"   -- Modal value
*          "Median" -- Median value.  Note that this is extremely memory
*                      and CPU intensive for large datasets; use with 
*                      care!  If strange things happen, use "Mean".
*
*          "Absdev" -- Mean absolute deviation from the unweighted mean.
*          "Comax"  -- Co-ordinate of the maximum value.
*          "Comin"  -- Co-ordinate of the minimum value.
*          "Integ"  -- Integrated value, being the sum of the products 
*                      of the value and pixel width in world
*                      co-ordinates.
*          "Iwc"    -- Intensity-weighted co-ordinate, being the sum of 
*                      each value times its co-ordinate, all divided by
*                      the integrated value (see the "Integ" option).
*          "Iwd"    -- Intensity-weighted dispersion of the
*                      co-ordinate, normalised like "Iwc" by the 
*                      integrated value.
*          "Max"    -- Maximum value.
*          "Min"    -- Minimum value.
*          "Rms"    -- Root-mean-square value.
*          "Sigma"  -- Standard deviation about the unweighted mean.
*          "Sum"    -- The total value.
*        ["Mean"]
*     HIGH = LITERAL (Read)
*        A value for the axis specified by parameter AXIS.  For example,
*        if AXIS is 3 and the current Frame of the input NDF has axes
*        RA/DEC/Wavelength, then a wavelength value should be supplied.
*        If, on the other hand, the current Frame in the NDF was the
*        PIXEL Frame, then a pixel co-ordinate value would be required 
*        for the third axis (note, the pixel with index I covers a range
*        of pixel co-ordinates from (I-1) to I).  Together with 
*        parameter LOW, this parameter gives the range of axis values to
*        be compressed.  Note, HIGH and LOW should not be equal.  If a
*        null value (!) is supplied for either HIGH or LOW, the entire
*        range of the axis is collapsed. [!]
*     IN  = NDF (Read)
*        The input NDF. 
*     LOW = LITERAL (Read)
*        A value for the axis specified by parameter AXIS.  For example,
*        if AXIS is 3 and the current Frame of the input NDF has axes
*        RA/DEC/Wavelength, then a wavelength value should be supplied.
*        If, on the other hand, the current Frame in the NDF was the
*        PIXEL Frame, then a pixel co-ordinate value would be required
*        for the third axis (note, the pixel with index I covers a range
*        of pixel co-ordinates from (I-1) to I).  Together with 
*        parameter HIGH, this parameter gives the range of axis values
*        to be compressed.  Note, LOW and HIGH should not be equal.
*        If a null value (!) is supplied for either LOW or HIGH, the
*        entire range of the axis is collapsed. [!]
*     OUT = NDF (Write)
*        The output NDF.
*     TITLE = LITERAL (Read)
*        Title for the output NDF structure.  A null value (!)
*        propagates the title from the input NDF to the output NDF. [!]
*     WLIM = _REAL (Read)
*        If the input NDF contains bad pixels, then this parameter
*        may be used to determine the number of good pixels which must
*        be present within the range of collapsed input pixels before a 
*        valid output pixel is generated.  It can be used, for example,
*        to prevent output pixels from being generated in regions where
*        there are relatively few good pixels to contribute to the
*        collapsed result.
*
*        WLIM specifies the minimum fraction of good pixels which must
*        be present in order to generate a good output pixel.  If this 
*        specified minimum fraction of good input pixels is not present,
*        then a bad output pixel will result, otherwise an good output 
*        value will be calculated.  The value of this parameter should 
*        lie between 0.0 and 1.0 (the actual number used will be rounded
*        up if necessary to correspond to at least one pixel). [0.3]

*  Examples:
*     collapse cube slab lambda 4500 4550 
*        The current Frame in the input three-dimensional NDF called 
*        cube has axes with labels "RA", "DEC" and "Lambda", with the 
*        lambda axis being parallel to the third pixel axis.  The above 
*        command extracts a slab of the input cube between wavelengths 
*        4500 and 4550 Angstroms, and collapses this slab into a single
*        two-dimensional output NDF called slab with RA and DEC axes. 
*        Each pixel in the output NDF is the mean of the corresponding 
*        input pixels with wavelengths between 4500 and 4550 Angstroms.
*     collapse cube slab 3 4500 4550 
*        The same as above except the axis to collapse along is
*        specified by index (3) rather than label (lambda).
*     collapse cube slab 3 101.0 134.0
*        This is the same as the above examples, except that the current
*        Frame in the input NDF has been set to the PIXEL Frame (using
*        WCSFRAME), and so the high and low axis values are specified in
*        pixel co-ordinates instead of Angstroms.  Note the difference 
*        between floating point pixel co-ordinates, and integer pixel 
*        indices (for instance the pixel with index 10 extends from 
*        pixel co-ordinate 9.0 to pixel co-ordinate 10.0).
*     collapse cube slab 3 low=99.0 high=100.0
*        This is the same as the above examples, except that a single
*        pixel plane in the cube (pixel 100) is used to create the
*        output NDF.  Following the usual definition of pixel 
*        co-ordinates, pixel 100 extends from pixel co-ordinate 99.0 to
*        pixel co-ordinate 100.0.  So the given HIGH and LOW values 
*        encompass the single pixel plane at pixel 100.

*  Notes:
*     -  The collapse is always performed along one of the pixel axes,
*     even if the current Frame in the input NDF is not the PIXEL Frame.
*     Special care should be taken if the current Frame axes are not
*     parallel to the pixel axes.  The algorithm used to choose the 
*     pixel axis and the range of values to collapse along this pixel
*     axis proceeds as follows.
*     
*     The current Frame co-ordinates of the central pixel in the input
*     NDF are determined (or some other point if the co-ordinates of the
*     central pixel are undefined).  Two current Frame positions are
*     then generated by substituting in turn into this central position 
*     each of the HIGH and LOW values for the current Frame axis 
*     specified by parameter AXIS.  These two current Frame positions
*     are transformed into pixel co-ordinates, and the projections of 
*     the vector joining these two pixel positions on to the pixel axes 
*     are found.  The pixel axis with the largest projection is selected
*     as the collapse axis, and the two end points of the projection 
*     define the range of axis values to collapse.

*  Related Applications:
*     KAPPA: WCSFRAME, COMPAVE, COMPICK, COMPADD.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, VARIANCE,
*     LABEL, TITLE, UNITS, WCS and HISTORY components of the input NDF
*     and propagates all extensions.  QUALITY is not propagated.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.
*     -  Any number of NDF dimensions is supported.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-AUG-2000 (DSB):
*        Original version.
*     27-OCT-2000 (DSB):
*        Modified to avoid allocating unnecessary workspace if the last
*        axis is being collapsed.
*     14-DEC-2001 (DSB):
*        Renamed wieghted mean estimator as "Wmean" and added new
*        unweighted mean estimator "Mean".
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     2-DEC-2005 (DSB):
*        Move the code for creating the output WCS FrameSet into a KPS
*        routine.
*     2005 December 22 (MJC):
*        Add MAX and MIN estimators.
*     2005 December 24 (MJC):
*        Add ABSDEV estimator.
*     2005 December 27 (MJC):
*        Add SIGMA estimator.
*     2005 December 28 (MJC):
*        Add RMS estimator.
*     2005 December 29 (MJC):
*        Add SUM estimator.
*     2005 December 30 (MJC):
*        Add INTEG, COMAX, COMIN, IWC, IWD estimators.
*     2006 January 2 (MJC):
*        Obtain co-ordinates and pass co-ordinate array for some
*        estimators.
*     2006 January 5 (MJC):
*        Added section headings in comments.  Copied FrameSet for
*        estimators using co-ordinates to fix bug.
*     2006 January 6 (MJC):
*        Obtain axis widths for Integ, Iwc, and Iwd options.
*     2006 January 8 (MJC):
*        Initialise UNITS for NDF_CGET call for INTEG estimator.
*     2006 January 20 (MJC):
*        Remove STATUS from a CHR_APPND, and tidy the widths correctly.
*     2006 January 27 (MJC):
*        For Integ estimator obtain co-ordinates and create workspace 
*        for widths instead of using AXIS-component widths.  Initialise
*        variance pointers.
*     9-FEB-2006 (DSB):
*        Report an error if the selected WCS axis has a constant value.
*     2006 February 10 (MJC):
*        Update the units for Iwc and Iwd estimators, as the incorrect
*        published formulae for these were dimensionless.
*     2006 February 13 (MJC):
*        Allow for very large datsets by blocking into manageable
*        sections.
*     2006 February 14 (MJC):
*        Correct calculation of the blocked bounds for the output NDF.
*     2006 March 3 (MJC):
*        Correct the bounds for blocking.  Use an NDF section to define
*        the number of blocks when the compression is derived from only
*        part of the collapse axis.
*     2006 April 11 (MJC):
*        Obtain bounds for each block passed to KPS1_CLPSx, rather
*        than the full array.  Do not use workspace if higher
*        dimensions are 1.
*     {enter_further_changes}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No default typing allowed

*  Global Constants:
      INCLUDE  'SAE_PAR'         ! Global SSE definitions
      INCLUDE  'PAR_ERR'         ! Parameter-system errors
      INCLUDE  'NDF_PAR'         ! NDF_ public constants
      INCLUDE  'DAT_PAR'         ! HDS public constants
      INCLUDE  'AST_PAR'         ! AST constants and functions
      INCLUDE  'CNF_PAR'         ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER KPG1_FLOOR         ! Most positive integer .LE. a given
                                 ! real
      INTEGER KPG1_CEIL          ! Most negative integer .GE. a given
                                 ! real

*  Local Constants:
      INTEGER MAXPIX 
      PARAMETER ( MAXPIX = 8388608 ) ! Guestimate a size: 8 mega

*  Local Variables:
      CHARACTER AUNITS*( 30 )    ! Units of co-ordinates 
      CHARACTER ATTRIB* ( 10 )   ! AST attribute name
      CHARACTER COMP * ( 13 )    ! List of components to process
      CHARACTER DTYPE*( NDF__SZFTP ) ! Numeric type for output arrays
      CHARACTER ESTIM*6          ! Method to use to estimate collapsed
                                 ! values
      CHARACTER ITYPE*( NDF__SZTYP ) ! Numeric type for processing
      CHARACTER LOC1*(DAT__SZLOC)! Locator to the whole NDF
      CHARACTER LOC2*(DAT__SZLOC)! Locator to NDF AXIS array
      CHARACTER LOC3*(DAT__SZLOC)! Locator to copy of the original AXIS 
                                 ! array
      CHARACTER LOC4*(DAT__SZLOC)! Locator to cell of the new AXIS array
      CHARACTER LOC5*(DAT__SZLOC)! Locator to cell of the old AXIS array
      CHARACTER LOC6*(DAT__SZLOC)! Locator to component of the old cell
      CHARACTER NAME*(DAT__SZNAM)! The component name
      CHARACTER TTLC*255         ! Title of original current Frame
      CHARACTER UNITS*( 60 )     ! Units of data 
      INTEGER AEL                ! Number of collapse axis elements
      DOUBLE PRECISION AXHIGH    ! High bound of collapse axis in
                                 ! current Frame
      DOUBLE PRECISION AXLOW     ! Low bound of collapse axis in current
                                 ! Frame
      DOUBLE PRECISION CPOS( 2, NDF__MXDIM ) ! Two current Frame 
                               ! positions
      DOUBLE PRECISION CURPOS( NDF__MXDIM ) ! Valid current Frame 
                               ! position
      DOUBLE PRECISION DLBND( NDF__MXDIM ) ! Lower bounds in pixel 
                               ! co-ords
      DOUBLE PRECISION DUBND( NDF__MXDIM ) ! Upper bounds in pixel
                               ! co-ords
      DOUBLE PRECISION GRDPOS( NDF__MXDIM ) ! Valid grid Frame position
      DOUBLE PRECISION PIXPOS( NDF__MXDIM ) ! Valid pixel Frame position
      DOUBLE PRECISION PPOS( 2, NDF__MXDIM ) ! Two pixel Frame positions
      DOUBLE PRECISION PRJ       ! Vector length projected on to a pixel
                                 ! axis
      DOUBLE PRECISION PRJMAX    ! Maximum vector length projected on to
                                 ! an axis
      INTEGER AXES( NDF__MXDIM ) ! A list of axis indices
      INTEGER CFRM               ! Original Current Frame pointer
      INTEGER D                  ! A dimension size
      INTEGER EL1                ! Number of elements in an input mapped
                                 ! array
      INTEGER EL2                ! Number of elements in an output
                                 ! mapped array
      LOGICAL HIGHER             ! Significant dimensions above collapse
                                 ! axis?
      INTEGER I                  ! Loop count
      INTEGER IAXIS              ! Index of collapse axis within current
                                 ! Frame
      INTEGER IBL                ! Identifier for input-NDF block
      INTEGER IBLOCK             ! Loop counter for the NDF blocks
      INTEGER IBLSIZ( NDF__MXDIM ) ! Input-NDF sizes for processing 
                                 ! large datasets in blocks
      INTEGER IERR               ! Position of first numerical error
      INTEGER INDF1              ! Input NDF identifier
      INTEGER INDF2              ! Output NDF identifier
      INTEGER INDFS              ! Input NDF-seciton identifier
      INTEGER IPAXCO             ! Pointers to mapped d.p. axis array
      INTEGER IPCO               ! Pointers to mapped co-ordinate array
      INTEGER IPIN( 2 )          ! Pointers to mapped input arrays
      INTEGER IPIX               ! Index of PIXEL Frame within WCS 
                                 ! FrameSet
      INTEGER IPOUT( 2 )         ! Pointers to mapped output arrays
      INTEGER IPW1               ! Pointer to first work array
      INTEGER IPW2               ! Pointer to second work array
      INTEGER IPW3               ! Pointer to third work array
      INTEGER IPWID              ! Pointers to mapped width work array
      INTEGER IWCS               ! WCS FrameSet pointer
      INTEGER IWCSO              ! Output NDF's WCS FrameSet pointer
      INTEGER J                  ! Loop count
      INTEGER JAXIS              ! Index of collapse axis within PIXEL
                                 ! Frame
      INTEGER JHI                ! High pixel index for collapse axis
      INTEGER JLO                ! Low pixel index for collapse axis
      INTEGER LBND( NDF__MXDIM ) ! Lower pixel index bounds of the input
                                 ! NDF
      INTEGER LBNDO( NDF__MXDIM )! Lower pixel index bounds of the
                                 ! output NDF
      INTEGER LBNDS( NDF__MXDIM ) ! Lower pixel index bounds of the
                                 ! section of the input NDF
      INTEGER MAXSIZ             ! Maximum size of block along current
                                 ! dimension
      INTEGER MAP                ! PIXEL Frame to Current Frame Mapping
                                 ! pointer
      INTEGER NAXC               ! Original number of current Frame axes
      INTEGER NBLOCK             ! Number of NDF blocks
      INTEGER NCOMP              ! No. of components within cell of AXIS
                                 ! array
      INTEGER NERR               ! Number of numerical errors
      INTEGER NC                 ! Used length of string
      INTEGER NDIM               ! No. of pixel axes in input NDF
      INTEGER NDIMO              ! No. of pixel axes in output NDF
      INTEGER NVAL               ! Number of values obtained (1)
      INTEGER OBL                ! Identifier for output-NDF block
      INTEGER OBLSIZ( NDF__MXDIM ) ! Output-NDF sizes for processing 
                                 ! large datasets in blocks
      INTEGER UBND( NDF__MXDIM ) ! Upper pixel index bounds of the input
                                 ! NDF
      INTEGER UBNDO( NDF__MXDIM )! Upper pixel index bounds of the 
                                 ! output NDF
      INTEGER UBNDS( NDF__MXDIM ) ! Upper pixel index bounds of the
                                 ! section of the input NDF
      LOGICAL GOTAX              ! Does the NDF have an AXIS component?
      LOGICAL LOOP               ! Continue to loop through dimensions?
      LOGICAL USEALL             ! Use the entire collapse pixel axis?
      LOGICAL VAR                ! Process variances?
      REAL WLIM                  ! Value of WLIM parameter
*.

*  Check the global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Obtain input NDF and some of its AST Frames.
*  ============================================

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Start an NDF context.
      CALL NDF_BEGIN

*  Obtain the input NDF.
      CALL LPG_ASSOC( 'IN', 'READ', INDF1, STATUS )

*  Get the bounds of the NDF.
      CALL NDF_BOUND( INDF1, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Get the WCS FrameSet from the NDF.
      CALL KPG1_GTWCS( INDF1, IWCS, STATUS )

*  Extract the current and base Frames, and get the number of axes in 
*  the current Frame, and its title.
      CFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )
      NAXC = AST_GETI( CFRM, 'NAXES', STATUS )
      TTLC = AST_GETC( CFRM, 'TITLE', STATUS )

*  Find the index of the PIXEL Frame.
      CALL KPG1_ASFFR( IWCS, 'PIXEL', IPIX, STATUS )

*  Extract the Mapping from PIXEL Frame to Current Frame. 
      MAP = AST_GETMAPPING( IWCS, IPIX, AST__CURRENT, STATUS )

*  Report an error if the Mapping is not defined in either direction.
      IF( .NOT. AST_GETL( MAP, 'TRANINVERSE', STATUS ) .AND.
     :    STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', INDF1 )
         CALL MSG_SETC( 'T', TTLC )
         CALL ERR_REP( 'COLLAPSE_ERR1', 'The transformation from the '//
     :                 'current co-ordinate Frame of ''^NDF'' '//
     :                 '(^T) to pixel co-ordinates is not defined.', 
     :                 STATUS )

      ELSE IF( .NOT. AST_GETL( MAP, 'TRANFORWARD', STATUS ) .AND.
     :         STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', INDF1 )
         CALL MSG_SETC( 'T', TTLC )
         CALL ERR_REP( 'COLLAPSE_ERR2', 'The transformation from '/
     :                 /'pixel co-ordinates to the current '/
     :                 /'co-ordinate Frame of ''^NDF'' (^T) is not '/
     :                 /'defined.', STATUS )
      END IF

*  Select the collapse axis and limits thereon.
*  ============================================
 
*  Get the index of the current Frame axis defining the collapse 
*  direction.  Use the last axis as the dynamic default.
      IAXIS = NAXC
      CALL KPG1_GTAXI( 'AXIS', CFRM, 1, IAXIS, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999  

*  Get the bounding values for the specified current Frame axis defining
*  the height of the slab to be collapsed.
      AXLOW = AST__BAD
      CALL KPG1_GTAXV( 'LOW', 1, .TRUE., CFRM, IAXIS, AXLOW, NVAL, 
     :                 STATUS )

      AXHIGH = AST__BAD
      CALL KPG1_GTAXV( 'HIGH', 1, .TRUE., CFRM, IAXIS, AXHIGH, NVAL, 
     :                 STATUS )

*  If a null value was supplied for either of these parameters, annul 
*  the error and set a flag indicating that the whole axis should be
*  used.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         USEALL = .TRUE.
      ELSE
         USEALL = .FALSE.
      END IF

*  Determine which pixel axis is most nearly aligned with the selected 
*  WCS axis.
*  ===================================================================

*  Find an arbitrary position within the NDF which has valid current 
*  Frame co-ordinates. Both pixel and current Frame co-ordinates for 
*  this position are returned.
      DO I = 1, NDIM
         DLBND( I ) = DBLE( LBND( I ) - 1 )
         DUBND( I ) = DBLE( UBND( I ) )
      END DO
      CALL KPG1_ASGDP( MAP, NDIM, NAXC, DLBND, DUBND, PIXPOS, CURPOS, 
     :                 STATUS )

*  Convert the pixel position into a grid position.
      DO I = 1, NDIM
         GRDPOS( I ) = PIXPOS( I ) - LBND( I ) + 1.5
      END DO 

*  Create two copies of these current Frame co-ordinates.
      DO I = 1, NAXC
         CPOS( 1, I ) = CURPOS( I )
         CPOS( 2, I ) = CURPOS( I )
      END DO 

*  If no high and low values for the collapse axis were supplied, modify
*  the collapse axis values in these positions by an arbitrary amount.
      IF( USEALL ) THEN
         IF( CURPOS( IAXIS ) .NE. 0.0 ) THEN
            CPOS( 1, IAXIS ) = 0.99 * CURPOS( IAXIS )
            CPOS( 2, IAXIS ) = 1.01 * CURPOS( IAXIS )
         ELSE
            CPOS( 1, IAXIS ) = CURPOS( IAXIS ) + 1.0D-4
            CPOS( 2, IAXIS ) = CURPOS( IAXIS ) - 1.0D-4
         END IF

*  If high and low values for the collapse axis were supplied,
*  substitute these into these positions.
      ELSE
         CPOS( 1, IAXIS ) = AXHIGH
         CPOS( 2, IAXIS ) = AXLOW
      END IF

*  Transform these two positions into pixel co-ordinates.
      CALL AST_TRANN( MAP, 2, NAXC, 2, CPOS, .FALSE., NDIM, 2, PPOS,
     :                STATUS ) 

*  Find the pixel axis with the largest projection of the vector joining
*  these two pixel positions.  The collapse will occur along this pixel
*  axis.  Report an error if the positions do not have valid pixel
*  co-ordinates.
      PRJMAX = -1.0
      DO I = 1, NDIM
         IF ( PPOS( 1, I ) .NE. AST__BAD .AND.
     :        PPOS( 2, I ) .NE. AST__BAD ) THEN

            PRJ = ABS( PPOS( 1, I ) - PPOS( 2, I ) )
            IF ( PRJ .GT. PRJMAX ) THEN
               JAXIS = I
               PRJMAX = PRJ
            END IF

         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'COLLAPSE_ERR3', 'The WCS information is '//
     :                    'too complex (cannot find two valid pixel '//
     :                    'positions).', STATUS )
            GO TO 999
         END IF

      END DO

*  Report an error if the selected WCS axis is independent of pixel
*  position.
      IF ( PRJMAX .EQ. 0.0 ) THEN
         IF ( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'I', IAXIS )   
            CALL ERR_REP( 'COLLAPSE_ERR3B', 'The specified WCS axis '//
     :                    '(axis ^I) has a constant value over the '//
     :                    'whole NDF and so cannot be collapsed.',
     :                    STATUS )
         END IF
         GO TO 999
      END IF

*  Derive the pixel-index bounds along the collapse axis.
*  ======================================================

*  Choose the pixel index bounds of the slab to be collapsed on the
*  collapse pixel axis.  If no axis limits supplied, use the upper and
*  lower bounds.
      IF ( USEALL ) THEN
         JLO = LBND( JAXIS )
         JHI = UBND( JAXIS )

*  If limits were supplied...
      ELSE

*  Find the projection of the two test points on to the collapse axis.
         JLO = KPG1_FLOOR( REAL( MIN( PPOS( 1, JAXIS ), 
     :                                PPOS( 2, JAXIS ) ) ) ) + 1
         JHI = KPG1_CEIL( REAL( MAX( PPOS( 1, JAXIS ), 
     :                               PPOS( 2, JAXIS ) ) ) )

*  Ensure these are within the bounds of the pixel axis.
         JLO = MAX( LBND( JAXIS ), JLO )
         JHI = MIN( UBND( JAXIS ), JHI )

*  Report an error if there is no intersection.
         IF ( JLO .GT. JHI .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'COLLAPSE_ERR4', 'The axis range to '//
     :                    'collapse covers zero pixels (are the '//
     :                    'HIGH and LOW parameter values equal '//
     :                    'or outside the bounds of the NDF?)', 
     :                    STATUS )
            GO TO 999
         END IF

      END IF

*  Tell the user the range of pixels being collapsed.
      CALL MSG_SETI( 'I', JAXIS )
      CALL MSG_SETI( 'L', JLO )
      CALL MSG_SETI( 'H', JHI )
      CALL MSG_OUT( 'COLLAPSE_MSG1', '   Collapsing pixel axis ^I '//
     :             'from pixel ^L to pixel ^H inclusive...', STATUS )
      CALL MSG_BLANK( ' ', STATUS )
      AEL = JHI - JLO + 1

*  Propagate the input to the output NDF and define latter's bounds.
*  =================================================================

*  Create the output NDF by propagation from the input NDF.  This
*  results in history, etc., being passed on.  The shape and 
*  dimensionality will be wrong but this will be corrected later.
      CALL LPG_PROP( INDF1, 'Axis,Units', 'OUT', INDF2, STATUS )

*  Set the title of the output NDF.
      CALL KPG1_CCPRO( 'TITLE', 'TITLE', INDF1, INDF2, STATUS )

*  See if the input NDF has a Variance component.
      CALL NDF_STATE( INDF1, 'VARIANCE', VAR, STATUS )

*  Store a list of components to be accessed.
      IF ( VAR ) THEN
         COMP = 'DATA,VARIANCE'
      ELSE
         COMP = 'DATA'
      END IF

*  Determine the numeric type to be used for processing the input
*  data and variance (if any) arrays.  Since the subroutines that
*  perform the collapse need the data and variance arrays in the same
*  data type, the component list is used.  This application supports
*  single- and double-precision floating-point processing.
      CALL NDF_MTYPE( '_REAL,_DOUBLE', INDF1, INDF2, COMP, ITYPE, DTYPE,
     :                STATUS )

*  The output NDF will have one fewer axes than the input NDF.
      NDIMO = NDIM - 1

*  For each pixel axis I in the final output NDF, find the 
*  corresponding axis in the input NDF.
      DO I = 1, NDIMO
         IF ( I .LT. JAXIS ) THEN
            AXES( I ) = I
         ELSE
            AXES( I ) = I + 1
         END IF
      END DO

*  Find the pixel bounds of the NDF after axis permutation.
      DO I = 1, NDIMO
         LBNDO( I ) = LBND( AXES( I ) )
         UBNDO( I ) = UBND( AXES( I ) )
      END DO

*  Determine whether or not there are significant dimensions above
*  the collapse axis.
      HIGHER = JAXIS .NE. NDIM
      IF ( HIGHER ) THEN
         HIGHER = .FALSE.
         DO I = JAXIS + 1, NDIM
            HIGHER = HIGHER .OR. ( UBND( I ) - LBND( I ) ) .NE. 0
         END DO
      END IF

*  Adjust output NDF to its new shape.
*  ===================================

*  The shape and size of the output NDF created above will be wrong, so
*  we need to correct it by removing the collapse axis.  This is easy if
*  it is the final axis (we would just use NDF_SBND with specifying 
*  NDIM-1 axes), but is not so easy if the collapse axis is not the
*  final axis.  In this case, we do th following:
*    1) - Save copies of an AXIS structures in the output NDF (because
*         the following step will change their lengths to match the new
*         bounds).
*    2) - Change the bounds and dimensionality of the NDF to the
*         appropriate values.
*    3) - Restore the saved AXIS structures, permuting them so that they
*         apply to the correct axis.
*    4) - Adjust the WCS FrameSet to pick the required axis from the
*         original Base Frame.

*  First see if the AXIS component is defined.
      CALL NDF_STATE( INDF2, 'AXIS', GOTAX, STATUS )

*  If so, we need to save copies of the AXIS structures.
      IF ( GOTAX ) THEN

*  Get an HDS locator to the NDF structure,
         CALL NDF_LOC( INDF2, 'UPDATE', LOC1, STATUS )

*  Get a locator for the AXIS component.
         CALL DAT_FIND( LOC1, 'AXIS', LOC2, STATUS )

*  Take a copy of the AXIS component and call it OLDAXIS.
         CALL DAT_COPY( LOC2, LOC1, 'OLDAXIS', STATUS ) 

*  Get a locator for OLDAXIS.
         CALL DAT_FIND( LOC1, 'OLDAXIS', LOC3, STATUS )

      END IF

*  Set the output NDF bounds to the required values.  This will change
*  the lengths of the current AXIS arrays (but we have a copy of the
*  originals in OLDAXIS), and reduce the dimensionality by one.
      CALL NDF_SBND( NDIMO, LBNDO, UBNDO, INDF2, STATUS ) 

*  We now re-instate any AXIS structures, in their new order.
      IF( GOTAX ) THEN

*  Promote the NDF locator to a primary locator so that the HDS
*  container file is not closed when the NDF identifier is annulled.
         CALL DAT_PRMRY( .TRUE., LOC1, .TRUE., STATUS ) 

*  The DATA array of the output NDF will not yet be in a defined state. 
*  This would result in NDF_ANNUL reporting an error, so we temporarily
*  map the DATA array (which puts it into a defined state) to prevent
*  this.
         CALL NDF_MAP( INDF2, 'DATA', ITYPE, 'WRITE', IPOUT( 1 ), EL2, 
     :                 STATUS ) 

*  Annul the supplied NDF identifier so that we can change the contents
*  of the NDF using HDS, without getting out of step with the NDFs
*  libraries description of the NDF. 
         CALL NDF_ANNUL( INDF2, STATUS )

*  Loop round each cell in the returned AXIS structure.
         DO I = 1, NDIMO

*  Get a locator to this cell in the NDFs AXIS array.
            CALL DAT_CELL( LOC2, 1, I, LOC4, STATUS ) 

*  Empty it of any components
            CALL DAT_NCOMP( LOC4, NCOMP, STATUS )
            DO J = NCOMP, 1, -1
               CALL DAT_INDEX( LOC4, J, LOC5, STATUS )
               CALL DAT_NAME( LOC5, NAME, STATUS )
               CALL DAT_ANNUL( LOC5, STATUS )
               CALL DAT_ERASE( LOC4, NAME, STATUS )
               IF( STATUS .NE. SAI__OK ) GO TO 999
            END DO

*  Get a locator to the corresponding cell in the OLDAXIS array.
            CALL DAT_CELL( LOC3, 1, AXES( I ), LOC5, STATUS ) 

*  We now copy all the components of the OLDAXIS cell into the AXIS 
*  cell.  Find the number of components, and loop round them.
            CALL DAT_NCOMP( LOC5, NCOMP, STATUS )
            DO J = NCOMP, 1, -1

*  Get a locator to this component in the original OLDAXIS cell.
               CALL DAT_INDEX( LOC5, J, LOC6, STATUS )

*  Get its name.
               CALL DAT_NAME( LOC6, NAME, STATUS )

*  Copy it into the new AXIS structure.
               CALL DAT_COPY( LOC6, LOC4, NAME, STATUS )           

*  Annul the locators.
               CALL DAT_ANNUL( LOC6, STATUS )

*  Abort if an error has occurred.
               IF( STATUS .NE. SAI__OK ) GO TO 999

            END DO

*  Annul the locators.
            CALL DAT_ANNUL( LOC4, STATUS )
            CALL DAT_ANNUL( LOC5, STATUS )

         END DO

*  Annul the locator to the OLDAXIS structure and then erase the object.
         CALL DAT_ANNUL( LOC3, STATUS )
         CALL DAT_ERASE( LOC1, 'OLDAXIS', STATUS ) 

*  Annul the AXIS array locator.
         CALL DAT_ANNUL( LOC2, STATUS )

*  Import the modified NDF back into the NDF system.
         CALL NDF_FIND( LOC1, ' ', INDF2, STATUS ) 

*  Annul the NDF locator.
         CALL DAT_ANNUL( LOC1, STATUS )

      END IF

*  Alter the WCS FrameSet.
*  =======================

*  Copy the FrameSet, as we're about to change it, but the original
*  is sometimes needed.
      IWCSO = AST_COPY( IWCS, STATUS )

*  We now modify the input NDFs WCS FrameSet by removing the collapsed
*  axis from the base and current Frames.
      CALL KPS1_CLPA0( IWCSO, JAXIS, UBND( JAXIS ) - LBND( JAXIS ) + 1, 
     :                 GRDPOS, STATUS )

*  Save this modified WCS FrameSet in the output NDF.
      CALL NDF_PTWCS( IWCSO, INDF2, STATUS )      

*  Obtain the remaining parameters.
*  ================================

*  Get the ESTIMATOR and WLIM parameters.
      CALL PAR_CHOIC( 'ESTIMATOR', 'Mean','Mean,WMean,Mode,Median,Max,'/
     :                /'Min,Comax,Comin,Absdev,RMS,Sigma,Sum,Iwc,Iwd,'/
     :                /'Integ', .FALSE., ESTIM, STATUS )

      CALL PAR_GDR0R( 'WLIM', 0.3, 0.0, 1.0, .FALSE., WLIM, STATUS )

*  Redefine the data units.
*  ========================
      IF ( ESTIM .EQ. 'COMAX' .OR. ESTIM .EQ. 'COMIN' .OR.
     :     ESTIM .EQ. 'IWC' .OR. ESTIM .EQ. 'IWD' ) THEN

*  Obtain the collapsed-axis units of the input NDF; these now become
*  the data units in output NDF.
         ATTRIB = 'UNIT('
         NC = 5
         CALL CHR_PUTI( IAXIS, ATTRIB, NC )
         CALL CHR_PUTC( ')', ATTRIB, NC )
         UNITS = AST_GETC( IWCS, ATTRIB( :NC ), STATUS )

         CALL NDF_CPUT( UNITS, INDF2, 'Units', STATUS )

*  New unit is the existing unit times the co-ordinate's unit.  So
*  obtain each unit and concatenate the two inserting a blank between
*  them.
      ELSE IF ( ESTIM .EQ. 'INTEG' ) THEN
         ATTRIB = 'UNIT('
         NC = 5
         CALL CHR_PUTI( IAXIS, ATTRIB, NC )
         CALL CHR_PUTC( ')', ATTRIB, NC )
         AUNITS = AST_GETC( IWCS, ATTRIB( :NC ), STATUS )

         UNITS = ' '
         CALL NDF_CGET( INDF1, 'Unit', UNITS, STATUS )
         CALL NDF_CLEN( INDF1, 'Unit', NC, STATUS )
         NC = NC + 1
         UNITS( NC:NC ) = ' '
         CALL CHR_APPND( AUNITS, UNITS, NC )

         CALL NDF_CPUT( UNITS, INDF2, 'Units', STATUS )
      END IF

*  Process in blocks.
*  ==================

*  For large datasets, there may be insufficient memory.  Therefore
*  we form blocks to process, one at a time.  For this by definition
*  we need the collapse-axis pixels to always be present in full for
*  each pixel along the other pixel axes.  If this leaves room for a
*  full span of a dimension that becomes the block size along that
*  axis.  Partial fills take the remaining maximum size and subsequent
*  dimensions' block sizes are unity.
      IBLSIZ( IAXIS ) = AEL
      MAXSIZ = MAX( 1, MAXPIX / AEL )
      LOOP = .TRUE.
      J = 0
      DO I = 1, NDIM
         IF ( I .NE. IAXIS ) THEN
            IF ( LOOP ) THEN
               D = UBND( I ) - LBND( I ) + 1
               IF ( MAXSIZ .GE. D ) THEN
                  IBLSIZ( I ) = D
                  MAXSIZ = MAXSIZ / D
               ELSE
                  IBLSIZ( I ) = MAXSIZ
                  LOOP = .FALSE.
               END IF
            ELSE
               IBLSIZ( I ) = 1
            END IF

*  Copy the output NDF block sizes in sequence omitting the
*  collapse axis.
            J = J + 1
            OBLSIZ( J ) = IBLSIZ( I )
         END IF
      END DO

*  If the LOW and HIGH limits have reduced the collapsed section from
*  the whole collapsed dimension, then we cannot use the original input
*  NDF to derive the number of blocks.  Instead we create a subsection
*  spanning the actual collapse limits, as if the user had supplied
*  this section with the input NDF.
      DO I = 1, NDIM
         LBNDS( I ) = LBND( I )
         UBNDS( I ) = UBND( I )
      END DO
      LBNDS( JAXIS ) = JLO
      UBNDS( JAXIS ) = JHI
      IF ( USEALL ) THEN
         INDFS = INDF1
      ELSE
         CALL NDF_SECT( INDF1, NDIM, LBNDS, UBNDS, INDFS, STATUS )
      END IF

*  Determine the number of blocks.
      CALL NDF_NBLOC( INDFS, NDIM, IBLSIZ, NBLOCK, STATUS )


*  Loop through each block.  Start a new NDF context.
      DO IBLOCK = 1, NBLOCK
         CALL NDF_BEGIN
         CALL NDF_BLOCK( INDFS, NDIM, IBLSIZ, IBLOCK, IBL, STATUS ) 
         CALL NDF_BLOCK( INDF2, NDIMO, OBLSIZ, IBLOCK, OBL, STATUS ) 

*  Map the NDF arrays and workspace required.
*  ==========================================

*  Map the full input, and output data and (if needed) variance arrays.
         CALL NDF_MAP( IBL, COMP, ITYPE, 'READ', IPIN, EL1, STATUS )
         CALL NDF_MAP( OBL, COMP, ITYPE, 'WRITE', IPOUT, EL2, STATUS )

         IF ( .NOT. VAR ) THEN
            IPIN( 2 ) = IPIN( 1 )
            IPOUT( 2 ) = IPOUT( 1 )
         END IF

*  Obtain the bounds of the blocks.
         CALL NDF_BOUND( IBL, NDF__MXDIM, LBNDS, UBNDS, NDIM, STATUS )
         CALL NDF_BOUND( OBL, NDF__MXDIM, LBNDO, UBNDO, NDIMO, STATUS )


*  Allocate work space, unless the last axis is being collapsed (in
*  which case no work space is needed).
         IF ( HIGHER ) THEN
            CALL PSX_CALLOC( EL2 * AEL, ITYPE, IPW1, STATUS )
            IF( VAR ) THEN
               CALL PSX_CALLOC( EL2 * AEL, ITYPE, IPW2, STATUS )
            ELSE
               IPW2 = IPW1
            END IF  

*  Store safe pointer values if no work space is needed.
         ELSE
            IPW1 = IPIN( 1 )
           IPW2 = IPIN( 1 )
         END IF

*  Associate co-ordinate information.
*  ==================================

*  Obtain co-ordinates along the collapse axis for the following
*  methods.
         IF ( ESTIM .EQ. 'COMAX' .OR. ESTIM .EQ. 'COMIN' .OR.
     :        ESTIM .EQ. 'IWC' .OR. ESTIM .EQ. 'IWD' .OR. 
     :        ESTIM .EQ. 'INTEG' ) THEN

*  Create workspace for the co-ordinates along a single WCS axis
*  in the correct data type.
            CALL PSX_CALLOC( EL1, '_DOUBLE', IPAXCO, STATUS )
            CALL PSX_CALLOC( EL1, ITYPE, IPCO, STATUS )

*  Allocate work space, unless the last pixel axis is being collapsed 
*  (in which case no work space is needed).
            IF ( HIGHER ) THEN
               CALL PSX_CALLOC( EL2 * AEL, ITYPE, IPW3, STATUS )
            END IF

*  Obtain the double-precision co-ordinate centres along the collapse
*  axis in the current Frame.
            CALL KPG1_WCFAX( IBL, IWCS, IAXIS, EL1, 
     :                       %VAL( CNF_PVAL( IPAXCO ) ), STATUS )

*  Copy the centres to the required precision.
            IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL VEC_DTOR( .TRUE., EL1, %VAL( CNF_PVAL( IPAXCO ) ),
     :                        %VAL( CNF_PVAL( IPCO ) ), IERR, NERR,
     :                        STATUS )

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL VEC_DTOD( .TRUE., EL1, %VAL( CNF_PVAL( IPAXCO ) ),
     :                        %VAL( CNF_PVAL( IPCO ) ), IERR, NERR,
     :                        STATUS )

            END IF
            CALL PSX_FREE( IPAXCO, STATUS )

*  Store safe pointer value if axis centres are not needed.
         ELSE
            IPCO = IPIN( 1 )
            IPW3 = IPIN( 1 )
         END IF

*  Associate AXIS-width information.
*  =================================

*  Obtain AXIS widths along the collapse axis for the following
*  methods.
         IF ( ESTIM .EQ. 'INTEG' ) THEN
         
*  Allocate work space for thw widths to be derived from the
*  co-ordinates.  This assumes full filling of pixels.
            CALL PSX_CALLOC( EL2 * AEL, ITYPE, IPWID, STATUS )

*  Store safe pointer value if widths are not needed.
         ELSE
            IPWID = IPIN( 1 )
         END IF

*  Collapse.
*  =========

*  Now do the work, using a routine appropriate to the numeric type.
         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPS1_CLPSR( JAXIS, JLO, JHI, VAR, ESTIM, WLIM, EL2,
     :                       NDIM, LBNDS, UBNDS, 
     :                       %VAL( CNF_PVAL( IPIN( 1 ) ) ),
     :                       %VAL( CNF_PVAL( IPIN( 2 ) ) ), 
     :                       %VAL( CNF_PVAL( IPCO ) ),
     :                       %VAL( CNF_PVAL( IPWID ) ), NDIMO, LBNDO,
     :                       UBNDO, HIGHER,
     :                       %VAL( CNF_PVAL( IPOUT( 1 ) ) ), 
     :                       %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                       %VAL( CNF_PVAL( IPW1 ) ), 
     :                       %VAL( CNF_PVAL( IPW2 ) ), 
     :                       %VAL( CNF_PVAL( IPW3 ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPS1_CLPSD( JAXIS, JLO, JHI, VAR, ESTIM, WLIM, EL2,
     :                       NDIM, LBNDS, UBNDS, 
     :                       %VAL( CNF_PVAL( IPIN( 1 ) ) ),
     :                       %VAL( CNF_PVAL( IPIN( 2 ) ) ), 
     :                       %VAL( CNF_PVAL( IPCO ) ),
     :                       %VAL( CNF_PVAL( IPWID ) ), NDIMO, LBNDO, 
     :                       UBNDO, HIGHER,
     :                       %VAL( CNF_PVAL( IPOUT( 1 ) ) ), 
     :                       %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                       %VAL( CNF_PVAL( IPW1 ) ), 
     :                       %VAL( CNF_PVAL( IPW2 ) ), 
     :                       %VAL( CNF_PVAL( IPW3 ) ), STATUS )

         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'T', ITYPE )
            CALL ERR_REP( 'COLLAPSE_ERR5', 'COLLAPSE: Unsupported '//
     :                    'data type ^T (programming error).', STATUS )
         END IF

*  Free the work space.
         IF ( HIGHER ) THEN
            CALL PSX_FREE( IPW1, STATUS )
            IF( VAR ) CALL PSX_FREE( IPW2, STATUS )
         END IF

         IF ( ESTIM .EQ. 'COMAX' .OR. ESTIM .EQ. 'COMIN' .OR.
     :        ESTIM .EQ. 'IWC' .OR. ESTIM .EQ. 'IWD' ) THEN
             CALL PSX_FREE( IPCO, STATUS )
             IF ( HIGHER ) CALL PSX_FREE( IPW3, STATUS )
         END IF
         
         IF ( ESTIM .EQ. 'INTEG' ) THEN
            CALL PSX_FREE( IPWID, STATUS )
            IF ( HIGHER ) CALL PSX_FREE( IPW3, STATUS )
         END IF

*   Close NDF context.
         CALL NDF_END( STATUS )
      END DO

*  Come here if something has gone wrong.
  999 CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Report a contextual message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'COLLAPSE_ERR6', 'COLLAPSE: Unable to collapse '/
     :                  /'an NDF along one axis.', STATUS )
      END IF

      END
