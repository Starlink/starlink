      SUBROUTINE BLOCK( STATUS )
*+
*  Name:
*     BLOCK

*  Purpose:
*     Smooths a 1- or 2-dimensional image using a square or rectangular
*     box filter.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL BLOCK( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application applies a square or rectangular box filter to a
*     1- or 2-dimensional image so as to smooth it.  Each output pixel
*     is either the mean or the median of the input pixels within the
*     filter box.  The mean estimator provides one of the fastest
*     methods of smoothing an image and is often useful as a
*     general-purpose smoothing algorithm when the exact form of the
*     smoothing point-spread function is not important.  The image is
*     held in an NDF data structure.

*  Usage:
*     block in out box [estimator]

*  ADAM Parameters:
*     BOX(2) = _INTEGER (Read)
*        The x and y sizes (in pixels) of the rectangular box to be
*        applied to smooth the image.  If only a single value is given,
*        then it will be duplicated so that a square filter is used,
*        except where the image is 1-dimensional, for which the box size
*        along the insignificant dimension is set to 1.  The values
*        given will be rounded up to positive odd integers, if
*        necessary.
*     ESTIMATOR = LITERAL (Read)
*        The method to use for estimating the output pixel values.  It
*        can be either "Mean" or "Median". ["Mean"]
*     IN = NDF (Read)
*        The input NDF containing the 1- or 2-dimensional image to which
*        box smoothing is to be applied.
*     OUT = NDF (Write)
*        The output NDF which is to contain the smoothed image.
*     TITLE = LITERAL (Read)
*        Value for the title of the output NDF. A null value will cause
*        the title of the input NDF to be used. [!]
*     WLIM = _REAL (Read)
*        If the input image contains bad pixels, then this parameter
*        may be used to determine the number of good pixels which must
*        be present within the smoothing box before a valid output
*        pixel is generated.  It can be used, for example, to prevent
*        output pixels from being generated in regions where there are
*        relatively few good pixels to contribute to the smoothed
*        result.
*
*        By default, a null (!) value is used for WLIM, which causes
*        the pattern of bad pixels to be propagated from the input
*        image to the output image unchanged. In this case, smoothed
*        output values are only calculated for those pixels which are
*        not bad in the input image.
*
*        If a numerical value is given for WLIM, then it specifies the
*        minimum fraction of good pixels which must be present in the
*        smoothing box in order to generate a good output pixel.  If
*        this specified minimum fraction of good input pixels is not
*        present, then a bad output pixel will result, otherwise a
*        smoothed output value will be calculated.  The value of this
*        parameter should lie between 0.0 and 1.0 (the actual number
*        used will be rounded up if necessary to correspond to at least
*        1 pixel). [!]

*  Examples:
*     block aa bb 9
*        Smooths the 2-dimensional image held in the NDF structure aa,
*        writing the result into the structure bb.  The smoothing box is
*        9 pixels square.  If any pixels in the input image are bad,
*        then the corresponding pixels in the output image will also be
*        bad.  Each output pixel is the mean of the corresponding input
*        pixels.
*     block spectrum spectrums 5 median title="Smoothed spectrum"
*        Smooths the 1-dimensional data in the NDF called spectrum
*        using a box size of 5 pixels, and stores the result in the NDF
*        structure spectrums.  Each output pixel is the median of the
*        corresponding input pixels.  If any pixels in the input image
*        are bad, then the corresponding pixels in the output image
*        will also be bad.  The output NDF has the title "Smoothed
*        spectrum".
*     block ccdin(123,) ccdcol [1,9] 
*        Smooths the 123rd column in the 2-dimensional NDF called ccdin
*        using a box size of 9 pixels, and stores the result in the NDF
*        structure ccdcol.  The first value of the smoothing box is
*        ignored as the first dimension has only one element.  Each
*        output pixel is the mean of the corresponding input pixels.
*     block in=image1 out=image2 box=[5,7] estimator=median
*        Smooths the 2-dimensional image held in the NDF structure
*        image1 using a rectangular box of size 5x7 pixels.  The
*        smoothed image is written to the structure image2.  Each
*        output pixel is the median of the corresponding input pixels.
*     block etacar etacars box=[7,1] wlim=0.6
*        Smooths the specified image data using a rectangular box 7x1
*        pixels in size.  Smoothed output values are generated only if
*        at least 60% of the pixels in the smoothing box are good,
*        otherwise the affected output pixel is bad.
*        
*  Timing:
*     When using the mean estimator, the execution time is
*     approximately proportional to the number of pixels in the image
*     to be smoothed and is largely independent of the smoothing box
*     size. This makes the routine particularly suitable for applying
*     heavy smoothing to an image.  Execution time will be
*     approximately doubled if a variance array is present in the input
*     NDF.
*
*     The median estimator is much slower than the mean estimator, and is 
*     heavily dependent on the smoothing box size.

*  Related Applications:
*     KAPPA: CONVOLVE, FFCLEAN, GAUSMOOTH, MEDIAN; Figaro: ICONV3,
*     ISMOOTH, IXSMOOTH, MEDFILT.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     LABEL, TITLE, UNITS, WCS and HISTORY components of the input NDF and
*     propagates all extensions.  In addition, if the mean estimator is
*     used, the VARIANCE component is also processed.  If the median
*     estimator is used, then the output NDF will have no VARIANCE
*     component, even if there is a VARIANCE component in the input
*     NDF.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.  The bad-pixel flag is also written for the data and
*     variance arrays.
*     -  All non-complex numeric data types can be handled.  Arithmetic
*     is performed using single-precision floating point, or double
*     precision, if appropriate.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-SEP-1990 (RFWS):
*        Original version.
*     1990 November 28 (MJC):
*        Corrected call to generic routine for double-precision
*        variance.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 April 4 (MJC):
*        Made to handle significant dimensions for user-defined
*        sections.
*     16-DEC-1994 (DSB):
*        Title propagated by default from input to output NDF.
*        Introduced ESTIMATOR parameter.  Replaced AIF VM calls with PSX
*        calls.
*     1995 March 16 (MJC):
*        Made to operate on 1-dimensional arrays.  Enabled the writing
*        of the bad-pixel flags.  Usage and examples to lowercase.
*        Added a "Related Applications" section.  Fixed bug that
*        attempted to create an output variance array when the median
*        estimator was selected.
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
      INCLUDE 'PAR_PAR'          ! Parameter system public constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PAR_ERR'          ! Parameter system error codes

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants: 
      INTEGER NDIM               ! Maximum dimensionality
      PARAMETER( NDIM = 2 )

*  Local Variables:
      CHARACTER * ( 13 ) COMP    ! List of components to process
      CHARACTER * ( 6 ) ESTIM    ! Method to use to estimate smoothed values
      CHARACTER * ( NDF__SZFTP ) DTYPE ! Numeric type for output arrays
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Numeric type for processing
      INTEGER BOX( NDIM )        ! Smoothing box size
      INTEGER BOXSIZ             ! Number of pixels in smoothing box
      INTEGER DIM( NDF__MXDIM )  ! NDF dimensions
      INTEGER EL                 ! Number of mapped array elements
      INTEGER I                  ! Loop counter
      INTEGER IBOX( NDIM )       ! Smoothing box half-size
      INTEGER NDF1               ! Identifier for input NDF
      INTEGER NDF2               ! Identifier for output NDF
      INTEGER NDIMS              ! Number of NDF dimensions
      INTEGER NLIM               ! Minimum good pixel limit
      INTEGER NVAL               ! Number of values obtained
      INTEGER PNTR1( 2 )         ! Pointers for mapped input arrays
      INTEGER PNTR2( 2 )         ! Pointers for mapped output arrays
      INTEGER SDIM( NDF__MXDIM ) ! Significant NDF dimensions
      INTEGER WPNTR1             ! Mapped workspace pointer
      INTEGER WPNTR2             ! Mapped workspace pointer
      LOGICAL BAD                ! Check for bad input pixels?
      LOGICAL BADOUT             ! Bad pixels in output array?
      LOGICAL SAMBAD             ! Propagate bad pixels to same place?
      LOGICAL VAR                ! Variance array present?
      REAL WLIM                  ! Fraction of good pixels required

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the input NDF.
      CALL NDF_ASSOC( 'IN', 'READ', NDF1, STATUS )

*  Find whether or not there are no more than two significant
*  dimensions and which ones they are.
      CALL KPG1_SDIMP( NDF1, NDIM, SDIM, STATUS )

*  Exit if an error occurred.  This is needed because the significant
*  dimensions are used as array indices.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Determine its dimensions.  Note that only two dimensions can be
*  accommodated, but of these only one need be siginifcant.  Then
*  ignore non-significant dimensions.
      CALL NDF_DIM( NDF1, SDIM( NDIM ), DIM, NDIMS, STATUS )
      DIM( 1 ) = DIM( SDIM( 1 ) )
      DIM( 2 ) = DIM( SDIM( 2 ) )

*  Obtain the smoothing box sizes, duplicating the value if only a
*  single value is given.  Each box size must be a positive odd number,
*  so derive IBOX so that BOX = 2*IBOX+1 is rounded up if necessary.
*  Also allow for one-dimensional data, setting the default box size
*  to 1 element.
      CALL PAR_GDRVI( 'BOX', NDIM, 1, VAL__BADI, BOX, NVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999
      IF ( NVAL .LT. NDIM ) THEN
         BOX( 2 ) = BOX( 1 )
      END IF

      DO I = 1, NDIM
         IF ( DIM( I ) .EQ. 1 ) THEN
            IBOX( I ) = 0
         ELSE
            IBOX( I ) = MAX( BOX( I ), 1 ) / 2
         END IF
      END DO
      BOXSIZ = ( 2 * IBOX( 1 ) + 1 ) * ( 2 * IBOX( 2 ) + 1 )

*  Obtain the minimum fraction of good pixels which should be used to
*  calculate an output pixel value.  Test if a null value is specified
*  and set SAMBAD appropriately, annulling the error.
      CALL ERR_MARK
      SAMBAD = .FALSE.
      CALL PAR_GDR0R( 'WLIM', 0.5, 0.0, 1.0, .FALSE., WLIM, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         SAMBAD = .TRUE.
         CALL ERR_ANNUL( STATUS )

*  Derive the minimum number of pixels, using at least one.
      ELSE
         NLIM = MAX( 1, NINT( REAL( BOXSIZ ) * WLIM ) )
      END IF
      CALL ERR_RLSE

*  Obtain the method to use for estimating the smoothed pixel values.
      CALL PAR_CHOIC( 'ESTIMATOR', 'Mean', 'Mean,Median', .FALSE.,
     :                ESTIM, STATUS )

*  Determine if a variance component is present and derive a list of
*  the components to be processed.  At the moment, VARIANCE components
*  can only be processed if the mean estimator is being used.
      CALL NDF_STATE( NDF1, 'Variance', VAR, STATUS )
      IF ( VAR .AND. ESTIM .EQ. 'MEAN' ) THEN
         COMP = 'Data,Variance'
      ELSE
         COMP = 'Data'
      END IF

*  Determine the numeric type to be used for processing the input
*  arrays.  This application supports single- and double-precision
*  floating-point processing.
      CALL NDF_MTYPE( '_REAL,_DOUBLE', NDF1, NDF1, COMP, ITYPE, DTYPE,
     :                STATUS )

*  Create an output NDF based on the input one.  Set an appropriate
*  numeric type for the output arrays.
      CALL NDF_PROP( NDF1, 'WCS,Axis,Quality,Units', 'OUT', NDF2, 
     :               STATUS )
      CALL NDF_STYPE( DTYPE, NDF2, COMP, STATUS )

*  Map the input and output arrays.
      CALL KPG1_MAP( NDF1, COMP, ITYPE, 'READ', PNTR1, EL, STATUS )
      CALL KPG1_MAP( NDF2, COMP, ITYPE, 'WRITE', PNTR2, EL, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  See if it is necessary to check for bad pixels in the input arrays.
      CALL NDF_MBAD( .TRUE., NDF1, NDF1, COMP, .FALSE., BAD, STATUS )

*  Obtain workspace arrays for the smoothing algorithm and map them.
      IF ( ESTIM .EQ. 'MEAN' ) THEN
         CALL PSX_CALLOC( DIM( 1 ), ITYPE, WPNTR1, STATUS )
         CALL PSX_CALLOC( DIM( 1 ), '_INTEGER', WPNTR2, STATUS )
      ELSE
         CALL PSX_CALLOC( BOXSIZ, ITYPE, WPNTR1, STATUS )
         CALL PSX_CALLOC( BOXSIZ, '_INTEGER', WPNTR2, STATUS )
      END IF

*  Apply smoothing to the mapped data array, using the appropriate
*  numeric type of processing.
*
*  Real
      IF ( ITYPE .EQ. '_REAL' ) THEN

         IF ( ESTIM .EQ. 'MEAN' ) THEN
            CALL KPG1_BLOCR( BAD, SAMBAD, .FALSE., DIM( 1 ), DIM( 2 ),
     :                       %VAL( PNTR1( 1 ) ), IBOX( 1 ), IBOX( 2 ),
     :                       NLIM, %VAL( PNTR2( 1 ) ), BADOUT, 
     :                       %VAL( WPNTR1 ), %VAL( WPNTR2 ), STATUS )
         ELSE
            CALL KPG1_BMEDR( BAD, SAMBAD, .TRUE., DIM( 1 ), DIM( 2 ), 
     :                       %VAL( PNTR1( 1 ) ), IBOX( 1 ), IBOX( 2 ),
     :                       NLIM, %VAL( PNTR2( 1 ) ), BADOUT, 
     :                       %VAL( WPNTR1 ), %VAL( WPNTR2 ), STATUS )
         END IF    

*  Double precision
      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN

         IF ( ESTIM .EQ. 'MEAN' ) THEN
            CALL KPG1_BLOCD( BAD, SAMBAD, .FALSE., DIM( 1 ), DIM( 2 ),
     :                       %VAL( PNTR1( 1 ) ), IBOX( 1 ), IBOX( 2 ),
     :                       NLIM, %VAL( PNTR2( 1 ) ), BADOUT, 
     :                       %VAL( WPNTR1 ), %VAL( WPNTR2 ), STATUS )
         ELSE
            CALL KPG1_BMEDD( BAD, SAMBAD, .TRUE., DIM( 1 ), DIM( 2 ), 
     :                       %VAL( PNTR1( 1 ) ), IBOX( 1 ), IBOX( 2 ),
     :                       NLIM, %VAL( PNTR2( 1 ) ), BADOUT, 
     :                       %VAL( WPNTR1 ), %VAL( WPNTR2 ), STATUS )
         END IF    

      END IF

*  Indicate whether or not the output data array has bad pixels.
      CALL NDF_SBAD( BADOUT, NDF2, 'Data', STATUS )

*  If a variance array is present, then also apply smoothing to it.
*  At the moment, variances are found only if the mean estimator is
*  being used.
      IF ( VAR .AND. ESTIM .EQ. 'MEAN' ) THEN
         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPG1_BLOCR( BAD, SAMBAD, .TRUE., DIM( 1 ), DIM( 2 ), 
     :                       %VAL( PNTR1( 2 ) ), IBOX( 1 ), IBOX( 2 ), 
     :                       NLIM, %VAL( PNTR2( 2 ) ), BADOUT,
     :                       %VAL( WPNTR1 ), %VAL( WPNTR2 ), STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_BLOCD( BAD, SAMBAD, .TRUE., DIM( 1 ), DIM( 2 ), 
     :                       %VAL( PNTR1( 2 ) ), IBOX( 1 ), IBOX( 2 ), 
     :                       NLIM, %VAL( PNTR2( 2 ) ), BADOUT,
     :                       %VAL( WPNTR1 ), %VAL( WPNTR2 ), STATUS )
         END IF

*  Indicate whether or not the output variance array has bad pixels.
         CALL NDF_SBAD( BADOUT, NDF2, 'Variance', STATUS )
      END IF

*  Release the temporary workspace arrays.
      CALL PSX_FREE( WPNTR1, STATUS )
      CALL PSX_FREE( WPNTR2, STATUS )

*  Obtain a new title for the output NDF, with the default value
*  being the input title.
      CALL KPG1_CCPRO( 'TITLE', 'Title', NDF1, NDF2, STATUS )

*  End the NDF context.
  999 CONTINUE
      CALL NDF_END( STATUS )

*  If an error occurred, then report contextual information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'BLOCK_ERR',
     :     'BLOCK: Error smoothing a 1- or 2-dimensional image using '/
     :     /'a rectangular box filter.', STATUS )
      END IF

      END
