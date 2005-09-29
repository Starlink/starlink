      SUBROUTINE FITLINES( STATUS )
*+
*  Name:
*     FITLINES

*  Purpose:
*     Fits polynomials to data lines that are parallel to a WCS axis.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FITLINES( STATUS )

*  Description:
*     This routine performs the fitting of polynomials of any order up
*     to 15 to individual lines of data that lie parallel to a chosen axis.
*
*     For instance when dealing with a cube whose third axis represents
*     the spectral dispersion, the lines that will be fitted (when the
*     third axis is chosen) are each of the spectra, one for each
*     position on the sky.
*
*     The polynomial fits can either be evaluated and written to a new
*     NDF or they can be subtracted from the input NDF. If evaluated and
*     subtracted later, then the subtraction can be undone, at some
*     speed cost.
*
*     The data used in each fit can be restricted by supplying a series
*     of coordinate ranges along the axis.

*  Usage:
*     fitlines in axis ranges order out

*  ADAM Parameters:
*     AXIS = LITERAL (Read)
*        The axis of the current coordinate system that defines the
*        direction that polynomials should be fitted.  This is specified
*        by its integer index within the current Frame of the input NDF
*        (in the range 1 to the number of axes in the current Frame), or
*        by its symbol string. A list of acceptable values is displayed
*        if an illegal value is supplied. If the axes of the current
*        Frame are not parallel to the NDF pixel axes, then the pixel
*        axis which is most nearly parallel to the specified current
*        Frame axis will be used. Defaults to the last axis. [!]
*     IN = NDF (Read)
*        The input NDF. On successful completion this may have the
*        fit subtracted, if SUBTRACT and MODIFYIN are both set true.
*     MODIFYIN = _LOGICAL (Read)
*        Whether to subtract the fit from the input NDF. Only used when
*        SUBTRACT is true. If this value is false then a NDF name must
*        be supplied by the OUT parameter. [false]
*     ORDER = _INTEGER (Read)
*        The order of the polynomials to be fitted to each line of
*        data. A polynomial of order 0 is a constant and 1 a line the
*        maximum value is 15.
*        [3]
*     OUT = NDF (Read)
*        The output NDF containing either the difference between the
*        input NDF and the various line fits, or the values of the line
*        fits themselves. Will not be used if SUBTRACT and MODIFYIN
*        are true (in that case the input NDF will be modified).
*     RANGES() = LITERAL (Read)
*        Pairs of coordinates that define ranges along the fit axis. When
*        given these ranges are used to select the values that are used
*        in the fit. If not given then all the data along each line is
*        used. The units of these ranges is determined by the current
*        axis of the world coordinate system that corresponds to the
*        selected axis. Up to 10 pairs of values are allowed. [!]
*     SUBTRACT = _LOGICAL (Read)
*        Whether to subtract the fit from the input NDF or not. If not
*        then the line fits will be evaluated and written to a new NDF.
*        [false]
*     TITLE = LITERAL (Read)
*        Value for the title of the output NDF.  A null value will cause
*        the title of the NDF supplied for parameter IN to be used
*        instead.  [!]
*     VARIANCE = _LOGICAL (Read)
*        If true and the input NDF contains variances then the
*        polynomial fits will be weighted by the variances.

*  Examples:
*     fitlines in=cube axis=3 ranges="1000,2000,3000,4000" order=4 out=fit
*        This example fits cubic polynomials to the spectral axis of
*        a data cube. The fits only use the data lying within the
*        ranges 1000 to 2000 and 3000 to 4000 angstroms (assuming
*        the spectral axis is calibrated in angstroms and that is the
*        current coordinate system). The fit is evaluated and
*        written to the data cube "fit".

*  Notes:
*     This application attempts to solve the problem of evaluating
*     numerous line fits that do not following the natural ordering of the
*     NDF data in the most efficient way possible. To do this requires
*     the use of additional memory (of order one less than the
*     dimensionality of the NDF itself, times the polynomial order squared).
*     To minimise the use of memory and get the fastest possible
*     determinations you should not use weighting and assert that the
*     input data do not have any BAD values (use the application SETBAD
*     to set the appropriate flag).

*  Related Applications:
*     KAPPA: SETBAD

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     LABEL, UNITS, TITLE, HISTORY, WCS and VARIANCE components of an NDF data
*     structure and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.
*     -  Handles data of up to 7 dimensions.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}

*  Copyright:
*     Copyright (C) Particle Physics and Astronomy Research Council.

*  History:
*     14-SEP-2005 (PWD):
*        Original version, some parts from COLLAPSE.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'AST_PAR'         ! AST parameters and functions
      INCLUDE 'CNF_PAR'         ! For CNF_PVAL function
      INCLUDE 'NDF_PAR'         ! NDF_ public constants
      INCLUDE 'PAR_ERR'         ! Parameter-system errors


*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      INTEGER KPG1_FLOOR       ! Most positive integer .LE. a given real
      INTEGER KPG1_CEIL        ! Most negative integer .GE. a given real

*  Local Variables:
      CHARACTER * ( 255 ) TTLC  ! Title of original current Frame
      CHARACTER ITYPE * ( NDF__SZTYP ) ! Numeric type for processing
      DOUBLE PRECISION AXHIGH   ! High bound of axis in current Frame
      DOUBLE PRECISION AXLOW    ! L ow bound of axis in current Frame
      DOUBLE PRECISION CPOS( 2, NDF__MXDIM ) ! Two current Frame positions
      DOUBLE PRECISION CURPOS( NDF__MXDIM ) ! A valid current Frame position
      DOUBLE PRECISION DLBND( NDF__MXDIM ) ! Lower bounds in pixel co-ords
      DOUBLE PRECISION DRANGE( 20 ) ! The fit ranges world coordinates
      DOUBLE PRECISION DUBND( NDF__MXDIM ) ! Upper bounds in pixel co-ords
      DOUBLE PRECISION PIXPOS( NDF__MXDIM ) ! A valid pixel Frame position
      DOUBLE PRECISION PPOS( 2, NDF__MXDIM ) ! Two pixel Frame positions
      DOUBLE PRECISION PRJ      ! Vector length projected onto a pixel axis
      DOUBLE PRECISION PRJMAX   ! Maximum vector length projected onto an axis
      INTEGER AREA              ! Area of axes orthogonal to fit axis.
      INTEGER AXES( NDF__MXDIM )! A list of axis indices
      INTEGER CFRM              ! Current frame
      INTEGER DIMS( NDF__MXDIM )    ! Dimensions of NDF
      INTEGER EL                ! Number of mapped elements
      INTEGER I                 ! Loop variable
      INTEGER IAXIS             ! Index of axis within current Frame
      INTEGER INNDF             ! NDF identifier if input NDF
      INTEGER IPAS              ! Pointer to workspace
      INTEGER IPBS              ! Pointer to coefficients
      INTEGER IPDAT( 1 )        ! Pointer to NDF data component
      INTEGER IPIN( 1 )         ! Pointer to mapped data
      INTEGER IPIX              ! Index of PIXEL Frame within WCS FrameSet
      INTEGER IPOUT( 1 )        ! Pointer to mapped data
      INTEGER IPTMP( 1 )        ! Pointer to temporary NDF component
      INTEGER IPVAR( 1 )        ! Pointer to NDF variance component
      INTEGER IPWRK1            ! Pointer to workspace
      INTEGER IPWRK2            ! Pointer to workspace
      INTEGER IWCS              ! AST FrameSet identifier
      INTEGER JAXIS             ! Index of axis within pixel Frame
      INTEGER JHI               ! High pixel index for axis
      INTEGER JLO               ! Low pixel index for axis
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of NDF
      INTEGER MAP               ! PIXEL Frame to Current Frame Mapping pointer
      INTEGER NAXC              ! Number of axes in current frame
      INTEGER NDIM              ! Number of NDF dimensions.
      INTEGER NRANGE            ! Number of range values (not pairs)
      INTEGER ORDER             ! The order of the polynomial to fit
      INTEGER OUTNDF            ! NDF identifier of output NDF
      INTEGER RANGES( 20 )      ! The fit ranges pixels
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of NDF
      LOGICAL BAD               ! Need to check for bad pixels?
      LOGICAL HASBAD            ! Input NDF may have BAD pixels
      LOGICAL HAVVAR            ! Have a variance component
      LOGICAL MODIN             ! Modify input NDF by subtracting fits.
      LOGICAL SUBTRA            ! Whether to subtract fit from data
      LOGICAL USEALL            ! Use the entire axis?
      LOGICAL USEVAR            ! Use variance as weights in fits.

*.

*  Future development notes: Should look at storing the coefficients and
*  write a model evaluating application MAKELINES? This would follow
*  the KAPPA model more closely and allow the fit to be undone, even
*  when subtracting directly. Maybe a need for a statistics generating
*  version too, but the quality of the fits is a potentially large
*  amount of information.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get the order of the polynomial.
      CALL PAR_GDR0I( 'ORDER', 3, 0, 15, .FALSE., ORDER, STATUS )

*  See if we should subtract fit from data. Need to do this early as we
*  may be modifying the input NDF.
      CALL PAR_GET0L( 'SUBTRACT', SUBTRA, STATUS )

*  See if the input NDF should have the fits subtracted, only matters if
*  we're subtracting the fit.
      MODIN = .FALSE.
      IF ( SUBTRA ) THEN
         CALL PAR_GET0L( 'MODIFYIN', MODIN, STATUS )
      END IF

*  Obtain identifier for the input NDF.
      IF ( MODIN ) THEN 
         CALL LPG_ASSOC( 'IN', 'UPDATE', INNDF, STATUS )
      ELSE
         CALL LPG_ASSOC( 'IN', 'READ', INNDF, STATUS )
      END IF

*  Get the bounds and dimensionality.
      CALL NDF_BOUND( INNDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Extra dimensions have nominal size 1.
      DO I = NDIM + 1, NDF__MXDIM
         LBND( I ) = 1
         UBND( I ) = 1
      END DO

*  Get dimensions of NDF.
      DO I = 1, NDF__MXDIM
         DIMS( I ) = UBND( I ) - LBND( I ) + 1
      END DO

*  Get the fit ranges
*  ==================

*  Get the WCS FrameSet from the NDF.
      CALL KPG1_GTWCS( INNDF, IWCS, STATUS )

*  Extract the current Frame, this is used for picking the axis and the
*  units of the ranges.
      CFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )
      NAXC = AST_GETI( CFRM, 'NAXES', STATUS )
      TTLC = AST_GETC( CFRM, 'TITLE', STATUS )

*  Get axis to fit the lines to. Default is last axis in the WCS.
      IF ( NDIM .NE. 1 ) THEN
         IAXIS = NAXC
         CALL KPG1_GTAXI( 'AXIS', CFRM, 1, IAXIS, STATUS )
      ELSE
         IAXIS = 1
      END IF

*  Find the index of the PIXEL Frame.
      CALL KPG1_ASFFR( IWCS, 'PIXEL', IPIX, STATUS )

*  Extract the Mapping from PIXEL Frame to Current Frame.
      MAP = AST_GETMAPPING( IWCS, IPIX, AST__CURRENT, STATUS )

*  Report an error if the Mapping is not defined in either direction.
      IF( .NOT. AST_GETL( MAP, 'TRANINVERSE', STATUS ) .AND.
     :    STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', INNDF )
         CALL MSG_SETC( 'T', TTLC )
         CALL ERR_REP( 'FITLINES_ERR2', 'The transformation from the '//
     :                 'current co-ordinate Frame of ''^NDF'' '//
     :                 '(^T) to pixel co-ordinates is not defined.',
     :                 STATUS )

      ELSE IF( .NOT. AST_GETL( MAP, 'TRANFORWARD', STATUS ) .AND.
     :         STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', INNDF )
         CALL MSG_SETC( 'T', TTLC )
         CALL ERR_REP( 'FITLINES_ERR3', 'The transformation from '//
     :                 'pixel co-ordinates to the current co-ordinate'//
     :                 ' Frame of ''^NDF'' (^T) is not defined.',
     :                 STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the ranges to use. These values are transformed from current
*  coordinates along the fit axis to pixel coordinates on some
*  NDF axis (we've yet to determine).
      DRANGE( 1 ) = AST__BAD
      CALL KPG1_GTAXV( 'RANGES', 20, .FALSE., CFRM, IAXIS, DRANGE,
     :                 NRANGE, STATUS )

*  If a null value was supplied then we should use the full extent.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         USEALL = .TRUE.
      ELSE
         USEALL = .FALSE.

*  Ranges must come in pairs.
         IF ( 2 * ( NRANGE / 2 ) .NE. NRANGE ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'FITLINES_ERR4',
     :                    'Range values must be supplied in pairs',
     :                    STATUS )
            GO TO 999
         END IF
      END IF

*  The next section deals the determination of which NDF axis the WCS
*  axis corresponds to. WCS axes can be permuted, rotated etc. so we
*  must check.

*  Find an arbitrary position within the NDF which has valid current
*  Frame co-ordinates. Both pixel and current Frame co-ordinates for
*  this position are returned.
      DO I = 1, NDIM
         DLBND( I ) = DBLE( LBND( I ) - 1 )
         DUBND( I ) = DBLE( UBND( I ) )
      END DO
      CALL KPG1_ASGDP( MAP, NDIM, NAXC, DLBND, DUBND, PIXPOS, CURPOS,
     :                 STATUS )

*  Create two copies of these current Frame co-ordinates.
      DO I = 1, NAXC
         CPOS( 1, I ) = CURPOS( I )
         CPOS( 2, I ) = CURPOS( I )
      END DO

*  If no ranges were supplied, modify the values in these positions by
*  an arbitrary amount.
      IF ( USEALL ) THEN
         IF ( CURPOS( IAXIS ) .NE. 0.0 ) THEN
            CPOS( 1, IAXIS ) = 0.99 * CURPOS( IAXIS )
            CPOS( 2, IAXIS ) = 1.01 * CURPOS( IAXIS )
         ELSE
            CPOS( 1, IAXIS ) = CURPOS( IAXIS ) + 1.0D-4
            CPOS( 2, IAXIS ) = CURPOS( IAXIS ) - 1.0D-4
         END IF

*  Use the first set of ranges.
      ELSE
         CPOS( 1, IAXIS ) = DRANGE( 2 )
         CPOS( 2, IAXIS ) = DRANGE( 1 )
      END IF

*  Transform these two positions into pixel co-ordinates.
      CALL AST_TRANN( MAP, 2, NAXC, 2, CPOS, .FALSE., NDIM, 2, PPOS,
     :                STATUS )

*  Find the pixel axis with the largest projection of the vector joining
*  these two pixel positions. The ranges apply to this axis. Report an
*  error if the positions do not have valid pixel coordinates.
      PRJMAX = -1.0
      DO I = 1, NDIM
         IF ( PPOS( 1, I ) .NE. AST__BAD .AND.
     :        PPOS( 2, I ) .NE. AST__BAD ) THEN

            PRJ = ABS( PPOS( 1, I ) - PPOS( 2, I ) )
            IF ( PRJ .GT. PRJMAX ) THEN
               JAXIS = I
               PRJMAX = PRJ
            END IF

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'FITLINES_ERR5', 'The WCS information is '//
     :                    'too complex (cannot find two valid pixel '//
     :                    'positions). Change current frame to PIXEL '//
     :                    'and try again', STATUS )
            GO TO 999
         END IF
      END DO

*  OK, use NDF axis JAXIS. Pick full extent if no values were given.
      IF ( USEALL ) THEN
         RANGES( 1 ) = LBND( JAXIS )
         RANGES( 2 ) = UBND( JAXIS )
         NRANGE = 2
      ELSE

*  Project the given ranges into pixel coordinates.
         DO I = 1, NRANGE, 2
            CPOS( 1, IAXIS ) = DRANGE( I + 1 )
            CPOS( 2, IAXIS ) = DRANGE( I )

            CALL AST_TRANN( MAP, 2, NAXC, 2, CPOS, .FALSE., NDIM, 2,
     :                      PPOS,STATUS )

*  Find the projection of the two test points onto the axis.
            JLO = KPG1_FLOOR( REAL( MIN( PPOS( 1, JAXIS ),
     :                                   PPOS( 2, JAXIS ) ) ) )
            JHI = KPG1_CEIL( REAL( MAX( PPOS( 1, JAXIS ),
     :                                  PPOS( 2, JAXIS ) ) ) )

*  Ensure these are within the bounds of the pixel axis.
            JLO = MAX( LBND( JAXIS ), JLO )
            JHI = MIN( UBND( JAXIS ), JHI )

*  Report an error if there is no intersection.
            IF ( JLO .GT. JHI .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'LO', JLO )
               CALL MSG_SETI( 'HI', JHI )
               CALL ERR_REP( 'FITLINES_ERR6', 'An axis range '//
     :                       'covers zero pixels (are the '//
     :                       'RANGE values equal or outside the '//
     :                       'bounds of the NDF?)(^LO:^HI)',
     :                       STATUS )
               GO TO 999
            END IF

*  Store pixel coordinates.
            RANGES( I ) = JLO
            RANGES( I + 1 ) = JHI
         END DO
      END IF

*  Tell the user the ranges of pixel being used.
      CALL MSG_SETI( 'I', JAXIS )
      CALL MSG_OUT( ' ', '   Fitting NDF axis ^I, using pixel ranges:',
     :              STATUS )
      DO I = 1, NRANGE, 2
         CALL MSG_SETI( 'L', RANGES( I ) )
         CALL MSG_SETI( 'H', RANGES( I + 1 ) )
         CALL MSG_OUT( ' ', '      ^L : ^H ', STATUS )
      END DO
      CALL MSG_BLANK( ' ', STATUS )

*  Convert ranges into indices of the NDF data arrays by correcting for
*  the origin.
      DO I = 1, NRANGE
         RANGES( I ) = RANGES( I ) - LBND( JAXIS ) + 1
      END DO

*  End of get fit ranges
*  =====================

*  If needed create a new output NDF based on the input NDF.
      IF ( SUBTRA ) THEN
         IF ( .NOT. MODIN ) THEN

*  Propagate all components except data and variance to the new NDF.
            CALL LPG_PROP( INNDF, 'Quality,Units,Label,Axis,WCS',
     :                     'OUT', OUTNDF, STATUS )
         END IF
      ELSE

*  Will write evals to a new NDF. Don't propagate quality as this is
*  model data now. Note we will also not propagate the variance. 
         CALL LPG_PROP( INNDF, 'Units,Label,Axis,WCS', 'OUT', OUTNDF, 
     :                  STATUS )
      END IF

*  Do we have any variances to use for weights and should they be used?
      CALL NDF_STATE( INNDF, 'Variance', HAVVAR, STATUS )
      IF ( HAVVAR ) THEN
         CALL PAR_GET0L( 'VARIANCE', USEVAR, STATUS )
      ELSE
         USEVAR = .FALSE.
      END IF

*  Determine if the input NDF has an explicit no bad pixels flag. Could
*  make the check really check if there's no variances as this speeds
*  the calculations, but should let the user control that.
      CALL NDF_BAD( INNDF, 'Data', .FALSE., HASBAD, STATUS )

*  Get the data type.
      CALL NDF_TYPE( INNDF, 'DATA', ITYPE, STATUS )

*  Map in the data. Note we transfer the data component from the input
*  NDF to the output NDF, as this saves on an unmap by HDS followed by a
*  map by us (if we allowed this to propagate).
      IF ( SUBTRA ) THEN
         IF ( .NOT. MODIN ) THEN
            CALL NDF_MAP( INNDF, 'DATA', ITYPE, 'READ', IPTMP, EL,
     :                    STATUS )
            CALL NDF_MAP( OUTNDF, 'DATA', ITYPE, 'WRITE', IPDAT, EL,
     :                    STATUS )

*  Copy data to the output NDF.
            CALL KPG1_COPY( ITYPE, EL, IPTMP( 1 ), IPDAT( 1 ), STATUS )
            CALL NDF_UNMAP( INNDF, 'DATA', STATUS )

*  Same for variances.
            IF ( USEVAR ) THEN
               CALL NDF_MAP( INNDF, 'VARIANCE', ITYPE, 'READ', IPTMP, 
     :                       EL, STATUS )
               CALL NDF_MAP( OUTNDF, 'VARIANCE', ITYPE, 'WRITE', IPVAR,
     :                       EL, STATUS )
               CALL KPG1_COPY( ITYPE, EL, IPTMP( 1 ), IPVAR( 1 ), 
     :                         STATUS )
               CALL NDF_UNMAP( INNDF, 'VARIANCE', STATUS )
            END IF
         ELSE

*  Subtracting from the input DATA, just map that in update mode.
            CALL NDF_MAP( INNDF, 'DATA', ITYPE, 'UPDATE', IPDAT, EL,
     :                    STATUS )
            IF ( USEVAR ) THEN
               CALL NDF_MAP( INNDF, 'VARIANCE', ITYPE, 'UPDATE', IPVAR,
     :                       EL, STATUS )
            END IF
         END IF
      ELSE

*  No need to copy input data, will just populate output NDF data
*  component with model values.
         CALL NDF_MAP( INNDF, 'DATA', ITYPE, 'READ', IPDAT, EL,
     :                 STATUS )
         IF ( USEVAR ) THEN
            CALL NDF_MAP( INNDF, 'VARIANCE', ITYPE, 'READ', IPVAR, EL, 
     :                    STATUS )
         END IF
      END IF

*  Allocate various workspaces. The requirements for these depends on
*  the dimensionality. We need space for the cumilative coefficient sums
*  and the coefficients themselves (Ax=B).
      AREA = 1
      DO 5 I = 1, NDIM
         IF ( I .NE. JAXIS ) THEN
            AREA = AREA * DIMS( I )
         END IF
 5    CONTINUE

      IF ( USEVAR .OR. HASBAD ) THEN
         CALL PSX_CALLOC( AREA * ( ORDER + 1 ) * ( ORDER + 1 ),
     :                    '_DOUBLE',IPAS, STATUS )
      ELSE

*  When there are no variances and we also know there are no BAD values
*  useful savings in memory and speed are available as the cumulative
*  sums for matrix inversion are fixed.
         CALL PSX_CALLOC( ( ORDER + 1 ) * ( ORDER + 1 ), '_DOUBLE',
     :                    IPAS, STATUS )
      END IF
      CALL PSX_CALLOC( AREA * ( ORDER + 1 ), '_DOUBLE', IPBS, STATUS )
      CALL PSX_CALLOC( AREA * ( ORDER + 1 ), '_DOUBLE', IPWRK1, STATUS )
      CALL PSX_CALLOC( AREA * ( ORDER + 1 ), '_INTEGER', IPWRK2,
     :                 STATUS )


*  Do the fits and optional subtraction. NB could reduce memory use by
*  NDF blocking though planes for higher dimensional data, or just
*  mapping the intersection of ranges, or individual ranges, but that
*  would only be good if working with the last axis (need contiguity).
      IF ( USEVAR .OR. HASBAD ) THEN
         IF ( ITYPE .EQ. '_BYTE' ) THEN
            CALL KPS1_LFTB( ORDER, JAXIS, RANGES, NRANGE, USEVAR,
     :                      %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                      %VAL( CNF_PVAL( IPVAR( 1 ) ) ), DIMS,
     :                      %VAL( CNF_PVAL( IPAS ) ),
     :                      %VAL( CNF_PVAL( IPBS ) ),
     :                      %VAL( CNF_PVAL( IPWRK1 ) ),
     :                      %VAL( CNF_PVAL( IPWRK2 ) ),
     :                      STATUS )
         ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
            CALL KPS1_LFTUB( ORDER, JAXIS, RANGES, NRANGE, USEVAR,
     :                       %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                       %VAL( CNF_PVAL( IPVAR( 1 ) ) ), DIMS,
     :                       %VAL( CNF_PVAL( IPAS ) ),
     :                       %VAL( CNF_PVAL( IPBS ) ),
     :                       %VAL( CNF_PVAL( IPWRK1 ) ),
     :                       %VAL( CNF_PVAL( IPWRK2 ) ),
     :                       STATUS )
         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPS1_LFTD( ORDER, JAXIS, RANGES, NRANGE, USEVAR,
     :                      %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                      %VAL( CNF_PVAL( IPVAR( 1 ) ) ), DIMS,
     :                      %VAL( CNF_PVAL( IPAS ) ),
     :                      %VAL( CNF_PVAL( IPBS ) ),
     :                      %VAL( CNF_PVAL( IPWRK1 ) ),
     :                      %VAL( CNF_PVAL( IPWRK2 ) ),
     :                      STATUS )
         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL KPS1_LFTI( ORDER, JAXIS, RANGES, NRANGE, USEVAR,
     :                      %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                      %VAL( CNF_PVAL( IPVAR( 1 ) ) ), DIMS,
     :                      %VAL( CNF_PVAL( IPAS ) ),
     :                      %VAL( CNF_PVAL( IPBS ) ),
     :                      %VAL( CNF_PVAL( IPWRK1 ) ),
     :                      %VAL( CNF_PVAL( IPWRK2 ) ),
     :                      STATUS )
         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPS1_LFTR( ORDER, JAXIS, RANGES, NRANGE, USEVAR,
     :                      %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                      %VAL( CNF_PVAL( IPVAR( 1 ) ) ), DIMS,
     :                      %VAL( CNF_PVAL( IPAS ) ),
     :                      %VAL( CNF_PVAL( IPBS ) ),
     :                      %VAL( CNF_PVAL( IPWRK1 ) ),
     :                      %VAL( CNF_PVAL( IPWRK2 ) ),
     :                      STATUS )
         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL KPS1_LFTW( ORDER, JAXIS, RANGES, NRANGE, USEVAR,
     :                      %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                      %VAL( CNF_PVAL( IPVAR( 1 ) ) ), DIMS,
     :                      %VAL( CNF_PVAL( IPAS ) ),
     :                      %VAL( CNF_PVAL( IPBS ) ),
     :                      %VAL( CNF_PVAL( IPWRK1 ) ),
     :                      %VAL( CNF_PVAL( IPWRK2 ) ),
     :                      STATUS )
         ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
            CALL KPS1_LFTUW( ORDER, JAXIS, RANGES, NRANGE, USEVAR,
     :                       %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                       %VAL( CNF_PVAL( IPVAR( 1 ) ) ), DIMS,
     :                       %VAL( CNF_PVAL( IPAS ) ),
     :                       %VAL( CNF_PVAL( IPBS ) ),
     :                       %VAL( CNF_PVAL( IPWRK1 ) ),
     :                       %VAL( CNF_PVAL( IPWRK2 ) ),
     :                       STATUS )
         END IF
      ELSE

*  No variances and no bad values, use fastest method.
         IF ( ITYPE .EQ. '_BYTE' ) THEN
            CALL KPS1_LFTQB( ORDER, JAXIS, RANGES, NRANGE,
     :                       %VAL( CNF_PVAL( IPDAT( 1 ) ) ), DIMS,
     :                       %VAL( CNF_PVAL( IPAS ) ),
     :                       %VAL( CNF_PVAL( IPBS ) ),
     :                       %VAL( CNF_PVAL( IPWRK1 ) ),
     :                       %VAL( CNF_PVAL( IPWRK2 ) ),
     :                       STATUS )
         ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
            CALL KPS1_LFTQUB( ORDER, JAXIS, RANGES, NRANGE,
     :                        %VAL( CNF_PVAL( IPDAT( 1 ) ) ), DIMS,
     :                        %VAL( CNF_PVAL( IPAS ) ),
     :                        %VAL( CNF_PVAL( IPBS ) ),
     :                        %VAL( CNF_PVAL( IPWRK1 ) ),
     :                        %VAL( CNF_PVAL( IPWRK2 ) ),
     :                        STATUS )
         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPS1_LFTQD( ORDER, JAXIS, RANGES, NRANGE,
     :                       %VAL( CNF_PVAL( IPDAT( 1 ) ) ), DIMS,
     :                       %VAL( CNF_PVAL( IPAS ) ),
     :                       %VAL( CNF_PVAL( IPBS ) ),
     :                       %VAL( CNF_PVAL( IPWRK1 ) ),
     :                       %VAL( CNF_PVAL( IPWRK2 ) ),
     :                       STATUS )
         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL KPS1_LFTQI( ORDER, JAXIS, RANGES, NRANGE,
     :                       %VAL( CNF_PVAL( IPDAT( 1 ) ) ), DIMS,
     :                       %VAL( CNF_PVAL( IPAS ) ),
     :                       %VAL( CNF_PVAL( IPBS ) ),
     :                       %VAL( CNF_PVAL( IPWRK1 ) ),
     :                       %VAL( CNF_PVAL( IPWRK2 ) ),
     :                       STATUS )
         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPS1_LFTQR( ORDER, JAXIS, RANGES, NRANGE,
     :                       %VAL( CNF_PVAL( IPDAT( 1 ) ) ), DIMS,
     :                       %VAL( CNF_PVAL( IPAS ) ),
     :                       %VAL( CNF_PVAL( IPBS ) ),
     :                       %VAL( CNF_PVAL( IPWRK1 ) ),
     :                       %VAL( CNF_PVAL( IPWRK2 ) ),
     :                       STATUS )
         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL KPS1_LFTQW( ORDER, JAXIS, RANGES, NRANGE,
     :                       %VAL( CNF_PVAL( IPDAT( 1 ) ) ), DIMS,
     :                       %VAL( CNF_PVAL( IPAS ) ),
     :                       %VAL( CNF_PVAL( IPBS ) ),
     :                       %VAL( CNF_PVAL( IPWRK1 ) ),
     :                       %VAL( CNF_PVAL( IPWRK2 ) ),
     :                       STATUS )
         ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
            CALL KPS1_LFTQUW( ORDER, JAXIS, RANGES, NRANGE,
     :                        %VAL( CNF_PVAL( IPDAT( 1 ) ) ), DIMS,
     :                        %VAL( CNF_PVAL( IPAS ) ),
     :                        %VAL( CNF_PVAL( IPBS ) ),
     :                        %VAL( CNF_PVAL( IPWRK1 ) ),
     :                        %VAL( CNF_PVAL( IPWRK2 ) ),
     :                        STATUS )
         END IF
      END IF

*  Subtract the result from the NDF data or write the evaluate the fit.
*  If evaluating, then we need to map in the data component, may as
*  well release the input one to save VM.
      IF ( .NOT. SUBTRA ) THEN
         CALL NDF_UNMAP( INNDF, 'DATA', STATUS )
         IF ( USEVAR ) THEN
            CALL NDF_UNMAP( INNDF, 'VARIANCE', STATUS )
         END IF
         CALL NDF_MAP( OUTNDF, 'DATA', ITYPE, 'WRITE', IPDAT,
     :                 EL, STATUS )
      END IF
      IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL KPS1_LFTSB( ORDER, JAXIS, SUBTRA,
     :                    %VAL( CNF_PVAL( IPDAT( 1 ) ) ), DIMS,
     :                    %VAL( CNF_PVAL( IPBS ) ), STATUS )
      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL KPS1_LFTSUB( ORDER, JAXIS, SUBTRA,
     :                     %VAL( CNF_PVAL( IPDAT( 1 ) ) ), DIMS,
     :                     %VAL( CNF_PVAL( IPBS ) ), STATUS )
      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL KPS1_LFTSD( ORDER, JAXIS, SUBTRA,
     :                    %VAL( CNF_PVAL( IPDAT( 1 ) ) ), DIMS,
     :                    %VAL( CNF_PVAL( IPBS ) ), STATUS )
      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL KPS1_LFTSI( ORDER, JAXIS, SUBTRA,
     :                    %VAL( CNF_PVAL( IPDAT( 1 ) ) ), DIMS,
     :                    %VAL( CNF_PVAL( IPBS ) ), STATUS )
      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL KPS1_LFTSR( ORDER, JAXIS, SUBTRA,
     :                    %VAL( CNF_PVAL( IPDAT( 1 ) ) ), DIMS,
     :                    %VAL( CNF_PVAL( IPBS ) ), STATUS )
      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL KPS1_LFTSW( ORDER, JAXIS, SUBTRA,
     :                    %VAL( CNF_PVAL( IPDAT( 1 ) ) ), DIMS,
     :                    %VAL( CNF_PVAL( IPBS ) ), STATUS )
      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL KPS1_LFTSUW( ORDER, JAXIS, SUBTRA,
     :                     %VAL( CNF_PVAL( IPDAT( 1 ) ) ), DIMS,
     :                     %VAL( CNF_PVAL( IPBS ) ), STATUS )
      END IF

*  Free workspace.
      CALL PSX_FREE( IPAS, STATUS )
      CALL PSX_FREE( IPBS, STATUS )
      CALL PSX_FREE( IPWRK1, STATUS )
      CALL PSX_FREE( IPWRK2, STATUS )

*  Obtain the output title and insert it into the result NDF.
      IF ( MODIN ) THEN
         CALL NDF_CINP( 'TITLE', INNDF, 'TITLE', STATUS )
      ELSE
         CALL NDF_CINP( 'TITLE', OUTNDF, 'TITLE', STATUS )
      END IF

*  Exit in error label, tidyup after this point.
 999  CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report context information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'FITLINE_ERR',
     :        'FITLINES: Error fitting polynomials to NDF lines',
     :                 STATUS )
      END IF

      END
