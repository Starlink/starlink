      SUBROUTINE ROTATE ( STATUS )
*+
*  Name:
*     ROTATE

*  Purpose:
*     Rotates a two-dimensional NDF about its centre through any angle.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ROTATE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine rotates an array stored in an NDF data structure
*     by an arbitrary angle.  The rotation angle can be chosen
*     automatically to make north vertical in the output NDF (see
*     parameter ANGLE).  The origin of the rotation is around the point
*     (0,0) in pixel co-ordinates.  The output array dimensions just
*     accommodate the rotated array.  Output pixels can be generated
*     from the input array by one of two methods: nearest-neighbour
*     substitution or by bi-linear interpolation.  The latter is slower,
*     but gives better results.  Output pixels not corresponding to
*     input pixels take the bad value.
*
*     The NDF may have two or three dimensions.  If it has three
*     dimensions, then the rotation is applied in turn to each plane in
*     the cube and the result written to the corresponding plane in the
*     output cube.  The orientation of the rotation plane can be
*     specified using the AXES parameter.

*  Usage:
*     rotate in out angle

*  ADAM Parameters:
*     ANGLE = _REAL (Read)
*        Number of clockwise degrees by which the data array is to be
*        rotated.  It must lie between -360 and 360 degrees.  The
*        suggested default is the current value.  If a null (!) value is
*        supplied, then the rotation angle is chosen to make north
*        vertical at the centre of the image.  If the current co-ordinate
*        Frame in the input NDF is not a celestial co-ordinate frame, then
*        the rotation angle is chosen to make the second axis of the
*        current Frame vertical.
*     ANGLEUSED = _REAL (Write)
*        An output parameter holding the rotation angle actually used, in
*        degrees. This is useful if a null value is supplied for parameter
*        ANGLE.
*     AXES(2) = _INTEGER (Read)
*        This parameter is only accessed if the NDF has exactly three
*        significant pixel axes.  It should be set to the indices of the
*        NDF pixel axes which span the plane in which rotation is to
*        be applied.  All pixel planes parallel to the specified plane
*        will be rotated independently of each other.  The dynamic
*        default comprises the indices of the first two significant
*        axes in the NDF.  Note that excluding the first significant
*        axis may be very inefficient for large cubes; a prior
*        reconfiguration with application PERMAXES that is compatible
*        with the dynamic default for AXES, will often prove beneficial.
*        []
*     IN = NDF (Read)
*        NDF structure containing the two- or three-dimensional array to
*        be rotated.
*     NNMETH = _LOGICAL (Read)
*        If TRUE, the nearest-neighbour method will be used to evaluate
*        the output data-array pixels.  This is only accessed when the
*        rotation is not a multiple of 90 degrees.  [FALSE]
*     OUT = NDF (Write)
*        Output NDF to contain the rotated arrays.
*     QUALITY = _LOGICAL (Read)
*        This parameter is only accessed when NNMETH is FALSE and ANGLE
*        is not a multiple of 90 degrees.  Strictly, the quality values
*        are undefined by the bi-linear interpolation and hence cannot
*        be propagated.  However, QUALITY = TRUE offers an approximation
*        to the quality array by propagating the nearest-neighbour
*        quality to the output NDF.  [FALSE]
*     TITLE = LITERAL (Read)
*        A title for the output NDF.  A null value will cause the title
*        of the NDF supplied for parameter IN to be used instead.  [!]
*     USEAXIS = GROUP (Read)
*        USEAXIS is only accessed if the current co-ordinate Frame of
*        the NDF has more than two axes.  A group of two strings should
*        be supplied specifying the two axes which are to be used when
*        determining the rotation angle needed to make north vertical.
*        Each axis can be specified using one of the following options.
*
*        - Its integer index within the current Frame of the input
*        NDF (in the range 1 to the number of axes in the current
*        Frame).
*        - Its symbol string such as "RA" or "VRAD".
*        - A generic option where "SPEC" requests the spectral axis,
*        "TIME" selects the time axis, "SKYLON" and "SKYLAT" picks the
*        sky longitude and latitude axes respectively.  Only those axis
*        domains present are available as options.
*
*        A list of acceptable values is displayed if an illegal value is
*        supplied.  If a null (!) value is supplied, the axes with the
*        same indices as the two used pixel axes within the NDF are
*        used.  [!]
*     VARIANCE = _LOGICAL (Read)
*        A TRUE value causes variance values to be used as weights for
*        the pixel values in bi-linear interpolation, and also causes
*        output variances to be created.  This parameter is ignored if
*        ANGLE is a multiple of 90 degrees or NNMETH=TRUE; in these
*        cases the variance array is merely propagated.  If a null (!)
*        value is supplied, the value used is TRUE if the input NDF has
*        a VARIANCE component, and FALSE otherwise.  Note that following
*        this operation the errors are no longer independent.  [!]

*  Examples:
*     rotate ns ew 90
*        This rotates the array components in the NDF called ns by 90
*        degrees clockwise around pixel co-ordinates [0,0] and stores
*        the result in the NDF called ew.  The former x axis becomes the
*        new y axis, and the former y axis becomes the new x axis.  The
*        former y-axis arrays are also reversed in the process.
*     rotate m31 m31r angle=!
*        This rotates the NDF called m31 so that north is vertical and
*        stores the results in an NDF called m31r.  This assumes that
*        the current WCS Frame in the input NDF is a celestial
*        co-ordinate Frame.
*     rotate angle=180 out=sn in=ns
*        This rotates the array components in the NDF called ns by 180
*        degrees clockwise around the pixel co-ordinates [0,0], and
*        stores the result in the NDF called sn.  The axis arrays are
*        flipped in the output NDF.
*     rotate f1 f1r 37.2 novariance
*        This rotates the array components in the NDF called f1 by 37.2
*        degrees clockwise around the pixel co-ordinates [0,0], and
*        stores the result in the NDF called f1r.  The original axis
*        information is lost.  Bi-linear interpolation is used without
*        variance information.  No quality or variance information is
*        propagated.
*     rotate f1 f1r 106 nnmeth title="Reoriented features map"
*        This rotates the array components in the NDF called f1 by 106
*        degrees clockwise around the pixel co-ordinates [0,0], and
*        stores the result in the NDF called f1r.  The original axis
*        information is lost.  The resultant array components, all of
*        which are propagated, are calculated by the nearest-neighbour
*        method.  The title of the output NDF is "Reoriented features
*        map".
*     rotate velmap rotvelmap 70
*        This rotates the array components in the three-dimensional NDF
*        called velmap by 70 degrees clockwise around the pixel
*        co-ordinates [0,0], and stores the result in the NDF called
*        rotvelmap.  The rotation is applied to the first two pixel axes
*        repeated for all the planes in the cube's third pixel axis.
*     rotate velmap rotvelmap 70 axes=[1,3]
*        This as the previous example except that the rotation is
*        applied in the plane given by the first and third pixel axes.

*  Notes:
*     -  Bad pixels are ignored in the bi-linear interpolation.  If all
*     four pixels are bad, the result is bad.

*  Related Applications:
*     KAPPA: FLIP, RESAMPLE; Figaro: IREVX, IREVY, IROT90.

*  Implementation Status:
*     The propagation rules depend on parameters ANGLE and NNMETH.
*
*     -  For rotations that are multiples of 90-degrees, VARIANCE,
*     QUALITY, AXIS, HISTORY, LABEL WCS, and UNITS components of the
*     input NDF are propagated to the output NDF.  The axis and WCS
*     components are switched and flipped as appropriate.
*     -  For the nearest-neighbour method VARIANCE, QUALITY, HISTORY,
*     LABEL, WCS, and UNITS components of the input NDF are propagated
*     to the output NDF.
*     -  For the linear interpolation method HISTORY, LABEL, WCS, and
*     UNITS components of the input NDF are propagated to the output
*     NDF.  In addition if parameter VARIANCE is TRUE, variance
*     information is derived from the input variance; and if parameter
*     QUALITY is TRUE, QUALITY is propagated using the nearest
*     neighbour.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric types are supported, though for linear
*     interpolation the arithmetic is performed using single- or
*     double-precision floating point as appropriate; and for 90 and
*     270-degree rotations _INTEGER is used for all integer types.

*  Copyright:
*     Copyright (C) 1995, 1998-1999, 2002, 2004 Central Laboratory of
*     the Research Councils.
*     Copyright (C) 2005-2006 Particle Physics & Astronomy Research
*     Council.
*     Copyright (C) 2008-2009, 2012 Science and Technology Facilities
*     Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TDCA: Tim Ash (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1995 May 14 (MJC):
*        Original NDF version.
*     12-JUN-1998 (DSB):
*        Added propagation of the NDF WCS component.  Fixed bug which
*        prevented two-dimensional slices from n-dimensional cubes
*        being processed.
*     30-JUN-1999 (TDCA):
*        Allowed rotation around an arbitary point.
*     02-JUL-1999 (TDCA):
*        Removed PIVOT parameter, and restricted rotation to around
*        point (0,0) in pixel co-ordinates.
*     17-AUG-1999 (DSB):
*        Tidied up the above TDCA changes.
*     15-FEB-2002 (DSB):
*        Added option to rotate north to vertical.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     24-AUG-2005 (DSB):
*        Corrected to work with one-dimensional NDFs.  Also treat zero
*        rotation as a special case.
*     2006 April 12 (MJC):
*        Remove unused variables and wrapped long lines.
*     2006 August 19 (MJC):
*        Added support for rotating all two-dimensional planes in a
*        cube.
*     2007 May 16 (MJC):
*        Fixed bug introduced at previous change, where for a
*        two-dimensional array with an insignificant dimension, or a
*        cube with two insignificant dimensions, the wrong bounds were
*        assigned to the output NDF.
*     2008 June 17 (MJC):
*        Trim trailing blanks from output NDF character components.
*     2009 March 26 (MJC):
*        Remove unnecessary processing for rotations of zero degrees and
*        arbitrary angles.  The latter case had unnecessary
*        propagation of the array components that could exhaust memory
*        for large cubes.  Reduced memory requirements for 180-degree
*        rotation of cubes.
*     28-AUG-2009 (DSB):
*        Correct procedure for determining default rotation angle (old
*        system assumed the WCS was 2-dimensional).
*     2012 May 9 (MJC):
*        Add _INT64 support.
*     14-DEC-2012 (DSB):
*        More accurate estimation of north - Estimate north at the centre
*        of the image, using a 1 pixel increment.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'PRM_PAR'          ! VAL__ errors
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      DOUBLE PRECISION DTOR      ! Degs to radians factor
      PARAMETER ( DTOR = 0.01745329251994329577 )

      INTEGER NDIM
      PARAMETER ( NDIM = 2 )     ! Dimensionality of input/output arrays

      INTEGER SQRMAX
      PARAMETER ( SQRMAX = 256 ) ! Maximum size of the square sub-array
                                 ! to be used in +/-90-degree rotations

      REAL TOLER                 ! Tolerance for equality of angles
      PARAMETER ( TOLER = 0.002 )
                                 ! corresponds to half-a-pixel shift
                                 ! across 14 000 pixels.

*  Local Variables:
      DOUBLE PRECISION A( 2 )    ! First point
      CHARACTER * ( 8 ) ACOMP( 3 ) ! Axis array components to process
      REAL ANGLE                 ! Clockwise degrees rotation
      DOUBLE PRECISION ANGLED    ! Rotation angle
      CHARACTER * ( 80 ) AXCOMP  ! Axis character component
      INTEGER AXES( 2 )          ! Indices of inputs to be picked
      INTEGER AXSUM              ! Sum of the axis indices
      INTEGER IAXIS              ! Axis index
      LOGICAL AXIS               ! Axis structure present?
      LOGICAL AXNORM             ! Axis normalisation flag
      DOUBLE PRECISION B( 2 )    ! Second point
      LOGICAL BAD                ! Bad-pixel flag
      BYTE BB                    ! Quality Bad-bits value
      INTEGER BCMAP              ! Base to current WCS Mapping
      DOUBLE PRECISION C( 2 )    ! Third point
      INTEGER CFRM               ! Current WCS Frame
      CHARACTER * ( 8 ) COMP( 3 ) ! Array components to process
      CHARACTER * ( 13 ) COMPS   ! Array components to process (more
                                 ! than one at a time)
      DOUBLE PRECISION COSANG    ! Cosine of rotation angle
      INTEGER DIMSI( NDIM )      ! Significant dimensions of input array
      INTEGER DIMSO( NDIM )      ! Significant dimensions of o/p array
      CHARACTER * ( NDF__SZFTP ) DTYPE ! Array component storage type
      INTEGER EL                 ! Number of elements mapped
      CHARACTER * ( NDF__SZFRM ) FORM ! Form of the NDF array
      INTEGER FRM2D              ! WCS rotation plane
      DOUBLE PRECISION GXC       ! GRID X at centre of image
      DOUBLE PRECISION GYC       ! GRID Y at centre of image
      INTEGER I                  ! Loop counter
      INTEGER ICOMP              ! Loop counter for array components
      INTEGER IDIM               ! Total number of dimensions
      INTEGER ISHIFT( NDIM + 1 ) ! Extra shift to align pivot points
      INTEGER IWCS               ! WCS FrameSet for input NDF
      DOUBLE PRECISION IXC       ! X pixel co-ord., centre of i/p array
      DOUBLE PRECISION IYC       ! Y pixel co-ord., centre of i/p array
      INTEGER JUNK               ! An unused Mapping
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of NDF pixel axes
      INTEGER LBNDO( NDF__MXDIM ) ! Lower bounds of output array
      INTEGER LBNDS( NDIM + 1 )  ! Lower bounds of plane in cube
      INTEGER LONG               ! Longer dimension of input array
      INTEGER M( 4 )             ! MATRIX indices
      INTEGER MAP2D              ! Mapping from 2D GRID to 2D WCS
      DOUBLE PRECISION MATRIX( NDF__MXDIM*NDF__MXDIM ) ! Rotation matrix
                                 ! for i/p -> o/p mapping
      INTEGER NC                 ! No. characters in text buffer
      INTEGER NDFI               ! Input NDF identifier
      INTEGER NDFIB              ! Section of input NDF to be rotated
      INTEGER NDFO               ! Output NDF identifier
      INTEGER NDFOB              ! Section of output NDF to be filled
      INTEGER NDFS               ! NDF section identifier
      LOGICAL NNMETH             ! Use nearest-neighbour method?
      LOGICAL NRAFLG             ! Non-right angle rotation requested?
      INTEGER NUMRA              ! Number of clockwise right angles to
                                 ! be applied
      DOUBLE PRECISION OFFSET( NDF__MXDIM ) ! Offset vector for
                                 ! i/p -> o/p mapping
      INTEGER OUTAX( NDF__MXDIM )! Indices of outputs to be picked
      DOUBLE PRECISION OXC       ! X pixel co-ord., centre of o/p array
      DOUBLE PRECISION OYC       ! Y pixel co-ord., centre of o/p array
      INTEGER PAXHI              ! Upper pixel bound of perp. axis
      INTEGER PAXLO              ! Lower pixel bound of perp. axis
      INTEGER PAXVAL             ! Current pixel value on perp. axis
      INTEGER PERPAX             ! Index of axis perp. to rotation plane
      INTEGER PNTRI( 2 )         ! Pointer to mapped input arrays
      INTEGER PNTRO( 2 )         ! Pointer to mapped output arrays
      LOGICAL QUAL               ! Propagate quality?
      INTEGER ROTSZE             ! Size of the square sub-array for
                                 ! rotation
      INTEGER SDIM( NDIM )       ! Indices of significant dimensions
      INTEGER SWDIM( NDIM + 1 )  ! Indices of WCS significant dimensions
      INTEGER SHORT              ! Shorter dimension of input array
      DOUBLE PRECISION SINANG    ! Sine of rotation angle
      INTEGER SLBNDI( NDIM + 1 ) ! Significant lower bounds, input array
      INTEGER SUBNDI( NDIM + 1 ) ! Significant upper bounds, input array
      LOGICAL THERE              ! Component is defined?
      CHARACTER * ( NDF__SZTYP ) TYPE ! Array component numeric type
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of NDF pixel axes
      INTEGER UBNDO( NDF__MXDIM ) ! Upper bounds of output array
      INTEGER UBNDS( NDIM + 1 )  ! Upper bounds of plane in cube
      LOGICAL VAR                ! Use variance in interpolation?
      INTEGER WKPNTR             ! Pointer to mapped workspace
      LOGICAL XLARGE             ! First dimension is larger dimension?
      DOUBLE PRECISION XP( 2 )   ! Axis-1 values
      DOUBLE PRECISION YP( 2 )   ! Axis-2 values

*  Local Data:
      DATA COMP / 'Data', 'Variance', 'Quality' /
      DATA ACOMP / 'Centre', 'Width', 'Variance' /

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the input NDF and its dimensions.
*  ========================================

*  Obtain the identifier of the input NDF.
      CALL LPG_ASSOC( 'IN', 'READ', NDFI, STATUS )

*  Obtain the input NDF, its significant axes up to the maximum two
*  dimensions for processing, and the NDF's bounds.  If the NDF
*  possesses three significant dimensions, obtain an iteration axis
*  through parameter AXES, so that planes along that axis can be
*  processed in sequence.
      CALL KPG1_GNDFP( 'IN', 'AXES', NDIM, 'READ', NDFI, SDIM, LBND,
     :                 UBND, PERPAX, STATUS )

*  Exit if an error occurred.  This is needed because the significant
*  dimensions are used as array indices.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Determine its dimensions (note that only two significant dimensions
*  can be accommodated).  Then ignore non-significant dimensions.
      DIMSI( 1 ) = UBND( SDIM( 1 ) ) - LBND( SDIM( 1 ) ) + 1
      DIMSI( 2 ) = UBND( SDIM( 2 ) ) - LBND( SDIM( 2 ) ) + 1

*  Find the total number of dimensions, and all the bounds of the NDF.
*  Store these in the output bounds.  The two-dimensional rotate plane
*  will be modified below.
      CALL NDF_BOUND( NDFI, NDF__MXDIM, LBNDO, UBNDO, IDIM, STATUS )

*  Ensure we are using at least two pixel axes and no more than three.
      IDIM = MAX( NDIM, MIN( IDIM, NDIM + 1 ) )

*  If we are dealing with a cube, create a two-dimensional section
*  from the input NDF of the size of each plane to be rotated.
      IF ( IDIM .GT. NDIM ) THEN

*  Allow for the special case when a three-dimensional array has two
*  single-element dimensions.  The first term is the sum of the
*  dimension indices from 1 to NDIM
         IF ( DIMSI( 1 ) .EQ. 1 .OR. DIMSI( 2 ) .EQ. 1 ) THEN
            AXSUM = 0
            DO I = 1, IDIM
               AXSUM = AXSUM + I
            END DO
            PERPAX = AXSUM - SDIM( 1 ) - SDIM( 2 )
         END IF

         DO I = 1, NDIM
            LBNDS( SDIM( I ) ) = LBNDO( SDIM( I ) )
            UBNDS( SDIM( I ) ) = UBNDO( SDIM( I ) )
         END DO
         LBNDS( PERPAX ) = LBNDO( PERPAX )
         UBNDS( PERPAX ) = LBNDS( PERPAX )
         CALL NDF_SECT( NDFI, IDIM, LBNDS, UBNDS, NDFS, STATUS )

      ELSE

*  Allow for the special case when a two-dimensional array has a
*  single-element dimension.
         IF ( PERPAX .LE. NDIM ) PERPAX = NDIM + 1

         CALL NDF_CLONE( NDFI, NDFS, STATUS )
      END IF

*  Get an AST pointer to a FrameSet describing the co-ordinate Frames
*  present in the NDF's WCS component.  Modify it to ensure that the
*  Base, PIXEL and Current frames all have IDIM dimensions.  The NDF
*  must have no more than IDIM significant dimensions (i.e. axes
*  spanning more than one pixel).  A single significant axis is
*  allowed.
      CALL KPG1_ASGET( NDFS, IDIM, .FALSE., .TRUE., .TRUE., SWDIM,
     :                 SLBNDI, SUBNDI, IWCS, STATUS )
      CALL NDF_ANNUL( NDFS, STATUS )

*  Use the earlier two-dimensional rotation plane bounds rather than
*  the three-dimensional bounds from KPG1_ASGET.
      IF ( IDIM .GT. NDIM ) THEN
         DO I = 1, NDIM
            SLBNDI( I ) = LBND( SDIM( I ) )
            SUBNDI( I ) = UBND( SDIM( I ) )
         END DO
      END IF

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Set bounds and flags for loop through planes of a cube.  This will
*  a single loop for a two-dimensional array as the bounds will both
*  be 1.
      PAXLO = LBND( PERPAX )
      PAXHI = UBND( PERPAX )

*  Find the rotation parameters.
*  =============================

*  Get the number of clockwise degrees rotation to be applied
      CALL PAR_GDR0R( 'ANGLE', 90.0, -360.0 + VAL__SMLR,
     :                360.0 - VAL__SMLR, .FALSE., ANGLE, STATUS )

*  If a null value was supplied, annull the error and find the angle
*  between the second significant pixel axis and north.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  Get the Mapping from GRID coords to WCS coords.
         BCMAP = AST_GETMAPPING( IWCS, AST__BASE, AST__CURRENT,
     :                             STATUS )

*  Get the current WCS Frame.
         CFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  We need (effectively) a FrameSet in which the base Frame contains the
*  two GRID axes that span the rotation plane, and the current Frame
*  contains two corresponding WCS axes. If the WCS is 2-dimensional then
*  we can just use the above Mapping amd Frame to form the FrameSet.
         IF( IDIM .EQ. 2 ) THEN
            MAP2D = AST_CLONE( BCMAP, STATUS )
            FRM2D = AST_CLONE( CFRM, STATUS )

*  If the WCS is 3D, we need to split off the two GRID axes that span the
*  rotation plane.
         ELSE

*  Get the indices of the GRID axes that span the rotation plane.
            IF( PERPAX .EQ. 1 ) THEN
               AXES( 1 ) = 2
               AXES( 2 ) = 3
            ELSE IF( PERPAX .EQ. 2 ) THEN
               AXES( 1 ) = 1
               AXES( 2 ) = 3
            ELSE
               AXES( 1 ) = 1
               AXES( 2 ) = 2
            END IF

*  Attempt to split the full WCS Mapping in order to get a Mapping from
*  the above selected GRID axes and the corresponding WCS axes.
            CALL AST_MAPSPLIT( BCMAP, 2, AXES, OUTAX, MAP2D, STATUS )

*  Report an error if the Mapping could not be split.
            IF( MAP2D .EQ. AST__NULL ) THEN
               IF( STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'Rotation angle is undefined.',
     :                          STATUS )
               END IF

*  Report an error if the two GRID axes do not map into exactly two WCS
*  axes.
            ELSE IF( STATUS .EQ. SAI__OK ) THEN
               IF( AST_GETI( MAP2D, 'Nout', STATUS ) .NE. 2 ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'Rotation angle is undefined.',
     :                          STATUS )

*  Otherwise, ceate a Frame containing the two corresponding WCS axes.
               ELSE
                  FRM2D = AST_PICKAXES( CFRM, 2, OUTAX, JUNK, STATUS )
               END IF
            END IF
         END IF

*  If the 2D WCS Frame is a celestial co-ord Frame, get the index
*  of the latitude axis.  Otherwise, use the second axis.
         IF ( AST_ISASKYFRAME( FRM2D, STATUS ) ) THEN
            IAXIS = AST_GETI( FRM2D, 'LATAXIS', STATUS )
         ELSE
            IAXIS = 2
         END IF

*  GRID coords at centre of rotation plane.
         GXC = 0.5D0*( 1.0D0 + DIMSI( 1 ) )
         GYC = 0.5D0*( 1.0D0 + DIMSI( 2 ) )

*  Transform two points on the second GRID axis into the current Frame.
         XP( 1 ) = GXC
         YP( 1 ) = GYC
         XP( 2 ) = GXC
         YP( 2 ) = GYC + 1.0D0
         CALL AST_TRAN2( MAP2D, 2, XP, YP, .TRUE., XP, YP, STATUS )

*  Find another point (C) which is to the north of point 1 (A). The
*  arc-distance from C to A is equal to the arc-distance form B to A.
         A( 1 ) = XP( 1 )
         A( 2 ) = YP( 1 )
         B( 1 ) = XP( 2 )
         B( 2 ) = YP( 2 )
         C( IAXIS ) = AST_AXOFFSET( FRM2D, IAXIS, A( IAXIS ),
     :                              AST_DISTANCE( FRM2D, A, B, STATUS ),
     :                              STATUS )
         C( 3 - IAXIS ) = A( 3 - IAXIS )

*  Convert A and C back into GRID co-ords.
         XP( 1 ) = A( 1 )
         YP( 1 ) = A( 2 )
         XP( 2 ) = C( 1 )
         YP( 2 ) = C( 2 )
         CALL AST_TRAN2( MAP2D, 2, XP, YP, .FALSE., XP, YP, STATUS )

*  Find the angle between the line joining these transformed points in
*  the GRID Frame, and the second GRID axis.
         A( 1 ) = XP( 1 )
         A( 2 ) = YP( 1 )
         B( 1 ) = XP( 2 )
         B( 2 ) = YP( 2 )

         ANGLED = AST_AXANGLE( AST_FRAME( 2, 'Domain=GRID', STATUS ),
     :                         A, B, 2, STATUS )

*  Check the angle is OK.  If so, convert to degrees.  Otherwise report
*  an error.
         IF ( ANGLED .NE. AST__BAD ) THEN
            ANGLE = REAL( -ANGLED/DTOR )
            IF ( ANGLE .LT. 0.0 ) ANGLE = 360 + ANGLE
            CALL MSG_BLANK( STATUS )
            CALL MSG_SETR( 'A', ANGLE )
            CALL MSG_OUT( 'ROTATE_MSG1', '  Rotating by ^A degrees',
     :                    STATUS )
            CALL MSG_BLANK( STATUS )
         ELSE
            IF ( STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'ROTATE_ERR1', 'Rotation angle is '//
     :                       'undefined.', STATUS )
            END IF
         END IF

      END IF

*  Ensure the angle is in the range 0 - 360.
      DO WHILE ( ANGLE .LT. 0.0 )
         ANGLE = 360 + ANGLE
      END DO

      DO WHILE ( ANGLE .GT. 360.0 )
         ANGLE = ANGLE - 360.0
      END DO

*  Write out the angle actually used.
      CALL PAR_PUT0R( 'ANGLEUSED', ANGLE, STATUS )

*  Look for the special cases.
*  A simple 0-degree rotation...
      IF ( ABS( ANGLE ) .LT. VAL__SMLR ) THEN
         NUMRA   =  0
         NRAFLG  = .FALSE.

*  A simple 90-degree rotation...
      ELSE IF ( ABS( ANGLE - 90.0 ) .LT. VAL__SMLR ) THEN
         NUMRA   =  1
         NRAFLG  = .FALSE.

*  Simple 180-degree rotation...
      ELSE IF ( ABS( ANGLE - 180.0 ) .LT. VAL__SMLR ) THEN
         NUMRA   =  2
         NRAFLG  = .FALSE.

*  Simple 270-degree rotation...
      ELSE IF ( ABS( ANGLE - 270.0 ) .LT. VAL__SMLR ) THEN
         NUMRA   =  3
         NRAFLG  = .FALSE.

*  Not a simple 90-degree rotation...  Use an undefined value
*  for the multiple of right angles, as zero is being used
*  for the null, i.e. zero degrees, rotation.
      ELSE
         NUMRA   =  VAL__BADI
         NRAFLG  = .TRUE.

      END IF

      IF ( NRAFLG ) THEN

*  Work out the dimensions of the output array to hold the results of
*  the non-right angle rotation.
         CALL KPS1_ROSIZ( DIMSI, ANGLE, DIMSO, STATUS )

*  Compute the output bounds and dimensions.
         LBNDO( SDIM( 1 ) ) = 1
         LBNDO( SDIM( 2 ) ) = 1
         UBNDO( SDIM( 1 ) ) = DIMSO( 1 )
         UBNDO( SDIM( 2 ) ) = DIMSO( 2 )
      ELSE

*  A rotation angle divisible by 90.0 degrees has been requested;
*  proceed according to the set value of NUMRA.
         IF ( NUMRA .EQ. 0 .OR. NUMRA .EQ. 2 ) THEN

*  A 180-degree rotation so output dimensions are same as the work (or
*  input) dimensions.  The array components are copied so the section
*  needs to be correct.
            DO I = 1, NDIM
               DIMSO( I ) = DIMSI( I )
               LBNDO( SDIM( I ) ) = SLBNDI( I )
               UBNDO( SDIM( I ) ) = SUBNDI( I )
            END DO
         ELSE

*  Must be 90- or 270-degree rotation so reverse dimensions and bounds.
            DIMSO( 1 ) = DIMSI( 2 )
            DIMSO( 2 ) = DIMSI( 1 )
            LBNDO( SDIM( 1 ) ) = SLBNDI( 2 )
            LBNDO( SDIM( 2 ) ) = SLBNDI( 1 )
            UBNDO( SDIM( 1 ) ) = SUBNDI( 2 )
            UBNDO( SDIM( 2 ) ) = SUBNDI( 1 )

         END IF
      END IF

*  Find which array components to propagate.
*  =========================================

*  Find the method used to evaluate output values.  This one of two
*  basic methods, depending on whether or not the input rotation angle
*  was found to be an integer multiple of 90.0 degrees, i.e. if NRAFLG
*  is true, then a non-90.0-degree rotation has been requested.
      IF ( NRAFLG ) THEN
         CALL PAR_GTD0L( 'NNMETH', .FALSE., .TRUE., NNMETH, STATUS )

*  For 90-degree multiples and nearest-neighbour, the element is defined
*  and so variance and quality information can be propagated.
         IF ( .NOT. NNMETH ) THEN

*  See if there is a variance array.
            CALL NDF_STATE( NDFI, 'Variance', VAR, STATUS )

*  Decide whether or not to process the VARIANCE array.
            IF ( STATUS .EQ. SAI__OK ) THEN
               CALL PAR_DEF0L( 'VARIANCE', VAR, STATUS )
               CALL PAR_GET0L( 'VARIANCE', VAR, STATUS )
               IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
            END IF

*  Decide whether or not to process the QUALITY array.
            CALL PAR_GET0L( 'QUALITY', QUAL, STATUS )
         END IF

      ELSE
         NNMETH = .FALSE.
         VAR = .TRUE.
         QUAL = .TRUE.
      END IF

*  Create the output NDF.
*  ======================
*
*  Take a shortcut to propagate ancillary data from the input NDF.
*  Create a section from the input NDF of the size of the required NDF.
      CALL NDF_SECT( NDFI, IDIM, LBNDO, UBNDO, NDFS, STATUS )

*  Create the output NDF.  0-degree rotation special case needs the
*  array components to be copied, as nothing else need be done.
      IF ( NUMRA .EQ. 0 ) THEN
         CALL LPG_PROP( NDFS, 'AXIS,DATA,VARIANCE,QUALITY,UNITS',
     :                   'OUT', NDFO, STATUS )

*  Axis arrays will be overwritten later for the other right-angle
*  multiples.
      ELSE IF ( NUMRA .GE. 1 .AND. NUMRA .LE. 3 ) THEN
         CALL LPG_PROP( NDFS, 'AXIS,UNITS', 'OUT', NDFO, STATUS )

      ELSE
         CALL LPG_PROP( NDFS, 'UNITS', 'OUT', NDFO, STATUS )
      END IF

*  Obtain a new title for the output NDF.
      CALL NDF_CINP( 'TITLE', NDFO, 'Title', STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Nearest-neighbour method
*  ========================
*
*  This is for an angle that is not a multiple of 90 degrees.  Axis
*  information is not generated.
      IF ( NNMETH ) THEN

*  Disable automatic quality masking, since the quality array (if
*  present) will be handled explicitly.
         CALL NDF_SQMF( .FALSE., NDFI, STATUS )

*  Loop to process the data, variance and quality arrays in turn.
         DO ICOMP = 1, 3

*  Determine if the input array is defined.
            CALL NDF_STATE( NDFI, COMP( ICOMP ), THERE, STATUS )

*  If so, then determine its numeric type and map the input and output
*  arrays for access using this type.  The number of elements is the
*  same for both.
            IF ( THERE ) THEN
               CALL NDF_TYPE( NDFI, COMP( ICOMP ), TYPE, STATUS )

*  Loop through all the planes.
               DO PAXVAL = PAXLO, PAXHI

*  Get identifiers for the required slices of the input and output NDF.
                  LBND( PERPAX ) = PAXVAL
                  UBND( PERPAX ) = PAXVAL
                  LBNDO( PERPAX ) = PAXVAL
                  UBNDO( PERPAX ) = PAXVAL
                  CALL NDF_SECT( NDFI, NDF__MXDIM, LBND, UBND, NDFIB,
     :                           STATUS )
                  CALL NDF_SECT( NDFO, NDF__MXDIM, LBNDO, UBNDO, NDFOB,
     :                           STATUS )

*  Map these input and output arrays.
                  CALL KPG1_MAP( NDFIB, COMP( ICOMP ), TYPE, 'READ',
     :                           PNTRI, EL, STATUS )
                  CALL KPG1_MAP( NDFOB, COMP( ICOMP ), TYPE, 'WRITE',
     :                           PNTRO, EL, STATUS )
                  IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Call the appropriate routine to generate the output array using
*  the nearest-neighbour method, depending on its numeric type.
                  IF ( TYPE .EQ. '_BYTE' ) THEN
                     CALL KPS1_RONNB( DIMSI( 1 ), DIMSI( 2 ),
     :                                %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                ANGLE, DIMSO( 1 ), DIMSO( 2 ),
     :                                %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                STATUS )

                  ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
                     CALL KPS1_RONND( DIMSI( 1 ), DIMSI( 2 ),
     :                                %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                ANGLE, DIMSO( 1 ), DIMSO( 2 ),
     :                                %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                STATUS )

                  ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
                     CALL KPS1_RONNI( DIMSI( 1 ), DIMSI( 2 ),
     :                                %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                ANGLE, DIMSO( 1 ), DIMSO( 2 ),
     :                                %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                STATUS )

                  ELSE IF ( TYPE .EQ. '_INT64' ) THEN
                     CALL KPS1_RONNK( DIMSI( 1 ), DIMSI( 2 ),
     :                                %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                ANGLE, DIMSO( 1 ), DIMSO( 2 ),
     :                                %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                STATUS )

                  ELSE IF ( TYPE .EQ. '_REAL' ) THEN
                     CALL KPS1_RONNR( DIMSI( 1 ), DIMSI( 2 ),
     :                                %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                ANGLE, DIMSO( 1 ), DIMSO( 2 ),
     :                                %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                STATUS )

                  ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
                     CALL KPS1_RONNUB( DIMSI( 1 ), DIMSI( 2 ),
     :                                 %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                 ANGLE, DIMSO( 1 ), DIMSO( 2 ),
     :                                 %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                 STATUS )

                  ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
                     CALL KPS1_RONNUW( DIMSI( 1 ), DIMSI( 2 ),
     :                                 %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                 ANGLE, DIMSO( 1 ), DIMSO( 2 ),
     :                                 %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                 STATUS )

                  ELSE IF ( TYPE .EQ. '_WORD' ) THEN
                     CALL KPS1_RONNW( DIMSI( 1 ), DIMSI( 2 ),
     :                                %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                ANGLE, DIMSO( 1 ), DIMSO( 2 ),
     :                                %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                STATUS )

                  END IF

*  Free the section identifiers.
                  CALL NDF_ANNUL( NDFIB, STATUS )
                  CALL NDF_ANNUL( NDFOB, STATUS )
               END DO

*  If a quality array is being processed, then transfer the quality
*  component's bad-bits value.
               IF ( COMP( ICOMP ) .EQ. 'Quality' ) THEN
                  CALL NDF_BB( NDFI, BB, STATUS )
                  CALL NDF_SBB( BB, NDFO, STATUS )

*  Otherwise, transfer the bad-pixel flag from the input to the output
*  array unless its storage format is primitive.
               ELSE
                  CALL NDF_FORM( NDFO, COMP( ICOMP ), FORM, STATUS )
                  CALL NDF_BAD( NDFI, COMP( ICOMP ), .FALSE., BAD,
     :                          STATUS )
                  IF ( FORM .NE. 'PRIMITIVE' ) THEN
                     CALL NDF_SBAD( BAD, NDFO, COMP( ICOMP ), STATUS )
                  END IF
               END IF

            END IF
         END DO

*  Bi-linear interpolation.
*  ========================
      ELSE IF ( NRAFLG ) THEN

*  Determine if the variance array is defined.
         IF ( VAR ) CALL NDF_STATE( NDFI, 'Variance', VAR, STATUS )

*  Set the types to map.
         IF ( VAR ) THEN
            COMPS = 'Data,Variance'
         ELSE
            COMPS='Data'
         END IF

*  Determine the processing type for the array component(s).  Map the
*  input and output arrays for access using this type.
         CALL NDF_MTYPE( '_REAL,_DOUBLE', NDFI, NDFI, COMPS, TYPE,
     :                   DTYPE, STATUS )

*  Loop through all the planes.
         DO PAXVAL = PAXLO, PAXHI

*  Get identifiers for the required slices of the input and output NDF.
            LBND( PERPAX ) = PAXVAL
            UBND( PERPAX ) = PAXVAL
            LBNDO( PERPAX ) = PAXVAL
            UBNDO( PERPAX ) = PAXVAL
            CALL NDF_SECT( NDFI, NDF__MXDIM, LBND, UBND, NDFIB, STATUS )
            CALL NDF_SECT( NDFO, NDF__MXDIM, LBNDO, UBNDO, NDFOB,
     :                     STATUS )

*  Map these input and output arrays.
            CALL KPG1_MAP( NDFIB, COMPS, TYPE, 'READ', PNTRI, EL,
     :                     STATUS )
            CALL KPG1_MAP( NDFOB, COMPS, TYPE, 'WRITE', PNTRO, EL,
     :                     STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Call the appropriate routine to generate the output array using
*  the bi-linear interpolation method, depending on its numeric type.
            IF ( TYPE .EQ. '_REAL' ) THEN
               CALL KPS1_ROLIR( DIMSI( 1 ), DIMSI( 2 ),
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          VAR, %VAL( CNF_PVAL( PNTRI( 2 ) ) ),
     :                          ANGLE, DIMSO( 1 ), DIMSO( 2 ),
     :                          %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                          %VAL( CNF_PVAL( PNTRO( 2 ) ) ), STATUS )

            ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
               CALL KPS1_ROLID( DIMSI( 1 ), DIMSI( 2 ),
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          VAR, %VAL( CNF_PVAL( PNTRI( 2 ) ) ),
     :                          ANGLE, DIMSO( 1 ), DIMSO( 2 ),
     :                          %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                          %VAL( CNF_PVAL( PNTRO( 2 ) ) ), STATUS )
            END IF

*  Free the section identifiers.
            CALL NDF_ANNUL( NDFIB, STATUS )
            CALL NDF_ANNUL( NDFOB, STATUS )
         END DO

*  Set the bad-pixel flag to indicate that bad values may be present
*  unless the array's storage format is primitive.
         CALL NDF_FORM( NDFO, 'Data', FORM, STATUS )
         IF ( FORM .NE. 'PRIMITIVE' ) THEN
            CALL NDF_SBAD( .TRUE., NDFO, 'Data', STATUS )
         END IF

*  Repeat for the variance array.
         IF ( VAR ) THEN
            CALL NDF_FORM( NDFO, 'Data', FORM, STATUS )
            IF ( FORM .NE. 'PRIMITIVE' ) THEN
               CALL NDF_SBAD( .TRUE., NDFO, 'Data', STATUS )
            END IF
         END IF

*  If a quality array is being processed, then transfer the quality
*  component's bad-bits value.
         CALL NDF_STATE( NDFI, 'Quality', THERE, STATUS )
         QUAL = QUAL .AND. THERE

         IF ( QUAL ) THEN

*  Loop through all the planes.
            DO PAXVAL = PAXLO, PAXHI

*  Get identifiers for the required slices of the input and output NDF.
               LBND( PERPAX ) = PAXVAL
               UBND( PERPAX ) = PAXVAL
               LBNDO( PERPAX ) = PAXVAL
               UBNDO( PERPAX ) = PAXVAL
               CALL NDF_SECT( NDFI, NDF__MXDIM, LBND, UBND, NDFIB,
     :                        STATUS )
               CALL NDF_SECT( NDFO, NDF__MXDIM, LBNDO, UBNDO, NDFOB,
     :                        STATUS )

*  Map the quality arrays using the only valid type, unsigned byte.
               CALL KPG1_MAP( NDFIB, 'Quality', '_UBYTE', 'Read', PNTRI,
     :                        EL, STATUS )
               CALL KPG1_MAP( NDFOB, 'Quality', '_UBYTE', 'Write',
     :                        PNTRO, EL, STATUS )

*  Assign the QUALITY array using the nearest-neighbour technique.  This
*  is an approximation to retain quality information.
               CALL KPS1_RONNUB( DIMSI( 1 ), DIMSI( 2 ),
     :                           %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                           ANGLE, DIMSO( 1 ), DIMSO( 2 ),
     :                           %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                           STATUS )

*  Free the section identifiers.
               CALL NDF_ANNUL( NDFIB, STATUS )
               CALL NDF_ANNUL( NDFOB, STATUS )
            END DO

*  Transfer the quality component's bad-bits value.
            CALL NDF_BB( NDFI, BB, STATUS )
            CALL NDF_SBB( BB, NDFO, STATUS )
         END IF

*  90-degree multiple.
*  ===================

*  Nothing to do for the zero-degree case.  It has been handdled by the
*  earlier propagation.
      ELSE IF ( NUMRA .NE. 0 ) THEN

*  Process the main NDF array components.
*  --------------------------------------

*  Disable automatic quality masking, since the quality array (if
*  present) will be handled explicitly.
         CALL NDF_SQMF( .FALSE., NDFI, STATUS )

*  Loop to process the data, variance and quality arrays in turn.
         DO ICOMP = 1, 3

*  Determine if the input array is defined.
            CALL NDF_STATE( NDFI, COMP( ICOMP ), THERE, STATUS )

*  If so, then determine its numeric type and map the input and output
*  arrays for access using this type.  PSX_CALLOC does not allow one-
*  and two-byte integers.
            IF ( THERE ) THEN
               IF ( NUMRA .EQ. 2 ) THEN
                  CALL NDF_TYPE( NDFI, COMP( ICOMP ), TYPE, STATUS )
               ELSE
                  CALL NDF_MTYPE( '_INTEGER,_REAL,_DOUBLE', NDFI, NDFI,
     :                            COMP( ICOMP ), TYPE, DTYPE, STATUS )
               END IF

*  Loop through all the planes.
               DO PAXVAL = PAXLO, PAXHI

*  Get identifiers for the required slices of the input and output NDF.
                  LBND( PERPAX ) = PAXVAL
                  UBND( PERPAX ) = PAXVAL
                  LBNDO( PERPAX ) = PAXVAL
                  UBNDO( PERPAX ) = PAXVAL
                  CALL NDF_SECT( NDFI, NDF__MXDIM, LBND, UBND, NDFIB,
     :                           STATUS )
                  CALL NDF_SECT( NDFO, NDF__MXDIM, LBNDO, UBNDO, NDFOB,
     :                           STATUS )

*  Map these input and output arrays.
                  CALL KPG1_MAP( NDFIB, COMP( ICOMP ), TYPE, 'READ',
     :                           PNTRI, EL, STATUS )

                  CALL KPG1_MAP( NDFOB, COMP( ICOMP ), TYPE, 'WRITE',
     :                           PNTRO, EL, STATUS )

                  IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Rotation is through 180 degrees
                  IF ( NUMRA .EQ. 2 ) THEN

*  This process works in situ swapping pixel values, therefore the array
*  components must first be copied to the output NDF.  At one time all
*  the arrays were copied in a single LPG_PROP call.   While this is
*  efficient for smaller arrays, it mapped both input and output array
*  in their entirity, and could lead to excessive memory demands for
*  cubes.  The current coding only maps planes of the cube at a time.
                     CALL KPG1_COPY( TYPE, EL, PNTRI( 1 ), PNTRO( 1 ),
     :                               STATUS )

*  Call the appropriate routine to generate the output array for a
*  180-degree rotation, depending on its numeric type.
                     IF ( TYPE .EQ. '_BYTE' ) THEN
                        CALL KPS1_RORAB( NUMRA, DIMSO( 1 ), DIMSO( 2 ),
     :                                   DIMSO( 1 ), DIMSO( 2 ),
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )

                     ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
                        CALL KPS1_RORAD( NUMRA, DIMSO( 1 ), DIMSO( 2 ),
     :                                   DIMSO( 1 ), DIMSO( 2 ),
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )

                     ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
                        CALL KPS1_RORAI( NUMRA, DIMSO( 1 ), DIMSO( 2 ),
     :                                   DIMSO( 1 ), DIMSO( 2 ),
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )

                     ELSE IF ( TYPE .EQ. '_INT64' ) THEN
                        CALL KPS1_RORAK( NUMRA, DIMSO( 1 ), DIMSO( 2 ),
     :                                   DIMSO( 1 ), DIMSO( 2 ),
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )

                     ELSE IF ( TYPE .EQ. '_REAL' ) THEN
                        CALL KPS1_RORAR( NUMRA, DIMSO( 1 ), DIMSO( 2 ),
     :                                   DIMSO( 1 ), DIMSO( 2 ),
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )

                     ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
                        CALL KPS1_RORAUB( NUMRA, DIMSO( 1 ), DIMSO( 2 ),
     :                                   DIMSO( 1 ), DIMSO( 2 ),
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )

                     ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
                        CALL KPS1_RORAUW( NUMRA, DIMSO( 1 ), DIMSO( 2 ),
     :                                   DIMSO( 1 ), DIMSO( 2 ),
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )

                     ELSE IF ( TYPE .EQ. '_WORD' ) THEN
                        CALL KPS1_RORAW( NUMRA, DIMSO( 1 ), DIMSO( 2 ),
     :                                   DIMSO( 1 ), DIMSO( 2 ),
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )

                     END IF

*  Rotation is through +/- 90 degrees
                  ELSE


*  Set up rotation box size, long-dimension flag etc.  This needs only
*  to be once, not for every plane.
                     IF ( PAXVAL .EQ. PAXLO ) THEN
                        CALL KPS1_ROBOS( DIMSI, SQRMAX, XLARGE, ROTSZE,
     :                                   LONG, SHORT, STATUS )

*  Create workspace and map it.
                        CALL PSX_CALLOC( ROTSZE * ROTSZE, TYPE, WKPNTR,
     :                                   STATUS )
                     END IF

*  Perform the +/- 90-deg. rotation
                     IF ( TYPE .EQ. '_DOUBLE' ) THEN
                        CALL KPS1_ROBLD( NUMRA, LONG, SHORT, ROTSZE,
     :                                   XLARGE, DIMSI( 1 ), DIMSI( 2 ),
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   DIMSO( 1 ), DIMSO( 2 ),
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   %VAL( CNF_PVAL( WKPNTR ) ),
     :                                   STATUS )

                     ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
                        CALL KPS1_ROBLI( NUMRA, LONG, SHORT, ROTSZE,
     :                                   XLARGE, DIMSI( 1 ), DIMSI( 2 ),
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   DIMSO( 1 ), DIMSO( 2 ),
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   %VAL( CNF_PVAL( WKPNTR ) ),
     :                                   STATUS )

                     ELSE IF ( TYPE .EQ. '_INT64' ) THEN
                        CALL KPS1_ROBLK( NUMRA, LONG, SHORT, ROTSZE,
     :                                   XLARGE, DIMSI( 1 ), DIMSI( 2 ),
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   DIMSO( 1 ), DIMSO( 2 ),
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   %VAL( CNF_PVAL( WKPNTR ) ),
     :                                   STATUS )

                     ELSE IF ( TYPE .EQ. '_REAL' ) THEN
                        CALL KPS1_ROBLR( NUMRA, LONG, SHORT, ROTSZE,
     :                                   XLARGE, DIMSI( 1 ), DIMSI( 2 ),
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   DIMSO( 1 ), DIMSO( 2 ),
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   %VAL( CNF_PVAL( WKPNTR ) ),
     :                                   STATUS )
                     END IF

*  Tidy up the workspace.
                     IF ( PAXVAL .EQ. PAXHI ) THEN
                        CALL PSX_FREE( WKPNTR, STATUS )
                     END IF
                  END IF

*  Free the section identifiers.
                  CALL NDF_ANNUL( NDFIB, STATUS )
                  CALL NDF_ANNUL( NDFOB, STATUS )
               END DO

*  If a quality array is being processed, then transfer the quality
*  component's bad-bits value.
               IF ( COMP( ICOMP ) .EQ. 'Quality' ) THEN
                  CALL NDF_BB( NDFI, BB, STATUS )
                  CALL NDF_SBB( BB, NDFO, STATUS )

*  Otherwise, transfer the bad-pixel flag from the input to the output
*  array unless its storage format is primitive.
               ELSE
                  CALL NDF_FORM( NDFO, COMP( ICOMP ), FORM, STATUS )
                  CALL NDF_BAD( NDFI, COMP( ICOMP ), .FALSE., BAD,
     :                          STATUS )
                  IF ( FORM .NE. 'PRIMITIVE' ) THEN
                     CALL NDF_SBAD( BAD, NDFO, COMP( ICOMP ), STATUS )
                  END IF
               END IF

            END IF
         END DO

*  Process the NDF axis arrays components.
*  =======================================

*  If axis information for the reversed dimension is also to be
*  reversed, then loop to process the axis centre, width and variance
*  arrays.
         CALL NDF_STATE( NDFI, 'Axis', AXIS, STATUS )
         IF ( AXIS ) THEN
            DO ICOMP = 1, 3

*  First deal with the input x axis.
*  ---------------------------------

*  Determine if the input axis array is defined.
               CALL NDF_ASTAT( NDFI, ACOMP( ICOMP ), SDIM( 1 ), THERE,
     :                         STATUS )

*  If so, then determine its numeric type and map the input and output
*  axis arrays for access using this type.
               IF ( THERE ) THEN
                  CALL NDF_ATYPE( NDFI, ACOMP( ICOMP ), SDIM( 1 ), TYPE,
     :                            STATUS )

                  CALL NDF_AMAP( NDFI, ACOMP( ICOMP ), SDIM( 1 ), TYPE,
     :                           'READ', PNTRI, EL, STATUS )

*  Output array depends on the rotation angle.  For 180 degrees the
*  axes are flipped.  For 90 and 270 degrees there is an interchange
*  as well.
                  IF ( NUMRA .EQ. 2 ) THEN
                     CALL NDF_AMAP( NDFO, ACOMP( ICOMP ), SDIM( 1 ),
     :                              TYPE, 'WRITE', PNTRO, EL, STATUS )
                  ELSE
                     CALL NDF_AMAP( NDFO, ACOMP( ICOMP ), SDIM( 2 ),
     :                              TYPE, 'WRITE', PNTRO, EL, STATUS )
                  END IF

*  Call the appropriate routine to flip the axis array, depending on its
*  numeric type.
                  IF ( NUMRA .EQ. 1 .OR. NUMRA .EQ. 2 ) THEN
                     IF ( TYPE .EQ. '_BYTE' ) THEN
                        CALL KPG1_FLIPB( 1, EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   1,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )

                     ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
                        CALL KPG1_FLIPUB( 1, EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   1,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )

                     ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
                        CALL KPG1_FLIPD( 1, EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   1,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )

                     ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
                        CALL KPG1_FLIPI( 1, EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   1,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )

                     ELSE IF ( TYPE .EQ. '_REAL' ) THEN
                        CALL KPG1_FLIPR( 1, EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   1,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )

                     ELSE IF ( TYPE .EQ. '_WORD' ) THEN
                        CALL KPG1_FLIPW( 1, EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   1,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )

                     ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
                        CALL KPG1_FLIPUW( 1, EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   1,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )
                     END IF

                  ELSE

*  Call the appropriate routine to copy the axis array, depending on its
*  numeric type.
                     CALL KPG1_COPY( TYPE, EL, PNTRI( 1 ), PNTRO( 1 ),
     :                               STATUS )

                  END IF

*  Unmap the input and output axis arrays.
                  CALL NDF_AUNMP( NDFI, ACOMP( ICOMP ), SDIM( 1 ),
     :                            STATUS )
                  IF ( NUMRA .EQ. 2 ) THEN
                     CALL NDF_AUNMP( NDFO, ACOMP( ICOMP ), SDIM( 1 ),
     :                               STATUS )
                  ELSE
                     CALL NDF_AUNMP( NDFO, ACOMP( ICOMP ), SDIM( 2 ),
     :                               STATUS )
                  END IF
               END IF

*  Now with the input y axis.
*  --------------------------

*  Determine if the input axis array is defined.
               CALL NDF_ASTAT( NDFI, ACOMP( ICOMP ), SDIM( 2 ), THERE,
     :                         STATUS )

*  If so, then determine its numeric type and map the input and output
*  axis arrays for access using this type.
               IF ( THERE ) THEN
                  CALL NDF_ATYPE( NDFI, ACOMP( ICOMP ), SDIM( 2 ),
     :                            TYPE, STATUS )
                  CALL NDF_AMAP( NDFI, ACOMP( ICOMP ), SDIM( 2 ), TYPE,
     :                           'READ', PNTRI, EL, STATUS )

*  Output array depends on the rotation angle.  For 180 degrees the
*  axes are flipped.  For 90 and 270 degrees there is an interchange
*  as well.
                  IF ( NUMRA .EQ. 2 ) THEN
                     CALL NDF_AMAP( NDFO, ACOMP( ICOMP ), SDIM( 2 ),
     :                              TYPE, 'WRITE', PNTRO, EL, STATUS )
                  ELSE
                     CALL NDF_AMAP( NDFO, ACOMP( ICOMP ), SDIM( 1 ),
     :                              TYPE, 'WRITE', PNTRO, EL, STATUS )
                  END IF

*  Call the appropriate routine to flip the axis array, depending on its
*  numeric type.
                  IF ( NUMRA .EQ. 3 .OR. NUMRA .EQ. 2 ) THEN
                     IF ( TYPE .EQ. '_BYTE' ) THEN
                        CALL KPG1_FLIPB( 1, EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   1,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )

                     ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
                        CALL KPG1_FLIPUB( 1, EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   1,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )

                     ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
                        CALL KPG1_FLIPD( 1, EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   1,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )

                     ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
                        CALL KPG1_FLIPI( 1, EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   1,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )

                     ELSE IF ( TYPE .EQ. '_REAL' ) THEN
                        CALL KPG1_FLIPR( 1, EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   1,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )

                     ELSE IF ( TYPE .EQ. '_WORD' ) THEN
                        CALL KPG1_FLIPW( 1, EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   1,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )

                     ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
                        CALL KPG1_FLIPUW( 1, EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   1,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )
                     END IF

                  ELSE

*  Call the appropriate routine to copy the axis array, depending on its
*  numeric type.
                     CALL KPG1_COPY( TYPE, EL, PNTRI( 1 ), PNTRO( 1 ),
     :                               STATUS )

                  END IF

*  Unmap the input and output axis arrays.
                  CALL NDF_AUNMP( NDFI, ACOMP( ICOMP ), SDIM( 2 ),
     :                            STATUS )
                  IF ( NUMRA .EQ. 2 ) THEN
                     CALL NDF_AUNMP( NDFO, ACOMP( ICOMP ), SDIM( 2 ),
     :                               STATUS )
                  ELSE
                     CALL NDF_AUNMP( NDFO, ACOMP( ICOMP ), SDIM( 1 ),
     :                               STATUS )
                  END IF
               END IF
            END DO

*  Switch other axis components.
*  =============================
*
*  When the axes have been interchanged, the other axis components are
*  likely to be erroneous and must be interchanged too.
            IF ( NUMRA .EQ. 1 .OR. NUMRA .EQ. 3 ) THEN

*  Transfer the labels.  Note that NDF_ACPUT does not truncate trailing
*  blanks.
               CALL NDF_ASTAT( NDFI, 'Label', SDIM( 1 ), THERE, STATUS )
               IF ( THERE ) THEN
                  CALL NDF_ACGET( NDFI, 'Label', SDIM( 1 ), AXCOMP,
     :                            STATUS )
                  NC = CHR_LEN( AXCOMP )
                  CALL NDF_ACPUT( AXCOMP( :NC ), NDFO, 'Label',
     :                            SDIM( 2 ), STATUS )
               END IF

               CALL NDF_ASTAT( NDFI, 'Label', SDIM( 2 ), THERE, STATUS )
               IF ( THERE ) THEN
                  CALL NDF_ACGET( NDFI, 'Label', SDIM( 2 ), AXCOMP,
     :                            STATUS )
                  NC = CHR_LEN( AXCOMP )
                  CALL NDF_ACPUT( AXCOMP( :NC ), NDFO, 'Label',
     :                            SDIM( 1 ), STATUS )
               END IF

*  Transfer the units.
               CALL NDF_ASTAT( NDFI, 'Units', SDIM( 1 ), THERE, STATUS )
               IF ( THERE ) THEN
                  CALL NDF_ACGET( NDFI, 'Units', SDIM( 1 ), AXCOMP,
     :                            STATUS )
                  NC = CHR_LEN( AXCOMP )
                  CALL NDF_ACPUT( AXCOMP( :NC ), NDFO, 'Units',
     :                            SDIM( 2 ), STATUS )
               END IF

               CALL NDF_ASTAT( NDFI, 'Units', SDIM( 2 ), THERE, STATUS )
               IF ( THERE ) THEN
                  CALL NDF_ACGET( NDFI, 'Units', SDIM( 2 ), AXCOMP,
     :                            STATUS )
                  NC = CHR_LEN( AXCOMP )
                  CALL NDF_ACPUT( AXCOMP( :NC ), NDFO, 'Units',
     :                            SDIM( 1 ), STATUS )
               END IF

*  Transfer the normalisation flag.
               CALL NDF_ANORM( NDFI, SDIM( 1 ), AXNORM, STATUS )
               CALL NDF_ASNRM( AXNORM, NDFO, SDIM( 2 ), STATUS )

               CALL NDF_ANORM( NDFI, SDIM( 2 ), AXNORM, STATUS )
               CALL NDF_ASNRM( AXNORM, NDFO, SDIM( 1 ), STATUS )
            END IF

         END IF
      END IF

*  Propagate the WCS component, incorporating a linear mapping between
*  pixel co-ordinates.  This mapping is described by a matrix and an
*  offset vector.  Initialise the matrix to hold a unit matrix, and the
*  offset vector to be zero vector.  The matrix is declared as a
*  one-dimensional array because the dimensionality of the output NDF is
*  only known at run time.  Therefore we have to do the conversion from
*  row and column numbers to a one-dimensional vectorised index
*  explicitly.  Row I, column J of the matrix is stored in element
*  I + IDIM * ( J - 1 ).
      DO I = 1, IDIM * IDIM
         MATRIX( I ) = 0.0D0
      END DO

      DO I = 1, IDIM
         MATRIX( I + IDIM * ( I - 1 ) ) = 1.0D0
         OFFSET( I ) = 0.0D0
         ISHIFT ( I ) = 0
      END DO

*  Calculate the required cosine and sine values, and store them
*  in the matrix elements for the plane spanned by the significant
*  axes.
      COSANG = DBLE( COS( ANGLE * DTOR ) )
      SINANG = DBLE( SIN( ANGLE * DTOR ) )

      M( 1 ) = SDIM( 1 ) + IDIM * ( SDIM( 1 ) - 1 )
      M( 2 ) = SDIM( 1 ) + IDIM * ( SDIM( 2 ) - 1 )
      M( 3 ) = SDIM( 2 ) + IDIM * ( SDIM( 1 ) - 1 )
      M( 4 ) = SDIM( 2 ) + IDIM * ( SDIM( 2 ) - 1 )

      MATRIX( M( 1 ) ) = COSANG
      MATRIX( M( 2 ) ) = -SINANG
      MATRIX( M( 3 ) ) = SINANG
      MATRIX( M( 4 ) ) = COSANG

*  Calculate the pixel co-ordinates on the significant axes at the
*  centre of the output image.
      OXC = 0.5D0 * DBLE( UBNDO( SDIM( 1 ) ) + LBNDO( SDIM( 1 ) ) - 1 )
      OYC = 0.5D0 * DBLE( UBNDO( SDIM( 2 ) ) + LBNDO( SDIM( 2 ) ) - 1 )
      IXC = 0.5D0 * DBLE( SUBNDI( 1 ) + SLBNDI( 1 ) - 1 )
      IYC = 0.5D0 * DBLE( SUBNDI( 2 ) + SLBNDI( 2 ) - 1 )

*  Calculate the pixel offsets produced by the rotation on the
*  significant axes.
      OFFSET( SDIM( 1 ) ) = OXC - IXC * COSANG - IYC * SINANG
      OFFSET( SDIM( 2 ) ) = OYC + IXC * SINANG - IYC * COSANG

*  Calculate shift required to align old and new pivot points.
      ISHIFT( SDIM( 1 ) ) = NINT( - OFFSET( SDIM( 1 ) ) )
      ISHIFT( SDIM( 2 ) ) = NINT( - OFFSET( SDIM( 2 ) ) )

*  Propagate the WCS component.
      CALL KPG1_ASPRP( IDIM, NDFI, NDFO, MATRIX, OFFSET, STATUS )

*  Apply shift to align old and new pivot points.
      CALL NDF_SHIFT( IDIM, ISHIFT, NDFO, STATUS )

  999 CONTINUE

*  Release the NDF resources.
      CALL NDF_END( STATUS )

*  Release the AST resources.
      CALL AST_END( STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ROTATE_ERR2', 'ROTATE: Error occurred whilst '//
     :                 'trying to rotate an NDF.', STATUS )
      END IF

      END
