      SUBROUTINE ROTATE ( STATUS )
*+
*  Name:
*     ROTATE

*  Purpose:
*     Rotates a 2-dimensional NDF about its centre through any angle.

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
*     This rotates a 2-dimensional array stored in an NDF data
*     structure by an arbitrary angle.  The origin of the rotation is
*     the centre of the array.  The output array dimensions just
*     accommodate the rotated array.  Output pixels can be generated
*     from the input array by one of two methods: nearest-neighbour
*     substitution or by bi-linear interpolation.  The latter is
*     slower, but gives better results.  Output pixels not
*     corresponding to input pixels take the bad value.

*  Usage:
*     rotate in out angle

*  ADAM Parameters:
*     ANGLE  = _REAL (Read)
*        Number of clockwise degrees by which the data array is to be
*        rotated.  It must lie between 0 and 360 degrees.  The suggested
*        default is the current value.
*     IN = NDF (Read)
*        NDF structure containing the 2-dimensional array to be rotated.
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
*        be propagated.  However, QUALITY = TRUE offers an
*        approximation to the quality array by propagating the
*        nearest-neighbour quality to the output NDF. [FALSE]
*     TITLE = LITERAL (Read)
*        A title for the output NDF.  A null value will cause the title
*        of the NDF supplied for parameter IN to be used instead. [!]
*     VARIANCE = _LOGICAL (Read)
*        VARIANCE = TRUE instructs that variance values weight the
*        pixels in the bi-linear interpolation and that output variance
*        is derived from the neighbouring pixels' variance values,
*        otherwise the data values are given equal weight.  This
*        parameter is ignored if ANGLE is a multiple of 90 degrees or
*        NNMETH=TRUE; in these cases the variance array is merely
*        propagated.  The run-time default is TRUE if the input NDF has
*        a VARIANCE component, and FALSE otherwise.  Note that
*        following this operation the errors are no longer independent.
*        []

*  Notes:
*     -  Bad pixels are ignored in the bi-linear interpolation.  If all
*     four pixels are bad, the result is bad.

*  Examples:
*     rotate ns ew 90
*        This rotates the array components in the NDF called ns by 90
*        degrees clockwise, and stores the result in the NDF called
*        ew.  The former x axis becomes the new y axis, and the former
*        y axis becomes the new x axis.  The former y-axis arrays are
*        also reversed in the process.
*     rotate angle=180 out=sn in=ns
*        This rotates the array components in the NDF called ns by 180
*        degrees clockwise, and stores the result in the NDF called
*        sn.  The axis arrays are flipped in the output NDF.
*     rotate f1 f1r 37.2 novariance
*        This rotates the array components in the NDF called f1 by 37.2
*        degrees clockwise, and stores the result in the NDF called
*        f1r.  The original axis information is lost.  Bi-linear
*        interpolation is used without variance information.  No
*        quality or variance information is propagated.
*     rotate f1 f1r 106 nnmeth title="Reoriented features map"
*        This rotates the array components in the NDF called f1 by 106
*        degrees clockwise, and stores the result in the NDF called
*        f1r.  The original axis information is lost.  The resultant
*        array components, all of which are propagated, are calculated
*        by the nearest-neighbour method.  The title of the output
*        NDF is "Reoriented features map".

*  Related Applications:
*     KAPPA: FLIP, TRANSFORMER; Figaro: IREVX, IREVY, IROT90.

*  Implementation Status:
*     The propagation rules depend on parameters ANGLE and NNMETH.
*     -  For rotations that are multiples of 90-degrees, VARIANCE,
*     QUALITY, AXIS, HISTORY, LABEL WCS and UNITS components of the input
*     NDF are propagated to the output NDF.  The axis and WCS components 
*     are switched and flipped as appropriate.
*     -  For the nearest-neighbour method VARIANCE, QUALITY, HISTORY,
*     LABEL, WCS and UNITS components of the input NDF are propagated to 
*     the output NDF.
*     -  For the linear interpolation method HISTORY, LABEL, WCS and
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

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1995 May 14 (MJC):
*        Original NDF version.
*     12-JUN-1998 (DSB):
*        Added propagation of the NDF WCS component. Fixed bug which
*        prevented 2-D slices from n-D cubes being processed.
*     {enter_any_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'PRM_PAR'          ! VAL__ errors
      INCLUDE 'PAR_ERR'          ! Parameter-system errors

*  Status:
      INTEGER STATUS             ! Global status

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
      DOUBLE PRECISION
     :  COSANG,                  ! Cosine of rotation angle
     :  IXC,                     ! X pix. coord. at centre of input array
     :  IYC,                     ! Y pix. coord. at centre of input array
     :  MATRIX( NDF__MXDIM*NDF__MXDIM ), ! Rotation matrix for i/p -> o/p mapping
     :  OFFSET( NDF__MXDIM ),    ! Offset vector for i/p -> o/p mapping
     :  OXC,                     ! X pix. coord. at centre of output array
     :  OYC,                     ! Y pix. coord. at centre of output array
     :  SINANG                   ! Sine of rotation angle

      CHARACTER * ( 8 ) ACOMP( 3 ) ! Axis array components to process
      REAL ANGLE                 ! Clockwise degrees rotation
      CHARACTER * ( 80 ) AXCOMP  ! Axis character component
      LOGICAL AXIS               ! Axis structure present?
      LOGICAL AXNORM             ! Axis normalisation flag
      LOGICAL BAD                ! Bad-pixel flag
      BYTE BB                    ! Quality Bad-bits value
      CHARACTER * ( 8 ) COMP( 3 ) ! Array components to process
      CHARACTER * ( 13 ) COMPS   ! Array components to process (more
                                 ! than one at a time)
      INTEGER DIMSI( NDIM )      ! Significant dimensions of input array
      INTEGER DIMSO( NDIM )      ! Significant dimensions of output
                                 ! array
      CHARACTER * ( NDF__SZFTP ) DTYPE ! Array component storage type
      INTEGER EL                 ! Number of elements mapped
      CHARACTER * ( NDF__SZFRM ) FORM ! Form of the NDF array
      INTEGER I                  ! Loop counter
      INTEGER ICOMP              ! Loop counter for array components
      INTEGER IDIM               ! Total number of dimensions
      INTEGER IERR               ! Location of first conversion error
      INTEGER LBNDO( NDF__MXDIM ) ! Lower bounds of output array
      INTEGER LONG               ! Longer dimension of input array
      INTEGER NDFI               ! Input NDF identifier
      INTEGER NDFO               ! Output NDF identifier
      INTEGER NDFS               ! NDF section identifier
      INTEGER NERR               ! Number of conversion errors.
      LOGICAL NNMETH             ! Use nearest-neighbour method?
      LOGICAL NRAFLG             ! Non-right angle rotation requested?
      INTEGER NUMRA              ! Number of clockwise right angles to
                                 ! be applied
      INTEGER PNTRI( 2 )         ! Pointer to mapped input arrays
      INTEGER PNTRO( 2 )         ! Pointer to mapped output arrays
      LOGICAL QUAL               ! Propagate quality?
      INTEGER ROTSZE             ! Size of the square sub-array for
                                 ! rotation
      INTEGER SDIM( NDIM )       ! Indices of significant dimensions
      INTEGER SHORT              ! Shorter dimension of input array
      INTEGER SLBNDI( NDIM )     ! Significant lower bounds, input array
      INTEGER SUBNDI( NDIM )     ! Significant upper bounds, input array
      LOGICAL THERE              ! Component is defined?
      CHARACTER * ( NDF__SZTYP ) TYPE ! Array component numeric type
      INTEGER UBNDO( NDF__MXDIM ) ! Upper bounds of output array
      INTEGER WKPNTR             ! Pointer to mapped workspace
      LOGICAL VAR                ! Use variance in interpolation?
      LOGICAL XLARGE             ! First dimension is larger dimension?

*  Local Data:
      DATA COMP / 'Data', 'Variance', 'Quality' /
      DATA ACOMP / 'Centre', 'Width', 'Variance' /

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the rotation parameters.
*  =============================

*  Get the number of clockwise degrees rotation to be applied
      CALL PAR_GDR0R( 'ANGLE', 90.0, VAL__SMLR, 360.0 - VAL__SMLR,
     :                .FALSE., ANGLE, STATUS )

*  Look for the special cases.
*  A simple 90-degree rotation...
      IF ( ABS( ANGLE - 90.0 ) .LT. VAL__SMLR ) THEN
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

*  Not a simple 90-degree rotation...
      ELSE
         NUMRA   =  0
         NRAFLG  = .TRUE.

      END IF

*  Obtain the input NDF and its dimensions.
*  ========================================

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get the NDF containing the input data.  There must be only two
*  significant dimensions.
      CALL KPG1_GTNDF( 'IN', NDIM, .TRUE., 'READ', NDFI, SDIM,
     :                 SLBNDI, SUBNDI, STATUS )

*  Find the dimensions of the input array.
      DIMSI( 1 ) = SUBNDI( 1 ) - SLBNDI( 1 ) + 1
      DIMSI( 2 ) = SUBNDI( 2 ) - SLBNDI( 2 ) + 1

*  Find the total number of dimensions, and all the bounds of the NDF.
*  Store these in the output bounds.  The two-dimensional rotate plane
*  will be modified below.
      CALL NDF_BOUND( NDFI, NDF__MXDIM, LBNDO, UBNDO, IDIM, STATUS )

      IF ( NRAFLG ) THEN

*  Work out the dimensions of the output array to hold the results of
*  the non-right angle rotation.
         CALL KPS1_ROSIZ( DIMSI, ANGLE, DIMSO, STATUS )

*  Compute the output bounds and dimensions.
         DO I = 1, NDIM
            LBNDO( SDIM( I ) ) = 1
            UBNDO( SDIM( I ) ) = DIMSO( I )
         END DO
      ELSE

*  A rotation angle divisible by 90.0 degrees has been requested;
*  proceed according to the set value of NUMRA.
         IF ( NUMRA .EQ. 2 ) THEN

*  A 180-degree rotation so output dimensions are same as the work (or
*  input) dimensions.  The array components are copied so the section
*  needs to be correct.
            DO I = 1, NDIM
               DIMSO( SDIM( I ) ) = DIMSI( I )
               LBNDO( SDIM( I ) ) = SLBNDI( I )
               UBNDO( SDIM( I ) ) = SUBNDI( I )
            END DO
         ELSE

*  Must be 90- or 270-degree rotation so reverse dimensions and bounds.
            DIMSO( SDIM( 1 ) ) = DIMSI( 2 )
            DIMSO( SDIM( 2 ) ) = DIMSI( 1 )
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
            CALL PAR_DEF0L( 'VARIANCE', VAR, STATUS )
            CALL PAR_GET0L( 'VARIANCE', VAR, STATUS )
         
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

*  Create the output NDF.  One of the special cases (180-degree
*  rotation) needs the array components to be copied.  Axes will be
*  overwritten later for right-angle multiples.
      IF ( NUMRA .EQ. 2 ) THEN
         CALL NDF_PROP( NDFS, 'AXIS,DATA,VARIANCE,QUALITY,UNITS', 'OUT',
     :                  NDFO, STATUS )

      ELSE IF ( NUMRA .EQ. 1 .OR. NUMRA .EQ. 3 ) THEN
         CALL NDF_PROP( NDFS, 'AXIS,UNITS', 'OUT', NDFO, STATUS )

      ELSE
         CALL NDF_PROP( NDFS, 'UNITS', 'OUT', NDFO, STATUS )
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

               CALL NDF_MAP( NDFI, COMP( ICOMP ), TYPE, 'READ', PNTRI,
     :                       EL, STATUS )
               CALL NDF_MAP( NDFO, COMP( ICOMP ), TYPE, 'WRITE', PNTRO,
     :                       EL, STATUS )

*  Call the appropriate routine to generate the output array using
*  the nearest-neighbour method, depending on its numeric type.
               IF ( TYPE .EQ. '_BYTE' ) THEN
                  CALL KPS1_RONNB( DIMSI( 1 ), DIMSI( 2 ),
     :                             %VAL( PNTRI( 1 ) ), ANGLE,
     :                             DIMSO( 1 ), DIMSO( 2 ),
     :                             %VAL( PNTRO( 1 ) ), STATUS )
 
               ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
                  CALL KPS1_RONND( DIMSI( 1 ), DIMSI( 2 ),
     :                             %VAL( PNTRI( 1 ) ), ANGLE,
     :                             DIMSO( 1 ), DIMSO( 2 ),
     :                             %VAL( PNTRO( 1 ) ), STATUS )
 
               ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
                  CALL KPS1_RONNI( DIMSI( 1 ), DIMSI( 2 ),
     :                             %VAL( PNTRI( 1 ) ), ANGLE,
     :                             DIMSO( 1 ), DIMSO( 2 ),
     :                             %VAL( PNTRO( 1 ) ), STATUS )
 
               ELSE IF ( TYPE .EQ. '_REAL' ) THEN
                  CALL KPS1_RONNR( DIMSI( 1 ), DIMSI( 2 ),
     :                             %VAL( PNTRI( 1 ) ), ANGLE,
     :                             DIMSO( 1 ), DIMSO( 2 ),
     :                             %VAL( PNTRO( 1 ) ), STATUS )
 
               ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
                  CALL KPS1_RONNUB( DIMSI( 1 ), DIMSI( 2 ),
     :                              %VAL( PNTRI( 1 ) ), ANGLE,
     :                              DIMSO( 1 ), DIMSO( 2 ),
     :                              %VAL( PNTRO( 1 ) ), STATUS )
 
               ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
                  CALL KPS1_RONNUW( DIMSI( 1 ), DIMSI( 2 ),
     :                              %VAL( PNTRI( 1 ) ), ANGLE,
     :                              DIMSO( 1 ), DIMSO( 2 ),
     :                              %VAL( PNTRO( 1 ) ), STATUS )

               ELSE IF ( TYPE .EQ. '_WORD' ) THEN
                  CALL KPS1_RONNW( DIMSI( 1 ), DIMSI( 2 ),
     :                             %VAL( PNTRI( 1 ) ), ANGLE,
     :                             DIMSO( 1 ), DIMSO( 2 ),
     :                             %VAL( PNTRO( 1 ) ), STATUS )
 
               END IF

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

*  Unmap the input and output arrays.
               CALL NDF_UNMAP( NDFI, COMP( ICOMP ), STATUS )
               CALL NDF_UNMAP( NDFO, COMP( ICOMP ), STATUS )
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

         CALL NDF_MAP( NDFI, COMPS, TYPE, 'READ', PNTRI, EL, STATUS )
         CALL NDF_MAP( NDFO, COMPS, TYPE, 'WRITE', PNTRO, EL, STATUS )

*  Call the appropriate routine to generate the output array using
*  the nearest-neighbour method, depending on its numeric type.
         IF ( TYPE .EQ. '_REAL' ) THEN
            CALL KPS1_ROLIR( DIMSI( 1 ), DIMSI( 2 ), %VAL( PNTRI( 1 ) ),
     :                       VAR, %VAL( PNTRI( 2 ) ), ANGLE, DIMSO( 1 ),
     :                       DIMSO( 2 ), %VAL( PNTRO( 1 ) ),
     :                       %VAL( PNTRO( 2 ) ), STATUS )
 
         ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
            CALL KPS1_ROLID( DIMSI( 1 ), DIMSI( 2 ), %VAL( PNTRI( 1 ) ),
     :                       VAR, %VAL( PNTRI( 2 ) ), ANGLE, DIMSO( 1 ),
     :                       DIMSO( 2 ), %VAL( PNTRO( 1 ) ),
     :                       %VAL( PNTRO( 2 ) ), STATUS )
         END IF

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

*  Unmap the input and output arrays.
         CALL NDF_UNMAP( NDFI, COMPS, STATUS )
         CALL NDF_UNMAP( NDFO, COMPS, STATUS )

*  If a quality array is being processed, then transfer the quality
*  component's bad-bits value.
         CALL NDF_STATE( NDFI, 'Quality', THERE, STATUS )
         QUAL = QUAL .AND. THERE

         IF ( QUAL ) THEN

*  Map the quality arrays using the only valid type, unsigned byte.
            CALL NDF_MAP( NDFI, 'Quality', '_UBYTE', 'Read', PNTRI,
     :                    EL, STATUS )
            CALL NDF_MAP( NDFO, 'Quality', '_UBYTE', 'Write', PNTRO,
     :                    EL, STATUS )

*  Assign the QUALITY array using the nearest-neghbour technique.  This
*  is an approximation to retain quality information.
            CALL KPS1_RONNUB( DIMSI( 1 ), DIMSI( 2 ),
     :                        %VAL( PNTRI( 1 ) ), ANGLE, DIMSO( 1 ),
     :                        DIMSO( 2 ), %VAL( PNTRO( 1 ) ), STATUS )
 
*  Transfer the quality component's bad-bits value.
            CALL NDF_BB( NDFI, BB, STATUS )
            CALL NDF_SBB( BB, NDFO, STATUS )

*  Unmap the input and output arrays.
            CALL NDF_UNMAP( NDFI, 'Quality', STATUS )
            CALL NDF_UNMAP( NDFO, 'Quality', STATUS )
         END IF

*  90-degree multiple.
*  ===================
      ELSE

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
*  arrays for access using this type.  Note the array components have
*  been copied to the output NDF already for 180-degree rotation so use
*  update access for these.  PSX_CALLOC does not allow one- and two-byte
*  integers.
            IF ( THERE ) THEN      
               IF ( NUMRA .EQ. 2 ) THEN
                  CALL NDF_TYPE( NDFI, COMP( ICOMP ), TYPE, STATUS )
               ELSE
                  CALL NDF_MTYPE( '_INTEGER,_REAL,_DOUBLE', NDFI, NDFI,
     :                            COMP( ICOMP ), TYPE, DTYPE, STATUS )
               END IF

               CALL NDF_MAP( NDFI, COMP( ICOMP ), TYPE, 'READ', PNTRI,
     :                       EL, STATUS )

               IF ( NUMRA .EQ. 2 ) THEN
                  CALL NDF_MAP( NDFO, COMP( ICOMP ), TYPE, 'UPDATE',
     :                          PNTRO, EL, STATUS )
               ELSE
                  CALL NDF_MAP( NDFO, COMP( ICOMP ), TYPE, 'WRITE',
     :                          PNTRO, EL, STATUS )
               END IF

*  Rotation is through 180 degrees
               IF ( NUMRA .EQ. 2 ) THEN

*  Call the appropriate routine to generate the output array for a
*  180-degree rotation, depending on its numeric type.
                  IF ( TYPE .EQ. '_BYTE' ) THEN
                     CALL KPS1_RORAB( NUMRA, DIMSO( 1 ), DIMSO( 2 ),
     :                                DIMSO( 1 ), DIMSO( 2 ),
     :                                %VAL( PNTRO( 1 ) ), STATUS )
 
                  ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
                     CALL KPS1_RORAD( NUMRA, DIMSO( 1 ), DIMSO( 2 ),
     :                                DIMSO( 1 ), DIMSO( 2 ),
     :                                %VAL( PNTRO( 1 ) ), STATUS )
 
                  ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
                     CALL KPS1_RORAI( NUMRA, DIMSO( 1 ), DIMSO( 2 ),
     :                                DIMSO( 1 ), DIMSO( 2 ),
     :                                %VAL( PNTRO( 1 ) ), STATUS )
 
                  ELSE IF ( TYPE .EQ. '_REAL' ) THEN
                     CALL KPS1_RORAR( NUMRA, DIMSO( 1 ), DIMSO( 2 ),
     :                                DIMSO( 1 ), DIMSO( 2 ),
     :                                %VAL( PNTRO( 1 ) ), STATUS )
 
                  ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
                     CALL KPS1_RORAUB( NUMRA, DIMSO( 1 ), DIMSO( 2 ),
     :                                DIMSO( 1 ), DIMSO( 2 ),
     :                                %VAL( PNTRO( 1 ) ), STATUS )
 
                  ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
                     CALL KPS1_RORAUW( NUMRA, DIMSO( 1 ), DIMSO( 2 ),
     :                                DIMSO( 1 ), DIMSO( 2 ),
     :                                %VAL( PNTRO( 1 ) ), STATUS )

                  ELSE IF ( TYPE .EQ. '_WORD' ) THEN
                     CALL KPS1_RORAW( NUMRA, DIMSO( 1 ), DIMSO( 2 ),
     :                                DIMSO( 1 ), DIMSO( 2 ),
     :                                %VAL( PNTRO( 1 ) ), STATUS )
 
                  END IF

*  Rotation is through +/- 90 degrees
               ELSE


*  Set up rotation box size, long-dimension flag etc.
                  CALL KPS1_ROBOS( DIMSI, SQRMAX, XLARGE, ROTSZE,
     :                             LONG, SHORT, STATUS )

*  Create workspace and map it.
                  CALL PSX_CALLOC( ROTSZE * ROTSZE, TYPE, WKPNTR,
     :                             STATUS )

*  Perform the +/- 90-deg. rotation
                  IF ( TYPE .EQ. '_DOUBLE' ) THEN
                     CALL KPS1_ROBLD( NUMRA, LONG, SHORT, ROTSZE,
     :                                XLARGE, DIMSI( 1 ), DIMSI( 2 ),
     :                                %VAL( PNTRI( 1 ) ), DIMSO( 1 ),
     :                                DIMSO( 2 ), %VAL( PNTRO( 1 ) ),
     :                                %VAL( WKPNTR ), STATUS )
 
                  ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
                     CALL KPS1_ROBLI( NUMRA, LONG, SHORT, ROTSZE,
     :                                XLARGE, DIMSI( 1 ), DIMSI( 2 ),
     :                                %VAL( PNTRI( 1 ) ), DIMSO( 1 ),
     :                                DIMSO( 2 ), %VAL( PNTRO( 1 ) ),
     :                                %VAL( WKPNTR ), STATUS )
 
                  ELSE IF ( TYPE .EQ. '_REAL' ) THEN
                     CALL KPS1_ROBLR( NUMRA, LONG, SHORT, ROTSZE,
     :                                XLARGE, DIMSI( 1 ), DIMSI( 2 ),
     :                                %VAL( PNTRI( 1 ) ), DIMSO( 1 ),
     :                                DIMSO( 2 ), %VAL( PNTRO( 1 ) ),
     :                                %VAL( WKPNTR ), STATUS )
                  END IF
 
*  Tidy up the workspace
                  CALL PSX_FREE( WKPNTR, STATUS )
               END IF

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

*  Unmap the input and output arrays.
               CALL NDF_UNMAP( NDFI, COMP( ICOMP ), STATUS )
               CALL NDF_UNMAP( NDFO, COMP( ICOMP ), STATUS )
            END IF
         END DO

*  Process the NDF axis arrays components.
*  ---------------------------------------

*  If axis information for the reversed dimension is also to be
*  reversed, then loop to process the axis centre, width and variance
*  arrays.
         CALL NDF_STATE( NDFI, 'Axis', AXIS, STATUS )
         IF ( AXIS ) THEN
            DO ICOMP = 1, 3

*  First deal with the input x axis.

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
                        CALL KPG1_FLIPB( 1, EL, %VAL( PNTRI( 1 ) ),
     :                                   1, %VAL( PNTRO( 1 ) ), STATUS )
 
                     ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
                        CALL KPG1_FLIPUB( 1, EL, %VAL( PNTRI( 1 ) ),
     :                                   1, %VAL( PNTRO( 1 ) ), STATUS )
 
                     ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
                        CALL KPG1_FLIPD( 1, EL, %VAL( PNTRI( 1 ) ),
     :                                   1, %VAL( PNTRO( 1 ) ), STATUS )
 
                     ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
                        CALL KPG1_FLIPI( 1, EL, %VAL( PNTRI( 1 ) ),
     :                                   1, %VAL( PNTRO( 1 ) ), STATUS )
 
                     ELSE IF ( TYPE .EQ. '_REAL' ) THEN
                        CALL KPG1_FLIPR( 1, EL, %VAL( PNTRI( 1 ) ),
     :                                   1, %VAL( PNTRO( 1 ) ), STATUS )
  
                     ELSE IF ( TYPE .EQ. '_WORD' ) THEN
                        CALL KPG1_FLIPW( 1, EL, %VAL( PNTRI( 1 ) ),
     :                                   1, %VAL( PNTRO( 1 ) ), STATUS )
 
                     ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
                        CALL KPG1_FLIPUW( 1, EL, %VAL( PNTRI( 1 ) ),
     :                                   1, %VAL( PNTRO( 1 ) ), STATUS )
                     END IF

                  ELSE

*  Call the appropriate routine to copy the axis array, depending on its
*  numeric type.  There will be no conversion errors so no need to check
*  the returned IERR and NERR.
                     IF ( TYPE .EQ. '_BYTE' ) THEN
                        CALL VEC_BTOB( .FALSE., EL, %VAL( PNTRI( 1 ) ),
     :                                 %VAL( PNTRO( 1 ) ), IERR, NERR,
     :                                 STATUS )

                     ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
                        CALL VEC_DTOD( .FALSE., EL, %VAL( PNTRI( 1 ) ),
     :                                 %VAL( PNTRO( 1 ) ), IERR, NERR,
     :                                 STATUS )

                     ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
                        CALL VEC_ITOI( .FALSE., EL, %VAL( PNTRI( 1 ) ),
     :                                 %VAL( PNTRO( 1 ) ), IERR, NERR,
     :                                 STATUS )

                     ELSE IF ( TYPE .EQ. '_REAL' ) THEN
                        CALL VEC_RTOR( .FALSE., EL, %VAL( PNTRI( 1 ) ),
     :                                 %VAL( PNTRO( 1 ) ), IERR, NERR,
     :                                 STATUS )

                     ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
                        CALL VEC_UBTOUB( .FALSE., EL,
     :                                   %VAL( PNTRI( 1 ) ),
     :                                   %VAL( PNTRO( 1 ) ), IERR, NERR,
     :                                   STATUS )

                     ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
                        CALL VEC_UWTOUW( .FALSE., EL,
     :                                   %VAL( PNTRI( 1 ) ),
     :                                   %VAL( PNTRO( 1 ) ), IERR, NERR,
     :                                   STATUS )

                     ELSE IF ( TYPE .EQ. '_WORD' ) THEN
                        CALL VEC_WTOW( .FALSE., EL, %VAL( PNTRI( 1 ) ),
     :                                 %VAL( PNTRO( 1 ) ), IERR, NERR,
     :                                 STATUS )

                     END IF
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
                        CALL KPG1_FLIPB( 1, EL, %VAL( PNTRI( 1 ) ),
     :                                   1, %VAL( PNTRO( 1 ) ), STATUS )
 
                     ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
                        CALL KPG1_FLIPUB( 1, EL, %VAL( PNTRI( 1 ) ),
     :                                   1, %VAL( PNTRO( 1 ) ), STATUS )
 
                     ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
                        CALL KPG1_FLIPD( 1, EL, %VAL( PNTRI( 1 ) ),
     :                                1, %VAL( PNTRO( 1 ) ), STATUS )
 
                     ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
                        CALL KPG1_FLIPI( 1, EL, %VAL( PNTRI( 1 ) ),
     :                                   1, %VAL( PNTRO( 1 ) ), STATUS )
 
                     ELSE IF ( TYPE .EQ. '_REAL' ) THEN
                        CALL KPG1_FLIPR( 1, EL, %VAL( PNTRI( 1 ) ),
     :                                   1, %VAL( PNTRO( 1 ) ), STATUS )
  
                     ELSE IF ( TYPE .EQ. '_WORD' ) THEN
                        CALL KPG1_FLIPW( 1, EL, %VAL( PNTRI( 1 ) ),
     :                                   1, %VAL( PNTRO( 1 ) ), STATUS )
 
                     ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
                        CALL KPG1_FLIPUW( 1, EL, %VAL( PNTRI( 1 ) ),
     :                                   1, %VAL( PNTRO( 1 ) ), STATUS )
                     END IF

                  ELSE

*  Call the appropriate routine to copy the axis array, depending on its
*  numeric type.  There will be no conversion errors so no need to check
*  the returned IERR and NERR.
                     IF ( TYPE .EQ. '_BYTE' ) THEN
                        CALL VEC_BTOB( .FALSE., EL, %VAL( PNTRI( 1 ) ),
     :                                 %VAL( PNTRO( 1 ) ), IERR, NERR,
     :                                 STATUS )


                     ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
                        CALL VEC_DTOD( .FALSE., EL, %VAL( PNTRI( 1 ) ),
     :                                 %VAL( PNTRO( 1 ) ), IERR, NERR,
     :                                 STATUS )

                     ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
                        CALL VEC_ITOI( .FALSE., EL, %VAL( PNTRI( 1 ) ),
     :                                 %VAL( PNTRO( 1 ) ), IERR, NERR,
     :                                 STATUS )

                     ELSE IF ( TYPE .EQ. '_REAL' ) THEN
                        CALL VEC_RTOR( .FALSE., EL, %VAL( PNTRI( 1 ) ),
     :                                 %VAL( PNTRO( 1 ) ), IERR, NERR,
     :                                 STATUS )

                     ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
                        CALL VEC_UBTOUB( .FALSE., EL,
     :                                   %VAL( PNTRI( 1 ) ),
     :                                   %VAL( PNTRO( 1 ) ), IERR, NERR,
     :                                   STATUS )

                     ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
                        CALL VEC_UWTOUW( .FALSE., EL,
     :                                   %VAL( PNTRI( 1 ) ),
     :                                   %VAL( PNTRO( 1 ) ), IERR, NERR,
     :                                   STATUS )

                     ELSE IF ( TYPE .EQ. '_WORD' ) THEN
                        CALL VEC_WTOW( .FALSE., EL, %VAL( PNTRI( 1 ) ),
     :                                 %VAL( PNTRO( 1 ) ), IERR, NERR,
     :                                 STATUS )

                     END IF
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

*  Transfer the labels.
            CALL NDF_ASTAT( NDFI, 'Label', SDIM( 1 ), THERE, STATUS )
            IF ( THERE ) THEN
               CALL NDF_ACGET( NDFI, 'Label', SDIM( 1 ), AXCOMP,
     :                         STATUS )
               CALL NDF_ACPUT( AXCOMP, NDFO, 'Label', SDIM( 2 ),
     :                         STATUS )
            END IF

            CALL NDF_ASTAT( NDFI, 'Label', SDIM( 2 ), THERE, STATUS )
            IF ( THERE ) THEN
               CALL NDF_ACGET( NDFI, 'Label', SDIM( 2 ), AXCOMP,
     :                         STATUS )
               CALL NDF_ACPUT( AXCOMP, NDFO, 'Label', SDIM( 1 ),
     :                         STATUS )
            END IF

*  Transfer the units.
            CALL NDF_ASTAT( NDFI, 'Units', SDIM( 1 ), THERE, STATUS )
            IF ( THERE ) THEN
               CALL NDF_ACGET( NDFI, 'Units', SDIM( 1 ), AXCOMP,
     :                         STATUS )
               CALL NDF_ACPUT( AXCOMP, NDFO, 'Units', SDIM( 2 ),
     :                         STATUS )
            END IF

            CALL NDF_ASTAT( NDFI, 'Units', SDIM( 2 ), THERE, STATUS )
            IF ( THERE ) THEN
               CALL NDF_ACGET( NDFI, 'Units', SDIM( 2 ), AXCOMP,
     :                         STATUS )
               CALL NDF_ACPUT( AXCOMP, NDFO, 'Units', SDIM( 1 ),
     :                         STATUS )
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
*  pixel coordinates. This mapping is described by a matrix and an offset
*  vector. Initialise the matrix to hold a unit matrix, and the offset
*  vector to be zero vector. The matrix is declared as a 1-d array because
*  the dimensionality of the output NDF is only known at run time.
*  Therefore we have to do the conversion from row and column numbers to
*  a 1-d vectorised index explicitly. Row I, column J of the matrix is
*  stored in element I + IDIM*( J - 1 ).
      DO I = 1, IDIM*IDIM
         MATRIX( I ) = 0.0D0
      END DO

      DO I = 1, IDIM
         MATRIX( I + IDIM*( I - 1 ) ) = 1.0D0
         OFFSET( I ) = 0.0D0
      END DO

*  Calculate the required cosine and sine values, and store them
*  in the matrix elements for the plane spanned by the significant
*  axes.
      COSANG = DBLE( COS( ANGLE * DTOR ) )
      SINANG = DBLE( SIN( ANGLE * DTOR ) )

      MATRIX( SDIM( 1 ) + IDIM*( SDIM( 1 ) - 1 ) ) = COSANG
      MATRIX( SDIM( 1 ) + IDIM*( SDIM( 2 ) - 1 ) ) = -SINANG
      MATRIX( SDIM( 2 ) + IDIM*( SDIM( 1 ) - 1 ) ) = SINANG
      MATRIX( SDIM( 2 ) + IDIM*( SDIM( 2 ) - 1 ) ) = COSANG

*  Calculate the pixel coordinates on the significant axes at the centre of 
*  the output image
      OXC = 0.5D0*DBLE( UBNDO( SDIM( 1 ) ) + LBNDO( SDIM( 1 ) ) - 1 )
      OYC = 0.5D0*DBLE( UBNDO( SDIM( 2 ) ) + LBNDO( SDIM( 2 ) ) - 1 )
      IXC = 0.5D0*DBLE( SUBNDI( 1 ) + SLBNDI( 1 ) - 1 )
      IYC = 0.5D0*DBLE( SUBNDI( 2 ) + SLBNDI( 2 ) - 1 )

*  Calculate the pixel offsets produced by the rotation on the significant
*  axes.
      OFFSET( SDIM( 1 ) ) = OXC - IXC*COSANG - IYC*SINANG
      OFFSET( SDIM( 2 ) ) = OYC + IXC*SINANG - IYC*COSANG

*  Propagate the WCS component.
      CALL KPG1_ASPRP( IDIM, NDFI, NDFO, MATRIX, OFFSET, STATUS )

  999 CONTINUE

*  Release the NDF resources.
      CALL NDF_END( STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ROTATE_ERR',
     :     'ROTATE: Error occurred whilst trying to rotate an NDF.',
     :     STATUS )
      END IF

      END
