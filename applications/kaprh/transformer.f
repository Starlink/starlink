      SUBROUTINE TRANSFORMER( STATUS )
*+
*  Name:
*     TRANSFORMER

*  Purpose:
*     Applies a transformation to an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL TRANSFORMER( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application performs an arbitrary transformation on an NDF to
*     create an output NDF.  There is full control of the shape, origin,
*     and co-ordinate limits of the output NDF.  The output NDF is
*     calculated by resampling within the input NDF.  Output array
*     elements are set to the bad value if their inverse-transformed
*     co-ordinates lie outside the input NDF's co-ordinate limits.

*  Usage:
*     transformer in transform out [method] [shape] { lcoord=? ucoord=?
*                                                   { lbound=? ubound=?
*                                                 shape

*  ADAM Parameters:
*     CONSERVE = _LOGICAL (Read)
*        If CONSERVE is TRUE, the output values are normalised by the
*        ratio of the output-to-input pixel areas.  In other words this
*        conserves flux.  If CONSERVE is FALSE, there is no
*        normalisation.  [FALSE]
*     COSYS = LITERAL (Read)
*        The co-ordinate system to be used.  This can be either "World"
*        or "Data".  "World" inputs pixel co-ordinates to the supplied
*        transformation to derive the co-ordinates in the output NDF.
*        "Data" causes the NDF's axis information to be the input
*        co-ordinates to the transformation.  See the SHAPE parameter.
*        [Current co-ordinate system]
*     FULL = _LOGICAL (Read)
*        When the number of input variables is less than the number
*        of dimensions of input NDF (but not less than the number of
*        output variables), FULL set to TRUE applies the transformation
*        to all the higher dimensions.  For example, FULL = TRUE
*        would apply a 2-dimensional transformation to all the planes
*        along the third dimension of a cube NDF.  FULL = FALSE would
*        only transformed the first plane.  [FALSE]
*     IN = NDF (Read)
*        The NDF to be transformed.
*     LBOUND() = _INTEGER (Read)
*        The lower pixel-index bounds of the output NDF.  The number of
*        values should equal the number of output variables in the
*        transformation.  This parameter is only used when SHAPE is
*        "Full" or "Bounds".  The suggested defaults are the lower
*        bounds of the input NDF, and where there are more output than
*        input dimensions they are set to 1.
*     LCOORD() = _DOUBLE (Read)
*        The lower co-ordinate limits of the output NDF.  The number of
*        values should equal the number of output variables in the
*        transformation.  This parameter is only used when SHAPE is
*        "Full" or "Limits".  The suggested defaults are the lower
*        co-ordinate limits determined from applying the transformation
*        to a series of test points.  Where there are more output than
*        input dimensions they are set to 0.0.
*     METHOD = LITERAL (Read)
*        The interpolation method used to resample the input array.
*        Permitted values are "Nearest" for nearest-neighbour, and
*        "Linint" for linear interpolation.  ["Nearest"]
*     OUT = NDF (Write)
*        The transformed NDF.
*     SHAPE = LITERAL (Read)
*        The method by which to define the bounds and co-ordinate limits
*        of the output NDF.  See Section "Co-ordinate Limits and
*        Bounds".  The options for SHAPE are:
*
*        -  "Bounds" -- Specify the output bounds with LBOUND and UBOUND.
*                       Use the default co-ordinate limits derived from
*                       the transformation of test points in the input
*                       NDF.
*
*        -  "Full"   -- Specify the output co-ordinate limits and bounds
*                       with LCOORD, UCOORD, LBOUND and UBOUND.
*
*        -  "Limits" -- Use the bounds of the input NDF and specify the
*                       output co-ordinate limits with LCOORD and UCOORD.
*
*        -  "Match"  -- Use the co-ordinate limits from transformed test
*                       points of the input NDF, and make a co-ordinate
*                       unit equivalent to one pixel.  The bounds are the
*                       integer-rounded co-ordinate limits.  This option
*                       results in an output NDF that is not clipped and
*                       unlike the other options guarantees no further
*                       linear compression or expansion.
*
*        -  "Same"   -- Use the bounds of the input NDF and take the
*                       co-ordinate limits from transformed test points
*                       of the input NDF.
*
*        The first three also cause the co-ordinate limits to be
*        reported before obtaining the limits and/or bounds.

*        Not all of these are permitted simultaneously.  "Same" is not
*        allowed when the number of input and output transformation
*        variables are not equal.  Otherwise it is the value of
*        COSYS that controls the options.  When COSYS = "Data" all but
*        "Match" are allowed, and COSYS = "World" excludes "Limits" and
*        "Full".  There is a special case where SHAPE is fixed to be
*        "Bounds".  This is when the number of output variables exceeds
*        the number of input variables, and that in turn equals the
*        number of dimensions in the input NDF.
*
*        If a null (!) value is supplied, a default value is used. When
*        COSYS = "Data" this is "Bounds", and when COSYS = "World" the
*        default is "Match".  [!]
*     TITLE = LITERAL (Read)
*        Title for the output NDF structure.  A null value (!)
*        propagates the title from the input NDF to the output NDF. [!]
*     TRANSFORM = TRN (Read)
*        The transformation to be applied.  This may be an HDS
*        container file, in which case the transformation structure is
*        assumed to be called TRANSFORM at the top level of the file;
*        or a path to the HDS object.  For example, a value of
*        distort.mapping would use the transformation structure called
*        MAPPING in the HDS file distort.sdf; and a value of aitoff
*        would make the routine look for the transformation in top-level
*        object TRANSFORM within the HDS file aifoff.sdf.  Normally the
*        object name is TRANSFORM.  The structure must contain both the
*        forward and inverse mappings.
*
*        Structures can be made using CCDEDIT in CCDPACK or TRANMAKE.
*     UBOUND() = _INTEGER (Read)
*        The upper pixel-index bounds of the output NDF.  The number of
*        values should equal the number of output variables in the
*        transformation.  This parameter is only used when SHAPE is
*        "Full" or "Bounds".  Each suggested-default value is the
*        maximum of the input upper bound and the output lower bound.
*     UCOORD() = _DOUBLE (Read)
*        The upper co-ordinate limits of the output NDF.  The number of
*        values should equal the number of output variables in the
*        transformation.  This parameter is only used when SHAPE is
*        "Full" or "Limits".  Each suggested-default value is the
*        upper co-ordinate limit determined from applying the
*        transformation to a series of test points.

*  Examples:
*     transformer curved sdist straight
*        This transforms the NDF called curved into an NDF called
*        straight.sdf, using the transformation TRANSFORM in the HDS
*        file called sdist.sdf.  It uses nearest-neighbour resampling.
*        Assuming the current co-ordinate system is world, the
*        transformation is performed in pixel co-ordinates, setting the
*        bounds to just enclose the transformed input array.
*     transformer curved sdist.transform straight linint same
*        As above, except linear interpolation is used, and the array
*        of NDF straight array uses the bounds of NDF curved.
*     transformer a119 proj.merc a119s shape=bounds lbound=[1,-20]
*     ubound=[256,172]
*        This transforms the NDF called a119, using the transformation
*        MERC in the HDS file called proj.sdf, into an NDF called
*        a119s.  It uses nearest-neighbour resampling.  a119s just
*        encloses the transformed arrays from a119, and has 256 x 192
*        pixels from origin (1,-20).
*     transformer spec2d scrunch.trn full method=l out=spec2d_l
*     shape=limits lcoord=5000 ucoord=6500
*        This transforms the 2-dimensional NDF called spec2d, using
*        the 1-dimensional transformation TRN in the HDS file called
*        scrunch.sdf, into an NDF called spec2d_l.  (NDF spec2d might
*        be a set of spectra before scrunching.)  The
*        linear-interpolation resampling is applied to all the lines in
*        spec2d_l.  The NDFs have the same pixel-index bounds.
*        spec2d_l is constrained to contain elements whose transformed
*        co-ordinates lie between 5000 to 6500.

*  Notes:
*     -  In general the test points to calculate the co-ordinate limits
*     for LCOORD and UCOORD are situated at the corners of each pixel,
*     assuming spaced axes.  Thus for a 2-dimensional array of 9-by-7
*     pixels there are 80 (10*8) test points.  For linear
*     transformations there is a smaller set of test points for
*     improved efficiency.  These are the corners of each axis and the
*     midpoints between them.
*     -  On completion, the current transformation global parameter
*     takes the value of parameter TRANSFORM.

*  Co-ordinate Limits and Bounds:
*     The limits are the lower co-ordinates of the first element, and
*     the upper co-ordinates of the last element of the NDF.  Using
*     these limits, TRANSFORMER derives the co-ordinates of the output
*     NDF's pixel centres by linear interpolation.  Therefore, the
*     co-ordinate limits define the region of the input NDF that will
*     appear in the output.

*     The bounds of the output NDF define its shape and origin.  So
*     an additional linear scaling transformation can be applied along
*     each axis by adjusting the shape independently of the co-ordinate
*     limits.

*  Implementation Status:
*     -  Flux conservation can only be applied to constant-determinant
*     or linear transformations.
*     -  The NDF components are processed by this application as
*     follows.
*        o  LABEL, UNITS, HISTORY, and extensions are propagated.
*        o  TITLE is controlled by the TITLE parameter.
*        o  QUALITY is not derived from the input NDF for a linearly
*        interpolated NDF.  The DATA and VARIANCE arrays are resampled.
*        o  Axis centre arrays are created using the co-ordinate
*        limits for COSYS = "Data".
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.
*     -  There can be an arbitrary number of NDF dimensions.

*  Related Applications:
*     KAPRH: FLIP, ROTATE, SLIDE, TRANINVERT, TRANJOIN, TRANMAKE,
*     TRANTRACE; CCDPACK: CCDEDIT, TRANLIST, TRANNDF.

*  Implementation Deficiencies:
*     Both mappings are required in all cases.
*     [routine_deficiencies]...

*  [optional_A_task_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1993 March 16 (MJC):
*        Original version.
*     1995 August 17 (MJC):
*        Used PSX for workspace.  Introduced an improved determination
*        of the extreme co-ordinates using the pixel vertices as test
*        points.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants
      INCLUDE 'MSG_PAR'          ! MSG constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
      INCLUDE 'TRN_PAR'          ! TRANSFORM constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NACOMP             ! Number NDF array components
      PARAMETER ( NACOMP = 3 )

*  Local Variables:
      INTEGER ABOUND             ! Work variable in MATCH bounds
      INTEGER ADIMS( NDF__MXDIM ) ! Number of elements in axis to be
                                 ! pasted
      INTEGER AEL( NDF__MXDIM )  ! Axis-array dimensions
      DOUBLE PRECISION AEND( NDF__MXDIM ) ! End co-ord of each axis
      DOUBLE PRECISION ASTART( NDF__MXDIM ) ! Start co-ord of each axis
      CHARACTER * ( DAT__SZTYP ) ATYPE ! Axis system data type
      INTEGER AXOFFS( NDF__MXDIM ) ! Offset in concatenated axis vector
      INTEGER AXPNTR( 2, NDF__MXDIM ) ! Pointers to the axis centre and
                                 ! width arrays
      LOGICAL BAD                ! If true, test for bad values
      INTEGER BMAX( NDF__MXDIM ) ! Maximum upper bounds of output NDF
      INTEGER CADIMS( NDF__MXDIM ) ! Number of elements in concatenated-
                                 ! axis vector
      INTEGER CAXPTR             ! Pointer to the concatenated axes
      LOGICAL CLASS( TRN__MXCLS ) ! Transformation classifications
      DOUBLE PRECISION COIN( 0:NDF__MXDIM, NDF__MXDIM ) ! Input
                                 ! co-ordinates for determinant finding
      CHARACTER * ( 5 ) COMP( NACOMP ) ! NDF array component names
      LOGICAL CONSRV             ! If true, the flux will be altered
      DOUBLE PRECISION COOUT( 0:NDF__MXDIM, NDF__MXDIM ) ! Output
                                 ! co-ordinates for determinant finding
      CHARACTER * ( 5 ) COSYS    ! Co-ordinate system to use
      INTEGER CTRID              ! Identifier for the compound
                                 ! transformation
      LOGICAL DACOOR             ! Input NDF has an axis system
      LOGICAL DATACO             ! If true, use data co-ordinates
      DOUBLE PRECISION DDLBND( NDF__MXDIM ) ! Data co-ordinate lower
                                 ! bounds of output NDF
      DOUBLE PRECISION DDUBND( NDF__MXDIM ) ! Data co-ordinate upper
                                 ! bounds of output NDF
      REAL DLBND( NDF__MXDIM )   ! Data co-ordinate lower
                                 ! bounds of output NDF
      CHARACTER * ( DAT__SZTYP ) DTYPE ! NDF output array type
      REAL DUBND( NDF__MXDIM )   ! Data co-ordinate upper
                                 ! bounds of output NDF
      LOGICAL DPAXIS             ! If true, the axis system is d.p.
      CHARACTER * ( 60 ) DTOW( NDF__MXDIM ) ! Expressions for transform-
                                 ! ing output data co-ordinates to world
      DOUBLE PRECISION DWLBND( NDF__MXDIM ) ! World co-ordinate lower
                                 ! bounds of output NDF
      DOUBLE PRECISION DWUBND( NDF__MXDIM ) ! World co-ordinate upper
                                 ! bounds of output NDF
      INTEGER EL                 ! Number of elements in higher dims
      INTEGER ELIN               ! Number of elements in input array
      INTEGER ELOUT              ! Number of elements in output array
      INTEGER EXTDIM             ! Number of input dimensions more than
                                 ! the number of input variables
      CHARACTER * ( 256 ) FILNAM ! HDS file name containg transformation
      DOUBLE PRECISION FLUX      ! Flux conservation factor
      LOGICAL FULL               ! If true, apply the transformation to
                                 ! all higher dimensions
      INTEGER I                  ! Loop counter
      INTEGER IAXIS              ! Loop counter through the axes
      INTEGER ICOMP              ! Loop counter through array components
      INTEGER IDIMS( NDF__MXDIM ) ! Dimensions of the input NDF
      INTEGER ILBND( NDF__MXDIM ) ! Lower bounds of the input NDF
      INTEGER INPNTR             ! Pointer to nearest-neighbour list
      INTEGER IPNTR( 2 )         ! Pointers to the input arrays
      CHARACTER * ( DAT__SZTYP ) ITYPE ! NDF array implementation type
      INTEGER IUBND( NDF__MXDIM ) ! Upper bounds of the input NDF
      INTEGER J                  ! Work variable
      DOUBLE PRECISION JACOB( NDF__MXDIM, NDF__MXDIM ) ! Jacobian matrix
      INTEGER K                  ! Loop counter
      CHARACTER * ( DAT__SZLOC ) LOC ! Locator to the TRANSFORM object
      CHARACTER * ( DAT__SZLOC ) LOCTR ! Locator to the transformation
      CHARACTER * ( DAT__SZLOC ) LOCTRF ! Locator to the compound
                                 ! transformation
      INTEGER NBAD               ! Number of bad elements in output
                                 ! array
      INTEGER NDFI               ! NDF identifier of input NDF
      INTEGER NDFIS              ! NDF identifier of input NDF section
      INTEGER NDFO               ! NDF identifier of output NDF
      INTEGER NDFOS              ! NDF identifier of output NDF section
      INTEGER NDIMI              ! Number of dimensions in input NDF
      INTEGER NDIMO              ! Number of dimensions in output NDF
      INTEGER NLEV               ! Number of levels in the path to the
                                 ! transformation structure
      LOGICAL NORM               ! Conserve flux if true
      INTEGER NSECT              ! Number of NDF sections to transform
      INTEGER NVIN               ! Number of input variables in the
                                 ! transformation
      INTEGER NVOUT              ! Number of output variables in the
                                 ! transformation
      CHARACTER * ( 7 ) METHOD   ! Resampling method
      INTEGER ODIMS( NDF__MXDIM ) ! Dimensions of the output NDF
      DOUBLE PRECISION OFFSET    ! Offset of data to world co-ordinates
                                 ! for output NDF
      INTEGER OLBND( NDF__MXDIM ) ! Lower bounds of the output NDF
      INTEGER OPNTRW             ! Pointer to the output array or
                                 ! workspace for flux conservation
      INTEGER OPNTR( 2 )         ! Pointers to the output arrays
      INTEGER OUBND( NDF__MXDIM ) ! Upper bounds of the output NDF
      CHARACTER * ( 132 ) PATH   ! Path of the transformation structure
      LOGICAL SAME               ! NDFs have same dimensions
      DOUBLE PRECISION SCALE     ! Scale of data to world co-ordinates
                                 ! for output NDF
      INTEGER SHADEF( NDF__MXDIM ) ! Suggested default output NDF shape
                                 ! upper bound
      CHARACTER * ( 6 ) SHAPE    ! How to specify shape of output NDF
      INTEGER STRIDE( NDF__MXDIM ) ! Strides between dimensions greater
                                 ! than the number of input variables
      LOGICAL THERE              ! If true, the array component is
                                 ! present in the NDF or the TRANSFORM
                                 ! structure in the transformation file
      INTEGER TRIDF              ! Identifier to the forward input
                                 ! transformation
      INTEGER TRIDI              ! Identifier to the inverse input
                                 ! transformation
      LOGICAL VAR                ! Variance array is present in NDF
      BYTE VARB                  ! Dummy input variance
      BYTE VARBO                 ! Dummy output variance
      INTEGER VARI               ! Dummy input variance
      INTEGER VARIO              ! Dummy output variance
      DOUBLE PRECISION VARD      ! Dummy input variance
      DOUBLE PRECISION VARDO     ! Dummy output variance
      REAL VARR                  ! Dummy input variance
      REAL VARRO                 ! Dummy output variance
      INTEGER * 2 VARW           ! Dummy input variance
      INTEGER * 2 VARWO          ! Dummy output variance
      INTEGER WDIM               ! Workspace dimension
      REAL WLBND( NDF__MXDIM )   ! World co-ordinate lower
                                 ! bounds of output NDF
      INTEGER WORK1( NDF__MXDIM ) ! Workspace for determinant finding
      DOUBLE PRECISION WORK2( NDF__MXDIM ) ! Workspace for determinant
      INTEGER WPNTR1             ! Pointer to co-ordinate workspace
      INTEGER WPNTR2             ! Pointer to indices workspace
      INTEGER WPNTR3             ! Pointer to co-ordinate workspace
      CHARACTER * ( 60 ) WTOD( NDF__MXDIM ) ! Expression for transform-
                                 ! ing output world co-ordinates to data
      REAL WUBND( NDF__MXDIM )   ! World co-ordinate upper
                                 ! bounds of output NDF

*  Local Data:
      DATA COMP / 'Data', 'Variance', 'Quality' /


*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the qualifications to the transformation.
*  =============================================

*  Get the method for calculating the output array value from the
*  input values.
      CALL PAR_CHOIC( 'METHOD', 'NEAREST', 'NEAREST,LININT', .FALSE.,
     :                METHOD, STATUS )

*  Are the output values to be normalised by the output-to-input pixel
*  areas, i.e. flux conservation; or just take the values from the
*  input NDF?
      CALL PAR_GET0L( 'CONSERVE', NORM, STATUS )

*  Obtain the desired co-ordinate system.
      CALL PAR_CHOIC( 'COSYS', 'Data', 'Data,World', .FALSE., COSYS,
     :                STATUS )

*  Get and validate the transformation file.
*  =========================================

*  Obtain the TRN transform file.
      CALL DAT_ASSOC( 'TRANSFORM', 'READ', LOC, STATUS )

*  Look to see if this the top level or to a structure within the file.
*  Get the path of the object and the number of nodes within it.
      CALL HDS_TRACE( LOC, NLEV, PATH, FILNAM, STATUS )

*  There is only one level.
      IF ( NLEV .EQ. 1 .AND. STATUS .EQ. SAI__OK ) THEN

*  Check that there is a TRANSFORM structure.
         CALL DAT_THERE( LOC, 'TRANSFORM', THERE, STATUS )

*  Report an error if the TRANSFORM structure is absent.
         IF ( .NOT. THERE ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'NOTRANSFORM',
     :        'The container file does not contain a TRANSFORM '/
     :        /'structure at the top level.', STATUS )
            GOTO 980
         ELSE

*  Obtain a locator to the TRANSFORM structure.
            CALL DAT_FIND( LOC, 'TRANSFORM', LOCTR, STATUS )

*  Annul the original locator.
            CALL DAT_ANNUL( LOC, STATUS )
         END IF
      ELSE

*  The original locator was to the transformation structure, so assign
*  its locator to the transformation locator.
         LOCTR = LOC
      END IF

*  Validate the transformation.  Need forward and backward
*  transformations.
      CALL TRN_COMP( LOCTR, .TRUE., TRIDF, STATUS )
      CALL TRN_COMP( LOCTR, .FALSE., TRIDI, STATUS )

*  Obtain the number of variables in the transformation.
      CALL TRN_GTNV( LOCTR, NVIN, NVOUT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 980

*  Obtain the method to define the bounds of the output NDF.
*  =========================================================

*  This encompasses both the lower and upper bounds in pixels, but also
*  the limits in the output co-ordinates corresponding to those limits
*  (so that clipping can be provided).  When the transformation changes
*  dimension, the output NDF cannot have the same shape.
      IF ( NVIN .EQ. NVOUT ) THEN
         IF ( COSYS .EQ. 'DATA' ) THEN
            CALL PAR_CHOIC( 'SHAPE', 'Bounds', 'Bounds,Full,Limits,'/
     :                      /'Same', .FALSE., SHAPE, STATUS )
         ELSE
            CALL PAR_CHOIC( 'SHAPE', 'Match', 'Bounds,Match,Same',
     :                      .FALSE., SHAPE, STATUS )
         END IF
         SAME = SHAPE .EQ. 'SAME'

      ELSE
         IF ( COSYS .EQ. 'DATA' ) THEN
            CALL PAR_CHOIC( 'SHAPE', 'Bounds', 'Bounds,Full,Limits',
     :                      .TRUE., SHAPE, STATUS )

*  When the number of dimensions increases but the NDF has more
*  dimensions than the input NDF it is hard to know what to do.
*  Therefore take a simple approach of not permitting the MATCH option.
         ELSE IF ( NVOUT .GT. NVIN .AND. NVIN .EQ. NDIMI ) THEN
            SHAPE = 'BOUNDS'

         ELSE
            CALL PAR_CHOIC( 'SHAPE', 'Match', 'Bounds,Match',
     :                      .TRUE., SHAPE, STATUS )
         END IF
         SAME = .FALSE.
      END IF

*  Inquire the classification of the transformation.
*  =================================================
*
*  Certain efficiencies gains can be made for certain types of
*  transformation, mostly notably a linear transformation, where the
*  flux conservation factor is a constant.
      CALL TRN_GTCL( LOCTR, .TRUE., CLASS, STATUS )

*  Test if the transformation can be processed.
      IF ( .NOT. CLASS( TRN__LIN ) .AND. .NOT. CLASS( TRN__CONDT ) .AND.
     :     NORM ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'TRANSFORMER_ERR',
     :     'TRANSFORMER: The transformation $TRANSFORM is non-linear, '/
     :     /'or does not have a constant determinant, and cannot be '/
     :     /'handled with flux conservation by this preliminary '/
     :     /'version of TRANSFORMER.', STATUS )
         GOTO 980
      END IF

*  Obtain the NDF.
*  ===============
*
*  Start a new NDF context.
      CALL NDF_BEGIN

*  Open the input NDF.
      CALL LPG_ASSOC( 'IN', 'READ', NDFI, STATUS )

*  Obtain the dimensions of the NDF.
      CALL NDF_DIM( NDFI, NDF__MXDIM, IDIMS, NDIMI, STATUS )

*  Obtain the bounds of the NDF.
      CALL NDF_BOUND( NDFI, NDF__MXDIM, ILBND, IUBND, NDIMI, STATUS )

*  Is there an axis system?
      CALL NDF_STATE( NDFI, 'Axis', DACOOR, STATUS )

*  Find the effective co-ordinate system.
      DATACO = DACOOR .AND. COSYS .EQ. 'DATA'

      IF ( STATUS .NE. SAI__OK ) GOTO 960

*  Validate dimensions and number of input co-ordinates.
*  =====================================================
*
*  Check that processing is possible.  The number of dimensions in the
*  NDF must be at least the number of input variables for the
*  transformation to be applied.
      IF ( NVIN .GT. NDIMI .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', NDFI )
         CALL MSG_SETI( 'NVIN', NVIN )
         CALL MSG_SETI( 'NDIMI', NDIMI )
         CALL ERR_REP( 'TRANSFORM_MISMATCH',
     :     'TRANSFORM: There is a mismatch between the transformation '/
     :     /'file $TRANSFORM, which expects ^NVIN variables, and the '/
     :     /'NDF ^NDF, which has only ^NDIMI dimensions.', STATUS )
         GOTO 960

      ELSE IF ( NVIN .LT. NDIMI .AND. NVIN .GE. NVOUT ) THEN

*  Check that the transformation is to be applied to all higher
*  dimensions.
         CALL PAR_GET0L( 'FULL', FULL, STATUS )

      ELSE
         FULL = .FALSE.
      END IF

*  Define the input co-ordinate system.
*  ====================================

*  Get the type of the axis system.
      CALL KPG1_AXTYP( NDFI, 'Centre', ATYPE, STATUS )

*  Force the array type to be floating-point.
      IF ( ATYPE .NE. '_DOUBLE' ) ATYPE = '_REAL'

*  See if the axis co-ordinates are to be used, and if so whether or not
*  double-precision processing is required.
      IF ( DATACO ) THEN
         DPAXIS = ATYPE .EQ. '_DOUBLE'

*  Map the axis centre and width arrays.  Use double precision as this
*  precision is needed to determine the range of each axis.
         DO IAXIS = 1, NVIN
            CALL NDF_AMAP( NDFI, 'Centre,Width', IAXIS, '_DOUBLE',
     :                     'READ', AXPNTR( 1, IAXIS ), AEL( IAXIS ),
     :                     STATUS )
         END DO

*  Although there are no data co-ordinates an array of concatenated
*  co-ordinates of the input array is needed when applying the
*  transformation, so just create a vector of pixel co-ordinates.
      ELSE
         DPAXIS = .FALSE.
         DO IAXIS = 1, NVIN
            AEL( IAXIS ) = IDIMS( IAXIS )
            CALL PSX_CALLOC( AEL( IAXIS ), '_REAL', AXPNTR( 1, IAXIS ),
     :                       STATUS )
            CALL KPG1_SSAZR( AEL( IAXIS ), 1.0D0,
     :                       DBLE( ILBND( IAXIS ) ) - 0.5D0,
     :                       %VAL( AXPNTR( 1, IAXIS ) ), STATUS )
         END DO
      END IF

*  Find the co-ordinate bounds of the output NDF.
*  ==============================================

*  The transformed co-ordinates will not, in general, be in pixel
*  co-ordinates, so we shall need to derive and then join a
*  transformation that converts between the output co-ordinates and
*  pixel co-ordinates.  The first stage is to estimate the bounds.

*  First when data co-ordinates are to be input.
      IF ( DATACO ) THEN
         DO IAXIS = 1, NVIN

*  Use the axis centre and width arrays to determine the overall extent
*  of the NDF along the current axis.
            CALL KPG1_AXRNG( AEL( IAXIS ), %VAL( AXPNTR( 1, IAXIS ) ),
     :                       %VAL( AXPNTR( 2, IAXIS ) ),
     :                       ASTART( IAXIS ), AEND( IAXIS ), STATUS )

         END DO
      ELSE

*  Use the pixel co-ordinates as the world co-ordinate system has been
*  selected directly (or indirectly when the NDF does not have an AXIS
*  component).
         DO IAXIS = 1, NVIN
            ASTART( IAXIS ) = DBLE( ILBND( IAXIS ) )
            AEND( IAXIS ) = DBLE( IUBND( IAXIS ) )
         END DO
      END IF

      IF ( CLASS( TRN__LIN ) ) THEN

*  Use test points at the extremes and midpoints of each axis to obtain
*  an estimate of the extent of the output NDF's co-ordinates.  This
*  assumes that the transformation does not move the innards of the
*  input array to the outside of the output array, which is fine for
*  a linear transformation.
         CALL KPG1_TRBOD( NVIN, ASTART, AEND, TRIDF, NVOUT, DDLBND,
     :                    DDUBND, STATUS )

      ELSE

*  Obtain workspace for the co-ordinate transformations.
         WDIM = IDIMS( 1 ) + 1
         CALL PSX_CALLOC( WDIM * NDIMI, '_DOUBLE', WPNTR1, STATUS )
         CALL PSX_CALLOC( WDIM * NDIMI, '_DOUBLE', WPNTR2, STATUS )

*  In the general case use equally spaced co-ordinates numbering the
*  dimension plus one along each axis to obtain a good estimate of the
*  extent of the output NDF's co-ordinates.  For spaced data
*  co-ordinates or pixel co-ordinates, the test points are located at
*  the pixel vertices.
         CALL KPG1_TRALD( NVIN, IDIMS, ASTART, AEND, TRIDF, WDIM,
     :                    NVOUT, %VAL( WPNTR1 ), %VAL( WPNTR2 ),
     :                    DDLBND, DDUBND, STATUS )

*  Release the workspace.
         CALL PSX_FREE( WPNTR1, STATUS )
         CALL PSX_FREE( WPNTR2, STATUS )
      END IF

*  Make real copies for single-precision axes, and they are also needed
*  to get the bounds in the SHAPE=MATCH option.
      IF ( .NOT. DPAXIS .OR. SHAPE .EQ. 'MATCH' ) THEN
         DO IAXIS = 1, NVOUT
            DLBND( IAXIS ) = REAL( DDLBND( IAXIS ) )
            DUBND( IAXIS ) = REAL( DDUBND( IAXIS ) )
         END DO
      END IF

      IF ( STATUS .NE. SAI__OK ) GOTO 960

*  Find the co-ordinate limits of the output NDF.
*  ==============================================

*  This only needs to be done if the shape of the output NDF is unknown
*  at this point.
      IF ( .NOT. ( SAME .OR. SHAPE .EQ. 'MATCH' ) ) THEN

*  Report the extents using an appropriate precision.
         DO IAXIS = 1, NVOUT
            CALL MSG_SETI( 'AXIS', IAXIS )
            IF ( DPAXIS ) THEN
               CALL MSG_SETD( 'LOW', DDLBND( IAXIS ) )
               CALL MSG_SETD( 'HIGH', DDUBND( IAXIS ) )
            ELSE
               CALL MSG_SETR( 'LOW', DLBND( IAXIS ) )
               CALL MSG_SETR( 'HIGH', DUBND( IAXIS ) )
            END IF

            IF ( IAXIS .EQ. 1 ) THEN
               CALL MSG_OUTIF( MSG__NORM, 'OUTBOUNDS',
     :           'Co-ordinates limits for axis ^AXIS: ^LOW to ^HIGH.',
     :           STATUS )
            ELSE
               CALL MSG_OUTIF( MSG__NORM, 'OUTBOUNDS',
     :           '                             ^AXIS: ^LOW to ^HIGH.',
     :           STATUS )
            END IF
         END DO

*  The output array may be clipped for certain shape categories.
*  Specify the range of data co-ordinates to appear in the output NDF.
         IF ( SHAPE .EQ. 'LIMITS' .OR. SHAPE .EQ. 'FULL' ) THEN

*  Find the co-ordinate limits of the output array.  Set the default
*  limits to the co-ordinates believed to encompass the entire output
*  array.
            CALL PAR_GDR1D( 'LCOORD', NVOUT, DDLBND, -VAL__MAXD,
     :                      VAL__MAXD, .FALSE., DDLBND, STATUS )

            CALL PAR_GDR1D( 'UCOORD', NVOUT, DDUBND, -VAL__MAXD,
     :                      VAL__MAXD, .FALSE., DDUBND, STATUS )

*  Make real copies of the revised co-ordinate limits for single-
*  precision axes.
            IF ( .NOT. DPAXIS ) THEN
               DO IAXIS = 1, NVOUT
                  DLBND( IAXIS ) = REAL( DDLBND( IAXIS ) )
                  DUBND( IAXIS ) = REAL( DDUBND( IAXIS ) )
               END DO
            END IF
         END IF

*  Set the bounds of the output NDF.
*  =================================
         IF ( SHAPE .EQ. 'LIMITS' ) THEN

*  The bounds of the input NDF are used.
            DO IAXIS = 1, NVOUT
               OLBND( IAXIS ) = ILBND( IAXIS )
               OUBND( IAXIS ) = IUBND( IAXIS )
               ODIMS( IAXIS ) = IDIMS( IAXIS )
            END DO

         ELSE

*  Find the dimensions and bounds of the output array when SHAPE is FULL
*  or BOUNDS.  Set the default bounds to be that of the input NDF.
*  Note that when there are more output variables than input
*  dimensions, any higher dimensions have been set to 1.  The upper
*  bounds are further constrained to be at least the value of the
*  output NDF's lower bounds.
            CALL PAR_GDR1I( 'LBOUND', NVOUT, ILBND, -VAL__MAXI,
     :                      VAL__MAXI, .FALSE., OLBND, STATUS )
            DO I = 1, NVOUT
               SHADEF( I ) = MAX( OLBND( I ), IUBND( I ) )
               BMAX( I ) = VAL__MAXI
            END DO
            CALL PAR_GRM1I( 'UBOUND', NVOUT, SHADEF, OLBND, BMAX,
     :                      .FALSE., OUBND, STATUS )

*  Derive the output NDF's dimensions.
            DO IAXIS = 1, NVOUT
               ODIMS( IAXIS ) = OUBND( IAXIS ) - OLBND( IAXIS ) + 1
            END DO
         END IF

*  Make the output NDF have the same bounds as the input NDF.
      ELSE IF ( SAME ) THEN
         DO IAXIS = 1, NVOUT
            OLBND( IAXIS ) = ILBND( IAXIS )
            OUBND( IAXIS ) = IUBND( IAXIS )
            ODIMS( IAXIS ) = IDIMS( IAXIS )
         END DO

*  Make the output NDF have the bounds as the limiting points of the
*  input NDF after the forward mapping.  Note we round outwards so as
*  not to clip the transformed array.  Note this does not use the
*  double-precision bounds as they are derived when using the data
*  co-ordinate system.  Also should the point be located on a pixel
*  centre there is no need to expand the bounds (allow for some rounding
*  errors hence the test against 3 times the machine precision).
      ELSE IF ( SHAPE .EQ. 'MATCH' ) THEN
         DO IAXIS = 1, NVOUT
            ABOUND = INT( DLBND( IAXIS ) )
            IF ( ABS( REAL( ABOUND ) - DLBND( IAXIS ) ) .LT.
     :           3.0 * VAL__EPSR * ABS( DLBND( IAXIS ) ) ) THEN
               OLBND( IAXIS ) = ABOUND
            ELSE IF ( DLBND( IAXIS ) .LT. 0.0 ) THEN
               OLBND( IAXIS ) = ABOUND - 1
            ELSE
               OLBND( IAXIS ) = ABOUND
            END IF

            ABOUND = INT( DUBND( IAXIS ) )
            IF ( ABS( REAL( ABOUND ) - DUBND( IAXIS ) ) .LT.
     :           3.0 * VAL__EPSR * ABS( DUBND( IAXIS ) ) ) THEN
               OUBND( IAXIS ) = ABOUND
            ELSE IF ( DUBND( IAXIS ) .LT. 0.0 ) THEN
               OUBND( IAXIS ) = ABOUND
            ELSE
               OUBND( IAXIS ) = ABOUND + 1
            END IF

            ODIMS( IAXIS ) = OUBND( IAXIS ) - OLBND( IAXIS ) + 1
         END DO
      END IF

      IF ( STATUS .NE. SAI__OK ) GOTO 960

*  Calculate the number of NDF sections and strides between them.
*  ==============================================================

*  Initialise the number of NDF sections to be transformed.
      NSECT = 1

      IF ( FULL ) THEN

*  Find the number of dimensions greater than the number of dimensions.
         EXTDIM = NDIMI - NVIN

*  Padout the dimensions of the output NDF if the transformation is to
*  be applied to a series of higher dimensions.  Also determine the
*  number of NDF sections to be transformed, and the strides between
*  the higher dimensions.
         STRIDE( NVOUT ) = 1
         DO I = 1, EXTDIM
            J = I + NVOUT
            ODIMS( J ) = IDIMS( I + NVIN )
            NSECT = NSECT * IDIMS( I + NVIN )
            STRIDE( J ) = STRIDE( J - 1 ) * ODIMS( J )
         END DO

*  Assign the number of dimensions in the output array.
         NDIMO = NVOUT + EXTDIM
      ELSE
         NDIMO = NVOUT
      END IF

*  Map single-precision axis centres.
*  ==================================

*  A single-precision axis array of data co-ordinates was mapped
*  with type _DOUBLE.  This was because KPG1_AXRNG is not generic and
*  used double-precision axis centres and widths.  Subsequent routines
*  are generic, so we need to unmap the axis arrays, and remap the
*  axis centres as single precision where appropriate.
      IF ( DATACO .AND. .NOT. DPAXIS ) THEN
         CALL NDF_AUNMP( NDFI, 'Centre,Width', 0, STATUS )

*  Map the axis centre array.  Use single precision.
         DO IAXIS = 1, NVIN
            CALL NDF_AMAP( NDFI, 'Centre', IAXIS, '_REAL', 'READ',
     :                     AXPNTR( 1, IAXIS ), AEL( IAXIS ), STATUS )
         END DO
      END IF

      IF ( STATUS .NE. SAI__OK ) GOTO 960

*  Create a concatenated input axis array.
*  =======================================
*
*  The subroutines that perform the resampling need the axis arrays
*  to convert co-ordinates into pixel indices in the input array.
*  For convenience the axis centres are passed in a single vector
*  and required values are found using offsets equal to the sum of
*  the lower axis dimensions.
*
*  Find the length of the concatenated axes.
      CADIMS( 1 ) = 0
      DO IAXIS = 1, NVIN
         CADIMS( 1 ) = CADIMS( 1 ) + AEL( IAXIS )
      END DO

*  Create some workspace for the concatenated array.
      CALL PSX_CALLOC( CADIMS( 1 ), ATYPE, CAXPTR, STATUS )

*  Pasting routine needs filled dimension arrays of NDF__MXDIM values.
      AXOFFS( 1 ) = 0
      DO IAXIS = 2, NDF__MXDIM
         AXOFFS( IAXIS ) = 0
         ADIMS( IAXIS ) = 1
         CADIMS( IAXIS ) = 1
      END DO

      DO IAXIS = 1, NVIN

*  Assign the axis dimension.
         ADIMS( 1 ) = AEL( IAXIS )

*  Paste each axis into the work array.
         IF ( DPAXIS ) THEN
            CALL KPG1_PASTD( .FALSE., .TRUE., AXOFFS, ADIMS, ADIMS( 1 ),
     :                       %VAL( AXPNTR( 1, IAXIS ) ), CADIMS,
     :                       CADIMS( 1 ), %VAL( CAXPTR ), STATUS )
         ELSE
            CALL KPG1_PASTR( .FALSE., .TRUE., AXOFFS, ADIMS, ADIMS( 1 ),
     :                       %VAL( AXPNTR( 1, IAXIS ) ), CADIMS,
     :                       CADIMS( 1 ), %VAL( CAXPTR ), STATUS )
         END IF

*  Increment the offsets for the next axis.
         AXOFFS( 1 ) = AXOFFS( 1 ) + AEL( IAXIS )
      END DO

      IF ( STATUS .NE. SAI__OK ) GOTO 940

*  Tidy the axis centres.
*  ======================

*  Unmap the axis-centre and axis-width arrays.  Use a wildcard to cover
*  the cases where the width may be mapped.
      IF ( DATACO ) THEN
         CALL NDF_AUNMP( NDFI, '*', 0, STATUS )

*  Free the workspace used to create the arrays of pixel co-ordinates.
      ELSE
         DO IAXIS = 1, NVIN
            CALL PSX_FREE( AXPNTR( 1, IAXIS ), STATUS )
         END DO
      END IF


*  Make data <--> pixel transformation.
*  ====================================

*  This only applies to data co-ordinates.
      IF ( COSYS .EQ. 'DATA' ) THEN

*  Define a transformation for converting between co-ordinates as
*  produced by the input transformation and pixel co-ordinates, both in
*  the output array.

*  First define the bounds of the world co-ordinates.  Note that data
*  limits define the extents, not the centres of the first and last
*  elements.  Thus the world co-ordinates must match these.  This
*  is outside the DPAXIS test because the double-precision bounds of
*  the world co-ordinates are needed to fill the axis-centre arrays.
         DO I = 1, NVOUT
            DWLBND( I ) = DBLE( OLBND( I ) ) - 1.0D0
            DWUBND( I ) = DBLE( OUBND( I ) )
         END DO

         IF ( DPAXIS ) THEN

*  Substitute the transformation expressions.
            CALL KPG1_LITND( NVOUT, DDLBND, DDUBND, DWLBND, DWUBND,
     :                       DTOW, WTOD, STATUS )
         ELSE

*  Define the bounds of the world co-ordinates.  Note that data limits
*  define the extents, not the centres of the first and last elements.
*  thus the world co-ordinates must match these.
            DO I = 1, NVOUT
               WLBND( I ) = REAL( OLBND( I ) ) - 1.0
               WUBND( I ) = REAL( OUBND( I ) )
            END DO

*  Substitute the transformation expressions. N.B. single-precision
*  versions of bound to be made.
            CALL KPG1_LITNR( NVOUT, DLBND, DUBND, WLBND, WUBND, DTOW,
     :                       WTOD, STATUS )
         END IF

*  Create the transformation.
         IF ( DPAXIS ) THEN
            CALL TRN_NEW( NVOUT, NVOUT, DTOW, WTOD, '_DOUBLE:', ' ',
     :                    ' ', ' ', LOCTRF, STATUS )
         ELSE
            CALL TRN_NEW( NVOUT, NVOUT, DTOW, WTOD, '_REAL:', ' ', ' ',
     :                    ' ', LOCTRF, STATUS )
         END IF

*  Prefix the input transformation to the this one.
         CALL TRN_PRFX( LOCTR, LOCTRF, STATUS )

*  Compile the inverse transformation for efficiency.  Note it is the
*  inverse because output elements are transformed back into the input
*  array.
         CALL TRN_COMP( LOCTRF, .FALSE., CTRID, STATUS )

*  World co-ordinates are being used so the extra transformation is not
*  needed.  In order to use one identifier for the large number of
*  calls to perform the resampling, just copy the identifier of the
*  inverse transformation.
      ELSE
         CTRID = TRIDI
      END IF

*  Create the output NDF.
*  ======================
      CALL LPG_PROP( NDFI, 'Units', 'OUT', NDFO, STATUS )

*  Change its shape where required.
      IF ( .NOT. SAME ) THEN
         CALL NDF_SBND( NDIMO, OLBND, OUBND, NDFO, STATUS )
      END IF

*  Obtain a title and assign it to the output NDF.
*  ===============================================

*  Obtain the output title and insert it into the output NDF.  A null
*  results in the output title being the same as the input title.
      CALL KPG1_CCPRO( 'TITLE', 'TITLE', NDFI, NDFO, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 940

*  Write AXIS structure to the output NDF.
*  =======================================

*  Create a default axes in the output NDF.  Doing this conveniently
*  handles any higher dimensions not defined in the transformation,
*  even though may all be replaced by data co-ordinates.
      CALL NDF_ACRE( NDFO, STATUS )

*  Only need to change this when using data co-ordinates.
      IF ( COSYS .EQ. 'DATA' ) THEN

*  Loop for each axis.
         DO IAXIS = 1, NVOUT

*  Map the centre array.
            CALL NDF_AMAP( NDFO, 'Centre', IAXIS, ATYPE, 'WRITE',
     :                     AXPNTR( 1, IAXIS ), AEL( IAXIS ), STATUS )

*  Compute the scale and offset between pixel and data co-ordinates.
*  Note the axes centres run from the lower data co-ordinates plus
*  a half-pixel offset.
            SCALE = ( DDUBND( IAXIS ) - DDLBND( IAXIS ) ) /
     :              ( DWUBND( IAXIS ) - DWLBND( IAXIS ) )
            OFFSET = DDLBND( IAXIS ) + SCALE * 0.5D0

            IF ( DPAXIS ) THEN

*  Fill the centre array using the scale and offset.  Scale is applied
*  from the second element in this routine, rather than the centre of
*  the first.
               CALL KPG1_SSAZD( AEL( IAXIS ), SCALE, OFFSET,
     :                          %VAL( AXPNTR( 1, IAXIS ) ), STATUS )

            ELSE

*  Fill the centre array using the scale and offset.  Scale is applied
*  from the second element in this routine, rather than the centre of
*  the first.
               CALL KPG1_SSAZR( AEL( IAXIS ), SCALE, OFFSET,
     :                          %VAL( AXPNTR( 1, IAXIS ) ), STATUS )
            END IF

*  Unmap the axis-centre array.
            CALL NDF_AUNMP( NDFO, 'Centre', IAXIS, STATUS )
         END DO
      END IF

*  Find the flux-conservation factor.
*  ==================================

*  Find a constant determinant by transforming unit vectors along each
*  dimension.
      IF ( NORM .AND. NVIN .EQ. NVOUT .AND.
     :     ( CLASS( TRN__LIN ) .OR. CLASS( TRN__CONDT ) ) ) THEN

*  First set the input co-ordinates to all zero.
         DO IAXIS = 1, NVIN
            DO I = 0, NVIN
               COIN( I, IAXIS ) = 0.0D0
            END DO
         END DO

*  Revise the diagonal terms to one.
         DO I = 1, NVIN
            COIN( I, I ) = 1.0D0
         END DO
         EL = NVIN * NVIN + 1

*  Apply the transformation.
         CALL TRN_TRND( .FALSE., NDF__MXDIM + 1, NVIN, EL, COIN, TRIDF,
     :                  NDF__MXDIM + 1, NVOUT, COOUT, STATUS )

*  Assign the Jacobian matrix.
         DO IAXIS = 1, NVIN
            DO I = 1, NVIN
               JACOB( I, IAXIS ) = COOUT( I, IAXIS ) - COOUT( 0, IAXIS )
            END DO
         END DO

*  The flux term is the inverse of the absolute determinant of the
*  Jacobian.
         CALL KPG1_MDETD( NVIN, NDF__MXDIM, JACOB, WORK1, WORK2, FLUX,
     :                    STATUS )
         IF ( FLUX .GT. VAL__SMLD ) THEN
            FLUX = ABS( 1.0D0 / FLUX )
         ELSE
            FLUX = 1.0D0
         END IF

*      ELSE IF ( NORM ) THEN
      ELSE
         FLUX = 1.0D0
      END IF

*  See if there is any conservation of flux required.
      CONSRV = ABS( FLUX - 1.0D0 ) .GT. VAL__EPSD

      IF ( STATUS .NE. SAI__OK ) GOTO 940

*  Resample using the nearest-neighbour technique.
*  ===============================================
      IF ( METHOD .EQ. 'NEAREST' ) THEN

*  This method is applicable to the data, variance and quality arrays.
*  First determine the vector indices of the nearest neighbours applying
*  the transformation.  To do this we need some work space for the
*  input and output co-ordinates, and input fractional indices.
         WDIM = ODIMS( 1 )
         CALL PSX_CALLOC( WDIM * NDIMI, ATYPE, WPNTR1, STATUS )
         CALL PSX_CALLOC( WDIM * NDIMI, ATYPE, WPNTR2, STATUS )
         CALL PSX_CALLOC( WDIM * NDIMO, ATYPE, WPNTR3, STATUS )

*  Get workspace to hold the indices of the nearest neighbours. There
*  is one for each output array element.
         EL = 1
         DO I = 1, NVOUT
            EL = EL * ODIMS( I )
         END DO
         CALL PSX_CALLOC( EL, '_INTEGER', INPNTR, STATUS )

*  Generate the list of vector indices for the resampling.
         IF ( DPAXIS ) THEN
            CALL KPG1_TRPID( NDIMI, IDIMS, CTRID, %VAL( CAXPTR ),
     :                       WDIM, NVOUT, OLBND, ODIMS,
     :                       %VAL( WPNTR1 ), %VAL( WPNTR3 ),
     :                       %VAL( WPNTR2 ), %VAL( INPNTR ), STATUS )
         ELSE
            CALL KPG1_TRPIR( NDIMI, IDIMS, CTRID, %VAL( CAXPTR ),
     :                       WDIM, NVOUT, OLBND, ODIMS,
     :                       %VAL( WPNTR1 ), %VAL( WPNTR3 ),
     :                       %VAL( WPNTR2 ), %VAL( INPNTR ), STATUS )
         END IF

*  Free the workspace that is no longer needed.
         CALL PSX_FREE( WPNTR1, STATUS )
         CALL PSX_FREE( WPNTR2, STATUS )
         CALL PSX_FREE( WPNTR3, STATUS )

*  Apply the transformation with n-n resampling.
*  =============================================

*  Loop through all sections.  Normally there will be just one.
         DO I = 1, NSECT

*  Create a section to which to apply the transformation.  First set its
*  bounds to give a sequence of just one element along each axis greater
*  than the number of transformation variables.
            IF ( FULL ) THEN
               DO K = 1, EXTDIM
                  J = K + NVOUT
                  ILBND( K + NVIN ) = MOD( I - 1, STRIDE( J ) ) + 1
                  IUBND( K + NVIN ) = ILBND( K + NVIN )
                  OLBND( J ) = MOD( I - 1, STRIDE( J ) ) + 1
                  OUBND( J ) = OLBND( J )
               END DO

*  Create the sections.
               CALL NDF_SECT( NDFI, NDIMI, ILBND, IUBND, NDFIS, STATUS )
               CALL NDF_SECT( NDFO, NDIMO, OLBND, IUBND, NDFOS, STATUS )

*  There is only one section, so just clone the identifiers.
            ELSE
               CALL NDF_CLONE( NDFI, NDFIS, STATUS )
               CALL NDF_CLONE( NDFO, NDFOS, STATUS )
            END IF

*  Loop through all the components.
            DO ICOMP = 1, NACOMP

*  See if the component is present.
               IF ( COMP( ICOMP ) .NE. 'Data' ) THEN
                  CALL NDF_STATE( NDFI, COMP( ICOMP ), THERE, STATUS )
               ELSE
                  THERE = .TRUE.
               END IF

*  Can only process when the array component is present.
               IF ( THERE ) THEN

*  Get the type of the array.
                  CALL NDF_TYPE( NDFI, COMP( ICOMP ), ITYPE, STATUS )

*  Map the input and output arrays.
                  CALL KPG1_MAP( NDFIS, COMP( ICOMP ), ITYPE, 'READ',
     :                          IPNTR, ELIN, STATUS )
                  CALL KPG1_MAP( NDFOS, COMP( ICOMP ), ITYPE,
     :                          'WRITE/BAD', OPNTR, ELOUT, STATUS )

*  When there is flux conservation there is a two-stage process, so
*  workspace is needed to hold the uncorrected values.  Use the same
*  pointer for both cases to save code.  Flux conservation is not
*  required for the QUALITY array.
                  IF ( CONSRV .AND. COMP( ICOMP ) .NE. 'Quality' ) THEN
                     CALL PSX_CALLOC( ELOUT, ITYPE, OPNTRW, STATUS )
                  ELSE
                     OPNTRW = OPNTR( 1 )
                  END IF

*  Perform the transformation on the data array for the numeric data
*  type.  First for a byte array
                  IF ( ITYPE .EQ. '_BYTE' ) THEN
                     CALL KPG1_VASVB( ELOUT, %VAL( INPNTR ), ELIN,
     :                                %VAL( IPNTR( 1 ) ),
     :                                %VAL( OPNTRW ), NBAD, STATUS )

*  Transform a double-precision array.
                  ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                     CALL KPG1_VASVD( ELOUT, %VAL( INPNTR ), ELIN,
     :                                %VAL( IPNTR( 1 ) ),
     :                                %VAL( OPNTRW ), NBAD, STATUS )

*  Transform an integer array.
                  ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
                     CALL KPG1_VASVI( ELOUT, %VAL( INPNTR ), ELIN,
     :                                %VAL( IPNTR( 1 ) ),
     :                                %VAL( OPNTRW ), NBAD, STATUS )

*  Transform a single-precision array.
                  ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
                     CALL KPG1_VASVR( ELOUT, %VAL( INPNTR ), ELIN,
     :                                %VAL( IPNTR( 1 ) ),
     :                                %VAL( OPNTRW ), NBAD, STATUS )

*  Transform an unsigned-byte array.
                  ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
                     CALL KPG1_VASVUB( ELOUT, %VAL( INPNTR ), ELIN,
     :                                 %VAL( IPNTR( 1 ) ),
     :                                 %VAL( OPNTRW ), NBAD, STATUS )

*  Transform an unsigned-word array.
                  ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
                     CALL KPG1_VASVUW( ELOUT, %VAL( INPNTR ), ELIN,
     :                                 %VAL( IPNTR( 1 ) ),
     :                                 %VAL( OPNTRW ), NBAD, STATUS )

*  Transform a word array.
                  ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
                     CALL KPG1_VASVW( ELOUT, %VAL( INPNTR ), ELIN,
     :                                %VAL( IPNTR( 1 ) ),
     :                                %VAL( OPNTRW ), NBAD, STATUS )
                  END IF

*  Unmap the input NDF array.
                  CALL NDF_UNMAP( NDFIS, COMP( ICOMP ), STATUS )

*  If there is no flux conservation we only need to unmap the output
*  array.
                  IF ( CONSRV .AND. COMP( ICOMP ) .NE. 'Quality' ) THEN

*  Decide whether or not we need to test for bad pixels.
                     BAD = NBAD .NE. 0

*  Select the appropriate routine for the data type being processed and
*  multiply the data array by the constant.
                     IF ( ITYPE .EQ. '_BYTE' ) THEN
                        CALL KPG1_CMULB( BAD, EL, %VAL( OPNTRW ), FLUX,
     :                                   %VAL( OPNTR( 1 ) ), NBAD,
     :                                   STATUS )

                     ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
                        CALL KPG1_CMULUB( BAD, EL, %VAL( OPNTRW ), FLUX,
     :                                    %VAL( OPNTR( 1 ) ), NBAD,
     :                                    STATUS )

                     ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                        CALL KPG1_CMULD( BAD, EL, %VAL( OPNTRW ), FLUX,
     :                                   %VAL( OPNTR( 1 ) ), NBAD,
     :                                   STATUS )

                     ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
                        CALL KPG1_CMULI( BAD, EL, %VAL( OPNTRW ), FLUX,
     :                                   %VAL( OPNTR( 1 ) ), NBAD,
     :                                   STATUS )

                     ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
                        CALL KPG1_CMULR( BAD, EL, %VAL( OPNTRW ), FLUX,
     :                                   %VAL( OPNTR( 1 ) ), NBAD,
     :                                   STATUS )

                     ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
                        CALL KPG1_CMULW( BAD, EL, %VAL( OPNTRW ), FLUX,
     :                                   %VAL( OPNTR( 1 ) ), NBAD,
     :                                   STATUS )

                     ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
                        CALL KPG1_CMULUW( BAD, EL, %VAL( OPNTRW ), FLUX,
     :                                    %VAL( OPNTR( 1 ) ), NBAD,
     :                                    STATUS )
                     END IF

*  Tidy the workspace.
                     CALL PSX_FREE( OPNTRW, STATUS )
                  END IF

*  Set the bad pixel flag when bad pixels have been found.
                  IF ( NBAD .GT. 0 .AND.
     :                 COMP( ICOMP ) .NE. 'Quality' )
     :              CALL NDF_SBAD( .TRUE., NDFO, COMP( ICOMP ), STATUS )

*  Unmap the output array.
                  CALL NDF_UNMAP( NDFOS, COMP( ICOMP ), STATUS )
               END IF
            END DO

*  Annul the NDF sections.
            CALL NDF_ANNUL( NDFIS, STATUS )
            CALL NDF_ANNUL( NDFOS, STATUS )
         END DO

*  Free the workspace holding the resampled vector indices.
         CALL PSX_FREE( INPNTR, STATUS )

*  Apply linear-interpolation resampling.
*  ======================================
      ELSE IF ( METHOD .EQ. 'LININT' ) THEN

*  Get work arrays for the transformation. Needed is space for the input
*  and output co-ordinates, and input fractional indices.
         WDIM = ODIMS( 1 )
         CALL PSX_CALLOC( WDIM * NDIMI, ATYPE, WPNTR1, STATUS )
         CALL PSX_CALLOC( WDIM * NDIMI, ATYPE, WPNTR2, STATUS )
         CALL PSX_CALLOC( WDIM * NDIMO, ATYPE, WPNTR3, STATUS )

*  See if the variance component is present.  Note that a quality array
*  cannot be linearly interpolated.
         CALL NDF_STATE( NDFI, 'Variance', VAR, STATUS )

*  Loop through all sections.  Normally there will be just one.
         DO I = 1, NSECT

*  Create a section to which to apply the transformation.  First set its
*  bounds to give a sequence of just one element along each axis greater
*  than the number of transformation variables.
            IF ( FULL ) THEN
               DO K = 1, EXTDIM
                  J = K + NVOUT
                  ILBND( K + NVIN ) = MOD( I - 1, STRIDE( J ) ) + 1
                  IUBND( K + NVIN ) = ILBND( K + NVIN )
                  OLBND( J ) = MOD( I - 1, STRIDE( J ) ) + 1
                  OUBND( J ) = OLBND( J )
               END DO

*  Create the sections.
               CALL NDF_SECT( NDFI, NDIMI, ILBND, IUBND, NDFIS, STATUS )
               CALL NDF_SECT( NDFO, NDIMO, OLBND, IUBND, NDFOS, STATUS )

*  There is only one section, so just clone the identifiers.
            ELSE
               CALL NDF_CLONE( NDFI, NDFIS, STATUS )
               CALL NDF_CLONE( NDFO, NDFOS, STATUS )
            END IF

            IF ( VAR ) THEN

*  Get the implementation type of the arrays.
               CALL NDF_MTYPE( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,'/
     :                         /'_REAL,_DOUBLE', NDFI, NDFI,
     :                         'Data,Variance', ITYPE, DTYPE, STATUS )

*  Map the input and output arrays.
               CALL KPG1_MAP( NDFIS, 'Data,Variance', ITYPE, 'READ',
     :                       IPNTR, ELIN, STATUS )
               CALL KPG1_MAP( NDFOS, 'Data,Variance', ITYPE,
     :                       'WRITE/BAD', OPNTR, ELOUT, STATUS )

               IF ( DPAXIS ) THEN

*  Perform the transformation on the data array for the numeric data
*  type.  First apply to a byte array.
                  IF ( ITYPE .EQ. '_BYTE' ) THEN
                     CALL KPG1_TDLIB( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                VAR, %VAL( IPNTR( 2 ) ), CTRID,
     :                                FLUX, %VAL( CAXPTR ), ODIMS( 1 ),
     :                                NDIMO, OLBND, ODIMS,
     :                                %VAL( OPNTR( 1 ) ),
     :                                %VAL( OPNTR( 2 ) ),
     :                                %VAL( WPNTR1 ), %VAL( WPNTR3 ),
     :                                %VAL( WPNTR2 ), STATUS )

*  Transform a double-precision array.
                  ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                     CALL KPG1_TDLID( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                VAR, %VAL( IPNTR( 2 ) ), CTRID,
     :                                FLUX, %VAL( CAXPTR ), ODIMS( 1 ),
     :                                NDIMO, OLBND, ODIMS,
     :                                %VAL( OPNTR( 1 ) ),
     :                                %VAL( OPNTR( 2 ) ),
     :                                %VAL( WPNTR1 ), %VAL( WPNTR3 ),
     :                                %VAL( WPNTR2 ), STATUS )

*  Transform an integer array.
                  ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
                     CALL KPG1_TDLII( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                VAR, %VAL( IPNTR( 2 ) ), CTRID,
     :                                FLUX, %VAL( CAXPTR ), ODIMS( 1 ),
     :                                NDIMO, OLBND, ODIMS,
     :                                %VAL( OPNTR( 1 ) ),
     :                                %VAL( OPNTR( 2 ) ),
     :                                %VAL( WPNTR1 ), %VAL( WPNTR3 ),
     :                                %VAL( WPNTR2 ), STATUS )

*  Transform a single-precision array.
                  ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
                     CALL KPG1_TDLIR( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                VAR, %VAL( IPNTR( 2 ) ), CTRID,
     :                                FLUX, %VAL( CAXPTR ), ODIMS( 1 ),
     :                                NDIMO, OLBND, ODIMS,
     :                                %VAL( OPNTR( 1 ) ),
     :                                %VAL( OPNTR( 2 ) ),
     :                                %VAL( WPNTR1 ), %VAL( WPNTR3 ),
     :                                %VAL( WPNTR2 ), STATUS )

*  Transform an unsigned-byte array.
                  ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
                     CALL KPG1_TDLIUB( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                 VAR, %VAL( IPNTR( 2 ) ), CTRID,
     :                                 FLUX, %VAL( CAXPTR ), ODIMS( 1 ),
     :                                 NDIMO, OLBND, ODIMS,
     :                                 %VAL( OPNTR( 1 ) ),
     :                                 %VAL( OPNTR( 2 ) ),
     :                                 %VAL( WPNTR1 ), %VAL( WPNTR3 ),
     :                                 %VAL( WPNTR2 ), STATUS )

*  Transform an unsigned-word array.
                  ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
                     CALL KPG1_TDLIUW( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                 VAR, %VAL( IPNTR( 2 ) ), CTRID,
     :                                 FLUX, %VAL( CAXPTR ), ODIMS( 1 ),
     :                                 NDIMO, OLBND, ODIMS,
     :                                 %VAL( OPNTR( 1 ) ),
     :                                 %VAL( OPNTR( 2 ) ),
     :                                 %VAL( WPNTR1 ), %VAL( WPNTR3 ),
     :                                 %VAL( WPNTR2 ), STATUS )

*  Transform a word array.
                  ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
                     CALL KPG1_TDLIW( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                VAR, %VAL( IPNTR( 2 ) ), CTRID,
     :                                FLUX, %VAL( CAXPTR ), ODIMS( 1 ),
     :                                NDIMO, OLBND, ODIMS,
     :                                %VAL( OPNTR( 1 ) ),
     :                                %VAL( OPNTR( 2 ) ),
     :                                %VAL( WPNTR1 ), %VAL( WPNTR3 ),
     :                                %VAL( WPNTR2 ), STATUS )
                  END IF

*  Now repeat the above except this this it is for single-precision
*  axes.
               ELSE

*  Perform the transformation on the data array for the numeric
*  data type.  First apply to a byte array.
                  IF ( ITYPE .EQ. '_BYTE' ) THEN
                     CALL KPG1_TRLIB( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                VAR, %VAL( IPNTR( 2 ) ), CTRID,
     :                                FLUX, %VAL( CAXPTR ), ODIMS( 1 ),
     :                                NDIMO, OLBND, ODIMS,
     :                                %VAL( OPNTR( 1 ) ),
     :                                %VAL( OPNTR( 2 ) ),
     :                                %VAL( WPNTR1 ), %VAL( WPNTR3 ),
     :                                %VAL( WPNTR2 ), STATUS )

*  Transform a double-precision array.
                  ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                     CALL KPG1_TRLID( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                VAR, %VAL( IPNTR( 2 ) ), CTRID,
     :                                FLUX, %VAL( CAXPTR ), ODIMS( 1 ),
     :                                NDIMO, OLBND, ODIMS,
     :                                %VAL( OPNTR( 1 ) ),
     :                                %VAL( OPNTR( 2 ) ),
     :                                %VAL( WPNTR1 ), %VAL( WPNTR3 ),
     :                                %VAL( WPNTR2 ), STATUS )

*  Transform an integer array.
                  ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
                     CALL KPG1_TRLII( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                VAR, %VAL( IPNTR( 2 ) ), CTRID,
     :                                FLUX, %VAL( CAXPTR ), ODIMS( 1 ),
     :                                NDIMO, OLBND, ODIMS,
     :                                %VAL( OPNTR( 1 ) ),
     :                                %VAL( OPNTR( 2 ) ),
     :                                %VAL( WPNTR1 ), %VAL( WPNTR3 ),
     :                                %VAL( WPNTR2 ), STATUS )

*  Transform a single-precision array.
                  ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
                     CALL KPG1_TRLIR( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                VAR, %VAL( IPNTR( 2 ) ), CTRID,
     :                                FLUX, %VAL( CAXPTR ), ODIMS( 1 ),
     :                                NDIMO, OLBND, ODIMS,
     :                                %VAL( OPNTR( 1 ) ),
     :                                %VAL( OPNTR( 2 ) ),
     :                                %VAL( WPNTR1 ), %VAL( WPNTR3 ),
     :                                %VAL( WPNTR2 ), STATUS )

*  Transform an unsigned-byte array.
                  ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
                     CALL KPG1_TRLIUB( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                 VAR, %VAL( IPNTR( 2 ) ), CTRID,
     :                                 FLUX, %VAL( CAXPTR ), ODIMS( 1 ),
     :                                 NDIMO, OLBND, ODIMS,
     :                                 %VAL( OPNTR( 1 ) ),
     :                                 %VAL( OPNTR( 2 ) ),
     :                                 %VAL( WPNTR1 ), %VAL( WPNTR3 ),
     :                                 %VAL( WPNTR2 ), STATUS )

*  Transform an unsigned-word array.
                  ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
                     CALL KPG1_TRLIUW( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                 VAR, %VAL( IPNTR( 2 ) ), CTRID,
     :                                 FLUX, %VAL( CAXPTR ), ODIMS( 1 ),
     :                                 NDIMO, OLBND, ODIMS,
     :                                 %VAL( OPNTR( 1 ) ),
     :                                 %VAL( OPNTR( 2 ) ),
     :                                 %VAL( WPNTR1 ), %VAL( WPNTR3 ),
     :                                 %VAL( WPNTR2 ), STATUS )

*  Transform a word array.
                  ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
                     CALL KPG1_TRLIW( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                VAR, %VAL( IPNTR( 2 ) ), CTRID,
     :                                FLUX, %VAL( CAXPTR ), ODIMS( 1 ),
     :                                NDIMO, OLBND, ODIMS,
     :                                %VAL( OPNTR( 1 ) ),
     :                                %VAL( OPNTR( 2 ) ),
     :                                %VAL( WPNTR1 ), %VAL( WPNTR3 ),
     :                                %VAL( WPNTR2 ), STATUS )
                  END IF

               END IF
            ELSE

*  Get the type of the data array.
               CALL NDF_TYPE( NDFI, 'Data', ITYPE, STATUS )

*  Map the input and output arrays.  Use a dummy variance value.  It is
*  not assigned as it will not be used.
               CALL KPG1_MAP( NDFIS, 'Data', ITYPE, 'READ', IPNTR, ELIN,
     :                       STATUS )
               CALL KPG1_MAP( NDFOS, 'Data', ITYPE, 'WRITE/BAD', OPNTR,
     :                       ELOUT, STATUS )

               IF ( DPAXIS ) THEN

*  Perform the transformation on the data array for the numeric data
*  type.  First apply to a byte array.
                  IF ( ITYPE .EQ. '_BYTE' ) THEN
                     CALL KPG1_TDLIB( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                VAR, VARB, CTRID, FLUX,
     :                                %VAL( CAXPTR ), ODIMS( 1 ), NDIMO,
     :                                OLBND, ODIMS, %VAL( OPNTR( 1 ) ),
     :                                VARBO, %VAL( WPNTR1 ),
     :                                %VAL( WPNTR3 ), %VAL( WPNTR2 ),
     :                                STATUS )

*  Transform a double-precision array.
                  ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                     CALL KPG1_TDLID( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                VAR, VARD, CTRID, FLUX,
     :                                %VAL( CAXPTR ), ODIMS( 1 ), NDIMO,
     :                                OLBND, ODIMS, %VAL( OPNTR( 1 ) ),
     :                                VARDO, %VAL( WPNTR1 ),
     :                                %VAL( WPNTR3 ), %VAL( WPNTR2 ),
     :                                STATUS )

*  Transform an integer array.
                  ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
                     CALL KPG1_TDLII( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                VAR, VARI, CTRID, FLUX,
     :                                %VAL( CAXPTR ), ODIMS( 1 ), NDIMO,
     :                                OLBND, ODIMS, %VAL( OPNTR( 1 ) ),
     :                                VARIO, %VAL( WPNTR1 ),
     :                                %VAL( WPNTR3 ), %VAL( WPNTR2 ),
     :                                STATUS )

*  Transform a single-precision array.
                  ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
                     CALL KPG1_TDLIR( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                VAR, VARR, CTRID, FLUX,
     :                                %VAL( CAXPTR ), ODIMS( 1 ), NDIMO,
     :                                OLBND, ODIMS, %VAL( OPNTR( 1 ) ),
     :                                VARRO, %VAL( WPNTR1 ),
     :                                %VAL( WPNTR3 ), %VAL( WPNTR2 ),
     :                                STATUS )

*  Transform an unsigned-byte array.
                  ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
                     CALL KPG1_TDLIUB( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                 VAR, VARB, CTRID, FLUX,
     :                                 %VAL( CAXPTR ), ODIMS( 1 ),
     :                                 NDIMO, OLBND, ODIMS,
     :                                 %VAL( OPNTR( 1 ) ), VARBO,
     :                                 %VAL( WPNTR1 ), %VAL( WPNTR3 ),
     :                                 %VAL( WPNTR2 ), STATUS )

*  Transform an unsigned-word array.
                  ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
                     CALL KPG1_TDLIUW( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                 VAR, VARW, CTRID, FLUX,
     :                                 %VAL( CAXPTR ), ODIMS( 1 ),
     :                                 NDIMO, OLBND, ODIMS,
     :                                 %VAL( OPNTR( 1 ) ), VARWO,
     :                                 %VAL( WPNTR1 ), %VAL( WPNTR3 ),
     :                                 %VAL( WPNTR2 ), STATUS )

*  Transform a word array.
                  ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
                     CALL KPG1_TDLIW( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                VAR, VARW, CTRID, FLUX,
     :                                %VAL( CAXPTR ), ODIMS( 1 ), NDIMO,
     :                                OLBND, ODIMS, %VAL( OPNTR( 1 ) ),
     :                                VARWO, %VAL( WPNTR1 ),
     :                                %VAL( WPNTR3 ), %VAL( WPNTR2 ),
     :                                STATUS )
                  END IF

*  Now repeat the above except this this it is for single-precision
*  axes.
               ELSE

*  Perform the transformation on the data array for the numeric data
*  type.  First apply to a byte array.
                  IF ( ITYPE .EQ. '_BYTE' ) THEN
                     CALL KPG1_TRLIB( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                VAR, VARB, CTRID, FLUX,
     :                                %VAL( CAXPTR ), ODIMS( 1 ), NDIMO,
     :                                OLBND, ODIMS, %VAL( OPNTR( 1 ) ),
     :                                VARBO, %VAL( WPNTR1 ),
     :                                %VAL( WPNTR3 ), %VAL( WPNTR2 ),
     :                                STATUS )

*  Transform a double-precision array.
                  ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                     CALL KPG1_TRLID( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                VAR, VARD, CTRID, FLUX,
     :                                %VAL( CAXPTR ), ODIMS( 1 ), NDIMO,
     :                                OLBND, ODIMS, %VAL( OPNTR( 1 ) ),
     :                                VARDO, %VAL( WPNTR1 ),
     :                                %VAL( WPNTR3 ), %VAL( WPNTR2 ),
     :                                STATUS )

*  Transform an integer array.
                  ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
                     CALL KPG1_TRLII( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                VAR, VARI, CTRID, FLUX,
     :                                %VAL( CAXPTR ), ODIMS( 1 ), NDIMO,
     :                                OLBND, ODIMS, %VAL( OPNTR( 1 ) ),
     :                                VARIO, %VAL( WPNTR1 ),
     :                                %VAL( WPNTR3 ), %VAL( WPNTR2 ),
     :                                STATUS )

*  Transform a single-precision array.
                  ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
                     CALL KPG1_TRLIR( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                VAR, VARR, CTRID, FLUX,
     :                                %VAL( CAXPTR ), ODIMS( 1 ), NDIMO,
     :                                OLBND, ODIMS, %VAL( OPNTR( 1 ) ),
     :                                VARRO, %VAL( WPNTR1 ),
     :                                %VAL( WPNTR3 ), %VAL( WPNTR2 ),
     :                                STATUS )

*  Transform an unsigned-byte array.
                  ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
                     CALL KPG1_TRLIUB( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                 VAR, VARB, CTRID, FLUX,
     :                                 %VAL( CAXPTR ), ODIMS( 1 ),
     :                                 NDIMO, OLBND, ODIMS,
     :                                 %VAL( OPNTR( 1 ) ), VARBO,
     :                                 %VAL( WPNTR1 ), %VAL( WPNTR3 ),
     :                                 %VAL( WPNTR2 ), STATUS )

*  Transform an unsigned-word array.
                  ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
                     CALL KPG1_TRLIUW( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                 VAR, VARW, CTRID, FLUX,
     :                                 %VAL( CAXPTR ), ODIMS( 1 ),
     :                                 NDIMO, OLBND, ODIMS,
     :                                 %VAL( OPNTR( 1 ) ), VARWO,
     :                                 %VAL( WPNTR1 ), %VAL( WPNTR3 ),
     :                                 %VAL( WPNTR2 ), STATUS )

*  Transform a word array.
                  ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
                     CALL KPG1_TRLIW( NDIMI, IDIMS, %VAL( IPNTR( 1 ) ),
     :                                VAR, VARW, CTRID, FLUX,
     :                                %VAL( CAXPTR ), ODIMS( 1 ), NDIMO,
     :                                OLBND, ODIMS, %VAL( OPNTR( 1 ) ),
     :                                VARWO, %VAL( WPNTR1 ),
     :                                %VAL( WPNTR3 ), %VAL( WPNTR2 ),
     :                                STATUS )
                  END IF

               END IF
            END IF

*  Annul (and unmap) the NDF sections.
            CALL NDF_ANNUL( NDFIS, STATUS )
            CALL NDF_ANNUL( NDFOS, STATUS )
         END DO

*  Free the workspace that is no longer needed.
         CALL PSX_FREE( WPNTR1, STATUS )
         CALL PSX_FREE( WPNTR2, STATUS )
         CALL PSX_FREE( WPNTR3, STATUS )

      END IF

*  Tidy resources.
*  ===============

*  Free the workspace holding the concatenated axes.
  940 CONTINUE
      CALL PSX_FREE( CAXPTR, STATUS )

*  Tidy the NDF context.
  960 CONTINUE
      CALL NDF_END( STATUS )

*  Free the transformation resources.
  980 CONTINUE
      CALL DAT_ANNUL( LOCTR, STATUS )
      CALL TRN_CLOSE( STATUS )

  999 CONTINUE

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'TRANSFORMER_ERR',
     :     'TRANSFORMER: Unable to transform the NDF.',
     :     STATUS )
      END IF

      END
