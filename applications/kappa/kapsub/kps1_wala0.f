      SUBROUTINE KPS1_WALA0( NDIM2, INDF1, INDF2, MAP, MAP4, IWCSR, 
     :                       METHOD, PARAMS, XY1, XY2, ERRLIM, MAXPIX, 
     :                       REBIN, WLIM, STATUS )
*+
*  Name:
*     KPS1_WALA0

*  Purpose:
*     Process a single pair of input and output NDFs for WCSALIGN.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_WALA0( NDIM2, INDF1, INDF2, MAP, MAP4, IWCSR, METHOD, 
*                      PARAMS, XY1, XY2, ERRLIM, MAXPIX, REBIN, WLIM,
*                      STATUS )

*  Description:
*     This routine first finds the Mapping from the input pixel
*     co-ordinates to the reference (and hence output) pixel co-ordinates.
*     If the user has explicitly specified bounds for the output image,
*     these are used, otherwise the bounds of the output image which
*     just include the input image are calculated and used instead. The
*     WCS FrameSet is now created for the output NDF. This is a copy of
*     the reference FrameSet, but modified to take account of any
*     difference in the pixel origins between the reference and output
*     NDFs. Finally, the output NDF is resampled or rebinned using the 
*     specified method. If nearest-neighbour is the chosen method, and 
*     the input NDF contains a QUALITY array, then this array is copied 
*     to the output.

*  Arguments:
*     NDIM2 = INTEGER (Given)
*        The numner of axes in the reference NDF.
*     INDF1 = INTEGER (Given)
*        Identifier for the input NDF.
*     INDF2 = INTEGER (Given)
*        Identifier for the output NDF.
*     MAP = INTEGER (Given)
*        AST pointer to the Mapping from input pixel coords to reference
*        pixel coords.
*     MAP4 = INTEGER (Given)
*        AST pointer to the Mapping from input grid coords to input
*        pixel coords.
*     IWCSR = INTEGER (Given)
*        AST pointer for the WCS FrameSet from the reference NDF.
*     METHOD = INTEGER (Given)
*        The interpolation method to use when re-sampling the input
*        image; AST__NEAREST, AST__LINEAR, AST__SINCSINC, etc.
*     PARAMS = DOUBLE PRECISION (Given)
*        An optional array containing ay additonal parameter values
*        required by the sub-pixel interpolation scheme.
*     XY1( NDIM2 ) = INTEGER (Given)
*        The indices of the bottom left pixel in the output NDF. If set
*        to VAL__BADI then default bounds will be found for the output
*        NDF. The number iof values in the array should equal the number
*        of pixel axes in the output NDF.
*     XY2( NDIM2 ) = INTEGER (Given)
*        The indices of the top right pixel in the output NDF. If set
*        to VAL__BADI then default bounds will be found for the output
*        NDF. The number iof values in the array should equal the number
*        of pixel axes in the output NDF.
*     ERRLIM = REAL (Given)
*        The position accuracy required when re-sampling the input NDF.
*        Given as a number of pixels.
*     MAXPIX = INTEGER (Given)
*        The initial scale size, in pixels, for the adaptive algorithm
*        which approximates non-linear Mappings with piece-wise linear
*        transformations.
*     REBIN = LOGICAL (Given)
*        Calculate output pixel values by rebinning? Otherwise they will
*        be calculated by resampling.
*     WLIM = REAL (Given)
*        The lower weight limit for a valid output pixel.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     TDCA: Tim Ash (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     6-OCT-1998 (DSB):
*        Original version, based on IRAS90:SALIA0
*     1-JUL-1999 (TDCA):
*        Modified to use AST_RESAMPLE<X>
*     5-AUG-1999 (DSB):
*        Tidied up.
*     19-SEP-2001 (DSB):
*        Allow use with 1-dimensional NDFs by changing kpg1_asget EXACT
*        argument to .false.
*     31-OCT-2002 (DSB):
*        Make N-dimensional.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     19-JUL-2005 (DSB):
*        Add argument REBIN.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'MSG_PAR'          ! MSG constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NDIM2
      INTEGER INDF1 
      INTEGER INDF2
      INTEGER MAP
      INTEGER MAP4
      INTEGER IWCSR
      INTEGER METHOD
      DOUBLE PRECISION PARAMS( 2 )
      INTEGER XY1( NDIM2 )
      INTEGER XY2( NDIM2 )
      REAL ERRLIM
      INTEGER MAXPIX
      LOGICAL REBIN
      REAL WLIM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER DOMLST*50          ! List of preferred alignment domains
      CHARACTER DTYPE*(NDF__SZFTP) ! Data type
      CHARACTER TY_IN*(NDF__SZTYP) ! Numeric type for processing
      DOUBLE PRECISION DUMMY( 1 )  ! Dummy array.
      DOUBLE PRECISION PLBND1( NDF__MXDIM ) ! Lower pixel co-ord bounds in input
      DOUBLE PRECISION PLBND2( NDF__MXDIM ) ! Lower pixel co-ord bounds in output
      DOUBLE PRECISION PUBND1( NDF__MXDIM ) ! Upper pixel co-ord bounds in input
      DOUBLE PRECISION PUBND2( NDF__MXDIM ) ! Upper pixel co-ord bounds in output
      DOUBLE PRECISION TOL         ! Max tolerable geometrical distort.
      DOUBLE PRECISION XL( NDF__MXDIM )     ! I/p position of output lower bound
      DOUBLE PRECISION XU( NDF__MXDIM )     ! I/p position of output upper bound
      INTEGER BAD_PIXELS         ! Value returned from AST_RESAMPLE<x>
      INTEGER EL                 ! No. of elements in a mapped array
      INTEGER FLAGS              ! Sum of AST__USEBAD and AST__USEVAR
      INTEGER I                  ! Loop count
      INTEGER IAT                ! No. of characters in string
      INTEGER IPD1               ! Pointer to input data array
      INTEGER IPD2               ! Pointer to output data array
      INTEGER IPIX1              ! Index of PIXEL Frame in input NDF FrameSet
      INTEGER IPIX2              ! Index of PIXEL Frame in output NDF FrameSet
      INTEGER IPIXR              ! Index of PIXEL Frame in ref. NDF FrameSet
      INTEGER IPQ1               ! Pointer to input quality array
      INTEGER IPQ2               ! Pointer to output quality array
      INTEGER IPV1               ! Pointer to input variance array
      INTEGER IPV2               ! Pointer to output variance array
      INTEGER IWCS1              ! AST pointer to input WCS FrameSet
      INTEGER IWCS2              ! AST pointer to original output WCS FrameSet
      INTEGER IWCSR2             ! AST pointer to new output WCS FrameSet
      INTEGER J                  ! Loop count
      INTEGER LBND1( NDF__MXDIM )         ! Lower bounds of input NDF
      INTEGER LBND2( NDF__MXDIM )         ! Lower bounds of output NDF
      INTEGER LGRID1( NDF__MXDIM )        ! Lower bounds of input grid co-ords
      INTEGER LGRID2( NDF__MXDIM )        ! Lower bounds of output grid co-ords
      INTEGER MAP2               ! AST Mapping (o/p PIXEL -> o/p GRID)
      INTEGER MAP3               ! AST Mapping (ref. GRID -> o/p GRID)
      INTEGER MAP5               ! AST Mapping (i/p GRID -> o/p GRID)
      INTEGER MAPR               ! AST Mapping (ref. GRID -> ref. PIXEL)
      INTEGER NDIM1              ! No. of pixel axes in input NDF
      INTEGER NFRM               ! No. of Frames in input NDF FrameSet
      INTEGER RESULT             ! Dummy value returned from AST_RESAMPLE<x>
      INTEGER UBND1( NDF__MXDIM )         ! Upper bounds of input NDF
      INTEGER UBND2( NDF__MXDIM )         ! Upper bounds of output NDF
      INTEGER UGRID1( NDF__MXDIM )        ! Upper bounds of input grid co-ords
      INTEGER UGRID2( NDF__MXDIM )        ! Upper bounds of output grid co-ords 
      LOGICAL BAD_DV             ! Bad pixels present in DATA/VARIANCE arrays?
      LOGICAL QUAL               ! Are quality values to be copied?
      LOGICAL VAR                ! Are variance values to be copied?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the pixel bounds of the input NDF.
      CALL NDF_BOUND( INDF1, NDF__MXDIM, LBND1, UBND1, NDIM1, STATUS ) 

*  Get the number of pixel axes in the input NDF (may not be the same as the
*  number in the output NDF).
      NDIM1 = AST_GETI( MAP, 'Nin', STATUS )

*  Find the index of the PIXEL Frame in the reference NDF.
      CALL KPG1_ASFFR( IWCSR, 'PIXEL', IPIXR, STATUS )

*  Set the bounds of the output NDF.
*  =================================

*  If the user supplied explicitly specified bounds for the output
*  images, use them. 
      IF( XY1( 1 ) .NE. VAL__BADI ) THEN
         DO I = 1, NDIM2
            LBND2( I ) = XY1( I )
            UBND2( I ) = XY2( I )
         END DO

*  Otherwise, find the bounds of the box within the pixel co-ordinate
*  Frame of the reference image which just includes the input image.
      ELSE

*  Store the pixel co-ordinate bounds of the input image.
         DO I = 1, NDIM2
            PLBND1( I ) = DBLE( LBND1( I ) - 1 )
            PUBND1( I ) = DBLE( UBND1( I ) )
         END DO

*  Find the bounds on each axis of the corresponding area in the output image.
         DO I = 1, NDIM2
            CALL AST_MAPBOX( MAP, PLBND1, PUBND1, .TRUE., I, 
     :                       PLBND2( I ), PUBND2( I ), XL, XU, STATUS ) 

*  Convert to pixel index bounds.
            LBND2( I ) = NINT( PLBND2( I ) )
            UBND2( I ) = NINT( PUBND2( I ) )
         END DO

      END IF

*  Report the bounds of the output NDF.
      DO I = 1, NDIM2
         CALL MSG_SETI( 'B', LBND2( I ) )
         CALL MSG_SETC( 'B', ':' )
         CALL MSG_SETI( 'B', UBND2( I ) )
         IF( I .NE. NDIM2 ) CALL MSG_SETC( 'B', ', ' )
      END DO
      CALL MSG_OUTIF( MSG__VERB, 'KPS1_WALA0_MSG2', '    The output '//
     :                'NDF has bounds ( ^B )', STATUS )

*  Get bounds of the input and output NDFs in grid co-ords
      DO I = 1, NDIM2
         LGRID1( I ) = 1
         UGRID1( I ) = UBND1( I ) - LBND1( I ) + 1
  
         LGRID2( I ) = 1
         UGRID2( I ) = UBND2( I ) - LBND2( I ) + 1

*  Report an error if the output image would be too large.
         IF( ( UGRID2( I ) .GT. 50000 ) .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR

            DO J = 1, NDIM2
               CALL MSG_SETI( 'B', UBND2( J ) - LBND2( J ) +1 )
               IF( J .NE. NDIM2 ) CALL MSG_SETC( 'B', ', ' )
            END DO

            CALL ERR_REP( 'KPS1_WALA0_ERR1', 'The output image '//
     :                 'dimensions are too big (^B).', STATUS )
            GO TO 999
         END IF
      END DO

*  Change the bounds of the output NDF to the values required to cover
*  all the input data.
      CALL NDF_SBND( NDIM2, LBND2, UBND2, INDF2, STATUS )

*  Store WCS information in the output NDF.
*  ========================================

*  We now create the WCS FrameSet for the output NDF. This will be a copy
*  of the reference FrameSet, modified to take account of any difference
*  in the pixel origins between the reference and output NDFs. We do this
*  by taking a copy of the reference WCS FrameSet and then re-mapping the 
*  GRID Frame in the copy. The Mapping used is the mapping from reference
*  GRID Frame to output GRID Frame, going via the common PIXEL Frame.

*  Get the default WCS FrameSet for the output NDF.
      CALL NDF_GTWCS( INDF2, IWCS2, STATUS )

*  Find the PIXEL Frame.
      CALL KPG1_ASFFR( IWCS2, 'PIXEL', IPIX2, STATUS )

*  Get the Mapping from the PIXEL Frame to the output GRID Frame.
      MAP2 = AST_GETMAPPING( IWCS2, IPIX2, AST__BASE, STATUS )

*  Take a copy of the reference FrameSet.
      IWCSR2 = AST_COPY( IWCSR, STATUS )

*  Get the Mapping from the reference GRID Frame to the PIXEL Frame.
      MAPR = AST_GETMAPPING( IWCSR2, AST__BASE, IPIXR, STATUS )

*  Get the Mapping from input GRID to output GRID.
      MAP5 = AST_SIMPLIFY( AST_CMPMAP( AST_CMPMAP( MAP4, MAP, .TRUE., 
     :                                             ' ', STATUS ), 
     :                                 MAP2, .TRUE., ' ', STATUS ),
     :                     STATUS )

*  Concatenate and simplify MAPR and MAP2 to get the Mapping from
*  reference GRID Frame to output GRID Frame.
      MAP3 = AST_SIMPLIFY( AST_CMPMAP( MAPR, MAP2, .TRUE., ' ', 
     :                                 STATUS ), STATUS )

*  Re-map the GRID Frame in the copy of the reference WCS FrameSet so
*  that it corresponds to the GRID Frame in the output NDF.
      CALL AST_REMAPFRAME( IWCSR2, AST__BASE, MAP3, STATUS )

*  Store this FrameSet in the output NDF.
      CALL NDF_PTWCS( IWCSR2, INDF2, STATUS )

*  Do the resampling.
*  ==================

      FLAGS = 0

*  Set TOL (DOUBLE) to ERRLIM (REAL)
      TOL = ERRLIM

*  Map the DATA component of the input and output NDF. Rebinning is only
*  available in _INTEGER, _REAL or _DOUBLE. Resampling can gandle any
*  numeric data type.
      IF( .NOT. REBIN ) THEN
         CALL NDF_TYPE( INDF1, 'DATA', TY_IN, STATUS )
      ELSE
         CALL NDF_MTYPE( '_INTEGER,_REAL,_DOUBLE', INDF1, INDF1, 'DATA', 
     :                   TY_IN, DTYPE, STATUS )
      END IF

      CALL NDF_MAP( INDF1, 'DATA', TY_IN, 'READ', IPD1, EL, STATUS )
      CALL NDF_MAP( INDF2, 'DATA', TY_IN, 'WRITE', IPD2, EL, STATUS )

*  If VARIANCE component present, map it, else assign value of
*  corresponding DATA component (safe value).
      CALL NDF_STATE( INDF1, 'VAR', VAR, STATUS )
      IF ( VAR ) THEN
         CALL NDF_MAP( INDF1, 'VAR', TY_IN, 'READ', IPV1, EL, STATUS )
         CALL NDF_MAP( INDF2, 'VAR', TY_IN, 'WRITE', IPV2, EL, 
     :                 STATUS )
         FLAGS = FLAGS + AST__USEVAR
      ELSE
         IPV1 = IPD1
         IPV2 = IPD2
      END IF

*  Check for bad pixels in DATA and VARIANCE components.
      CALL NDF_BAD( INDF1, 'DATA,VARIANCE', .FALSE., BAD_DV, STATUS )
      IF( BAD_DV ) FLAGS = FLAGS + AST__USEBAD 

*  Call the appropriate resampling routine
      IF( .NOT. REBIN ) THEN
         IF ( TY_IN .EQ. '_INTEGER' ) THEN
            BAD_PIXELS = AST_RESAMPLEI( MAP5, NDIM1, LGRID1, UGRID1,
     :                                %VAL( CNF_PVAL( IPD1 ) ), 
     :                                %VAL( CNF_PVAL( IPV1 ) ), METHOD,
     :                                AST_NULL, PARAMS, FLAGS, TOL, 
     :                                MAXPIX, VAL__BADI, NDIM2, LGRID2, 
     :                                UGRID2, LGRID2, UGRID2, 
     :                                %VAL( CNF_PVAL( IPD2 ) ), 
     :                                %VAL( CNF_PVAL( IPV2 ) ),
     :                                STATUS )
         
         ELSE IF ( TY_IN .EQ. '_REAL' ) THEN
            BAD_PIXELS = AST_RESAMPLER( MAP5, NDIM1, LGRID1, UGRID1,
     :                                %VAL( CNF_PVAL( IPD1 ) ), 
     :                                %VAL( CNF_PVAL( IPV1 ) ), METHOD,
     :                                AST_NULL, PARAMS, FLAGS, TOL, 
     :                                MAXPIX, VAL__BADR, NDIM2, LGRID2, 
     :                                UGRID2, LGRID2, UGRID2, 
     :                                %VAL( CNF_PVAL( IPD2 ) ), 
     :                                %VAL( CNF_PVAL( IPV2 ) ),
     :                                STATUS )
         
         ELSE IF ( TY_IN .EQ. '_DOUBLE' ) THEN
            BAD_PIXELS = AST_RESAMPLED( MAP5, NDIM1, LGRID1, UGRID1,
     :                                %VAL( CNF_PVAL( IPD1 ) ), 
     :                                %VAL( CNF_PVAL( IPV1 ) ), METHOD,
     :                                AST_NULL, PARAMS, FLAGS, TOL, 
     :                                MAXPIX, VAL__BADD, NDIM2, LGRID2, 
     :                                UGRID2, LGRID2, UGRID2, 
     :                                %VAL( CNF_PVAL( IPD2 ) ), 
     :                                %VAL( CNF_PVAL( IPV2 ) ),
     :                                STATUS )
         
         ELSE IF ( TY_IN .EQ. '_BYTE' ) THEN 
            BAD_PIXELS = AST_RESAMPLEB( MAP5, NDIM1, LGRID1, UGRID1, 
     :                                %VAL( CNF_PVAL( IPD1 ) ), 
     :                                %VAL( CNF_PVAL( IPV1 ) ), METHOD,
     :                                AST_NULL, PARAMS, FLAGS, TOL, 
     :                                MAXPIX, VAL__BADB, NDIM2, LGRID2, 
     :                                UGRID2, LGRID2, UGRID2, 
     :                                %VAL( CNF_PVAL( IPD2 ) ), 
     :                                %VAL( CNF_PVAL( IPV2 ) ),
     :                                STATUS )
         
         ELSE IF ( TY_IN .EQ. '_UBYTE' ) THEN
            BAD_PIXELS = AST_RESAMPLEUB( MAP5, NDIM1, LGRID1, UGRID1,
     :                                %VAL( CNF_PVAL( IPD1 ) ), 
     :                                %VAL( CNF_PVAL( IPV1 ) ), METHOD,
     :                                AST_NULL, PARAMS, FLAGS, TOL, 
     :                                MAXPIX, VAL__BADUB, NDIM2, LGRID2, 
     :                                UGRID2, LGRID2, UGRID2, 
     :                                %VAL( CNF_PVAL( IPD2 ) ), 
     :                                %VAL( CNF_PVAL( IPV2 ) ),
     :                                STATUS )
         
         ELSE IF ( TY_IN .EQ. '_WORD' ) THEN 
            BAD_PIXELS = AST_RESAMPLEW( MAP5, NDIM1, LGRID1, UGRID1,
     :                                %VAL( CNF_PVAL( IPD1 ) ), 
     :                                %VAL( CNF_PVAL( IPV1 ) ), METHOD,
     :                                AST_NULL, PARAMS, FLAGS, TOL, 
     :                                MAXPIX, VAL__BADW, NDIM2, LGRID2, 
     :                                UGRID2, LGRID2, UGRID2, 
     :                                %VAL( CNF_PVAL( IPD2 ) ), 
     :                                %VAL( CNF_PVAL( IPV2 ) ),
     :                                STATUS )
         
         ELSE IF ( TY_IN .EQ. '_UWORD' ) THEN 
            BAD_PIXELS = AST_RESAMPLEUW( MAP5, NDIM1, LGRID1, UGRID1,
     :                                %VAL( CNF_PVAL( IPD1 ) ), 
     :                                %VAL( CNF_PVAL( IPV1 ) ), METHOD,
     :                                AST_NULL, PARAMS, FLAGS, TOL, 
     :                                MAXPIX, VAL__BADUW, NDIM2, LGRID2, 
     :                                UGRID2, LGRID2, UGRID2, 
     :                                %VAL( CNF_PVAL( IPD2 ) ), 
     :                                %VAL( CNF_PVAL( IPV2 ) ),
     :                                STATUS )
         
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'TY', TY_IN )
            CALL ERR_REP( 'KPS1_WALA0_ERR2', 'KPS1_WALA0: Unsupported'//
     :             ' resampling data type ''^TY'' (programming error).', 
     :             STATUS )
         END IF

*  Call the appropriate rebinning routine
      ELSE
         IF ( TY_IN .EQ. '_INTEGER' ) THEN
            CALL AST_REBINI( MAP5, DBLE( WLIM ), NDIM1, LGRID1, UGRID1,
     :                       %VAL( CNF_PVAL( IPD1 ) ), 
     :                       %VAL( CNF_PVAL( IPV1 ) ), METHOD,
     :                       PARAMS, FLAGS, TOL, MAXPIX, VAL__BADI, 
     :                       NDIM2, LGRID2, UGRID2, LGRID1, UGRID1, 
     :                       %VAL( CNF_PVAL( IPD2 ) ), 
     :                       %VAL( CNF_PVAL( IPV2 ) ),
     :                       STATUS )
         
         ELSE IF ( TY_IN .EQ. '_REAL' ) THEN
            CALL AST_REBINR( MAP5, DBLE( WLIM ), NDIM1, LGRID1, UGRID1,
     :                       %VAL( CNF_PVAL( IPD1 ) ), 
     :                       %VAL( CNF_PVAL( IPV1 ) ), METHOD,
     :                       PARAMS, FLAGS, TOL, MAXPIX, VAL__BADR, 
     :                       NDIM2, LGRID2, UGRID2, LGRID1, UGRID1, 
     :                       %VAL( CNF_PVAL( IPD2 ) ), 
     :                       %VAL( CNF_PVAL( IPV2 ) ),
     :                       STATUS )

         ELSE IF ( TY_IN .EQ. '_DOUBLE' ) THEN
            CALL AST_REBIND( MAP5, DBLE( WLIM ), NDIM1, LGRID1, UGRID1,
     :                       %VAL( CNF_PVAL( IPD1 ) ), 
     :                       %VAL( CNF_PVAL( IPV1 ) ), METHOD,
     :                       PARAMS, FLAGS, TOL, MAXPIX, VAL__BADD, 
     :                       NDIM2, LGRID2, UGRID2, LGRID1, UGRID1, 
     :                       %VAL( CNF_PVAL( IPD2 ) ), 
     :                       %VAL( CNF_PVAL( IPV2 ) ),
     :                       STATUS )

         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'TY', TY_IN )
            CALL ERR_REP( 'KPS1_WALA0_ERR2', 'KPS1_WALA0: Unsupported'//
     :             ' rebinning data type ''^TY'' (programming error).', 
     :             STATUS )
         END IF

         BAD_PIXELS = 1

      END IF

*  Set the bad pixel flags for the output DATA and VARIANCE arrays.
      IF ( BAD_PIXELS .GT. 0 ) THEN 
         CALL NDF_SBAD( .TRUE., INDF2, 'DATA', STATUS )
         CALL NDF_SBAD( .TRUE., INDF2, 'VARIANCE', STATUS )
      END IF

*  Resample QUALITY arrays if appropriate
      CALL NDF_STATE( INDF1, 'QUAL', QUAL, STATUS )
      IF ( ( METHOD .EQ. AST__NEAREST ) .AND. QUAL .AND. 
     :     .NOT. REBIN ) THEN

*  Map the QUALITY component.
         CALL NDF_MAP( INDF1, 'QUAL', '_UBYTE', 'READ', IPQ1, EL, 
     :                 STATUS )
         CALL NDF_MAP( INDF2, 'QUAL', '_UBYTE', 'WRITE', IPQ2, EL,
     :                 STATUS )

*  Reset FLAGS (QUALITY arrays cannot have bad pixels).
         FLAGS = 0

*  Do the resampling.
         RESULT = AST_RESAMPLEUW( MAP5, NDIM1, LGRID1, UGRID1, 
     :                            %VAL( CNF_PVAL( IPQ1 ) ), 
     :                            DUMMY, METHOD, AST_NULL,
     :                            PARAMS, FLAGS, TOL, MAXPIX, 
     :                            VAL__BADUW, NDIM2, LGRID2, UGRID2, 
     :                            LGRID2, UGRID2, 
     :                            %VAL( CNF_PVAL( IPQ2 ) ), DUMMY,
     :                            STATUS )

      END IF

*  Tidy up.
 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

      END
