      SUBROUTINE KPS1_WALA0( INDF1, INDF2, IWCSR, METHOD, PARAMS, XY1, 
     :                       XY2, ERRLIM, MAXPIX, STATUS )
*+
*  Name:
*     KPS1_WALA0

*  Purpose:
*     Process a single pair of input and output NDFs for WCSALIGN.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_WALA0( INDF1, INDF2, IWCSR, METHOD, PARAMS, XY1, XY2, 
*                      ERRLIM, MAXPIX, STATUS )

*  Description:
*     This routine first finds the Mapping from the input pixel
*     co-ordinates to the reference (and hence output) pixel co-ordinates.
*     If the user has explicitly specified bounds for the output image,
*     these are used, otherwise the bounds of the output image which
*     just include the input image are calculated and used instead. The
*     WCS FrameSet is now created for the output NDF. This is a copy of
*     the reference FrameSet, but modified to take account of any
*     difference in the pixel origins between the reference and output
*     NDFs. Finally, the output NDF is resampled using the specified
*     method. If nearest-neighbour interpolation is the chosen resampling
*     method, and the input NDF contains a QUALITY array, then this array
*     is resampled also.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        Identifier for the input NDF.
*     INDF2 = INTEGER (Given)
*        Identifier for the output NDF.
*     IWCSR = INTEGER (Given)
*        AST pointer for the WCS FrameSet defining the reference pixel grid.
*     METHOD = INTEGER (Given)
*        The interpolation method to use when re-sampling the input
*        image; AST__NEAREST, AST__LINEAR, AST__SINCSINC, etc.
*     PARAMS = DOUBLE PRECISION (Given)
*        An optional array containing ay additonal parameter values
*        required by the sub-pixel interpolation scheme.
*     XY1( 2 ) = INTEGER (Given)
*        The indices of the bottom left pixel in the output NDF. If set
*        to VAL__BADI then default bounds will be found for the output
*        NDF.
*     XY2( 2 ) = INTEGER (Given)
*        The indices of the top right pixel in the output NDF. If set
*        to VAL__BADI then default bounds will be found for the output
*        NDF.
*     ERRLIM = REAL (Given)
*        The position accuracy required when re-sampling the input NDF.
*        Given as a number of pixels.
*     MAXPIX = INTEGER (Given)
*        The initial scale size, in pixels, for the adaptive algorithm
*        which approximates non-linear Mappings with piece-wise linear
*        transformations.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     TDCA: Tim Ash (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-OCT-1998 (DSB):
*        Original version, based on IRAS90:SALIA0
*     1-JUL-1999 (TDCA):
*        Modified to use AST_RESAMPLE<X>
*     5-AUG-1999 (DSB):
*        Tidied up.
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

*  Arguments Given:
      INTEGER INDF1 
      INTEGER INDF2
      INTEGER IWCSR
      INTEGER METHOD
      DOUBLE PRECISION PARAMS( 2 )
      INTEGER XY1( 2 )
      INTEGER XY2( 2 )
      REAL ERRLIM
      INTEGER MAXPIX

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER DOMLST*50          ! List of preferred alignment domains
      CHARACTER TY_IN*(NDF__SZTYP) ! Data type for processing
      DOUBLE PRECISION DUMMY( 1 )  ! Dummy array.
      DOUBLE PRECISION PLBND1( 2 ) ! Lower pixel co-ord bounds in input
      DOUBLE PRECISION PLBND2( 2 ) ! Lower pixel co-ord bounds in output
      DOUBLE PRECISION PUBND1( 2 ) ! Upper pixel co-ord bounds in input
      DOUBLE PRECISION PUBND2( 2 ) ! Upper pixel co-ord bounds in output
      DOUBLE PRECISION TOL         ! Max tolerable geometrical distort.
      DOUBLE PRECISION XL( 2 )     ! I/p position of output lower bound
      DOUBLE PRECISION XU( 2 )     ! I/p position of output upper bound
      INTEGER BAD_PIXELS         ! Value returned from AST_RESAMPLE<x>
      INTEGER DIM1( 2 )          ! Indices of significant axes in input NDF
      INTEGER EL                 ! No. of elements in a mapped array
      INTEGER FLAGS              ! Sum of AST__USEBAD and AST__USEVAR
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
      INTEGER IPX                ! Pointer to array holding X image coords
      INTEGER IPY                ! Pointer to array holding Y image coords
      INTEGER IWCS1              ! AST pointer to input WCS FrameSet
      INTEGER IWCS2              ! AST pointer to original output WCS FrameSet
      INTEGER IWCSR2             ! AST pointer to new output WCS FrameSet
      INTEGER LBND1( 2 )         ! Lower bounds of input NDF
      INTEGER LBND2( 2 )         ! Lower bounds of output NDF
      INTEGER LGRID1( 2 )        ! Lower bounds of input grid co-ords
      INTEGER LGRID2( 2 )        ! Lower bounds of output grid co-ords
      INTEGER MAP                ! AST Mapping (i/p PIXEL -> ref. PIXEL)
      INTEGER MAP2               ! AST Mapping (o/p PIXEL -> o/p GRID)
      INTEGER MAP3               ! AST Mapping (ref. GRID -> o/p GRID)
      INTEGER MAP4               ! AST Mapping (i/p GRID -> i/p PIXEL)
      INTEGER MAP5               ! AST Mapping (i/p GRID -> o/p GRID)
      INTEGER MAPR               ! AST Mapping (ref. GRID -> ref. PIXEL)
      INTEGER NDIM               ! No. of dimensions in input NDF
      INTEGER NFRM               ! No. of Frames in input NDF FrameSet
      INTEGER RESULT             ! Dummy value returned from AST_RESAMPLE<x>
      INTEGER UBND1( 2 )         ! Upper bounds of input NDF
      INTEGER UBND2( 2 )         ! Upper bounds of output NDF
      INTEGER UGRID1( 2 )        ! Upper bounds of input grid co-ords
      INTEGER UGRID2( 2 )        ! Upper bounds of output grid co-ords 
      LOGICAL BAD_DV             ! Bad pixels present in DATA/VARIANCE arrays?
      LOGICAL BAD_Q              ! Bad pixels present in QUALITY array?
      LOGICAL DATA               ! Data component present ?
      LOGICAL QUAL               ! Are quality values to be copied?
      LOGICAL VAR                ! Are variance values to be copied?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Find the Mapping from input pixel co-ordinates to reference (i.e.
*  output) pixel co-ordinates.
*  =================================================================

*  Find the index of the PIXEL Frame in the reference NDF.
      CALL KPG1_ASFFR( IWCSR, 'PIXEL', IPIXR, STATUS )

*  Get the WCS FrameSet from the input NDF. Report an error if the NDF is not
*  2-dimensional.
      CALL KPG1_ASGET( INDF1, 2, .TRUE., .FALSE., .TRUE., DIM1, 
     :                 LBND1, UBND1, IWCS1, STATUS )

*  Find the index of the PIXEL Frame in the input NDF.
      CALL KPG1_ASFFR( IWCS1, 'PIXEL', IPIX1, STATUS )

*  Get the Mapping from the input GRID Frame to the input PIXEL Frame
      MAP4 = AST_GETMAPPING( IWCS1, AST__BASE, IPIX1, STATUS )

*  Save the number of Frames in the input WCS FrameSet.
      NFRM = AST_GETI( IWCS1, 'NFRAME', STATUS )

*  Store the list of preferences for the alignment Frame Domain (current
*  FRAME in the input NDF, followed by PIXEL). KPG1_ASMRG always uses the 
*  Domain of the second FrameSet (IWCSR) first, so we do not need to include 
*  it in this list.
      DOMLST = ' '
      IAT = 0
      CALL CHR_APPND( AST_GETC( IWCS1, 'DOMAIN', STATUS ), DOMLST, IAT )
      CALL CHR_APPND( ',PIXEL', DOMLST, IAT )

*  Merge the reference WCS FrameSet into this NDFs WCS FrameSet, aligning
*  them in a suitable Frame (the current Frame of IWCSR by preference, or 
*  the first possible domain in the above list otherwise).
      CALL KPG1_ASMRG( IWCS1, IWCSR, DOMLST( : IAT ), .FALSE., 4, 
     :                 STATUS )

*  Get the simplified Mapping from input pixel Frame to reference (i.e.
*  output) pixel Frame.
      MAP = AST_SIMPLIFY( AST_GETMAPPING( IWCS1, IPIX1, IPIXR + NFRM, 
     :                                    STATUS ), STATUS )
      
*  Set the bounds of the output NDF.
*  =================================

*  If the user supplied explicitly specified bounds for the output
*  images, use them. 
      IF( XY1( 1 ) .NE. VAL__BADI ) THEN
         LBND2( 1 ) = XY1( 1 )
         LBND2( 2 ) = XY1( 2 )
         UBND2( 1 ) = XY2( 1 )
         UBND2( 2 ) = XY2( 2 )

*  Otherwise, find the bounds of the box within the pixel co-ordinate
*  Frame of the reference image which just includes the input image.
      ELSE

*  Store the pixel co-ordinate bounds of the input image.
         PLBND1( 1 ) = DBLE( LBND1( 1 ) - 1 )
         PLBND1( 2 ) = DBLE( LBND1( 2 ) - 1 )
         PUBND1( 1 ) = DBLE( UBND1( 1 ) )
         PUBND1( 2 ) = DBLE( UBND1( 2 ) )

*  Find the axis 1 bounds of the corresponding area in the output image.
         CALL AST_MAPBOX( MAP, PLBND1, PUBND1, .TRUE., 1, PLBND2( 1 ), 
     :                    PUBND2( 1 ), XL, XU, STATUS ) 

*  Find the axis 2 bounds of the corresponding area in the output image.
         CALL AST_MAPBOX( MAP, PLBND1, PUBND1, .TRUE., 2, PLBND2( 2 ), 
     :                    PUBND2( 2 ), XL, XU, STATUS ) 

*  Convert to pixel index bounds.
         LBND2( 1 ) = NINT( PLBND2( 1 ) )
         UBND2( 1 ) = NINT( PUBND2( 1 ) )
         LBND2( 2 ) = NINT( PLBND2( 2 ) )
         UBND2( 2 ) = NINT( PUBND2( 2 ) )

      END IF

*  Report the bounds of the output NDF.
      CALL MSG_SETI( 'LX', LBND2( 1 ) )
      CALL MSG_SETI( 'LY', LBND2( 2 ) )
      CALL MSG_SETI( 'UX', UBND2( 1 ) )
      CALL MSG_SETI( 'UY', UBND2( 2 ) )
      CALL MSG_OUTIF( MSG__VERB, 'KPS1_WALA0_MSG2', '    The output '//
     :                'NDF has bounds ( ^LX:^UX, ^LY:^UY )', STATUS )

*  Change the bounds of the output NDF to the values required to cover
*  all the input data.
      CALL NDF_SBND( 2, LBND2, UBND2, INDF2, STATUS )

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

*  Get bounds of input NDF in grid co-ords.
      LGRID1( 1 ) = 1
      LGRID1( 2 ) = 1
      UGRID1( 1 ) = UBND1( 1 ) - LBND1( 1 ) + 1
      UGRID1( 2 ) = UBND1( 2 ) - LBND1( 2 ) + 1

*  Get bounds of output NDF in grid co-ords.
      LGRID2( 1 ) = 1
      LGRID2( 2 ) = 1
      UGRID2( 1 ) = UBND2( 1 ) - LBND2( 1 ) + 1
      UGRID2( 2 ) = UBND2( 2 ) - LBND2( 2 ) + 1     

*  Map the DATA component of the input and output NDF.
      CALL NDF_TYPE( INDF1, 'DATA', TY_IN, STATUS )

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
      IF ( TY_IN .EQ. '_INTEGER' ) THEN
         BAD_PIXELS = AST_RESAMPLEI( MAP5, 2, LGRID1, UGRID1,
     :                               %VAL( IPD1 ), %VAL( IPV1 ), METHOD, 
     :                               AST_NULL, PARAMS, FLAGS, TOL, 
     :                               MAXPIX, VAL__BADI, 2, LGRID2, 
     :                               UGRID2, LGRID2, UGRID2, 
     :                               %VAL( IPD2 ), %VAL( IPV2 ), 
     :                               STATUS )

      ELSE IF ( TY_IN .EQ. '_REAL' ) THEN
         BAD_PIXELS = AST_RESAMPLER( MAP5, 2, LGRID1, UGRID1,
     :                               %VAL( IPD1 ), %VAL( IPV1 ), METHOD, 
     :                               AST_NULL, PARAMS, FLAGS, TOL, 
     :                               MAXPIX, VAL__BADR, 2, LGRID2, 
     :                               UGRID2, LGRID2, UGRID2, 
     :                               %VAL( IPD2 ), %VAL( IPV2 ), 
     :                               STATUS )

      ELSE IF ( TY_IN .EQ. '_DOUBLE' ) THEN
         BAD_PIXELS = AST_RESAMPLED( MAP5, 2, LGRID1, UGRID1,
     :                               %VAL( IPD1 ), %VAL( IPV1 ), METHOD, 
     :                               AST_NULL, PARAMS, FLAGS, TOL, 
     :                               MAXPIX, VAL__BADD, 2, LGRID2, 
     :                               UGRID2, LGRID2, UGRID2, 
     :                               %VAL( IPD2 ), %VAL( IPV2 ), 
     :                               STATUS )

      ELSE IF ( TY_IN .EQ. '_BYTE' ) THEN 
         BAD_PIXELS = AST_RESAMPLEB( MAP5, 2, LGRID1, UGRID1, 
     :                               %VAL( IPD1 ), %VAL( IPV1 ), METHOD, 
     :                               AST_NULL, PARAMS, FLAGS, TOL, 
     :                               MAXPIX, VAL__BADB, 2, LGRID2, 
     :                               UGRID2, LGRID2, UGRID2, 
     :                               %VAL( IPD2 ), %VAL( IPV2 ), 
     :                               STATUS )

      ELSE IF ( TY_IN .EQ. '_UBYTE' ) THEN
         BAD_PIXELS = AST_RESAMPLEUB( MAP5, 2, LGRID1, UGRID1,
     :                               %VAL( IPD1 ), %VAL( IPV1 ), METHOD, 
     :                               AST_NULL, PARAMS, FLAGS, TOL, 
     :                               MAXPIX, VAL__BADUB, 2, LGRID2, 
     :                               UGRID2, LGRID2, UGRID2, 
     :                               %VAL( IPD2 ), %VAL( IPV2 ), 
     :                               STATUS )

      ELSE IF ( TY_IN .EQ. '_WORD' ) THEN 
         BAD_PIXELS = AST_RESAMPLEW( MAP5, 2, LGRID1, UGRID1,
     :                               %VAL( IPD1 ), %VAL( IPV1 ), METHOD, 
     :                               AST_NULL, PARAMS, FLAGS, TOL, 
     :                               MAXPIX, VAL__BADW, 2, LGRID2, 
     :                               UGRID2, LGRID2, UGRID2, 
     :                               %VAL( IPD2 ), %VAL( IPV2 ), 
     :                               STATUS )

      ELSE IF ( TY_IN .EQ. '_UWORD' ) THEN 
         BAD_PIXELS = AST_RESAMPLEUW( MAP5, 2, LGRID1, UGRID1,
     :                               %VAL( IPD1 ), %VAL( IPV1 ), METHOD, 
     :                               AST_NULL, PARAMS, FLAGS, TOL, 
     :                               MAXPIX, VAL__BADUW, 2, LGRID2, 
     :                               UGRID2, LGRID2, UGRID2, 
     :                               %VAL( IPD2 ), %VAL( IPV2 ), 
     :                               STATUS )

      ELSE 
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TY', TY_IN )
         CALL ERR_REP( 'WCSALIGN_UTYP', 'KPS1_WALA0: Unsupported data'//
     :                 ' type ''^TY'' (programming error).', STATUS )
      END IF

*  Set the bad pixel flags for the output DATA and VARIANCE arrays.
      IF ( RESULT .GT. 0 ) THEN 
         CALL NDF_SBAD( .TRUE., INDF2, 'DATA', STATUS )
         CALL NDF_SBAD( .TRUE., INDF2, 'VARIANCE', STATUS )
      END IF

*  Resample QUALITY arrays if appropriate
      CALL NDF_STATE( INDF1, 'QUAL', QUAL, STATUS )
      IF ( ( METHOD .EQ. AST__NEAREST ) .AND. QUAL ) THEN

*  Map the QUALITY component.
         CALL NDF_MAP( INDF1, 'QUAL', TY_IN, 'READ', IPQ1, EL, 
     :                 STATUS )
         CALL NDF_MAP( INDF2, 'QUAL', TY_IN, 'WRITE', IPQ2, EL,
     :                 STATUS )

*  Reset FLAGS (QUALITY arrays cannot have bad pixels).
         FLAGS = 0

*  Do the resampling.
         RESULT = AST_RESAMPLEUW( MAP5, 2, LGRID1, UGRID1, %VAL( IPQ1 ), 
     :                            DUMMY, METHOD, AST_NULL, PARAMS, 
     :                            FLAGS, TOL, MAXPIX, VAL__BADUW, 2, 
     :                            LGRID2, UGRID2, LGRID2, UGRID2, 
     :                            %VAL( IPQ2 ), DUMMY, STATUS )
      
      END IF

*  End the AST context.
      CALL AST_END( STATUS )

      END
