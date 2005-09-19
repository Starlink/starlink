      SUBROUTINE KPS1_WMOS0( INDFR, IGRP, NDIM, LBND, UBND, USEVAR, 
     :                       MAPS, STATUS )
*+
*  Name:
*     KPS1_WMOS0

*  Purpose:
*     Extract required global information from a group of input NDFs for
*     WCSMOSAIC.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_WMOS0( INDFR, IGRP, NDIM, LBND, UBND, USEVAR, MAPS, STATUS )

*  Description:
*     This routine extracts the global information required by WCSMOSAIC
*     from the supplied group of input NDFs.

*  Arguments:
*     INDFR = INTEGER (Given)
*        The NDF identifier for the reference NDF.
*     IGRP = INTEGER (Given)
*        The GRP identifier for the group containing the input NDF names.
*     NDIM
*        The number of pixel axes in the output NDF.
*     LBND( NDIM ) = INTEGER (Returned)
*        The lower pixel index bounds for the output NDF so that the output NDF just
*        encompasses all the input data.
*     UBND( NDIM ) = INTEGER (Returned)
*        The upper pixel index bounds for the output NDF so that the output NDF just
*        encompasses all the input data.
*     USEVAR = LOGICAL (Returned)
*        Returned .TRUE. if and only if all the input NDFs have defined
*        Variance components.
*     MAPS( * ) = INTEGER (Returned)
*        An array in which to store AST identifiers for the pixel_in to
*        pixel_out Mapping for each input NDF. The array should have the
*        same number of elements as the supplied group of input NDFs (IGRP).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-SEP-2005 (DSB):
*        Original version.
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
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER INDFR
      INTEGER IGRP 
      INTEGER NDIM

*  Arguments Returned:
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )
      LOGICAL USEVAR
      INTEGER MAPS( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER KPG1_CEIL
      INTEGER KPG1_FLOOR

*  Local Variables:
      CHARACTER DOMLST*50        ! List of preferred alignment domains
      DOUBLE PRECISION ALBND     ! Lower axis bound
      DOUBLE PRECISION AUBND     ! Upper axis bound
      DOUBLE PRECISION DLBND1( NDF__MXDIM )! Lower bounds of input NDF
      DOUBLE PRECISION DUBND1( NDF__MXDIM )! Upper bounds of input NDF
      DOUBLE PRECISION XL( NDF__MXDIM )! Input position at lower bound
      DOUBLE PRECISION XU( NDF__MXDIM )! Input position at upper bound
      INTEGER I                  ! Loop count
      INTEGER IAT                ! No. of characters in string
      INTEGER IL                 ! Integer lower bound
      INTEGER INDF1              ! Input NDF identifier
      INTEGER IPIX1              ! Index of PIXEL Frame in input NDF FrameSet
      INTEGER IPIXR              ! Index of PIXEL Frame in ref. NDF FrameSet
      INTEGER IU                 ! Integer upper bound
      INTEGER IWCS1              ! AST pointer to input WCS FrameSet
      INTEGER IWCSR              ! AST pointer to reference WCS FrameSet
      INTEGER J                  ! Axis count
      INTEGER LBND1( NDF__MXDIM )! Lower bounds of input NDF
      INTEGER NDIM1              ! No. of pixel axes in input NDF 
      INTEGER NFRM               ! No. of Frames in input NDF FrameSet
      INTEGER SIZE               ! No. of input NDFs
      INTEGER UBND1( NDF__MXDIM )! Upper bounds of input NDF
      REAL RLBND                 ! Lower axis bound
      REAL RUBND                 ! Upper axis bound

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context
      CALL AST_BEGIN( STATUS )

*  Get the WCS FrameSet from the reference NDF. This will be inherited by
*  the output NDF.
      CALL KPG1_GTWCS( INDFR, IWCSR, STATUS )

*  Find the index of the PIXEL Frame in the reference NDF.
      CALL KPG1_ASFFR( IWCSR, 'PIXEL', IPIXR, STATUS )

*  Initialise the returned values.
      DO J = 1, NDIM
         LBND( J ) = VAL__MAXI
         UBND( J ) = VAL__MINI
      END DO
      USEVAR = .TRUE.

*  Loop round each NDF to be processed.
      CALL GRP_GRPSZ( IGRP, SIZE, STATUS )
      DO I = 1, SIZE
         CALL NDG_NDFAS( IGRP, I, 'Read', INDF1, STATUS )

*  We do not use variances if any input NDF has no Variance component.
         IF( USEVAR ) CALL NDF_STATE( INDF1, 'VARIANCE', USEVAR, 
     :                                STATUS )

*  Get the WCS FrameSet from the current input NDF.
         CALL KPG1_GTWCS( INDF1, IWCS1, STATUS )

*  Find the index of the PIXEL Frame in the input NDF.
         CALL KPG1_ASFFR( IWCS1, 'PIXEL', IPIX1, STATUS )

*  Save the number of Frames in the input WCS FrameSet.
         NFRM = AST_GETI( IWCS1, 'NFRAME', STATUS )

*  Store the list of preferences for the alignment Frame Domain (current
*  FRAME in the input NDF, followed by PIXEL). KPG1_ASMRG always uses the 
*  Domain of the second FrameSet (IWCSR) first, so we do not need to include 
*  it in this list.
         DOMLST = ' '
         IAT = 0
         CALL CHR_APPND( AST_GETC( IWCS1, 'DOMAIN', STATUS ), DOMLST, 
     :                   IAT )
         CALL CHR_APPND( ',PIXEL', DOMLST, IAT )

*  Merge the reference WCS FrameSet into this NDFs WCS FrameSet, aligning
*  them in a suitable Frame (the current Frame of IWCSR by preference, or 
*  the first possible domain in the above list otherwise).
         CALL KPG1_ASMRG( IWCS1, IWCSR, DOMLST( : IAT ), .TRUE., 4, 
     :                    STATUS )

*  Get the simplified Mapping from input pixel Frame to reference (i.e.
*  output) pixel Frame.
         MAPS( I ) = AST_SIMPLIFY( AST_GETMAPPING( IWCS1, IPIX1, 
     :                                           IPIXR + NFRM, STATUS ), 
     :                             STATUS )

*  Export the AST pointer to the parent AST context.
         CALL AST_EXPORT( MAPS( I ), STATUS )

*  Get the pixel index bounds of this input NDF, and convert to double
*  precision pixel coordinates which have inegral values at the centre of
*  each pixel. Note, this is different to the usual Starlink convention
*  for pixel coordinates which has integral values at the edges of each
*  pixel, but it is the convention required by AST_REBINSEQ.
         CALL NDF_BOUND( INDF1, NDF__MXDIM, LBND1, UBND1, NDIM1, 
     :                   STATUS )
         DO J = 1, NDIM
            DLBND1( J ) = DBLE( LBND1( J ) ) - 0.5D0
            DUBND1( J ) = DBLE( UBND1( J ) ) + 0.5D0
         END DO

*  Extend the output pixel bounds to encompass this input image.
         DO J = 1, NDIM
            CALL AST_MAPBOX( MAPS( I ), DLBND1, DUBND1, .TRUE., J,
     :                       ALBND, AUBND, XL, XU, STATUS )
            RLBND = REAL( ALBND )
            RUBND = REAL( AUBND )
            IU = KPG1_FLOOR( RUBND )
            IL = KPG1_CEIL( RLBND )
            IF( IU .GT. UBND( J ) ) UBND( J ) = IU
            IF( IL .LT. LBND( J ) ) LBND( J ) = IL
         END DO

*  Annul the input NDF identifier.
         CALL NDF_ANNUL( INDF1, STATUS )

*  If an error occurred processing the current input NDF, abort.
         IF( STATUS .NE. SAI__OK  ) GO TO 999

      END DO

 999  CONTINUE

*  End the AST context
      CALL AST_END( STATUS )

      END
