      SUBROUTINE SALIA0( INDF1, INDF2, IDAR, SCS, METHOD, XY1, XY2,
     :                   ERRLIM, STATUS )
*+
*  Name:
*     SALIA0

*  Purpose:
*     Process a single pair of input and output NDFs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SALIA0( INDF1, INDF2, IDAR, SCS, METHOD, XY1, XY2, ERRLIM,
*                  STATUS )

*  Description:
*     This routine modifies the output NDF by over-writing any existing
*     astrometry information with the supplied reference astrometry
*     information. It changes the bounds of the output NDF to those
*     supplied by the user. If no bounds were supplied by the user then
*     default bounds are found by explicitly transforming a grid of
*     points in the input NDF to the output image coordinate system. The
*     extreme coordinates of the transformed points are used as the
*     output NDF bounds. Two work arrays are then created which have
*     the same shape as the output NDF. These arrays are filled with the
*     X and Y image coordinates in the input NDF corresponding to the
*     centre of each output pixel. Finally, the input data array is
*     re-sampled using the coordinates held in the work arrays.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        Identifier for the input NDF.
*     INDF2 = INTEGER (Given)
*        Identifier for the output NDF.
*     IDAR = INTEGER (Given)
*        IRA identifier for the astrometry information defining the
*        reference pixel grid.
*     SCS = CHARACTER * ( * ) (Given)
*        The sky coordinate system used by the reference astrometry
*        information.
*     METHOD = CHARACTER * ( * ) (Given)
*        The interpolation method to use when re-sampling the input
*        image; BILINEAR or NEAREST.
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
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-FEB-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants

*  Arguments Given:
      INTEGER INDF1
      INTEGER INDF2
      INTEGER IDAR
      CHARACTER SCS*(*)
      CHARACTER METHOD*(*)
      INTEGER XY1( 2 )
      INTEGER XY2( 2 )
      REAL ERRLIM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER
     :        ASLOC*(DAT__SZLOC),  ! Locator to i/p astrometry structure
     :        ASNAME*(DAT__SZNAM), ! Name of i/p astrometry structure
     :        XNAME*(DAT__SZNAM),  ! Name of NDF extension holding the
     :                             ! input astrometry structure
     :        XLOC*(DAT__SZLOC)    ! Locator to NDF extension holding
                                   ! the input astrometry structure

      INTEGER
     :        EL,                  ! No. of elements in a mapped array.
     :        IDA1,                ! IRA identifier for input astrometry
     :                             ! information.
     :        IPD1,                ! Pointer to input data array.
     :        IPD2,                ! Pointer to output data array.
     :        IPQ1,                ! Pointer to input quality array.
     :        IPQ2,                ! Pointer to output quality array.
     :        IPV1,                ! Pointer to input variance array.
     :        IPV2,                ! Pointer to output variance array.
     :        IPX,                 ! Pointer to array holding X image
                                   ! coordinates for each output pixel.
     :        IPY,                 ! Pointer to array holding Y image
                                   ! coordinates for each output pixel.
     :        LBND1( 2 ),          ! Lower bounds of input NDF.
     :        LBND2( 2 ),          ! Lower bounds of output NDF.
     :        NDIM,                ! No. of dimensions in input NDF.
     :        UBND1( 2 ),          ! Upper bounds of input NDF.
     :        UBND2( 2 )           ! Upper bounds of output NDF.

      LOGICAL
     :        QUAL,                ! True if quality values are to be
     :                             ! copied from input to output.
     :        THERE,               ! True if astrometry found.
     :        VAR                  ! True if variance values are to be
     :                             ! copied from input to output.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the location of the astrometry information within the input
*  NDF.
      CALL IRA_FIND( INDF1, THERE, XNAME, ASNAME, XLOC, STATUS )

*  If no astrometry information was found, report an error.
      IF( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SALIA0_ERR1',
     :         'SALIA0: No astrometry information found in input NDF',
     :                 STATUS )
      END IF

*  Read the astrometry information from the input NDF and get an IRA
*  identifier for it.
      CALL DAT_FIND( XLOC, ASNAME, ASLOC, STATUS )
      CALL IRA_READ( ASLOC, IDA1, STATUS )

*  Annul the HDS locators to the extension and the astrometry structure.
      CALL DAT_ANNUL( XLOC, STATUS )
      CALL DAT_ANNUL( ASLOC, STATUS )

*  Get the bounds of the input NDF.
      CALL NDF_BOUND( INDF1, 2, LBND1, UBND1, NDIM, STATUS )

*  If the user supplied explicitly specified bounds for the output
*  images, use them. Otherwise, calculate bounds by transforming an
*  array of test points from input image coordinates to output image
*  coordinates.
      IF( XY1( 1 ) .NE. VAL__BADI ) THEN
         LBND2( 1 ) = XY1( 1 )
         LBND2( 2 ) = XY1( 2 )
         UBND2( 1 ) = XY2( 1 )
         UBND2( 2 ) = XY2( 2 )
      ELSE
         CALL SALIA2( LBND1, UBND1, IDA1, IDAR, SCS, LBND2, UBND2,
     :                STATUS )
      END IF

*  Report the bounds of the output NDF.
      CALL MSG_SETI( 'LX', LBND2( 1 ) )
      CALL MSG_SETI( 'LY', LBND2( 2 ) )
      CALL MSG_SETI( 'UX', UBND2( 1 ) )
      CALL MSG_SETI( 'UY', UBND2( 2 ) )
      CALL MSG_OUTIF( MSG__VERB, 'SALIA0_MSG2',
     :   '    The output NDF has bounds ( ^LX:^UX, ^LY:^UY )', STATUS )

*  Change the bounds of the output NDF to the values required to cover
*  all the input data.
      CALL NDF_SBND( 2, LBND2, UBND2, INDF2, STATUS )

*  Ensure that the output astrometry information will be created at the
*  same location within the NDF as the input astrometry information was
*  found.
      CALL IRA_LOCAT( XNAME, ASNAME, STATUS )

*  Export the reference astrometry information to the output NDF. This
*  overwrites the astrometry information propagated from the input.
      CALL IRA_EXPRT( IDAR, INDF2, STATUS )

*  Create two temporary arrays the same size as the output NDF. These
*  will hold the X and Y images coordinates within the input NDF which
*  correspond to the centre of each output pixel.
      EL = ( UBND2( 1 ) - LBND2( 1 ) + 1 )*
     :     ( UBND2( 2 ) - LBND2( 2 ) + 1 )
      CALL PSX_CALLOC( EL, '_REAL', IPX, STATUS )
      CALL PSX_CALLOC( EL, '_REAL', IPY, STATUS )

*  Fill these arrays with the required image coordinates.
      CALL SALIA3( IDAR, IDA1, SCS, LBND2( 1 ), UBND2( 1 ), LBND2( 2 ),
     :             UBND2( 2 ), ERRLIM, %VAL( IPX ), %VAL( IPY ),
     :             STATUS )

*  Map the DATA component of the input and output NDF.
      CALL NDF_MAP( INDF1, 'DATA', '_REAL', 'READ', IPD1, EL, STATUS )
      CALL NDF_MAP( INDF2, 'DATA', '_REAL', 'WRITE', IPD2, EL, STATUS )

*  See if the input has a defined VARIANCE component.
      CALL NDF_STATE( INDF1, 'VAR', VAR, STATUS )

*  See if the input has a defined QUALITY component.
      CALL NDF_STATE( INDF1, 'QUAL', QUAL, STATUS )

*  We have to face the question of whether or not to copy VARIANCE and
*  QUALITY values from the input to the output. If bilinear
*  interpolation is being used, each output data values is contributed
*  to by four input data values. In this case, it is not obvious how to
*  assign quality or variance values to the output pixels. Get round
*  this problem by only copying quality and variance if nearest
*  neighbour interpolation is being used.
      IF( METHOD .EQ. 'BILINEAR' ) THEN
         VAR = .FALSE.
         QUAL = .FALSE.
      END IF

*  If required map the VARIANCE component of the input and output
*  NDFs.
      IF( VAR ) THEN
         CALL NDF_MAP( INDF1, 'VAR', '_REAL', 'READ', IPV1, EL, STATUS )
         CALL NDF_MAP( INDF2, 'VAR', '_REAL', 'WRITE', IPV2, EL,
     :                 STATUS )
      END IF

*  If required map the QUALITY component of the input and output NDF.
      IF( QUAL ) THEN
         CALL NDF_MAP( INDF1, 'QUAL', '_UBYTE', 'READ', IPQ1, EL,
     :                 STATUS )
         CALL NDF_MAP( INDF2, 'QUAL', '_UBYTE', 'WRITE', IPQ2, EL,
     :                 STATUS )
      END IF

*  Fill the output arrays by resampling the input arrays.
      CALL SALIA4( QUAL, VAR, METHOD, LBND2( 1 ), UBND2( 1 ),
     :             LBND2( 2 ), UBND2( 2 ), LBND1( 1 ), UBND1( 1 ),
     :             LBND1( 2 ), UBND1( 2 ), %VAL( IPD1 ), %VAL( IPV1 ),
     :             %VAL( IPQ1 ), %VAL( IPX ), %VAL( IPY ), %VAL( IPD2 ),
     :             %VAL( IPV2 ), %VAL( IPQ2 ), STATUS )

*  Release the temporary arrays used to hold input coordinates.
      CALL PSX_FREE( IPX, STATUS )
      CALL PSX_FREE( IPY, STATUS )

*  Annul the IDA identifiers for input astrometry information.
      CALL IRA_ANNUL( IDA1, STATUS )

      END
