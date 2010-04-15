      SUBROUTINE PREPB1( INDF, NCARD, FITS, TYPE, PROJ, SCS, FLDLON,
     :                   FLDLAT, A, B, XFLIP, YFLIP, PIXSIZ, STATUS )
*+
*  Name:
*     PREPB1

*  Purpose:
*     Create an astrometry structure within the output NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREPB1( INDF, NCARD, FITS, TYPE, PROJ, SCS, FLDLON, FLDLAT,
*                  A, B, XFLIP, YFLIP, PIXSIZ, STATUS )

*  Description:
*     This routine creates an astrometry structure within the output
*     NDF. It uses the FITS keywords CRVAL1, CRVAL2, CRPIX1, CRPIX2,
*     CDELT1, CDELT2, CROTA1 and CROTA2 as the basis for the projection
*     parameters P1 to P7 (P8 is always set to zero). Certain image
*     types use different conventions for these values, and some images
*     are stored reflected about various axes (relative to a standard
*     IRAS90 image). Decisions on which axes to reflect about and on the
*     sign conventions to use for the parameters are determined by the
*     supplied image type. Unknown images are assumed to use the same
*     conventions as IRAS90, and to be in the projection specified by
*     parameter PROJ, and to have CRVAL1 and CRVAL2 values in the sky
*     coordinate system specified by argument SCS. This routine does not
*     actually perform the required reflections, instead it returns
*     information describing what reflections are required.

*  Arguments:
*     INDF = INTEGER (Given)
*        The identifier for the output NDF.
*     NCARD = INTEGER (Given)
*        The number of FITS header cards.
*     FITS( NCARD ) = CHARACTER * ( * ) (Given)
*        The FITS header cards.
*     TYPE = CHARACTER * ( * ) (Given)
*        The image type. This should be equal to one of the symbolic
*        constants defined within the IRI subsystem.
*     PROJ = CHARACTER * ( * ) (Given)
*        The projection type to assume if the input image type is
*        unrecognised.
*     SCS = CHARACTER * ( * ) (Given)
*        The sky coordinate system to which FLDLON and FLDLAT refer,
*        and in which A an B are returned.  This is also the sky
*        coordinate system assumed for the CRVAL1 and CRVAL2 keywords
*        if the input image type is unrecognised.
*     FLDLON = CHARACTER * ( * ) (Given)
*        The longitude value to be stored as the field position (as a
*        formatted string).
*     FLDLAT = CHARACTER * ( * ) (Given)
*        The latitude value to be stored as the field position (as a
*        formatted string).
*     A = DOUBLE PRECISION (Returned)
*        The longitude to use for the field position. This is taken from
*        FLDLON if FLDLON is not blank, and CRVAL1 otherwise. The value
*        refers to the sky coordinate system specified by SCS.
*     B = DOUBLE PRECISION (Returned)
*        The latitude to use for the field position. This is taken from
*        FLDLAT if FLDLAT is not blank, and CRVAL2 otherwise. The value
*        refers to the sky coordinate system specified by SCS.
*     XFLIP = LOGICAL (Returned)
*        True if the output image needs to be reflected in the X
*        direction in order to make it conform to the IRAS90 convention
*        of rotation from north to east being anti-clockwise when the
*        image is viewed normally.
*     YFLIP = LOGICAL (Returned)
*        True if the output image needs to be reflected in the Y
*        direction in order to make it conform to the IRAS90 convention
*        of rotation from north to east being anti-clockwise when the
*        image is viewed normally.
*     PIXSIZ = DOUBLE PRECISION (Returned)
*        The nominal pixel area, in steradians.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-DEC-1992 (DSB):
*        Original version.
*     4-FEB-1993 (DSB):
*        Flip in Y direction for ISSA images removed.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants.
      INCLUDE 'IRI_PAR'          ! IRI_ constants.

*  Arguments Given:
      INTEGER INDF
      INTEGER NCARD
      CHARACTER FITS( NCARD )*(*)
      CHARACTER TYPE*(*)
      CHARACTER PROJ*(*)
      CHARACTER SCS*(*)
      CHARACTER FLDLON*(*)
      CHARACTER FLDLAT*(*)

*  Arguments Returned:
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      LOGICAL XFLIP
      LOGICAL YFLIP
      DOUBLE PRECISION PIXSIZ

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER PROJIN*(IRA__SZPRJ)! Projection used when creating the
                                  ! input.
      CHARACTER SCSIN*(IRA__SZSCS)! Sky Coordinate System in which
                                  ! CRVAL1,2 are stored in the input.


      DOUBLE PRECISION P(8)       ! Projection parameters.


      INTEGER CARD                ! Card number within FITS header.
      INTEGER IDA                 ! IRA identifier for astrometry info.


      LOGICAL ROTDIR              ! True if CROTA1,2 are in same sense
                                  ! as projection parameter P(7).
      LOGICAL THERE               ! True if FITS keyword was found.


      REAL CDELT1                 ! Value of FITS keyword CDELT1.
      REAL CDELT2                 ! Value of FITS keyword CDELT2.
      REAL CROTA1                 ! Value of FITS keyword CROTA1.
      REAL CROTA2                 ! Value of FITS keyword CROTA2.
      REAL CRPIX1                 ! Value of FITS keyword CRPIX1.
      REAL CRPIX2                 ! Value of FITS keyword CRPIX2.
      REAL CRVAL1                 ! Value of FITS keyword CRVAL1.
      REAL CRVAL2                 ! Value of FITS keyword CRVAL2.
      REAL EQU                    ! Epoch of reference equinox.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      ROTDIR = .FALSE.

*  Get the value of FITS keyword EPOCH (the epoch of the reference
*  equinox, not of the observations).
      CALL IRM_GKEYR( NCARD, FITS, 1, 'EPOCH', THERE, EQU, CARD,
     :                STATUS )

*  If the keyword EPOCH does not exit, set a default value for the
*  equinox.
      IF( .NOT. THERE ) EQU = 1950.0

*  Get values for FITS keywords CRVAL1, CRVAL2, CRPIX1, CRPIX2, CDELT1,
*  CDELT2.
      CALL IRM_GKEYR( NCARD, FITS, 1, 'CRVAL1', THERE, CRVAL1, CARD,
     :             STATUS )
      CALL IRM_GKEYR( NCARD, FITS, 1, 'CRVAL2', THERE, CRVAL2, CARD,
     :             STATUS )
      CALL IRM_GKEYR( NCARD, FITS, 1, 'CRPIX1', THERE, CRPIX1, CARD,
     :             STATUS )
      CALL IRM_GKEYR( NCARD, FITS, 1, 'CRPIX2', THERE, CRPIX2, CARD,
     :             STATUS )
      CALL IRM_GKEYR( NCARD, FITS, 1, 'CDELT1', THERE, CDELT1, CARD,
     :             STATUS )
      CALL IRM_GKEYR( NCARD, FITS, 1, 'CDELT2', THERE, CDELT2, CARD,
     :             STATUS )

*  Get a value for FITS keyword CROTA1. If it does not exist use zero.
      CALL IRM_GKEYR( NCARD, FITS, 1, 'CROTA1', THERE, CROTA1, CARD,
     :             STATUS )
      IF( .NOT. THERE ) CROTA1 = 0.0

*  Get a value for FITS keyword CROTA2. If it does not exist use zero.
      CALL IRM_GKEYR( NCARD, FITS, 1, 'CROTA2', THERE, CROTA2, CARD,
     :             STATUS )
      IF( .NOT. THERE ) CROTA2 = 0.0

*  Among the eight projection parameters defined in the IRA system, P1
*  to P6 are related to FITS keywords, CRVAL1, CRVAL2, CRPIX1, CRPIX2,
*  CDELT1 and CDELT2 in the same way for all kinds of images.
      P( 1 ) = DBLE( CRVAL1 ) * IRA__DTOR
      P( 2 ) = DBLE( CRVAL2 ) * IRA__DTOR
      P( 3 ) = DBLE( CRPIX1 ) - 0.5D0
      P( 4 ) = DBLE( CRPIX2 ) - 0.5D0
      P( 5 ) = DBLE( ABS( CDELT1 ) ) * IRA__DTOR
      P( 6 ) = DBLE( ABS( CDELT2 ) ) * IRA__DTOR

*  Get projection type, sky coordinate type and rotation angle direction
*  for different kinds of image.
      IF( TYPE .EQ. IRI__ISSA ) THEN
         SCSIN = 'EQUATORIAL'
         CALL IRA_SETEQ( DBLE( EQU ), 'B', SCSIN, STATUS )
         PROJIN = 'GNOMONIC'

*  The rotation angle of ISSA is not defind in the FITS header. Regard
*  it as having a rotation of zero (i.e. north upwards) and the
*  definition of positive rotation being the same as that in IRA system
*  (north through east).
         ROTDIR = .TRUE.

*  The first ("X") axis of the input ISSA plate is in the direction of
*  decreasing RA, which is the same as the X axis definition of IRA
*  system, so no flipping is required for X axis of the image.
         XFLIP = .FALSE.

*  The second ("Y") axis of the input ISSA plate is in the direction of
*  increasing DEC, which is the same as the Y axis definition of IRA
*  system, so the Y axis of the output image need not be flipped.
         YFLIP = .FALSE.

*  Now do the same for SKYFLUX images.
      ELSE IF( TYPE .EQ. IRI__SKYFL ) THEN
         SCSIN = 'EQUATORIAL'
         CALL IRA_SETEQ( DBLE( EQU ), 'B', SCSIN, STATUS )
         PROJIN = 'GNOMONIC'
         ROTDIR = .TRUE.
         XFLIP = .FALSE.

*  The second ("Y") axis of the input SKYFLUX plate is in the direction
*  of decreasing DEC, which is opposite to the Y axis definition of IRA
*  system, so the Y axis of the output image needs to be flipped.
         YFLIP = .TRUE.

*  Now do the same for galactic plane maps.
      ELSE IF( TYPE .EQ. IRI__GALPL ) THEN
         SCSIN = 'GALACTIC'
         PROJIN = 'LAMBERT'
         ROTDIR = .TRUE.
         XFLIP = .FALSE.
         YFLIP = .FALSE.

*  Now do the same for all sky maps.
      ELSE IF( TYPE .EQ. IRI__ALLSK ) THEN
         SCSIN = 'GALACTIC'
         PROJIN = 'AITOFF'
         ROTDIR = .TRUE.
         XFLIP = .FALSE.
         YFLIP = .FALSE.

*  Now do the same for PO images, CPC images and images created using
*  the IPAC "YORIC" processor.
      ELSE IF( TYPE .EQ. IRI__DSCO .OR. TYPE .EQ. IRI__CPC .OR.
     :         TYPE .EQ. IRI__YORIC ) THEN
         SCSIN = 'EQUATORIAL'
         CALL IRA_SETEQ( DBLE( EQU ), 'B', SCSIN, STATUS )
         PROJIN = 'ORTHOGRAPHIC'

*  The definition of the positive rotation direction of the DEEPSKY,
*  IRAS-CPC and YORIC images is opposite to that of IRAS sytem.
         ROTDIR = .FALSE.

*  X axes (Z-axis) of the unprepared DEEPSKY, IRAS-CPC and YORIC are in
*  the direction of decreasing RA, no flipping is required for this
*  axis.
         XFLIP = .FALSE.

*  Y axes of these kinds of images is in the direction of increasing
*  DEC, no flipping is required for this axis as well.
         YFLIP = .FALSE.

*  If the image is unknown kind, default values are used.
      ELSE IF( TYPE .EQ. IRI__NONAM ) THEN

         CALL MSG_SETC( 'S', SCS )
         CALL MSG_OUTIF( MSG__QUIET, 'PREPB1_MSG1',
     :         '    Assuming FITS keywords CRVAL1 (etc) are ^S values.',
     :                   STATUS )

         SCSIN = SCS
         PROJIN = PROJ
         ROTDIR = .TRUE.
         XFLIP = .FALSE.
         YFLIP = .FALSE.

      END IF

*  Projection parameter no. 7 is the angle from north to the positive Y
*  axis, in the same sense as rotation from north to east. This may be
*  the same as CROTA or the oposite of CROTA. If it is the same...
      IF( ROTDIR ) THEN

*  In some case only one axis rotation is specified in the FITS header
*  leaving the other unspecified or being 0, so take the one which is
*  not zero.
         IF( CROTA1 .EQ. 0.0  ) THEN
            P( 7 ) = DBLE( CROTA2 ) * IRA__DTOR
         ELSE
            P( 7 ) = DBLE( CROTA1 ) * IRA__DTOR
         END IF

*  If CROTA is the opposite of p7...
      ELSE
         IF( CROTA1 .EQ. 0.0 ) THEN
            P( 7 ) = -DBLE( CROTA2 ) * IRA__DTOR
         ELSE
            P( 7 ) = -DBLE( CROTA1 ) * IRA__DTOR
         END IF
      END IF

*  For all IRAS images, projection parameter P8 is zero.
      P( 8 ) = 0.0D0

*  Create an Astrometry structure within the output NDF, and then annul
*  the identifier to it.
      CALL IRA_CREAT( PROJIN, 8, P, SCSIN, IRA__IRJEP, INDF, IDA,
     :                STATUS )
      CALL IRA_ANNUL( IDA, STATUS )

*  Return the pixel size in steradians.
      PIXSIZ = P( 5 )*P( 6 )

*  If text was supplied for both FLDLON adn FLDLAT, convert the strings
*  to numeric values and return in A and B.
      IF( FLDLON .NE. ' ' .AND. FLDLAT .NE. ' ' ) THEN
         CALL IRA_CTOD( FLDLON, FLDLAT, SCS, A, B, STATUS )

*  Otherwise, convert the projection parameters P1 and P2 to the sky
*  coordinate  system specified by SCS.
      ELSE
         CALL IRA_CONVT( 1, P(1), P(2), SCSIN, SCS, IRA__IRJEP, A, B,
     :                   STATUS )
      END IF

*  If an error has occurred, give a context message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PREPB1_ERR1',
     :'PREPB1: Unable to add astrometry information to the output NDF.',
     :                 STATUS )
      END IF

      END
