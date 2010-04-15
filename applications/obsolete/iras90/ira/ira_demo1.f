      SUBROUTINE IRA_DEMO1( STATUS )
*+
*  Name:
*     IRA_DEMO1

*  Purpose:
*     A demonstration program for the IRA package.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL IRA_DEMO1( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This program creates an image of an articial sky consisting of a
*     patchwork of black and white "squares". Each "square" ia an area
*     with sides parallel to the two sky coordinate axes. The user
*     provides the required projection, sky coordinate system, sky
*     coordinates for the centre of the image, pixel size, the
*     arc-length of each image axis, the size of a single patch and the
*     orientation of the image. It then finds the dimensions (in
*     pixels) of a primative NDF which will just cover the required
*     area, creates the NDF and stores an astrometry structure in it.
*     Program IRA_TEST2 reads in such an image and uses the astrometry
*     structure to calculate the sky coordinates of given pixels, or
*     vica-versa.

*  ADAM Parameters:
*     SCS = LITERAL (Read)
*        The name of the sky coordinate system required for the output
*        image. See routine IRA_ISCS.
*     PROJ = LITERAL (Read)
*        The name of the projection required for the output image.
*     SQUARE( 2 ) = _REAL (Read)
*        The size of a single patchwork square in the output image,
*        given in degrees. Two values can be entered, giving the size
*        parallel to the two sky axes. If only one value is supplied,
*        it is used for both axes.
*     ACENTRE = LITERAL (Read)
*        The sky longitude at the image centre. See routine IRA_GETCO.
*     BCENTRE = LITERAL (Read)
*        The sky latitude at the image centre. See routine IRA_GETCO.
*     SIZE( 2 ) = _REAL (Read)
*        The length of each axis of the output image, given in degrees.
*        Two values can be entered, giving the width and height of the
*        output image. If only one value is supplied, it is used for
*        both width and height.
*     PIXSIZE( 2 ) = _REAL (Read)
*        The size of an image pixel given in degrees. Two values can
*        be entered, giving the size parallel to the two image axes.
*        If only one value is supplied, it is used for both axes. Note,
*        for most projections, the pixel size varies across the image.
*        The values given for this parameter determine the pixel size at
*        the image centre. Pixels at the edge will in general have a
*        different size.
*     ORIENT = _REAL (Read)
*        The position angle on the sky of the second ("Y") image axis,
*        measured from north through east, in degrees. The X axis is at
*        position angle (ORIENT-PI/2)
*     OUT = NDF (Write)
*        The output NDF. This is in primative storage form and thus can
*        be directly viewed using KAPPA. This means that the lower pixel
*        bound of each image axis is forced to be 1.
*     TILT = _REAL (Read)
*        The angle through which to tilt the celestial sphere before
*        applying the projection, in degrees. The tilt is in the
*        anti-clockwise direction when looking at the centre of the
*        celestial sphere from the reference point.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-JAN-1991 (DSB):
*        Original version.
*     2-MAY-1991 (DSB):
*        Modified for IRA version 2.
*     11-SEP-1992 (DSB):
*        P( 8 ) added.
*     25-FEB-1993 (DSB):
*        ACEN and BCEN arguments added to call to IRA_XYLIM
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'NDF_PAR'          ! NDF constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION	ACEN     ! Sky longitude at image centre.
      INTEGER 		ARYPLC   ! ARY temporary place holder.
      DOUBLE PRECISION  BCEN     ! Sky latitude at image centre.
      INTEGER 		DIMS(2)  ! Dimensions of output NDF.
      INTEGER 		IARYW1   ! ARY identifier for 1st work array.
      INTEGER 		IARYW2   ! ARY identifier for 2nd work array.
      INTEGER 		IDA      ! IRA identifier.
      INTEGER 		INDF     ! NDF identifier.
      INTEGER 		IPDAT    ! Pointer to output data array.
      INTEGER 		IPW1     ! Pointer to 1st work array.
      INTEGER 		IPW2     ! Pointer to 2nd work array.
      DOUBLE PRECISION  LBND(2)  ! Lower pixel bound on each axis.
      INTEGER 		NEL      ! No. of elements in an array.
      INTEGER 		NVAL     ! No. of values obtained from the
                                 ! environment.
      DOUBLE PRECISION  ORIENT   ! Orientation of final image.
      DOUBLE PRECISION  P(8)     ! Projection parameters.
      DOUBLE PRECISION  PIXSIZ(2)! Size of a pixel near the image
                                 ! centre.
      CHARACTER 	PRJLST*(IRA__SZPLS)! List of available projections.
      CHARACTER 	PROJ*(IRA__SZPRJ)! Selected projection.
      CHARACTER 	SCS*(IRA__SZSCS)! Selected SCS.
      DOUBLE PRECISION  SIZE(2)  ! Size of each image axis in radians.
      DOUBLE PRECISION  SQSIZE(2)! Size of each "patch" in radians.
      DOUBLE PRECISION  TILT     ! Tilt of celestial sphere.
      DOUBLE PRECISION  UBND(2)  ! Upper pixel bound on each axis.
      CHARACTER 	XLOC*(DAT__SZLOC)! HDS locator to NDF extension.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise IRA.
      CALL IRA_INIT( STATUS )

*  Get the name of the Sky Coordinate System.
      SCS = 'Eq'
      CALL IRA_GTSCS( 'SCS', .TRUE., SCS, STATUS )

*  Get the name of the projection.
      CALL IRA_IPROJ( PRJLST, STATUS )
      CALL MSG_SETC( 'P', PRJLST )
      CALL MSG_OUT( 'IRA_TEST_MSG1', 'Available projections: ^P',
     :              STATUS )

      CALL PAR_GET0C( 'PROJ', PROJ, STATUS )

*  Get the size of each square along the both sky axes in degrees and
*  convert to radians.
      CALL PAR_GET1D( 'SQUARE', 2, SQSIZE, NVAL, STATUS )

      IF( NVAL .EQ. 1 ) SQSIZE( 2 ) = SQSIZE( 1 )
      SQSIZE( 1 ) = IRA__DTOR*SQSIZE( 1 )
      SQSIZE( 2 ) = IRA__DTOR*SQSIZE( 2 )

*  Get the sky coordinates of the image centre (default = (0,0) ).
      ACEN = 0.0
      BCEN = 0.0
      CALL IRA_GETCO( 'ACENTRE', 'BCENTRE', ' of image centre', SCS,
     :                 .TRUE., ACEN, BCEN, STATUS )

*  Get the size of the image along both sky axes in degrees, and
*  convert to radians.
      CALL PAR_GET1D( 'SIZE', 2, SIZE, NVAL, STATUS )

      IF( NVAL .EQ. 1 ) SIZE( 2 ) = SIZE( 1 )
      SIZE( 1 ) = IRA__DTOR*SIZE( 1 )
      SIZE( 2 ) = IRA__DTOR*SIZE( 2 )

*  Get the pixel size along each image axes in degrees, and convert to
*  radians.
      CALL PAR_GET1D( 'PIXSIZE', 2, PIXSIZ, NVAL, STATUS )

      IF( NVAL .EQ. 1 ) PIXSIZ( 2 ) = PIXSIZ( 1 )
      PIXSIZ( 1 ) = IRA__DTOR*PIXSIZ( 1 )
      PIXSIZ( 2 ) = IRA__DTOR*PIXSIZ( 2 )

*  Get the position angle of the second (Y) image axis, (i.e. the angle
*  from north,through east, to the positive image Y axis) and convert
*  to radians.
      CALL PAR_GET0D( 'ORIENT', ORIENT, STATUS )
      ORIENT = ORIENT*IRA__DTOR

*  Get the anti-clockwise angle through which to tilt the celetsial
*  sphere before projecting and convert to radians.
      CALL PAR_GET0D( 'TILT', TILT, STATUS )
      TILT = TILT*IRA__DTOR

*  Set up projection parameters which puts the reference point at the
*  image centre, with image coordinates (0,0).
      P( 1 ) = ACEN
      P( 2 ) = BCEN
      P( 3 ) = 0.0D0
      P( 4 ) = 0.0D0
      P( 5 ) = PIXSIZ( 1 )
      P( 6 ) = PIXSIZ( 2 )
      P( 7 ) = ORIENT
      P( 8 ) = TILT

*  Get an IRA identifier for this astrometry information.
      CALL IRA_CREAT( PROJ, 8, P, SCS, 1991D0, NDF__NOID, IDA, STATUS )

*  Find the bounds of the image using these projection parameters.
      CALL IRA_XYLIM( IDA, ACEN, BCEN, SIZE(1), SIZE(2), LBND, UBND,
     :                STATUS )

*  Annull the IRA identifier.
      CALL IRA_ANNUL( IDA, STATUS )

*  Since a primative NDF is being created, the lower bound of each
*  image axis is required to be 1. Shift all image coordinates so that
*  the lower bound of each axis is 1.
      DIMS( 1 ) = NINT( UBND(1) - LBND(1) )
      P( 3 ) = DBLE( DIMS( 1 ) )*0.5

      DIMS( 2 ) = NINT( UBND(2) - LBND(2) )
      P( 4 ) = DBLE( DIMS( 2 ) )*0.5

*  Tell the user what the image size is.
      CALL MSG_SETI( 'NPIX', DIMS( 1 ) )
      CALL MSG_SETI( 'NLIN', DIMS( 2 ) )
      CALL MSG_OUT( 'IRA_TEST_MSG2', 'Output image is ^NPIX by ^NLIN',
     :              STATUS )

*  Start an NDF context.
      CALL NDF_BEGIN

*  Create the output NDF in primative storage form with the image size
*  just calculated.
      CALL NDF_CREP( 'OUT', '_REAL', 2, DIMS, INDF, STATUS )

*  Create an extension called IRAS of type IRAS, and then annul the
*  locator.
      CALL NDF_XNEW( INDF, 'IRAS', 'IRAS', 0, 0, XLOC, STATUS )
      CALL DAT_ANNUL( XLOC, STATUS )

*  Create the finalised astrometry structure.
      CALL IRA_CREAT( PROJ, 8, P, SCS, 1991D0, INDF, IDA, STATUS )

*  Map the output data array.
      CALL NDF_MAP( INDF, 'DATA', '_REAL', 'WRITE/BAD', IPDAT, NEL,
     :              STATUS )

*  Create two temporary work arrays.
      CALL ARY_TEMP( ARYPLC, STATUS )
      CALL ARY_NEWP( '_DOUBLE', 2, DIMS, ARYPLC, IARYW1, STATUS )
      CALL ARY_MAP( IARYW1, '_DOUBLE', 'WRITE/BAD', IPW1, NEL, STATUS )

      CALL ARY_TEMP( ARYPLC, STATUS )
      CALL ARY_NEWP( '_DOUBLE', 2, DIMS, ARYPLC, IARYW2, STATUS )
      CALL ARY_MAP( IARYW2, '_DOUBLE', 'WRITE/BAD', IPW2, NEL, STATUS )

*  Check status before calling FILDAT to fill the output data array
*  with a checked pattern.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL FILDAT( IDA, SCS, SQSIZE, DIMS(1), DIMS(2), %VAL(IPDAT),
     :                %VAL(IPW1), %VAL(IPW2), STATUS )
      END IF

*  Release the two temporary work arrays.
      CALL ARY_ANNUL( IARYW1, STATUS )
      CALL ARY_ANNUL( IARYW2, STATUS )

*  Close IRA.
 999  CALL IRA_CLOSE( STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

      END










      SUBROUTINE FILDAT( IDA, SCS, SQSIZE, NPIX, NLIN, DATA, WORK1,
     :                   WORK2, STATUS )
*+
*  Name:
*     FILDAT

*  Purpose:
*     Fill the data array of the output NDF generated by program
*     IRA_DEMO1.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FILDAT( IDA, SCS, SQSIZE, NPIX, NLIN, DATA, WORK1,
*                  WORK2, STATUS )

*  Description:
*     This routine sets up a patchwork quilt appearance for the output
*     data array, in which dark squares have a value of -1 and light
*     squares have a value of +1. Each square consists of an area
*     bounded by lines of constant sky longitude and latitude
*     values. A "sledge hammer" approach is used in which the sky
*     coordinates of every pixel in the image is calculated by using the
*     projection stored in the supplied IRA structure.

*  Arguments:
*     IDA = INTEGER (Given)
*        An identifier for an IRA structure describing the astrometry of
*        the output data array.
*     SCS = CHARACTER * ( * ) (Given)
*        The Sky Coordinate System produced by the forward mapping of
*        the projection contained in the supplied IRA structure.
*     SQSIZE( 2 ) = DOUBLE PRECISION (Given)
*        The size of a single square in the patchwork pattern. The two
*        values refer to the size parallel to east and north. Given in
*        degrees.
*     NPIX = INTEGER (Given)
*        The number of pixels per line in the output array.
*     NLIN = INTEGER (Given)
*        The number of lines in the output array.
*     DATA( NPIX, NLIN ) = REAL (Returned)
*        An image of a "patchwork" sky, projected using the projection
*        stored in the IRA structure.
*     WORK1( NPIX, NLIN ) = DOUBLE PRECISION (Returned)
*        Work array to hold the X coordinate of each pixel.
*     WORK2( NPIX, NLIN ) = DOUBLE PRECISION (Returned)
*        Work array to hold the Y coordinate of each pixel.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-JAN-1991 (DSB):
*        Original version.
*     2-MAY-1991 (DSB):
*        Modified for IRA version 2.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! BAD values.

*  Arguments Given:
      INTEGER          IDA
      CHARACTER        SCS*(*)
      DOUBLE PRECISION SQSIZE( 2 )
      INTEGER          NPIX
      INTEGER          NLIN

*  Arguments Returned:
      REAL             DATA( NPIX, NLIN )
      DOUBLE PRECISION WORK1( NPIX, NLIN )
      DOUBLE PRECISION WORK2( NPIX, NLIN )

*  Status:
      INTEGER          STATUS    ! Global status

*  Local Variables:
      INTEGER          LIN       ! Image line counter
      INTEGER          PIX       ! Image pixel counter
      DOUBLE PRECISION RECIPA    ! Reciprocal of longitude patch size.
      DOUBLE PRECISION RECIPB    ! Reciprocal of latitude patch size.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Fill WORK1 and WORK2 with the X and Y image coordinates of the centre
*  of every pixel in the output image.
      DO LIN = 1, NLIN
         DO PIX = 1, NPIX
            WORK1( PIX, LIN ) = DBLE( PIX - 0.5 )
            WORK2( PIX, LIN ) = DBLE( LIN - 0.5 )
         END DO
      END DO

*  Transform these into sky coordinates.
      CALL IRA_TRANS( NPIX*NLIN, WORK1, WORK2, .TRUE., SCS, IDA, WORK1,
     :                WORK2, STATUS )

*  Create the output image as a "patch-work", each square having a value
*  of either +1 or -1.
      RECIPA = 1.0/SQSIZE( 1 )
      RECIPB = 1.0/SQSIZE( 2 )

      DO LIN = 1, NLIN
         DO PIX = 1, NPIX

            IF( WORK1(PIX,LIN).NE.VAL__BADD .AND.
     :          WORK2(PIX,LIN).NE.VAL__BADD ) THEN
               DATA( PIX, LIN ) = (-1)**( INT( WORK1(PIX,LIN)*RECIPA ) +
     :                                 INT( WORK2(PIX,LIN)*RECIPB ) )

            ELSE
               DATA( PIX, LIN ) = VAL__BADR

            END IF

         END DO
      END DO


      END
