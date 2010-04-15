      SUBROUTINE SBOXA0( PACEN, PBCEN, PXYCEN, BOXSIZ, SKYCO, CURSOR,
     :                   IDA, SCS, XC, YC, STATUS )

*+
*  Name:
*     SBOXA0

*  Purpose:
*     Find image coordinates for two opposite corners of a box
*     specified by centre and extent.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SBOXA0( PACEN, PBCEN, PXYCEN, BOXSIZ, SKYCO, CURSOR, IDA,
*                  SCS, XC, YC, STATUS )

*  Description:
*     The sky and image coordinates of the centre of the box are
*     obtained using the cursor or parameter interface, depending on
*     the value of parameter CURSOR. If the box size was specified in
*     arc-minutes the corresponding sizes in pixels are found using an
*     IRA routine which takes account of the changing pixel size across
*     the image. The image coordinates of the bottom left and top right
*     corners are then found and returned. The returned coordinates are
*     set bad if the centre position was outside the projection.

*  Arguments:
*     PACEN = CHARACTER * ( * ) (Given)
*        The parameter associated with the longitude at the box centre.
*     PBCEN = CHARACTER * ( * ) (Given)
*        The parameter associated with the latitude at the box centre.
*     PXYCEN = CHARACTER * ( * ) (Given)
*        The parameter associated with the image coordinates at the box
*        centre.
*     BOXSIZ( 2 ) = REAL (Given)
*        The dimensions of the box parallel to X and Y. If SKYCO is
*        true, then they should be in radians. Otherwise, they should be
*        in pixels.
*     SKYCO = LOGICAL (Given)
*        True if the values held in BOXSIZ are in arc-minutes.
*     CURSOR = LOGICAL (Given)
*        True if the cursor is to be used to get the coordinates of the
*        centre of the box. Otherwise the parameter system is used.
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information.
*     SCS = CHARACTER * ( * ) (Given)
*        The Sky Coordinate System to use.
*     XC( 2 ) = REAL (Returned)
*        The X image coordinates of the two opposite corners.
*     YC( 2 ) = REAL (Returned)
*        The Y image coordinates of the two opposite corners.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-FEB-1993 (DSB):
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
      INCLUDE 'MSG_PAR'          ! MSG_ constants

*  Arguments Given:
      CHARACTER PACEN*(*)
      CHARACTER PBCEN*(*)
      CHARACTER PXYCEN*(*)
      REAL BOXSIZ( 2 )
      LOGICAL SKYCO
      LOGICAL CURSOR
      INTEGER IDA
      CHARACTER SCS*(*)

*  Arguments Returned:
      REAL XC( 2 )
      REAL YC( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION
     :        AA( 2 ),           ! Longitude values
     :        ACEN,              ! Longitude at box centre
     :        BB( 2 ),           ! Latitude values
     :        BCEN,              ! Latitude at sky centre
     :        DIST,              ! Arc-length between edge mid points
     :        DLBND( 2 ),        ! Lower bounds of box
     :        DUBND( 2 ),        ! Upper bounds of box
     :        XX( 2 ),           ! Image coordinate X values
     :        YY( 2 )            ! Image coordinate Y values

      REAL
     :        FRACX,             ! Size of actual box wrt requested box
     :        FRACY,             ! Size of actual box wrt requested box
     :        XCEN,              ! X coordinate of box centre
     :        XYCEN( 2 ),        ! Image coordinates of box centre
     :        YCEN               ! Y coordinate of box centre

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the returned corner coordinates to bad values.
      XC( 1 ) = VAL__BADR
      YC( 1 ) = VAL__BADR
      XC( 2 ) = VAL__BADR
      YC( 2 ) = VAL__BADR

*  If the cursor is to be used to get the position of the box centre...
      IF( CURSOR ) THEN

*  Display instructions describing the use of the cursor.
         CALL MSG_BLANKIF( MSG__NORM, STATUS )
         CALL MSG_OUTIF( MSG__NORM, 'SBOXA0_MSG1',
     :     '  Position the cursor at the centre of the box and press '//
     :     'any key ', STATUS )
         CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Get a position from the cursor.
         XCEN = VAL__BADR
         CALL SBOXB0( .FALSE., XCEN, YCEN, STATUS )

*  Convert the image coordinates returned by the cursor to sky
*  coordinates.
         CALL IRA_TRANS( 1, DBLE( XCEN ), DBLE( YCEN ), .TRUE.,
     :                   SCS, IDA, ACEN, BCEN, STATUS )

*  If the supplied box centre is not within the domain of the
*  projection, return with bad corner coordinates.
         IF( ACEN .EQ. VAL__BADD .OR. BCEN .EQ. VAL__BADD ) GO TO 999

*  If the parameter interface is to be used to get the position of the
*  box centre...
      ELSE

*  If the user wants to give sky coordinates, use IRA.
         IF( SKYCO ) THEN
            CALL IRA_GETCO( PACEN, PBCEN, ' of the box centre', SCS,
     :                      .FALSE., ACEN, BCEN, STATUS )

*  Convert the sky coordinates to image coordinates.
            CALL IRA_TRANS( 1, ACEN, BCEN, .FALSE., SCS, IDA, XX, YY,
     :                      STATUS )

*  If the supplied box centre is not within the domain of the
*  projection, return with bad corner coordinates.
            IF( XX( 1 ) .EQ. VAL__BADD .OR.
     :          YY( 1 ) .EQ. VAL__BADD ) GO TO 999

*  Store the single precision image coordinates at the centre.
            XCEN = REAL( XX( 1 ) )
            YCEN = REAL( YY( 1 ) )

*  If the user wants to give image coordinates, use PAR.
         ELSE
            CALL PAR_EXACR( PXYCEN, 2, XYCEN, STATUS )

*  Store the supplied image coordinates at the box centre
            XCEN = XYCEN( 1 )
            YCEN = XYCEN( 2 )

*  Find the corresponding sky coordinates.
            CALL IRA_TRANS( 1, DBLE( XCEN ), DBLE( YCEN ), .TRUE., SCS,
     :                      IDA, ACEN, BCEN, STATUS )

*  If the supplied box centre is not within the domain of the
*  projection, return with bad corner coordinates.
            IF( ACEN .EQ. VAL__BADD .OR. BCEN .EQ. VAL__BADD ) GO TO 999

         END IF

      END IF

*  We now have both the image and the sky coordinates at the box centre.
*  The method to use for calculating the image coordinates of the box
*  corners depends on whether the box size has been supplied in
*  arc-minutes or in pixels. First deal with the case of arc-minutes...
      IF( SKYCO ) THEN

*  Use an IRA routine to find the bounds (in image coordinates) of a
*  box which has the requested dimensions in arc-minutes. The box is
*  truncated at the edge of the projection, if the edge of the
*  projection is encountered. This may happen, for instance, if the box
*  is situated close to the elliptical boundary of an Aitoff
*  projection.
         CALL IRA_XYLIM( IDA, ACEN, BCEN, DBLE( BOXSIZ( 1 ) ),
     :                   DBLE( BOXSIZ( 2 ) ), DLBND, DUBND, STATUS )

*  Return the single precision image coordinates of the bottom left and
*  top right corners of the box.
         XC( 1 ) = REAL( DLBND( 1 ) )
         YC( 1 ) = REAL( DLBND( 2 ) )

         XC( 2 ) = REAL( DUBND( 1 ) )
         YC( 2 ) = REAL( DUBND( 2 ) )

*  Find the size in arc-minutes of the X dimension of the returned box.
*  This is the arc-length joining the mid points of the left and right
*  hand edges.
         XX( 1 ) = DLBND( 1 )
         YY( 1 ) = 0.5D0*( DLBND( 2 ) + DUBND( 2 ) )
         XX( 2 ) = DUBND( 1 )
         YY( 2 ) = YY( 1 )
         CALL IRA_TRANS( 2, XX, YY, .TRUE., SCS, IDA, AA, BB, STATUS )
         CALL IRA_DIST( AA( 1 ), BB( 1 ), AA( 2 ), BB( 2 ), DIST,
     :                  STATUS )

*  Store the actual size as a fraction of the requested box size.
         IF( DIST .NE. VAL__BADD ) THEN
            FRACX = ABS( REAL( DIST ) )/BOXSIZ( 1 )
         ELSE
            FRACX = VAL__BADR
         END IF

*  Find the size in arc-minutes of the Y dimension of the returned box.
*  This is the arc-length joining the mid points of the top and bottom
*  edges.
         XX( 1 ) = 0.5D0*( DLBND( 1 ) + DUBND( 1 ) )
         YY( 1 ) = DLBND( 2 )
         XX( 2 ) = XX( 1 )
         YY( 2 ) = DUBND( 2 )
         CALL IRA_TRANS( 2, XX, YY, .TRUE., SCS, IDA, AA, BB, STATUS )
         CALL IRA_DIST( AA( 1 ), BB( 1 ), AA( 2 ), BB( 2 ), DIST,
     :                  STATUS )

*  Store the actual size as a fraction of the requested box size.
         IF( DIST .NE. VAL__BADD ) THEN
            FRACY = ABS( REAL( DIST ) )/BOXSIZ( 2 )
         ELSE
            FRACY = VAL__BADR
         END IF

*  Warn the user if the box has been truncated significantly.
         IF( FRACX .EQ. VAL__BADR .OR. FRACY .EQ. VAL__BADR ) THEN
            CALL MSG_OUTIF( MSG__NORM, 'SBOXA0_MSG2',
     :                '  Box has been truncated by the projection edge',
     :                      STATUS )

         ELSE IF( FRACX .LT. 0.99 .OR. FRACY .LT. 0.99 ) THEN
            CALL MSG_SETR( 'FX', FRACX )
            CALL MSG_SETR( 'FY', FRACY )
            CALL MSG_OUTIF( MSG__NORM, 'SBOXA0_MSG3',
     :  '  Box has been truncated to ^FX x ^FY of its requested size',
     :                      STATUS )
         END IF

*  If the box size was supplied in image coordinates, then just divide
*  the box dimensions in half and offset away from the box centre.
      ELSE
         XC( 1 ) = XCEN - 0.5*BOXSIZ( 1 )
         XC( 2 ) = XC( 1 ) + BOXSIZ( 1 )

         YC( 1 ) = YCEN - 0.5*BOXSIZ( 2 )
         YC( 2 ) = YC( 1 ) + BOXSIZ( 2 )

      END IF

 999  CONTINUE

      END
