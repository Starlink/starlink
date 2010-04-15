      SUBROUTINE FIG_PGEND( )
*+
*  Name:
*     FIG_PGEND

*  Purpose:
*     Terminate PGPLOT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIG_PGEND

*  Description:
*     This routine closes down PGPLOT as begun with FIG_PGBEGIN.
*     Depending on the implementation this may either simply call PGEND,
*     or it may make more calls to achieve compliance with the
*     Applications Graphics Interface AGI.
*
*     In the AGI case this routine first makes some enquiries about the
*     viewport and then stores it as an AGI picture. This will take note if
*     any coordinate runs backwards. The new AGI picture will have the
*     name 'DATA' and the comment 'FIGARO_PGVIEW'.

*  Arguments:
*     None.

*  Copyright:
*     Copyright (C) 1995 Particle Physics & Astronomy Research Council

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     10 Jan 1995 (hme):
*        Original version. Based on SPD_UGAD.
*     11 Jan 1995 (hme):
*        In FIG_PGBEG use original PGBEG directly, slip all AGI
*        processing behind the PGEND call here.
*     14 Mar 1996 (hme):
*        AGI_CLOSE did not annul picture identifiers, so now we use an
*        AGI context to annul all picture identifiers. AGI_END then
*        hopefully implies the AGI_CLOSE.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Local Constants:
      CHARACTER * ( 13 ) COMENT  ! The comment for the new AGI picture
      PARAMETER ( COMENT = 'FIGARO_PGVIEW' )

*  Local Variables:
      INTEGER DEVLEN             ! Actual length of device name
      INTEGER PICID              ! Current picture ID
      INTEGER BASID              ! Base picture ID
      INTEGER STATUS             ! Global status
      INTEGER I                  ! Index into string
      REAL L1, R1, B1, T1        ! World coordinates of current picture
      REAL L2, R2, B2, T2        ! Viewport location in view surface
      REAL L3, R3, B3, T3        ! Viewport loc. in world coordinates
      REAL L4, R4, B4, T4        ! Window
      CHARACTER * ( 32 ) DEVICE  ! Graphics device name
      CHARACTER * ( 80 ) FORW( 2 ) ! Forward coordinate transform
      CHARACTER * ( 80 ) INVS( 2 ) ! Inverse coordinate transform

*.


*  PGPLOT processing.
*  ==================

*  Get the device name. We cannot use the device or file inquiry, since
*  the Starlink PGPLOT library returns those as blank. So we must strip
*  off any qualifier ourselves.
      CALL PGQINF( 'DEV/TYPE', DEVICE, DEVLEN )
      I = INDEX( DEVICE, '/' )
      IF ( I .GT. 0 ) DEVICE(I:) = ' '

*  Get the location of the view port in the view surface. The assumption
*  later will be that the base picture coincides with the view surface.
      CALL PGQVP( 0, L2, R2, B2, T2 )

*  Get the window. This will become the world coordinates of the new
*  picture.
      CALL PGQWIN( L4, R4, B4, T4 )

*  All information from the PGPLOT system is now local, we can end
*  PGPLOT. Indeed we must end it before we open AGI.
      CALL PGEND


*  AGI processing.
*  ===============

*  Begin error context and AGI context.
      CALL ERR_MARK
      STATUS = SAI__OK
      CALL AGI_BEGIN

*     Open AGI. Use update access to be sure no plot is erased.
         CALL AGI_OPEN( DEVICE, 'UPDATE', PICID, STATUS )

*     Make the base picture the current picture.
         CALL AGI_IBASE( BASID, STATUS )
         CALL AGI_SELP(  BASID, STATUS )

*     Get the world coordinates of the current AGI picture. In these
*     coordinates must we express the location of the view port.
         CALL AGI_IWOCO( L1, R1, B1, T1, STATUS )

*     Transform these normalised device coordinates to the world
*     coordinates of the current picture. L2 etc. give the location of
*     the view port in fractions of the view surface. This
*     transformation would turn 0 into L1/B1 and 1 into R1/T1.
         L3 = L1 + L2 * ( R1 - L1 )
         R3 = L1 + R2 * ( R1 - L1 )
         B3 = B1 + B2 * ( T1 - B1 )
         T3 = B1 + T2 * ( T1 - B1 )

*     If extent is positive in both directions.
         IF ( L4 .LT. R4 .AND. B4 .LT. T4 ) THEN

*        Create a new AGI picture with location L3 etc. and world
*        coordinates L4 etc. AGI_NUPIC will do, no transformation is
*        necessary.

            CALL AGI_NUPIC( L3, R3, B3, T3, 'DATA', COMENT,
     :         L4, R4, B4, T4, PICID, STATUS )

*     Else (extent is negative in one or both directions).
         ELSE

*        Create a new AGI picture with location L3 etc. But we cannot
*        use L4 etc. as world coordinates, since world coordinates have
*        to be increasing. So we set world coordinates to the range 0.
*        to 1. and specify a transformation afterwards.
            CALL AGI_NUPIC( L3, R3, B3, T3, 'DATA', COMENT,
     :         0., 1., 0., 1., PICID, STATUS )

*        The first forward function turns data file values into picture
*        world coordinates for X.
*        X = ( XL - L4 ) / ( R4 - L4 )
            FORW(1) = 'X = ( XL - ( '
            I = 13
            CALL CHR_PUTR( L4, FORW(1), I )
            FORW(1)(I+1:) = ' ) ) / ( '
            I = I + 9
            CALL CHR_PUTR( R4-L4, FORW(1), I )
            FORW(1)(I+1:) = ' )'

*        The second forward function turns data file values into picture
*        world coordinates for Y.
*        Y = ( YL - B4 ) / ( T4 - B4 )
            FORW(2) = 'Y = ( YL - ( '
            I = 13
            CALL CHR_PUTR( B4, FORW(2), I )
            FORW(2)(I+1:) = ' ) ) / ( '
            I = I + 9
            CALL CHR_PUTR( T4-B4, FORW(2), I )
            FORW(2)(I+1:) = ' )'

*        The first inverse function turns picture world coordinates into
*        data file values for X.
*        XL = X * ( R4 - L4 ) + L4
            INVS(1) = 'XL = X * ( '
            I = 11
            CALL CHR_PUTR( R4-L4, INVS(1), I )
            INVS(1)(I+1:) = ' ) + ( '
            I = I + 7
            CALL CHR_PUTR( L4, INVS(1), I )
            INVS(1)(I+1:) = ' )'

*        The second inverse function turns picture world coordinates
*        into data file values for Y.
*        YL = Y * ( T4 - B4 ) + B4
            INVS(2) = 'YL = Y * ( '
            I = 11
            CALL CHR_PUTR( T4-B4, INVS(2), I )
            INVS(2)(I+1:) = ' ) + ( '
            I = I + 7
            CALL CHR_PUTR( B4, INVS(2), I )
            INVS(2)(I+1:) = ' )'

*        Store the transformation.
            CALL AGI_TNEW( 2, 2, FORW, INVS, PICID, STATUS )

         END IF

*     End AGI context, making the base picture current, since this is the
*     frame used by Figaro.
         CALL AGI_END( BASID, STATUS )

*  Flush error reports and end error context.
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
      CALL ERR_RLSE


*  Return.
*  =======

      END
