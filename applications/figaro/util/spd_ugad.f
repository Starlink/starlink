      SUBROUTINE SPD_UGAD( PNAME, COMENT, PICID, STATUS )
*+
*  Name:
*     SPD_UGAD

*  Purpose:
*     Save PGPLOT view port as AGI picture.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_UGAD( PNAME, COMENT, PICID, STATUS )

*  Description:
*     The routine quadruplet SPD_UGA{ABCD} is used by
*     Specdre instead of AGP_ASSOC, AGP_DEASS, PGPAGE, AGP_SVIEW to
*     overcome the problem that normally the view surface is the base
*     picture and PGPAGE will clear more than the AGI picture to be
*     used.
*
*     This routine assumes the current AGI picture to be the AGI picture
*     that corresponds to the SGS zone in which PGPLOT was openend. It
*     saves the current PGPLOT view port as a new AGI picture. To that
*     end this routine calls AGI_NUPIC, which returns the picture
*     identifier PICID.

*  Arguments:
*     PNAME = CHARACTER * ( * ) (Given)
*        The picture name to be given to the new picture.
*     COMENT = CHARACTER * ( * ) (Given)
*        The picture comment to be given to the new picture.
*     PICID = INTEGER (Returned)
*        The picture identifier for the new picture.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     29 Jun 1993 (hme):
*        Original version.
*     19 May 1994 (hme):
*        Renamed from SPAEE.
*     21 Jun 1994 (hme):
*        If window runs backwards, store a transformation.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) PNAME
      CHARACTER * ( * ) COMENT

*  Arguments Returned:
      INTEGER PICID

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I
      REAL L1, R1, B1, T1        ! World coordinates of current picture
      REAL L2, R2, B2, T2        ! Viewport location in view surface
      REAL L3, R3, B3, T3        ! Viewport loc. in world coordinates
      REAL L4, R4, B4, T4        ! Window
      CHARACTER * ( 80 ) FORW( 2 ) ! Forward coordinate transform
      CHARACTER * ( 80 ) INVS( 2 ) ! Inverse coordinate transform

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the world coordinates of the current AGI picture. In these
*  coordinates must be express the location of the view port.
      CALL AGI_IWOCO( L1, R1, B1, T1, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Get the location of the view port in the view surface. The assumption
*  is that the current picture coincides with the view surface.
      CALL PGQVP( 0, L2, R2, B2, T2 )

*  Transform these normalised device coordinates to the world
*  coordinates of the current picture. L2 etc. give the location of the
*  view port in fractions of the view surface. This transformation would
*  turn 0 into L1/B1 and 1 into R1/T1.
      L3 = L1 + L2 * ( R1 - L1 )
      R3 = L1 + R2 * ( R1 - L1 )
      B3 = B1 + B2 * ( T1 - B1 )
      T3 = B1 + T2 * ( T1 - B1 )

*  Get the window. This will become the world coordinates of the new
*  picture. Should the extent be negative, then AGI_NUPIC will fail.
      CALL PGQWIN( L4, R4, B4, T4 )

*  If extent is positive in both directions.
      IF ( L4 .LT. R4 .AND. B4 .LT. T4 ) THEN

*     Create a new AGI picture with location L3 etc. and world
*     coordinates L4 etc. AGI_NUPIC will do, no transformation is
*     necessary.
         CALL AGI_NUPIC( L3, R3, B3, T3, PNAME, COMENT, L4, R4, B4, T4,
     :      PICID, STATUS )

*  Else (extent is negative in one or both directions).
      ELSE

*     Create a new AGI picture with location L3 etc. But we cannot use
*     L4 etc. as world coordinates, since world coordinates have to be
*     increasing. So we set world coordinates to the range 0. to 1. and
*     specify a transformation afterwards.
         CALL AGI_NUPIC( L3, R3, B3, T3, PNAME, COMENT, 0., 1., 0., 1.,
     :      PICID, STATUS )

*     The first forward function turns data file values into picture
*     world coordinates for X.
*     X = ( XL - L4 ) / ( R4 - L4 )
         FORW(1) = 'X = ( XL - ( '
         I = 13
         CALL CHR_PUTR( L4, FORW(1), I )
         FORW(1)(I+1:) = ' ) ) / ( '
         I = I + 9
         CALL CHR_PUTR( R4-L4, FORW(1), I )
         FORW(1)(I+1:) = ' )'

*     The second forward function turns data file values into picture
*     world coordinates for Y.
*     Y = ( YL - B4 ) / ( T4 - B4 )
         FORW(2) = 'Y = ( YL - ( '
         I = 13
         CALL CHR_PUTR( B4, FORW(2), I )
         FORW(2)(I+1:) = ' ) ) / ( '
         I = I + 9
         CALL CHR_PUTR( T4-B4, FORW(2), I )
         FORW(2)(I+1:) = ' )'

*     The first inverse function turns picture world coordinates into
*     data file values for X.
*     XL = X * ( R4 - L4 ) + L4
         INVS(1) = 'XL = X * ( '
         I = 11
         CALL CHR_PUTR( R4-L4, INVS(1), I )
         INVS(1)(I+1:) = ' ) + ( '
         I = I + 7
         CALL CHR_PUTR( L4, INVS(1), I )
         INVS(1)(I+1:) = ' )'

*     The second inverse function turns picture world coordinates
*     into data file values for Y.
*     YL = Y * ( T4 - B4 ) + B4
         INVS(2) = 'YL = Y * ( '
         I = 11
         CALL CHR_PUTR( T4-B4, INVS(2), I )
         INVS(2)(I+1:) = ' ) + ( '
         I = I + 7
         CALL CHR_PUTR( B4, INVS(2), I )
         INVS(2)(I+1:) = ' )'

*     Store the transformation.
         CALL AGI_TNEW( 2, 2, FORW, INVS, PICID, STATUS )

      END IF

*  Return.
 500  CONTINUE
      END
