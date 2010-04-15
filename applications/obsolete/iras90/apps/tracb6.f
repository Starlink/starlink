      SUBROUTINE TRACB6( NSMP, NLIN, INSCN, PROFIL, LINNO, BG, ED, XLMT,
     :                   YLMT, XPOSN, OFFSET, CONST, SLOPE, STRTH,
     :                   SCNDIR, PEN, STATUS )
*+
*  Name:
*     TRACB6

*  Purpose:
*     Draw a point source profile.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRACB6( NSMP, NLIN, INSCN, PROFIL, LINNO, BG, ED, XLMT,
*                  YLMT, XPOSN, OFFSET, CONST, SLOPE, STRTH,
*                  SCNDIR, PEN, STATUS )

*  Description:
*     This rouinte draw a point source profile centred at the specified
*     in-scan position with given y offset, constant and slope.

*  Arguments:
*     NSMP = INTEGER (Given)
*        The number of sample in the data array containning point source
*        profiles.
*     NLIN = INTEGER (Given)
*        The number of line in the data array containning point source
*        profiles.
*     INSCN( NSMP, NLIN ) = REAL (Given)
*        In-scan distance of each sample in the profiles north of the
*        point source centre.
*     PROFIL( NSMP, NLIN ) = REAL (Given)
*        The data array containning point source profiles.
*     LINNO = INTEGER (Given)
*        Line number of profile to be draw
*     BG = INTEGER (Given)
*        Begin sample index of the point source profile in the line.
*     ED = INTEGER (Given)
*        End sample index of the point source profile in the line.
*     XLMT( 2 ) = REAL (Given)
*        The display limits in x direction.
*     YLMT( 2 ) = REAL (Given)
*        The display limits in y direction.
*     XPOSN = REAL (Given)
*        The in-scan position of the centre of the point source profile.
*     OFFSET = REAL (Given)
*        The offset in y direction of the profile.
*     CONST = REAL (Given)
*        The constant of the linear background.
*     SLOPE = REAL (Given)
*        The slope of the linear background.
*     STRTH = REAL (Given)
*        The strength of the point source.
*     SCNDIR = LOGICAL (Given)
*        If true, the scan is from north to south. Otherwise from south
*        to north.
*     PEN = INTEGER (Given)
*        Pen number used to draw the source profile.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     24-MAR-1991 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NSMP
      INTEGER NLIN
      REAL INSCN( NSMP, NLIN )
      REAL PROFIL( NSMP, NLIN )
      INTEGER LINNO
      INTEGER BG
      INTEGER ED
      REAL XLMT( 2 )
      REAL YLMT( 2 )
      REAL XPOSN
      REAL OFFSET
      REAL CONST
      REAL SLOPE
      REAL STRTH
      LOGICAL SCNDIR
      INTEGER PEN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER XORDER             ! x mapping order while drawing curve
      REAL YOFFST                ! Offset in y direction

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the total offset in y dirction.
      YOFFST = OFFSET + CONST

*  If scan direction is from north to south, set x mapping order such
*  that x is increasing from right to left in the display.
      IF ( SCNDIR ) THEN
         XORDER = 1

*  If scan direction is from south to north, set x mapping order such
*  that x is increasing from left to right in the display.
      ELSE
         XORDER = 0
      END IF

*  Draw the point source profile.
      CALL TRACB9( 1, NSMP, INSCN, PROFIL( 1, LINNO ), BG, ED, XPOSN,
     :             YOFFST, STRTH, SLOPE, XLMT, YLMT, XORDER, 0, PEN,
     :             STATUS )

      END
