      SUBROUTINE KPG1_LUTK3( IPLOT, PARAM, APP, LP, UP, X, RGB, STATUS )
*+
*  Name:
*     KPG1_LUTK3

*  Purpose:
*     Produces a GRAPH colour table key for KPG1_LUTKY.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_LUTK3( IPLOT, PARAM, APP, LP, UP, X, RGB, STATUS )

*  Description:
*     This routine produces a colour table key consisting of one or
*     three line plots (1 if the colour table is a greyscale, and 3 if
*     it is not). The line plots are drawn using the supplied Plot.

*  Arguments:
*     IPLOT = INTEGER (Given)
*        The Plot to use.
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the style parameter to use (e.g. STYLE, KEYSTYLE).
*        The appearance of the three curves can be set using attribute
*        qualifiers (R), (G), and (B) (e.g. "width(r)=10").
*     APP = CHARACTER * ( * ) (Given)
*        The calling application, in the form "KAPPA_LUTVIEW".
*     LP = INTEGER (Given)
*        The lowest PGPLOT colour index to copy.
*     UP = INTEGER (Given)
*        The highest PGPLOT colour index to copy.
*     X( LP:UP, 2 ) = DOUBLE PRECISION (Returned)
*        Work space to hold the X values
*     RGB( LP:UP, 3 ) = DOUBLE PRECISION (Returned)
*        Work space to hold the RGB intensities.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-OCT-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions

*  Arguments Given:
      INTEGER IPLOT
      CHARACTER PARAM*(*)
      CHARACTER APP*(*)
      INTEGER LP
      INTEGER UP

*  Arguments Returned:
      DOUBLE PRECISION X( LP:UP, 2 )
      DOUBLE PRECISION RGB( LP:UP, 3 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count
      INTEGER IPENY              ! Index of PEN-Y Frame within IPLOT
      INTEGER MAP                ! Pointer to (PEN-Y -> GRAPHICS) Mapping
      INTEGER NCURVE             ! Number of curves to draw
      INTEGER NPEN               ! Number of pens
      INTEGER PEN( 3 )           ! Pen indices for drawing three curves
      LOGICAL GREY               ! Is the colour table a greyscale?
      REAL R, G, B               ! Red, Green and Blue intensities
      CHARACTER SYN(3)*3         ! Synonyms for CURVES attributes

      DATA SYN / '(R)', '(G)', '(B)' /
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Save the number of pens in use.
      NPEN = UP - LP + 1

*  Find the PEN-Y Frame in the Plot. Axis 1 of this Frame is pen number,
*  and Axis 2 is RGB intensity.
      CALL KPG1_ASFFR( IPLOT, 'PEN-Y', IPENY, STATUS )

*  Get the Mapping from the PEN-Y Frame to the GRAPHICS Frame.
      MAP = AST_GETMAPPING( IPLOT, IPENY, AST__BASE, STATUS )

*  Copy the RGB intensities (0-1) into the RGB array. Also store the
*  pen numbers in X. Also note if the RGB intensities are equal
*  for every pen.
      GREY = .TRUE.
      DO I = LP, UP
         CALL PGQCR( I, R, G, B )
         X( I, 1 ) = DBLE( I )
         RGB( I, 1 ) = DBLE( R )
         RGB( I, 2 ) = DBLE( G )
         RGB( I, 3 ) = DBLE( B )
         IF( R .NE. G .OR. R .NE. B .OR. G .NE. B ) GREY = .FALSE.
      END DO

*  If we have a greyscale colour table only draw 1 curve.
      IF( GREY ) THEN
         NCURVE = 1

*  Find the index of a grey pen which contrast with the background.
         CALL KPG1_PGCOL( 'WHITE', LP, UP, PEN( 1 ), STATUS )
         IF( PEN( 1 ) .EQ. 0 ) CALL KPG1_PGCOL( 'BLACK', LP, UP,
     :                                          PEN( 1 ), STATUS )

*  If we have a non-greyscale colour table, draw 3 curves.
      ELSE
         NCURVE = 3

*  Find the indices of red, blur and green pens which contrast with the
*  background.
         CALL KPG1_PGCOL( 'RED', LP, UP, PEN( 1 ), STATUS )
         IF( PEN( 1 ) .EQ. 0 ) CALL KPG1_PGCOL( 'BLACK', LP, UP,
     :                                          PEN( 1 ), STATUS )

         CALL KPG1_PGCOL( 'GREEN', LP, UP, PEN( 2 ), STATUS )
         IF( PEN( 2 ) .EQ. 0 ) CALL KPG1_PGCOL( 'BLACK', LP, UP,
     :                                          PEN( 2 ), STATUS )

         CALL KPG1_PGCOL( 'BLUE', LP, UP, PEN( 3 ), STATUS )
         IF( PEN( 3 ) .EQ. 0 ) CALL KPG1_PGCOL( 'BLACK', LP, UP,
     :                                          PEN( 3 ), STATUS )

      END IF

*  Draw the required curves.
      DO I = 1, NCURVE

*  Map the RGB and X values from the PEN-Y Frame into the GRAPHICS Frame.
*  Overwrite the original RGB values since they are not needed any more.
         CALL AST_TRAN2( MAP, NPEN, X( LP, 1 ), RGB( LP, I ), .TRUE.,
     :                   X( LP, 2 ), RGB( LP, I ), STATUS )

*  Select the default pen for drawing curves.
         CALL AST_SETI( IPLOT, 'COLOUR(CURVES)', PEN( I ), STATUS )

         CALL KPG1_ASPSY( SYN( I ), '(CURVES)', STATUS )

*  Produce the line plot.
         CALL KPG1_PLTLN( NPEN, 1, NPEN, X( LP, 2 ), RGB( LP, I ),
     :                    .FALSE., .FALSE., 0.0D0, 0.0D0, 0.0D0, PARAM,
     :                    IPLOT, 1, 1, 1, 1, APP, STATUS )

         CALL KPG1_ASPSY( ' ', ' ', STATUS )

      END DO

*  Annul the Mapping.
      CALL AST_ANNUL( MAP, STATUS )

      END
