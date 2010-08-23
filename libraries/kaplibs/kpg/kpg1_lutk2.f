      SUBROUTINE KPG1_LUTK2( FORM, CAXIS, LBND1, UBND1, LBND2, UBND2,
     :                       HSTDAT, MAXPOP, LOG, COLS, STATUS )
*+
*  Name:
*     KPG1_LUTK2

*  Purpose:
*     Set up the pixel colour indices which form a LUT key.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_LUTK2( FORM, CAXIS, LBND1, UBND1, LBND2, UBND2,
*                      HSTDAT, MAXPOP, LOG, COLS, STATUS )

*  Description:
*     This routine fills the COLS array with integer colour indices
*     so that the array can be used as a colour table key. It can produce
*     histogram or ramp keys.

*  Arguments:
*     FORM = INTEGER (Given)
*        Indicates the form of key required: 0 - ramp, 1 - histogram.
*     CAXIS = INTEGER (Given)
*        The index of the array axis corresponding to colour index (1 or 2).
*     LBND1 = INTEGER (Given)
*        The lower bound on Axis 1.
*     UBND1 = INTEGER (Given)
*        The upper bound on Axis 1.
*     LBND2 = INTEGER (Given)
*        The lower bound on Axis 2.
*     UBND2 = INTEGER (Given)
*        The upper bound on Axis 2.
*     HSTDAT( * ) = INTEGER (Given)
*        A histogram of colour index counts. The length of this vector
*        should be equal to the length of axis CAXIS.
*     MAXPOP = INTEGER (Given)
*        The maximum population in any cell of the histogram.
*     LOG = LOGICAL (Given)
*        If TRUE the histogram displays Log(count).
*     COLS( LBND1:UBND1, LBND2:UBND2 ) = INTEGER (Returned)
*        The returned array of colour indices.
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
*     useful,but WITHOUT ANY WARRANTY; without even the implied
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
*     4-OCT-2001 (DSB):
*        Original version.
*     11-MAY-2010 (DSB):
*        Fix calculation of BARTOP for colour indices which have 
*        zero population.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER FORM
      INTEGER CAXIS
      INTEGER LBND1
      INTEGER UBND1
      INTEGER LBND2
      INTEGER UBND2
      INTEGER HSTDAT( * )
      INTEGER MAXPOP
      LOGICAL LOG

*  Arguments Returned:
      INTEGER COLS( LBND1:UBND1, LBND2:UBND2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER RAMP               ! Symbolic constant for RAMP key form
      PARAMETER ( RAMP = 0 )

      INTEGER HIST               ! Symbolic constant for HIST key form
      PARAMETER ( HIST = 1 )

*  Local Variables:
      INTEGER BARTOP             ! Pixel index of top of histogram bar
      INTEGER I                  ! Pixel index
      INTEGER J                  ! Pixel index
      REAL POP                   ! Bin population
      REAL MXPOP                 ! Max bin population
      REAL SCALE                 ! Pixels per pop for histogram vertical axis
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  For histogram keys...
      IF( FORM .EQ. HIST ) THEN

*  Save the upper limit on the count or log(count) axis.
         IF( LOG .AND. MAXPOP .GT. 0 ) THEN
            MXPOP = LOG10( REAL( MAXPOP ) )
         ELSE
            MXPOP = REAL( MAXPOP )
         END IF

*  If Axis 1 corresponds to colour index...
         IF( CAXIS .EQ. 1 ) THEN

*  Find the pixels per log(count)
            SCALE = REAL( UBND2 - LBND2 + 1 )/MXPOP

*  Do each colour index.
            DO I = LBND1, UBND1

*  Get the population for this bin.
               POP = REAL( HSTDAT( I - LBND1 + 1 ) )
               IF( LOG ) THEN
                  IF( POP .GT. 1 ) THEN
                     POP = LOG10( POP )
                  ELSE
                     POP = 0.0
                  END IF
               END IF

*  Find the pixel index of the top of the histogram bar for the current
*  colour index.
               BARTOP = MIN( UBND2, NINT( POP*SCALE ) + LBND2 )

*  Fill the pixels with the current colour index up to the height of the
*  bar.
               DO J = LBND2, BARTOP
                  COLS( I, J ) = I
               END DO

*  Fills any remaining pixels above the bar with the background colour
*  index, 0.
               DO J = BARTOP + 1, UBND2
                  COLS( I, J ) = 0
               END DO

*  Draw the bar top in the current foreground colour to distinguish it
*  from the background.
               IF( BARTOP .GT. LBND2 ) COLS( I, BARTOP - 1 ) = 1
               COLS( I, BARTOP ) = 1

            END DO

*  If Axis 2 corresponds to colour index...
         ELSE

*  Find the pixels per count.
            SCALE = REAL( UBND1 - LBND1 + 1 )/MXPOP

*  Do each colour index.
            DO J = LBND2, UBND2

*  Get the population for this bin.
               POP = REAL( HSTDAT( J - LBND2 + 1 ) )
               IF( LOG ) THEN
                  IF( POP .GT. 1 ) THEN
                     POP = LOG10( POP )
                  ELSE
                     POP = 0.0
                  END IF
               END IF

*  Find the pixel index of the top of the histogram bar for the current
*  colour index.
               BARTOP = MIN( UBND1, NINT( POP*SCALE ) + LBND1 - 1 )

*  Fills the pixels with the current colour index up to the height of the
*  bar.
               DO I = LBND1, BARTOP
                  COLS( I, J ) = J
               END DO

*  Fills any remaining pixels above the bar with the background colour
*  index, 0.
               DO I = BARTOP + 1, UBND1
                  COLS( I, J ) = 0
               END DO

*  Draw the bar top in the current foreground colour to distinguish it
*  from the background.
               IF( BARTOP .GT. LBND1 ) COLS( BARTOP - 1, J ) = 1
               COLS( BARTOP, J ) = 1

            END DO

         END IF

*  For ramp keys...
      ELSE IF( FORM .EQ. RAMP ) THEN

*  If Axis 1 corresponds to colour index, just assign the Axis-1 value
*  to all cells of the returned colour index array.
         IF( CAXIS .EQ. 1 ) THEN
            DO J = LBND2, UBND2
               DO I = LBND1, UBND1
                  COLS( I, J ) = I
               END DO
            END DO

*  If Axis 2 corresponds to colour index, just assign the Axis-2 value
*  to all cells of the returned colour index array.
         ELSE
            DO J = LBND2, UBND2
               DO I = LBND1, UBND1
                  COLS( I, J ) = J
               END DO
            END DO
         END IF

      END IF

      END
