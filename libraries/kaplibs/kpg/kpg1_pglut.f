      SUBROUTINE KPG1_PGLUT( NCOL, COLS, LO, HI, NN, STATUS )
*+
*  Name:
*     KPG1_PGLUT

*  Purpose:
*     Uses an array of colour representations to set up the PGPLOT
*     colour table.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PGLUT( NCOL, COLS, LO, HI, NN, STATUS )

*  Description:
*     This routine stores new colour representations for a range of PGPLOT
*     colour indices. A list of NCOL colour representations is supplied.
*     These are normalized so that the highest value produces full colour
*     intensity. The first colour (1) is assigned to PGPLOT colour index LO,
*     and the last (NCOL) is assigned to PGPLOT colour index HI. The colour
*     representations for the PGPLOT colour indices between LO and HI
*     are formed by interpolation amonst the supplied NCOL colours, using
*     either nearest-neighbour or linear interpolation.

*  Arguments:
*     NCOL = INTEGER (Given)
*        The number of colour indices in the supplied colour table.
*     COLS( 3, NCOL ) = REAL (Given)
*        The lookup table.  The first dimension is RGB.  Values
*        should lie in the range 0.0--1.0.
*     LO = INTEGER (Given)
*        The lowest PGPLOT colour index to use.
*     HI  = INTEGER (Given)
*        The highest PGPLOT colour index to use.
*     NN = LOGICAL (Given)
*        If true, and the number of input and output colour indices are
*        different, the nearest-neighbour method is used for assigning
*        values in the output array.  Otherwise linear interpolation
*        is used.  Nearest-neighbour preserves sharp edges in the
*        lookup; linear interpolation is recommended for smoothly
*        varying lookup tables.
*     STATUS = INTEGER (Given)
*        Global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-OCT-2001 (DSB):
*        Original version.
*     14-NOV-2001 (DSB):
*        Only normalize the LUT if the max value is greater than 1.0
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No assumed typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*  Arguments Given:
      INTEGER NCOL
      REAL COLS( 3, NCOL )
      INTEGER LO
      INTEGER HI
      LOGICAL NN

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER I                 ! PGPLOT colour index
      INTEGER J                 ! Index into supplied colour table
      INTEGER J1                ! Lower index into supplied colour table
      INTEGER J2                ! Upper index into supplied colour table
      REAL NORM                 ! Normalization factor
      REAL TOP                  ! Highest intensity value
      REAL FJ                   ! Fraction index into supplied colour table
      REAL R, G, B              ! Red, green, blue intensities
      REAL SCALE                ! Scale factor between elements of the lookup tables
      REAL W1                   ! Weight for lower colour table entry
      REAL W2                   ! Weight for upper colour table entry
*.

*  Check the inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Find the largest supplied RGB intensity value.
      TOP = -1.0
      DO J = 1, NCOL
         TOP = MAX( COLS( 1, J ), MAX( COLS( 2, J ), COLS( 3, J ) ) )
      END DO

*  If this is larger than 1, set up the normalization factor which
*  scales the TOP value to unity.
      IF( TOP .NE. 0 .AND. TOP .GT. 1.0 ) THEN
         NORM = 1.0/TOP
      ELSE
         NORM = 1.0
      END IF

*  If exactly the correct number of colours have been supplied, just use
*  them as supplied, without interpolation.
      IF( HI - LO + 1 .EQ. NCOL ) THEN
         J = 1
         DO  I = LO, HI
            CALL PGSCR( I, NORM*COLS( 1, J ), NORM*COLS( 2, J ),
     :                  NORM*COLS( 3, J ) )
            J = J + 1
         END DO

*  Otherwise, we need to do interpolation.
      ELSE

*  Find the resampling scale factor.
         SCALE = REAL( NCOL - 1 ) / REAL( HI - LO  )

*  Nearest-neighbour method.
         IF ( NN ) THEN

*  For all the specified PGPLOT colour indices
            DO  I = LO, HI

*  Calculate the nearest position in the input table.
               J = MAX( 1, MIN( NCOL,
     :                  NINT( SCALE * REAL( I - LO ) ) + 1 ) )

*  Set the colour representation.
               CALL PGSCR( I, NORM*COLS( 1, J ), NORM*COLS( 2, J ),
     :                     NORM*COLS( 3, J ) )

            END DO

*  Linear interpolation method.
         ELSE

*  For all the specified PGPLOT colour indices
            DO  I = LO, HI

*  Calculate the fractional position in the input table.
               FJ = SCALE * REAL( I - LO )

*  Find the corresponding RGB values.
               IF( FJ .LE. 1.0 ) THEN
                  R = COLS( 1, 1 )
                  G = COLS( 2, 1 )
                  B = COLS( 3, 1 )

               ELSE IF( FJ .GE. REAL( NCOL ) ) THEN
                  R = COLS( 1, NCOL )
                  G = COLS( 2, NCOL )
                  B = COLS( 3, NCOL )

               ELSE
                  J1 = INT( FJ )
                  J2 = J1 + 1
                  W2 = FJ - REAL( J1 )
                  W1 = 1.0 - W2
                  R = W1*COLS( 1, J1 ) + W2*COLS( 1, J2 )
                  G = W1*COLS( 2, J1 ) + W2*COLS( 2, J2 )
                  B = W1*COLS( 3, J1 ) + W2*COLS( 3, J2 )
               END IF

*  Set the colour representation.
               CALL PGSCR( I, NORM*R, NORM*G, NORM*B )

            END DO

         END IF

      END IF

      END

