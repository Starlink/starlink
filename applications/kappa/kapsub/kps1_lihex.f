      SUBROUTINE KPS1_LIHEX( EL, XPOS, XMIN, XMAX, STATUS )
*+
*  Name:
*     KPS1_LIHEX

*  Purpose:
*     Gets the the x extent of an line plot in histogram form.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LIHEX( EL, XPOS, XMIN, XMAX, STATUS )

*  Description:
*     This routine finds the x-axis limits for a line plot drawn as
*     an histogram.  The limits go from the first valid x co-ordinate
*     minus half the distance to the next valid co-ordinate, to the last
*     valid x co-ordinate plus half the distance to the previous valid
*     co-ordinate.  An error results if separate limits cannot be
*     located.

*  Arguments:
*     EL = INTEGER (Given)
*        The number of x co-ordinates.
*     XPOS( EL ) = REAL (Given)
*        The x co-ordinates of the points whose extent is to be
*        determined.
*     XMIN = REAL (Given)
*        The lower x co-ordinate bound of the histogram.
*     XMAX = REAL (Given)
*        The upper x co-ordinate bound of the histogram.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 October 2 (MJC):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER EL
      REAL XPOS( EL )

*  Arguments Returned:
      REAL XMIN
      REAL XMAX

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter
      INTEGER J                  ! Loop counter
      INTEGER K                  ! Loop counter
      INTEGER L                  ! Loop counter
      LOGICAL LOOP               ! Loop to search for valid co-ordinate?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate the lower limit.
*  ==========================

*  Find the first point with a valid x co-ordinate.
      I = 1
      LOOP = .TRUE.
      DO WHILE ( I .LE. EL .AND. LOOP )
         IF ( XPOS( I ) .NE. VAL__BADR ) THEN
            LOOP = .FALSE.
         ELSE
            I = I + 1
         END IF
      END DO

*  Report an error there is no point with a valid x co-ordinate.
      IF ( LOOP ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_LIHEX_BADX',
     :     'The x co-ordinate data are all bad.', STATUS )
         GOTO 999
      END IF

*  Obtain the indices to the next valid x co-ordinate.
      J = I + 1
      LOOP = .TRUE.
      DO WHILE ( J .LE. EL .AND. LOOP )
         IF ( XPOS( J ) .NE. VAL__BADR ) THEN
            LOOP = .FALSE.
         ELSE
            J = J + 1
         END IF
      END DO

*  Report an error there is no point with a valid x co-ordinate.
      IF ( LOOP ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_LIHEX_ONEX',
     :     'There is only one valid x co-ordinate.', STATUS )
         GOTO 999
      END IF

*  Now find the lower limit for the x co-ordinate.
      XMIN = XPOS( I ) - 0.5 * ( XPOS( J ) - XPOS( I ) )

*  Calculate the upper limit.
*  ==========================

*  Find the last point with a valid x co-ordinate.  Only loop back as
*  far as the second valid x co-ordinate.  This is still adequate even
*  if there are just two valid co-ordinates.  There is no need to check
*  for a valid value as we know there are at least two.
      K = EL
      LOOP = .TRUE.
      DO WHILE ( K .GE. J .AND. LOOP )
         IF ( XPOS( K ) .NE. VAL__BADR ) THEN
            LOOP = .FALSE.
         ELSE
            K = K - 1
         END IF
      END DO

*  Obtain the index to the previous valid x co-ordinate.  Can loop
*  back to the first valid co-ordinate to allow for just two valid
*  co-ordinates.  We already know that there are least two points so
*  there is no need to check the final value of LOOP.
      L = K - 1
      LOOP = .TRUE.
      DO WHILE ( L .GE. I .AND. LOOP )
         IF ( XPOS( L ) .NE. VAL__BADR ) THEN
            LOOP = .FALSE.
         ELSE
            L = L - 1
         END IF
      END DO

*  Now find the upper limit for the x co-ordinate.
      XMAX = XPOS( K ) + 0.5 * ( XPOS( K ) - XPOS( L ) )

  999 CONTINUE

      END
