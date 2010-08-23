      INTEGER FUNCTION KPG1_ASPLN( GRFCON, N, X, Y )
*+
*  Name:
*     KPG1_ASPLN

*  Purpose:
*     Draws a line for an AST Plot and log it at the same time.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = KPG1_ASPLN( GRFCON, N, X, Y )

*  Description:
*     This routine is intended to be registered with an AST Plot (using
*     AST_GRFSET) so that it is subsequently used to draw all lines. When
*     called by AST, it calls the underlying PGPLOT line drawing function
*     to draw the polyline specified by N, X and Y, and then records the
*     details of the drawn polyline in an AST KeyMap (previously specified
*     by calling KPG1_ASPLG). This KeyMap can then be interogated in order
*     to determine what lines were drawn. A new entry is added to the
*     KeyMap each time this function is called. Each such entry is itself
*     a KeyMap containing three entries with keys "N", "X" and "Y". The "N"
*     entry is a scalar integer holding the corresponding N value, and
*     the other two are single precision vector entries holding the X and
*     Y values.
*
*     KPG1_ASPLG should be called prior to registering this function
*     using AST_GRFSET.

*  Arguments:
*     GRFCON = INTEGER (Given)
*        A KeyMap containing information passed from the calling application.
*     N = INTEGER (Given)
*        The number of pointsin the polyline.
*     X( N ) = REAL (Given)
*        The graphics X coord at each point on the polyline.
*     Y( N ) = REAL (Given)
*        The graphics Y coord at each point on the polyline.

*  Notes:
*     -  The PGPLOT interface to the AGI library should be opened before
*     calling this routine.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-JUN-2006 (DSB):
*        Original version.
*     12-JUN-2006 (DSB):
*        Avoid direct access to the KPG_AST common blocks.
*     11-SEP-2007 (DSB):
*        Add new GRFCON parameter, required by AST_GRFSET.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Needed by KPG_AST
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  External References:
      INTEGER KPG1_GETASTPLN
      LOGICAL KPG1_GETASTBLE
      REAL KPG1_GETASTX1
      REAL KPG1_GETASTX2
      REAL KPG1_GETASTY1
      REAL KPG1_GETASTY2

*  Arguments Given:
      INTEGER GRFCON
      INTEGER N
      REAL X( N )
      REAL Y( N )

*  Local Variables:
      CHARACTER KEY*20           ! Key for new polyline description
      INTEGER I                  ! Point index
      INTEGER IAT                ! Used length of string
      INTEGER KM                 ! KeyMap holding supplied N, X and Y
      INTEGER LSTAT              ! Local status used within this routine
      INTEGER PLN                ! Pointer to an AST KeyMap
      LOGICAL BAD                ! Should the line be ignored?
      REAL X1                    ! The X value at the left edge
      REAL X2                    ! The X value at the right edge
      REAL Y1                    ! The Y value at the bottom edge
      REAL Y2                    ! The Y value at the top edge
*.

*  Assume no error.
      KPG1_ASPLN = 1

*  Initialise a local status variable
      LSTAT = SAI__OK

*  Check some points are being drawn.
      IF( N .GT. 0 ) THEN

* Save local copies of the reqired global variables.
         PLN = KPG1_GETASTPLN()
         X1 = KPG1_GETASTX1()
         Y1 = KPG1_GETASTY1()
         X2 = KPG1_GETASTX2()
         Y2 = KPG1_GETASTY2()

*  If we are keeping the edges blank, see if any of the points in the
*  polyline touch the edge.
         BAD = .FALSE.
         IF( KPG1_GETASTBLE() ) THEN
            DO I = 1, N
               IF( ABS( X( I ) - X1 ) .LT. 0.1 .OR.
     :             ABS( X( I ) - X2 ) .LT. 0.1 .OR.
     :             ABS( Y( I ) - Y1 ) .LT. 0.1 .OR.
     :             ABS( Y( I ) - Y2 ) .LT. 0.1 ) THEN
                  BAD = .TRUE.
                  GO TO 10
               END IF
            END DO
         END IF

*  Draw the polyline using PGPLOT.
 10      IF( .NOT. BAD ) CALL PGLINE( N, X, Y )

*  Create a KeyMap to hold the details of this polyline.
         KM = AST_KEYMAP( ' ', LSTAT )

*  Add in the N, X and Y values to this KeyMap.
         CALL AST_MAPPUT0I( KM, 'N', N, ' ', LSTAT )
         CALL AST_MAPPUT1R( KM, 'X', N, X, ' ', LSTAT )
         CALL AST_MAPPUT1R( KM, 'Y', N, Y, ' ', LSTAT )

*  Add this KeyMap into the global KeyMap. The key is of the form
*  "POLYLINE<i>" where <i> is an integer index.
         KEY = 'POLYLINE'
         IAT = 8
         CALL CHR_PUTI( AST_MAPSIZE( PLN, LSTAT ) + 1, KEY, IAT )

         CALL AST_MAPPUT0A( PLN, KEY( : IAT ), KM, ' ', LSTAT )

*  Tidy up.
         CALL AST_ANNUL( KM, LSTAT )

      END IF

*  Test for error.
      IF( LSTAT .NE. SAI__OK ) KPG1_ASPLN = 0

      END
