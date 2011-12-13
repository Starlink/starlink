      SUBROUTINE KPS1_LSHCP( FIRST, LAST, STEP, NPOS, NCAX, LABIN, POS,
     :                       ID, NDISP, SID, SPOS, LABOUT, STATUS )
*+
*  Name:
*     KPS1_LSHCP

*  Purpose:
*     Copy the selected positions to be displayed by LISTSHOW.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LSHCP( FIRST, LAST, STEP, NPOS, NCAX, LABIN, POS, ID, NDISP,
*                      SID, SPOS, LABOUT, STATUS )

*  Description:
*     This routine copies positions and identifiers from the input
*     arrays to the output arrays. Only positions with identifiers
*     in the range FIRST to LAST (inclusive) are copied.

*  Arguments:
*     FIRST = INTEGER (Given)
*        The lowest position identifier to be selected.
*     LAST = INTEGER (Given)
*        The highest position identifier to be selected.
*     STEP = INTEGER (Given)
*        The increment between position identifier to be selected.
*     NPOS = INTEGER (Given)
*        The number of supplied position identifiers.
*     NCAX = INTEGER (Given)
*        The number of axes for the supplied positions.
*     LABIN = INTEGER (Given)
*        A GRP identifier for a group holding all the position labels.
*        Supplied equal to GRP__NOID if there are no labels.
*     POS( NPOS, NCAX ) = DOUBLE PRECISION (Given)
*        The supplied positions.
*     ID( NPOS ) = INTEGER (Given)
*        The supplied position identifiers.
*     NDISP = INTEGER (Given)
*        The number of positions identifiers within ID which are within
*        the range FIRST to LAST (inclusive).
*     SID( NDISP ) = INTEGER (Returned)
*        The selected position identifiers.
*     SPOS( NDISP, NCAX ) = DOUBLE PRECISION (Returned)
*        The selected positions.
*     LABOUT = INTEGER (Returned)
*        A GRP identifier for a group holding the position labels for the
*        selected positios. Returned equal to GRP__NOID if there are no
*        labels.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-SEP-1998 (DSB):
*        Original version.
*     21-NOV-2006 (DSB):
*        Added arguments LABIN and LABOUT.
*     18-FEB-2010 (DSB):
*        Added argument STEP.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants

*  Arguments Given:
      INTEGER FIRST
      INTEGER LAST
      INTEGER STEP
      INTEGER NPOS
      INTEGER NCAX
      INTEGER LABIN
      DOUBLE PRECISION POS( NPOS, NCAX )
      INTEGER ID( NPOS )
      INTEGER NDISP

*  Arguments Returned:
      INTEGER SID( NDISP )
      DOUBLE PRECISION SPOS( NDISP, NCAX )
      INTEGER LABOUT

*  Status:
      INTEGER STATUS              ! Global status

*  Local Variables:
      CHARACTER LABEL*(GRP__SZNAM)! Label text
      INTEGER I                   ! Input position count
      INTEGER J                   ! Output position count
      INTEGER K                   ! Axis count
*.

      LABOUT = GRP__NOID

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the index of the previous position stored in the output
*  arrays.
      J = 0

*  Create an output labels group if required.
      IF( LABIN .NE. GRP__NOID ) CALL GRP_NEW( ' ', LABOUT, STATUS )

*  Check each position.
      DO I = 1, NPOS

*  If this position identifier is within the sipplied range...
         IF( ID( I ) .GE. FIRST .AND.
     :       ID( I ) .LE. LAST .AND.
     :       MOD( ID( I ) - FIRST, STEP ) .EQ. 0 ) THEN

*  Find the index of the position in the output arrays. Skip the position
*  if we have found too many.
            J = J + 1
            IF( J .LE. NDISP ) THEN

*  Copy all axis values for this position from the input array to the
*  output array.
               DO K = 1, NCAX
                  SPOS( J, K ) = POS( I, K )
               END DO

*  Copy the position identifier from the input array to the output array.
               SID( J ) = ID( I )

*  Copy the label to the outptu labels group.
               IF( LABIN .NE. GRP__NOID ) THEN
                  CALL GRP_GET( LABIN, I, 1, LABEL, STATUS )
                  CALL GRP_PUT1( LABOUT, LABEL, 0, STATUS )
               ENDIF

            END IF

         END IF

      END DO

      END
