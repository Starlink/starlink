      SUBROUTINE ATL_FINDSKY( FRAME, SKYFRAME, LATAX, LONAX, STATUS )
*+
*  Name:
*     ATL_FINDSKY

*  Purpose:
*     Locate any sky axes within a Frame.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_FINDSKY( FRAME, SKYFRAME, LATAX, LONAX, STATUS )

*  Description:
*     This routine searches the supplied Frame (which may be a CmpFrame)
*     for a SkyFrame. If found, it returns a pointer to the SkyFrame,
*     together with the indices (within the supplied Frame) of the longitude
*     and latitude axes.

*  Arguments:
*     FRAME = INTEGER (Given)
*        The Frame to be searched.
*     SKYFRAME = INTEGER (Returned)
*        A pointer to the SkyFrame contained within FRAME, if any. If no
*        SkyFrame is found, AST__NULL is returned.
*     LATAX = INTEGER (Returned)
*        The index (one-based) of the celestial latitude axis in FRAME.
*        Returned equal to zero if no SkyFrame is found.
*     LONAX = INTEGER (Returned)
*        The index (one-based) of the celestial lonitude axis in FRAME.
*        Returned equal to zero if no SkyFrame is found.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
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
*     2-MAY-2007 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'ATL_PAR'          ! ATL constants

*  Arguments Given:
      INTEGER FRAME

*  Arguments Returned:
      INTEGER SKYFRAME
      INTEGER LATAX
      INTEGER LONAX

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER FS
      INTEGER INAX( 2 )
      INTEGER JUNK
      INTEGER MAP
      INTEGER NAX
      INTEGER OUTAX( ATL__MXDIM )
      INTEGER TMPLT

*  Initialise
      SKYFRAME = AST__NULL
      LONAX = 0
      LATAX = 0

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a default skyframe to use as the template when searching the
*  supplied Frame for SkyFrames.
      TMPLT = AST_SKYFRAME( ' ', STATUS )

*  Set the MinAxes and MaxAxes attributes of this SkyFrame to equal the
*  number of axes in the supplied Frame. Normally a SkyFrame will only
*  match a 2D Frame, but setting these attributes makes it possible for a
*  SkyFrame to match a Frame like the supplied Frame.
      NAX = AST_GETI( FRAME, 'Naxes', STATUS )
      CALL AST_SETI( TMPLT, 'MinAxes', NAX, STATUS )
      CALL AST_SETI( TMPLT, 'MaxAxes', NAX, STATUS )

*  Search for a SkyFrame in the supplied Frame.
      FS = AST_FINDFRAME( FRAME, TMPLT, ' ', STATUS )

*  Return immediately if no skyframe was found.
      IF( FS .NE. AST__NULL ) THEN

*  See which FRAME axes feed the two template SkyFrame axes.
         CALL AST_INVERT( FS, STATUS )
         INAX( 1 ) = AST_GETI( TMPLT, 'LatAxis', STATUS )
         INAX( 2 ) = AST_GETI( TMPLT, 'LonAxis', STATUS )
         CALL AST_MAPSPLIT( FS, 2, INAX, OUTAX, MAP, STATUS )
         IF( MAP .NE. AST__NULL ) THEN
            IF( AST_GETI( MAP, 'Nout', STATUS ) .EQ. 2 ) THEN

*  Pick the celestial axes from the supplied frame.
               SKYFRAME = AST_PICKAXES( FRAME, 2, OUTAX, JUNK, STATUS )

*  The lat/lon axes in FRAME will be in the same order as they are in
*  SKYFRAME. Get their indices.
               IF( AST_GETI( SKYFRAME, 'LatAxis', STATUS ) .EQ. 1 ) THEN
                  LATAX = OUTAX( 1 )
                  LONAX = OUTAX( 2 )
               ELSE
                  LATAX = OUTAX( 2 )
                  LONAX = OUTAX( 1 )
               END IF

*  Free resources
               CALL AST_ANNUL( JUNK, STATUS )
            END IF

            CALL AST_ANNUL( MAP, STATUS )
         END IF

         CALL AST_ANNUL( FS, STATUS )
      END IF

      CALL AST_ANNUL( TMPLT, STATUS )

      END
