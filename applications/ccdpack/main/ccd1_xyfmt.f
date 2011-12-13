      SUBROUTINE CCD1_XYFMT( XC, YC, FRAME, XSTR, YSTR, STATUS )
*+
*  Name:
*     CCD1_XYFMT

*  Purpose:
*     Formats an X and Y value according to a given AST frame.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_XYFMT( XC, YC, FRAME, XSTR, YSTR, STATUS )

*  Description:
*     This routine returns strings representing the value of an X and
*     a Y coordinate, in a format dicated by the FRAME parameter.
*     If FRAME is an AST pointer to a Frame object, then the
*     characteristics of this frame will determine the formatting.
*     If it is set to AST__NULL, the formatting will be done in a
*     default way.

*  Arguments:
*     XC = DOUBLE PRECISION (Given)
*        X value.
*     YC = DOUBLE PRECISION (Given)
*        Y value.
*     FRAME = INTEGER (Given)
*        AST pointer to a Frame object which governs the transformation,
*        or the value AST__NULL for default formatting.
*     XSTR = CHARACTER * ( * ) (Returned)
*        String containing the formatted X value.
*     YSTR = CHARACTER * ( * ) (Returned)
*        String containing the formatted Y value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999, 2001 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-NOV-1999 (MBT):
*        Original version.
*     25-APR-2001 (MBT):
*        Modified to take account of whether each SkyFrame axis should
*        be represented as a time or not.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST system constants

*  Arguments Given:
      DOUBLE PRECISION XC
      DOUBLE PRECISION YC
      INTEGER FRAME
      CHARACTER * ( * ) XSTR
      CHARACTER * ( * ) YSTR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER FRM                ! AST frame to use for formatting

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin a new AST context.
      CALL AST_BEGIN( STATUS )

*  Construct a suitable frame to control the formatting.

*  If we did not get a valid frame in the first place, construct a
*  default one.
      IF ( FRAME .EQ. AST__NULL ) THEN
         FRM = AST_FRAME( 2, 'Unit(1)=pixels,Unit(2)=pixels', STATUS )

*  If we did get a frame, tweak it to give the output as we want.
      ELSE
         FRM = AST_COPY( FRAME, STATUS )
         IF ( AST_ISASKYFRAME( FRM, STATUS ) ) THEN
            IF ( AST_GETI( FRM, 'AsTime(1)', STATUS ) .NE. 0 ) THEN
               CALL AST_SETC( FRM, 'Format(1)', '+ihmst.3', STATUS )
            ELSE
               CALL AST_SETC( FRM, 'Format(1)', '+idmst.2', STATUS )
            END IF
            IF ( AST_GETI( FRM, 'AsTime(2)', STATUS ) .NE. 0 ) THEN
               CALL AST_SETC( FRM, 'Format(2)', '+ihmst.3', STATUS )
            ELSE
               CALL AST_SETC( FRM, 'Format(2)', '+idmst.2', STATUS )
            END IF
         ELSE
            CALL AST_SETC( FRM, 'Format(1)', '%+14.6f', STATUS )
            CALL AST_SETC( FRM, 'Format(2)', '%+14.6f', STATUS )
         END IF
      END IF

*  Now use the frame we have to do the actual formatting.
      XSTR = AST_FORMAT( FRM, 1, XC, STATUS )
      YSTR = AST_FORMAT( FRM, 2, YC, STATUS )

*  Exit AST context.
      CALL AST_END( STATUS )

      END
* $Id$
