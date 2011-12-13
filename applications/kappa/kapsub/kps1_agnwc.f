      SUBROUTINE KPS1_AGNWC( IPLOT, IGRP, STATUS )
*+
*  Name:
*     KPS1_AGNWC

*  Purpose:
*     Add an ARD WCS or COFRAME statement to the group created by ARDGEN.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_AGNWC( IPLOT, IGRP, STATUS )

*  Description:
*     Adds a WCS or COFRAME statement describing the Frame in which the
*     positions are given within the ARD description. The supplied group
*     is deleted, and an identifier for a new group is returned.

*  Arguments:
*     IPLOT = INTEGER (Given)
*        An AST pointer to the Plot.
*     IGRP = INTEGER (Given and Returned)
*        The GRP group containing the ARD description
*     STATUS = INTEGER (Given and Returned)
*        The inherited status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-SEP-2001 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'AST_PAR'          ! AST functions and constants

*  Arguments Given:
      INTEGER IPLOT

*  Arguments Given and Returned:
      INTEGER IGRP

*  Status:
      INTEGER STATUS

*  External References:
      LOGICAL CHR_SIMLR          ! Strings equal apart from case?

*  Local Variables:
      CHARACTER TEXT*(GRP__SZNAM)! GRP element text
      INTEGER IAT                ! Used length of the string
      INTEGER I                  ! GRP element index
      INTEGER IGRP2              ! GRP identifier for group to be plotted
      INTEGER FRM                ! WCS Frame
      INTEGER SIZE               ! Size of group.

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a new group.
      CALL GRP_NEW( ' ', IGRP2, STATUS )

*  Get the current Frame in the Plot.
      FRM = AST_GETFRAME( IPLOT, AST__CURRENT, STATUS )

*  If the current Frame is a simple Frame, add a simple COFRAME statement
*  containing just the domain name.
      IF( CHR_SIMLR( AST_GETC( FRM, 'CLASS', STATUS ), 'FRAME' ) ) THEN
         TEXT = 'COFRAME('
         IAT = 8
         CALL CHR_APPND( AST_GETC( FRM, 'DOMAIN', STATUS ), TEXT, IAT )
         CALL CHR_APPND( ')', TEXT, IAT )
         CALL GRP_PUT( IGRP2, 1, TEXT( : IAT ), 0, STATUS )

*  If the current Frame in Plot is a SkyFrame, add a COFRAME statement
*  containing the major extra SkyFrame attributes (if set).
      ELSE IF( CHR_SIMLR( AST_GETC( FRM, 'CLASS', STATUS ),
     :                    'SKYFRAME' ) ) THEN
         TEXT = 'COFRAME(SKY,SYSTEM='
         IAT = 19
         CALL CHR_APPND( AST_GETC( FRM, 'SYSTEM', STATUS ), TEXT, IAT )

         IF( AST_TEST( FRM, 'EQUINOX', STATUS ) ) THEN
            CALL CHR_APPND( ',EQUINOX=', TEXT, IAT )
            CALL CHR_APPND( AST_GETC( FRM, 'EQUINOX', STATUS ), TEXT,
     :                      IAT )
         END IF

         IF( AST_TEST( FRM, 'EPOCH', STATUS ) ) THEN
            CALL CHR_APPND( ',EPOCH=', TEXT, IAT )
            CALL CHR_APPND( AST_GETC( FRM, 'EPOCH', STATUS ), TEXT,
     :                      IAT )
         END IF

         CALL CHR_APPND( ')', TEXT, IAT )
         CALL GRP_PUT( IGRP2, 1, TEXT( : IAT ), 0, STATUS )

*  For other classes of Frame, write out the full Frame description using
*  a WCS statement.
      ELSE

*  Create a FrameSet holding just the current Frame of the Plot, and
*  put it into the new group.
         CALL ARD_PTWCS( AST_FRAMESET( FRM, ' ', STATUS ), IGRP2,
     :                   STATUS )

      END IF

*  Now append the contents of the supplied group to the end of the new
*  group.
      CALL GRP_GRPSZ( IGRP, SIZE, STATUS )
      DO I = 1, SIZE
         CALL GRP_GET( IGRP, I, 1, TEXT, STATUS )
         CALL GRP_PUT( IGRP2, 1, TEXT, 0, STATUS )
      END DO

*  Delete the original group.
      CALL GRP_DELET( IGRP, STATUS )

*  Return the new group.
      IGRP = IGRP2

      END
