      SUBROUTINE KPS1_AGNCH( IGRP, MXGRP, GRPS, TOPGRP, CHANGE, STATUS )
*+
*  Name:
*     KPS1_AGNCH

*  Purpose:
*     Has the ARDGEN group contents changed?

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_AGNCH( IGRP, MXGRP, GRPS, TOPGRP, CHANGE, STATUS )

*  Description:
*     Compares the group IGRP with the TOPRP entry in the GRPS list, and
*     returns .TRUE. for CHANGE if any entries are different.

*  Arguments:
*     IGRP = INTEGER (Given)
*        The GRP group containing the regions.
*     MXGRP = INTEGER (Given)
*        The size of the GRPS array.
*     GRPS( MXGRP ) = INTEGER (Given)
*        The list of GRP groups forming the undo list.
*     TOPGRP = INTEGER (Given)
*        The index within GRPS of the most recently stored group.
*     CHANGE = LOGICAL (Returned)
*        .TRUE. if the contents of IGRP differ from the contents of
*        GRPS( TOPGRP ).
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
*     25-SEP-2001 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'GRP_PAR'          ! GRP constants

*  Arguments Given:
      INTEGER IGRP
      INTEGER MXGRP
      INTEGER GRPS( MXGRP )
      INTEGER TOPGRP

*  Arguments Returned:
      LOGICAL CHANGE

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER NAME1*(GRP__SZNAM)! GRP element text from IGRP
      CHARACTER NAME2*(GRP__SZNAM)! GRP element text from GRPS( TOPGRP )
      INTEGER I                  ! GRP element index
      INTEGER SIZE1              ! Size of group IGRP.
      INTEGER SIZE2              ! Size of group GRPS( TOPGRP ).

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If there is no top group, return .TRUE. if the IGRP group has any
*  contents, and false otherwise.
      IF( TOPGRP .LT. 1 .OR. GRPS( TOPGRP ) .EQ. GRP__NOID ) THEN
         CALL GRP_GRPSZ( IGRP, SIZE1, STATUS )
         CHANGE = ( SIZE1 .GT. 0 )
      ELSE

*  Get the sizes of the two groups.
         CALL GRP_GRPSZ( IGRP, SIZE1, STATUS )
         CALL GRP_GRPSZ( GRPS( TOPGRP ), SIZE2, STATUS )

*  If the size of the two groups differ, return .TRUE.
         IF( SIZE1 .NE. SIZE2 ) THEN
            CHANGE = .TRUE.

*  Otherwise, loop through the contents of hte two groups until a
*  different entry is found.
         ELSE
            CHANGE = .FALSE.
            DO I = 1, SIZE1
               CALL GRP_GET( IGRP, I, 1, NAME1, STATUS )
               CALL GRP_GET( GRPS( TOPGRP ), I, 1, NAME2, STATUS )
               IF( NAME1 .NE. NAME2 ) THEN
                  CHANGE = .TRUE.
                  GO TO 10
               END IF
            END DO

 10         CONTINUE

         END IF
      END IF

      END
