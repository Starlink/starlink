      SUBROUTINE KPS1_AGNLS( IGRP, STATUS )
*+
*  Name:
*     KPS1_AGNLS

*  Purpose:
*     List the regions in the supplied group (for ARDGEN)

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_AGNLS( IGRP, STATUS )

*  Description:
*     The supplied GRP group contains the ARD descriptions for each
*     region.  The first region in the group has `index' 1, and
*     subsequent regions are indexed sequentially.  The `index' of a
*     region should not be confused with the indices used by GRP to
*     refer to individual elements within a group.  Each ARD
*     description may occupy several elements in the group.  The ARD
*     keyword for a region always starts at column 1 of a new element.
*     If there are too many arguments to fit them into the rest of the
*     element, then they continue in the next element.  Such
*     continuation elements are marked by the fact that they start with
*     one or more spaces.
*
*     Each region is displayed in turn with the corresponding region
*     index.  The user is requested to press RETURN to continue when
*     the screen is full.

*  Arguments:
*     IGRP = INTEGER (Given)
*        The GRP identifier for the group holding the ARD descriptions.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1995, 2004 Central Laboratory of the Research
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
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-DEC-1994 (DSB):
*        Original version.
*     1995 March 16 (MJC):
*        Corrected some typo's and used the modern style for variable
*        declarations.
*     6-OCT-2004 (DSB):
*        Use SHL_PAGTXT to output text.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP public constants

*  Arguments Given:
      INTEGER IGRP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER LINE*80          ! Output buffer
      CHARACTER TEXT*(GRP__SZNAM)! Element of text from the group
      INTEGER ARGLST             ! Position of start of argument list
      INTEGER I                  ! Current group element index
      INTEGER LINLEN             ! Used length of LINE
      INTEGER REG                ! Current region index
      INTEGER SIZE               ! Number of elements in group
      LOGICAL BLANK              ! Display a blank line?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the SHL paged output routines.
      CALL SHL_PAGRST( STATUS )

*  Space output from the previous text.
      CALL MSG_BLANK( STATUS )

*  Get the group size.  If it zero there are no defined regions, so
*  warn the user and return.
      CALL GRP_GRPSZ( IGRP, SIZE, STATUS )
      IF( SIZE .EQ. 0 ) THEN
         CALL MSG_OUT( 'KPS1_AGNLS_MSG1', 'There are currently no '/
     :                 /'regions defined.', STATUS )
         GO TO 999
      END IF

*  Initialise the next region index.
      REG = 0

*  Display a heading.  Abort if an error occurs while writing to the
*  screen.
      CALL SHL_PAGTXT( ' Region          Region Description', STATUS )
      CALL SHL_PAGTXT( ' Index', STATUS )

*  Put a blank line above the first region.
      BLANK = .TRUE.
      ARGLST = 1

*  Go through the group.
      DO I = 1, SIZE

*  Get the text of the next element.
         CALL GRP_GET( IGRP, I, 1, TEXT, STATUS )

*  If the first character is not blank, this is the start of a new
*  region.  Display a blank line if the previous line was a
*  continuation line.  Store the region index and text ready for
*  display.  Find the position of the opening parenthesis which marks
*  the start of the argument list.
         IF ( TEXT( 1 : 1 ) .NE. ' ' ) THEN
            REG = REG + 1

            IF ( BLANK ) THEN
               CALL SHL_PAGTXT( ' ', STATUS )
               BLANK = .FALSE.
            END IF

            LINE = ' '
            WRITE( LINE, '(I4)' ) REG
            LINE( 6 : ) = '  -  '//TEXT
            ARGLST = INDEX( LINE, '(' )

*  If the first character is blank, this element is a continuation of
*  the current region.  Store the text with the first non-blank
*  character aligned with the start of the argument list.
         ELSE
            BLANK = .TRUE.
            CALL CHR_LDBLK( TEXT )
            LINE = ' '
            LINE( ARGLST + 2: ) = TEXT

         END IF

*  Display the stored text.
         LINLEN = CHR_LEN( LINE )
         CALL SHL_PAGTXT( LINE( : LINLEN ), STATUS )

      END DO

      CALL SHL_PAGTXT( ' ', STATUS )

 999  CONTINUE

      END
