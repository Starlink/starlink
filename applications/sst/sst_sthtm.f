      SUBROUTINE SST_STHTM( FD, PACK, STATUS )
*+
*  Name:
*     SST_STHTM

*  Purpose:
*     Start an html document.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_STHTM( FD, PACK, STATUS )

*  Description:
*     The routine writes the commands necessary to start an html
*     document to the file connected to UNIT.

*  Arguments:
*     FD = INTEGER (Given)
*        FIO file descriptor. The html startup commands are written to
*        this file.
*     PACK = CHARACTER * ( * ) (Given)
*        Name of the package (or similar) that is being converted to
*        html. This ise used in the document titles.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify it under
*     the terms of the GNU General Public License as published by the Free Software
*     Foundation; either version 2 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,but WITHOUT ANY
*     WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
*     PARTICULAR PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License along with
*     this program; if not, write to the Free Software Foundation, Inc., 59 Temple
*     Place,Suite 330, Boston, MA  02111-1307, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     6-DEC-1994 (PDRAPER):
*        Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER FD
      CHARACTER * ( * ) PACK

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a title for the document.
      CALL FIO_WRITE( FD, '<TITLE>', STATUS )
      CALL FIO_WRITE( FD, PACK, STATUS )
      CALL FIO_WRITE( FD, ' -- routine descriptions.', STATUS )
      CALL FIO_WRITE( FD, '</TITLE>', STATUS )

*  And a header.
      CALL FIO_WRITE( FD, '<H1>', STATUS )
      CALL FIO_WRITE( FD, PACK, STATUS )
      CALL FIO_WRITE( FD, ' -- index of routine descriptions', STATUS )
      CALL FIO_WRITE( FD, '</H1>', STATUS )

99    CONTINUE
* @(#)sst_sthtm.f   1.4   95/03/06 10:56:50   96/07/05 10:27:33
      END
