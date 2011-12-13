      LOGICAL FUNCTION CCD1_ISCOM( LINE, STATUS )
*+
*  Name:
*     CCD1_ISCOM

*  Purpose:
*     To check whether a given line is a comment line.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CCD1_ISCOM( LINE, STATUS )

*  Description:
*     The routine simply checks the first character to see if it is an
*     `!' or a `#'. The routine also checks to see if the line is
*     completly blank, if so it is treat like a comment line.

*  Arguments:
*     LINE = CHARACTER * ( * ) (Given)
*        The line which is to be checked.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     CCD1_ISCOM = LOGICAL
*        Set true if the line is to be treat as a comment line.

*  Notes:
*     The input line should have its leading blanks removed.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-FEB-1992 (PDRAPER):
*        Original Version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) LINE

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set default return
      CCD1_ISCOM = .FALSE.

*  Check for comment delimeters.
      IF ( LINE( 1 : 1 ) .EQ. '!' ) THEN
          CCD1_ISCOM = .TRUE.
      ELSE IF ( LINE( 1 : 1 ) .EQ. '#' ) THEN
          CCD1_ISCOM = .TRUE.
      ELSE IF ( LINE( 1: 1 ) .EQ. ' ' ) THEN
          CCD1_ISCOM = .TRUE.
      END IF

      END
* $Id$
