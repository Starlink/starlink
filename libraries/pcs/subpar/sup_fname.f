      SUBROUTINE SUBPAR_FNAME( INNAM, OUTNAM, NAMLEN, STATUS )
*+
*  Name:
*     SUBPAR_FNAME

*  Purpose:
*     To return a fully expanded filename

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_FNAME( INNAM, OUTNAM, NAMLEN, STATUS )

*  Description:
*     This is the VMS version of the subroutine.
*     It does not need to do anything except transfer INNAM to OUTNAM and
*     find its used length.

*
*  Deficiencies:
*
*  Arguments:
*     INNAM = CHARACTER*(*) (Given)
*        The filename to be expanded
*     OUTNAM = CHARACTER*(*) (Returned)
*        The filename expanded
*     NAMLEN = INTEGER (Returned)
*        The used length of OUTNAM
*     STATUS = INTEGER (Given)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-AUG-1991 (AJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER*(*) INNAM

*  Arguments Returned:
      CHARACTER*(*) OUTNAM
      INTEGER NAMLEN

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of string
      EXTERNAL CHR_LEN

*  Local Variables:
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      OUTNAM = INNAM
      NAMLEN = CHR_LEN( OUTNAM )

      END
