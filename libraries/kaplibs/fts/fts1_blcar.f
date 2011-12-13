      LOGICAL FUNCTION FTS1_BLCAR( RECORD )
*+
*  Name:
*     FTS1_BLCAR

*  Purpose:
*     Determines whether or not the first card in a FITS record has a
*     blank keyword.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = FTS1_BLCAR( RECORD )

*  Description:
*     This routine is needed for UNIX portability, since the calling
*     application only knows the pointer to the FITS record, and not the
*     actual values.  Hence a function is required.

*  Arguments:
*     RECORD( 36 ) = CHARACTER * ( 80 ) (Given)
*        The FITS record to be tested.

*  Returned Value:
*     FTS1_BLCAR = LOGICAL
*        If true, the first card image in RECORD has a blank keyword,
*        i.e. characters 1 to 8 in the card image are blank.

*  [optional_function_items]...
*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1993 January 5 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( 36 ) RECORD( 80 )

*.

*  Test the keyword value.
      IF ( RECORD( 1 )( 1:8 ) .EQ. '        ' ) THEN
         FTS1_BLCAR = .TRUE.
      ELSE
         FTS1_BLCAR = .FALSE.
      END IF

      END
