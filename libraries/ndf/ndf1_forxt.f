      SUBROUTINE NDF1_FORXT( NAME, X1, X2, STATUS )
*+
*  Name:
*     NDF1_FORXT

*  Purpose:
*     Locate a foreign format extension specified within a complere
*     structure specification.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_FORXT( NAME, X1, X2, STATUS )

*  Description:
*     Some foreign formats can hold the equaivalent of several NDFs within
*     a single data file. An example is the FITS format. FITS files may
*     include image extensions, each of which could give rise to a
*     separate NDF. When a foreign data structure is specified, any syntax
*     required to identify a particular sub-structure within the foreign
*     format file should be specified after the file type (or file name if
*     no file type is given), and before any NDF slice specification. The
*     syntax should be enclosed within matching square brackets.
*
*     This routine looks for such syntax within the specified name, and
*     returns the index of the opening and closing square brackets. No
*     checks can be performed on the validity of the syntax since it is
*     format-specific.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        Name of the foreign NDF to be opened. NOTE, this should not
*        include an NDF slice specification.
*     X1 = INTEGER (Returned)
*        Index of the opening square bracket within NAME, or one more
*        than the used length of the string if the name does not include
*        a foreign extension specifier.
*     X2 = INTEGER (Returned)
*        Index of the closing square bracket within NAME, or -1 if the name
*        does not include a foreign extension specifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     If this routine is called with STATUS set, then value of zero
*     and -1 will be returned for X1 and X2. The same values will also be
*     returned if the routine should fail for any reason.

*  Copyright:
*     Copyright (C) 2000 Science & Engineering Research Council

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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JUL-2000 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) NAME

*  Arguments Returned:
      INTEGER X1
      INTEGER X2

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      INTEGER I                  ! Character index
      INTEGER LN                 ! Used length of NAME
*.

*  Set initial values for the X1 and X2 arguments.
      X1 = CHR_LEN( NAME ) + 1
      X2 = -1

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Save the used length of the string.
      LN = X1 - 1

*  Any NDF slice specification should have been removed from NAME prior
*  to calling this routine. Therefore, if there is a foreign extension
*  specifier, the final non-blank character should be a "]".
      IF( NAME( LN : LN ) .EQ. ']' ) THEN

*  Find the preceeding opening bracket "[".
         DO I = LN - 1, 1, -1
            IF( NAME( I : I ) .EQ. '[' ) THEN
               X1 = I
               X2 = LN
               GO TO 1
            END IF
         END DO
 1       CONTINUE

      END IF

      END
