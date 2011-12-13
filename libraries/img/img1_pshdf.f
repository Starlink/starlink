      SUBROUTINE IMG1_PSHDF( STR, DIM, LBND, UBND, STATUS )
*+
*  Name:
*     IMG1_PSHDF

*  Purpose:
*     Parse an HDS dimension bound field.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_PSHDF( STR, DIM, LBND, UBND, STATUS )

*  Description:
*     The routine parses a dimension bound field for an HDS array
*     object to determine the lower and upper bounds to be applied when
*     selecting a subset in that dimension. The lower and upper bounds
*     are separated by a colon ':' (e.g. '10:20').  Suitable default
*     values are returned if either or both halves of the field are
*     omitted (e.g. '100:', ':100', ':', etc.). If no colon is present,
*     then the upper bound is set to equal the lower bound (unless the
*     string is blank, which is equivalent to ':').

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        The string to be parsed.
*     DIM = INTEGER (Given)
*        The object dimension size.
*     LBND = INTEGER (Returned)
*        Lower bound.
*     UBND = INTEGER (Returned)
*        Upper bound.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The values obtained by parsing the string are constrained to
*     lie within the object bounds. An error will be reported, and
*     STATUS set, if they do not.
*     -  An error will be reported, and STATUS set, if the lower bound
*     exceeds the upper bound.

*  Algorithm:
*     -  Find the first and last non-blank characters in the string.
*     -  If the string is blank, then return the default lower and
*     upper bounds.
*     -  Otherwise, locate the colon ':' which separates the lower and
*     upper bounds.
*     -  If a colon appears at the start, then use the default lower
*     bound.
*     -  Otherwise, parse the string in front of the colon to obtain
*     the lower bound.
*     -  Check that the value obtained lies inside the object bounds
*     and report an error if it does not.
*     -  If there is no colon present, then the upper bound equals the
*     lower bound.
*     -  Otherwise, if the colon appears at the end of the string, then
*     use the default upper bound.
*     -  Otherwise, parse the string which follows the colon to
*     determine the upper bound.
*     -  Check that the value obtained lies inside the object bounds
*     and report an error if it does not.
*     -  If the lower bound exceeds the upper bound, then report an
*     error.

*  Copyright:
*     Copyright (C) 1990, 1991, 1994 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     29-OCT-1990 (RFWS):
*        Original version.
*     14-NOV-1990 (RFWS):
*        Re-structured status checking.
*     6-DEC-1990 (RFWS):
*        Added checks that the values obtained lie within the HDS object
*        bounds.
*     7-DEC-1990 (RFWS):
*        Removed use of '*' to indicate default bounds.
*     11-DEC-1990 (RFWS):
*        Improved error reports.
*     25-FEB-1991 (RFWS):
*        Fixed error in substring indices.
*     11-AUG-1994 (PDRAPER):
*        Imported into IMG_ from NDF_ (was NDF1_PSHDF).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_ERR'          ! IMG_ error codes

*  Arguments Given:
      CHARACTER * ( * ) STR
      INTEGER DIM

*  Arguments Returned:
      INTEGER LBND
      INTEGER UBND

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER COLON              ! Character position of colon ':'
      INTEGER F                  ! Position of first non-blank character
      INTEGER L                  ! Position of last non-blank character

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the first and last non-blank characters in the string.
      CALL CHR_FANDL( STR, F, L )

*  If the string is blank, then return the default lower and upper
*  bounds.
      IF ( F .GT. L ) THEN
         LBND = 1
         UBND = DIM

*  Otherwise, locate the colon ':' which separates the lower and upper
*  bounds.
      ELSE
         COLON = INDEX( STR, ':' )
         IF ( COLON .EQ. 0 ) COLON = LEN( STR ) + 1

*  If a colon appears at the start, then use the default lower bound.
         IF ( COLON .LE. F ) THEN
            LBND = 1

*  Otherwise, parse the string in front of the colon to obtain the
*  lower bound.
         ELSE
            CALL IMG1_PSHDB( STR( F : COLON - 1 ), 1, LBND, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that the value obtained lies inside the object bounds and
*  report an error if it does not.
               IF ( ( LBND .LT. 1 ) .OR.
     :              ( LBND .GT. DIM ) ) THEN
                  STATUS = IMG__BDBND
                  CALL MSG_SETI( 'LBND', LBND )
                  CALL MSG_SETI( 'DIM', DIM )
                  CALL ERR_REP( 'IMG1_PSHDF_LBND',
     :                          'Lower bound (^LBND) lies outside ' //
     :                          'object bounds (1:^DIM).', STATUS )
               END IF
            END IF
         END IF

*  If there is no colon present, then the upper bound equals the lower
*  bound.
         IF ( COLON .GT. L ) THEN
            UBND = LBND

*  Otherwise, if the colon appears at the end of the string, then use
*  the default upper bound.
         ELSE IF ( COLON .EQ. L ) THEN
            UBND = DIM

*  Otherwise, parse the string which follows the colon to determine the
*  upper bound.
         ELSE
            CALL IMG1_PSHDB( STR( COLON + 1 : L ), DIM, UBND, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that the value obtained lies inside the object bounds and
*  report an error if it does not.
               IF ( ( UBND .LT. 1 ) .OR.
     :              ( UBND .GT. DIM ) ) THEN
                  STATUS = IMG__BDBND
                  CALL MSG_SETI( 'UBND', UBND )
                  CALL MSG_SETI( 'DIM', DIM )
                  CALL ERR_REP( 'IMG1_PSHDF_UBND',
     :                          'Upper bound (^UBND) lies outside ' //
     :                          'object  bounds (1:^DIM).', STATUS )
               END IF
            END IF
         END IF
      END IF

*  If the lower bound exceeds the upper bound, then report an error.
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( LBND .GT. UBND ) ) THEN
         STATUS = IMG__BDBND
         CALL MSG_SETI( 'LBND', LBND )
         CALL MSG_SETI( 'UBND', UBND )
         CALL ERR_REP( 'IMG1_PSHDF_ERR',
     :                 'Lower bound (^LBND) exceeds upper bound ' //
     :                 '(^UBND).', STATUS )
      END IF
      END
* $Id$
