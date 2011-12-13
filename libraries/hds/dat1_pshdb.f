      SUBROUTINE DAT1_PSHDB( STR, DEF, VALUE, STATUS )
*+
*  Name:
*     DAT1_PSHDB

*  Purpose:
*     Parse an HDS object dimension bound.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DAT1_PSHDB( STR, DEF, VALUE, STATUS )

*  Description:
*     The routine parses a string representing an upper or lower
*     dimension bound of an HDS array object. If the string is blank,
*     then a default value is returned. Leading and trailing spaces are
*     ignored.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        String to be parsed.
*     DEF = INTEGER (Given)
*        Default value to be returned if the string is blank.
*     VALUE = INTEGER (Returned)
*        Dimension bound value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Find the first and last non-blank characters in the string.
*     -  If the input string is blank, then return the default value.
*     -  Otherwise, attempt to convert the string to an integer.
*     -  If the attempt fails, then report an error message.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
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
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     DSB: David Berry (UCLan, Starlink)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     29-OCT-1990 (RFWS):
*        Original version.
*     7-DEC-1990 (RFWS):
*        Removed use of '*' to indicate the default bound; only a blank
*        is now used for this purpose.
*     15-FEB-1998 (DSB):
*        Brought into NDG from NDF.
*     23-DEC-2005 (TIMJ):
*        Brought into HDS
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'DAT_ERR'          ! DAT_ error codes

*  Arguments Given:
      CHARACTER * ( * ) STR
      INTEGER DEF

*  Arguments Returned:
      INTEGER VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER F                  ! Position of first non-blank character
      INTEGER L                  ! Position of last non-blank character

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the first and last non-blank characters in the string.
      CALL CHR_FANDL( STR, F, L )

*  If the input string is blank, then return the default value.
      IF ( F .GT. L ) THEN
         VALUE = DEF

*  Otherwise, attempt to convert the string to an integer.
      ELSE
         CALL CHR_CTOI( STR( F : L ), VALUE, STATUS )

*  If the attempt fails, then report an error message.
         IF ( STATUS .NE. SAI__OK ) THEN
            STATUS = DAT__DIMIN
            CALL EMS_SETC( 'BADBOUND', STR )
            CALL EMS_REP( 'DAT1_PSHDB_SYN',
     :                    'Invalid dimension bound ''^BADBOUND'' ' //
     :                    'specified; bad syntax.', STATUS )
         END IF
      END IF

      END
