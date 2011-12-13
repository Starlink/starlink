      SUBROUTINE CCD1_TMPNM( PREFIX, NAME, STATUS )
*+
*  Name:
*     CCD1_TMPNM

*  Purpose:
*     Creates a unique name string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_TMPNM( PREFIX, NAME, STATUS )

*  Description:
*     The routine appends a incremented number to the string PREFIX,
*     creating a unique name on each call. The name is PREFIXn.tmp,
*     where n is the new number.

*  Arguments:
*     PREFIX = CHARACTER * ( * ) (Given)
*        Prefix of the unique name. Note this must be supplied as
*        the unique part of the name is just a number and HDS files
*        may not begin with a number.
*     NAME = CHARACTER * ( * ) (Returned)
*        The generated name. This is truncated from the right if the
*        name will not fit. If it is impossible to fit the unique name
*        string into this then status will be set and an error
*        reported.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     26-FEB-1992 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants

*  Arguments Given:
      CHARACTER * ( * ) PREFIX

*  Arguments Returned:
      CHARACTER * ( * ) NAME

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( VAL__SZI ) NUMBER ! String holding current number
      INTEGER NCOUNT             ! Current number count.
      INTEGER OUTLEN             ! Length of output string.
      INTEGER NUMLEN             ! Length of numeric encoded string
      INTEGER PRELEN             ! Length of PREFIX string

*  Local Data:
      DATA NCOUNT / 0 /
      SAVE NCOUNT

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If NCOUNT is 0 then this is the first call. Generate a random number
*  to look more temporary (use the process PID).
      IF ( NCOUNT .EQ. 0 ) CALL PSX_GETPID( NCOUNT, STATUS )

*  Increment the number counter
      NCOUNT = NCOUNT + 1

*  Write the value into a string.
      CALL CHR_ITOC( NCOUNT, NUMBER, NUMLEN )

*  Get the length of the output string.
      OUTLEN = LEN( NAME )

*  Check this against the length of the number string.
      IF ( OUTLEN .LT. NUMLEN ) THEN

*  Output string not long enough.
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_TMPNM1',
     :'  CCD1_TMPNM: Error creating temporary name - output name'//
     : ' exceeds buffer length', STATUS )
      ELSE

*  Get the length of the prefix.
         PRELEN = LEN( PREFIX )

*  Truncate PRELEN to fit output string if required.
         PRELEN = MIN( ( OUTLEN - NUMLEN - 3 ), PRELEN )

*  Put PREFIX and NUMBER into output string.
         NAME = PREFIX( 1 : PRELEN ) // NUMBER( 1 : NUMLEN ) //'.tmp'
      END IF
      END
* $Id$
