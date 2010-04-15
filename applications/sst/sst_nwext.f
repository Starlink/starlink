      SUBROUTINE SST_NWEXT( EXT, FNAME, STATUS )
*+
*  Name:
*    SST_NWEXT

*  Purpose:
*     Replace/add a new extension to a file name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_NWEXT( EXT, FNAME, STATUS )

*  Description:
*     This routine locates the file extension in the given name and
*     replaces it with a new extension. The signature for a file
*     extension is the first period from the right of the string, that
*     is also left of a given set of delimeters. For UNIX file names the
*     delimeter is '/' for VMS ']' or '>'.

*  Arguments:
*     EXT = CHARACTER * ( * )  (Given)
*        The new extension for the file name. This should include the
*        period.
*     FNAME = CHARACTER * ( * )  (Given and Returned)
*        The filename. On exit this is the new name.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Machine-Specific Features Used:
*     Uses the system specific routine SST_SYSNM.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councls.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     6-DEC-1994 (PDRAPER):
*        Original version.
*     28-AUG-2004 (TIMJ):
*        Force initialisation of STRLEN
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * )  EXT

*  Arguments Given and Returned:
      CHARACTER * ( * )  FNAME

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 4 ) SYSNAM   ! Name of system
      CHARACTER * ( 2 ) DELIMS   ! Final directory field delimeter
      INTEGER I                  ! Loop variable
      INTEGER IAPPN              ! Append position
      INTEGER STRLEN             ! Used length of string
      LOGICAL ALLBLK             ! If all characters so far are blank

*.
      STRLEN = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get name of system and set delimeters.
      CALL SST_SYSNM( SYSNAM, STATUS )
      IF ( SYSNAM .EQ. 'UNIX' ) THEN
         DELIMS = '/'
      ELSE
         DELIMS = ']>'
      END IF

*  Scan the file from the right looking for the first period.
      ALLBLK = .TRUE.
      IAPPN = 1
      DO 1 I = LEN( FNAME ), 1, -1

*  Keep track of first non-blank character, we might need this position
*  if file extension is absent.
         IF ( FNAME( I : I ) .NE. ' ' .AND. ALLBLK ) ALLBLK = .FALSE.
         IF ( FNAME( I : I ) .EQ. '.' ) THEN

*  Is there a delimeter to the right of this point?
            IF ( INDEX( DELIMS, FNAME( I : ) ) .EQ. 0 ) THEN

*  Must be extension period.
               IAPPN = I
            ELSE

*  Delimeter before period, so just append the next file extension (must
*  be none).
               IAPPN = STRLEN
            END IF
         END IF
         IF ( ALLBLK ) STRLEN = I
 1    CONTINUE

*  Change the file name.
      FNAME( IAPPN: ) = EXT

* @(#)sst_nwext.f   1.6   95/03/06 10:56:49   96/07/05 10:27:33
      END
