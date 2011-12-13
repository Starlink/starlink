      SUBROUTINE GRP_LISTF( FILENM, INDXLO, INDXHI, COMNT, IGRP,
     :                      STATUS )
*+
*  Name:
*     GRP_LISTF

*  Purpose:
*     Write names to a specified text file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_LISTF( FILENM, INDXLO, INDXHI, COMNT, IGRP, STATUS )

*  Description:
*     A text file is created with the specified name.  The supplied
*     comment is written to the file as the first record (so long as it
*     is not blank), using the groups current comment character (see
*     routine GRP_SETCC). All the names stored within the specified
*     group section are then written to the file, one name per record.
*     If the group is case insensitive (as set up by a call to routine
*     GRP_SETCS) then the names are written out in upper case,
*     otherwise they are written out as supplied.
*
*     The routine GRP_LIST can be used if the file name is to be
*     obtained through the parameter system.

*  Arguments:
*     FILENM = CHARACTER * ( * ) (Given)
*        The name of the text file to be created.
*     INDXLO = INTEGER (Given)
*        The low index limit of the group section. If both INDXLO and
*        INDXHI are zero, then the entire group is used.
*     INDXHI = INTEGER (Given)
*        The high index limit of the group section.
*     COMNT = CHARACTER * ( * ) (Given)
*        A comment line to form the first record in the file. The text
*        is prefixed with a the groups current comment character before
*        being written to the file.
*     IGRP = INTEGER (Given)
*        The GRP identifier for the group to be listed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1992 (DSB):
*        Original version
*     1-OCT-1992 (DSB):
*        Omit trailing spaces from the file name passed to the Fortran
*        OPEN statement.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_ERR'          ! GRP error values.

*  Arguments Given:
      CHARACTER FILENM*(*)
      INTEGER INDXLO
      INTEGER INDXHI
      CHARACTER COMNT*(*)
      INTEGER IGRP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Returns used length of a string.

*  Local Variables:
      INTEGER IOS                ! Fortran i/o status value.
      INTEGER UNIT               ! IO unit on which the file is opened.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a free Fortran logical unit number.
      CALL GRP1_LUNIT( UNIT, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to open the file.
      OPEN( UNIT, FILE = FILENM( : CHR_LEN( FILENM ) ), STATUS = 'NEW',
     :      IOSTAT=IOS )

*  If an IO error was detected, report an error.
      IF( IOS. NE. 0 ) THEN

         STATUS = GRP__FIOER
         CALL MSG_SETI( 'UNIT', UNIT )
         CALL MSG_SETC( 'F', FILENM )
         CALL ERR_FIOER( 'TEXT', IOS )
         CALL ERR_REP( 'GRP_LISTF_ERR1',
     :'GRP_LISTF: Error opening file ^F on Fortran unit ^UNIT - '//
     :'"^TEXT"', STATUS )

         GO TO 999

      END IF

*  Call GRP1_LISTU to write the names and comment out to the opened
*  unit.
      CALL GRP1_LISTU( UNIT, INDXLO, INDXHI, COMNT, IGRP, STATUS )

*  Close the file.
      CLOSE( UNIT )

*  If an error occurred, give a context message.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GRP_LISTF_ERR2',
     :                 'GRP_LISTF: Unable to produce a list file '//
     :                 'containing the names in a group', STATUS )
      END IF

      END
