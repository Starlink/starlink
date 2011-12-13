      SUBROUTINE CCD1_OPFIO( FNAME, ACMODE, FORM, RECSZ, FD, STATUS )
*+
*  Name:
*     CCD1_OPFIO

*  Purpose:
*     Opens a sequential file - given its name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_OPFIO( FNAME, ACMODE, FORM, RECSZ, FD, STATUS )

*  Description:
*     This routine opens a sequential file of name FNAME.  If the file
*     exists and is opened for write access and the open fails then an
*     attempt to a previous file is made. If this is successful another
*     attempt to open the file for write is made, this facility is
*     intended for UNIX based system with no file versions. If this
*     fails then a error is reported etc. The file opened should be
*     closed using FIO_CLOSE.

*  Arguments:
*     FNAME = CHARACTER *( * ) (Given)
*        Name of the file to be opened
*     ACMODE = CHARACTER *( * ) (Given)
*        Expression giving the required access mode.
*        Valid modes are: 'READ', 'WRITE', 'UPDATE' and 'APPEND'.
*        For details, see FIO_OPEN.
*     FORM = CHARACTER *( * )(Given)
*         Expression giving the required formatting of the file.
*         Valid formats are: 'FORTRAN', 'LIST', 'NONE' and
*         'UNFORMATTED'. For details, see FIO_OPEN.
*     RECSZ = INTEGER (Given)
*         Expression giving the maximum record size in bytes.
*         Set it to zero if the Fortran default is required.
*     FD = INTEGER (Returned)
*         Variable to contain the file descriptor.
*     STATUS = INTEGER (Given and Returned)
*         Global status value

*  Copyright:
*     Copyright (C) 1989, 1992-1993 Science & Engineering Research
*     Council. All Rights Reserved.

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
*     MJC: Malcolm Currie (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-MAY-1989 (MJC):
*        Original version.
*     11-MAY-1992 (PDRAPER):
*        Extracted from AIF and renamed for PISA use. Updated
*        prologue.
*     18-MAR-1993 (PDRAPER):
*        Added checks for file opened for write access which fails
*        (on UNIX system).
*     12-MAY-1993 (PDRAPER):
*        Made into a CCDPACK subroutine. Removed parameter access mode.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Parameter system constants

*  Arguments Given:
      CHARACTER * ( * ) FNAME
      CHARACTER * ( * ) ACMODE
      CHARACTER * ( * ) FORM
      INTEGER RECSZ

*  Arguments Returned:
      INTEGER FD

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL FIO_TEST
      LOGICAL FIO_TEST           ! Tests FIO generic errors

*  Local Variables:


*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open file with the given name and characteristics.
      CALL FIO_OPEN( FNAME, ACMODE, FORM, RECSZ, FD, STATUS )

*  Check status for PAR_ABORT or PAR_NULL.
      IF ( STATUS .EQ. PAR__ABORT .OR. STATUS .EQ. PAR__NULL ) THEN

*  If PAR__NULL then annul status and any error messages.
         IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  Other non-OK status, must have problems opening the file.
      ELSE IF ( STATUS .NE. SAI__OK ) THEN

*  If the file is opened for write access and FIO has issued an OPEN
*  error then try to delete the current file (assumes a UNIX system)
*  and if this succeeds try to open the file again).
         IF ( ACMODE .EQ. 'WRITE' .AND.
     :        FIO_TEST( 'OPEN error', STATUS ) ) THEN

*  Annul the error.
            CALL ERR_ANNUL( STATUS )

*  Delete the file!
            CALL FIO_ERASE( FNAME, STATUS )

*  And try to open it again.
            CALL FIO_OPEN( FNAME, ACMODE, FORM, RECSZ, FD, STATUS )
         END IF
      END IF

*  If status set on exit then affirm action.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'FNAME', FNAME )
         CALL ERR_REP( 'CCD1_OPFIO2', '  Unable to open file ^FNAME',
     :   STATUS )
      END IF

      END
* $Id$
