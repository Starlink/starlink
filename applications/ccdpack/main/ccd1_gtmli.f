      SUBROUTINE CCD1_GTMLI( PARNAM, MGRP, MINOPN, MAXOPN, NOPEN, FDS,
     :                       GID, STATUS )
*+
*  Name:
*     CCD1_GTMLI

*  Purpose:
*     Opens a list of formatted files via the ADAM parameter PARNAM.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_GTLIS( PARNAM, MGRP, MINOPN, MAXOPN, NOPEN, FDS, GID,
*                      STATUS )

*  Description:
*     This routine opens a series of formatted files. The names of the
*     files are accessed as a list through the GRP system (which allows
*     indirection of the names, and wildcards based on the input
*     modification group MGRP), using the parameter PARNAM. The files
*     are opened one by one using FIO_OPEN. File names which fail to
*     open a file are reported, status is set and an exit occurs.

*  Arguments:
*     PARNAM = CHARACTER * ( * ) (Given)
*        The ADAM parameter name via which the list of file names is to
*        be accessed.
*     MGRP = INTEGER (Given)
*        The GRP group identifier of a list of names which will be used
*        as a modification group for the file names. If this is set to
*        GRP__NOID on entry then no modification group is used.
*     MINOPN = INTEGER (Given)
*        The minimum number of files which need to be opened.
*     MAXOPN = INTEGER (Given)
*        The maximum number of files which can be opened. This should be
*        not less than the dimensions of the FDS and ROPEN arrays.
*     NOPEN = INTEGER (Given)
*        The number of files which were opened.
*     FDS( MAXOPN ) = INTEGER (Returned)
*        FIO file system descriptors to the opened files.
*     GID = INTEGER (Returned)
*        The GRP identifier of the group of filenames.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The calling routine must close all the files which are opened
*     before exit.
*     -  The calling routine must annul the GRP group before exit.

*  Copyright:
*     Copyright (C) 1992-1993 Science & Engineering Research Council.
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-JUL-1992 (PDRAPER):
*        Original version.
*     28-JUL-1992 (PDRAPER):
*        Added minimum number of input files option.
*     25-JAN-1993 (PDRAPER):
*        Changed to use a modification group.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! Standard GRP constants

*  Arguments Given:
      CHARACTER * ( * ) PARNAM
      INTEGER MGRP
      INTEGER MINOPN
      INTEGER MAXOPN

*  Arguments Returned:
      INTEGER NOPEN
      INTEGER FDS( MAXOPN )
      INTEGER GID

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( GRP__SZNAM ) FNAME ! Filename
      INTEGER NRET
      INTEGER I
      INTEGER FD
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the list of names from the user.
      CALL CCD1_STRGR( PARNAM, MGRP, MINOPN, MAXOPN, GID, NRET, STATUS )

*  Report the number of file names returned.
      IF ( NRET .EQ. 1 ) THEN
         CALL MSG_SETI( 'NOPEN', NRET )
         CALL MSG_SETC( 'PARNAM', PARNAM )
         CALL MSG_OUT( ' ',
     :'  ^NOPEN name accessed using parameter %^PARNAM', STATUS )
      ELSE
         CALL MSG_SETI( 'NOPEN', NRET )
         CALL MSG_SETC( 'PARNAM', PARNAM )
         CALL MSG_OUT( ' ',
     :'  ^NOPEN names accessed using parameter %^PARNAM', STATUS )
      END IF

*  If all's well then proceed to open all the files.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Extract a name from the group.
         NOPEN = 0
         DO 1 I = 1, NRET
            CALL GRP_GET( GID, I, 1, FNAME, STATUS )

*  Try to open the file.
            CALL FIO_OPEN( FNAME, 'READ', 'LIST', 0, FD, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Everything ok - file exists and is open.
               NOPEN = NOPEN + 1
               FDS( NOPEN ) = FD
            ELSE

*  Failed to open the file. Issue a message to the user.
               CALL MSG_SETC( 'FNAME', FNAME )
               CALL ERR_REP( 'CCD1_GTLIS_FERR',
     :         '  Failed to open file ^FNAME', STATUS )
               GO TO 99
            END IF
 1       CONTINUE
      END IF
 99   CONTINUE
      END
* $Id$
