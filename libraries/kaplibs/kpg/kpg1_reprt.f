      SUBROUTINE KPG1_REPRT( MESS, QUIET, LOG, FD, STATUS )
*+
*  Name:
*     KPG1_REPRT

*  Purpose:
*     Reports a MSG message to user and also optionally write it to a file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_REPRT( MESS, QUIET, LOG, FD, STATUS )

*  Description:
*     This routine dislays the supplied message using MSG_OUT (unless
*     QUIET is .TRUE), and (if LOG is .TRUE.) writes out the same text
*     to the file identified by FD.

*  Arguments:
*     MESS = CHARACTER * ( * ) (Given)
*        The message, which may contain MSG tokens.
*     QUIET = LOGICAL (Given)
*        Supress screen output?
*     LOG = LOGICAL (Given)
*        Write the text to  a file?
*     FD = INTEGER (Given)
*        An FIO identifier for a file. If LOG is .TRUE., then the message
*        is written to this file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-AUG-2001 (DSB):
*        Original version.
*     21-OCT-2009 (DSB):
*        If there are no msg tokens in the text, avoid truncation by
*        internal MERS buffer length when writing text to log file.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER MESS*(*)
      LOGICAL QUIET
      LOGICAL LOG
      INTEGER FD

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER OPSTR*255        ! Buffer for message text
      INTEGER OPLEN              ! Length of message text

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Show the message to the user.
      IF( .NOT. QUIET ) CALL MSG_OUT( ' ', MESS, STATUS )

*  If writing to the file...
      IF( LOG ) THEN

*  If there are no msg tokens in the text, just write it out directly.
*  This avoids limitation imposed by the fixed length nature of some MERS
*  internal buffers.
         IF( INDEX( MESS, '^' ) .EQ. 0 ) THEN
            CALL FIO_WRITE( FD, MESS( : CHR_LEN( MESS ) ), STATUS )

*  Otherwise, get the expanded text of the message, and write it to the
*  file.
         ELSE
            CALL MSG_LOAD( ' ', MESS, OPSTR, OPLEN, STATUS )
            CALL FIO_WRITE( FD, OPSTR( : OPLEN ), STATUS )
         END IF

      END IF

      END
