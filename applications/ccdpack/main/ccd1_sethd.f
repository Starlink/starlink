      SUBROUTINE CCD1_SETHD( KEYGRP, ISUB, PRETXT, KEYTYP, STATUS )
*+
*  Name:
*     CCD1_SETHD

*  Purpose:
*     Writes a log message about Set subgroup processing.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_SETHD( KEYGRP, ISUB, PRETXT, KEYTYP, STATUS )

*  Description:
*     This routine writes a short heading through the CCDPACK logging
*     system to indicate that the following messages relate to
*     processing a particular Set-based subgroup.  The message will
*     look something like:
*
*        ^PRETXT for Set ^KEYTYP ^KEYVAL:
*        --------------------------------
*
*     where KEYVAL is the ISUB'th index of KEYGRP.

*  Arguments:
*     KEYGRP = INTEGER (Given)
*        A GRP identifier for the group containing the names of keys
*        identifying the Set subgroup.
*     ISUB = INTEGER (Given)
*        The index of the member of KEYGRP whose subgroup is about to
*        be processed.
*     PRETXT = CHARACTER * ( * ) (Given)
*        A string to start the message.
*     KEYTYP = CHARACTER * ( * ) (Given)
*        A string indicating the type of key represented by KEYGRP.
*        This would normally be 'Index' or 'Name'.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-FEB-2001 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! Standard GRP constants
      INCLUDE 'CCD1_PAR'         ! Private CCDPACK constants

*  Arguments Given:
      INTEGER KEYGRP
      INTEGER ISUB
      CHARACTER * ( * ) KEYTYP
      CHARACTER * ( * ) PRETXT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( CCD1__BLEN ) BUFFER ! Line output buffer
      CHARACTER * ( GRP__SZNAM ) KEYVAL ! Value of key name
      INTEGER BUFLEN             ! Used length of buffer

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the value of the key.
      CALL GRP_GET( KEYGRP, ISUB, 1, KEYVAL, STATUS )

*  Construct the message.
      CALL MSG_SETC( 'PRETEXT', PRETXT )
      CALL MSG_SETC( 'KEYTYPE', KEYTYP )
      CALL MSG_SETC( 'KEYVAL', KEYVAL )
      CALL MSG_LOAD( ' ', '  ^PRETEXT for Set ^KEYTYPE ^KEYVAL:',
     :               BUFFER, BUFLEN, STATUS )

*  Output the message.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', BUFFER, STATUS )

*  Underline the message.
      CALL CHR_FILL( '-', BUFFER( 3:BUFLEN ) )
      CALL CCD1_MSG( ' ', BUFFER, STATUS )

      END
* $Id$
