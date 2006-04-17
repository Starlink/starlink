      SUBROUTINE MSG1_FORM( MSG, TEXT, CLEAN, MSGSTR, MSGLEN, STATUS )
*+
*  Name:
*     MSG1_FORM

*  Purpose:
*     Form a message from its text and components.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MSG1_FORM( MSG, TEXT, CLEAN, MSGSTR, MSGLEN, STATUS )

*  Description:
*     Construct the final text, MSGSTR( 1 : MSGLEN ), of a message
*     using the text in TEXT, and available message tokens.

*  Arguments:
*     MSG = CHARACTER * ( * ) (Given)
*        The message parameter name.
*     TEXT = CHARACTER * ( * ) (Given)
*        The input message text, with any tokens.
*     CLEAN = LOGICAL (Given)
*        If the string is to be 'cleaned'
*     MSGSTR = CHARACTER * ( * ) (Returned)
*        Resultant message text, with parsed tokens.
*     MSGLEN = INTEGER (Returned)
*        The filled length of MSGSTR.
*     STATUS = INTEGER (Given)
*        The global status.

*  Implementation Notes:
*     -  This subroutine is the STANDALONE version of MSG1_FORM.

*  Algorithm:
*     -  Call EMS_EXPND.

*  Copyright:
*     Copyright (C) 1982, 1984, 1985, 1989 Science & Engineering Research Council.
*     Copyright (C) 1999, 2001 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     JRG: Jack Giddings (UCL)
*     BDK: Dennis Kelly (ROE)
*     AJC: Alan Chipperfield (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1982 (JRG):
*        Original version.
*     13-NOV-1984 (BDK):
*        ADAM version.
*     11-JUN-1985 (BDK):
*        Discard leading % from error message.
*     5-JUN-1989 (AJC):
*        Check whole string for equality with escape.
*     13-SEP_1989 (PCTR):
*        Converted to new prologue and layout.
*     15-SEP-1999 (AJC):
*        Add CLEAN argument.
*     26-FEB-2001 (AJC):
*        Use EMS_EXPND not EMS1_FORM
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER * ( * ) MSG
      CHARACTER * ( * ) TEXT
      LOGICAL CLEAN

*  Arguments Returned:
      CHARACTER * ( * ) MSGSTR
      INTEGER MSGLEN

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER LSTAT     ! Local STATUS
*.

*  Set local status OK so EMS_EXPND works
      LSTAT = SAI__OK

*  Call EMS_EXPND.
      CALL EMS_EXPND( TEXT, MSGSTR, MSGLEN, LSTAT )

      END
