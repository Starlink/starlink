      SUBROUTINE PARSECON_ERFL( STATUS )













*+
*  Name:
*     {routine_name}

*  Purpose:
*     {routine_purpose}

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_ERFL( STATUS )

*  Description:
*     To flush out any error messages for PARSECON

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The global status.

*  Implementation Deficiencies:
*     1.The format has to be changed to just ( A ) for the Unix version
*     2.One could consider also writing to the error output channel.

*  Name:
*     PARSECON_EROUT

*  Language:
*     Starlink Fortran 77

*  External Routines Used:
*     EMS_ELOAD
*     PARSECON_WRITE

*  Copyright:
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.

*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.

*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}
*     {enter_new_authors_here}

*  History:
*     13-JUN-1991 (AJC):
*          Original version.
*        1-OCT-1991 (AJC)
*          Prefix messages with "!! " etc.
*     21-JAN-1992 (AJC):
*          Remove ref to ISTAT
*        6-MAR-1992 (AJC):
*          Use WRITE with FORMAT
*     {enter_changes_here}
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*     {note_new_bugs_here}

*-

*.


*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'EMS_PAR'          ! EMS constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER*(EMS__SZPAR) PARSTR ! The message name
      CHARACTER*(EMS__SZMSG) MSSTR ! The message string
      CHARACTER*3 MSPRE          ! Message prefix
      INTEGER PARLEN             ! The message name length
      INTEGER MSLEN              ! The message length
*.

*   Set STATUS to the last reported value (ie if any reported).
      CALL EMS_STAT( STATUS )

*   If there are any messages pending, unpack and output them

*   Set the initial prefix
      MSPRE = '!! '

      DOWHILE ( STATUS .NE. SAI__OK )
         CALL EMS_ELOAD( PARSTR, PARLEN, MSSTR, MSLEN, STATUS )
         WRITE(*,10) MSPRE//MSSTR(1:MSLEN)
10       FORMAT ( A )
*     Set the subsequent prefix
         MSPRE = '!  '
      ENDDO

      END
