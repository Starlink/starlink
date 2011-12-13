************************************************************************

      SUBROUTINE AGS_1GKSNM ( WKID, CONID, GNAME, STRLEN, STATUS )

*+
*  Name:
*     AGS_1GKSNM

*  Purpose:
*     Make up a GKS name string from the wkid and conid.

*  Language:
*     VAX Fortran

*  Type of Module:
*     Subroutine

*  Invocation:
*     CALL AGS_1GKSNM( WKID, CONID, GNAME, STRLEN, STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine makes up a GKS name from the workstation id and
*     the connection id. The name is made up as 'GKS_<wkid>_<conid>'.
*     The resulting name can be used to open SGS on the particular
*     device.

*  Algorithm:
*     Convert the workstation and connection identifiers to character
*     strings.
*     Append these onto the prefix and calculate the string length.

*  Copyright:
*     Copyright (C) 1988, 1989 Science & Engineering Research Council.
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
*     Nick Eaton  ( DUVAD::NE )
*     {enter_new_authors_here}

*  History:
*     Aug 1988
*     Nov 1989  Changed CHR_LDBLNK to CHR_LDBLK
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'


*  Arguments Given:
*     GKS workstation identifier
      INTEGER WKID

*     GKS connection identifier
      INTEGER CONID


*  Arguments Returned:
*     GKS name for the device
      CHARACTER * ( * ) GNAME

*     Length of name string
      INTEGER STRLEN


*  Status:
      INTEGER STATUS


*  Local Variables:
      INTEGER CHR_LEN, CI2, WI2
      CHARACTER SCONID * 20, SWKID * 20

*.


      IF ( STATUS .EQ. SAI__OK ) THEN

*   Write the WKID and CONID into character strings
         WRITE( SWKID,  '( I10 )' ) WKID
         WRITE( SCONID, '( I10 )' ) CONID

*   Find the start and finish of these strings
         CALL CHR_LDBLK( SWKID )
         WI2 = CHR_LEN( SWKID )
         CALL CHR_LDBLK( SCONID )
         CI2 = CHR_LEN( SCONID )

*   Construct a name as GKS_<wkid>_<conid>
         GNAME = 'GKS_' // SWKID( :WI2 ) //
     :           '_' // SCONID( :CI2 )
         STRLEN = 5 + WI2 + CI2

      ENDIF

      END

