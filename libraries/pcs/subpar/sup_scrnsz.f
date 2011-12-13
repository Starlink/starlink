      SUBROUTINE SUBPAR_SCRNSZ (SCREEN_WIDTH, SCREEN_HEIGHT, STATUS)
*+
*  Name:
*     SUBPAR_SCRNSZ

*  Purpose:
*     Determine terminal width and height.

*  Description:
*     This is the Unix version
*     It interrogates the system to find the width and height
*     of the screen on which it is running.

*  Invocation:
*     CALL SUBPAR_SCRNSZ (SCREEN_WIDTH, SCREEN_HEIGHT, STATUS)

*  Arguments:
*     SCREEN_WIDTH   =INTEGER       (returned)
*            width of the screen - defaults to 80 characters
*     SCREEN_HEIGHT  =INTEGER       (returned)
*            height of the screen - defaults to 0 lines,
*            ie no paging (eg if running in batch)

*  Algorithm:
*     Call the C function SUP_TRMSZ.
*     If this is unsuccessful, set the default which will result in no
*     paging.

*  Bugs:
*     <description of any "bugs" which have not been fixed>

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
*     AJC: A. J. Chipperfield (Starlink)

*  History:
*     17-JUN-1992 (AJC):
*        Original

*  Notes:
*     See also ONE_SCRNSZ

*-

*    Type Definitions :
      IMPLICIT NONE

*    Global constants
      INCLUDE 'SAE_PAR'             ! SAE constants

*    Export :
      INTEGER SCREEN_WIDTH          ! Screen width (characters)
      INTEGER SCREEN_HEIGHT         ! Screen height (lines)

*    Status :
      INTEGER STATUS                ! Global status

*    External References :
      INTEGER SUBPAR_TRMSZ
      EXTERNAL SUBPAR_TRMSZ

*    Local variables :
      INTEGER ISTAT                 ! Local status

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Inquire the terminal size
      ISTAT = SUBPAR_TRMSZ ( SCREEN_WIDTH, SCREEN_HEIGHT )

*   If this failed to get a good value, set default which causes no paging.
      IF ( ( ISTAT .NE. 1 ) .OR. ( SCREEN_WIDTH .LE. 0 ) ) THEN
         SCREEN_WIDTH = 80
         SCREEN_HEIGHT = 0
      ENDIF

      END

