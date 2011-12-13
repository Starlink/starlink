************************************************************************

      SUBROUTINE AGI_BEGIN

*+
*  Name:
*     AGI_BEGIN
*
*  Purpose:
*     Mark the beginning of a new AGI scope.
*
*  Invocation:
*     CALL AGI_BEGIN
*
*  Description:
*     Mark the beginning of a new AGI scope. This should be matched with
*     a call to AGI_END. Up to eight levels of nested begin-end blocks
*     are allowed.
*
*  Algorithm:
*     Initialise the database close flag
*     Store the details of the current picture at the current nest level
*     Increment the nest counter
*
*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     NE: Nick Eaton (Durham University)
*
*  History:
*     Nov 1990 (NE):
*        Original version
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'

*  Global variables :
      INCLUDE 'agi_nest'
      INCLUDE 'agi_pfree'

*  External references :
      EXTERNAL AGI_BLOCK
*.

*   Initialise the database close flag
      IF ( CNEST .EQ. 1 ) THEN
         CLOSEF = .FALSE.
      ENDIF

*   Store the details of the current picture at the current nest level
      IF ( CURPID .GT. 0 ) THEN
         CNESWK( CNEST ) = CAGIWK( CURPID )
         CNESPN( CNEST ) = CPICNM( CURPID )
      ELSE
         CNESWK( CNEST ) = ' '
         CNESPN( CNEST ) = -1
      ENDIF

*   Increment the nest counter
      IF ( CNEST .LT. NESLEN ) THEN
         CNEST = CNEST + 1
      ENDIF

      END

