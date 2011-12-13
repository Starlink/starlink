************************************************************************

      SUBROUTINE AGS_ACTIV ( STATUS )

*+
*  Name:
*     AGS_ACTIV
*
*  Purpose:
*     Initialise SGS
*
*  Invocation:
*     CALL AGS_ACTIV( STATUS )
*
*  Description:
*     Initialise SGS. This has to be called before any other AGS or
*     SGS routines. An error is returned if this or any other graphics
*     interface is already active.
*
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     Check that no other graphics packages are active.
*     Initialise SGS.
*
*  Copyright:
*     Copyright (C) 1988, 1990, 1991 Science & Engineering Research Council.
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
*     Aug 1988 (NE):
*        Original version
*     Jun 1990 (NE):
*        See if other packages are open
*     Apr 1991 (NE):
*        See if this package is already open
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_ERR'

*  Global variables :
      INCLUDE 'agi_idips'

*  Status :
      INTEGER STATUS

*  Local Constants :
      INTEGER LUEGKS
      PARAMETER ( LUEGKS = 6 )
*.

*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Check if any other packages are open
         IF ( CGKSON .OR. CPGPON .OR. CIDION ) THEN
            STATUS = AGI__GRPON
            CALL ERR_REP( 'AGS_ACTIV_GRPON', 'Graphics package in use',
     :                    STATUS )
            GOTO 99
         ELSE
            CGKSON = .TRUE.
         ENDIF

*   Initialise SGS
         CALL SGS_INIT( LUEGKS, STATUS )

      ENDIF

  99  CONTINUE

      END

