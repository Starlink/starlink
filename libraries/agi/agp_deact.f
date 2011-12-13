************************************************************************

      SUBROUTINE AGP_DEACT ( STATUS )

*+
*  Name:
*     AGP_DEACT
*
*  Purpose:
*     Close down PGPLOT
*
*  Invocation:
*     CALL AGP_DEACT( STATUS )
*
*  Description:
*     Close down PGPLOT whatever the value of status. This should be
*     called after all AGP and PGPLOT routines.
*
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Close down PGPLOT.
*     Clear the PGPLOT active flag.
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
*        Reset graphics package flag
*     Apr 1991 (NE):
*        Return error if interface is not active
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'AGI_ERR'

*  Global variables :
      INCLUDE 'agi_idips'

*  Status :
      INTEGER STATUS
*.

*   Signal an error if the interface is not active
      IF ( .NOT. CPGPON ) THEN
         STATUS = AGI__GRPNA
         CALL ERR_REP( 'AGP_DEACT_GRPNA', 'Graphics package not active',
     :                 STATUS )
         GOTO 99
      ENDIF

*   Without checking status close down PGPLOT
      CALL PGEND

*   Reset the graphics package flag
      CPGPON = .FALSE.

  99  CONTINUE

      END

