************************************************************************

      SUBROUTINE AGI_SROOT ( STATUS )

*+
*  Name:
*     AGI_SROOT
*
*  Purpose:
*     Select the root picture for searching
*
*  Invocation:
*     CALL AGI_SROOT( STATUS )
*
*  Description:
*     The root picture is selected for searching operations. The root
*     picture contains all other pictures (including the base picture)
*     and can be used to recall any picture whether it lies within the
*     current picture or not. This is used to override the usual
*     restriction that a recalled picture must lie within the bounds of
*     the current picture. The root picture is automatically deselected
*     after a call to any of the recall routines AGI_RC*. Recall is the
*     only operation allowed with the root picture, any other operation
*     called while the root picture is selected will use the current
*     picture.
*
*  Arguments:
*    STATUS = INTEGER (Given and Returned)
*       The global status
*
*  Algorithm:
*     A flag in the common block is set.
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
*     July 1990 (NE):
*        Original version
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'

*  Global variables :
      INCLUDE 'agi_pfree'

*  Status :
      INTEGER STATUS
*.

*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Set the root flag
         CROOT = 1

      ENDIF

      END

