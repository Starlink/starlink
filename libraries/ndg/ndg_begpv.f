      SUBROUTINE NDG_BEGPV( STATUS )
*+
*  Name:
*     NDG_BEGPV

*  Purpose:
*     Begin an NDF provenance block.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_BEGPV( STATUS )

*  Description:
*     This routine should be called to mark the start of an NDF
*     provenance block. The block should be ended by a matching
*     call to NDG_ENDPV. See NDG_ENDPV for more details.
*
*     Note - provenance blocks must not be nested.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2007-2008 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-OCT-2007 (DSB):
*        Original version.
*     2-JUN-2008 (DSB):
*        Use a pair of AST KeyMaps to hold the NDF names rather than a
*        pair of GRP groups (avoids the need to purgew duplicate NDF
*        names, which can be very slow for large numbers of NDFs).
*     1-SEP-2008 (DSB):
*        Create a KeyMap to hold the names of the mapped input NDFs.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions

*  Global Variables:
      INCLUDE 'NDG_COM1'         ! Global provenance information

*  External References:
      EXTERNAL NDG1_HNDLR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL OLD
      LOGICAL URDKMP
      LOGICAL UWRKMP
      LOGICAL UMPKMP
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get sole access to the NDG globals
      CALL NDG1_GLOCK( .TRUE. )

*  Indicate that NDF event handlers needed to record the NDFs in which
*  provenance should be stored have not yet been established, and then
*  establish them.
      STATE_COM1 = .FALSE.
      CALL NDG_HLTPV( .TRUE., OLD, STATUS )

*  Indicate that the PROVENANCE extension should not be propagated by
*  default when NDF_PROP or NDF_SCOPY is called.
      CALL NDF_TUNE( 0, 'PXTPROVENANCE', STATUS )

*  Create a AST KeyMap to hold the paths to the NDFs that are read during
*  the provenance block. Each KeyMap entry has a key that is an NDF name
*  (the entry value is of no significance and will be set arbitrarily to
*  zero).
      RDKMP_COM1 = AST_KEYMAP( ' ', STATUS )

*  Create a AST KeyMap to hold the paths to the NDFs that are written during
*  the provenance block.
      WRKMP_COM1 = AST_KEYMAP( ' ', STATUS )

*  Create a AST KeyMap to hold the paths to the NDFs that have their Data
*  array mapped for read or update access.
      MPKMP_COM1 = AST_KEYMAP( ' ', STATUS )

*  Unlock them so they can be locked for use by another thread.
      CALL NDG1_ALOCK( .FALSE., RDKMP_COM1, URDKMP, STATUS )
      CALL NDG1_ALOCK( .FALSE., WRKMP_COM1, UWRKMP, STATUS )
      CALL NDG1_ALOCK( .FALSE., MPKMP_COM1, UMPKMP, STATUS )

*  Allow other threads to access the NDG globals
      CALL NDG1_GLOCK( .FALSE. )

      END
