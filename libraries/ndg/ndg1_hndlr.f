      SUBROUTINE NDG1_HNDLR( EVNAME, EVTEXT, STATUS )
*+
*  Name:
*     NDG1_HNDLR

*  Purpose:
*     Handle an NDF event.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG1_HNDLR( EVNAME, EVTEXT, STATUS )

*  Description:
*     NDG_BEGPV registers this routine with the NDF library as an event
*     handler to be called whenever an NDF is opened or closed, or has
*     its data array mapped for read or update access.

*  Arguments:
*     EVNAME = CHARACTER * ( * ) (Given)
*        The type of NDF event that has occurred.
*     EVTEXT = CHARACTER * ( * ) (Given)
*        Descriptive information associated with the event. This will
*        usually be the path to the file that was opened, closed, etc.
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
*     1-NOV-2007 (DSB):
*        Original version.
*     2-JUN-2008 (DSB):
*        Use a pair of AST KeyMaps to hold the NDF names rather than a
*        pair of GRP groups (avoids the need to purge duplicate NDF
*        names, which can be very slow for large numbers of NDFs).
*     1-SEP-2008 (DSB):
*        Record each input NDF that is mapped for read or update access.
*     3-AUG-2009 (DSB):
*        Ensure a re-opened output NDF is not treated as an input NDF.
*     15-OCT-2009 (DSB):
*        Add handler for DEF_HISTORY event.
*     21-JUL-2011 (DSB):
*        The DHKMP_COM2 KeyMap is now used to associate a list of
*        parameter names with each NDF (previously, an arbitrary integer
*        value (0) was stored for each NDF). This list is held in a
*        nested KeyMap, and is used later to hold the names of all
*        parameters that have been written to the NDF's history component.
*        This is needed to avoid writing the same parameter more than
*        once to an NDF.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'DAT_PAR'          ! DAT constants

*  Global Variables:
      INCLUDE 'NDG_COM1'         ! Global provenance information
      INCLUDE 'NDG_COM2'         ! Global GRP history information

*  Arguments Given:
      CHARACTER EVNAME*(*)
      CHARACTER EVTEXT*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER PKM
      LOGICAL URDKMP             ! KeyMap unlocked on entry?
      LOGICAL UWRKMP             ! KeyMap unlocked on entry?
      LOGICAL UMPKMP             ! KeyMap unlocked on entry?
      LOGICAL UDHKMP             ! KeyMap unlocked on entry?

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Lock the mutex that serialises access to NDG globals
      CALL NDG1_GLOCK( .TRUE. )

*  Now lock the required global AST objects
      CALL NDG1_ALOCK( .TRUE., RDKMP_COM1, URDKMP, STATUS )
      CALL NDG1_ALOCK( .TRUE., WRKMP_COM1, UWRKMP, STATUS )
      CALL NDG1_ALOCK( .TRUE., MPKMP_COM1, UMPKMP, STATUS )
      CALL NDG1_ALOCK( .TRUE., DHKMP_COM2, UDHKMP, STATUS )

*  If the event was the opening of an input NDF( i.e. an existing NDF
*  opened for READ or UPDATE mode), add the path to the NDF to the RDKMP_COM1
*  KeyMap. Use the NDF name as the key, rather than the value so that any
*  subsequent invocations of this routine with the same NDF will overwrite
*  the earlier KeyMap entry, rather than creating an extra KeyMap entry.
*  Arbitrarily store zero as the value for the KeyMap entry.
*
*  If an application creates an output NDF, closes it, and then re-opens
*  it before the application terminates, we do not want the NDF to be
*  included in the list of input NDFs, so do not add the NDF into the
*  RDKMP_COM1 keymap if it is already in the WRKMP_COM1 keymap.
      IF( EVNAME .EQ. 'READ_EXISTING_NDF' .OR.
     :    EVNAME .EQ. 'UPDATE_EXISTING_NDF' ) THEN

         IF( .NOT. AST_MAPHASKEY( WRKMP_COM1, EVTEXT, STATUS ) ) THEN
            CALL AST_MAPPUT0I( RDKMP_COM1, EVTEXT, 0, ' ', STATUS )
         ENDIF

      END IF

*  If the event was the opening of an output NDF( i.e. an existing NDF
*  opened for UPDATE mode or a new NDF opened), add the path to the NDF
*  to the WRKMP_COM1 KeyMap.
      IF( EVNAME .EQ. 'UPDATE_EXISTING_NDF' .OR.
     :    EVNAME .EQ. 'OPEN_NEW_NDF' ) THEN
         CALL AST_MAPPUT0I( WRKMP_COM1, EVTEXT, 0, ' ', STATUS )
      END IF

*  If the event was the mapping of a data array for read or update access,
*  add the path to the NDF to the MPKMP_COM1 KeyMap.
      IF( EVNAME .EQ. 'UPDATE_DATA' .OR. EVNAME .EQ. 'READ_DATA' ) THEN
         CALL AST_MAPPUT0I( MPKMP_COM1, EVTEXT, 0, ' ', STATUS )
      END IF

*  If the event was the writing of default history, add the path to the NDF
*  to the DHKMP_COM2 KeyMap. The associated value is an empty KeyMap that
*  will be used later to record the parameter names written out to the NDF's
*  history component.
      IF( EVNAME .EQ. 'DEF_HISTORY' ) THEN
         PKM = AST_KEYMAP( ' ', STATUS )
         CALL AST_MAPPUT0A( DHKMP_COM2, EVTEXT, PKM, ' ', STATUS )
         CALL AST_ANNUL( PKM, STATUS )
      END IF

*  Now unlock the required global AST objects, but only if if they were
*  unlocked on entry.
      IF(URDKMP) CALL NDG1_ALOCK( .FALSE., RDKMP_COM1, URDKMP, STATUS )
      IF(UWRKMP) CALL NDG1_ALOCK( .FALSE., WRKMP_COM1, UWRKMP, STATUS )
      IF(UMPKMP) CALL NDG1_ALOCK( .FALSE., MPKMP_COM1, UMPKMP, STATUS )
      IF(UDHKMP) CALL NDG1_ALOCK( .FALSE., DHKMP_COM2, UDHKMP, STATUS )

*  Unlock the mutex that serialises access to NDG globals
      CALL NDG1_GLOCK( .FALSE. )

      END
