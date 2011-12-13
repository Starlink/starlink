      SUBROUTINE CCD1_KPSV( PARAM, KEY, SLOC, STATUS )
*+
*  Name:
*     CCD1_KPSV

*  Purpose:
*     Save the keyed value of a parameter from the parameter file.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_KPSV( PARAM, KEY, SLOC, STATUS )

*  Description:
*     This routine saves the version of a parameter keyed by an integer
*     value into the ADAM GLOBAL parameter file.  It should be called
*     after a parameter is accessed, if the parameter is key-sensitive
*     and has WRITE global association.  If the parameter has no value
*     when this routine is called, nothing is saved.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Name of the parameter in question.
*     KEY = INTEGER (Given)
*        The key to be used.  The value of the parameter will be saved
*        keyed by this value in the ADAM parameter file.
*     SLOC = CHARACTER * ( * ) (Given)
*        A locator to the keyed parameters structure.  Should be opened
*        for UPDATE access.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The keyed values are stored in the same ADAM parameter file
*     as the variables which PAR looks for them in, within a
*     structure called CCDPACK_KEYPARS.
*
*     Meticulous explicit error processing is not undertaken in this
*     routine.  If the GLOBAL parameter file differs badly from the
*     expected structure, the routine will exit with the error status
*     corresponding to the HDS call which failed.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-MAY-2001 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard HDS constants

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      INTEGER KEY
      CHARACTER * ( DAT__SZLOC ) SLOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) ALOC ! HDS locator for keyed parameter
      CHARACTER * ( DAT__SZNAM ) KEYNAM ! Name of key component
      CHARACTER * ( DAT__SZLOC ) VLOC ! HDS locator for matched value
      INTEGER LENG               ! Length of string
      LOGICAL THERE              ! Does HDS component exist?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Try to get a locator for the current value of the parameter we are
*  being asked to save.
      CALL DAT_EXIST( PARAM, 'READ', VLOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL DAT_ANNUL( VLOC, STATUS )
         CALL ERR_ANNUL( STATUS )
      ELSE

*  Get a locator for the component of the keyed parameters structure
*  representing the parameter we are interested in; if none exists,
*  create one.
         CALL DAT_THERE( SLOC, PARAM, THERE, STATUS )
         IF ( .NOT. THERE ) THEN
            CALL DAT_NEW( SLOC, PARAM, 'KEYED_PARAMETER', 0, 0, STATUS )
         END IF
         CALL DAT_FIND( SLOC, PARAM, ALOC, STATUS )

*  Generate the name of the component of this structure corresponding
*  to the key we are going to save.
         CALL MSG_SETI( 'KEY', KEY )
         CALL MSG_LOAD( ' ', 'KEY_^KEY', KEYNAM, LENG, STATUS )

*  If a component already exists for this key, erase it.
         CALL DAT_THERE( ALOC, KEYNAM, THERE, STATUS )
         IF ( THERE ) THEN
            CALL DAT_ERASE( ALOC, KEYNAM, STATUS )
         END IF

*  Now write a new component with the key corresponding to this key
*  and the value from the original variable.
         CALL DAT_COPY( VLOC, ALOC, KEYNAM, STATUS )

*  Release locators.
         CALL DAT_ANNUL( ALOC, STATUS )
         CALL DAT_ANNUL( VLOC, STATUS )
      END IF

      END
* $Id$
