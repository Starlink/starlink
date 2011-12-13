      SUBROUTINE REF_ANNUL( LOC, STATUS )
*+
*  Name:
*     REF_ANNUL

*  Purpose:
*     Annul a locator to a referenced object.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL REF_ANNUL( LOC, STATUS )

*  Description:
*     This routine annuls the locator and, if the locator was linked to
*     group $$REFERENCED$, issues HDS_CLOSE for the container file of
*     the object.

*  Arguments:
*     LOC = CHARACTER * ( DAT__SZLOC ) (Given and Returned)
*        Locator to be annulled.
*     STATUS = INTEGER (Given and Returned)
*        Inherited global status.

*  Notes:
*     This routine attempts to execute even if STATUS is set on entry,
*     although no further error report will be made if it subsequently
*     fails under these circumstances. In particular, it will fail if
*     the locator supplied is not initially valid, but this will only
*     be reported if STATUS is set to SAI__OK on entry.

*  Copyright:
*     Copyright (C) 1989, 1990, 1992 Science & Engineering Research Council.
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
*     AJC: A.J. Chipperfield (STARLINK, RAL)
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     29-NOV-1989 (AJC):
*        Original version.
*     6-mAR-1990 (AJC):
*        Error reporting to SUN/104.
*     20-FEB-1992 (RFWS):
*        Standardised prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'		 ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'REF_PAR'		 ! REF_ public constants

*  Arguments Given:
      CHARACTER * ( * ) LOC

*  Status:
      INTEGER STATUS		 ! Global status

*  Local Variables:
      CHARACTER * ( 20 ) GROUP   ! Group of locator
      CHARACTER * ( DAT__SZLOC ) TLOC1 ! Locator to reference top level
      CHARACTER * ( DAT__SZLOC ) TLOC2 ! Clone to reference top level
      CHARACTER * ( REF__SZREF ) FILE ! Name of container file
      CHARACTER * ( REF__SZREF ) PATH ! Path name of object.
      INTEGER NLEV               ! Number of components in path

*.

*  Begin a new error reporting context.
      CALL EMS_BEGIN( STATUS )

*  Find if the locator is  REFERENCED.
      CALL HDS_GROUP( LOC, GROUP, STATUS )
      IF ( GROUP .EQ. '$$REFERENCED$' ) THEN

*  The locator was obtained via a reference. Find the container
*  filename.
         CALL HDS_TRACE( LOC, NLEV, PATH, FILE, STATUS )

*  Annul the locator.
         CALL DAT_ANNUL( LOC, STATUS )

*  Now obtain a locator to the container file.
         CALL HDS_OPEN( FILE, 'READ', TLOC1, STATUS )

*  Close it twice, once for this OPEN and once for the OPEN in REF_GET.
         CALL DAT_CLONE( TLOC1, TLOC2, STATUS )
         CALL HDS_CLOSE( TLOC2, STATUS )
         CALL HDS_CLOSE( TLOC1, STATUS )

*  If the locator was not obtained via a reference, annul it in the
*  ordinary way.
      ELSE
         CALL DAT_ANNUL( LOC, STATUS )
      ENDIF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL EMS_REP( 'REF_ANNUL_ERR',
     :   'REF_ANNUL: Error annulling a locator to a referenced object.',
     :   STATUS )
      END IF

*  End the error reporting context.
      CALL EMS_END( STATUS )

      END
