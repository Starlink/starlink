      SUBROUTINE REF_FIND( ELOC, CNAME, MODE, LOC, STATUS )
*+
*  Name:
*     REF_FIND

*  Purpose:
*     Get locator to data object (via reference if necessary).

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL REF_FIND( ELOC, CNAME, MODE, LOC, STATUS )

*  Description:
*     This routine gets a locator to a component of a specified
*     structure or, if the component is a reference object, it gets a
*     locator to the object referenced. Any locator obtained in this
*     way should be annulled, when finished with, by REF_ANNUL so that
*     the top-level object will also be closed if the locator was
*     obtained via a reference.

*  Arguments:
*     ELOC = CHARACTER * ( * ) (Given)
*        The locator of a structure.
*     CNAME = CHARACTER * ( * ) (Given)
*        The name of the component of the specified structure.
*     MODE = CHARACTER * ( * ) (Given)
*        Mode of access required to the object. ('READ', 'WRITE' or
*        'UPDATE'). This is specified so that the container file of any
*        referenced object can be opened in the correct mode.
*     LOC = CHARACTER * ( DAT__SZLOC ) (Returned)
*        A locator associated with the object found.
*     STATUS = INTEGER (given and Returned)
*        Inherited global status.

*  Copyright:
*     Copyright (C) 1982, 1988, 1989, 1992 Science & Engineering Research Council.
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
*     18-FEB-1982 (AJC):
*        Original version.
*     13-Jan-1988 (AJC):
*        Improve error reporting.
*     15-Jan-1988 (AJC):
*        Only set output locator if OK.
*     20-JAN-1988 (AJC):
*        Improve prologue.
*     29-NOV-1989 (AJC):
*        Improve prologue ref REF_ANNUL.
*     21-FEB-1992 (RFWS):
*        Standardised prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'		 ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants

*  Arguments Given:
      CHARACTER * ( * ) ELOC
      CHARACTER * ( * ) CNAME
      CHARACTER * ( * ) MODE

*  Arguments Returned:
      CHARACTER * ( * ) LOC

*  Status:
      INTEGER STATUS		 ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) TLOC ! Locator to component
      CHARACTER * ( DAT__SZTYP ) TYPE ! Type of 'reference' object

*.

*  Check inherited global status.
      IF (STATUS .NE. SAI__OK) RETURN

*  Obtain a locator to the required component.
      CALL DAT_FIND( ELOC, CNAME, TLOC, STATUS )

*  Check type of object.
      CALL DAT_TYPE( TLOC, TYPE, STATUS )

*  If it is a reference, obtain locator to referenced object.
      IF ( TYPE .EQ. 'REFERENCE_OBJ' ) THEN
          CALL REF_GET( TLOC, MODE, LOC, STATUS )

*  If not a reference object return the locator to the specified
*  component.
      ELSE
         CALL DAT_CLONE( TLOC, LOC, STATUS )
      ENDIF

*  Annul locator to the specified component.
      CALL DAT_ANNUL( TLOC, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL EMS_REP( 'REF_FIND_ERR',
     :   'REF_FIND: Error obtaining a locator, via a reference ' //
     :   'if necessary.', STATUS )
      END IF

      END

