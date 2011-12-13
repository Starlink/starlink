       SUBROUTINE REF_CRPUT( ELOC, CNAME, LOC, INTERN, STATUS )
*+
*  Name:
*     REF_CRPUT

*  Purpose:
*     Create and write a reference object.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL REF_CRPUT( ELOC, CNAME, LOC, INTERN, STATUS )

*  Description:
*     This routine creates a reference object as a component of a
*     specified structure and writes a reference to an HDS object in
*     it.  If the specified component already exists and is a reference
*     object, it will be used. If it is not a reference object, an
*     error is reported.  The reference may be described as "internal"
*     which means that the referenced object is in the same container
*     file as the reference object.

*  Arguments:
*     ELOC = CHARACTER * ( DAT__SZLOC ) (Given)
*        A locator associated with the structure which is to contain
*        the reference object.
*     CNAME = CHARACTER * ( DAT__SZNAM ) (Given)
*        The component name of the reference object to be created.
*     LOC = CHARACTER * ( * ) (Given)
*        A locator associated with the object to be referenced.
*     INTERN = LOGICAL (Given)
*        Whether or not the referenced object is "internal".  Set this
*        to .TRUE. if the reference is "internal" and to .FALSE. if it
*        is not.
*     STATUS = INTEGER (Given and Returned)
*        Inherited global status.

*  Copyright:
*     Copyright (C) 1982, 1987, 1988, 1992 Science & Engineering Research Council.
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
*     SLW: Sid Wright (UCL)
*     AJC: A.J. Chipperfield (STARLINK, RAL)
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     24-APR-1982 (SLW):
*        Original version.
*     17-FEB-1987 (AJC):
*        Add INTERNAL flag.
*     18-FEB-1987 (AJC):
*        Allow component to exist already.
*     13-JAN-1988 (AJC):
*        Improve error reporting.
*     15-JAN-1988 (AJC):
*        Use DAT_THERE to check for existence.
*     20-JAN-1988 (AJC):
*        Improve prologue.
*     29-JAN-1988 (AJC):
*        Rename INTERNAL to INTERN.
*     20-FEB-1992 (RFWS):
*        Standardise prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'		 ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'REF_ERR'		 ! REF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) ELOC
      CHARACTER * ( * ) CNAME
      CHARACTER * ( * ) LOC
      LOGICAL INTERN

*  Status:
      INTEGER STATUS		 ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) TLOC ! Locator to reference component
      CHARACTER * ( DAT__SZTYP ) TYPE ! Type of existing component
      LOGICAL THERE              ! Whether component already exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check if the required component already exists.
      CALL DAT_THERE( ELOC, CNAME, THERE, STATUS )
      IF ( THERE ) THEN

*  Check it is a reference object.
         CALL DAT_FIND( ELOC, CNAME, TLOC, STATUS )
         CALL DAT_TYPE( TLOC, TYPE, STATUS )

*  Report an error if it is not.
         IF ( TYPE .NE. 'REFERENCE_OBJ' ) THEN
            STATUS = REF__OBJIN
            CALL EMS_SETC( 'CNAME', CNAME )
            CALL DAT_MSG( 'STRUCT', ELOC )
            CALL EMS_SETC( 'TYPE', TYPE )
            CALL EMS_REP( 'REF_CRPUT_INV',
     :                    'The ^CNAME component in the HDS ' //
     :                    'structure ^STRUCT has an invalid ' //
     :                    'type of ''^TYPE''; it should be of ' //
     :                    'type ''REFERENCE_OBJ''.', STATUS )
         END IF

*  Create the required reference object.
      ELSE
         CALL REF_NEW( ELOC, CNAME, STATUS )
         CALL DAT_FIND( ELOC, CNAME, TLOC, STATUS )
      END IF

*  Write the reference information to it.
      CALL REF_PUT( TLOC, LOC, INTERN, STATUS )
      CALL DAT_ANNUL( TLOC, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL EMS_REP( 'REF_CRPUT_ERR',
     :   'REF_CRPUT: Error creating and writing a reference object.',
     :   STATUS )
      END IF

      END
