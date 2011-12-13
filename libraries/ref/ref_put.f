       SUBROUTINE REF_PUT( ELOC, LOC, INTERN, STATUS )
*+
*  Name:
*     REF_PUT

*  Purpose:
*     Write a reference into a reference object.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL REF_PUT( ELOC, LOC, INTERN, STATUS )

*  Description:
*     This routine writes a reference to an HDS object into an existing
*     reference structure.  An error is reported if an attempt is made
*     to write a reference into an object which is not a reference
*     object.  The reference may be described as "internal" which means
*     that the referenced object is in the same container file as the
*     reference object.

*  Arguments:
*     ELOC = CHARACTER * ( * ) (Given)
*        A locator associated with the reference object.
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
*     16-FEB-1987 (AJC):
*        Expanded REFERENCE structure.
*     24-DEC-1987 (AJC):
*        Change ERR_REP to DAT_ERTXT.
*     13-JAN-1988 (AJC):
*        Improve error reporting.
*     21-JAN-1988 (AJC):
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
      INCLUDE 'REF_PAR'		 ! REF_ public constants
      INCLUDE 'REF_ERR'          ! REF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) ELOC
      CHARACTER * ( * ) LOC
      LOGICAL INTERN

*  Status:
      INTEGER STATUS		 ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZTYP ) TYPE ! Type of object
      CHARACTER * ( REF__SZREF ) FILE ! Container file of ref'd object
      CHARACTER * ( REF__SZREF ) PATH ! Pathname of referenced object
      INTEGER NLEV               ! Number of levels in pathname

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check locator is to structure of type REFERENCE_OBJ. Report an error
*  if it is not.
      CALL DAT_TYPE( ELOC, TYPE, STATUS )
      IF ( TYPE .NE. 'REFERENCE_OBJ' ) THEN
         STATUS = REF__OBJIN
         CALL DAT_MSG( 'OBJECT', ELOC )
         CALL EMS_SETC( 'TYPE', TYPE )
         CALL EMS_REP( 'REF_PUT_INV',
     :                 'The HDS object ^OBJECT has an invalid type ' //
     :                 'of ''^TYPE''; it should be of type ' //
     :                 '''REFERENCE_OBJ''.', STATUS )

*  Obtain the components of the reference from the locator.  Use a
*  blank filename if the reference is "internal".
      ELSE
         CALL HDS_TRACE( LOC, NLEV, PATH, FILE, STATUS )
         IF ( INTERN ) FILE = ' '

*  Insert the reference components.
         CALL CMP_PUT0C( ELOC, 'FILE', FILE, STATUS )
         CALL CMP_PUT0C( ELOC, 'PATH', PATH, STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL EMS_REP( 'REF_PUT_ERR',
     :   'REF_PUT: Error writing a reference into a reference object.',
     :   STATUS )
      END IF

      END
