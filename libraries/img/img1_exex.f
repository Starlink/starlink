      SUBROUTINE IMG1_EXEX( SLOT, ESLOT, ITEM, FOUND, STATUS )
*+
* Name:
*    IMG1_EXEX

*  Purpose:
*    Checks for the existence of an extension item.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_EXEX( SLOT, ESLOT, ITEM, FOUND, STATUS )

*  Description:
*     This routine locates the HDS object ITEM in the extension (ESLOT)
*     of an NDF (SLOT). ITEM is the name of the object, which may be
*     hiearchical and refer to objects in structures and arrays,
*     provided the final object is a primitive.

*  Arguments:
*     SLOT = INTEGER (Given)
*        The slot number in the Parameter and Extension control blocks
*        of the NDF.
*     ESLOT = INTEGER (Given)
*        The slot number of the extension.
*     ITEM = CHARACTER * ( * ) (Given)
*        The name of the object which is to be located. This may be a
*        hierarchical list of objects. The final component must be a
*        primitive.
*     FOUND = LOGICAL (Returned)
*        True if the object is located.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     11-AUG-1994 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_CONST'        ! IMG_ constants
      INCLUDE 'IMG_ERR'          ! IMG_ error codes
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters

*  Global Variables:
      INCLUDE 'IMG_ECB'          ! IMG Extension Control Block
*        ECB_XLOC( IMG__MXPAR, IMG__MXEXT ) =
*           CHARACTER ( DAT__SZLOC ) (Read)
*        The locator to the extension.

*  Arguments Given:
      INTEGER SLOT
      INTEGER ESLOT
      CHARACTER * ( * ) ITEM

*  Arguments Returned:
      LOGICAL FOUND

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL IMG1_INIT         ! Initialise common blocks
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used length of string

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOC1 ! Primary locator
      CHARACTER * ( DAT__SZLOC ) LOC2 ! Secondary locator
      CHARACTER * ( 2 * DAT__SZNAM ) OBJECT ! Name of "current" object
      INTEGER IAT                ! Current start position in ITEM
      INTEGER INOW               ! New position of period
      LOGICAL MORE               ! ITEM string has more periods
      LOGICAL YES                ! Object exists
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise MORE variable for first pass.
      MORE = .TRUE.

*  Object is not found by default.
      FOUND = .FALSE.

*  Current position in the ITEM string.
      IAT = 1

*  Clone the extension locator (so we can safely annul/re-use this
*  later).
      CALL DAT_CLONE( ECB_XLOC( SLOT, ESLOT ), LOC1, STATUS )

*  The basic idea is to loop locating each period in the item name until
*  no more are left. At each level getting a locator to the new object.
 1    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( STATUS .EQ. SAI__OK .AND. MORE ) THEN

*  Look for a period in the current string.
         INOW = INDEX( ITEM( IAT: ), '.' )
         IF ( INOW .NE. 0 ) THEN

*  Extract the name of the object to be accessed.
            INOW = IAT + INOW - 2
            OBJECT = ITEM( IAT: INOW )

*  Attempt to locate the object.
            CALL ERR_MARK
            CALL IMG1_FOBJ( LOC1, OBJECT, YES, LOC2, STATUS )
            IF ( YES .AND. STATUS .EQ. SAI__OK ) THEN

*  Now release the locator to the previous object and make this the
*  current object.
               CALL DAT_ANNUL( LOC1, STATUS )
               LOC1 = LOC2
               IAT = INOW + 2
            ELSE

*  Didn't find the object for some reason.
               MORE = .FALSE.
            END IF

*  Remove any errors.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )
            END IF
            CALL ERR_RLSE
         ELSE

*  No period. Final object name (or first if not hierarchical) is
*  ITEM( IAT: )
            MORE = .FALSE.
            INOW = CHR_LEN( ITEM )
            OBJECT = ITEM( IAT: INOW )

*  Test for the existence of the object.
            CALL ERR_MARK
            CALL IMG1_FOBJ( LOC1, OBJECT, YES, LOC2, STATUS )
            IF ( YES .AND. STATUS .EQ. SAI__OK ) THEN

*  Test that it is a primitive.
               CALL DAT_PRIM( LOC2, YES, STATUS )
               IF ( YES .AND. STATUS .EQ. SAI__OK ) THEN

*  Object exists.
                  FOUND = .TRUE.
               END IF
            END IF

*  Remove any errors.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )
            END IF
            CALL ERR_RLSE
         END IF

*  Return for next loop.
         GO TO 1
      END IF

*  Release the locators. Take care that failure to an object doesn't
*  cause an further error.
      CALL DAT_VALID( LOC1, YES, STATUS )
      IF ( YES ) CALL DAT_ANNUL( LOC1, STATUS )
      CALL DAT_VALID( LOC2, YES, STATUS )
      IF ( YES ) CALL DAT_ANNUL( LOC2, STATUS )
      END
* $Id$
