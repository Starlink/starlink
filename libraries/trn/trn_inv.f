      SUBROUTINE TRN_INV( LOCTR, STATUS )








*+
*  Name:
*     TRN_INV

*  Purpose:
*     invert transformation.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN_INV( LOCTR, STATUS )

*  Description:
*     The routine inverts a transformation structure passed by HDS
*     locator.  The inversion process interchanges the forward and
*     inverse transformation definitions.

*  Arguments:
*     LOCTR = CHARACTER * ( * ) (given)
*        HDS locator to the transformation structure - the structure
*        is modified by this routine.
*     STATUS = INTEGER (given & returned)
*        Inherited error status.

*  Algorithm:
*     - Validate the transformation structure.
*     - Locate the structure's MODULE_ARRAY component.
*     - If necessary, extend the size of this array by one to provide
*       some workspace.
*     - Reverse the order of the array elements by interchanging them in
*       pairs.
*     - After interchanging each pair, invert the transformation module
*       which each element contains.
*     - Restore the MODULE_ARRAY to its original size.
*     - If the number of modules was odd, invert the remaining central
*       one.
*     - Reverse the transformation structure's definition status
*       information.
*     - Update the software version number.

*  Implementation Deficiencies:
*     - The routine runs less efficiently than is desirable.  This is
*       largely due to the relatively high cost of moving the contents
*       of one HDS structure into another.  The performance would
*       probably improve if HDS provided a routine to do this.

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council.
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
*     R.F. Warren-Smith (DUVAD::RFWS)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1988:  Original version (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     None known.
*     {note_new_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants


*  Arguments Given:
      CHARACTER * ( * ) LOCTR   ! Locator to transformation structure


*  Status:
      INTEGER STATUS            ! Error status


*  Local Variables:
      INTEGER NMOD              ! Number of modules in transformation

      INTEGER IMOD              ! Loop counter for interchanging modules

      CHARACTER * ( DAT__SZLOC ) LOCMA
                                ! Locator to MODULE_ARRAY component

      CHARACTER * ( DAT__SZLOC ) LOCT
                                ! Locator to temporary array element

      CHARACTER * ( DAT__SZLOC ) LOCA
                                ! First module to be interchanged

      CHARACTER * ( DAT__SZLOC ) LOCB
                                ! Second module to be interchanged


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Validate the transformation structure.
      CALL TRN1_VTR( LOCTR, STATUS )


*   Obtain a locator to the MODULE_ARRAY component and determine the
*   number of elements (modules) in this array.
      CALL DAT_FIND( LOCTR, 'MODULE_ARRAY', LOCMA, STATUS )
      CALL DAT_SIZE( LOCMA, NMOD, STATUS )


*   If there is no error...
      IF( STATUS .EQ. SAI__OK ) THEN


*   If there is more than 1 module, temporarily extend the array size
*   by one to provide workspace for interchanging its elements.
        IF( NMOD .GT. 1 ) THEN
          CALL DAT_ALTER( LOCMA, 1, NMOD + 1, STATUS )


*   Obtain a locator to the new temporary element.
          CALL DAT_CELL( LOCMA, 1, NMOD + 1, LOCT, STATUS )


*   Loop to reverse the order of the array elements.  Obtain locators
*   to each pair of elements to be interchanged.
          DO IMOD = 1, ( NMOD / 2 )
            CALL DAT_CELL( LOCMA, 1, IMOD, LOCA, STATUS )
            CALL DAT_CELL( LOCMA, 1, NMOD - IMOD + 1, LOCB, STATUS )


*   Interchange the modules by moving their contents, using the
*   final temporary array element as workspace.
            CALL TRN1_MOVST( LOCA, LOCT, STATUS )
            CALL TRN1_MOVST( LOCB, LOCA, STATUS )
            CALL TRN1_MOVST( LOCT, LOCB, STATUS )


*   Invert each of the modules which have been interchanged.
            CALL TRN1_INVTM( LOCA, STATUS )
            CALL TRN1_INVTM( LOCB, STATUS )


*   Annul the module locators.
            CALL DAT_ANNUL( LOCA, STATUS )
            CALL DAT_ANNUL( LOCB, STATUS )


*   End of "loop to interchange modules" loop.
          ENDDO


*   Annul the locator to the temporary array element and change the
*   array back to its original size.
          CALL DAT_ANNUL( LOCT, STATUS )
          CALL DAT_ALTER( LOCMA, 1, NMOD, STATUS )


*   End of "there is more than one module" condition.
        ENDIF


*   If the number of modules is odd, obtain a locator to the central
*   one and invert it.
        IF( MOD( NMOD, 2 ) .EQ. 1 ) THEN
          CALL DAT_CELL( LOCMA, 1, ( NMOD / 2 ) + 1, LOCA, STATUS )
          CALL TRN1_INVTM( LOCA, STATUS )
          CALL DAT_ANNUL( LOCA, STATUS )
        ENDIF


*   Annul the locator to the MODULE_ARRAY component.
        CALL DAT_ANNUL( LOCMA, STATUS )


*   Interchange the FORWARD and INVERSE definition status components
*   in the transformation structure.
        CALL TRN1_CSWAP( LOCTR, 'FORWARD', 'INVERSE', STATUS )


*   Update the software version number.
        CALL TRN1_UPVSN( LOCTR, STATUS )


*   End of "no error finding the MODULE_ARRAY component" condition.
      ENDIF


*   Exit routine.
      END
