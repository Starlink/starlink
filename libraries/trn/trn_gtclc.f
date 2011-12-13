      SUBROUTINE TRN_GTCLC( IDT, CLASS, STATUS )
*+
*  Name:
*     TRN_GTCLC

*  Purpose:
*     get compiled classification.

*  Language:
*     Starlink Fortran

*  Invocation:
*     TRN_GTCLC( IDT, CLASS, STATUS )

*  Description:
*     The routine obtains the logical classification array associated
*     with a compiled transformation.

*  Arguments:
*     IDT = INTEGER (given)
*        Identifier for the compiled transformation.
*     CLASS( TRN__MXCLS ) = LOGICAL (returned)
*        Logical classification array.
*     STATUS = INTEGER (given & returned)
*        Inherited error status.

*  Algorithm:
*     - Ensure that the TRANSFORM facility is active.
*     - Obtain the compiled transformation table (CTT) slot number from
*       the identifier supplied, validating the identifier in the
*       process.
*     - Extract the classification information from the appropriate slot
*       in the CTT classification array list.

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
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_PAR'          ! TRN_ public constants


*  Arguments Given:
      INTEGER IDT               ! Identifier for the compiled
                                ! transformation


*  Arguments Returned:
      LOGICAL CLASS( TRN__MXCLS )
                                ! Classification array


*  Status:
      INTEGER STATUS            ! Error status


*  Global Variables:
      INCLUDE 'TRN_CMN'         ! TRN_ common blocks


*  Local Variables:
      INTEGER SLOT              ! CTT slot number


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Ensure that the TRANSFORM facility is active.
      CALL TRN1_SETUP( .TRUE., STATUS )


*   Import the transformation ID, obtaining the compiled transformation
*   table (CTT) slot number.
      CALL TRN1_IMPID( IDT, %VAL( CNF_PVAL( TRN_PCTTI ) ), SLOT,
     :                 STATUS )

*   Obtain the classification array from the appropriate slot in the
*   CTT classification array list.
      CALL TRN1_GTCLC( SLOT, %VAL( CNF_PVAL( TRN_PCTTC ) ), CLASS,
     :                 STATUS )

*   Exit routine.
      END
