      SUBROUTINE TRN_GTNVC( IDT, NVIN, NVOUT, STATUS )
*+
*  Name:
*     TRN_GTNVC

*  Purpose:
*     get numbers of compiled variables.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN_GTNVC( IDT, NVIN, NVOUT, STATUS )

*  Description:
*     The routine obtains the numbers of input and output variables from
*     a compiled transformation passed by identifier.

*  Arguments:
*     IDT = INTEGER (given)
*        Identifier for the compiled transformation.
*     NVIN = INTEGER (returned)
*        Number of input variables.
*     NVOUT = INTEGER (returned)
*        Number of output variables.
*     STATUS = INTEGER (given & returned)
*        Inherited error status.

*  Algorithm:
*     - Ensure the TRANSFORM facility is active.
*     - Obtain the compiled transformation table (CTT) slot number from
*       the identifier supplied, validating the identifier in the
*       process.
*     - Extract the numbers of variables information from the
*       appropriate CTT slot.

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


*  Arguments Given:
      INTEGER IDT               ! Identifier for the compiled
                                ! transformation


*  Arguments Returned:
      INTEGER NVIN              ! Number of input variables

      INTEGER NVOUT             ! Number of output variables


*  Status:
      INTEGER STATUS            ! Error status


*  Global Variables:
      INCLUDE 'TRN_CMN'         ! TRN_ common blocks


*  Local Variables:
      INTEGER SLOT              ! CTT slot number


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Ensure the TRANSFORM facility is active.
      CALL TRN1_SETUP( .TRUE., STATUS )


*   Import the identifier, obtaining the compiled transformation table
*   (CTT) slot number.
      CALL TRN1_IMPID( IDT, %VAL( CNF_PVAL( TRN_PCTTI ) ), SLOT,
     :                 STATUS )

*   Obtain the numbers of variables information from the CTT slot.
      CALL TRN1_GTNVS( SLOT, %VAL( CNF_PVAL( TRN_PCTTI ) ), NVIN, NVOUT,
     :                 STATUS )

*   Exit routine.
      END
