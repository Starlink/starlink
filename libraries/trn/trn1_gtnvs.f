      SUBROUTINE TRN1_GTNVS( SLOT, CTTI, NVIN, NVOUT, STATUS )
*+
*  Name:
*     TRN1_GTNVS

*  Purpose:
*     get numbers of variables from a CTT slot.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_GTNVS( SLOT, CTTI, NVIN, NVOUT, STATUS )

*  Description:
*     The routine returns the numbers of input and output variables for
*     the transformation associated with the compiled transformation
*     table (CTT) slot specified.  The slot number is not validated
*     before use.

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
*     10-MAY-1988:  Original version (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_CONST'        ! TRN_ private constants


*  Global Variables:
      INCLUDE 'TRN_CMN'         ! TRN_ common blocks


*  Arguments Given:
      INTEGER SLOT              ! CTT slot number to use
      INTEGER CTTI( TRN_CT_NITEM, TRN_SZCTT )
                                ! CTT integer array


*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
      INTEGER NVIN              ! Number of input variables for the
                                ! transformation
      INTEGER NVOUT             ! Number of output variables for the
                                ! transformation


*  Status:
      INTEGER STATUS


*  External References:
*     <declarations for external function references>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
*     <declarations for local variables>


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Obtain the numbers of variables from the compiled module list for
*   the slot.
      CALL TRN1_GTNVL( CTTI( TRN_CT_NMOD, SLOT ),
     :                 %VAL( CNF_PVAL( CTTI( TRN_CT_PNVAR, SLOT ) ) ),
     :                 NVIN, NVOUT, STATUS )

*   Exit routine.
      END
