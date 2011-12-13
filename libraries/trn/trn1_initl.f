      SUBROUTINE TRN1_INITL( SLOT1, SLOT2, CTTI, STATUS )








*+
*  Name:
*     TRN1_INITL

*  Purpose:
*     initialise slots in the CTT.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_INITL( SLOT1, SLOT2, CTTI, STATUS )

*  Description:
*     The routine initialises newly created slots in the compiled
*     transformation table (CTT) by setting the "number of compiled
*     modules" entries to zero.  The slot numbers supplied are not
*     validated before use.

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
*     11-FEB-1988:  Original version (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_CONST'        ! TRN_ private constants


*  Global Variables:
      INCLUDE 'TRN_CMN'         ! TRN_ common blocks


*  Arguments Given:
      INTEGER SLOT1             ! First slot to initialise
      INTEGER SLOT2             ! Last slot to initialise


*  Arguments Given and Returned:
      INTEGER CTTI( TRN_CT_NITEM, TRN_SZCTT )
                                ! CTT integer array

*  Arguments Returned:
*     <declarations and descriptions for exported arguments>


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
*     <declarations for external function references>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
      INTEGER SLOT              ! Loop counter for slots


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Zero the "number of modules" entries in the CTT integer array.
      DO SLOT = SLOT1, SLOT2
        CTTI( TRN_CT_NMOD, SLOT ) = 0
      ENDDO


*   Exit routine.
      END
