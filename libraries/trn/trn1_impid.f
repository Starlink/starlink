      SUBROUTINE TRN1_IMPID( IDT, CTTI, SLOT, STATUS )








*+
*  Name:
*     TRN1_IMPID

*  Purpose:
*     import compiled transformation ID.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_IMPID( IDT, CTTI, SLOT, STATUS )

*  Description:
*     The routine checks a compiled transformation ID to ensure that it
*     is valid and refers to an occupied slot in the compiled
*     transformation table (CTT).  If the ID is OK, the associated CTT
*     slot number is returned, otherwise a STATUS value is set and an
*     error is reported.

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
*     12-FEB-1988:  Original version (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_PAR'          ! TRN_ public constants
      INCLUDE 'TRN_CONST'        ! TRN_ private constants
      INCLUDE 'TRN_ERR'          ! TRN_ error codes


*  Global Variables:
      INCLUDE 'TRN_CMN'         ! TRN_ common blocks


*  Arguments Given:
      INTEGER IDT               ! The transformation ID to be tested
      INTEGER CTTI( TRN_CT_NITEM, TRN_SZCTT )
                                ! The CTT integer array


*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
      INTEGER SLOT              ! The CTT slot number


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
*     <declarations for external function references>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
      LOGICAL IDOK              ! Whether the ID is OK


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Check the ID value is positive.
      IDOK = ( IDT .GT. 0 )


*   If OK, calculate the slot number associated with the ID and check
*   the slot number is valid.
      IF( IDOK ) THEN
        SLOT = MOD( IDT, TRN__MXCTR )
        IDOK = ( ( SLOT .GE. 1 ) .AND.
     :           ( SLOT .LE. MIN( TRN_SZCTT, TRN__MXCTR ) ) )


*   If OK, check the CTT slot contains a compiled transformation and
*   that the ID matches the check count in the CTT.
        IF( IDOK ) IDOK = ( ( CTTI( TRN_CT_NMOD, SLOT ) .NE. 0 ) .AND.
     :                      ( CTTI( TRN_CT_CHECK, SLOT ) .EQ. IDT ) )
      ENDIF


*   If the ID was not OK, set the slot number to zero and report an
*   error.
      IF( .NOT. IDOK ) THEN
        SLOT = 0
        STATUS = TRN__MIDIN     ! compiled mapping identifier invalid
        CALL TRN1_ERROR( 'TRN1_IMPID', ' ', STATUS )
      ENDIF


*   Exit routine.
      END
