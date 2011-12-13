      SUBROUTINE TRN1_GETSL( CTTI, SLOT, IDT, STATUS )








*+
*  Name:
*     TRN1_GETSL

*  Purpose:
*     get a free slot in the compiled transformation table.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_GETSL( CTTI, SLOT, IDT, STATUS )

*  Description:
*     The routine searches the compiled transformation table (CTT) for a
*     slot which does not yet contain a compiled transformation.  If one
*     is found, the slot number is returned and an associated
*     transformation ID is issued. If no free slot is present, a slot
*     number and ID of zero are returned.

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
*     24-MAR-1988:  Improved arithmetic security in IDT calculation
*        (DUVAD::RFWS)
*     28-MAR-1988:  Added further error checking (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants
      INCLUDE 'TRN_PAR'          ! TRN_ public constants
      INCLUDE 'TRN_CONST'        ! TRN_ private constants
      INCLUDE 'TRN_ERR'          ! TRN_ error codes


*  Global Variables:
      INCLUDE 'TRN_CMN'         ! TRN_ common blocks


*  Arguments Given:
      INTEGER CTTI( TRN_CT_NITEM, TRN_SZCTT )
                                ! CTT integer array


*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
      INTEGER SLOT              ! Slot nunber
      INTEGER IDT               ! Associated transformation ID


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
*     <declarations for external function references>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
      INTEGER IDCODE            ! Used for encoding IDT values


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Search for the first slot with no modules recorded.
      SLOT = 1
      DO WHILE ( ( SLOT .LE. TRN_SZCTT ) .AND.
     :           ( CTTI( TRN_CT_NMOD, SLOT ) .NE. 0 ) )
        SLOT = SLOT + 1
      ENDDO


*   Check the slot number does not exceed the maximum number of compiled
*   transformations allowed and report an error if it does (this check
*   is required to prevent duplicate IDs being issued, but it should
*   never be encountered as the number of compiled transformations
*   allowed is very large).
      IF( SLOT .GE. TRN__MXCTR ) THEN
        STATUS = TRN__CMTOF     ! compiled mapping table overflow
        CALL TRN1_ERROR( 'TRN1_GETSL', ' ', STATUS )


*   If no slot was found, return zero.
      ELSE IF( SLOT .GT. TRN_SZCTT ) THEN
        SLOT = 0
        IDT = TRN__NOID


*   If a slot was found, set the associated compiled transformation ID.
*   This is calculated from the slot number and the value IDCODE.  This
*   latter value is set to the number of IDs issued so far (TRN_IDCNT,
*   stored in the common block), except that it cycles to prevent
*   overflow when calculating the IDT value. This arrangement prevents
*   old (annulled) ID values being re-used (for a very long time, at
*   least), so that ID values may be regarded as unique.
      ELSE
        IDCODE = MOD( TRN_IDCNT, ( NUM__MAXI / TRN__MXCTR ) )
        IDT = SLOT + ( TRN__MXCTR * IDCODE )


*   Store the ID as a check count in the CTT and increment the count of
*   IDs issued.
        CTTI( TRN_CT_CHECK, SLOT ) = IDT
        TRN_IDCNT = TRN_IDCNT + 1
      ENDIF


*   Exit routine.
      END
