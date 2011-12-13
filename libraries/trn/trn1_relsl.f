      SUBROUTINE TRN1_RELSL( SLOT1, SLOT2, CTTI, CTTL, STATUS )
*+
*  Name:
*     TRN1_RELSL

*  Purpose:
*     Release a series of slots in the CTT.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_RELSL( SLOT1, SLOT2, CTTI, CTTL, STATUS )

*  Description:
*     The routine releases a series of slots in the compiled
*     transformation table (CTT).  The resources used by any of the
*     slots which contain a compiled transformation are released,
*     associated temporary HDS objects are deleted and the "number of
*     modules" entry for the slot in the CTT is reset to zero. This
*     makes the slot available for re-use.  The slot numbers supplied
*     are not validated before use.  This routine attempts to execute
*     even if STATUS is set on entry, although no error report will be
*     made if it fails under these circumstances.  If more than one
*     error occurs (and STATUS is not set on entry) then a report will
*     only be made for the first error.

*  Copyright:
*     Copyright (C) 1988, 1992 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     11-FEB-1988 (RFWS):
*        Original version.
*     13-FEB-1992 (RFWS):
*        Added handling of character string length when passing mapped
*        values (for Unix compatibility).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_CONST'        ! TRN_ private constants

*  Global Variables:
      INCLUDE 'TRN_CMN'          ! TRN_ common blocks

*  Arguments Given:
      INTEGER SLOT1              ! First CTT slot to release
      INTEGER SLOT2              ! Last CTT slot to release

*  Arguments Given and Returned:
      INTEGER CTTI( TRN_CT_NITEM, TRN_SZCTT ) ! CTT integer array
      CHARACTER * ( * ) CTTL( TRN_SZCTT ) ! CTT locator list

*  Status:
      INTEGER STATUS             ! Error status

*  Local Variables:
      INTEGER SLOT               ! Loop counter for CTT slots
      INTEGER LSTAT              ! Local status variable

*.

*  Initialise the local status variable.
      LSTAT = STATUS

*  Process each slot in turn.
      DO SLOT = SLOT1, SLOT2

*  If the slot contains a compiled transformation, pass the compiled
*  module list (CML) locator array to TRN1_RELTS, which deletes all the
*  compiled modules in the transformation and releases the resources
*  used.
        IF( CTTI( TRN_CT_NMOD, SLOT ) .NE. 0 ) THEN
          CALL TRN1_RELTS( CTTI( TRN_CT_NMOD, SLOT ),
     :                   %VAL( CNF_PVAL( CTTI( TRN_CT_PLOCM, SLOT ) ) ),
     :                     LSTAT, %VAL( CNF_CVAL( DAT__SZLOC ) ) )

*  Use TRN1_RELTS to delete the temporary structure holding the CML
*  (which includes the locator list just used), releasing the resources
*  used.
          CALL TRN1_RELTS( 1, CTTL( SLOT ), LSTAT )

*  Set the number of modules in the slot to zero, indicating there is
*  no transformation present.
          CTTI( TRN_CT_NMOD, SLOT ) = 0
        ENDIF
      ENDDO

*  If STATUS was not set on entry, return the local status value.
      IF( STATUS .EQ. SAI__OK ) STATUS = LSTAT

*  Exit routine.
      END
