      SUBROUTINE NDF1_WRWCS( IWCS, IACB, STATUS )
*+
*  Name:
*     NDF1_WRWCS

*  Purpose:
*     Write WCS information to an entry in the ACB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_WRWCS( IWCS, IACB, STATUS )

*  Description:
*     The routine writes new WCS (World Coordinate System) information
*     to an entry in the ACB, over-writing any information that may
*     already be present.

*  Arguments:
*     IWCS = INTEGER (Given)
*        A pointer to an AST_ FrameSet which contains the coordinate
*        system information to be written. This should satisfy all the
*        requirements imposed by the NDF_ library. The information is
*        fully validated by this routine before use.
*     IACB = INTEGER (Given)
*        Index to the NDF entry in the ACB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine can only be used to modify the WCS information in
*     a base NDF. It returns without action if the ACB entry supplied
*     identifies an NDF section.
*     - This routine stores pointers to independent copies of the WCS
*     information (not cloned pointers) in the DCB for future use.

*  Copyright:
*     Copyright (C) 1997 Rutherford Appleton Laboratory.

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
*     24-JUN-1997 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'AST_PAR'          ! AST_ public interface

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read)
*           Whether an NDF is a cut (i.e. section).
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IWCS
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDCB               ! Index to the NDF's DCB entry
      INTEGER IWCSV              ! Pointer to validated WCS information

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the WCS information (this results in a copy of the
*  information).
      CALL NDF1_VWCS( IACB, IWCS, IWCSV, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If the NDF is a section, we do nothing more. Otherwise, obtain an
*  index to the data object entry in the DCB and store the validated
*  WCS information in the DCB entry.
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( .NOT. ACB_CUT( IACB ) ) THEN
               IDCB = ACB_IDCB( IACB )
               CALL NDF1_WWRT( IWCSV, IDCB, STATUS )
            END IF
         END IF
      END IF

*  Annul the pointer to the validated WCS information.
      CALL AST_ANNUL( IWCSV, STATUS )

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_WRWCS', STATUS )

      END
