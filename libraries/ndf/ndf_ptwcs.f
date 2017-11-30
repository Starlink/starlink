      SUBROUTINE NDF_PTWCS( IWCS, INDF, STATUS )
*+
*  Name:
*     NDF_PTWCS

*  Purpose:
*     Store world coordinate system information in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_PTWCS( IWCS, INDF, STATUS )

*  Description:
*     The routine stores new world coordinate system (WCS) information
*     in an NDF, over-writing any already present.

*  Arguments:
*     IWCS = INTEGER (Given)
*        An AST pointer to a FrameSet (SUN/210) containing information
*        about the new world coordinate systems to be associated with
*        the NDF.
*     INDF = INTEGER (Given)
*        NDF identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine may only be used to store WCS information in a
*     base NDF. If an NDF section is supplied, it will simply return
*     without action.
*     - The AST FrameSet supplied must conform to the various
*     restrictions imposed by the NDF_ system (e.g. on the nature of
*     its base Frame). An error will result if any of these
*     restrictions is not met.
*     - This routine makes a copy of the information in the FrameSet
*     supplied, so the original FrameSet may subsequently be modified
*     without affecting the behaviour of the NDF_ system.

*  Copyright:
*     Copyright (C) 1997 Rutherford Appleton Laboratory

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
*     2-JUL-1997 (RFWS):
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
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'AST_PAR'          ! AST_ public interface

*  Arguments Given:
      INTEGER INDF
      INTEGER IWCS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to the NDF entry in the ACB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Check that WRITE access to the NDF is available.
      CALL NDF1_CHACC( IACB, 'WRITE', STATUS )

*  Write the information to the NDF (this also validates it and makes a
*  copy).
      CALL NDF1_WRWCS( IWCS, IACB, STATUS )

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_PTWCS_ERR',
     :   'NDF_PTWCS: Error storing world coordinate system ' //
     :   'information in an NDF.', STATUS )
         CALL NDF1_TRACE( 'NDF_PTWCS', STATUS )
      END IF

      END
