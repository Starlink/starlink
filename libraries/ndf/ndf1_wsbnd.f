      SUBROUTINE NDF1_WSBND( NDIM, LBND, UBND, IACB, IWCS, STATUS )
*+
*  Name:
*     NDF1_WSBND

*  Purpose:
*     Obtain the effect of new pixel-index bounds on WCS information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_WSBND( NDIM, LBND, UBND, IACB, IWCS, STATUS )

*  Description:
*     The routine obtains the new WCS information that should apply to
*     an NDF if its pixel-index bounds are changed in a specified way
*     (including possible changes to the number of dimensions).

*  Arguments:
*     NDIM = INTEGER (Given)
*        New number of NDF dimensions.
*     LBND( NDIM ) = INTEGER (Given)
*        New lower pixel-index bounds.
*     UBND( NDIM ) = INTEGER (Given)
*        New upper pixel-index bounds.
*     IACB = INTEGER (Given)
*        Index to the NDF entry in the ACB.
*     IWCS = INTEGER (Returned)
*        AST_ pointer to the new WCS information.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - The NDF's WCS component need not previously exist.
*     - This routine should be invoked prior to actually changing the
*     bounds of the NDF's axis component or main data array, upon whose
*     original shape it depends.
*     - If this routine is called with STATUS set, then a value of
*     AST__NULL will be returned for the IWCS argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

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
*     10-JUL-1997 (RFWS):
*        Original version.
*     3-FEB-1999 (RFWS):
*        Changed to return the new WCS information and not to actually
*        write it back to the NDF (because this may fail if the number
*        of dimensions is modified).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'AST_PAR'          ! AST_ public interface
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants

*  Arguments Given:
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )
      INTEGER IACB

*  Arguments Returned:
      INTEGER IWCS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACBT              ! Index of temporary ACB entry

*.

*  Initialise the returned AST_ pointer.
      IWCS = AST__NULL

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a temporary section from the input NDF, with its shape
*  determined by the new NDF bounds. This results in a new ACB entry.
      CALL NDF1_CUT( IACB, NDIM, LBND, UBND, IACBT, STATUS )

*  Read the NDF's WCS information via this section, which causes the
*  effects of the new bounds to be imposed on it.
      CALL NDF1_RDWCS( IACBT, IWCS, STATUS )

*  Annul the temporary section's entry in the ACB.
      CALL NDF1_ANL( IACBT, STATUS )

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_WSBND', STATUS )

      END
