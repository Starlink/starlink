      SUBROUTINE NDF_SECT( INDF1, NDIM, LBND, UBND, INDF2, STATUS )
*+
*  Name:
*     NDF_SECT

*  Purpose:
*     Create an NDF section.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_SECT( INDF1, NDIM, LBND, UBND, INDF2, STATUS )

*  Description:
*     The routine creates a new NDF section which refers to a selected
*     region of an existing NDF (or NDF section). The region may be
*     larger or smaller in extent than the initial NDF.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        Identifier for the initial NDF.
*     NDIM = INTEGER (Given)
*        Number of dimensions for the new section.
*     LBND( NDIM ) = INTEGER (Given)
*        Lower pixel-index bounds of the section.
*     UBND( NDIM ) = INTEGER (Given)
*        Upper pixel-index bounds of the section.
*     INDF2 = INTEGER (Returned)
*        Identifier for the new section.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The number of section dimensions need not match the number of
*     dimensions in the initial NDF. Pixel-index bounds will be padded
*     with 1's as necessary to identify the pixels to which the new
*     section should refer.
*     -  The array components of sections which extend beyond the
*     pixel-index bounds of the initial NDF will be padded with bad
*     pixels.
*     -  If the section bounds extend beyond the bounds of the
*     associated base NDF and any of the NDF's axis arrays have defined
*     values, then these values will be extrapolated as necessary.
*     -  If this routine is called with STATUS set, then a value of
*     NDF__NOID will be returned for the INDF2 argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason. The NDF__NOID
*     constant is defined in the include file NDF_PAR.

*  Algorithm:
*     -  Set an initial value of NDF__NOID for the INDF2 argument before
*     checking the inherited status.
*     -  Check the bounds of the region to be selected for validity.
*     -  Import the initial NDF identifier.
*     -  Create the new NDF entry in the ACB.
*     -  Export an identifier for the new NDF.
*     -  If an error occurred, then set a value of NDF__NOID for the
*     IACB2 argument and report context information.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-OCT-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     23-NOV-1989 (RFWS):
*        Changed the routine's name to NDF_SECT.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants

*  Arguments Given:
      INTEGER INDF1
      INTEGER NDIM
      INTEGER LBND( * )
      INTEGER UBND( * )

*  Arguments Returned:
      INTEGER INDF2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB1              ! Index of initial NDF in the ACB
      INTEGER IACB2              ! Index of new NDF in the ACB

*.

*  Set an initial value for the INDF2 argument.
      INDF2 = NDF__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the initial NDF identifier.
      CALL NDF1_IMPID( INDF1, IACB1, STATUS )

*  Check the bounds of the region to select for validity.
      CALL NDF1_VBND( NDIM, LBND, UBND, STATUS )

*  Create an ACB entry for the new NDF.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL NDF1_CUT( IACB1, NDIM, LBND, UBND, IACB2, STATUS )
      END IF

*  Export an identifier for the new NDF.
      CALL NDF1_EXPID( IACB2, INDF2, STATUS )

*  If an error occurred, then set a value of NDF__NOID for the INDF2
*  argument, report context information and call the error tracing
*  routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         INDF2 = NDF__NOID
         CALL ERR_REP( 'NDF_SECT_ERR',
     :   'NDF_SECT: Error creating an NDF section.', STATUS )
         CALL NDF1_TRACE( 'NDF_SECT', STATUS )
      END IF

      END
