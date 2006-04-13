      SUBROUTINE ARY_SECT( IARY1, NDIM, LBND, UBND, IARY2, STATUS )
*+
*  Name:
*     ARY_SECT

*  Purpose:
*     Create an array section.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_SECT( IARY1, NDIM, LBND, UBND, IARY2, STATUS )

*  Description:
*     The routine creates a new array section which refers to a
*     selected region of an existing array (or array section). The
*     section may be larger or smaller in extent than the original
*     array.

*  Arguments:
*     IARY = INTEGER (Given)
*        Identifier for the initial array.
*     NDIM = INTEGER (Given)
*        Number of dimensions for new section.
*     LBND( NDIM ) = INTEGER (Given)
*        Lower pixel-index bounds for the new section.
*     UBND( NDIM ) = INTEGER (Given)
*        Upper pixel-index bounds for the new section.
*     IARY2 = INTEGER (Returned)
*        Identifier for the new section.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The number of section dimensions need not match the number of
*     dimensions in the initial array. Pixel-index bounds will be
*     padded with 1's as necessary to identify the pixels to which the
*     new section should refer.
*     -  Note that sections which extend beyond the pixel-index bounds
*     of the initial array will be padded with bad pixels.
*     -  If this routine is called with STATUS set, then a value of
*     ARY__NOID will be returned for the IARY2 argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason. The ARY__NOID
*     constant is defined in the include file ARY_PAR.

*  Algorithm:
*     -  Set an initial value of ARY__NOID for the IARY2 argument before
*     checking the inherited status.
*     -  Check the bounds of the region to be selected for validity.
*     -  Import the initial array identifier.
*     -  Create the new array entry in the ACB.
*     -  Export an identifier for the new array.
*     -  If an error occurred, then set a value of ARY__NOID for the
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-AUG-1989 (RFWS):
*        Original version.
*     15-SEP-1989 (RFWS):
*        Changed LBND and UBND to be assumed size arrays to prevent
*        run-time errors if NDIM is not valid. Also added a check to
*        prevent ARY1_CUT being called under these circumstances.
*     20-OCT-1989 (RFWS):
*        Renamed to ARY_SECT.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants

*  Arguments Given:
      INTEGER IARY1
      INTEGER NDIM
      INTEGER LBND( * )
      INTEGER UBND( * )

*  Arguments Returned:
      INTEGER IARY2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB1              ! Index of initial array in the ACB
      INTEGER IACB2              ! Index of new array in the ACB

*.

*  Set an initial value for the IARY2 argument.
      IARY2 = ARY__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the initial array identifier.
      CALL ARY1_IMPID( IARY1, IACB1, STATUS )

*  Check the bounds of the region to select for validity.
      CALL ARY1_VBND( NDIM, LBND, UBND, STATUS )

*  Create an ACB entry for the new array.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL ARY1_CUT( IACB1, NDIM, LBND, UBND, IACB2, STATUS )
      END IF

*  Export an identifier for the new array.
      CALL ARY1_EXPID( IACB2, IARY2, STATUS )

*  If an error occurred, then set a value of ARY__NOID for the IARY2
*  argument, report context information and call the error tracing
*  routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         IARY2 = ARY__NOID
         CALL ERR_REP( 'ARY_SECT_ERR',
     :   'ARY_SECT: Error creating an array section.', STATUS )
         CALL ARY1_TRACE( 'ARY_SECT', STATUS )
      END IF       

      END
