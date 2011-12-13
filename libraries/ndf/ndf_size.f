      SUBROUTINE NDF_SIZE( INDF, NPIX, STATUS )
*+
*  Name:
*     NDF_SIZE

*  Purpose:
*     Determine the size of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_SIZE( INDF, NPIX, STATUS )

*  Description:
*     The routine returns the number of pixels in the NDF whose
*     identifier is supplied (i.e. the product of its dimensions).

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     NPIX = INTEGER (Returned)
*        Number of pixels in the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If this routine is called with STATUS set, then a value of 1
*     will be returned for the NPIX argument, although no further
*     processing will occur.  The same value will also be returned if
*     the routine should fail for any reason.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Obtain the NDF size from its data array component.
*     -  If an error occurred, then report context information.
*     -  Under error conditions, return a "safe" value of NPIX.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
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
*     29-SEP-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     28-MAR-1990 (RFWS):
*        Renamed SIZE to NPIX.
*     4-DEC-1990 (RFWS):
*        Changed to return a "safe" value of NPIX under error
*        conditions.
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
      INCLUDE 'NDF_CONST'        ! NDF_ private constants

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        DCB_DID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.

*  Arguments Given:
      INTEGER INDF

*  Arguments Returned:
      INTEGER NPIX

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to NDF entry in the ACB

*.

*  Check inherited global status.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Import the NDF identifier.
         CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Obtain the NDF size from its data array component.
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL ARY_SIZE( ACB_DID( IACB ), NPIX, STATUS )
         END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'NDF_SIZE_ERR',
     :      'NDF_SIZE: Error determining the size of an NDF.', STATUS )
            CALL NDF1_TRACE( 'NDF_SIZE', STATUS )
         END IF
      END IF

*  Under error conditions, return a "safe" value of NPIX.
      IF ( STATUS .NE. SAI__OK ) THEN
         NPIX = 1
      END IF

      END
