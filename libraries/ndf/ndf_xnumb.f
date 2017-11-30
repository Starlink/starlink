      SUBROUTINE NDF_XNUMB( INDF, NEXTN, STATUS )
*+
*  Name:
*     NDF_XNUMB

*  Purpose:
*     Determine the number of extensions in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_XNUMB( INDF, NEXTN, STATUS )

*  Description:
*     The routine returns the number of extensions present in the NDF
*     whose identifier is supplied.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     NEXTN = INTEGER (Returned)
*        Number of extensions present.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     If this routine is called with STATUS set, then a value of zero
*     will be returned for the NEXTN argument, although no further
*     processing will occur. The same value will also be returned if the
*     routine should fail for any reason.

*  Copyright:
*     Copyright (C) 1994 Particle Physics & Astronomy Research Council

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
*     24-NOV-1989 (RFWS):
*        Original version.
*     27-JUL-1994 (RFWS):
*        Return a value of zero for NEXTN under error conditions.
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
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_XLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to extension (MORE) structure.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER INDF

*  Arguments Returned:
      INTEGER NEXTN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Set an initial default value for the NEXTN argument.
      NEXTN = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain an index to the data object entry in the DCB.
         IDCB = ACB_IDCB( IACB )

*  Ensure that extension (MORE) structure information is available in
*  the DCB.
         CALL NDF1_DX( IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If the extension (MORE) structure does not exist, then there can be
*  no extensions present. Otherwise, enquire how many extension
*  components there are.
            IF ( DCB_XLOC( IDCB ) .NE. DAT__NOLOC ) THEN
               CALL DAT_NCOMP( DCB_XLOC( IDCB ), NEXTN, STATUS )
            END IF
         END IF
      END IF

*  If an error occurred, then return a zero value for NEXTN.
      IF ( STATUS .NE. SAI__OK ) THEN
         NEXTN = 0

*  Call error tracing routine and exit.
         CALL NDF1_TRACE( 'NDF_XNUMB', STATUS )
      END IF

      END
