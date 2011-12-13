      SUBROUTINE NDF_LOC( INDF, MODE, LOC, STATUS )
*+
*  Name:
*     NDF_LOC

*  Purpose:
*     Obtain an HDS locator for an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_LOC( INDF, MODE, LOC, STATUS )

*  Description:
*     The routine returns an HDS locator for an NDF whose identifier is
*     supplied.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     MODE = CHARACTER * ( * ) (Given)
*        Mode of access required to the NDF: 'READ', 'UPDATE' or
*        'WRITE'.
*     LOC = CHARACTER * ( * ) (Returned)
*        HDS locator to the NDF data structure.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If an identifier for an NDF section is supplied to this
*     routine, then the returned locator will refer to the associated
*     base NDF.
*     -  It is the caller's responsibility to annul the locator
*     returned by this routine (e.g. by calling the HDS routine
*     DAT_ANNUL) when it is no longer required. The NDF_ system will not
*     perform this task itself.
*     -  If this routine is called with STATUS set, then an invalid
*     locator value of DAT__NOLOC will be returned for the LOC argument,
*     although no further processing will occur. The same value will
*     also be returned if the routine should fail for any reason.
*     The constant DAT__NOLOC is defined in the include file DAT_PAR.
*     -  Although this routine will check the access mode value
*     supplied against the available access to the NDF, HDS does not
*     allow the returned locator to be protected against write access
*     in the case where WRITE access to an NDF is available, but only
*     READ access was requested. In this case it is the responsibility
*     of the caller to respect the locator access restriction.
*     -  The locator returned by this routine should not be used to
*     make alterations to any part of a data structure which is
*     simultaneously being used by the NDF_ system, otherwise there is
*     the possibility of serious internal errors and data corruption.

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     29-OCT-1990 (RFWS):
*        Original version.
*     14-OCT-1991 (RFWS):
*        Corrected typo in prologue.
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
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) MODE

*  Arguments Returned:
      CHARACTER * ( * ) LOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( NDF__SZMOD ) VMODE ! Validated access mode
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Set an initial invalid value for the locator.
      LOC = DAT__NOLOC

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Validate the requested access mode string.
      CALL NDF1_VMOD( MODE, VMODE, STATUS )

*  Check that the requested mode of access is available.
      CALL NDF1_CHMOD( IACB, VMODE, STATUS )

*  Obtain an index to the data object entry in the DCB and clone a
*  locator for it.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IDCB = ACB_IDCB( IACB )
         CALL DAT_CLONE( DCB_LOC( IDCB ), LOC, STATUS )
      END IF

*  If an error occurred, then return an invalid locator.
      IF ( STATUS .NE. SAI__OK ) THEN
         LOC = DAT__NOLOC

*  Report context information and call the error tracing routine.
         CALL ERR_REP( 'NDF_LOC_ERR',
     :   'NDF_LOC: Error obtaining an HDS locator for an NDF.',
     :   STATUS )
         CALL NDF1_TRACE( 'NDF_LOC', STATUS )
      END IF

      END
