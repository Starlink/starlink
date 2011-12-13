      SUBROUTINE NDF_XDEL( INDF, XNAME, STATUS )
*+
*  Name:
*     NDF_XDEL

*  Purpose:
*     Delete a specified NDF extension.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_XDEL( INDF, XNAME, STATUS )

*  Description:
*     The routine deletes a named extension in an NDF together with its
*     contents, if any. No error results if the specified extension
*     does not exist.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     XNAME = CHARACTER * ( * ) (Given)
*        Name of the extension to be deleted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Check that write access to the NDF is available.
*     -  Check the extension name.
*     -  Obtain an index to the data object entry in the DCB.
*     -  Ensure that extension information is available in the DCB.
*     -  See if an extension (MORE) structure exists, otherwise the
*     extension component cannot exist.
*     -  If it does, then see if the named component exists.
*     -  If it does, then erase it.
*     -  See how many extension components remain. If there are none,
*     then annul the extension (MORE) structure's DCB locator and erase
*     the structure.

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
*     20-SEP-1989 (RFWS):
*        Original version.
*     26-SEP-1989 (RFWS):
*        Changed so that no error results if the named extension does
*        not exist and finished the prologue.
*     2-OCT-1989 (RFWS):
*        Minor correction to prologue.
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
*        DCB_XLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Locator to extension (MORE) structure.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.


*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) XNAME

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER NCOMP              ! No. extension structure components
      LOGICAL THERE              ! Whether the extension exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Check that WRITE access to the NDF is available.
      CALL NDF1_CHACC( IACB, 'WRITE', STATUS )

*  Check the extension name.
      CALL NDF1_CHXNM( XNAME, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain an index to the data object in the DCB.
         IDCB = ACB_IDCB( IACB )

*  Ensure that extension information is available in the DCB.
         CALL NDF1_DX( IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Check there is an extension (MORE) structure, otherwise the
*  specified extension component cannot exist.
            IF ( DCB_XLOC( IDCB ) .NE. DAT__NOLOC ) THEN

*  Determine if the extension component exists.
               CALL DAT_THERE( DCB_XLOC( IDCB ), XNAME, THERE, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If it does, then erase it.
                  IF ( THERE ) THEN
                     CALL DAT_ERASE( DCB_XLOC( IDCB ), XNAME, STATUS )

*  See how many extension components remain.
                     CALL DAT_NCOMP( DCB_XLOC( IDCB ), NCOMP, STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  If there are none left, then annul the extension (MORE) structure
*  locator and erase the structure.
                        IF ( NCOMP .EQ. 0 ) THEN
                           CALL DAT_ANNUL( DCB_XLOC( IDCB ), STATUS )
                           CALL DAT_ERASE( DCB_LOC( IDCB ), 'MORE',
     :                                     STATUS )
                        END IF
                     END IF
                  END IF
               END IF
            END IF
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_XDEL_ERR',
     :   'NDF_XDEL: Error deleting a specified NDF extension.', STATUS )
         CALL NDF1_TRACE( 'NDF_XDEL', STATUS )
      END IF

      END
