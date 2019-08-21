      SUBROUTINE NDF1_HINCR( IDCB, STATUS )
*+
*  Name:
*     NDF1_HINCR

*  Purpose:
*     Increment the history record for an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_HINCR( IDCB, STATUS )

*  Description:
*     The routine opens a new record in the history structure for an
*     NDF, ready to receive new history information. If the history
*     record array is not large enough to contain a new record, it is
*     extended. The DCB current history record count is incremented to
*     identify the new record. This routine does not create any
*     components within the new record structure.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index to a DCB entry identifying the NDF whose history is to be
*        incremented.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The history component which this routine is to modify must exist
*     before the routine is called, and DCB information must exist
*     about it. This routine does not check for this itself.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     5-MAY-1993 (RFWS):
*        Original version.
*     11-MAY-1993 (RFWS):
*        Removed history structure creation.
*     {enter_further_changes_here}

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
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_HLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator for NDF history component.
*        DCB_HNREC( NDF__MXDCB ) = INTEGER (Read and Write)
*           Number of valid history records present.
*        DCB_HNTXT( NDF__MXDCB ) = INTEGER (Write)
*           Number of lines of text written to the current history
*           record.
*        DCB_HRLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator for array of history records.

*  Arguments Given:
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOC ! Component locator
      CHARACTER * ( DAT__SZTYP ) TYPE ! Component data type string
      INTEGER DIM( DAT__MXDIM )  ! Object dimension sizes
      INTEGER INC                ! Array extension increment
      INTEGER MXREC              ! Size of history records array
      INTEGER NDIM               ! Number of object dimensions
      LOGICAL THERE              ! Is component present?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the number of elements in the array of history records.
      CALL DAT_SIZE( DCB_HRLOC( IDCB ), MXREC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If there is insufficient room for another record, then extend the
*  array.
         IF ( MXREC .LT. ( DCB_HNREC( IDCB ) + 1 ) ) THEN
            DIM( 1 ) = MXREC + DCB_HEXT( IDCB )
            CALL DAT_ALTER( DCB_HRLOC( IDCB ), 1, DIM, STATUS )
         END IF

*  Increment the current record counter, both in the data structure and
*  in the DCB.
         CALL CMP_PUT0I( DCB_HLOC( IDCB ), 'CURRENT_RECORD',
     :                   DCB_HNREC( IDCB ) + 1, STATUS )
         IF ( STATUS .EQ. SAI__OK ) DCB_HNREC( IDCB ) =
     :                              DCB_HNREC( IDCB ) + 1
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_HINCR',
     :                                            STATUS )

      END
