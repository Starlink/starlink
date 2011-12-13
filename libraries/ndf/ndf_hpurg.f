      SUBROUTINE NDF_HPURG( INDF, IREC1, IREC2, STATUS )
*+
*  Name:
*     NDF_HPURG

*  Purpose:
*     Delete a range of records from an NDF history component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_HPURG( INDF, IREC1, IREC2, STATUS )

*  Description:
*     The routine deletes a specified range of records from an NDF
*     history component. The remaining records are re-numbered starting
*     from 1.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     IREC1 = INTEGER (Given)
*        Number of the first history record to be deleted.
*     IREC2 = INTEGER (Given)
*        Number of the last history record to be deleted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine is provided primarily to allow a lengthy NDF
*     history to be truncated in order to save space. To avoid
*     deceiving subsequent readers, it is normally advisable not to
*     delete arbitrary sections of an NDF's history, but to delete only
*     the earliest part (by setting IREC1 to 1).
*     -  The IREC1 and IREC2 arguments must both identify valid history
*     records which are actually present. Their values may be
*     interchanged without affecting the behaviour of this routine.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     DSB: David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     10-MAY-1993 (RFWS):
*        Original version.
*     12-MAY-1993 (RFWS):
*        Reset DCB entries if the current history record is deleted.
*     4-AUG-1993 (RFWS):
*        Remove any junk stored in the history records array before
*        attempting to truncate it.
*     13-AUG-1993 (RFWS):
*        Improved the cleaning up of junk left at the end of the history
*        records array.
*     7-SEP-1993 (RFWS):
*        Fixed bug - failure to annul cell locators. Also improved
*        status checking and the emptying of unused cells (avoid
*        emptying any cell twice).
*     27-SEP-1993 (RFWS):
*        Improved error messages.
*     23-JAN-2009 (DSB):
*        Added DCB_HTIME.
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
*        DCB_HDEF( NDF__MXDCB ) = LOGICAL (Write)
*           Whether default history information is to be written.
*        DCB_HEXT( NDF__MXDCB ) = INTEGER (Read)
*           Extension increment for the history records array.
*        DCB_HLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator for NDF history component.
*        DCB_HNREC( NDF__MXDCB ) = INTEGER (Read and Write)
*           Number of valid history records present.
*        DCB_HRLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator for array of history records.
*        DCB_HTIME( NDF__MXDCB ) = DOUBLE PRECISION (Write)
*           The date/time to attach to the next history record to be
*           created, as a UTC Modified Julian Date. If negative, then
*           the current time will be used.
*        DCB_HTLEN( NDF__MXDCB ) = LOGICAL (Write)
*           Current history record text length.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER INDF
      INTEGER IREC1
      INTEGER IREC2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) CELL1 ! Locator for source history cell
      CHARACTER * ( DAT__SZLOC ) CELL2 ! Locator for destination cell
      CHARACTER * ( DAT__SZLOC ) LOC ! Component locator
      CHARACTER * ( DAT__SZTYP ) NAME ! Component name
      INTEGER DIM( 1 )           ! New array size
      INTEGER I                  ! Loop counter for array cells
      INTEGER I1                 ! First record number
      INTEGER I2                 ! Last record number
      INTEGER IACB               ! Index to the NDF entry in the ACB
      INTEGER ICOMP              ! Loop counter for structure components
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER MXREC              ! Size of history records array
      INTEGER NCOMP              ! Number of structure components
      INTEGER NLAST              ! Old history record count
      INTEGER NREC               ! Number of history records remaining
      INTEGER SUB( 1 )           ! Array subscript for finding cells

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that WRITE access to the NDF is available.
         CALL NDF1_CHACC( IACB, 'WRITE', STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain an index to the data object entry in the DCB and ensure that
*  DCB history information is available.
            IDCB = ACB_IDCB( IACB )
            CALL NDF1_DH( IDCB, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Find the first and last history records to be deleted.
               I1 = MIN( IREC1, IREC2 )
               I2 = MAX( IREC1, IREC2 )

*  Check that a history component is present and report an error if it
*  is not.
               IF ( DCB_HLOC( IDCB ) .EQ. DAT__NOLOC ) THEN
                  STATUS = NDF__NOHIS
                  CALL NDF1_DMSG( 'NDF', IDCB )
                  CALL ERR_REP( 'NDF_HPURG_NOHIS',
     :                          'There is no history component ' //
     :                          'present in the NDF structure ^NDF ' //
     :                          '(possible programming error).',
     :                          STATUS )

*  Check that the first record number is at least 1 and report an error
*  if it is not.
               ELSE IF ( I1 .LT. 1 ) THEN
                  STATUS = NDF__HRNIN
                  CALL MSG_SETI( 'BADREC', I1 )
                  CALL ERR_REP( 'NDF_HPURG_I1',
     :                          'Invalid history record number ' //
     :                          '^BADREC specified; values smaller ' //
     :                          'then 1 are not allowed (possible ' //
     :                          'programming error).', STATUS )

*  Also check that the last record number does not exceed the number of
*  history records actually present. Report an error if it does.
               ELSE IF ( I2 .GT. DCB_HNREC( IDCB ) ) THEN
                  STATUS = NDF__HRNIN
                  CALL MSG_SETI( 'BADREC', I2 )
                  CALL MSG_SETI( 'NREC', DCB_HNREC( IDCB ) )
                  CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )

*  Adjust the error message according to how many records are actually
*  present.
                  IF ( DCB_HNREC( IDCB ) .EQ. 0 ) THEN
                     CALL ERR_REP( 'NDF_HPURG_I2',
     :                             'Invalid history record number ' //
     :                             '^BADREC specified; there are no ' //
     :                             'history records present in ' //
     :                             'the NDF history structure ^HIST ' //
     :                             '(possible programming error).',
     :                             STATUS )
                  ELSE IF ( DCB_HNREC( IDCB ) .EQ. 1 ) THEN
                     CALL ERR_REP( 'NDF_HPURG_I2',
     :                             'Invalid history record number ' //
     :                             '^BADREC specified; there is ' //
     :                             'only 1 history record present ' //
     :                             'in the NDF history structure ' //
     :                             '^HIST (possible programming ' //
     :                             'error).', STATUS )
                  ELSE
                     CALL ERR_REP( 'NDF_HPURG_I2',
     :                             'Invalid history record number ' //
     :                             '^BADREC specified; there are ' //
     :                             'only ^NREC history records ' //
     :                             'present in the NDF history ' //
     :                             'structure ^HIST (possible ' //
     :                             'programming error).', STATUS )
                  END IF

*  If OK, loop to move all those history records which occur after the
*  deleted section into earlier elements of the history record array.
               ELSE
                  DO 3 I = I2 + 1, DCB_HNREC( IDCB )

*  Obtain a locator for the record array cell to be moved.
                     SUB( 1 ) = I
                     CALL DAT_CELL( DCB_HRLOC( IDCB ), 1, SUB, CELL1,
     :                              STATUS )

*  Obtain a locator for the destination record array cell.
                     SUB( 1 ) = I1 + I - I2 - 1
                     CALL DAT_CELL( DCB_HRLOC( IDCB ), 1, SUB, CELL2,
     :                              STATUS )

*  Erase all existing components in the destination cell.
                     CALL NDF1_HRST( CELL2, STATUS )

*  Determine how many components exist in the cell to be moved and loop
*  to move each in turn.
                     CALL DAT_NCOMP( CELL1, NCOMP, STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        DO 1 ICOMP = 1, NCOMP

*  Repeatedly obtain a locator to the first component, determine its
*  name and move it to the new cell. This process eventually leaves the
*  original cell empty.
                           CALL DAT_INDEX( CELL1, 1, LOC, STATUS )
                           CALL DAT_NAME( LOC, NAME, STATUS )
                           CALL DAT_MOVE( LOC, CELL2, NAME, STATUS )

*  Quit looping if an error occurs.
                           IF ( STATUS .NE. SAI__OK ) GO TO 2
 1                      CONTINUE
 2                      CONTINUE
                     END IF

*  Annul the cell locators.
                     CALL DAT_ANNUL( CELL1, STATUS )
                     CALL DAT_ANNUL( CELL2, STATUS )
 3                CONTINUE
 4                CONTINUE

*  If the current history record has been deleted, then reset the
*  default history writing flag and current record text length to their
*  initial values. Also clear any user-supplied date-stamp.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     IF ( I2 .GE. DCB_HNREC( IDCB ) ) THEN
                        DCB_HDEF( IDCB ) = .TRUE.
                        DCB_HTLEN( IDCB ) = 0
                        DCB_HTIME( IDCB ) = -1.0D0
                     END IF
                  END IF

*  Determine how many valid history records now remain and retain the
*  original record count.
                  NREC = DCB_HNREC( IDCB ) - ( I2 - I1 + 1 )
                  NLAST = DCB_HNREC( IDCB )

*  Update the number of records remaining, both in the data object and
*  in the DCB.
                  CALL CMP_PUT0I( DCB_HLOC( IDCB ), 'CURRENT_RECORD',
     :                            NREC, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) DCB_HNREC( IDCB ) = NREC

*  Loop to obtain a locator to each cell in the history record array
*  which still contains information which is no longer required (we
*  exclude those cells from which components were copied, since the
*  copying operation will have emptied them).
                  DO 5 I = NREC + 1, I2
                     SUB( 1 ) = I
                     CALL DAT_CELL( DCB_HRLOC( IDCB ), 1, SUB, CELL2,
     :                              STATUS )

*  Delete all components stored in these cells.
                     CALL NDF1_HRST( CELL2, STATUS )
                     CALL DAT_ANNUL( CELL2, STATUS )
                     IF ( STATUS .NE. SAI__OK ) GO TO 6
 5                CONTINUE
 6                CONTINUE

*  Determine the size of the history record array.
                  CALL DAT_SIZE( DCB_HRLOC( IDCB ), MXREC, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  If the number of elements left at the end of the array exceeds the
*  array extend size, then the array must be truncated. Loop to obtain
*  a locator to each cell which is to be discarded and which has not
*  already been emptied.
                     IF ( ( MXREC - NREC ) .GT. DCB_HEXT( IDCB ) ) THEN
                        DO 7 I = MAX( NREC + DCB_HEXT( IDCB ) + 1,
     :                                I2 + 1 ), MXREC
                           SUB( 1 ) = I
                           CALL DAT_CELL( DCB_HRLOC( IDCB ), 1, SUB,
     :                                    CELL2, STATUS )

*  Delete all components stored in these cells. This is necessary to
*  stop any junk which may remain from preventing truncation of the
*  array.
                           CALL NDF1_HRST( CELL2, STATUS )
                           CALL DAT_ANNUL( CELL2, STATUS )
                           IF ( STATUS .NE. SAI__OK ) GO TO 8
 7                      CONTINUE
 8                      CONTINUE

*  Truncate the array, leaving one extend size worth of free elements
*  for future expansion.
                        DIM( 1 ) = NREC + DCB_HEXT( IDCB )
                        CALL DAT_ALTER( DCB_HRLOC( IDCB ), 1, DIM,
     :                                  STATUS )
                     END IF
                  END IF
               END IF
            END IF
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_HPURG_ERR',
     :   'NDF_HPURG: Error deleting records from an NDF history ' //
     :   'component.', STATUS )
         CALL NDF1_TRACE( 'NDF_HPURG', STATUS )
      END IF

      END
