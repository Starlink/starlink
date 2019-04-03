      SUBROUTINE NDF1_HSRT( IDCB, NREC, WORK1, WORK2, STATUS )
*+
*  Name:
*     NDF1_HSRT

*  Purpose:
*     Sort the history component of an NDF into chronological order.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_HSRT( IDCB, NREC, WORK1, WORK2, STATUS )

*  Description:
*     This routine ensures that the records in the NDFs history component
*     are stored in chronological order. It replaces the current RECORDS
*     array with a new, sorted array.

*  Arguments:
*     IDCB = INTEGER (Given)
*        DCB index identifying the NDF whose history is to be modified.
*     NREC = INTEGER (Given)
*        The number of records to be sorted.
*     WORK1( NREC ) = DOUBLE PRECISION (Returned)
*        Work space.
*     WORK2( NREC ) = INTEGER (Returned)
*        Work space.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
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
*     DSB: David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     23-JAN-2009 (DSB):
*        Original version.
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

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_HLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator for NDF history component.
*        DCB_HNREC( NDF__MXDCB ) = INTEGER (Write)
*           Number of valid history records present.
*        DCB_HRLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read & Write)
*           Locator for array of history records.

*  Arguments Given:
      INTEGER IDCB
      INTEGER NREC

*  Arguments Returned:
      DOUBLE PRECISION WORK1( NREC )
      INTEGER WORK2( NREC )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) CELL ! Locator for array cell
      CHARACTER * ( DAT__SZLOC ) CELL1! Locator for destination cell
      CHARACTER * ( DAT__SZLOC ) CELL2! Locator for source cell
      CHARACTER * ( DAT__SZLOC ) LOC  ! Temporary locator
      CHARACTER * ( DAT__SZLOC ) NEWLOC ! Locator for new records array
      CHARACTER * ( DAT__SZLOC ) TLOC ! Locator for new text lines
      CHARACTER * ( NDF__SZHDT ) DATE ! Date string
      DOUBLE PRECISION MJD       ! Date converted to an MJD
      INTEGER DIM( 1 )           ! Object dimension size
      INTEGER I                  ! Cell index
      INTEGER J                  ! Cell index
      INTEGER LAST               ! Index of last element to be tested
      INTEGER SUB( 1 )           ! Array subscript
      LOGICAL REORD              ! Was the original array out of order?
      LOGICAL SORTED             ! Are the work arrays now sorted?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that history information is available in the DCB.
      CALL NDF1_DH( IDCB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Check if a history component is present, otherwise there is nothing
*  more to do.
         IF ( DCB_HLOC( IDCB ) .NE. DAT__NOLOC ) THEN

*  Loop round all the history records. Get a locator to the record structure,
*  and read the formatted date/time string from it. Convert it to an MJD and
*  store in the first work array. Store its index in the second work array.
*  Annul the locator.
            DO I = 1, NREC
               SUB( 1 ) = I
               CALL DAT_CELL( DCB_HRLOC( IDCB ), 1, SUB, CELL, STATUS )
               CALL CMP_GET0C( CELL, 'DATE', DATE, STATUS )
               CALL NDF1_CHTIM( DATE, MJD, STATUS )
               WORK1( I ) = MJD
               WORK2( I ) = I
               CALL DAT_ANNUL( CELL, STATUS )
            END DO

*  Initialise a flag indicating that the records do not need to be re-ordered.
            REORD = .FALSE.

*  We do a bubble sort to sort the second work array containing the
*  record indices into chronological order. Loop until the array is sorted.
            LAST = NREC - 1
            SORTED = .FALSE.
            DO WHILE( .NOT. SORTED .AND. LAST .GT. 0 )

*  Loop round all the history records that have not yet been sorted. We
*  know that those with index higher than LAST are already sorted so we
*  can skip them. Assume, to begin with, that the array is now sorted.
               SORTED = .TRUE.
               DO I = 1, LAST

*  Compare records I and I+1. If necessary, swap them so that the date
*  associated with record I+1 is larger than the date associated with
*  record I. If the pair need swapping indicate that we have not yet
*  finished sorting the array. Also indicate that the records need
*  re-ordering.
                  IF( WORK1( WORK2( I ) ) .GT.
     :                WORK1( WORK2( I + 1 ) ) ) THEN
                     J = WORK2( I )
                     WORK2( I ) = WORK2( I + 1 )
                     WORK2( I + 1 ) = J
                     SORTED = .FALSE.
                     REORD = .TRUE.
                  END IF

               END DO

*  The above loop will have caused the largest remaining time to bubble
*  up to the end of the array, so we can decrement the number of records
*  remaining to be sorted.
               LAST = LAST - 1
            END DO

*  Nothing more to do unless the records array needs to be re-ordered.
            IF( REORD .AND. STATUS .EQ. SAI__OK ) THEN


*  Create a new records array of the required length.
               DIM( 1 ) = NREC
               CALL DAT_NEW( DCB_HLOC( IDCB ), 'NEW_RECORDS',
     :                       'HIST_REC', 1, DIM, STATUS )
               CALL DAT_FIND( DCB_HLOC( IDCB ), 'NEW_RECORDS',
     :                        NEWLOC, STATUS )

*  Copy all items from the old array to the new array, in the correct
*  order.
               DO I = 1, NREC
                  SUB( 1 ) = I
                  CALL DAT_CELL( NEWLOC, 1, SUB, CELL1, STATUS )
                  SUB( 1 ) = WORK2( I )
                  CALL DAT_CELL( DCB_HRLOC( IDCB ), 1, SUB, CELL2,
     :                           STATUS )
                  CALL NDF1_HCOPY( CELL2, CELL1, STATUS )
                  CALL DAT_ANNUL( CELL1, STATUS )
                  CALL DAT_ANNUL( CELL2, STATUS )
               END DO

*  Annul the old array locator and erase the array.
               CALL DAT_ANNUL( DCB_HRLOC( IDCB ), STATUS )
               CALL DAT_ERASE( DCB_HLOC( IDCB ), 'RECORDS', STATUS )

*  Rename the NEW_RECORDS array, and save its locator in the DCB.
               CALL DAT_RENAM( NEWLOC, 'RECORDS', STATUS )
               DCB_HRLOC( IDCB ) = NEWLOC

*  Store its size (it may have changed since there may have been some
*  empty cells at the end of it).
               DCB_HNREC( IDCB ) = NREC

            END IF

         END IF

      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_HSRT', STATUS )

      END
