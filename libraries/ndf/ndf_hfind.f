      SUBROUTINE NDF_HFIND( INDF, YMDHM, SEC, EQ, IREC, STATUS )
*+
*  Name:
*     NDF_HFIND

*  Purpose:
*     Find an NDF history record by date and time.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_HFIND( INDF, YMDHM, SEC, EQ, IREC, STATUS )

*  Description:
*     The routine searches the history component of an NDF to identify
*     the first history record which was written after a specified date
*     and time. The record number is returned. A value of zero is
*     returned if no suitable record exists.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     YMDHM( 5 ) = INTEGER (Given)
*        The year, month, day, hour and minute fields of the required
*        date and time, in that order, stored as integers (the month
*        field starts at 1 for January).
*     SEC = REAL (Given)
*        The seconds field of the required date and time.
*     EQ = LOGICAL (Given)
*        If a .TRUE. value is given for this argument, then a history
*        record whose date and time exactly matches that specified may
*        be returned. Otherwise, the record must have been written
*        strictly later than specified.
*     IREC = INTEGER (Returned)
*        The record number of the required history record, or zero if
*        no suitable record exists.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The last history record written before a specified date and time
*     may be found by subtracting 1 from the record number returned by
*     this routine (or using the final record if this routine returns
*     zero).

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
*     2-JUN-1993 (RFWS):
*        Original version.
*     3-JUN-1993 (RFWS):
*        Improved the error reporting if records are out of
*        chronological order.
*     4-AUG-1993 (RFWS):
*        Handle the case where a history component exists but does not
*        yet contain any records.
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
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.
*        DCB_HNREC( NDF__MXDCB ) = INTEGER (Read)
*           Number of valid history records present.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER INDF
      INTEGER YMDHM( 5 )
      REAL SEC
      LOGICAL EQ

*  Arguments Returned:
      INTEGER IREC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for date/time fields
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER INEW               ! Next record number to try
      INTEGER IREC1              ! Earlier best guess record number
      INTEGER IREC2              ! Later best guess record number
      INTEGER ORDER              ! Date/time order
      INTEGER Y( 5 )             ! Date/time fields of test record
      INTEGER YMDHM1( 5 )        ! Date/time fields, earlier best guess
      INTEGER YMDHM2( 5 )        ! Date/time fields, later best guess
      LOGICAL DONE               ! Found required record?
      REAL S                     ! Seconds field of test record
      REAL SEC1                  ! Seconds field of earlier best guess
      REAL SEC2                  ! Seconds field of later best guess

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier and validate the date/time specification.
      CALL NDF1_IMPID( INDF, IACB, STATUS )
      CALL NDF1_VDAT( YMDHM, SEC, STATUS )

*  If OK, obtain an index to the data object entry in the DCB and
*  ensure that DCB history information is available.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IDCB = ACB_IDCB( IACB )
         CALL NDF1_DH( IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that a history component is present and report an error if it
*  is not.
            IF ( DCB_HLOC( IDCB ) .EQ. DAT__NOLOC ) THEN
               STATUS = NDF__NOHIS
               CALL NDF1_DMSG( 'NDF', IDCB )
               CALL ERR_REP( 'NDF_HFIND_NOHIS',
     :                       'There is no history component present ' //
     :                       'in the NDF structure ^NDF (possible ' //
     :                       'programming error).', STATUS )

*  Check whether there are currently any records in the history
*  component. If not, then return a record number of zero.
            ELSE IF ( DCB_HNREC( IDCB ) .EQ. 0 ) THEN
               IREC = 0

*  Otherwise, obtain the date/time of the first history record and
*  compare it with the date/time supplied. If it is OK, then note that
*  there is nothing more to do and set IREC to 1.
            ELSE
               DONE = .FALSE.
               CALL NDF1_GTHDT( IDCB, 1, YMDHM1, SEC1, STATUS )
               CALL NDF1_HTCMP( YMDHM, SEC, YMDHM1, SEC1, ORDER,
     :                          STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  DONE = ( ORDER .EQ. 1 ) .OR.
     :                   ( EQ .AND. ( ORDER .EQ. 0 ) )
                  IF ( DONE ) IREC = 1
               END IF

*  If the first record was not OK, then try the last record. If this is
*  not OK, then there is no record with the date/time requested, so
*  note there is nothing more to do and set IREC to zero.
               IF ( .NOT. DONE ) THEN
                  CALL NDF1_GTHDT( IDCB, DCB_HNREC( IDCB ), YMDHM2,
     :                             SEC2, STATUS )
                  CALL NDF1_HTCMP( YMDHM, SEC, YMDHM2, SEC2, ORDER,
     :                             STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     DONE = .NOT. ( ( ORDER .EQ. 1 ) .OR.
     :                              ( EQ .AND. ( ORDER .EQ. 0 ) ) )
                     IF ( DONE ) IREC = 0
                  END IF
               END IF

*  If the required record has not been located, then perform a binary
*  chop to identify it.
               IREC1 = 1
               IREC2 = DCB_HNREC( IDCB )
 1             CONTINUE          ! Start of 'DO WHILE' loop
               IF ( ( .NOT. DONE ) .AND. ( STATUS .EQ. SAI__OK ) ) THEN

*  Obtain the date/time of the record lying mid-way between the two
*  previous best guesses.
                  INEW = ( IREC1 + IREC2 ) / 2
                  CALL NDF1_GTHDT( IDCB, INEW, Y, S, STATUS )

*  Check that the new date/time does not lie outside the range defined
*  by the previous best guesses. If it does, then the history records
*  are not in chronological order, so report an error. First check
*  against the earlier best guess.
                  CALL NDF1_HTCMP( YMDHM1, SEC1, Y, S, ORDER, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     IF ( ORDER .EQ. -1 ) THEN
                        STATUS = NDF__HRORD
                        CALL MSG_SETI( 'IREC1', IREC1 )
                        CALL MSG_SETI( 'INEW', INEW )
                        CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )
                        CALL ERR_REP( 'NDF_HFIND_ORD1',
     :                                'Error detected in history ' //
     :                                'records ^IREC1 and ^INEW in ' //
     :                                'the NDF history structure ' //
     :                                '^HIST; these records appear ' //
     :                                'to be out of chronological ' //
     :                                'order.', STATUS )
                     END IF
                  END IF

*  Then check against the later best guess.
                  CALL NDF1_HTCMP( Y, S, YMDHM2, SEC2, ORDER, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     IF ( ORDER .EQ. -1 ) THEN
                        STATUS = NDF__HRORD
                        CALL MSG_SETI( 'INEW', INEW )
                        CALL MSG_SETI( 'IREC2', IREC2 )
                        CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )
                        CALL ERR_REP( 'NDF_HFIND_ORD2',
     :                                'Error detected in history ' //
     :                                'records ^INEW and ^IREC2 in ' //
     :                                'the NDF history structure ' //
     :                                '^HIST; these records appear ' //
     :                                'to be out of chronological ' //
     :                                'order.', STATUS )
                     END IF
                  END IF

*  See if the new date/time is OK.
                  CALL NDF1_HTCMP( YMDHM, SEC, Y, S, ORDER, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  If so, then update the later best guess and its associated
*  date/time.
                     IF ( ( ORDER .EQ. 1 ) .OR.
     :                    ( EQ .AND. ( ORDER .EQ. 0 ) ) ) THEN
                        IREC2 = INEW
                        DO 2 I = 1, 5
                           YMDHM2( I ) = Y( I )
 2                      CONTINUE
                        SEC2 = S

*  Otherwise, update the earlier best guess.
                     ELSE
                        IREC1 = INEW
                        DO 3 I = 1, 5
                           YMDHM1( I ) = Y( I )
 3                      CONTINUE
                        SEC1 = S
                     END IF

*  Note if the required record has been found. This will be so if the
*  two best guesses are now adjacent. When found, return the required
*  record number.
                     DONE = ( ( IREC1 + 1 ) .GE. IREC2 )
                     IF ( DONE ) IREC = IREC2
                     GO TO 1
                  END IF
               END IF
            END IF
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_HFIND_ERR',
     :   'NDF_HFIND: Error finding an NDF history record by date ' //
     :   'and time.', STATUS )
         CALL NDF1_TRACE( 'NDF_HFIND', STATUS )
      END IF

      END
