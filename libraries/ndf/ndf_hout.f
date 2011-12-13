      SUBROUTINE NDF_HOUT( INDF, IREC, ROUTIN, STATUS )
*+
*  Name:
*     NDF_HOUT

*  Purpose:
*     Display text from an NDF history record.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_HOUT( INDF, IREC, ROUTIN, STATUS )

*  Description:
*     The routine displays the text associated with a specified NDF
*     history record by invoking a service routine suppled by the
*     caller. A standard service routine NDF_HECHO is provided, but
*     this may be replaced if required.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     IREC = INTEGER (Given)
*        Number of the NDF history record whose text is to be
*        displayed.
*     ROUTIN = SUBROUTINE (Given)
*        A service routine to which the text will be passed for
*        display. For a specification of this routine, see the default
*        routine NDF_HECHO. This service routine must be declared
*        EXTERNAL in the routine which invokes NDF_HOUT.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     11-MAY-1993 (RFWS):
*        Original version.
*     28-SEP-1993 (RFWS):
*        Improved error messages.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_HLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator for NDF history component.
*        DCB_HNREC( NDF__MXDCB ) = INTEGER (Read)
*           Number of valid history records present.
*        DCB_HRLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator for array of history records.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER INDF
      INTEGER IREC
      EXTERNAL ROUTIN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) CELL ! Array cell locator
      CHARACTER * ( DAT__SZLOC ) LOC ! Component locator
      CHARACTER * ( DAT__SZTYP ) TYPE ! Component data type string
      INTEGER CLEN               ! Character string length
      INTEGER DIM( DAT__MXDIM )  ! Component dimension sizes
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER NDIM               ! Number of component dimensions
      INTEGER PNTR               ! Pointer to mapped character array
      INTEGER SUB( 1 )           ! Cell subscript
      LOGICAL THERE              ! Is component present?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  If OK, obtain an index to the data object entry in the DCB.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IDCB = ACB_IDCB( IACB )

*  Ensure that history structure information is available in the DCB.
         CALL NDF1_DH( IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that there is a history component present. Report an error if
*  there is not.
            IF ( DCB_HLOC( IDCB ) .EQ. DAT__NOLOC ) THEN
               STATUS = NDF__NOHIS
               CALL NDF1_DMSG( 'NDF', IDCB )
               CALL ERR_REP( 'NDF_HOUT_NOHIS',
     :                       'There is no history component present ' //
     :                       'in the NDF structure ^NDF (possible ' //
     :                       'programming error).', STATUS )

*  Check that the history record number specified is greater than zero
*  and report an error if it is not.
            ELSE
               IF ( IREC .LT. 1 ) THEN
                  STATUS = NDF__HRNIN
                  CALL MSG_SETI( 'BADREC', IREC )
                  CALL ERR_REP( 'NDF_HOUT_IREC1',
     :                          'Invalid history record number ' //
     :                          '^BADREC specified; it should be ' //
     :                          'greater than zero (possible ' //
     :                          'programming error).', STATUS )

*  Also check that the record number does not exceed the number of
*  history records actually present and report an error if it does.
               ELSE IF ( IREC .GT. DCB_HNREC( IDCB ) ) THEN
                  STATUS = NDF__HRNIN
                  CALL MSG_SETI( 'BADREC', IREC )
                  CALL MSG_SETI( 'NREC', DCB_HNREC( IDCB ) )
                  CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )

*  Adjust the error message according to how many records are actually
*  present.
                  IF ( DCB_HNREC( IDCB ) .EQ. 0 ) THEN
                     CALL ERR_REP( 'NDF_HOUT_IREC',
     :                             'Invalid history record number ' //
     :                             '^BADREC specified; there are no ' //
     :                             'history records present in ' //
     :                             'the NDF history structure ^HIST ' //
     :                             '(possible programming error).',
     :                             STATUS )
                  ELSE IF ( DCB_HNREC( IDCB ) .EQ. 1 ) THEN
                     CALL ERR_REP( 'NDF_HOUT_IREC',
     :                             'Invalid history record number ' //
     :                             '^BADREC specified; there is ' //
     :                             'only 1 history record present ' //
     :                             'in the NDF history structure ' //
     :                             '^HIST (possible programming ' //
     :                             'error).', STATUS )
                  ELSE
                     CALL ERR_REP( 'NDF_HOUT_IREC',
     :                             'Invalid history record number ' //
     :                             '^BADREC specified; there are ' //
     :                             'only ^NREC history records ' //
     :                             'present in the NDF history ' //
     :                             'structure ^HIST (possible ' //
     :                             'programming error).', STATUS )
                  END IF

*  If OK, then select the requested record by locating its cell in the
*  history record structure array.
               ELSE
                  SUB( 1 ) = IREC
                  CALL DAT_CELL( DCB_HRLOC( IDCB ), 1, SUB, CELL,
     :                           STATUS )

*  Check that the mandatory TEXT component is present in this cell.
*  Report an error if it is not.
                  CALL DAT_THERE( CELL, 'TEXT', THERE, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     IF ( .NOT. THERE ) THEN
                        STATUS = NDF__NOHTX
                        CALL DAT_MSG( 'STRUCT', CELL )
                        CALL ERR_REP( 'NDF_HOUT_TEXT',
     :                                'The TEXT component is ' //
     :                                'missing from the NDF history ' //
     :                                'record structure ^STRUCT',
     :                                STATUS )

*  If OK, obtain a locator to the component and determine its type and
*  shape.
                     ELSE
                        CALL DAT_FIND( CELL, 'TEXT', LOC, STATUS )
                        CALL DAT_TYPE( LOC, TYPE, STATUS )
                        CALL DAT_SHAPE( LOC, DAT__MXDIM, DIM, NDIM,
     :                                  STATUS )

*  Check that the TEXT component is of type '_CHAR' and report an error
*  if it is not.
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           IF ( TYPE( : 6 ) .NE. '_CHAR*' ) THEN
                              STATUS = NDF__TYPIN
                              CALL DAT_MSG( 'STRUC', CELL )
                              CALL MSG_SETC( 'BADTYPE', TYPE )
                              CALL ERR_REP( 'NDF1_HOUT_TYPE',
     :                                      'The TEXT component in ' //
     :                                      'the NDF history record ' //
     :                                      'structure ^STRUC has ' //
     :                                      'an invalid type of ' //
     :                                      '''^BADTYPE''; it ' //
     :                                      'should be of type ' //
     :                                      '''_CHAR''.', STATUS )

*  Also check that the TEXT component is 1-dimensional and report an
*  error if it is not.
                           ELSE IF ( NDIM .NE. 1 ) THEN
                              STATUS = NDF__NDMIN
                              CALL DAT_MSG( 'STRUC', CELL )
                              CALL MSG_SETI( 'BADNDIM', NDIM )
                              CALL ERR_REP( 'NDF1_HOUT_NDIM',
     :                                      'The TEXT component in ' //
     :                                      'the NDF history record ' //
     :                                      'structure ^STRUC is ' //
     :                                      '^BADNDIM-dimensional; ' //
     :                                      'it should be ' //
     :                                      '1-dimensional.', STATUS )
                           END IF
                        END IF

*  Map the TEXT component for reading and determine its character
*  string length.
                        CALL DAT_MAPC( LOC, 'READ', NDIM, DIM, PNTR,
     :                                 STATUS )
                        CALL DAT_CLEN( LOC, CLEN, STATUS )

*  Pass the mapped array to the service routine. Report a contextual
*  error if this fails.
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           CALL ROUTIN( DIM( 1 ),
     :                                  %VAL( CNF_PVAL( PNTR ) ),
     :                                  STATUS,
     :                                  %VAL( CNF_CVAL( CLEN ) ) )
                           IF ( STATUS .NE. SAI__OK ) THEN
                              CALL ERR_REP( 'NDF_HOUT_SERV',
     :                                      'Error status set by ' //
     :                                      'service routine.', STATUS )
                           END IF
                        END IF

*  Annul the TEXT component locator (thus unmapping it).
                        CALL DAT_ANNUL( LOC, STATUS )
                     END IF
                  END IF

*  Annul the history record cell locator.
                  CALL DAT_ANNUL( CELL, STATUS )
               END IF
            END IF
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_HOUT_ERR',
     :   'NDF_HOUT: Error displaying text from an NDF history record.',
     :                 STATUS )
         CALL NDF1_TRACE( 'NDF_HOUT', STATUS )
      END IF

      END
