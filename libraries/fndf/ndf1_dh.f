      SUBROUTINE NDF1_DH( IDCB, STATUS )
*+
*  Name:
*     NDF1_DH

*  Purpose:
*     Ensure that history information is available in the DCB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_DH( IDCB, STATUS )

*  Description:
*     The routine ensures that information about a data object's
*     history component is available in the DCB. It does nothing if
*     this information is already available. Otherwise, it obtains this
*     information by inspecting the actual data object, performing
*     necessary validation checks in the process.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index to the DCB entry for which history information is
*        required.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     5-MAY-1993 (RFWS):
*        Original version.
*     7-MAY-1993 (RFWS):
*        Added checks on the EXTEND_SIZE history component.
*     18-MAY-1993 (RFWS):
*        Added support for the UPDATE_MODE history component.
*     4-AUG-1993 (RFWS):
*        Chaged to ensure locators are annulled at the correct point
*        under error conditions.
*     5-AUG-2009 (TIMJ):
*        Remove the CNF_PVAL code and simplify by reading the UPDATE_MODE
*        and VARIANTS into local buffers rather than mapping them.
*        Strip leading space from history update mode on read. At some
*        point two leading spaces were being added.
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
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_HEXT( NDF__MXDCB ) = INTEGER (Write)
*           Extension increment for the history records array.
*        DCB_HLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Locator for NDF history component.
*        DCB_HNREC( NDF__MXDCB ) = INTEGER (Write)
*           Number of valid history records present.
*        DCB_HRLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Locator for array of history records.
*        DCB_HUMOD( NDF__MXDCB ) = INTEGER (Write)
*           History recording update mode.
*        DCB_KH( NDF__MXDCB ) = LOGICAL Read and Write)
*           Whether DCB information is available for the NDF's history
*           component.
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.

*  Arguments Given:
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Local Variables:
      CHARACTER * ( 128 ) BUFFER ! Buffer for temporary strings
      CHARACTER * ( DAT__SZLOC ) LOC ! Component locator
      CHARACTER * ( DAT__SZTYP ) TYPE ! Component data type
      INTEGER DIM( DAT__MXDIM )  ! Component dimension sizes
      INTEGER MXREC              ! Size of the RECORDS array
      INTEGER NDIM               ! Number of component dimensions
      LOGICAL THERE              ! Is component present?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if history information is already available. There is nothing to
*  do if it is.
      IF ( .NOT. DCB_KH( IDCB ) ) THEN

*  HISTORY structure.
*  =================
*  See if a history component is present in the NDF structure.
         CALL DAT_THERE( DCB_LOC( IDCB ), 'HISTORY', THERE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Initialise locators for the history structure and the array of
*  history records it contains.
            DCB_HLOC( IDCB ) = DAT__NOLOC
            DCB_HRLOC( IDCB ) = DAT__NOLOC

*  If a history component is present, obtain a locator for it and
*  determine its type and shape.
            IF ( THERE ) THEN
               CALL DAT_FIND( DCB_LOC( IDCB ), 'HISTORY',
     :                        DCB_HLOC( IDCB ), STATUS )
               CALL DAT_TYPE( DCB_HLOC( IDCB ), TYPE, STATUS )
               CALL DAT_SHAPE( DCB_HLOC( IDCB ), DAT__MXDIM, DIM, NDIM,
     :                         STATUS )

*  Check that the structure is of type 'HISTORY' and report an error if
*  it is not.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( TYPE .NE. 'HISTORY' ) THEN
                     STATUS = NDF__TYPIN
                     CALL NDF1_DMSG( 'NDF', IDCB )
                     CALL MSG_SETC( 'BADTYPE', TYPE )
                     CALL ERR_REP( 'NDF1_DH_HTYPE',
     :                             'The HISTORY component in the ' //
     :                             'NDF structure ^NDF has an ' //
     :                             'invalid type of ''^BADTYPE''; ' //
     :                             'it should be of type ''HISTORY''.',
     :                             STATUS )

*  Also check that the structure is scalar and report an error if it is
*  not.
                  ELSE IF ( NDIM .NE. 0 ) THEN
                     STATUS = NDF__NDMIN
                     CALL NDF1_DMSG( 'NDF', IDCB )
                     CALL MSG_SETI( 'BADNDIM', NDIM )
                     CALL ERR_REP( 'NDF1_DH_HNDIM',
     :                             'The HISTORY component in the ' //
     :                             'NDF structure ^NDF is ' //
     :                             '^BADNDIM-dimensional; it should ' //
     :                             'be scalar.', STATUS )
                  END IF
               END IF

*  VARIANT component.
*  =================
*  If a history structure has been found, see if it contains the
*  optional VARIANT component.
               CALL DAT_THERE( DCB_HLOC( IDCB ), 'VARIANT', THERE,
     :                         STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If so, obtain a locator for it and determine its type and shape.
                  IF ( THERE ) THEN
                     CALL DAT_FIND( DCB_HLOC( IDCB ), 'VARIANT', LOC,
     :                              STATUS )
                     CALL DAT_TYPE( LOC, TYPE, STATUS )
                     CALL DAT_SHAPE( LOC, DAT__MXDIM, DIM, NDIM,
     :                               STATUS )

*  Check that the VARIANT component is of type _CHAR and report an
*  error if it is not.
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        IF ( TYPE( : 6 ) .NE. '_CHAR*' ) THEN
                           STATUS = NDF__TYPIN
                           CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )
                           CALL MSG_SETC( 'BADTYPE', TYPE )
                           CALL ERR_REP( 'NDF1_DH_VTYPE',
     :                                   'The VARIANT component in ' //
     :                                   'the NDF history structure ' //
     :                                   '^HIST has an invalid type ' //
     :                                   'of ''^BADTYPE''; it ' //
     :                                   'should be of type ''_CHAR''.',
     :                                   STATUS )

*  Also check that the VARIANT component is scalar and report an error
*  if it is not.
                        ELSE IF ( NDIM .NE. 0 ) THEN
                           STATUS = NDF__NDMIN
                           CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )
                           CALL MSG_SETI( 'BADNDIM', NDIM )
                           CALL ERR_REP( 'NDF1_DH_VNDIM',
     :                                   'The VARIANT component in ' //
     :                                   'the NDF history structure ' //
     :                                   '^HIST is ' //
     :                                   '^BADNDIM-dimensional; it ' //
     :                                   'should be scalar.', STATUS )
                        END IF
                     END IF

*  Read the VARIANT component and determine its length.
                     CALL DAT_GET0C( LOC, BUFFER, STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  Test its value. Report an error if it is not 'SIMPLE'.
                        IF ( .NOT. CHR_SIMLR( BUFFER, 'SIMPLE' ) ) THEN
                           STATUS = NDF__VARIN
                           CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )
                           CALL NDF1_SETC( BUFFER, 'BADVAR' )
                           CALL ERR_REP( 'NDF1_DH_VAR',
     :                     'The VARIANT component in the NDF ' //
     :                     'history structure ^HIST has an invalid ' //
     :                     'value of ''^BADVAR''; only the value ' //
     :                     '''SIMPLE'' is defined.', STATUS )
                        END IF
                     END IF

*  Annul the VARIANT component locator.
                     CALL DAT_ANNUL( LOC, STATUS )
                  END IF
               END IF

*  CREATED component.
*  =================
*  See if the history structure contains the mandatory CREATED
*  component (giving the date of history creation).
               CALL DAT_THERE( DCB_HLOC( IDCB ), 'CREATED', THERE,
     :                         STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If it does not, then report an error.
                  IF ( .NOT. THERE ) THEN
                     STATUS = NDF__NOHCD
                     CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )
                     CALL ERR_REP( 'NDF1_DH_NOCRE',
     :                             'The CREATED component is ' //
     :                             'missing from the NDF history ' //
     :                             'structure ^HIST', STATUS )

*  Otherwise, obtain a locator for it and determine its type and shape.
                  ELSE
                     CALL DAT_FIND( DCB_HLOC( IDCB ), 'CREATED', LOC,
     :                              STATUS )
                     CALL DAT_TYPE( LOC, TYPE, STATUS )
                     CALL DAT_SHAPE( LOC, DAT__MXDIM, DIM, NDIM,
     :                               STATUS )

*  Check that the CREATED component has type _CHAR and report an error
*  if it does not.
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        IF ( TYPE( : 6 ) .NE. '_CHAR*' ) THEN
                           STATUS = NDF__TYPIN
                           CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )
                           CALL MSG_SETC( 'BADTYPE', TYPE )
                           CALL ERR_REP( 'NDF1_DH_CTYPE',
     :                                   'The CREATED component in ' //
     :                                   'the NDF history structure ' //
     :                                   '^HIST has an invalid type ' //
     :                                   'of ''^BADTYPE''; it ' //
     :                                   'should be of type ''_CHAR''.',
     :                                   STATUS )

*  Also check that the CREATED component is scalar and report an error
*  if it is not.
                        ELSE IF ( NDIM .NE. 0 ) THEN
                           STATUS = NDF__NDMIN
                           CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )
                           CALL MSG_SETI( 'BADNDIM', NDIM )
                           CALL ERR_REP( 'NDF1_DH_CNDIM',
     :                                   'The CREATED component in ' //
     :                                   'the NDF history structure ' //
     :                                   '^HIST is ' //
     :                                   '^BADNDIM-dimensional; it ' //
     :                                   'should be scalar.', STATUS )
                        END IF
                     END IF

*  Annul the CREATED component locator.
                     CALL DAT_ANNUL( LOC, STATUS )
                  END IF
               END IF

*  RECORDS component.
*  =================
*  See if the history structure contains the mandatory RECORDS
*  component (the array of history records).
               CALL DAT_THERE( DCB_HLOC( IDCB ), 'RECORDS', THERE,
     :                         STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If not, then report an error.
                  IF ( .NOT. THERE ) THEN
                     STATUS = NDF__NOHRA
                     CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )
                     CALL ERR_REP( 'NDF1_DH_NOREC',
     :                             'The RECORDS component is ' //
     :                             'missing from the NDF history ' //
     :                             'structure ^HIST', STATUS )

*  Otherwise, obtain a locator for it, storing the locator in the DCB.
*  Determine the component's type and shape.
                  ELSE
                     CALL DAT_FIND( DCB_HLOC( IDCB ), 'RECORDS',
     :                              DCB_HRLOC( IDCB ), STATUS )
                     CALL DAT_TYPE( DCB_HRLOC( IDCB ), TYPE, STATUS )
                     CALL DAT_SHAPE( DCB_HRLOC( IDCB ), DAT__MXDIM,
     :                               DIM, NDIM, STATUS )

*  Check that the RECORDS component has type 'HIST_REC' and report an
*  error if it does not.
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        IF ( TYPE .NE. 'HIST_REC' ) THEN
                           STATUS = NDF__TYPIN
                           CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )
                           CALL MSG_SETC( 'BADTYPE', TYPE )
                           CALL ERR_REP( 'NDF1_DH_RTYPE',
     :                                   'The RECORDS component in ' //
     :                                   'the NDF history structure ' //
     :                                   '^HIST has an invalid type ' //
     :                                   'of ''^BADTYPE''; it ' //
     :                                   'should be of type ' //
     :                                   '''HIST_REC''.', STATUS )

*  Also check that the RECORDS component is a 1-dimensional array and
*  report an error if it is not.
                        ELSE IF ( NDIM .NE. 1 ) THEN
                           STATUS = NDF__NDMIN
                           CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )
                           CALL MSG_SETI( 'BADNDIM', NDIM )
                           CALL ERR_REP( 'NDF1_DH_RNDIM',
     :                                   'The RECORDS component in ' //
     :                                   'the NDF history structure ' //
     :                                   '^HIST is ' //
     :                                   '^BADNDIM-dimensional; it ' //
     :                                   'should be 1-dimensional.',
     :                                   STATUS )
                        END IF

*  If OK, retain the array size.
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           MXREC = DIM( 1 )
                        END IF
                     END IF
                  END IF
               END IF

*  CURRENT_RECORD component.
*  ========================
*  See if the history structure contains the mandatory CURRENT_RECORD
*  component (giving the number of history records which contain
*  information).
               CALL DAT_THERE( DCB_HLOC( IDCB ), 'CURRENT_RECORD',
     :                         THERE, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If not, then report an error.
                  IF ( .NOT. THERE ) THEN
                     STATUS = NDF__NOHRC
                     CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )
                     CALL ERR_REP( 'NDF1_DH_NOCUR',
     :                             'The CURRENT_RECORD component is ' //
     :                             'missing from the NDF history ' //
     :                             'structure ^HIST', STATUS )

*  Otherwise, obtain a locator for it and determine its type and shape.
                  ELSE
                     CALL DAT_FIND( DCB_HLOC( IDCB ), 'CURRENT_RECORD',
     :                              LOC, STATUS )
                     CALL DAT_TYPE( LOC, TYPE, STATUS )
                     CALL DAT_SHAPE( LOC, DAT__MXDIM, DIM, NDIM,
     :                               STATUS )

*  Check that the CURRENT_RECORD component has type '_INTEGER' and
*  report an error if it does not.
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        IF ( TYPE .NE. '_INTEGER' ) THEN
                           STATUS = NDF__TYPIN
                           CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )
                           CALL MSG_SETC( 'BADTYPE', TYPE )
                           CALL ERR_REP( 'NDF1_DH_CRTYPE',
     :                                   'The CURRENT_RECORD ' //
     :                                   'component in the NDF ' //
     :                                   'history structure ^HIST ' //
     :                                   'has an invalid type of ' //
     :                                   '''^BADTYPE''; it should ' //
     :                                   'be of type ''_INTEGER''.',
     :                                   STATUS )

*  Also check that the CURRENT_RECORD component is scalar and report an
*  error if it is not.
                        ELSE IF ( NDIM .NE. 0 ) THEN
                           STATUS = NDF__NDMIN
                           CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )
                           CALL MSG_SETI( 'BADNDIM', NDIM )
                           CALL ERR_REP( 'NDF1_DH_CRNDIM',
     :                                   'The CURRENT_RECORD ' //
     :                                   'component in the NDF ' //
     :                                   'history structure ^HIST ' //
     :                                   'is ^BADNDIM-dimensional; ' //
     :                                   'it should be scalar.',
     :                                   STATUS )
                        END IF
                     END IF

*  Read the CURRENT_RECORD value.
                     CALL DAT_GET0I( LOC, DCB_HNREC( IDCB ), STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that the value is not negative and report an error if it is.
                        IF ( DCB_HNREC( IDCB ) .LT. 0 ) THEN
                           STATUS = NDF__HRCIN
                           CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )
                           CALL MSG_SETI( 'BADNREC', DCB_HNREC( IDCB ) )
                           CALL ERR_REP( 'NDF1_DH_NREC1',
     :                                   'The CURRENT_RECORD ' //
     :                                   'component in the NDF ' //
     :                                   'history structure ^HIST ' //
     :                                   'has an invalid value of ' //
     :                                   '^BADNREC; negative values ' //
     :                                   'are not allowed.', STATUS )

*  Also check that the value does not exceed the size of the RECORDS
*  array and report an error if it does.
                        ELSE IF ( DCB_HNREC( IDCB ) .GT. MXREC ) THEN
                           STATUS = NDF__HRCIN
                           CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )
                           CALL MSG_SETI( 'BADNREC', DCB_HNREC( IDCB ) )
                           CALL MSG_SETI( 'MXREC', MXREC )
                           CALL ERR_REP( 'NDF1_DH_NREC2',
     :                                   'The CURRENT_RECORD ' //
     :                                   'component in the NDF ' //
     :                                   'history structure ^HIST ' //
     :                                   'has an invalid value of ' //
     :                                   '^BADNREC; it should not ' //
     :                                   'exceed the size of the ' //
     :                                   'RECORDS component (^MXREC).',
     :                                   STATUS )
                        END IF
                     END IF

*  Annul the CURRENT_RECORD component locator.
                     CALL DAT_ANNUL( LOC, STATUS )
                  END IF
               END IF

*  UPDATE_MODE component.
*  =====================
*  See if the history structure contains the optional UPDATE_MODE
*  component (giving the degree of verbosity required when adding new
*  history information). Initially set the default value.
               CALL DAT_THERE( DCB_HLOC( IDCB ), 'UPDATE_MODE', THERE,
     :                         STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  DCB_HUMOD( IDCB ) = NDF__HNORM

*  If the component is present, obtain a locator for it and determine
*  its type and shape.
                  IF ( THERE ) THEN
                     CALL DAT_FIND( DCB_HLOC( IDCB ), 'UPDATE_MODE',
     :                              LOC, STATUS )
                     CALL DAT_TYPE( LOC, TYPE, STATUS )
                     CALL DAT_SHAPE( LOC, DAT__MXDIM, DIM, NDIM,
     :                               STATUS )

*  Check that the UPDATE_MODE component has type '_CHAR' and report an
*  error if it does not.
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        IF ( TYPE( : 6 ) .NE. '_CHAR*' ) THEN
                           STATUS = NDF__TYPIN
                           CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )
                           CALL MSG_SETC( 'BADTYPE', TYPE )
                           CALL ERR_REP( 'NDF1_DH_UTYPE',
     :                                   'The UPDATE_MODE ' //
     :                                   'component in the NDF ' //
     :                                   'history structure ^HIST ' //
     :                                   'has an invalid type of ' //
     :                                   '''^BADTYPE''; it should ' //
     :                                   'be of type ''_CHAR''.',
     :                                   STATUS )

*  Also check that the UPDATE_MODE component is scalar and report an
*  error if it is not.
                        ELSE IF ( NDIM .NE. 0 ) THEN
                           STATUS = NDF__NDMIN
                           CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )
                           CALL MSG_SETI( 'BADNDIM', NDIM )
                           CALL ERR_REP( 'NDF1_DH_UNDIM',
     :                                   'The UPDATE_MODE ' //
     :                                   'component in the NDF ' //
     :                                   'history structure ^HIST ' //
     :                                   'is ^BADNDIM-dimensional; ' //
     :                                   'it should be scalar.',
     :                                   STATUS )
                        END IF
                     END IF

*  Read the UPDATE_MODE component and determine its length.
                     CALL DAT_GET0C( LOC, BUFFER, STATUS )
                     CALL CHR_RMBLK( BUFFER )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  Check it against each recognised value in turn, setting the
*  appropriate update mode in the DCB.
                        IF ( CHR_SIMLR( BUFFER, 'DISABLED'  ) ) THEN
                           DCB_HUMOD( IDCB ) = NDF__HDISA
                        ELSE IF ( CHR_SIMLR( BUFFER, 'QUIET' ) ) THEN
                           DCB_HUMOD( IDCB ) = NDF__HQUIE
                        ELSE IF ( CHR_SIMLR( BUFFER, 'NORMAL' ) ) THEN
                           DCB_HUMOD( IDCB ) = NDF__HNORM
                        ELSE IF ( CHR_SIMLR( BUFFER, 'VERBOSE' ) ) THEN
                           DCB_HUMOD( IDCB ) = NDF__HVERB

*  If the UPDATE_MODE value was not recognised, then report an error.
                        ELSE
                           STATUS = NDF__HUMIN
                           CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )
                           CALL NDF1_SETC( BUFFER, 'BADUMODE' )
                           CALL ERR_REP( 'NDF1_DH_UMODE',
     :                                   'The UPDATE_MODE component ' //
     :                                   'in the NDF history ' //
     :                                   'structure ^HIST has an ' //
     :                                   'invalid value of ' //
     :                                   '''^BADUMODE''.', STATUS )
                        END IF
                     END IF

*  Annul the UPDATE_MODE component locator.
                     CALL DAT_ANNUL( LOC, STATUS )
                  END IF
               END IF

*  EXTEND_SIZE component.
*  =====================
*  See if the history structure contains the optional EXTEND_SIZE
*  component (giving the number of elements by which the RECORDS array
*  should be extended when necessary). Initially set the default value.
               CALL DAT_THERE( DCB_HLOC( IDCB ), 'EXTEND_SIZE', THERE,
     :                         STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  DCB_HEXT( IDCB ) = 5

*  If the component is present, obtain a locator for it and determine
*  its type and shape.
                  IF ( THERE ) THEN
                     CALL DAT_FIND( DCB_HLOC( IDCB ), 'EXTEND_SIZE',
     :                              LOC, STATUS )
                     CALL DAT_TYPE( LOC, TYPE, STATUS )
                     CALL DAT_SHAPE( LOC, DAT__MXDIM, DIM, NDIM,
     :                               STATUS )

*  Check that the EXTEND_SIZE component has type '_INTEGER' and report
*  an error if it does not.
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        IF ( TYPE .NE. '_INTEGER' ) THEN
                           STATUS = NDF__TYPIN
                           CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )
                           CALL MSG_SETC( 'BADTYPE', TYPE )
                           CALL ERR_REP( 'NDF1_DH_ETYPE',
     :                                   'The EXTEND_SIZE ' //
     :                                   'component in the NDF ' //
     :                                   'history structure ^HIST ' //
     :                                   'has an invalid type of ' //
     :                                   '''^BADTYPE''; it should ' //
     :                                   'be of type ''_INTEGER''.',
     :                                   STATUS )

*  Also check that the EXTEND_SIZE component is scalar and report an
*  error if it is not.
                        ELSE IF ( NDIM .NE. 0 ) THEN
                           STATUS = NDF__NDMIN
                           CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )
                           CALL MSG_SETI( 'BADNDIM', NDIM )
                           CALL ERR_REP( 'NDF1_DH_ENDIM',
     :                                   'The EXTEND_SIZE ' //
     :                                   'component in the NDF ' //
     :                                   'history structure ^HIST ' //
     :                                   'is ^BADNDIM-dimensional; ' //
     :                                   'it should be scalar.',
     :                                   STATUS )
                        END IF
                     END IF

*  Read the EXTEND_SIZE value.
                     CALL DAT_GET0I( LOC, DCB_HEXT( IDCB ), STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that the value is not less than one and report an error if it
*  is.
                        IF ( DCB_HEXT( IDCB ) .LT. 1 ) THEN
                           STATUS = NDF__HEXIN
                           CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )
                           CALL MSG_SETI( 'BADEXT', DCB_HEXT( IDCB ) )
                           CALL ERR_REP( 'NDF1_DH_EXT',
     :                                   'The EXTEND_SIZE ' //
     :                                   'component in the NDF ' //
     :                                   'history structure ^HIST ' //
     :                                   'has an invalid value of ' //
     :                                   '^BADEXT; it should be at ' //
     :                                   'least 1.', STATUS )
                        END IF
                     END IF

*  Annul the EXTEND_SIZE component locator.
                     CALL DAT_ANNUL( LOC, STATUS )
                  END IF
               END IF

*  If an error occurred, then annul any locators which may have been
*  acquired.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL DAT_ANNUL( DCB_HRLOC( IDCB ), STATUS )
                  CALL DAT_ANNUL( DCB_HLOC( IDCB ), STATUS )
               END IF
            END IF
         END IF

*  Note whether DCB history information is now up to date.
         DCB_KH( IDCB ) = ( STATUS .EQ. SAI__OK )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_DH', STATUS )

      END
