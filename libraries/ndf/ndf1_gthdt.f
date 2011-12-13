      SUBROUTINE NDF1_GTHDT( IDCB, IREC, YMDHM, SEC, STATUS )
*+
*  Name:
*     NDF1_GTHDT

*  Purpose:
*     Get the date/time value from a history record.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_GTHDT( IDCB, IREC, YMDHM, SEC, STATUS )

*  Description:
*     The routine returns the date/time associated with a record in an
*     NDF history component.

*  Arguments:
*     IDCB = INTEGER (Given)
*        DCB index identifying the NDF for which the information is
*        required.
*     IREC = INTEGER (Given)
*        History record number for which the information is required.
*     YMDHM( 5 ) = INTEGER (Returned)
*        The year, month, day, hour and minute fields of the record's
*        date/time value (in that order), stored as integers.
*     SEC = REAL (Returned)
*        The seconds field of the date/time value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     A history component must be present in the specified NDF. This
*     routine does not check for this.

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
*     1-JUN-1993 (RFWS):
*        Original version.
*     3-JUN-1993 (RFWS):
*        Added contexual error message.
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
*        DCB_HRLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator for array of history records.

*  Arguments Given:
      INTEGER IDCB
      INTEGER IREC

*  Arguments Returned:
      INTEGER YMDHM( 5 )
      REAL SEC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) CELL ! Array cell locator
      CHARACTER * ( DAT__SZLOC ) LOC ! Locator to DATE component
      CHARACTER * ( DAT__SZTYP ) TYPE ! Object data type
      INTEGER CLEN               ! Length of mapped string
      INTEGER DIM( DAT__MXDIM )  ! Object dimension sizes
      INTEGER NDIM               ! Number of object dimensions
      INTEGER PNTR               ! Pointer to mapped string
      INTEGER SUB( 1 )           ! Array cell subscript
      LOGICAL THERE              ! Component present?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that history information is available in the DCB.
      CALL NDF1_DH( IDCB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain a locator to the required cell of the history record array.
         SUB( 1 ) = IREC
         CALL DAT_CELL( DCB_HRLOC( IDCB ), 1, SUB, CELL, STATUS )

*  Check whether the mandatory DATE component is present within the
*  cell. Report an error if it is not.
         CALL DAT_THERE( CELL, 'DATE', THERE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( .NOT. THERE ) THEN
               STATUS = NDF__NOHDT
               CALL DAT_MSG( 'STRUCT', CELL )
               CALL ERR_REP( 'NDF1_GTHDT_DATE',
     :                       'The DATE component is missing from ' //
     :                       'the NDF history record structure ' //
     :                       '^STRUCT', STATUS )

*  Otherwise, obtain a locator to the DATE component and determine its
*  type and shape.
            ELSE
               CALL DAT_FIND( CELL, 'DATE', LOC, STATUS )
               CALL DAT_TYPE( LOC, TYPE, STATUS )
               CALL DAT_SHAPE( LOC, DAT__MXDIM, DIM, NDIM, STATUS )

*  Check that the DATE component is of type '_CHAR' and report an error
*  if it is not.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( TYPE( : 6 ) .NE. '_CHAR*' ) THEN
                     STATUS = NDF__TYPIN
                     CALL DAT_MSG( 'STRUC', CELL )
                     CALL MSG_SETC( 'BADTYPE', TYPE )
                     CALL ERR_REP( 'NDF1_GTHDT_TYPE',
     :                             'The DATE component in the NDF ' //
     :                             'history record structure ^STRUC ' //
     :                             'has an invalid type of ' //
     :                             '''^BADTYPE''; it should be of ' //
     :                             'type ''_CHAR''.', STATUS )

*  Also check that the DATE component is scalar and report an error if
*  it is not.
                  ELSE IF ( NDIM .NE. 0 ) THEN
                     STATUS = NDF__NDMIN
                     CALL DAT_MSG( 'STRUC', CELL )
                     CALL MSG_SETI( 'BADNDIM', NDIM )
                     CALL ERR_REP( 'NDF1_GTHDT_NDIM',
     :                             'The DATE component in the NDF ' //
     :                             'history record structure ' //
     :                             '^STRUC is ^BADNDIM-dimensional; ' //
     :                             'it should be scalar.', STATUS )
                  END IF
               END IF

*  Map the DATE component and determine its length.
               CALL DAT_MAPC( LOC, 'READ', 0, DIM, PNTR, STATUS )
               CALL DAT_CLEN( LOC, CLEN, STATUS )

*  Parse the string and report a contextual error message if necessary.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL NDF1_PSHDT( %VAL( CNF_PVAL( PNTR ) ), YMDHM, SEC,
     :                             STATUS, %VAL( CNF_CVAL( CLEN ) ) )
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL MSG_SETI( 'IREC', IREC )
                     CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )
                     CALL ERR_REP( 'NDF1_GTHDT_CTX',
     :                             'Unable to obtain date/time ' //
     :                             'information for record ^IREC ' //
     :                             'in the NDF history structure ' //
     :                             '^HIST.', STATUS )
                  END IF
               END IF

*  Annul the DATE locator.
               CALL DAT_ANNUL( LOC, STATUS )
            END IF
         END IF

*  Annul the history record cell locator.
         CALL DAT_ANNUL( CELL, STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_GTHDT', STATUS )

      END
