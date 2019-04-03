      SUBROUTINE NDF1_DX( IDCB, STATUS )
*+
*  Name:
*     NDF1_DX

*  Purpose:
*     Ensure that extension information is available in the DCB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_DX( IDCB, STATUS )

*  Description:
*     The routine ensures that information about a data object's
*     extension (MORE) structure is available for a DCB entry. If this
*     information is already available, then it does nothing.
*     Otherwise, it obtains this information by inspecting the data
*     object itself, performing any necessary validation checks in the
*     process.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index to the DCB entry for which extension information is
*        required.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Particle Physics and Astronomy Research Council

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
*        Corrected wrong error code.
*     26-JAN-1990 (RFWS):
*        Added checks on the validity of a VARIANT component in the MORE
*        structure, if present.
*     19-MAR-1990 (RFWS):
*        Changed error code.
*     2-JAN-1991 (RFWS):
*        Changed erroneous call to MSG_SETC to use MSG_SETI.
*     4-OCT-1991 (RFWS):
*        Added use of warning message flag.
*     2-DEC-1991 (RFWS):
*        Changed behaviour of warning flag - continue without setting
*        STATUS if it is not set.
*     17-JAN-1992 (RFWS):
*        Added handling of mapped character length for UNIX
*        compatibility.
*     9-MAR-1994 (RFWS):
*        Ensure that the TCB is initialised.
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
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_KX( NDF__MXDCB ) = LOGICAL (Read and Write)
*           Whether extension information is available in the DCB.
*        DCB_LOC( NDF_MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.
*        DCB_XLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Extension (MORE) structure locator.

      INCLUDE 'NDF_TCB'          ! NDF_ Tuning Control Block
*        TCB_WARN = LOGICAL (Read)
*           Warning message flag.

*  Arguments Given:
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOCVAR ! Locator to VARIANT component
      CHARACTER * ( DAT__SZTYP ) TYPE ! HDS data type string
      INTEGER DIM( DAT__MXDIM )  ! Array of HDS dimensions
      INTEGER LENV               ! Length of mapped character value
      INTEGER NDIM               ! Number of HDS dimensions
      INTEGER PNTR               ! Pointer to mapped VARIANT value
      LOGICAL THERE              ! Whether a component is present

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  There is nothing to do if extension information is already available.
      IF ( .NOT. DCB_KX( IDCB ) ) THEN

*  Ensure that the TCB is initialised.
         CALL NDF1_INTCB( STATUS )

*  Initialise the extension (MORE) structure locator and see if a MORE
*  structure exists in the NDF.
         DCB_XLOC( IDCB ) = DAT__NOLOC
         CALL DAT_THERE( DCB_LOC( IDCB ), 'MORE', THERE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If so, then obtain a locator to it, storing it in the DCB.
            IF ( THERE ) THEN
               CALL DAT_FIND( DCB_LOC( IDCB ), 'MORE', DCB_XLOC( IDCB ),
     :                        STATUS )

*  Obtain the MORE structure's type and shape.
               CALL DAT_TYPE( DCB_XLOC( IDCB ), TYPE, STATUS )
               CALL DAT_SHAPE( DCB_XLOC( IDCB ), DAT__MXDIM, DIM,
     :                         NDIM, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If its type is not 'EXT', but the TCB_WARN flag is set, then issue a
*  warning message, but continue.
                  IF ( TYPE .NE. 'EXT' ) THEN
                     IF ( TCB_WARN ) THEN
                        CALL ERR_MARK
                        STATUS = NDF__TYPIN
                        CALL MSG_SETC( 'BADTYPE', TYPE )
                        CALL NDF1_DMSG( 'NDF', IDCB )
                        CALL ERR_REP( 'NDF1_DX_WTYP',
     :                  'Warning: the MORE component in the NDF ' //
     :                  'structure ^NDF has an invalid data type ' //
     :                  'of ''^BADTYPE''; its type should be ' //
     :                  '''EXT''.', STATUS )
                        CALL ERR_FLUSH( STATUS )
                        CALL ERR_RLSE
                     END IF
                  END IF

*  If it is not scalar, then report an error.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     IF ( NDIM .NE. 0 ) THEN
                        STATUS = NDF__NDMIN
                        CALL MSG_SETI( 'BADNDIM', NDIM )
                        CALL NDF1_DMSG( 'NDF', IDCB )
                        CALL ERR_REP( 'NDF1_DX_NDIM',
     :                  'The MORE component in the NDF structure ' //
     :                  '^NDF is ^BADNDIM-dimensional; it should ' //
     :                  'be scalar.', STATUS )
                     END IF
                  END IF
               END IF

*  See if a VARIANT component is present in the MORE structure.
               CALL DAT_THERE( DCB_XLOC( IDCB ), 'VARIANT', THERE,
     :                         STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If so, then obtain a locator to it and determine its type and shape.
                  IF ( THERE ) THEN
                     CALL DAT_FIND( DCB_XLOC( IDCB ), 'VARIANT',
     :                              LOCVAR, STATUS )
                     CALL DAT_TYPE( LOCVAR, TYPE, STATUS )
                     CALL DAT_SHAPE( LOCVAR, DAT__MXDIM, DIM, NDIM,
     :                               STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that the VARIANT is a character object and report an error if
*  it is not.
                        IF ( TYPE( : 6 ) .NE. '_CHAR*' ) THEN
                           STATUS = NDF__TYPIN
                           CALL DAT_MSG( 'MORE', DCB_XLOC( IDCB ) )
                           CALL MSG_SETC( 'BADTYPE', TYPE )
                           CALL ERR_REP( 'NDF1_DX_VTYP',
     :                    'The VARIANT component in the NDF ' //
     :                    'extension structure ^MORE has an ' //
     :                    'invalid data type of ''^BADTYPE''; it ' //
     :                    'should be of type ''_CHAR''.', STATUS )

*  Check that it is scalar and report an error if it is not.
                        ELSE IF ( NDIM .NE. 0 ) THEN
                           STATUS = NDF__NDMIN
                           CALL DAT_MSG( 'MORE', DCB_XLOC( IDCB ) )
                           CALL MSG_SETI( 'BADNDIM', NDIM )
                           CALL ERR_REP( 'NDF1_DX_VNDM',
     :                     'The VARIANT component in the NDF ' //
     :                     'extension structure ^MORE is ' //
     :                     '^BADNDIM-dimensional; it should be ' //
     :                     'scalar.', STATUS )

*  If the VARIANT component is OK so far, then map it and determine its
*  length.
                        ELSE
                           DIM( 1 ) = 0
                           CALL DAT_MAPC( LOCVAR, 'READ', 0, DIM, PNTR,
     :                                    STATUS )
                           CALL DAT_CLEN( LOCVAR, LENV, STATUS )
                           IF ( STATUS .EQ. SAI__OK ) THEN

*  If its value is not 'SIMPLE', then report an error.
                              IF ( .NOT.
     :                             CHR_SIMLR( %VAL( CNF_PVAL( PNTR ) ),
     :                                        'SIMPLE',
     :                                %VAL( CNF_CVAL( LENV ) ) ) ) THEN
                                 STATUS = NDF__VARIN
                                 CALL DAT_MSG( 'MORE',
     :                                         DCB_XLOC( IDCB ) )
                                 CALL NDF1_SETC(
     :                                         %VAL( CNF_PVAL( PNTR ) ),
     :                                           'BADVARIANT',
     :                                        %VAL( CNF_CVAL( LENV ) ) )
                                 CALL ERR_REP( 'NDF1_DX_VRNT',
     :                           'The VARIANT component in the NDF ' //
     :                           'extension structure ^MORE has an ' //
     :                           'invalid value of ''^BADVARIANT''; ' //
     :                           'only the value ''SIMPLE'' is ' //
     :                           'defined.', STATUS )
                              END IF
                           END IF
                        END IF
                     END IF

*  Annul the locator to the VARIANT component.
                     CALL DAT_ANNUL( LOCVAR, STATUS )
                  END IF
               END IF

*  If an error occurred, then annul the extension (MORE) structure
*  locator.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL DAT_ANNUL( DCB_XLOC( IDCB ), STATUS )
               END IF
            END IF
         END IF

*  Note whether extension information is now available in the DCB.
         DCB_KX( IDCB ) = STATUS .EQ. SAI__OK
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_DX', STATUS )

      END
