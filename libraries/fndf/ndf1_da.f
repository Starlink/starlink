      SUBROUTINE NDF1_DA( IDCB, STATUS )
*+
*  Name:
*     NDF1_DA

*  Purpose:
*     Ensure that axis structure information is available for a data
*     object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_DA( IDCB, STATUS )

*  Description:
*     The routine ensures that axis structure information (including
*     HDS locators to elements of the axis structure array) is
*     available in the DCB for an NDF identified by its DCB entry. If
*     this information is already available, then the routine returns
*     without action. Otherwise, it examines the actual data object to
*     obtain this information. Only those checks on the data object's
*     validity which are necessary to obtain this information are
*     performed.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index to the data object entry in the DCB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Check whether axis structure information is already available.
*     There is nothing to do if it is.
*     -  Ensure that information about the NDF's data array is
*     available for the DCB slot.
*     -  Obtain the NDF bounds from the ARY_ system identifier for the
*     data array.
*     -  See if an axis component is present in the NDF.
*     -  Set initial null values for the axis element locators.
*     -  If an axis component is present, then obtain a locator to it
*     and determine its type and shape.
*     -  Check that its type is 'AXIS'. Report an error if it is not.
*     -  Check it is 1-dimensional. Report an error if it it not.
*     -  Check its dimension size matches the NDF's dimensionality.
*     Report an error if it does not.
*     -  Obtain a locator to each element of the axis structure array
*     for storage in the DCB.
*     -  See if each axis structure element contains a VARIANT
*     component.
*     -  If so, then obtain its type and shape.
*     -  Check that it is a character object. Report an error if it is
*     not.
*     -  Check that it is scalar. Report an error if it is not.
*     -  Map the VARIANT component and test its value. Report an error
*     if it is not 'SIMPLE'.
*     -  Annul the locator to the VARIANT component.
*     -  Quit considering axis structure elements if an error occurs.
*     -  Annul the locator to the axis structure array itself.
*     -  If an error occurred, then annul any locators which may have
*     been allocated.
*     -  Note whether axis structure information is now available in
*     the DCB.

*  Copyright:
*     Copyright (C) 1990, 1992 Science & Engineering Research Council.
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
*     29-MAY-1990 (RFWS):
*        Original version.
*     1-AUG-1990 (RFWS):
*        Added checks on the validity of the variant component (if
*        present) in each element of the axis structure array.
*     16-NOV-1990 (RFWS):
*        Fixed number of dimensions error in DAT_MAPC call.
*     29-NOV-1990 (RFWS):
*        Removed initialisation of axis data array default attributes;
*        these are now set by NDF1_DAD.
*     17-JAN-1992 (RFWS):
*        Added handling of mapped character string length for UNIX
*        compatibility.
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
*        DCB_ALOC( NDF_MXDIM, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC )
*        (Write)
*           Locators to axis component elements.
*        DCB_DID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        DCB_KA( NDF__MXDCB ) = LOGICAL (Read and Write)
*           Whether axis component information is available.
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.

*  Arguments Given:
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) ALOC ! Locator to axis structure array
      CHARACTER * ( DAT__SZLOC ) LOCV ! Axis variant component locator
      CHARACTER * ( DAT__SZTYP ) TYPE ! HDS type string
      INTEGER CELL( 1 )          ! Cell indices for axis structure
      INTEGER DIMA( DAT__MXDIM ) ! Dimension sizes of axis structure
      INTEGER DIMV( DAT__MXDIM ) ! Dimensions of variant component
      INTEGER IAX                ! Loop counter for axes
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER LENV               ! Length of mapped variant value
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER NDIMA              ! Number of axis structure dimensions
      INTEGER NDIMV              ! No. of variant component dimensions
      INTEGER PNTR               ! Pointer to mapped variance
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds
      LOGICAL THERE              ! Whether axis structure is present

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check whether axis structure information is already available. There
*  is nothing to do if it is.
      IF ( .NOT. DCB_KA( IDCB ) ) THEN

*  Ensure that information about the NDF's data array is available for
*  the DCB slot.
         CALL NDF1_DD( IDCB, STATUS )

*  Obtain the NDF bounds from the ARY_ system identifier for the data
*  array.
         CALL ARY_BOUND( DCB_DID( IDCB ), NDF__MXDIM, LBND, UBND, NDIM,
     :                   STATUS )

*  See if an axis component is present in the NDF.
         CALL DAT_THERE( DCB_LOC( IDCB ), 'AXIS', THERE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Set initial null values for the axis element locators.
            DO 1 IAX = 1, NDF__MXDIM
               DCB_ALOC( IAX, IDCB ) = DAT__NOLOC
1           CONTINUE

*  If an axis component is present, then obtain a locator to it and
*  determine its type and shape.
            IF ( THERE ) THEN
               CALL DAT_FIND( DCB_LOC( IDCB ), 'AXIS', ALOC, STATUS )
               CALL DAT_TYPE( ALOC, TYPE, STATUS )
               CALL DAT_SHAPE( ALOC, DAT__MXDIM, DIMA, NDIMA, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that its type is 'AXIS'. Report an error if it is not.
                  IF ( TYPE .NE. 'AXIS' ) THEN
                     STATUS = NDF__TYPIN
                     CALL NDF1_DMSG( 'NDF', IDCB )
                     CALL MSG_SETC( 'BADTYPE', TYPE )
                     CALL ERR_REP( 'NDF1_DA_TYPE',
     :               'The AXIS component in the NDF structure ^NDF ' //
     :               'has an invalid type of ''^BADTYPE''; it ' //
     :               'should be of type ''AXIS''.', STATUS )

*  Check it is 1-dimensional. Report an error if it it not.
                  ELSE IF ( NDIMA .NE. 1 ) THEN
                     STATUS = NDF__NDMIN
                     CALL NDF1_DMSG( 'NDF', IDCB )
                     CALL MSG_SETI( 'BADNDIM', NDIMA )
                     CALL ERR_REP( 'NDF1_DA_NDIM',
     :               'The AXIS component in the NDF structure ^NDF ' //
     :               'is ^BADNDIM-dimensional; it should be ' //
     :               '1-dimensional.', STATUS )

*  Check its dimension size matches the NDF's dimensionality. Report an
*  error if it does not.
                  ELSE IF ( DIMA( 1 ) .NE. NDIM ) THEN
                     STATUS = NDF__DIMIN
                     CALL NDF1_DMSG( 'NDF', IDCB )
                     CALL MSG_SETI( 'BADDIM', DIMA( 1 ) )
                     CALL MSG_SETI( 'NDIM', NDIM )
                     CALL ERR_REP( 'NDF1_DA_DIM',
     :               'The AXIS component in the NDF structure ^NDF ' //
     :               'has ^BADDIM element(s); this number should ' //
     :               'match the number of NDF dimensions (^NDIM).',
     :               STATUS )
                  END IF
               END IF

*  Obtain a locator to each element of the axis structure array for
*  storage in the DCB.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  DO 2 IAX = 1, NDIM
                     CELL( 1 ) = IAX
                     CALL DAT_CELL( ALOC, 1, CELL,
     :                              DCB_ALOC( IAX, IDCB ), STATUS )

*  See if the axis structure element contains a VARIANT component.
                     CALL DAT_THERE( DCB_ALOC( IAX, IDCB ), 'VARIANT',
     :                               THERE, STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  If so, then obtain its type and shape.
                        IF ( THERE ) THEN
                           CALL DAT_FIND( DCB_ALOC( IAX, IDCB ),
     :                                    'VARIANT', LOCV, STATUS )
                           CALL DAT_TYPE( LOCV, TYPE, STATUS )
                           CALL DAT_SHAPE( LOCV, DAT__MXDIM, DIMV,
     :                                     NDIMV, STATUS )
                           IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that it is a character object. Report an error if it is not.
                              IF ( TYPE( : 6 ) .NE. '_CHAR*' ) THEN
                                 STATUS = NDF__TYPIN
                                 CALL DAT_MSG( 'AXIS',
     :                                         DCB_ALOC( IAX, IDCB ) )
                                 CALL MSG_SETC( 'BADTYPE', TYPE )
                                 CALL ERR_REP( 'NDF1_DA_VTYPE',
     :                           'The VARIANT component in the NDF ' //
     :                           'axis structure ^AXIS has an ' //
     :                           'invalid data type of ' //
     :                           '''^BADTYPE''; it should be of ' //
     :                           'type ''_CHAR''.', STATUS )

*  Check that it is scalar. Report an error if it is not.
                              ELSE IF ( NDIMV .NE. 0 ) THEN
                                 STATUS = NDF__NDMIN
                                 CALL DAT_MSG( 'AXIS',
     :                                         DCB_ALOC( IAX, IDCB ) )
                                 CALL MSG_SETI( 'BADNDIM', NDIMV )
                                 CALL ERR_REP( 'NDF1_DA_VNDIM',
     :                           'The VARIANT component in the ' //
     :                           'NDF axis structure ^AXIS is ' //
     :                           '^BADNDIM-dimensional; it should ' //
     :                           'be scalar.', STATUS )
                              END IF
                           END IF

*  Map the VARIANT component and determine its length.
                           DIMV( 1 ) = 0
                           CALL DAT_MAPC( LOCV, 'READ', 0, DIMV, PNTR,
     :                                    STATUS )
                           CALL DAT_CLEN( LOCV, LENV, STATUS )
                           IF ( STATUS .EQ. SAI__OK ) THEN

*  Test its value. Report an error if it is not 'SIMPLE'.
                              IF ( .NOT.
     :                             CHR_SIMLR( %VAL( CNF_PVAL( PNTR ) ),
     :                                        'SIMPLE',
     :                                %VAL( CNF_CVAL( LENV ) ) ) ) THEN
                                 STATUS = NDF__VARIN
                                 CALL DAT_MSG( 'AXIS',
     :                                         DCB_ALOC( IAX, IDCB ) )
                                 CALL NDF1_SETC(
     :                                         %VAL( CNF_PVAL( PNTR ) ),
     :                                           'BADVAR',
     :                                        %VAL( CNF_CVAL( LENV ) ) )
                                 CALL ERR_REP( 'NDF1_DA_VAR',
     :                           'The VARIANT component in the NDF ' //
     :                           'axis structure ^AXIS has an ' //
     :                           'invalid value of ''^BADVAR''; ' //
     :                           'only the value ''SIMPLE'' is ' //
     :                           'defined.', STATUS )
                              END IF
                           END IF

*  Annul the locator to the VARIANT component.
                           CALL DAT_ANNUL( LOCV, STATUS )
                        END IF
                     END IF

*  Quit considering axis structure elements if an error occurs.
                     IF ( STATUS .NE. SAI__OK ) GO TO 3
2                 CONTINUE
3                 CONTINUE
               END IF

*  Annul the locator to the axis structure array itself.
               CALL DAT_ANNUL( ALOC, STATUS )

*  If an error occurred, then annul any locators which may have been
*  allocated.
               IF ( STATUS .NE. SAI__OK ) THEN
                  DO 4 IAX = 1, NDIM
                     CALL DAT_ANNUL( DCB_ALOC( IAX, IDCB ), STATUS )
4                 CONTINUE
               END IF
            END IF
         END IF

*  Note whether axis structure information is now available in the DCB.
         DCB_KA( IDCB ) = STATUS .EQ. SAI__OK
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_DA', STATUS )

      END
