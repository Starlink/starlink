      SUBROUTINE NDF1_DQ( IDCB, STATUS )
*+
*  Name:
*     NDF1_DQ

*  Purpose:
*     Ensure that quality information is available in the DCB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_DQ( IDCB, STATUS )

*  Description:
*     The routine ensures that information about a data object's
*     quality component is available in the DCB. It does nothing if
*     this information is already available. Otherwise, it obtains this
*     information by inspecting the actual data object, performing
*     necessary validation checks in the process.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index to the DCB entry for which quality information is
*        required.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  See if quality information is already available. There is
*     nothing to do if it is.
*     -  Ensure that information about the data array is available in
*     the DCB.
*     -  See if the QUALITY component is present. If not, then signify
*     this by storing null quality values in the DCB.  If it is
*     present, then obtain a locator to it, storing this in the DCB.
*     -  Obtain the type and shape of the QUALITY component.
*     -  Check that the type is 'QUALITY'. Report an error if it is
*     not.
*     -  Check that the component is scalar. Report an error if it is
*     not.
*     -  See if the VARIANT component of the QUALITY structure is
*     present.  If so, then obtain a locator to it and determine its
*     type and shape.
*     -  Check that it is a character object. Report an error if it is
*     not.
*     -  Check that it is scalar. Report an error if it is not.
*     -  Map the VARIANT component and test its value. Report an error
*     if it is not 'SIMPLE'.
*     -  Annul the locator to the VARIANT component.
*     -  See if there is a BADBITS component in the QUALITY structure.
*     If not, then the badbits value defaults to zero.
*     -  Otherwise, obtain a locator to the BADBITS component and
*     determine its type and shape.
*     -  Check it is of type '_UBYTE'. Report an error if it is not.
*     -  Check it is scalar. Report an error if it is not.
*     -  Read the badbits value and store it in the DCB. Annul the
*     BADBITS locator.
*     -  See if the QUALITY array is present in the QUALITY structure.
*     Report an error if it is not.
*     -  Import the QUALITY array component (of the QUALITY structure)
*     into the ARY_ system, storing the resulting identifier in the
*     DCB.
*     -  Obtain the number of dimensions and the pixel index bounds of
*     the NDF's data array and quality array. Also determine the full
*     data type of the quality array.
*     -  Report an error if the number of quality dimensions does not
*     match that of the data array.
*     -  Check that the array has a data type of '_UBYTE' and report an
*     error if it does not.
*     -  Check that the quality array pixel index bounds in each
*     dimension match those of the data array.  Report an error if a
*     discrepancy is found.
*     -  Set the default storage form of the quality component using
*     the values initially derived from the NDF's data array component.
*     -  If the NDF's quality component is not suitable, then annul any
*     identifiers and locators which may have been allocated.
*     -  Initialise the quality mapping count and note whether quality
*     information is now available in the DCB.

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
*     31-JAN-1990 (RFWS):
*        Original, derived from the NDF1_DV routine.
*     15-NOV-1990 (RFWS):
*        Removed unnecessary DCB initialisation.
*     16-NOV-1990 (RFWS):
*        Fixed number of dimensions error in call to DAT_MAPC.
*     21-DEC-1990 (RFWS):
*        Removed initialisation of the default DCB_QBB value; this is
*        now performed by NDF1_FFS.
*     17-JAN-1992 (RFWS):
*        Added handling of mapped character length for UNIX
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
      INCLUDE 'ARY_PAR'          ! ARY_ public constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_DEFRM( NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM ) (Read)
*           Default storage form for NDF components.
*        DCB_DID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        DCB_KQ( NDF__MXDCB ) = LOGICAL (Read and Write)
*           Whether information about the NDF's quality component is
*           available in the DCB.
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.
*        DCB_QFRM( NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM ) (Write)
*           Default storage form for the quality component.
*        DCB_QID( NDF__MXDCB ) = INTEGER (Write)
*           ARY_ system identifier for the NDF's quality array.
*        DCB_QLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Quality component locator.

*  Arguments Given:
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Local Constants:
      BYTE ZEROUB                ! Zero as un unsigned byte value
      PARAMETER ( ZEROUB = 0 )

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOCBB ! Locator to BADBITS object
      CHARACTER * ( DAT__SZLOC ) LOCV ! Locator to VARIANT component
      CHARACTER * ( DAT__SZTYP ) TYPE ! HDS data type string
      CHARACTER * ( NDF__SZFTP ) FTYPEQ ! Full type of the quality array
      INTEGER DIM( NDF__MXDIM )  ! HDS object dimensions
      INTEGER I                  ! Loop counter for dimensions
      INTEGER LBNDD( NDF__MXDIM ) ! Data component lower bounds
      INTEGER LBNDQ( NDF__MXDIM ) ! Quality component lower bounds
      INTEGER LENV               ! Length of mapped character value
      INTEGER NDIM               ! Number of HDS object dimensions
      INTEGER NDIMD              ! Number of data component dimensions
      INTEGER NDIMQ              ! Number of quality dimensions
      INTEGER PNTR               ! Pointer to mapped VARIANT value
      INTEGER UBNDD( NDF__MXDIM ) ! Data component upper bounds
      INTEGER UBNDQ( NDF__MXDIM ) ! Quality component upper bounds
      LOGICAL THERE              ! Whether quality component exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if quality information is already available. There is nothing to
*  do if it is.
      IF ( .NOT. DCB_KQ( IDCB ) ) THEN

*  Ensure that information about the data array is available in the DCB.
         CALL NDF1_DD( IDCB, STATUS )

*  See if the QUALITY component is present. If not, then signify this
*  by storing null quality values in the DCB.
         CALL DAT_THERE( DCB_LOC( IDCB ), 'QUALITY', THERE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            DCB_QLOC( IDCB ) = DAT__NOLOC
            DCB_QID( IDCB ) = ARY__NOID

*  If it is present, then obtain a locator to it, storing this in the
*  DCB.
            IF ( THERE ) THEN
               CALL DAT_FIND( DCB_LOC( IDCB ), 'QUALITY',
     :                        DCB_QLOC( IDCB ), STATUS )

*  Obtain the type and shape of the QUALITY component.
               CALL DAT_TYPE( DCB_QLOC( IDCB ), TYPE, STATUS )
               CALL DAT_SHAPE( DCB_QLOC( IDCB ), DAT__MXDIM, DIM, NDIM,
     :                         STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that the type is 'QUALITY'. Report an error if it is not.
                  IF ( TYPE .NE. 'QUALITY' ) THEN
                     STATUS = NDF__TYPIN
                     CALL NDF1_DMSG( 'NDF', IDCB )
                     CALL MSG_SETC( 'BADTYPE', TYPE )
                     CALL ERR_REP( 'NDF1_DQ_TYPE',
     :               'The QUALITY component in the NDF structure ' //
     :               '^NDF has an invalid data type of ' //
     :               '''^BADTYPE''; it should be of type ' //
     :               '''QUALITY''.', STATUS )

*  Check that the component is scalar. Report an error if it is not.
                  ELSE IF ( NDIM .NE. 0 ) THEN
                     STATUS = NDF__NDMIN
                     CALL NDF1_DMSG( 'NDF', IDCB )
                     CALL MSG_SETI( 'BADNDIM', NDIM )
                     CALL ERR_REP( 'NDF1_DQ_NDIM',
     :               'The QUALITY component in the NDF structure ' //
     :               '^NDF is ^BADNDIM-dimensional; it should be ' //
     :               'scalar.', STATUS )
                  END IF
               END IF

*  See if the VARIANT component of the QUALITY structure is present.
               CALL DAT_THERE( DCB_QLOC( IDCB ), 'VARIANT', THERE,
     :                         STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If so, then obtain a locator to it and determine its type and shape.
                  IF ( THERE ) THEN
                     CALL DAT_FIND( DCB_QLOC( IDCB ), 'VARIANT', LOCV,
     :                              STATUS )
                     CALL DAT_TYPE( LOCV, TYPE, STATUS )
                     CALL DAT_SHAPE( LOCV, DAT__MXDIM, DIM, NDIM,
     :                               STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that it is a character object. Report an error if it is not.
                        IF ( TYPE( : 6 ) .NE. '_CHAR*' ) THEN
                           STATUS = NDF__TYPIN
                           CALL DAT_MSG( 'QUAL', DCB_QLOC( IDCB ) )
                           CALL MSG_SETC( 'BADTYPE', TYPE )
                           CALL ERR_REP( 'NDF1_DQ_VTYPE',
     :                     'The VARIANT component in the NDF ' //
     :                     'quality structure ^QUAL has an invalid ' //
     :                     'data type of ''^BADTYPE''; it should be ' //
     :                     'of type ''_CHAR''.', STATUS )

*  Check that it is scalar. Report an error if it is not.
                        ELSE IF ( NDIM .NE. 0 ) THEN
                           STATUS = NDF__NDMIN
                           CALL DAT_MSG( 'QUAL', DCB_QLOC( IDCB ) )
                           CALL MSG_SETI( 'BADNDIM', NDIM )
                           CALL ERR_REP( 'NDF1_DQ_VNDIM',
     :                     'The VARIANT component in the NDF ' //
     :                     'quality structure ^QUAL is ' //
     :                     '^BADNDIM-dimensional; it should be ' //
     :                     'scalar.', STATUS )
                        END IF
                     END IF

*  Map the VARIANT component and determine its length.
                     DIM( 1 ) = 0
                     CALL DAT_MAPC( LOCV, 'READ', 0, DIM, PNTR, STATUS )
                     CALL DAT_CLEN( LOCV, LENV, STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  Test its value. Report an error if it is not 'SIMPLE'.
                        IF ( .NOT. CHR_SIMLR( %VAL( CNF_PVAL( PNTR ) ),
     :                                        'SIMPLE',
     :                                %VAL( CNF_CVAL( LENV ) ) ) ) THEN
                           STATUS = NDF__VARIN
                           CALL DAT_MSG( 'QUAL', DCB_QLOC( IDCB ) )
                           CALL NDF1_SETC( %VAL( CNF_PVAL( PNTR ) ),
     :                                     'BADVAR',
     :                                     %VAL( CNF_CVAL( LENV ) ) )
                           CALL ERR_REP( 'NDF1_DQ_VAR',
     :                     'The VARIANT component in the NDF ' //
     :                     'quality structure ^QUAL has an invalid ' //
     :                     'value of ''^BADVAR''; only the value ' //
     :                     '''SIMPLE'' is defined.', STATUS )
                        END IF
                     END IF

*  Annul the locator to the VARIANT component.
                     CALL DAT_ANNUL( LOCV, STATUS )
                  END IF
               END IF

*  See if there is a BADBITS component in the QUALITY structure.
               CALL DAT_THERE( DCB_QLOC( IDCB ), 'BADBITS', THERE,
     :                         STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If not, then the badbits value defaults to zero.  Otherwise, obtain
*  a locator to the BADBITS component and determine its type and shape.
                  IF ( THERE ) THEN
                     CALL DAT_FIND( DCB_QLOC( IDCB ), 'BADBITS',
     :                              LOCBB, STATUS )
                     CALL DAT_TYPE( LOCBB, TYPE, STATUS )
                     CALL DAT_SHAPE( LOCBB, DAT__MXDIM, DIM, NDIM,
     :                               STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  Check it is of type '_UBYTE'. Report an error if it is not.
                        IF ( TYPE .NE. '_UBYTE' ) THEN
                           STATUS = NDF__TYPIN
                           CALL DAT_MSG( 'QUAL', DCB_QLOC( IDCB ) )
                           CALL MSG_SETC( 'BADTYPE', TYPE )
                           CALL ERR_REP( 'NDF1_DQ_BBTYP',
     :                     'The BADBITS component in the NDF ' //
     :                     'quality structure ^QUAL has an invalid ' //
     :                     'data type of ''^BADTYPE''; it should be ' //
     :                     'of type ''_UBYTE''.', STATUS )

*  Check it is scalar. Report an error if it is not.
                        ELSE IF ( NDIM .NE. 0 ) THEN
                           STATUS = NDF__NDMIN
                           CALL DAT_MSG( 'QUAL', DCB_QLOC( IDCB ) )
                           CALL MSG_SETI( 'BADNDIM', NDIM )
                           CALL ERR_REP( 'NDF1_DQ_BBNDIM',
     :                     'The BADBITS component in the NDF ' //
     :                     'quality structure ^QUAL is ' //
     :                     '^BADNDIM-dimensional; it should be ' //
     :                     'scalar.', STATUS )
                        END IF
                     END IF

*  Read the badbits value and store it in the DCB.
                     DIM( 1 ) = 0
                     CALL DAT_GET( LOCBB, '_UBYTE', 0, DIM,
     :                             DCB_QBB( IDCB ), STATUS )

*  Annul the BADBITS locator.
                     CALL DAT_ANNUL( LOCBB, STATUS )
                  END IF
               END IF

*  See if the QUALITY array is present in the QUALITY structure. Report
*  an error if it is not.
               CALL DAT_THERE( DCB_QLOC( IDCB ), 'QUALITY', THERE,
     :                         STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( .NOT. THERE ) THEN
                     STATUS = NDF__NOQLY
                     CALL DAT_MSG( 'QUAL', DCB_QLOC( IDCB ) )
                     CALL ERR_REP( 'NDF1_DQ_NOQLY',
     :               'The QUALITY array is missing from the NDF ' //
     :               'quality structure ^QUAL', STATUS )
                  ELSE

*  Import the QUALITY array component (of the QUALITY structure) into
*  the ARY_ system, storing the resulting identifier in the DCB.
                     CALL ARY_FIND( DCB_QLOC( IDCB ), 'QUALITY',
     :                              DCB_QID( IDCB ), STATUS )

*  Obtain the number of dimensions and the pixel index bounds of the
*  NDF's data array and quality array. Also determine the full data
*  type of the quality array.
                     CALL ARY_BOUND( DCB_DID( IDCB ), NDF__MXDIM, LBNDD,
     :                               UBNDD, NDIMD, STATUS )
                     CALL ARY_BOUND( DCB_QID( IDCB ), NDF__MXDIM, LBNDQ,
     :                               UBNDQ, NDIMQ, STATUS )
                     CALL ARY_FTYPE( DCB_QID( IDCB ), FTYPEQ, STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  Report an error if the number of quality dimensions does not match
*  that of the data array.
                        IF ( NDIMQ .NE. NDIMD ) THEN
                           STATUS = NDF__NDMIN
                           CALL DAT_MSG( 'QUAL', DCB_QLOC( IDCB ) )
                           CALL MSG_SETI( 'BADNDIM', NDIMQ )
                           CALL MSG_SETI( 'NDIM', NDIMD )
                           CALL ERR_REP( 'NDF1_DQ_QANDIM',
     :                     'The QUALITY array in the NDF ' //
     :                     'quality structure ^QUAL has an invalid ' //
     :                     'number of dimensions (^BADNDIM); it ' //
     :                     'should be ^NDIM-dimensional.', STATUS )

*  Check that the array has a data type of '_UBYTE' and report an error
*  if it does not.
                        ELSE IF ( FTYPEQ .NE. '_UBYTE' ) THEN
                           STATUS = NDF__TYPIN
                           CALL DAT_MSG( 'QUAL', DCB_QLOC( IDCB ) )
                           CALL MSG_SETC( 'BADTYPE', FTYPEQ )
                           CALL ERR_REP( 'NDF1_DQ_QATYP',
     :                     'The QUALITY array in the NDF ' //
     :                     'quality structure ^QUAL has an invalid ' //
     :                     'type of ''^BADTYPE''; it should be of ' //
     :                     'type ''_UBYTE''.', STATUS )

*  Check that the quality array pixel index bounds in each dimension
*  match those of the data array.
                        ELSE
                           DO 1 I = 1, NDIMD
                              IF ( ( LBNDQ( I ) .NE. LBNDD( I ) ) .OR.
     :                             ( UBNDQ( I ) .NE. UBNDD( I ) ) ) THEN

*  Report an error if a discrepancy is found.
                                 STATUS = NDF__BNDIN
                                 CALL MSG_SETI( 'DIM', I )
                                 CALL DAT_MSG( 'QUAL',
     :                                         DCB_QLOC( IDCB ) )
                                 CALL ERR_REP( 'NDF1_DQ_QABND',
     :                           'The pixel-index bounds of ' //
     :                           'dimension ^DIM of the NDF quality ' //
     :                           'structure ^QUAL do not match ' //
     :                           'those of the NDF''s DATA_ARRAY ' //
     :                           'component.', STATUS )
                                 GO TO 2
                              END IF
1                          CONTINUE
2                          CONTINUE
                        END IF
                     END IF
                  END IF
               END IF
            END IF

*  Set the default storage form of the quality component using the
*  values initially derived from the NDF's data array component.
            DCB_QFRM( IDCB ) = DCB_DEFRM( IDCB )

*  If the component is not suitable, then annul any identifiers and
*  locators which may have been allocated.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ARY_ANNUL( DCB_QID( IDCB ), STATUS )
               CALL DAT_ANNUL( DCB_QLOC( IDCB ), STATUS )
            END IF
         END IF

*  Note whether quality information is now available in the DCB.
         DCB_KQ( IDCB ) = STATUS .EQ. SAI__OK
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_DQ', STATUS )

      END
