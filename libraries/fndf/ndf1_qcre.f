      SUBROUTINE NDF1_QCRE( IACB, STATUS )
*+
*  Name:
*     NDF1_QCRE

*  Purpose:
*     Create a quality component for an NDF, if necessary.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_QCRE( IACB, STATUS )

*  Description:
*     The routine ensures that a quality component containing a quality
*     array exists for an NDF, creating one if necessary. The array is
*     created using the default quality storage form held in the DCB.
*     An HDS locator for the quality structure is stored in the DCB by
*     this routine and ARY_ system identifiers for the new array (and
*     appropriate sections thereof) are entered into the DCB and also
*     into those ACB entries which refer to the NDF data object in
*     question. The NDF is identified to this routine by its ACB entry.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the ACB entry for the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Ensure that quality information is available in the DCB and
*     ACB.
*     -  Obtain an index to the data object entry in the DCB.
*     -  See if the quality component locator is valid. If not, then
*     the component does not exist and must be created.
*     -  Create the quality component if necessary and obtain a locator
*     to it to retain in the DCB.
*     -  Note that the quality array does not yet exist.
*     -  See if the ARY_ system identifier for the quality array is
*     valid. If not, then the quality array does not exist and must be
*     created.
*     -  Obtain the NDF bounds from its data array.
*     -  Tune HDS for the expected maximum number of components in the
*     quality structure.
*     -  Obtain a placeholder for the quality array, then handle the
*     creation of each form of array in turn using the default storage
*     form stored in the DCB.
*     -  If the default quality storage form entry in the DCB was not
*     recognised, then report an error.
*     -  Loop to identify all the ACB entries which refer to this DCB
*     entry.  Select those entries with the correct DCB index.
*     -  For each relevant ACB entry, create a section from the quality
*     array which matches the ACB's data array section and store the
*     resulting ARY_ system identifier in the ACB.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     22-JAN-1990 (RFWS):
*        Original, derived from the NDF1_VCRE routine.
*     15-FEB-1990 (RFWS):
*        Installed support for primitive arrays.
*     22-MAR-1990 (RFWS):
*        Tune HDS for the expected maximum number of components in the
*        quality structure.
*     20-JAN-1999 (RFWS):
*        Call NDF1_SSDUP to eliminate problems with ARY_SSECT when the
*        dimensionality of a section differs from that of the base
*        array.
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
      INCLUDE 'ARY_PAR'          ! ARY_ public constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_DID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.
*        DCB_QFRM( NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM ) (Read)
*           The default form of array used to store data in the NDF's
*           quality component.
*        DCB_QID( NDF__MXDCB ) = INTEGER (Read and Write)
*           ARY_ system identifier for the NDF's quality array.
*        DCB_QLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Locator to the NDF's quality component.

         INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_QID( NDF__MXACB ) = INTEGER (Write)
*           ARY_ system identifier for the NDF's quality array.

*  Arguments Given:
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DIM( 1 )           ! Dummy dimension array
      INTEGER IACBT              ! ACB entry to test
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER IDCBT              ! DCB index to test
      INTEGER LBND( NDF__MXDIM ) ! NDF lower pixel index bounds
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER NEXT               ! Next ACB entry to test
      INTEGER PLACE              ! Placeholder for quality array
      INTEGER UBND( NDF__MXDIM ) ! NDF upper pixel index bounds
      LOGICAL THERE              ! Whether the quality array exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that quality information is available in the DCB and ACB.
      CALL NDF1_QIMP( IACB, STATUS )

*  Obtain an index to the data object entry in the DCB.
      IDCB = ACB_IDCB( IACB )

*  See if the quality component locator is valid. If not, then the
*  component does not exist and must be created.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( DCB_QLOC( IDCB ) .EQ. DAT__NOLOC ) THEN

*  Create the quality component if necessary and obtain a locator to it
*  to retain in the DCB.
            DIM( 1 ) = 0
            CALL DAT_NEW( DCB_LOC( IDCB ), 'QUALITY', 'QUALITY', 0,
     :                    DIM, STATUS )
            CALL DAT_FIND( DCB_LOC( IDCB ), 'QUALITY', DCB_QLOC( IDCB ),
     :                     STATUS )

*  Note that the quality array does not yet exist.
            DCB_QID( IDCB ) = ARY__NOID
         END IF
      END IF

*  See if the ARY_ system identifier for the quality array is valid. If
*  not, then the quality array does not exist and must be created.
      CALL ARY_VALID( DCB_QID( IDCB ), THERE, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( .NOT. THERE ) THEN

*  Obtain the NDF bounds from its data array.
            CALL ARY_BOUND( DCB_DID( IDCB ), NDF__MXDIM, LBND, UBND,
     :                      NDIM, STATUS )

*  Tune HDS for the expected maximum number of components in the quality
*  structure.
            CALL HDS_TUNE( 'NCOMP', 3, STATUS )

*  Obtain a placeholder for the quality array, then handle the creation
*  of each form of array in turn using the default storage form stored
*  in the DCB.
            CALL ARY_PLACE( DCB_QLOC( IDCB ), 'QUALITY', PLACE, STATUS )

*  Primitive array.
*  ===============
            IF ( DCB_QFRM( IDCB ) .EQ. 'PRIMITIVE' ) THEN
               CALL ARY_NEWP( '_UBYTE', NDIM, UBND, PLACE,
     :                        DCB_QID( IDCB ), STATUS )

*  Simple array.
*  ============
            ELSE IF ( DCB_QFRM( IDCB ) .EQ. 'SIMPLE' ) THEN
               CALL ARY_NEW( '_UBYTE', NDIM, LBND, UBND, PLACE,
     :                       DCB_QID( IDCB ), STATUS )

*  If the default quality storage form entry in the DCB was not
*  recognised, then report an error.
            ELSE
               STATUS = NDF__FATIN
               CALL MSG_SETC( 'BADFORM', DCB_QFRM( IDCB ) )
               CALL ERR_REP( 'NDF1_QCRE_FORM',
     :         'Invalid array storage form ''^BADFORM'' encountered ' //
     :         'in the NDF_ system Data Control Block (internal ' //
     :         'programming error).', STATUS )
            END IF

*  Loop to identify all the ACB entries which refer to this DCB entry.
            NEXT = 0
            IACBT = 0
1           CONTINUE             ! Start of 'DO WHILE' loop
            CALL NDF1_NXTSL( NDF__ACB, IACBT, NEXT, STATUS )
            IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NEXT .NE. 0 ) ) THEN
               IACBT = NEXT

*  Select those entries with the correct DCB index.
               IDCBT = ACB_IDCB( IACBT )
               IF ( IDCBT .EQ. IDCB ) THEN

*  Create a section from the quality array which matches the ACB's data
*  array section and store the resulting ARY_ system identifier in the
*  ACB.
                  CALL NDF1_SSDUP( DCB_QID( IDCB ), ACB_DID( IACBT ),
     :                             ACB_QID( IACBT ), STATUS )
               END IF
               GO TO 1
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_QCRE', STATUS )

      END
