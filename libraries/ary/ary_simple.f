      SUBROUTINE ARY_SIMPLE( IARY, STATUS )
*+
*  Name:
*     ARY_SIMPLE

*  Purpose:
*     Convert an array in-situ into simple storage form.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_SIMPLE( IARY, STATUS )

*  Description:
*     This routine changes the storage form of the supplied array to simple
*     form. If the input array is stored in delta form, it is uncompressed.
*     If the input array is scaled form, it is unscaled.

*  Arguments:
*     IARY = INTEGER (Given)
*        The array identifier. No error is reported if the array is
*        already stored in simple form.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine may only be used to convert a base array. If it is
*     called with an array which is not a base array, then it will return
*     without action. No error will result.
*     -  An error will result if the array, or any part of it, is
*     currently mapped for access (e.g. through another identifier).

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     4-NOV-2010 (DSB):
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
      INCLUDE 'CNF_PAR'          ! CNF_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_ERR'          ! ARY_ error constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block

*  Arguments Given:
      INTEGER IARY

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants
      INTEGER NCOMP
      PARAMETER( NCOMP = 11 )

*  Local Variables:
      CHARACTER COMP( NCOMP )*12 ! SCALED and DELTA component names
      CHARACTER LOCT*(DAT__SZLOC)! Locator for temporary storage
      CHARACTER STYPE*(DAT__SZTYP)! Data type for simple array
      INTEGER DIM( ARY__MXDIM )  ! Array dimensions
      INTEGER LBND( ARY__MXDIM ) ! Dummy lower bounds
      INTEGER EL                 ! Number of elements in array
      INTEGER IACB               ! Index to array entry in the ACB
      INTEGER ICOMP              ! Component index
      INTEGER IDCB               ! Index to input array entry in the DCB
      INTEGER IDIM               ! Axis index
      INTEGER IERR               ! Position of conversion error (dummy)
      INTEGER NDIM               ! Number of array axes
      INTEGER NERR               ! Number of conversion errors (dummy)
      INTEGER PNTR1              ! Pointer to original data values
      INTEGER PNTR2              ! Pointer to new data values
      LOGICAL BAD                ! Were any bad values found?
      LOGICAL DCE                ! Did any data conversion errors occur?
      LOGICAL ISCOPY             ! Mapped data is a copy?
      LOGICAL THERE              ! Whether object exists

      DATA COMP / 'ZAXIS', 'ZDIM', 'ZRATIO', 'VALUE', 'REPEAT',
     :            'FIRST_DATA', 'FIRST_VALUE', 'FIRST_REPEAT',
     :            'SCALE', 'ZERO', 'VARIANT' /
      DATA LBND / ARY__MXDIM*1 /

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Import the input array identifier.
      CALL ARY1_IMPID( IARY, IACB, STATUS )

*  Check it is safe to index an array using IACB.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain an index to the input data object entry in the DCB and ensure
*  that storage form, data type, bounds and scaling information is
*  available for it.
      IDCB = ACB_IDCB( IACB )
      CALL ARY1_DFRM( IDCB, STATUS )
      CALL ARY1_DTYP( IDCB, STATUS )
      CALL ARY1_DBND( IDCB, STATUS )
      CALL ARY1_DSCL( IDCB, STATUS )

*  Get the number of axes in the input array.
      NDIM = DCB_NDIM( IDCB )

*  Check if the data object is mapped for access. Report an error if it is.
      IF( ( DCB_NWRIT( IDCB ) .NE. 0 ) .OR.
     :     ( DCB_NREAD( IDCB ) .NE. 0 ) .AND.
     :     STATUS .EQ. SAI__OK ) THEN
         STATUS = ARY__ISMAP
         CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
         CALL ERR_REP( 'ARY_SIMPLE_MAP', 'The array ^ARRAY is '//
     :                 'mapped for access, perhaps through '//
     :                 'another identifier (possible programming '//
     :                 'error).', STATUS )
         GO TO 999
      END IF

*  Do nothing if the array is already stored in simple form or if it is
*  not a base array.
      IF( DCB_FRM( IDCB ) .NE. 'SIMPLE' .AND. ( .NOT. ACB_CUT( IACB ) )
     :    .AND. STATUS .EQ. SAI__OK ) THEN

*  Check that DELETE access is available for the array.
         CALL ARY1_CHACC( IACB, 'DELETE', STATUS )
         IF( STATUS .EQ. SAI__OK ) THEN

*  Primitive arrays.
*  =================
            IF( DCB_FRM( IDCB ) .EQ. 'PRIMITIVE' ) THEN
               CALL ARY1_DP2S( IDCB, STATUS )

*  Delta and scaled arrays.
*  =======================
            ELSE IF( DCB_FRM( IDCB ) .EQ. 'SCALED' .OR.
     :               DCB_FRM( IDCB ) .EQ. 'DELTA' ) THEN

*  Get the data type for the simple array.
               IF( DCB_FRM( IDCB ) .EQ. 'SCALED' ) THEN
                  CALL CMP_TYPE( DCB_SCLOC( IDCB ), 'SCALE', STYPE,
     :                           STATUS )
               ELSE
                  STYPE = DCB_TYP( IDCB )
               END IF

*  Get the dimensions of the array and the number of elements.
               EL = 1
               DO IDIM = 1, NDIM
                  DIM( IDIM ) = DCB_UBND( IDIM, IDCB ) -
     :                          DCB_LBND( IDIM, IDCB ) + 1
                  EL = EL * DIM( IDIM )
               END DO

*  Allocate space for a temporary array to hold a copy of the
*  external (i.e. uncompressed or unscaled) data values.
               CALL ARY1_CMTMP( STYPE, NDIM, DIM, LOCT, PNTR1, STATUS )
               IF( STATUS .EQ. SAI__OK ) THEN

*  Copy the external data values into this array. Do scaled arrays first.
                  IF( DCB_FRM( IDCB ) .EQ. 'SCALED' ) THEN

*  Ensure any required scale and zero terms are available.
                     CALL ARY1_DSCL( IDCB, STATUS )

*  Copy the whole DATA array into the temporary array, performing unscaling
*  in the process using the scale and zero in DCB_SCLOC.
                     CALL ARY1_GTN( .TRUE., DCB_TYP( IDCB ),
     :                              DCB_DLOC( IDCB ), NDIM,
     :                              DCB_LBND( 1, IDCB ),
     :                              DCB_UBND( 1, IDCB ),
     :                              DCB_LBND( 1, IDCB ),
     :                              DCB_UBND( 1, IDCB ), STYPE,
     :                              DCB_LBND( 1, IDCB ),
     :                              DCB_UBND( 1, IDCB ), .FALSE.,
     :                              DCB_SCLOC( IDCB ), PNTR1, DCE,
     :                              STATUS )

*  Reset the DCB data type to the external data type.
                     DCB_TYP( IDCB ) = STYPE

*  Annul any locator for scaling information in the DCB.
                     CALL DAT_ANNUL( DCB_SCLOC( IDCB ), STATUS )

*  Now do delta arrays.
                  ELSE
                     CALL ARY1_UNDLT( DCB_LOC( IDCB ), NDIM, LBND, DIM,
     :                                PNTR1, BAD, STATUS )
                  END IF
               END IF

*  Delete all SIMPLE and SCALED components from the data object.
               DO ICOMP = 1, NCOMP
                  CALL DAT_THERE( DCB_LOC( IDCB ), COMP( ICOMP ),
     :                            THERE, STATUS )
                  IF( THERE ) CALL DAT_ERASE( DCB_LOC( IDCB ),
     :                                        COMP( ICOMP ), STATUS )
               END DO

*  Annul any locator for the original DATA array in the DCB.
               IF( DCB_DLOC( IDCB ) .NE. DAT__NOLOC ) THEN
                  CALL DAT_ANNUL( DCB_DLOC( IDCB ), STATUS )
               END IF

*  Delete the original DATA array.
               CALL DAT_ERASE( DCB_LOC( IDCB ), 'DATA', STATUS )

*  Create a new DATA array inside the data object, then find it and store
*  the locator in the DCB.
               CALL DAT_NEW( DCB_LOC( IDCB ), 'DATA', STYPE,
     :                       NDIM, DIM, STATUS )
               CALL DAT_FIND( DCB_LOC( IDCB ), 'DATA', DCB_DLOC( IDCB ),
     :                        STATUS )

*  Map it for write access.
               CALL DAT_MAP(  DCB_DLOC( IDCB ), STYPE, 'WRITE',
     :                        NDIM, DIM, PNTR2, STATUS )

*  Copy the uncompressed values into the new DATA array.
*  ...byte data.
               IF( STYPE .EQ. '_BYTE' ) THEN
                  CALL VEC_BTOB( .FALSE., EL, %VAL( CNF_PVAL( PNTR1 ) ),
     :                           %VAL( CNF_PVAL( PNTR2 ) ), IERR, NERR,
     :                           STATUS )

*  ...unsigned byte data.
               ELSE IF( STYPE .EQ. '_UBYTE' ) THEN
                  CALL VEC_UBTOUB( .FALSE., EL, %VAL( CNF_PVAL(PNTR1) ),
     :                             %VAL( CNF_PVAL( PNTR2 ) ), IERR,
     :                             NERR, STATUS )

*  ...double precision data.
               ELSE IF( STYPE .EQ. '_DOUBLE' ) THEN
                  CALL VEC_DTOD( .FALSE., EL, %VAL( CNF_PVAL( PNTR1 ) ),
     :                           %VAL( CNF_PVAL( PNTR2 ) ), IERR, NERR,
     :                           STATUS )

*  ...integer data.
               ELSE IF( STYPE .EQ. '_INTEGER' ) THEN
                  CALL VEC_ITOI( .FALSE., EL, %VAL( CNF_PVAL( PNTR1 ) ),
     :                           %VAL( CNF_PVAL( PNTR2 ) ), IERR, NERR,
     :                           STATUS )

*  ...real data.
               ELSE IF( STYPE .EQ. '_REAL' ) THEN
                  CALL VEC_RTOR( .FALSE., EL, %VAL( CNF_PVAL( PNTR1 ) ),
     :                           %VAL( CNF_PVAL( PNTR2 ) ), IERR, NERR,
     :                           STATUS )

*  ...word data.
               ELSE IF( STYPE .EQ. '_WORD' ) THEN
                  CALL VEC_WTOW( .FALSE., EL, %VAL( CNF_PVAL( PNTR1 ) ),
     :                           %VAL( CNF_PVAL( PNTR2 ) ), IERR, NERR,
     :                           STATUS )

*  ...unsigned word data.
               ELSE IF( STYPE .EQ. '_UWORD' ) THEN
                  CALL VEC_UWTOUW( .FALSE., EL, %VAL( CNF_PVAL(PNTR1) ),
     :                             %VAL( CNF_PVAL( PNTR2 ) ), IERR,
     :                             NERR, STATUS )
               END IF

*  Annul the temporary copy of the mapped values.
               CALL ARY1_ANTMP( LOCT, STATUS )

*  Unmap the new DATA array.
               CALL DAT_UNMAP(  DCB_DLOC( IDCB ), STATUS )

*  Store the new storage form in the DCB.
               DCB_FRM( IDCB ) = 'SIMPLE'

*  Indicate state information is out of date.
               DCB_KSTA( IDCB ) = .FALSE.

*  If the form entry in the DCB was not recognised, then report an
*  error.
            ELSE IF( STATUS .EQ. SAI__OK ) THEN
               STATUS = ARY__FATIN
               CALL MSG_SETC( 'BADFORM', DCB_FRM( IDCB ) )
               CALL ERR_REP( 'ARY_SIMPLE_FORM', 'Unsupported array '//
     :                       'form ''^BADFORM'' found in Data Control'//
     :                       ' Block (internal programming error).',
     :                       STATUS )
            END IF
         END IF
      END IF

*  Arrive here if an error occurrs.
 999  CONTINUE

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_SIMPLE_ERR', 'ARY_SIMPLE: Error '//
     :                 'converting an array to simple form.', STATUS )
         CALL ARY1_TRACE( 'ARY_SIMPLE', STATUS )
      END IF

      END
