      SUBROUTINE ARY1_GTN( BAD, HTYPE, LOC, NDIM, LBNDD, UBNDD, LSUB,
     :                     USUB, ATYPE, LBNDA, UBNDA, PAD, SCLOC,
     :                     PNTR, DCE, STATUS )
*+
*  Name:
*     ARY1_GTN

*  Purpose:
*     Get an n-dimensional subregion from an HDS object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_GTN( BAD, HTYPE, LOC, NDIM, LBNDD, UBNDD, LSUB, USUB,
*                    ATYPE, LBNDA, UBNDA, PAD, SCLOC, PNTR, DCB, STATUS )

*  Description:
*     The routine extracts an n-dimensional subregion of any numeric
*     data type from a primitive numeric HDS array, making use of lower
*     and upper bounds information for both arrays. Data type conversion
*     and scaling is performed if necessary, with bad pixel testing if
*     required. Optionally, the surrounding region of the output array which
*     does not receive data may be padded with "bad" values. The output array
*     which receives the extracted data is passed by pointer.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether it is necessary to test for "bad" values during data
*        type conversion.
*     HTYPE = CHARACTER * ( * ) (Given)
*        The data type of the HDS object. This should be a primitive
*        numeric HDS data type string (case insensitive).
*     LOC = CHARACTER * ( * ) (Given)
*        Locator to the HDS object.
*     NDIM = INTEGER (Given)
*        Number of object dimensions.
*     LBNDD( NDIM ) = INTEGER (Given)
*        Lower bounds of the HDS object.
*     UBNDD( NDIM ) = INTEGER (Given)
*        Upper bounds of the HDS object.
*     LSUB( NDIM ) = INTEGER (Given)
*        Lower bounds of subregion to be extracted.
*     USUB( NDIM ) = INTEGER (Given)
*        Upper bounds of subregion to be extracted.
*     ATYPE = CHARACTER * ( * ) (Given)
*        The data type of the output array. This should be a primitive
*        numeric HDS data type string (case insensitive).
*     LBNDA( NDIM ) = INTEGER (Given)
*        Lower bounds of output array.
*     UBNDA( NDIM ) = INTEGER (Given)
*        Upper bounds of output array.
*     PAD = LOGICAL (Given)
*        Whether to fill regions of the output array which do not
*        receive data with "bad" values.
*     SCLOC = CHARACTER * ( * ) (Given)
*        Locator to an HDS object containing the scale and zero terms to
*        apply to the stored values. If this is DAT__NOLOC then no
*        scaling will be performed.
*     PNTR = INTEGER (Given)
*        Pointer to the output array which is to receive the extracted
*        data. The pointer value itself is not changed by this routine,
*        although the array elements are.
*     DCE = LOGICAL (Returned)
*        Whether an error occurred during data type comversion.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  It is assumed that the input data object and the output array
*     have the same number of dimensions. If this is not the case
*     intrinsically, then the NDIM argument should be set to match
*     whichever object has the larger dimensionality and the dimension
*     bounds of the other object (and possibly of the subregion also)
*     should be padded to match this dimensionality, normally with 1's.
*     It does not matter that the value of NDIM may not match the
*     actual dimensionality of the HDS object in such cases.
*     -  The lower and upper bounds of the subregion to be extracted
*     must lie within the bounds of both the input data object and the
*     output array, although the routine does not check for this.
*     -  The input data object must be suitable for vectorisation using
*     the HDS routine DAT_VEC.

*  Algorithm:
*     -  Check the data type string supplied via the ATYPE argument is
*     not too long and convert it to upper case.
*     -  Test this data type string against each permitted value in
*     turn, calling the appropriate routine to extract the data
*     subregion.
*     -  Note if the data type is not recognised.
*     -  If the data type string is not valid, then report an error.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
*     All Rights Reserved.
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council. All Rights Reserved.

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
*     DSB: David S Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     27-JUN-1989 (RFWS):
*        Original version.
*     7-SEP-1989 (RFWS):
*        Added message token for routine name, to prevent '$' from
*        affecting error messages.
*     22-MAR-1990 (RFWS):
*        Added further explanation to the notes section.
*     24-APR-2006 (DSB):
*        Added arguments SCLOC.
*     2012-05-07 (TIMJ):
*        Add _INT64 support.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNV_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Arguments Given:
      LOGICAL BAD
      CHARACTER * ( * ) HTYPE
      CHARACTER * ( * ) LOC
      INTEGER NDIM
      INTEGER LBNDD( NDIM )
      INTEGER UBNDD( NDIM )
      INTEGER LSUB( NDIM )
      INTEGER USUB( NDIM )
      CHARACTER * ( * ) ATYPE
      INTEGER LBNDA( NDIM )
      INTEGER UBNDA( NDIM )
      LOGICAL PAD
      CHARACTER * ( * ) SCLOC
      INTEGER PNTR

*  Arguments Returned:
      LOGICAL DCE

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      CHARACTER * ( ARY__SZTYP ) TYPE ! Data type of output array
      LOGICAL TYPOK              ! Whether the ATYPE argument is OK

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the string supplied for the ATYPE argument is not too
*  long.
      TYPE = ATYPE
      TYPOK = TYPE .EQ. ATYPE

*  If OK, then convert the string to upper case.
      IF ( TYPOK ) THEN
         CALL CHR_UCASE( TYPE )

*  Test the output data type string against each permitted value in
*  turn, calling the appropriate routine to extract the data subregion.
         IF ( TYPE .EQ. '_BYTE' ) THEN
            CALL ARY1_GTNB( BAD, HTYPE, LOC, NDIM, LBNDD, UBNDD,
     :                      LSUB, USUB, LBNDA, UBNDA, PAD, SCLOC,
     :                      %VAL( CNF_PVAL( PNTR ) ), DCE, STATUS )

         ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
            CALL ARY1_GTNUB( BAD, HTYPE, LOC, NDIM, LBNDD, UBNDD,
     :                       LSUB, USUB, LBNDA, UBNDA, PAD, SCLOC,
     :                       %VAL( CNF_PVAL( PNTR ) ), DCE, STATUS )

         ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
            CALL ARY1_GTND( BAD, HTYPE, LOC, NDIM, LBNDD, UBNDD,
     :                      LSUB, USUB, LBNDA, UBNDA, PAD, SCLOC,
     :                      %VAL( CNF_PVAL( PNTR ) ), DCE, STATUS )

         ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
            CALL ARY1_GTNI( BAD, HTYPE, LOC, NDIM, LBNDD, UBNDD,
     :                      LSUB, USUB, LBNDA, UBNDA, PAD, SCLOC,
     :                      %VAL( CNF_PVAL( PNTR ) ), DCE, STATUS )

         ELSE IF ( TYPE .EQ. '_REAL' ) THEN
            CALL ARY1_GTNR( BAD, HTYPE, LOC, NDIM, LBNDD, UBNDD,
     :                      LSUB, USUB, LBNDA, UBNDA, PAD, SCLOC,
     :                      %VAL( CNF_PVAL( PNTR ) ), DCE, STATUS )

         ELSE IF ( TYPE .EQ. '_WORD' ) THEN
            CALL ARY1_GTNW( BAD, HTYPE, LOC, NDIM, LBNDD, UBNDD,
     :                      LSUB, USUB, LBNDA, UBNDA, PAD, SCLOC,
     :                      %VAL( CNF_PVAL( PNTR ) ), DCE, STATUS )

         ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
            CALL ARY1_GTNUW( BAD, HTYPE, LOC, NDIM, LBNDD, UBNDD,
     :                       LSUB, USUB, LBNDA, UBNDA, PAD, SCLOC,
     :                       %VAL( CNF_PVAL( PNTR ) ), DCE, STATUS )

         ELSE IF ( TYPE .EQ. '_INT64' ) THEN
            CALL ARY1_GTNK( BAD, HTYPE, LOC, NDIM, LBNDD, UBNDD,
     :                      LSUB, USUB, LBNDA, UBNDA, PAD, SCLOC,
     :                      %VAL( CNF_PVAL( PNTR ) ), DCE, STATUS )


*  Note if the data type string is not recognised.
         ELSE
            TYPOK = .FALSE.
         END IF
      END IF

*  If the string supplied for the ATYPE argument is not valid, then
*  report an error.
      IF ( .NOT. TYPOK ) THEN
         STATUS = ARY__FATIN
         CALL MSG_SETC( 'ROUTINE', 'ARY1_GTN' )
         CALL MSG_SETC( 'BADATYPE', ATYPE )
         CALL ERR_REP( 'ARY1_GTN_TYPE',
     :   'Routine ^ROUTINE called with an invalid ATYPE argument ' //
     :   'of ''^BADATYPE'' (internal programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_GTN', STATUS )

      END
