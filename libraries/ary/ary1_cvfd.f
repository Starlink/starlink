      SUBROUTINE ARY1_CVFD( BAD, N, ARRAY, TYPE, PNTR, DCE, STATUS )
*+
*  Name:
*     ARY1_CVFD

*  Purpose:
*     Convert a vectorised array from DOUBLE PRECISION.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_CVFD( BAD, N, ARRAY, TYPE, PNTR, DCE, STATUS )

*  Description:
*     The routine converts a vectorised array from DOUBLE PRECISION to any
*     primitive numeric type, checking for the presence of bad pixels
*     if required. The input array is passed directly; the output array
*     is passed by pointer.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether to check for bad pixel values.
*     N = INTEGER (Given)
*        Number of array elements to convert.
*     ARRAY( N ) = DOUBLE PRECISION (Given)
*        The input DOUBLE PRECISION vectorised array containing the values to be
*        converted.
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type of the output vectorised array. This value must
*        be one of the HDS primitive numeric data type strings and must
*        be supplied in upper case.
*     PNTR = INTEGER (Given)
*        Pointer to the output vectorised array.
*     DCE = LOGICAL (Returned)
*        Whether any data conversion errors occurred (bad values are
*        entered into the affected elements of the output array if this
*        happens).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise and mark the error stack.
*     -  Test the output data type specification against each valid
*     value in turn, calling the appropriate data conversion routine.
*     -  Note if the data type specification is not valid.
*     -  If a data conversion error occurred, then note this fact and
*     annul any error reports this might have generated.
*     -  Release the error stack.
*     -  If the output data type specification was not valid, then
*     report an error.

*  Copyright:
*     Copyright (C) 1989, 1993 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-JUL-1989 (RFWS):
*        Original version.
*     30-AUG-1989 (RFWS):
*        Changed to use message token to prevent '$' affecting error
*        messages.
*     6-APR-1993 (RFWS):
*        Changed routine name.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNV_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Arguments Given:
      LOGICAL BAD
      INTEGER N
      DOUBLE PRECISION ARRAY
      CHARACTER * ( * ) TYPE
      INTEGER PNTR

*  Arguments Returned:
      LOGICAL DCE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IERR               ! Position of first conversion error
      INTEGER NERR               ! Number of conversion errors
      LOGICAL TYPOK              ! Whether the TYPE argument is valid

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise and mark the error stack.
      TYPOK = .TRUE.
      NERR = 0
      CALL ERR_MARK

*  Test for each valid output data type in turn and call the
*  appropriate conversion routine.
      IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL VEC_DTOB( BAD, N, ARRAY, %VAL( CNF_PVAL( PNTR ) ), IERR,
     :                  NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL VEC_DTOUB( BAD, N, ARRAY, %VAL( CNF_PVAL( PNTR ) ), IERR,
     :                   NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL VEC_DTOD( BAD, N, ARRAY, %VAL( CNF_PVAL( PNTR ) ), IERR,
     :                  NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL VEC_DTOI( BAD, N, ARRAY, %VAL( CNF_PVAL( PNTR ) ), IERR,
     :                  NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL VEC_DTOR( BAD, N, ARRAY, %VAL( CNF_PVAL( PNTR ) ), IERR,
     :                  NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL VEC_DTOW( BAD, N, ARRAY, %VAL( CNF_PVAL( PNTR ) ), IERR,
     :                  NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL VEC_DTOUW( BAD, N, ARRAY, %VAL( CNF_PVAL( PNTR ) ), IERR,
     :                   NERR, STATUS )

*  Note if the output data type specified is not valid.
      ELSE
         TYPOK = .FALSE.
      END IF

*  If a data conversion error occurred, then annul any error reports it
*  might have produced.
      DCE = NERR .NE. 0
      IF ( DCE ) CALL ERR_ANNUL( STATUS )
      CALL ERR_RLSE

*  Report an error if the output data type specified was not valid.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF( .NOT. TYPOK ) THEN
            STATUS = ARY__FATIN
            CALL MSG_SETC( 'ROUTINE', 'ARY1_CVFD' )
            CALL MSG_SETC( 'BADTYPE', TYPE )
            CALL ERR_REP( 'ARY1_CVFD_TYPE',
     :      'Routine ^ROUTINE called with an invalid TYPE ' //
     :      'argument of ''^BADTYPE'' (internal programming error).',
     :      STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_CVFD',
     :                                             STATUS )

      END
