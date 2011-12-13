      SUBROUTINE NDF1_CVTB( BAD, N, TYPE, PNTR, RESULT, DCE, STATUS )
*+
*  Name:
*     NDF1_CVTB

*  Purpose:
*     Convert a vectorised array to BYTE.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_CVTB( BAD, N, TYPE, PNTR, RESULT, DCE, STATUS )

*  Description:
*     The routine converts a vectorised array of any primitive numeric
*     type to a data type of BYTE, checking for the presence of bad
*     pixels if required. The input array is passed by pointer; the
*     output array is passed directly.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether to check for bad pixel values.
*     N = INTEGER (Given)
*        Number of array elements to convert.
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type of the input vectorised array. This value must be
*        one of the HDS primitive numeric data type strings and must be
*        supplied in upper case.
*     PNTR = INTEGER (Given)
*        Pointer to the input vectorised array.
*     RESULT( N ) = BYTE (Returned)
*        The output BYTE vectorised array, to contain the converted
*        values.
*     DCE = LOGICAL (Returned)
*        Whether any data conversion errors occurred (bad values are
*        entered into the affected elements of the output array if this
*        happens).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise and mark the error stack.
*     -  Test the input data type specification against each valid value
*     in turn, calling the appropriate data conversion routine.
*     -  Note if the data type specification is not valid.
*     -  If a data conversion error ocurred, note this fact and annul
*     any error reports this might have generated.
*     -  Release the error stack.
*     -  If the input data type specification was not valid, then report
*     an error.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     2-NOV-1990 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      LOGICAL BAD
      INTEGER N
      CHARACTER * ( * ) TYPE
      INTEGER PNTR

*  Arguments Returned:
      BYTE RESULT
      LOGICAL DCE

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
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

*  Test for each valid input data type in turn and call the appropriate
*  conversion routine.
      IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL VEC_BTOB( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL VEC_UBTOB( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                     IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL VEC_DTOB( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL VEC_ITOB( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL VEC_RTOB( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL VEC_WTOB( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL VEC_UWTOB( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                     IERR, NERR, STATUS )

*  Note if the input data type specified is not valid.
      ELSE
         TYPOK = .FALSE.
      END IF

*  If a data conversion error occurred, then annul any error reports it
*  might have produced.
      DCE = NERR .NE. 0
      IF ( DCE ) CALL ERR_ANNUL( STATUS )
      CALL ERR_RLSE

*  Report an error if the input data type specified was not valid.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF( .NOT. TYPOK ) THEN
            STATUS = NDF__FATIN
            CALL MSG_SETC( 'ROUTINE', 'NDF1_CVTB' )
            CALL MSG_SETC( 'BADTYPE', TYPE )
            CALL ERR_REP( 'NDF1_CVTB_TYPE',
     :      'Routine ^ROUTINE called with an invalid TYPE ' //
     :      'argument of ''^BADTYPE'' (internal programming error).',
     :      STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_CVTB',
     :STATUS )

      END
      SUBROUTINE NDF1_CVTUB( BAD, N, TYPE, PNTR, RESULT, DCE, STATUS )
*+
*  Name:
*     NDF1_CVTUB

*  Purpose:
*     Convert a vectorised array to UNSIGNED BYTE.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_CVTUB( BAD, N, TYPE, PNTR, RESULT, DCE, STATUS )

*  Description:
*     The routine converts a vectorised array of any primitive numeric
*     type to a data type of UNSIGNED BYTE, checking for the presence of bad
*     pixels if required. The input array is passed by pointer; the
*     output array is passed directly.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether to check for bad pixel values.
*     N = INTEGER (Given)
*        Number of array elements to convert.
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type of the input vectorised array. This value must be
*        one of the HDS primitive numeric data type strings and must be
*        supplied in upper case.
*     PNTR = INTEGER (Given)
*        Pointer to the input vectorised array.
*     RESULT( N ) = BYTE (Returned)
*        The output UNSIGNED BYTE vectorised array, to contain the converted
*        values.
*     DCE = LOGICAL (Returned)
*        Whether any data conversion errors occurred (bad values are
*        entered into the affected elements of the output array if this
*        happens).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise and mark the error stack.
*     -  Test the input data type specification against each valid value
*     in turn, calling the appropriate data conversion routine.
*     -  Note if the data type specification is not valid.
*     -  If a data conversion error ocurred, note this fact and annul
*     any error reports this might have generated.
*     -  Release the error stack.
*     -  If the input data type specification was not valid, then report
*     an error.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-NOV-1990 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      LOGICAL BAD
      INTEGER N
      CHARACTER * ( * ) TYPE
      INTEGER PNTR

*  Arguments Returned:
      BYTE RESULT
      LOGICAL DCE

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
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

*  Test for each valid input data type in turn and call the appropriate
*  conversion routine.
      IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL VEC_BTOUB( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL VEC_UBTOUB( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                     IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL VEC_DTOUB( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL VEC_ITOUB( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL VEC_RTOUB( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL VEC_WTOUB( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL VEC_UWTOUB( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                     IERR, NERR, STATUS )

*  Note if the input data type specified is not valid.
      ELSE
         TYPOK = .FALSE.
      END IF

*  If a data conversion error occurred, then annul any error reports it
*  might have produced.
      DCE = NERR .NE. 0
      IF ( DCE ) CALL ERR_ANNUL( STATUS )
      CALL ERR_RLSE

*  Report an error if the input data type specified was not valid.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF( .NOT. TYPOK ) THEN
            STATUS = NDF__FATIN
            CALL MSG_SETC( 'ROUTINE', 'NDF1_CVTUB' )
            CALL MSG_SETC( 'BADTYPE', TYPE )
            CALL ERR_REP( 'NDF1_CVTUB_TYPE',
     :      'Routine ^ROUTINE called with an invalid TYPE ' //
     :      'argument of ''^BADTYPE'' (internal programming error).',
     :      STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_CVTUB',
     :STATUS )

      END
      SUBROUTINE NDF1_CVTD( BAD, N, TYPE, PNTR, RESULT, DCE, STATUS )
*+
*  Name:
*     NDF1_CVTD

*  Purpose:
*     Convert a vectorised array to DOUBLE PRECISION.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_CVTD( BAD, N, TYPE, PNTR, RESULT, DCE, STATUS )

*  Description:
*     The routine converts a vectorised array of any primitive numeric
*     type to a data type of DOUBLE PRECISION, checking for the presence of bad
*     pixels if required. The input array is passed by pointer; the
*     output array is passed directly.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether to check for bad pixel values.
*     N = INTEGER (Given)
*        Number of array elements to convert.
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type of the input vectorised array. This value must be
*        one of the HDS primitive numeric data type strings and must be
*        supplied in upper case.
*     PNTR = INTEGER (Given)
*        Pointer to the input vectorised array.
*     RESULT( N ) = DOUBLE PRECISION (Returned)
*        The output DOUBLE PRECISION vectorised array, to contain the converted
*        values.
*     DCE = LOGICAL (Returned)
*        Whether any data conversion errors occurred (bad values are
*        entered into the affected elements of the output array if this
*        happens).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise and mark the error stack.
*     -  Test the input data type specification against each valid value
*     in turn, calling the appropriate data conversion routine.
*     -  Note if the data type specification is not valid.
*     -  If a data conversion error ocurred, note this fact and annul
*     any error reports this might have generated.
*     -  Release the error stack.
*     -  If the input data type specification was not valid, then report
*     an error.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-NOV-1990 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      LOGICAL BAD
      INTEGER N
      CHARACTER * ( * ) TYPE
      INTEGER PNTR

*  Arguments Returned:
      DOUBLE PRECISION RESULT
      LOGICAL DCE

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
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

*  Test for each valid input data type in turn and call the appropriate
*  conversion routine.
      IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL VEC_BTOD( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL VEC_UBTOD( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                     IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL VEC_DTOD( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL VEC_ITOD( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL VEC_RTOD( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL VEC_WTOD( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL VEC_UWTOD( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                     IERR, NERR, STATUS )

*  Note if the input data type specified is not valid.
      ELSE
         TYPOK = .FALSE.
      END IF

*  If a data conversion error occurred, then annul any error reports it
*  might have produced.
      DCE = NERR .NE. 0
      IF ( DCE ) CALL ERR_ANNUL( STATUS )
      CALL ERR_RLSE

*  Report an error if the input data type specified was not valid.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF( .NOT. TYPOK ) THEN
            STATUS = NDF__FATIN
            CALL MSG_SETC( 'ROUTINE', 'NDF1_CVTD' )
            CALL MSG_SETC( 'BADTYPE', TYPE )
            CALL ERR_REP( 'NDF1_CVTD_TYPE',
     :      'Routine ^ROUTINE called with an invalid TYPE ' //
     :      'argument of ''^BADTYPE'' (internal programming error).',
     :      STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_CVTD',
     :STATUS )

      END
      SUBROUTINE NDF1_CVTI( BAD, N, TYPE, PNTR, RESULT, DCE, STATUS )
*+
*  Name:
*     NDF1_CVTI

*  Purpose:
*     Convert a vectorised array to INTEGER.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_CVTI( BAD, N, TYPE, PNTR, RESULT, DCE, STATUS )

*  Description:
*     The routine converts a vectorised array of any primitive numeric
*     type to a data type of INTEGER, checking for the presence of bad
*     pixels if required. The input array is passed by pointer; the
*     output array is passed directly.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether to check for bad pixel values.
*     N = INTEGER (Given)
*        Number of array elements to convert.
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type of the input vectorised array. This value must be
*        one of the HDS primitive numeric data type strings and must be
*        supplied in upper case.
*     PNTR = INTEGER (Given)
*        Pointer to the input vectorised array.
*     RESULT( N ) = INTEGER (Returned)
*        The output INTEGER vectorised array, to contain the converted
*        values.
*     DCE = LOGICAL (Returned)
*        Whether any data conversion errors occurred (bad values are
*        entered into the affected elements of the output array if this
*        happens).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise and mark the error stack.
*     -  Test the input data type specification against each valid value
*     in turn, calling the appropriate data conversion routine.
*     -  Note if the data type specification is not valid.
*     -  If a data conversion error ocurred, note this fact and annul
*     any error reports this might have generated.
*     -  Release the error stack.
*     -  If the input data type specification was not valid, then report
*     an error.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-NOV-1990 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      LOGICAL BAD
      INTEGER N
      CHARACTER * ( * ) TYPE
      INTEGER PNTR

*  Arguments Returned:
      INTEGER RESULT
      LOGICAL DCE

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
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

*  Test for each valid input data type in turn and call the appropriate
*  conversion routine.
      IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL VEC_BTOI( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL VEC_UBTOI( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                     IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL VEC_DTOI( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL VEC_ITOI( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL VEC_RTOI( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL VEC_WTOI( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL VEC_UWTOI( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                     IERR, NERR, STATUS )

*  Note if the input data type specified is not valid.
      ELSE
         TYPOK = .FALSE.
      END IF

*  If a data conversion error occurred, then annul any error reports it
*  might have produced.
      DCE = NERR .NE. 0
      IF ( DCE ) CALL ERR_ANNUL( STATUS )
      CALL ERR_RLSE

*  Report an error if the input data type specified was not valid.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF( .NOT. TYPOK ) THEN
            STATUS = NDF__FATIN
            CALL MSG_SETC( 'ROUTINE', 'NDF1_CVTI' )
            CALL MSG_SETC( 'BADTYPE', TYPE )
            CALL ERR_REP( 'NDF1_CVTI_TYPE',
     :      'Routine ^ROUTINE called with an invalid TYPE ' //
     :      'argument of ''^BADTYPE'' (internal programming error).',
     :      STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_CVTI',
     :STATUS )

      END
      SUBROUTINE NDF1_CVTR( BAD, N, TYPE, PNTR, RESULT, DCE, STATUS )
*+
*  Name:
*     NDF1_CVTR

*  Purpose:
*     Convert a vectorised array to REAL.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_CVTR( BAD, N, TYPE, PNTR, RESULT, DCE, STATUS )

*  Description:
*     The routine converts a vectorised array of any primitive numeric
*     type to a data type of REAL, checking for the presence of bad
*     pixels if required. The input array is passed by pointer; the
*     output array is passed directly.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether to check for bad pixel values.
*     N = INTEGER (Given)
*        Number of array elements to convert.
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type of the input vectorised array. This value must be
*        one of the HDS primitive numeric data type strings and must be
*        supplied in upper case.
*     PNTR = INTEGER (Given)
*        Pointer to the input vectorised array.
*     RESULT( N ) = REAL (Returned)
*        The output REAL vectorised array, to contain the converted
*        values.
*     DCE = LOGICAL (Returned)
*        Whether any data conversion errors occurred (bad values are
*        entered into the affected elements of the output array if this
*        happens).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise and mark the error stack.
*     -  Test the input data type specification against each valid value
*     in turn, calling the appropriate data conversion routine.
*     -  Note if the data type specification is not valid.
*     -  If a data conversion error ocurred, note this fact and annul
*     any error reports this might have generated.
*     -  Release the error stack.
*     -  If the input data type specification was not valid, then report
*     an error.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-NOV-1990 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      LOGICAL BAD
      INTEGER N
      CHARACTER * ( * ) TYPE
      INTEGER PNTR

*  Arguments Returned:
      REAL RESULT
      LOGICAL DCE

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
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

*  Test for each valid input data type in turn and call the appropriate
*  conversion routine.
      IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL VEC_BTOR( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL VEC_UBTOR( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                     IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL VEC_DTOR( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL VEC_ITOR( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL VEC_RTOR( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL VEC_WTOR( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL VEC_UWTOR( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                     IERR, NERR, STATUS )

*  Note if the input data type specified is not valid.
      ELSE
         TYPOK = .FALSE.
      END IF

*  If a data conversion error occurred, then annul any error reports it
*  might have produced.
      DCE = NERR .NE. 0
      IF ( DCE ) CALL ERR_ANNUL( STATUS )
      CALL ERR_RLSE

*  Report an error if the input data type specified was not valid.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF( .NOT. TYPOK ) THEN
            STATUS = NDF__FATIN
            CALL MSG_SETC( 'ROUTINE', 'NDF1_CVTR' )
            CALL MSG_SETC( 'BADTYPE', TYPE )
            CALL ERR_REP( 'NDF1_CVTR_TYPE',
     :      'Routine ^ROUTINE called with an invalid TYPE ' //
     :      'argument of ''^BADTYPE'' (internal programming error).',
     :      STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_CVTR',
     :STATUS )

      END
      SUBROUTINE NDF1_CVTW( BAD, N, TYPE, PNTR, RESULT, DCE, STATUS )
*+
*  Name:
*     NDF1_CVTW

*  Purpose:
*     Convert a vectorised array to WORD.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_CVTW( BAD, N, TYPE, PNTR, RESULT, DCE, STATUS )

*  Description:
*     The routine converts a vectorised array of any primitive numeric
*     type to a data type of WORD, checking for the presence of bad
*     pixels if required. The input array is passed by pointer; the
*     output array is passed directly.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether to check for bad pixel values.
*     N = INTEGER (Given)
*        Number of array elements to convert.
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type of the input vectorised array. This value must be
*        one of the HDS primitive numeric data type strings and must be
*        supplied in upper case.
*     PNTR = INTEGER (Given)
*        Pointer to the input vectorised array.
*     RESULT( N ) = INTEGER*2 (Returned)
*        The output WORD vectorised array, to contain the converted
*        values.
*     DCE = LOGICAL (Returned)
*        Whether any data conversion errors occurred (bad values are
*        entered into the affected elements of the output array if this
*        happens).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise and mark the error stack.
*     -  Test the input data type specification against each valid value
*     in turn, calling the appropriate data conversion routine.
*     -  Note if the data type specification is not valid.
*     -  If a data conversion error ocurred, note this fact and annul
*     any error reports this might have generated.
*     -  Release the error stack.
*     -  If the input data type specification was not valid, then report
*     an error.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-NOV-1990 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      LOGICAL BAD
      INTEGER N
      CHARACTER * ( * ) TYPE
      INTEGER PNTR

*  Arguments Returned:
      INTEGER*2 RESULT
      LOGICAL DCE

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
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

*  Test for each valid input data type in turn and call the appropriate
*  conversion routine.
      IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL VEC_BTOW( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL VEC_UBTOW( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                     IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL VEC_DTOW( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL VEC_ITOW( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL VEC_RTOW( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL VEC_WTOW( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL VEC_UWTOW( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                     IERR, NERR, STATUS )

*  Note if the input data type specified is not valid.
      ELSE
         TYPOK = .FALSE.
      END IF

*  If a data conversion error occurred, then annul any error reports it
*  might have produced.
      DCE = NERR .NE. 0
      IF ( DCE ) CALL ERR_ANNUL( STATUS )
      CALL ERR_RLSE

*  Report an error if the input data type specified was not valid.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF( .NOT. TYPOK ) THEN
            STATUS = NDF__FATIN
            CALL MSG_SETC( 'ROUTINE', 'NDF1_CVTW' )
            CALL MSG_SETC( 'BADTYPE', TYPE )
            CALL ERR_REP( 'NDF1_CVTW_TYPE',
     :      'Routine ^ROUTINE called with an invalid TYPE ' //
     :      'argument of ''^BADTYPE'' (internal programming error).',
     :      STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_CVTW',
     :STATUS )

      END
      SUBROUTINE NDF1_CVTUW( BAD, N, TYPE, PNTR, RESULT, DCE, STATUS )
*+
*  Name:
*     NDF1_CVTUW

*  Purpose:
*     Convert a vectorised array to UNSIGNED WORD.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_CVTUW( BAD, N, TYPE, PNTR, RESULT, DCE, STATUS )

*  Description:
*     The routine converts a vectorised array of any primitive numeric
*     type to a data type of UNSIGNED WORD, checking for the presence of bad
*     pixels if required. The input array is passed by pointer; the
*     output array is passed directly.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether to check for bad pixel values.
*     N = INTEGER (Given)
*        Number of array elements to convert.
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type of the input vectorised array. This value must be
*        one of the HDS primitive numeric data type strings and must be
*        supplied in upper case.
*     PNTR = INTEGER (Given)
*        Pointer to the input vectorised array.
*     RESULT( N ) = INTEGER*2 (Returned)
*        The output UNSIGNED WORD vectorised array, to contain the converted
*        values.
*     DCE = LOGICAL (Returned)
*        Whether any data conversion errors occurred (bad values are
*        entered into the affected elements of the output array if this
*        happens).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise and mark the error stack.
*     -  Test the input data type specification against each valid value
*     in turn, calling the appropriate data conversion routine.
*     -  Note if the data type specification is not valid.
*     -  If a data conversion error ocurred, note this fact and annul
*     any error reports this might have generated.
*     -  Release the error stack.
*     -  If the input data type specification was not valid, then report
*     an error.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-NOV-1990 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      LOGICAL BAD
      INTEGER N
      CHARACTER * ( * ) TYPE
      INTEGER PNTR

*  Arguments Returned:
      INTEGER*2 RESULT
      LOGICAL DCE

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
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

*  Test for each valid input data type in turn and call the appropriate
*  conversion routine.
      IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL VEC_BTOUW( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL VEC_UBTOUW( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                     IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL VEC_DTOUW( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL VEC_ITOUW( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL VEC_RTOUW( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL VEC_WTOUW( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                    IERR, NERR, STATUS )

      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL VEC_UWTOUW( BAD, N, %VAL( CNF_PVAL( PNTR ) ), RESULT,
     :                     IERR, NERR, STATUS )

*  Note if the input data type specified is not valid.
      ELSE
         TYPOK = .FALSE.
      END IF

*  If a data conversion error occurred, then annul any error reports it
*  might have produced.
      DCE = NERR .NE. 0
      IF ( DCE ) CALL ERR_ANNUL( STATUS )
      CALL ERR_RLSE

*  Report an error if the input data type specified was not valid.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF( .NOT. TYPOK ) THEN
            STATUS = NDF__FATIN
            CALL MSG_SETC( 'ROUTINE', 'NDF1_CVTUW' )
            CALL MSG_SETC( 'BADTYPE', TYPE )
            CALL ERR_REP( 'NDF1_CVTUW_TYPE',
     :      'Routine ^ROUTINE called with an invalid TYPE ' //
     :      'argument of ''^BADTYPE'' (internal programming error).',
     :      STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_CVTUW',
     :STATUS )

      END
