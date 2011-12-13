      SUBROUTINE TRN1_CLEXP( NEXP, DEFIN, EXPOUT, DEF, STATUS )
*+
*  Name:
*     TRN1_CLEXP

*  Purpose:
*     Clean (and check) expressions.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_CLEXP( NEXP, DEFIN, EXPOUT, DEF, STATUS )

*  Description:
*     The routine extracts expressions from the right hand sides of a
*     set of function definitions and cleans the resulting strings by
*     removing embedded blanks and converting to upper case.  The
*     expressions are not themselves validated, but the routine checks
*     to see if any are missing.  If all are present DEF is assigned
*     the value TRN_DS_DEF, if all are absent DEF is assigned the value
*     TRN_DS_UDEF, if only some are present then STATUS is set and an
*     error is reported.  The constants used for setting DEF values are
*     defined in the include file TRN_CONST.

*  Copyright:
*     Copyright (C) 1988, 1992 Science & Engineering Research Council.
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
*     {enter_new_authors_here}

*  History:
*     12-MAY-1988 (RFWS):
*        Original version.
*     13-FEB-1992 (RFWS):
*        Improved the error reporting.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_CONST'        ! TRN_ private constants
      INCLUDE 'TRN_ERR'          ! TRN_ error codes

*  Arguments Given:
      INTEGER NEXP               ! Number of expressions to process
      CHARACTER * ( * ) DEFIN( NEXP ) ! Input function definitions

*  Arguments Returned:
      CHARACTER * ( * ) EXPOUT( NEXP ) ! Validated expressions
      INTEGER DEF                ! Transformation definition status

*  Status:
      INTEGER STATUS             ! Error status

*  Local Variables:
      INTEGER I                  ! Loop counter for indexing arrays
      INTEGER IUD                ! First undefined function index
      INTEGER NUD                ! Number of undefined expressions
      INTEGER STEXP              ! Position of start of an expression

*.

*  Check status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Initialise the count of undefined expressions.
      NUD = 0

*  Loop to process each expression until they are all done or an error
*  is detected.
      I = 0
      DO WHILE( ( I .LT. NEXP ) .AND. ( STATUS .EQ. SAI__OK ) )
        I = I + 1

*  Find the start of the expression field in the input function
*  definition from the delimiting '='.
        STEXP = INDEX( DEFIN( I ), '=' )
        IF( STEXP .NE. 0 ) STEXP = STEXP + 1

*  If there is no '=' sign, count an undefined expression.  If it is
*  the first one, remember it.  Clear the corresponding output element.
        IF( STEXP .EQ. 0 ) THEN
          NUD = NUD + 1
          IF( NUD .EQ. 1 ) IUD = I
          EXPOUT( I ) = ' '

*  If the '=' sign is present, but there is no expression to the right
*  of it, report an error.
        ELSE IF( STEXP .GT. LEN( DEFIN( I ) ) ) THEN
          STATUS = TRN__EXPUD
          CALL MSG_SETC( 'EXPRS', DEFIN( I ) )
          CALL ERR_REP( 'TRN1_CLEXP_RHS1',
     :                  'Missing right hand side in the ' //
     :                  'expression ''^EXPRS'' (possible ' //
     :                  'programming error).', STATUS )

        ELSE IF( DEFIN( I )( STEXP : ) .EQ. ' ' ) THEN
          STATUS = TRN__EXPUD
          CALL MSG_SETC( 'EXPRS', DEFIN( I ) )
          CALL ERR_REP( 'TRN1_CLEXP_RHS2',
     :                  'Missing right hand side in the ' //
     :                  'expression ''^EXPRS'' (possible ' //
     :                  'programming error).', STATUS )

*  If the expression is present, extract it and clean it.
        ELSE
          EXPOUT( I ) = DEFIN( I )( STEXP : )
          CALL CHR_RMBLK( EXPOUT( I ) )
          CALL CHR_UCASE( EXPOUT( I ) )
        ENDIF
      ENDDO

*  If there was no error cleaning the expressions, then if all the
*  expressions are present, the transformation is defined.
      IF( STATUS .EQ. SAI__OK ) THEN
        IF( NUD .EQ. 0 ) THEN
          DEF = TRN_DS_DEF

*  If all the expressions are absent, the transformation is undefined.
        ELSE IF( NUD .EQ. NEXP ) THEN
          DEF = TRN_DS_UDEF

*  If only some are present, report an error citing the first one which
*  was absent.
        ELSE
          STATUS = TRN__EXPUD
          CALL MSG_SETC( 'EXPRS', DEFIN( IUD ) )
          CALL ERR_REP( 'TRN1_CLEXP_UNDEF',
     :                  'Function definition missing in the ' //
     :                  'expression ''^EXPRS'' (possible ' //
     :                  'programming error).', STATUS )
        ENDIF
      ENDIF

*  Exit routine.
      END
