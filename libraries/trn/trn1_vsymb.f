      SUBROUTINE TRN1_VSYMB( EXPRS, IEND, GCODE, MXSYMB, MXCON, SYM,
     :                       LPAR, ARGCNT, OPNSYM, CON, NCON, STATUS )
*+
*  Name:
*     TRN1_VSYMB

*  Purpose:
*     Validate a symbol in an expression.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_VSYMB( EXPRS, IEND, GCODE, MXSYMB, MXCON, SYM,
*                      LPAR, ARGCNT, OPNSYM, CON, NCON, STATUS )

*  Description:
*     The routine validates an identified "standard" symbol during
*     compilation of an expression from the right hand side of a
*     transformation function definition.  Its main task is to keep
*     track of the level of parenthesis in the expression and to count
*     the number of arguments supplied to functions at each level of
*     parenthesis (for nested function calls).  On this basis it is
*     able to interpret and accept or reject symbols which represent
*     function calls, parentheses and delimiters.  Other symbols are
*     accepted automatically.

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
*     25-MAY-1988 (RFWS):
*        Original version.
*     13-FEB-1992 (RFWS):
*        Improved the error reporting.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_ERR'          ! TRN_ error codes


*  Arguments Given:
      CHARACTER * ( * ) EXPRS   ! The expression being parsed - this is
                                ! only used by this routine for
                                ! reporting errors
      INTEGER IEND              ! The position of the final character of
                                ! the current symbol in EXPRS
      LOGICAL GCODE             ! Whether code (and constants) are being
                                ! generated during the compilation
      INTEGER MXSYMB            ! Size of the ARGCNT and OPNSYM arrays
      INTEGER MXCON             ! Size of the CON array
      INTEGER SYM               ! The symbol data table entry number of
                                ! the current symbol, which is to be
                                ! validated


*  Arguments Given and Returned:
      INTEGER LPAR              ! The current level of parenthesis
      INTEGER ARGCNT( MXSYMB )  ! Array containing the number of
                                ! function arguments encountered so far
                                ! at each level of parenthesis
      INTEGER OPNSYM( MXSYMB )  ! Array containing the symbol data
                                ! table entry number of the symbol
                                ! which opened each level of
                                ! parenthesis
      DOUBLE PRECISION CON( MXCON )
                                ! The list of constants being generated
                                ! during the compilation (not used if
                                ! GCODE is .FALSE.)
      INTEGER NCON              ! The number of constants generated so
                                ! far


*  Arguments Returned:
*     <declarations and descriptions for exported arguments>


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
      EXTERNAL TRN1_SYMDT       ! BLOCK DATA routine for initialising
                                ! the symbol data table in the common
                                ! blocks defined in the include file
                                ! TRN_CMN_SYM


*  Global Variables:
      INCLUDE 'TRN_CMN_SYM'     ! Symbol data table common blocks


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
*     <declarations for local variables>


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Check if the symbol is a comma.
      IF( SYM .EQ. SYM_COMMA ) THEN


*   A comma is only used to delimit function arguments.  If the current
*   level of parenthesis is zero, we cannot be inside a function
*   argument list, so report an error.
        IF( LPAR .EQ. 0 ) THEN
          STATUS = TRN__DELIN
          CALL MSG_SETC( 'EXPRS', EXPRS( : IEND ) )
          CALL ERR_REP( 'TRN1_VSYMB_COM1',
     :                  'Spurious comma encountered in the ' //
     :                  'expression ''^EXPRS''... (possible ' //
     :                  'programming error).', STATUS )


*   A comma is not valid if the symbol which opened the current level of
*   parenthesis was not a function call.  This is indicated by an
*   argument count of zero at the current level of parenthesis.  Report
*   the same error if this occurs.
        ELSE IF( ARGCNT( LPAR ) .EQ. 0 ) THEN
          STATUS = TRN__DELIN
          CALL MSG_SETC( 'EXPRS', EXPRS( : IEND ) )
          CALL ERR_REP( 'TRN1_VSYMB_COM2',
     :                  'Spurious comma encountered in the ' //
     :                  'expression ''^EXPRS''... (possible ' //
     :                  'programming error).', STATUS )


*   If a comma is valid, then increment the argument count at the
*   current level of parenthesis.
        ELSE
          ARGCNT( LPAR ) = ARGCNT( LPAR ) + 1
        ENDIF


*   If the symbol is not a comma, check if it increases the current
*   level of parenthesis.
      ELSE IF( SYM_DPAR( SYM ) .GT. 0 ) THEN


*   Increment the level of parenthesis and initialise the argument
*   count at the new level.  This count is set to zero if the symbol
*   which opens the parenthesis level is not a function call (indicated
*   by a zero NARGS entry in the symbol data table), and it subsequently
*   remains at zero.  If the symbol is a function call, the argument
*   count is initially set to 1 and increments whenever a comma is
*   encountered at this parenthesis level.
        LPAR = LPAR + 1
        IF( SYM_NARGS( SYM ) .EQ. 0 ) THEN
          ARGCNT( LPAR ) = 0
        ELSE
          ARGCNT( LPAR ) = 1
        ENDIF


*   Remember the symbol which opened this parenthesis level.
        OPNSYM( LPAR ) = SYM


*   Check if the symbol decreases the current parenthesis level.
      ELSE IF( SYM_DPAR( SYM ) .LT. 0 ) THEN


*   Ensure that the parenthesis level is not already at zero.  If it is,
*   then there is a missing left parenthesis in the expression being
*   compiled, so report an error.
        IF( LPAR .EQ. 0 ) THEN
          STATUS = TRN__MLPAR
          CALL MSG_SETC( 'EXPRS', EXPRS( : IEND ) )
          CALL ERR_REP( 'TRN1_VSYMB_COM1',
     :                  'Missing left parenthesis in the ' //
     :                  'expression ''^EXPRS''... (possible ' //
     :                  'programming error).', STATUS )


*   If the parenthesis level is valid and the symbol which opened this
*   level of parenthesis was a function call with a fixed number of
*   arguments (indicated by a positive NARGS entry in the symbol data
*   table), then we must check the number of function arguments which
*   have been encountered. Report an error if the number of arguments
*   is wrong.
        ELSE IF( SYM_NARGS( OPNSYM( LPAR ) ) .GT. 0 ) THEN
          IF( ARGCNT( LPAR ) .NE. SYM_NARGS( OPNSYM( LPAR )  ) ) THEN
            STATUS = TRN__WRNFA
            CALL MSG_SETC( 'EXPRS', EXPRS( : IEND ) )
            CALL ERR_REP( 'TRN1_VSYMB_NARG1',
     :                    'Wrong number of function arguments in ' //
     :                    'the expression ''^EXPRS''... (possible ' //
     :                    'programming error).', STATUS )


*   If the number of arguments is valid, decrement the parenthesis
*   level.
          ELSE
            LPAR = LPAR - 1
          ENDIF


*   If the symbol which opened this level of parenthesis was a function
*   call with a variable number of arguments (indicated by a negative
*   NARGS entry in the symbol data table), then we must check and
*   process the number of function arguments.
        ELSE IF( SYM_NARGS( OPNSYM( LPAR ) ) .LT. 0 ) THEN


*   Check that the minimum required number of arguments have been
*   supplied.  Report an error if they have not.
          IF( ARGCNT( LPAR ) .LT.
     :        ( -SYM_NARGS( OPNSYM( LPAR ) ) ) ) THEN
            STATUS = TRN__WRNFA
            CALL MSG_SETC( 'EXPRS', EXPRS( : IEND ) )
            CALL ERR_REP( 'TRN1_VSYMB_NARG2',
     :                    'Insufficient function arguments in ' //
     :                    'the expression ''^EXPRS''... (possible ' //
     :                    'programming error).', STATUS )


*   If the number of arguments is valid, add the argument count to the
*   end of the list of constants (unless GCODE is .FALSE.) and
*   decrement the parenthesis level.
          ELSE
            NCON = NCON + 1
            IF( GCODE ) CON( NCON ) = DBLE( ARGCNT( LPAR ) )
            LPAR = LPAR - 1
          ENDIF


*   Finally, if the symbol which opened this level of parenthesis was
*   not a function call (NARGS entry in the symbol data table is zero),
*   then decrement the parenthesis level.  In this case there is no
*   need to check the argument count, because it will not have been
*   incremented.
        ELSE
          LPAR = LPAR - 1
        ENDIF


*   End of "the symbol decreases the parenthesis level" condition.
      ENDIF


*   Exit routine.
      END
