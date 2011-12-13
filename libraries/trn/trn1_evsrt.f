      SUBROUTINE TRN1_EVSRT( GCODE, MXSYMB, MXCODE, MXCON, CON, NSYMB,
     :                       SYMLST, CODE, NCODE, MXSTK, STATUS )








*+
*  Name:
*     TRN1_EVSRT

*  Purpose:
*     perform an evaluation order sort on expression symbols.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_EVSRT( GCODE, MXSYMB, MXCODE, MXCON, CON, NSYMB,
*                      SYMLST, CODE, NCODE, MXSTK, STATUS )

*  Description:
*     The routine sorts a sequence of numbers representing symbols
*     identified in a transformation function expression.  The symbols
*     (i.e. the expression syntax) must have been fully validated before
*     calling this routine, as no validation is performed here.
*        The symbols are sorted into the order in which corresponding
*     operations must be performed on a push-down arithmetic stack in
*     order to evaluate the expression.  Operation codes (defined in
*     the include file TRN_CONST_STM) are then substituted for the
*     symbol numbers.

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council.
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
*     R.F. Warren-Smith (DUVAD::RFWS)
*     {enter_new_authors_here}

*  History:
*     25-MAY-1988:  Original version (DUVAD::RFWS)
*     2-JUN-1988:  Finished tidying code & prologue (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_CONST_STM'    ! Private TRN_ constants for use by
                                 ! routines which process standard
                                 ! transformation modules


*  Arguments Given:
      LOGICAL GCODE             ! Whether code (and constants) are being
                                ! generated
      INTEGER MXSYMB            ! Size of the SYMLST array
      INTEGER MXCODE            ! Size of the CODE array
      INTEGER MXCON             ! Size of the CON array
      DOUBLE PRECISION CON( MXCON )
                                ! List of constants (not used if GCODE
                                ! is .FALSE.)
      INTEGER NSYMB             ! Number of symbols to be processed in
                                ! the SYMLST array


*  Arguments Given and Returned:
      INTEGER SYMLST( MXSYMB )  ! Array containing the list of symbol
                                ! numbers to be sorted - also used as
                                ! workspace to hold a push-down stack


*  Arguments Returned:
      INTEGER CODE( MXCODE )    ! List of operation codes produced (not
                                ! used if GCODE is .FALSE.)
      INTEGER NCODE             ! Number of operation codes produced
      INTEGER MXSTK             ! Maximum stack size required when
                                ! evaluating the expression (not used
                                ! if GCODE is .FALSE.)


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
      EXTERNAL TRN1_SYMDT       ! BLOCK DATA routine for initialising
                                ! the symbol data table in the common
                                ! blocks in the TRN_CMN_SYM file


*  Global Variables:
      INCLUDE 'TRN_CMN_SYM'     ! Common blocks containing symbol data
                                ! table


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
      LOGICAL PUSH              ! Whether to push a new symbol on to
                                ! the stack
      LOGICAL FLUSH             ! Whether to flush a parenthesised
                                ! symbol sequence from the stack
      INTEGER ISYMB             ! Input symbol counter
      INTEGER ICON              ! Input constant counter
      INTEGER SYM               ! Variable for symbol number
      INTEGER TOS               ! "Top of sort stack" pointer
      INTEGER STKSZ             ! Evaluation stack size


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Perform initialisation.

*   ...flag for flushing symbols in parentheses from the sort stack:
      FLUSH = .FALSE.

*   ...next symbol to be obtained from the input symbol list:
      ISYMB=1

*   ...number of input constants used:
      ICON = 0

*   ...the sort stack pointer:
      TOS=0

*   ...next element of the output code array to fill:
      NCODE=1

*   ...evaluation stack size:
      STKSZ = 0
      IF( GCODE ) MXSTK = 0


*   Loop to generate output operation codes until the sort stack is
*   empty and there are no further symbols to process, or an error is
*   detected.
      DO WHILE ( ( ( TOS .GT. 0 ) .OR. ( ISYMB .LE. NSYMB ) ) .AND.
     :           ( STATUS .EQ. SAI__OK ) )


*   Decide whether to push a symbol on to the sort stack (which
*   "diverts" it so that higher-priority symbols can be output), or to
*   pop the top symbol off the sort stack and send it to the output
*   stream...


*   We must push a symbol on to the sort stack if the stack is currently
*   empty.
        IF( TOS .EQ. 0 ) THEN
          PUSH = .TRUE.


*   We must pop the top symbol off the sort stack if there are no more
*   input symbols to process.
        ELSE IF( ISYMB .GT. NSYMB ) THEN
          PUSH = .FALSE.


*   If the sort stack is being flushed to complete the evaluation of a
*   parenthesised expression, then the top symbol (which will be the
*   opening parenthesis or function call) must be popped.  This is only
*   done once, so reset the FLUSH flag before the next loop.
        ELSE IF( FLUSH ) THEN
          PUSH = .FALSE.
          FLUSH = .FALSE.


*   In all other circumstances, we must push a symbol on to the sort
*   stack if its evaluation priority (seen from the left) is higher
*   than that of the current top of stack symbol (seen from the right).
*   This means it will eventually be sent to the output stream ahead of
*   the current top of stack symbol.
        ELSE
          PUSH = ( SYM_LPRI( SYMLST( ISYMB ) ) .GT.
     :             SYM_RPRI( SYMLST( TOS ) ) )
        ENDIF


*   If a symbol is being pushed on to the sort stack, then get the next
*   input symbol which is to be used.
        IF( PUSH ) THEN
          SYM = SYMLST( ISYMB )
          ISYMB = ISYMB + 1


*   If the symbol decreases the parenthesis level (a closing
*   parenthesis), then all the sort stack entries down to the symbol
*   which opened the current level of parenthesis (the matching opening
*   parenthesis or function call) will already have been sent to the
*   output stream as a consequence of the evaluation priority defined
*   for a closing parenthesis in the symbol data table.  The opening
*   parenthesis (or function call) must next be flushed from the sort
*   stack, so set the FLUSH flag which is interpreted on the next loop.
*   Ignore the current symbol, which cancels with the opening
*   parenthesis on the stack.
          IF( SYM_DPAR( SYM ) .LT. 0 ) THEN
            FLUSH = .TRUE.


*   All other symbols are pushed on to the sort stack.  The stack
*   occupies that region of the SYMLST array from which the input
*   symbol numbers have already been extracted.
          ELSE
            TOS = TOS + 1
            SYMLST( TOS ) = SYM
          ENDIF


*   If a symbol is being popped from the top of the sort stack, then
*   the top of stack entry is transferred to the output stream.  Obtain
*   the symbol number from the stack.  Increment the local constant
*   counter if the associated operation will use a constant.
        ELSE
          SYM = SYMLST( TOS )
          TOS = TOS - 1
          IF( ( SYM .EQ. SYM_LDVAR ) .OR. ( SYM .EQ. SYM_LDCON ) )
     :      ICON = ICON + 1


*   If the output symbol does not represent a "null" operation, add its
*   operation code to the end of the output code list (unless GCODE is
*   .FALSE.)
          IF( SYM_OPCOD( SYM ) .NE. TRN_OP_NULL ) THEN
            NCODE = NCODE + 1
            IF( GCODE ) CODE( NCODE ) = SYM_OPCOD( SYM )


*   Increment/decrement the counter representing the stack size required
*   for evaluation of the expression and note the maximum size of this
*   stack.  If the symbol is a function with a variable number of
*   arguments (indicated by a negative NARGS entry in the symbol data
*   table), then the change in stack size must be determined from the
*   argument number stored in the constant table.  This is only done if
*   GCODE is .TRUE., since otherwise the constant list doesn't exist.
            IF( GCODE ) THEN
              IF( SYM_NARGS( SYM ) .GE. 0 ) THEN
                STKSZ = STKSZ + SYM_DSTK( SYM )
              ELSE
                ICON = ICON + 1
                STKSZ = STKSZ - ( NINT( CON( ICON ) ) - 1 )
              ENDIF
              MXSTK = MAX( MXSTK, STKSZ )
            ENDIF


*   End of "the output symbol does not represent a null operation"
*   condition.
          ENDIF


*   End of "the stack is being popped" condition.
        ENDIF


*   End of "loop until the stack is empty..." loop.
      ENDDO


*   Exit routine.
      END
