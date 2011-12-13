      SUBROUTINE TRN1_CMPEX( EXPRS, NVAR, VARNAM, MXSYMB, GCODE,
     :                       MXCODE, MXCON, CODE, NCODE, CON, NCON,
     :                       MXSTK, WRK, STATUS )
*+
*  Name:
*     TRN1_CMPEX

*  Purpose:
*     Compile a transformation function expression.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_CMPEX( EXPRS, NVAR, VARNAM, MXSYMB, GCODE,
*                      MXCODE, MXCON, CODE, NCODE, CON, NCON,
*                      MXSTK, WRK, STATUS )

*  Description:
*     The routine checks and compiles an arithmetic expression
*     extracted from the right hand side of a transformation function.
*     If required, it produces a sequence of operation codes and a list
*     of numerical constants which may subsequently be used to evaluate
*     the function with the TRN1_EVEXx routines.  The operation codes
*     produced are defined in the include file TRN_CONST_STM.

*  Arguments:
*     EXPRS = CHARACTER * ( * ) (Given)
*        The input expression to be compiled.  This must be in upper
*        case, with no embedded blanks.
*     NVAR = INTEGER (Given)
*        The number of variable names defined for use in the
*        expression.
*     VARNAM( NVAR ) = CHARACTER * ( * ) (Given)
*        A list of the variable names.  These must be in upper case,
*        with no embedded blanks.
*     MXSYMB = INTEGER (Given)
*        The maximum number of symbols which may appear in the
*        expression.  This value specifies the size of the workspace
*        array WRK.  The only safe upper limit is the number of (non-
*        blank) characters in the input expression.
*     GCODE = LOGICAL (Given)
*        Whether the compilation is to produce a list of operation
*        codes and constants (or whether it is simply to check the
*        syntax and calculate the number of code and constant items
*        which will be produced).  If GCODE is .FALSE., the CODE, CON
*        and MXSTK arguments are not used.
*     MXCODE = INTEGER (Given)
*        The size of the CODE array.  The size required cannot be
*        larger than MXSYMB, but may be considerably smaller.  A
*        definite value can be obtained by calling the routine with
*        GCODE set to .FALSE. and using the value returned for NCODE.
*     MXCON = INTEGER (Given)
*        The size of the CON array.  The size required cannot be larger
*        than MXSYMB, but may be considerably smaller.  A definite
*        value can be obtained by calling the routine with GCODE set to
*        .FALSE. and using the value returned for NCON.
*     CODE( MXCODE ) = INTEGER (Returned)
*        On successful exit, if GCODE is .TRUE., this array contains a
*        list of the operation codes produced.  The first item in the
*        list is a count of the number which follow.
*     NCODE = INTEGER (Returned)
*        The total number of items returned in the CODE array
*        (including the initial item).  If GCODE is .FALSE., the same
*        value is still returned, although no items will actually be
*        placed into the CODE array.
*     CON( MXCON ) = DOUBLE PRECISION (Returned)
*        On successful exit, if GCODE is .TRUE., this array contains a
*        list of the constants produced.  This list includes all the
*        constants explicitly appearing in the input expression, in
*        left-right order.  It also includes identification numbers for
*        all the variables identified (at the position where they occur
*        in the expression) and counts of the number of arguments
*        supplied for built-in functions which accept a variable number
*        of arguments.  The position of the latter "constants" is
*        determined by the position of the closing parenthesis which
*        terminates the function call.
*     NCON = INTEGER (Returned)
*        The total number of values returned in the CON array.  If
*        GCODE is .FALSE., the same number is still returned, although
*        no values will actually be placed into the CON array.
*     MXSTK = INTEGER (Returned)
*        The maximum size of the arithmetic stack required to evaluate
*        the expression.  This value may be used to allocate workspace
*        for the TRN1_EVEXx routines.  No value is returned unless
*        GCODE is .TRUE.
*     WRK( MXSYMB, 3 ) = INTEGER (Returned)
*        A workspace array.  See the description of the MXSYMB argument
*        for how to determine its size.
*     STATUS = INTEGER (Given & Returned)
*        Standard error status argument.

*  Algorithm:
*     The routine passes through the input expression searching for
*     "symbols".  It looks for "standard" symbols (arithmetic
*     operators, parentheses, function calls and delimiters) in the
*     next part of the expression to be parsed, using identification
*     information stored in the symbol data table, which is accessed
*     via common blocks.  It ignores certain symbols, according to
*     whether they appear to be "operators" or "operands".  The choice
*     depends on what the previous symbol was; for instance, two
*     operators may not occur in succession.  Unary (+ and -) operators
*     are also ignored in situations where they are not permitted.

*     If a standard symbol is found, it is passed to the routine
*     TRN1_VSYMB, which keeps track of the current level of parenthesis
*     in the expression and of the number of arguments supplied to any
*     outstanding (possibly nested) function calls.  This routine then
*     accepts or rejects the symbol according to whether it is valid
*     within the current context.  An error is reported if it is
*     rejected.

*     If the part of the expression currently being parsed did not
*     contain a "standard" symbol, an attempt is made to parse it first
*     as a constant, then as a variable name.  If either of these
*     succeeds, an appropriate symbol number is added to the list of
*     symbols identified so far, and a value is added to the list of
*     constants - this is either the value of the constant itself, or
*     the identification number of the variable.  If the expression
*     cannot be parsed, an error is reported.

*     When the entire expression has been analysed as a sequence of
*     symbols (and associated constants), the routine TRN1_EVSRT is
*     called.  This sorts the symbols into "evaluation order", which is
*     the order in which the associated operations must be performed on
*     a push-down arithmetic stack to evaluate the expression.  This
*     routine also substitutes operation codes (defined in the include
*     file TRN_CONST_STM) for the symbol numbers and calculates the
*     maximum size of evaluation stack which will be required.

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
*     2-JUN-1988 (RFWS):
*        Installed new version of TRN1_PSCON and finished tidying the
*        code and prologue.
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
      INCLUDE 'TRN_CONST_STM'    ! TRN_ private constants for use by
                                 ! routines which process standard
                                 ! transformation modules
      INCLUDE 'TRN_ERR'          ! TRN_ error codes

*  Arguments Given:
      CHARACTER * ( * ) EXPRS    ! Input expression
      INTEGER NVAR               ! Number of variables
      CHARACTER * ( * ) VARNAM( NVAR )
                                 ! Names of variables
      INTEGER MXCODE             ! Maximum number of output code items
                                 ! to generate (size of the CODE array)
      INTEGER MXCON              ! Maximum number of output constants to
                                 ! generate (size of the CON array)
      INTEGER MXSYMB             ! Determines the size of the workspace
                                 ! array - must be at least equal to the
                                 ! number of symbols which will appear
                                 ! in the input expression
      LOGICAL GCODE              ! Whether to generate code and
                                 ! constants lists

*  Arguments Returned:
      INTEGER CODE( MXCODE )     ! Output list of operation codes
      INTEGER NCODE              ! Number of operation codes generated
      DOUBLE PRECISION CON( MXCON )
                                 ! Output list of numerical constants
      INTEGER NCON               ! Number of constants generated
      INTEGER MXSTK              ! Stack size required to evaluate the
                                 ! expression with the TRN1_EVEXx
                                 ! routines
      INTEGER WRK( MXSYMB, 3 )   ! Workspace array

*  Status:
      INTEGER STATUS             ! Error status

*  External References:
      INTEGER CHR_LEN            ! Length of a string with trailing
                                 ! blanks removed
      EXTERNAL TRN1_SYMDT        ! BLOCK DATA routine to initialise the
                                 ! symbol data table held in the common
                                 ! blocks in the include file
                                 ! TRN_CMN_SYM

*  Global Variables:
      INCLUDE 'TRN_CMN_SYM'      ! Symbol data table common blocks

*  Local Constants:
      INTEGER SYMLST             ! Workspace element for storing a list
      PARAMETER ( SYMLST = 1 )   ! of symbol numbers
      INTEGER OPNSYM             ! Workspace element for storing the
      PARAMETER ( OPNSYM = 2 )   ! symbols which open each level of
                                 ! parenthesis
      INTEGER ARGCNT             ! Workspace element for storing
                                 ! function
      PARAMETER ( ARGCNT = 3 )   ! argument counts for each level of
                                 ! parenthesis

*  Local Variables:
      LOGICAL OPNEXT             ! Whether the next symbol is expected
                                 ! to be an operator (seen from the
                                 ! left), rather than an operand
      LOGICAL UNEXT              ! Whether the next symbol may be a
                                 ! unary (+ or -) operator
      LOGICAL FOUND              ! Symbol identified in input
                                 ! expression?
      INTEGER NCIN               ! Number of non-blank characters in the
                                 ! input expression
      INTEGER SYM                ! Symbol data table entry number
      INTEGER NSYMB              ! Number of symbols identified in the
                                 ! expression
      INTEGER ISTART             ! Start of a symbol in the expression
      INTEGER IEND               ! End of a symbol in the expression
      INTEGER IVAR               ! Variable identification number
      INTEGER LPAR               ! Parenthesis level
      DOUBLE PRECISION CONST     ! Variable for value of parsed
                                 ! constants

*.

*  Check status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Perform initialisation.

*  ...number of constants produced:
      NCON = 0

*  ...number of symbols found:
      NSYMB = 0

*  ...parenthesis level:
      LPAR = 0

*  Find the number of non-blank characters in the input expression.
      NCIN = MAX( CHR_LEN( EXPRS ), 1 )

*  The first symbol to be encountered must not look like an operator
*  from the left.  It may be a unary + or - operator.
      OPNEXT = .FALSE.
      UNEXT = .TRUE.

*  Search through the expression to classify each symbol which appears
*  in it.  Stop when there are no more input characters or an error is
*  detected.
      ISTART = 1
      DO WHILE( ( ISTART .LE. NCIN ) .AND. ( STATUS .EQ. SAI__OK ) )

*  Compare each of the "standard" symbols in the symbol data table
*  with the next section of the expression, stopping if a match is
*  found.
        SYM = 0
        FOUND = .FALSE.
        DO WHILE( ( SYM .LT. SYM_MXSYM ) .AND. ( .NOT. FOUND ) )
          SYM = SYM + 1

*  Only consider symbols which look like operators or operands from
*  the left, according to the setting of the OPNEXT flag.  Thus, if
*  an operator or operand is missing from the input expression, the
*  next symbol will not be identified, because it will be of the wrong
*  type.  Also exclude unary (+ and -) operators if they are out of
*  context.
          IF( ( ( SYM_OPERL( SYM ) .EQ. 1 ) .EQV. OPNEXT ) .AND.
     :        ( ( SYM_UNOPR( SYM ) .NE. 1 ) .OR. UNEXT ) ) THEN
            IEND = MIN( ISTART + SYM_SIZE( SYM ) - 1, NCIN )
            FOUND = ( SYM_NAME( SYM ) .EQ. EXPRS( ISTART : IEND ) )
          ENDIF
        ENDDO

*  If the symbol was identified as one of the "standard" symbols, then
*  validate it, updating the parenthesis level and argument count
*  information at the same time.
        IF( FOUND ) THEN
          CALL TRN1_VSYMB( EXPRS( : NCIN ), IEND, GCODE, MXSYMB, MXCON,
     :                     SYM, LPAR,
     :                     WRK( 1, ARGCNT ), WRK( 1, OPNSYM ),
     :                     CON, NCON, STATUS )

*  If it was not one of the "standard" symbols, then check if the next
*  symbol was expected to be an operator.  If so, then there is a
*  missing operator, so report an error.
        ELSE
          IF ( OPNEXT ) THEN
            STATUS = TRN__MIOPR
            CALL MSG_SETC( 'EXPRS', EXPRS( : ISTART ) )
            CALL ERR_REP( 'TRN1_CMPEX_MIOPR',
     :                    'Missing or invalid operator in the ' //
     :                    'expression ''^EXPRS''... (possible ' //
     :                    'programming error).', STATUS )

*  If the next symbol was expected to be an operand, then it may be a
*  constant, so try to parse it as one.  If successful, set the symbol
*  number to LDCON (load constant on to evaluation stack) and add the
*  constant to the list of constants (unless GCODE is .FALSE.).
          ELSE
            CALL TRN1_PSCON( EXPRS( : NCIN ), ISTART, CONST, IEND,
     :                       STATUS )
            IF ( ( IEND .NE. 0 ) .AND. ( STATUS .EQ. SAI__OK ) ) THEN
              SYM = SYM_LDCON
              NCON = NCON + 1
              IF( GCODE ) CON( NCON ) = CONST

*  If the symbol did not parse as a constant (even an invalid one),
*  then it may be a variable name, so try to parse it as one.  If
*  successful, set the symbol to LDVAR (load variable on to evaluation
*  stack) and add the variable identification number to the list of
*  constants (unless GCODE is .FALSE.).
            ELSE IF( STATUS .EQ. SAI__OK ) THEN
              CALL TRN1_PSVAR( EXPRS, ISTART, NVAR, VARNAM, IVAR,
     :                         IEND, STATUS )
              IF( ( IVAR .NE. 0 ) .AND. ( STATUS .EQ. SAI__OK ) ) THEN
                SYM = SYM_LDVAR
                NCON = NCON + 1
                IF( GCODE ) CON( NCON ) = DBLE( IVAR )

*  If the expression did not parse as a variable name (even an invalid
*  one), then there is a missing operand in the expression.  Report an
*  error.
              ELSE IF( STATUS .EQ. SAI__OK ) THEN
                STATUS = TRN__MIOPA
                CALL MSG_SETC( 'EXPRS', EXPRS( : ISTART ) )
                CALL ERR_REP( 'TRN1_CMPEX_MIS1',
     :                        'Missing or invalid operand in the ' //
     :                        'expression ''^EXPRS''... (possible ' //
     :                        'programming error).', STATUS )
              ENDIF
            ENDIF
          ENDIF
        ENDIF

*  If there is no error, then the next symbol in the input expression
*  has been identified and is valid.  Add its symbol data table entry
*  number to the end of the list of identified symbols and advance the
*  "start of symbol" pointer to the next character in the input
*  expression.
        IF( STATUS .EQ. SAI__OK ) THEN
          NSYMB = NSYMB + 1
          WRK( NSYMB, SYMLST ) = SYM
          ISTART = IEND + 1

*  Decide whether the next symbol should look like an operator or an
*  operand from the left.  This is determined by the nature of the
*  symbol just identified (seen from the right) - two operands or two
*  operators cannot be adjacent.
          OPNEXT = ( SYM_OPERR( SYM ) .NE. 1 )

*  Also decide whether the next symbol may be a unary (+ or -)
*  operator, according to the UNEXT symbol data table entry for the
*  symbol just identified.
          UNEXT = ( SYM_UNEXT( SYM ) .EQ. 1 )
        ENDIF
      ENDDO

*  If there is no error, check the final context after classifying all
*  the symbols...
      IF( STATUS .EQ. SAI__OK ) THEN

*  If an operand is still expected, then there is an unsatisfied
*  operator on the end of the expression, so report an error.
        IF( .NOT. OPNEXT ) THEN
          STATUS = TRN__MIOPA
          CALL MSG_SETC( 'EXPRS', EXPRS( : NCIN ) )
          CALL ERR_REP( 'TRN1_CMPEX_MIS2',
     :                  'Missing or invalid operand in the ' //
     :                  'expression ''^EXPRS'' (possible ' //
     :                  'programming error ).', STATUS )

*  If the final parenthesis level is positive, then there is a missing
*  right parenthesis, so report an error.
        ELSE IF( LPAR .GT. 0 ) THEN
          STATUS = TRN__MRPAR
          CALL MSG_SETC( 'EXPRS', EXPRS( : NCIN ) )
          CALL ERR_REP( 'TRN1_CMPEX_MRPAR',
     :                  'Missing right parenthesis in the ' //
     :                  'expression ''^EXPRS'' (posssible ' //
     :                  'programming error).', STATUS )
        ENDIF
      ENDIF

*  Sort the symbols into evaluation order to produce output operation
*  codes (no codes are generated if GCODE is .FALSE., but the sort must
*  still be performed to determine the number of codes which would be
*  produced).
      CALL TRN1_EVSRT( GCODE, MXSYMB, MXCODE, MXCON, CON, NSYMB,
     :                 WRK( 1, SYMLST ), CODE, NCODE, MXSTK, STATUS )


*  If there is no error, put the number of code items which follow at
*  the start of the output code stream (unless GCODE is .FALSE.).
      IF( ( STATUS .EQ. SAI__OK ) .AND. GCODE ) CODE( 1 ) = NCODE - 1

*  Exit routine.
      END
