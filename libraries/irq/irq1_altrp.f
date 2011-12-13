      SUBROUTINE IRQ1_ALTRP( MXOPCO, MXQNAM, EXPRES, OPCODE, NOPC,
     :                       QNAMES, NQNAME, ERRPNT, STATUS )
*+
*  Name:
*     IRQ1_ALTRP

*  Purpose:
*     Converts an algebraic quality expression to a reverse-Polish form.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_ALTRP( MXOPCO, MXQNAM, EXPRES, OPCODE, NOPC, QNAMES,
*                      NQNAME, ERRPNT, STATUS )

*  Description:
*     The given quality expression is converted to upper case and any
*     leading blanks are removed. It is then split into "symbols", each
*     symbol being either one of the recognised logical operators (eg
*     .AND., .NOT. etc ), a logical constant (.TRUE. or .FALSE.), or
*     a quality name. A check is made that operators and operands occur
*     in the right order. Integer op.codes representing the separate
*     symbols are initially stored in the same order that they occur
*     in the quality expression. Each occurrence of a quality name is
*     replaced by a "LOAD QUALITY" op. code, and the quality name to
*     which the op. code refers is stored in the QNAMES array.
*
*     When the entire expression has been analysed as a sequence of
*     symbols (and associated constants and quality names), they are
*     sorted into "evaluation order", which is the order in which the
*     associated operations must be performed on a "First In, Last Out"
*     (FILO) stack to evaluate the expression.
*
*     If a syntax error is found which can be located within the
*     quality expression (IRQ__BADSY, IRQ__MSOPT or IRQ__MSOPD ), a
*     pointer is returned which identifies where the error occurred in
*     the returned version of the quality expression. If STATUS is not
*     returned equal to any of these three values, then ERRPNT is
*     returned zero.

*  Arguments:
*     MXOPCO = INTEGER (Given)
*        The size of the OPCODE array argument.
*     MXQNAM = INTEGER (Given)
*        The size of the QNAMES array arguments.
*     EXPRES = CHARACTER * ( * ) (Given and Returned)
*        The quality expression. On exit, it is converted to upper case
*        and any unneeded blanks are removed.
*     OPCODE( MXOPCO ) = INTEGER (Returned)
*        The integer codes representing the operations to be performed
*        on a FILO stack in order to evaluate the quality expression.
*     NOPC = INTEGER (Returned)
*        No. of operation codes returned in array OPCODE.
*     QNAMES( MXQNAM ) = CHARACTER * ( * ) (Returned)
*        The quality names referenced in the quality expression. These
*        are listed in the same order in which they are required to
*        evaluate the expression, i.e. QNAMES(1) is used by the first
*        "LOAD QUALITY" op. code stored in OPCODE; QNAMES(2) is used by
*        the second such op. code, etc.
*     NQNAME = INTEGER (Returned)
*        The number of quality names referenced in the quality
*        expression and returned in array QNAMES.
*     ERRPNT = INTEGER (Returned)
*        The offset within the returned quality expression (EXPRES) at
*        which certain errors were detected. These errors have
*        associated STATUS values IRQ__BADSY ("Unrecognised logical
*        operator or constant"), IRQ__MSOPT ("Missing operator") and
*        IRQ__MSOPD ("Missing operand"). ERRPNT is returned zero if none
*        of these errors are found.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-JUN-1991 (DSB):
*        Original version. Based on EDRS routine ALGTRP, written by R.F.
*        Warren-Smith.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants.
      INCLUDE 'IRQ_PAR'          ! IRQ constants.
      INCLUDE 'IRQ_ERR'          ! IRQ error values.

*  Arguments Given:
      INTEGER MXOPCO
      INTEGER MXQNAM

*  Arguments Given and Returned:
      CHARACTER EXPRES*(*)

*  Arguments Returned:
      INTEGER OPCODE( MXOPCO )
      INTEGER NOPC
      CHARACTER QNAMES( MXQNAM )*(*)
      INTEGER NQNAME
      INTEGER ERRPNT

*  Status:
      INTEGER STATUS             ! Global status

*  Include definitions of logical operations supported by IRQ.
      INCLUDE 'IRQ_OPC'

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Retuirns used length of a string.

*  Local Variables:
      CHARACTER C*1              ! This character
      INTEGER D1                 ! Index of first delimiting dot in a
                                 ! pair of dots.
      INTEGER D2                 ! Index of second delimiting dot in a
                                 ! pair of dots.
      LOGICAL FOUND              ! True if the next part of the quality
                                 ! expression is a recognised symbol.
      INTEGER I                  ! Loop count.
      INTEGER INST               ! Integer code representing an
                                 ! instruction.
      INTEGER INSTR( IRQ__NSYMS )! Instructions corresponding to each
                                 ! symbol contained in the input
                                 ! expression, in the order they
                                 ! appear in the input expression.
      INTEGER IOP                ! Index into an array of instructions.
      INTEGER ITERM              ! Position of given potential quality
                                 ! name terminator.
      INTEGER LNONSP             ! Index of last non-space character
      INTEGER LSYM               ! No. of characters in current symbol.
      INTEGER NCIN               ! No. of used characters in IN.
      INTEGER NSYMB              ! No. of recognised symbols found so
                                 ! far.
      INTEGER OFFSET             ! Position within input expression from
                                 ! which next character will be read.
      LOGICAL OPNEXT             ! True if the next symbol must be an
                                 ! operator.
      CHARACTER QEXP*(IRQ__SZQEX)! The quality expression with an
                                 ! appended "=" symbol.
      INTEGER QNMEND             ! Position of end of a quality name.
      INTEGER STACK( 0:IRQ__NSYMS )! "First In Last Out"  stack used for
                                 ! temporary storage of symbols.
      INTEGER START              ! Start of rest of input string.
      INTEGER STKSIZ             ! Current size of evaluation stack.
      CHARACTER STRING*(OPC__MXSSZ)! Candidate logical operator or
                                 ! constant.
      INTEGER TERM               ! Position of quality name terminator.
      INTEGER TOS                ! Current position of top of stack.

*.

*  Initialise the returned syntax error pointer to zero.
      ERRPNT = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert input expression to upper case, remove leading blanks,
*  and find used length.
      CALL CHR_UCASE( EXPRES )
      CALL CHR_LDBLK( EXPRES )
      NCIN = CHR_LEN( EXPRES )

*  Check that the expression is not too long (there must be room for an
*  "=" symbol to be appended to the end of the expression).
      IF( NCIN .GE. IRQ__SZQEX ) THEN
         STATUS = IRQ__QEXPL
         CALL ERR_REP( 'IRQ1_ALTRP_ERR1',
     :            'IRQ1_ALTRP: Supplied quality expression is too long',
     :                 STATUS )
         GO TO 999
      END IF

*  If the expression is "ANY", the compiled quality expression consists
*  of the two instructions "LOAD TRUE", "RETURN".
      IF( EXPRES(:NCIN) .EQ. 'ANY' ) THEN
         OPCODE( 1 ) = OPC__LDT
         OPCODE( 2 ) = OPC__RET
         NOPC = 2
         NQNAME = 0
         GO TO 999

      END IF

*  Remove spaces from around dots.
      LNONSP = 0
      D2 = 1
      DO D1 = 1, NCIN
         C = EXPRES( D1:D1 )
         IF( C .EQ. '.' ) THEN
            LNONSP = LNONSP + 1
            EXPRES( LNONSP:LNONSP ) = '.'
            D2 = LNONSP + 1
         ELSE
            IF( C .NE. ' ' ) THEN
               LNONSP = LNONSP + 1
               EXPRES( D2:D2 ) = C
               D2 = D2 + 1
            ELSE IF( EXPRES( LNONSP:LNONSP ) .NE. '.' ) THEN
               EXPRES( D2:D2 ) = C
               D2 = D2 + 1
            END IF
         END IF
      END DO

      IF( LNONSP .LT. NCIN ) THEN
         EXPRES( LNONSP + 1: NCIN ) = ' '
         NCIN = LNONSP
      END IF

*  Check that any dots in the string delimit recognised symbols (dots
*  are not allowed in quality names).
      START = 1
 10   CONTINUE

*  Find the next dot. This should be the first dot of a pair of dots
*  delimiting a recognised symbol.
      D1 = INDEX( EXPRES( START : ), '.' )
      IF( D1 .NE. 0 ) THEN
         D1 = D1 + START - 1

*  Find the second dot in the pair. If no second dot exists, report an
*  error.
         D2 = INDEX( EXPRES( D1 + 1: ), '.' )
         IF( D2 .EQ. 0 ) THEN
            STATUS = IRQ__MSDOT
            CALL ERR_REP( 'IRQ1_ALTRP_ERR2',
     :                 'IRQ1_ALTRP: Missing delimiter character - "."',
     :                  STATUS )
            GO TO 999
         END IF
         D2 = D2 + D1

*  Check that the dot-delimited string corresponds to one of the
*  recognised symbols.
         FOUND = .FALSE.

         IF( D2 .GT. D1 + 1 ) THEN
            IF( EXPRES( D1 + 1 : D2 - 1 ) .NE. ' ' ) THEN

               STRING = EXPRES( D1:D2 )

               DO I = 1, IRQ__MXINS
                  IF( STRING .EQ. OPC_SYM( I ) ) FOUND = .TRUE.
               END DO

            END IF
         END IF

*  If it didn't, report an error.
         IF( .NOT. FOUND ) THEN
            STATUS = IRQ__BADSY
            CALL MSG_SETC( 'STRING', EXPRES( D1:D2 ) )
            CALL ERR_REP( 'IRQ1_ALTRP_ERR3',
     :       'IRQ1_ALTRP: Bad logical operator or constant - "^STRING"',
     :                    STATUS )
            ERRPNT = D1
            GO TO 999
         END IF

*  Go round and check for any more dots.
         START = D2 + 1
         GO TO 10

      END IF

*  The string has passed the "dots" test. Append an '=' operator to
*  terminate the expression.
      QEXP = EXPRES( :NCIN )//'='
      NCIN = NCIN + 1

*  Initialise counters.
      NQNAME = 0
      OFFSET = 1
      NSYMB = 0

*  OPNEXT indicates if an operator is expected next. The first entity
*  must not look like an operator.
      OPNEXT = .FALSE.

*  Work through the input expression, until an '=' symbol is reached.
*  This symbol corresponds to the RET(urn) instruction.
      INST = OPC__NULL
      DO WHILE( INST .NE. OPC__RET )

*  If the maximum number of symbols has been exceeded report an error.
         IF( NSYMB .GE. IRQ__NSYMS ) THEN
            STATUS = IRQ__CMPLX
            CALL ERR_REP( 'IRQ1_ALTRP_ERR4',
     :                    'IRQ1_ALTRP: Quality expression too complex',
     :                    STATUS )
            GO TO 999
         END IF

*  Search through the list of instructions to see if the next section of
*  the input expression matches any of the symbols used to represent the
*  instructions. If no match is found the symbol is assumed to be a
*  quality name.
         FOUND = .FALSE.
         INST = 0

         DO WHILE( (.NOT. FOUND) .AND. (INST .LT. IRQ__MXINS) )
            INST = INST + 1

*  Some instructions do not correspond to symbols which can be included
*  in a quality expression (eg LDQ). Skip over these.
            IF( OPC_SYM( INST ) .NE. ' ' ) THEN

*  The instruction can only be used in the current context if it looks
*  like an operator or operand from the left, as appropriate (OPC__OPL
*  is 1 if there must be an operand to the left of the symbol, 0
*  otherwise).
               IF( OPNEXT .EQV. ( OPC_OPL( INST ) .EQ. 1 ) ) THEN
                  LSYM = OPC_L( INST )

*  If the instruction passed the above test, see if the corresponding
*  symbol matches the string at the start of the remainder of the
*  input expression.
                  IF( INDEX( QEXP( OFFSET : ),
     :                OPC_SYM( INST )( : LSYM ) ) .EQ. 1 ) FOUND=.TRUE.

               END IF

            END IF

         END DO

*  If the symbol was not found, it constitutes an error if an operator
*  was expected.
         IF( .NOT. FOUND ) THEN

            IF( OPNEXT ) THEN
               STATUS = IRQ__MSOPT
               CALL ERR_REP( 'IRQ1_ALTRP_ERR5',
     :                    'IRQ1_ALTRP: Missing or invalid operator',
     :                     STATUS )
               ERRPNT = OFFSET
               GO TO 999

*  If an operand was expected next, assume that the unrecognised symbol
*  is a quality name. Add the name to the quality name stack. A quality
*  name is terminated by any of the recognised symbols.
            ELSE

               TERM = VAL__MAXI

               DO INST = 1, IRQ__MXINS
                  LSYM = OPC_L( INST )

                  IF( LSYM .GT. 0 ) THEN
                     ITERM = INDEX( QEXP( OFFSET: ),
     :                              OPC_SYM( INST )( :LSYM ) )

                     IF( ITERM .NE. 0 ) TERM = MIN( TERM, ITERM )

                  END IF

               END DO

               QNMEND = OFFSET + TERM - 2

*  If the quality name is not blank, set the next opcode to "Load
*  Quality Value" (LDQ), and add the name to the quality name stack. If
*  the quality name stack is full, report an error.
                  IF( QNMEND .GE. OFFSET ) THEN
                  INST = OPC__LDQ
                  NQNAME = NQNAME + 1

                  IF( NQNAME .GT. MXQNAM ) THEN
                     STATUS = IRQ__QREFS
                     CALL MSG_SETI( 'MX', MXQNAM )
                     CALL ERR_REP( 'IRQ1_ALTRP_ERR6',
     :      'IRQ1_ALTRP: Quality expression must not contain more '//
     :      'than ^MX quality names', STATUS )
                     GO TO 999
                  END IF

                  QNAMES( NQNAME ) = QEXP( OFFSET : QNMEND )
                  LSYM = QNMEND - OFFSET + 1

*  If the quality name is blank, there is a bad operand error
               ELSE
                  STATUS = IRQ__MSOPD
                  CALL ERR_REP( 'IRQ1_ALTRP_ERR7',
     :                       'IRQ1_ALTRP: Missing operand', STATUS )
                  ERRPNT = OFFSET
                  GO TO 999

               END IF

            END IF

         END IF

*  Put the identified instruction into the output array and move the
*  input pointer to the next symbol
         NSYMB = NSYMB + 1
         INSTR( NSYMB ) = INST
         OFFSET = OFFSET + LSYM

*  Decide whether an operator or operand follows
         OPNEXT = ( OPC_OPR( INST ) .NE. 1 )

*  Go round for the next symbol.
      END DO

*  All the symbols in the expression have now been classified.
*  Initialise the operator stack for converting to reverse Polish.
      DO I = 0, NSYMB
         STACK( I ) = 0
      END DO

*  Pre-load a RET instruction on the bottom of the stack.
      STACK( 0 ) = OPC__RET

*  Initialise pointers and counters.
      TOS = 0
      IOP = 1
      NOPC = 0
      STKSIZ = 0
      INST = OPC__NULL

*  Loop round until the RET instruction is output.
      DO WHILE( INST .NE. OPC__RET )

*  If the top of stack and input stream have matching parentheses,
*  cancel them
         IF( ( STACK( TOS ) .EQ. OPC__OPN ) .AND.
     :       ( INSTR( IOP ) .EQ. OPC__CLS ) ) THEN
            TOS = TOS - 1
            IOP = IOP + 1

*  If the instruction on the top of the stack has a high enough
*  precedence, transfer it to the output stream.
         ELSE IF( OPC_PRR( STACK( TOS ) ) .GE.
     :            OPC_PRL( INSTR( IOP ) ) ) THEN
            NOPC = NOPC + 1
            INST = STACK( TOS )
            OPCODE( NOPC ) = INST
            TOS = TOS - 1

*  If a bracket appears in the output, it results from unpaired
*  parentheses in the input expression...report an error.
            IF( INST .EQ. OPC__OPN .OR. INST .EQ. OPC__CLS ) THEN
               STATUS = IRQ__MSPAR

               IF( INST .EQ. OPC__OPN ) THEN
                  CALL MSG_SETC( 'CHAR', OPC_SYM( OPC__CLS ) )
               ELSE
                  CALL MSG_SETC( 'CHAR', OPC_SYM( OPC__OPN ) )
               END IF

               CALL ERR_REP( 'IRQ1_ALTRP_ERR8',
     :           'IRQ1_ALTRP: Missing character - "^CHAR"', STATUS )
               GO TO 999
            END IF

*  Increment or decrement the size of the stack which would be used to
*  evaluate the expression.
            STKSIZ = STKSIZ + OPC_DSTK( INST )

*  Otherwise, transfer the next symbol to the stack.
         ELSE
            TOS = TOS + 1
            STACK( TOS ) = INSTR( IOP )
            IOP = IOP + 1

         END IF

      END DO

*  If an error has occurred, ensure that NOPC is returned with a
*  non-zero value.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) NOPC = 1

      END
