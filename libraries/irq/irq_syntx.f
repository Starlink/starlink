      SUBROUTINE IRQ_SYNTX( QEXP, ERRPNT, STATUS )
*+
*  Name:
*     IRQ_SYNTX

*  Purpose:
*     Check the syntax of a quality expression.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ_SYNTX( QEXP, ERRPNT, STATUS )

*  Description:
*     The syntax of the supplied quality expression is checked, and an
*     error is reported if a syntax error is detected.  If any of the
*     STATUS values IRQ__BADSY, IRQ__MSOPT or IRQ__MSOPD are returned
*     (all of which correspond to various forms of syntax error in the
*     quality expression, see ID6 appendix E), a pointer to the
*     approximate position of the error within the quality expression
*     is returned in ERRPNT.  Note, in order for a quality expression
*     to compile successfully (using IRQ_COMP), it must not only contain
*     no syntax errors, but must also contain no undefined quality
*     names. IRQ_SYNTX cannot check for undefined quality names.
*
*  Arguments:
*     QEXP = CHARACTER*(*) (Given and Returned)
*        A quality expression. See ID6 section 5 for details of the
*        allowed formats for quality expressions. On exit, the string is
*        converted to upper case and any leading blanks are removed.
*     ERRPNT = INTEGER (Returned)
*        If any of the STATUS values IRQ__BADSY( "Unrecognised logical
*        operator or constant"), IRQ__MSOPT ( "Missing operator") or
*        IRQ__MSOPD ( "Missing operand") are returned, then ERRPNT
*        returns the offset within the quality expression at which the
*        error was detected. Note, the offset refers to the returned
*        form of QEXP, not the given form. These will be different if
*        the given form of QEXP has any leading blanks. An offset of
*        zero is returned if none of the errors associated with the
*        above STATUS values occur.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     7-JAN-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'IRQ_PAR'          ! IRQ constants.

*  Arguments Given and Returned:
      CHARACTER QEXP*(*)

*  Arguments Returned:
      INTEGER ERRPNT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NOPC               ! No. of valid op codes in OPCODE.
      INTEGER NQNAME             ! No. of quality names in QNAMES.
      INTEGER OPCODE( IRQ__NSYMS )! Codes giveing the operations
                                 ! required to evaluate the quality
                                 ! expression.
      CHARACTER QNAMES( IRQ__QNREF )*(IRQ__SZQNM )! Quality names
                                 ! referenced in the quality expression.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Identify the quality names referenced in the quality expression, and
*  compile the expression.
      CALL IRQ1_ALTRP( IRQ__NSYMS, IRQ__QNREF, QEXP, OPCODE, NOPC,
     :                 QNAMES, NQNAME, ERRPNT, STATUS )

*  If an error occurred, give a context message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRQ_SYNTX_ERR1',
     :          'IRQ_SYNTX: Syntax error found in quality expression',
     :                 STATUS )
      END IF

      END
