      SUBROUTINE IRQ_COMP( LOCS, SIZE, INFO, QEXP, UNDEF, NUNDEF,
     :                     ERRPNT, IDQ, STATUS )
*+
*  Name:
*     IRQ_COMP

*  Purpose:
*     Compile a quality expression.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ_COMP( LOCS, SIZE, INFO, QEXP, UNDEF, NUNDEF, ERRPNT,
*                    IDQ, STATUS )

*  Description:
*     All the quality names referenced in the given quality expression
*     (QEXP) are identified. If all quality names referenced in QEXP
*     are defined within the NDF specified in LOCS, then the quality
*     expression is `compiled', i.e. converted into a form that can
*     be used by IRQ_SBADx. The compiled quality expression is
*     identified by the returned IRQ identifier which should be
*     released using IRQ_ANNUL when no longer needed. If any error is
*     reported, then IRQ is returned set to the value IRQ__NOID.
*
*     If any quality names referenced in the quality expression are not
*     defined in the NDF specified by LOCS, they are returned in UNDEF,
*     the number of such undefined quality names is returned in NUNDEF,
*     an error is reported and STATUS is returned with value
*     IRQ__NOQNM. Additionally, if INFO is true, then a message is
*     generated identifying each undefined quality name.
*
*     If any of the STATUS values IRQ__BADSY, IRQ__MSOPT or IRQ__MSOPD
*     are returned (all of which correspond to various forms of syntax
*     error in the quality expression, see ID6 appendix E), a pointer
*     to the approximate position of the error within the quality
*     expression is returned in ERRPNT.
*

*  Arguments:
*     LOCS(5) = CHARACTER * ( * ) (Given)
*        An array of five HDS locators. These locators identify the NDF
*        and the associated quality name information.  They should have
*        been obtained using routine IRQ_FIND or routine IRQ_NEW.
*     SIZE = INTEGER (Given)
*        The size of the UNDEF array. This should be at least equal to
*        the value of the symbolic constant IRQ__QNREF
*     INFO = LOGICAL (Given)
*        If set to .TRUE., then messages are produced identifying any
*        undefined quality names.
*     QEXP = CHARACTER*(*) (Given and Returned)
*        A quality expression. See ID6 section 5 for details of the
*        allowed formats for quality expressions. On exit, the string is
*        converted to upper case and any leading blanks are removed.
*     UNDEF( SIZE ) = CHARACTER * ( * ) (Returned)
*        An array holding any undefined quality names referenced in the
*        quality expression. The array should have at least IRQ__QNREF
*        elements, each element being a string of length IRQ__SZQNM.
*     NUNDEF = INTEGER (Returned)
*        The number of undefined quality names referenced in the quality
*        expression.
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
*     IDQ = INTEGER (Returned)
*        An IRQ identifier for the compiled quality expression. This
*        identifier can be passed to IRQ_SBADx. This identifier should
*        be annulled using routine IRQ_ANNUL or IRQ_CLOSE when it is no
*        longer needed.  If an error is reported, then an invalid
*        identifier (equal to IRQ__NOID) is returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     5-DEC-1990 (DSB):
*        Original version.
*     15-FEB-2008 (DSB):
*        Added RDONLY argument to IRQ1_SEARC.
*     4-MAR-2008 (DSB):
*        Added FIXBIT argument to IRQ1_SEARC.
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
      INCLUDE 'IRQ_ERR'          ! IRQ error values.

*  Global Variables:
      INCLUDE 'IRQ_COM'          ! IRQ common blocks.
*        QCM_STATE = CHARACTER (Read)
*           Set to the value of symbolic constant IRQ__GOING when IRQ is
*           initialised.

*  Arguments Given:
      CHARACTER LOCS(5)*(*)
      INTEGER SIZE
      LOGICAL INFO

*  Arguments Given and Returned:
      CHARACTER QEXP*(*)

*  Arguments Returned:
      CHARACTER UNDEF( SIZE )*(*)
      INTEGER NUNDEF
      INTEGER ERRPNT
      INTEGER IDQ

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BIT                ! QUALITY bit corresponding to the
                                 ! quality name.
      CHARACTER COMMNT*(IRQ__SZCOM)! Descriptive comment stored with the
                                 ! quality name.
      LOGICAL FIXBIT             ! Does quality have a fixed bit number?
      LOGICAL FIXED              ! True if the quality is fixed over the
                                 ! entire NDF.
      LOGICAL HELD               ! Value of a constant quality name.
      INTEGER I                  ! Loop count.
      INTEGER J                  ! Loop count.
      INTEGER MASK( IRQ__QBITS ) ! Bit masks corresponding to each
                                 ! quality bit.
      INTEGER MASKS( IRQ__QNREF )! Masks corresponding to each quality
                                 ! name in QNAMES.
      INTEGER MXSTK              ! Max. size of stack needed to evaluate
                                 ! the quality expression.
      INTEGER NMASKS             ! No. of non-fixed qualities referenced
                                 ! in the quality expression.
      INTEGER NCMASK             ! No. of quality masks needed to
                                 ! evaluate the quality expression.
      INTEGER NOPC               ! No. of valid op codes in OPCODE.
      INTEGER NQNAME             ! No. of quality names in QNAMES.
      INTEGER OPCODE( IRQ__NSYMS )! Codes giveing the operations
                                 ! required to evaluate the quality
                                 ! expression.
      CHARACTER QNAMES( IRQ__QNREF )*(IRQ__SZQNM )! Quality names
                                 ! referenced in the quality expression.
      LOGICAL RDONLY             ! Read-only flag for quality name.
      LOGICAL REPORT             ! True if an undefined quality name is
                                 ! to be reported to the user.
      INTEGER SLOT               ! Slot in the QUAL array in which the
                                 ! quality name was stored.
      INTEGER TEMP               ! Temporary integer value.

*.
*  Initialise the returned identifier to an invalid value.
      IDQ = IRQ__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Identify the quality names referenced in the quality expression, and
*  compile the expression.
      CALL IRQ1_ALTRP( IRQ__NSYMS, IRQ__QNREF, QEXP, OPCODE, NOPC,
     :                 QNAMES, NQNAME, ERRPNT, STATUS )

*  Set up masks for each bit in the QUALITY component.
      TEMP = 1
      DO BIT = 1, IRQ__QBITS
         MASK( BIT ) = TEMP
         TEMP = 2*TEMP
      END DO

*  Initialise the number of undefined quality names to zero.
      NUNDEF = 0

*  Initialise the index of the next stored mask to zero.
      NMASKS = 0

*  Start an new error reporting context.
      CALL ERR_MARK

*  Loop round each quality name referenced in the quality expression.
      DO I = 1, NQNAME

*  Try to find the quality name in the QUALITY_NAMES structure.
         CALL IRQ1_SEARC( LOCS, QNAMES( I ), FIXED, HELD, BIT, COMMNT,
     :                    RDONLY, FIXBIT, SLOT, STATUS )

*  If the name was found...
         IF( STATUS .EQ. SAI__OK ) THEN

*  If there is a mix of on and off values for the quality, store a bit mask
*  in which the only set bit is the one corresponding to the quality bit
*  represented by the quality name.
            IF( .NOT. FIXED ) THEN
               NMASKS = NMASKS + 1
               MASKS( NMASKS ) = MASK( BIT )

*  If all values are either on or off, replace the "LOAD QUALITY VALUE"
*  instruction which refers to this quality, with a "LOAD TRUE" or
*  "LOAD FALSE" instruction.
            ELSE
               CALL IRQ1_VTOFX( I, HELD, NOPC, OPCODE, STATUS )

            END IF

*  If the quality name was not found, give a message identifying the
*  name and add it to the list of undefined names. If the names has
*  already been reported as undefined, then don't report it again.
         ELSE IF( STATUS .EQ. IRQ__NOQNM ) THEN

            CALL ERR_ANNUL( STATUS )

            REPORT = .TRUE.
            DO J = 1, NUNDEF
               IF( QNAMES( I ) .EQ. UNDEF( J ) ) REPORT = .FALSE.
            END DO

            IF( REPORT ) THEN
               NUNDEF = NUNDEF + 1
               UNDEF( NUNDEF ) = QNAMES( I )

               IF( INFO ) THEN

                  IF( NUNDEF .EQ. 1 ) CALL MSG_BLANK( STATUS )
                  CALL MSG_SETC( 'QN', QNAMES( I ) )
                  CALL MSG_OUT( 'IRQ_COMP_MSG1',
     :                          '  Quality "^QN" is undefined', STATUS )
               END IF

            END IF

         END IF

*  If an error occurred, abort.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Find the next quality name referenced in the quality expression.
      END DO

*  Release the current error reporting context.
      CALL ERR_RLSE

*  If any undefined quality names were referenced, report an error.
      IF( NUNDEF .NE. 0 ) THEN
         CALL MSG_BLANK( STATUS )
         STATUS = IRQ__NOQNM
         CALL MSG_SETI( 'NUN', NUNDEF )
         CALL MSG_SETC( 'QX', QEXP )
         CALL ERR_REP( 'IRQ_COMP_ERR1',
     :  'IRQ_COMP: ^NUN undefined quality name(s) referenced', STATUS )
      END IF
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Apply any possible simplifications to the compiled quality
*  expression.
      CALL IRQ1_SIMPL( NOPC, OPCODE, STATUS )

*  If possible, combine multiple quality masks into single masks.
      CALL IRQ1_CMQM( NOPC, NMASKS, OPCODE, MASKS, NCMASK, STATUS )

*  Evaluate the maximum required size of the evaluation stack.
      CALL IRQ1_EVSTK( NOPC, OPCODE, MXSTK, STATUS )

*  If necessary, initialise the IRQ identifier system.
      IF( QCM_STATE .NE. IRQ__GOING ) CALL IRQ1_INIT( STATUS )

*  Get the next free IRQ identifier. A temporary structure is created
*  to hold information describing the compiled quality expression. The
*  structure is one cell in an array of such structures. The returned
*  IRQ identifier is the index into the array.
      CALL IRQ1_GTIDQ( LOCS, NCMASK, MASKS, NOPC, OPCODE, MXSTK, IDQ,
     :                 STATUS )

*  If an error occurred, annul the identifier.
      IF( STATUS .NE. SAI__OK ) CALL IRQ1_IANNU( IDQ, STATUS )

*  If an error occurred, give a context message.
 999  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'QX', QEXP )
         CALL ERR_REP( 'IRQ_COMP_ERR2',
     :          'IRQ_COMP: Unable to compile quality expression "^QX"',
     :                 STATUS )
      END IF

      END
