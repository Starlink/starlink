      SUBROUTINE NDF1_PSFFL( LIST, MXEL, IBEG, IEND, EL, STATUS )
*+
*  Name:
*     NDF1_PSFFL

*  Purpose:
*     Parse a foreign format list.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_PSFFL( LIST, MXEL, IBEG, IEND, EL, STATUS )

*  Description:
*     The routine locates elements in a list of foreign data format
*     specifications held as a character string and returns the
*     character positions at which they start and end. All blanks
*     surrounding each list element are discarded, as also are blank
*     elements themselves.  Checks are performed to ensure that the
*     space available for storing element positions is not exceeded.

*  Arguments:
*     LIST = CHARACTER * ( * ) (Given)
*        The foreign format list which is to be split up.
*     MXEL = INTEGER (Given)
*        The maximum number of list elements expected.
*     IBEG( MXEL ) = INTEGER (Returned)
*        Array of character positions identifying the start of each
*        element in LIST.
*     IEND( MXEL ) = INTEGER (Returned)
*        Array of character positions identifying the end of each
*        element in LIST.
*     EL = INTEGER (Returned)
*        Number of list elements returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     11-OCT-1993 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) LIST
      INTEGER MXEL

*  Arguments Returned:
      INTEGER IBEG( MXEL )
      INTEGER IEND( MXEL )
      INTEGER EL

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      INTEGER F                  ! First non-blank character position
      INTEGER I1                 ! Pointer to start of element
      INTEGER I2                 ! Pointer to end of element
      INTEGER L                  ! Last non-blank character position
      INTEGER LENGTH             ! Significant length of input list

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the element count.
      EL = 0

*  Obtain the significant length of the input list.
      LENGTH = CHR_LEN( LIST )

*  Initialise the character pointer to the start of the list and loop
*  to extract each element.
      I1 = 1
 1    CONTINUE                   ! Start of "DO WHILE" loop
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( I1 .LE. LENGTH ) ) THEN

*  Find the final character of the next element in the list (the last
*  character before the next separator or the end of the list).
         I2 = INDEX( LIST( I1 : ), ',' )
         IF ( I2 .EQ. 0 ) THEN
            I2 = LENGTH
         ELSE
            I2 = I2 + I1 - 2
         END IF

*  Locate the first and last non-blank characters in the element,
*  checking that it is not entirely blank.
         IF ( I2 .GE. I1 ) THEN
            CALL CHR_FANDL( LIST( I1 : I2 ), F, L )
            IF ( L .GE. F ) THEN
               F = F + I1 - 1
               L = L + I1 - 1

*  Check that the output arrays will not overflow and report an error if
*  necessary.
               IF ( EL .GE. MXEL ) THEN
                  STATUS = NDF__XSFMT
                  CALL MSG_SETI( 'MXEL', MXEL )
                  CALL MSG_SETC( 'LIST', LIST )
                  CALL ERR_REP( 'NDF1_PSFFL_XS',
     :                 'Too many foreign data formats specified ' //
     :                 '(maximum permitted is ^MXEL) in the list ' //
     :                 '''^LIST''.', STATUS )

*  Increment the element count and store the position of the current
*  element in the output arrays.
               ELSE
                  EL = EL + 1
                  IBEG( EL ) = F
                  IEND( EL ) = L
               END IF
            END IF
         END IF

*  Increment the character pointer to the start of the next element in
*  the list and return to process the next element.
         I1 = I2 + 2
         GO TO 1
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_PSFFL', STATUS )

      END
