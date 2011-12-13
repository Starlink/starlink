      SUBROUTINE ARD1_FIELD( ELEM, L, START, KEYW, OPER, STAT, TYPE,
     :                       STATUS )
*+
*  Name:
*     ARD1_FIELD

*  Purpose:
*     Identify the next field in an ARD expression

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_FIELD( ELEM, L, START, KEYW, OPER, STAT, TYPE, STATUS )

*  Description:
*     The symbols for each type of field are compared in turn with the
*     start of the supplied element. If more than ARD__SZABB characters
*     match, then the field is identified (unless the field is an
*     operator in which case no abbreviation is allowed).

*  Arguments:
*     ELEM = CHARACTER * ( * ) (Given)
*        The current element of the ARD description.
*     L = INTEGER (Given)
*        The index of the last non-blank character in ELEM.
*     START = INTEGER (Given and Returned)
*        The index of the next character to be checked in ELEM.
*        Updated by this routine.
*     KEYW = LOGICAL (Returned)
*        .TRUE. if the next field is a keyword field.
*     OPER = LOGICAL (Returned)
*        .TRUE. if the next field is an operator field.
*     STAT = LOGICAL (Returned)
*        .TRUE. if the next field is a statement field.
*     TYPE = INTEGER (Given)
*        The integer value used to identify the current field type
*        (defined in include file ARD_CONST).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
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
*     14-FEB-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ARD_CONST'        ! ARD_ private constants
      INCLUDE 'ARD_ERR'          ! ARD_ error constants

*  Global Variables:
      INCLUDE 'ARD_COM'          ! ARD common blocks
*        CMN_KWLEN( ARD__NKEYW ) = INTEGER (Read)
*           Used lengths of each keyword symbol.
*        CMN_KYSYM( ARD__NKEYW ) = CHARACTER * ( ARD__SZKEY ) (Read)
*           Symbols used to represent keyword fields.
*        CMN_OPCOD( ARD__NOPSY ) = INTEGER (Read)
*           Instruction codes for each operator symbol.
*        CMN_OPLEN( ARD__NOPSY ) = INTEGER (Read)
*           Used lengths of each operator symbol.
*        CMN_OPSYM( ARD__NOPSY ) = CHARACTER * ( ARD__SZOPR ) (Read)
*           Symbols used to represent operator fields.
*        CMN_STLEN( ARD__NSTAT ) = INTEGER (Read)
*           Used lengths of each statement symbol.
*        CMN_STSYM( ARD__NSTAT ) = CHARACTER * ( ARD__SZSTA ) (Read)
*           Symbols used to represent statement fields.

*  Arguments Given:
      CHARACTER ELEM*(*)

*  Arguments Given and Returned:
      INTEGER START
      INTEGER L

*  Arguments Returned:
      LOGICAL KEYW
      LOGICAL OPER
      LOGICAL STAT
      INTEGER TYPE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL ARD1_INIT         ! Initialise ARD common blocks

*  Local Variables:
      INTEGER
     :  END,                     ! Last duff character
     :  I,                       ! Symbol count
     :  NMATCH,                  ! No. of non-blank matching characters
     :  NUSED                    ! No. of matching char.s (inc. spaces)

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the returned flags.
      OPER = .FALSE.
      KEYW = .FALSE.
      STAT = .FALSE.

*  Compare the start of the specified section of the current element
*  against each keyword in turn.
      DO I = 1, ARD__NKEYW
         CALL ARD1_MATCH( ELEM, START, L,
     :                    CMN_KWSYM( I )( : CMN_KWLEN( I ) ),
     :                    NMATCH, NUSED )

*  If ARD__SZABB (or more) matching characters were found at the start
*  of the string, the field is identified.
         IF( NMATCH .GE. ARD__SZABB ) THEN
            KEYW = .TRUE.
            TYPE = I
            START = START + NUSED
            GO TO 999
         END IF

      END DO

*  Compare the start of the specified section of the current element
*  against each statement in turn.
      DO I = 1, ARD__NSTAT
         CALL ARD1_MATCH( ELEM, START, L,
     :                    CMN_STSYM( I )( : CMN_STLEN( I ) ),
     :                    NMATCH, NUSED )

*  If ARD__SZABB (or more) matching characters were found at the start
*  of the string, the field is identified.
         IF( NMATCH .GE. ARD__SZABB ) THEN
            STAT = .TRUE.
            TYPE = I
            START = START + NUSED
            GO TO 999
         END IF

      END DO

*  Compare the start of the specified section of the current element
*  against each operator symbol in turn.
      DO I = 1, ARD__NOPSY
         CALL ARD1_MATCH( ELEM, START, L,
     :                    CMN_OPSYM( I )( : CMN_OPLEN( I ) ),
     :                    NMATCH, NUSED )

*  If the start of the specified section of the current element matches
*  the current operator (without abbreviation), the field is
*  identified. Return the corresponding operator code.
         IF( NMATCH .EQ. CMN_OPLEN( I ) ) THEN
            OPER = .TRUE.
            START = START + NUSED
            TYPE = CMN_OPCOD( I )
            GO TO 999
         END IF

      END DO

*  Arrive here if no matching field has been found. Report an error.
      STATUS = ARD__BADFL
      CALL MSG_SETC( 'DESC', ELEM )
      CALL MSG_SETI( 'START', START )

      END = START + ARD__SZABB
      IF( END .LT. L ) THEN
         CALL MSG_SETC( 'SUBTEXT', ELEM( START : END ) )
         CALL MSG_SETC( 'SUBTEXT', '...' )
      ELSE
         CALL MSG_SETC( 'SUBTEXT', ELEM( START : ) )
      END IF

      CALL ERR_REP( 'ARD1_FIELD1', 'Unknown or mis-spelled field '//
     :              'found in ARD description ''^DESC'' (at start of '//
     :              'sub-string ''^SUBTEXT'').', STATUS )

*  Jump to here if a field was identified succesfully.
 999  CONTINUE

      END
