      SUBROUTINE SST_NSECT( MATCH, N, NAME, HEADER, FIRST, LAST,
     :                      STATUS )
*+
*  Name:
*     SST_NSECT

*  Purpose:
*     Find a named top-level section in a routine prologue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_NSECT( MATCH, N, NAME, HEADER, FIRST, LAST, STATUS )

*  Description:
*     The routine searches the prologue lines held in the internal
*     source code buffer to identify a section having one (or
*     alternatively having none) of the names specified. Trailing
*     colons are ignored. Searching commences at line HEADER + 1 (or
*     line 1 if HEADER is zero) and the header line number of the
*     section found is returned in HEADER. Zero is returned if no
*     suitable section is found. The first and last line numbers of the
*     body of the identified section are returned in FIRST and LAST. If
*     no body exists, then FIRST wii be greater than LAST.

*  Arguments:
*     MATCH = LOGICAL (Given)
*        If .TRUE., then the section name must match one of the names
*        supplied. If .FALSE., then it must match none of these names.
*     N = INTEGER (Given)
*        Number of names to test against.
*     NAME( N ) = CHARACTER * ( * ) (Given)
*        List of names for section identification.
*     HEADER = INTEGER (Given and Returned)
*        Searching commences at line HEADER + 1 and the header line
*        number of the identified section is returned in this argument.
*     FIRST = INTEGER (Returned)
*        First line number of the body of the section.
*     LAST = INTEGER (Given)
*        Last line number of the body of the section.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify it under
*     the terms of the GNU General Public License as published by the Free Software
*     Foundation; either version 2 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,but WITHOUT ANY
*     WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
*     PARTICULAR PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License along with
*     this program; if not, write to the Free Software Foundation, Inc., 59 Temple
*     Place,Suite 330, Boston, MA  02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-DEC-1989 (RFWS):
*        Original version.
*     8-AUG-1990 (RFWS):
*        Changed to eliminate the use of an internal character buffer.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'SST_PAR'          ! SST_ consants

*  Global Variables:
      INCLUDE 'SST_SCB'          ! SST_ Source Code Buffer

*  Arguments Given:
      LOGICAL MATCH
      INTEGER N
      CHARACTER * ( * ) NAME( N )

*  Arguments Given and Returned:
      INTEGER HEADER

*  Arguments Returned:
      INTEGER FIRST
      INTEGER LAST

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Local Variables:
      INTEGER I                  ! Loop counter for names
      INTEGER L                  ! Last header character to test

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop to find sections.
1     CONTINUE                   ! Start of 'DO WHILE' loop
      CALL SST_FSECT( HEADER, FIRST, LAST, STATUS )

*  If one is found, then see if it ends with a colon. Omit the colon if
*  it does, being careful to leave at least one character in the
*  header.
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( HEADER .NE. 0 ) ) THEN
         L = SCB_LC( HEADER )
         IF ( SCB_LINE( HEADER)( L : L ) .EQ. ':' ) THEN
            L = MAX( L - 1, SCB_FC( HEADER ) )
         END IF

*  If testing for a name match, then compare the header with each name
*  in turn. Quit searching if a match is found.
         IF ( MATCH ) THEN
            DO 2 I = 1, N
               IF ( CHR_SIMLR( NAME( I ),
     :                         SCB_LINE( HEADER )( SCB_FC( HEADER ) :
     :                                             L ) ) ) THEN
                  GO TO 9
               END IF
2           CONTINUE

*  If testing for no name match, then compare with each name in turn.
         ELSE
            DO 3 I = 1, N
               IF ( CHR_SIMLR( NAME( I ),
     :                         SCB_LINE( HEADER )( SCB_FC( HEADER ) :
     :                                             L ) ) ) THEN
                  GO TO 4
               END IF
3           CONTINUE

*  Quit searching if the section header didn't match any of the names.
            GO TO 9
4           CONTINUE
         END IF

*  If still searching, then return to find the next section.
         GO TO 1
      END IF

*  Exit to here if a suitable section is found.
9     CONTINUE

      END
* @(#)sst_nsect.f   1.1   94/12/05 11:31:30   96/07/05 10:27:28
