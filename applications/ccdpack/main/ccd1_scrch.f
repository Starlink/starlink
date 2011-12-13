      SUBROUTINE CCD1_SCRCH( STYPE, COMMEN, PREFIX, USEPRO, PROTEC,
     :                       CONTIN, QUOTE, SAY, DEL, STATUS )
*+
*  Name:
*     CCD1_SCRCH

*  Purpose:
*     Characterises a script type.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_SCRCH( STYPE, COMMEN, PREFIX, USEPRO, PROTEC, CONTIN,
*                      QUOTE, SAY, DEL, STATUS )

*  Description:
*     This routine characterises a script type by defining the elements
*     which change between them. I.e. most (script/command procedure)
*     languages have fixed differences descibed by
*
*        - COMMEN    -- a character string for indicating that this line
*                       is a comment
*        - PREFIX    -- a character string indicating that this is a
*                       command line
*        - USEPRO    -- whether protection is required for special
*                       characters or not
*        - PROTEC(2) -- character used to quote special characters
*        - CONTIN    -- the continuation character
*        - QUOTE     -- the characters used to quote a string
*        - SAY       -- command for writing to the user.
*        - DEL       -- command for deleting a file.
*     If protection is used two characters are defined. These encompass
*     any values. For the C-shell these are both single quotes.
*        KEYWORD='VALUE'
*
*     So for the C-shell we have.
*         COMMEN = '#'
*         PREFIX = ' '
*         USEPRO = .TRUE.
*         PROTEC(1) = ''''           ! a single '
*         PROTEC(2) = ''''           ! a single '
*         CONTIN = ' \'              ! a single \
*         QUOTE  = '"'
*         SAY    = 'echo'
*         DEL    = '\rm'
*
*     DCL:
*         COMMEN = '$!'
*         PREFIX = '$'
*         USEPRO = .FALSE.
*         PROTEC(1) = ' '
*         PROTEC(2) = ' '
*         CONTIN = '-'
*         QUOTE  = '"'
*         SAY    = 'write sys$output'
*         DEL    = 'delete/nolog/noconfirm'
*
*     ICL (UNIX):
*         COMMEN = '{'
*         PREFIX = ' '
*         USEPRO = .FALSE.
*         PROTEC(1) = ' '
*         PROTEC(2) = ' '
*         CONTIN = '~'
*         QUOTE  = '"'
*         SAY    = 'print'
*         DEL    = 'rm'

*  Arguments:
*     STYPE = CHARACTER * ( * ) (Given)
*        The script type. Must be one of "csh", "dcl" or "icl".
*     COMMEN = CHARACTER * ( * ) (Returned)
*        The comment character for this script type.
*     PREFIX = CHARACTER * ( * ) (Returned)
*        The prefix for commands.
*     USEPRO = LOGICAL (Returned)
*        Whether quoting protection should be used (in case special
*        characters are present).
*     PROTEC( 2 ) = CHARACTER * ( * ) (Returned)
*        The protection characters (if used).
*     CONTIN = CHARACTER * ( * ) (Returned)
*        The continuation character for this script type.
*     QUOTE = CHARACTER * ( * )(Returned)
*        The quotation character.
*     SAY = CHARACTER * ( * ) (Returned)
*        The expression used to write output to the user.
*     DEL = CHARACTER * ( * ) (Returned)
*        Guaranteed delete command (no prompts etc.).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - make sure that all the input strings are long enough.
*     - the DEL argument is also architecture specific.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     23-SEP-1993 (PDRAPER):
*        Original version.
*     20-OCT-1995 (PDRAPER):
*        Added DEL.
*     15-APR-2005 (PDRAPER):
*        Parameterized printing of backslashes for increased portability.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameters

*  Arguments Given:
      CHARACTER * ( * ) STYPE

*  Arguments Returned:
      CHARACTER * ( * ) COMMEN
      CHARACTER * ( * ) PREFIX
      CHARACTER * ( * ) PROTEC( 2 )
      LOGICAL USEPRO
      CHARACTER * ( * ) CONTIN
      CHARACTER * ( * ) QUOTE
      CHARACTER * ( * ) SAY
      CHARACTER * ( * ) DEL

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      INTEGER LSTAT              ! Local status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the appropriate characterisation checking for truncation if
*  appropriate (more than one character).
      LSTAT = 0
      IF ( STYPE .EQ. 'csh' ) THEN
         COMMEN = '#'
         PREFIX = ' '
         USEPRO = .TRUE.
         PROTEC( 1 ) = ''''
         PROTEC( 2 ) = ''''
         CALL CHR_COPY( ' ' // CCD1__BKSLH, .FALSE., CONTIN, LSTAT )
         QUOTE ='"'
         IF ( LSTAT .EQ. 0 ) THEN
            CALL CHR_COPY( 'echo ', .FALSE., SAY, LSTAT )
         END IF
         IF ( LSTAT .EQ. 0 ) THEN
            CALL CHR_COPY( CCD1__BKSLH // 'rm ', .FALSE., DEL, LSTAT )
         ENDIF

      ELSE IF ( STYPE .EQ. 'dcl' ) THEN
         CALL CHR_COPY( '$!', .FALSE., COMMEN, LSTAT )
         IF ( LSTAT .EQ. 0 ) THEN
            PREFIX = '$'
            USEPRO = .FALSE.
            PROTEC( 1 ) = ' '
            PROTEC( 2 ) = ' '
            CALL CHR_COPY( ' -', .FALSE., CONTIN, LSTAT )
            QUOTE ='"'
            IF ( LSTAT .EQ. 0 ) THEN
               CALL CHR_COPY( '$write sys$output ',.FALSE., SAY,
     :                        LSTAT )
            END IF
            IF ( LSTAT .EQ. 0 ) THEN
               CALL CHR_COPY( '$delete/nolog/noconfirm ', .FALSE.,
     :                        DEL, LSTAT )
            ENDIF
         END IF

      ELSE IF ( STYPE .EQ. 'icl' ) THEN
         COMMEN = '{'
         PREFIX = ' '
         USEPRO = .FALSE.
         PROTEC( 1 ) = ' '
         PROTEC( 2 ) = ' '
         QUOTE ='"'
         CALL CHR_COPY( ' ~', .FALSE., CONTIN, LSTAT )
         IF ( LSTAT .EQ. 0 ) THEN
            CALL CHR_COPY( 'print ', .FALSE., SAY, LSTAT )
         END IF
         IF ( LSTAT .EQ. 0 ) THEN
            CALL CHR_COPY( CCD1__BKSLH // 'rm ', .FALSE., DEL, LSTAT )
         ENDIF
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'STYPE', STYPE )
         CALL ERR_REP( 'CCD1_SCRSH1',
     :   '  CCD1_SCRSH: Unknown script type: ^STYPE', STATUS )
      END IF

*  Check for truncation errors.
      IF ( LSTAT .EQ. 1 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_SCRCH2', '  CCD1_SCRCH: Unable to'//
     :   ' set script characterisation -- string truncated', STATUS )
      END IF
      END
* $Id$
