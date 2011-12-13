      SUBROUTINE CCD1_CFGRP( FD, NWORDS, MINWRD, CASE, GROUPS, LINGRP,
     :                       STATUS )
*+
*  Name:
*     CCD1_CFGRP

*  Purpose:
*     Create GRP groups from a formatted file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_CFGRP( FD, NWORDS, MINWRD, CASE, GROUPS, LINGRP,
*                      STATUS )

*  Description:
*     This routine reads in the data from a formatted file using the
*     CCDPACK character data preprocessing routine CCD1_RDLIN. This
*     joins continuation lines, strips leading blanks, removes commas
*     etc. This data is then parsed into blank separated words. These
*     words are then entered into GRP groups (and are available in
*     dynamic - fast - memory) one for each "column" of words. The
*     final word is special and consists of all characters after the
*     penultimate word (this is the whole line if NWORDS is one). The
*     original line numbers are stored in the additional group LINGRP.
*     These are intended for use in error messages.
*
*     The output groups can be made case insensitive by setting CASE
*     .FALSE.

*  Arguments:
*     FD = INTEGER (Given)
*        FIO file descriptor. The information in this file is extracted
*        as a line in the sense of CCDPACK ascii files (continuation and
*        comments allowed, commas striped out, leading blanks removed
*        -- see CCD1_RDLIN).
*     NWORDS = INTEGER (Given)
*        The number of words to be extracted from each line.
*     MINWRD = INTEGER (Given)
*        The minimum number of words which must exist in each line.
*        If this number are not encountered then an error is issued
*        together with the contents of the offending line and its line
*        number.
*     CASE = LOGICAL (Given)
*        The case sensitivity of the created groups. TRUE indicates case
*        sensitive, FALSE case insensitive.
*     GROUPS( NWORDS ) = INTEGER (Returned)
*        The created GRP identifiers.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The GRP groups GROUPS( * ) and LINGRP, should be deleted
*     before the calling application exits.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     9-DEC-1993 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameterisations

*  Arguments Given:
      INTEGER FD
      INTEGER NWORDS
      INTEGER MINWRD
      LOGICAL CASE

*  Arguments Returned:
      INTEGER GROUPS( NWORDS )
      INTEGER LINGRP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! used length of string

*  Local Variables:
      CHARACTER * ( 24 ) TYPE    ! Type of created groups etc.
      CHARACTER * ( CCD1__BLEN ) BUFFER ! Buffer for reading file contents
      INTEGER FIRST              ! Position of first character in word
      INTEGER I                  ! Loop variable
      INTEGER IAT                ! Position in string
      INTEGER LAST               ! Position of last character in word
      INTEGER LINNUM             ! Current line number
      INTEGER NCHAR              ! Number of characters
      INTEGER NTYPE              ! Number of characters
      LOGICAL EOF                ! At end of file
      LOGICAL NOTFND             ! Next word in string not found
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create the required groups.
      DO 1 I = 1, NWORDS
         TYPE = 'CCD1_CFGRP group '
         CALL CHR_ITOC( I, TYPE( 18: ) , NTYPE )
         CALL GRP_NEW( TYPE, GROUPS( I ), STATUS )
         CALL GRP_SETCS( GROUPS( I ), CASE, STATUS )
 1    CONTINUE

*  And a special group for the original line nos.
      CALL GRP_NEW( 'CCD1_CFGRP line numbers', LINGRP, STATUS )
      CALL GRP_SETCS( LINGRP, CASE, STATUS )

*  Read the file extracting the lines. Parse the data into NWORDS-1
*  the final word being and characters left after the penultimate word.
      LINNUM = 0
      EOF = .FALSE.
 2    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( STATUS .EQ. SAI__OK .AND. .NOT. EOF ) THEN
         CALL CCD1_RDLIN( FD, CCD1__BLEN, BUFFER, NCHAR, LINNUM, EOF,
     :                    STATUS )
         IF ( STATUS .EQ. SAI__OK .AND. .NOT. EOF ) THEN

*  Record line numbers.
            TYPE = ' '
            CALL CHR_ITOC( LINNUM, TYPE, NTYPE )
            CALL GRP_PUT( LINGRP, 1, TYPE, 0, STATUS )
            IF ( NWORDS .EQ. 1 ) THEN
               CALL GRP_PUT( GROUPS( 1 ), 1, BUFFER( : NCHAR ), 0,
     :                       STATUS )
            ELSE

*  Parse looking for words up to NWORD-1.
               IAT = 1
               DO 3 I = 1, NWORDS - 1
                  CALL KPG_NXWRD( BUFFER, IAT, FIRST, LAST, NOTFND,
     :                             STATUS )
                  IF ( NOTFND .AND. I .LT. MINWRD
     :                 .AND. STATUS .EQ. SAI__OK ) THEN

*  Havn't found enough words to satisfy. Set status issue error and
*  stop.
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'LINNUM', LINNUM )
                     CALL MSG_SETI( 'MINWRD', MINWRD )
                     CALL ERR_REP( 'CCD1_CFGRP_TOOFEW',
     :'  Not enough words, need at least ^MINWRD - line ^LINNUM',
     :               STATUS )
                     CALL MSG_SETC( 'BUFFER', BUFFER( :NCHAR ) )
                     CALL ERR_REP( 'CCD1_CFGRP_CON',
     :               '  Line contents - ^BUFFER', STATUS )
                     GO TO 99
                  ELSE

*  Ok enter this word into appropriate group.
                     IAT = LAST + 1
                     CALL GRP_PUT( GROUPS( I ), 1,
     :                             BUFFER( FIRST : LAST ), 0, STATUS )
                  END IF
 3             CONTINUE

*  Now extract last word. If MINWRD = NWORDS then check that this isn't
*  blank.
               NCHAR = CHR_LEN( BUFFER ( IAT : ) )
               IF ( NCHAR .EQ. 0 ) THEN
                  IF ( MINWRD .GE. NWORDS .AND. STATUS .EQ. SAI__OK )
     :            THEN

*  Last word is blank. This is an error.
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'LINNUM', LINNUM )
                     CALL MSG_SETI( 'MINWRD', MINWRD )
                     CALL ERR_REP( 'CCD1_CFGRP_TOOFEW',
     :'  Not enough words, need at least ^MINWRD - line ^LINNUM',
     :               STATUS )
                     CALL MSG_SETC( 'BUFFER', BUFFER )
                     CALL ERR_REP( 'CCD1_CFGRP_CON',
     :               '  Line contents - ^BUFFER', STATUS )
                     GO TO 99
                  END IF
               ELSE
                  NCHAR = 1
               END IF

*  Insert contents into group (make sure any leading blanks are removed).
               CALL CHR_LDBLK( BUFFER( IAT: ) )
               CALL GRP_PUT( GROUPS( NWORDS ), 1, BUFFER( IAT: ),
     :                       0, STATUS )
            END IF
         END IF
         GO TO 2
      END IF

 99   CONTINUE
      END
* $Id$
