      SUBROUTINE CCD1_SDECT( FILNAM, HAVRES, HAVTAB, STATUS )
*+
*  Name:
*     CCD1_SDECT

*  Purpose:
*     Selects a known (CCDPACK) detector.

*  Language:
*     Starlink Fortran-77

*  Invocation:
*     CALL CCD1_SDECT( FILNAM, HAVRES, HAVTAB, STATUS )

*  Description:
*     This routine is used to select from a known list of
*     telescopes/detector combinations. The configurations are either
*     CCDSETUP-like files (which describe the CCD characteristics) or
*     an FITS Import Control Table. The two are differentiated by
*     their contents (CCDSETUP-like files have word = statements).
*
*     These files must have a data type .DAT and be stored either in the
*     directory $CCDPACK_DIR or the directory $CCDPACK_CONFIG. The
*     CCDPACK_CONFIG variable is intended for use by users.

*  Arguments:
*     FILNAM = CHARACTER * ( * ) (Returned)
*        The name of the selected file.
*     HAVRES = LOGICAL (Returned)
*        If TRUE then the selected file is a restoration file.
*     HAVTAB = LOGICAL (Returned)
*        If TRUE then the selected file is an import table.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - Uses the ADAM parameters INDEX, CHOICE and CONTINUE. These must
*     be defined in the interface file of the calling application.

*  Copyright:
*     Copyright (C) 1997, 2000, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JUN-1997 (PDRAPER):
*        Original version.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     29-AUG-2004 (TIMJ):
*        Use ONE_FIND_FILE
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'PSX_ERR'         ! PSX error codes
      INCLUDE 'MSG_PAR'         ! Message system parameters
      INCLUDE 'FIO_ERR'         ! FIO error codes
      INCLUDE 'PAR_ERR'         ! Parameter system errors
      INCLUDE 'ONE_ERR'         ! Errors from FIND_FILE

*  Arguments Returned:
      CHARACTER * ( * ) FILNAM
      LOGICAL HAVRES
      LOGICAL HAVTAB

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN          ! Used length of string
      LOGICAL ONE_FIND_FILE
      EXTERNAL ONE_FIND_FILE        ! Lists files in directory

*  Local Variables:
      CHARACTER * ( MSG__SZMSG ) BUF ! General character buffer
      CHARACTER * ( MSG__SZMSG ) CCDDIR( 2 ) ! Directories containing files
      CHARACTER * ( MSG__SZMSG ) FILE ! Name of file
      CHARACTER * ( MSG__SZMSG ) FSPEC ! File specification
      CHARACTER * ( 78 ) SHORT  ! One line of output
      INTEGER COMGRP            ! File comments group
      INTEGER CONTXT            ! File search context
      INTEGER FD                ! File descriptor
      INTEGER I                 ! Loop variable
      INTEGER IAT               ! Position in string
      INTEGER IFILE             ! File index
      INTEGER N                 ! Number of files located
      INTEGER NAMGRP            ! File name group
      INTEGER TYPGRP            ! File types group
      INTEGER BUFLEN            ! Length of string
      LOGICAL AGAIN             ! Loop control variable
      LOGICAL CONT              ! Continue
      LOGICAL COMMEN            ! Line is a comment
      LOGICAL FIRST             ! First time in while loop
      LOGICAL FOUND             ! Found a file ok
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialisation.
      HAVRES = .FALSE.
      HAVTAB = .FALSE.

*  Set the directories to search for config files.
      CCDDIR( 1 )  = ' '
      CALL PSX_GETENV( 'CCDPACK_DIR', CCDDIR( 1 ), STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NOCCDDIR',
     :        'Failed to locate any known detector files', STATUS )
         GO TO 99
      END IF
      CALL PSX_GETENV( 'CCDPACK_CONFIG', CCDDIR( 2 ), STATUS )
      IF ( STATUS .EQ. PSX__NOENV ) THEN
         CALL ERR_ANNUL( STATUS )
         CCDDIR( 2 ) = ' '
      END IF

*  Create groups for the file names, types and comments.
      CALL GRP_NEW( 'file names', NAMGRP, STATUS )
      CALL GRP_NEW( 'file types', TYPGRP, STATUS )
      CALL GRP_NEW( 'comments', COMGRP, STATUS )

*  Scan directories for *.DAT files.
      N = 0
      DO 1 I = 1, 2
         IF ( CCDDIR( I ) .NE. ' ' ) THEN
            FSPEC = CCDDIR( I )( :CHR_LEN( CCDDIR( 1 ) ) )//'/*.DAT'
            CONTXT = 0
 2          CONTINUE
            FOUND =  ONE_FIND_FILE( FSPEC, .TRUE., FILE, CONTXT, STATUS)
            IF ( FOUND .AND. STATUS .EQ. SAI__OK ) THEN

*  Have a list of files to read, store name and then derive type.
               N = N + 1
               FIRST = .TRUE.
               CALL GRP_PUT( NAMGRP, 1, FILE, N, STATUS )
               CALL FIO_OPEN( FILE, 'READ', 'LIST', 0, FD, STATUS )
 3             CONTINUE
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL FIO_READF( FD, BUF, STATUS )
                  IF ( STATUS .EQ. FIO__EOF ) THEN

*  Stop reading this file (assumed to be a table) and try next.
                     CALL ERR_ANNUL( STATUS )
                     CALL GRP_PUT( TYPGRP, 1, 'table', N, STATUS )
                     CALL FIO_CLOSE( FD, STATUS )
                     GO TO 2
                  END IF
                  COMMEN = BUF( 1:1 ) .EQ. '#'
                  IF ( FIRST ) THEN
                     FIRST = .FALSE.

*  Check this line it may be a description.
                     IF ( COMMEN ) THEN
                        CALL GRP_PUT( COMGRP, 1, BUF, N, STATUS )
                     ELSE
                        CALL GRP_PUT( COMGRP, 1, ' ', N, STATUS )
                     END IF
                  END IF
                  IF ( .NOT. COMMEN ) THEN

*  Scan line for a "word = word" pattern, this should only occur in
*  restoration files. The method used is to scan for the first '=' sign,
*  then remove this and any blanks just before it. If the characters
*  left are just one word (i.e. do not have a blank), then this is a
*  restoration file.
                     IAT = INDEX( BUF, '=' )
                     IF ( IAT .GT. 0 ) THEN
                        IAT = IAT - 1
                        IAT = CHR_LEN( BUF( : IAT ) )
                        IF ( INDEX( BUF( : IAT ), ' ' ) .EQ. 0 ) THEN

*  This is a restoration file, so record this fact and start
*  processing next file.
                           CALL GRP_PUT( TYPGRP, 1, 'setup', N, STATUS )
                           CALL FIO_CLOSE( FD, STATUS )
                           GO TO 2
                        END IF
                     END IF
                  END IF

*  Back for next line.
                  GO TO 3
               END IF
            END IF
*  Clear find_file context and reset NOFILES status
            IF (STATUS .EQ. ONE__NOFILES) CALL ERR_ANNUL( STATUS )
            CALL ONE_FIND_FILE_END( CONTXT, STATUS )
         END IF
 1    CONTINUE


*  Now output these.
      IF ( N .EQ. 0 ) THEN
         CALL CCD1_WRTPA( 'Sorry no detectors seem to be available '//
     :'on this system. You''ll have to attempt the reduction without '//
     :'any help.', 72, 3, .FALSE., STATUS )
      ELSE
         AGAIN = .TRUE.
 4       CONTINUE
         IF ( AGAIN .AND. STATUS .EQ. SAI__OK ) THEN

*  Now we can introduce this section.
            CALL MSG_BLANK( STATUS )
            CALL MSG_OUT( ' ','   Listing of known detector files:',
     :                    STATUS )
            CALL MSG_BLANK( STATUS )

*  List all the detector files that have been found. Each name is
*  associated with a number.
            CALL MSG_OUT( ' ',
     :'Index  Name        Description          (type)', STATUS )
            CALL MSG_BLANK( STATUS )
            DO 5 I = 1, N
               CALL MSG_SETI( 'N', I )
               CALL GRP_GET( NAMGRP, I, 1, BUF, STATUS )
               CALL CCD1_FSPEC( BUF, ' ', 'NAME', FILE, STATUS )
               CALL MSG_SETC( 'NAME', FILE )
               CALL GRP_GET( COMGRP, I, 1, BUF, STATUS )
               CALL MSG_SETC( 'COMMENT', BUF )
               CALL GRP_GET( TYPGRP, I, 1, BUF, STATUS )
               CALL MSG_SETC( 'TYPE', BUF )
               CALL MSG_LOAD( ' ', ' ^N)   ^NAME   ^COMMENT  (^TYPE)',
     :                        BUF, BUFLEN, STATUS )

*  If the line is too long for the typical screen, break it into
*  shorter pieces. It should never be longer than two lines. Note
*  that the second line is indented somewhat.
               IAT = 1
               CALL CHR_LINBR( BUF( :BUFLEN ), IAT, SHORT )
               CALL MSG_OUT( ' ', SHORT, STATUS )
               IF ( IAT .NE. 0 ) THEN
                  CALL CHR_LINBR( BUF( :BUFLEN ), IAT, SHORT )
                  CALL MSG_SETC( 'REST', SHORT )
                  CALL MSG_OUT( ' ', '         ^REST', STATUS )
               END IF
 5          CONTINUE
            CALL MSG_BLANK( STATUS )

*  Get the file index. ! means no file selected.
            CALL PAR_GDR0I( 'INDEX', 1, 1, N, .FALSE., IFILE, STATUS )
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
               AGAIN = .FALSE.
               GO TO 4
            END IF
            CALL PAR_CANCL( 'INDEX', STATUS )
            CALL PAR_CHOIC( 'CHOICE', 'V', 'view,select,continue',
     :                      .FALSE., BUF, STATUS )
            CALL PAR_CANCL( 'CHOICE', STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               AGAIN = .FALSE.
            ELSE IF ( BUF( 1:1 ) .EQ. 'V' ) THEN
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL GRP_GET( NAMGRP, IFILE, 1, BUF, STATUS )
                  CALL CCD1_CAT( BUF, STATUS )
                  CALL PAR_GET0L( 'CONTINUE', CONT, STATUS )
                  CALL PAR_CANCL( 'CONTINUE', STATUS )
               END IF
            ELSE IF ( BUF( 1:1 ) .EQ. 'S' ) THEN
               CALL GRP_GET( NAMGRP, IFILE, 1, FILNAM, STATUS )
               CALL GRP_GET( TYPGRP, IFILE, 1, BUF, STATUS )
               IF ( BUF .EQ. 'setup' ) THEN
                  HAVRES = .TRUE.
               ELSE
                  HAVTAB = .TRUE.
               END IF
               AGAIN = .FALSE.
            ELSE

*  No file selected.
               AGAIN = .FALSE.
            END IF

*  Process next request, or terminate.
            GO TO 4
        END IF
      END IF
 99   CONTINUE

*  Release group resources.
      CALL CCD1_GRDEL( NAMGRP, STATUS )
      CALL CCD1_GRDEL( TYPGRP, STATUS )
      CALL CCD1_GRDEL( COMGRP, STATUS )

      END
* $Id$
