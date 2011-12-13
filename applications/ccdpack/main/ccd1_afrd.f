      SUBROUTINE CCD1_AFRD( PARNAM, MXFSET, FSET, FSID, FITRTS, NFSET,
     :                      STATUS )
*+
*  Name:
*     CCD1_AFRD

*  Purpose:
*     Reads AST framesets from a CCDPACK AST file.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_AFRD( PARNAM, MXFSET, FSET, FSID, FITRTS, NFSET, STATUS )

*  Description:
*     This routine reads an AST file whose name is specified by an ADAM
*     parameter.  The file is of the sort which is written by the
*     ASTEXP task.  It gets a set of AST framesets each containing
*     two frames and a mapping between them.  Information and perhaps
*     warnings are output to the user via the CCDPACK message system.

*  Arguments:
*     PARNAM = CHARACTER * ( * ) (Given)
*        The name of the ADAM parameter via which the AST file is to
*        be opened.
*     MXFSET = INTEGER (Given)
*        The maximum number of framesets which can be read from the
*        AST file.
*     FSET( MXFSET ) = INTEGER (Returned)
*        On exit, the first NFSET elements of this array will be AST
*        pointers to the framesets represented in the AST file.
*     FSID( MXFSET ) = INTEGER (Returned)
*        On exit, the first NFSET elements of this array will be ID
*        strings for the framesets.
*     FITRTS( MXFSET ) = CHARACTER * ( * ) (Returned)
*        On exit, the first NFSET elements of this array will be FITS
*        rotation keyword frameset modifiers.
*     NFSET = INTEGER (Returned)
*        The number of framesets read from the AST file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-FEB-2001 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_PAR'          ! Standard FIO constants
      INCLUDE 'AST_PAR'          ! Standard AST constants
      INCLUDE 'CCD1_PAR'         ! Private CCDPACK constants

*  Arguments Given:
      CHARACTER * ( * ) PARNAM
      INTEGER MXFSET

*  Arguments Returned:
      INTEGER FSET( MXFSET )
      CHARACTER * ( * ) FSID( MXFSET )
      CHARACTER * ( * ) FITRTS( MXFSET )
      INTEGER NFSET

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string without trailing blanks

*  Local Variables:
      INTEGER FCHAN              ! AST pointer to frameset file channel
      INTEGER FDAST              ! FIO file descriptor for frameset file
      INTEGER FS                 ! AST pointer to read frameset
      INTEGER I                  ! Loop variable
      INTEGER IAT                ! Position in string
      LOGICAL DIFBAS             ! Base domains don't all match
      LOGICAL DIFCUR             ! Current domains don't all match
      LOGICAL DUPID              ! Duplicate ID value was generated
      CHARACTER * ( 80 ) FITRT0  ! FITS rotation keyword global modifier
      CHARACTER * ( 80 ) RTS     ! FITS rotation keyword modifier
      CHARACTER * ( AST__SZCHR ) DMBAS ! Domain of Base frame
      CHARACTER * ( AST__SZCHR ) DMCUR ! Domain of Current frame
      CHARACTER * ( AST__SZCHR ) DMBAS1 ! Reference domain of Current frame
      CHARACTER * ( AST__SZCHR ) DMCUR1 ! Reference domain of Current frame
      CHARACTER * ( AST__SZCHR ) ID ! Frameset identifier string
      CHARACTER * ( FIO__SZFNM ) ASTFIL ! Name of frameset file
      CHARACTER * ( CCD1__BLEN ) BUF ! Output buffer

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open the AST frameset file and notify the user.
      CALL FIO_ASSOC( 'ASTFILE', 'READ', 'LIST', 0, FDAST, STATUS )
      CALL FIO_FNAME( FDAST, ASTFIL, STATUS )
      CALL MSG_SETC( 'ASTFIL', ASTFIL )
      CALL CCD1_MSG( ' ', '  Framesets read from file ^ASTFIL:',
     :               STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Create AST channel on open frameset file.
      CALL CCD1_ACHAN( FDAST, ' ', FCHAN, STATUS )

*  Get global frameset modifiers from file.
      CALL CCD1_AGTMD( FDAST, FITRT0, STATUS )

*  Print header for per-frameset information.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      BUF = ' '
      BUF( 6: ) = 'N'
      BUF( 11: ) = 'Base domain'
      BUF( 31: ) = 'Current domain'
      BUF( 51: ) = 'Frameset ID'
      CALL CCD1_MSG( ' ', BUF, STATUS )
      BUF( 6: ) = '--'
      BUF( 11: ) = '-----------'
      BUF( 31: ) = '--------------'
      BUF( 51: ) = '-----------'
      CALL CCD1_MSG( ' ', BUF, STATUS )

*  Initialise warning flags.
      DIFBAS = .FALSE.
      DIFCUR = .FALSE.
      DUPID = .FALSE.
      NFSET = 0

*  Read frameset objects one by one from file.
 1    CONTINUE

*  Read the next frameset object.
         FS = AST_READ( FCHAN, STATUS )

*  Exit if the end of the file has been reached.
         IF ( FS .EQ. AST__NULL ) GO TO 2

*  Error if the object is not a frameset.
         IF ( .NOT. AST_ISAFRAMESET( FS, STATUS ) ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'ASTFIL', ASTFIL )
            CALL ERR_REP( 'CCD1_AFRD_BADAST',
     :'  Non-frameset object found in file ^ASTFIL', STATUS )
            GO TO 99
         END IF

*  Error if we have read too much.
         IF ( NFSET .GE. MXFSET ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'ASTFIL', ASTFIL )
            CALL ERR_REP( 'CCD1_AFRD_NFSET',
     :      '  Too many items in AST file ^ASTFIL', STATUS )
            GO TO 99
         END IF

*  Get frameset modifiers for this frameset from the file.
         CALL CCD1_AGTMD( FDAST, RTS, STATUS )

*  Get object ID string and frame domain names.
         ID = AST_GETC( FS, 'Id', STATUS )
         CALL AST_INVERT( FS, STATUS )
         DMBAS = AST_GETC( FS, 'Domain', STATUS )
         CALL AST_INVERT( FS, STATUS )
         DMCUR = AST_GETC( FS, 'Domain', STATUS )

*  Check whether the ID is unique.  If not, ignore this entry.
         DO I = 1, NFSET
            IF ( FSID( I ) .EQ. ID ) THEN
               DUPID = .TRUE.
               GO TO 1
            END IF
         END DO

*  First time round, set up reference values for validation.
         IF ( NFSET .EQ. 0 ) THEN
            DMCUR1 = DMCUR
            DMBAS1 = DMBAS
         END IF

*  Make a note if the Current or Base domains of this NDF do not
*  match those of the first NDF encountered.
         IF ( DMBAS .NE. DMBAS1 ) DIFBAS = .TRUE.
         IF ( DMCUR .NE. DMCUR1 ) DIFCUR = .TRUE.

*  Store the frameset.
         NFSET = NFSET + 1
         FSET( NFSET ) = FS
         FITRTS( NFSET ) = RTS
         FSID( NFSET ) = ID

*  Output basic information to the user.
         BUF = ' '
         CALL CHR_ITOC( NFSET, BUF( 6: ), IAT )
         IF ( CHR_LEN( DMBAS ) .GT. 18 ) DMBAS( 17: ) = '..'
         IF ( CHR_LEN( DMCUR ) .GT. 18 ) DMCUR( 17: ) = '..'
         BUF( 11: ) = DMBAS( 1:18 )
         BUF( 31: ) = DMCUR( 1:18 )
         BUF( 51: ) = ID
         CALL MSG_SETC( 'BUF', BUF )
         CALL CCD1_MSG( ' ', '^BUF ', STATUS )

*  Back to start of loop.
         GO TO 1
 2    CONTINUE

*  All framesets are read in.  Close the file.
      CALL AST_ANNUL( FCHAN, STATUS )
      CALL FIO_ANNUL( FDAST, STATUS )

*  Warn if there were non-matching domain names or duplicate frameset
*  ID values.
      IF ( DIFBAS ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ',
     :   '  ** WARNING **  Not all AST file Base frames had '//
     :   'matching domain names.', STATUS )
      END IF
      IF ( DIFCUR ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ',
     :   '  ** WARNING **  Not all AST file Current frames had '//
     :   'matching domain names.', STATUS )
      END IF
      IF ( DUPID ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ',
     :   '  ** WARNING **  There were duplicate frameset ID values.',
     :      STATUS )
      END IF

*  If any global modifier flags have been specified, use them in place
*  of unspecified per-frameset modifier flags.  Currently only one
*  is defined, FITSROT.
      IF ( FITRT0 .NE. ' ' ) THEN
         DO I = 1, NFSET
            IF ( FITRTS( I ) .EQ. ' ' ) FITRTS( I ) = FITRT0
         END DO
      END IF

*  Error exit label.
 99   CONTINUE

      END
* $Id$
