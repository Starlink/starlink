      SUBROUTINE SST_FWILD( NSPEC, SPEC, UNIT, NFILE, STATUS )
*+
*  Name:
*     SST_FWILD

*  Purpose:
*     Write wild-carded file names to a specified I/O stream.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_FWILD( NSPEC, SPEC, UNIT, NFILE, STATUS )

*  Description:
*     This routine accepts a character array, each element of which
*     contains a file specification, optionally wild-carded. It
*     searches for all the files which match any of the file
*     specifications given and writes the names of these files to an
*     output stream, creating a new output file if necessary.  The
*     number of file names written to this stream is returned. The
*     output file is left positioned at the end.

*  Arguments:
*     NSPEC = INTEGER (Given)
*        Number of file specifications.
*     SPEC( NSPEC ) = CHARACTER * ( * ) (Given)
*        Array of file specifications.
*     UNIT = INTEGER (Given)
*        Fortran I/O unit number to which file names are to be written.
*        A suitable (formatted sequential) file will be opened on this
*        unit if it is not already opened.
*     NFILE = INTEGER (Returned)
*        Number of file names written.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     Copyright (C) 1990, 1994 Science & Engineering Research Council.
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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     10-AUG-1990 (RFWS):
*        Original version.
*     13-AUG-1990 (RFWS):
*        Fixed bug in file-finding loop.
*     3-SEP-1990 (RFWS):
*        Added missing call to LIB$FIND_FILE_END.
*     6-SEP-1990 (RFWS):
*        Added standardisation of input file specifications to
*        eliminate '<>' characters in directory names.
*     17-SEP-1990 (RFWS):
*        Changed to open a named output file in the SYS$SCRATCH
*        directory instead of an un-named Fortran scratch file
*        (overcomes problem of shared disk quota).
*     5-DEC-1994 (PDRAPER):
*        Now uses UNIX find_file call.
*     2010-03-19 (TIMJ):
*        Use PSX_WORDEXP instead of ONE_FIND_FILE
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_PAR'          ! FIO_ public constants

*  Arguments Given:
      INTEGER NSPEC
      CHARACTER * ( * ) SPEC( NSPEC )
      INTEGER UNIT

*  Arguments Returned:
      INTEGER NFILE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Significant length of a string
      EXTERNAL ONE_FIND_FILE
      LOGICAL ONE_FIND_FILE          ! Wild card file search routine

*  Local Constants:
      CHARACTER * ( 8 ) UNIX     ! Name of UNIX temporary file
      PARAMETER ( UNIX = '.sst.tmp' )
      CHARACTER * ( 20 ) VMS     ! Name of VMS temporary file
      PARAMETER ( VMS = 'SYS$SCRATCH:SST.TMP' )

*  Local Variables:
      CHARACTER * ( FIO__SZFNM ) FNAME ! File name string
      CHARACTER * ( FIO__SZFNM ) SSPEC ! Standardised file specification
      CHARACTER * ( 4 ) SYSNAM   ! Name of system  (for temp file)
      INTEGER ICONTX             ! Wild card search context
      INTEGER IOERR              ! I/O error status
      INTEGER IOS                ! INQUIRE error status
      INTEGER ISPEC              ! Loop counter for file specifications
      INTEGER NC                 ! No. characters in file name
      LOGICAL OPENED             ! Has a file been opened?
      LOGICAL FIRST              ! First time calling PSX_WORDEXP?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if the I/O unit has an open file attached.
      INQUIRE ( UNIT = UNIT, OPENED = OPENED, IOSTAT = IOS )

*  If not, then open a scratch file.
      IF ( .NOT. OPENED ) THEN
         CALL SST_SYSNM( SYSNAM, STATUS )
         IF ( SYSNAM .EQ. 'VMS' ) THEN
            OPEN( UNIT = UNIT, FILE = VMS, STATUS = 'NEW',
     :            IOSTAT = IOERR )
         ELSE
            OPEN( UNIT = UNIT, FILE = UNIX, STATUS = 'NEW',
     :            IOSTAT = IOERR )
         END IF

*  If an error occurs, then set a suitable status value and report it.
         IF ( IOERR .NE. 0 ) THEN
            CALL FIO_SERR( IOERR, STATUS )
            CALL MSG_SETI( 'UNIT', UNIT )
            CALL ERR_FIOER( 'MESSAGE', IOERR )
            IF ( SYSNAM .EQ. 'VMS' ) THEN
               CALL MSG_SETC( 'TF', VMS )
            ELSE
               CALL MSG_SETC( 'TF', UNIX )
            ENDIF
            CALL ERR_REP( 'SST_FWILD_OPEN',
     :      'Error opening file ''^TF'' on Fortran unit ^UNIT '//
     :           '- ^MESSAGE.', STATUS )
         END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Initialise the count of files found and loop to process each file
*  specification.
      NFILE = 0
      DO 1 ISPEC = 1, NSPEC

*  Initialise the wild card search context.
         ICONTX = 0
         FIRST = .TRUE.

*  Get file specification.
         SSPEC = SPEC( ISPEC )

*  Loop to obtain file names until no more are found.
         DO WHILE ( FIRST .OR. ICONTX .NE. 0 )
            FNAME = ' '
            FIRST = .FALSE.
            CALL PSX_WORDEXP( SSPEC, ICONTX, FNAME, STATUS )
            IF ( FNAME .NE. ' ' .AND. STATUS .EQ. SAI__OK) THEN

*  When a file name is found, write it to the output unit.
               NC = MAX( 1, CHR_LEN( FNAME ) )
               WRITE( UNIT, '( A )', IOSTAT = IOERR ) FNAME( : NC )

*  Check for I/O errors. Set a suitable STATUS value and report the
*  error.
               IF ( IOERR .NE. 0 ) THEN
                  CALL FIO_SERR( IOERR, STATUS )
                  FNAME = '?'
                  INQUIRE( UNIT = UNIT, NAME = FNAME, IOSTAT = IOS )
                  CALL MSG_SETC( 'FILE', FNAME )
                  CALL MSG_SETI( 'UNIT', UNIT )
                  CALL ERR_FIOER( 'MESSAGE', IOERR )
                  CALL ERR_REP( 'SST_FWILD_WRITE',
     :            'Error writing to file ^FILE on Fortran unit ' //
     :            '^UNIT - ^MESSAGE.', STATUS )
                  GO TO 99
               END IF

*  Count the number of file names written successfully.
               NFILE = NFILE + 1
            END IF
         END DO

1     CONTINUE

*  Arrive here if an error occurs.
99    CONTINUE

* $Id$
      END
