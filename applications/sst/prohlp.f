      SUBROUTINE PROHLP( STATUS )
*+
*  Name:
*     PROHLP

*  Purpose:
*     Converts routine prologue information into a help library input
*     file.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PROHLP( STATUS )

*  Description:
*     This application reads a series of Fortran 77 source code files
*     containing prologue information formatted using STARLSE (SUN/105)
*     and produces an output file containing user documentation for
*     each routine in a format suitable for insertion into a help
*     library. The help library format may be chosen to suit either a
*     package of application programs (i.e. ADAM A-tasks) or a set of
*     ordinary subroutines or functions, such as a subroutine library.

*  Usage:
*     PROHLP IN [OUT]

*  ADAM Parameters:
*     ATASK = _LOGICAL (Read)
*        If ATASK is set to TRUE, then a help library format suitable
*        for a package of application programs (i.e. ADAM A-tasks) is
*        produced. If it is set to FALSE, then the help library format
*        produced is suitable for a subroutine library.  [TRUE]
*     IN() = LITERAL (Read)
*        A list of (optionally wild-carded) file specifications which
*        identify the Fortran 77 source code files to be used for
*        input. Up to 10 values may be given, but only a single
*        specification such as '*.FOR' is normally required.
*
*        If the SINGLE parameter is set to TRUE (the default), then
*        only a single prologue will be expected in each input file. If
*        it is set to FALSE, then there is no limit to the number of
*        prologues which may be held in each input file.
*     OUT = FILE (Write)
*        The output file to which the help information will be written.
*        The information in this file will need to be inserted into a
*        help library for use.  [PROHLP.HLP]
*     SINGLE = _LOGICAL (Read)
*        If SINGLE is set to TRUE, then only a single prologue will be
*        expected at the start of each input file; if the file contains
*        more than one prologue, then the remaining ones will be
*        ignored. If SINGLE is set to FALSE, then each input file will
*        be searched for all the prologues it contains and each will be
*        processed in turn. When appropriate, the former option (the
*        default) will result in faster execution since only the
*        initial prologue information must then be read, rather than
*        the entire contents of each input file.  [TRUE]

*  Examples:
*     PROHLP PROG.FOR PROG.HLP
*        Extracts prologue information from the application program
*        source code held in the file PROG.FOR and produces a help
*        library entry for it. The program's name is used as the
*        level-1 help keyword. The output is written to the file
*        PROG.HLP.
*     PROHLP *.FOR OUT=SUBS.HLP ATASK=FALSE
*        Extracts prologue information for a subroutine library, whose
*        source code resides in the files *.FOR, one routine per file.
*        Help library entries describing the routines are written to
*        the file SUBS.HLP, with each routine's name being used as a
*        level-1 help keyword.
*     PROHLP IN=SOURCE.FOR NOATASK NOSINGLE
*        Extracts prologue information from a sequence of subroutines
*        or functions which are all held in the file SOURCE.FOR and
*        produces help library entries for them in the default output
*        file PROHLP.HLP. Each routine name is used to form a separate
*        level-1 help keyword.
*     PROHLP IN=["A*.FOR","B*.FOR"] NOATASK
*        In this example, a sequence of input file specifications is
*        given. Each will be processed in turn to generate a help file
*        entry from the first prologue encountered in each file.
*        Output is written to the file PROHLP.HLP. Once again, each
*        routine name generates a separate level-1 help keyword.

*  Notes:
*     Care must be taken to ensure that begin-prologue and end-prologue
*     lines (starting '*+' and '*-' respectively) appear before and
*     after each prologue and that '+' and '-' symbols are not used in
*     the second column elsewhere in the file, otherwise prologues may
*     not be correctly identified.

*  Timing:
*     The execution time is approximately proportional to the amount of
*     information to be read from the input files. In addition, the
*     time will be increased somewhat if the input code resides in a
*     large number of separate files, due to the need to open and close
*     each file. If there is only one prologue in each input file, then
*     execution time will be minimised if SINGLE is set to TRUE, since
*     only the initial prologue information need then be read, rather
*     than the entire contents of each file.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
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
*     {enter_new_authors_here}

*  History:
*     14-AUG-1990 (RFWS):
*        Original version, derived from the PROLAT routine.
*     12-SEP-1990 (RFWS):
*        Improved the prologue documentation.
*     17-SEP-1990 (RFWS):
*        Close scratch file with explicit delete status.
*     5-DEC-1994 (PDRAPER):
*        Changed to use FIO_OPENs instead if OPEN, should be more
*        portable.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_PAR'          ! FIO_ public constants
      INCLUDE 'SST_PAR'          ! SST_ constants

*  Global Variables:
      INCLUDE 'SST_SCB'          ! SST Source Code Buffer

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXSPEC             ! Max. no of file specifications
      PARAMETER ( MXSPEC = 10 )

*  Local Variables:
      CHARACTER * ( FIO__SZFNM ) FNAME ! Input file name
      CHARACTER * ( FIO__SZFNM ) OUTFIL ! Output file name
      CHARACTER * ( FIO__SZFNM ) SPEC( MXSPEC ) ! File specifications
      INTEGER FDIN               ! Input file descriptor
      INTEGER FDOUT              ! Output file descriptor
      INTEGER IFILE              ! Loop counter for input files
      INTEGER NFILE              ! Number of input files
      INTEGER NPRO               ! Number of prologues read
      INTEGER NSPEC              ! Number of file specifications
      INTEGER SCRAT              ! I/O unit for scratch file
      INTEGER TOTPRO             ! Total prologues processed
      LOGICAL ATASK              ! Whether processing ADAM A-tasks
      LOGICAL INPOPN             ! Input file open status
      LOGICAL OUTOPN             ! Input file open status
      LOGICAL SINGLE             ! Whether single prologue/file expected

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain I/O units for scratch files.
      CALL FIO_GUNIT( SCRAT, STATUS )

*  Input and output files aren't open.
      INPOPN = .FALSE.
      OUTOPN = .FALSE.

*  Get the input file specifications.
      CALL PAR_GET1C( 'IN', MXSPEC, SPEC, NSPEC, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Write the list of input files to be processed into a scratch file.
      CALL SST_FWILD( NSPEC, SPEC, SCRAT, NFILE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  If no files were found, then report an error.
      IF ( NFILE .LE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'SPEC', SPEC( 1 ) )
         CALL ERR_REP( 'PROHLP_NOFILES',
     :   'No input files found matching the specification ''^SPEC''...',
     :   STATUS )
         GO TO 99
      END IF

*  Report how many files are to be processed.
      CALL MSG_OUT( 'BLANK', ' ', STATUS )
      IF ( NFILE .EQ. 1 ) THEN
         CALL MSG_OUT( 'NFILE1', '   1 file to be processed...',
     :                 STATUS )
      ELSE
         CALL MSG_SETI( 'NFILE', NFILE )
         CALL MSG_OUT( 'NFILE', '   ^NFILE files to be processed...',
     :                 STATUS )
      END IF
      CALL MSG_OUT( 'BLANK', ' ', STATUS )

*  Rewind the scratch file ready for re-reading.
      REWIND( SCRAT )

*  Obtain the name of the output file.
      CALL PAR_GET0C( 'OUT', OUTFIL, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Open the output file.
      CALL FIO_OPEN( OUTFIL, 'WRITE', 'LIST', 0, FDOUT, STATUS )
      CALL FIO_UNIT( FDOUT, SCB_OUT, STATUS )

*  Check for errors, setting a suitable STATUS value and reporting the
*  error.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'FILE', OUTFIL )
         CALL MSG_SETI( 'UNIT', SCB_OUT )
         CALL ERR_REP( 'PROHLP_OPENOUT',
     :   'Error opening output file ^FILE on Fortran unit ^UNIT.',
     :   STATUS )
         GO TO 99
      ELSE
         OUTOPN = .TRUE.
      END IF

*  Determine if the input files to be processed are for ADAM A-tasks.
      CALL PAR_GET0L( 'ATASK', ATASK, STATUS )

*  See whether only a single prologue per input file is expected.
      CALL PAR_GET0L( 'SINGLE', SINGLE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Output a heading for the list of files being processed.
      IF ( NFILE .EQ. 1 ) THEN
         CALL MSG_OUT( 'PROCESSING1', '   Processing file...',
     :                 STATUS )
      ELSE
         CALL MSG_OUT( 'PROCESSING', '   Processing files...',
     :                 STATUS )
      ENDIF
      CALL MSG_OUT( 'BLANK', ' ', STATUS )

*  Initialise the total count of prologues processed.
      TOTPRO = 0

*  Read the name of each input file from the scratch file.
      DO 2 IFILE = 1, NFILE
         CALL SST_GET( SCRAT, FNAME, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Show which file is being processed.
         CALL MSG_SETI( 'IFILE', IFILE )
         CALL MSG_SETC( 'FILE', FNAME )
         CALL MSG_OUT( 'CURRENT_FILE', '      ^IFILE: ^FILE', STATUS )

*  Open the input file.
         CALL FIO_OPEN( FNAME, 'READ', 'LIST', 0, FDIN, STATUS )
         CALL FIO_UNIT( FDIN, SCB_IN, STATUS )

*  Check for errors, setting a suitable STATUS value and reporting the
*  error.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'FNAME', FNAME )
            CALL MSG_SETI( 'UNIT', SCB_IN )
            CALL ERR_REP( 'PROHLP_OPENIN',
     :      'Error opening input file ^FNAME on Fortran unit ^UNIT.',
     :      STATUS )
            GO TO 99
         ELSE
            INPOPN = .TRUE.
         END IF

*  Loop to read prologues from the input file, stopping when no
*  prologue line are read or an error occurs. Read only a single
*  prologue from each file unless SINGLE is .FALSE..
         NPRO = 0
1        CONTINUE                ! Start of 'DO WHILE' loop
         IF ( ( NPRO .EQ. 0 ) .OR. ( .NOT. SINGLE ) ) THEN
            CALL SST_RDPRO( STATUS )

*  Translate each prologue into a help library entry and count the
*  number translated successfully.
            IF ( SCB_NLINE .GT. 0 ) THEN
               CALL SST_TRHLP( ATASK, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  NPRO = NPRO + 1
                  GO TO 1
               END IF
            END IF
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Check that at least one prologue is read from each file. Report an
*  error if none was found.
         IF ( NPRO .LE. 0 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'FILE', FNAME )
            CALL ERR_REP( 'PROHLP_NOPRO',
     :      'No prologue found in input file ^FILE', STATUS )
            GO TO 99
         END IF

*  If more than one prologue was expected, then show how many were
*  found.
         IF ( .NOT. SINGLE ) THEN
            IF ( NPRO .EQ. 1 ) THEN
               CALL MSG_OUT( 'NPROLOGUES1',
     :                       '         1 prologue found', STATUS )
            ELSE
               CALL MSG_SETI( 'NPRO', NPRO )
               CALL MSG_OUT( 'NPROLOGUES',
     :                       '         ^NPRO prologues found', STATUS )
            END IF
         END IF

*  Count the total number of prologues processed.
         TOTPRO = TOTPRO + NPRO

*  Close the input file if opened.
         IF ( INPOPN ) THEN
            CALL FIO_CLOSE( FDIN, STATUS )
            INPOPN = .FALSE.
         END IF
2     CONTINUE

*  Arrive here when all the input files have been processed, or earlier
*  if an error occurred.
99    CONTINUE

*  Close the last input file, the output file and the scratch file.
      IF ( INPOPN ) CALL FIO_CLOSE( FDIN, STATUS )
      IF ( OUTOPN ) CALL FIO_CLOSE( FDOUT, STATUS )
      CLOSE( SCRAT, STATUS = 'DELETE' )

*  Release the I/O channels.
      CALL FIO_PUNIT( SCRAT, STATUS )

*  If there were no errors, then report successful completion.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_OUT( 'BLANK', ' ', STATUS )
         CALL MSG_SETI( 'NFILE', NFILE )
         IF ( NFILE .EQ. 1 ) THEN
            CALL MSG_SETC( 'FILES', 'file' )
         ELSE
            CALL MSG_SETC( 'FILES', 'files' )
         END IF
         CALL MSG_SETI( 'TOTPRO', TOTPRO )
         IF ( TOTPRO .EQ. 1 ) THEN
            CALL MSG_SETC( 'PROLOGUES', 'prologue' )
         ELSE
            CALL MSG_SETC( 'PROLOGUES', 'prologues' )
         END IF
         CALL MSG_OUT( 'ALL_DONE',
     :                 '   ^NFILE ^FILES (^TOTPRO ^PROLOGUES) ' //
     :                 'processed successfully', STATUS )

*  Say where the output is.
         CALL MSG_SETC( 'OUTFILE', OUTFIL )
         CALL MSG_OUT( 'OUT_FILE',
     :                 '   Help library information written to file ' //
     :                 '^OUTFILE', STATUS )
         CALL MSG_OUT( 'BLANK', ' ', STATUS )
      END IF

*  If an error occurred, then report contextual information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PROHLP_ERR',
     :   'PROHLP: Error converting routine prologues into help ' //
     :   'library entries.', STATUS )
      END IF

      END
* @(#)prohlp.f   1.4   94/12/05 15:52:26   96/07/05 10:27:40
