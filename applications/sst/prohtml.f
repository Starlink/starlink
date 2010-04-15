      SUBROUTINE PROHTML( STATUS )
*+
*  Name:
*     PROHTML

*  Purpose:
*     Converts routine prologue information into html documentation.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PROHTML( STATUS )

*  Description:
*     This application reads a series of Fortran 77 source code files
*     containing prologue information formatted using STARLSE, EDSTAR
*     or their conventions and produces an output file containing html
*     documentation for each routine. The documentation format may be
*     chosen to suit either a package of application programs (i.e.
*     ADAM A-tasks) or a set of ordinary subroutines or functions, such
*     as a subroutine library. The output can be formatted for use as
*     an inclusion in a LaTeX document for processing by the LaTeX2html
*     package.

*  Usage:
*     PROHTML IN [OUT]

*  ADAM Parameters:
*     ATASK = _LOGICAL (Read)
*        If ATASK is set to TRUE, then a style of documentation
*        suitable for a package of application programs (i.e. ADAM
*        A-tasks) is produced. If it is set to FALSE, then the
*        documentation produced is suitable for a subroutine library.
*        [TRUE]
*     IN() = LITERAL (Read)
*        A list of (optionally wild-carded) file specifications which
*        identify the Fortran 77 source code files to be used for
*        input. Up to 10 values may be given, but only a single
*        specification such as '*.f' is normally required.
*
*        If the SINGLE parameter is set to TRUE (the default), then
*        only a single prologue will be expected in each input file. If
*        it is set to FALSE, then there is no limit to the number of
*        prologues which may be held in each input file.
*     INCLUSION = _LOGICAL (Read)
*        If INCLUSION is set to TRUE then the output document will
*        contain LaTeX2html commands so that it can be included as part
*        of an existing document. The environments will assume that the
*        descriptions are part of a section and each routine will be
*        assigned to a subsection of this. The html parts will be part
*        of an environment that forces LaTeX to ignore them. If used
*        this is specific to the LaTeX2html package.
*        [TRUE]
*     OUT = FILE (Write)
*        The output file to which the Latex documentation will be
*        written. [prohtml.html]
*     PACKAGE = LITERAL (Read)
*        Name of package. Only used if SEPARATE is TRUE.
*     REFORMAT = _LOGICAL (Read)
*        If REFORMAT is set to FALSE then the current formatting (line
*        breaks, indentation etc.) will not be modified in the final
*        document. If TRUE then the text formatting facilities of HTML
*        will be used, so paragraphs will scale to fit the browser and
*        lists will be detected.
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
*     SEPARATE = LITERAL (Read)
*        If the output is not to be used in a LaTeX document (INCLUSION
*        is FALSE), then the output can be distributed in separate
*        files, one for each input file (called the same name but with
*        the file extension .html). If this option is chosen then the
*        OUT file will contain an index of these files.

*  Examples:
*     PROHTML MYPROG.F MYPROG.HTML
*        Extracts prologue information from the application program
*        source code held in the file MYPROG.FOR and produces a html
*        document for it.  The html output is written to the file
*        MYPROG.HTML.
*     PROHTML *.F OUT=SUBS.HTML ATASK=FALSE
*        Extracts prologue information for a subroutine library, whose
*        source code resides in the files *.F, one routine per file.
*        A html document describing the routines is written to the
*        file SUBS.HTML.

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

*  Latex Definitions:
*     If used the INCLUSION option will use the special environments
*     defined by the LaTeX2html style files (html.sty).

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-DEC-1994 (PDRAPER):
*        Original version. Derived from RFWS's PROLAT.
*     {enter_changes_here}

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

*  External References:
      EXTERNAL CHR_LEN           ! Used length of string
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER * ( 30 ) PACK    ! Name of package
      CHARACTER * ( FIO__SZFNM ) FNAME ! Input file name
      CHARACTER * ( FIO__SZFNM ) OUTFIL ! Output file name
      CHARACTER * ( FIO__SZFNM ) TMPFIL ! Output file name
      CHARACTER * ( FIO__SZFNM ) SPEC( MXSPEC ) ! File specifications
      CHARACTER * ( SST__SZLIN ) BUF ! Local buffer
      INTEGER FDIN               ! Input file descriptor
      INTEGER FDOUT              ! Output file descriptor
      INTEGER FDTMP              ! Output file descriptor
      INTEGER IFILE              ! Loop counter for input files
      INTEGER NFILE              ! Number of input files
      INTEGER NPRO               ! Number of prologues read
      INTEGER NSPEC              ! Number of file specifications
      INTEGER OUTPUT             ! I/O unit for main output file
      INTEGER SCRAT              ! I/O unit for scratch file
      INTEGER STRLEN             ! Length of string
      INTEGER TOTPRO             ! Total prologues processed
      LOGICAL ATASK              ! Whether processing ADAM A-tasks
      LOGICAL INDEX              ! Main output file needs an index
      LOGICAL INPOPN             ! Input file is open
      LOGICAL LATEX              ! Is a LaTeX document required?
      LOGICAL OUTOPN             ! Output file is open
      LOGICAL PREFOR             ! Save current formatting
      LOGICAL SINGLE             ! Whether single prologue/file expected
      LOGICAL TMPOPN             ! Output file is open

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain I/O unit for scratch file.
      CALL FIO_GUNIT( SCRAT, STATUS )

*  Status of input and output files.
      INPOPN = .FALSE.
      OUTOPN = .FALSE.
      TMPOPN = .FALSE.

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
         CALL ERR_REP( 'PROHTML_NOFILES',
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
         CALL ERR_REP( 'PROHTML_OPENOUT',
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

*  See whether a Latex document is required.
      CALL PAR_GET0L( 'INCLUSION', LATEX, STATUS )

*  If we're not expanding to LaTeX then do we want to produce an index
*  and separate the output into different source files?
      INDEX = .FALSE.
      IF ( .NOT. LATEX ) THEN
         CALL PAR_GET0L( 'SEPARATE', INDEX, STATUS )

*  Need to swap the output file unit number so that it can be used for
*  each of the output files.
         OUTPUT = SCB_OUT

*  Get a name for the package.
         CALL PAR_GET0C( 'PACKAGE', PACK, STATUS )
      END IF

*  See if the formatting can be changed.
      CALL PAR_GET0L( 'REFORMAT', PREFOR, STATUS )
      PREFOR = .NOT. PREFOR
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Start the html index file if used.
      IF ( INDEX ) THEN
         CALL SST_STHTM( FDOUT, PACK, STATUS )
      END IF

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

*  Check for errors.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'FILE', FNAME )
            CALL MSG_SETI( 'UNIT', SCB_IN )
            CALL ERR_REP( 'PROHTML_OPENIN',
     :      'Error opening input file ^FILE on Fortran unit ^UNIT.',
     :      STATUS )
            GO TO 99
         ELSE
            INPOPN = .TRUE.
         END IF

         IF ( INDEX ) THEN

*  Open an output file, whose name corresponds to the input file.
            TMPFIL = FNAME
            CALL SST_NWEXT( '.html', TMPFIL, STATUS )
            CALL FIO_OPEN( TMPFIL, 'WRITE', 'LIST', 0, FDTMP, STATUS )
            CALL FIO_UNIT( FDTMP, SCB_OUT, STATUS )

*  Check for errors.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETC( 'FNAME', TMPFIL )
               CALL MSG_SETI( 'UNIT', SCB_OUT )
               CALL ERR_REP( 'PROHTML_OPENIN',
     :         'Error opening output file ^FILE on Fortran unit ^UNIT.',
     :         STATUS )
               GO TO 99
            ELSE
               TMPOPN = .TRUE.
            END IF

*  Add an index to this file.
            CALL SST_NWEXT( ' ', TMPFIL, STATUS )
            STRLEN = CHR_LEN( TMPFIL )
            BUF = '<A HREF="'//TMPFIL( : STRLEN )//'.html"> '//
     :                         TMPFIL( : STRLEN )//'</A>'
            CALL FIO_WRITE( FDOUT, BUF, STATUS )
            CALL FIO_WRITE( FDOUT, '<BR>', STATUS )
         END IF

*  Loop to read prologues from the input file, stopping when no
*  prologue line are read or an error occurs. Read only a single
*  prologue from each file unless SINGLE is .FALSE..
         NPRO = 0
1        CONTINUE                ! Start of 'DO WHILE' loop
         IF ( ( NPRO .EQ. 0 ) .OR. ( .NOT. SINGLE ) ) THEN
            CALL SST_RDPRO( STATUS )
            IF ( SCB_NLINE .GT. 0 ) THEN

*  Translate each prologue into part of the output html document and
*  count the number translated successfully.
               CALL SST_TRHTM( ATASK, LATEX, PREFOR, STATUS )
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
            CALL ERR_REP( 'PROHTML_NOPRO',
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

*  Close input file if opened.
         IF ( INPOPN ) THEN
            CALL FIO_CLOSE( FDIN, STATUS )
            INPOPN = .FALSE.
         END IF

*  Also close the output file.
         IF ( TMPOPN ) THEN
            CALL FIO_CLOSE( FDTMP, STATUS )
            TMPOPN = .FALSE.
            CALL MSG_SETC( 'FILE', TMPFIL )
            CALL MSG_OUT( 'HTML_FILE', '           -->^FILE.html',
     :                    STATUS )
         END IF
2     CONTINUE

*  Arrive here when all the input files have been processed, or earlier
*  if an error occurred.
99    CONTINUE

*  Close the last input file, the output file and the scratch file.
      IF ( INPOPN ) CALL FIO_CLOSE( FDIN, STATUS )
      IF ( OUTOPN ) CALL FIO_CLOSE( FDOUT, STATUS )
      IF ( TMPOPN ) CALL FIO_CLOSE( FDTMP, STATUS )
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
     :                 '   Html output written to file ^OUTFILE',
     :                 STATUS )
         CALL MSG_OUT( 'BLANK', ' ', STATUS )
      END IF

*  If an error occurred, then report contextual information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PROHTML_ERR',
     :   'PROHTML: Error converting routine prologues into html ' //
     :   'documentation.', STATUS )
      END IF

* @(#)prohtml.f   1.6   95/03/06 10:56:40   96/07/05 10:27:41
      END
