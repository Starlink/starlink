      SUBROUTINE FORSTATS( STATUS )
*+
*  Name:
*     FORSTATS

*  Purpose:
*     Produces statistics on Fortran 77 source code and comments.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FORSTATS( STATUS )

*  Description:
*     This application analyses a sequence of Fortran 77 source code
*     files, divides their contents into program units, and produces
*     statistics about the number and distribution of code and comment
*     lines in each unit. These statistics are compared with typical
*     values expected for well-crafted Fortran 77 code and obvious
*     deviations from the expected values are flagged.
*
*     It is intended as a quick-look tool for assessing the size and
*     likely quality of a Fortran 77 software system and is based on
*     the observation that adequate commenting is one of the first
*     things to be sacrificed if corners are cut during code
*     development.  A relatively simple analysis of comment and code
*     lines can therefore reveal if this has occurred and can highlight
*     potential trouble spots for visual inspection.

*  Usage:
*     FORSTATS IN [OUT]

*  ADAM Parameters:
*     IN() = LITERAL (Read)
*        A list of (optionally wild-carded) file specifications which
*        identify the Fortran 77 source code files to be used for
*        input.  Up to 10 values may be given, but only a single
*        specification such as '*.FOR' is normally required. There is
*        no limit to the number of program units which may be held in
*        each input file.
*     KEY = _LOGICAL (Read)
*        If KEY is set to TRUE, then a explanatory key will be appended
*        to the output statistics file to describe how to interpret the
*        values it contains. If KEY is set to FALSE, no such key will
*        be appended. This parameter behaves as a latch and remains set
*        to the value previously used until a new value is specified on
*        the command line. [TRUE]
*     OUT = FILE (Write)
*        The file to which the statistics derived from the input source
*        code will be written. This file is intended for printing.
*        [FORSTATS.LIS]

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     FORSTATS *.FOR
*        Analyses the source code held in the files *.FOR and writes a
*        summary of the derived statistics to the default output file
*        FORSTATS.LIS. This file may then be printed.
*     FORSTATS SOFTWARE.FOR SOFTWARE.LIS NOKEY
*        Analyses the source code held in the file SOFTWARE.FOR and
*        writes the derived statistics to the file SOFTWARE.LIS. No
*        explanatory key is written to the output file on this or
*        subsequent invocations of FORSTATS until a TRUE value is given
*        for the KEY parameter on the command line.
*     FORSTATS IN=["A*.FOR","B*.FOR"] OUT=ANALYSIS.LIS
*        In this example, a sequence of input file specifications is
*        given and each will be analysed in turn. The combined
*        statistics will be written to the file ANALYSIS.LIS.

*  Notes:
*     This is a general-purpose tool and may be used on any Fortran 77
*     software system, since it does not depend on any particular
*     layout or format. However, the analysis performed does include a
*     test for the existence of prologue information. For this purpose,
*     prologues are taken to be delimited by '+' or '-' characters
*     appearing in the second column of a comment line. Note that
*     unlike some other SST applications, both these characters are
*     considered as equivalent by FORSTATS; i.e. the first occurrence
*     begins a prologue and the second occurrence ends it, regardless
*     of which character is used.

*  Timing:
*     The execution time is approximately proportional to the total
*     number of source code lines supplied for analysis. The time will
*     be increased somewhat if the code resides in a large number of
*     separate files, due to the need to open and close each file.

*  Implementation Deficiencies:
*     -  No account is taken of end-of-line comments (i.e.  they are
*     ignored and treated as part of the code).

*  Copyright:
*     Copyright (C) 1989, 1990, 1994 Science & Engineering Research Council.
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
*     10-AUG-1989 (RFWS):
*        Original version.
*     3-AUG-1990 (RFWS):
*        Changed to call SST_CNTAC instead of CNTCC.
*     8-AUG-1990 (RFWS):
*        Changed to call SST_GTEND instead of GTEND.
*     8-AUG-1990 (RFWS):
*        Changed to call SST_GTPUN instead of GTMODN.
*     5-SEP-1990 (RFWS):
*        Substantial re-write to move the program-unit analysis code
*        into a separate routine.
*     6-SEP-1990 (RFWS):
*        Fixed problem whereby program units terminated by an
*        end-of-file were not included in the output summary.
*     12-SEP-1990 (RFWS):
*        Fixed typo in output summary heading and revised the prologue
*        documentation.
*     17-SEP-1990 (RFWS):
*        Close scratch file with explicit delete status.
*     28-SEP-1990 (RFWS):
*        Changed to take account of code lines which have end-of-line
*        comments.
*     28-SEP-1990 (RFWS):
*        Fixed bug causing certain of the total statistics to include
*        the last program unit twice.
*     5-DEC-1994 (PDRAPER):
*        Changed file handling to use FIO_OPEN instead of OPEN. This is
*        a more portable arrangement. Removed unused IOSTATs from WRITE
*        statements.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'SST_PAR'          ! SST_ constants
      INCLUDE 'FIO_PAR'          ! FIO_ public constants

*  Global Variables:
      INCLUDE 'SST_SCB'          ! SST_ Source Code Buffer

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL SST_OKCHR
      CHARACTER * ( 1 ) SST_OKCHR ! Marker for out-of-range values
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Constants:
      INTEGER MXSPEC             ! Max. number of file specifications
      PARAMETER ( MXSPEC = 10 )

      INTEGER SZFFLD             ! Size of output field for file names
      PARAMETER ( SZFFLD = 18 )

      INTEGER SZPUNM             ! Size of a program unit name
      PARAMETER ( SZPUNM = 12 )

*  Local Variables:
      CHARACTER * ( 1 ) PUTYPE   ! Program unit type character
      CHARACTER * ( 132 ) BUF    ! Text output buffer
      CHARACTER * ( 30 ) DATE    ! Current date string
      CHARACTER * ( FIO__SZFNM ) FNAME ! Input file name
      CHARACTER * ( FIO__SZFNM ) OUTFIL ! Output file name
      CHARACTER * ( FIO__SZFNM ) SPEC( MXSPEC ) ! Wild card file spec
      CHARACTER * ( SZFFLD ) FFIELD ! Buffer for filename text
      CHARACTER * ( SZPUNM ) PUNAME ! Program unit name
      INTEGER ARG                ! Dummy argument for UNITAV function
      INTEGER COMMFR             ! Fraction (percent) of code commenting
      INTEGER FDIN               ! Input file descriptors
      INTEGER FDOUT              ! Output file descriptors
      INTEGER IFILE              ! Loop counter for input files
      INTEGER ISPEC              ! Loop counter for file specifications
      INTEGER MEANCC             ! Mean alphanumerics per comment line
      INTEGER MXCODE             ! Maximum contiguous code lines
      INTEGER NBLANK             ! Number of blank lines
      INTEGER NC                 ! Number of characters
      INTEGER NCCHR              ! Number of in-code comment characters
      INTEGER NCCOM              ! Number of in-code comment lines
      INTEGER NCF                ! Number of characters in filename
      INTEGER NCF1               ! Start of file name field
      INTEGER NCF2               ! End of file name field
      INTEGER NCFU               ! Start of usable filename
      INTEGER NCODBL             ! Number of code blocks
      INTEGER NCODE              ! Number of (non-blank) code lines
      INTEGER NCODEU             ! No. code lines without eol comments
      INTEGER NCOM               ! Number of (non-blank) comment lines
      INTEGER NCOMBL             ! No. contiguous in-code comment blocks
      INTEGER NFILE              ! Number of files read
      INTEGER NLINE              ! Number of lines in program unit
      INTEGER NNBLLN             ! Number of non-blank lines
      INTEGER NPROL              ! Number of (non-blank) prologue lines
      INTEGER NPROLB             ! Number of prologue BEGIN lines
      INTEGER NPROLE             ! Number of prologue END lines
      INTEGER NSPEC              ! Number of file specifications
      INTEGER NSUSP              ! Number of suspect program units
      INTEGER NTICKS             ! Number of time ticks
      INTEGER NUNIT              ! Number of program units read
      INTEGER SCRAT              ! I/O unit for scratch file
      INTEGER TMXCOD             ! Overall max length of a code block
      INTEGER TNBLAN             ! Overall count of blank lines
      INTEGER TNCCHR             ! Overall number of comment characters
      INTEGER TNCCOM             ! Overall in-code comment line count
      INTEGER TNCODB             ! Overall number of code blocks
      INTEGER TNCODE             ! Overall number of code lines
      INTEGER TNCODU             ! Overall code lines, no eol comments
      INTEGER TNCOM              ! Overall number of comment lines
      INTEGER TNCOMB             ! Overall number of comment blocks
      INTEGER TNLINE             ! Overall number of lines
      INTEGER TNNBLL             ! Overall number of non-blank lines
      INTEGER TNPROB             ! Overall number of prologue BEGINs
      INTEGER TNPROL             ! Overall number of prologue lines
      LOGICAL EOF                ! Whether at end of input file
      LOGICAL FILOPN             ! Input file is still open
      LOGICAL KEY                ! Whether explanatory key is required
      LOGICAL NEWFIL             ! Whether program unit is in a new file
      LOGICAL PRGOK              ! Program unit is OK (not suspect)?
      LOGICAL XAVCDB             ! Whether mean code block length is OK
      LOGICAL XBLANK             ! Whether no. of blank lines is OK
      LOGICAL XCODE              ! Whether no. of code lines is OK
      LOGICAL XCOMMF             ! Whether in-code comment fraction OK
      LOGICAL XMXCOD             ! Whether max. code block length is OK
      LOGICAL XPROL              ! Whether no. prologue comments is OK
      LOGICAL XPROLB             ! Whether no. of prologue BEGINs is OK
      REAL AVCDBL                ! Mean length uncommented code blocks
      REAL AVCMBL                ! Average in-code comment block length
      REAL EFFCOM                ! Effective number of comment lines
      REAL TEFFCO                ! Overall effective no. comment lines

*  Internal References:
      INTEGER UNITAV             ! Calculate average over program units
      UNITAV( ARG ) = NINT( REAL( ARG ) / REAL( NUNIT ) )

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise and obtain input parameters.
*  ======================================
*  Initialise variables for forming overall statistics.
      NFILE = 0
      NSUSP = 0
      NUNIT = 0
      TEFFCO = 0.0
      TMXCOD = 0
      TNBLAN = 0
      TNCCHR = 0
      TNCCOM = 0
      TNCODB = 0
      TNCODE = 0
      TNCODU = 0
      TNCOM = 0
      TNCOMB = 0
      TNLINE = 0
      TNNBLL = 0
      TNPROB = 0
      TNPROL = 0

*  Initialise FFIELD variable.
      FFIELD = ' '

*  Get I/O units for the scratch file.
      CALL FIO_GUNIT( SCRAT, STATUS )

*  Get the input file specifications.
      CALL PAR_GET1C( 'IN', MXSPEC, SPEC, NSPEC, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Write the list of input files to be processed into a scratch file.
      CALL SST_FWILD( NSPEC, SPEC, SCRAT, NFILE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  If no files were found, then report an error.
      IF ( NFILE .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'SPEC', SPEC( 1 ) )
         CALL ERR_REP( 'FORSTATS_NOFILES',
     :   'No input files found matching the specification ''^SPEC''...',
     :   STATUS )
         GO TO 99
      END IF

*  Report how many files are to be processed.
      CALL MSG_OUT( 'BLANK', ' ', STATUS )
      IF ( NFILE .EQ. 1 ) THEN
         CALL MSG_OUT( 'NFILE1', '   1 file to process...', STATUS )
      ELSE
         CALL MSG_SETI( 'NFILE', NFILE )
         CALL MSG_OUT( 'NFILE', '   ^NFILE files to process...',
     :                 STATUS )
      END IF
      CALL MSG_OUT( 'BLANK', ' ', STATUS )

*  Rewind the scratch file ready for re-reading.
      REWIND( SCRAT )

*  Obtain and prepare the output summary file.
*  ==========================================
*  Obtain the name of the output file.
      CALL PAR_GET0C( 'OUT', OUTFIL, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Open the output file.
      CALL FIO_OPEN( OUTFIL, 'WRITE', 'LIST', 0, FDOUT, STATUS )
      CALL FIO_UNIT( FDOUT, SCB_OUT, STATUS )

*  Check for errors, setting a suitable status value and reporting the
*  error.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'FILE', OUTFIL )
         CALL MSG_SETI( 'UNIT', SCB_OUT )
         CALL ERR_REP( 'FORSTATS_OPENOUT',
     :   'Error opening output file ^FILE on Fortran unit ^UNIT.',
     :   STATUS )
         GO TO 99
      END IF

*  See if an explanatory key is required in the output file.
      CALL PAR_GET0L( 'KEY', KEY, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Produce a heading for the output summary.
      CALL PSX_TIME( NTICKS, STATUS )
      CALL PSX_CTIME( NTICKS, DATE, STATUS )
      CALL SST_PUT( 0, 'Statistics on Fortran 77 source code ' //
     :                 'produced by FORSTATS on '// DATE, STATUS )
      CALL SST_PUT( 0, ' ', STATUS )

*  Display the file specifications as part of the heading.
      DO 1 ISPEC = 1, NSPEC
         IF ( ISPEC .EQ. 1 ) THEN
            CALL SST_PUT( 0,
     :                    'File specification: ' // SPEC( ISPEC ),
     :                    STATUS )
         ELSE
            CALL SST_PUT( 20, SPEC( ISPEC ), STATUS )
         END IF
1     CONTINUE
      CALL SST_PUT( 0, ' ', STATUS )
      CALL SST_PUT( 0, ' ', STATUS )

*  Produce overall column headings for the output summary.
      WRITE( BUF, 11 )
      CALL SST_PUT( 17, BUF( 18 : 111 ), STATUS )
11    FORMAT(           '+----------------',
     :                  '+-------------------------------',
     :                  '+-------------------',
     :                  '+-------------',
     :                  '+--------------------------+',
     :                  '--------------------+' )

      CALL SST_PUT( 17, '|   Overall Numbers of Lines    ' //
     :                  '|  Contiguous Code  ' //
     :                  '|   Prologue  ' //
     :                  '|     In-Code Comments     |', STATUS )

      WRITE( BUF, 11 )
      CALL SST_PUT( 0, BUF, STATUS )

*  Produce individual column sub-headings.
      CALL SST_PUT( 0,  '| PrgUnit   Type ' //
     :                  '| Total  Blank NonBl Code  Comm ' //
     :                  '| N_Blk AvBlk MxBlk ' //
     :                  '| N_Pro PrLin ' //
     :                  '|  N_Com AvLin AvChr ComFr ' //
     :                  '| File               |', STATUS )

      WRITE( BUF, 11 )
      CALL SST_PUT( 0, BUF, STATUS )

*  Read the input files.
*  ====================
*  Indicate that files are being processed.
      IF ( NFILE .EQ. 1 ) THEN
         CALL MSG_OUT( 'PROCESSING1',
     :                 '   Producing statistics on file...' , STATUS )
      ELSE
         CALL MSG_OUT( 'PROCESSING',
     :                 '   Producing statistics on files...' , STATUS )
      END IF
      CALL MSG_OUT( 'BLANK', ' ', STATUS )

*  Input file isn't open.
      FILOPN = .FALSE.

*  Loop to process each input file.
      DO 3 IFILE = 1, NFILE

*  Read the input file name from the scratch file.
         CALL SST_GET( SCRAT, FNAME, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Show which file is being processed.
         CALL MSG_SETI( 'IFILE', IFILE )
         CALL MSG_SETC( 'FILE', FNAME )
         CALL MSG_OUT( 'CURRENT_FILE', '      ^IFILE: ^FILE', STATUS )

*  Open the input file.
         NEWFIL = .TRUE.
         CALL FIO_OPEN( FNAME, 'READ', 'LIST', 0, FDIN, STATUS )
         CALL FIO_UNIT( FDIN, SCB_IN, STATUS )

*  Check for errors when opening input files, noting the resulting
*  missing files in the output summary.
         IF ( STATUS .NE. SAI__OK ) THEN
            NC = MAX( 1, CHR_LEN( FNAME ) )
            CALL SST_PUT( 0,
     :      '<<< Cannot read file ' // FNAME( : NC ) // ' >>>',
     :                    STATUS )

*  Set a suitable status value and report the error.
            CALL MSG_SETC( 'FILE', FNAME )
            CALL MSG_SETI( 'UNIT', SCB_IN )
            CALL ERR_REP( 'FORSTATS_OPENIN',
     :      'Error opening file ^FILE for reading on Fortran unit ' //
     :      '^UNIT.', STATUS )

*  Flush the error and go on to process the next file.
            CALL ERR_FLUSH( STATUS )
            GO TO 3
         ELSE

*  Input file opened.
            FILOPN = .TRUE.
         END IF

*  Obtain statistics for each program unit.
*  =======================================
*  Loop to analyse each program unit in the input file.
         EOF = .FALSE.
2        CONTINUE                ! Start of "DO WHILE" loop
         IF ( ( STATUS .EQ. SAI__OK ) .AND. ( .NOT. EOF ) ) THEN

*  Analyse the next program unit, checking for errors and end-of-file.
            CALL SST_ANALP( PUNAME, PUTYPE, MXCODE, NBLANK, NCODBL,
     :                      NCODE, NCODEU, NCOM, NCOMBL, NLINE, NPROL,
     :                      NPROLB, NPROLE, NCCHR, EOF, EFFCOM,
     :                      STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Analyse the statistics for each program unit.
*  ============================================
*  Only produce statistics for those program units with non-blank lines
*  (this prevents blank regions at the end of input files from being
*  counted as program units).  Count the program units used.
            NNBLLN = NLINE - NBLANK
            IF ( NNBLLN .GT. 0 ) THEN
               NUNIT = NUNIT + 1

*  Display the program unit name and type.
               IF ( PUNAME .NE. '<unknown>' ) THEN
                  IF ( PUTYPE .EQ. 'B' ) THEN
                     CALL MSG_SETC( 'TYPE', 'block data' )
                  ELSE IF ( PUTYPE .EQ. 'F' ) THEN
                     CALL MSG_SETC( 'TYPE', 'function' )
                  ELSE IF ( PUTYPE .EQ. 'P' ) THEN
                     CALL MSG_SETC( 'TYPE', 'program' )
                  ELSE IF ( PUTYPE .EQ. 'S' ) THEN
                     CALL MSG_SETC( 'TYPE', 'subroutine' )
                  END IF
                  CALL MSG_SETC( 'PROG_UNIT', PUNAME )
                  CALL MSG_OUT( 'PROG_UNIT',
     :            '            ^TYPE ^PROG_UNIT', STATUS )
               ELSE
                  CALL MSG_OUT( 'PROG_UNKNOWN',
     :            '            <unknown program unit>', STATUS )
               END IF

*  Calculate the number of non-blank in-code comments by removing the
*  total count of prologue comment lines from the total comment line
*  count.
               NCCOM = NCOM - ( NPROLB + NPROL + NPROLE )

*  Calculate the mean length of contiguous blocks of uncommented code,
*  the mean number of alphanumeric characters per comment line and the
*  percentage of comments in the code.
               AVCDBL = REAL( NCODEU ) / REAL( MAX( NCODBL, 1 ) )
               AVCMBL = REAL( NCCOM ) / REAL( MAX( NCOMBL, 1 ) )
               MEANCC = NINT( REAL( NCCHR ) / REAL( MAX( NCCOM, 1 ) ) )
               COMMFR = NINT( 100.0 * EFFCOM /
     :                        MAX( 1.0E-6, EFFCOM + REAL( NCODE ) ) )

*  Test if the statistics for the current program unit lie within their
*  acceptable range. (Note that these limits have been derived
*  empirically by running the application on a variety of source code
*  classified as acceptable or unacceptable by visual inspection.)
               XAVCDB = AVCDBL .LE. 7.0
               XBLANK = ( NBLANK .LE. ( 2 * NLINE ) / 5 ) .OR.
     :                  ( NLINE .LE. 20 )
               XCODE =  NCODE .LE. 250
               XCOMMF = COMMFR .GE.
     :                  MIN( MAX( 0, ( NCODE - 4 ) / 6 ), 10 )
               XMXCOD = MXCODE .LE. 20
               XPROL = NPROL .GE. MIN( MAX( 9, NCODE / 3 ), 20 )
               XPROLB = ( NPROLB .EQ. 1 ) .AND. ( NPROLE .EQ. 1 )

*  Note if the program unit is suspect on any grounds (i.e. lies
*  outside any of the acceptable ranges). Count the number of program
*  units which fail this test and inform the user when this happens.
               PRGOK = ( XAVCDB .AND. XBLANK .AND. XCODE .AND.
     :                   XCOMMF .AND. XMXCOD .AND. XPROL .AND. XPROLB )
               IF ( .NOT. PRGOK ) THEN
                  NSUSP = NSUSP + 1
                  CALL MSG_OUT( 'SUSPECT',
     :            '               ...this program unit is suspect',
     :                          STATUS )
               END IF

*  Display the program unit statistics in the output file.
*  ======================================================
*  If a new input file is open, format the file name for output within
*  the allowed field width.
               IF ( NEWFIL ) THEN
                  NEWFIL = .FALSE.
                  NCF1 = INDEX( FNAME, ']' ) + 1
                  NCF1 = MIN( NCF1, LEN( FNAME ) )
                  NCF2 = INDEX( FNAME( NCF1 : ), ';' ) + NCF1 - 2
                  NCF2 = MAX( NCF1, NCF2 )
                  NCF = NCF2 - NCF1 + 1
                  IF ( NCF .LE. LEN( FFIELD ) ) THEN
                     FFIELD = FNAME( NCF1 : NCF2 )

*  If the name is too long to fit, truncate it on the left and indicate
*  this with a leading ellipsis '...'.
                  ELSE
                     NCFU = NCF2 - LEN( FFIELD ) + 1
                     FFIELD = FNAME( NCFU : NCF2 )
                     FFIELD( 1 : 3 ) = '...'
                  END IF

*  Don't display the same filename more than once; use dittos.
               ELSE
                  FFIELD = ' '
                  FFIELD( NCF / 2 + 1 : NCF / 2 + 1 ) = '"'
               END IF

*  Insert a spacing line between each group of 5 output lines.
               IF ( MOD( NUNIT - 1, 5 ) .EQ. 0 ) THEN
                  WRITE( BUF, 12 )
                  CALL SST_PUT( 0, BUF, STATUS )
12    FORMAT(           '|                ',
     :                  '|                               ',
     :                  '|                   ',
     :                  '|             ',
     :                  '|                          ',
     :                  '|                    |' )

               END IF

*  Write the statistics for the current program unit to the output file
*  together with flag characters to indicate values which are out of
*  range.
               WRITE( BUF, 13 )
     :            SST_OKCHR( PRGOK ),
     :            PUNAME, PUTYPE,   '| ',
     :            NLINE,            ' ',
     :            NBLANK,           SST_OKCHR( XBLANK ),
     :            NNBLLN,           ' ',
     :            NCODE,            SST_OKCHR( XCODE ),
     :            NCOM,             ' |',
     :            NCODBL,           ' ',
     :            AVCDBL,           SST_OKCHR( XAVCDB ),
     :            MXCODE,           SST_OKCHR( XMXCOD ) // ' |',
     :            NPROLB,           SST_OKCHR( XPROLB ),
     :            NPROL,            SST_OKCHR( XPROL ) // ' |',
     :            NCCOM,            ' ',
     :            AVCMBL,           ' ',
     :            MEANCC,           ' ',
     :            COMMFR,           '%' // SST_OKCHR( XCOMMF ) // ' |',
     :            FFIELD
               CALL SST_PUT( 0, BUF, STATUS )
13    FORMAT( '|', A, A, 1X, A, 1X, A, 6( I5, A ), F5.1, A, 4( I5, A ),
     :        F5.1, A, 2( I5, A ), 1X, A, ' |' )

*  Form overall statistics.
*  =======================
*  Accumulate overall statistics (i.e. summing over all program
*  modules).
               TEFFCO = TEFFCO + EFFCOM
               TMXCOD = MAX( TMXCOD, MXCODE )
               TNBLAN = TNBLAN + NBLANK
               TNCCHR = TNCCHR + NCCHR
               TNCCOM = TNCCOM + NCCOM
               TNCODB = TNCODB + NCODBL
               TNCODE = TNCODE + NCODE
               TNCODU = TNCODU + NCODEU
               TNCOM = TNCOM + NCOM
               TNCOMB = TNCOMB + NCOMBL
               TNLINE = TNLINE + NLINE
               TNNBLL = TNNBLL + NNBLLN
               TNPROB = TNPROB + MIN( NPROLB, 1 )
               TNPROL = TNPROL + NPROL
            END IF

*  Return to read the next program unit from the input file.
            GO TO 2
         END IF

*  End of loop for reading each input file. Close file if open.
         IF ( FILOPN ) THEN
            CALL FIO_CLOSE( FDIN, STATUS )
         END IF
3     CONTINUE

*  If no program units were found in the input file(s), the report an
*  error and give up.
      IF ( NUNIT .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'FORSTATS_NOUNIT',
     :   'No program units found in input file(s).', STATUS )
         GO TO 99
      END IF

*  Display the overall statistics.
*  ==============================
*  Write out a heading for the overall statistics.
      WRITE( BUF, 12 )
      CALL SST_PUT( 0, BUF, STATUS )

      WRITE( BUF, 14 )
      CALL SST_PUT( 0, BUF, STATUS )
14    FORMAT(           'Totals/Maxima ---',
     :                  '+-------------------------------',
     :                  '+-------------------',
     :                  '+-------------',
     :                  '+--------------------------',
     :                  '+--------------------+' )

      WRITE( BUF, 12 )
      CALL SST_PUT( 0, BUF, STATUS )

*  Write out the overall statistics.
      WRITE( BUF, 15 )
     :   NUNIT, TNLINE, TNBLAN, TNNBLL, TNCODE, TNCOM, TNCODB, TMXCOD,
     :   TNPROB, TNPROL, TNCCOM, NFILE
      CALL SST_PUT( 0, BUF, STATUS )
15    FORMAT( '|', I4, 1X, 'PrgUnit(s)', 1X, '|', 5I6, 1X, '|', I5,
     :        5X, '-', I6, 2X, '|', I5, I6, 2X, '|', I5, 5X, '-', 5X,
     :        '-', 5X, '-', 3X, '|', I4, 1X, 'File(s)', 8X, '|' )

      WRITE ( BUF, 12 )
      CALL SST_PUT( 0, BUF, STATUS )

*  Write out a heading for the overall averages.
      WRITE ( BUF, 16 )
      CALL SST_PUT( 0, BUF, STATUS )
16    FORMAT(           'Averages --------',
     :                  '+-------------------------------',
     :                  '+-------------------',
     :                  '+-------------',
     :                  '+--------------------------',
     :                  '+--------------------+' )

      WRITE( BUF, 12 )
      CALL SST_PUT( 0, BUF, STATUS )

*  Calculate the overall mean length of contiguous blocks of
*  uncommented code, and the fraction of comments in the code.
      AVCDBL = REAL( TNCODU ) / REAL( MAX( TNCODB, 1 ) )
      COMMFR = NINT( 100.0 * TEFFCO /
     :                       MAX( 1.0E-6, TEFFCO + REAL( TNCODE ) ) )

*  Write out the overall averages.
      WRITE( BUF, 17 )
     :    UNITAV( TNLINE ), UNITAV( TNBLAN ), UNITAV( TNNBLL ),
     :    UNITAV( TNCODE ), UNITAV( TNCOM ), UNITAV( TNCODB ), AVCDBL,
     :    NINT( 100.0 * REAL( TNPROB ) / REAL( NUNIT ) ),
     :    UNITAV( TNPROL ), UNITAV( TNCCOM ),
     :    REAL( TNCCOM ) / MAX( 1.0E-6, REAL( TNCOMB ) ),
     :    NINT( REAL( TNCCHR ) / REAL( MAX( 1, TNCCOM ) ) ), COMMFR
      CALL SST_PUT( 0, BUF, STATUS )
17    FORMAT( '|       -        |', 5I6, ' |', I5, F6.1, 5X, '-  |',
     :        I5, '%', I5, '  |', I5, F6.1, 2I6,
     :        '%  |         -          |' )

      WRITE( BUF, 12 )
      CALL SST_PUT( 0, BUF, STATUS )

      WRITE( BUF, 11 )
      CALL SST_PUT( 0, BUF, STATUS )

      CALL SST_PUT( 0, ' ', STATUS )

*  Produce termination messages.
*  ============================
*  Set up message tokens for reporting the number of program units/files
*  processed.
      CALL MSG_OUT( 'BLANK', ' ', STATUS )
      CALL MSG_SETI( 'NUNIT', NUNIT )

*  Allow for plural wording.
      IF ( NUNIT .EQ. 1 ) THEN
         CALL MSG_SETC( 'UNITS', 'unit' )
      ELSE
         CALL MSG_SETC( 'UNITS', 'units' )
      END IF
      CALL MSG_SETI( 'NFILE', NFILE )
      IF ( NFILE .EQ. 1 ) THEN
         CALL MSG_SETC( 'FILES', 'file' )
      ELSE
         CALL MSG_SETC( 'FILES', 'files' )
      END IF
      CALL MSG_SETI( 'NSUSP', NSUSP )
      IF ( NSUSP .EQ. 1 ) THEN
         CALL MSG_SETC( 'WERE', 'was' )
      ELSE
         CALL MSG_SETC( 'WERE', 'were' )
      END IF

*  Show how many program units were processed.
      IF ( NSUSP .GT. 0 ) THEN
         CALL MSG_OUT( 'FILES_PROC_S',
     :   '   ^NUNIT program ^UNITS processed from ^NFILE input ' //
     :   '^FILES; ^NSUSP ^UNITS ^WERE suspect', STATUS )

*  If suspect units were found, indicate this in the output summary.
         CALL SST_PUT( 47, '* = statistic outside expected range',
     :                 STATUS )
         CALL SST_PUT( 0, ' ', STATUS )

*  Say how many were suspect.
         NC = 0
         CALL CHR_PUTI( NSUSP, BUF, NC )
         CALL CHR_PUTC( ' program unit(s) out of ', BUF, NC )
         CALL CHR_PUTI( NUNIT, BUF, NC )
         CALL CHR_PUTC( ' had out-of-range statistics', BUF, NC )
         CALL SST_PUT( 42, BUF( : NC ), STATUS )
         CALL SST_PUT( 0, ' ', STATUS )

*  Similarly handle the case where no suspect program units were found.
      ELSE
         CALL MSG_OUT( 'FILES_PROC_OK',
     :   '   ^NUNIT program ^UNITS processed from ^NFILE input ' //
     :   '^FILES; no units were suspect', STATUS )
         CALL SST_PUT( 0, ' ', STATUS )
         CALL SST_PUT( 42,
     :   'No program units had out-of-range statistics', STATUS )
         CALL SST_PUT( 0, ' ', STATUS )
      END IF

*  Write an explanatory key, if required.
      IF ( KEY ) THEN
         CALL SST_PUT( 18,
     :                 'Use the NOKEY option to supress the ' //
     :                 'following explanatory key for future ' //
     :                 'invocations of FORSTATS', STATUS )
         CALL SST_PUT( 0, ' ', STATUS )
         CALL SST_FSKEY( STATUS )

*  Otherwise explain how to obtain one.
      ELSE
         CALL SST_PUT( 24,
     :                 'Use the KEY option to obtain an ' //
     :                 'explanatory key for future invocations ' //
     :                 'of FORSTATS', STATUS )
         CALL SST_PUT( 0, ' ', STATUS )
      ENDIF

*  Show where the output has been written.
      CALL MSG_SETC( 'OUTFILE', OUTFIL )
      CALL MSG_OUT( 'OUTPUT_FILE',
     : '   Output summary written to file ^OUTFILE', STATUS )
      CALL MSG_OUT( 'BLANK', ' ', STATUS )

*  Clean up.
*  ========
*  Arrive here if an error occurs.
99    CONTINUE

*  Close the output and scratch files.
      CALL FIO_CLOSE( FDOUT, STATUS )
      CLOSE( SCRAT, STATUS = 'DELETE' )

*  Release the I/O units.
      CALL FIO_PUNIT( SCRAT, STATUS )

*  If an error occurred, then report contextual information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'FORSTATS_ERR',
     :   'FORSTATS: Error producing statistics on Fortran 77 ' //
     :   'source code and comments.', STATUS )
      END IF

      END
* @(#)forstats.f   1.7   94/12/05 15:32:22   96/07/05 10:27:40
