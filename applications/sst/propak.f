      SUBROUTINE PROPAK( STATUS )
*+
*  Name:
*     PROPAK

*  Purpose:
*     Converts routine prologue information into a STARLSE package
*     definition.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PROPAK( STATUS )

*  Description:
*     This application reads a series of Fortran 77 source code files
*     containing prologue information formatted using STARLSE (SUN/105)
*     and produces an output file containing an LSE package definition
*     suitable for use with STARLSE. Help library references may be
*     included if required, allowing the package definition to access
*     help information extracted using the PROHLP application.

*  Usage:
*     PROPAK IN [OUT] PACK [HELP]

*  ADAM Parameters:
*     HELP = LITERAL (Read)
*        Name of the help file to which the package definition should
*        refer for on-line help information (suitable help libraries
*        may be produced using the PROHLP application).  The full file
*        name, including a directory name, should be given.  Logical
*        names may also be used. The help file need not actually exist
*        at the time PROPAK is run.
*
*        If a null (!) or blank value is given for this parameter, then
*        no help library references will be included in the package
*        definition. [!]
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
*        The output file to which the STARLSE package definition will
*        be written.  The VAXset documentation on LSE should be
*        consulted for details of how to use this file to extend the
*        STARLSE editing environment. [PROPAK.LSE]
*     PACK = LITERAL (Read)
*        This value is the name of the LSE package to be generated. To
*        avoid possible clashes with existing STARLSE packages, the
*        prefix characters used on the routines themselves are
*        recommended for use as the package name. For example, if
*        routines in the package have names such as XYZ_ROUTN, then
*        "XYZ" should be used as the package name.
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
*     PROPAK MPK_*.FOR MYPACK.LSE MPK
*        Extracts prologue information from the routines held in the
*        files MPK_*.FOR, one per file, and produces a STARLSE package
*        definition for them. The package is called "MPK" and its
*        definition is written to the file MYPACK.LSE.
*     PROPAK *.FOR PACK=TEST HELP=TEST_DIR:TESTHELP
*        Extracts prologue information for a subroutine library, whose
*        source code resides in the files *.FOR, one routine per file.
*        The resulting definition, of a STARLSE package called "TEST",
*        is written to the default output file PROPAK.LSE. The package
*        contains references to the help library TEST_DIR:TESTHELP,
*        from which on-line help may be obtained when using the package
*        within STARLSE.
*     PROPAK IN=SOURCE.FOR PACK=MYLIB NOSINGLE
*        Extracts prologue information from a sequence of subroutines
*        or functions, all of which are held in the file SOURCE.FOR,
*        and defines a STARLSE package called "MYLIB" in the default
*        output file PROPAK.TEX.
*     PROPAK IN=["A*.FOR","B*.FOR"] HELP=PACK_HELP
*        In this example, a sequence of input file specifications is
*        given. Each will be processed in turn to generate a combined
*        package definition which refers to a help library with the
*        logical name PACK_HELP. The package name will be prompted for.

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
*     15-AUG-1990 (RFWS):
*        Original, derived from the PROLAT routine.
*     12-SEP-1990 (RFWS):
*        Improved the prologue documentation.
*     17-SEP-1990 (RFWS):
*        Close scratch file with explicit delete status.
*     5-DEC-1994 (PDRAPER):
*        Changed OPENs to FIO_OPENs, should be more portable.
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
      INCLUDE 'PAR_ERR'          ! PAR_ error codes

*  Global Variables:
      INCLUDE 'SST_SCB'          ! SST Source Code Buffer

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*
*  Local Constants:
      INTEGER MXARG              ! Maximum no. of routine arguments
      PARAMETER ( MXARG = 5000 )

      INTEGER SZARG              ! Max. length of an argument name
      PARAMETER ( SZARG = 32 )

      INTEGER MXSPEC             ! Max. no of file specifications
      PARAMETER ( MXSPEC = 10 )

      INTEGER SZPACK             ! Max. length of a package name
      PARAMETER ( SZPACK = 32 )

*  Local Variables:
      CHARACTER * ( FIO__SZFNM ) FNAME ! Input file name
      CHARACTER * ( FIO__SZFNM ) HELPFL ! Help file name
      CHARACTER * ( FIO__SZFNM ) OUTFIL ! Output file name
      CHARACTER * ( FIO__SZFNM ) SPEC( MXSPEC ) ! File specifications
      CHARACTER * ( SZARG ) ARG( MXARG ) ! Argument names
      CHARACTER * ( SZARG ) NAME ! Temporary argument name store
      CHARACTER * ( SZPACK ) PACK ! Package name
      INTEGER FDIN               ! Input file descriptor
      INTEGER FDOUT              ! Output file descriptor
      INTEGER I                  ! Loop counter for sorting arguments
      INTEGER IFILE              ! Loop counter for input files
      INTEGER IMAX               ! Max. argument number to sort
      INTEGER NARG               ! Number of routine arguments
      INTEGER NC                 ! Number of characters
      INTEGER NCPACK             ! No. characters in package name
      INTEGER NDIFF              ! Number of different argument names
      INTEGER NFILE              ! Number of input files
      INTEGER NPRO               ! Number of prologues read
      INTEGER NSPEC              ! Number of file specifications
      INTEGER SCRAT              ! I/O unit for scratch file
      INTEGER TOTPRO             ! Total prologues processed
      LOGICAL INPOPN             ! Input file is open
      LOGICAL OUTOPN             ! Output file is open
      LOGICAL HELP               ! Help library references required?
      LOGICAL SINGLE             ! Whether single prologue/file expected
      LOGICAL SORTED             ! Sort completed?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain I/O units for scratch files.
      CALL FIO_GUNIT( SCRAT, STATUS )

*  Status of input and output files.
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
         CALL ERR_REP( 'PROPAK_NOFILES',
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

*  Obtain the package name, determine its length and convert to upper
*  case.
      CALL PAR_GET0C( 'PACK', PACK, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      NCPACK = MAX( 1, CHR_LEN( PACK ) )
      CALL CHR_UCASE( PACK( : NCPACK ) )

*  Obtain the name of the help file, to which the package definition
*  will refer for on-line help information.
      HELP = .TRUE.
      CALL PAR_GET0C( 'HELP', HELPFL, STATUS )

*  Note if no help file was specified.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         HELP = .FALSE.
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( HELPFL .EQ. ' ' ) THEN
         HELP = .FALSE.
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

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
         CALL ERR_REP( 'PROPAK_OPENOUT',
     :   'Error opening output file ^FILE on Fortran unit ^UNIT.',
     :   STATUS )
         GO TO 99
      ELSE
         OUTOPN = .TRUE.
      END IF

*  See whether only a single prologue per input file is expected.
      CALL PAR_GET0L( 'SINGLE', SINGLE, STATUS )

*  Write the definition of the package to the output file. Include a
*  help file definition if a help file was specified.
      CALL SST_PUT( 0, '!  Define the package.', STATUS )
      CALL SST_PUT( 0, 'DEFINE PACKAGE "' //
     :                 PACK( : NCPACK ) // '" -', STATUS )
      IF ( HELP ) THEN
         NC = MAX( 1, CHR_LEN( HELPFL ) )
         CALL SST_PUT( 3, '/HELP_LIBRARY = ' // HELPFL( : NC ) //
     :                    ' -', STATUS )
      END IF
      CALL SST_PUT( 3, '/LANGUAGE = STARLINK_FORTRAN -', STATUS )
      CALL SST_PUT( 3, '/ROUTINE_EXPAND = LSE$PKG_EXPAND_ROUT_ -',
     :              STATUS )
      CALL SST_PUT( 3, '/PARAMETER_EXPAND = LSE$PKG_EXPAND_PARM_',
     :              STATUS )
      CALL SST_PUT( 0, ' ', STATUS )

*  Output a comment for the routine definitions.
      CALL SST_PUT( 0, '!  Define the routines.', STATUS )

*  Display a heading for the list of files being processed.
      IF ( NFILE .EQ. 1 ) THEN
         CALL MSG_OUT( 'PROCESSING1', '   Processing file...',
     :                 STATUS )
      ELSE
         CALL MSG_OUT( 'PROCESSING', '   Processing files...',
     :                 STATUS )
      ENDIF
      CALL MSG_OUT( 'BLANK', ' ', STATUS )

*  Initialise the total count of prologues processed and the number of
*  routine arguments found.
      TOTPRO = 0
      NARG = 0

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
            CALL ERR_REP( 'PROPAK_OPENIN',
     :      'Error opening input file ^FILE on Fortran unit ^UNIT.',
     :      STATUS )
            GO TO 99
         ELSE
            INPOPN = .TRUE.
         END IF

*  Loop to read prologues from the input file, stopping when no
*  prologue lines are read or an error occurs. Read only a single
*  prologue from each file unless SINGLE is .FALSE..
         NPRO = 0
1        CONTINUE                ! Start of 'DO WHILE' loop
         IF ( ( NPRO .EQ. 0 ) .OR. ( .NOT. SINGLE ) ) THEN
            CALL SST_RDPRO( STATUS )

*  Translate each prologue into part of the package definition and
*  count the number translated successfully.
            IF ( SCB_NLINE .GT. 0 ) THEN
               CALL SST_TRPAK( PACK, HELP, MXARG, ARG, NARG, STATUS )
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
            CALL ERR_REP( 'PROPAK_NOPRO',
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

*  Close the input file.
         IF ( INPOPN ) THEN
            CALL FIO_CLOSE( FDIN, STATUS )
            INPOPN = .FALSE.
         END IF
2     CONTINUE

*  Sort the list of arguments accumulated from all the routines into
*  alphabetical order.
      SORTED = .FALSE.
      IMAX = NARG
3     CONTINUE                   ! Start of "DO WHILE" loop
      IF ( .NOT. SORTED ) THEN
         SORTED = .TRUE.
         IMAX = IMAX - 1

*  Bubble sort: swap adjacent out-of-order elements until no more swaps
*  are needed.
         DO 4 I = 1, IMAX
            IF ( ARG( I + 1 ) .LT. ARG( I ) ) THEN
               NAME = ARG( I )
               ARG( I ) = ARG( I + 1 )
               ARG( I + 1 ) = NAME
               SORTED = .FALSE.
            END IF
4        CONTINUE
         GO TO 3
      END IF

*  Remove duplicate entries from the argument list, deriving the number
*  of distinct argument names.
      NDIFF = MIN( 1, NARG )
      DO 5 I = 2, NARG
         IF ( ARG( I ) .NE. ARG( NDIFF ) ) THEN
            NDIFF = NDIFF + 1
            ARG( NDIFF ) = ARG( I )
         END IF
5     CONTINUE

*  Write a parameter definition for each distinct argument.
      IF ( NDIFF .GT. 0 ) THEN
         CALL SST_PUT( 0, '!  Define the parameters.', STATUS )
         DO 7 I = 1, NDIFF
            NC = MAX( 1, CHR_LEN( ARG( I ) ) )
            CALL SST_PUT( 0, 'DEFINE PARAMETER /PACKAGE = "' //
     :                        PACK( : NCPACK ) // '" ' //
     :                        ARG( I )( : NC ), STATUS )
7        CONTINUE
      END IF

*  Arrive here when all the processing is complete, or earlier if an
*  error occurred.
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
     :   '   LSE package definition written to file ^OUTFILE',
     :   STATUS )
         CALL MSG_OUT( 'BLANK', ' ', STATUS )
      END IF

*  If an error occurred, then report contextual information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PROPAK_ERR',
     :   'PROPAK: Error converting routine prologue information ' //
     :   'into an LSE package definition.', STATUS )
      END IF

      END
* @(#)propak.f   1.3   94/12/05 15:52:41   96/07/05 10:27:41
