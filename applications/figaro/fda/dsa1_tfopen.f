      SUBROUTINE DSA1_TFOPEN( LU, SYMDIR, FILE, DEFEXT, NEW, WRITE,
     :   EXIST, ERROR, STATUS )
*+
*  Name:
*     DSA1_TFOPEN

*  Purpose:
*     Perform part of opening a text file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA1_TFOPEN( LU, SYMDIR, FILE, DEFEXT, NEW, WRITE,
*        EXIST, ERROR, STATUS )

*  Description:
*     This routine will try to open either a new or existing text file.
*     It is passed a 'directory specification', which is a way of
*     specifying symbolically the name of a directory that the file may
*     be found in. This is intended to be something like 'FIGARO_PROG_L'
*     and under Unix will be an environment variable. This can be
*     blank. The special case where this is the single character '$' is
*     taken as a reference to the directory from which the current
*     program is being run. Since on Unix this concept cannot be
*     implemented this routine will simply not find an existing file in
*     this 'execute' directory and will not be able to create a new file
*     in it. The routine is also passed a file name, which may be a
*     completely specified file name, complete with directory
*     specification, file extension and version number (where
*     supported), or can be just the bare filename. It is also passed a
*     default file extension for use if this is not provided in the
*     filename. (If the filename appears to contain an extension, i.e. a
*     dot within the body of the filename, then the default extension
*     will be ignored, so a filename 'file.ext' with a default extension
*     of '.ext' will not be taken as the file 'file.ext.ext', even on a
*     system where this would be a valid name.) The default extension
*     should begin with a dot character, if specified. If it is blank,
*     no extension is assumed by this program. This routine respects the
*     case of all these strings and does not attempt to change them;
*     whether the underlying operating system respects filename case is
*     not a question addressed by this routine.  A file will be opened
*     in the most suitable way for a text file. The file can be
*     specified as 'new' or 'not new' - i.e. existing; a new file will
*     always be opened for writing. An existing file can be opened for
*     writing or not. Opening a 'new' file when a file of the same name
*     exists will either create a new version of the file or will
*     replace the existing file - this depends on the operating
*     system. Note that the symbolic directory is always used if it is
*     non-blank; usually this program will be called several times as a
*     file is searched for, with various symbols for the directory, and
*     in one of these cases the symbolic directory will normally be
*     blank to allow the case where the filename itself contains the
*     directory specification to succeed.

*  Arguments:
*     LU = INTEGER (Given)
*        The number of the Fortran logical unit to be used to access the
*        file.
*     SYMDIR = CHARACTER * ( * ) (Given)
*        The symbolic name (environment variable) for the directory.
*     FILE = CHARACTER * ( * ) (Given)
*        The name ofthe file to be opened, either bare or with an
*        explicit directory specification and/or extension.
*     DEFEXT = CHARACTER * ( * ) (Given)
*        A string specifying the default extension for the file,
*        including the leading dot.
*     NEW = LOGICAL (Given)
*        True if a new file is to be created.
*     WRITE = LOGICAL (Given)
*        True if an existing file is to be opened for writing.
*     EXIST = LOGICAL (Returned)
*        Independent of whether or not the file was opened, this
*        indicates whether it existed or not.
*     ERROR = CHARACTER * ( * ) (Returned)
*        A string describing any error.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This routine does not generate an error
*        report on its own. Instead it may return a non-zero status and
*        an error string.

*  Notes:
*     This routine assumes a Unix file system.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     tdca: Tim Ash (RAL, Starlink)
*     {enter_new_authors_here}

*  History:
*     24 Aug 1992 (ks):
*        Original version.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     19 Oct 1992 (hme):
*        Create different files for mips and sun4. Avoids preprocessor.
*     08 Apr 1993 (hme):
*        This IS the mips version, using READONLY. It was used until now
*        on sun4 instead of mips. Avoid INQUIRE.
*     12 Apr 1993 (hme):
*        Alpha version; use INQUIRE
*     20 Dec 1995 (hme):
*        FDA library.
*     19 Feb 1996 (hme):
*        Add ENTRY DSAZ_TFOPEN for the benefit of FIG_OPFILE.
*        Use application-side status in the interface of this routine.
*     09 Jun 1999 (tdca):
*        Removed unsupported keywords from OPEN statement.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

      ENTRY DSAZ_TFOPEN( LU, SYMDIR, FILE, DEFEXT, NEW, WRITE,
     :   EXIST, ERROR, STATUS )

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER LU
      CHARACTER * ( * ) SYMDIR
      CHARACTER * ( * ) FILE
      CHARACTER * ( * ) DEFEXT
      LOGICAL NEW
      LOGICAL WRITE

*  Arguments Returned:
      LOGICAL EXIST
      CHARACTER * ( * ) ERROR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL FAULT              ! Memorise whether something went wrong
      LOGICAL EXT_GIVEN          ! True if extension specified in FILE
      INTEGER ISTAT2             ! Copy of status
      INTEGER ISTAT              ! Status of INQUIRE and OPEN
      INTEGER I                  ! Loop index through FILE chars
      INTEGER FLEN               ! Number of characters in FILE
      INTEGER SLEN               ! Number of characters in SYMDIR
      INTEGER TLEN               ! Number of characters in TRAN_NAME
      INTEGER DLEN               ! Number of characters in DEFEXT
      CHARACTER * ( 256 ) FULNAM ! Name of file to be opened
      CHARACTER * ( 256 ) TRAN_NAME ! Translated version of SYMDIR

*  Internal References:
      INTEGER CHR_LEN            ! Used length of a string

*.

*  Format for internal write of INQUIRE or OPEN.
 101  FORMAT( 'A Fortran error occured: IOSTAT = ', I8 )

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN
      FAULT = .FALSE.

*  Under Unix we don't handle the special case where '$' is used for the
*  symbolic directory.
      IF ( SYMDIR .EQ. '$' ) THEN
         ERROR = '"Execution directory" not available under UNIX'
         FAULT = .TRUE.
         EXIST = .FALSE.
         GO TO 500
      END IF


*  Generate the filename to use.
*  =============================

*  This involves looking at FILE and prepending the translated symbolic
*  directory name (allowing for a '/' if this seems necessary). If FILE
*  seems not to contain an extension already, we append the specified
*  default.

*  Start by looking back through FILE to see if there is a '.' in the
*  last part of the name - up to the last '/'.
      FLEN = CHR_LEN( FILE )
      EXT_GIVEN = .FALSE.
      DO 1 I = FLEN, 1 , -1
         IF ( FILE(I:I) .EQ. '/' ) GO TO 2
         IF ( FILE(I:I) .EQ. '.' ) THEN
            EXT_GIVEN = .TRUE.
            GO TO 2
         END IF
 1    CONTINUE
 2    CONTINUE

*  Now, if the symbolic directory name is not blank, treat it as an
*  environment variable and translate it. If it doesn't translate treat
*  this as an error. Prepend the result to FILE, inserting a '/' if the
*  translation doesn't end with this.
      FULNAM = FILE
      SLEN = CHR_LEN( SYMDIR )
      IF ( SLEN .GT. 0 ) THEN
         ISTAT2 = STATUS
         STATUS = SAI__OK
         CALL PSX_GETENV( SYMDIR, TRAN_NAME, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            EXIST = .FALSE.
            ERROR = 'Cannot translate "' // SYMDIR(:SLEN) // '"'
            CALL ERR_ANNUL( STATUS )
            FAULT = .TRUE.
            GO TO 500
         END IF
         STATUS = ISTAT2
         TLEN = CHR_LEN( TRAN_NAME )
         IF ( TLEN .GT. 0 ) THEN
            IF ( TRAN_NAME(TLEN:TLEN) .EQ. '/' ) THEN
               FULNAM = TRAN_NAME(:TLEN) // FILE
               FLEN = FLEN + TLEN
            ELSE
               FULNAM = TRAN_NAME(:TLEN) // '/' // FILE
               FLEN = FLEN + TLEN + 1
            END IF
         END IF
      END IF

*  Finally, append the extension, if one was given in DEFEXT and no
*  extension seemed to be specified in FILE.
      DLEN = CHR_LEN( DEFEXT )
      IF ( DLEN .GT. 0 .AND. .NOT. EXT_GIVEN ) THEN
         FULNAM(FLEN+1:) = DEFEXT
         FLEN = FLEN + DLEN
      END IF


*  Open the file.
*  ==============

*  First, see if the file exists - we need to know this to report using
*  EXIST, even if a new version is to be created.
      INQUIRE( FILE=FULNAM, EXIST=EXIST, IOSTAT=ISTAT )
      IF ( ISTAT .NE. 0 ) EXIST = .FALSE.

*  If the file is to be created.
      IF ( NEW ) THEN

*     See if we can create it.
*     Under Unix, an open using 'NEW' will fail if a file of the same
*     name already exists.
         IF ( EXIST ) THEN
            OPEN( UNIT=LU, FILE=FULNAM, STATUS='OLD', IOSTAT=ISTAT )
         ELSE
            OPEN( UNIT=LU, FILE=FULNAM, STATUS='NEW', IOSTAT=ISTAT )
         END IF
         IF ( ISTAT .NE. 0 ) THEN
            FAULT = .TRUE.
            WRITE( ERROR, 101 ) ISTAT
         END IF

*  Else (the file should already exist).
      ELSE

*     See if it did - this assumes that this routine will be called more
*     frequently for a file that doesn't exist than for one that does,
*     and that the INQUIRE is faster than a tentative OPEN. (Otherwise,
*     we'd only do the INQUIRE for the 'new' case and just use an OPEN
*     here.) If it does exist, try to open it.
*     Unless write access was requested, the file is opened read-only.
*     Some Fortran implementations may throw up a warning on the
*     READONLY keyword, but it is in the Fortran standard.
         IF ( EXIST ) THEN
            IF ( WRITE ) THEN
               OPEN( UNIT=LU, FILE=FULNAM, STATUS='OLD', IOSTAT=ISTAT )
            ELSE
               OPEN( UNIT=LU, FILE=FULNAM, STATUS='OLD',
     :            IOSTAT=ISTAT )
            END IF
            IF ( ISTAT .NE. 0 ) THEN
               FAULT = .TRUE.
               WRITE( ERROR, 101 ) ISTAT
            END IF
         ELSE
            FAULT = .TRUE.
            ERROR = 'File does not exist'
         END IF

      END IF


*  Return.
*  =======

 500  CONTINUE
      IF ( FAULT ) STATUS = 1
      END
