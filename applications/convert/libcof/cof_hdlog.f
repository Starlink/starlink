      SUBROUTINE COF_HDLOG( FUNIT, FDL, FILE, CHDU, STATUS )
*+
*  Name:
*     COF_HDLOG

*  Purpose:
*     Writes the FITS headers of the current header and data unit to a
*     logfile.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_HDLOG( FUNIT, FDL, FILE, CHDU, STATUS )

*  Description:
*     This writes the FITS headers of the current header and data unit
*     to a previously opened text file.  The logfile is not closed by
*     this routine.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The FITSIO unit number for the FITS file.
*     FDL = INTEGER (Given)
*        The file descriptor for the logfile.
*     FILE = CHARACTER * ( * ) (Given)
*        The name of the FITS file or tape device to appear in the
*        heading of the log.
*     CHDU = INTEGER (Given)
*        The number of the current header and data unit of the FITS
*        file, which will appear in the heading of the log.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     Both the FITS and log files must already be opened, the former
*     with the FITSIO library.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 January 19 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER FUNIT
      INTEGER FDL
      CHARACTER * ( * ) FILE
      INTEGER CHDU

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a string less trailing
                                 ! blanks

*  Local Constants:
      INTEGER FITSOK             ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

      INTEGER HEDLEN             ! FITS header length
      PARAMETER( HEDLEN = 80 )

      INTEGER MAXLEN             ! Maximum logfile record length
      PARAMETER( MAXLEN = 80 )

*  Local Variables:
      CHARACTER * ( 256 ) BUFFER ! Used to form error messages
      CHARACTER * ( MAXLEN ) DUMMY ! Output record
      INTEGER FSTAT              ! FITSIO status
      CHARACTER * ( HEDLEN ) HEADER ! A FITS header
      INTEGER KEYADD             ! Number of headers that can be added
      INTEGER IHEAD              ! Loop counter for headers
      INTEGER LUNIT              ! Logical unit number of logfile
      INTEGER MXFILN             ! Maximum number of characters in the
                                 ! file name that can be displayed
      INTEGER NCF                ! Number of characters in the FITS file
                                 ! name
      INTEGER NCO                ! Number of characters in the output
      INTEGER NHEAD              ! Number of FITS headers
      INTEGER RECLEN             ! Record length of logfile


*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the length of the filename.
      NCF = CHR_LEN( FILE )

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Find the number of headers.
      CALL FTGHSP( FUNIT, NHEAD, KEYADD, FSTAT )
      IF ( FSTAT .NE. FITSOK ) THEN
         BUFFER = 'Error obtaining the number of header cards from '/
     :            /'FITS file '//FILE( :NCF )//'.'

         CALL COF_FIOER( FSTAT, 'COF_HDLOG_NHEAD', 'FTGHSP',
     :                   BUFFER, STATUS )
         GOTO 999
      END IF

*  The END card is required too.  FTGHSP excludes it from its count.
      NHEAD = NHEAD + 1

*  Write blank line.
      CALL FIO_WRITE( FDL, ' ', STATUS )

*  Get the record length of the file.
      CALL FIO_UNIT( FDL, LUNIT, STATUS )
      INQUIRE( UNIT = LUNIT, RECL = RECLEN )

*  On some operating systems such as Solaris the record length is
*  returned as zero.  Supply a sensible default in this case.
      IF ( RECLEN .EQ. 0 ) RECLEN = 80

*  Find the maximum number of characters that can be output in the
*  caption.
      MXFILN = MIN( MAXLEN, RECLEN ) - 30

*  Add ellipsis where truncated.  Write caption to buffer, truncating
*  the filename.
      IF ( NCF .GT. MXFILN ) THEN
         WRITE( DUMMY, '(''** File: ...'',A,'' ('',I3,'' ) header '/
     :     /'cards'')' ) FILE( MAX( 1, NCF - MXFILN + 3 ):NCF ), CHDU

         NCO = MXFILN + 30

*  Write the caption using the full file name.
      ELSE
         WRITE( DUMMY, '(''** File: '',A,'' ('',I3,'' ) header '/
     :     /'cards'')' ) FILE( 1:NCF ), CHDU

         NCO = NCF + 30
      END IF

      CALL FIO_WRITE( FDL, DUMMY( :NCO ), STATUS )

*  Loop through the headers.
      DO IHEAD = 1, NHEAD

*  Obtain the header.
         CALL FTGREC( FUNIT, IHEAD, HEADER, FSTAT )
         IF ( FSTAT .NE. FITSOK ) THEN
            CALL MSG_SETI( 'NH', NHEAD )
            CALL MSG_SETI( 'IH', IHEAD )
            CALL MSG_SETC( 'FILE', FILE( :NCF ) )
            CALL COF_FIOER( FSTAT, 'COF_HDLOG_GHEAD', 'FTGREC',
     :        'Error obtaining a FITS header (^IH of ^NH) from '/
     :        /'FITS file ^FILE.', STATUS )
            GOTO 999
         END IF

*  Print the header to the logfile.
         CALL FIO_WRITE( FDL, HEADER( :HEDLEN ), STATUS )

*  Write the header to the FITS extension.
      END DO

  999 CONTINUE

      END
