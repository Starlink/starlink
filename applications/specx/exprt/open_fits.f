      SUBROUTINE SPECX_OPEN_FITS( IFAIL )
*+
*  Name:
*     SPECX_OPEN_FITS

*  Purpose:
*     Open a disk-FITS file for output.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPECX_OPEN_FITS( IFAIL )

*  Description:
*     This routine serves the Specx command OPEN-FITS-FILE to open a new
*     disk file for FITS output. Tape units are not supported.
*
*     It just so happens, that the routines FIT_DFOPEN and FIT_DINIT can
*     also be used for existing files. That is why the OPEN-FITS-FILE
*     command can also be used prior to reading disk-FITS.

*  Arguments:
*     IFAIL = INTEGER (Returned)
*        The global status. The status is reset on entry.

*  Authors:
*     rp: Rachael Padman (UCB, MRAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     rpt: Remo Tilanus (JAC, Hilo)
*     {enter_new_authors_here}

*  History:
*     {date} (rp):
*        Original version.
*     22 Nov 1993 (hme):
*        Replace LIB$GET_LUN with FIO_GUNIT, STR$UPCASE with CHR_UCASE.
*     09 Jan 1994 (rp):
*        Replace FIO_GUNIT with IGETLUN
*     15 Jan 1994 (rp):
*        Replace CHR_UCASE with UUCASE
*     27 Jan 1994 (hme):
*        Review to support only disk-FITS and to maximise use of FITS
*        related libraries in Portable Figaro.
*     03 May 1999 (rpt):
*        Since Figaro GEN routines for Byte-swap are now tied to
*        host type, byte swap can no longer be an option.
*     22 Aug 2005 (timj):
*        Initialise variables using data statements
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:

*  Global Variables:
      INCLUDE 'SPECX_FITS'

*  Status:
      INTEGER IFAIL              ! Global status

*  Local Variables:
      INTEGER ISTAT
      INTEGER STATUS
      CHARACTER * ( 64 ) ERROR
      CHARACTER * ( 16 ) FPROMPT
      CHARACTER * ( 80 ) FILENAME

      DATA ERROR / ' ' /
      DATA FPROMPT / ' ' /
      DATA FILENAME / ' ' /

*  Internal References:
      INTEGER GEN_ILEN
      INTEGER IGETLUN

*.

*  Reset inherited status.

      IFAIL = 0

*  FITS output already open?

      IF ( FITS_OPEN ) THEN
         IFAIL = 112
         RETURN
      END IF

*  Make the "choice" between disk and tape.

      DISKFITS = .TRUE.

*  Get a filename

      IF (FILENAME .EQ. ' ') FILENAME = 'fitsfile.fit'
      WRITE (FPROMPT, '(''A'',I3.3)') GEN_ILEN(FILENAME)
      CALL GEN_GETSTR( 'Filename for FITS output file?',
     :   FILENAME, FPROMPT, FILENAME, ISTAT )

*  If filename still blank, invent something

      IF( GEN_ILEN(FILENAME) .LE. 0 ) THEN
         NFILE    = NFILE + 1
         FILENAME = 'specxscan_????.fit'
         WRITE( FILENAME(11:14), '(I4.4)' ) NFILE
         WRITE( *, * ) 'Output filename = ',
     :      FILENAME(:GEN_ILEN(FILENAME))
      END IF

*  Open the FITS disk file...

      IFAIL = IGETLUN( LU, 'specx_open_fits', .FALSE. )
      IF ( IFAIL .NE. 0 ) RETURN

      STATUS = 0
      CALL FIT_DFOPEN( LU, FILENAME, .TRUE., STATUS )
      IF ( STATUS .NE. 0 ) THEN
         WRITE( *, * ) 'Trouble opening disk file: '
         GO TO 500
      END IF

      BYTESWAP = .TRUE.
*       CALL GEN_YESNO( 'Should disk file be byte-reversed?',
*     :   BYTESWAP, BYTESWAP, ISTAT )
      CALL FIT_DINIT( LU, BYTESWAP, STATUS )
      IF ( STATUS .NE. 0 ) GO TO 500
      FITS_OPEN = .TRUE.

*  Normal exit.

      RETURN

*  Error exit.

 500  CONTINUE
      FITS_OPEN = .FALSE.
      CALL FIT_ERROR( STATUS, ERROR )
      WRITE( 6, * ) ERROR

      END
