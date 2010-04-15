      SUBROUTINE PSA1_CCATF( CI, FI, STATUS )
*+
*  Name:
*     PSA1_CCATF

*  Purpose:
*     To create a PISAFIND type catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PSA1_CCATF( NAME, STATUS )

*  Description:
*     The routine sets up the parameters and fields required for a
*     pisafind data file and the copys the data from the file
*     into the catalogue.

*  Arguments:
*     CI = INTEGER (Given)
*        The catalogue identifier.
*     FI = INTEGER (Given)
*        The PISAFIND results file (FIO) identfier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-NOV-1995 (PDRAPER):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'CAT_PAR'         ! CAT constants
      INCLUDE 'FIO_ERR'         ! FIO constants

*  Arguments Given:
      INTEGER CI
      INTEGER FI

*  Status:
      INTEGER STATUS            ! Global status

*  Local Constants:
      INTEGER NFLD              ! Number of fields
      PARAMETER ( NFLD = 11 )

*  Local Variables:
      CHARACTER * ( 200 ) BUF   ! Input line buffer
      INTEGER IVALS( NFLD )     ! Integer values
      REAL RVALS( NFLD )        ! Real values
      INTEGER INDEX, XPOS, YPOS, ! Column identifiers
     :        INT, NPIX, PEAK,
     :        ELLIPT, ANGLE, SXX,
     :        SYY, SXY
      INTEGER QIP               ! Parameter identifier

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Add some parameters.
      CALL CAT_PNEW0( CI, CAT__QITYP, 'CATSOURCE', CAT__TYPEC, QIP,
     :                STATUS )
      CALL CAT_TATTI( QIP, 'CSIZE', 4, STATUS )
      CALL CAT_TATTC( QIP, 'VALUE', 'PISA', STATUS )
      CALL CAT_TATTC( QIP, 'COMM', 'Catalogue source', STATUS )
      CALL CAT_PNEW0( CI, CAT__QITYP, 'KEYFIELD', CAT__TYPEC, QIP,
     :                STATUS )
      CALL CAT_TATTI( QIP, 'CSIZE', 5, STATUS )
      CALL CAT_TATTC( QIP, 'VALUE', 'INDEX', STATUS )
      CALL CAT_TATTC( QIP, 'COMM', 'INDEX - ASCENDING', STATUS )

*  Catalogue columns.
      CALL CAT_PNEW0( CI, CAT__FITYP, 'INDEX', CAT__TYPEI, INDEX,
     :                STATUS )
      CALL CAT_PNEW0( CI, CAT__FITYP, 'XPOS', CAT__TYPER, XPOS,
     :                STATUS )
      CALL CAT_PNEW0( CI, CAT__FITYP, 'YPOS', CAT__TYPER, YPOS,
     :                STATUS )
      CALL CAT_PNEW0( CI, CAT__FITYP, 'INTENSITY', CAT__TYPER, INT,
     :                STATUS )
      CALL CAT_PNEW0( CI, CAT__FITYP, 'NPIX', CAT__TYPEI, NPIX,
     :                STATUS )
      CALL CAT_PNEW0( CI, CAT__FITYP, 'PEAK', CAT__TYPER, PEAK,
     :                STATUS )
      CALL CAT_PNEW0( CI, CAT__FITYP, 'ELLIPT', CAT__TYPER, ELLIPT,
     :                STATUS )
      CALL CAT_PNEW0( CI, CAT__FITYP, 'ANGLE', CAT__TYPER, ANGLE,
     :                STATUS )
      CALL CAT_PNEW0( CI, CAT__FITYP, 'SXX', CAT__TYPER, SXX,
     :                STATUS )
      CALL CAT_PNEW0( CI, CAT__FITYP, 'SYY', CAT__TYPER, SYY,
     :                STATUS )
      CALL CAT_PNEW0( CI, CAT__FITYP, 'SXY', CAT__TYPER, SXY,
     :                STATUS )

*  Add the output field formats.
      CALL CAT_TATTC( INDEX, 'EXFMT', 'I7', STATUS )
      CALL CAT_TATTC( XPOS, 'EXFMT', 'F10.3', STATUS )
      CALL CAT_TATTC( YPOS, 'EXFMT', 'F10.3', STATUS )
      CALL CAT_TATTC( INT, 'EXFMT', 'F10.0', STATUS )
      CALL CAT_TATTC( NPIX, 'EXFMT', 'I7', STATUS )
      CALL CAT_TATTC( PEAK, 'EXFMT', 'F10.0', STATUS )
      CALL CAT_TATTC( ELLIPT, 'EXFMT', 'F8.3', STATUS )
      CALL CAT_TATTC( ANGLE, 'EXFMT', 'F8.2', STATUS )
      CALL CAT_TATTC( SXX, 'EXFMT', 'F8.2', STATUS )
      CALL CAT_TATTC( SYY, 'EXFMT', 'F8.2', STATUS )
      CALL CAT_TATTC( SXY, 'EXFMT', 'F8.2', STATUS )

*  The column units.
      CALL CAT_TATTC( XPOS, 'UNITS', 'PIXELS', STATUS )
      CALL CAT_TATTC( YPOS, 'UNITS', 'PIXELS', STATUS )
      CALL CAT_TATTC( INT, 'UNITS', 'INTENSITY', STATUS )
      CALL CAT_TATTC( NPIX, 'UNITS', 'PIXELS', STATUS )
      CALL CAT_TATTC( PEAK, 'UNITS', 'INTENSITY', STATUS )
      CALL CAT_TATTC( ANGLE, 'UNITS', 'DEGREES', STATUS )

*  Column comments.
      CALL CAT_TATTC( INDEX, 'COMM', 'INDEX NUMBER OF OBJECT', STATUS )
      CALL CAT_TATTC( XPOS, 'COMM', 'X COORDINATE', STATUS )
      CALL CAT_TATTC( YPOS, 'COMM', 'Y COORDINATE', STATUS )
      CALL CAT_TATTC( INT, 'COMM', 'TOTAL INTENSITY OF OBJECT', STATUS )
      CALL CAT_TATTC( NPIX, 'COMM', 'NUMBER OF PIXELS ABOVE THRESHOLD',
     :                STATUS )
      CALL CAT_TATTC( PEAK, 'COMM', 'PEAK INTENSITY', STATUS )
      CALL CAT_TATTC( ELLIPT, 'COMM', 'ELLIPTICITY', STATUS )
      CALL CAT_TATTC( ANGLE, 'COMM',
     :                'ORIENTATION - ANTI CLOCKWISE FROM Y-AXIS',
     :                STATUS )
      CALL CAT_TATTC( SXX, 'COMM', 'SECOND ORDER MOMENT 2,0', STATUS )
      CALL CAT_TATTC( SYY, 'COMM', 'SECOND ORDER MOMENT 0,2', STATUS )
      CALL CAT_TATTC( SXY, 'COMM', 'SECOND ORDER MOMENT 1,1', STATUS )

*  Indicate INDEX is ascending.
      CALL CAT_TATTI( INDEX, 'ORDER', CAT__ASCND, STATUS )

*  Now add the values to the fields.
*  Read the data - one line at a time from the PISAFIND data file
*  read in the data from this file until EOF is reached.
      CALL ERR_MARK
 1    CONTINUE
         CALL RDPIFD( FI, BUF, IVALS( 1 ), RVALS( 2 ), RVALS( 3 ),
     :                RVALS( 4 ), IVALS( 5 ), RVALS( 6 ), RVALS( 7 ),
     :                RVALS( 8 ), RVALS( 9 ), RVALS( 10 ), RVALS( 11 ),
     :                STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 2

*  Transfer the data.
         CALL CAT_PUT0I( INDEX, IVALS( 1 ), .FALSE., STATUS )
         CALL CAT_PUT0R( XPOS, RVALS( 2 ), .FALSE., STATUS )
         CALL CAT_PUT0R( YPOS, RVALS( 3 ), .FALSE., STATUS )
         CALL CAT_PUT0R( INT, RVALS( 4 ), .FALSE., STATUS )
         CALL CAT_PUT0I( NPIX, IVALS( 5 ), .FALSE., STATUS )
         CALL CAT_PUT0R( PEAK, RVALS( 6 ), .FALSE., STATUS )
         CALL CAT_PUT0R( ELLIPT, RVALS( 7 ), .FALSE., STATUS )
         CALL CAT_PUT0R( ANGLE, RVALS( 8 ), .FALSE., STATUS )
         CALL CAT_PUT0R( SXX, RVALS( 9 ), .FALSE., STATUS )
         CALL CAT_PUT0R( SYY, RVALS( 10 ), .FALSE., STATUS )
         CALL CAT_PUT0R( SXY, RVALS( 11 ), .FALSE., STATUS )
         CALL CAT_RAPND( CI, STATUS )
      GO TO 1
 2    CONTINUE

* may have status other than end-of-file, check for this
      IF ( STATUS .EQ. FIO__EOF ) THEN
         CALL ERR_ANNUL( STATUS )
      ENDIF
      CALL ERR_RLSE
      END
