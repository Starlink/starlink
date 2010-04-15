      SUBROUTINE PSA1_CCATS( CI, FI, STATUS )
*+
*  Name:
*     PSA1_CCATS

*  Purpose:
*     Setup a catalogue for a PISAFIND SIZE file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PSA1_CCATS( Ci, FI, STATUS )

*  Description:
*     The routine sets up the parameters and fields required for a
*     pisasize data file and copies the data into place. The
*     catalogue must have been already created.

*  Arguments:
*     CI = INTEGER (Given)
*        The catalogue identifier.
*     FI = INTEGER (Given)
*         FIO identifier of the file containing the ASCII
*         SIZEs results.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-NOV-1995 (PDRAPER):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'CAT_PAR'         ! CAT parameters
      INCLUDE 'FIO_ERR'         ! FIO parameters

*  Arguments Given:
      INTEGER CI
      INTEGER FI

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 200 ) BUF   ! Input line buffer
      INTEGER IVALS( 9 )        ! Integer values
      REAL RVALS( 9 )           ! Real values
      INTEGER INDEX, A1, A2, A3, A4, A5, A6, A7, A8 ! Column IDs
      INTEGER QIP               ! Parameter identifier
      LOGICAL NULL              ! Value is null

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
      CALL CAT_PNEW0( CI, CAT__FITYP, 'A1', CAT__TYPER, A1,
     :                STATUS )
      CALL CAT_PNEW0( CI, CAT__FITYP, 'A2', CAT__TYPER, A2,
     :                STATUS )
      CALL CAT_PNEW0( CI, CAT__FITYP, 'A3', CAT__TYPER, A3,
     :                STATUS )
      CALL CAT_PNEW0( CI, CAT__FITYP, 'A4', CAT__TYPER, A4,
     :                STATUS )
      CALL CAT_PNEW0( CI, CAT__FITYP, 'A5', CAT__TYPER, A5,
     :                STATUS )
      CALL CAT_PNEW0( CI, CAT__FITYP, 'A6', CAT__TYPER, A6,
     :                STATUS )
      CALL CAT_PNEW0( CI, CAT__FITYP, 'A7', CAT__TYPER, A7,
     :                STATUS )
      CALL CAT_PNEW0( CI, CAT__FITYP, 'A8', CAT__TYPER, A8,
     :                STATUS )

*  Formats.
      CALL CAT_TATTC( INDEX, 'EXFMT', 'I7', STATUS )
      CALL CAT_TATTC( A1, 'EXFMT', 'F10.0', STATUS )
      CALL CAT_TATTC( A2, 'EXFMT', 'F10.0', STATUS )
      CALL CAT_TATTC( A3, 'EXFMT', 'F10.0', STATUS )
      CALL CAT_TATTC( A4, 'EXFMT', 'F10.0', STATUS )
      CALL CAT_TATTC( A5, 'EXFMT', 'F10.0', STATUS )
      CALL CAT_TATTC( A6, 'EXFMT', 'F10.0', STATUS )
      CALL CAT_TATTC( A7, 'EXFMT', 'F10.0', STATUS )
      CALL CAT_TATTC( A8, 'EXFMT', 'F10.0', STATUS )


*  Units.
      CALL CAT_TATTC( A1, 'UNITS', 'PIXELS', STATUS )
      CALL CAT_TATTC( A2, 'UNITS', 'PIXELS', STATUS )
      CALL CAT_TATTC( A3, 'UNITS', 'PIXELS', STATUS )
      CALL CAT_TATTC( A4, 'UNITS', 'PIXELS', STATUS )
      CALL CAT_TATTC( A5, 'UNITS', 'PIXELS', STATUS )
      CALL CAT_TATTC( A6, 'UNITS', 'PIXELS', STATUS )
      CALL CAT_TATTC( A7, 'UNITS', 'PIXELS', STATUS )
      CALL CAT_TATTC( A8, 'UNITS', 'PIXELS', STATUS )

*  Comments.
      CALL CAT_TATTC( INDEX, 'COMM', 'INDEX NUMBER OF OBJECT', STATUS )
      CALL CAT_TATTC( A1, 'COMM',  'NUMBER OF PIXELS IN 1ST THRESHOLD',
     :                STATUS )
      CALL CAT_TATTC( A2, 'COMM', 'NUMBER OF PIXELS IN 2ND THRESHOLD',
     :                STATUS )
      CALL CAT_TATTC( A3, 'COMM', 'NUMBER OF PIXELS IN 3RD THRESHOLD',
     :                STATUS )
      CALL CAT_TATTC( A4, 'COMM', 'NUMBER OF PIXELS IN 4TH THRESHOLD',
     :                STATUS )
      CALL CAT_TATTC( A5, 'COMM', 'NUMBER OF PIXELS IN 5TH THRESHOLD',
     :                STATUS )
      CALL CAT_TATTC( A6, 'COMM', 'NUMBER OF PIXELS IN 6TH THRESHOLD',
     :                STATUS )
      CALL CAT_TATTC( A7, 'COMM', 'NUMBER OF PIXELS IN 7TH THRESHOLD',
     :                STATUS )
      CALL CAT_TATTC( A8, 'COMM', 'NUMBER OF PIXELS IN 8TH THRESHOLD',
     :                STATUS )

*  Enter data.
      CALL ERR_MARK
 1    CONTINUE
         CALL RDPISD( FI, BUF, IVALS( 1 ), RVALS( 2 ), RVALS( 3 ),
     :                RVALS( 4 ), RVALS( 5 ), RVALS( 6 ), RVALS( 7 ),
     :                RVALS( 8 ), RVALS( 9 ), STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 2

         CALL CAT_PUT0I( INDEX, IVALS( 1 ), .FALSE., STATUS )

         IF ( NINT( RVALS( 2 ) ) .EQ. -1 ) THEN
            NULL = .TRUE.
         ELSE
            NULL = .FALSE.
         END IF
         CALL CAT_PUT0R( A1, RVALS( 2 ), NULL, STATUS )

         IF ( NINT( RVALS( 3 ) ) .EQ. -1 ) THEN
            NULL = .TRUE.
         ELSE
            NULL = .FALSE.
         END IF
         CALL CAT_PUT0R( A2, RVALS( 3 ), NULL, STATUS )

         IF ( NINT( RVALS( 4 ) ) .EQ. -1 ) THEN
            NULL = .TRUE.
         ELSE
            NULL = .FALSE.
         END IF
         CALL CAT_PUT0R( A3, RVALS( 4 ), NULL, STATUS )

         IF ( NINT( RVALS( 5 ) ) .EQ. -1 ) THEN
            NULL = .TRUE.
         ELSE
            NULL = .FALSE.
         END IF
         CALL CAT_PUT0R( A4, RVALS( 5 ), NULL, STATUS )

         IF ( NINT( RVALS( 6 ) ) .EQ. -1 ) THEN
            NULL = .TRUE.
         ELSE
            NULL = .FALSE.
         END IF
         CALL CAT_PUT0R( A5, RVALS( 6 ), NULL, STATUS )

         IF ( NINT( RVALS( 7 ) ) .EQ. -1 ) THEN
            NULL = .TRUE.
         ELSE
            NULL = .FALSE.
         END IF
         CALL CAT_PUT0R( A6, RVALS( 7 ), NULL, STATUS )

         IF ( NINT( RVALS( 8 ) ) .EQ. -1 ) THEN
            NULL = .TRUE.
         ELSE
            NULL = .FALSE.
         END IF
         CALL CAT_PUT0R( A7, RVALS( 8 ), NULL, STATUS )

         IF ( NINT( RVALS( 9 ) ) .EQ. -1 ) THEN
            NULL = .TRUE.
         ELSE
            NULL = .FALSE.
         END IF
         CALL CAT_PUT0R( A8, RVALS( 9 ), NULL, STATUS )

*  Append row.
         CALL CAT_RAPND( CI, STATUS )
      GO TO 1
 2    CONTINUE

* may have status other than end-of-file, check for this
      IF ( STATUS .EQ. FIO__EOF ) THEN
         CALL ERR_ANNUL( STATUS )
      ENDIF
      CALL ERR_RLSE
      END
