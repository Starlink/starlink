      SUBROUTINE PSA1_CCATP( CI, FI, STATUS )
*+
*  Name:
*     PSA1_CCATP

*  Purpose:
*     Set up a PISAPEAK type catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PSA1_CCATP( CI, FI, STATUS )

*  Description:
*     The routine sets up the parameters and fields required for a
*     PISAPEAK data file and copies the data into place. The
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
      INTEGER INDEX, RRATIO, IRATIO, ELLIP, ABSSXY ! Column IDs
      INTEGER IVALS( 5 )        ! Integer values
      INTEGER QIP               ! Parameter identifier
      REAL RVALS( 5 )           ! Real values

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

*  Catalogue columns
      CALL CAT_PNEW0( CI, CAT__FITYP, 'INDEX', CAT__TYPEI, INDEX,
     :                STATUS )
      CALL CAT_PNEW0( CI, CAT__FITYP, 'RRATIO', CAT__TYPER, RRATIO,
     :                STATUS )
      CALL CAT_PNEW0( CI, CAT__FITYP, 'IRATIO', CAT__TYPER, IRATIO,
     :                STATUS )
      CALL CAT_PNEW0( CI, CAT__FITYP, 'ELLIP', CAT__TYPER, ELLIP,
     :                STATUS )
      CALL CAT_PNEW0( CI, CAT__FITYP, 'ABSSXY', CAT__TYPER, ABSSXY,
     :                STATUS )

*  Formats.
      CALL CAT_TATTC( INDEX, 'EXFMT', 'I7', STATUS )
      CALL CAT_TATTC( RRATIO, 'EXFMT', 'F10.3', STATUS )
      CALL CAT_TATTC( IRATIO, 'EXFMT', 'F10.3', STATUS )
      CALL CAT_TATTC( ELLIP, 'EXFMT', 'F10.3', STATUS )
      CALL CAT_TATTC( ABSSXY, 'EXFMT', 'F10.3', STATUS )

*  Field comments.
      CALL CAT_TATTC( INDEX, 'COMM', 'INDEX NUMBER OF OBJECT', STATUS )
      CALL CAT_TATTC( RRATIO, 'COMM', 'RADIUS RATIO OF OBJECT TO MODEL',
     :                STATUS )
      CALL CAT_TATTC( IRATIO, 'COMM',
     :                'NORMALISED TOTAL TO PEAK INTENSITY RATIO ',
     :                STATUS )
      CALL CAT_TATTC( ELLIP, 'COMM', 'ELLIPTICITY', STATUS )
      CALL CAT_TATTC( ABSSXY, 'COMM',
     :                'ABSOLUTE VALUE OF SECOND ORDER MOMENT 1,1',
     :                STATUS )


*  Read the data - one line at a time from the data file
*  read in the data from this file until EOF is reached.
      CALL ERR_MARK
 1    CONTINUE
         CALL RDPIPD( FI, BUF, IVALS( 1 ), RVALS( 2 ), RVALS( 3 ),
     :                RVALS( 4 ), RVALS( 5 ), STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 2

*  Transfer the data.
         CALL CAT_PUT0I( INDEX, IVALS( 1 ), .FALSE., STATUS )
         CALL CAT_PUT0R( RRATIO, RVALS( 2 ), .FALSE., STATUS )
         CALL CAT_PUT0R( IRATIO, RVALS( 3 ), .FALSE., STATUS )
         CALL CAT_PUT0R( ELLIP, RVALS( 4 ), .FALSE., STATUS )
         CALL CAT_PUT0R( ABSSXY, RVALS( 5 ), .FALSE., STATUS )
         CALL CAT_RAPND( CI, STATUS )
      GO TO 1
 2    CONTINUE

* may have status other than end-of-file, check for this
      IF ( STATUS .EQ. FIO__EOF ) THEN
         CALL ERR_ANNUL( STATUS )
      ENDIF
      CALL ERR_RLSE


      END
