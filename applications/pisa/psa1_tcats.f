      SUBROUTINE PSA1_TCATS( NAME, IFS, STATUS )
*+
*  Name:
*     PSA1_TCATS

*  Purpose:
*     To transfer the PISASIZE catalogue of data attached to the
*     file IFS to a SCAR file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PSA1_TCATS( NAME, IFS, STATUS )

*  Description:
*     The routine reads in the PISAFIND data a line at a time. This is
*     then transfered to the presently opened CHI data file with file
*     name NAME. A call to PSA1_CCATF must be made before calling this
*     routine to create an empty SCAR catalogue of the appropriate
*     type.

*  Arguments:
*     NAME = CHARACTER * ( * )
*        Name of the CHI/SCAR catalogue.
*     IFS = INTEGER (Given)
*        The FIO system descriptor of file containing the PISASIZE
*        pixel counts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-MAY-1991 (PDRAPER):
*        Original Version.
*     3-SEP-1992 (PDRAPER):
*        Changed to CHI version 1.1.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_ERR'          ! FIO system error codes
      INCLUDE 'CHI_PAR'          ! CHI system buffer sizes

*  Arguments Given:
      CHARACTER * ( * ) NAME
      INTEGER IFS

*  Status:
      INTEGER STATUS             ! Global status

*  Local constants
      INTEGER NFLD               ! Number of fields in output catalogue
      PARAMETER ( NFLD = 9 )

*  Local Variables:
      CHARACTER BUF*132
      CHARACTER * 1 FTYPE( NFLD ) ! Used buffer types
      INTEGER IVALS( NFLD )      ! Integer buffer
      REAL RVALS( NFLD )         ! Real buffer
      LOGICAL LVALS( NFLD )      ! Logical buffer
      DOUBLE PRECISION DVALS( NFLD ) ! DBLE buffer
      CHARACTER * 1 CVALS( NFLD ) ! character buffer
      CHARACTER * ( CHI__SZFNAME ) FNAMES( NFLD ) ! field names

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up the field names.
      FNAMES( 1 ) = 'INDEX'
      FNAMES( 2 ) = 'A1'
      FNAMES( 3 ) = 'A2'
      FNAMES( 4 ) = 'A3'
      FNAMES( 5 ) = 'A4'
      FNAMES( 6 ) = 'A5'
      FNAMES( 7 ) = 'A6'
      FNAMES( 8 ) = 'A7'
      FNAMES( 9 ) = 'A8'

*  Set the field types, showing which fields are being given at which
*  type.
      FTYPE( 1 ) = 'I'
      FTYPE( 2 ) = 'R'
      FTYPE( 3 ) = 'R'
      FTYPE( 4 ) = 'R'
      FTYPE( 5 ) = 'R'
      FTYPE( 6 ) = 'R'
      FTYPE( 7 ) = 'R'
      FTYPE( 8 ) = 'R'
      FTYPE( 9 ) = 'R'

*  Read the data - one line at a time from the PISAFIND data file
*  read in the data from this file until EOF is reached.
 1    CONTINUE
         CALL RDPISD( IFS, BUF, IVALS( 1 ), RVALS( 2 ), RVALS( 3 ),
     :                RVALS( 4 ), RVALS( 5 ), RVALS( 6 ), RVALS( 7 ),
     :                RVALS( 8 ), RVALS( 9 ), STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 2

*  Transfer the data.
         CALL CHI_PUTENT( NAME, FNAMES, NFLD, 1, CVALS, DVALS, IVALS,
     :                    LVALS, RVALS, FTYPE, STATUS )
      GO TO 1
 2    CONTINUE

* may have status other than end-of-file, check for this
      IF ( STATUS .EQ. FIO__EOF ) THEN
          STATUS = SAI__OK
      ENDIF

      END
