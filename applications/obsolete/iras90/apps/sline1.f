      SUBROUTINE SLINE1( FID, SCS, NPARL, LON, LAT, SCT, STATUS )
*+
*  Name:
*     SLINE1

*  Purpose:
*     Save the parallel section specifications into a text file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SLINE1( FID, SCS, NPARL, LON, LAT, SCTLN, STATUS )

*  Description:
*     This subroutine saves the specifications of the parallel sections
*     store in three arrays.

*  Arguments:
*     FID = INTEGER (Given)
*        The FID of the text file.
*     SCS = CHARACTER*( * )
*        The name of the sky coordinate system used.
*     NPARL = INTEGER (Given)
*        The number of parallel sections.
*     LON( NGCRL ) = DOUBLE PRECISION
*        The longitude of the begin positions of the sections.
*     LAT( NGCRL ) = DOUBLE PRECISION
*        The latitude of the begin positions of the sections.
*     SCT( NGCRL ) = DOUBLE PRECISION
*        The length of the sections
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     1-JUL-1992 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants

*  Arguments Given:
      INTEGER FID
      CHARACTER*( * ) SCS
      INTEGER NPARL
      DOUBLE PRECISION LON( NPARL ), LAT( NPARL ), SCT( NPARL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER BUF*80           ! Buffer for output text.
      CHARACTER*( IRA__SZFSC ) LONST, LATST, SCTST
                                 ! Formated long. lat. & section length

      INTEGER BUFLEN             ! Used length of BUFFER.
      INTEGER I                  ! Do loop index.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write the keyword for PARALLEL sections.
      CALL FIO_WRITE( FID, ' ', STATUS )
      CALL FIO_WRITE( FID, 'Parallel', STATUS )

*  Write the specifications one by one.
      DO I = 1, NPARL

*  Get the formated string form of the begin position of a parallel
*  section.
         CALL IRA_DTOC( LON( I ), LAT( I ), SCS, 0, LONST, LATST,
     :                  STATUS )

*  Set up message tokens.
         CALL MSG_SETC( 'A', LONST )
         CALL MSG_SETC( 'B', LATST )
         CALL MSG_SETR( 'L', REAL( SCT( I )*IRA__RTOD ) )

*  Construct a string holding all three items.
         CALL MSG_LOAD( ' ', '  ^A, ^B, ^L', BUF, BUFLEN, STATUS )

*  Write the specification of the section into text file.
         CALL FIO_WRITE( FID, BUF( : MIN( 80, BUFLEN ) ), STATUS )

*  If anything wrong, exit.
         IF ( STATUS .NE. SAI__OK ) GOTO 999

      END DO

 999  CONTINUE

      END
