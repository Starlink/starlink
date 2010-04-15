      SUBROUTINE SLINE3( FID, SCS, MXNSCT, MXVTCE, NPOLY, NVTCE,
     :                   LON, LAT, STATUS )
*+
*  Name:
*     SLINE3

*  Purpose:
*     Save the polyline specifications into a text file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SLINE3( FID, SCS, MXNSCT, MXVTCE, NPOLY, NVTCE, LON, LAT,
*                  STATUS )

*  Description:
*     This subroutine saves the specifications of the polylines stored
*     in two arrays into a text file.

*  Arguments:
*     FID = INTEGER (Given)
*        The FID of the text file.
*     SCS = CHARACTER*( * )
*        The name of the sky coordinate system used.
*     MXVTCE = INTEGER (Given)
*        The max. number of polylines.
*     MXVTCE = INTEGER (Given)
*        The max. number of vertices in each polyline.
*     NPOLY = INTEGER (Given)
*        The number of polylines.
*     NVTCE( NPOLY ) = INTEGER (Given)
*        the number of vertices in each polyline.
*     LON( NPOLY, MXVTCE ) = DOUBLE PRECISION
*        The longitude of the vertices of each polyline.
*     LAT( NPOLY, MXVTCE ) = DOUBLE PRECISION
*        The latitude of the vertices of each polyline.
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
      INTEGER MXNSCT
      INTEGER MXVTCE
      INTEGER NPOLY
      INTEGER NVTCE( MXNSCT )
      DOUBLE PRECISION LON( MXNSCT, MXVTCE ), LAT( MXNSCT, MXVTCE )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER BUF*80           ! Buffer for output text.
      CHARACTER*( IRA__SZFSC ) LONST, LATST, SCTST
                                 ! Formated long. lat. & section length

      INTEGER BUFLEN             ! Used length of BUFFER.
      INTEGER I, J               ! Do loop indices.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write the specifications of polylines one by one.
      DO I = 1, NPOLY

*  Write the keyword for sections.
         CALL FIO_WRITE( FID, ' ', STATUS )
         CALL FIO_WRITE( FID, 'Polyline', STATUS )

*  Write the position of the vertices one by one.
         DO J = 1, NVTCE( I )

*  Get the formated string form of the vertice.
            CALL IRA_DTOC( LON( I, J ), LAT( I, J ), SCS, 0, LONST,
     :                     LATST, STATUS )

*  Set up message tokens.
            CALL MSG_SETC( 'A', LONST )
            CALL MSG_SETC( 'B', LATST )

*  Construct a string holding both items.
            CALL MSG_LOAD( ' ', '  ^A, ^B', BUF, BUFLEN, STATUS )

*  Write the specification of the meridian section into text file.
            CALL FIO_WRITE( FID, BUF( : MIN( 80, BUFLEN ) ), STATUS )

*  If anything wrong, exit.
            IF ( STATUS .NE. SAI__OK ) GOTO 999

         END DO

      END DO

 999  CONTINUE

      END
