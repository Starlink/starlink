      SUBROUTINE POINB1( IDC, IDET, MAXDET, MAXSRC, EXPSRC, SRCN,
     :                   SRCSMP, DETNSM, SRCRA, SRCDEC,
     :                   SRCINS, SRCSPD, SRCANG, STATUS )
*+
*  Name:
*     POINB1

*  Purpose:
*     Find sky positions of sources.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POINB1( IDC, IDET, MAXDET, MAXSRC, EXPSRC, SRCN,
*                  SRCSMP, DETNSM, SRCRA, SRCDEC,
*                  SRCINS, SRCSPD, SRCANG, STATUS )

*  Description:
*     This subroutine find the sky positions: RA and DEC of the sources,
*     and in-scan distances from the expected source position if a source
*     position has been supplied.
*     The sources are given by their detector index and their sample
*     indices in the trace.

*  Arguments:
*     IDC = INTEGER (Given)
*        The IRC ID of the input CRDD NDF.
*     IDET = INTEGER (Given)
*        Detector index of the trace.
*     MAXDET = INTEGER (Given)
*        Maximum number of detectors
*     MAXSRC = INTEGER (Given)
*        Maximum number of sources
*     EXPSRC = LOGICAL (Given)
*        Expected position source flag.
*     SRCN = INTEGER (Given)
*        The number of sources.
*     SRCSMP( MAXSRC ) = REAL (Given)
*        Fracture sample index number of the souces.
*     DETNSM( MAXDET ) = REAL (Given)
*        The sample position at which the trace crosses the expected
*        source position.
*     SRCRA( MAXSRC ) = DOUBLE PRECISION (Returned)
*        The RA of the source position.
*     SRCDEC( MAXSRC ) = DOUBLE PRECISION (Returned)
*        The DEC of the source position.
*     SRCINS( MAXSRC ) = REAL (Returned)
*        The in-scan distance of the source from the expected source
*        position, in arcmins.
*     SRCSPD( MAXSRC ) = REAL (Returned)
*        The scan speed at each source position.
*     SRCANG( MAXSRC ) = DOUBLE PRECISION (Returned)
*        The scan angle at each source position.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DCP: Diana Parsons (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     5-MAR-1993 (WG):
*        Original version.
*     17-oct-1994 (DCP):
*        Modified for new pointcrdd
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRA_PAR'          ! IRA package constants

*  Arguments Given:
      INTEGER IDC
      INTEGER IDET
      INTEGER MAXDET
      INTEGER MAXSRC
      LOGICAL EXPSRC
      INTEGER SRCN
      REAL SRCSMP( MAXSRC )
      REAL DETNSM( MAXDET )

*  Arguments Returned:
      DOUBLE PRECISION SRCRA( MAXSRC )
      DOUBLE PRECISION SRCDEC( MAXSRC )
      REAL SRCINS( MAXSRC )
      REAL SRCSPD( MAXSRC )
      DOUBLE PRECISION SRCANG( MAXSRC )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL INSCAN                ! In scan distance between expected
				 ! source sample and actual source sample
      INTEGER ISRC               ! Do loop index

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the RA and DEC of the source positions.
      DO ISRC = 1, SRCN
         CALL IRC_DPOS( IDC, 1, SRCSMP( ISRC ), IDET, SRCRA( ISRC ),
     :                  SRCDEC( ISRC ), SRCANG( ISRC ), SRCSPD( ISRC ),
     :                  STATUS )
      END DO

*  If an expected point source was supplied, find the in-scan distance
*  from the detected source to the expected source position.
      IF ( EXPSRC ) THEN
         DO ISRC = 1, SRCN
            CALL IRC_DIST( IDC, DETNSM( IDET ), IDET, SRCSMP( ISRC ),
     :                     IDET, INSCAN, STATUS )

*  Convert to arcmin.
            SRCINS( ISRC ) = INSCAN * REAL( IRA__RTOD ) * 60.0
         END DO
      END IF

      END
