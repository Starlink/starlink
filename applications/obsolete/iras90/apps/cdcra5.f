      SUBROUTINE CDCRA5( NCROS, CRSFLX, CRSDTX, CRSDIS, CRSSMP, STATUS )
*+
*  Name:
*     CDCRA5

*  Purpose:
*     Sort the crossings into X-scan distance order.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CDCRA5( NCROS, CRSFLX, CRSDTX, CRSDIS, CRSSMP, STATUS )

*  Description:
*     This subroutine is used to sort the crossings (file indices,
*     detector indices, X-distances, crossing samples and crossing
*     directions ) into the absolute X-scan distance order from nearest
*     to farest.

*  Arguments:
*     NCROS = INTEGER (Given)
*        Number of crossing.
*     CRSFLX( NCROS ) = INTEGER (Given and Returned)
*        File index of each crossing.
*     CRSDTX( NCROS ) = INTEGER (Given and Returned)
*        Detector index of each crossing.
*     CRSDIS( NCROS ) = REAL (Given and Returned)
*        X-scan distance of each crossing.
*     CRSSMP( NCROS ) = REAL (Given and Returned)
*        Crossing sample of each crossing.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     23-NOV-1992 (WG):
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
      INTEGER NCROS

*  Arguments Given and Returned:
      INTEGER CRSFLX( NCROS ), CRSDTX( NCROS )
      REAL CRSDIS( NCROS ), CRSSMP( NCROS )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Do loop index
      INTEGER ITEMP              ! A temporary integer value
      REAL RTEMP                 ! A temporary real value
      LOGICAL SWAP               ! Swap flag

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Compare the adjacent pair and swap if required until no more swap
*  happens.
      SWAP = .TRUE.
      DO WHILE ( SWAP )
         SWAP = .FALSE.
         DO I = 1, NCROS - 1
            IF ( ABS( CRSDIS( I ) ) .GT. ABS( CRSDIS( I + 1 ) ) ) THEN
               SWAP = .TRUE.

*  Swap file index.
               ITEMP = CRSFLX( I )
               CRSFLX( I ) = CRSFLX( I + 1 )
               CRSFLX( I + 1 ) = ITEMP

*  Swap detector index.
               ITEMP = CRSDTX ( I )
               CRSDTX( I ) = CRSDTX( I + 1 )
               CRSDTX( I + 1 ) = ITEMP

*  Swap x-scan distance.
               RTEMP = CRSDIS( I )
               CRSDIS( I ) = CRSDIS( I + 1 )
               CRSDIS( I + 1 ) = RTEMP

*  Swap crossing sample.
               RTEMP = CRSSMP ( I )
               CRSSMP( I ) = CRSSMP( I + 1 )
               CRSSMP( I + 1 ) = RTEMP
            END IF
         END DO
      END DO

      END
