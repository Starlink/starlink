      SUBROUTINE CDCRA2( PWIDX, DETWID, STATUS )
*+
*  Name:
*     CDCRA2

*  Purpose:
*     Extend the width of the IRAS detectors.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CDCRA2( PWIDX, DETWID, STATUS )

*  Description:
*     This subroutine is used to extend the width ( given as Delt Z
*     in the Table II.C.3 of Exp. Supp. ) of the IRAS detectors.  The
*     retuned detector width are: DeltZ + WIDX * DeltZ, where WIDX is
*     the value obtained from the environment.

*  Arguments:
*     PWIDX = CHARACTER (Given)
*        Name of the parameter used to get the extension of detectors.
*     DETWID( I90__DETS ) = REAL (Returned)
*        The modified detector width.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     20-NOV-1992 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 package constants

*  Arguments Given:
      CHARACTER*( * ) PWIDX

*  Arguments Returned:
      REAL DETWID( I90__DETS )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER  I                 ! Do loop index
      REAL WIDX                  ! Detector extension factor

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the extension factor from the environment.
      CALL PAR_GET0R( PWIDX, WIDX, STATUS )

*  Calculate the width of each detector.
      DO I = 1, I90__DETS
         DETWID( I ) = I90__DETDZ( I ) + WIDX * I90__DETDZ( I )
      END DO

      END

