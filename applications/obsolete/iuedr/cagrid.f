      SUBROUTINE CAGRID( STATUS )
*+
*  Name:
*     SUBROUTINE CAGRID

*  Purpose:
*     Define calibration to correct for uneven sampling rate.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CAGRID( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     If the wavelength grid is evenly spaced, but this is superposed
*     on a distorted dataset, then the effect pixel area integrated
*     per wavelength bin varies along the spectrum.
*     This routine works out the effective pixel bin size for each
*     point on the wavelength "grid".
*     The array SBIN is then filled with scale factors by which SNET is
*     multiplied so that it produces a spectrum that is consistent with
*     IUESIPS calibration.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     08-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     16-DEC-94 (MJC)
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER STATUS     ! Global status.

*  Global Variables:
      INCLUDE 'CMDATA'
      INCLUDE 'CMEXTP'
      INCLUDE 'CMWAV'
      INCLUDE 'CMBIN'

*  Local Variables:
      REAL*8 BINSIZ      ! bin size

      INTEGER I          ! loop index

*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Set BIN undefined
      NOBIN = .TRUE.

*   For each point in the wavelength array
      DO I = 1, NWAV
         BINSIZ = 1.0
         SBIN( I ) = DSCALE * ( 1.414 / BINSIZ )
      END DO
      NOBIN = .FALSE.

      END
