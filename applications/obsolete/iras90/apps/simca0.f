      SUBROUTINE SIMCA0( PSFPRE, IGRP, STATUS )
*+
*  Name:
*     SIMCA0

*  Purpose:
*     Create a group holding the names of all available detector PSFs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SIMCA0( PSFPRE, IGRP, STATUS )

*  Description:
*     This routine creates a group in which are stored the names of the
*     NDFs holding the PSFs for each detector. Element N of the group
*     holds the name of NDF containing the PSF for detector N (N = 1 to
*     62). The names are formed by appending the strings "1" to "62" to
*     the end of the supplied file name prefix. No checks on the
*     existence of these NDFs are performed.

*  Arguments:
*     PSFPRE = CHARACTER * ( * ) (Given)
*        The file name prefix.
*     IGRP = INTEGER (Returned)
*        The GRP identifier for the group holding the names of the PSF
*        NDFs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-JAN-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 data

*  Arguments Given and Returned:
      CHARACTER PSFPRE*(*)

*  Arguments Returned:
      INTEGER IGRP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Returns used length of a string.

*  Local Variables:
      INTEGER DETNO              ! Detector number.
      INTEGER START              ! Position for detector number within
                                 ! PSFPRE.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a new group.
      CALL GRP_NEW( 'PSF NDF NAMES', IGRP, STATUS )

*  Store the position of the first blank after the PSF prefix.
      START = CHR_LEN( PSFPRE ) + 1

*  Store the name of the NDF holding each detector PSF.
      DO DETNO = 1, 9
         WRITE( PSFPRE( START : ), '(I1)' ) DETNO
         CALL GRP_PUT( IGRP, 1, PSFPRE, DETNO, STATUS )
      END DO

      DO DETNO = 10, I90__DETS
         WRITE( PSFPRE( START : ), '(I2)' ) DETNO
         CALL GRP_PUT( IGRP, 1, PSFPRE, DETNO, STATUS )
      END DO

      END
