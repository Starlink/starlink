      SUBROUTINE CCDB1_ABIA( IMAGE, NPIX, SEED, STATUS )
*+
*  Name:
*     CCDB1_ABIA

*  Purpose:
*     To add a reproducible pseudo-random bias frame to data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCDB1_ABIA( IMAGE, NPIX, SEED, STATUS )

*  Arguments:
*     IMAGE( NPIX ) = REAL (Given and Returned)
*        The image to which noise is to be added.
*     NPIX = INTEGER (Given)
*        The size of the array image, note that we can handle
*        n-dimensional arrays.
*     SEED = INTEGER (Given)
*        Integer seed, fed to PDA pseudo-random number routines
*        for generating a reproducible sequence of random numbers.
*        Should be of form 4 * INT + 1
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     Assumes there are no bad pixels.

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JUN-1998 (MBT):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NPIX
      INTEGER SEED

*  Arguments Given and Returned:
      REAL IMAGE( NPIX )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL PDA_RNNOR
      REAL PDA_RNNOR             ! The normal random number routine

*  Local Variables:
      INTEGER I                  ! loop variable
      REAL MEAN                  ! mean of Gaussian distribution
      REAL SD                    ! std deviation of Gaussian distribution

*  Data
      DATA MEAN /100.0/

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set standard deviation
      SD = SQRT( MEAN )

*  Set pseudo-random number generation seed
      CALL PDA_RNSED( SEED )

*  Add Gaussian noise to each pixel
      DO I = 1, NPIX
         IMAGE( I ) = IMAGE( I ) + PDA_RNNOR( MEAN, SD )
      END DO

      END
* $Id: ccdb1_abia.f,v 1.2 1998/06/16 17:37:19 mbt Exp mbt $
