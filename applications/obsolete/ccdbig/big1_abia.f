      SUBROUTINE CCDB1_ABIA( IMAGE, DIM1, DIM2, INL, INR, SEED, STATUS )
*+
*  Name:
*     CCDB1_ABIA

*  Purpose:
*     To impose a reproducible pseudo-random bias frame to data.
*     This is done in two parts: over the bias strips noise only is
*     written, while between the bias strips noise is added to the
*     existing value of the image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCDB1_ABIA( IMAGE, DIM1, DIM2, INL, INR, SEED, STATUS )

*  Arguments:
*     IMAGE( DIM1, DIM2 ) = REAL (Given and Returned)
*        The image to which noise is to be added.
*     DIM1 = INTEGER (Given)
*        First dimension of IMAGE.
*     DIM2 = INTEGER (Given)
*        Second dimension of IMAGE.
*     INL = INTEGER (Given)
*        Width of left bias strip.
*     INR = INTEGER (Given)
*        Width of right bias strip.
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
      INTEGER DIM1
      INTEGER DIM2
      INTEGER INL
      INTEGER INR
      INTEGER SEED

*  Arguments Given and Returned:
      REAL IMAGE( DIM1, DIM2 )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL PDA_RNNOR
      REAL PDA_RNNOR             ! The normal random number routine

*  Local Variables:
      INTEGER I                  ! loop variable
      INTEGER J                  ! loop variable
      INTEGER UP                 ! right boundary of data region
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

*  Set right boundary of bias region.
      UP = DIM1 - INR

*  Add Gaussian noise to each pixel
      DO J = 1, DIM2
         DO I = 1, DIM1
            NOISE = PDA_RNNOR( MEAN, SD )
            IF ( I .GE. INL .AND. I .LE. UP ) THEN

*     Add bias noise to image in data region.
               IMAGE( I ) = IMAGE( I ) + NOISE

            ELSE

*     Replace image by bias noise in bias strip region.
               IMAGE( I ) = NOISE

            END IF
         END DO

      END
* $Id: ccdb1_abia.f,v 1.3 1998/06/17 10:23:45 mbt Exp $
