      SUBROUTINE ADDNOISE( STATUS )
*+
*  Name:
*     ADDNOISE

*  Purpose:
*     Adds noise to model data.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ADDNOISE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     ADDNOISE, adds noise to model data. The noise can be either
*     poissonian or gaussian. If gaussian a constant noise level,
*     described by a given standard deviation, is introduced. If
*     poissonian the data is scaled by a factor to change data values
*     into counts. The counts are then used as estimates of the mean
*     value in that pixel and noise is added on this basis. Note that
*     the poissonian noise is pseudo gaussian and so the count levels in
*     the frame need to be greater than 10.
*
*     Each time this program is started a different set of random
*     numbers should be generated.

*  Usage:
*     ADDNOISE IN NOISE OUT { ADU = ?
*                           { SIGMA = ?

*  ADAM Parameters:
*     IN = NDF (Read)
*        The input NDF containing the data to which noise needs to be
*        added.
*     NOISE = LITERAL (Read)
*        The noise type to introduce into the data. Either Gaussian or
*        Poissonian, which may be abbreviated to G and P. [P]
*     ADU = REAL (Read)
*        The scaling factor to convert the data values in the input NDF
*        to counts for which poisson statistics are assumed valid. [1.0]
*     SIGMA = REAL (Read)
*        The standard deviation of the gaussian noise. [1.0]
*     OUT = NDF (Write)
*        The output NDF to contain the data with noise added.

*  Examples:
*     ADDNOISE IN=MODEL NOISE=G SIGMA=20 OUT=MODEL_WITH_NOISE
*        This adds gaussian noise to the input NDF model. The noise has
*        a standard deviation of 20 units.
*     ADDNOISE IN=MODEL NOISE=P ADU=10 OUT=MODEL_WITH_NOISE
*        This adds poissonian noise to the input NDF model. The data
*        values are scaled by a factor of 10 before the noise is
*        calculated.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-DEC-1990 (PDRAPER):
*        Original version.
*     07-SEP-2004 (PDRAPER):
*        Changed to use CNF_PVAL.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! CNF functions

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER*10 NTYPE         ! noise type required
      INTEGER IPOUT              ! pointer to output data comp
      INTEGER IPVAR              ! pointer to output variance comp
      INTEGER NDFIN              ! identifier for input NDF
      INTEGER NDFOUT             ! identifier for output NDF
      LOGICAL POISON             ! set to true if poisson noise needed
      REAL ADU                   ! the conversion factor from data to
                                 ! counts
      REAL SIGMA                 ! the gaussian noise standard deviation
      INTEGER EL                 ! the number of pixels in data components

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  access the input NDF containing the data to add noise too
      CALL NDF_BEGIN
      CALL NDF_ASSOC( 'IN', 'READ', NDFIN, STATUS )

*  what sort of noise do we want, poisson or gaussian
      CALL PAR_GET0C( 'NOISE', NTYPE, STATUS )
      CALL CHR_UCASE( NTYPE )
      ADU = 1.0D0
      SIGMA = 1.0D0
      POISON = .FALSE.
      IF ( NTYPE(1:1) .EQ. 'P' ) THEN
         POISON = .TRUE.

*  find the ADU conversion factor
         CALL PAR_GET0R( 'ADU', ADU, STATUS )
      ELSE

*  get the sigma for the noise
         CALL PAR_GET0R( 'SIGMA', SIGMA, STATUS )
      END IF

*  get the output image, remove any variances it may have, and copy the
*  input data array into it.
      CALL NDF_PROP( NDFIN, 'DATA,QUALITY,NOVARIANCE', 'OUT', NDFOUT,
     :               STATUS )

*  map in the data component
      CALL NDF_MAP( NDFOUT, 'DATA', '_REAL', 'UPDATE', IPOUT, EL,
     :              STATUS )

*  add a variance component to the output NDF
      CALL NDF_MAP( NDFOUT, 'VARIANCE', '_REAL', 'WRITE', IPVAR,
     :              EL, STATUS )

*  right do the processing
      CALL ANOISE( %VAL( CNF_PVAL( IPOUT ) ), EL, POISON, SIGMA, ADU,
     :             %VAL( CNF_PVAL( IPVAR ) ), STATUS )

*  close down
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ADDNOISE_ERR',
     :                 'ADDNOISE: Error adding noise to model frame.',
     :                  STATUS )
      END IF

      END
* $Id$
