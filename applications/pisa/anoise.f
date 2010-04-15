      SUBROUTINE ANOISE( IMAGE, NPIX, POISON, SIGMA, ADU, VARI, STATUS )
*+
*  Name:
*     ANOISE

*  Purpose:
*     To add pseudo-random noise to model data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ANOISE( IMAGE, NPIX, POISON, SIGMA, ADU, VARI, STATUS )

*  Description:
*     The routine accesses an one dimensional real array and attempts to
*     add reasonable noise values to it. If the parameter POISON is set
*     to false then normal ( Gaussian ) noise , with a standard
*     deviation SIGMA is added to the data. Calls to the routine
*     RANORM supply the random values. If POISON is set to true then an
*     attempt to add poisson noise is made. This works only if the
*     counts are sufficiently great ( gt 10 ) so that the distribution
*     is approximately normal. The noise estimates are made with each
*     pixel being taken as representative of the mean value at that
*     point. Finally the variances are recorded in a dimension which can
*     be used in an NDF.

*  Arguments:
*     IMAGE( NPIX ) = REAL (Given and Returned)
*        The image to which noise is to be added.
*     NPIX = INTEGER (Given)
*        The size of the array image, note that we can handle
*        n-dimensional arrays.
*     POISON = LOGICAL (Given)
*        If set to true then poisson noise is added. If set to
*        false then normal noise is added.
*     SIGMA = REAL (Given)
*        The sigma of the normal distribution if used.
*     ADU = REAL (Returned)
*        The scaling factor to get the values in IMAGE to their counting
*        values.
*     VARI( NPIX ) = REAL (Returned)
*        The variances of the noise in array IMAGE on output.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-DEC-1990 (PDRAPER):
*        Original version.
*     6-MAY-1992 (PDRAPER):
*        Removed BAD_PAR changed to more standard PRM_PAR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Bad values VAL__BADR

*  Arguments Given:
      INTEGER NPIX
      LOGICAL POISON
      REAL SIGMA
      REAL ADU

*  Arguments Given and Returned:
      REAL IMAGE( NPIX )

*  Arguments Returned:
      REAL VARI( NPIX )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL RANORM
      REAL RANORM                ! The normal random number routine

*  Local Variables:
      INTEGER I                  ! loop variable
      REAL VALUE
      REAL NOISE

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Branch for different statistics
      IF ( .NOT. POISON ) THEN

*  Doing pseudo-gaussian noise
         DO 1 I = 1, NPIX

*  find a value about a mean of zero, with standard deviation sigma
            IF( IMAGE( I ) .NE. VAL__BADR ) THEN
               NOISE = RANORM( 0.0, SIGMA, STATUS )
               IMAGE( I ) = IMAGE( I ) + NOISE
               VARI( I ) = SIGMA * SIGMA
            END IF
 1       CONTINUE
      ELSE

*  Doing pseudo-poissonian noise, do each pixel
         DO 2  I = 1, NPIX
            IF( IMAGE( I ) .NE. VAL__BADR ) THEN

*  scale image value to estimate the real mean.
               VALUE = IMAGE( I ) * ADU

*  find the noise value for this means standard deviation
               NOISE = RANORM( 0.0, SQRT( ABS( VALUE)  ), STATUS )

* scale noise back down
               NOISE = NOISE / ADU

*  form the variance before modifying the image values
               VARI( I ) = IMAGE( I ) / ADU

*  modify the image value with this noise estimate
               IMAGE( I ) = IMAGE( I ) + NOISE
            END IF
 2       CONTINUE
      END IF

      END
* $Id$
