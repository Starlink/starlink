      SUBROUTINE PSF_CONVOLVE( NX, NY, IN, KNX, KNY, KERN, OUT, STATUS )
*+
*  Name:
*     PSF_CONVOLVE

*  Purpose:
*     Convolve an input image with a psf kernel

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PSF_CONVOLVE( NX, NY, IN, KNX, KNY, KERN, OUT, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     NX = INTEGER (given)
*        X dimension of image to be convolved
*     NY = INTEGER (given)
*        Y dimension of image to be convolved
*     IN[NX,NY] = REAL (given)
*        Image to convolve
*     KNX = INTEGER (given)
*        X dimension of kernel
*     KNY = INTEGER (given)
*        Y dimension of kernel
*     KERN[-KNX/2:KNX/2,-KNY/2:KNY/2] = REAL (given)
*        Kernel image
*     OUT[NX,NY] = REAL (returned)
*        Convolved image
*     STATUS = INTEGER (given)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     PSF Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/psf.html

*  Keywords:
*     package:psf, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     15 May 1996 (DJA):
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
      INTEGER			NX, NY, KNX, KNY
      REAL			IN(NX,NY),
     :                          KERN(-KNX/2:KNX/2,-KNY:2,KNY/2)

*  Arguments Returned:
      REAL			OUT(NX,NY)

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Variables:
      INTEGER			I,J 			! Loop over image
      INTEGER			II, JJ			! Loop over kernel
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      DO J = 1, NY
        DO I = 1, NX
          OUT(I,J) = 0.0
        END DO
      END DO

*  Convolve
      DO J = 1, NY
        DO I = 1, NX
          DO JJ = MAX(1,J-KNY/2), MIN(NY,J+KNY/2)
            DO II = MAX(1,I-KNX/2), MIN(NX,I+KNX/2)
              OUT(II,JJ) = OUT(II,JJ) + IN(I,J) * KERN(II-I,JJ-J)
            END DO
          END DO
        END DO
      END DO

      END
