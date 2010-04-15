      SUBROUTINE POL_CALTI( NEL, IN1, IN2, F, OUT, STATUS )
*+
* Name:
*    POL_CALTI

*  Purpose:
*     To calculate an F-factor corrected total intensity image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL_CALTI(  NEL, IN1, IN2, F, OUT, STATUS )

*  Description:
*     Given a pair of polarimetric images (from the same exposure and at
*     the same waveplate angle), calculate the total intensity image
*     by adding them together, after correcting the second image with
*     the instrumental polarisation efficiency factor.

*  Arguments:
*     NEL = INTEGER (Given)
*        Number of image elements
*     IN1( NEL ) = REAL (Given)
*        First image in the pair
*     IN2( NEL ) = REAL (Given)
*        Second image in the pair
*     F = REAL (Given)
*        Instrumental polarisation efficiency
*     OUT( NEL ) = REAL (Returned)
*        Output total intensity image
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     It is assumed that bad pixels are present in the input data.

*  [optional_subroutine_items]...
*
*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     TMG: Tim Gledhill (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-SEP-1997 (TMG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Primdat constants

*  Arguments Given:
      INTEGER NEL
      REAL IN1( NEL )
      REAL IN2( NEL )
      REAL F

*  Arguments Returned:
      REAL OUT( NEL )

*  Local Variables:
      INTEGER IEL                ! loop counter
      REAL RF                    ! reciprocal F

*  Status:
      INTEGER STATUS             ! Global status

*.


* Loop through the elements.
      RF = 1.0 / MAX( F, 1.0E-20 )
      DO IEL = 1, NEL

* If either of the input elements is bad then the output element is bad.
         IF ( IN1( IEL ) .EQ. VAL__BADR .OR.
     :        IN2( IEL ) .EQ. VAL__BADR ) THEN
            OUT( IEL ) = VAL__BADR

* If both input elements are good then add the two input arrays after
* dividing the second one by F.
         ELSE
            OUT( IEL ) = IN1( IEL ) + IN2( IEL ) * RF
         ENDIF
      ENDDO

* Close down.
 99   CONTINUE
      END
