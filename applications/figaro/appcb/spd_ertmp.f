      SUBROUTINE SPD_ERTMP ( NELM, INTEN, MAXINT, MININT, VAR, STATUS)
*+
*  Name:
*     SPD_ERTMP
*  Purpose:
*     Compute variance array for black-body spectrum.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL SPD_ERTMP ( NELM, INTEN, MAXINT, MININT; VAR; STATUS)
*  Description:
*     This routine computes a variance array for each point in a
*     black-body spectrum.  It is given the intensity of the point,
*     and the minimum and maximum intensities corresponding to the
*     error on the temperature of the black-body.
*  Arguments:
*     NELM  =  INTEGER (Given)
*        The number of elements in the spectrum.
*     INTEN(NELM)  =  REAL (Given)
*        Intensity of the black-body spectrum at each point.
*     MAXINT(NELM)  =  REAL (Given)
*        Maximum intensity of the black-body spectrum at each point
*        corresponding to the error in the black-body temperature.
*     MININT(NELM)  =  REAL (Given)
*        Minimum intensity of the black-body spectrum at each point
*        corresponding to the error in the black-body temperature.
*     VAR(NELM)  =  REAL (Returned)
*        Variance of the intensity of the black-body spectrum at each
*        point.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For each point in the spectrum
*       Compute the absolute difference between the maximum intensity
*       and the intensity.
*       Compute the absolute difference between the minimum intensity
*       and the intensity.
*       Average the two differences.
*       Square the differences to give the variance.
*     end if
*  Implementation Deficiencies:
*     Note that the errors are asymetric and this procedure is an
*     approximation.
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     7/7/01 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
*  Arguments Given:
      INTEGER
     :  NELM
      REAL
     :  INTEN(NELM),
     :  MAXINT(NELM),
     :  MININT(NELM)
*  Arguments Returned:
      REAL
     :  VAR(NELM)
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  LOOP        ! Loop index.
      REAL
     :  DIFFMX,     ! Absolute difference of maximum temp.
     :  DIFFMN,     !     "         "     "  minimum  "  .
     :  DIFFAV      ! Average difference.
*.

      IF (STATUS .EQ. SAI__OK) THEN

         DO LOOP = 1, NELM
            DIFFMX = ABS( MAXINT(LOOP) - INTEN(LOOP) )
            DIFFMN = ABS( INTEN(LOOP) - MININT(LOOP) )

            DIFFAV = (DIFFMX + DIFFMN) / 2.0E0

            VAR(LOOP) = DIFFAV * DIFFAV
         END DO

      END IF

      END
