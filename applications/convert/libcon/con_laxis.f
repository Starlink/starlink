      SUBROUTINE CON_LAXIS (ZEROPT, SCALE, NPTS, AXIS, STATUS)
*+
*  Name:
*     CON_LAXIS
*  Purpose:
*     Compute a linear axis.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CON_LAXIS (ZEROPT, SCALE, NPTS; AXIS; STATUS)
*  Description:
*     Compute a linear axis, given a zero point and a scale factor.
*  Arguments:
*     ZEROPT  =  REAL (Given)
*        Zero point.
*     SCALE  =  REAL (Given)
*        Scale factor.
*     NPTS  =  INTEGER (Given)
*        Number of points in the axis.
*     AXIS(NPTS)  =  REAL (Returned)
*        Central radial velocity of each point in the axis (Km/sec).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For each point in the axis
*       Compute the axis value.
*     end for
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     3/9/97 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
*  Arguments Given:
      REAL
     :  ZEROPT,
     :  SCALE
      INTEGER
     :  NPTS
*  Arguments Returned:
      REAL
     :  AXIS(NPTS)
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      INTEGER
     :  LOOP      ! Loop index.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Compute the value for each point along the axis.

         DO LOOP = 1, NPTS
            AXIS(LOOP) = ZEROPT + (REAL(LOOP-1) * SCALE)
         END DO

      END IF

      END
