      SUBROUTINE CON_SAXIS (OFFSET, NPTS, AXIS, STATUS)
*+
*  Name:
*     CON_SAXIS
*  Purpose:
*     Compute one of the sky axes.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CON_SAXIS (OFFSET, NPTS; AXIS; STATUS)
*  Description:
*     Compute one of the sky axes for a data cube.  The axis is
*     computed as an offset from a central point on the celestial sphere.
*  Arguments:
*     OFFSET  =  REAL (Given)
*        The offset between successive elements in the sky map.
*     NPTS  =  INTEGER (Given)
*        Number of points in the axis.
*     AXIS(NPTS)  =  REAL (Returned)
*        Central radial velocity of each point in the axis (Km/sec).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Compute the start offset
*     For each point in the axis
*       Compute the offset.
*     end for
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     7/7/97  (ACD): Original version.
*     23/7/97 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
*  Arguments Given:
      REAL
     :  OFFSET
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
      REAL
     :  START     ! Start offset.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Compute the start offset.

         START = - (OFFSET * REAL( (NPTS/2) + 1 ))

*
*       Compute the offset for each point along the axis.

         DO LOOP = 1, NPTS
            AXIS(LOOP) = START + (REAL(LOOP) * OFFSET)
         END DO

      END IF

      END
