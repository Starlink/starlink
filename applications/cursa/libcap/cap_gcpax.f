      SUBROUTINE CAP_GCPAX (START, INCR, ELEM, AXVALS, STATUS)
*+
*  Name:
*     CAP_GCPAX
*  Purpose:
*     Compute an array of values for one axis of a grid.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GCPAX (START, INCR, ELEM, AXVALS, STATUS)
*  Description:
*     Compute an array of values for one axis of a grid.
*  Arguments:
*     START  =  REAL (Given)
*        Axis value corresponding to the first element in the grid.
*     INCR  =  REAL (Given)
*        Axis range corresponding to the increment between successive
*        grid elements.
*     ELEM  =  INTEGER (Given)
*        Number of elements in the grid.
*     AXVALS(ELEM)  =  REAL (Returned)
*        Array of axis values.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Compute the central value for each element of the axis.
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     25/6/99 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'  ! Standard Starlink constants.
*  Arguments Given:
      REAL
     :  START,
     :  INCR
      INTEGER
     :  ELEM
*  Arguments Returned:
      REAL
     :  AXVALS(ELEM)
*  Status:
      INTEGER STATUS     ! Global status.
*  Local Variables:
      INTEGER
     :  LOOP             ! Loop index.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Compute the central value for each element of the axis.

         DO LOOP = 1, ELEM
            AXVALS(LOOP) = START + (INCR * (LOOP - 1) )
         END DO

      END IF

      END
