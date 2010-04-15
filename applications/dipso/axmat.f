      SUBROUTINE AXMAT( N, AXIS, DATA, NGOOD, STATUS )
*+
* Name:
*    AXMAT

*  Purpose:
*     Fill a data array with flux values with matching axis values

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL AXMAT( N, AXIS, DATA, NGOOD, STATUS )

*  Description:
*     An array of axis values is supplied. Elements from the internal
*     FLUX array which have WAVE values which match any of these axis
*     values are placed in the corresponding elements of the output
*     DATA array. The algorithm used will allow the axis values to
*     increase or decrease with pixel number (in fact they need not
*     even be monotonic).  However, the alogorithm is faster if the
*     axis values are monotonic increasing.
*
*     Axis values are considered to match if the fractional difference
*     between them is no more than the machine precision for single
*     precision floating point values.
*
*     Elements of the DATA array which do not correspond to any element
*     in the internal arrays are set bad (i.e. equal to VAL__BADR).

*  Arguments:
*     N = INTEGER (Given)
*        The size of the DATA and AXIS arrays.
*     AXIS( N ) = REAL (Given)
*        The axis value (i.e. wavelength, velocity, etc) at each element
*        of the DATA array.
*     DATA( N ) = REAL (Returned)
*        The array holding the flux values.
*     NGOOD = INTEGER (Returned)
*        The number of elements in the returned data array which are not
*        set to VAL__BADR.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-SEP-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Global Variables:
      INCLUDE 'DECLARE_STKS'     ! DIPSO array sizes, etc.
*        ASIZE1 = INTEGER (Read)
*           The declared size of the X and Y current arrays.

      INCLUDE 'DECLARE_DATA'     ! DIPSO current arrays
*        FLUX( ASIZE1 ) = REAL (Read)
*           The data value at each element.
*        NPOINT = INTEGER (Read)
*           The number of data elements used in FLUX and WAVE, starting
*           at element 1.
*        WAVE( ASIZE1 ) = REAL (Read)
*           The X value (usually wavelength or velocity) at the
*           corresponding element in the FLUX array.

*  Arguments Given:
      INTEGER N
      REAL AXIS( N )

*  Arguments Returned:
      REAL DATA( N )
      INTEGER NGOOD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :     I,                    ! Index into internal array
     :     J,                    ! Index into output array
     :     J0,                   ! Most likely index of next match
     :     JHI,                  ! Upper bound of current section
     :     JLO,                  ! Lower bound of current section
     :     SECT                  ! Section count

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Fill the returned data array with bad values.
      DO J = 1, N
         DATA( J ) = VAL__BADR
      END DO

*  Initialise the count of good values stored in the returned data
*  array.
      NGOOD = 0

*Initialise the index of the element of the output array which is
*  most likely to have the same axis value as the current element from
*  the input array (assuming the axis values are monotonic increasing
*  with pixel number).
      J0 = 1

*  Loop round all the elements in the input array.
      DO I = 1, NPOINT

*  The output array is divided into two sections; the high end section
*  which extends from the element with index J0 to the end, and the low
*  end section which extends from element 1 to element J0-1. The most
*  likely place to find an element with the same axis value as the
*  current input element is the first element in the high end section
*  (i.e. index J0). If this guess turns out to be wrong, then the rest
*  of the high end section is searched until a matching axis value is
*  found. If no match is found, then the low end section is searched.
*
*  Set up the upper and lower bounds of the high end section of the
*  output data array.
         JHI = N
         JLO = J0

*  Search through each of the two sections of the output array.
         DO SECT = 1, 2

*  Check each axis value in this section. If one is found which is the
*  same as the axis value of the current element of the internal arrays
*  (to within a relative error given by the machine precision, VAL__EPSR),
*  then increment the count of good values in the data array (so long as
*  a value has not already been stored in theis output pixel), store the
*  current internal flux value in the output data array, indicate that
*  the most likely place for the next match is the next pixel (this
*  assumes the axis values are monotonic increasing), and jump out of
*  the loop to do the next element of the internal arrays. Ignore bad
*  axis values.
            DO J = JLO, JHI
               IF( AXIS( J ) .NE. VAL__BADR ) THEN

                  IF( ABS( WAVE( I ) - AXIS( J ) ) .LE. VAL__EPSR*
     :                (0.5*ABS( WAVE( I ) + AXIS( J ) ) ) ) THEN

                     IF( DATA( J ) .EQ. VAL__BADR ) NGOOD = NGOOD + 1
                     DATA( J ) = FLUX( I )
                     J0 = J + 1
                     GO TO 10

                  END IF

               END IF
            END DO

*  If we get here, then we can't have found a matching axis value in the
*  high end section, so try the low end section.
            JHI = J0 - 1
            JLO = 1

         END DO

*  Jump to here if a mcthing axis value is found.
 10      CONTINUE

*  Try to find a match for the next element of the internal arrays.
      END DO

      END
