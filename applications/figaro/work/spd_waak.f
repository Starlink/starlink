      SUBROUTINE SPD_WAAK( BAD, NELM, VEC1, VEC2, VEC3, VEC4, STATUS )
*+
*  Name:
*     SPD_WAAK

*  Purpose:
*     From data & reciprocal errors to error bars.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WAAK( BAD, NELM, VEC1, VEC2, NGOOD, VEC3, VEC4, STATUS )

*  Description:
*     This routine operates on two input vectors element by element and
*     returns two new vectors. The first result vector is the difference
*     of the first input minus the reciprocal of the second input. The
*     second result vector is the sum of the first input plus the
*     reciprocal of the second input.
*
*        VECTOR3(I) = VECTOR1(I) + 1 / VECTOR2(I)
*        VECTOR4(I) = VECTOR1(I) - 1 / VECTOR2(I)
*
*     Thus if the first input is a data vector and the second input holds
*     the reciprocal errors corresponding to the data, then the output
*     are the lower and upper bounds of the error bars.
*
*     Optionally, this routine can take care of bad values. In any case,
*     whenever an element of the second input vector is zero, that
*     element in both output vectors will be the bad value. The status
*     variable will always be returned unchanged.

*  Arguments:
*     BAD = LOGICAL (Given)
*        True if check for bad values necessary.
*     NELM = INTEGER (Given)
*        The number of elements in each of the vectors. All vectors must
*        be of the same length.
*     VEC1( NELM ) = REAL (Given)
*        The first input vector.
*     VEC2( NELM ) = REAL (Given)
*        The second input vector.
*     NGOOD = INTEGER (Returned)
*        The number of non-bad result elements.
*     VEC3( NELM ) = REAL (Returned)
*        The first output vector.
*     VEC4( NELM ) = REAL (Returned)
*        The second output vector.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     27 Mar 1992 (hme):
*        Original version.
*     27 Jan 1995 (hme):
*        Renamed from SPABQ.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants

*  Arguments Given:
      LOGICAL BAD
      INTEGER NELM
      REAL VEC1( NELM )
      REAL VEC2( NELM )

*  Arguments Returned:
      INTEGER NGOOD
      REAL VEC3( NELM )
      REAL VEC4( NELM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index
      REAL V1, V2                ! Local copies of input vector elements

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise counter for good result elements.
      NGOOD = 0

*  If check for bad values needed.
      IF ( BAD ) THEN
         DO 1 I = 1, NELM
            IF ( VEC1(I) .EQ. VAL__BADR .OR.
     :           VEC2(I) .EQ. VAL__BADR .OR. VEC2(I) .EQ. 0. ) THEN
               VEC3(I) = VAL__BADR
               VEC4(I) = VAL__BADR
            ELSE
               V1 = VEC1(I)
               V2 = VEC2(I)
               VEC3(I) = V1 - 1./V2
               VEC4(I) = V1 + 1./V2
               NGOOD   = NGOOD   + 1
            END IF
 1       CONTINUE

*  Else (no check needed).
      ELSE
         DO 2 I = 1, NELM
            IF ( VEC2(I) .EQ. 0. ) THEN
               VEC3(I) = VAL__BADR
               VEC4(I) = VAL__BADR
            ELSE
               V1 = VEC1(I)
               V2 = VEC2(I)
               VEC3(I) = V1 - 1./V2
               VEC4(I) = V1 + 1./V2
               NGOOD   = NGOOD   + 1
            END IF
 2       CONTINUE
      END IF

*  Return.
      END
