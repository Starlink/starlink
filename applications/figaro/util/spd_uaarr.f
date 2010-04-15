      SUBROUTINE SPD_UAARR( BAD, NELM, FACTOR, VECTOR, RESULT,
     :   IERR, NERR, STATUS )
*+
*  Name:
*     SPD_UAAR{DR}

*  Purpose:
*     Multiply an array with a constant.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_UAARR( BAD, NELM, FACTOR, VECTOR, RESULT, IERR, NERR,
*        STATUS )

*  Description:
*     This routine multiplies a vector with a constant factor, taking
*     care of bad values if required. This routine cannot work in situ,
*     that is VECTOR and RESULT have to be different arrays.

*  Arguments:
*     BAD = LOGICAL (Given)
*        If true, bad values are propagated.
*     NELM = INTEGER (Given)
*        Length of the vector arrays.
*     FACTOR = REAL (Given)
*        The factor to be multiplied with.
*     VECTOR( NELM ) = REAL (Given)
*        The vector to be multiplied with the factor.
*     RESULT( NELM ) = REAL (Returned)
*        The result vector.
*     IERR = INTEGER (Returned)
*        Argument returned by VEC_MULR.
*     NERR = INTEGER (Returned)
*        Argument returned by VEC_MULR.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     18 Sep 1991 (hme):
*        Original version.
*     25 Nov 1994 (hme):
*        Renamed from MULCR. Generalised for double and single
*        precision.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      LOGICAL BAD
      INTEGER NELM
      REAL FACTOR
      REAL VECTOR( NELM )

*  Arguments Returned:
      INTEGER IERR
      INTEGER NERR
      REAL RESULT( NELM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Fill result vector with factor and multiply the two vectors.
      DO 1 I = 1, NELM
         RESULT(I) = FACTOR
 1    CONTINUE
      CALL VEC_MULR( BAD, NELM, RESULT, VECTOR, RESULT, IERR, NERR,
     :   STATUS )

      END
