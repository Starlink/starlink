      SUBROUTINE SPD_UAAWD( BAD, NELM, VECTOR,
     :   NGOOD, SQRSUM, STATUS )
*+
*  Name:
*     SPD_UAAW{DR}

*  Purpose:
*     Sum of squares over a vector.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_UAAWD( BAD, NELM, VECTOR, NGOOD, SQRSUM, STATUS )

*  Description:
*     This routine returns the sum of the squares of all elements of a
*     given vector. It can optionally take care of bad values.

*  Arguments:
*     BAD = LOGICAL (Given)
*        True if the vector has to be checked for bad values.
*     NELM = INTEGER (Given)
*        The number of elements in the vector.
*     VECTOR( NELM ) = DOUBLE PRECISION (Given)
*        The vector from which all elements are squared and added up.
*     NGOOD = INTEGER (Returned)
*        The number of elements contributing to the sum.
*     SQRSUM = DOUBLE PRECISION (Returned)
*        The sum of squares of all elements from the vector.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     27 Mar 1992 (hme):
*        Original version.
*     26 Jan 1995 (hme):
*        Renamed from SPABN. Made generic.
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
      DOUBLE PRECISION VECTOR( NELM )

*  Arguments Returned:
      INTEGER NGOOD
      DOUBLE PRECISION SQRSUM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise result.
      SQRSUM = 0.
      NGOOD  = 0

*  If check for bad values needed.
      IF ( BAD ) THEN
         DO 1 I = 1, NELM
            IF ( VECTOR(I) .NE. VAL__BADD ) THEN
               SQRSUM = SQRSUM + VECTOR(I) * VECTOR(I)
               NGOOD  = NGOOD  + 1
            END IF
 1       CONTINUE

*  Else (no check for bad values).
      ELSE
         DO 2 I = 1, NELM
            SQRSUM = SQRSUM + VECTOR(I) * VECTOR(I)
 2       CONTINUE
         NGOOD = NELM
      END IF

*  Return.
      END
