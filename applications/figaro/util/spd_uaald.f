      SUBROUTINE SPD_UAALD( NELM, ARRAY1, ARRAY2, EPS, EQUAL, STATUS )
*+
*  Name:
*     SPD_UAAL{DR}

*  Purpose:
*     Compare two arrays for equality.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_UAALD( NELM, ARRAY1, ARRAY2, EPS, EQUAL, STATUS )

*  Description:
*     This routine compares element-by-element two given arrays. If any
*     element differst by more than a specified amount, then the routine
*     returns a value of false. The equality criterium is that the
*     difference between ARRAY1(I) and ARRAY2(I) must be less than or
*     equal to EPS times their average. EPS can be specified as zero.

*  Arguments:
*     NELM = INTEGER (Given)
*        The size of the arrays.
*     ARRAY1( NELM ) = DOUBLE PRECISION (Given)
*        The first array.
*     ARRAY2( NELM ) = DOUBLE PRECISION (Given)
*        The second array.
*     EPS = DOUBLE PRECISION (Given)
*        The relative difference which array elements may exhibit
*        without being considered unequal.
*     EQUAL = LOGICAL (Returned)
*        This is returned false if one or more corresponding elements
*        from the two arrays are unequal. The test is not for exact
*        equality. What is considered equal depends on the given EPS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     01 Jul 1992 (hme):
*        Original version.
*     24 May 1993 (hme):
*        Fix bug: absolute difference must be checked against average of
*        absolute values, not agains absolute value of average.
*     25 Nov 1994 (hme):
*        Renamed from SPACJD.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NELM
      DOUBLE PRECISION ARRAY1( NELM )
      DOUBLE PRECISION ARRAY2( NELM )
      DOUBLE PRECISION EPS

*  Arguments Returned:
      LOGICAL EQUAL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index
      DOUBLE PRECISION HLFEPS              ! Half of EPS
      DOUBLE PRECISION NUMBR1, NUMBR2      ! Copied elements

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      HLFEPS = EPS / 2.
      EQUAL = .TRUE.
      DO 1 I = 1, NELM
         NUMBR1 = ARRAY1(I)
         NUMBR2 = ARRAY2(I)
         IF ( ABS( NUMBR1-NUMBR2 ) .GT.
     :        ( ABS( NUMBR1) + ABS( NUMBR2 ) ) * HLFEPS ) THEN
            EQUAL = .FALSE.
            GO TO 2
         END IF
 1    CONTINUE
 2    CONTINUE

      END
