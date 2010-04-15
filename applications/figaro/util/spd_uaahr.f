      SUBROUTINE SPD_UAAHR( NELM, ARRAY, EPS,
     :   LVAL, UVAL, LINEAR, STATUS )
*+
*  Name:
*     SPD_UAAH{DR}

*  Purpose:
*     Check an array for linearity.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_UAAHR( NELM, ARRAY, EPS, LVAL, UVAL, LINEAR, STATUS )

*  Description:
*     This routine checks element-by-element a given array as to whether
*     the array element value is a linear function of the array index.
*     If any element differs by more than a specified amount from what
*     is expected from a linear change from ARRAY(1) to ARRAY(NELM),
*     then the routine returns a value of false.
*
*     The equality criterium is that the difference between ARRAY(I) and
*     the linear function must be less than or equal to EPS times the
*     maximum of the absolute values of ARRAY(1) and ARRAY(NELM). Thus
*     each element is allowed the same deviation as the end points, and
*     that tolerance expressed in relative terms is EPS. EPS can be
*     specified as zero.

*  Arguments:
*     NELM = INTEGER (Given)
*        The size of the array.
*     ARRAY( NELM ) = REAL (Given)
*        The array to be tested.
*     EPS = REAL (Given)
*        The relative difference which array elements may exhibit
*        without being considered unequal.
*     LVAL = REAL (Returned)
*        Value of the first array element.
*     UVAL = REAL (Returned)
*        Value of the last array element.
*     LINEAR = LOGICAL (Returned)
*        This is returned false if one or more elements from the array
*        differ from the linear interpolation using LVAL and UVAL. The
*        test is not for exact equality. What is considered equal
*        depends on the given EPS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     22 May 1993 (hme):
*        Original version, adapted from SPACJ{DR}.
*     24 May 1993 (hme):
*        Fix bug: absolute difference must be checked against average of
*        absolute values, not agains absolute value of average.
*        That still does not work. We must allow each array element the
*        same absolute tolerance as we allow the biggest one.
*     21 Jun 1994 (hme):
*        Copy from SPADC to SPD_UAAH.
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
      REAL ARRAY( NELM )
      REAL EPS

*  Arguments Returned:
      REAL LVAL
      REAL UVAL
      LOGICAL LINEAR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index
      REAL LOCEPS              ! Local, absolute EPS
      REAL INCR                ! Average value step
      REAL NUMBR1, NUMBR2      ! Copied elements

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      LVAL = ARRAY(1)
      UVAL = ARRAY(NELM)
      INCR = ( UVAL - LVAL ) / FLOAT( NELM - 1 )
      LOCEPS = EPS * MAX( ABS(LVAL), ABS(UVAL) )

      LINEAR = .TRUE.
      DO 1 I = 1, NELM
         NUMBR1 = ARRAY(I)
         NUMBR2 = LVAL + ( I - 1 ) * INCR
         IF ( ABS( NUMBR1-NUMBR2 ) .GT. LOCEPS ) THEN
            LINEAR = .FALSE.
            GO TO 2
         END IF
 1    CONTINUE
 2    CONTINUE

      END
